# The R script generator (R/service_analysis_codegen.R, PRD §A7.9).
# The end-to-end check - every Analyze step, script vs app - is
# tests/manual/codegen/run_check.R; these are the fast parts.

.cg_test_state <- function(fit = FALSE) {
  input <- liver_tx
  input$referral_source <- as.character(input$referral_source)
  input$preop_sodium    <- as.character(input$preop_sodium)
  orig <- cast_column_types(input, 20)
  la <- list(
    included_columns       = setdiff(names(orig), c("patient_mrn", "case_date")),
    column_type_overrides  = list(surgery_start_hour = "factor"),
    column_transform_specs = list(
      recipient_bmi    = list(method = "cutpoints", breakpoints = c(25, 30, 500)),
      intraop_ebl_ml   = list(method = "winsorize", lower_pct = 5, upper_pct = 95),
      preop_creatinine = list(method = "log", log_base = "log10"),
      donor_age        = list(method = "standardize"),
      preop_hb         = list(method = "round", decimal_places = 0),
      intraop_rbc_units = list(method = "auto")
    ),
    row_filter_specs = list(
      recipient_age   = list(type = "numeric", min = 20, max = 75),
      referral_source = list(type = "categorical", levels_selected = c("internal", "external"))
    )
  )
  wd <- .apply_column_type_overrides(orig, la$column_type_overrides)
  wd <- wd[, intersect(la$included_columns, names(wd)), drop = FALSE]
  wd <- .apply_row_filters(.apply_column_transforms(wd, la$column_transform_specs), la$row_filter_specs)

  spec <- res <- adata <- NULL
  if (fit) {
    adata <- wd
    adata$.edark_row_id <- seq_len(nrow(adata))
    spec <- list(
      specification_metadata = list(dataset_signature = digest::digest(wd, algo = "sha256")),
      variable_roles = list(outcome_variable = "ead", exposure_variable = "liver_donor_type",
                            final_model_covariates = c("recipient_age", "preop_meld"),
                            univariable_test_pool = c("recipient_age", "preop_meld"),
                            reference_levels = list()),
      model_design = utils::modifyList(.default_model_design(), list(model_type = "logistic")),
      purpose_specification = .default_purpose_specification()
    )
    f <- fit_analysis_model(spec, adata)
    res <- list(specification_snapshot = spec,
                run_status = list(status = f$status, n_used = f$n_used, n_total = f$n_total,
                                  outcome_event = f$outcome_event),
                fitted_models = list(primary_model = f$model),
                inference_summary = list(coefficients = f$coefficients))
  }
  export_state(dataset_input = input, dataset_original = orig, dataset_working = wd,
               original_column_types = detect_column_types(orig), last_applied_specs = la,
               analysis_data = adata, analysis_spec = spec, analysis_result = res)
}

# Evaluate the script from "Input data" on, with the dplyr verbs it uses -
# skipping the package set-up (pacman), which tests must not run
.cg_run_prepare <- function(lines, input) {
  start <- grep("^# ---- 1\\. Input data", lines)
  stopifnot(length(start) == 1L)
  verbs <- list(`%>%` = magrittr::`%>%`, mutate = dplyr::mutate, select = dplyr::select,
                filter = dplyr::filter, all_of = dplyr::all_of)
  env <- new.env(parent = list2env(verbs, parent = globalenv()))
  env$input_data <- input
  eval(parse(text = lines[start:length(lines)]), envir = env)
  env
}


test_that("string literals are plain ASCII and read back unchanged", {
  x <- c("a \"quoted\" \\ path", "25 \u2013 < 30", "\u2265 40", "tab\there", "caf\u00e9")
  code <- .cg_chr(x)
  expect_false(any(utf8ToInt(code) > 127L))
  expect_identical(eval(parse(text = code)), x)
  expect_identical(.cg_chr(character(0)), "character(0)")
  expect_identical(.cg_name(c("ok_name", "two words", "1st")), c("ok_name", "`two words`", "`1st`"))
})

test_that("wrapped formulas, vectors and numbers parse to what they say", {
  preds <- paste0("a_long_predictor_name_", 1:12)
  f <- .cg_formula("outcome", preds, "center", start = 17L)
  expect_true(all(nchar(strsplit(f, "\n")[[1]]) <= 78L))
  # A wrapped line never starts with "+", which would end the formula above it
  expect_false(any(grepl("^\\s*\\+", strsplit(f, "\n")[[1]])))
  expect_identical(all.vars(eval(parse(text = f))), c("outcome", preds, "center"))
  expect_identical(eval(parse(text = .cg_chr(preds))), preds)
  num <- c(-Inf, 0.1, 25.123456789012, 1e-12, 3, Inf)
  expect_equal(eval(parse(text = .cg_num(num))), num, tolerance = 1e-14)
})

test_that("the script rebuilds the working dataset from the input dataset", {
  st <- .cg_test_state()
  g  <- generate_analysis_script(st, list(data_source = "file", data_path = "x.rds"))
  expect_silent(parse(text = g$text))
  expect_false(any(utf8ToInt(g$text) > 127L))
  env <- .cg_run_prepare(g$lines, st$dataset_input)
  wd <- env$working_data
  rownames(wd) <- NULL
  ref <- st$dataset_working
  rownames(ref) <- NULL
  expect_equal(wd, ref)
  expect_identical(g$sections$status[g$sections$id == "prepare"], "included")
  expect_true(all(g$sections$status[g$sections$id != "prepare"] == "not_run"))
  expect_identical(g$packages, "dplyr")
})

test_that("the built-in dataset is read from the package when asked", {
  st <- .cg_test_state()
  g <- generate_analysis_script(st, list(data_source = "liver_tx"))
  expect_true(any(grepl("input_data <- edark::liver_tx", g$lines, fixed = TRUE)))
  g <- generate_analysis_script(st, list(data_source = "file", data_path = "C:/data/my file.rds"))
  expect_true(any(grepl("readRDS(\"C:/data/my file.rds\")", g$lines, fixed = TRUE)))
})

test_that("a fitted model is included, and left out once it is stale", {
  skip_on_cran()
  st <- .cg_test_state(fit = TRUE)
  g  <- generate_analysis_script(st)
  expect_identical(g$sections$status[g$sections$id == "model"], "included")
  expect_true(any(grepl("^model <- fit_model\\(model_formula, model_rows\\)", g$lines)))
  expect_silent(parse(text = g$text))

  # The spec moved on after the fit: the model is stale and not in the script
  st$analysis_spec$variable_roles$final_model_covariates <- "recipient_age"
  g <- generate_analysis_script(st)
  expect_identical(g$sections$status[g$sections$id == "model"], "stale")
  expect_false(any(grepl("model_formula", g$lines, fixed = TRUE)))
  expect_true(any(g$messages$level == "stale"))

  # Prepare changed since the freeze: nothing from Analyze
  st <- .cg_test_state(fit = TRUE)
  st$prepare_changed <- TRUE
  g <- generate_analysis_script(st)
  expect_identical(g$sections$status[g$sections$id == "analysis_data"], "stale")
  expect_false(any(grepl("analysis_data <-", g$lines, fixed = TRUE)))
})

test_that("the export registry offers the script and writes both files", {
  st <- .cg_test_state()
  it <- export_items(st)
  expect_identical(it$status[it$id == "reproduce/analysis_script"], "available")
  job <- export_job(it, "reproduce/analysis_script", st,
                    list(data_format = "rds", report_format = "docx", script = list(data_source = "liver_tx")))
  job <- .export_job_step(job)
  path <- file.path(job$root, "reproduce", "analysis_script.R")
  expect_true(file.exists(path))
  expect_true(file.exists(file.path(job$root, "reproduce", "edark_functions.R")))
  expect_true(any(grepl("edark::liver_tx", readLines(path), fixed = TRUE)))
  fin <- .export_job_finish(job)
  expect_true(any(grepl("edark_functions.R", readLines(file.path(job$root, "README.txt")), fixed = TRUE)))
  unlink(fin$dir, recursive = TRUE)
})

test_that("settings are written as R code that reads back unchanged", {
  spec <- list(variable_roles = list(outcome_variable = "ead", exposure_variable = NULL,
                                     final_model_covariates = paste0("covariate_number_", 1:9),
                                     reference_levels = list(bmi = "25 – < 30", `odd name` = "a")),
               table1_specification = list(stratify_by_exposure = TRUE, include_pvalues_outcome = FALSE),
               validation_settings = list(cv_folds = 10L, bootstrap_reps = NULL, seed = 20260919L, frac = 0.25))
  code <- .cg_value(spec)
  expect_false(any(utf8ToInt(code) > 127L))
  expect_true(all(nchar(strsplit(code, "\n")[[1]]) <= 78L))
  expect_identical(eval(parse(text = code)), spec)
})

test_that("edark_functions.R is EDARK's own code, exactly", {
  skip_on_cran()
  st <- .cg_test_state(fit = TRUE)
  g  <- generate_analysis_script(st)
  fn <- g$functions
  expect_gt(fn$n, 5L)
  expect_false(any(utf8ToInt(fn$text) > 127L))
  expect_false(any(grepl("shiny::", fn$lines, fixed = TRUE)))
  # Every object the script needs is defined, and reads back identical to the
  # one in the package
  env <- new.env(parent = globalenv())
  eval(parse(text = fn$text), envir = env)
  ns <- asNamespace("edark")
  expect_true(all(c("edark_coef_table", ".prepare_model_rows", ".fit_statistics") %in% ls(env, all.names = TRUE)))
  for (n in ls(env, all.names = TRUE)) {
    a <- get(n, envir = ns)
    b <- get(n, envir = env)
    if (is.function(a)) {
      expect_identical(deparse(a), deparse(b), info = n)
    } else {
      expect_identical(a, b, info = n)
    }
  }
  # Everything the script calls from EDARK is in the file
  expect_true(all(.cg_entry_points(g$lines) %in% ls(env, all.names = TRUE)))
})
