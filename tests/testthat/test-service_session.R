# Session save / load service (§M8). Pure functions only - the Shiny side is
# verified in the browser.

.session_df <- function(n = 40) {
  set.seed(1)
  data.frame(
    age   = round(runif(n, 20, 80)),
    bmi   = runif(n, 18, 40),
    sex   = factor(sample(c("F", "M"), n, replace = TRUE)),
    site  = sample(c("A", "B", "C"), n, replace = TRUE),
    alive = sample(c(TRUE, FALSE), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

.session_state <- function(df) {
  cast <- cast_column_types(df)
  list(cast = cast, types = detect_column_types(cast))
}

.session_prepare <- function(df) {
  list(
    included_columns       = setdiff(names(df), "alive"),
    column_type_overrides  = list(),
    column_transform_specs = list(age = list(method = "cutpoints", breakpoints = c(40, 60))),
    row_filter_specs       = list(
      bmi = list(type = "numeric", min = 20, max = 35),
      sex = list(type = "factor", levels_selected = c("F", "M"))
    )
  )
}

.session_spec <- function() {
  list(
    variable_roles = list(
      outcome_variable       = "sex",
      exposure_variable      = "age",
      candidate_covariates   = c("bmi", "site"),
      cluster_variables      = NULL,
      reference_levels       = list(sex = "F", site = "A"),
      final_model_covariates = "bmi"
    ),
    purpose_specification = list(model_purpose = "prediction", validation_method = "cv",
                                 split_variable = NULL, training_level = NULL),
    validation_settings   = list(cv_folds = 5L, cv_repeats = 2L,
                                 bootstrap_reps = NULL, seed = 42L)
  )
}

.session_build <- function(df = .session_df(), ...) {
  st <- .session_state(df)
  build_session(df, st$types, .session_prepare(df), .session_spec(), ...)
}


test_that("definition uses input classes and the signature ignores column order", {
  df  <- .session_df()
  def <- dataset_definition(df)
  expect_equal(unname(def[c("site", "alive")]), c("character", "logical"))
  expect_identical(dataset_signature(def), dataset_signature(dataset_definition(df[, rev(names(df))])))
  expect_false(identical(dataset_signature(def), dataset_signature(dataset_definition(df[, -1]))))
})

test_that("save then read gives an identical session", {
  s    <- .session_build()
  path <- tempfile(fileext = ".edark.rds")
  saveRDS(s, path)
  expect_identical(read_session(path), s)
  expect_null(s$data)
  expect_equal(s$analysis$final_model_covariates, "bmi")
  expect_equal(s$analysis$validation_settings$cv_folds, 5L)
})

test_that("data is stored only when asked", {
  df <- .session_df()
  expect_identical(.session_build(df, include_data = TRUE)$data, df)
})

test_that("no analysis block before Start Analysis", {
  df <- .session_df()
  st <- .session_state(df)
  expect_null(build_session(df, st$types, .session_prepare(df), NULL)$analysis)
})

test_that("invalid files are refused", {
  path <- tempfile(fileext = ".edark.rds")
  writeLines("not an rds", path)
  expect_error(read_session(path), "not a valid EDARK session", class = "edark_session_error")

  s <- .session_build(); s$prepare <- NULL
  expect_error(validate_session(s), "not a valid EDARK session")

  s <- .session_build(); s$prepare$row_filter_specs <- list(f = function() 1)
  expect_error(validate_session(s), "not a valid EDARK session")
})

test_that("a newer schema is refused with the update message", {
  s <- .session_build()
  s$session_schema_version <- .SESSION_SCHEMA_VERSION + 1L
  expect_error(validate_session(s), "newer version of EDARK", class = "edark_session_error")
})

test_that("the same columns and types match, whatever the rows", {
  df <- .session_df()
  s  <- .session_build(df)
  df2 <- .session_df(100)
  expect_null(session_dataset_mismatch(s, df2, .session_state(df2)$types))
})

test_that("a dropped, added or retyped column is refused - no partial loads", {
  df <- .session_df()
  s  <- .session_build(df)

  d1 <- df[, names(df) != "bmi"]
  expect_match(session_dataset_mismatch(s, d1, .session_state(d1)$types), "Missing column", all = FALSE)

  d2 <- df; d2$extra <- 1
  expect_match(session_dataset_mismatch(s, d2, .session_state(d2)$types), "not in the session", all = FALSE)

  d3 <- df; d3$age <- as.character(d3$age)
  expect_match(session_dataset_mismatch(s, d3, .session_state(d3)$types), "Different type", all = FALSE)

  d4 <- datasets::mtcars
  expect_false(is.null(session_dataset_mismatch(s, d4, .session_state(d4)$types)))
})

test_that("a character column cast differently at launch is refused", {
  df <- .session_df()
  s  <- .session_build(df)
  d2 <- df
  d2$site <- paste0("S", seq_len(nrow(d2)))   # 40 unique values: stays character
  expect_match(session_dataset_mismatch(s, d2, .session_state(d2)$types),
               "Read as a different type", all = FALSE)
})

test_that("the Prepare pipeline is rebuilt from the session", {
  df <- .session_df()
  s  <- .session_build(df)
  wd <- session_prepare_dataset(s, .session_state(df)$cast)
  expect_false("alive" %in% names(wd))
  expect_true(is.ordered(wd$age))
  expect_true(all(wd$bmi >= 20 & wd$bmi <= 35))
})

test_that("a transform invalid on the new data, or no rows left, is refused", {
  df <- .session_df()
  s  <- .session_build(df)
  s$prepare$column_transform_specs <- list(bmi = list(method = "log"))
  d2 <- df; d2$bmi[1] <- -1
  expect_error(session_prepare_dataset(s, .session_state(d2)$cast), "transforms do not fit",
               class = "edark_session_error")

  s <- .session_build(df)
  s$prepare$row_filter_specs <- list(bmi = list(type = "numeric", min = 100, max = 200))
  expect_error(session_prepare_dataset(s, .session_state(df)$cast), "leave no rows",
               class = "edark_session_error")
})

test_that("custom report thumbnails travel as bytes", {
  png <- tempfile(fileext = ".png")
  writeBin(as.raw(1:50), png)
  items <- list(list(id = "i1", plot_spec = list(plot_type = "bar_count"),
                     thumb_path = png, title = "sex", added_at = Sys.time()))
  packed <- .session_pack_items(items)
  expect_null(packed[[1]]$thumb_path)
  expect_identical(packed[[1]]$thumb_png, as.raw(1:50))

  unpacked <- .session_unpack_items(packed)
  expect_true(file.exists(unpacked[[1]]$thumb_path))
  expect_identical(readBin(unpacked[[1]]$thumb_path, "raw", 100), as.raw(1:50))
  expect_null(unpacked[[1]]$thumb_png)
})
