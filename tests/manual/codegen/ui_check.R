# Drive the app in headless Chrome and check 4 · Export › R Code (PRD §A7.9).
#
#   Rscript tests/manual/codegen/ui_check.R
#
# Launches edark() twice in a background R process - once plain, once from a
# session file built from a scenario (so Analyze roles are restored) - and
# checks: the Content / R Code pills, that the script appears on entry and
# parses, the info pane, Download Script, the Copy button, the script row on
# the Content checklist, and that changing an option regenerates the script.
# Screenshots go to $EDARK_CHECK_DIR (default tempdir()).

suppressMessages(devtools::load_all(quiet = TRUE))
source("tests/manual/codegen/scenarios.R")
out_dir <- Sys.getenv("EDARK_CHECK_DIR", file.path(tempdir(), "edark_codegen_ui"))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

fails <- 0L
.ok <- function(what, cond) {
  cat(sprintf("  %s %s\n", if (isTRUE(cond)) "ok  " else "FAIL", what))
  if (!isTRUE(cond)) fails <<- fails + 1L
}

.launch <- function(port, session_path = NULL) {
  callr::r_bg(function(port, session_path, pkg) {
    suppressMessages(devtools::load_all(pkg, quiet = TRUE))
    app <- if (is.null(session_path)) edark() else edark(liver_tx, session = session_path)
    shiny::runApp(app, port = port, launch.browser = FALSE)
  }, args = list(port = port, session_path = session_path, pkg = getwd()), supervise = TRUE)
}

.wait_for <- function(b, js, timeout = 60) {
  t0 <- Sys.time()
  repeat {
    v <- tryCatch(b$Runtime$evaluate(js)$result$value, error = function(e) NULL)
    if (isTRUE(v)) return(TRUE)
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout) return(FALSE)
    Sys.sleep(0.5)
  }
}
.js <- function(b, js) b$Runtime$evaluate(js)$result$value
.click <- function(b, selector) .js(b, sprintf("document.querySelector(%s).click(); true", jsonlite::toJSON(selector, auto_unbox = TRUE)))

.check_app <- function(label, port, session_path = NULL, expect_analysis = FALSE) {
  cat(sprintf("\n== %s ==\n", label))
  proc <- .launch(port, session_path)
  on.exit(proc$kill(), add = TRUE)
  url <- sprintf("http://127.0.0.1:%d", port)
  ok <- FALSE
  for (i in 1:120) {
    ok <- tryCatch({ readLines(url, warn = FALSE); TRUE }, error = function(e) FALSE)
    if (ok) break
    Sys.sleep(1)
  }
  .ok("app started", ok)
  if (!ok) { cat(proc$read_all_error()); return(invisible()) }

  b <- chromote::ChromoteSession$new(width = 1440, height = 900)
  on.exit(b$close(), add = TRUE)
  b$Page$navigate(url)
  .ok("app connected", .wait_for(b, "!!(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected())"))
  # A launch session navigates once it is applied; let it finish first
  Sys.sleep(if (is.null(session_path)) 2 else 10)

  # Export > R Code
  .click(b, "a[data-value='export']")
  .ok("Export shows Content and R Code pills",
      .wait_for(b, "!!document.querySelector(\"a[data-value='content']\") && !!document.querySelector(\"a[data-value='code']\")", 20))
  .ok("script row is tickable on the Content checklist",
      .wait_for(b, "(function(){var x=document.querySelector(\"input.edark-export-box[data-id='reproduce/analysis_script']\");return !!x && !x.disabled;})()", 30))
  .ok("script row is ticked by default",
      isTRUE(.js(b, "document.querySelector(\"input.edark-export-box[data-id='reproduce/analysis_script']\").checked")))
  .click(b, "a[data-value='code']")
  .ok("script appears on entry", .wait_for(b, "(document.getElementById('export-code')||{}).innerText && document.getElementById('export-code').innerText.indexOf('EDARK analysis script') >= 0", 60))
  txt <- .js(b, "document.getElementById('export-code').innerText")
  parsed <- tryCatch({ parse(text = txt); TRUE }, error = function(e) conditionMessage(e))
  .ok(paste("script parses", if (!isTRUE(parsed)) parsed else ""), isTRUE(parsed))
  .ok("script is plain ASCII", !any(utf8ToInt(txt) > 127L))
  .ok("script reads the built-in dataset", grepl("input_data <- edark::liver_tx", txt, fixed = TRUE))
  .ok(sprintf("analysis section %s", if (expect_analysis) "present" else "absent"),
      identical(grepl("Analysis dataset (Analyze > Setup)", txt, fixed = TRUE), expect_analysis))
  info <- .js(b, "document.getElementById('export-code_info').innerText")
  .ok("info pane lists what the script repeats", grepl("THE SCRIPT REPEATS|The script repeats", info) && grepl("Input data and Prepare", info))
  .ok("Copy button carries its target", identical(.js(b, "document.getElementById('export-code_copy').getAttribute('data-copy-target')"), "export-code"))
  .ok("Download Script has a link", .wait_for(b, "(document.getElementById('export-code_download')||{}).getAttribute && /session\\//.test(document.getElementById('export-code_download').getAttribute('href')||'')", 20))
  href <- .js(b, "document.getElementById('export-code_download').href")
  dl <- tryCatch(readLines(href, warn = FALSE), error = function(e) character(0))
  .ok("Download Script returns the same script", length(dl) > 10 && identical(paste(dl, collapse = "\n"), gsub("\r", "", txt)) ||
        identical(trimws(paste(dl, collapse = "\n")), trimws(gsub("\r", "", txt))))
  b$screenshot(file.path(out_dir, paste0(label, "_rcode.png")), selector = "body", wait_ = TRUE)

  # Switching to "A file" regenerates the script
  .js(b, "(function(){var r=document.querySelector(\"input[name='export-code_source'][value='file']\"); r.click(); return true;})()")
  .ok("choosing a file regenerates the script",
      .wait_for(b, "document.getElementById('export-code').innerText.indexOf('readRDS(\"input_data.rds\")') >= 0", 20))
  .js(b, "(function(){var c=document.getElementById('export-code_figures'); c.click(); return true;})()")
  .ok("turning figures off drops the ggplot code",
      .wait_for(b, "document.getElementById('export-code').innerText.indexOf('pacman::p_load(dplyr)') >= 0 || document.getElementById('export-code').innerText.indexOf('ggplot(') < 0", 20))
  b$screenshot(file.path(out_dir, paste0(label, "_rcode_file.png")), selector = "body", wait_ = TRUE)

  errs <- proc$read_error()
  .ok("no errors in the R console", !grepl("Error|Warning: Error", errs))
  if (nzchar(errs)) cat(errs)
}

.check_app("plain", 8811)

# A session restores Prepare and the Analyze roles (no fitted outputs)
st <- cg_build_state(cg_scenarios$logistic_bootstrap)
sess <- build_session(dataset_input = liver_tx, column_types = st$original_column_types,
                      prepare = st$last_applied_specs, analysis_spec = st$analysis_spec,
                      custom_report_items = list(), include_data = FALSE)
sess_path <- file.path(out_dir, "check.edark.rds")
saveRDS(sess, sess_path)
.check_app("session", 8812, sess_path, expect_analysis = TRUE)

cat(sprintf("\n%s\n", if (fails == 0L) "All UI checks passed." else sprintf("%d UI check(s) failed.", fails)))
if (fails > 0L) quit(status = 1)
