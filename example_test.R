#!/usr/bin/env Rscript
# example_test.R
#
# Runs ALL documented examples in the idealstan package, including \dontrun{}
# blocks, and prints a labelled PASS / FAIL summary for each function.
#
# Usage
#   Rscript example_test.R          # from a terminal
#   source("example_test.R")        # from an R session

# Functions that launch interactive sessions (Shiny apps) and cannot be run
# non-interactively — skip them regardless of \dontrun{} status.
INTERACTIVE_SKIP <- c("id_plot_gbeta_prior", "launch_shinystan")

suppressPackageStartupMessages({
  library(idealstan)
  library(tools)
})

# ── helpers ──────────────────────────────────────────────────────────────────

.has_examples <- function(rd) {
  any(vapply(rd, function(x) identical(attr(x, "Rd_tag"), "\\examples"), logical(1)))
}

# Muffle package-load warnings that are not caused by our code.
.benign <- function(msg) {
  grepl("was built under R version|Registered S3|masked from|no non-missing", msg)
}

# ── discover topics ───────────────────────────────────────────────────────────

rd_db   <- tools::Rd_db("idealstan")
rd_keys <- names(rd_db)                   # e.g. "id_make.Rd"
rd_fns  <- sub("\\.Rd$", "", rd_keys)     # e.g. "id_make"

# Keep topics that have \examples; drop -method alias pages (they duplicate
# the generic's examples and would run the same code twice).
keep    <- vapply(rd_db, .has_examples, logical(1))
no_dash <- !grepl("-", rd_fns)

topics_fn  <- sort(rd_fns[keep & no_dash])
topics_key <- paste0(topics_fn, ".Rd")

# ── run ───────────────────────────────────────────────────────────────────────

cat("=== idealstan Example Tests (including \\dontrun{}) ===\n\n")
cat(sprintf("  Package version : %s\n", packageVersion("idealstan")))
cat(sprintf("  Topics to test  : %d\n\n", length(topics_fn)))

COL_W <- 35L

results <- vector("list", length(topics_fn))
names(results) <- topics_fn

for (i in seq_along(topics_fn)) {
  fn  <- topics_fn[[i]]
  cat(sprintf("  %-*s", COL_W, fn))

  if (fn %in% INTERACTIVE_SKIP) {
    cat("[SKIP] (launches interactive Shiny session)\n")
    results[[fn]] <- list(status = "SKIP", message = "")
    next
  }

  tmp_pdf <- tempfile(fileext = ".pdf")

  res <- withCallingHandlers(
    tryCatch({
      grDevices::pdf(tmp_pdf)
      # run.dontrun = TRUE executes \dontrun{} blocks as well.
      # Use do.call so 'fn' is evaluated to its string value — example()
      # uses substitute() internally, so passing a bare variable would make
      # it search for help on the symbol name rather than its value.
      do.call("example", list(fn, package = "idealstan",
                              run.dontrun = TRUE,
                              echo = FALSE, verbose = FALSE, local = TRUE))
      grDevices::dev.off()
      unlink(tmp_pdf)
      list(status = "PASS", message = "")
    }, error = function(e) {
      try(grDevices::dev.off(), silent = TRUE)
      unlink(tmp_pdf)
      list(status = "FAIL", message = conditionMessage(e))
    }),
    warning = function(w) {
      if (.benign(conditionMessage(w))) invokeRestart("muffleWarning")
    }
  )

  cat(sprintf("[%s]\n", res$status))
  if (nchar(res$message) > 0) cat(sprintf("      %s\n", res$message))
  results[[fn]] <- res
}

# ── summary ───────────────────────────────────────────────────────────────────

n_pass <- sum(vapply(results, function(r) r$status == "PASS", logical(1)))
n_fail <- sum(vapply(results, function(r) r$status == "FAIL", logical(1)))
n_skip <- sum(vapply(results, function(r) r$status == "SKIP", logical(1)))

cat(sprintf(
  "\n=== Summary (%d topics) ===\n  PASS %-3d  FAIL %-3d  SKIP %d\n",
  length(topics_fn), n_pass, n_fail, n_skip
))

failures <- Filter(function(r) r$status == "FAIL", results)
if (length(failures) > 0) {
  cat("\nFailed topics:\n")
  for (nm in names(failures)) {
    cat(sprintf("  - %s\n      %s\n", nm, failures[[nm]]$message))
  }
  cat(sprintf("\n%d example(s) failed.\n", n_fail))
  if (!interactive()) quit(status = 1L)
} else {
  cat("\nAll examples passed.\n")
}

invisible(results)
