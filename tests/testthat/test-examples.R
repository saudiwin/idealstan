# test-examples.R
# Runs every non-\dontrun{} code example in the package documentation and
# expects no errors.  Topics whose entire \examples block is wrapped in
# \dontrun{} are skipped automatically with an informative message.

library(tools)

# ── helpers ───────────────────────────────────────────────────────────────────

.has_examples <- function(rd) {
  any(vapply(rd, function(x) identical(attr(x, "Rd_tag"), "\\examples"), logical(1)))
}

.all_dontrun <- function(rd) {
  ex_sections <- Filter(function(x) identical(attr(x, "Rd_tag"), "\\examples"), rd)
  if (length(ex_sections) == 0) return(TRUE)
  for (item in ex_sections[[1]]) {
    tag <- attr(item, "Rd_tag")
    if (is.null(tag) || identical(tag, "RCODE")) {
      if (nchar(trimws(paste(as.character(item), collapse = ""))) > 0) return(FALSE)
    }
  }
  TRUE
}

# ── discover topics ───────────────────────────────────────────────────────────

rd_db   <- tools::Rd_db("idealstan")
rd_keys <- names(rd_db)
rd_fns  <- sub("\\.Rd$", "", rd_keys)

keep    <- vapply(rd_db, .has_examples, logical(1))
no_dash <- !grepl("-", rd_fns)

selected_idx <- which(keep & no_dash)
topics_fn    <- sort(rd_fns[selected_idx])
topics_key   <- paste0(topics_fn, ".Rd")

# ── one test_that block per topic ─────────────────────────────────────────────

for (i in seq_along(topics_fn)) {
  fn  <- topics_fn[[i]]
  key <- topics_key[[i]]

  local({
    .fn  <- fn
    .key <- key
    .rd  <- rd_db[[.key]]

    test_that(paste("example:", .fn), {

      if (.all_dontrun(.rd)) {
        skip(paste0("all examples wrapped in \\dontrun{} — nothing to run for ", .fn))
      }

      tmp_pdf <- tempfile(fileext = ".pdf")
      on.exit({
        try(grDevices::dev.off(), silent = TRUE)
        unlink(tmp_pdf)
      }, add = TRUE)

      grDevices::pdf(tmp_pdf)

      # Use do.call so .fn is evaluated to its string value; example() uses
      # substitute() internally so passing a variable would search for "fn".
      expect_no_error(
        do.call("example", list(.fn, package = "idealstan", run.dontrun = FALSE,
                                echo = FALSE, verbose = FALSE, local = TRUE))
      )
    })
  })
}
