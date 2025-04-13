test_that("use_template_script() generates a file.", {
  if (interactive()) {
    con <- file.path(tempdir(), "tmp.R")
    use_template_script(con, open = FALSE, overwrite = TRUE)
    expect_true(file.exists(con))
    expect_error(use_template_script(con, overwrite = FALSE))
  }
})
