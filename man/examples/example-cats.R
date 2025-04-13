# Define example function
wrap <- function(.verbose = getOption("Patter.verbose")) {
  # Set up messages and exit handler
  cats <- cat_setup(.fun = "wrap", .verbose = .verbose)
  on.exit(eval(cats$exit, envir = cats$envir), add = TRUE)
  # Run function
  cats$cat("Running function...")
  Sys.sleep(1)
}

# `.verbose = TRUE` sends user output to the console
wrap(.verbose = TRUE)

# `.verbose = {log}.txt` sends user output to file
log.txt <- tempfile(fileext = ".txt")
wrap(.verbose = log.txt)
readLines(log.txt)
unlink(log.txt)

# `.verbose = TRUE` suppresses user output
wrap(.verbose = FALSE)