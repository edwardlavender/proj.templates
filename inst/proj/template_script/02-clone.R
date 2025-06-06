###########################
###########################
#### 02-clone.R

#### Aims
# 1) Clone a project.
# This script is designed to be used to set up an established project, shared
# via GitHub, on another machine. This includes:
# ... A) Creation of directory structure
# ... B) Installation of dependencies

#### Prerequisites
# 1) NA


###########################
###########################
#### Restore project dependencies

#### Option (1): Automatic restoration via renv
# Restore the project's dependencies (including proj.templates) from the lockfile
restore <- tryCatch(renv::restore(), error = function(e) e)

#### Option (2): Manual re-installation of packages
if (inherits(restore, "error")) {

  # Read dependency list
  pkg <- readRDS(file.path("data", "inst", "dependencies.rds"))
  # Install {renv}
  if (!requireNamespace("renv", quietly = TRUE)) {
    install.packages("renv")
  }
  # Manually re-install packages using {renv}
  t1 <- Sys.time()
  inst_log <-
    lapply(split(pkg, seq_len(nrow(pkg))), function(d) {
      success <- tryCatch(eval(parse(text = d$install)),
                          error = function(e) e)

      if (!inherits(success, "error")) {
        msg <- ""
        success <- TRUE
      } else {
        msg <- as.character(success)
        success <- FALSE
      }
      d$success <- success
      d$msg     <- msg
      d
    })
  # Check success
  t2 <- Sys.time()
  difftime(t2, t1)
  inst_log <- do.call(rbind, inst_log)
  inst_log

  # Manually handle any failures e.g., via renv::hydrate().
  # ... OK.

}


###########################
###########################
#### Rebuild project structure

#### Recreate high-level project structure
proj.templates::use_template_proj(overwrite = FALSE)

#### Rebuild directory tree
tree <- here::here("data", "inst", "tree.rds")
if (file.exists(tree)) {
  tree <- readRDS(tree)
  proj.templates::use_template_tree(tree = tree, recreate = TRUE)
}

#### Re-run project workflow
# Re-obtain raw dataset(s)
# Re-run data processing
# Re-run analysis


#### End of code.
###########################
###########################
