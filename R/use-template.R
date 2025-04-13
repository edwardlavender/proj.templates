#' @title Use a template RStudio Project structure
#' @description This function creates standard set of folders/files within an established RStudio Project.
#'
#' @param root A character string that defines the project root.
#' @param overwrite A logical variable that defines whether or not to overwrite pre-existing R files (see Details). This does not apply to directories, which are never overwritten.
#'
#' @details This function creates the following sequence of folders and files within the project:
#' * data-raw/ for raw data
#' * data/ for data
#'   * inst/ for [`proj.templates`] project-management files
#' * R/ for `R` scripts (relating to data processing and analysis), including:
#'   * define_global_param.R, a template script for global parameter definitions
#'   * define_helpers.R, a template script for helper function definitions
#' * dev/ for R scripts (relating to project development); namely:
#'   * 01-dev.R, which records project development
#'   * 02-clone.R, which facilitates project cloning
#' * fig/ for figures
#' * doc/ for documents
#'
#' @examples
#' #### Set up example
#' # Create a temporary project
#' proj <- file.path(tempdir(), "tmp")
#' usethis::create_project(path = proj, open = FALSE)
#'
#' #### Example (1): Use a template project
#' list.files(proj)
#' use_template_proj(root = proj)
#' list.dirs(proj, full.names = FALSE)
#' list.files(proj, recursive = TRUE)
#'
#' #### Clean up example
#' unlink(proj, recursive = TRUE)
#'
#' @return The function is called for the side effect of creating files/folders. `invisible(NULL)` is returned.
#'
#' @seealso [`use_template_proj()`], [`use_template_gitignore()`], [`use_template_readme()`], [`use_template_script()`], [`use_template_tree()`]
#' @author Edward Lavender
#' @export

use_template_proj <- function(root = rprojroot::find_rstudio_root_file(),
                              overwrite = FALSE) {
  #### Define helpers
  rlang::check_installed("rprojroot")
  root <- check_dir_exists(root)
  create_dir <- function(dir) {
    if (!dir.exists(dir)) {
      dir.create(dir)
    } else {
      rlang::warn(glue::glue("The '{dir}' directory already exists."))
    }
  }

  #### Define directories
  lapply(c("data-raw", "data", "R", "dev", "doc", "fig"), function(folder) {
    create_dir(file.path(root, folder))
  })
  create_dir(file.path(root, "dev", "templates"))
  create_dir(file.path(root, "data", "inst"))

  #### Add template scripts
  sys_file <- function(...) {
    system.file("proj", "template_script", ..., package = "proj.templates", mustWork = TRUE)
  }
  mapply(
    list(
      "01-dev.R", "02-clone.R",
      "define_global_param.R", "define_helpers.R"
    ),
    list("dev", "dev", "R", "R"),
    FUN = function(file, folder) {
      out <- file.path(root, folder, file)
      success <- file.copy(sys_file(file), out, overwrite = overwrite)
      if (!success) {
        rlang::warn(glue::glue("The file '{file}' already exists."))
      }
    }
  )
  invisible(NULL)
}

#' @title Use a template .gitignore file
#' @description This function updates a .gitignore file with standard additions.
#'
#' @param root A character string that defines the project root.
#'
#' @details This function runs [`usethis::git_vaccinate()`] and adds the following to .gitignore:
#' * data-raw/
#' * data/*
#' * !data/inst
#' * doc/
#' * fig/
#' * *.html
#' * *.pdf
#' * *.docx
#'
#' This function is designed to be called after [`use_template_proj()`].
#'
#' @examples
#' #### Set up example
#' # Create a temporary project
#' wd <- getwd()
#' proj <- file.path(tempdir(), "tmp")
#' usethis::create_project(path = proj, open = FALSE)
#' # Populate with standard 'proj.templates' files
#' # ... see `use_template_proj()` and associated functions
#' # Use git
#' setwd(proj)
#' usethis::use_git()
#'
#' #### Example (1): Update .gitignore file
#' readLines(".gitignore")
#' use_template_gitignore(proj)
#' readLines(".gitignore")
#'
#' setwd(wd)
#'
#' @return The function is called for the side effect of updating .gitignore. `invisible(NULL)` is returned.
#'
#' @seealso [`use_template_proj()`], [`use_template_gitignore()`], [`use_template_readme()`], [`use_template_script()`], [`use_template_tree()`]
#' @author Edward Lavender
#' @export

use_template_gitignore <- function(root = rprojroot::find_rstudio_root_file()) {
  rlang::check_installed(c("rprojroot", "usethis"))
  root <- check_dir_exists(root)
  usethis::git_vaccinate()
  con <- file.path(root, ".gitignore")
  check_file_exists(con)
  gitignore <- readLines(con)
  write <- function(x) cat(paste0(x, "\n"), file = con, append = TRUE)
  ignore <- c(
    "data-raw/",
    "data/*", "!data/inst",
    "doc/",
    "fig/",
    "*.html", "*.pdf", "*.docx"
  )
  sapply(ignore, function(ig) {
    # print(ig)
    if (all(!gitignore == stringr::str_trim(ig))) {
      write(ig)
    } else {
      rlang::warn(glue::glue("'{ig}' detected in .gitignore."))
    }
  })
  invisible(NULL)
}

#' @title Use a template README file
#' @description This function adds a standard template README file to a project.
#'
#' @param title A character string that defines the README's title.
#' @param author A character string that defines the lead author (e.g., `"Edward Lavender"`).
#' @param email A character string that defines a contact email address.
#' @param root A character string that defines the project root.
#' @param open A logical variable that defines whether or not to open the template. This requires `rstudioapi` is available. If not, `open` is silently ignored.
#'
#' @details This function creates a standard README as part of [`proj.templates`]'s `use_template_*()` workflow. The README includes the following sections:
#' * Introduction
#' * Description
#'   * Dependencies
#'   * Directories
#' * Instructions
#' * Methods
#' * Results
#'
#' These sections assume that the project has been created following a standard analytical workflow, with the project using `renv` for dependency management, a standard set of directories and selected file-naming conventions. This can be tailored manually to suite project requirements.
#'
#' Internally, the function calls [`usethis::use_readme_rmd()`], which has several nice features (such as error checks and the addition of a git hook for .md and .Rmd files) and then updates the contents of the README with a standardised template structure, customised by user inputs.
#'
#' @return The function is called for the side effect of creating a README file. If a file is created, `invisible(TRUE)` is returned; otherwise, `invisible(FALSE)` is returned.
#'
#' @examples
#' #### Set up example
#' # Create a temporary project
#' proj <- file.path(tempdir(), "tmp")
#' usethis::create_project(path = proj, open = FALSE)
#' # Populate with standard 'proj.templates' files
#' # ... see `use_template_proj()` and associated functions
#'
#' #### Example (1): Use a template readme
#' use_template_readme(
#'   title = "proj.templates",
#'   author = "Edward Lavender",
#'   email = "el72@st-andrews.ac.uk",
#'   root = proj
#' )
#' list.files(proj)
#'
#' @seealso [`use_template_proj()`], [`use_template_gitignore()`], [`use_template_readme()`], [`use_template_script()`], [`use_template_tree()`]
#' @author Edward Lavender
#' @export

use_template_readme <- function(title = "README",
                                author,
                                email,
                                root = rprojroot::find_rstudio_root_file(),
                                open = rlang::is_interactive()) {
  # Checks
  rlang::check_installed(c("rprojroot", "usethis", "knitr"))
  if (missing(author)) rlang::abort("`author` argument is required.")
  if (missing(email)) rlang::abort("`email` argument is required.")
  # Create default readme
  # ... This approach implements useful checks
  # ... And creates a git hook for Rmd/md
  wd <- getwd()
  setwd(root)
  on.exit(setwd(wd), add = TRUE)
  overwrite <- usethis::use_readme_rmd(open = FALSE)
  if (!overwrite) {
    return(invisible(FALSE))
  } else {
    # Read template readme contents
    template <-
      system.file("proj", "README_template.Rmd",
        package = "proj.templates", mustWork = TRUE
      ) |>
      readLines(warn = FALSE)
    # Update template with project-specific information
    inserts <- list(title = title, author = author, email = email)
    for (i in seq_len(length(inserts))) {
      template <-
        gsub(
          pattern = paste0("insert_", names(inserts)[i]),
          replacement = inserts[[i]],
          x = template,
          ignore.case = FALSE
        )
    }
    # Overwrite readme contents with (updated) template
    file <- file.path(root, "README.Rmd")
    writeLines(text = template, con = file)
    # Update md file
    knitr::knit(file)
    # Open file
    if (open && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      rstudioapi::navigateToFile(file)
    }
  }
  invisible(TRUE)
}


#' @title Use a template R script
#' @description This function generates a template R script.
#' @param file A character string that defines the name of the file to be created.
#' @param open A logical variable that defines whether or not to open the template. This requires `rstudioapi` is available. If not, `open` is silently ignored.
#' @param overwrite A logical variable that defines whether or not to overwrite `file` if it already exists.
#' @return The function is used for the side effect of generating a template R script.
#' @examples
#' if (interactive()) use_template_script(file.path(tempdir(), "tmp.R"))
#' @seealso [`use_template_proj()`], [`use_template_gitignore()`], [`use_template_readme()`], [`use_template_script()`], [`use_template_tree()`]
#' @author Edward Lavender
#' @export

use_template_script <- function(file, open = rlang::is_interactive(), overwrite = FALSE) {
  #### Define helper functions
  add_space <- function() cat("\n")
  add_spaces <- function() {
    add_space()
    add_space()
  }
  add_title_para <- function(title) {
    cat(glue::glue("{glue::glue_collapse(rep('#', 4))} {title}"))
    add_space()
  }

  add_title_section <- function(title) {
    add_title_para(title)
    add_space()
  }
  # add_hash       <- function() cat("# \n")
  add_hash_num <- function(x) cat(paste0("# ", x, ") \n"))
  add_hash_line <- function() cat(glue::glue_collapse(rep("#", 25)))
  add_hash_lines <- function(end = FALSE) {
    add_hash_line()
    add_space()
    add_hash_line()
    if (!end) add_space()
  }
  add_section <- function(title) {
    add_hash_lines()
    add_title_section(title)
    add_title_para("")
    add_spaces()
    add_title_para("")
    add_spaces()
    add_title_para("")
    add_spaces()
    add_space()
  }

  #### Implement checks
  # Add '.R' to file name if necessary
  if (substr(file, nchar(file) - 1, nchar(file)) != ".R") file <- glue::glue("{file}.R")
  if (file.exists(file)) {
    if (overwrite) {
      file.remove(file)
    } else {
      rlang::abort(glue::glue("'{file}' already exists. Use `overwrite = TRUE` to overwrite the file."))
    }
  }

  #### Create file
  file.create(file)
  sink(file)

  ## Header
  add_hash_lines()
  add_title_section(basename(file))

  add_title_para("Aims")
  add_hash_num(1)
  add_space()

  add_title_para("Prerequisites")
  add_hash_num(1)
  add_hash_num(2)
  add_spaces()

  ## Set up section
  add_hash_lines()
  add_title_section("Set up")
  add_title_para("Wipe workspace")
  cat("rm(list = ls()) \n")
  cat('try(pacman::p_unload("all"), silent = TRUE) \n')
  cat("proj.templates::clear() \n")
  add_space()
  add_title_para("Essential packages")
  add_spaces()
  add_title_para("Load data")
  add_spaces()
  add_space()

  ## Sections
  add_section("Section one")
  add_section("Section two")

  ## End
  add_title_para("End of code.")
  add_hash_lines(end = TRUE)

  ## Close sink and open file
  sink()
  if (open && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(file)
  }
  invisible(NULL)
}


#' @title Use a template directory tree
#' @description This function lists and/or recreates the system of directories (termed the 'directory tree') in a project.
#' @param root A string that defines the project root (within which directories are identified or recreated).
#' @param tree A list of directories. If `NULL`, the directories in `root` (excluding `ignore` patterns) are recursively listed (see [`list.dirs()`]).
#' @param ignore (optional) If `tree = NULL`, `ignore` is a character vector of patterns that are ignored (see [`stringr::str_detect()`]). `NULL` suppresses this argument.
#' @param save (optional) A file path used to save the `tree` (see [`saveRDS()`]).
#' @param recreate A logical variable that defines whether or not to recreate the `tree` locally. If `TRUE`, any directories in `tree` that do not exist are created (see [`dir.create()`]).
#'
#' @details This function was motivated by the need to recreate project directories for RStudio Projects shared via GitHub. Typically, GitHub repositories do not contain all folders used by a project. This function is designed to be called to (a) list and save the list of directories used by a project locally such that (b) the directory system can be recreated on another machine. This is especially useful for stand-alone projects. For non-stand-alone projects, external files may need to be added into recreated directories for `R` code to run.
#'
#' The function uses a two-stage approach to list directories. First, the function lists all top-level directories, dropping any that contain an `ignore` pattern. For each remaining top-level directory, all sub-directories are recursively listed. Any sub-directories that contain an `ignore` pattern are dropped. This two-stage approach is typically quicker than listing all directories recursively in the first instance within RStudio Projects because the default ignored directories such as '.git' and 'renv' can contain many sub-directories.
#'
#' @examples
#' #### Set up example
#' # Create a blank project
#' proj <- file.path(tempdir(), "tmp")
#' usethis::create_project(proj, open = FALSE)
#' use_template_proj(proj)
#' # Add some additional files
#' dir.create(file.path(proj, "data", "a"))
#' dir.create(file.path(proj, "data", "b"))
#' dir.create(file.path(proj, "data", "ignore"))
#'
#' #### Example (1): Generating a tree for a project
#' # Generate tree using default options
#' (use_template_tree(root = proj))
#' # Ignore certain directories
#' (use_template_tree(root = proj, ignore = "ignore"))
#' # Save the tree
#' tree_path <- file.path(proj, "data", "inst", "tree.rds")
#' (use_template_tree(root = proj, save = tree_path))
#' file.exists(tree_path)
#'
#' #### Example (2): Recreate the tree in a new project
#' # Define a new (blank) project
#' new <- file.path(tempdir(), "new")
#' usethis::create_project(new, open = FALSE)
#' # Read the tree from the old project
#' tree <- readRDS(tree_path)
#' # Save and recreate the tree in the new project
#' dir.create(file.path(new, "data", "proj.templates"), recursive = TRUE)
#' success <- use_template_tree(
#'   root = new,
#'   tree = tree,
#'   save = file.path(new, "data", "proj.templates", "tree.rds"),
#'   recreate = TRUE
#' )
#' # The tree now exists in the new path
#' file.exists(file.path(new, "data", "proj.templates", "tree.rds"))
#' # Examine the logical vector that defines whether or not each tree element
#' # ... was created
#' success
#' # List directories in new project
#' list.dirs(new, full.names = FALSE, recursive = TRUE)
#'
#' @return The function returns either:
#' * A list of file path components, if `recreate = FALSE`
#' * A logical vector that defines whether or not each directory was successfully recreated, if `tree` is supplied and `recreate = TRUE`
#'
#' If `save` is supplied, the tree list is also written to file.
#'
#' @seealso [`use_template_proj()`], [`use_template_gitignore()`], [`use_template_readme()`], [`use_template_script()`], [`use_template_tree()`]
#' @author Edward Lavender
#' @export

use_template_tree <- function(root = rprojroot::find_rstudio_root_file(),
                              tree = NULL,
                              ignore = c(".git", ".Rproj.user", "renv", "docs", "deprecated"),
                              save = NULL,
                              recreate = ifelse(is.null(tree), FALSE, TRUE)) {
  #### Checks
  rlang::check_installed("stringr")
  if (is.null(tree) && recreate) {
    rlang::abort("`tree` is NULL and recreate is TRUE; either you want to list the directories in `root` or you want to recreate previously listed directories elsewhere.")
  }

  #### Define tree
  out <- NULL
  if (is.null(tree)) {
    # List 'top level directories'
    tree <- list.dirs(root, full.names = FALSE, recursive = FALSE)
    if (!is.null(ignore)) {
      # Drop any 'top level' directories to be ignored
      bool_ignore <- sapply(ignore, function(ig) stringr::str_detect(tree, ig))
      if (length(ignore) > 1) {
        bool_ignore <- apply(bool_ignore, 1, any)
      }
      if (any(bool_ignore)) {
        which_ignore <- which(bool_ignore)
        tree <- tree[-which_ignore]
      }
    }

    # List both top & sub-level directories and drop any to be ignored
    tree <- lapply(tree, function(f) list.dirs(path = file.path(root, f), recursive = TRUE))
    tree <- unlist(tree)
    tree <- substr(tree, nchar(root) + 2, nchar(tree))
    if (!is.null(ignore)) {
      bool_ignore <- sapply(ignore, function(ig) stringr::str_detect(tree, ig))
      if (length(ignore) > 1) {
        bool_ignore <- apply(bool_ignore, 1, any)
      }
      if (any(bool_ignore)) {
        which_ignore <- which(bool_ignore)
        tree <- tree[-which_ignore]
      }
    }
    # Split tree records by platform separator to facilitate recreation on a different system
    tree <- stringr::str_split(tree, .Platform$file.se)
  }
  if (!is.null(save)) saveRDS(tree, file = save)

  #### Recreate directory structure
  if (!recreate) {
    out <- tree
  } else {
    out <- lapply(tree, function(f) {
      success <- FALSE
      if (length(f) == 1) {
        con <- file.path(root, f)
      } else {
        con <- do.call(file.path, lapply(c(root, f), function(x) x))
      }
      if (!dir.exists(con)) {
        success <- dir.create(con)
      } else {
        message(glue::glue("Path '{con}' not overwritten."))
      }
      success
    }) |>
      unlist()
  }

  #### Return outputs
  invisible(out)
}
