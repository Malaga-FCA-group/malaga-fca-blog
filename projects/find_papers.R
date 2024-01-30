find_papers <- function(
    base_folder = here::here("publications"),
    project_folder = ".") {

  library(RefManageR)

  files <- list.files(
    path = base_folder,
    pattern = "*.qmd$",
    full.names = TRUE,
    recursive = TRUE
  )

  project_folder <- normalizePath(project_folder)
  project <- basename(project_folder)

  res <- list()
  result <- c()
  s <- NULL
  URLs <- c()

  for (file in files) {

    lines <- readLines(con = file)

    idx <- which(lines |> stringr::str_detect("---"))

    header <- lines[seq(idx[1], idx[2])]

    contents <- yaml::yaml.load(header)
    contents$rel_path <- make_path_relative(project_folder, dirname(file))

    if (!is.null(contents$project) && (tolower(contents$project) == tolower(project))) {

      bibfile <- file.path(contents$rel_path, "cite.bib")

      if (file.exists(bibfile)) {

        if (is.null(s)) {

          s <- ReadBib(bibfile)

        } else {

          s <- s + ReadBib(bibfile)

        }

        URLs <- c(URLs, contents$rel_path)

      }
      res <- c(res, list(contents))

    }

  }

  if (length(s) > 0) {

    NoCite(s)

    for (i in seq_along(s)) {

      txt <- capture.output(PrintBibliography(s[[i]], .opts = list(bib.style = "alphabetic", style = "text"))) |> stringr::str_flatten(" ")

      txt <- txt |>
        stringr::str_remove_all("^\\[.+\\]\\s")

      result <- c(result, glue::glue(
        "<li><a href='{URLs[i]}'>{txt}</a></li>"
      ))


    }

    result <- c("<ol>", result, "</ol>")
    # result <- capture.output(PrintBibliography(s, .opts = list(bib.style = "alphabetic", style = "text")))


    # yaml::write_yaml(res, file = "./related_papers.yml")

  }

  return(result)

}

make_path_relative <- function(this_path, target_path) {

  this_path <- normalizePath(this_path)
  target_path <- normalizePath(target_path)

  rel_path <- ""
  repeat {

    if (this_path == "/") {

      rel_path <- target_path
      break

    }

    if (stringr::str_detect(target_path,
                            paste0("^", this_path))) {

      final_path <- stringr::str_remove_all(
        target_path,
        paste0("^", this_path))

      rel_path <- file.path(rel_path, final_path)

      break

    } else {

      if (rel_path == "") {

        rel_path <- ".."

      } else {

        rel_path <- file.path(rel_path, "..")

      }
      this_path <- dirname(this_path)

    }

  }

  return(rel_path)

}
