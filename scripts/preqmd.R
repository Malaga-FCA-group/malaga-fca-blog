process_preqmd <- function(file) {

  lines <- readLines(con = file)

  idx <- lines |>
    stringr::str_detect("---") |>
    which()

  header <- lines[seq(idx[1], idx[2])] |>
    yaml::yaml.load()

  pattern <- "\\{\\{.*\\}\\}"

  idx <- lines |>
    stringr::str_detect(pattern) |>
    which()

  if (length(idx) > 0) {

    my_funs <- lines[idx] |>
      stringr::str_extract_all(pattern) |>
      stringr::str_remove_all("[\\{|\\}]")

    for (i in seq_along(idx)) {

      lines[idx[i]] <- funs[[my_funs[i]]](header)

    }

  }

  newfile <- stringr::str_replace(file,
                       pattern = "preqmd$",
                       "qmd")

  cat(lines, file = newfile, sep = "\n")

}

# Auxiliary functions
# Must return a single string (correctly formatted in HTML or LaTeX)
# and may include linebreaks by using \n
papers_of_project <- function(header) {

  project_id <- header$slug

  project_folder <- here::here(
    "projects",
    project_id)

  pub_folder <- here::here("publications")
  files <- list.files(
    path = pub_folder,
    pattern = "index.qmd$",
    full.names = TRUE,
    recursive = TRUE
  )

  pubs <- list()

  for (file in files) {

    lines <- readLines(con = file)

    type <- basename(dirname(dirname(file)))

    idx <- which(lines |> stringr::str_detect("---"))

    header <- lines[seq(idx[1], idx[2])]

    contents <- yaml::yaml.load(header)

    if (!is.null(contents$project) &&
        tolower(contents$project) == tolower(project_id)) {

      contents$path <- glue::glue("/publications/{type}/{contents$slug}")
      pubs <- c(pubs,
                list(list(
                  title = contents$title,
                  author = contents$author,
                  date = contents$date,
                  description = contents$details,
                  path = contents$path)))

    }

  }

  newfile <- here::here(
    project_folder,
    paste0("published_works.yml")
  )

  if (length(pubs) > 0) {

    yaml::write_yaml(pubs, file = newfile)

    str <- "\n# Published works\n\n:::{#published_works}\n:::\n"
    return(str)

  } else {

    cat("", file = newfile)
    return("")

  }

}


funs <- list(
  published_works = papers_of_project
)
