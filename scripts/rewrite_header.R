folder <- here::here("publications")
journals <- list.files(
  path = file.path(folder, "journals"),
  pattern = "*.qmd$",
  full.names = TRUE,
  recursive = TRUE)
conferences <- list.files(
  path = file.path(folder, "conferences"),
  pattern = "*.qmd$",
  full.names = TRUE,
  recursive = TRUE)
books <- list.files(
  path = file.path(folder, "books"),
  pattern = "*.qmd$",
  full.names = TRUE,
  recursive = TRUE)

files <- c(journals,
           conferences,
           books)

rewrite_yaml <- function(file) {

  lines <- readLines(con = file)

  idx <- lines |>
    stringr::str_detect("---") |>
    which()

  header <- lines[seq(idx[1], idx[2])]
  other <- lines[-seq(idx[2])]

  L <- yaml::yaml.load(header)

  L$howtocite <- NULL
  L$bibtex <- NULL
  L$abstract <- NULL
  YAML <- yaml::as.yaml(L)

  fs::file_move(file, paste0(file, "_old"))

  # Find citation
  idx <- which(other == "# Citation")
  if (length(idx) > 0 && idx > 1) {

    main <- other[seq(idx - 1)]

    final_citation <- readLines(
      con = here::here(
        "scripts",
        "final_citation.qmd_txt"
      )
    )

    cat("---\n", file = file)
    cat(YAML, file = file,
        append = TRUE)
    cat("---\n\n", file = file,
        append = TRUE)
    cat(main, file = file, sep = "\n",
        append = TRUE)
    cat(final_citation, file = file, sep = "\n",
        append = TRUE)

  } else {

    cat("---\n", file = file)
    cat(YAML, file = file,
        append = TRUE)
    cat("---\n\n", file = file,
        append = TRUE)
    cat(other, file = file,  sep = "\n",
        append = TRUE)


  }

  return(invisible(NULL))

}

conferences |> sapply(rewrite_yaml)

change <- function(file) {

  lines <- readLines(con = file)

  lines <- lines |>
    stringr::str_replace_all("'journals',",
                             "'conferences',")

  cat(lines, file = file, sep = "\n")

}

conferences |> lapply(change)
