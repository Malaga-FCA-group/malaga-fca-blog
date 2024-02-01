generate_posts_from_publications <- function(file,
                                             type = "journals") {

  lines <- readLines(con = file)

  idx <- lines |>
    stringr::str_detect("---") |>
    which()

  header <- lines[seq(idx[1], idx[2])]
  other <- lines[-seq(idx[2])]

  L <- yaml::yaml.load(header)

  post_folder <- here::here(
    "posts",
    L$slug)

  # if (fs::dir_exists(post_folder)) return(invisible(NULL))

  fs::dir_create(post_folder,
                 recurse = TRUE)

  # Find citation
  idx1 <- other |>
    stringr::str_detect("\\{\\{.*\\}\\}") |>
    which()

  # idx <- which(other == "# Citation")
  # if (length(idx) > 0 && idx > 1) {
  #
  #   main <- other[seq(idx - 1)]
  #
  # } else main <- other

  idx <- other |>
    stringr::str_detect("# Abstract") |>
    which()
  idx_final <- min(idx1[idx1 > idx]) - 1

  # idx_final <- min(idx2[idx2 > idx]) - 1

  abstract <- other[seq(idx + 1, idx_final)] |>
    paste0(collapse = "\n")

  my_title <- switch(tolower(type),
    "journals" = "New journal paper: <em>{L$title}</em>",
    "conferences" = "Conference paper accepted: <em>{L$title}</em>"
  )

  post_header <- list(
    title = glue::glue(my_title),
    author = L$author,
    date = L$date,
    categories = L$categories,
    comments = list(giscus = list(repo = "quarto-dev/quarto-docs"))
  )

  YAML <- yaml::as.yaml(post_header)

  # path <- "/publications/{type}/{L$slug}"

  body <- c(
    "The work <u>{L$title}</u> has been published in <em>{L$details}</em>.",
    "",
    "<u>Abstract</u>:",
    "",
    "{abstract}",
    "",
    "For more details on this work, visit <a href='/publications/{type}/{L$slug}'>its own page</a>."
  ) |>
    stringr::str_flatten("\n") |>
    glue::glue()
  # other <- "The work <u>{L$title}</u> has been published in <em>{L$details}</em>.\n\n<u>Abstract</u>:\n\n{abstract}" |>
  #   glue::glue()


  newfile <- file.path(post_folder,
                       "index.qmd")

  cat("---\n", file = newfile)
  cat(YAML, file = newfile,
      append = TRUE)
  cat("---\n\n", file = newfile,
      append = TRUE)
  cat(body, file = newfile,  sep = "\n",
      append = TRUE)

}

folder <- here::here("publications")
journals <- list.files(
  path = file.path(folder, "journals"),
  pattern = "index.preqmd$",
  full.names = TRUE,
  recursive = TRUE)

journals |> sapply(\(f) generate_posts_from_publications(f, "journals"))

conferences <- list.files(
  path = file.path(folder, "conferences"),
  pattern = "index.preqmd$",
  full.names = TRUE,
  recursive = TRUE)

conferences |> sapply(\(f) generate_posts_from_publications(f, "conferences"))
