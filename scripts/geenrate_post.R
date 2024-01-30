files <- list.files(
  path = folder,
  pattern = "index.qmd$",
  full.names = TRUE,
  recursive = TRUE)

generate_posts_from_publications <- function(file) {

  lines <- readLines(con = file)

  idx <- lines |>
    stringr::str_detect("---") |>
    which()

  header <- lines[seq(idx[1], idx[2])]
  other <- lines[-seq(idx[2])]

  L <- yaml::yaml.load(header)

  # fs::file_move(file, paste0(file, "_old"))

  # Find citation
  idx <- which(other == "# Citation")
  if (length(idx) > 0 && idx > 1) {

    main <- other[seq(idx - 1)]

  } else main <- other

  idx <- main |>
    stringr::str_detect("# Abstract") |>
    which()
  idx2 <- main |>
    stringr::str_detect("(#|(```))") |>
    which()

  idx_final <- min(idx2[idx2 > idx]) - 1

  abstract <- main[seq(idx+1, idx_final)] |>
    paste0(collapse = "\n")

  post_header <- list(
    title = glue::glue("Publication of <em>{L$title}</em>"),
    author = L$author,
    date = L$date,
    categories = L$categories
  )

  YAML <- yaml::as.yaml(post_header)

  other <- "The work <u>{L$title}</u> has been published in <em>{L$details}</em>.\n\n<u>Abstract</u>:\n\n{abstract}" |>
    glue::glue()

  post_folder <- here::here(
    "posts",
    L$slug)
  fs::dir_create(post_folder,
                 recurse = TRUE)
  newfile <- file.path(post_folder,
                       "index.qmd")

  cat("---\n", file = newfile)
  cat(YAML, file = newfile,
      append = TRUE)
  cat("---\n\n", file = newfile,
      append = TRUE)
  cat(other, file = newfile,  sep = "\n",
      append = TRUE)

}

files |> sapply(generate_posts_from_publications)
