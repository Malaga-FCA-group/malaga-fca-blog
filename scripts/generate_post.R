generate_posts_from_publications <- function(
    file,
    type = "journals",
    force = FALSE) {

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

  if (!force && fs::dir_exists(post_folder)) return(invisible(NULL))

  fs::dir_create(post_folder,
                 recurse = TRUE)

  # Find other section
  idx1 <- other |>
    stringr::str_detect("(#|\\{\\{.*\\}\\})") |>
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
                     "conferences" = "Conference paper accepted: <em>{L$title}</em>",
                     "books" = "New publication: <em>{L$title}</em>",
                     "projects" = "Start of project <em>{L$title}</em>"
  )

  my_image <- glue::glue("/assets/images/{type}.jpg")

  if (type == "projects") {

    my_image <- glue::glue(
      "/projects/{L$slug}/{L$image}"
    )
  }

  post_header <- list(
    title = glue::glue(my_title),
    image = my_image,
    author = L$author,
    date = L$date,
    categories = L$categories,
    # citation = TRUE,
    comments = list(giscus = list(
      repo = "malaga-fca-group/malaga-fca-blog"))
  )

  YAML <- yaml::as.yaml(
    post_header,
    handlers = list(
      logical = function(x) {
        result <- ifelse(x, "true", "false")
        class(result) <- "verbatim"
        return(result)
      }
    ))

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

journals |> sapply(
  \(f)
  generate_posts_from_publications(f, "journals"))

conferences <- list.files(
  path = file.path(folder, "conferences"),
  pattern = "index.preqmd$",
  full.names = TRUE,
  recursive = TRUE)

conferences |> sapply(
  \(f)
  generate_posts_from_publications(f, "conferences"))

books <- list.files(
  path = file.path(folder, "books"),
  pattern = "index.preqmd$",
  full.names = TRUE,
  recursive = TRUE)

books |> sapply(
  \(f)
  generate_posts_from_publications(f, "books"))

projects <- list.files(
  path = here::here("projects"),
  pattern = "index.preqmd$",
  full.names = TRUE,
  recursive = TRUE)

projects |> sapply(
  \(f)
  generate_posts_from_publications(f, "projects"))
