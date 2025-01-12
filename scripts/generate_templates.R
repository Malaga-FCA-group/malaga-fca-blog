generate_publication_preqmd <- function(
    type = "journals",
    author,
    date,
    title,
    categories,
    header_image = NULL,
    details,
    slug,
    doi = NULL,
    project,
    keywords,
    abstract) {
  author <- author |>
    stringr::str_split_1("\\s*,\\s*") |>
    mask_authors()

  authors <- glue::glue(
    "  - {author}",
    .trim = FALSE
  ) |>
    stringr::str_flatten("\n")
  if (is.null(project)) {
    projects <- "[]"
  } else {
    projects <- glue::glue(
      "  - {project}",
      .trim = FALSE
    ) |>
      stringr::str_flatten("\n")

    projects <- paste0("\n", projects)
  }

  categories <- categories |>
    stringr::str_split_1("\\s*,\\s*") |>
    stringr::str_to_sentence() |>
    stringr::str_flatten_comma()

  if (missing(slug)) {
    slug <- c(
      date |>
        lubridate::year(),
      "-",
      author |>
        stringr::str_split_1("\\s*,\\s*") |>
        stringr::str_extract_all("[A-Z]") |>
        lapply(\(x) paste0(stringr::str_flatten(x), "-")) |>
        unlist() |>
        stringr::str_flatten(),
      title |>
        stringr::str_extract("[a-zA-Z]*")
    ) |>
      stringr::str_flatten()
  }

  str <- glue::glue(
    "- title: '{title}'\n",
    "  author: \n{authors}\n",
    "  date: {date}\n",
    "  categories: [{categories}]\n",
    # "  header_image: {header_image}\n",
    "  doi: {doi}\n",
    "  slug: {slug}\n",
    "  keywords: '{keywords}'\n",
    "  project: {projects}\n",
    "  details: {details}",
    .trim = FALSE
  )

  new_file <- here::here(
    "publications",
    type,
    slug,
    "index.preqmd"
  )

  fs::dir_create(here::here(
    "publications",
    type,
    slug
  ))

  cat("---\n", file = new_file)
  cat(str, file = new_file, append = TRUE)
  cat("---\n\n", file = new_file, append = TRUE)
  cat("\n\n# Abstract\n\n", file = new_file, append = TRUE)
  cat(abstract, file = new_file, append = TRUE)
  cat("\n{{funding}}\n\n{{citation_history}}\n\n{{citation}}",
    file = new_file, append = TRUE
  )

  return(invisible(NULL))
}


generate_publication_preqmd_text <- function(
    type = "journals",
    author,
    date,
    title,
    categories,
    header_image = NULL,
    details,
    slug,
    doi = NULL,
    project,
    keywords,
    abstract) {
  title <- title |>
    stringr::str_remove_all("\n") |>
    stringr::str_replace_all("\\s+", " ")

  author <- author |>
    stringr::str_split_1("\\s*,\\s*") |>
    mask_authors()

  authors <- glue::glue(
    "  - {author}",
    .trim = FALSE
  ) |>
    stringr::str_flatten("\n")
  if (is.null(project)) {
    projects <- "[]"
  } else {
    projects <- glue::glue(
      "  - {project}",
      .trim = FALSE
    ) |>
      stringr::str_flatten("\n")

    projects <- paste0("\n", projects)
  }

  # categories <- categories |>
  #   stringr::str_split_1("\\s*,\\s*") |>
  #   stringr::str_to_sentence() |>
  #   stringr::str_flatten_comma()

  if (missing(slug)) {
    slug <- c(
      date |>
        lubridate::year(),
      "-",
      author |>
        stringr::str_split_1("\\s*,\\s*") |>
        stringr::str_extract_all("[A-Z]") |>
        lapply(\(x) paste0(stringr::str_flatten(x), "-")) |>
        unlist() |>
        stringr::str_flatten(),
      title |>
        stringr::str_extract("[a-zA-Z]*")
    ) |>
      stringr::str_flatten()
  }

  res <- list(
    title = title,
    author = author,
    date = date,
    categories = categories,
    # doi = doi,
    slug = slug,
    # keywords = keywords,
    # project = project,
    details = details
  )

  if (!is.null(doi)) res$doi <- doi
  if (!is.null(project)) res$project <- project
  if (!is.null(keywords)) res$keywords <- keywords

  # str <- glue::glue(
  #   "title: '{title}'\n",
  #   "author: \n{authors}\n",
  #   "date: '{date}'\n",
  #   "categories: [{categories}]\n",
  #   # "  header_image: {header_image}\n",
  #   "doi: {doi}\n",
  #   "slug: {slug}\n",
  #   "keywords: '{keywords}'\n",
  #   "project: {projects}\n",
  #   "details: '{details}'",
  #   .trim = FALSE
  # )

  str <- yaml::as.yaml(res)
  new_file <- here::here(
    "publications",
    type,
    slug,
    "index.preqmd"
  )

  fs::dir_create(here::here(
    "publications",
    type,
    slug
  ))

  str <- c(
    "---",
    str,
    "---", "", "",
    "# Abstract", "", "", abstract, "", "",
    "{{funding}}", "", "",
    "{{prepare}}", "", "",
    "{{citation}}", "", "",
    "{{citation_history}}", "", "",
    "{{citations}}", "", ""
  ) |>
    stringr::str_flatten("\n")

  return(invisible(str))
}
