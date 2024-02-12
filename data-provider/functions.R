generate_publication_preqmd <- function(
    type = "journals",
    author,
    date,
    title,
    categories,
    header_image = NULL,
    details,
    doi = NULL,
    project,
    keywords,
    abstract) {

  author <- author |>
    stringr::str_split_1("\\s*,\\s*")

  authors <- glue::glue(
    "  - {author}",
    .trim = FALSE
  ) |>
    stringr::str_flatten("\n")

  if (is.null(project)) {

    projects <- "~"

  } else {

    projects <- glue::glue(
      "  - {project}",
      .trim = FALSE
    ) |>
      stringr::str_flatten("\n")
    projects <- glue::glue(
      "\n{projects}\n",
      .trim = FALSE
    )

  }

  categories <- categories |>
    stringr::str_flatten_comma()

  slug <- c(lubridate::year(date),
            "-",
            author |>
              stringr::str_extract_all("[A-Z]") |>
              lapply(\(x) paste0(stringr::str_flatten(x), "-")) |>
              unlist() |>
              stringr::str_flatten(),
            title |>
              stringr::str_extract("[a-zA-Z]*")) |>
    stringr::str_flatten()

  assign("slug", slug, envir = globalenv())

  if (is.null(header_image)) header_image <- "~"

  if (is.null(keywords)) keywords <- ""

  str <- glue::glue(
    "title: '{title}'\n",
    "type: {type}\n",
    "author: \n{authors}\n",
    "date: '{date}'\n",
    "categories: [{categories}]\n",
    "header_image: {header_image}\n",
    "doi: {doi}\n",
    "slug: {slug}\n",
    "keywords: '{keywords}'\n",
    "project: {projects}\n",
    "details: {details}\n",
    .trim = FALSE
  )

  new_file <- file.path(
    tempdir(),
    "index.preqmd"
  )

  cat("---\n", file = new_file)
  cat(str, file = new_file, append = TRUE)
  cat("---\n\n", file = new_file, append = TRUE)
  cat("\n\n# Abstract\n\n", file = new_file, append = TRUE)
  cat(abstract, file = new_file, append = TRUE)
  cat("\n\n{{funding}}\n\n{{citation_history}}\n\n{{citation}}",
      file = new_file, append = TRUE)

  return(list(file = new_file, slug = slug))

}

# Generate preqmd for projects
generate_project_preqmd <- function(
    author,
    start_date,
    end_date,
    title,
    reference,
    members,
    collaborators,
    categories,
    header_image = NULL,
    path,
    abstract) {

  author_v <- author |>
    stringr::str_split_1("\\s*,\\s*")

  authors <- glue::glue(
    "  - {author_v}",
    .trim = FALSE
  ) |>
    stringr::str_flatten("\n")

  categories <- categories |>
    stringr::str_flatten_comma()

  slug <- c(
    lubridate::year(start_date),
    "-",
    title |>
      stringr::str_extract("[a-zA-Z]*")
  ) |>
    stringr::str_flatten()

  assign("slug", slug, envir = globalenv())

  if (is.null(header_image))
    header_image <- "~"

  str <- glue::glue(
    "title: '{title}'\n",
    "type: project\n",
    "author: \n{authors}\n",
    "date: {start_date}\n",
    "end_date: {end_date}\n",
    "categories: [{categories}]\n",
    "header_image: {header_image}\n",
    "slug: {slug}\n",
    "code: {reference}\n",
    .trim = FALSE
  )

  new_file <- file.path(
    tempdir(),
    "index.preqmd"
  )

  cat("---\n", file = new_file)
  cat(str, file = new_file, append = TRUE)
  cat("---\n\n", file = new_file, append = TRUE)
  cat("\n\n# Abstract\n\n", file = new_file, append = TRUE)
  cat(abstract, file = new_file,
      append = TRUE)

  cat(
    glue::glue("\n\n# Members\n\n**Principal investigator(s)**: {author}.\n\n", .trim = FALSE),
    file = new_file,
    append = TRUE)

  if (stringr::str_length(members) > 0) {

    cat(
      glue::glue("**Investigator(s)**: {members}.\n\n", .trim = FALSE),
      file = new_file,
      append = TRUE)

  }

  if (stringr::str_length(collaborators) > 0) {

    cat(
      glue::glue("**Collaborator(s)**: {collaborators}.\n\n", .trim = FALSE),
      file = new_file,
      append = TRUE)

  }

  cat(glue::glue("# How to acknowledge\n\nPlease, use the reference {reference} in all published works funded by this project.\n\n", .trim = FALSE),
      file = new_file,
      append = TRUE)

  cat("\n\n{{published_works}}\n\n",
      file = new_file, append = TRUE)

  return(list(file = new_file, slug = slug))

}


