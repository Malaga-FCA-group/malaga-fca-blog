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
  projects <- glue::glue(
    "  - {project}",
    .trim = FALSE
  ) |>
    stringr::str_flatten("\n")

  categories <- categories |>
    stringr::str_split_1("\\s*,\\s*") |>
    stringr::str_to_sentence() |>
    stringr::str_flatten_comma()

  slug <- c(date |>
              lubridate::year(),
            "-",
            author |>
              stringr::str_split_1("\\s*,\\s*") |>
              stringr::str_extract_all("[A-Z]") |>
              lapply(\(x) paste0(stringr::str_flatten(x), "-")) |>
              unlist() |>
              stringr::str_flatten(),
            title |>
              stringr::str_extract("[a-zA-Z]*")) |>
    stringr::str_flatten()


  str <- glue::glue(
      "- title: '{title}'\n",
      "  author: \n{authors}\n",
      "  date: {date}\n",
      "  categories: [{categories}]\n",
      "  header_image: {header_image}\n",
      "  doi: {doi}\n",
      "  slug: {slug}\n",
      "  keywords: '{keywords}'\n",
      "  project: \n{projects}\n",
      "  details: {details}",
      .trim = FALSE
    )

  new_file <- here::here("publications",
                         type,
                         slug,
                         "index.preqmd")

  fs::dir_create(here::here("publications",
                            type,
                            slug))

  cat("---\n", file = new_file)
  cat(str, file = new_file, append = TRUE)
  cat("---\n\n", file = new_file, append = TRUE)
  cat("\n\n# Abstract\n\n", file = new_file, append = TRUE)
  cat(abstract, file = new_file, append = TRUE)
  cat("\n{{funding}}\n\n{{citation_history}}\n\n{{citation}}",
      file = new_file, append = TRUE)

  return(invisible(NULL))

}

