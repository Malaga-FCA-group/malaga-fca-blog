## Migración

extract_year <- function(input_string) {
  # Usa una expresión regular para extraer los primeros 4 dígitos (el año)
  year <- sub("^([0-9]{4})-.*$", "\\1", input_string)
  return(year)

migrate <- function(input_file) {
  folder <- dirname(input_file)
  basefolder <- dirname(folder)
  type <- basename(basefolder)
  current_slug <- basename(folder)
  cli::cli_alert_info("Processing {.val {current_slug}}")

  year <- extract_year(current_slug)

  preqmd <- read_qmd(input_file)
  header <- preqmd$contents
  title <- header$title

  slug2 <- generate_acronym(title)
  header$slug <- file.path(year, slug2)

  header$author <- header$author |> mask_authors()

  body <- preqmd$other |>
    stringr::str_remove_all(stringr::fixed("{{citation_history}}")) |>
    stringr::str_remove_all(stringr::fixed("{{citation}}"))
  body <- c(
    body,
    "{{prepare}}", "", "",
    "{{citation}}", "", "",
    "{{citation_history}}", "", "",
    "{{citations}}", "", ""
  )

  new_folder <- file.path(basefolder, year, slug2)

  if (fs::dir_exists(new_folder)) {
    return(invisible())
  }
  fs::dir_create(new_folder, recurse = TRUE)

  preqmd <- list(contents = header, other = body)

  other_files <- fs::dir_ls(folder,
    regexp = "([.]bib$|[.]jpg$|[.]pdf$|[.]png$|[.]jpeg$)"
  )

  write_qmd(preqmd,
    file = file.path(new_folder, "index.preqmd")
  )

  cli::cli_alert_info("  Copied 'index.preqmd'")


  fs::file_copy(other_files, new_folder)
  cli::cli_alert_info("  Copied {.val {basename(other_files)}}")


  cli::cli_alert_success("  Created {.val {header$slug}}")
}

# all_files <- fs::dir_ls(
#   path = here::here("publications"),
#   regexp = "[.]preqmd$",
#   recurse = TRUE
# )

# all_files |> lapply(migrate)
