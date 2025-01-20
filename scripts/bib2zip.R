source(here::here("scripts", "generate_templates.R"))
source(here::here("scripts", "qmd_utils.R"))

generate_acronym <- function(input_string) {
  # Verifica que los paquetes necesarios estén instalados
  if (!requireNamespace("tm", quietly = TRUE)) {
    stop("El paquete 'tm' no está instalado. Instálalo con install.packages('tm').")
  }

  # Divide la cadena en palabras
  words <- strsplit(input_string, "(\\s|\\-)")[[1]]

  # Elimina palabras que no son alfabéticas
  words <- words[grepl("^[a-zA-Z]+$", words)]

  # Convierte a minúsculas para comparar con la lista de stopwords
  words <- tolower(words)

  # Elimina stopwords según el idioma detectado
  stopwords_list <- c(tm::stopwords("en"), tm::stopwords("es"))
  words <- words[!words %in% stopwords_list]

  # Toma las dos primeras letras de cada palabra, ajusta mayúsculas y minúsculas
  acronym_parts <- sapply(words, function(word) {
    if (nchar(word) >= 2) {
      # Toma las dos primeras letras y ajusta el caso
      paste0(toupper(substr(word, 1, 1)), tolower(substr(word, 2, 2)))
    } else if (nchar(word) == 1) {
      # Maneja palabras de un solo carácter
      toupper(substr(word, 1, 1))
    } else {
      ""
    }
  })

  # Concatena todas las partes y devuelve el resultado
  paste(acronym_parts, collapse = "")
}

# # Ejemplo de uso
# cadena <- "Generar una Función en R"
# acronym <- generate_acronym(cadena)
# cat("Acrónimo generado:", acronym, "\n")

generate_short_hash <- function(input_string, length = 5) {
  # Verifica que el paquete digest esté instalado
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("El paquete 'digest' no está instalado. Instálalo con install.packages('digest').")
  }

  # Genera un hash MD5 y toma los primeros caracteres
  hash <- substr(digest::digest(input_string, algo = "md5"), 1, length)

  # Asegúrate de que el hash sea apto para nombres de archivo (sin caracteres inválidos)
  # Reemplaza cualquier carácter no válido
  gsub("[^a-zA-Z0-9_-]", "_", hash)
}


bib2zip <- function(bibori) {
  bib <- unlist(bibori)
  authors <- bib$author |>
    sapply(\(s) glue::glue("{stringr::str_flatten(s$given, collapse = ' ')} {stringr::str_flatten(s$family, collapse = ' ')}")) |>
    stringr::str_replace_all("\\\\'\\\\i", "í")
  title <- bib$title
  journal <- bib$journal
  booktitle <- bib$booktitle
  date <- bib$year
  # if (!is.null(bib$dateobj)) date <- bib$dateobj
  doi <- bib$doi
  volume <- bib$volume
  issue <- bib$issue
  if (!is.null(bib$number)) issue <- bib$number
  pages <- bib$pages
  str_pages <- glue::glue(
    ", pages {pages}"
  )
  if (!is.null(bib[["article-number"]])) {
    pages <- bib[["article-number"]]
    str_pages <- glue::glue(
      ", article number {pages}"
    )
  }
  keywords <- bib$keywords
  if (!is.null(bib$abstract)) {
    abstract <- bib$abstract
  } else {
    if (!is.null(doi)) {
      works_from_dois <- openalexR::oa_fetch(
        entity = "works",
        doi = doi,
        verbose = FALSE
      )

      abstract <- works_from_dois$abstract
    } else {
      abstract <- NULL
    }
  }

  series <- bib$series

  # browser()

  type <- switch(tolower(bib$bibtype),
    "article" = "journals",
    "inproceedings" = "conferences",
    "proceedings" = "books",
    "inbook" = "books",
    "incollection" = "books"
  )

  str_issue <- ifelse(
    is.null(issue),
    "",
    glue::glue("({issue})")
  )
  str_vol <- ifelse(
    is.null(volume),
    "",
    glue::glue("vol. {volume}")
  )

  str_series <- ifelse(
    is.null(series),
    "",
    glue::glue(", {series} {str_vol}")
  )

  details <- switch(type,
    journals = glue::glue(
      "{journal} {str_vol} {str_issue}{str_pages}."
    ),
    conferences = glue::glue(
      "{booktitle} {str_series}{str_pages}."
    ),
    books = glue::glue(
      "{booktitle} {str_series}{str_pages}."
    )
  )

  author <- authors |>
    stringr::str_flatten_comma()

  # slug <- generate_short_hash(paste0(date, title, author))
  slug <- generate_acronym(title)

  new_folder <- here::here("publications", type, date, slug)

  if (dir.exists(new_folder)) {
    warning("This paper was already on the site.",
      immediate. = TRUE
    )
    print(bibori)

    return(invisible(NULL))
  }

  dir.create(folder, showWarnings = FALSE, recursive = TRUE)

  # slug <- c(
  #   date,
  #   "-",
  #   indices + 1
  # ) |>
  #   stringr::str_flatten()

  # filename <- paste(slug, ".zip", sep = "")

  doi <- doi |>
    stringr::str_remove_all(stringr::fixed("https://doi.org/"))

  content <- generate_publication_preqmd_text(
    type = type,
    author = author,
    date = date,
    title = title,
    details = details,
    project = NULL,
    categories = "uncategorised",
    slug = file.path(date, slug),
    doi = doi,
    keywords = keywords,
    abstract = abstract
  )

  # slug <- L$slug

  # new_folder <- file.path(folder, slug)
  # fs::dir_create(new_folder)

  my_bib <- file.path(new_folder, "cite.bib")
  RefManageR::WriteBib(bibori, file = my_bib)

  cat(content, file = file.path(new_folder, "index.preqmd"))

  return(invisible(NULL))
}


process_bibtex <- function(bibtex_file) {
  allbib <- RefManageR::ReadBib(
    bibtex_file
  )

  lapply(allbib, bib2zip) |> invisible()
}

## Migración

extract_year <- function(input_string) {
  # Usa una expresión regular para extraer los primeros 4 dígitos (el año)
  year <- sub("^([0-9]{4})-.*$", "\\1", input_string)
  return(year)
}

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
