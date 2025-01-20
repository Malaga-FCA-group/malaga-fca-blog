titles_orig <- get_publication_value(here::here("publications"), "title") |>
  stringr::str_remove_all("(\\{|\\})")
titles <- titles_orig |>
  sapply(\(x) gsub("[^a-z\\sA-Z0-9_-]", "", x)) |> sapply(tolower)
authors_orig <- get_publication_value(here::here("publications"), "author")  |>
  sapply(stringr::str_flatten_comma) |> sapply(tolower)
authors <- authors_orig
dates_orig <- get_publication_value(here::here("publications"), "date") |>
  sapply(\(x) ifelse(stringr::str_length(x) == 4, x, lubridate::year(x)))
dates <- dates_orig
full <- cbind(titles, authors, dates) |>
  as.data.frame() |>
  dplyr::mutate(full = paste0(dates, titles, authors)) |>
  dplyr::pull(full)

slugs_orig <- get_publication_value(here::here("publications"), "slug")

hashes <- titles |> sapply(\(x) digest::digest(x, algo = "md5"))
idx <- which(duplicated(hashes))

for (i in idx) {

  dup <- hashes[i]
  orig <- which(hashes == dup)[1]

  titulo1 <- titles_orig[orig]
  autor1 <- authors_orig[orig]
  fecha1 <- dates_orig[orig]
  slug1 <- slugs_orig[orig]

  cita1 <- glue::glue("{slug1}: {autor1}. '{titulo1}' ({fecha1})")

  titulo2 <- titles_orig[i]
  autor2 <- authors_orig[i]
  fecha2 <- dates_orig[i]
  slug2 <- slugs_orig[i]

  cita2 <- glue::glue("{slug2}: {autor2}. '{titulo2}' ({fecha2})")

  cli::cli_alert_info("Posible duplicado:")
  cli::cli_alert_warning(cita1)
  cli::cli_alert_warning(cita2)

}

works_from_dois <- oa_fetch(
  entity = "works",
  doi = c("10.1016/J.FSS.2021.04.023"),
  verbose = TRUE
)


authors_from_orcids <- oa_fetch(
  entity = "authors",
  orcid = c("https://orcid.org/0000-0002-0172-1585")
)
