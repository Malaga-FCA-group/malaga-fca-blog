##%######################################################%##
#                                                          #
####              Obtain publication data               ####
#                                                          #
##%######################################################%##


# SCHOLAR
library(scholar)
library(tidyverse)
source(here::here("scripts",
                  "manage_publications.R"))
source(here::here("scripts",
                  "manage_citations.R"))
source(here::here("scripts",
                  "connections.R"))


scholar_id <- "X24XQHIAAAAJ"
orcid_id <- "0000-0002-0172-1585"

# Obtain all publications from Scholar
pubs <- scholar::get_publications(scholar_id)  |>
  dplyr::mutate(author = author |>
                  as.character() |>
                  stringr::str_trim(),
                journal = journal |>
                  replace(journal %in% "bioRxiv", "BioRxiv"),
                preprint = dplyr::case_when(journal %in% c("bioRxiv", "BioRxiv") ~ TRUE,
                                            TRUE ~ FALSE)) |>
  dplyr::arrange(desc(year))

# Compare with my listed publications
pubtitles <- c(
  get_publication_value(
    folder = here::here("publications",
                        "journals"),
    value = "title"),
  get_publication_value(
    folder = here::here("publications",
                        "conferences"),
    value = "title"),
  get_publication_value(
    folder = here::here("publications",
                        "books"),
    value = "title"))

pubyear <- c(
  get_publication_year(
    folder = here::here("publications",
                        "journals")),
  get_publication_year(
    folder = here::here("publications",
                        "conferences")),
  get_publication_year(
    folder = here::here("publications",
                        "books")))

pubslugs <- c(
  paste0("journals/", get_publication_value(
    folder = here::here("publications",
                        "journals"),
    value = "slug")),
  paste0("conferences/", get_publication_value(
    folder = here::here("publications",
                        "conferences"),
    value = "slug")),
  paste0("books/", get_publication_value(
    folder = here::here("publications",
                        "books"),
    value = "slug")))
order <- stringdist::amatch(
  tolower(pubs$title),
  table = tolower(pubtitles),
  maxDist = 2)

# Extended information
pubs2 <- pubs |>
  mutate(order = order,
         slug = pubslugs[order],
         type = stringr::str_split_i(slug, pattern = "/", 1),
         year = pubyear[order]) |>
  filter(!is.na(order)) |>
  arrange(order)

# Obtain citation history for papers
# First, clean up

list.files(path = here::here(
  "publications"),
  pattern = "citation_history.rds",
  full.names = TRUE,
  recursive = TRUE) |>
  fs::file_delete()

# cites
all_cites <- vector(mode = "integer",
                    length = nrow(pubs2))
citations_year <- c()
for (i in seq(nrow(pubs2))) {

  publication <- pubs2[i, ]

  Sys.sleep(runif(1, min = 0.5, max = 2))
  this_citation <- scholar::get_article_cite_history(scholar_id, publication$pubid)

  if (nrow(this_citation) > 0) {

    all_cites[i] <- sum(this_citation$cites)
    citations_year <- rbind(citations_year,
                            this_citation)

  }

  destfolder <- here::here(
    "publications",
    publication$slug)

  destfile <- file.path(destfolder,
                        "citation_history.rds")

  df_cit <- this_citation |>
    select(-pubid)

  df2 <- data.frame(
    year = seq(publication$year,
               lubridate::year(lubridate::now())),
               cites = 0)

  this_citation <- rbind(
    anti_join(df2,
              df_cit,
              by = "year"),
    df_cit) |>
    arrange(year)

  if (sum(this_citation$cites) > 0) {

    saveRDS(this_citation, destfile)

  } else {

    glue::glue("No hay citas para {publication$slug}\n") |> cat()

    cat("\n")

  }

}
pubs2 <- pubs2 |>
  mutate(cites = all_cites)

saveRDS(pubs2,
        here::here("publications",
                   "pubdata.rds"))
saveRDS(citations_year,
        here::here("publications",
                   "citdata.rds"))

# 5 most-cited publications
pubs2 |>
  slice_max(cites, n = 5) |>
  select(title, slug, cites, year) |>
  mutate(title = stringr::str_to_title(title)) |>
  mutate(title = glue::glue("- title: {title}")) |>

  mutate(url = glue::glue(
    "  path: /publications/{slug}/index.qmd")) |>
  mutate(cites = glue::glue("  cites: {cites}")) |>
  mutate(year = glue::glue("  year: {year}")) |>
  select(title, url, cites, year) |>
  mutate(contents = paste0(title, "\n",
                           url, "\n",
                           cites, "\n",
                           year)) |>
  select(contents) |>
  unlist() |>
  cat(file = here::here("research",
                        "items.yml"),
      sep = "\n")


# # citations (experimental, may break things)
# chrome_session <- open_connection()
# check_connection(chrome_session)
#
# for (i in seq_along(pubs2$cid)) {
#
#   cid <- pubs2$cid[i]
#   my_id <- pubs2$pubid[i]
#   slug <- pubs2$slug[i]
#
#   if (is.na(cid)) next
#
#   glue::glue(
#     "Retrieving data for {slug}\n\n"
#   ) |>
#     cat()
#
#   df <- find_ids_of_citing_scholar(cid)
#
#   glue::glue(
#     "  Found {nrow(df)} citing papers for {slug}\n\n"
#   ) |>
#     cat()
#
#   # citations <- c()
#   #
#   # for (id in df$id) {
#   #
#   #   citations <- c(citations,
#   #                  find_citation_in_scholar(id))
#   #
#   # }
#
#   # bibtex <- c()
#   # for (t in df$title) {
#   #
#   #   bibtex <- c(bibtex,
#   #               dblp_lookup_title(t))
#   #
#   # }
#
#   bibtex <- c()
#   for (id in df$id) {
#
#     bibtex <- c(bibtex,
#                 find_bibtex_in_scholar(id))
#
#   }
#
#   glue::glue(
#     "  Retrieved BibTeX of citing papers for {slug}\n\n"
#   ) |>
#     cat()
#
#   bibfile <- here::here(
#     "publications",
#     slug,
#     "cited_by.bib"
#   )
#
#   cat(bibtex, file = bibfile, sep = "\n")
#
# }
#
#
# close_connection(chrome_session)

