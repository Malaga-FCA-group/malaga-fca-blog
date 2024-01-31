# Steps to compile

source(here::here("scripts",
                  "qmd_utils.R"))
source(here::here("scripts",
                  "find_selected_items.R"))
source(here::here("scripts",
                  "collect_bib.R"))
source(here::here("scripts",
                  "publication_data.R"))

# Generate bibtex file
collect_bib(here::here("publications"))

##### N/A
# # Selected items
# fs::dir_delete(here::here("selected", "publications"))
# fs::dir_delete(here::here("selected", "projects"))
#
# find_selected_items("publications", 5)
# find_selected_items("projects", 3)
#
# # Format selected items
# fs::file_copy(here::here("publications", "publication.html"),
#               here::here("selected", "publications",
#                          "publication.html"))
# fs::file_copy(here::here("publications", "title-metadata.html"),
#               here::here("selected", "publications",
#                          "title-metadata.html"))
# fs::file_copy(here::here("publications", "_metadata.yml"),
#               here::here("selected", "publications",
#                          "_metadata.yml"))

# Reset preview
fs::dir_delete(here::here("_site"))

folder <- here::here("projects")
files <- list.files(
  path = folder,
  pattern = "index.preqmd$",
  full.names = TRUE,
  recursive = TRUE
)

source(
  here::here("scripts",
             "preqmd.R")
)

files |> lapply(process_preqmd)

# folder <- here::here("publications")
# journals <- list.files(
#   path = file.path(folder, "journals"),
#   pattern = "*.qmd$",
#   full.names = TRUE,
#   recursive = TRUE)
# conferences <- list.files(
#   path = file.path(folder, "conferences"),
#   pattern = "*.qmd$",
#   full.names = TRUE,
#   recursive = TRUE)
# books <- list.files(
#   path = file.path(folder, "books"),
#   pattern = "*.qmd$",
#   full.names = TRUE,
#   recursive = TRUE)
#
# files <- c(
#   journals,
#   books,
#   conferences
# )
#
# files |> lapply(quarto::quarto_render)

# Make actual preview and render
quarto::quarto_preview()

# fs::dir_delete(here::here("selected", "publications"))
# fs::dir_delete(here::here("selected", "projects"))

quarto::quarto_preview_stop()

# quarto::quarto_publish_site(
#   title = "Domingo López Rodríguez",
#   server = "shinyapps.io",
#   account = "dominlopez",
#   render = "server")

# quarto::quarto_render(here::here("index.qmd"))
