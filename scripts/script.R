# Steps to compile

# source(here::here("scripts",
#                   "qmd_utils.R"))
# source(here::here("scripts",
#                   "collect_bib.R"))


# Reset preview
fs::dir_delete(here::here("_site"))

# Prepare updated content
folder <- here::here("projects")
projects <- list.files(
  path = folder,
  pattern = "index.preqmd$",
  full.names = TRUE,
  recursive = TRUE
)

source(
  here::here("scripts",
             "preqmd.R")
)

projects |> sapply(process_preqmd)

folder <- here::here("publications")
journals <- list.files(
  path = file.path(folder, "journals"),
  pattern = "*.qmd$",
  full.names = TRUE,
  recursive = TRUE)
conferences <- list.files(
  path = file.path(folder, "conferences"),
  pattern = "*.qmd$",
  full.names = TRUE,
  recursive = TRUE)
books <- list.files(
  path = file.path(folder, "books"),
  pattern = "*.qmd$",
  full.names = TRUE,
  recursive = TRUE)

publications <- c(
  journals,
  books,
  conferences
)

publications |> sapply(process_preqmd)

# Make actual preview and render
quarto::quarto_preview()

quarto::quarto_preview_stop()

