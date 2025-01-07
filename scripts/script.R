# Steps to compile

source(here::here(
  "scripts",
  "qmd_utils.R"
))

source(
  here::here(
    "scripts",
    "master_table_people.R"
  )
)
# source(here::here("scripts",
#                   "collect_bib.R"))

library(RefManageR)

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
  here::here(
    "scripts",
    "preqmd.R"
  )
)

projects |> sapply(process_preqmd)

folder <- here::here("publications")
journals <- list.files(
  path = file.path(folder, "journals"),
  pattern = "index.preqmd$",
  full.names = TRUE,
  recursive = TRUE
)

journals |> sapply(process_preqmd)

conferences <- list.files(
  path = file.path(folder, "conferences"),
  pattern = "index.preqmd$",
  full.names = TRUE,
  recursive = TRUE
)

conferences |> sapply(process_preqmd)

books <- list.files(
  path = file.path(folder, "books"),
  pattern = "index.preqmd$",
  full.names = TRUE,
  recursive = TRUE
)

books |> sapply(process_preqmd)

FORCE <- TRUE
source(here::here(
  "scripts",
  "generate_post.R"
))

source(here::here(
  "scripts",
  "for_data_provider.R"
))

# Make actual preview and render
quarto::quarto_preview()

# quarto::quarto_preview_stop()

API_tiny <- "ukcniod2pt7c65psnhhht72k5ed545rf8mp9o028j7o4qz4s"
