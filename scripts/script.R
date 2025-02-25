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

source(
  here::here(
    "scripts",
    "citations_openalex.R"
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

projects |>
  sapply(process_preqmd) |>
  invisible()

folder <- here::here("publications")
journals <- list.files(
  path = file.path(folder, "journals"),
  pattern = "index.preqmd$",
  full.names = TRUE,
  recursive = TRUE
)

journals |>
  sapply(process_preqmd) |>
  invisible()

conferences <- list.files(
  path = file.path(folder, "conferences"),
  pattern = "index.preqmd$",
  full.names = TRUE,
  recursive = TRUE
)

conferences |> sapply(process_preqmd) |> invisible()

books <- list.files(
  path = file.path(folder, "books"),
  pattern = "index.preqmd$",
  full.names = TRUE,
  recursive = TRUE
)

books |> sapply(process_preqmd) |> invisible()

# FORCE <- FALSE
# source(here::here(
#   "scripts",
#   "generate_post.R"
# ))

# source(here::here(
#   "scripts",
#   "for_data_provider.R"
# ))

source(here::here(
  "scripts",
  "process_members_papers.R"
))


# Make actual preview and render
# quarto::quarto_preview()

if ("rstudioapi" %in% installed.packages()) {

  quarto::quarto_render()

} else {

  quarto::quarto_render(as_job = FALSE)

}


# quarto::quarto_preview_stop()

API_tiny <- "ukcniod2pt7c65psnhhht72k5ed545rf8mp9o028j7o4qz4s"
