##%######################################################%##
#                                                          #
####                    For the app                     ####
#                                                          #
##%######################################################%##


# List of categories
source(here::here(
  "scripts", "manage_publications.R"
))
p_folder <- here::here("publications")
pr_folder <- here::here("projects")

categories <-  c(
  get_publication_value(
    p_folder,
    value = "categories"
  ) |> unlist(),
  get_publication_value(
    pr_folder,
    value = "categories"
  ) |> unlist()) |>
  sort() |>
  stringr::str_to_sentence() |>
  unique()

categories <- as.list(categories) |>
  rlang::set_names(categories)


# List of projects
pr <- get_publication_value(
  pr_folder,
  value = c("title", "slug")
)
projects <- do.call(rbind, pr)
projects <- as.list(projects$slug) |>
  rlang::set_names(projects$title)

saveRDS(
  object = categories,
  file = here::here("data-provider",
                    "categories.RDS")
)

saveRDS(
  object = projects,
  file = here::here("data-provider",
                    "projects.RDS")
)
