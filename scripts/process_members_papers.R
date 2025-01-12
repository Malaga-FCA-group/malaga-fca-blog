source(
  here::here(
    "scripts",
    "manage_publications.R"
  )
)

jfolder <- here::here("publications", "journals")
journal_papers <<- list.files(
  path = jfolder,
  full.names = TRUE,
  pattern = ".preqmd$",
  recursive = TRUE
)

slugs <<- journal_papers |>
  dirname() |>
  stringr::str_remove_all(jfolder)

all_authors <<- lapply(journal_papers, \(f) get_publication_value(dirname(f), "author"))

papers_of_member <- function(nm) {
  idx <- which(sapply(all_authors, \(x) nm %in% x))

  authors <- lapply(
    journal_papers[idx],
    \(f) get_publication_value(dirname(f), "author")
  )

  titles <- lapply(
    journal_papers[idx],
    \(f) get_publication_value(dirname(f), "title")
  )

  years <- lapply(
    journal_papers[idx],
    \(f) get_publication_value(dirname(f), "date")
  )

  details <- lapply(
    journal_papers[idx],
    \(f) get_publication_value(dirname(f), "details")
  )

  paths <- slugs[idx]

  result <- list()

  for (i in seq_along(idx)) {
    res <- list(
      title = titles[[i]],
      author = authors[[i]] |> process_authors(),
      date = years[[i]],
      details = details[[i]],
      path = glue::glue("/publications/journals{paths[i]}")
    )

    result <- append(result, list(res))
  }

  nm_folder <- fs::dir_ls(here::here("people"), type = "directory", recurse = TRUE) |> stringr::str_subset(nm)

  yaml::write_yaml(
    result,
    file = file.path(nm_folder, "papers.yml")
  )

  return(invisible(TRUE))
}

members <- fs::dir_ls(here::here("people", "staff")) |>
  purrr::map_chr(basename)

sapply(members, papers_of_member)

former_members <- fs::dir_ls(here::here("people", "former")) |>
  purrr::map_chr(basename)
sapply(former_members, papers_of_member)
