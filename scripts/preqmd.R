process_preqmd <- function(file) {

  lines <- readLines(con = file)

  idx <- lines |>
    stringr::str_detect("---") |>
    which()

  header <- lines[seq(idx[1], idx[2])] |>
    yaml::yaml.load()

  body <- lines[-seq(idx[2])]

  pattern <- "\\{\\{.*\\}\\}"

  idx <- body |>
    stringr::str_detect(pattern) |>
    which()

  if (length(idx) > 0) {

    my_funs <- body[idx] |>
      stringr::str_extract_all(pattern) |>
      stringr::str_remove_all("[\\{|\\}]")

    for (i in seq_along(idx)) {

      f <- funs[[my_funs[i]]]
      if (!is.null(f)) {

        res <- f(header, dirname(file))
        body[idx[i]] <- res$inline
        if (length(res$header) > 0) {

          header <- combine_lists(list(header, res$header))

        }

      } else {

        body[idx[i]] <- "\n"

      }

    }

  }

  newfile <- stringr::str_replace(file,
                                  pattern = "preqmd$",
                                  "qmd")

  cat("---\n", file = newfile)
  cat(yaml::as.yaml(header), file = newfile,
      append = TRUE)
  cat("---\n\n", file = newfile,
      append = TRUE)
  cat(body, file = newfile,  sep = "\n",
      append = TRUE)

}

# Auxiliary functions
# -------------------
# Must return a list with two components: inline, header.
# inline must be single string (correctly
# formatted in HTML or LaTeX) and may include linebreaks
# by using \n
# header must be a list
papers_of_project <- function(header, folder) {

  project_id <- header$slug

  project_folder <- here::here(
    "projects",
    project_id)

  pub_folder <- here::here("publications")
  files <- list.files(
    path = pub_folder,
    pattern = "index.qmd$",
    full.names = TRUE,
    recursive = TRUE
  )

  # pubs <- list()

  str <- NULL

  for (file in files) {

    lines <- readLines(con = file)

    type <- basename(dirname(dirname(file)))

    idx <- which(lines |> stringr::str_detect("---"))

    # header <- lines[seq(idx[1], idx[2])]

    contents <- lines[seq(idx[1], idx[2])] |>
      yaml::yaml.load()

    if (!is.null(contents$project) &&
        tolower(project_id) %in% tolower(contents$project)) {

      contents$path <- glue::glue("/publications/{type}/{contents$slug}")

      authors <- glue::glue(
        "  - {contents$author}",
        .trim = FALSE
      ) |>
        stringr::str_flatten("\n")
      str <- c(
        str,
        glue::glue(
          "- title: '{contents$title}'\n",
          "  author: \n{authors}\n",
          "  date: {contents$date}\n",
          "  categories: [{stringr::str_flatten_comma(contents$categories)}]\n",
          "  description: {contents$details}\n",
          "  path: {contents$path}",
          .trim = FALSE
        ))

    }

  }

  newfile <- here::here(
    project_folder,
    paste0("published_works.yml")
  )

  cat(str, file = newfile, sep = "\n")

  if (length(str) > 0) {

    res <- "\n# Published works\n\n:::{#published_works}\n:::\n"
    return(list(
      inline = res,
      header = list(
        listing = list(list(
          id = "published_works",
          contents = "published_works.yml",
          type = "default",
          sort = "date desc"))
      )
    ))

  } else {

    return(list(
      inline = "",
      header = list()
    ))

  }

}

projects_of_paper <- function(header, folder) {

  # pub_id <- header$slug
  #
  # pub_folder <- here::here(
  #   "publications",
  #   pub_id)

  # browser()

  projects <- header$project |>
    stringr::str_match_all(".+") |>
    unlist()

  if (length(projects) == 0) return(nul_f(header, folder))

  projects_folder <- here::here("projects")
  str <- ""

  for (pr in projects) {

    this_proj <- here::here(projects_folder,
                            pr)

    qmd <- list.files(path = this_proj,
                      pattern = "index.qmd$",
                      full.names = TRUE)

    lines <- readLines(con = qmd)

    idx <- which(lines |> stringr::str_detect("---"))

    contents <- lines[seq(idx[1], idx[2])] |>
      yaml::yaml.load()

    contents$path <- glue::glue("/projects/{contents$slug}")

    authors <- glue::glue(
      "  - {contents$author}",
      .trim = FALSE
    ) |>
      stringr::str_flatten("\n")
    str <- c(
      str,
      glue::glue(
        "- title: '{contents$title}'\n",
        "  author: \n{authors}\n",
        "  date: {contents$date}\n",
        "  categories: [{stringr::str_flatten_comma(contents$categories)}]\n",
        "  image: {contents$path}/{contents$image}\n",
        "  path: {contents$path}",
        .trim = FALSE
      ))

  }

  newfile <- here::here(
    folder,
    paste0("funding.yml")
  )

  cat(str, file = newfile, sep = "\n")

  if (length(str) > 0) {

    res <- "\n# Funding\n::: {.callout-note appearance='minimal' collapse=false}\n## Projects funding this work\n\n:::{#funding}\n:::\n:::\n"
    return(list(
      inline = res,
      header = list(
        listing = list(list(
          id = "funding",
          contents = "funding.yml",
          type = "default",
          sort = "date desc"))
      )
    ))

  } else {

    return(list(
      inline = "",
      header = list()
    ))

  }

}

citation_of_work <- function(header, folder) {

  bibfile <- file.path(folder, 'cite.bib')

  string <- NULL
  if (file.exists(bibfile)) {

    # Part 1: cite
    s <- ReadBib(bibfile)
    NoCite(s)
    options(width = 1000)
    txt <- s |>
      RefManageR::PrintBibliography(.opts = list(bib.style = 'alphabetic', style = 'markdown')) |>
      capture.output()

    string <- c(
      "# Citation\nPlease, cite this work as:\n\n",
      txt)

    # Part 2: bibtex
    txt <- bibfile |>
      readLines()

    init <- txt |>
      stringr::str_detect("@") |>
      which()
    end <- txt |>
      stringr::str_detect("^\\}") |>
      which()
    n <- length(txt)

    browser()

    new_txt <- c(txt[init], "<br>&nbsp;&nbsp;&nbsp;&nbsp;",
                 stringr::str_flatten(txt[-c(1:init, end:n)], "<br>&nbsp;&nbsp;&nbsp;&nbsp;"),
                 "<br>", txt[end])

    txt <- stringr::str_flatten(new_txt)

    string <- c(
      string,
      '\n::: {.callout-note appearance="minimal" collapse=true}\n\n## BibTeX\n',
      txt,
      "\n:::\n"
    )

    string <- stringr::str_flatten(string, "\n")

    return(
      list(
        inline = string,
        header = list()
      )
    )

  } else {

    return(list(
      inline = "",
      header = list()
    ))

  }


}

citation_history <- function(header, folder) {

  this_file <- file.path(folder, "citation_history.rds")

  if (file.exists(this_file)) {

    string <- c(
      "# Cites\n",
      "The following graph plots the number of cites received by this work from its publication, on a yearly basis.\n",
      "```{r citing2}", "#| echo: false", "#| results: asis",
      "#| warning: false", "#| message: false",
      "source(here::here('scripts', 'manage_publications.R'))",
      "df <- readRDS('citation_history.rds')",
      "plot_citation_history(df)",
      "```\n") |>
      stringr::str_flatten("\n")

    return(
      list(
        inline = string,
        header = list()
      )
    )

  } else {

    nul_f(header, folder)

  }

}

nul_f <- function(header, folder) {

  return(list(
    inline = "",
    header = list()
  ))

}

funs <- list(
  published_works = papers_of_project,
  funding = projects_of_paper,
  citation = citation_of_work,
  citation_history = citation_history
)

combine_lists <- function(input_list) {

  cat_lists <- function(list1, list2) {

    keys <- unique(c(names(list1), names(list2)))
    purrr::map2(list1[keys], list2[keys], c) |>
      purrr::set_names(keys)

  }

  combined_output <- purrr::reduce(input_list, cat_lists)

  return(combined_output)

}
