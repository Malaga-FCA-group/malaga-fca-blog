process_preqmd <- function(file) {
  lines <- brio::readLines(con = file)

  idx <- lines |>
    stringr::str_detect("---") |>
    which()

  header <- lines[seq(idx[1], idx[2])] |>
    yaml::yaml.load()

  if (!is.null(header$doi)) {
    if ((length(header$doi) == 0) || (nchar(header$doi) < 3)) {
      header$doi <- NULL
    } else {
      header$doi <- header$doi |>
        stringr::str_replace_all(
          stringr::fixed("\\_"),
          "_"
        )
    }
  }

  cli::cli_alert_info("Processing {.val {header$slug}}")

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
    "qmd"
  )

  header$author <- process_authors(header$author)

  # print(header)

  cat("---\n", file = newfile)
  cat(yaml::as.yaml(header),
    file = newfile,
    append = TRUE
  )
  cat("---\n\n",
    file = newfile,
    append = TRUE
  )
  cat(body,
    file = newfile, sep = "\n",
    append = TRUE
  )

  cli::cli_alert_success("Processed {.val {header$slug}}")
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
    project_id
  )

  pub_folder <- here::here("publications")
  files <- list.files(
    path = pub_folder,
    pattern = "index.preqmd$",
    full.names = TRUE,
    recursive = TRUE
  )

  # pubs <- list()

  str <- NULL

  for (file in files) {
    lines <- brio::readLines(con = file)

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

      categories <- contents$categories |>
        stringr::str_to_sentence() |>
        stringr::str_flatten_comma()

      str <- c(
        str,
        glue::glue(
          "- title: '{contents$title}'\n",
          "  author: \n{authors}\n",
          "  date: {contents$date}\n",
          "  categories: [{categories}]\n",
          "  description: '{contents$details}'\n",
          "  path: {contents$path}",
          .trim = FALSE
        )
      )
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
          sort = "date desc"
        ))
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

  if (length(projects) == 0) {
    return(nul_f(header, folder))
  }

  projects_folder <- here::here("projects")
  str <- ""

  for (pr in projects) {
    this_proj <- here::here(
      projects_folder,
      pr
    )

    qmd <- list.files(
      path = this_proj,
      pattern = "index.preqmd$",
      full.names = TRUE
    )

    lines <- brio::readLines(con = qmd)

    idx <- which(lines |> stringr::str_detect("---"))

    contents <- lines[seq(idx[1], idx[2])] |>
      yaml::yaml.load()

    contents$path <- glue::glue("/projects/{contents$slug}")

    contents$author <- process_authors(contents$author)

    authors <- glue::glue(
      "  - {contents$author}",
      .trim = FALSE
    ) |>
      stringr::str_flatten("\n")

    categories <- contents$categories |>
      stringr::str_to_sentence() |>
      stringr::str_flatten_comma()

    str <- c(
      str,
      glue::glue(
        "- title: '{contents$title}'\n",
        "  author: \n{authors}\n",
        "  date: {contents$date}\n",
        "  categories: [{categories}]\n",
        "  image: {contents$path}/{contents$image}\n",
        "  path: {contents$path}",
        .trim = FALSE
      )
    )
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
          sort = "date desc"
        ))
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
  bibfile <- file.path(folder, "cite.bib")

  string <- NULL
  if (file.exists(bibfile)) {
    # Part 1: cite
    s <- ReadBib(bibfile)
    NoCite(s)
    options(width = 1000)
    txt <- s |>
      RefManageR::PrintBibliography(.opts = list(bib.style = "alphabetic", style = "markdown")) |>
      capture.output()

    string <- c(
      "# Citation\nPlease, cite this work as:\n\n",
      txt
    )

    # Part 2: bibtex
    txt <- bibfile |>
      readLines()

    bool <- txt[length(txt)] |>
      stringr::str_detect("\\}\\}")

    if (bool) {
      txt[length(txt)] <- txt[length(txt)] |>
        stringr::str_replace_all(
          "\\}\\}",
          "}"
        )
      txt <- c(txt, "}")
    }
    init <- txt |>
      stringr::str_detect("@") |>
      which()
    end <- txt |>
      stringr::str_detect("\\}") |>
      which()
    end <- end[length(end)]
    num <- length(txt)

    # browser()

    new_txt <- c(
      txt[init], "<br>&nbsp;&nbsp;&nbsp;&nbsp;",
      stringr::str_flatten(txt[-c(1:init, end:num)], "<br>&nbsp;&nbsp;&nbsp;&nbsp;"),
      "<br>", txt[end]
    )

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

prepare_for_citations <- function(header, folder) {
  if (!is.null(header$doi)) {
    L <- get_citations_from_doi(header$doi)
    if (!is.na(L$by_year)) {
      citation_history <- L$by_year[[1]] |>
        rlang::set_names(c("year", "cites"))
      saveRDS(citation_history,
        file = file.path(folder, "citation_history.rds")
      )
    }

    if (!is.null(L$citations) && (length(L$citations) > 0) && (L$citations[1] != "NULL")) {
      cat(L$citations,
        file = file.path(folder, "citations.txt"),
        sep = "\n"
      )
    }
  }

  return(list(
    inline = "",
    header = list()
  ))
}

citation_history <- function(header, folder) {
  this_file <- file.path(folder, "citation_history.rds")

  if (file.exists(this_file)) {
    string <- c(
      "# Bibliometric data\n\n",
      "The following data has been extracted from resources such as [OpenAlex](https://openalex.org/), [Dimensions](https://app.dimensions.ai/), [PlumX](https://www.elsevier.com/insights/metrics/plumx) or [Altmetric](https://www.altmetric.com/).",
      "",
      ifelse(!is.null(header$doi),
        glue::glue('<div style="display: flex; justify-content: center; align-items: center; height: 100%;">\n<span class="__dimensions_badge_embed__" data-doi="{header$doi}" data-legend="always"></span></div><script async src="https://badge.dimensions.ai/badge.js" charset="utf-8"></script>'), ""
      ),
      "",
      ifelse(!is.null(header$doi),
        glue::glue('<div style="display: flex; justify-content: center; align-items: center; height: 100%;">\n<a href="https://plu.mx/plum/a/?doi={header$doi}" class="plumx-details" data-site="plum" data-hide-when-empty="true">{header$title}</a></div>'), ""
      ),
      "",
      ifelse(!is.null(header$doi),
        glue::glue('<div style="display: flex; justify-content: center; align-items: center; height: 100%;">\n<script type="text/javascript" src="https://d1bxh8uas1mnw7.cloudfront.net/assets/embed.js"></script><div data-badge-type="medium-donut" class="altmetric-embed" data-badge-details="right" data-doi="{header$doi}"></div></div>'), ""
      ),
      "\n\n## Cites\n",
      "The following graph plots the number of cites received by this work from its publication, on a yearly basis.\n",
      "```{r citing2}", "#| echo: false", "#| results: asis",
      "#| warning: false", "#| message: false",
      "source(here::here('scripts', 'manage_publications.R'))",
      "df <- readRDS('citation_history.rds')",
      "plot_citation_history(df)",
      "```\n"
    ) |>
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

my_citations <- function(header, folder) {
  this_file <- file.path(folder, "citations.txt")

  if (file.exists(this_file)) {
    txt <- brio::readLines(this_file)
    string <- c("## Papers citing this work\n\nThe following is a non-exhaustive list of papers that cite this work:\n\n", txt) |>
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
  prepare = prepare_for_citations,
  citation_history = citation_history,
  citations = my_citations
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
