library(shiny)
library(ggplot2)
library(bslib)
library(gridlayout)
library(RefManageR)
library(shinyWidgets)

source("functions.R")

categories <<- readRDS("categories.RDS")
projects <<- readRDS("projects.RDS")

ui <- page_navbar(
  title = "Data provider",
  selected = "Publication",
  collapsible = TRUE,
  theme = bslib::bs_theme(),
  nav_panel(
    title = "Publication",
    grid_container(
      layout = c(
        "Author Author",
        "Author Author"
      ),
      row_sizes = c(
        "1fr",
        "1fr"
      ),
      col_sizes = c(
        "1fr",
        "1fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "Author",
        full_screen = TRUE,
        card_header("Basic data"),
        card_body(
          fileInput(inputId = "bibfile",
                    label = "Upload .bib file",
                    accept = "*.bib"),
          h2(strong("Mandatory fields")),
          textInput(
            inputId = "author",
            label = "Author names separated by commas",
            value = "",
            width = "100%"
          ),
          textInput(
            inputId = "title",
            label = "Title",
            value = "",
            width = "100%"
          ),
          # textInput(
          #   inputId = "date",
          #   label = "Publication date (YYYY-MM-DD)",
          #   value = "",
          #   width = "100%"
          # ),
          dateInput(
            inputId = "date",
            label = "Publication date (YYYY-MM-DD)",
            value = "",
            width = "100%"
          ),
          textInput(
            inputId = "details",
            label = "Publication details: journal, volume...",
            value = "",
            width = "100%"
          ),
          textInput(
            inputId = "doi",
            label = "DOI",
            value = "",
            width = "100%"
          ),
          multiInput(
            inputId = "categories",
            label = "Categories",
            choices = list("None" = ""),
            width = "100%"
          ),
          # textInput(,
          #   inputId = "categories",,
          #   label = "Categories, separated by commas",,
          #   value = "",,
          #   width = "100%",
          # ),,
          # checkboxGroupInput(,
          #   inputId = "funding_projects",,
          #   label = "Projects",,
          #   choices = list("..." = "1", ",,," = "2"),,
          #   width = "50%",
          # ),,
          multiInput(
            inputId = "proj",
            label = "Projects",
            choices = list("None" = ""),
            width = "100%"
          ),
          radioButtons(
            inputId = "type",
            label = "Publication type",
            choices = list(
              "Journal" = "journals",
              "Conference" = "conferences",
              "Book (or book chapter)" = "books"
            ),
            width = "100%"
          ),
          textOutput(outputId = "slug"),
          h2(strong("Optional fields")),
          textInput(
            inputId = "abstract",
            label = "Abstract",
            value = "",
            width = "100%"
          ),
          fileInput(
            inputId = "header_image",
            label = "Image for the publication"
          ),
          downloadButton(outputId = "generate", label = "Generate ZIP")
        )
      )
    )
  ),
  nav_panel(
    title = "Project",
    grid_container(
      layout = c(
        "basic",
        "basic"
      ),
      row_sizes = c(
        "165px",
        "1fr"
      ),
      col_sizes = c(
        "1fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "basic",
        card_body(
          textInput(
            inputId = "pr_title",
            label = "Project title (including acronym):",
            value = "",
            width = "100%"
          ),
          textInput(
            inputId = "pr_ref",
            label = "Project code or reference:",
            value = "",
            width = "100%"
          ),
          textInput(
            inputId = "pr_ips",
            label = "Principal investigator(s), separated by comma:",
            value = "",
            width = "100%"
          ),
          textInput(
            inputId = "pr_members",
            label = "Participants, separated by comma:",
            value = "",
            width = "100%"
          ),
          textInput(
            inputId = "pr_other",
            label = "Other collaborators, separated by comma:",
            value = "",
            width = "100%"
          ),
          dateRangeInput("pr_date", "Start/end dates:"),
          textInput(
            inputId = "pr_abstract",
            label = "Abstract",
            value = "",
            width = "100%"
          ),
          multiInput(
            inputId = "pr_categories",
            label = "Categories",
            choices = list("choice a" = "a", "choice b" = "b"),
            width = "100%"
          ),
          textInput(
            inputId = "pr_link",
            label = "URL",
            value = "",
            width = "100%"
          ),
          fileInput(
            inputId = "pr_header_image",
            label = "Image for the project (maybe a logo?)"
          ),
          downloadButton(outputId = "pr_generate", label = "Generate ZIP")
        )
      )
    )
  )
)


server <- function(input, output, session) {

  # output$files <- renderTable(input$bibfile)

  # projects <- get("projects",
  #                 envir = globalenv())

  categories <- readRDS("categories.RDS")
  projects <- readRDS("projects.RDS")

  updateMultiInput(
    session = session,
    inputId = "proj",
    choices = projects
  )

  # categories <- get("categories",
  #                   envir = globalenv())

  updateMultiInput(
    session = session,
    inputId = "categories",
    choices = categories
  )

  updateMultiInput(
    session = session,
    inputId = "pr_categories",
    choices = categories
  )

  observe({

    req(input$bibfile)

    # browser()

    # message(input$bibfile)

    bib <- RefManageR::ReadBib(
      input$bibfile$datapath
    )[[1]] |>
      unlist()

    authors <- bib$author |>
      sapply(\(s) glue::glue("{stringr::str_flatten(s$given, collapse = ' ')} {s$family}"))
    title <- bib$title
    journal <- bib$journal
    booktitle <- bib$booktitle
    date <- bib$year
    # if (!is.null(bib$dateobj)) date <- bib$dateobj
    doi <- bib$doi
    volume <- bib$volume
    issue <- bib$issue
    if (!is.null(bib$number)) issue <- bib$number
    pages <- bib$pages
    str_pages <- glue::glue(
      ", pages {pages}"
    )
    if (!is.null(bib[["article-number"]])) {

      pages <- bib[["article-number"]]
      str_pages <- glue::glue(
        ", article number {pages}"
      )

    }
    keywords <- bib$keywords
    abstract <- bib$abstract
    series <- bib$series

    # browser()

    type <- switch(
      tolower(bib$bibtype),
      "article" = "journals",
      "inproceedings" = "conferences",
      "inbook" = "books",
      "incollection" = "books"
    )

    str_issue <- ifelse(
      is.null(issue),
      "",
      glue::glue("({issue})")
    )
    str_vol <- ifelse(
      is.null(volume),
      "",
      glue::glue("vol. {volume}")
    )

    str_series <- ifelse(
      is.null(series),
      "",
      glue::glue(", {series} {str_vol}")
    )

    details <- switch(
      type,
      journals =  glue::glue(
        "{journal} {str_vol} {str_issue}{str_pages}."),
      conferences = glue::glue(
        "{booktitle} {str_series}{str_pages}."),
      books = glue::glue(
        "{booktitle} {str_series}{str_pages}.")
    )

    updateTextInput(
      inputId = "author",
      value = stringr::str_flatten_comma(authors)
    )

    updateTextInput(
      inputId = "doi",
      value = doi
    )

    updateTextInput(
      inputId = "keywords",
      value = keywords
    )

    updateTextInput(
      inputId = "abstract",
      value = abstract
    )

    updateTextInput(
      inputId = "details",
      value = details
    )

    updateTextInput(
      inputId = "title",
      value = title
    )

    updateDateInput(
      inputId = "date",
      value = paste0(date, "-01-01")
    )

    updateCheckboxInput(
      inputId = "type",
      value = type
    )

  })

  output$generate <- downloadHandler(
    filename = function() {

      slug <- c(
        lubridate::year(input$date),
        "-",
        input$author |>
          stringr::str_split_1("\\s*,\\s*") |>
          stringr::str_extract_all("[A-Z]") |>
          lapply(\(x) paste0(stringr::str_flatten(x), "-")) |>
          unlist() |>
          stringr::str_flatten(),
        input$title |>
          stringr::str_extract("[a-zA-Z]*")) |>
        stringr::str_flatten()

      paste(slug, ".zip", sep = "")
    },

    content =  function(file) {

      if (is.null(input$header_image$datapath)) {

        path <- "~"

      } else {

        path <- input$header_image$datapath

      }

      # browser()

      L <- generate_publication_preqmd(
        type = input$type,
        author = input$author,
        date = input$date,
        title = input$title,
        categories = input$categories,
        header_image = path,
        details = input$details,
        doi = input$doi,
        project = input$proj,
        keywords = input$keywords,
        abstract = input$abstract
      )

      index_file <- L$file
      slug <- L$slug

      old_bib <- input$bibfile$datapath
      bib_dir <- dirname(old_bib)
      my_bib <- file.path(bib_dir, "cite.bib")
      fs::file_copy(path = old_bib,
                    new_path = my_bib,
                    overwrite = TRUE)

      files <- c(index_file,
                 my_bib)

      if (!is.null(input$header_image)) {

        files <- c(files,
                   input$header_image$datapath)
      }

      zipfile <- file.path(
        tempdir(),
        paste0(slug, ".zip")
      )

      zipfile <- zip::zip(
        zipfile = file,
        files = files,
        mode = "cherry-pick"
      )

    },
    contentType = "application/zip"
  )

  output$pr_generate <- downloadHandler(
    filename = function() {

      slug <- c(
        lubridate::year(input$pr_date[1]),
        "-",
        input$pr_title |>
          stringr::str_extract("[a-zA-Z]*")
      ) |>
        stringr::str_flatten()

      # slug <- get("slug",
      #             envir = globalenv())

      paste(slug, ".zip", sep = "")
    },

    content =  function(file) {

      L <- generate_project_preqmd(
        author = input$pr_ips,
        start_date = input$pr_date[1],
        end_date = input$pr_date[2],
        title = input$pr_title,
        reference = input$pr_ref,
        members = input$pr_members,
        collaborators = input$pr_other,
        categories = input$pr_categories,
        header_image = input$pr_header_image$datapath,
        path = input$pr_link,
        abstract = input$pr_abstract
      )

      # browser()

      index_file <- L$file
      slug <- L$slug

      files <- index_file

      if (!is.null(input$pr_header_image)) {

        files <- c(files,
                   input$pr_header_image$datapath)

      }

      zipfile <- file.path(
        tempdir(),
        paste0(slug, ".zip")
      )

      zipfile <- zip::zip(
        zipfile = file,
        files = files,
        mode = "cherry-pick"
      )

    },
    contentType = "application/zip"
  )

}

shinyApp(ui, server)


