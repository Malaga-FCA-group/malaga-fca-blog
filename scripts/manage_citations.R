require(httr)
require(rvest)
require(jsonlite)
require(purrr)
require(stringr)
require(glue)
require(dplyr)
require(polite)

# chrome_folder <- "/Applications/Google\ Chrome\ Beta.app/Contents/MacOS/Google\ Chrome\ Beta"
#
# library(chromote)
# Sys.setenv("CHROMOTE_CHROME" = chrome_folder)
#
#
# user_agents <- readLines(here::here("user-agents.txt"))
#
# headers_def <- c("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/113.0.0.0 Safari/537.36")

find_ids_of_citing_scholar <- function(cid) {

  b <- get("chrome_session")

  params <- list(
    hl = "en",
    astart = 0
  )

  url <- "https://scholar.google.es/scholar?oi=bibs&hl=es&cites={cid}" |>
    glue::glue()

  id <- c()
  titles <- c()

  # browser()
  there_are_citations <- TRUE
  while (there_are_citations) {

    Sys.sleep(time = runif(1, min = 0, max = 2))

    # response <- GET(url, query = params,
    #                 add_headers(.headers = headers))
    # page <- read_html(content(response, "text"))

    page <- secure_navigate_to(b, url)
    if (is.na(page)) break

    # b$Page$navigate(url, wait_ = TRUE)
    # Sys.sleep(1)
    # response <- b$DOM$getDocument()
    # page <- b$DOM$getOuterHTML(
    #   nodeId = response$root$nodeId)$outerHTML |>
    #   rvest::read_html()

    refs <- page |>
      html_elements(".gs_ri")

    new_id <- refs |>
      html_element("h3 a") |>
      html_attr(name = "id")

    new_titles <- refs |>
      html_elements("h3 a") |>
      html_text()

    new_id2 <- new_id
    new_titles2 <- new_titles

    if (anyNA(new_id)) {

      for (i in seq_along(new_id)) {

        if (is.na(new_id[i])) {

          tmp <- refs[[i]] |>
            html_elements("h3 span") |>
            html_attr(name = "id")

          tmp2 <- refs[[i]] |>
            html_elements("h3 span") |>
            html_text()

          idx <- which(!is.na(tmp))

          if (length(idx) > 0) {

            new_id2[i] <- tmp[idx]
            new_titles2[i] <- tmp2[idx]

          }

        }

      }

    }

    if (length(new_titles2) < length(new_id2)) browser()

    id <- c(id, new_id2)
    titles <- c(titles, new_titles2)

    # pagination
    next_page_button <- page |>
      html_elements("button.gs_btnPR") |>
      html_attr("onclick")

    # browser()
    #
    if (length(next_page_button) == 0) break

    if (!is.na(next_page_button)) {

      next_page_button <- next_page_button |>
        normalize_url()
      # extract the "after_author" parameter from the "onclick" attribute of the "Next" button using regex
      # and assign it to the "after_author" URL parameter which is the next token pagination
      # along with "astart" URL param
      params$start <- str_match(next_page_button, "start=(.*)$")[, 2]
      params$astart <- params$astart + 10

      url <- "https://scholar.google.es/scholar?start={params$astart}&oi=bibs&hl=es&cites={cid}" |>
        glue::glue()

    } else {
      there_are_citations <- FALSE
    }


  }

  return(data.frame(id = id,
                    title = titles))

}

find_bibtex_in_scholar <- function(id) {

  b <- get("chrome_session")

  url3 <- "https://scholar.google.es/scholar?q=info:{id}:scholar.google.com/&output=cite&scirp=0&hl=es" |>
    glue::glue()

  page_cite <- secure_navigate_to(b, url3)
  if (is.na(page_cite)) return(NULL)
  # b$Page$navigate(url3)
  # Sys.sleep(time = runif(1, min = 0, max = 2))
  # response <- b$DOM$getDocument()
  #
  # page_cite <- b$DOM$getOuterHTML(nodeId = response$root$nodeId)$outerHTML |>
  #   rvest::read_html()


  bibtex_url <- page_cite |>
    rvest::html_elements("div a") |>
    rvest::html_attr("href") |>
    stringr::str_subset("scholar.bib")

  page_bib <- secure_navigate_to(b, bibtex_url)
  if (is.na(page_bib)) return(NULL)

  # b$Page$navigate(bibtex_url)
  # Sys.sleep(0.5)
  # response <- b$DOM$getDocument()

  bib <- page_bib |>
    rvest::html_element("pre") |>
    rvest::html_text()

  return(bib)

}


find_citation_in_scholar <- function(id) {

  b <- get("chrome_session")

  url3 <- "https://scholar.google.es/scholar?q=info:{id}:scholar.google.com/&output=cite&scirp=0&hl=es" |>
    glue::glue()

  Sys.sleep(time = runif(1, min = 0, max = 2))

  b$Page$navigate(url3)
  Sys.sleep(0.5)
  response <- b$DOM$getDocument()

  page_cite <- b$DOM$getOuterHTML(nodeId = response$root$nodeId)$outerHTML |>
    rvest::read_html()

  citations <- page_cite |>
    rvest::html_elements(".gs_citr") |>
    rvest::html_text()

  return(citations[1])

}


normalize_url <- function(url) {

  url |>
    stringr::str_replace_all("\\\\x3d", "=") |>
    stringr::str_replace_all("\\\\x26", "&")

}
