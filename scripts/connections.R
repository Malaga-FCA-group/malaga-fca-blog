require(httr)
require(rvest)
require(jsonlite)
require(purrr)
require(stringr)
require(glue)
require(dplyr)
require(polite)

chrome_folder <- "/Applications/Google\ Chrome\ Beta.app/Contents/MacOS/Google\ Chrome\ Beta"

library(chromote)
Sys.setenv("CHROMOTE_CHROME" = chrome_folder)


user_agents <- readLines(here::here("user-agents.txt"))

headers_def <- c("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/113.0.0.0 Safari/537.36")

open_connection <- function() {

  b <- ChromoteSession$new()
  b$view()
  b$Network$setUserAgentOverride(userAgent = headers_def["User-Agent"])

  if (!file.exists(here::here("cookies.rds"))) {

    b$Page$navigate("https://scholar.google.es")

    cookies <- b$Network$getCookies()
    str(cookies)
    saveRDS(cookies, here::here("cookies2.rds"))

  } else {

    cookies <- readRDS(here::here("cookies2.rds"))
    b$Network$setCookies(cookies = cookies$cookies)

  }

  return(b)

}

check_connection <- function(b) {

  b$Page$navigate("https://scholar.google.es")

  readline()

  cookies <- b$Network$getCookies()
  str(cookies)
  saveRDS(cookies, here::here("cookies2.rds"))

}

close_connection <- function(b) {

  # p <- b$parent
  p_browser <- b$parent$get_browser()
  b$close()
  p_browser$close()

}

secure_navigate_to <- function(b, url) {

  b$Page$navigate(url)
  Sys.sleep(time = runif(1, min = 0, max = 2))

  e <- try({

    response <- b$DOM$getDocument()

    page_html <- b$DOM$getOuterHTML(nodeId = response$root$nodeId)$outerHTML |>
      rvest::read_html()

  })

  if (inherits(e, "try-error")) {

    res <- glue::glue(
      "Error navigating to {url}. (P = proceed / N = next)"
    ) |>
      readline() |>
      tolower()

    if (res == "p" || res == "") {

      return(secure_navigate_to(b, url))

    }

    if (res == "n") {

      return(NA)

    }

  }

  return(page_html)
}

dblp_lookup_title <- function(title) {

  url_query <- "http://dblp.org/search/publ/api?q={normalize_query(title)}" |>
    glue::glue()

  Sys.sleep(time = runif(1, min = 0, max = 2))
  resp <- GET(url_query)

  results <- content(resp, "text") |>
    read_html() |>
    html_element("hits") |>
    html_attr("total") |>
    as.numeric()

  if (results == 0) return(NULL)

  doi <- content(resp, "text") |>
    read_html() |>
    html_element("doi") |>
    html_text()

  url <- content(resp, "text") |>
    read_html() |>
    html_element("url") |>
    html_text()

  url_bib <- paste0(url, ".bib?param=1")

  readLines(con = url_bib) |>
    stringr::str_flatten("\n")

}

normalize_query <- function(q) {

  q |>
    stringr::str_replace_all("\\s+", "+")

}
