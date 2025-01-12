get_citations_from_doi <- function(doi) {
  work <- openalexR::oa_fetch(
    entity = "works",
    doi = doi,
    verbose = FALSE
  )

  by_year <- work$counts_by_year
  url <- work$cited_by_api_url
  json <- jsonlite::read_json(url)

  dois <- json$results |> sapply(\(x) x$doi)
  years <- json$results |> sapply(\(x) x$publication_year)
  o <- order(years, decreasing = TRUE)
  bibs <- RefManageR::GetBibEntryWithDOI(dois[o])
  RefManageR::NoCite(bibs)
  txt <- bibs |>
    RefManageR::PrintBibliography(.opts = list(
      bib.style = "numeric",
      style = "markdown"
    )) |>
    capture.output()

  return(list(by_year = by_year, citations = txt))
}
