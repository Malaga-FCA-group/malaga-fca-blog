get_citations_from_doi <- function(doi) {
  work <- openalexR::oa_fetch(
    entity = "works",
    doi = doi,
    verbose = FALSE
  )

  if (is.null(work)) {

    return(list(by_year = NA, citations = NULL))

  }

  by_year <- work$counts_by_year
  url <- work$cited_by_api_url
  
  e <- tryCatch(json <- jsonlite::read_json(url))

  if (inherits(e, "try-error")) {

    return(list(by_year = by_year, citations = NULL))

  }

  if (length(json$results) == 0) {

    return(list(by_year = by_year, citations = NULL))

  }

  dois <- json$results |> sapply(\(x) x$doi) |> unlist()
  years <- json$results |> sapply(\(x) x$publication_year)
  o <- order(years, decreasing = TRUE)
  # suppressWarnings(
  #   txt <- rcrossref::cr_cn(
  #     dois[o], 
  #     format = "text", 
  #     style = "elsevier-harvard") |> 
  #     unlist())
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

format_citations <- function(results) {

  formatted_citations <- sapply(results, function(pub) {
    # Autores
    authors <- paste(sapply(pub$authorships, function(auth) auth$author$display_name), collapse = ", ")
    
    # Título
    title <- pub$title
    
    # Revista o libro
    venue <- if (!is.null(pub$host_venue$name)) {
      pub$host_venue$name
    } else if (!is.null(pub$locations) && length(pub$locations) > 0) {
      # Buscar nombre del libro/revista en locations
      location_venue <- sapply(pub$locations, function(loc) loc$source$display_name)
      location_venue <- unique(location_venue[!is.na(location_venue)])
      if (length(location_venue) > 0) {
        paste(location_venue, collapse = "; ")
      } else {
        ""
      }
    } else {
      ""
    }
    
    # DOI o URL de la publicación
    doi <- ifelse(!is.null(pub$doi), paste0("DOI: ", pub$doi), "")
    
    # Número de páginas (si hay)
    pages <- if (!is.null(pub$biblio$first_page) && !is.null(pub$biblio$last_page)) {
      paste0(", pp. ", pub$biblio$first_page, "-", pub$biblio$last_page)
    } else {
      ""
    }
  
    vol <- ifelse(!is.null(pub$biblio$volume), paste0(" vol. ", pub$biblio$volume), "")
    issue <- ifelse(!is.null(pub$biblio$issue), paste0(" (", pub$biblio$volume, ")"), "")
    
    
    # Año de publicación
    publication_year <- ifelse(
      !is.null(pub$publication_year), 
      pub$publication_year, "")
    
    # Combinar todos los detalles en una cita
    glue::glue(
      "{authors} ({publication_year}). _{title}_. {venue}{vol}{issue}{pages}. {doi}"
    )
  }) |> unlist()

  paste0("[", seq_along(formatted_citations), "] ", formatted_citations)
  
}

