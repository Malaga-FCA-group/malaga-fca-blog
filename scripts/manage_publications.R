## %######################################################%##
#                                                          #
####                Get publication data                ####
#                                                          #
## %######################################################%##

get_publication_year <- function(folder) {
  files <- list.files(
    path = folder,
    pattern = "index.qmd$",
    full.names = TRUE,
    recursive = TRUE
  )

  files |>
    sapply(\(file) {
      lines <- readLines(con = file)

      idx <- which(lines |> stringr::str_detect("---"))

      header <- lines[seq(idx[1], idx[2])]
      other <- lines[-seq(idx[2])]


      L <- yaml::yaml.load(header)

      if (stringr::str_length(L$date) == 4) {
        return(as.character(L$date))
      } else {
        return(lubridate::year(L$date))
      }
    })
}

get_publication_value <- function(folder, value) {
  files <- list.files(
    path = folder,
    pattern = "index.preqmd$",
    full.names = TRUE,
    recursive = TRUE
  )

  files |>
    sapply(\(file) {
      lines <- brio::readLines(con = file)

      idx <- which(lines |> stringr::str_detect("---"))

      header <- lines[seq(idx[1], idx[2])]
      other <- lines[-seq(idx[2])]


      L <- yaml::yaml.load(header)

      if (length(value) == 1) {
        return(L[[value]])
      } else {
        return(as.data.frame(L[value]))
      }
    })
}

plot_citation_history <- function(df) {
  library(ggplot2)

  gpubs <- ggplot(df,
    mapping = aes(
      x = year,
      y = cites
    )
  ) +
    # geom_line(
    #   color = "#0EB1D2",
    #   linewidth = 1.1
    # ) +
    # geom_point(
    #   size = 2,
    #   color = "#2660A4"
    # ) +
    geom_col(color = "#2660A4", fill = "#2660A4") +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      plot.background = element_rect(fill = "#FFFFFF"),
      panel.background = element_rect(fill = "#FFFFFF"),
      legend.position = "bottom"
    ) +
    scale_x_continuous(breaks = df$year) +
    ylim(0, max(df$cites))
  plotly::ggplotly(gpubs) |>
    plotly::config(displayModeBar = FALSE)
}

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
