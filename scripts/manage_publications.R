##%######################################################%##
#                                                          #
####                Get publication data                ####
#                                                          #
##%######################################################%##

get_publication_year <- function(folder) {

  files <- list.files(path = folder,
                      pattern = "index.qmd$",
                      full.names = TRUE,
                      recursive = TRUE)

  files |>
    sapply(\(file) {

      lines <- readLines(con = file)

      idx <- which(lines |> stringr::str_detect("---"))

      header <- lines[seq(idx[1], idx[2])]
      other <- lines[-seq(idx[2])]


      L <- yaml::yaml.load(header)

      lubridate::year(L$date)

    })

}

get_publication_value <- function(folder, value) {

  files <- list.files(path = folder,
                      pattern = "index.qmd$",
                      full.names = TRUE,
                      recursive = TRUE)

  files |>
    sapply(\(file) {

      lines <- readLines(con = file)

      idx <- which(lines |> stringr::str_detect("---"))

      header <- lines[seq(idx[1], idx[2])]
      other <- lines[-seq(idx[2])]


      L <- yaml::yaml.load(header)

      L[[value]]

    })

}

plot_citation_history <- function(df) {

  library(ggplot2)

  gpubs <- ggplot(df,
                  mapping = aes(x = year,
                                y = cites)) +
    geom_line(color = "#0EB1D2",
              linewidth = 1.1) +
    geom_point(size = 2,
               color = "#2660A4") +
    theme_minimal() +
    theme(legend.title = element_blank(),
          plot.background = element_rect(fill = "#FFFFFF"),
          panel.background = element_rect(fill = "#FFFFFF"),
          legend.position = "bottom") +
    scale_x_continuous(breaks = df$year)
  plotly::ggplotly(gpubs) |>
    plotly::config(displayModeBar = FALSE)
}
