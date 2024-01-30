find_selected_items <- function(type, n) {

  folder <- here::here(tolower(type))

  files <- list.files(path = folder,
                      pattern = "*.qmd$",
                      full.names = TRUE,
                      recursive = TRUE)
  files_not <- list.files(path = folder,
                      pattern = "*.qmd$",
                      full.names = TRUE,
                      recursive = FALSE)

  files <- setdiff(files, files_not)
  selected <- c()
  dates <- c()
  for (file in files) {

    L <- read_qmd(file)

    if (!is.null(L$contents$selected)) {

      selected <- c(selected, dirname(file))
      dates <- c(dates, L$contents$date)

    }

  }

  if (length(selected) > 0) {

    o <- order(dates, decreasing = TRUE)
    if (length(selected) > n) {

      o <- o[seq(n)]

    }
    dates <- dates[o]
    selected <- selected[o]

    for (d in selected) {

      fs::dir_copy(d, here::here("selected", type, basename(d)), overwrite = TRUE)

    }

  }

}
