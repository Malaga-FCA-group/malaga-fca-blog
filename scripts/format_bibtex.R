format_bibtex <- function(file) {

  txt <- file |>
    readLines()

  init <- txt |>
    stringr::str_detect("@") |>
    which()
  end <- txt |>
    stringr::str_detect("^\\}") |>
    which()
  n <- length(txt)

  new_txt <- c(txt[init], "<br>&nbsp;&nbsp;&nbsp;&nbsp;",
               stringr::str_flatten(txt[-c(1:init, end:n)], "<br>&nbsp;&nbsp;&nbsp;&nbsp;"),
               "<br>", txt[end])


  txt <- stringr::str_flatten(new_txt)

  return(txt)

}
