folder <- here::here("people", "staff")
people_folders <- fs::dir_ls(path = folder)

read_name <- function(file) {
  contents <- read_qmd(file)
  contents$contents$title
}

actual_names <- sapply(
  people_folders,
  \(nm) read_name(file.path(nm, "index.qmd"))
)

people_listing <<- cbind(actual_names, basename(people_folders))
rownames(people_listing) <- colnames(people_listing) <- NULL


readr::write_csv(
  as.data.frame(people_listing),
  here::here("people", "listing.csv"),
  col_names = FALSE
)


replace_name <- function(nm) {
  idx <- which(people_listing[, 2] == nm)

  glue::glue("<a href='/people/staff/{nm}/' style='color:rgb(100,0,50);'>{people_listing[idx, 1]}</a>")
}


process_authors <- function(author_list) {
  idx <- which(author_list %in% people_listing[, 2])

  if (length(idx) > 0) {
    author_list[idx] <- sapply(author_list[idx], replace_name)
  }

  return(author_list)
}
