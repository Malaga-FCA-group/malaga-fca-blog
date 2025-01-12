folder <- here::here("people")
people_folders <- c(
  fs::dir_ls(
    path = file.path(folder, "staff"),
    glob = "*.qmd",
    recurse = TRUE,
    type = "any"
  ),
  fs::dir_ls(
    path = file.path(folder, "former"),
    glob = "*.qmd",
    recurse = TRUE,
    type = "any"
  ),
  fs::dir_ls(
    path = file.path(folder, "alumni"),
    glob = "*.qmd",
    recurse = TRUE,
    type = "any"
  )
) |>
  sapply(dirname)

read_name <- function(file) {
  contents <- read_qmd(file)
  contents$contents$title
}
read_signature <- function(file) {
  contents <- read_qmd(file)
  contents$contents$signature
}

actual_names <- sapply(
  people_folders,
  \(nm) read_name(file.path(nm, "index.qmd"))
)

signatures <- sapply(
  people_folders,
  \(nm) read_signature(file.path(nm, "index.qmd"))
)

people_listing <<- cbind(actual_names, basename(people_folders), signatures)
rownames(people_listing) <- colnames(people_listing) <- NULL


readr::write_csv(
  as.data.frame(people_listing),
  here::here("people", "listing.csv"),
  col_names = FALSE
)


replace_name <- function(nm) {
  idx <- which(people_listing[, 2] == nm)

  glue::glue("<a href='/people/staff/{nm}/' style='color:rgb(100,0,50);'>{people_listing[idx, 3]}</a>")
}


process_authors <- function(author_list) {
  idx <- which(author_list %in% people_listing[, 2])

  if (length(idx) > 0) {
    author_list[idx] <- sapply(author_list[idx], replace_name)
  }

  return(author_list)
}

mask_authors <- function(author_list) {
  indices <- stringdist::amatch(author_list, people_listing[, 3], maxDist = 5)
  idx <- which(!is.na(indices))

  if (length(idx) > 0) {
    author_list[idx] <- people_listing[indices[idx], 2]
  }

  return(author_list)
}
