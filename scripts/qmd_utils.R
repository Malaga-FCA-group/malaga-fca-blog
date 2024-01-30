read_qmd <- function(file) {

  lines <- readLines(con = file)

  idx <- which(lines |> stringr::str_detect("---"))

  header <- lines[seq(idx[1], idx[2])]
  other <- lines[-seq(idx[2])]


  L <- yaml::yaml.load(header)

  return(list(contents = L, other = other))

}

write_qmd <- function(x, file) {

  contents <- x$contents
  other <- x$other
  YAML <- yaml::as.yaml(contents)
  cat("---\n", file = file)
  cat(YAML, file = file, append = TRUE)
  cat("---\n", file = file, append = TRUE)
  cat(other, file = file, append = TRUE)

}
