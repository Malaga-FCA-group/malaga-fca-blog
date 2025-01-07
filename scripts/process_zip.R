process_zip <- function(file, force = FALSE) {

  my_dir <- file.path(tempdir(),
                      "process_zip")

  if (dir.exists(my_dir)) {

    fs::dir_delete(my_dir)

  }

  fs::dir_create(my_dir)

  res <- zip::unzip(zipfile = file,
                    exdir = my_dir)

  index <- file.path(my_dir, "index.preqmd")
  contents <- read_qmd(index)$contents

  base_folder <- ifelse(
    contents$type == "project",
    here::here("projects"),
    here::here("publications", contents$type)
  )

  new_folder <- file.path(base_folder, contents$slug)

  if (!force && dir.exists(new_folder)) return(NULL)
  fs::dir_copy(my_dir, new_folder)

  return(invisible(NULL))

}

# files <- list.files(
#   path = here::here("tmp_zip"),
#   pattern = "*.zip",
#   full.names = TRUE
# )
#
# files |> lapply(process_zip)
