collect_bib <- function(folder) {

  bibfiles <- list.files(
    path = folder,
    pattern = "cite.bib",
    all.files = TRUE,
    full.names = TRUE,
    recursive = TRUE)

  bibs <- lapply(bibfiles, RefManageR::ReadBib)

  output <- Reduce(RefManageR:::merge.BibEntry, bibs, bibs[[1]])

  RefManageR::WriteBib(output,
                       file = file.path(folder,
                                        "all_refs.bib"))

}
