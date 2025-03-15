status_list <- function(x) {
  colnames(x) <- tolower(colnames(x))
  out <- list()
  fields <- setdiff(colnames(x), "package")
  for (index in seq_len(nrow(x))) {
    for (field in fields) {
      entry <- x[[field]][[index]]
      if (!all(is.na(entry))) {
        out[[x$package[index]]][[field]] <- entry
      }
    }
  }
  out[order(as.character(names(out)))]
}
