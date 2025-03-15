status_list <- function(x) {
  colnames(x) <- tolower(colnames(x))
  out <- list()
  fields <- setdiff(colnames(x), "package")
  for (index in seq_len(nrow(x))) {
    for (field in fields) {
      out[[x$package[index]]][[field]] <- x[[field]][[index]]
    }
  }
  out[order(as.character(names(out)))]
}
