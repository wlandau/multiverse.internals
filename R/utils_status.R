status_list <- function(x) {
  colnames(x) <- tolower(colnames(x))
  out <- list()
  for (index in seq_len(nrow(x))) {
    for (field in setdiff(colnames(x), "package")) {
      if (!all(is.na(x[[field]][[index]]))) {
        out[[x$package[index]]][[field]] <- x[[field]][[index]]
      }
    }
  }
  out[order(as.character(names(out)))]
}
