issues_list <- function(x) {
  colnames(x) <- tolower(colnames(x))
  out <- list()
  for (index in seq_len(nrow(x))) {
    for (field in setdiff(colnames(x), "package")) {
      out[[x$package[index]]][[field]] <- x[[field]][[index]]
    }
  }
  out[order(as.character(names(out)))]
}
