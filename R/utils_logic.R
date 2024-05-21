`%||%` <- function(x, y) {
  if (length(x) > 0L) {
    x
  } else {
    y
  }
}

`%|||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}
