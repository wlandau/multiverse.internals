assert_file <- function(x) {
  if (!file.exists(x)) {
    stop("File ", shQuote(x), " does not exist.", call. = FALSE)
  }
}

assert_character_scalar <- function(x, message) {
  if (!is_character_scalar(x)) {
    stop(message, call. = FALSE)
  }
}

assert_positive_scalar <- function(x, message) {
  if (!is_positive_scalar(x)) {
    stop(message, call. = FALSE)
  }
}

is_character_scalar <- function(x) {
  is.character(x) &&
    length(x) == 1L &&
    !anyNA(x) &&
    nzchar(x)
}

is_positive_scalar <- function(x) {
  is.numeric(x) &&
    length(x) == 1L &&
    all(is.finite(x)) &&
    all(x > 0)
}

try_message <- function(try_error, collapse = " ") {
  paste(conditionMessage(attr(try_error, "condition")), collapse = collapse)
}
