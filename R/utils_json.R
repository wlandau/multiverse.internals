name_json <- function(json) {
  names(json) <- vapply(
    X = json,
    FUN = function(package) {
      package$package
    },
    FUN.VALUE = character(1L)
  )
  json
}
