url_parse <- function(url) {
  nanonext::parse_url(trim_trailing_slash(url))
}

trim_trailing_slash <- function(url) {
  sub(pattern = "/$", replacement = "", x = url, perl = TRUE)
}
