#' @title Update topics
#' @export
#' @family topics
#' @description Update the list of packages for each
#'   R-multiverse topic.
#' @return `NULL` (invisibly). Called for its side effects.
#' @param path Character string,
#'   local file path to the topics repository source code.
#' @param repo Character string, URL of the Community universe.
#' @param mock List of named objects for testing purposes only.
#' @examples
#' \dontrun{
#' path <- tempfile()
#' gert::git_clone("https://github.com/r-multiverse/topics", path = path)
#' update_topics(path = path, repo = "https://community.r-multiverse.org")
#' }
update_topics <- function(
  path,
  repo = "https://community.r-multiverse.org",
  mock = NULL
) {
  meta <- mock$meta %||% meta_packages(repo)
  meta <- meta[, c("package", "title", "description_url")]
  unlink(file.path(path, "*.html"))
  topics <- list.files(file.path(path, "topics"))
  for (topic in topics) {
    update_topic(topic, path, meta)
  }
  topic_urls <- file.path(
    "https://r-multiverse.org/topics",
    paste0(topics, ".html")
  )
  topic_list <- sprintf("* [%s](%s)", topics, topic_urls)
  template <- readLines(file.path(path, "index.md"))
  text <- c(template, "", topic_list)
  writeLines(text, file.path(path, "index.md"))
}

update_topic <- function(topic, path, meta) {
  url <- file.path("https://r-multiverse.org/topics", paste0(topic, ".html"))
  which <- grepl(pattern = url, x = meta$description_url, fixed = TRUE)
  meta <- meta[which,, drop = FALSE] # nolint
  about <- readLines(file.path(path, "topics", topic))
  urls <- file.path("https://community.r-multiverse.org", meta$package)
  packages <- sprintf("|[%s](%s)|%s|", meta$package, urls, meta$title)
  packages <- paste(packages, collapse = "\n")
  template <- file.path(path, "topic.md")
  text <- readLines(template)
  text <- gsub(pattern = "ABOUT", replacement = about, x = text)
  text <- gsub(pattern = "PACKAGES", replacement = packages, x = text)
  text <- gsub(pattern = "TOPIC", replacement = topic, x = text)
  writeLines(text, file.path(path, paste0(topic, ".md")))
}
