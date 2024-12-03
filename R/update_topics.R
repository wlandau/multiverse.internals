#' @title Update topics
#' @export
#' @family topics
#' @description Update the list of packages for each
#'   R-multiverse topic.
#' @return `NULL` (invisibly). Called for its side effects.
#' @inheritParams update_staging
#' @param path Character string,
#'   local file path to the topics repository source code.
#' @param repo Character string, URL of the Community universe.
update_topics <- function(
  path,
  repo = "https://community.r-multiverse.org",
  mock = NULL
) {
  meta <- mock$meta %||% meta_packages(repo, fields = c("Title", "URL"))
  meta <- meta[, c("package", "title", "url")]
  unlink(file.path(path, "*.html"))
  topics <- setdiff(
    list.files(path),
    c(".gitignore", "README.md", "LICENSE.md")
  )
  for (topic in topics) {
    update_topic(topic, path, meta)
  }
  topic_urls <- file.path(
    "https://r-multiverse.org/topics", paste0(topics, ".html")
  )
  topic_list <- paste0(
    "<li>",
    topics,
    ": <a href=\"",
    topic_urls,
    "\">",
    topic_urls,
    "</a>",
    "</li>"
  )
  topic_text <- paste(topic_list, collapse = "\n")
  template <- system.file(
    file.path("topics", "index.html"),
    package = "multiverse.internals",
    mustWork = TRUE
  )
  text <- readLines(template)
  text <- gsub(pattern = "TOPICS", replacement = topic_text, x = text)
  writeLines(text, file.path(path, "index.html"))
}

update_topic <- function(topic, path, meta) {
  url <- file.path("https://r-multiverse.org/topics", paste0(topic, ".html"))
  meta <- meta[grepl(pattern = url, x = meta$url, fixed = TRUE), ]
  about <- readLines(file.path(path, topic))
  template <- system.file(
    file.path("topics", "topic.html"),
    package = "multiverse.internals",
    mustWork = TRUE
  )
  line <- paste0(
    "<li>",
    "<a href=\"https://community.r-multiverse.org/PACKAGE\">PACKAGE</a>: ",
    "TITLE",
    "</li>"
  )
  packages <- character(0L)
  for (row in seq_len(nrow(meta))) {
    package <- meta$package[row]
    title <- meta$title[row]
    element <- line
    element <- gsub(pattern = "PACKAGE", replacement = package, x = element)
    element <- gsub(pattern = "TITLE", replacement = title, x = element)
    packages <- c(packages, element)
  }
  packages <- paste(packages, collapse = "\n")
  text <- readLines(template)
  text <- gsub(pattern = "ABOUT", replacement = about, x = text)
  text <- gsub(pattern = "PACKAGES", replacement = packages, x = text)
  text <- gsub(pattern = "TOPIC", replacement = topic, x = text)
  writeLines(text, file.path(path, paste0(topic, ".html")))
}
