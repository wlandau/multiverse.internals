test_that("dependency graph is correct", {
  mock <- mock_meta_packages
  packages <- c("nanonext", "mirai", "crew", "crew.cluster", "crew.aws.batch")
  mock <- mock[mock$package %in% packages, ]
  graph <- issues_dependencies_graph(meta = mock)
  out <- igraph::as_data_frame(graph, what = "edges")
  out <- out[order(out$from), ]
  expect_equal(out$from[out$to == "crew.aws.batch"], "crew")
  expect_equal(out$from[out$to == "crew.cluster"], "crew")
  expect_equal(out$to[out$from == "mirai"], "crew")
  expect_equal(
    sort(out$to[out$from == "nanonext"]),
    c("crew", "mirai")
  )
})

test_that("issues_dependencies() no problems", {
  exp <- data.frame(package = character(0L))
  exp$dependencies <- list()
  expect_equal(
    issues_dependencies(
      packages = character(0L),
      meta = mock_meta_packages,
      verbose = FALSE
    ),
    exp
  )
})

test_that("issues_dependencies() no revdeps", {
  exp <- data.frame(package = character(0L))
  exp$dependencies <- list()
  expect_equal(
    issues_dependencies(
      packages = "crew.aws.batch",
      meta = mock_meta_packages,
      verbose = FALSE
    ),
    exp
  )
})

test_that("issues_dependencies() nanonext", {
  mock <- mock_meta_packages
  packages <- c("nanonext", "mirai", "crew", "crew.cluster", "crew.aws.batch")
  mock <- mock[mock$package %in% packages, ]
  out <- suppressMessages(
    issues_dependencies(
      packages = "nanonext",
      meta = mock
    )
  )
  expect_equal(
    out$package,
    c("crew", "crew.aws.batch", "crew.cluster", "mirai")
  )
  expect_equal(
    out$dependencies,
    list(
      list(nanonext = "mirai"),
      list(nanonext = "crew"),
      list(nanonext = "crew"),
      list(nanonext = character(0L))
    )
  )
})

test_that("issues_dependencies() mirai", {
  mock <- mock_meta_packages
  packages <- c("nanonext", "mirai", "crew", "crew.cluster", "crew.aws.batch")
  mock <- mock[mock$package %in% packages, ]
  out <- issues_dependencies(
    packages = "mirai",
    meta = mock,
    verbose = FALSE
  )
  expect_equal(out$package, c("crew", "crew.aws.batch", "crew.cluster"))
  expect_equal(
    out$dependencies,
    list(
      list(mirai = character(0L)),
      list(mirai = "crew"),
      list(mirai = "crew")
    )
  )
})

test_that("issues_dependencies() crew", {
  mock <- mock_meta_packages
  packages <- c("nanonext", "mirai", "crew", "crew.cluster", "crew.aws.batch")
  mock <- mock[mock$package %in% packages, ]
  out <- issues_dependencies(
    packages = "crew",
    meta = mock,
    verbose = FALSE
  )
  expect_equal(out$package, c("crew.aws.batch", "crew.cluster"))
  expect_equal(
    out$dependencies,
    list(
      list(crew = character(0L)),
      list(crew = character(0L))
    )
  )
})

test_that("issues_dependencies() nanonext and mirai", {
  mock <- mock_meta_packages
  packages <- c("nanonext", "mirai", "crew", "crew.cluster", "crew.aws.batch")
  mock <- mock[mock$package %in% packages, ]
  out <- issues_dependencies(
    packages = c("nanonext", "mirai"),
    meta = mock,
    verbose = FALSE
  )
  expect_equal(
    out$package,
    c("crew", "crew.aws.batch", "crew.cluster", "mirai")
  )
  expect_equal(
    out$dependencies,
    list(
      list(nanonext = "mirai", mirai = character(0L)),
      list(nanonext = "crew", mirai = "crew"),
      list(nanonext = "crew", mirai = "crew"),
      list(nanonext = character(0L))
    )
  )
})

test_that("issues_dependencies() nanonext and mirai", {
  mock <- mock_meta_packages
  packages <- c("nanonext", "mirai", "crew", "crew.cluster", "crew.aws.batch")
  mock <- mock[mock$package %in% packages, ]
  out <- issues_dependencies(
    packages = c("crew", "mirai"),
    meta = mock,
    verbose = FALSE
  )
  expect_equal(
    out$package,
    c("crew", "crew.aws.batch", "crew.cluster")
  )
  expect_equal(
    out$dependencies,
    list(
      list(mirai = character(0L)),
      list(crew = character(0L), mirai = "crew"),
      list(crew = character(0L), mirai = "crew")
    )
  )
})

test_that("issues_dependencies() with more than one direct dependency", {
  meta <- mock_meta_packages
  packages <- c("nanonext", "mirai", "crew", "crew.cluster", "crew.aws.batch")
  meta <- meta[meta$package %in% packages, ]
  row <- meta[meta$package == "crew", ]
  row$package <- "x"
  meta <- rbind(meta, row)
  meta[["dependencies"]][meta$package == "crew.aws.batch"][[1L]] <- rbind(
    meta[["dependencies"]][meta$package == "crew.aws.batch"][[1L]],
    data.frame(package = "x", version = NA_character_, role = "Imports")
  )
  out <- issues_dependencies(packages = "mirai", meta = meta, verbose = FALSE)
  expect_equal(
    out$package,
    c("crew", "crew.aws.batch", "crew.cluster", "x")
  )
  expect_equal(
    out$dependencies,
    list(
      list(mirai = character(0L)),
      list(mirai = c("crew", "x")),
      list(mirai = "crew"),
      list(mirai = character(0L))
    )
  )
})

test_that("issues_dependencies() with more than one direct dependency (2)", {
  meta <- mock_meta_packages
  packages <- c("nanonext", "mirai", "crew", "crew.cluster", "crew.aws.batch")
  meta <- meta[meta$package %in% packages, ]
  row <- meta[meta$package == "crew", ]
  row$package <- "x"
  meta <- rbind(meta, row)
  meta[["dependencies"]][meta$package == "crew.aws.batch"][[1L]] <- rbind(
    meta[["dependencies"]][meta$package == "crew.aws.batch"][[1L]],
    data.frame(package = "x", version = NA_character_, role = "Imports")
  )
  out <- issues_dependencies(
    packages = c("mirai", "nanonext"),
    meta = meta,
    verbose = FALSE
  )
  expect_equal(
    out$package,
    c("crew", "crew.aws.batch", "crew.cluster", "mirai", "x")
  )
  expect_equal(
    out$dependencies,
    list(
      list(mirai = character(0L), nanonext = "mirai"),
      list(
        mirai = c("crew", "x"),
        nanonext = c("crew", "x")
      ),
      list(
        mirai = "crew",
        nanonext = "crew"
      ),
      list(nanonext = character(0L)),
      list(mirai = character(0L), nanonext = "mirai")
    )
  )
})
