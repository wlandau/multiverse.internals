test_that("dependency graph is correct", {
  mock <- mock_meta_packages
  packages <- c("nanonext", "mirai", "crew", "crew.cluster", "crew.aws.batch")
  mock <- mock[mock$package %in% packages, ]
  graph <- status_dependencies_graph(meta = mock)
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

test_that("status_dependencies() no problems", {
  expect_equal(
    status_dependencies(
      packages = character(0L),
      meta = mock_meta_packages,
      verbose = FALSE
    ),
    list()
  )
})

test_that("status_dependencies() no revdeps", {
  expect_equal(
    status_dependencies(
      packages = "crew.aws.batch",
      meta = mock_meta_packages,
      verbose = FALSE
    ),
    list()
  )
})

test_that("status_dependencies() nanonext", {
  mock <- mock_meta_packages
  packages <- c("nanonext", "mirai", "crew", "crew.cluster", "crew.aws.batch")
  mock <- mock[mock$package %in% packages, ]
  expect_equal(
    suppressMessages(
      status_dependencies(
        packages = "nanonext",
        meta = mock
      )
    ),
    list(
      mirai = list(nanonext = character(0L)),
      crew = list(nanonext = "mirai"),
      crew.aws.batch = list(nanonext = "crew"),
      crew.cluster = list(nanonext = "crew")
    )
  )
})

test_that("status_dependencies() mirai", {
  mock <- mock_meta_packages
  packages <- c("nanonext", "mirai", "crew", "crew.cluster", "crew.aws.batch")
  mock <- mock[mock$package %in% packages, ]
  expect_equal(
    status_dependencies(
      packages = "mirai",
      meta = mock,
      verbose = FALSE
    ),
    list(
      crew = list(mirai = character(0L)),
      crew.aws.batch = list(mirai = "crew"),
      crew.cluster = list(mirai = "crew")
    )
  )
})

test_that("status_dependencies() crew", {
  mock <- mock_meta_packages
  packages <- c("nanonext", "mirai", "crew", "crew.cluster", "crew.aws.batch")
  mock <- mock[mock$package %in% packages, ]
  expect_equal(
    status_dependencies(
      packages = "crew",
      meta = mock,
      verbose = FALSE
    ),
    list(
      crew.aws.batch = list(crew = character(0L)),
      crew.cluster = list(crew = character(0L))
    )
  )
})

test_that("status_dependencies() nanonext and mirai", {
  mock <- mock_meta_packages
  packages <- c("nanonext", "mirai", "crew", "crew.cluster", "crew.aws.batch")
  mock <- mock[mock$package %in% packages, ]
  expect_equal(
    status_dependencies(
      packages = c("nanonext", "mirai"),
      meta = mock,
      verbose = FALSE
    ),
    list(
      mirai = list(nanonext = character(0L)),
      crew = list(nanonext = "mirai", mirai = character(0L)),
      crew.aws.batch = list(nanonext = "crew", mirai = "crew"),
      crew.cluster = list(nanonext = "crew", mirai = "crew")
    )
  )
})

test_that("status_dependencies() nanonext and mirai", {
  mock <- mock_meta_packages
  packages <- c("nanonext", "mirai", "crew", "crew.cluster", "crew.aws.batch")
  mock <- mock[mock$package %in% packages, ]
  expect_equal(
    status_dependencies(
      packages = c("crew", "mirai"),
      meta = mock,
      verbose = FALSE
    ),
    list(
      crew.aws.batch = list(crew = character(0L), mirai = "crew"),
      crew.cluster = list(crew = character(0L), mirai = "crew"),
      crew = list(mirai = character(0L))
    )
  )
})

test_that("status_dependencies() with more than one direct dependency", {
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
  expect_equal(
    status_dependencies(packages = "mirai", meta = meta, verbose = FALSE),
    list(
      crew = list(mirai = character(0L)),
      x = list(mirai = character(0L)),
      crew.aws.batch = list(mirai = c("crew", "x")),
      crew.cluster = list(mirai = "crew")
    )
  )
})

test_that("status_dependencies() with more than one direct dependency (2)", {
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
  expect_equal(
    status_dependencies(
      packages = c("mirai", "nanonext"),
      meta = meta,
      verbose = FALSE
    ),
    list(
      crew = list(mirai = character(0L), nanonext = "mirai"),
      x = list(mirai = character(0L), nanonext = "mirai"),
      crew.aws.batch = list(
        mirai = c("crew", "x"),
        nanonext = c("crew", "x")
      ),
      crew.cluster = list(
        mirai = "crew",
        nanonext = "crew"
      ),
      mirai = list(nanonext = character(0L))
    )
  )
})
