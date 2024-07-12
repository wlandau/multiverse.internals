test_that("promote_packages()", {
  dir_production <- tempfile()
  dir_community <- tempfile()
  path_production <- file.path(dir_production, "production")
  path_community <- file.path(dir_community, "community")
  dir.create(dir_production)
  dir.create(dir_community)
  on.exit(unlink(path_production, recursive = TRUE))
  on.exit(unlink(path_community, recursive = TRUE), add = TRUE)
  mock <- system.file(
    "mock",
    package = "multiverse.internals",
    mustWork = TRUE
  )
  file.copy(
    from = file.path(mock, "production"),
    to = dir_production,
    recursive = TRUE
  )
  file.copy(
    from = file.path(mock, "community"),
    to = dir_community,
    recursive = TRUE
  )
  json <- jsonlite::read_json(file.path(path_community, "packages.json"))
  names <- vapply(
    json,
    function(x) x$package,
    FUN.VALUE = character(1L)
  )
  meta_community <- data.frame(
    package = names,
    remotesha = paste0("sha-", names)
  )
  promote_packages(
    path_production = path_production,
    path_community = path_community,
    meta = meta_community
  )
  packages <- jsonlite::read_json(
    file.path(path_production, "packages.json"),
    simplifyVector = TRUE
  )
  expect_true(is.data.frame(packages))
  expect_equal(dim(packages), c(6L, 3L))
  expect_equal(
    sort(packages$package),
    sort(
      c(
        "community-good", "community-notice", "community-remove",
        "production-good", "production-notice", "production-remove"
      )
    )
  )
  expect_equal(
    packages$url,
    file.path("https://github.com/owner", packages$package)
  )
  expect_equal(packages$branch, paste0("sha-", packages$package))
  removing <- jsonlite::read_json(
    file.path(path_production, "removing.json"),
    simplifyVector = TRUE
  )
  expect_equal(
    sort(removing),
    sort(c("production-removed", "production-removing"))
  )
})
# 
# test_that("clear_removed()", {
#   dir_production <- tempfile()
#   path_production <- file.path(dir_production, "production")
#   dir.create(dir_production)
#   on.exit(unlink(path_production, recursive = TRUE))
#   mock <- system.file(
#     "mock",
#     package = "multiverse.internals",
#     mustWork = TRUE
#   )
#   file.copy(
#     from = file.path(mock, "production"),
#     to = dir_production,
#     recursive = TRUE
#   )
#   json <- jsonlite::read_json(file.path(path_production, "packages.json"))
#   names <- vapply(
#     json,
#     function(x) x$package,
#     FUN.VALUE = character(1L)
#   )
#   names <- c(names, "production-removing")
#   meta_production <- data.frame(
#     package = names,
#     remotesha = paste0("sha-", names)
#   )
#   clear_removed(
#     path_production = path_production,
#     meta_production = meta_production
#   )
#   removing <- jsonlite::read_json(
#     file.path(path_production, "removing.json"),
#     simplifyVector = TRUE
#   )
#   expect_equal(removing, "production-removing")
# })
# 
# test_that("demote_packages()", {
#   dir_production <- tempfile()
#   path_production <- file.path(dir_production, "production")
#   dir.create(dir_production)
#   on.exit(unlink(path_production, recursive = TRUE))
#   mock <- system.file(
#     "mock",
#     package = "multiverse.internals",
#     mustWork = TRUE
#   )
#   file.copy(
#     from = file.path(mock, "production"),
#     to = dir_production,
#     recursive = TRUE
#   )
#   demote_packages(
#     path_production = path_production,
#     days_notice = 28L
#   )
#   packages <- jsonlite::read_json(
#     file.path(path_production, "packages.json"),
#     simplifyVector = TRUE
#   )
#   expect_equal(dim(packages), c(2L, 3L))
#   expect_equal(
#     sort(packages$package),
#     sort(c("production-good", "production-notice"))
#   )
#   expect_equal(
#     packages$url,
#     file.path("https://github.com/owner", packages$package)
#   )
#   expect_equal(packages$branch, paste0("sha-", packages$package))
#   removing <- jsonlite::read_json(
#     file.path(path_production, "removing.json"),
#     simplifyVector = TRUE
#   )
#   expect_equal(
#     sort(removing),
#     sort(c("production-remove", "production-removed", "production-removing"))
#   )
# })
#
# test_that("update_production()", {
#   dir_production <- tempfile()
#   dir_community <- tempfile()
#   path_production <- file.path(dir_production, "production")
#   path_community <- file.path(dir_community, "community")
#   dir.create(dir_production)
#   dir.create(dir_community)
#   on.exit(unlink(path_production, recursive = TRUE))
#   on.exit(unlink(path_community, recursive = TRUE), add = TRUE)
#   mock <- system.file(
#     "mock",
#     package = "multiverse.internals",
#     mustWork = TRUE
#   )
#   file.copy(
#     from = file.path(mock, "production"),
#     to = dir_production,
#     recursive = TRUE
#   )
#   file.copy(
#     from = file.path(mock, "community"),
#     to = dir_community,
#     recursive = TRUE
#   )
#   names_production <- vapply(
#     jsonlite::read_json(file.path(path_production, "packages.json")),
#     function(x) x$package,
#     FUN.VALUE = character(1L)
#   )
#   names_production <- c(names_production, "production-removing")
#   meta_production <- data.frame(
#     package = names_production,
#     remotesha = paste0("sha-", names_production)
#   )
#   names_community <- vapply(
#     jsonlite::read_json(file.path(path_community, "packages.json")),
#     function(x) x$package,
#     FUN.VALUE = character(1L)
#   )
#   meta_community <- data.frame(
#     package = names_community,
#     remotesha = paste0("sha-", names_community)
#   )
#   # Update production source. Updating twice because this case should be
#   # idempotent.
#   for (index in seq_len(2L)) {
#     update_production(
#       path_production,
#       path_community,
#       days_notice = 28L,
#       mock = list(production = meta_production, community = meta_community)
#     )
#     packages <- jsonlite::read_json(
#       file.path(path_production, "packages.json"),
#       simplifyVector = TRUE
#     )
#     expect_equal(dim(packages), c(5L, 3L))
#     expect_equal(
#       sort(packages$package),
#       sort(
#         c(
#           "community-good", "community-notice", "community-remove",
#           "production-good", "production-notice"
#         )
#       )
#     )
#     expect_equal(
#       packages$url,
#       file.path("https://github.com/owner", packages$package)
#     )
#     expect_equal(packages$branch, paste0("sha-", packages$package))
#     removing <- jsonlite::read_json(
#       file.path(path_production, "removing.json"),
#       simplifyVector = TRUE
#     )
#     expect_equal(
#       sort(removing),
#       sort(c("production-remove", "production-removing"))
#     )
#   }
#   # Then the production universe catches up.
#   meta_production <- packages
#   meta_production$remotesha <- meta_production$branch
#   meta_production$remotesha <- NULL
#   meta_production$url <- NULL
#   # community-remove is now in production and needs to be removed.
#   # production-remove and production-removing are were automatically
#   # removed before, so they are free to get promoted again below.
#   # Update the production source again below. This should be idempotent
#   # because the issue files and the universe have not had a chance to catch up
#   # between two updates in quick succession.
#   for (index in seq_along(2L)) {
#     update_production(
#       path_production,
#       path_community,
#       days_notice = 28L,
#       mock = list(production = meta_production, community = meta_community)
#     )
#     packages <- jsonlite::read_json(
#       file.path(path_production, "packages.json"),
#       simplifyVector = TRUE
#     )
#     expect_equal(dim(packages), c(6L, 3L))
#     expect_equal(
#       sort(packages$package),
#       sort(
#         c(
#           "community-good", "community-notice",
#           "production-good", "production-notice",
#           "production-remove", "production-removing"
#         )
#       )
#     )
#     expect_equal(
#       packages$url,
#       file.path("https://github.com/owner", packages$package)
#     )
#     expect_equal(packages$branch, paste0("sha-", packages$package))
#     removing <- jsonlite::read_json(
#       file.path(path_production, "removing.json"),
#       simplifyVector = TRUE
#     )
#     expect_equal(removing, "community-remove")
#   }
#   # Then the production universe catches up, adding
#   # production-remove and production-removing back to production,
#   # but removing community-remove.
#   meta_production <- packages
#   meta_production$remotesha <- meta_production$branch
#   meta_production$remotesha <- NULL
#   meta_production$url <- NULL
#   # On the next update, production-remove and production-removing
#   # should be demoted and marked for removal. community-remove, on the
#   # other hand should be promoted back.
#   update_production(
#     path_production,
#     path_community,
#     days_notice = 28L,
#     mock = list(production = meta_production, community = meta_community)
#   )
#   packages <- jsonlite::read_json(
#     file.path(path_production, "packages.json"),
#     simplifyVector = TRUE
#   )
#   expect_equal(dim(packages), c(5L, 3L))
#   expect_equal(
#     sort(packages$package),
#     sort(
#       c(
#         "community-good", "community-notice", "community-remove",
#         "production-good", "production-notice"
#       )
#     )
#   )
#   expect_equal(
#     packages$url,
#     file.path("https://github.com/owner", packages$package)
#   )
#   expect_equal(packages$branch, paste0("sha-", packages$package))
#   removing <- jsonlite::read_json(
#     file.path(path_production, "removing.json"),
#     simplifyVector = TRUE
#   )
#   expect_equal(
#     sort(removing),
#     sort(c("production-remove", "production-removing"))
#   )
# })
