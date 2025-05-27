test_that("record_status() mocked", {
  status <- tempfile()
  nonstandard <- tempfile()
  record_status(
    versions = mock_versions(),
    mock = list(packages = mock_meta_packages),
    output = status,
    verbose = FALSE
  )
  record_nonstandard_licenses(
    path_nonstandard_licenses = nonstandard,
    path_status = status
  )
  out <- jsonlite::read_json(nonstandard, simplifyVector = TRUE)
  expect_equal(
    sort(out$package),
    sort(c("colorout", "INLA", "Rvision"))
  )
  expect_equal(out$license, rep("NOT FOUND", 3L))
})
