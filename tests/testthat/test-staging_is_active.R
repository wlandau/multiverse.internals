test_that("staging_is_active()", {
  thresholds <- c("01-15", "04-15", "07-15", "10-15")
  active <- c(
    "2024-01-15",
    "2024-01-16",
    "2024-02-13",
    "2024-04-15",
    "2024-04-25",
    "2024-05-14",
    "2024-07-15",
    "2024-08-12",
    "2024-08-13",
    "2024-10-15",
    "2024-11-01",
    "2024-11-13"
  )
  for (today in active) {
    expect_true(
      staging_is_active(
        thresholds = thresholds,
        today = today
      )
    )
  }
  inactive <- c(
    "2024-01-12",
    "2024-02-15",
    "2024-04-14",
    "2024-05-15",
    "2024-07-12",
    "2024-08-15",
    "2024-10-14",
    "2024-11-15"
  )
  for (today in inactive) {
    expect_false(
      staging_is_active(
        thresholds = thresholds,
        today = today
      )
    )
  }
})
