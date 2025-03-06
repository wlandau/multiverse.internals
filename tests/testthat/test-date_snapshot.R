test_that("date_snapshot()", {
  quarter <- function(months, old, new) {
    first <- c("01", "14")
    last <- c("15", "16", "28")
    full <- c(first, last)
    for (day in full) {
      expect_equal(
        date_snapshot(today = sprintf("2025-%s-%s", months[1L], day)),
        old
      )
    }
    for (day in first) {
      expect_equal(
        date_snapshot(today = sprintf("2025-%s-%s", months[2L], day)),
        old
      )
    }
    for (day in last) {
      expect_equal(
        date_snapshot(today = sprintf("2025-%s-%s", months[2L], day)),
        new
      )
    }
    for (day in full) {
      expect_equal(
        date_snapshot(today = sprintf("2025-%s-%s", months[3L], day)),
        new
      )
    }
  }
  quarter(c("01", "02", "03"), "2024-12-15", "2025-03-15")
  quarter(c("04", "05", "06"), "2025-03-15", "2025-06-15")
  quarter(c("07", "08", "09"), "2025-06-15", "2025-09-15")
  quarter(c("10", "11", "12"), "2025-09-15", "2025-12-15")
})
