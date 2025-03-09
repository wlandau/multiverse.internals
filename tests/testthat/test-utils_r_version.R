test_that("r_version_list()", {
  expect_true(is.data.frame(r_version_list()))
})

test_that("r_version_short()", {
  expect_equal(r_version_short(c("4.3.2", "7.8.9")), c("4.3", "7.8"))
})
