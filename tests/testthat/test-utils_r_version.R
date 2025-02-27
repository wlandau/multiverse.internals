test_that("r_version_list()", {
  expect_true(is.data.frame(r_version_list()))
})

test_that("r_version_short()", {
  expect_equal(r_version_short(c("4.3.2", "7.8.9")), c("4.3", "7.8"))
})

test_that("r_version_major()", {
  expect_equal(r_version_major(c("4.3.2", "7.8.9")), c(4L, 7L))
})

test_that("r_version_minor()", {
  expect_equal(r_version_minor(c("4.3.2", "7.8.9")), c(3L, 8L))
})

test_that("r_version_patch()", {
  expect_equal(r_version_patch(c("4.3.2", "7.8.9")), c(2L, 9L))
})
