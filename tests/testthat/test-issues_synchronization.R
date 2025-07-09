test_that("issues_synchronization()", {
  meta <- mock_meta_packages
  meta$published[c(2L, 5L)] <- format_time_stamp(Sys.time())
  issues <- suppressMessages(issues_synchronization(meta, verbose = TRUE))
  issues <- issues[issues$synchronization == "recent", ]
  expect_true(is.data.frame(issues))
  expect_equal(dim(issues), c(2L, 2L))
  expect_equal(sort(issues$package), sort(c("adbcdrivermanager", "arrow")))
  expect_equal(issues$synchronization, rep("recent", 2L))
})
