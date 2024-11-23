test_that("is_member_organization()", {
  expect_true(is_member_organization("wlandau", c("r-multiverse", "none1")))
  expect_false(is_member_organization("wlandau", c("none1", "none2")))
})
