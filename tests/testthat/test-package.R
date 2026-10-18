test_that("package metadata can be read", {
  desc <- utils::packageDescription("mipR")

  expect_identical(desc$Package, "mipR")
  expect_identical(desc$License, "MIT + file LICENSE")
})
