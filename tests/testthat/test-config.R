context("config")

test_that("read", {
  cfg <- orderly_config("example")
  expect_is(cfg, "orderly_config")
})
