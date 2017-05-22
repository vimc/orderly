context("config")

test_that("read", {
  cfg <- config_read("example")
  expect_is(cfg, "orderly_config")
})
