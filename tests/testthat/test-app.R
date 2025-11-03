test_that("app ui works", {
  ui <- app_ui()
  expect_s3_class(ui, "shiny.tag.list")
})

test_that("app server works", {
  expect_type(app_server, "closure")
})
