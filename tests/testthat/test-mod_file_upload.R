test_that("module file_upload ui works", {
  ui <- mod_file_upload_ui("test")
  expect_s3_class(ui, "shiny.tag.list")
})

test_that("module file_upload server works", {
  testServer(mod_file_upload_server, {
    # Test that the module returns a reactive
    result <- session$returned()
    expect_true(is.reactive(result))
    expect_null(result())
  })
})
