testServer(
  mod_egm_source_server,
  args = list(initial_egm = NULL),
  {
    ns <- session$ns
    expect_true(inherits(ns, "function"))
    returned <- session$returned()
    expect_type(returned, "list")
    expect_error(returned$egm())

    stub_egm <- list(signal = list())
    returned$set_egm(stub_egm)
    expect_identical(returned$egm(), stub_egm)
  }
)

testServer(
  mod_egm_source_server,
  args = list(initial_egm = list(initial = TRUE)),
  {
    returned <- session$returned()
    expect_identical(returned$egm(), list(initial = TRUE))
  }
)

test_that("module ui works", {
  ui <- mod_egm_source_ui(id = "test")
  golem::expect_shinytaglist(ui)
  readers <- egm_available_readers()
  expect_true(all(c("muse", "lspro") %in% names(readers)))
})
