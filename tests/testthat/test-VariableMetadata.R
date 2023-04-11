testthat::test_that("VariableMetadata funs work", {
  vd <- VariableMetadata(
    var_nm_dt = data.table::data.table(
      var_nm = c("a", "b", "c"),
      flavour = c("tasty", "rancid", "bitter")
    ),
    var_nm_set_dt = data.table::data.table(
      var_nm_set = list(c("a", "b"))
    )
  )
  vd@rename("a", "A")
  testthat::expect_identical(vd@meta_get("A", "flavour"), "tasty")
})
