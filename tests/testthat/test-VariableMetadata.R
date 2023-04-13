testthat::test_that("VariableMetadata funs work", {
  vd <- vame::VariableMetadata(
    var_dt = data.table::data.table(
      var_nm = c("a", "b", "c"),
      flavour = c("tasty", "rancid", "bitter")
    ),
    var_set_dt = data.table::data.table(
      id = "set_01",
      var_nm_set = list(c("a", "b")),
      value_space = list(list(dt = data.table::data.table(
        a = 1:2,
        b = 3:4
      )))
    )
  )

  vd@var_rename("a", "A")
  testthat::expect_identical(
    vd@var_meta_get("A", "flavour"), "tasty"
  )
  testthat::expect_identical(
    names(vd@var_set_value_space_get("set_01")[["dt"]]), c("A", "b")
  )
  testthat::expect_identical(
    vd@var_set_meta_get("set_01", "var_nm_set"), c("A", "b")
  )

  vd@var_set_rename("set_01", "Ab")
  testthat::expect_identical(
    vd@var_set_meta_get_all("id"), "Ab"
  )

  vd@var_remove("b")
  testthat::expect_identical(
    names(vd@var_set_value_space_get("Ab")[["dt"]]), "A"
  )
  testthat::expect_identical(
    vd@var_set_meta_get("Ab", "var_nm_set"), "A"
  )

  vd@var_set_remove("Ab")
  testthat::expect_identical(
    length(vd@var_set_meta_get_all("var_nm_set")), 0L
  )

  vd <- vame::VariableMetadata(
    var_dt = data.table::data.table(
      var_nm = c("a", "b", "c"),
      flavour = c("tasty", "rancid", "bitter")
    ),
    var_set_dt = data.table::data.table(
      id = "set_01",
      var_nm_set = list(c("a", "b"))
    )
  )
  vd@var_rename("a", "A")
  testthat::expect_identical(vd@var_meta_get("A", "flavour"), "tasty")
})
