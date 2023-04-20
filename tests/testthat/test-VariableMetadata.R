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
})

testthat::test_that("category_space funs work", {
  dt_01 <- data.table::CJ(a = 1:3, b = 3:1, c = 1:3)
  dt_02 <- data.table::CJ(d = 1:2, e = 2:1)
  vd <- vame::VariableMetadata(
    var_dt = data.table::data.table(
      var_nm = c("a", "b", "c", "d", "e"),
      type = "categorical"
    ),
    var_set_dt = data.table::data.table(
      id = c("set_01", "set_02"),
      var_nm_set = list(c("a", "b", "c"), c("d", "e")),
      value_space = list(
        list(dt = dt_01),
        list(expr = quote({
          dt_02[
            i = !duplicated(dt_02, by = var_nms),
            j = .SD,
            .SDcols = var_nms
          ]
        }))
      )
    )
  )

  testthat::expect_equal(
    vd@vame_category_space_dt(c("a", "b")),
    dt_01[
      i = !duplicated(dt_01, by = c("a", "b")),
      j = .SD,
      .SDcols = c("a", "b")
    ],
    ignore_attr = TRUE
  )
  testthat::expect_equal(
    vd@vame_category_space_dt(c("d", "e")),
    dt_02,
    ignore_attr = TRUE
  )
})
