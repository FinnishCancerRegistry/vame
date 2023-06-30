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

  dt_01 <- data.table::CJ(a = 1:3, b = 3:1, c = 1:3)
  dt_02 <- data.table::CJ(a = 0:1, e = 2:1)
  vd <- vame::VariableMetadata(
    var_dt = data.table::data.table(
      var_nm = c("a", "b", "c", "e"),
      type = "categorical",
      label_dt = list(
        a = data.table::data.table(
          level = 0:3,
          en = paste0("a_level_", 0:3)
        ),
        b = NULL,
        c = NULL,
        e = data.table::data.table(
          level = 1:2,
          en = paste0("e_level_", 1:2)
        )
      )
    ),
    var_set_dt = data.table::data.table(
      id = c("set_01", "set_02"),
      var_nm_set = list(c("a", "b", "c"), c("a", "e")),
      value_space = list(
        list(dt = dt_01),
        list(dt = dt_02)
      )
    )
  )

  obs <- vd@vame_category_space_dt(c("a", "b", "e"))
  exp <- data.table::data.table(
    a = c(0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L),
    b = c(NA, NA, 1L, 1L, 2L, 2L, 3L, 3L, 1L, 2L, 3L, 1L, 2L, 3L),
    e = c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, NA, NA, NA, NA, NA, NA)
  )
  data.table::setkeyv(obs, names(obs))
  data.table::setkeyv(exp, names(exp))
  testthat::expect_equal(object = obs, expected = exp, ignore_attr = TRUE)
})

testthat::test_that("VariableMetadata labels work", {
  dt_01 <- data.table::CJ(a = 1:3, b = 3:1, c = 4:5)
  vd <- vame::VariableMetadata(
    var_dt = data.table::data.table(
      var_nm = c("a", "b", "c"),
      type = "categorical",
      label_dt = list(
        a = data.table::data.table(
          level = 1:3,
          en = paste0("a_level_", 1:3)
        ),
        b = data.table::data.table(
          level = 1:3,
          en = paste0("b_level_", 1:3)
        ),
        c = NULL
      )
    ),
    var_set_dt = data.table::data.table(
      id = c("set_01"),
      var_nm_set = list(c("a", "b", "c")),
      value_space = list(
        list(dt = dt_01)
      )
    )
  )

  obs <- vd@var_labels_get(x = 1:4, var_nm = "a", label_col_nm = "en")
  exp <- c(paste0("a_level_", 1:3), NA)
  testthat::expect_identical(object = obs, expected = exp)

  testthat::expect_error(
    vd@var_labels_get(
      x = 1:4,
      var_nm = "a",
      label_col_nm = "this does not exist"
    ),
    regexp = "Label column does not exist: "
  )

  testthat::expect_error(
    vd@var_labels_get(
      x = 1:4,
      var_nm = "c",
      label_col_nm = "en"
    ),
    regexp = "Variable \"c\" has no label_dt defined"
  )
})
