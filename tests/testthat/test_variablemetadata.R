testthat::test_that("var_dt variables need not belong to any variable set", {
  vm <- vame::VariableMetadata(
    var_dt = data.table::data.table(
      var_nm = c("a", "b", "c"),
      type = c(
        "categorical",
        "categorical",
        "categorical"
      )
    ),
    var_set_dt = data.table::data.table(
      id = c("ab"),
      var_nm_set = list(
        ab = c("a", "b")
      ),
      value_space = list(
        ab = list(
          dt = data.table::data.table(
            a = c(1L, 2L, 2L),
            b = c(11L, 21L, 22L)
          )
        )
      )
    )
  )
  testthat::expect_identical(
    unname(vm@var_meta_get_all("var_nm")),
    c("a", "b", "c")
  )
  testthat::expect_identical(
    unname(vm@var_var_set_dt_id_set_get("a")),
    "ab"
  )
  testthat::expect_length(vm@var_var_set_dt_id_set_get("c"), 0L)
  testthat::expect_length(vm@var_var_set_dt_pos_set_get("c"), 0L)
  testthat::expect_identical(
    vm@var_value_space_eval("a")[["dt"]][["a"]],
    1:2
  )
  testthat::expect_error(
    vm@var_value_space_eval("c"),
    "No value_space defined"
  )
  vm@var_assert(x = 1:2, var_nm = "a")

  # removing a variable set removes variables which lose their last set,
  # but variables which never had a set are kept
  vm@var_set_remove(id = "ab")
  testthat::expect_identical(
    unname(vm@var_meta_get_all("var_nm")),
    "c"
  )
  testthat::expect_length(vm@var_set_meta_get_all("id"), 0L)

  # variables in var_set_dt but not in var_dt are still dropped from sets
  vm <- vame::VariableMetadata(
    var_dt = data.table::data.table(var_nm = c("a", "c")),
    var_set_dt = data.table::data.table(
      id = c("ab", "d"),
      var_nm_set = list(c("a", "b"), "d")
    )
  )
  testthat::expect_identical(
    unname(vm@var_meta_get_all("var_nm")),
    c("a", "c")
  )
  testthat::expect_identical(unname(vm@var_set_meta_get_all("id")), "ab")
  testthat::expect_identical(vm@var_set_meta_get("ab", "var_nm_set"), "a")
})
