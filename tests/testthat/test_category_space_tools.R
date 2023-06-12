testthat::test_that("category_space_dt_list_to_category_space_dt__ works", {
  dtl <- list(
    data.table::data.table(a = 1:5, b = 5:1),
    data.table::data.table(a = 0:3, c = 3:0)
  )
  testthat::expect_error(
    category_space_dt_list_to_category_space_dt__(dtl),
    regexp = "If a column appears in multiple tables"
  )

  dtl <- list(
    data.table::data.table(a = 1:5, b = 5:1),
    data.table::data.table(a = 1:5, c = 2:6)
  )
  obs <- category_space_dt_list_to_category_space_dt__(dtl)
  exp <- data.table::copy(dtl[[1]])
  exp[i = dtl[[2]], on = "a", j = "c" := i.c]
  testthat::expect_equal(obs, exp)
})
