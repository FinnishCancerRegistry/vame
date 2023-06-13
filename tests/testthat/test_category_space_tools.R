testthat::test_that("category_space_dt_list_to_category_space_dt__ works", {
  dtl <- list(
    data.table::data.table(a = 1:3, b = 3:1),
    data.table::CJ(a = 1:5, c = 2:6),
    data.table::data.table(d = 5:6)
  )
  obs <- category_space_dt_list_to_category_space_dt__(dtl)
  exp <- data.table::copy(dtl[[2]])
  exp[i = dtl[[1]], on = "a", j = "b" := i.b]
  exp <- exp[rep(seq_len(.N), each = 2L), ]
  exp[j = "d" := rep(5:6, .N/2)]
  data.table::setcolorder(exp, names(obs))
  data.table::setkeyv(obs, names(obs))
  data.table::setkeyv(exp, names(exp))
  testthat::expect_equal(obs, exp, ignore_attr = TRUE)

  dtl <- list(
    data.table::data.table(a = 1:5, b = 5:1),
    data.table::data.table(c = 2:6, d = 6:2)
  )
  obs <- category_space_dt_list_to_category_space_dt__(dtl)
  exp <- data.table::CJ(seq_len(nrow(dtl[[1]])), seq_len(nrow(dtl[[2]])))
  data.table::setnames(exp, c("i1", "i2"))
  exp[j = names(dtl[[1]]) := dtl[[1]][exp[["i1"]], ]]
  exp[j = names(dtl[[2]]) := dtl[[2]][exp[["i2"]], ]]
  exp[j = c("i1", "i2") := NULL]
  testthat::expect_equal(obs, exp, ignore_attr = TRUE)
})
