value_space_to_subset_dt <- function(value_space, var_nms, env) {
  if (length(var_nms) == 0L) {
    return(data.table::data.table(NULL)[])
  } else if ("dt" %in% names(value_space)) {
    dt <- value_space[["dt"]]
    dt <- dt[
      i = !duplicated(dt, by = var_nms),
      j = .SD,
      .SDcols = var_nms
    ]
  } else if ("expr" %in% names(value_space)) {
    expr_eval_env <- new.env(parent = env)
    expr_eval_env[["var_nms"]] <- var_nms
    dt <- eval(value_space[["expr"]], envir = expr_eval_env)
  } else {
    stop("A value_space did not have either \"dt\" or \"expr\" element. ",
         "Could not evaluate category space for ",
         "variable names ", deparse1(var_nms), ".")
  }
  data.table::setcolorder(dt, var_nms)
  dbc::assert_prod_output_is_data_table_with_required_names(
    dt,
    required_names = var_nms
  )
  return(dt[])
}

category_space_dt_list_to_category_space_dt <- function(dtl) {
  if (length(dtl) == 0L) {
    return(data.table::data.table(NULL))
  }
  dt <- do.call(
    what = data.table::CJ,
    args = lapply(dtl, function(dt) seq_len(nrow(dt))),
    quote = TRUE
  )
  data.table::setnames(dt, paste0("_________", seq_along(dtl)))
  for (i in seq_along(dtl)) {
    data.table::set(
      dt,
      j = names(dtl[[i]]),
      value = dtl[[i]][i = dt[[1]], j = .SD, .SDcols = names(dtl[[i]])]
    )
    data.table::set(
      dt,
      j = names(dt)[1],
      value = NULL
    )
  }
  return(dt[])
}