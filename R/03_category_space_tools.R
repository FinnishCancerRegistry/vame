value_space_to_subset_dt__ <- function(
  value_space,
  var_nms,
  env,
  assertion_type = "prod_input"
) {
  dbc::assert_is_list(
    value_space,
    assertion_type = assertion_type
  )
  dbc::assert_is_character_nonNA_vector(
    var_nms,
    assertion_type = assertion_type
  )
  dbc::assert_is_environment(
    env,
    assertion_type = assertion_type
  )
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

category_space_dt_list__ <- function(
  var_nms,
  vsd,
  env,
  assertion_type = "prod_input"
) {
  dbc::assert_is_character_nonNA_vector(
    var_nms,
    assertion_type = assertion_type
  )
  dbc::assert_is_data_table_with_required_names(
    vsd,
    required_names = c("id", "var_nm_set", "value_space"),
    assertion_type = assertion_type
  )
  dbc::assert_is_environment(
    env,
    assertion_type = assertion_type
  )
  dtl <- lapply(seq_along(vsd[["value_spaces"]]), function(i) {
    var_nm_set <- intersect(var_nms, vsd[["var_nm_sets"]][[i]])
    if (length(var_nm_set) == 0) {
      return(NULL)
    }
    value_space_to_subset_dt__(
      value_space = vsd[["value_spaces"]][[i]],
      var_nms = var_nms,
      env = env
    )
  })
  dtl[vapply(dtl, is.null, logical(1L))] <- NULL
  return(dtl)
}

category_space_dt_list_to_category_space_dt__ <- function(
  dtl,
  assertion_type = "prod_input"
  ) {
  dbc::assert_is_list(
    dtl,
    assertion_type = assertion_type
  )
  if (length(dtl) == 0L) {
    return(data.table::data.table(NULL))
  }
  index_dt <- do.call(
    what = data.table::CJ,
    args = lapply(dtl, function(dt) seq_len(nrow(dt))),
    quote = TRUE
  )
  index_col_nms <- paste0("index_", seq_len(ncol(index_dt)))
  data.table::setnames(dt, index_col_nms)
  dt <- data.table::data.table(
    "____tmp____" = rep(TRUE, nrow(index_dt))
  )
  for (i in seq_along(dtl)) {
    join_col_nms <- intersect(names(dt), names(dtl[[i]]))
    if (length(join_col_nms) > 0) {
      dt <- merge(
        dt,
        dtl[[i]],
        on = join_col_nms,
        all.x = TRUE,
        all.y = TRUE,
        allow.cartesian = TRUE
      )
    } else {
      indices <- dt[[index_col_nms[i]]]
      new_col_nms <- names(dtl[[i]])
      data.table::set(
        dt,
        j = new_col_nms,
        value = dtl[[i]][i = indices, j = .SD, .SDcols = new_col_nms]
      )
    }
    data.table::set(
      dt,
      j = index_col_nms[i],
      value = NULL
    )
    if (i == 1) {
      data.table::set(dt, j = "____tmp____", value = NULL)
    }
  }
  is_duplicated <- duplicated(dt, by = names(dt))
  if (any(is_duplicated)) {
    dt <- subset(dt, !is_duplicated)
  }
  return(dt[])
}