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
  } else if ("set" %in% names(value_space)) {
    dbc::assert_prod_interim_has_length(var_nms, expected_length = 1L)
    dt <- data.table::data.table(x = value_space[["set"]])
    data.table::setnames(dt, "x", var_nms)
    data.table::setkeyv(dt, var_nms)
  } else if ("bounds" %in% names(value_space)) {
    dbc::assert_prod_interim_has_length(var_nms, expected_length = 1L)
    lo <- value_space[["bounds"]][["lo"]]
    hi <- value_space[["bounds"]][["hi"]]
    if (!value_space[["bounds"]][["lo_inclusive"]]) {
      lo <- lo + 1L
    }
    if (!value_space[["bounds"]][["hi_inclusive"]]) {
      hi <- hi - 1L
    }
    dt <- data.table::data.table(x = lo:hi)
    data.table::setnames(dt, "x", var_nms)
    data.table::setkeyv(dt, var_nms)
  } else {
    stop("A value_space did not have a \"dt\", \"expr\", \"set\", or ",
         "\"bounds\" element. Could not evaluate category space for ",
         "variable name(s) ", deparse1(var_nms), ".")
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
  dtl <- lapply(seq_along(vsd[["value_space"]]), function(i) {
    var_nm_set <- intersect(var_nms, vsd[["var_nm_set"]][[i]])
    if (length(var_nm_set) == 0) {
      return(NULL)
    }
    value_space_to_subset_dt__(
      value_space = vsd[["value_space"]][[i]],
      var_nms = var_nm_set,
      env = env
    )
  })
  dtl[vapply(dtl, is.null, logical(1L))] <- NULL
  return(dtl)
}

assert_is_category_space_dt_list__ <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = dbc::assertion_type_default()
) {
  x_nm <- dbc::handle_arg_x_nm(x_nm = x_nm)
  call <- dbc::handle_arg_call(call = call)
  dbc::assert_is_list(
    x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type
  )
  lapply(seq_along(x), function(i) {
    dbc::assert_is_data_table(
      x[[i]],
      x_nm = paste0(x_nm, "[[", i, "]]"),
      call = call,
      assertion_type = assertion_type
    )
  })
  return(invisible(NULL))
}

category_space_dt_list_to_category_space_dt__ <- function(
  dtl,
  assertion_type = "prod_input"
) {
  # this could do with some optimisation. the loop takes many copies of the
  # data.
  assert_is_category_space_dt_list__(dtl, assertion_type = assertion_type)
  if (length(dtl) == 0L) {
    return(data.table::data.table(NULL))
  } else if (length(dtl) == 1L) {
    return(dtl[[1L]])
  }
  dt <- dtl[[1L]]
  for (i in 2:length(dtl)) {
    join_col_nms <- intersect(names(dt), names(dtl[[i]]))
    if (length(join_col_nms) > 0) {
      dt <- merge(
        x = dt,
        y = dtl[[i]],
        by = join_col_nms,
        all.x = TRUE,
        all.y = TRUE,
        allow.cartesian = TRUE
      )
    } else {
      dt_indices <- rep(seq_len(nrow(dt)), each = nrow(dtl[[i]]))
      dtli_indices <- rep(seq_len(nrow(dtl[[i]])), nrow(dt))
      dt <- dt[dt_indices, ]
      data.table::set(dt, j = names(dtl[[i]]), value = dtl[[i]][dtli_indices, ])
    }
  }
  is_duplicated <- duplicated(dt, by = names(dt))
  if (any(is_duplicated)) {
    dt <- subset(dt, !is_duplicated)
  }
  return(dt[])
}