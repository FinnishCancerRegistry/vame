value_space_to_subset_dt__ <- function(
  value_space,
  var_nms,
  env,
  assertion_type = "prod_input"
) {
  assert_is_value_space(value_space, assertion_type = assertion_type)
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
  } else if ("set" %in% names(value_space)) {
    dbc::assert_prod_interim_has_length(var_nms, expected_length = 1L)
    dt <- data.table::data.table(x = value_space[["set"]])
    data.table::setnames(dt, "x", var_nms)
    data.table::setkeyv(dt, var_nms)
  } else if ("bounds" %in% names(value_space)) {
    # @codedoc_comment_block news("vm@vame_category_space_dt_list", "2023-07-04", "0.1.2")
    # @codedoc_comment_block news("vm@vame_category_space_dt", "2023-07-04", "0.1.2")
    # Also `bounds` value space can be used when creating category spaces.
    # @codedoc_comment_block news("vm@vame_category_space_dt", "2023-07-04", "0.1.2")
    # @codedoc_comment_block news("vm@vame_category_space_dt_list", "2023-07-04", "0.1.2")

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
  vm,
  var_nms,
  env,
  assertion_type = "prod_input"
) {
  dbc::assert_is_character_nonNA_vector(
    var_nms,
    assertion_type = assertion_type
  )
  dbc::assert_is_environment(
    env,
    assertion_type = assertion_type
  )

  meta_dt <- local({
    id <- NULL
    var_nms <- var_nms
    handle_arg_ids_et_var_nms_inplace__(
      vm = vm,
      ids_arg_nm = "id",
      var_nms_arg_nm = "var_nms",
      required_var_set_meta_nms = "value_space"
    )
    meta_dt <- data.table::data.table(
      id = id
    )
    data.table::set(
      meta_dt,
      j = "var_nm_set",
      value = lapply(meta_dt[["id"]], function(id) {
        var_set_meta_get(vm, id = id, meta_nm = "var_nm_set")
      })
    )
    meta_dt[]
  })

  dtl <- lapply(seq_along(meta_dt[["id"]]), function(i) {
    id_i <- meta_dt[["id"]][i]
    var_nms_i <- intersect(var_nms, meta_dt[["var_nm_set"]][[i]])
    vs_i <- var_set_value_space_eval(
      vm = vm,
      id = id_i,
      var_nms = var_nms_i,
      env = env
    )
    value_space_to_subset_dt__(
      value_space = vs_i,
      var_nms = var_nms_i,
      env = env
    )
  })
  names(dtl) <- meta_dt[["id"]]
  # @codedoc_comment_block news("vm@vame_category_space_dt_list", "2025-08-13", "1.12.0")
  # `vm@vame_category_space_dt_list` now drops duplicated tables in output,
  # sorts from most complex to least complex (number of columns), and sets names
  # corresponding to the `var_set_dt$id` values.
  # @codedoc_comment_block news("vm@vame_category_space_dt_list", "2025-08-13", "1.12.0")
  dtl[vapply(dtl, is.null, logical(1L))] <- NULL
  lapply(dtl, data.table::setDF)
  dtl[duplicated(dtl)] <- NULL
  lapply(dtl, data.table::setDT)
  dtl <- dtl[order(-vapply(dtl, ncol, integer(1L)))]
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
  # @codedoc_comment_block news("vm@vame_category_space_dt", "2025-08-13", "1.12.0")
  # `vm@vame_category_space_dt` now merges multiple separate `data.table`
  # objects by starting from the most complex one (most columns).
  # Formerly order was the order of appearance in the `VariableMetadata` object.
  # This avoids the error of e.g. first combining two single-variable
  # `data.table` objects (effectively `data.table::CJ`) and only then their
  # joint `data.table`.
  # @codedoc_comment_block news("vm@vame_category_space_dt", "2025-08-13", "1.12.0")
  dtl <- dtl[order(-vapply(dtl, ncol, integer(1L)))]
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
