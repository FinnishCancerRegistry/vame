# data -------------------------------------------------------------------------
data_obj_get <- function(
  vm,
  obj_nm
) {
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  dbc::assert_input_is_character_nonNA_atom(obj_nm)
  data <- environment(vm@var_assert)[["data"]]
  dbc::assert_input_atom_is_in_set(obj_nm, set = names(data))
  data[[obj_nm]]
}
data_obj_set <- function(
  vm,
  obj_nm,
  value
) {
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  dbc::assert_input_is_character_nonNA_atom(obj_nm)
  vm_env <- environment(vm@var_assert)
  dbc::assert_input_atom_is_in_set(
    obj_nm,
    set = names(vm_env[["data"]])
  )
  vm_env[["data"]][[obj_nm]] <- value
}

# assertions -------------------------------------------------------------
assert_is_value_space <- function(
  vm,
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL
) {
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  assertion_type <- dbc::handle_arg_assertion_type(assertion_type)
  dbc::assert_is_one_of(
    x,
    funs = list(dbc::report_is_NULL, dbc::report_is_list)
  )
  if (is.null(x)) {
    return(invisible(NULL))
  }
  dbc::assert_has_length(
    x = x,
    expected_length = 1L,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type
  )
  dbc::assert_atom_is_in_set(
    x = names(x),
    set = value_space_type_names__(),
    x_nm = paste0("names(", x_nm, ")"),
    call = call,
    assertion_type = assertion_type
  )
  dbc::report_to_assertion(
    value_space_type_report_funs__()[[names(x)]](
      x = x,
      x_nm = x_nm,
      call = call
    ),
    raise_error_call = call,
    assertion_type = assertion_type
  )
}
assert_is_var_nm <- function(
  vm,
  var_nm,
  assertion_type = NULL
) {
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  dbc::assert_is_character_nonNA_atom(
    var_nm,
    assertion_type = assertion_type
  )
  dbc::assert_atom_is_in_set(
    var_nm,
    set = var_meta_get_all(vm, "var_nm"),
    assertion_type = assertion_type
  )
}
assert_is_var_meta_nm <- function(
  vm,
  meta_nm,
  assertion_type = NULL
) {
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  dbc::assert_is_character_nonNA_atom(
    meta_nm,
    assertion_type = assertion_type
  )
  dbc::assert_atom_is_in_set(
    meta_nm,
    set = names(vd_get(vm)),
    assertion_type = assertion_type
  )
}
assert_is_var_set_id <- function(
  vm,
  id,
  assertion_type = NULL
) {
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  dbc::assert_is_atom(
    id,
    assertion_type = assertion_type
  )
  dbc::assert_is_nonNA(
    id,
    assertion_type = assertion_type
  )
  dbc::assert_atom_is_in_set(
    id,
    set = vsd_get(vm)[["id"]],
    assertion_type = assertion_type
  )
}
assert_is_var_set_meta_nm <- function(
  vm,
  meta_nm,
  assertion_type = NULL
) {
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  dbc::assert_is_character_nonNA_atom(
    meta_nm,
    assertion_type = assertion_type
  )
  dbc::assert_atom_is_in_set(
    meta_nm,
    set = names(vsd_get(vm)),
    assertion_type = assertion_type
  )
}
assert_var_set_value_space_is_defined <- function(
  vm
) {
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  if (!var_set_value_space_is_defined(vm)) {
    stop("No value spaces have been defined")
  }
}
assert_is_var_label_dt <- function(
  vm,
  value,
  assertion_type = NULL
) {
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  dbc::assert_is_one_of(
    value,
    funs = list(
      dbc::report_is_data_table,
      dbc::report_is_NULL
    ),
    assertion_type = assertion_type
  )
  if (data.table::is.data.table(value)) {
    dbc::assert_has_names(
      value,
      required_names = "level",
      assertion_type = assertion_type
    )
  }
}

# vd funs ----------------------------------------------------------------
vd_get <- function(
  vm,
  var_nms = NULL
) {
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  out <- data_obj_get(vm, "var_dt")
  if (is.null(var_nms)) {
    var_nms <- names(out)
  }
  out <- dt_independent_frame_dependent_contents__(out, var_nms)
  return(out[])
}
vd_set <- function(
  vm,
  dt
) {
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  dbc::assert_prod_input_is_data_table(dt)
  data_obj_set(vm, "var_dt", dt)
}
vd_implied_get <- function(
  vm
) {
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  vsd <- vsd_get(vm)
  dt <- data.table::data.table(
    var_nm = unique(unlist(vsd[["var_nm_set"]]))
  )
  if (nrow(dt) == 0L) {
    return(data.table::data.table(
      var_nm = character(0L),
      var_set_dt_pos_set = integer(0L)
    ))
  }
  
  dt <- data.table::data.table(
    var_nm = unlist(vsd[["var_nm_set"]]),
    var_set_dt_pos_set = unlist(lapply(
      seq_along(vsd[["var_nm_set"]]),
      function(i) {
        rep(i, length(vsd[["var_nm_set"]][[i]]))
      }
    ))
  )
  #' @importFrom data.table .SD
  dt <- dt[
    j = list("var_set_dt_pos_set" = list(.SD[["var_set_dt_pos_set"]])),
    keyby = "var_nm"
  ]
  if ("var_nm" %in% names(dt)) {
    data.table::setkeyv(dt, "var_nm")
  }
  return(dt[])
}

# vsd funs ---------------------------------------------------------------
vsd_get <- function(
  vm,
  var_nms = NULL
) {
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  out <- data_obj_get(vm, "var_set_dt")
  if (!is.null(var_nms)) {
    dbc::assert_vector_elems_are_in_set(var_nms, set = names(out))
    out <- dt_independent_frame_dependent_contents__(out, var_nms)
  }
  return(out[])
}
vsd_set <- function(
  vm,
  dt
) {
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  dbc::assert_prod_input_is_data_table(dt)
  data_obj_set(vm, "var_set_dt", dt)
}

# vd_vsd funs ------------------------------------------------------------
vd_vsd_linkage_refresh <- function(
  vm
) {
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  vd <- vd_get(vm)
  if (nrow(vd) == 0L) {
    return(invisible(NULL))
  }
  vdi <- vd_implied_get(vm)
  i.var_set_dt_pos_set <- NULL # appease R CMD CHECK
  #' @importFrom data.table :=
  vd[
    i = vdi,
    on = "var_nm",
    j = "var_set_dt_pos_set" := i.var_set_dt_pos_set
  ]
  data.table::setkeyv(vd, "var_nm")
  vd_set(vm, vd)
  return(invisible(NULL))
}
vd_vsd_intersect <- function(
  vm
) {
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  vd <- vd_get(vm)
  vdi <- vd_implied_get(vm)
  rm_var_nms <- union(
    setdiff(
      vd[["var_nm"]],
      vdi[["var_nm"]]
    ),
    setdiff(
      vdi[["var_nm"]],
      vd[["var_nm"]]
    )
  )
  if (length(rm_var_nms) > 0) {
    vd_subset <- !vd[["var_nm"]] %in% rm_var_nms
    vd <- vd[vd_subset, ]
    vd_set(vm, vd)

    vsd <- vsd_get(vm)
    #' @importFrom data.table :=
    vsd[
      j = "var_nm_set" := list(
        var_nm_set = lapply(.SD[["var_nm_set"]], setdiff, y = rm_var_nms)
      )
    ]
    vsd_subset <- vapply(vsd[["var_nm_set"]], length, integer(1L)) > 0L
    vsd <- vsd[vsd_subset, ]
    if ("value_space" %in% names(vsd)) {
      vsd[
        j = "value_space" := list(
          #' @importFrom data.table .N
          value_space = lapply(seq_len(.N), function(i) {
            vs_i <- .SD[["value_space"]][[i]]
            if ("dt" %in% names(vs_i)) {
              dt_i <- vs_i[["dt"]]
              keep_nms_i <- setdiff(names(dt_i), rm_var_nms)
              keep_dt_i <- dt_i[
                i = !duplicated(dt_i, by = keep_nms_i, fromLast = TRUE),
                j = .SD,
                .SDcols = keep_nms_i
              ]
              vs_i[["dt"]] <- keep_dt_i
            }
            vs_i
          })
        )
      ]
    }
    vsd_set(vm, vsd)
  }
  vd_vsd_linkage_refresh(vm)
}

# var_nm_set funs --------------------------------------------------------
var_set_id_to_pos <- function(
  vm,
  id
) {
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  dt <- data.table::setDT(list(id = var_set_meta_get_all(vm, "id")))
  jdt <- data.table::setDT(list(id = id))
  out <- dt[
    i = jdt,
    on = "id",
    which = TRUE
  ]
  return(out)
}
var_set_pos_to_id <- function(
  vm,
  pos
) {
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  vsd_get(vm)[["id"]][pos]
}
var_to_var_set_id <- function(
  vm,
  var_nm
) {
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  pos <- var_meta_get(vm, var_nm = var_nm, meta_nm = "var_set_dt_pos_set")
  return(var_set_pos_to_id(vm, pos))
}

# var_set_value_space funs -----------------------------------------------
var_set_value_space_is_defined <- function(
  vm
) {
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  vsd <- vsd_get(vm)
  return("value_space" %in% names(vsd))
}

var_set_value_set_dt_subset_expr <- function(
  vm,
  id,
  expr
) {
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  assert_var_set_value_space_is_defined(vm)
  dbc::assert_is_language_object(expr, assertion_type = "prod_input")
  vs <- var_set_value_space_get(vm, id)
  dt <- vs[["dt"]]
  if (!data.table::is.data.table(dt)) {
    stop("Value space for id = \"", id, "\"  is not a data.table.")
  }
  dt_expr <- substitute(dt[i = expr], list(expr = expr))
  dt <- eval(dt_expr)
  var_set_value_space_set(vm, id, dt)
  return(invisible(NULL))
}

# var funs ---------------------------------------------------------------------
var_is_aggregateable_to__ <- function(
  vm,
  from_var_nm,
  to_var_nm,
  dt
) {
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  stopifnot(
    identical(sort(names(dt)), sort(c(from_var_nm, to_var_nm)))
  )
  if (from_var_nm == to_var_nm) {
    return(TRUE)
  }
  from_type <- var_meta_get(vm, from_var_nm, "type")
  to_type <- var_meta_get(vm, to_var_nm, "type")
  if (from_type != to_type || from_type != "categorical") {
    return(FALSE)
  }
  from_pos_set <- var_meta_get(vm, from_var_nm, "var_set_dt_pos_set")
  to_pos_set <- var_meta_get(vm, to_var_nm, "var_set_dt_pos_set")
  if (length(intersect(from_pos_set, to_pos_set)) == 0) {
    return(FALSE)
  }
  sum(duplicated(dt[[from_var_nm]])) == 0L
}

vame_subset_expr <- function(
  vm,
  expr
) {
  # TODO: rename expr -> var_dt_expr, maybe add var_set_dt_expr
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  dbc::assert_is_language_object(expr, assertion_type = "prod_input")
  vd <- vd_get(vm)
  vd <- eval(
    substitute(vd[i = expr], list(expr = expr))
  )
  vd_set(vm, vd)
  vd_vsd_intersect(vm)
}
