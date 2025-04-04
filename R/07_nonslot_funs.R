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
    # @codedococ_comment_block news("vm@vame_union_append", "2025-04-04", "1.9.2")
    # Fixed a bug which sometimes caused an issue with `vm@vame_union_append`:
    # `var_dt$var_set_dt_pos_set` could be created as an `integer`
    # column instead of the intended `list` column.
    # @codedococ_comment_block news("vm@vame_union_append", "2025-04-04", "1.9.2")
    return(data.table::data.table(
      var_nm = character(0L),
      var_set_dt_pos_set = list()
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
var_to_var_set_pos <- function(
  vm,
  var_nm,
  style = c("all", "smallest_set")[1]
) {
  stopifnot(style %in%  c("all", "smallest_set"))
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  pos <- var_meta_get(vm, var_nm = var_nm, meta_nm = "var_set_dt_pos_set")
  if (style == "smallest_set" && length(pos) > 1) {
    vsd <- vsd_get(vm = vm)
    set_sizes <- vapply(
      pos,
      function(p) {
        length(vsd[["var_nm_set"]][[p]])
      },
      integer(1L)
    )
    pos <- pos[which.min(set_sizes)]
  }
  return(pos)
}
var_to_var_set_id <- function(
  vm,
  var_nm,
  style = c("all", "smallest_set")[1]
) {
  # TODO: rename -> var_nm_to_var_set_id
  pos <- var_to_var_set_pos(vm = vm, var_nm = var_nm, style = style)
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
    identical(sort(names(dt)), sort(union(from_var_nm, to_var_nm)))
  )
  # @codedoc_comment_block news("vm@var_aggregate", "2024-02-15", "0.4.0")
  # `vm@var_aggregate` now always considers it possible to aggregate to
  # `to_var_nm` if it only has one value.
  # @codedoc_comment_block news("vm@var_aggregate", "2024-02-15", "0.4.0")
  if (from_var_nm == to_var_nm || data.table::uniqueN(dt[[to_var_nm]]) == 1L) {
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

# vame funs --------------------------------------------------------------------
vame_list_get <- function(vm) {
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  out <- data_obj_get(vm, "vame_list")
  return(out)
}

vame_list_set <- function(vm, value) {
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  dbc::assert_prod_input_is_list(value)
  data_obj_set(vm, "vame_list", value)
}

vame_subset_expr <- function(
  vm,
  var_dt_expr = NULL,
  var_set_dt_expr = NULL
) {
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  dbc::assert_is_one_of(
    var_dt_expr,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_language_object)
  )
  dbc::assert_is_one_of(
    var_set_dt_expr,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_language_object)
  )
  need_to_intersect <- FALSE
  if (!is.null(var_dt_expr)) {
    vd <- vd_get(vm)
    vd <- eval(substitute(vd[i = expr], list(expr = var_dt_expr)))
    vd_set(vm, vd)
    need_to_intersect <- TRUE
  }
  if (!is.null(var_set_dt_expr)) {
    vsd <- vsd_get(vm)
    vsd <- eval(substitute(vsd[i = expr], list(expr = var_set_dt_expr)))
    vsd_set(vm, vsd)
    need_to_intersect <- TRUE
  }
  if (need_to_intersect) {
    vd_vsd_intersect(vm)
  }
  return(invisible(NULL))
}


# user_utils -------------------------------------------------------------------
#' @title Utilities for User-Defined Functions and Expressions
#' @description
#' These utility functions make it easier for the user to define functions
#' and expressions to store into a `VariableMetadata` object.
#' @name user_utils
NULL

#' @rdname user_utils
#' @export
self <- function() {
  # @codedoc_comment_block vame::self
  # `vame::self` gets the `VariableMetadata` object whose slot function was
  # just called. It is intended only for use within functions and expressions
  # stored into a `VariableMetadata` object.
  # @codedoc_comment_block vame::self
  # @codedoc_comment_block news("vame::self", "2023-12-27", "0.3.0")
  # New function `vame::self`.
  # @codedoc_comment_block news("vame::self", "2023-12-27", "0.3.0")
  # @codedoc_comment_block news("vame::self", "2024-09-13", "1.1.0")
  # `vame::self()` deprecated. Refer to the `vame::VariableMetadata` itself
  # using `vm`.
  # @codedoc_comment_block news("vame::self", "2024-09-13", "1.1.0")
  msg <- paste0(
    "Your R expression/function stored into a `vame::VariableMetadata` object ",
    "called `vame::self()`, which has been deprecated in 1.1.0. ",
    "It will be deleted in a later release. ",
    "Use `vm` instead to refer to the `vame::VariableMetadata` itself."
  )
  if (utils::packageVersion("vame") >= "1.2.0") {
    stop(msg)
  } else {
    warning(msg)
  }
  # @codedoc_comment_block news("vame::self", "2024-09-16", "1.1.1")
  # Fixed `vame::self()` --- it failed to find the `VariableMetadata` object
  # in some cases although it is intended to work until deletion.
  # @codedoc_comment_block news("vame::self", "2024-09-16", "1.1.1")
  exprs <- list(
    quote(vm),
    quote(self_get()),
    quote(internal_self())
  )
  out <- NULL
  for (i in seq_along(exprs)) {
    out <- tryCatch(
      eval(exprs[[i]], parent.frame(1L)),
      error = function(e) NULL
    )
    if (!is.null(out)) {
      break
    }
  }
  if (is.null(out)) {
    stop("vame::self failed --- complain to the package maintainer if ",
         "you see this")
  }
  return(out)
}
self_set__ <- function(vm) NULL
self_rm__ <- function() NULL