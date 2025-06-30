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
    # @codedoc_comment_block news("vm@vame_union_append", "2025-04-04", "1.9.2")
    # Fixed a bug which sometimes caused an issue with `vm@vame_union_append`:
    # `var_dt$var_set_dt_pos_set` could be created as an `integer`
    # column instead of the intended `list` column.
    # @codedoc_comment_block news("vm@vame_union_append", "2025-04-04", "1.9.2")
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
  # @codedoc_comment_block news("vm@vame_union_append", "2025-04-04", "1.9.3")
  # Fixed a bug which sometimes caused an issue with refreshing
  # `var_dt$var_set_dt_pos_set`.
  # @codedoc_comment_block news("vm@vame_union_append", "2025-04-04", "1.9.3")
  if ("var_set_dt_pos_set" %in% names(vd)) {
    data.table::set(vd, j = "var_set_dt_pos_set", value = NULL)
  }
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
  # @codedoc_comment_block vame:::vd_vsd_intersect
  # Remove rows in `var_dt` where `var_dt[["var_nm"]]` does not appear in any
  # `var_set_dt[["var_nm_set"]]`. Remove variables names in
  # each `var_set_dt[["var_nm_set"]]` element that do not appear in
  # `var_dt[["var_nm_set"]]`. `var_set_dt[["value_space"]]` objects are
  # attempted to be subsetted when variables are removed from that variable set.
  # Empty variable sets are removed entirely.
  # @codedoc_comment_block vame:::vd_vsd_intersect
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

# var funs ---------------------------------------------------------------------
var_is_aggregateable_to__ <- function(
  vm,
  from_var_nm,
  to_var_nm,
  dt = NULL
) {
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  if (is.null(dt)) {
    dt <- vame_category_space_dt(vm = vm, var_nms = c(from_var_nm, to_var_nm))
  } else {
    dbc::assert_prod_input_is_data_table(dt)
    dbc::assert_prod_input_is_TRUE(
      length(setdiff(names(dt), c(from_var_nm, to_var_nm))) == 0L
    )
  }
  # @codedoc_comment_block news("vm@var_aggregate", "2024-02-15", "0.4.0")
  # `vm@var_aggregate` now always considers it possible to aggregate to
  # `to_var_nm` if it only has one value.
  # @codedoc_comment_block news("vm@var_aggregate", "2024-02-15", "0.4.0")
  # @codedoc_comment_block var_is_aggregateable_to__
  # - A variable named `from_var_nm` is aggregateble to `to_var_nm` if:
  #   + `from_var_nm` == `to_var_nm`.
  #   + Variable named `to_var_nm` has only one value in its `value_space`.
  # @codedoc_comment_block var_is_aggregateable_to__
  if (from_var_nm == to_var_nm || data.table::uniqueN(dt[[to_var_nm]]) == 1L) {
    return(TRUE)
  }
  from_type <- var_meta_get(vm, from_var_nm, "type")
  to_type <- var_meta_get(vm, to_var_nm, "type")
  # @codedoc_comment_block var_is_aggregateable_to__
  #   + Both `from_var_nm` and `to_var_nm` have `var_dt$type` set to
  #     `"categorical"`,
  #     AND they have a common `value_space` object,
  #     AND `from_var_nm` has no duplicates in a table that contains the
  #     complete joint `value_space` of `from_var_nm` and `to_var_nm`.
  # @codedoc_comment_block var_is_aggregateable_to__
  if (from_type != "categorical" || to_type != "categorical") {
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
  var_set_dt_expr = NULL,
  enclos = NULL
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
  dbc::assert_is_one_of(
    enclos,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_environment)
  )
  if (is.null(enclos)) {
    enclos <- parent.frame(1L)
  }
  # @codedoc_comment_block vm@vame_subset_expr
  # - Create an empty new evaluation environment `eval_env` with `enclos` as
  #   parent. Populate it with `var_dt` and `var_set_dt`.
  # @codedoc_comment_block vm@vame_subset_expr
  eval_env <- new.env(parent = enclos)
  eval_env[["var_dt"]] <-  vd_get(vm)
  eval_env[["var_set_dt"]] <- vsd_get(vm)
  need_to_intersect <- FALSE
  vame_subset_expr_env <- environment()
  lapply(c("var_dt", "var_set_dt"), function(dt_nm) {
    expr_obj_nm <- paste0(dt_nm, "_expr")
    obj_subset <- vame_subset_expr_env[[expr_obj_nm]]
    n_tries <- 0L
    while ((is.name(obj_subset) || is.call(obj_subset)) && n_tries <= 10) {
      # @codedoc_comment_block vm@vame_subset_expr
      # - `eval` `var_dt_expr` with
      #   `envir = eval_env[["var_dt"]]` and `enclos = enclos`
      #   and use the result to take a subset of `eval_env[["var_dt"]]`.
      #   Peform `eval` until the result is not a `name` or a `call` object
      #   or at most 10 times.
      # @codedoc_comment_block vm@vame_subset_expr
      # @codedoc_comment_block news("vm@vame_subset", "2025-06-25", "1.10.2")
      # `vm@vame_subset` fix: in 1.10.1 (but not before) mistakenly evaluated
      # `var_dt_expr` when `var_set_dt_expr` was supposed to be evaluted.
      # @codedoc_comment_block news("vm@vame_subset", "2025-06-25", "1.10.2")
      # @codedoc_comment_block news("vm@vame_subset", "2025-06-25", "1.10.3")
      # `vm@vame_subset` now allows `var_dt_expr` and `var_set_dt_expr` to
      # themselves be quoted expressions (`name` / `call` objects).
      # @codedoc_comment_block news("vm@vame_subset", "2025-06-25", "1.10.3")
      n_tries <- n_tries + 1L
      obj_subset <- eval(
        obj_subset,
        envir = eval_env[[dt_nm]],
        enclos = eval_env
      )
    }
    if (!inherits(obj_subset, c("logical", "integer", "NULL"))) {
      # @codedoc_comment_block vm@vame_subset_expr
      #   Raise an error if the result is not a `logical`,
      #   `integer`, or `NULL` object.
      # - Do the same with `var_set_dt_expr` and `eval_env[["var_set_dt"]]`.
      # @codedoc_comment_block vm@vame_subset_expr
      stop(
        "Evaluation of `", expr_obj_nm, "` did not result in a `logical`, ",
        "`integer`, nor `NULL` object after ", n_tries, " tries. Instead ",
        "result had class(es) ", deparse1(class(obj_subset))
      )
    }
    if (is.logical(obj_subset)) {
      obj_subset[is.na(obj_subset)] <- FALSE
    }
    if ((is.logical(obj_subset) && any(!obj_subset)) | is.integer(obj_subset)) {
      eval_env[[dt_nm]] <- eval_env[[dt_nm]][obj_subset, ]
      switch(
        dt_nm,
        var_dt = vd_set(vm, eval_env[["var_dt"]]),
        var_set_dt = vsd_set(vm, eval_env[["var_set_dt"]])
      )
      vame_subset_expr_env[["need_to_intersect"]] <- TRUE
    }
  })
  # @codedoc_comment_block vm@vame_subset_expr
  # - "Intersect" data in the `VariableMetadata` object:
  # @codedoc_insert_comment_block vame:::vd_vsd_intersect
  # @codedoc_comment_block vm@vame_subset_expr
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