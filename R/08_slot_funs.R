# utils ------------------------------------------------------------------------
call_slot_fun_alias_in_slot_fun__ <- function(
  fun_nm = NULL
) {
  slot_fun_eval_env <- parent.frame(1L)
  if (is.null(fun_nm)) {
    fun_nm <- as.character(sys.call(-1)[[1]])
  }
  if (!grepl("^vame::", fun_nm)) {
    fun_nm <- paste0("vame:::", fun_nm)
  }
  fun <- eval(parse(text = fun_nm), envir = environment(vame::VariableMetadata))
  arg_nms <- names(formals(fun))
  call_string <- paste0(
    fun_nm,
    "(",
    paste0(arg_nms, " = ", arg_nms, collapse = ", "),
    ")"
  )
  call_expr <- parse(text = call_string)[[1]]
  call_expr[["vm"]] <- quote(self())
  eval(call_expr, envir = slot_fun_eval_env)
}

# var_set funs -----------------------------------------------------------------
var_set_list_get <- function(
  vm
) {
  # @codedoc_comment_block vm@var_set_list_get
  # Get every variable name set.
  # @codedoc_comment_block vm@var_set_list_get
    
  # @codedoc_comment_block param_vm
  # @param vm `[VariableMetadata]` (no default)
  # 
  # A `VariableMetadata` object.
  # @codedoc_comment_block param_vm
  assert_is_variablemetadata(vm)
  var_set_list <- var_set_meta_get_all(vm, "var_nm_set")
  names(var_set_list) <- var_set_meta_get_all(vm, "id")
  return(var_set_list)
}

var_set_get <- function(
  vm,
  id
) {
  # @codedoc_comment_block vm@var_set_get
  # Get a specific variable name set.
  # @codedoc_comment_block vm@var_set_get
  
  assert_is_variablemetadata(vm)
  # @codedoc_comment_block param_id
  # @param id `[any]` (no default)
  # 
  # ID, or "name", of a variable set. The class of `id` is defined when you
  # create the `VariableMetadata` object and it can be pretty much anything.
  # @codedoc_comment_block param_id
  assert_is_var_set_id(vm, id)
  vsd <- vsd_get(vm)
  return(vsd[["var_nm_set"]][[var_set_id_to_pos(vm, id)]])
}


var_set_meta_get <- function(
  vm,
  id,
  meta_nm
) {
  # @codedoc_comment_block vm@var_set_meta_get
  # Get metadata for a specific variable set.
  # @codedoc_comment_block vm@var_set_meta_get
  
  assert_is_variablemetadata(vm)
  
  assert_is_var_set_id(vm, id)
    
  # @codedoc_comment_block param_meta_nm
  # @param meta_nm `[character]` (no default)
  # 
  # Name of a metadata column in `var_set_dt` or `var_dt` (depending on context)
  # of a `VariableMetadata` object.
  # @codedoc_comment_block param_meta_nm
  assert_is_var_set_meta_nm(vm, meta_nm)
  vsd <- vsd_get(vm)
  vsd[[meta_nm]][[var_set_id_to_pos(vm, id)]]
}


var_set_meta_set <- function(
  vm,
  id,
  meta_nm,
  value
) {
  # @codedoc_comment_block vm@var_set_meta_set
  # Set metadata for a specific variable set.
  # @codedoc_comment_block vm@var_set_meta_set
  
  assert_is_variablemetadata(vm)
  
  assert_is_var_set_id(vm, id)
  
  assert_is_var_set_meta_nm(vm, meta_nm)
    
  # @codedoc_comment_block param_value
  # @param value `[any]` (no default)
  # 
  # In `_set` functions the value to set for the specified metadata.
  # @codedoc_comment_block param_value
  vsd <- vsd_get(vm)
  data.table::set(
    vsd,
    i = var_set_id_to_pos(vm, id),
    j = meta_nm,
    value = value
  )
  vsd_set(vm, vsd)
}


var_set_meta_get_all <- function(
  vm,
  meta_nm
) {
  # @codedoc_comment_block vm@var_set_meta_get_all
  # Get all metadata for a specific variable set.
  # @codedoc_comment_block vm@var_set_meta_get_all
  assert_is_var_set_meta_nm(vm, meta_nm)
  vsd <- vsd_get(vm)
  vsd[[meta_nm]]
}

var_set_rename <- function(
  vm,
  old_ids,
  new_ids
) {  
  # @codedoc_comment_block vm@var_set_rename
  # Rename variable sets --- change `var_set_dt$id` values.
  # @codedoc_comment_block vm@var_set_rename

  # @codedoc_comment_block news("vm@vame_subset", "2023-12-01", "0.2.0")
  # Rename `old` to `old_ids` and `new` to `new_ids`.
  # @codedoc_comment_block news("vm@vame_subset", "2023-12-01", "0.2.0")

  # @codedoc_comment_block param_old_ids
  # @param old_ids `[any]` (no default)
  # 
  # Old variable set IDs.
  # @codedoc_comment_block param_old_ids
  # @codedoc_comment_block param_new_ids
  # @param new_ids `[any]` (no default)
  # 
  # New variable set IDs.
  # @codedoc_comment_block param_new_ids
  dbc::assert_is_nonNA(new_ids)
  dbc::assert_is_vector(new_ids)
  lapply(seq_along(old_ids), function(i) {
    assert_is_var_set_id(vm, old_ids[i])
    var_set_meta_set(vm, id = old_ids[i], meta_nm = "id", value = new_ids[i])
  })
  return(invisible(NULL))
}


var_set_remove <- function(
  vm,
  id
) {
  # @codedoc_comment_block vm@var_set_remove
  # Remove a variable set by `id`.
  # @codedoc_comment_block vm@var_set_remove
  assert_is_var_set_id(vm, id)
  pos <- var_set_id_to_pos(vm, id)
  vsd <- vsd_get(vm)
  vsd_subset <- setdiff(seq_len(nrow(vsd)), pos)
  vsd <- vsd[vsd_subset, ]
  vsd_set(vm, vsd)
  vd_vsd_intersect(vm)
  return(invisible(NULL))
}


var_set_value_space_eval <- function(
  vm,
  id,
  var_nms = NULL,
  env = NULL
) {
  # @codedoc_comment_block vm@var_set_value_space_eval
  # Retrieve and evaluate value space for a variable set given its `id`.
  # @codedoc_comment_block vm@var_set_value_space_eval

  # @codedoc_comment_block news("vm@var_set_value_space_eval", "2023-07-03", "0.1.1")
  # New slot `vm@var_set_value_space_eval`.
  # @codedoc_comment_block news("vm@var_set_value_space_eval", "2023-07-03", "0.1.1")
  # @codedoc_comment_block news("vm@var_set_value_space_eval", "2023-07-17", "0.1.7")
  # New exported fun `vame::var_set_value_space_eval` --- alternative for
  # `vm@var_set_value_space_eval`.
  # @codedoc_comment_block news("vm@var_set_value_space_eval", "2023-07-17", "0.1.7")
  # @codedoc_comment_block news("vm@var_set_value_space_eval", "2023-11-29", "0.1.9")
  # `vame::var_set_value_space_eval` + `vm@var_set_value_space_eval` gain
  # arg `var_nms`. You can now evaluate the value space for only a subset of
  # the variables in the set.
  # @codedoc_comment_block news("vm@var_set_value_space_eval", "2023-11-29", "0.1.9")

  # @codedoc_comment_block feature(VariableMetadata, value spaces)
  # The value spaces feature allows you to define what values (combinations of)
  # variables can have. This information can then be retrieved from one single
  # place (the `VariableMetadata` object) for use elsewhere, e.g. for computing
  # something by strata. See also the category spaces and assertions features.
  #
  # The value spaces feature becomes available when `var_set_dt` contains column
  # `value_space`. You can include it when `VariableMetadata` is constructed
  # or you can use `vm@var_set_value_space_set` later.
  #
  # @codedoc_insert_comment_block specification(var_set_dt$value_space)
  # @codedoc_comment_block feature(VariableMetadata, value spaces)

  dbc::assert_inherits(vm, required_class = "VariableMetadata")

  assert_is_var_set_id(vm, id = id)
  assert_var_set_value_space_is_defined(vm)
  # @codedoc_comment_block param_var_nms
  # @param var_nms `[NULL, character]` (default `NULL`)
  # 
  # - `NULL`: Get the value space for variables in the set.
  # - `character`: Get the value spaces for only these variables.
  # @codedoc_comment_block param_var_nms
  dbc::assert_is_one_of(
    var_nms,
    funs = list(
      dbc::report_is_NULL,
      dbc::report_is_character_nonNA_vector
    )
  )
  value_space_var_nms <- var_set_meta_get(vm, id = id, meta_nm = "var_nm_set")
  if (is.null(var_nms)) {
    var_nms <- value_space_var_nms
  } else {
    dbc::assert_vector_elems_are_in_set(x = var_nms, set = value_space_var_nms)
  }
  # @codedoc_comment_block param_env
  # @param env `[NULL, environment]` (default `NULL`)
  # 
  # Environment where a value space will be evaluated, if applicable.
  # 
  # - `NULL`: Use the environment where the function was called.
  # - `environment`: Use this environment.
  # @codedoc_comment_block param_env
  dbc::assert_is_one_of(
    env,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_environment)
  )
  if (is.null(env)) {
    env <- parent.frame(1L)
  }
  vs_expr <- substitute(
    var_set_meta_get(vm, id = id, meta_nm = "value_space"),
    list(id = id)
  )
  value_space <- eval(vs_expr)
  this_call <- match.call()
  assert_is_value_space(
    x = value_space,
    x_nm = deparse1(vs_expr),
    call = this_call,
    assertion_type = "general"
  )
  if ("expr" %in% names(value_space)) {
    eval_env <- new.env(parent = env)
    eval_env[["var_nms"]] <- var_nms
    value_space <- list(
      tmp = eval(value_space[["expr"]], envir = eval_env)
    )
  } else if ("fun" %in% names(value_space)) {
    value_space <- list(tmp = value_space[["fun"]](var_nms))
  }
  if ("tmp" %in% names(value_space)) {
    tmp <- value_space[["tmp"]]
    if (data.table::is.data.table(tmp)) {
      names(value_space) <- "dt"
    } else if (is.vector(tmp) && !is.list(tmp)) {
      names(value_space) <- "set"
    } else if (is.list(tmp) && "lo" %in% names(tmp)) {
      names(value_space) <- "bounds"
    } else {
      stop("value space for var_set with id = ", deparse1(id),
            " was either expr or fun, but did not evaluate into ",
            "dt, set, nor bounds. output had class(es) ",
            deparse1(class(tmp)), ".")
    }
  }
  if ("dt" %in% names(value_space) &&
        ncol(value_space[["dt"]]) > length(var_nms)) {
    #' @importFrom data.table .SD
    value_space <- list(dt = value_space[["dt"]][
      i = !duplicated(value_space[["dt"]], by = var_nms),
      j = .SD,
      .SDcols = var_nms
    ])
  }

  return(value_space)
}

# var_set_value_space funs -----------------------------------------------------
var_set_value_space_get <- function(
  vm,
  id
) {
  # @codedoc_comment_block vm@var_set_value_space_get
  # Get the value space of a specific variable set without evaluting it.
  # @codedoc_comment_block vm@var_set_value_space_get
  assert_is_var_set_id(vm, id)
  assert_var_set_value_space_is_defined(vm)
  vsd <- vsd_get(vm)
  pos <- var_set_id_to_pos(vm, id)
  return(vsd[["value_space"]][[pos]])
}


var_set_value_space_set <- function(
  vm,
  id,
  value_space
) {
  # @codedoc_comment_block vm@var_set_value_space_set
  # Set the value space of a specific variable set.
  # @codedoc_comment_block vm@var_set_value_space_set
  assert_var_set_value_space_is_defined(vm)
  vsd <- vsd_get(vm)
  pos <- var_set_id_to_pos(vm, id)
  # @codedoc_comment_block param_value_space
  # @param value_space `[list]` (no default)
  # 
  # A value space to assign for the specified variable set.
  # @codedoc_comment_block param_value_space
  data.table::set(
    vsd,
    i = pos,
    j = "value_space",
    # to ensure value_space remains a list
    value = list(list(value_space))
  )
  vsd_set(vm, vsd)
}


var_set_value_space_dt_subset <- function(
  vm,
  id,
  expr
) {
  # @codedoc_comment_block vm@var_set_value_space_dt_subset
  # Take a subset of a value space dt for a variable set and set that as the value space.
  # @codedoc_comment_block vm@var_set_value_space_dt_subset
  assert_var_set_value_space_is_defined(vm)
  assert_is_var_set_id(vm, id)
  # @codedoc_comment_block param_expr
  # @param expr `[any]` (no default)
  # 
  # Expression to subset a `data.table` object. Available columns depends on
  # context.
  # @codedoc_comment_block param_expr
  expr <- substitute(expr)
  var_set_value_set_dt_subset_expr(vm, id, expr)
}


var_is_aggregateable_to <- function(
  vm,
  from_var_nm,
  to_var_nm
) {
  # @codedoc_comment_block vm@var_is_aggregateable_to
  # Returns `TRUE` if `from_var_nm` can be aggregated into `to_var_nm`.
  # @codedoc_comment_block vm@var_is_aggregateable_to

  # @codedoc_comment_block news("vm@var_is_aggregateable_to", "2023-07-10", "0.1.3")
  # New slot `var_is_aggregateable_to`.
  # @codedoc_comment_block news("vm@var_is_aggregateable_to", "2023-07-10", "0.1.3")

  # @codedoc_comment_block param_from_var_nm
  # @param from_var_nm `[character]` (no default)
  # 
  # Name of a variable. Aggregation from this to another variable.
  # @codedoc_comment_block param_from_var_nm
  assert_is_var_nm(vm, from_var_nm)
  # @codedoc_comment_block param_to_var_nm
  # @param to_var_nm `[character]` (no default)
  # 
  # Name of a variable. Aggregation to this from another variable.
  # @codedoc_comment_block param_to_var_nm
  assert_is_var_nm(vm, to_var_nm)
  var_is_aggregateable_to__(
    vm,
    from_var_nm = from_var_nm,
    to_var_nm = to_var_nm,
    dt = vame_category_space_dt(vm, c(from_var_nm, to_var_nm))
  )
}


var_aggregate <- function(
  vm,
  x,
  from_var_nm,
  to_var_nm
) {
  # @codedoc_comment_block vm@var_aggregate
  # Returns correspoding level of `to_var_nm` for each value in `x`.
  # @codedoc_comment_block vm@var_aggregate

  # @codedoc_comment_block news("vm@var_aggregate", "2023-07-10", "0.1.3")
  # New slot `var_aggregate`.
  # @codedoc_comment_block news("vm@var_aggregate", "2023-07-10", "0.1.3")
  assert_is_var_nm(vm, from_var_nm)
  assert_is_var_nm(vm, to_var_nm)
  # @codedoc_comment_block param_x
  # @param x `[any]` (no default)
  # 
  # Values of a specified variable.
  # @codedoc_comment_block param_x
  dbc::assert_is_vector(x)
  dt <- vame_category_space_dt(vm, c(from_var_nm, to_var_nm))
  is_aggregateable <- var_is_aggregateable_to__(
    vm,
    from_var_nm = from_var_nm,
    to_var_nm = to_var_nm,
    dt = dt
  )
  if (!is_aggregateable) {
    stop("cannot aggregate ", from_var_nm, " to ", to_var_nm, "; ",
          "aggregation only possible when there is exactly one ",
          "level of the target variable for each level of the starting ",
          "variable. if e.g. ", from_var_nm, " = 1 can be either ",
          to_var_nm, " = 1 or 2, cannot aggregate.")
  }
  jdt <- data.table::setDT(list(x = x))
  data.table::setnames(jdt, "x", from_var_nm)
  dt[
    i = jdt,
    on = from_var_nm,
    j = .SD[[1L]],
    .SDcols = to_var_nm
  ]
}


var_value_space_eval <- function(
  vm,
  var_nm,
  env = NULL
) {
  # @codedoc_comment_block vm@var_aggregate
  # Get and evaluate value space for a variable.
  # @codedoc_comment_block vm@var_aggregate

  # @codedoc_comment_block news("vm@var_value_space_eval", "2023-07-03", "0.1.1")
  # New slot `vm@var_value_space_eval`.
  # @codedoc_comment_block news("vm@var_value_space_eval", "2023-07-03", "0.1.1")

  # @codedoc_comment_block param_var_nm
  # @param var_nm `[character]` (no default)
  # 
  # Name of a variable.
  # @codedoc_comment_block param_var_nm
  assert_is_var_nm(vm, var_nm)
  dbc::assert_is_one_of(
    env,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_environment)
  )
  if (is.null(env)) {
    env <- parent.frame(1L)
  }
  pos <- var_meta_get(vm, var_nm = var_nm, meta_nm = "var_set_dt_pos_set")
  vsd <- vsd_get(vm, var_nms = "id")
  value_space <- lapply(
    vsd[["id"]][pos],
    function(id) {
      var_set_value_space_eval(
        vm,
        id = id,
        var_nms = var_nm,
        env = env
      )
  })
  value_space <- unique(value_space)
  if (length(value_space) == 1) {
    value_space <- value_space[[1]]
  } else if (var_meta_get(vm, var_nm, "type") == "categorical") {
    value_space <- lapply(value_space, function(x) {
      if ("set" %in% names(x)) {
        x <- list(dt = data.table::data.table(x = x[["set"]]))
        data.table::setnames(x[["dt"]], "x", var_nm)
      }
      if ("dt" %in% names(x)) {
        x <- x[["dt"]]
      }
      x
    })
    value_space <- data.table::rbindlist(value_space)
    value_space <- list(dt = unique(value_space, by = var_nm))
  } else {
    stop(
      "Internal error: var_nm = \"", var_nm, "\" appears in more than ",
      "one variable set value space, and its own value spaces are not ",
      "all identical --- no logic has been defined for handling this ",
      "situation.")
  }
  if ("dt" %in% names(value_space)) {
    dt_subset <- !duplicated(value_space[["dt"]], by = var_nm)
    value_space[["dt"]] <- value_space[["dt"]][
      i = dt_subset,
      j = .SD,
      .SDcols = var_nm
    ]
  }
  return(value_space)
}


var_assert <- function(
  vm,
  x,
  var_nm,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL,
  env = NULL
) {
  # @codedoc_comment_block vm@var_assert
  # Assert that values in `x` are proper values of `var_nm`.
  # @codedoc_comment_block vm@var_assert

  # @codedoc_comment_block news("vm@var_assert", "2023-07-03", "0.1.1")
  # Fixed `var_assert` handling of a value space based on `bounds`.
  # @codedoc_comment_block news("vm@var_assert", "2023-07-03", "0.1.1")
  # @codedoc_comment_block news("vm@var_assert", "2023-07-04", "0.1.2")
  # Added arguments `x_nm`, `call`.
  # @codedoc_comment_block news("vm@var_assert", "2023-07-04", "0.1.2")

  # @codedoc_comment_block feature(VariableMetadata, assertions)
  # The assertions feature allows you to check that variables look like what
  # you expect. This feature relies on the value spaces feature --- see that
  # for more information. Once the value spaces feature is available, the main
  # way to use this feature is via `vm@var_assert`.
  # @codedoc_comment_block feature(VariableMetadata, assertions)

  # @codedoc_comment_block param_x_nm
  # @param x_nm `[NULL, character]` (default `NULL`)
  # 
  # See [dbc::handle_arg_x_nm].
  # @codedoc_comment_block param_x_nm
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  # @codedoc_comment_block param_call
  # @param call `[NULL, language]` (default `NULL`)
  # 
  # See [dbc::handle_arg_call].
  # @codedoc_comment_block param_call
  call <- dbc::handle_arg_call(call)
  # @codedoc_comment_block param_assertion_type
  # @param assertion_type `[NULL, character]` (default `NULL`)
  # 
  # See [dbc::handle_arg_assertion_type].
  # @codedoc_comment_block param_assertion_type
  assertion_type <- dbc::handle_arg_assertion_type(assertion_type)

  # @codedoc_comment_block news("vm@var_assert", "2023-07-11", "0.1.3")
  # `vm@var_assert` gains arg `env`. This is passed
  # to `vm@var_value_space_eval`.
  # @codedoc_comment_block news("vm@var_assert", "2023-07-11", "0.1.3")
  dbc::assert_is_one_of(
    env,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_environment)
  )
  if (is.null(env)) {
    env <- parent.frame(1L)
  }

  vs <- var_value_space_eval(vm, var_nm, env = env)
  dbc::assert_prod_interim_is_list(
    vs,
    call = call
  )
  dbc::assert_prod_interim_has_length(
    vs,
    expected_length = 1L,
    call = call
  )
  assertion_fun_list <- value_space_value_assertion_funs__()
  if (!names(vs) %in% names(assertion_fun_list)) {
    utils::str(vs, 1)
    stop("Internal error: no handling defined for value_space printed ",
          "above --- complain to the vame package maintainer.")
  }
  assertion_fun_list[[names(vs)]](
    x = x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type,
    var_nm = var_nm,
    value_space = vs
  )
}


var_meta_get <- function(
  vm,
  var_nm,
  meta_nm
) {
  # @codedoc_comment_block vm@var_meta_get
  # Get metadata for a variable.
  # @codedoc_comment_block vm@var_meta_get
  assert_is_var_nm(vm, var_nm)
  assert_is_var_meta_nm(vm, meta_nm)
  vd <- vd_get(vm)
  jdt <- data.table::setDT(list(var_nm = var_nm))
  # retrieve "pos" instead of joining directly to ensure that list-type
  # columns are handled correctly.
  pos <- vd[
    i = jdt,
    on = "var_nm",
    which = TRUE
  ]
  out <- vd[[meta_nm]][pos]
  if (inherits(out, "list") && length(out) == 1L) {
    out <- out[[1]]
  }
  return(out)
}


var_meta_set <- function(
  vm,
  var_nm,
  meta_nm,
  value
) {
  # @codedoc_comment_block vm@var_meta_set
  # Set metadata for a variable.
  # @codedoc_comment_block vm@var_meta_set
  assert_is_var_nm(vm, var_nm)
  vd <- vd_get(vm)
  data.table::set(
    vd,
    i = data.table::chmatch(var_nm, vd[["var_nm"]]),
    j = meta_nm,
    value = value
  )
  return(invisible(NULL))
}


var_meta_get_all <- function(
  vm,
  meta_nm
) {
  # @codedoc_comment_block vm@var_meta_get_all
  # Get metadata for all variables.
  # @codedoc_comment_block vm@var_meta_get_all
  assert_is_var_meta_nm(vm, meta_nm)
  vd <- vd_get(vm)
  vd[[meta_nm]]
}


var_rename <- function(
  vm,
  old_var_nms,
  new_var_nms
) {
  # @codedoc_comment_block vm@var_rename
  # Rename variables.
  # @codedoc_comment_block vm@var_rename

  # @codedoc_comment_block news("vm@var_rename", "2023-12-01", "0.2.0")
  # Rename `old` to `old_var_nms` and `new` to `new_var_nms`.
  # @codedoc_comment_block news("vm@var_rename", "2023-12-01", "0.2.0")

  # @codedoc_comment_block param_old_var_nms
  # @param old_var_nms `[character]` (no default)
  #
  # Variable names to change.
  # @codedoc_comment_block param_old_var_nms
  dbc::assert_is_character_nonNA_vector(old_var_nms)
  # @codedoc_comment_block param_new_var_nms
  # @param new_var_nms `[character]` (no default)
  #
  # Variable names to change to.
  # @codedoc_comment_block param_new_var_nms
  dbc::assert_is_character_nonNA_vector(new_var_nms)
  stopifnot(length(old_var_nms) == length(new_var_nms))
  lapply(seq_along(old_var_nms), function(i) {
    old_var_nm <- old_var_nms[i]
    assert_is_var_nm(vm, old_var_nm)
    new_var_nm <- new_var_nms[i]
    var_meta_set(
      vm,
      var_nm = old_var_nm,
      meta_nm = "var_nm",
      value = new_var_nm
    )
    id <- var_to_var_set_id(vm, new_var_nm)
    if (var_set_value_space_is_defined(vm)) {
      vs <- var_set_value_space_get(vm, id = id)
      if ("dt" %in% names(vs)) {
        data.table::setnames(vs[["dt"]], old_var_nm, new_var_nm)
      }
      var_set_value_space_set(vm, id = id, value_space = vs)
    }
    var_nm_set <- var_set_meta_get(vm, id = id, meta_nm = "var_nm_set")
    var_nm_set[var_nm_set == old_var_nm] <- new_var_nm
    var_set_meta_set(vm, id = id, meta_nm = "var_nm_set", value = var_nm_set)
  })
  invisible(NULL)
}

var_remove <- function(
  vm,
  var_nm
) {
  # @codedoc_comment_block vm@var_remove
  # Remove a variable.
  # @codedoc_comment_block vm@var_remove

  # @codedoc_comment_block news("vm@var_remove", "2023-08-11", "0.1.9")
  # `vm@var_remove` can now remove multiple variables in one go.
  # @codedoc_comment_block news("vm@var_remove", "2023-08-11", "0.1.9")
  for (vn in var_nm) {
    assert_is_var_nm(vm, vn)
  }
  expr <- substitute(!var_nm %in% VN, list(VN = var_nm))
  vame_subset_expr(vm, var_dt_expr = expr, var_set_dt_expr = NULL)
}

var_labeler_get <- function(
  vm,
  var_nm
) {
  # @codedoc_comment_block vm@var_labeler_get
  # Get the labeler for a variable.
  # @codedoc_comment_block vm@var_labeler_get
  assert_is_var_nm(vm, var_nm)
  out <- var_meta_get(vm, var_nm, "labeler")
  if (is.null(out)) {
    stop("Variable \"", var_nm, "\" has no labeler defined.")
  }
  return(out)
}

var_labeler_set <- function(
  vm,
  var_nm,
  value
) {
  # @codedoc_comment_block vm@var_labeler_get
  # Set the labeler for a variable.
  # @codedoc_comment_block vm@var_labeler_get
  assert_is_var_nm(vm, var_nm)
  assert_is_labeler(value)
  var_meta_set(vm, var_nm, "labeler", value)
}

var_labels_get <- function(
  vm,
  x,
  var_nm,
  label_col_nm = NULL
) {
  # @codedoc_comment_block vm@var_labels_get
  # Get label for each value in `x` for `var_nm`.
  # @codedoc_comment_block vm@var_labels_get
  
  # @codedoc_comment_block feature(VariableMetadata, labeling)
  # The labeling feature becomes available if
  # the `var_dt` of a `VariableMetadata` object has a `labeler` column value
  # for a variable. You can include `labeler` in `var_dt` when the
  # `VariableMetadata` object is constructed or use `vm@var_labeler_set` later.
  #
  # @codedoc_insert_comment_block specification(var_dt$labeler)
  #
  # The main way to use the labeling feature is via `vm@var_labels_get`.
  # There may be edge cases where you may want to retrieve all labels with
  # `vm@var_labeler_get`.
  # @codedoc_comment_block feature(VariableMetadata, labeling)
  assert_is_var_nm(vm, var_nm)
  labeler <- var_labeler_get(vm, var_nm = var_nm)
  
  # @codedoc_comment_block param_label_col_nm
  # @param label_col_nm `[NULL, character]` (default `NULL`)
  # 
  # Name of a column in the `labeler` that has been assigned for the variable.
  # Labels will be taken from this column.
  #
  # - `NULL`: Use first column name in `labeler` that is not `"level"` --- if
  #   `labeler` is a `data.table`.
  # - `character`: Use this column name.
  # @codedoc_comment_block param_label_col_nm
  dbc::assert_is_one_of(
    label_col_nm,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_character_nonNA_atom)
  )
  if (inherits(labeler, "data.table")) {
    label_col_nm_set <- setdiff(names(labeler), "level")
    if (is.null(label_col_nm)) {
      label_col_nm <- label_col_nm_set[1]
    } else if (!label_col_nm %in% label_col_nm_set) {
      stop("label_col_nm = \"", label_col_nm, "\" not one of the defined ",
            "label columns: ", deparse1(label_col_nm_set))
    }
    dbc::assert_has_class(x = x, required_class = class(labeler[["level"]]))
    jdt <- data.table::setDT(list(level = x))
    #' @importFrom data.table .SD
    out <- labeler[
      i = jdt,
      on = "level",
      j = .SD[[1]],
      .SDcols = label_col_nm
    ]
  } else if (is.function(labeler)) {
    # @codedoc_comment_block news("vm@var_labels_get", "2023-12-01", "0.1.10")
    # `vm@var_labels_get` now can handle `labeler`s of type `function`.
    # @codedoc_comment_block news("vm@var_labels_get", "2023-12-01", "0.1.10")
    if (is.null(label_col_nm)) {
      stop("label_col_nm = NULL, but labeler is a function so cannot ",
           "determine label_col_nm automatically.")
    }
    out <- labeler(x = x, label_col_nm = label_col_nm)
  } else {
    stop("no handling defined for labeler of class(es) ",
         deparse1(class(labeler)))
  }
  return(out)
}

# vame funs --------------------------------------------------------------------

vame_copy <- function(vm) {
  # @codedoc_comment_block vm@vame_copy
  # Take a deep copy of a VariableMetadata object. See `?data.table::copy`.
  # @codedoc_comment_block vm@vame_copy

  # @codedoc_comment_block news("vm@vame_copy", "2023-08-10", "0.1.8")
  # New slot fun `vm@vame_copy` + new exported fun `vame::vame_copy`.
  # @codedoc_comment_block news("vm@vame_copy", "2023-08-10", "0.1.8")
  vd <- data.table::copy(vd_get(vm))
  vsd <- data.table::copy(vsd_get(vm))
  out <- vame::VariableMetadata(var_dt = vd, var_set_dt = vsd)
  return(out)
}


vame_subset <- function(
  vm,
  var_dt_expr = NULL,
  var_set_dt_expr = NULL
) {
  # @codedoc_comment_block vm@vame_subset
  # Subset whole `VariableMetadata` object. Subset either `var_dt` or
  # `var_set_dt` (or both) and keep only metadata for variables that appear in
  # both `var_dt` and `var_set_dt`.
  # @codedoc_comment_block vm@vame_subset

  # @codedoc_comment_block news("vm@vame_subset", "2023-12-01", "0.2.0")
  # Rename `expr` to `var_dt_expr`. Add arg `var_set_dt_expr`.
  # @codedoc_comment_block news("vm@vame_subset", "2023-12-01", "0.2.0")

  # @codedoc_comment_block param_var_dt_expr
  # @param var_dt_expr `[NULL, logical, integer]` (default `NULL`)
  #
  # An R expression that should evaluate into `NULL`, `logical`, or `integer`.
  # The latter two are used to subset `var_dt`. `NULL` implies no subset.
  # @codedoc_comment_block param_var_dt_expr
  var_dt_expr <- substitute(var_dt_expr)
  # @codedoc_comment_block param_var_set_dt_expr
  # @param var_set_dt_expr `[NULL, logical, integer]` (default `NULL`)
  #
  # An R expression that should evaluate into `NULL`, `logical`, or `integer`.
  # The latter two are used to subset `var_set_dt_expr`. `NULL` implies no
  # subset.
  # @codedoc_comment_block param_var_set_dt_expr
  var_set_dt_expr <- substitute(var_set_dt_expr)
  vame_subset_expr(
    vm = vm,
    var_dt_expr = var_dt_expr,
    var_set_dt_expr = var_set_dt_expr
  )
  return(invisible(NULL))
}


vame_union_append <- function(
  vm,
  vm_2
) {
  assert_is_variablemetadata(vm)
  # @codedoc_comment_block param_vm_2
  # @param vm_2 `[VariableMetadata]` (no default)
  #
  # `VariableMetadata` object whose metadata will be appended to the current
  # `VariableMetadata` object.
  # @codedoc_comment_block param_vm_2
  assert_is_variablemetadata(vm_2)

  # @codedoc_comment_block vm@vame_union_append
  # Append new data into `VariableMetadata` object from another.
  # No pre-existing data are overwritten.
  # @codedoc_comment_block vm@vame_union_append

  # @codedoc_comment_block news("vm@vame_union_append", "2023-07-14", "0.1.4")
  # Fixed `vame_union_append` --- used to always raise an error due to
  # a misnamed object.
  # @codedoc_comment_block news("vm@vame_union_append", "2023-07-14", "0.1.4")
  # @codedoc_comment_block news("vm@vame_union_append", "2023-07-14", "0.1.5")
  # Robustify `vame_union_append` --- use `use.names = TRUE, fill = TRUE`
  # in `rbind` calls.
  # @codedoc_comment_block news("vm@vame_union_append", "2023-07-14", "0.1.5")
  # @codedoc_comment_block news("vm@vame_union_append", "2023-07-14", "0.1.6")
  # fix `vame_union_append` --- no longer attempt to remove duplicates
  # in rbind'd `var_dt` because some `by` columns may be of type `list`
  # which is not supported by `duplicated`.
  # @codedoc_comment_block news("vm@vame_union_append", "2023-07-14", "0.1.6")
  # @codedoc_comment_block news("vm@vame_union_append", "2023-12-01", "0.2.0")
  # Rename `x` to `vm_2`.
  # @codedoc_comment_block news("vm@vame_union_append", "2023-12-01", "0.2.0")
  vd_1 <- vd_get(vm)
  vsd_1 <- vsd_get(vm)
  vd_2 <- vd_get(vm_2)
  vsd_2 <- vsd_get(vm_2)
  vd <- data.table::data.table(
    var_nm = sort(union(vd_1[["var_nm"]], vd_2[["var_nm"]]))
  )
  dt_join_assign__(
    dt = vd,
    i = vd_2,
    on = "var_nm",
    dt_col_nms = setdiff(names(vd_2), "var_nm"),
    i_col_nms = setdiff(names(vd_2), "var_nm")
  )
  dt_join_assign__(
    dt = vd,
    i = vd_1,
    on = "var_nm",
    dt_col_nms = setdiff(names(vd_1), "var_nm"),
    i_col_nms = setdiff(names(vd_1), "var_nm")
  )
  
  #' @importFrom data.table .SD
  vsd <- data.table::data.table(
    id = union(vsd_1[["id"]], vsd_2[["id"]])
  )
  dt_join_assign__(
    dt = vsd,
    i = vsd_2,
    on = "id",
    dt_col_nms = setdiff(names(vsd_2), "id"),
    i_col_nms = setdiff(names(vsd_2), "id")
  )
  dt_join_assign__(
    dt = vsd,
    i = vsd_1,
    on = "id",
    dt_col_nms = setdiff(names(vsd_1), "id"),
    i_col_nms = setdiff(names(vsd_1), "id")
  )
  vd_set(vm, vd)
  vsd_set(vm, vsd)
  vd_vsd_linkage_refresh(vm)
  return(invisible(NULL))
}

# vame_category_space funs -----------------------------------------------

vame_category_space_dt_list <- function(
  vm,
  var_nms,
  env = NULL
) {
  # @codedoc_comment_block vm@vame_category_space_dt_list
  # Get list of category space `data.table` objects.
  # @codedoc_comment_block vm@vame_category_space_dt_list
  dbc::assert_is_one_of(
    env,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_environment)
  )
  if (is.null(env)) {
    env <- parent.frame(1L)
  }
  vd <- vd_get(vm, c("var_nm", "type"))
  is_categorical <- vd[["type"]] == "categorical"
  dbc::assert_vector_elems_are_in_set(
    var_nms,
    set = vd[["var_nm"]][is_categorical]
  )
  vsd <- vsd_get(vm, c("id", "var_nm_set", "value_space"))
  dtl <- category_space_dt_list__(
    var_nms = var_nms,
    vsd = vsd,
    env = env
  )
  return(dtl)
}


vame_category_space_dt <- function(
  vm,
  var_nms,
  env = NULL
) {
  # @codedoc_comment_block vm@vame_category_space_dt
  # Get a category space `data.table`.
  # @codedoc_comment_block vm@vame_category_space_dt
  
  # @codedoc_comment_block feature(VariableMetadata, category spaces)
  # The category spaces feature becomes available when the value spaces feature
  # is available and when
  # the `var_dt` of a `VariableMetadata` object has a `type` column, with
  # value `"categorical"` for at least one variable.
  # This features differs from the value spaces feature by being more specific
  # and being able to produce one (large) `data.table` of allowed variable
  # value combinations.
  #
  # @codedoc_insert_comment_block specification(var_dt$type)
  # 
  # This feature relies on the value spaces feature. See the documentation
  # for that feature for more information.
  #
  # The category spaces feature is used via
  # `vm@vame_category_space_dt` and `vm@vame_category_space_dt_list`.
  # @codedoc_comment_block feature(VariableMetadata, category spaces)
  if (is.null(env)) {
    env <- parent.frame(1L)
  }
  dtl <- vame_category_space_dt_list(vm, var_nms = var_nms, env = env)
  dt <- category_space_dt_list_to_category_space_dt__(dtl)
  return(dt[])
}
