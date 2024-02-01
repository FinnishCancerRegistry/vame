# var_set funs -----------------------------------------------------------------
var_set_dt_copy <- function(vm) {
  # @codedoc_comment_block vm@var_set_dt_copy
  # Returns a deep copy of `var_set_dt_copy`.
  # @codedoc_comment_block vm@var_set_dt_copy
  # @codedoc_comment_block news("vm@var_set_dt_copy", "2023-12-13", "0.2.2")
  # New function `vm@var_set_dt_copy`.
  # @codedoc_comment_block news("vm@var_set_dt_copy", "2023-12-13", "0.2.2")
  data.table::copy(data_obj_get(vm, "var_dt"))
}

var_set_meta_is_defined <- function(
  vm,
  id,
  meta_nm
) {
  # @codedoc_comment_block vm@var_set_meta_is_defined
  # Returns `TRUE` if `var_set_dt[[meta_nm]]` exists.
  # @codedoc_comment_block vm@var_set_meta_is_defined
  # @codedoc_comment_block news("vm@var_set_meta_is_defined", "2023-12-12", "0.2.2")
  # New function `vm@var_set_meta_is_defined`.
  # @codedoc_comment_block news("vm@var_set_meta_is_defined", "2023-12-12", "0.2.2")
  assert_is_variablemetadata(vm)
  vsd <- vsd_get(vm)
  if (!meta_nm %in% names(vsd)) {
    return(FALSE)
  }
  pos <- var_set_id_to_pos(vm, id)
  if (is.list(vsd[[meta_nm]])) {
    meta <- vsd[[meta_nm]][[pos]]
    return(!is.null(meta))
  } else {
    meta <- vsd[[meta_nm]][pos]
    return(!is.na(meta))
  }
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
  
  # @codedoc_comment_block param_id
  # @param id `[any]` (no default)
  # 
  # ID, or "name", of a variable set. The class of `id` is defined when you
  # create the `VariableMetadata` object and it can be pretty much anything.
  # @codedoc_comment_block param_id
  assert_is_var_set_id(vm, id)
      
  # @codedoc_comment_block param_value
  # @param value `[any]` (no default)
  # 
  # In `_set` functions the value to set for the specified metadata.
  # @codedoc_comment_block param_value
  vsd <- vsd_get(vm)
  # @codedoc_comment_block news("vm@var_set_meta_set", "2023-12-12", "0.2.2")
  # `vm@var_set_meta_set` now wraps `value` into a list if it isn't a list
  # and if the target column is a list.
  # @codedoc_comment_block news("vm@var_set_meta_set", "2023-12-12", "0.2.2")
  if (is.atomic(value) && meta_nm %in% names(vsd) && is.list(vsd[[meta_nm]])) {
    value <- list(value)
  }
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
  # @codedoc_comment_block news("vm@var_set_meta_get_all", "2023-12-04", "0.2.0")
  # `vm@var_set_meta_get_all` now always sets `var_set_dt$id` as the names
  # of the output list or vector.
  # @codedoc_comment_block news("vm@var_set_meta_get_all", "2023-12-04", "0.2.0")
  assert_is_var_set_meta_nm(vm, meta_nm)
  vsd <- vsd_get(vm)
  out <- vsd[[meta_nm]]
  names(out) <- vsd[["id"]]
  return(out)
}

var_set_var_nm_set_get_all <- function(
  vm
) {
  # @codedoc_comment_block vm@var_set_var_nm_set_get_all
  # Get every variable name set.
  # @codedoc_comment_block vm@var_set_var_nm_set_get_all

  # @codedoc_comment_block news("vm@var_set_var_nm_set_get_all", "2023-12-04", "0.2.0")
  # Rename `vm@var_set_list_get` to `vm@var_set_var_nm_set_get_all`.
  # @codedoc_comment_block news("vm@var_set_var_nm_set_get_all", "2023-12-04", "0.2.0")
    
  # @codedoc_comment_block param_vm
  # @param vm `[VariableMetadata]` (no default)
  # 
  # A `VariableMetadata` object.
  # @codedoc_comment_block param_vm
  var_set_meta_get_all(vm, meta_nm = "var_nm_set")
}

var_set_var_nm_set_get <- function(
  vm,
  id
) {
  # @codedoc_comment_block vm@var_set_var_nm_set_get
  # Get a specific variable name set.
  # @codedoc_comment_block vm@var_set_var_nm_set_get
  
  # @codedoc_comment_block news("vm@var_set_var_nm_set_get", "2023-12-04", "0.2.0")
  # Rename `vm@var_set_get` to `vm@var_set_var_nm_set_get`.
  # @codedoc_comment_block news("vm@var_set_var_nm_set_get", "2023-12-04", "0.2.0")
  var_set_meta_get(vm, id = id, meta_nm = "var_nm_set")
}

var_set_var_nm_set_set <- function(
  vm,
  id,
  value
) {
  # @codedoc_comment_block vm@var_set_var_nm_set_set
  # Set a new value for a specific variable name set.
  # @codedoc_comment_block vm@var_set_var_nm_set_set
  
  # @codedoc_comment_block news("vm@var_set_var_nm_set_set", "2023-12-04", "0.2.0")
  # New function `vm@var_set_var_nm_set_set`.
  # @codedoc_comment_block news("vm@var_set_var_nm_set_set", "2023-12-04", "0.2.0")

  var_set_meta_set(
    vm = vm,
    id = id,
    meta_nm = "var_set_nm",
    value = value
  )
}

var_set_rename <- function(
  vm,
  old_ids,
  new_ids
) {  
  # @codedoc_comment_block vm@var_set_rename
  # Rename variable sets --- change `var_set_dt$id` values.
  # @codedoc_comment_block vm@var_set_rename

  # @codedoc_comment_block news("vm@var_set_rename", "2023-12-01", "0.2.0")
  # Rename `old` to `old_ids` and `new` to `new_ids`.
  # @codedoc_comment_block news("vm@var_set_rename", "2023-12-01", "0.2.0")

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

# var_set_maker funs -----------------------------------------------------------
var_set_maker_get <- function(
  vm,
  id
) {
  # @codedoc_comment_block vm@var_set_maker_get
  # Get specific `var_set_dt$maker`.
  # @codedoc_comment_block vm@var_set_maker_get
  # @codedoc_comment_block feature_funs(make)
  # - `vm@var_set_maker_get`
  # @codedoc_comment_block feature_funs(make)
  # @codedoc_comment_block news("vm@var_set_maker_get", "2024-01-19", "0.3.0")
  # New function `vm@var_set_maker_get`.
  # @codedoc_comment_block news("vm@var_set_maker_get", "2024-01-19", "0.3.0")
  var_set_meta_get(vm = vm, id = id, meta_nm = "maker")
}

var_set_maker_set <- function(
  vm,
  id,
  value
) {
  # @codedoc_comment_block vm@var_set_maker_set
  # Assign specific `var_set_dt$maker`.
  # @codedoc_comment_block vm@var_set_maker_set
  # @codedoc_comment_block feature_funs(make)
  # - `vm@var_set_maker_set`
  # @codedoc_comment_block feature_funs(make)
  # @codedoc_comment_block news("vm@var_set_maker_set", "2024-01-19", "0.3.0")
  # New function `vm@var_set_maker_set`.
  # @codedoc_comment_block news("vm@var_set_maker_set", "2024-01-19", "0.3.0")
  assert_is_maker(value)
  var_set_meta_set(vm = vm, id = id, meta_nm = "maker", value = value)
}

var_set_make <- function(
  vm,
  id,
  data,
  env = NULL
) {
  # @codedoc_comment_block vm@var_set_make
  # Call specific `var_set_dt$maker`.
  # @codedoc_comment_block vm@var_set_make
  # @codedoc_comment_block feature_funs(make)
  # - `vm@var_set_make`
  # @codedoc_comment_block feature_funs(make)
  # @codedoc_comment_block news("vm@var_set_make", "2024-01-19", "0.3.0")
  # New function `vm@var_set_make`.
  # @codedoc_comment_block news("vm@var_set_make", "2024-01-19", "0.3.0")

  # @codedoc_comment_block feature(make)
  # The make feature allows for making variable sets based on other variable
  # sets. For instance, a year column might be "made" based on a data column,
  # say `start_year` from `start_date`. This feature becomes available
  # when one or more `var_set_dt$maker` objects have been defined:
  #
  # @codedoc_insert_comment_block specification(var_set_dt$maker)
  #
  # The following functions are related to this feature:
  #
  # @codedoc_insert_comment_block feature_funs(make)
  # @codedoc_comment_block feature(make)

  # @codedoc_comment_block specification(var_set_dt$maker)
  # The recommended approach to writing a `maker` for a variable set is to
  # write a function and store a call to it as the `maker`. See the example
  # for the `make` feature:
  # 
  # ```
  # @codedoc_insert_comment_block feature_example(make)
  # ```
  #
  # @codedoc_comment_block specification(var_set_dt$maker)

  # @codedoc_comment_block feature_example(make)
  # # Example of make feature
  # make_dg_y <- function(
  #   dg_date
  # ) {
  #   out <- data.table::data.table(dg_y = data.table::year(dg_date))
  #   return(out[])
  # }
  # # We do not store `my_fun` here directly because if a `VariableMetadata`
  # # object is written to disk, the entire enclosing env of `my_fun` is
  # # included. This can get very messy when it is read back into R.
  # # It is strongly recommended to store quoted expressions instead
  # # and store any functions you write somewhere else, ideally in an R package.
  # my_vame <- vame::VariableMetadata(
  #   var_dt = data.table::data.table(
  #     var_nm = c("dg_y", "dg_date", "area_01", "area_02", "area_03"),
  #     type = c("year", "date", "categorical", "categorical", "categorical")
  #   ),
  #   var_set_dt = data.table::data.table(
  #     id = c("dg_y", "dg_date", "area", "area_01", "area_02"),
  #     var_nm_set = list(
  #       dg_y = "dg_y",
  #       dg_date = "dg_date",
  #       area = c("area_01", "area_02", "area_03"),
  #       area_01 = "area_01",
  #       area_02 = "area_02"
  #     ),
  #     maker = list(
  #       dg_y = list(
  #         maker = quote(make_dg_y(dg_date = dg_date)),
  #         dep_var_nm_set = "dg_date"
  #       ),
  #       dg_date = NULL,
  #       area = NULL,
  #       area_01 = list(
  #         maker = quote({
  #           # This is an example of re-using data from the `VariableMetadata`
  #           # object itself. 
  #           vm <- vame::self()
  #           dt <- data.table::setDT(list(
  #             x = vm@var_aggregate(
  #               get(dep_var_nm_set),
  #               from_var_nm = dep_var_nm_set,
  #               to_var_nm = var_nms
  #             )
  #           ))
  #           data.table::setnames(dt, "x", var_nms)
  #           dt[]
  #         }),
  #         dep_var_nm_set = "area_02"
  #       ),
  #       # This example uses the shorthand var_aggregate approach.
  #       area_02 = list(maker = "var_aggregate", dep_var_nm_set = "area_03")
  #     ),
  #     value_space = list(
  #       dg_y = list(set = 1953:2024),
  #       dg_date = list(bounds = list(
  #         lo = as.Date("1953-01-01"),
  #         hi = as.Date("2024-12-31"),
  #         lo_inclusive = TRUE,
  #         hi_inclusive = TRUE
  #       )),
  #       area = list(dt = data.table::data.table(
  #         area_01 = c(1L,1L,1L,2L,2L,2L),
  #         area_02 = c(11L,12L,12L,21L,21L,22L),
  #         area_03 = c(111L,121L,122L,211L,212L,221L)
  #       )),
  #       area_01 = NULL,
  #       area_02 = NULL
  #     )
  #   )
  # )
  # obs <- my_vame@var_set_make(
  #   id = "dg_y",
  #   data = data.table::data.table(dg_date = as.Date("2001-01-01"))
  # )
  # exp <- data.table::data.table(dg_y = 2001L)
  # stopifnot(identical(obs[["dg_y"]], exp[["dg_y"]]))

  # obs <- my_vame@var_set_make(
  #   id = "area_01",
  #   data = data.table::data.table(area_02 = c(11L, 21L))
  # )
  # exp <- data.table::data.table(area_01 = c(1L,2L))
  # stopifnot(identical(obs[["area_01"]], exp[["area_01"]]))

  # obs <- my_vame@var_set_make(
  #   id = "area_02",
  #   data = data.table::data.table(area_03 = c(111L, 211L))
  # )
  # exp <- data.table::data.table(area_02 = c(11L,21L))
  # stopifnot(identical(obs[["area_02"]], exp[["area_02"]]))

  # obs <- my_vame@vame_make(
  #   ids = c("dg_y", "area_02", "area_01"),
  #   data = data.table::data.table(
  #     dg_date = as.Date("2001-01-01"),
  #     area_03 = c(111L,121L,122L,211L,212L,221L)
  #   )
  # )
  # exp <- data.table::data.table(
  #   dg_y = 2001L,
  #   area_02 = my_vame@var_aggregate(
  #     c(111L,121L,122L,211L,212L,221L),
  #     from_var_nm = "area_03",
  #     to_var_nm = "area_02"
  #   )
  # )
  # exp[
  #   j = "area_01" := my_vame@var_aggregate(
  #     exp[["area_02"]],
  #     from_var_nm = "area_02",
  #     to_var_nm = "area_01"
  #   )
  # ]
  # stopifnot(
  #   identical(obs[["dg_y"]], exp[["dg_y"]]),
  #   identical(obs[["area_01"]], exp[["area_01"]]),
  #   identical(obs[["area_02"]], exp[["area_02"]])
  # ) 
  # @codedoc_comment_block feature_example(make)
  dbc::assert_inherits(data, required_class = "data.frame")
  dbc::assert_has_one_of_classes(env, classes = c("NULL", "environment"))
  if (is.null(env)) {
    env <- parent.frame(1L)
  }
  maker <- var_set_maker_get(vm = vm, id = id)
  var_nms <- var_set_var_nm_set_get(vm = vm, id = id)
  if (is.function(maker)) {
    dbc::assert_has_names(data, required_names = names(formals(maker)))
    arg_list <- data
    dt <- do.call(maker, arg_list, quote = TRUE)
  } else if (inherits(maker, "list")) {
    dbc::assert_has_names(data, required_names = maker[["dep_var_nm_set"]])
    make_env <- new.env(parent = env)
    make_env[["var_nms"]] <- var_nms
    make_env[["dep_var_nm_set"]] <- maker[["dep_var_nm_set"]]
    lapply(names(data), function(obj_nm) {
      make_env[[obj_nm]] <- data[[obj_nm]]
    })
    if (identical(maker[["maker"]], "var_aggregate")) {
      maker_call <- quote({
        vm <- vame::self()
        dt <- data.table::setDT(list(
          x = vm@var_aggregate(
            get(dep_var_nm_set),
            from_var_nm = dep_var_nm_set,
            to_var_nm = var_nms
          )
        ))
        data.table::setnames(dt, "x", var_nms)
        dt[]
      })
    } else if (is.call(maker[["maker"]])) {
      maker_call <- maker[["maker"]]
    } else {
      stop("No logic defined for situation where element `maker` is not ",
           "a `function`, a `call` object, nor \"var_aggregate\".")
    }
    dt <- eval(maker_call, envir = make_env)
  } else {
    stop("Internal error: invaild var_set_dt$maker for id = ", deparse1(id))
  }
  
  dbc::assert_prod_output_is_data_table(dt)
  dbc::assert_prod_output_has_names(dt, required_names = var_nms)
  return(dt[])
}

vame_make <- function(
  vm,
  ids,
  data,
  env = NULL
) {
  # @codedoc_comment_block vm@vame_make
  # Call multiple `var_set_dt$maker`s in sequence.
  # @codedoc_comment_block vm@vame_make
  # @codedoc_comment_block feature_funs(make)
  # - `vm@vame_make`
  # @codedoc_comment_block feature_funs(make)
  # @codedoc_comment_block news("vm@vame_make", "2024-01-19", "0.3.0")
  # New function `vm@vame_make`.
  # @codedoc_comment_block news("vm@vame_make", "2024-01-19", "0.3.0")

  dbc::assert_inherits(data, required_class = "data.frame")
  dbc::assert_has_one_of_classes(env, classes = c("NULL", "environment"))
  if (is.null(env)) {
    env <- parent.frame(1L)
  }
  dt <- dt_independent_frame_dependent_contents__(data)
  lapply(ids, function(id) {
    # this called just in case some make expression makes use of a slot function
    # --- see where the slots are created.
    self_set__(vm = vm)
    out <- var_set_make(vm = vm, id = id, data = dt, env = env)
    data.table::set(
      x = dt,
      j = names(out),
      value = out
    )
    NULL
  })
  data.table::set(dt, j = names(data), value = NULL)
  return(dt[])
}

# var_set_value_space funs -----------------------------------------------------
var_set_value_space_eval <- function(
  vm,
  id,
  var_nms = NULL,
  env = NULL
) {
  # @codedoc_comment_block vm@var_set_value_space_eval
  # Retrieve and evaluate value space for a variable set given its `id`.
  # @codedoc_comment_block vm@var_set_value_space_eval
  # @codedoc_comment_block feature_funs(value spaces)
  # - `vm@var_set_value_space_eval`
  # @codedoc_comment_block feature_funs(value spaces)

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

  # @codedoc_comment_block feature(value spaces)
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
  #
  # The following functions are related to this feature:
  # @codedoc_insert_comment_block feature_funs(value spaces)
  # @codedoc_comment_block feature(value spaces)

  dbc::assert_inherits(vm, required_class = "VariableMetadata")

  assert_is_var_set_id(vm, id = id)
  assert_var_set_value_space_is_defined(vm)
  # @codedoc_comment_block param_var_nms
  # @param var_nms `[NULL, character]` (default `NULL`)
  # 
  # - `NULL`: Get the value space for all variables in the set.
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
    call_eval_env <- new.env(parent = env)
    call_eval_env[["var_nms"]] <- var_nms
    value_space <- list(
      tmp = eval(value_space[["expr"]], envir = call_eval_env)
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

var_set_value_space_get <- function(
  vm,
  id
) {
  # @codedoc_comment_block vm@var_set_value_space_get
  # Get the value space of a specific variable set without evaluting it.
  # @codedoc_comment_block vm@var_set_value_space_get
  # @codedoc_comment_block feature_funs(value spaces)
  # - `vm@var_set_value_space_get`
  # @codedoc_comment_block feature_funs(value spaces)
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
  # @codedoc_comment_block feature_funs(value spaces)
  # - `vm@var_set_value_space_set`
  # @codedoc_comment_block feature_funs(value spaces)
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
  # @codedoc_comment_block feature_funs(value spaces)
  # - `vm@var_set_value_space_dt_subset`
  # @codedoc_comment_block feature_funs(value spaces)
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

var_set_value_space_sampler_get <- function(
  vm,
  id
) {
  # @codedoc_comment_block vm@var_set_value_space_sampler_get
  # Retrieve value space sampler for given `id` from `var_set_dt$sampler`.
  # @codedoc_comment_block vm@var_set_value_space_sampler_get
  # @codedoc_comment_block feature_funs(random sampling)
  # - `vm@var_set_value_space_sampler_get`
  # @codedoc_comment_block feature_funs(random sampling)
  # @codedoc_comment_block news("vm@var_set_value_space_sampler_get", "2023-12-12", "0.2.2")
  # New slot function `var_set_value_space_sampler_get`.
  # @codedoc_comment_block news("vm@var_set_value_space_sampler_get", "2023-12-12", "0.2.2")
  var_set_meta_get(vm, id = id, meta_nm = "sampler")
}

var_set_value_space_sampler_set <- function(
  vm,
  id,
  value
) {
  # @codedoc_comment_block vm@var_set_value_space_sampler_set
  # Assign value space sampler for given `id` in `var_set_dt$sampler`.
  #
  # @codedoc_insert_comment_block specification(var_set_dt$sampler)
  # 
  # See the documentation for `var_set_value_space_sample` to understand how
  # the `sampler` object is used.
  # @codedoc_comment_block vm@var_set_value_space_sampler_set
  # @codedoc_comment_block feature_funs(random sampling)
  # - `vm@var_set_value_space_sampler_set`
  # @codedoc_comment_block feature_funs(random sampling)
  # @codedoc_comment_block news("vm@var_set_value_space_sampler_set", "2023-12-12", "0.2.2")
  # New slot function `var_set_value_space_sampler_set`.
  # @codedoc_comment_block news("vm@var_set_value_space_sampler_set", "2023-12-12", "0.2.2")
  assert_is_sampler(value)
  var_set_meta_set(vm, id = id, meta_nm = "sampler", value = value)
}

var_set_value_space_sample <- function(
  vm,
  id,
  var_nms = NULL,
  env = NULL,
  n = 1L
) {  
  # @codedoc_comment_block vm@var_set_value_space_sample
  # Returns `n` samples from the value space of `id`. 
  #
  # @codedoc_insert_comment_block feature_process(random sampling)
  # @codedoc_comment_block vm@var_set_value_space_sample

  # @codedoc_comment_block news("vm@var_set_value_space_sample", "2023-12-11", "0.2.2")
  # New slot function `var_set_value_space_sample`.
  # @codedoc_comment_block news("vm@var_set_value_space_sample", "2023-12-11", "0.2.2")

  # @codedoc_comment_block feature_funs(random sampling)
  # - `vm@var_set_value_space_sample`
  # @codedoc_comment_block feature_funs(random sampling)

  # @codedoc_comment_block feature(random sampling)
  # The random sampling feature is available when the value spaces feature
  # is available.
  # 
  # @codedoc_insert_comment_block feature_process(random sampling)
  #
  # The following functions are related to this feature:
  # @codedoc_insert_comment_block feature_funs(random sampling)
  # @codedoc_comment_block feature(random sampling)

  assert_is_var_set_id(vm, id)
  dbc::assert_is_one_of(
    var_nms,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_character_nonNA_vector)
  )
  allowed_var_nms <- var_set_meta_get(vm, id = id, meta_nm = "var_nm_set")
  dbc::assert_vector_elems_are_in_set(
    var_nms,
    set = allowed_var_nms
  )
  if (is.null(var_nms)) {
    var_nms <- allowed_var_nms
  }
  dbc::assert_is_one_of(
    env,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_environment)
  )
  if (is.null(env)) {
    env <- parent.frame(1L)
  }
  # @codedoc_comment_block param_n
  # @param n `[integer]` (default `1L`)
  # 
  # Number of random samples to take.
  # @codedoc_comment_block param_n
  dbc::assert_is_integer_nonNA_gtzero_atom(n)

  # @codedoc_comment_block feature_process(random sampling)
  # Random sampling is performed as follows:
  #
  # - `vm@var_set_value_space_eval` is called for the appropriate value space
  # @codedoc_comment_block feature_process(random sampling)
  vs <- var_set_value_space_eval(
    vm = vm,
    id = id,
    var_nms = var_nms,
    env = env
  )
  assert_is_value_space(
    x = vs,
    assertion_type = "prod_interim"
  )

  # @codedoc_comment_block feature_process(random sampling)
  # - The appropriate sampler is retrieved --- this is either the corresponding
  #   sampler stored in `var_set_dt$sampler`, or no sampler exists there,
  #   one of the defaults depending on the type of the value space:
  # @codedoc_insert_comment_block defaults(var_set_dt$sampler)
  # @codedoc_comment_block feature_process(random sampling)
  if (var_set_meta_is_defined(vm, id = id, meta_nm = "sampler")) {
    sampler <- var_set_value_space_sampler_get(vm, id = id)
    assert_is_sampler(sampler, assertion_type = "general")
  } else {
    sampler <- get_value_space_type_fun__(
      value_space_type = names(vs),
      fun_nm = "sampler"
    )
  }
  # @codedoc_comment_block feature_process(random sampling)
  # - The `sampler` is called or evaluated, which results in the random samples,
  #   and these are returned. If the `sampler` is a function it is called via
  #   `sampler(x = vs, var_nms = var_nms, n = n)`, where `vs` is the evaluated
  #   value space. If it is an expression, it is evaluated in a new environment
  #   that contains `x`, `arg_nms`, and `n`. This new environment's parent is
  #   `env`.
  # @codedoc_comment_block feature_process(random sampling)
  if (is.function(sampler)) {
    sample <- sampler(x = vs, var_nms = var_nms, n = n)
  } else if (is.call(sampler)) {
    call_eval_env <- new.env(parent = env)
    call_eval_env[["var_nms"]] <- var_nms
    call_eval_env[["n"]] <- n
    call_eval_env[["x"]] <- vs
    sample <- eval(sampler, envir = call_eval_env)
  }

  return(sample)
}

var_is_aggregateable_to <- function(
  vm,
  from_var_nm,
  to_var_nm
) {
  # @codedoc_comment_block vm@var_is_aggregateable_to
  # Returns `TRUE` if `from_var_nm` can be aggregated into `to_var_nm`.
  # @codedoc_comment_block vm@var_is_aggregateable_to
  # @codedoc_comment_block feature_funs(category spaces)
  # - `vm@var_is_aggregateable_to`
  # @codedoc_comment_block feature_funs(category spaces)

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
  # @codedoc_comment_block feature_funs(category spaces)
  # - `vm@var_aggregate`
  # @codedoc_comment_block feature_funs(category spaces)

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
  # @codedoc_comment_block feature_funs(value spaces)
  # - `vm@var_value_space_eval`
  # @codedoc_comment_block feature_funs(value spaces)

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
  # @codedoc_comment_block vm@var_value_space_eval
  # Get and evaluate value space for a variable.
  # This gets tricky when a variable has been defined in more than one
  # `var_set_dt$value_space`. If there is a single `var_set_dt$value_space`
  # for only the variable of interest, that is used. Otherwise:
  # @codedoc_comment_block vm@var_value_space_eval
  pos_set <- var_meta_get(vm, var_nm = var_nm, meta_nm = "var_set_dt_pos_set")
  vsd <- vsd_get(vm, var_nms = c("id", "var_nm_set", "value_space"))
  pos_set <- pos_set[
    !vapply(vsd[["value_space"]][pos_set], is.null, logical(1L))
  ]
  vs_lens <- vapply(vsd[["var_nm_set"]], length, integer(1L))
  is_singular <- vs_lens == 1
  if (sum(is_singular) == 1) {
    pos_set <- pos_set[is_singular]
  }

  # @codedoc_comment_block vm@var_value_space_eval
  # - Every value space for the variable is evaluated via
  #   `vm@var_set_value_space_eval`. If the exact same value space has been
  #   produced multiple times, duplicates are removed.
  # @codedoc_comment_block vm@var_value_space_eval
  value_space <- lapply(
    vsd[["id"]][pos_set],
    function(id) {
      var_set_value_space_eval(
        vm,
        id = id,
        var_nms = var_nm,
        env = env
      )
  })
  value_space <- unique(value_space)
  # @codedoc_comment_block vm@var_value_space_eval
  # - If we now have only one value space, all is well and that is returned.
  # @codedoc_comment_block vm@var_value_space_eval
  if (length(value_space) == 1) {
    value_space <- value_space[[1]]
  } else if (var_meta_get(vm, var_nm, "type") == "categorical") {
    # @codedoc_comment_block vm@var_value_space_eval
    # - If the `var_dt$type == "categorical"` for this variable, the union of
    #   all separate value spaces is formed and that will be returned.
    # @codedoc_comment_block vm@var_value_space_eval
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
    # @codedoc_comment_block vm@var_value_space_eval
    # - In other cases an error is raised because there does not seem to be
    #   any way to decide which value space to use.
    # @codedoc_comment_block vm@var_value_space_eval
    stop(
      "Internal error: var_nm = \"", var_nm, "\" appears in more than ",
      "one variable set value space, and its value spaces are different ",
      "in the different sets --- don't know which one to take."
    )
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
  # @codedoc_comment_block feature_funs(assertions)
  # - `vm@var_assert`
  # @codedoc_comment_block feature_funs(assertions)

  # @codedoc_comment_block news("vm@var_assert", "2023-07-03", "0.1.1")
  # Fixed `var_assert` handling of a value space based on `bounds`.
  # @codedoc_comment_block news("vm@var_assert", "2023-07-03", "0.1.1")
  # @codedoc_comment_block news("vm@var_assert", "2023-07-04", "0.1.2")
  # Added arguments `x_nm`, `call`.
  # @codedoc_comment_block news("vm@var_assert", "2023-07-04", "0.1.2")

  # @codedoc_comment_block feature(assertions)
  # The assertions feature allows you to check that variables look like what
  # you expect. This feature relies on the value spaces feature --- see that
  # for more information.
  #
  # The following functions are related to this feature:
  # @codedoc_insert_comment_block feature_funs(assertions)
  # @codedoc_comment_block feature(assertions)

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
  get_var_value_assertion_fun__(names(vs))(
    x = x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type,
    var_nm = var_nm,
    value_space = vs
  )
}


# var funs ---------------------------------------------------------------------
var_dt_copy <- function(vm) {
  # @codedoc_comment_block vm@var_dt_copy
  # Returns a deep copy of `var_dt`.
  # @codedoc_comment_block vm@var_dt_copy
  # @codedoc_comment_block news("vm@var_dt_copy", "2023-12-13", "0.2.2")
  # New function `vm@var_dt_copy`.
  # @codedoc_comment_block news("vm@var_dt_copy", "2023-12-13", "0.2.2")
  data.table::copy(data_obj_get(vm, "var_set_dt"))
}

var_meta_is_defined <- function(
  vm,
  var_nm,
  meta_nm
) {
  # @codedoc_comment_block vm@var_meta_is_defined
  # Returns `TRUE` if `var_dt[[meta_nm]]` exists.
  # @codedoc_comment_block vm@var_meta_is_defined
  # @codedoc_comment_block news("vm@var_meta_is_defined", "2023-12-12", "0.2.2")
  # New function `vm@var_meta_is_defined`.
  # @codedoc_comment_block news("vm@var_meta_is_defined", "2023-12-12", "0.2.2")
  # @codedoc_comment_block news("vm@var_meta_is_defined", "2024-01-24", "0.4.0")
  # `vm@var_meta_is_defined` internal problem fixed.
  # @codedoc_comment_block news("vm@var_meta_is_defined", "2024-01-24", "0.4.0")
  assert_is_var_nm(vm, var_nm = var_nm)
  vd <- vd_get(vm)
  pos <- data.table::chmatch(var_nm, vd[["var_nm"]])
  if (!meta_nm %in% names(vd)) {
    return(FALSE)
  }
  if (is.list(vd[[meta_nm]])) {
    meta <- vd[[meta_nm]][[pos]]
    return(!is.null(meta))
  } else {
    meta <- vd[[meta_nm]][pos]
    return(!is.na(meta))
  }
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
  pos <- data.table::chmatch(var_nm, vd[["var_nm"]])
  return(vd[[meta_nm]][[pos]])
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
  # @codedoc_comment_block news("vm@var_meta_set", "2023-12-12", "0.2.2")
  # `vm@var_meta_set` now wraps `value` into a list if it isn't a list
  # and if the target column is a list.
  # @codedoc_comment_block news("vm@var_meta_set", "2023-12-12", "0.2.2")
  if (is.atomic(value) && meta_nm %in% names(vd) && is.list(vd[[meta_nm]])) {
    value <- list(value)
  }
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
  # @codedoc_comment_block news("vm@var_meta_get_all", "2023-12-04", "0.2.0")
  # `vm@var_meta_get_all` now always sets `var_dt$var_nm` as the names
  # of the output list or vector.
  # @codedoc_comment_block news("vm@var_meta_get_all", "2023-12-04", "0.2.0")
  out <- vd[[meta_nm]]
  names(out) <- vd[["var_nm"]]
  return(out)
}


var_rename <- function(
  vm,
  old_var_nms,
  new_var_nms
) {
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
    # @codedoc_comment_block vm@var_rename
    # Rename variables. The name is changed in the following places:
    #
    # - `var_dt$var_nm`
    # @codedoc_comment_block vm@var_rename
    var_meta_set(
      vm,
      var_nm = old_var_nm,
      meta_nm = "var_nm",
      value = new_var_nm
    )
    # @codedoc_comment_block vm@var_rename
    # - `var_set_dt$value_space`, if the `value_space` object is of type `dt`.
    #   If the `value_space` is of type `expr` or `fun`, `vm@var_rename` emits
    #   a warning because it does not attempt to alter R expressions or 
    #   functions --- you will have to do that youself.
    # @codedoc_comment_block vm@var_rename
    # @codedoc_comment_block news("vm@var_rename", "2023-12-07", "0.2.2")
    # Fix renaming multiple variables on one go.
    # @codedoc_comment_block news("vm@var_rename", "2023-12-07", "0.2.2")
    id_set <- var_to_var_set_id(vm, new_var_nm)
    if (var_set_value_space_is_defined(vm)) {
      lapply(id_set, function(id) {
        vs <- var_set_value_space_get(vm, id = id)
        if ("dt" %in% names(vs)) {
          data.table::setnames(vs[["dt"]], old_var_nm, new_var_nm)
        } else if (any(c("expr", "fun") %in% names(vs))) {
          warning(
            "vm@var_rename cannot handle value_space object of type ", 
            names(vs)[1], "; you must alter the value_space object for ",
            "id = ", id, " yourself"
          )
        }
        var_set_value_space_set(vm, id = id, value_space = vs)   
        var_nm_set <- var_set_meta_get(vm, id = id, meta_nm = "var_nm_set")
        var_nm_set[var_nm_set == old_var_nm] <- new_var_nm      
        # @codedoc_comment_block vm@var_rename
        # - `var_set_dt$var_nm_set`
        # @codedoc_comment_block vm@var_rename
        var_set_meta_set(vm, id = id, meta_nm = "var_nm_set", value = var_nm_set) 
      })
    }
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
  # @codedoc_comment_block feature_funs(labeling)
  # - `vm@var_labeler_get`
  # @codedoc_comment_block feature_funs(labeling)
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
  # @codedoc_comment_block feature_funs(labeling)
  # - `vm@var_labeler_set`
  # @codedoc_comment_block feature_funs(labeling)
  assert_is_var_nm(vm, var_nm)
  assert_is_labeler(value)
  var_meta_set(vm, var_nm, "labeler", value)
}

var_labels_get <- function(
  vm,
  x,
  var_nm,
  label_nm = NULL,
  labeler_env = NULL
) {
  # @codedoc_comment_block vm@var_labels_get
  # Get label for each value in `x` for `var_nm`.
  # @codedoc_comment_block vm@var_labels_get
      
  # @codedoc_comment_block feature(labeling)
  # The labeling feature becomes available if
  # the `var_dt` of a `VariableMetadata` object has a `labeler` column value
  # for a variable. You can include `labeler` in `var_dt` when the
  # `VariableMetadata` object is constructed or use `vm@var_labeler_set` later.
  #
  # @codedoc_insert_comment_block specification(var_dt$labeler)
  #
  # The following functions are related to this feature:
  # @codedoc_insert_comment_block feature_funs(labeling)
  # @codedoc_comment_block feature(labeling)
  # @codedoc_comment_block feature_funs(labeling)
  # - `vm@var_labels_get`
  # @codedoc_comment_block feature_funs(labeling)
  assert_is_var_nm(vm, var_nm)
  labeler <- var_labeler_get(vm, var_nm = var_nm)
  
  # @codedoc_comment_block param_label_nm
  # @param label_nm `[NULL, character]` (default `NULL`)
  # 
  # Name of a column in the `labeler` that has been assigned for the variable.
  # Labels will be taken from this column.
  #
  # - `NULL`: Use first column name in `labeler` that is not `"x"` --- if
  #   `labeler` is a `data.table`.
  # - `character`: Use this column name.
  # @codedoc_comment_block param_label_nm
  # @codedoc_comment_block news("vm@var_labels_get", "2024-01-24", "0.4.0")
  # `vm@var_labels_get` arg `label_col_nm` renamed to `label_nm`.
  # @codedoc_comment_block news("vm@var_labels_get", "2024-01-24", "0.4.0")
  dbc::assert_is_one_of(
    label_nm,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_character_nonNA_atom)
  )

  # @codedoc_comment_block param_labeler_env
  # @param labeler_env `[NULL, environment]` (default `NULL`)
  # 
  # Environment where `labeler` of class `call` is evaluated.
  #
  # - `NULL`: Use the environment where this function is called.
  # - `environment`: Use this environment.
  # @codedoc_comment_block param_labeler_env
  dbc::assert_is_one_of(
    labeler_env,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_environment)
  )
  if (is.null(labeler_env)) {
    labeler_env <- parent.frame(1L)
  }

  if (inherits(labeler, "data.table")) {
    label_nm_set <- setdiff(names(labeler), "x")
    if (is.null(label_nm)) {
      label_nm <- label_nm_set[1]
    } else if (!label_nm %in% label_nm_set) {
      stop("label_nm = \"", label_nm, "\" not one of the defined ",
            "label names: ", deparse1(label_nm_set))
    }
    dbc::assert_has_class(x = x, required_class = class(labeler[["x"]]))
    jdt <- data.table::setDT(list(x = x))
    #' @importFrom data.table .SD
    out <- labeler[
      i = jdt,
      on = "x",
      j = .SD[[1]],
      .SDcols = label_nm
    ]
  } else if (is.function(labeler)) {
    # @codedoc_comment_block news("vm@var_labels_get", "2023-12-01", "0.2.0")
    # `vm@var_labels_get` now can handle `labeler`s of type `function`.
    # @codedoc_comment_block news("vm@var_labels_get", "2023-12-01", "0.2.0")
    if (is.null(label_nm)) {
      stop("label_nm = NULL, but labeler is a function so cannot ",
           "determine label_nm automatically.")
    }
    out <- labeler(x = x, label_nm = label_nm)
  } else if (is.call(labeler)) {
    # @codedoc_comment_block news("vm@var_labels_get", "2023-12-01", "0.2.1")
    # `vm@var_labels_get` now can handle `labeler`s of class `call`.
    # Added argument `labeler_env` for this purpose.
    # @codedoc_comment_block news("vm@var_labels_get", "2023-12-01", "0.2.1")
    out <- tryCatch(
      eval(labeler, envir = labeler_env),
      error = function(e) {
        stop(
          "Labeler for var_nm = \"", var_nm, "\" was of class 'call', but ",
          "evaluation failed. Error message: \"", e[["message"]], "\""
        )
      }
    )
    if (!is.character(out)) {
      utils::str(out)
      stop(
        "Labeler for var_nm = \"", var_nm, "\" was of class 'call', but ",
        "evaluation did not produce a character string vector. See what ",
        "`str(result)` printed above this error message."
      )     
    }
  } else {
    stop("no handling defined for labeler of class(es) ",
         deparse1(class(labeler)))
  }
  return(out)
}

var_describer_get <- function(
  vm,
  var_nm
) {
  # @codedoc_comment_block vm@var_describer_get
  # Get the describer for a variable.
  # @codedoc_comment_block vm@var_describer_get
  # @codedoc_comment_block feature_funs(describing)
  # - `vm@var_describer_getr`
  # @codedoc_comment_block feature_funs(describing)
  # @codedoc_comment_block news("vm@var_describer_get", "2024-01-22", "0.4.0")
  # New function `vm@var_describer_get`.
  # @codedoc_comment_block news("vm@var_describer_get", "2024-01-22", "0.4.0")
  assert_is_var_nm(vm, var_nm)
  out <- var_meta_get(vm, var_nm, "describer")
  if (is.null(out)) {
    stop("Variable \"", var_nm, "\" has no describer defined.")
  }
  return(out)
}

var_describer_set <- function(
  vm,
  var_nm,
  value
) {
  # @codedoc_comment_block vm@var_describer_get
  # Set the describer for a variable.
  # @codedoc_comment_block vm@var_describer_get
  # @codedoc_comment_block feature_funs(describing)
  # - `vm@var_describer_set`
  # @codedoc_comment_block feature_funs(describing)
  # @codedoc_comment_block news("vm@var_describer_set", "2024-01-22", "0.4.0")
  # New function `vm@var_describer_set`.
  # @codedoc_comment_block news("vm@var_describer_set", "2024-01-22", "0.4.0")
  assert_is_var_nm(vm, var_nm)
  assert_is_describer(value)
  var_meta_set(vm, var_nm, "describer", value)
}

var_description_get <- function(
  vm,
  var_nm,
  descr_nm = NULL,
  describer_env = NULL
) {
  # @codedoc_comment_block vm@var_description_get
  # Get description for `var_nm`.
  # @codedoc_comment_block vm@var_description_get
  # @codedoc_comment_block feature_funs(describing)
  # - `vm@var_description_get`
  # @codedoc_comment_block feature_funs(describing)
  # @codedoc_comment_block news("vm@var_description_get", "2024-01-22", "0.4.0")
  # New function `vm@var_description_get`.
  # @codedoc_comment_block news("vm@var_description_get", "2024-01-22", "0.4.0")
  
  # @codedoc_comment_block feature(describing)
  # The describing feature becomes available if
  # the `var_dt` of a `VariableMetadata` object has a `describer` column value
  # for a variable. You can include `describer` in `var_dt` when the
  # `VariableMetadata` object is constructed or use `vm@var_describer_set` later.
  #
  # @codedoc_insert_comment_block specification(var_dt$describer)
  #
  # The following functions are related to this feature:
  # @codedoc_insert_comment_block feature_funs(describing)
  # @codedoc_comment_block feature(describing)
  # @codedoc_comment_block feature_example(describing)
  # 
  # # Example of describing feature
  # my_vame <- vame::VariableMetadata(
  #   var_dt = data.table::data.table(
  #     var_nm = c("sex", "birth_date"),
  #     describer = list(
  #       sex = list(
  #         en = "Sex assigned at birth.",
  #         fi = "Syntymahetken sukupuoli."
  #       ),
  #       birth_date = quote(switch(
  #         descr_nm,
  #         en = "Date of birth.",
  #         fi = "Syntymapaivamaara."
  #       ))
  #     )
  #   ),
  #   var_set_dt = data.table::data.table(
  #     id = c("sex", "birth_date"),
  #     var_nm_set = list(sex = "sex", birth_date = "birth_date")
  #   )
  # )
  # stopifnot(
  #   identical(
  #     my_vame@var_description_get("sex"),
  #     "Sex assigned at birth."
  #   ),
  #   inherits(
  #     tryCatch(
  #       my_vame@var_description_get("birth_date"),
  #       error = function(e) e
  #     ),
  #     "error"
  #   ),
  #   identical(
  #     my_vame@var_description_get(
  #       var_nm = "birth_date",
  #       descr_nm = "fi"
  #     ),
  #     "Syntymapaivamaara."
  #   )
  # )
  # @codedoc_comment_block feature_example(describing)
  assert_is_var_nm(vm, var_nm)
  describer <- var_describer_get(vm, var_nm = var_nm)
  
  # @codedoc_comment_block param_descr_nm
  # @param descr_nm `[NULL, character]` (default `NULL`)
  # 
  # Name of a description in the `describer` that has been assigned for the
  # variable.
  #
  # - `NULL`: If `describer` is a `list`, use first element.
  #   Else raises an error.
  # - `character`: Use this description name.
  # @codedoc_comment_block param_descr_nm
  # @codedoc_comment_block news("vm@var_description_get", "2024-01-24", "0.4.0")
  # `vm@var_description_get` arg `description_name` renamed to `descr_nm`.
  # @codedoc_comment_block news("vm@var_description_get", "2024-01-24", "0.4.0")
  dbc::assert_is_one_of(
    descr_nm,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_character_nonNA_atom)
  )

  # @codedoc_comment_block param_describer_env
  # @param describer_env `[NULL, environment]` (default `NULL`)
  # 
  # Environment where `describer` of class `call` is evaluated.
  #
  # - `NULL`: Use the environment where this function is called.
  # - `environment`: Use this environment.
  # @codedoc_comment_block param_describer_env
  dbc::assert_is_one_of(
    describer_env,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_environment)
  )
  if (is.null(describer_env)) {
    describer_env <- parent.frame(1L)
  }

  if (inherits(describer, "list")) {
    descr_nm_set <- names(describer)
    if (is.null(descr_nm)) {
      descr_nm <- descr_nm_set[1]
    } else if (!descr_nm %in% descr_nm_set) {
      stop("descr_nm = \"", descr_nm,
           "\" not one of the defined ",
           "elements: ", deparse1(descr_nm_set))
    }
    out <- describer[[descr_nm]]
  } else if (is.function(describer)) {
    if (is.null(descr_nm)) {
      stop("descr_nm = NULL, but describer is a function so cannot ",
           "determine descr_nm automatically.")
    }
    out <- describer(descr_nm = descr_nm)
  } else if (is.call(describer)) {
    if (is.null(descr_nm)) {
      stop("descr_nm = NULL, but describer is a call so cannot ",
           "determine descr_nm automatically.")
    }
    eval_env <- new.env(parent = describer_env)
    eval_env[["descr_nm"]] <- descr_nm
    out <- tryCatch(
      eval(describer, envir = eval_env),
      error = function(e) {
        stop(
          "describer for var_nm = \"", var_nm, "\" was of class 'call', but ",
          "evaluation failed. Error message: \"", e[["message"]], "\""
        )
      }
    )
  } else {
    stop("no handling defined for describer of class(es) ",
         deparse1(class(describer)))
  }
  dbc::assert_prod_output_is_character_nonNA_atom(out)
  return(out)
}

var_value_space_sample <- function(
  vm,
  var_nm,
  env = NULL,
  n = 1L
) {
  # @codedoc_comment_block vm@var_value_space_sample
  # Calls `vm@var_set_value_space_sample` with `var_nms = var_nm`.
  # Output is always a vector.
  # @codedoc_comment_block vm@var_value_space_sample

  # @codedoc_comment_block news("vm@var_value_space_sample", "2023-12-12", "0.2.2")
  # New function `vm@var_value_space_sample`.
  # @codedoc_comment_block news("vm@var_value_space_sample", "2023-12-12", "0.2.2")

  # @codedoc_comment_block feature_funs(random sampling)
  # - `vm@var_value_space_sample`
  # @codedoc_comment_block feature_funs(random sampling)
  id <- var_to_var_set_id(vm = vm, var_nm = var_nm)
  out <- var_set_value_space_sample(
    vm = vm,
    id = id,
    var_nms = var_nm,
    n = n
  )
  if (inherits(out, "data.table")) {
    out <- out[[1]]
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

vame_meta_is_defined <- function(
  vm,
  meta_nm
) {
  # @codedoc_comment_block vm@vame_meta_is_defined
  # Returns `TRUE` if `vame_list` has non-`NULL` element named `meta_nm`.
  # @codedoc_comment_block vm@vame_meta_is_defined
  # @codedoc_comment_block news("vm@vame_meta_is_defined", "2023-12-12", "0.2.2")
  # New function `vm@vame_meta_is_defined`.
  # @codedoc_comment_block news("vm@vame_meta_is_defined", "2023-12-12", "0.2.2")
  dbc::assert_is_character_nonNA_vector(meta_nm)
  vl <- vame_list_get(vm = vm)
  return(meta_nm %in% names(vl) && !is.null(vl[[meta_nm]]))
}
vame_meta_get <- function(
  vm,
  meta_nm
) {
  # @codedoc_comment_block vm@vame_meta_get
  # Retrieve an element of `vame_list`.
  # @codedoc_comment_block vm@vame_meta_get
  # @codedoc_comment_block news("vm@vame_meta_get", "2023-12-12", "0.2.2")
  # New function `vm@vame_meta_get`.
  # @codedoc_comment_block news("vm@vame_meta_get", "2023-12-12", "0.2.2")
  dbc::assert_is_character_nonNA_vector(meta_nm)
  vl <- vame_list_get(vm = vm)
  dbc::assert_atom_is_in_set(meta_nm, set = names(vl))
  vl[[meta_nm]]
}
vame_meta_set <- function(
  vm,
  meta_nm,
  value
) {
  # @codedoc_comment_block vm@vame_meta_set
  # Assign an element of `vame_meta_set`.
  # @codedoc_comment_block vm@vame_meta_set
  # @codedoc_comment_block news("vm@vame_meta_set", "2023-12-12", "0.2.2")
  # New function `vm@vame_meta_set`.
  # @codedoc_comment_block news("vm@vame_meta_set", "2023-12-12", "0.2.2")
  dbc::assert_is_character_nonNA_vector(meta_nm)
  vl <- vame_list_get(vm = vm)
  vl[[meta_nm]] <- value
  vame_list_set(vm = vm, value = vl)
}

vame_value_space_sampler_get <- function(
  vm
) {
  # @codedoc_comment_block vm@vame_value_space_sampler_get
  # Retrieve `meta_list$sampler`.
  # @codedoc_comment_block vm@vame_value_space_sampler_get

  # @codedoc_comment_block news("vm@vame_value_space_sampler_get", "2023-12-12", "0.2.2")
  # New function `vm@vame_value_space_sampler_get`.
  # @codedoc_comment_block news("vm@vame_value_space_sampler_get", "2023-12-12", "0.2.2")

  # @codedoc_comment_block feature_funs(random sampling)
  # - `vm@vame_value_space_sampler_get`
  # @codedoc_comment_block feature_funs(random sampling)
  out <- vame_meta_get(vm = vm, meta_nm = "sampler")
  assert_is_sampler(out, assertion_type = "prod_output")
  return(out)
}
vame_value_space_sampler_set <- function(
  vm,
  value
) {
  # @codedoc_comment_block vm@vame_value_space_sampler_set
  # Assign `meta_list$sampler`.
  #
  # @codedoc_insert_comment_block specification(vame_list$sampler)
  # 
  # See the documentation for `vame_value_space_sample` to understand how
  # the `sampler` object is used.
  # @codedoc_comment_block vm@vame_value_space_sampler_set

  # @codedoc_comment_block news("vm@vame_value_space_sampler_set", "2023-12-12", "0.2.2")
  # New function `vm@vame_value_space_sampler_set`.
  # @codedoc_comment_block news("vm@vame_value_space_sampler_set", "2023-12-12", "0.2.2")

  # @codedoc_comment_block feature_funs(random sampling)
  # - `vm@vame_value_space_sampler_set`
  # @codedoc_comment_block feature_funs(random sampling)
  assert_is_sampler(value, assertion_type = "input")
  vame_meta_set(vm = vm, meta_nm = "sampler", value = value)
}
vame_value_space_sample <- function(
  vm,
  var_nms = NULL,
  env = NULL,
  n = 1L
) {
  # @codedoc_comment_block news("vm@vame_value_space_sample", "2023-12-12", "0.2.2")
  # New function `vm@var_set_meta_is_defined`.
  # @codedoc_comment_block news("vm@vame_value_space_sample", "2023-12-12", "0.2.2")

  # @codedoc_comment_block feature_funs(random sampling)
  # - `vm@vame_value_space_sample`
  # @codedoc_comment_block feature_funs(random sampling)
  dbc::assert_is_one_of(
    var_nms,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_character_nonNA_vector)
  )
  if (is.null(var_nms)) {
    var_nms <- var_meta_get_all(vm = vm, meta_nm = "var_nm")
  }
  dbc::assert_is_one_of(
    env,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_environment)
  )
  if (is.null(env)) {
    env <- parent.frame(1L)
  }
  dbc::assert_is_integer_nonNA_gtzero_atom(n)
  if (vame_meta_is_defined(vm = vm, meta_nm = "sampler")) {
  # @codedoc_comment_block vm@vame_value_space_sample
  # Take `n` random samples of the value spaces of `var_nms`.
  #
  # If `vame_list$sampler` exists and is a function, it is called via
  # `vame_list$sampler(x = vm, var_nms = var_nms, n = n)`, where
  # `vm` is the pertinent `VariableMetadata` object itself.
  # @codedoc_comment_block vm@vame_value_space_sample
    sampler <- vame_value_space_sampler_get(vm = vm)
    if (is.function(sampler)) {
      sample <- sampler(x = vm, var_nms = var_nms, n = n)
    } else if (is.call(sampler)) {
      # @codedoc_comment_block vm@vame_value_space_sample
      # If `vame_list$sampler` is a `call` object, it is evaluated in a new
      # environment whose parent is `env`.
      # @codedoc_comment_block vm@vame_value_space_sample
      call_eval_env <- new.env(parent = env)
      call_eval_env[["x"]] <- vm
      call_eval_env[["var_nms"]] <- var_nms
      call_eval_env[["n"]] <- n
      sample <- eval(sampler, envir = call_eval_env)
    }
  } else {
    # @codedoc_comment_block vm@vame_value_space_sample
    # If `vame_list$sampler` has not been set, `vm@vame_value_space_sample`
    # calls `vm@vame_value_space_sample_default`.
    # @codedoc_comment_block vm@vame_value_space_sample
    sample <- vame_value_space_sample_default(
      vm = vm,
      var_nms = var_nms,
      env = env,
      n = n
    )
  }
  # @codedoc_comment_block vm@vame_value_space_sample
  # `vm@vame_value_space_sample` always returns a `data.table` with `n` rows
  # and columns `var_nms`.
  # @codedoc_comment_block vm@vame_value_space_sample
  dbc::assert_prod_output_is_data_table_with_required_names(
    x = sample,
    required_names = var_nms
  )
  dbc::assert_prod_output_is_identical(
    x = nrow(sample),
    y = n
  )
  data.table::setcolorder(sample, var_nms)
  return(sample[])
}

vame_value_space_sample_default <- function(
  vm,
  var_nms = NULL,
  env = NULL,
  n = 1L
) {  
  # @codedoc_comment_block news("vm@vame_value_space_sample_default", "2023-12-12", "0.2.2")
  # New function `vm@vame_value_space_sample_default`.
  # @codedoc_comment_block news("vm@vame_value_space_sample_default", "2023-12-12", "0.2.2")

  # @codedoc_comment_block feature_funs(random sampling)
  # - `vm@vame_value_space_sample_default`
  # @codedoc_comment_block feature_funs(random sampling)
  dbc::assert_is_one_of(
    var_nms,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_character_nonNA_vector)
  )
  if (is.null(var_nms)) {
    var_nms <- var_meta_get_all(vm = vm, meta_nm = "var_nm")
  }
  dbc::assert_is_one_of(
    env,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_environment)
  )
  if (is.null(env)) {
    env <- parent.frame(1L)
  }
  dbc::assert_is_integer_nonNA_gtzero_atom(n)

  id_by_var_nm <- unlist(lapply(
    var_nms,
    var_to_var_set_id,
    vm = vm,
    style = "smallest_set"
  ))
  # @codedoc_comment_block vm@vame_value_space_sample_default
  # `vm@vame_value_space_sample_default`
  # calls `vm@var_set_value_space_sample` for every pertinent variable set
  # and combines the results into one large `data.table`.
  # @codedoc_comment_block vm@vame_value_space_sample_default
  sample <- lapply(unique(id_by_var_nm), function(id) {
    var_nms_i <- intersect(var_nms, var_set_var_nm_set_get(vm = vm, id = id))
    dt <- var_set_value_space_sample(
      vm = vm,
      id = id,
      var_nms = var_nms_i,
      env = env,
      n = n
    )
    if (length(var_nms_i) == 1 && !data.table::is.data.table(dt)) {
      dt <- data.table::data.table(x = dt)
      data.table::setnames(dt, var_nms_i)
    }
    dt[]
  })
  sample <- do.call(cbind, sample, quote = TRUE)
  # @codedoc_comment_block vm@vame_value_space_sample_default
  # `vm@vame_value_space_sample_default` always returns a `data.table` with `n`
  # rows and columns `var_nms`.
  # @codedoc_comment_block vm@vame_value_space_sample_default
  dbc::assert_prod_output_is_data_table_with_required_names(
    x = sample,
    required_names = var_nms
  )
  dbc::assert_prod_output_is_identical(
    x = nrow(sample),
    y = n
  )
  data.table::setcolorder(sample, var_nms)
  return(sample[])
}

vame_list_copy <- function(vm) {  
  # @codedoc_comment_block vm@vame_list_copy
  # Get a deep copy of `meta_list`.
  # @codedoc_comment_block vm@vame_list_copy

  # @codedoc_comment_block news("vm@vame_list_copy", "2023-12-13", "0.2.2")
  # New function `vm@vame_list_copy`.
  # @codedoc_comment_block news("vm@vame_list_copy", "2023-12-13", "0.2.2")
  data.table::copy(data_obj_get(vm = vm, "vame_list"))
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
  # @codedoc_comment_block feature_funs(category spaces)
  # - `vm@vame_category_space_dt_list`
  # @codedoc_comment_block feature_funs(category spaces)
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
  dtl <- category_space_dt_list__(
    vm = vm,
    var_nms = var_nms,
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
  
  # @codedoc_comment_block feature(category spaces)
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
  # The following functions are part of this feature:
  # @codedoc_insert_comment_block feature_funs(category spaces)
  # 
  # @codedoc_comment_block feature(category spaces)
  
  # @codedoc_comment_block feature_funs(category spaces)
  # - `vm@vame_category_space_dt`
  # @codedoc_comment_block feature_funs(category spaces)
  if (is.null(env)) {
    env <- parent.frame(1L)
  }
  dtl <- vame_category_space_dt_list(vm, var_nms = var_nms, env = env)
  dt <- category_space_dt_list_to_category_space_dt__(dtl)
  return(dt[])
}
