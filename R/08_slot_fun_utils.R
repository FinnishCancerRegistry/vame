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

# handle_arg_* funs ------------------------------------------------------------
handle_arg_data__ <- function(data) {
  assert_is_arg_data(
    x = data,
    x_nm = "data",
    call = eval(quote(match.call()), parent.frame()),
    assertion_type = "user_input"
  )

  if (is.data.frame(data)) {
    arg_list <- as.list(data)
  } else {
    arg_list <- data
    if ("df" %in% names(data)) {
      if (!is.data.frame(data[["df"]])) {
        stop("Arg `data` was of class `list`, and had element `data$df`, but ",
             "`data$df` was not a `data.frame`/`data.table` object. Instead ",
             "it had class vector ", deparse1(class(data[["df"]])))
      }
      arg_list["df"] <- NULL
      arg_list[names(data[["df"]])] <- data[["df"]]
    }
  }

  return(arg_list)
}

handle_arg_ids_et_var_nms_inplace__ <- function(vm) {
  calling_env <- parent.frame(1L)
  dbc::assert_prod_interim_is(
    quote(c("ids", "var_nms") %in% ls(envir = calling_env))
  )
  ids <- calling_env[["ids"]]
  var_nms <- calling_env[["var_nms"]]
  # @codedoc_comment_block doc_slot_fun_arg(ids)
  # @param ids `[NULL, vector]` (default `NULL`)
  #
  # - `NULL`: Behaviour varies. An error is raised if this cannot be inferred.
  #   If the function has the argument `var_nms`, that will be used
  #   to infer `ids`.
  # - `vector`: One or more values that can be found in `var_set_dt$id`.
  # @codedoc_comment_block doc_slot_fun_arg(ids)
  assert_is_arg_ids(vm = vm, x = ids)
  # @codedoc_comment_block doc_slot_fun_arg(var_nms)
  # @param var_nms `[NULL, character]` (default `NULL`)
  #
  # - `NULL`: Behaviour varies. If the function has the argument `ids` or `id`,
  #   uses all variable names in those variable sets.
  # - `character`: Use these variable names.
  # @codedoc_comment_block doc_slot_fun_arg(var_nms)
  assert_is_arg_var_nms(var_nms)
  all_var_nm_sets <- var_set_var_nm_set_get_all(vm = vm)
  all_ids <- var_set_meta_get_all(vm = vm, meta_nm = "id")
  if (is.null(ids) && is.null(var_nms)) {
    stop("Both `ids` and `var_nms` cannot be `NULL`")
  } else if (!is.null(var_nms) && is.null(ids)) {
    ids <- all_ids[vapply(
      seq_along(all_ids),
      function(i) {
        any(all_var_nm_sets[[i]] %in% var_nms)
      },
      logical(1L)
    )]
  } else if (is.null(var_nms) && !is.null(ids)) {
    var_nms <- unname(unlist(all_var_nm_sets[match(ids, all_ids)]))
  } else {
    inferred_var_nms <- unlist(all_var_nm_sets[match(ids, all_ids)])
    extra_var_nms <- setdiff(var_nms, inferred_var_nms)
    if (length(extra_var_nms) > 0) {
      stop("Arguments `ids` and `var_nms` are incongruent: `var_nms` contains ",
           "these variable names not found in any of the variable name sets ",
           "for the supplied `ids`: ", deparse1(extra_var_nms))
    }
  }
  calling_env[["ids"]] <- ids
  calling_env[["var_nms"]] <- var_nms
  return(invisible(NULL))
}
