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
handle_arg_data__ <- function(
  data,
  output_type = c("arg_list", "df_list")[1]
) {
  parent_call <- eval(quote(match.call()), parent.frame())
  assert_is_arg_data(
    x = data,
    x_nm = "data",
    call = parent_call,
    assertion_type = "user_input"
  )
  if (output_type == "arg_list") {
    if (is.data.frame(data)) {
      # @codedoc_comment_block specs(vame:::handle_arg_data__, "df_list")
      # @param data `[data.frame, data.table, list]` (no default)
      #
      # - `data.frame`/`data.table`: Columns contain necessary data.
      # @codedoc_comment_block specs(vame:::handle_arg_data__, "df_list")
      out <- as.list(data)
    } else if (inherits(data, "list")) {
      # @codedoc_comment_block specs(vame:::handle_arg_data__, "df_list")
      # - `list`: Element `data$df` can be a `data.frame`/
      #   `data.table` whose columns contain necessary data.
      #   Alternatively the list elements can contain necessary data directly,
      #   or a combination of the two.
      # @codedoc_comment_block specs(vame:::handle_arg_data__, "df_list")
      out <- data
      if ("df" %in% names(out)) {
        if (!is.data.frame(out[["df"]])) {
          stop(simpleError(
            message = paste0(
              "Arg `data` was of class `list`, and had element `data$df`, but ",
              "`data$df` was not a `data.frame`/`data.table` object. Instead ",
              "it had class vector ", deparse1(class(data[["df"]]))
            ),
            call = parent_call
          ))
        }
        df <- out[["df"]]
        out["df"] <- NULL
        out[names(df)] <- as.list(df)
      }
    } else if (is.null(data)) {
      out <- data
    } else {
      stop(simpleError(
        message = paste0(
          "Internal error: no handling defined for `data` of class(es) ",
          deparse1(class(data))
        ),
        call = parent_call
      ))
    }
  } else if (output_type == "df_list") {
    if (is.data.frame(data)) {
      out <- list(df = data)
    } else if (inherits(data, "list")) {
      out <- data
      if (!"df" %in% names(data) || !is.data.frame(data[["df"]])) {
        stop(
          "`data` was a list but `data$df` was not included or was not ",
          "a `data.frame`/`data.table`. Either supply `data` with `data$df` ",
          "or `data` as a `data.frame`/`data.table` directly."
        )
      }
    } else if (is.null(data)) {
      stop(simpleError(
        message = "This function does not permit `data` to be `NULL`.",
        call = parent_call
      ))
    } else {
      stop(simpleError(
        message = paste0(
          "Internal error: no handling defined for `data` of class(es) ",
          deparse1(class(data))
        ),
        call = parent_call
      ))
    }
  }

  return(out)
}

handle_arg_ids_et_var_nms_inplace__ <- function(
  vm,
  required_meta_nm = NULL,
  ids_arg_nm = "ids",
  var_nms_arg_nm = "var_nms"
) {
  calling_env <- parent.frame(1L)
  dbc::assert_prod_interim_is(
    quote(c(ids_arg_nm, var_nms_arg_nm) %in% ls(envir = calling_env))
  )
  dbc::assert_prod_input_has_one_of_classes(
    required_meta_nm,
    classes = c("NULL", "character")
  )
  ids <- calling_env[[ids_arg_nm]]
  var_nms <- calling_env[[var_nms_arg_nm]]
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
    if (is.null(required_meta_nm)) {
      condition <- rep(TRUE, length(ids))
    } else {
      condition <- vapply(ids, function(id) {
        var_set_meta_is_defined(vm = vm, id = id, meta_nm = required_meta_nm)
      }, logical(1L))
    }
    ids <- ids[condition]
    miss_var_nms <- setdiff(var_nms, unlist(all_var_nm_sets[ids]))
    if (length(miss_var_nms) > 0) {
      # @codedoc_comment_block news("vame", "2024-06-19", "0.5.3")
      # Fixed an extra comma in a function call when
      # `ids` was `NULL` and `var_nms` was used to infer the `ids`
      # (e.g. `vm@var_set_make`).
      # @codedoc_comment_block news("vame", "2024-06-19", "0.5.3")
      msg <- paste0(
        "`ids` was `NULL` and `var_nms` was used to infer the `ids`. ",
        "However, a variable set was not possible to detect for the ",
        "following `var_nms`: ", deparse1(miss_var_nms), "."
      )
      if (!is.null(required_meta_nm)) {
        msg <- paste0(
          msg,
          " This problem has likely occurred because no `var_set_dt$",
          required_meta_nm, "` was defined for the variable set(s) which ",
          "contain the variable names listed above."
        )
      }
      stop(msg)
    }
  } else if (is.null(var_nms) && !is.null(ids)) {
    # @codedoc_comment_block news("vame::vame_value_space_sample_default", "2024-05-10", "0.5.1")
    # Fixed a utility function used by `vame::vame_value_space_sample_default`.
    # @codedoc_comment_block news("vame::vame_value_space_sample_default", "2024-05-10", "0.5.1")
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
  calling_env[[ids_arg_nm]] <- ids
  calling_env[[var_nms_arg_nm]] <- var_nms
  return(invisible(NULL))
}

var_set_maker_req_obj_nm_set__ <- function(vm, id = NULL, maker = NULL) {
  if (is.null(maker)) {
    maker <- var_set_maker_get(vm = vm, id = id)
  }
  assert_is_maker(maker, assertion_type = "prod_input")
  if (inherits(maker, "list")) {
    return(maker[["dep_var_nm_set"]])
  } else if (is.function(maker)) {
    return(names(formals(maker)))
  } else {
    stop("Internal error: could not determine required object names for a ",
         "`maker` --- it was neither a list nor a function.")
  }
}
