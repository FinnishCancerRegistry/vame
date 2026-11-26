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
      # @codedoc_comment_block specs(vame:::handle_arg_data__("arg_list"))
      # **`data`** `[data.frame, data.table, list, NULL]`
      #
      # - `data.frame`/`data.table`: Columns contain necessary data.
      # @codedoc_comment_block steps(vame:::handle_arg_data__("arg_list"))
      #   + If `data` is a `data.frame` object, it is turned into a `list` via
      #     `as.list(data)` before use.
      # @codedoc_comment_block steps(vame:::handle_arg_data__("arg_list"))
      # @codedoc_comment_block specs(vame:::handle_arg_data__("arg_list"))
      out <- as.list(data)
    } else if (inherits(data, "list")) {
      # @codedoc_comment_block specs(vame:::handle_arg_data__("arg_list"))
      # - `list`: Individual elements and/or `data$df` can contain the necessary
      #   variables.
      # @codedoc_comment_block steps(vame:::handle_arg_data__("arg_list"))
      #   + If `data` is a `list` object and contains `data$df`, the columns
      #     of `data$df` are appended into `data` as list elements and
      #     `data$df` is removed before use.
      # @codedoc_comment_block steps(vame:::handle_arg_data__("arg_list"))
      # @codedoc_comment_block specs(vame:::handle_arg_data__("arg_list"))
      out <- data
      if ("df" %in% names(out)) {
        if (!is.data.frame(out[["df"]])) {
          stop(simpleError(
            message = paste0(
              "Arg `data` was of class `list`, and had element `data$df`, but ",
              "`data$df` was not a `data.frame`/`data.table` object. Instead ",
              "it had class vector ",
              deparse1(class(data[["df"]]))
            ),
            call = parent_call
          ))
        }
        df <- out[["df"]]
        out["df"] <- NULL
        out[names(df)] <- as.list(df)
      }
    } else if (is.null(data)) {
      # @codedoc_comment_block specs(vame:::handle_arg_data__("arg_list"))
      # - `NULL`: No data is passed.
      # @codedoc_comment_block steps(vame:::handle_arg_data__("arg_list"))
      #   + If `data` is `NULL` object, it is kept as-is.
      # @codedoc_comment_block steps(vame:::handle_arg_data__("arg_list"))
      # @codedoc_comment_block specs(vame:::handle_arg_data__("arg_list"))
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
      # @codedoc_comment_block specs(vame:::handle_arg_data__("df_list"))
      # **`data`** `[data.frame, data.table, list]` (no default)
      #
      # - `data.frame`/`data.table`: Columns contain necessary data.
      # @codedoc_comment_block specs(vame:::handle_arg_data__("df_list"))
      # @codedoc_comment_block steps(vame:::handle_arg_data__("df_list"))
      #   + If `data` is a `data.frame` object, it is turned into a `list`.
      #     via `list(df = data)`.
      # @codedoc_comment_block steps(vame:::handle_arg_data__("df_list"))
      out <- list(df = data)
    } else if (inherits(data, "list")) {
      # @codedoc_comment_block specs(vame:::handle_arg_data__("df_list"))
      # - `list`: Element `data$df` can be a `data.frame`/
      #   `data.table` whose columns contain necessary data.
      #   Alternatively the list elements can contain necessary data directly,
      #   or a combination of the two.
      # @codedoc_comment_block specs(vame:::handle_arg_data__("df_list"))
      # @codedoc_comment_block steps(vame:::handle_arg_data__("df_list"))
      #   + If `data` is a `list` object, the existence of `data$df` is checked
      #     and `data` is used as-is.
      # @codedoc_comment_block steps(vame:::handle_arg_data__("df_list"))
      out <- data
      if (!"df" %in% names(data) || !is.data.frame(data[["df"]])) {
        stop(
          "`data` was a list but `data$df` was not included or was not ",
          "a `data.frame`/`data.table`. Either supply `data` with `data$df` ",
          "or `data` as a `data.frame`/`data.table` directly."
        )
      }
    } else if (is.null(data)) {
      # @codedoc_comment_block steps(vame:::handle_arg_data__("df_list"))
      #   + If `data` is `NULL` object, an error is raised.
      # @codedoc_comment_block steps(vame:::handle_arg_data__("df_list"))
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
  ids_arg_nm = "ids",
  var_nms_arg_nm = "var_nms",
  required_var_set_meta_nms = NULL,
  require_meta_style = "and",
  n_max_ids = NULL
) {
  calling_env <- parent.frame(1L)
  dbc::assert_prod_interim_is(
    quote(c(ids_arg_nm, var_nms_arg_nm) %in% ls(envir = calling_env))
  )
  dbc::assert_prod_input_has_one_of_classes(
    required_var_set_meta_nms,
    classes = c("NULL", "character")
  )
  dbc::assert_prod_input_atom_is_in_set(
    require_meta_style,
    set = c("or", "and", "multi_or", "multi_and")
  )
  ids <- calling_env[[ids_arg_nm]]
  var_nms <- calling_env[[var_nms_arg_nm]]
  # @codedoc_comment_block doc_slot_fun_arg(ids)
  # **`ids`** `[NULL, vector]` (default `NULL`)
  #
  # - `NULL`: Behaviour varies. An error is raised if this cannot be inferred.
  #   If the function has the argument `var_nms`, that will be used
  #   to infer `ids`.
  # - `vector`: One or more values that can be found in `var_set_dt$id`.
  # @codedoc_comment_block doc_slot_fun_arg(ids)
  assert_is_arg_ids(vm = vm, x = ids)
  # @codedoc_comment_block doc_slot_fun_arg(var_nms)
  # **`var_nms`** `[NULL, character]` (default `NULL`)
  #
  # - `NULL`: Behaviour varies. If the function has the argument `ids` or `id`,
  #   uses all variable names in those variable sets.
  # - `character`: Use these variable names.
  # @codedoc_comment_block doc_slot_fun_arg(var_nms)
  assert_is_arg_var_nms(
    vm = vm,
    x = var_nms,
    must_exist = TRUE,
    allow_null = TRUE
  )
  all_var_nm_sets <- var_set_var_nm_set_get_all(vm = vm)
  all_ids <- var_set_meta_get_all(vm = vm, meta_nm = "id")
  if (is.null(ids) && is.null(var_nms)) {
    stop("Both `ids` and `var_nms` cannot be `NULL`")
  } else if (!is.null(var_nms) && is.null(ids)) {
    is_usable_id <- vapply(
      seq_along(all_ids),
      function(i) {
        any(all_var_nm_sets[[i]] %in% var_nms)
      },
      logical(1L)
    )
    ids <- all_ids[is_usable_id]
    if (!is.null(required_var_set_meta_nms)) {
      found_meta_nms_by_id <- lapply(ids, function(id) {
        required_var_set_meta_nms[vapply(
          required_var_set_meta_nms,
          function(rmn) {
            var_set_meta_is_defined(vm = vm, id = id, meta_nm = rmn)
          },
          logical(1L)
        )]
      })
      n_found_meta_nms_by_id <- vapply(
        found_meta_nms_by_id,
        length,
        integer(1L)
      )
      dt <- data.table::data.table(
        id = rep(ids, times = n_found_meta_nms_by_id),
        meta_nm = unlist(found_meta_nms_by_id),
        var_nm_set_length = rep(
          vapply(
            all_var_nm_sets[is_usable_id],
            length,
            integer(1L)
          ),
          times = n_found_meta_nms_by_id
        )
      )
      if (require_meta_style %in% c("and", "multi_and")) {
        good_ids <- ids[
          n_found_meta_nms_by_id == length(required_var_set_meta_nms)
        ]
        dt <- dt[dt[["id"]] %in% good_ids, ]
        if (nrow(dt) == 0) {
          stop(
            "Could not determine `ids` for ",
            "`var_nms = ",
            deparse1(unname(var_nms)),
            "`: ",
            "No variable set has all required ",
            "metadata `",
            deparse1(required_var_set_meta_nms),
            "`"
          )
        }
      }
      if (!is.null(n_max_ids)) {
        # @codedoc_comment_block news("vame", "2024-10-14", "1.3.1")
        # All functions that auto-infer `id`/`ids` which require multiple
        # metadata for the `id` now makes use of the preference defined in the
        # function. For instance, requiring an `id` to have either `sampler` or
        # `value_space` now causes the `id` value with `sampler` defined to
        # be preferred even if a separate one has `value_space`.
        # @codedoc_comment_block news("vame", "2024-10-14", "1.3.1")
        data.table::set(
          dt,
          j = "meta_preference_order",
          value = data.table::chmatch(
            dt[["meta_nm"]],
            required_var_set_meta_nms
          )
        )
        data.table::setkeyv(
          dt,
          c("id", "meta_preference_order", "var_nm_set_length")
        )
        dt <- dt[
          i = cumsum(!duplicated(dt[["id"]])) <= n_max_ids,
          #' @importFrom data.table .SD
          j = .SD,
          .SDcols = c("id", "meta_nm", "var_nm_set_length")
        ]
      }
      ids <- switch(
        require_meta_style,
        multi_or = dt,
        multi_and = dt,
        or = unique(dt[["id"]]),
        and = unique(dt[["id"]])
      )
    }
    id_idx <- if (data.table::is.data.table(ids)) {
      match(ids[["id"]], all_ids)
    } else {
      match(ids, all_ids)
    }
    miss_var_nms <- setdiff(
      var_nms,
      unlist(all_var_nm_sets[id_idx])
    )
    if (length(miss_var_nms) > 0) {
      # @codedoc_comment_block news("vame", "2024-06-19", "0.5.3")
      # Fixed an extra comma in a function call when
      # `ids` was `NULL` and `var_nms` was used to infer the `ids`
      # (e.g. `vm@var_set_make`).
      # @codedoc_comment_block news("vame", "2024-06-19", "0.5.3")
      msg <- paste0(
        "`ids` was `NULL` and `var_nms` was used to infer the `ids`. ",
        "However, there was no applicable variable set for the ",
        "following `var_nms`: ",
        deparse1(miss_var_nms),
        "."
      )
      if (!is.null(required_var_set_meta_nms)) {
        msg <- paste0(
          msg,
          " This problem has likely occurred because not all of the metadata `",
          deparse1(required_var_set_meta_nms),
          "` was defined for the variable set(s) ",
          "which contain the variable names listed above."
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
    idless_var_nms <- setdiff(var_nms, inferred_var_nms)
    if (length(idless_var_nms) > 0) {
      stop(
        "Arguments `ids` and `var_nms` are incongruent: `var_nms` contains ",
        "these variable names not found in any of the variable name sets ",
        "for the supplied `ids`: ",
        deparse1(idless_var_nms)
      )
    }
  }
  calling_env[[ids_arg_nm]] <- ids
  calling_env[[var_nms_arg_nm]] <- var_nms
  return(invisible(NULL))
}

var_set_maker_req_obj_nm_sets__ <- function(vm, id = NULL, maker = NULL) {
  if (is.null(maker)) {
    maker <- var_set_maker_get(vm = vm, id = id)
  }
  assert_is_maker(maker, assertion_type = "prod_input")
  if (inherits(maker, "list")) {
    if ("dep_var_nm_set" %in% names(maker)) {
      out <- list(maker[["dep_var_nm_set"]])
    } else {
      out <- maker[["dep_var_nm_sets"]]
    }
  } else if (is.function(maker)) {
    out <- list(names(formals(maker)))
  } else {
    stop(
      "Internal error: could not determine required object names for a ",
      "`maker` --- it was neither a list nor a function."
    )
  }
  return(out)
}

maker_dag_edge_dt__ <- function(vm, ids) {
  # @codedoc_comment_block vame:::maker_dag_edge_dt__
  # - For each variable set `id` that has a `maker`, collect the dependency
  #   variable names of the `maker` (see
  #   `vm@var_set_maker_get_dep_var_nm_sets`). For a `maker` of type
  #   `function`, the argument names `vm`, `var_nms`, and `dep_var_nm_set`
  #   are not considered dependencies because they are supplied by
  #   `vm@var_set_make` itself. Each dependency variable name is then
  #   considered to be used to make each variable in the `var_nm_set` of
  #   the variable set. If a `maker` has multiple alternative dependency
  #   sets (`dep_var_nm_sets`), all of them are included.
  # @codedoc_comment_block vame:::maker_dag_edge_dt__
  edge_dt_list <- lapply(ids, function(id) {
    maker <- var_set_maker_get(vm = vm, id = id)
    dep_var_nm_sets <- var_set_maker_req_obj_nm_sets__(
      vm = vm,
      id = id,
      maker = maker
    )
    if (is.function(maker)) {
      dep_var_nm_sets <- lapply(dep_var_nm_sets, function(dep_var_nm_set) {
        setdiff(dep_var_nm_set, c("vm", "var_nms", "dep_var_nm_set"))
      })
    }
    var_nm_set <- var_set_var_nm_set_get(vm = vm, id = id)
    dt_list <- lapply(seq_along(dep_var_nm_sets), function(i) {
      dep_var_nm_set <- as.character(dep_var_nm_sets[[i]])
      if (length(dep_var_nm_set) == 0L || length(var_nm_set) == 0L) {
        return(NULL)
      }
      data.table::CJ(
        id = id,
        dep_var_nm_set_no = i,
        from_var_nm = dep_var_nm_set,
        to_var_nm = var_nm_set,
        sorted = FALSE,
        unique = TRUE
      )
    })
    data.table::rbindlist(dt_list)
  })
  edge_dt <- data.table::rbindlist(edge_dt_list)
  if (nrow(edge_dt) == 0L) {
    edge_dt <- data.table::data.table(
      id = ids[0L],
      dep_var_nm_set_no = integer(0L),
      from_var_nm = character(0L),
      to_var_nm = character(0L)
    )
  }
  return(edge_dt[])
}

maker_dag_ancestor_edge_dt__ <- function(edge_dt, var_nms) {
  # @codedoc_comment_block vame:::maker_dag_ancestor_edge_dt__
  # - Keep only those edges which lead to `var_nms`, directly or via other
  #   variables made by other `maker`s.
  # @codedoc_comment_block vame:::maker_dag_ancestor_edge_dt__
  keep <- edge_dt[["to_var_nm"]] %in% var_nms
  repeat {
    keep_next <- keep |
      edge_dt[["to_var_nm"]] %in% edge_dt[["from_var_nm"]][keep]
    if (identical(keep_next, keep)) {
      break
    }
    keep <- keep_next
  }
  return(edge_dt[keep, ])
}

maker_dag_paths__ <- function(from_var_nms, to_var_nms) {
  # @codedoc_comment_block vame:::maker_dag_paths__
  # - Each path starts from a variable name that is not made by any `maker`
  #   (a root) and continues until a variable name which is not used by any
  #   `maker` (a leaf) is reached. If a variable name is encountered twice
  #   in the same path, i.e. there is a cycle, the path ends there and
  #   is marked with `(cycle)`. Variables that are only part of cycles
  #   and have no root are also handled.
  # @codedoc_comment_block vame:::maker_dag_paths__
  dbc::assert_prod_input_is_character_nonNA_vector(from_var_nms)
  dbc::assert_prod_input_is_character_nonNA_vector(to_var_nms)
  is_dup <- duplicated(data.frame(from_var_nms, to_var_nms))
  from_var_nms <- from_var_nms[!is_dup]
  to_var_nms <- to_var_nms[!is_dup]
  node_var_nms <- unique(c(from_var_nms, to_var_nms))
  child_var_nm_list <- lapply(node_var_nms, function(var_nm) {
    to_var_nms[from_var_nms == var_nm]
  })
  names(child_var_nm_list) <- node_var_nms
  paths <- list()
  visited <- character(0L)
  walk <- function(path) {
    var_nm <- path[length(path)]
    visited <<- union(visited, var_nm)
    child_var_nms <- child_var_nm_list[[var_nm]]
    if (length(child_var_nms) == 0L) {
      paths[[length(paths) + 1L]] <<- path
      return(invisible(NULL))
    }
    for (child_var_nm in child_var_nms) {
      if (child_var_nm %in% path) {
        cycle_path <- c(path, child_var_nm)
        attr(cycle_path, "is_cycle") <- TRUE
        paths[[length(paths) + 1L]] <<- cycle_path
      } else {
        walk(c(path, child_var_nm))
      }
    }
    return(invisible(NULL))
  }
  root_var_nms <- setdiff(node_var_nms, to_var_nms)
  for (root_var_nm in root_var_nms) {
    walk(root_var_nm)
  }
  while (length(unvisited_var_nms <- setdiff(node_var_nms, visited)) > 0L) {
    walk(unvisited_var_nms[1L])
  }
  return(paths)
}
