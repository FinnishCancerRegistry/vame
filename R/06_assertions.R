assert_is_variablemetadata <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL
) {
  dbc::handle_args_inplace()
  dbc::assert_inherits(
    x = x,
    x_nm = x_nm,
    call = call,
    required_class = "VariableMetadata",
    assertion_type = assertion_type
  )
}

assert_is_labeler <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL
) {
  dbc::handle_args_inplace()
  dbc::assert_is_one_of(
    x = x,
    x_nm = x_nm,
    call = call,
    funs = list(
      dbc::report_is_NULL,
      dbc::report_is_data_table,
      dbc::report_is_function,
      dbc::report_is_call
    ),
    assertion_type = assertion_type
  )
  if (is.function(x)) {
    # @codedoc_comment_block news("vm@var_labeler_set", "2023-12-01", "0.2.0")
    # A `labeler` can now be of type `function` in addition to `data.table`.
    # @codedoc_comment_block news("vm@var_labeler_set", "2023-12-01", "0.2.0")

    # @codedoc_comment_block specification(var_dt$labeler)
    # `var_dt$labeler` must be a list column.
    # A `labeler` for a variable can be one of these:
    #
    # - A function with arguments named `x` and `label_nm`.
    # @codedoc_comment_block specification(var_dt$labeler)
    dbc::assert_is_function_with_required_argument_names(
      x = x,
      x_nm = x_nm,
      call = call,
      required_argument_names = c("x", "label_nm"),
      assertion_type = assertion_type
    )
  } else if (data.table::is.data.table(x)) {
    # @codedoc_comment_block specification(var_dt$labeler)
    # - A `data.table` with column `x` and label columns --- you decide
    #   their names. For labels in different languages it is recommended to use
    #   ISO language codes as column names, e.g. "en".
    # @codedoc_comment_block specification(var_dt$labeler)
    # @codedoc_comment_block news("vame::VariableMetadata", "2024-02-01", "0.4.0")
    # `var_dt$labeler` of class `data.table` specs changed: Now column
    # containing values for the variable in question must be named `x`.
    # Formerly this was `level`.
    # @codedoc_comment_block news("vame::VariableMetadata", "2024-02-01", "0.4.0")
    dbc::assert_has_names(
      x = x,
      x_nm = x_nm,
      call = call,
      required_names = "x",
      assertion_type = assertion_type
    )
  } else if (is.language(x)) {
    # @codedoc_comment_block news("vm@var_labeler_set", "2023-12-04", "0.2.1")
    # A `labeler` can now also be of class `call`.
    # @codedoc_comment_block news("vm@var_labeler_set", "2023-12-04", "0.2.1")

    # @codedoc_comment_block specification(var_dt$labeler)
    # - An R expression object of class `call`.
    #   The expression must contain the variables `x` and `label_nm`.
    # @codedoc_comment_block specification(var_dt$labeler)
    report_df <- dbc::expressions_to_report(
      expressions = list(
        quote("x" %in% all.vars(x)),
        quote("label_nm" %in% all.vars(x))
      ),
      fail_messages = c(
        paste0("R expression `", deparse1(x), "` from object/expression `",
               x_nm, "` must contain variable `x`"),
        paste0("R expression `", deparse1(x), "` from object/expression `",
               x_nm, "` must contain variable `label_nm`")
      ),
      call = call,
      env = environment()
    )
    dbc::report_to_assertion(
      report_df,
      assertion_type = assertion_type,
      raise_error_call = call
    )
  }
}

assert_is_value_space <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL
) {
  dbc::handle_args_inplace()
  dbc::assert_has_one_of_classes(
    x,
    classes = c("NULL", "list")
  )
  if (is.null(x)) {
    return(invisible(NULL))
  }
  # @codedoc_comment_block specification(var_set_dt$value_space)
  # `var_set_dt$value_space` must be a list column.
  # A `value_space` for a variable set must be named list of length one or
  # `NULL`. Therefore `var_set_dt$value_space` is a list, in turn containing
  # lists of length one or `NULL` values.
  # The following element names are allowed and determine the type of the
  # `value_space`: ${deparse1(get_value_space_type_names__())}.
  #
  # The `value_space` element must be `NULL` or one of the following:
  # @codedoc_insert_comment_block types(var_set_dt$value_space)
  # @codedoc_comment_block specification(var_set_dt$value_space)
  dbc::assert_has_length(
    x = x,
    expected_length = 1L,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type
  )
  dbc::assert_atom_is_in_set(
    x = names(x),
    set = get_value_space_type_names__(),
    x_nm = paste0("names(", x_nm, ")"),
    call = call,
    assertion_type = assertion_type
  )
  dbc::report_to_assertion(
    get_value_space_type_report_fun__(names(x))(
      x = x,
      x_nm = x_nm,
      call = call
    ),
    raise_error_call = call,
    assertion_type = assertion_type
  )
}

test_is_value_space <- function(
  x,
  x_nm = NULL,
  call = NULL
) {
  out <- tryCatch(
    assert_is_value_space(x = x),
    error = function(e) e
  )
  return(!inherits(out, "error"))
}

assert_is_var_dt_column_type <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL
) {
  dbc::handle_args_inplace()
  # @codedoc_comment_block specification(var_dt$type)
  # `var_dt$type` must be a character string vector. Missing values are
  # allowed.
  # @codedoc_comment_block specification(var_dt$type)
  dbc::assert_is_character_vector(
    x = x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type
  )
}

assert_is_var_dt_column_is_harmonised <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL
) {
  dbc::handle_args_inplace()
  # @codedoc_comment_block specification(var_dt$is_harmonised)
  # `var_dt$is_harmonised` must be a logical vector. Missing values are
  # allowed.
  # @codedoc_comment_block specification(var_dt$is_harmonised)
  dbc::assert_is_logical_vector(
    x = x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type
  )
}

assert_is_var_dt <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL
) {
  dbc::handle_args_inplace()
  dbc::assert_is_data_table_with_required_names(
    x = x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type,
    required_names = c("var_nm")
  )
  # @codedoc_comment_block news("vame::VariableMetadata", "2025-02-28", "1.7.0")
  # `vame::VariableMetadata` now raises an error if `var_dt$var_nm` has any
  # duplicates.
  # @codedoc_comment_block news("vame::VariableMetadata", "2025-02-28", "1.7.0")
  dbc::assert_has_no_duplicates(
    x = x[["var_nm"]],
    x_nm = sprintf("%s[[\"var_nm\"]]", x_nm),
    call = call,
    assertion_type = assertion_type
  )
  # @codedoc_comment_block news("VariableMetadata", "2023-12-01", "0.2.0")
  # `label_dt` was renamed to `labeler`.
  # @codedoc_comment_block news("VariableMetadata", "2023-12-01", "0.2.0")
  lapply(intersect(c("labeler", "describer"), names(x)), function(col_nm) {
    lapply(seq_along(x[[col_nm]]), function(i) {
      fun_nm <- paste0("assert_is_", col_nm)
      do.call(
        fun_nm,
        list(
          x = x[[col_nm]][[i]],
          x_nm = sprintf("%s[[\"%s\"]][[%i]]", x_nm, col_nm, i),
          call = call,
          assertion_type = assertion_type
        ),
        quote = TRUE
      )
    })
  })
  fun_nms <- ls(envir = environment(assert_is_var_dt))
  var_dt_column_assertion_fun_nms <- fun_nms[grepl(
    "^assert_is_var_dt_column_",
    fun_nms
  )]
  for (afn in var_dt_column_assertion_fun_nms) {
    var_dt_col_nm <- sub("^assert_is_var_dt_column_", "", afn)
    if (var_dt_col_nm %in% names(x)) {
      do.call(
        afn,
        list(
          x = x[[var_dt_col_nm]],
          x_nm = sprintf("%s$%s", x_nm, var_dt_col_nm),
          call = call,
          assertion_type = assertion_type
        ),
        quote = TRUE
      )
    }
  }
  return(invisible(NULL))
}

assert_is_var_set_dt <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL
) {
  dbc::handle_args_inplace()
  dbc::assert_is_data_table_with_required_names(
    x = x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type,
    required_names = c("id", "var_nm_set")
  )
  # @codedoc_comment_block news("vame::VariableMetadata", "2025-02-28", "1.7.0")
  # `vame::VariableMetadata` now raises an error if `var_set_dt$id` has any
  # duplicates.
  # @codedoc_comment_block news("vame::VariableMetadata", "2025-02-28", "1.7.0")
  dbc::assert_has_no_duplicates(
    x = x[["id"]],
    x_nm = sprintf("%s[[\"id\"]]", x_nm),
    call = call,
    assertion_type = assertion_type
  )
  dbc::assert_is_list(
    x = x[["var_nm_set"]],
    x_nm = paste0(x_nm, "$var_nm_set"),
    call = call,
    assertion_type = assertion_type
  )
  for (i in seq_along(x[["var_nm_set"]])) {
    dbc::assert_is_character_nonNA_vector(
      x = x[["var_nm_set"]][[i]],
      x_nm = paste0(x_nm, "$var_nm_set[[", i, "]]"),
      call = call,
      assertion_type = assertion_type
    )
  }
  assert_col_nm_set <- c("value_space", "sampler", "maker")
  lapply(intersect(assert_col_nm_set, names(x)), function(col_nm) {
    dbc::assert_is_list(
      x = x[[col_nm]],
      x_nm = paste0(x_nm, "$", col_nm),
      call = call,
      assertion_type = assertion_type
    )
    lapply(seq_along(x[[col_nm]]), function(i) {
      assert_fun_nm <- paste0("assert_is_", col_nm)
      do.call(
        assert_fun_nm,
        list(
          x = x[[col_nm]][[i]],
          x_nm = sprintf("%s$%s[[%i]]", x_nm, col_nm, i),
          call = call,
          assertion_type = assertion_type
        ),
        quote = TRUE
      )
    })
  })
  return(invisible(NULL))
}

assert_is_sampler <- assert_is_value_space_sampler <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL
) {
  dbc::handle_args_inplace()
  # @codedoc_comment_block specification(var_set_dt$sampler)
  # @codedoc_comment_block specification(vame_list$sampler)
  # The `sampler` can be `NULL`, a `list`, a `function` or a `call` object.
  # A `NULL` `sampler`
  # object is considered to mean that one has not been defined.
  # @codedoc_comment_block specification(vame_list$sampler)
  # @codedoc_comment_block specification(var_set_dt$sampler)
  dbc::assert_has_one_of_classes(
    x = x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type,
    classes = c("NULL", "list", "function", "call", "{", "name")
  )
  if (is.null(x)) {
    return(invisible(NULL))
  } else if (is.function(x)) {
    # @codedoc_comment_block specification(var_set_dt$sampler)
    # @codedoc_comment_block specification(vame_list$sampler)
    # A `sampler` of type `function` must have arguments `x`, `vs`, and
    # `n`. `x` is the `vame::VariableMetadata` object itself,
    # `vs` is the pre-evaluated `value_space` for the variable set,
    # and `n` the number of samples.
    # @codedoc_comment_block specification(vame_list$sampler)
    # @codedoc_comment_block specification(var_set_dt$sampler)
    dbc::assert_is_function_with_required_argument_names(
      x = x,
      x_nm = x_nm,
      call = call,
      assertion_type = assertion_type,
      required_argument_names = c("x", "vs", "n")
    )
    x <- body(x)
    x_nm <- paste0("body(", x_nm, ")")
  } else if (inherits(x, "list")) {
    # @codedoc_comment_block specification(var_set_dt$sampler)
    # @codedoc_comment_block specification(vame_list$sampler)
    # A `sampler` of type `list` must have elements `sampler$dep_var_nm_set` and
    # `sampler$sampler`. `sampler$dep_var_nm_set` must be a character string
    # vector: use this to list names of variables needed for conditional
    # sampling. E.g. sampling `y` conditional on `x` means you should have
    # `dep_var_nm_set = "x"`. `sampler$sampler` must be a `function` or a
    # `call` object.
    # @codedoc_comment_block specification(vame_list$sampler)
    # @codedoc_comment_block specification(var_set_dt$sampler)
    dbc::assert_is_uniquely_named_list(
      x = x,
      x_nm = x_nm,
      call = call,
      assertion_type = assertion_type
    )
    dbc::assert_is_of_length(
      x = x,
      x_nm = x_nm,
      call = call,
      assertion_type = assertion_type,
      expected_length = 2L
    )
    dbc::assert_has_names(
      x = x,
      x_nm = x_nm,
      call = call,
      assertion_type = assertion_type,
      required_names = c("sampler", "dep_var_nm_set")
    )
    x <- x[["sampler"]]
    x_nm <- sprintf("%s[[\"sampler\"]]", x_nm)
  }
  # @codedoc_comment_block specification(var_set_dt$sampler)
  # @codedoc_comment_block specification(vame_list$sampler)
  # A `sampler` of type `call` must contain (mention) variable `n` ---
  # see `?all.vars`.
  # @codedoc_comment_block specification(vame_list$sampler)
  # @codedoc_comment_block specification(var_set_dt$sampler)
  report_df <- dbc::expressions_to_report(
    expressions = list(
      quote("n" %in% all.vars(x))
    ),
    fail_messages = c(
      paste0("R expression `", deparse1(x), "` from object/expression `",
             x_nm, "` must contain variable `n`")
    ),
    call = call,
    env = environment()
  )
  dbc::report_to_assertion(
    report_df,
    assertion_type = assertion_type,
    raise_error_call = call
  )
}

assert_is_vame_list <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL
) {
  dbc::handle_args_inplace()
  dbc::assert_is_one_of(
    x = x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_list)
  )
}

report_is_maker_var_aggregate <- function(
  x,
  x_nm = NULL,
  call = NULL
) {
  dbc::handle_args_inplace()
  dbc::report_is_identical(
    x = x,
    x_nm = x_nm,
    call = call,
    y = "var_aggregate",
    y_nm = NULL
  )
}
assert_is_maker <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL
) {
  dbc::handle_args_inplace()
  # @codedoc_comment_block specification(var_set_dt$maker)
  # The `maker` can be `NULL`, a `function`, or a `list`. A `NULL` `maker`
  # object is considered to mean that one has not been defined.
  # @codedoc_comment_block specification(var_set_dt$maker)
  dbc::assert_has_one_of_classes(
    x = x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type,
    classes = c("NULL", "function", "list")
  )
  if (is.null(x)) {
    return(invisible(NULL))
  } else if (is.function(x)) {
    # @codedoc_comment_block specification(var_set_dt$maker)
    # A `maker` of type `function` must have as arguments the invididual
    # required variables. Using `...` is not allowed to enable proper checks
    # on inputs when the `maker` is called.
    # @codedoc_comment_block specification(var_set_dt$maker)
    # @codedoc_comment_block news("vm@var_set_maker_set", "2024-06-19", "0.5.3")
    # `maker` objects of type `function` can no longer have `...` in their
    # definition to enable proper checks on inputs when the `maker` is called.
    # @codedoc_comment_block news("vm@var_set_maker_set", "2024-06-19", "0.5.3")
    dbc::assert(
      !"..." %in% names(formals(x)),
      x_nm = x_nm,
      call = call,
      assertion_type = assertion_type
    )
    return(invisible(NULL))
  }
  # @codedoc_comment_block specification(var_set_dt$maker)
  # A `maker` of type `list` must have element named `maker` and either
  # `dep_var_nm_set` of class `character` or `dep_var_nm_sets` of class `list`,
  # e.g.
  # `list(maker = quote(make(x, y)), dep_var_nm_set = c("x", "y"))`
  # or
  # `list(maker = quote(switch(dep_var_nm_set, b = b + 1L, c = c - 1L)), dep_var_nm_sets = list("b", "c"))`
  # Element `maker` must be either a `call` object or simply the string
  # `"var_aggregate"`, in which case `dep_var_nm_set` must be of length one.
  # Using `"var_aggregate"` is a shorthand that causes calling
  # `vm@var_aggregate`. E.g.
  # `list(maker = "var_aggregate", dep_var_nm_set = "b")`.
  # @codedoc_comment_block specification(var_set_dt$maker)
  dbc::assert_has_names(
    x = x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type,
    required_names = "maker"
  )
  dbc::assert_is_one_of(
    x = x[["maker"]],
    x_nm = sprintf("%s$maker", x_nm),
    call = call,
    assertion_type = assertion_type,
    funs = list(dbc::report_is_call,
                report_is_maker_var_aggregate)
  )
  if ("dep_var_nm_set" %in% names(x)) {
    dbc::assert_is_character_nonNA_vector(
      x = x[["dep_var_nm_set"]],
      x_nm = sprintf("%s$dep_var_nm_set", x_nm),
      call = call,
      assertion_type = assertion_type
    )
  } else if ("dep_var_nm_sets" %in% names(x)) {
    # @codedoc_comment_block news("vm@var_set_maker_set", "2024-10-26", "1.2.0")
    # A `maker` of type `list` can now have element `dep_var_nm_sets` instead
    # of `dep_var_nm_set`. `dep_var_nm_sets` must be a `list` of variable name
    # sets --- different allowed options. If your `maker` only works on one
    # specific set of dependency variables, it is easier for you to keep using
    # `dep_var_nm_set`. The possibility of using `dep_var_nm_sets` was added
    # to enable writing a `maker` such as
    # `quote(switch(dep_var_nm_set, b = b + 1L, c = c - 1L))`, where the target
    # variable can be made using either `b` or `c`.
    # @codedoc_comment_block news("vm@var_set_maker_set", "2024-10-26", "1.2.0")
    dbc::assert_is_list(
      x = x[["dep_var_nm_sets"]],
      x_nm = sprintf("%s$dep_var_nm_sets", x_nm),
      call = call,
      assertion_type = assertion_type
    )
    for (i in seq_along(x[["dep_var_nm_sets"]])) {
      dbc::assert_is_character_nonNA_vector(
        x = x[["dep_var_nm_sets"]][[i]],
        x_nm = sprintf("%s$dep_var_nm_sets[[%i]]", x_nm, i),
        call = call,
        assertion_type = assertion_type
      )
    }
  } else {
    x_nm <- paste0(x_nm, "$dep_var_nm_sets")
    stop("`", x_nm, "` must have either ",
         sprintf("`%s$%s`", x_nm, "dep_var_nm_set"), " or ",
         sprintf("`%s$%s`", x_nm, "dep_var_nm_sets"), ".")
  }
  return(invisible(NULL))
}

assert_is_description <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL
) {
  dbc::handle_args_inplace()
  # @codedoc_comment_block news("vame", "2024-09-10", "0.5.8")
  # An individual description text for a `describer` can now be a `character`
  # string vector (without `NA`) with more than one element. Formerly
  # length one was required.
  # @codedoc_comment_block news("vame", "2024-09-10", "0.5.8")
  dbc::assert_is_character_nonNA_vector(
    x = x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type
  )
  dbc::assert_is_gte(
    x = length(x),
    x_nm = sprintf("length(%s)", x_nm),
    call = call,
    assertion_type = assertion_type,
    lo = 1L
  )
}

assert_is_describer <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL
) {
  dbc::handle_args_inplace()
  # @codedoc_comment_block specification(var_dt$describer)
  # The `describer` can be `NULL`, a `list`, a `function`, or a `call` object.
  # A `NULL` `describer` object is considered to mean that one has not been
  # defined.
  # @codedoc_comment_block specification(var_dt$describer)
  dbc::assert_has_one_of_classes(
    x = x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type,
    classes = c("NULL", "function", "call", "{", "name", "list")
  )
  if (is.null(x)) {
    return(invisible(NULL))
  } else if (is.list(x)) {
    # @codedoc_comment_block specification(var_dt$describer)
    # A `describer` of type `list` must be named. Each element must be
    # a `character` string vector of length > 0. `NA` values are not allowed.
    # @codedoc_comment_block specification(var_dt$describer)
    dbc::assert_is_uniquely_named(
      x = x,
      x_nm = x_nm,
      call = call,
      assertion_type = assertion_type
    )
    for (i in seq_along(x)) {
      # @codedoc_comment_block news("vame", "2024-09-10", "0.5.8")
      # The elements of a `describer` of type `list` can now be vectors with
      # more than one element. Formerly only vectors of length one were allowed.
      # @codedoc_comment_block news("vame", "2024-09-10", "0.5.8")
      assert_is_description(
        x = x[[i]],
        x_nm = sprintf("%s[[%i]]", x_nm, i),
        call = call,
        assertion_type = assertion_type
      )
    }
    return(invisible(NULL))
  } else if (is.function(x)) {
    # @codedoc_comment_block specification(var_dt$describer)
    # A `describer` of type `function` must have argument `descr_nm`.
    # @codedoc_comment_block specification(var_dt$describer)
    dbc::assert_is_function_with_required_argument_names(
      x = x,
      x_nm = x_nm,
      call = call,
      assertion_type = assertion_type,
      required_argument_names = c("descr_nm")
    )
    x <- body(x)
    x_nm <- paste0("body(", x_nm, ")")
  }
  # @codedoc_comment_block specification(var_dt$describer)
  # A `describer` of type `call` must contain (mention) variable
  # `descr_nm`.
  # @codedoc_comment_block specification(var_dt$describer)
  report_df <- dbc::expressions_to_report(
    expressions = list(
      quote("descr_nm" %in% all.vars(x))
    ),
    fail_messages = c(
      paste0("R expression `", deparse1(x), "` from object/expression `",
             x_nm, "` must contain variable `descr_nm`")
    ),
    call = call,
    env = environment()
  )
  dbc::report_to_assertion(
    report_df,
    assertion_type = assertion_type,
    raise_error_call = call
  )
}

assert_is_arg_data <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL
) {
  dbc::handle_args_inplace()
  # @codedoc_comment_block doc_slot_fun_arg(data)
  # **`data`** `[NULL, data.frame, data.table, list]` (default `NULL`)
  #
  # - `NULL`: No data is used.
  # - `data.frame`/`data.table`: These variables are used as-is.
  # - `list`: Must by fully named. `list` elements are used as variables
  #   as-is, except the optional element `data$df` ---
  #   a `data.frame`/`data.table` --- is first turned into a `list`,
  #   and `data` (without `data$df`) and `data$df` are combined into one longer
  #   `list`. This allows you to easily pass a `data.frame`/`data.table` and
  #   necessary accompanying variables for settings etc.
  #   E.g.
  #   `data = list(df = data.frame(my_col_1 = 1, my_col_2 = 2), my_custom_setting = "something")`.
  #   Of course you can also pass
  #   `data = list(my_col_1 = 1, my_col_2 = 2, my_custom_setting = "something")`
  #   directly if that is more convenient.
  # @codedoc_comment_block doc_slot_fun_arg(data)
  # @codedoc_comment_block news("vm@var_set_make", "2024-02-19", "0.4.0")
  # `vm@var_set_make` argument `data` can now also be a `list` object.
  # @codedoc_comment_block news("vm@var_set_make", "2024-02-19", "0.4.0")
  dbc::assert_has_one_of_classes(
    x,
    classes = c("NULL", "data.frame", "data.table", "list"),
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type
  )
}

assert_is_arg_var_nms <- function(
  vm,
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL,
  must_exist = TRUE,
  allow_null = TRUE
) {
  dbc::handle_args_inplace()
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  if (allow_null) {
    dbc::assert_is_one_of(
      x,
      x_nm = x_nm,
      call = call,
      assertion_type = assertion_type,
      funs = list(dbc::report_is_NULL,
                  dbc::report_is_character_nonNA_vector)
    )
  } else {
    dbc::assert_is_character_nonNA_vector(
      x,
      x_nm = x_nm,
      call = call,
      assertion_type = assertion_type
    )
  }
  if (!is.null(x) && must_exist) {
    dbc::assert_vector_elems_are_in_set(
      x,
      x_nm = x_nm,
      call = call,
      assertion_type = assertion_type,
      set = var_meta_get_all(vm, "var_nm")
    )
  }
}

assert_is_var_nm <- function(
  vm,
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL,
  must_exist = TRUE
) {
  dbc::handle_args_inplace()
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  dbc::assert_is_character_nonNA_atom(
    x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type
  )
  if (must_exist) {
    dbc::assert_atom_is_in_set(
      x,
      x_nm = x_nm,
      call = call,
      assertion_type = assertion_type,
      set = var_meta_get_all(vm, "var_nm")
    )
  }
}

assert_is_id <- function(
  vm,
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL,
  must_exist = TRUE
) {
  dbc::handle_args_inplace()
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  dbc::assert_is_atom(
    x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type
  )
  if (must_exist) {
    dbc::assert_atom_is_in_set(
      x,
      x_nm = x_nm,
      call = call,
      assertion_type = assertion_type,
      set = var_set_meta_get_all(vm, "id")
    )
  }
}


assert_is_var_meta_nm <- function(
  vm,
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL
) {
  dbc::handle_args_inplace()
  assert_is_variablemetadata(
    vm,
    call = call,
    assertion_type = "prod_input"
  )
  dbc::assert_is_character_nonNA_atom(
    x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type
  )
  dbc::assert_atom_is_in_set(
    x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type,
    set = names(vd_get(vm))
  )
}

assert_is_var_set_id <- function(
  vm,
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL
) {
  dbc::handle_args_inplace()
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  dbc::assert_is_atom(
    x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type
  )
  dbc::assert_is_nonNA(
    x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type
  )
  dbc::assert_atom_is_in_set(
    x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type,
    set = vsd_get(vm)[["id"]]
  )
}

assert_is_var_set_meta_nm <- function(
  vm,
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL
) {
  dbc::handle_args_inplace()
  assert_is_variablemetadata(vm, assertion_type = "prod_input")
  dbc::assert_is_character_nonNA_atom(
    x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type
  )
  dbc::assert_atom_is_in_set(
    x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type,
    set = names(vsd_get(vm))
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


assert_is_arg_ids <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL,
  vm
) {
  dbc::handle_args_inplace()
  dbc::assert_is_one_of(
    x = x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_vector)
  )
  lapply(seq_along(x), function(i) {
    assert_is_var_set_id(
      vm = vm,
      x = x[[i]],
      x_nm = sprintf("x[[%i]]", i),
      call = call,
      assertion_type = assertion_type
    )
  })
}

assert_meta <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL,
  must_exist = TRUE,
  vm,
  meta_nm
) {
  dbc::handle_args_inplace()
  assert_fun_nm <- paste0("assert_is_", meta_nm)
  if (assert_fun_nm %in% ls(getNamespace("vame"))) {
    assert_fun <- getNamespace("vame")[[assert_fun_nm]]
    assert_fun_arg_list <- mget(ls())
    assert_fun_arg_list <- assert_fun_arg_list[
      intersect(names(assert_fun_arg_list), names(formals(assert_fun)))
    ]
    do.call(assert_fun, assert_fun_arg_list, quote = TRUE)
  }
}

assert_is_arg_optional_steps <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL
) {
  dbc::handle_args_inplace()
  # @codedoc_comment_block doc_slot_fun_arg(optional_steps)
  # **`optional_steps`** `[NULL, list]` (default `NULL`)
  #
  # - `NULL`: Function is called normally.
  # - `list`: These functions are called during the run. See **Description** /
  #   **Details**.
  # @codedoc_comment_block doc_slot_fun_arg(optional_steps)
  dbc::assert_has_one_of_classes(
    x = x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type,
    classes = c("list", "NULL")
  )
  if (inherits(x, "list")) {
    dbc::assert_is_uniquely_named(
      x = x,
      x_nm = x_nm,
      call = call,
      assertion_type = assertion_type
    )
    for (elem_nm in names(x)) {
      dbc::assert_is_function(
        x = x[[elem_nm]],
        x_nm = sprintf("%s[[%s]]", x_nm, elem_nm),
        call = call,
        assertion_type = assertion_type
      )
    }
  }
  return(invisible(NULL))
}
