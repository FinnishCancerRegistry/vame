assert_is_variablemetadata <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL
) {
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  assertion_type <- dbc::handle_arg_assertion_type(assertion_type)
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
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  call <- dbc::handle_arg_call(call)
  assertion_type <- dbc::handle_arg_assertion_type(assertion_type)
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
    # - A function with arguments named `x` and `label_col_nm`.
    # @codedoc_comment_block specification(var_dt$labeler)
    dbc::assert_is_function_with_required_argument_names(
      x = x,
      x_nm = x_nm,
      call = call,
      required_argument_names = c("x", "label_col_nm"),
      assertion_type = assertion_type
    )
  } else if (data.table::is.data.table(x)) {
    # @codedoc_comment_block specification(var_dt$labeler)
    # - A `data.table` with column `level` and label columns --- you decide
    #   their names. For labels in different languages it is recommended to use
    #   ISO language codes as column names, e.g. "en".
    # @codedoc_comment_block specification(var_dt$labeler)
    dbc::assert_has_names(
      x = x,
      x_nm = x_nm,
      call = call,
      required_names = "level",
      assertion_type = assertion_type
    )
  } else if (is.language(x)) {
    # @codedoc_comment_block news("vm@var_labeler_set", "2023-12-04", "0.2.1")
    # A `labeler` can now also be of class `call`.
    # @codedoc_comment_block news("vm@var_labeler_set", "2023-12-04", "0.2.1")

    # @codedoc_comment_block specification(var_dt$labeler)
    # - An R expression object of class `call`.
    #   The expression must contain the variables `x` and `label_col_nm`.
    # @codedoc_comment_block specification(var_dt$labeler)
    report_df <- dbc::expressions_to_report(
      expressions = list(
        quote("x" %in% all.vars(x)),
        quote("label_col_nm" %in% all.vars(x))
      ),
      fail_messages = c(
        paste0("R expression `", deparse1(x), "` from object/expression `",
               x_nm , "` must contain variable `x`"),
        paste0("R expression `", deparse1(x), "` from object/expression `",
               x_nm , "` must contain variable `label_col_nm`")
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

assert_is_var_dt <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL
) {
  dbc::assert_is_data_table_with_required_names(
    x = x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type,
    required_names = c("var_nm")
  )
  # @codedoc_comment_block news("VariableMetadata", "2023-12-01", "0.2.0")
  # `label_dt` was renamed to `labeler`. 
  # @codedoc_comment_block news("VariableMetadata", "2023-12-01", "0.2.0")
  if ("labeler" %in% names(x)) {
    lapply(x[["labeler"]], assert_is_labeler,
           x_nm = x_nm,
           call = call,
           assertion_type = assertion_type)
  }
  if ("type" %in% names(x)) {
    # @codedoc_comment_block specification(var_dt$type)
    # `var_dt$type` must be a character string vector. Missing values are
    # allowed.
    # @codedoc_comment_block specification(var_dt$type)
    dbc::assert_is_character_vector(
      x = x[["type"]],
      x_nm = paste0(x_nm, "$type"),
      call = call,
      assertion_type = assertion_type
    )
  }
  return(invisible(NULL))
}

assert_is_var_set_dt <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL
) {
  dbc::assert_is_data_table_with_required_names(
    x = x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type,
    required_names = c("id", "var_nm_set")
  )
  if ("value_space" %in% names(x)) {
    lapply(x[["value_space"]], assert_is_value_space,
           x_nm = x_nm,
           call = call,
           assertion_type = assertion_type)
  }
  return(invisible(NULL))
}
