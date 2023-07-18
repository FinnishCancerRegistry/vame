value_space_type_report_funs__ <- function() {
  list(
    dt = function(x, x_nm = NULL, call = NULL) {
      x_nm <- dbc::handle_arg_x_nm(x_nm)
      call <- dbc::handle_arg_call(call)
      dbc::report_is_data_table(
        x[["dt"]],
        x_nm = paste0(x_nm, "[[\"dt\"]]"),
        call = call
      )
    },
    set = function(x, x_nm = NULL, call = NULL) {
      x_nm <- dbc::handle_arg_x_nm(x_nm)
      call <- dbc::handle_arg_call(call)
      dbc::report_is_vector(
        x[["set"]],
        x_nm = paste0(x_nm, "[[\"set\"]]"),
        call = call
      )
    },
    expr = function(x, x_nm = NULL, call = NULL) {
      x_nm <- dbc::handle_arg_x_nm(x_nm)
      call <- dbc::handle_arg_call(call)
      dbc::report_is_language_object(
        x[["expr"]],
        x_nm = paste0(x_nm, "[[\"expr\"]]"),
        call = call
      )
    },
    fun = function(x, x_nm = NULL, call = NULL) {
      x_nm <- dbc::handle_arg_x_nm(x_nm)
      call <- dbc::handle_arg_call(call)
      dbc::report_is_function(
        x[["fun"]],
        x_nm = paste0(x_nm, "[[\"fun\"]]"),
        call = call
      )
    },
    unrestricted = function(x, x_nm = NULL, call = NULL) {
      x_nm <- dbc::handle_arg_x_nm(x_nm)
      x_nm <- paste0(x_nm, "[[\"unrestricted\"]]")
      x <- x[["unrestricted"]]
      call <- dbc::handle_arg_call(call)
      rbind(
        dbc::report_is_list(
          x = x,
          x_nm = x_nm,
          call = call
        ),
        dbc::report_has_length(
          x,
          expected_length = 1L,
          x_nm = x_nm, call = call
        ),
        dbc::report_has_names(
          x,
          required_names = "class_set",
          x_nm = x_nm, call = call
        ),
        dbc::report_is_character_nonNA_vector(
          x[["class_set"]],
          x_nm = paste0(x_nm, "[[\"class_set\"]]"),
          call = call
        )
      )
    },
    regex = function(x, x_nm = NULL, call = NULL) {
      x_nm <- dbc::handle_arg_x_nm(x_nm)
      x_nm <- paste0(x_nm, "[[\"regex\"]]")
      x <- x[["regex"]]
      call <- dbc::handle_arg_call(call)
      dbc::report_is_character_nonNA_atom(
        x,
        x_nm = x_nm, call = call
      )
    },
    bounds = function(x, x_nm = NULL, call = NULL) {
      x_nm <- dbc::handle_arg_x_nm(x_nm)
      x_nm <- paste0(x_nm, "[[\"bounds\"]]")
      x <- x[["bounds"]]
      call <- dbc::handle_arg_call(call)
      rbind(
        dbc::report_is_list(
          x,
          x_nm = x_nm, call = call
        ),
        dbc::report_has_length(
          x,
          expected_length = 4L,
          x_nm = x_nm, call = call
        ),
        dbc::report_has_names(
          x,
          required_names = c("lo", "hi", "lo_inclusive", "hi_inclusive"),
          x_nm = x_nm, call = call
        ),
        dbc::report_is_logical_nonNA_atom(
          x[["lo_inclusive"]],
          x_nm = paste0(x_nm, "[[\"lo_inclusive\"]]"),
          call = call
        ),
        dbc::report_is_logical_nonNA_atom(
          x[["hi_inclusive"]],
          x_nm = paste0(x_nm, "[[\"hi_inclusive\"]]"),
          call = call
        )
      )
    }
  )
}

value_space_type_assertion_funs__ <- function() {
  report_funs <- value_space_type_report_funs__()
  assertion_funs <- lapply(report_funs, function(rf) {
    af <- function(x, x_nm = NULL, call = NULL, assertion_type = NULL) {
      x_nm <- dbc::handle_arg_x_nm(x_nm)
      call <- dbc::handle_arg_call(call)
      assertion_type <- dbc::handle_arg_assertion_type(assertion_type)
      df <- rf(x = x, x_nm = x_nm, call = call)
      dbc::report_to_assertion(
        df,
        assertion_type = assertion_type,
        raise_error_call = call
      )
    }
    return(af)
  })
  return(assertion_funs)
}

value_space_type_names__ <- function() {
  names(value_space_type_report_funs__())
}

value_space_value_assertion_funs__ <- function() {
  list(
    dt = function(
      x,
      x_nm = NULL,
      call = NULL,
      assertion_type = NULL,
      value_space,
      var_nm
    ) {
      x_nm <- dbc::handle_arg_x_nm(x_nm)
      call <- dbc::handle_arg_call(call)
      assertion_type <- dbc::handle_arg_assertion_type(assertion_type)
      value_space_type_assertion_funs__()[["dt"]](
        x = value_space,
        assertion_type = "prod_input"
      )
      dbc::assert_has_names(
        x = value_space[["dt"]],
        call = call,
        required_names = var_nm,
        assertion_type = "prod_input"
      )
      value_space <- list(set = value_space[["dt"]][[var_nm]])
      fun <- value_space_value_assertion_funs__()[["set"]]
      do.call(fun, mget(names(formals(fun))), quote = TRUE)
    },
    set = function(
      x,
      x_nm = NULL,
      call = NULL,
      assertion_type = NULL,
      value_space,
      var_nm
    ) {
      x_nm <- dbc::handle_arg_x_nm(x_nm)
      call <- dbc::handle_arg_call(call)
      assertion_type <- dbc::handle_arg_assertion_type(assertion_type)
      value_space_type_assertion_funs__()[["set"]](
        x = value_space,
        assertion_type = "prod_input"
      )
      dbc::assert_is_identical(
        x = class(x),
        x_nm = paste0("class(", x_nm, ")"),
        y = class(value_space[["set"]]),
        y_nm = deparse1(class(value_space[["set"]])),
        call = call,
        assertion_type = assertion_type
      )
      dbc::assert_vector_elems_are_in_set(
        x = x,
        x_nm = x_nm,
        call = call,
        assertion_type = assertion_type,
        set = value_space[["set"]]
      )
    },
    unrestricted = function(
      x,
      x_nm = NULL,
      call = NULL,
      assertion_type = NULL,
      value_space,
      var_nm
    ) {
      # @codedoc_comment_block news("vame::VariableMetadata", "2023-07-18", "0.1.7")
      # New `value_space` type: "unrestricted". Use this when a variable must
      # be of a certain class but can take any value. E.g.
      # `list(unrestricted = list(class_set = c("IDate", "Date")))`.
      # @codedoc_comment_block news("vame::VariableMetadata", "2023-07-18", "0.1.7")
      x_nm <- dbc::handle_arg_x_nm(x_nm)
      call <- dbc::handle_arg_call(call)
      assertion_type <- dbc::handle_arg_assertion_type(assertion_type)
      value_space_type_assertion_funs__()[["unrestricted"]](
        x = value_space,
        assertion_type = "prod_input"
      )
      dbc::assert_is_identical(
        x = class(x),
        x_nm = paste0("class(", x_nm, ")"),
        y = value_space[["unrestricted"]][["class_set"]],
        y_nm = deparse1(value_space[["unrestricted"]][["class_set"]]),
        call = call,
        assertion_type = assertion_type
      )
    },
    regex = function(
      x,
      x_nm = NULL,
      call = NULL,
      assertion_type = NULL,
      value_space,
      var_nm
    ) {
      # @codedoc_comment_block news("vame::VariableMetadata", "2023-07-18", "0.1.7")
      # New `value_space` type: "regex". Use this when all values of a variable
      # must match a specific regex. E.g. `list(regex = "^[a-z]$")`.
      # @codedoc_comment_block news("vame::VariableMetadata", "2023-07-18", "0.1.7")
      x_nm <- dbc::handle_arg_x_nm(x_nm)
      call <- dbc::handle_arg_call(call)
      assertion_type <- dbc::handle_arg_assertion_type(assertion_type)
      value_space_type_assertion_funs__()[["regex"]](
        x = value_space,
        assertion_type = "prod_input"
      )
      dbc::assert_match_regex(
        x = x,
        x_nm = x_nm,
        call = call,
        assertion_type = assertion_type,
        grepl.arg.list = list(pattern = value_space[["regex"]])
      )
    },
    bounds = function(
      x,
      x_nm = NULL,
      call = NULL,
      assertion_type = NULL,
      value_space,
      var_nm
    ) {
      value_space_type_assertion_funs__()[["bounds"]](
        x = value_space,
        assertion_type = "prod_input"
      )
      b <- value_space[["bounds"]]
      if (b[["lo_inclusive"]]) {
        dbc::assert_is_gte(
          x = x,
          lo = b[["lo"]],
          x_nm = x_nm,
          call = call,
          assertion_type = assertion_type
        )
      } else {
        dbc::assert_is_gt(
          x = x,
          lo = b[["lo"]],
          x_nm = x_nm,
          call = call,
          assertion_type = assertion_type
        )
      }
      if (b[["hi_inclusive"]]) {
        dbc::assert_is_lte(
          x = x,
          hi = b[["hi"]],
          x_nm = x_nm,
          call = call,
          assertion_type = assertion_type
        )
      } else {
        dbc::assert_is_lt(
          x = x,
          hi = b[["hi"]],
          x_nm = x_nm,
          call = call,
          assertion_type = assertion_type
        )
      }
    }
  )
}
