value_space_type_report_funs__ <- function() {
  list(
    dt = function(x, x_nm, call) {
      dbc::report_is_data_table(
        x,
        x_nm = x_nm, call = call
      )
    },
    set = function(x, x_nm, call) {
      dbc::report_is_vector(
        x,
        x_nm = x_nm, call = call
      )
    },
    expr = function(x, x_nm, call) {
      dbc::report_is_language_object(
        x,
        x_nm = x_nm, call = call
      )
    },
    fun = function(x, x_nm, call) {
      dbc::report_is_function(
        x,
        x_nm = x_nm, call = call
      )
    },
    unrestricted = function(x, x_nm, call) {
      rbind(
        dbc::report_is_list(
          x,
          x_nm = x_nm, call = call
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
    regex = function(x, x_nm, call) {
      dbc::report_is_character_nonNA_atom(
        x,
        x_nm = x_nm, call = call
      )
    },
    bounds = function(x, x_nm, call) {
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

value_space_type_names__ <- function() {
  names(value_space_type_report_funs__)
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
      x_nm <- dbc::handle_arg_x_nm(x_nm)
      call <- dbc::handle_arg_call(call)
      assertion_type <- dbc::handle_arg_assertion_type(assertion_type)
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
      x_nm <- dbc::handle_arg_x_nm(x_nm)
      call <- dbc::handle_arg_call(call)
      assertion_type <- dbc::handle_arg_assertion_type(assertion_type)
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
      dbc::assert_prod_interim_is_list(
        value_space,
        x_nm = x_nm,
        call = call,
        assertion_type = assertion_type
      )
      dbc::assert_prod_interim_has_length(
        value_space,
        expected_length = 4L,
        x_nm = x_nm,
        call = call,
        assertion_type = assertion_type
      )
      dbc::assert_prod_interim_has_names(
        value_space,
        required_names = c("lo", "hi", "lo_inclusive", "hi_inclusive"),
        x_nm = x_nm,
        call = call,
        assertion_type = assertion_type
      )
      if (value_space[["lo_inclusive"]]) {
        dbc::assert_is_gte(
          x = x,
          lo = value_space[["lo"]],
          x_nm = x_nm,
          call = call,
          assertion_type = assertion_type
        )
      } else {
        dbc::assert_is_gt(
          x = x,
          lo = value_space[["lo"]],
          x_nm = x_nm,
          call = call,
          assertion_type = assertion_type
        )
      }
      if (value_space[["hi_inclusive"]]) {
        dbc::assert_is_lte(
          x = x,
          hi = value_space[["hi"]],
          x_nm = x_nm,
          call = call,
          assertion_type = assertion_type
        )
      } else {
        dbc::assert_is_lt(
          x = x,
          hi = value_space[["hi"]],
          x_nm = x_nm,
          call = call,
          assertion_type = assertion_type
        )
      }
    }
  )
}
