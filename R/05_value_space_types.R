value_space_type_funs__ <- list(
  dt = list(
    report = function(x, x_nm = NULL, call = NULL) {
      x_nm <- dbc::handle_arg_x_nm(x_nm)
      call <- dbc::handle_arg_call(call)
      # @codedoc_comment_block types(var_set_dt$value_space)
      # - `"dt"`: A `data.table`. The column names must be the same as those
      #   in the variable set. This can be used to define the (joint) value
      #   space of one or more variables.
      #   E.g. `list(dt = data.table::CJ(a = 1:2, b = 3:4))`.
      # @codedoc_comment_block types(var_set_dt$value_space)
      dbc::report_is_data_table(
        x[["dt"]],
        x_nm = paste0(x_nm, "[[\"dt\"]]"),
        call = call
      )
    },
    var_value_assertion = function(
      x,
      x_nm = NULL,
      call = NULL,
      assertion_type = NULL,
      value_space,
      var_nm
    ) {
      dbc::handle_args_inplace()
      get_value_space_type_assertion_fun__("dt")(
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
      fun <- get_var_value_assertion_fun__("set")
      do.call(fun, mget(names(formals(fun))), quote = TRUE)
    },
    sampler = function(x, var_nms, n) {
      # @codedoc_comment_block defaults(var_set_dt$sampler)
      #   + `dt`: Sample uniformly rows from `data.table` with replacement.
      # @codedoc_comment_block defaults(var_set_dt$sampler)
      subset <- sample(nrow(x[["dt"]]), size = n, replace = TRUE)
      out <- x[["dt"]][
        i = subset,
        j = .SD,
        .SDcols = var_nms
      ]
      return(out[])
    }
  ),
  set = list(
    report = function(x, x_nm = NULL, call = NULL) {
      x_nm <- dbc::handle_arg_x_nm(x_nm)
      call <- dbc::handle_arg_call(call)
      # @codedoc_comment_block types(var_set_dt$value_space)
      # - `"set"`: A vector of any kinds of values. Only allowed for a variable
      #   set containing exactly one variable.
      #   E.g. `list(set = 1:2)`.
      # @codedoc_comment_block types(var_set_dt$value_space)
      dbc::report_is_vector(
        x[["set"]],
        x_nm = paste0(x_nm, "[[\"set\"]]"),
        call = call
      )
    },
    var_value_assertion = function(
      x,
      x_nm = NULL,
      call = NULL,
      assertion_type = NULL,
      value_space,
      var_nm
    ) {
      dbc::handle_args_inplace()
      get_value_space_type_assertion_fun__("set")(
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
    sampler = function(x, var_nms, n) {
      # @codedoc_comment_block defaults(var_set_dt$sampler)
      #   + `set`: Sample uniformly from set with replacement.
      # @codedoc_comment_block defaults(var_set_dt$sampler)
      out <- sample(x[["set"]], size = n, replace = TRUE)
      return(out)
    }
  ),
  expr = list(
    report = function(x, x_nm = NULL, call = NULL) {
      x_nm <- dbc::handle_arg_x_nm(x_nm)
      call <- dbc::handle_arg_call(call)
      # @codedoc_comment_block types(var_set_dt$value_space)
      # - `"expr"`: An unevaluated R expression.
      #   This can be used to define the (joint) value
      #   space of one or more variables. What to do with the result of the
      #   expression is deduced from the type of the result --- e.g. a 
      #   `data.table` result will be handled in the same manner as if the
      #   `value_space` had been of type `dt`. This can be useful for making use
      #   of functions to define the value space:
      #   E.g. `list(expr = quote(my_fun()))`.
      # @codedoc_comment_block types(var_set_dt$value_space)
      dbc::report_is_language_object(
        x[["expr"]],
        x_nm = paste0(x_nm, "[[\"expr\"]]"),
        call = call
      )
    }
    # @codedoc_comment_block defaults(var_set_dt$sampler)
    #   + `expr` & `fun`: Handling depends on the type of output: A vector
    #      output is handled as a `set` and a `data.table` output as a `dt`.
    # @codedoc_comment_block defaults(var_set_dt$sampler)
  ),
  fun = list(
    report = function(x, x_nm = NULL, call = NULL) {
      x_nm <- dbc::handle_arg_x_nm(x_nm)
      call <- dbc::handle_arg_call(call)
      # @codedoc_comment_block types(var_set_dt$value_space)
      # - `"fun"`: A function.
      #   The idea is the same as `expr`, but assigning a function as the
      #   value space is less safe: The function's enclosing environment may
      #   not be what was intended if a `VariableMetadata` object is read from
      #   disk.
      #   E.g. `list(fun = my_fun)`.
      # @codedoc_comment_block types(var_set_dt$value_space)
      dbc::report_is_function(
        x[["fun"]],
        x_nm = paste0(x_nm, "[[\"fun\"]]"),
        call = call
      )
    }
  ),
  unrestricted = list(
    report = function(x, x_nm = NULL, call = NULL) {
      x_nm <- dbc::handle_arg_x_nm(x_nm)
      x_nm <- paste0(x_nm, "[[\"unrestricted\"]]")
      x <- x[["unrestricted"]]
      call <- dbc::handle_arg_call(call)
      # @codedoc_comment_block types(var_set_dt$value_space)
      # - `"unrestricted"`: A list with named element `class_set`.
      #   Sometimes a variable has no restrictions besides its class.
      #   String variables can often have any value.
      #   E.g. `list(unrestricted = list(class_set = "character"))`.
      # @codedoc_comment_block types(var_set_dt$value_space)
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
    var_value_assertion = function(
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
      dbc::handle_args_inplace()
      get_value_space_type_assertion_fun__("unrestricted")(
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
    sampler = function(x, var_nms, n) {
      # @codedoc_comment_block defaults(var_set_dt$sampler)
      #   + `unrestricted`: If `class_set = "character"`, sample random strings
      #     of length 16 using character pool `c(letters, LETTERS, 0:9)`. If
      #     `class_set` is anything else, raise an error because no sampler is
      #     defined for the general case.
      # @codedoc_comment_block defaults(var_set_dt$sampler)

      if (!identical(x[["unrestricted"]][["class_set"]], "character")) {
        stop("No default sampler defined for value_space_type ",
             "\"unrestricted\" unless class_set = \"character\"")
      }
      pool <- c(letters, LETTERS, 0:9)
      out <- vapply(
        seq_len(n),
        function(i) {
          paste0(sample(pool, size = 16L, replace = TRUE), collapse = "")
        },
        character(1L)
      )
      return(out)
    }
  ),
  regex = list(
    report = function(x, x_nm = NULL, call = NULL) {
      # @codedoc_comment_block types(var_set_dt$value_space)
      # - `"regex"`: A character string regular expression.
      #   Some string variables are known in advance to always match a specific
      #   regular expression.
      #   E.g. `list(regex = "^C[0-9.]+$")`.
      # @codedoc_comment_block types(var_set_dt$value_space)
      x_nm <- dbc::handle_arg_x_nm(x_nm)
      x_nm <- paste0(x_nm, "[[\"regex\"]]")
      x <- x[["regex"]]
      call <- dbc::handle_arg_call(call)
      dbc::report_is_character_nonNA_atom(
        x,
        x_nm = x_nm, call = call
      )
    },
    var_value_assertion = function(
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
      dbc::handle_args_inplace()
      get_value_space_type_assertion_fun__("regex")(
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
    sampler = function(x, var_nms, n) {
      # @codedoc_comment_block defaults(var_set_dt$sampler)
      #   + `regex`: Raise error because no sampler defined.
      # @codedoc_comment_block defaults(var_set_dt$sampler)
      stop("No default sampler for value_space_type \"regex\"")
    }
  ),
  bounds = list(
    report = function(x, x_nm = NULL, call = NULL) {
      # @codedoc_comment_block types(var_set_dt$value_space)
      # - `"bounds"`: A list defining the upper and lower bounds.
      #   You can define upper and lower limits for one variable with this
      #   approach.
      #   E.g. `list(bounds = list(lo = 0.0, hi = 1.0, lo_inclusive = FALSE, hi_inclusive = TRUE))`
      # @codedoc_comment_block types(var_set_dt$value_space)
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
    },
    var_value_assertion = function(
      x,
      x_nm = NULL,
      call = NULL,
      assertion_type = NULL,
      value_space,
      var_nm
    ) {
      get_value_space_type_assertion_fun__("bounds")(
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
    },
    sampler = function(x, var_nms, n) {
      # @codedoc_comment_block defaults(var_set_dt$sampler)
      #   + `bounds`: Sample uniformly between `lo` and `hi` --- with
      #     replacement from `lo:hi` if `storage.mode(lo) == "integer"`.
      # @codedoc_comment_block defaults(var_set_dt$sampler)
      sample_lo <- x[["bounds"]][["lo"]]
      sample_hi <- x[["bounds"]][["hi"]]
      out <- rep(sample_lo, n) # to ensure output has correct class
      if (storage.mode(sample_lo) == "integer") {
        if (!x[["bounds"]][["lo_inclusive"]]) {
          sample_lo <- sample_lo + 1L
        }
        if (!x[["bounds"]][["hi_inclusive"]]) {
          sample_hi <- sample_hi - 1L
        }
        out[1:n] <- sample(x = sample_lo:sample_hi, size = n, replace = TRUE)
      } else {
        out[1:n] <- runif(n = n, min = sample_lo, max = sample_hi)
      }
      return(out)
    }
  )
)
for (vst in names(value_space_type_funs__)) {
  value_space_type_funs__[[vst]][["assertion"]] <- eval(substitute(
    function(
      x,
      x_nm = NULL,
      call = NULL,
      assertion_type = NULL
    ) {
      dbc::handle_args_inplace()
      df <- value_space_type_funs__[[VST]][["report"]](
        x = x,
        x_nm = x_nm,
        call = call
      )
      dbc::report_to_assertion(
        df,
        assertion_type = assertion_type,
        raise_error_call = call
      )
    },
    list(VST = vst)
  ))
}
rm(list = "vst")

get_value_space_type_fun__ <- function(value_space_type, fun_nm) {
  dbc::assert_atom_is_in_set(
    value_space_type,
    set = names(value_space_type_funs__),
    assertion_type = "prod_input"
  )
  x <- value_space_type_funs__[[value_space_type]]
  dbc::assert_atom_is_in_set(
    fun_nm,
    set = names(x),
    assertion_type = "prod_input"
  )
  x[[fun_nm]]
}

get_value_space_type_report_fun__ <- function(value_space_type) {
  get_value_space_type_fun__(value_space_type, "report")
}

get_value_space_type_assertion_fun__ <- function(value_space_type) {
  get_value_space_type_fun__(value_space_type, "assertion")
}

get_var_value_assertion_fun__ <- function(value_space_type) {
  get_value_space_type_fun__(value_space_type, "var_value_assertion")
}

get_value_space_type_names__ <- function() {
  return(names(value_space_type_funs__))
}
