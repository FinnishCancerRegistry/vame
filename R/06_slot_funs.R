# utils ------------------------------------------------------------------------
call_hidden_vame_fun__ <- function(vm, fun_nm, arg_list = NULL)  {
  call <- create_call__(
    fun_nm = paste0("environment(vm@var_assert)[[\"", fun_nm, "\"]]"),
    arg_list = arg_list,
    arg_search_env = parent.frame(1L),
    fun_search_env = parent.frame(1L)
  )
  eval(expr = call, envir = parent.frame(1L))
}

call_slot_fun_alias__ <- function(fun_nm, self, arg_list = NULL) {
  call <- create_call__(
    fun_nm = paste0("vame::", fun_nm),
    arg_list = arg_list,
    arg_search_env = parent.frame(1L),
    fun_search_env = environment(call_slot_fun_alias__)
  )
  call[["vm"]] <- substitute(self)
  eval(call, envir = parent.frame(1L))
}

doc_slot_fun__ <- function(fun_nm, description) {
  c(
    "@rdname VariableMetadata-class",
    paste0("@name ", fun_nm),
    paste0("@slot ", fun_nm),
    description,
    "",
    "Usage alternatives given `VariableMetadata` object `vm`:",
    "",
    paste0("`vm@", fun_nm, "(id, env)`"),
    "",
    paste0("`vame::", fun_nm, "(vm, id, env)`"),
    "",
    "@export"
  )
}

# var_set funs -----------------------------------------------------------------
#' @eval doc_slot_fun__(
#'   "var_set_value_space_eval",
#'   "Retrieve and evaluate value space for a `var_set` given its `id`."
#' )
var_set_value_space_eval <- function(vm, id, env = NULL) {
  # @codedoc_comment_block news("vm@var_set_value_space_eval", "2023-07-03", "0.1.1")
  # New slot `vm@var_set_value_space_eval`.
  # @codedoc_comment_block news("vm@var_set_value_space_eval", "2023-07-03", "0.1.1")
  # @codedoc_comment_block news("vm@var_set_value_space_eval", "2023-07-17", "0.1.7")
  # New exported fun `vame::var_set_value_space_eval` --- alternative for
  # `vm@var_set_value_space_eval`.
  # @codedoc_comment_block news("vm@var_set_value_space_eval", "2023-07-17", "0.1.7")

  #' @param vm `[VariableMetadata]` (no default)
  #'
  #' A `VariableMetadata` object.
  dbc::assert_inherits(vm, required_class = "VariableMetadata")

  #' @param id `[any]` (no default)
  #'
  #' The ID of a `var_set`.
  call_hidden_vame_fun__(vm, "assert_is_var_set_id", list(id = id))
  call_hidden_vame_fun__(vm, "assert_var_set_value_space_is_defined")
  #' @param env `[NULL, environment]` (default `NULL`)
  #'
  #' - `NULL`: Take `env <- parent.frame(1L)`.
  #' - `environment`: If the value space is of type `expr`, evaluate it in this
  #'   environment.
  dbc::assert_is_one_of(
    env,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_environment)
  )
  if (is.null(env)) {
    env <- parent.frame(1L)
  }
  vs_expr <- substitute(
    vm@var_set_meta_get(id = id, meta_nm = "value_space"),
    list(id = id)
  )
  value_space <- eval(vs_expr)
  this_call <- match.call()
  call_hidden_vame_fun__(
    vm,
    "assert_is_value_space",
    list(
      x = value_space,
      x_nm = deparse1(vs_expr),
      call = this_call,
      assertion_type = "general"
    )
  )
  var_nms <- vm@var_set_meta_get(id = id, meta_nm = "var_nm_set")
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
  return(value_space)
}

# vame funs --------------------------------------------------------------------
#' @eval doc_slot_fun__(
#'   "vame_copy",
#'   "Take a deep copy of a VariableMetadata object. See `?data.table::copy`."
#' )
vame_copy <- function(vm) {
  # @codedoc_comment_block news("vm@vame_copy", "2023-08-10", "0.1.8")
  # New slot fun `vm@vame_copy` + new exported fun `vame::vame_copy`.
  # @codedoc_comment_block news("vm@vame_copy", "2023-08-10", "0.1.8")
  vd <- data.table::copy(call_hidden_vame_fun__(vm, "vd_get"))
  vsd <- data.table::copy(call_hidden_vame_fun__(vm, "vsd_get"))
  out <- vame::VariableMetadata(var_dt = vd, var_set_dt = vsd)
  return(out)
}