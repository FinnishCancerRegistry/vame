call_hidden_vame_fun__ <- function(vm, fun_nm, arg_list = NULL)  {
  fun_env <- environment(vm@var_assert)
  fun <- fun_env[[fun_nm]]
  call_arg_list <- as.list(formals(fun))
  arg_list <- as.list(arg_list)
  call_arg_list[names(arg_list)] <- arg_list
  do.call(fun, call_arg_list, quote = TRUE)
}

call_slot_fun__ <- function(fun_nm, arg_list = NULL) {
  dbc::assert_prod_input_is_character_nonNA_atom(fun_nm)
  dbc::assert_prod_input_is_one_of(
    arg_list,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_uniquely_named_list)
  )
  pkg_env <- environment(call_slot_fun__)
  dbc::assert_prod_input_atom_is_in_set(
    fun_nm,
    set = ls(pkg_env)
  )
  fun <- pkg_env[[fun_nm]]
  if (is.null(arg_list)) {
    arg_list <- formals(fun)
    if ("..." %in% names(arg_list)) {
      arg_list["..."] <- NULL
    }
    arg_env <- parent.frame(1L)
    def_arg_nms <- intersect(names(arg_list), ls(arg_env))
    arg_list[def_arg_nms] <- as.list(arg_env)[def_arg_nms]
  }

  vm_env <- parent.frame(2L)
  vm_nm <- deparse1(utils::tail(sys.calls(), 2L)[[1L]][[1]][[2L]])
  vm <- get(vm_nm, envir = vm_env)
  vm
  arg_list[["vm"]] <- vm

  do.call(fun, arg_list, quote = TRUE, envir = parent.frame(1L))
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

#' @eval doc_slot_fun__(
#'   "var_set_value_space_eval",
#'   "Retrieve and evaluate value space for a `var_set` given its `id`."
#' )
var_set_value_space_eval <- function(vm, id, env = NULL) {
  # @codedoc_comment_block news("vm@var_set_value_space_eval", "2023-07-03", "0.1.1")
  # New slot `vm@var_set_value_space_eval`.
  # @codedoc_comment_block news("vm@var_set_value_space_eval", "2023-07-03", "0.1.1")

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
  vsd <- call_hidden_vame_fun__(vm, "vsd_get")
  pos <- call_hidden_vame_fun__(vm, "var_set_id_to_pos", list(id = id))
  value_space <- vsd[["value_space"]][[pos]]
  var_nms <- vsd[["var_nm_set"]][[pos]]
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