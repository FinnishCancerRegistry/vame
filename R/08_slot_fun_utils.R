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
