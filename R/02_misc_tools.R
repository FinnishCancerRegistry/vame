dt_independent_frame_dependent_contents__ <- function(dt, col_nms) {
  out <- data.table::setDT(lapply(col_nms, function(col_nm) dt[[col_nm]]))
  data.table::setnames(out, col_nms)
  return(out[])
}

dt_join_assign__ <- function(
  dt,
  i,
  on,
  dt_col_nms,
  i_col_nms
) {
  expr_rhs <- parse(
    text = paste0("list(", paste0(i_col_nms, collapse = ", "), ")")
  )[[1]]
  expr <- substitute(
    dt[
      i = i,
      on = on,
      j = LHS := RHS
    ],
    list(LHS = dt_col_nms, RHS = expr_rhs)
  )
  eval(expr)
}

create_call__ <- function(
  fun_nm,
  arg_list = NULL,
  arg_search_env = parent.frame(1L),
  fun_search_env = parent.frame(1L)
) {
  arg_list_expr <- substitute(arg_list)
  if (is.name(arg_list_expr)) {
    arg_list_expr <- eval(substitute(
      substitute(EXPR, environment()),
      list(EXPR = arg_list_expr)
    ), envir = parent.frame(1L))
  }
  if (is.null(arg_list_expr)) {
    fun <- eval(parse(text = fun_nm)[[1]], envir = fun_search_env)
    def_arg_nms <- intersect(names(formals(fun)), ls(arg_search_env))
    if (length(def_arg_nms) > 0) {
      arg_list_expr <- parse(text = paste0(
        "list(", def_arg_nms, " = ", def_arg_nms, ")"
      ))[[1]]
    } else {
      arg_list_expr <- quote(list())
    }
  }
  arg_list <- as.list(arg_list_expr)[-1]
  expr <- parse(text = paste0(fun_nm, "()"))[[1]]
  expr[names(arg_list)] <- arg_list
  return(expr)
}
