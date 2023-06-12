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
