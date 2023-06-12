dt_independent_frame_dependent_contents__ <- function(dt, col_nms) {
  out <- data.table::setDT(lapply(col_nms, function(col_nm) dt[[col_nm]]))
  data.table::setnames(out, col_nms)
  return(out[])
}
