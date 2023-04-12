read_vd_slot_nms <- function() {
  lines <- readLines("R/S4.R")
  lhs_re <- "^[ ]*#+[ ]*slot:"
  value_re <- "[a-z_]+"
  rhs_re <- "[ ]*$"
  slot_nms <- lines[grepl(paste0(lhs_re, value_re, rhs_re), lines)]
  slot_nms <- sub(lhs_re, "", slot_nms)
  slot_nms <- sub(rhs_re, "", slot_nms)
  slot_nms
}
vd_slot_nms <- read_vd_slot_nms()
usethis::use_data(
  vd_slot_nms,
  internal = TRUE,
  overwrite = TRUE
)
