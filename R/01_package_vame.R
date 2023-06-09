#' @name vame
#' @docType package
#' @title vame: Variable Metadata
#'
#' @eval c(
#'   codedoc::codedoc_R_package_description("vame"),
#'   codedoc::codedoc_news_for_R_package()
#' )
NULL

# @codedoc_comment_block R_package_description(vame)
# `vame` makes it simpler to define and make use of metadata pertaining
# to one more variables (e.g. a tabular dataset).
#
# <!-- badges: start -->
# [![R-CMD-check](https://github.com/FinnishCancerRegistry/vame/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FinnishCancerRegistry/vame/actions/workflows/R-CMD-check.yaml)
# <!-- badges: end -->
#
# # Recommended installation
#
# ```r
# devtools::install_github(
#   "FinnishCancerRegistry/vame",
#   ref = readline("enter latest tag on github: ")
# )
# ```
#
# # Example of use
#
# Suppose you have a tabular dataset. You have ID columns, stratifying columns
# (e.g. demographical data), dates, what have you. `vame` makes it easy to
# define what kind of data each column contains. For instance you can define
# the precise set of category values that a categorical column can have.
# Further, this package makes quick work of writing assertions on columns
# or entire datasets, of producing stratum tables for the purpose of computing
# statistics within each stratum, and retrieving labels for (e.g.
# integer-valued) categorical variables.
#
# Consider the following tiny dataset of child patients.
# 
# ```r
# dt <- data.table::data.table(
#   subject_id = 1:3,
#   birth_date = as.Date(c("2001-01-01", "2002-01-01", "2003-01-01")),
#   dg_age = c(10.5, 15.2, 20.0),
#   dg_age_group = 3:5,
#   ex_age = c(21.0, 20.0, 19.0),
#   ex_age_group = c(5L, 4L, 4L),
#   social_class = c(0L, 0L, 1L)
# )
# 
# # "type" is up to you. you may want to write code that takes type into account.
# # however, type "categorical" is special: stratum tables and labels can
# # be retrieved for categorical columns only.
# age_group_labels <- data.table::data.table(
#   value = 1:18,
#   label = paste0(0:17 * 5, "-", c(1:17 * 5 - 1, 120))
# )
# var_dt <- data.table::data.table(
#   var_nm = c("subject_id", "birth_date",
#              "dg_age", "dg_age_group",
#              "ex_age", "ex_age_group",
#              "social_class"),
#   class_set = list(
#     "integer", "Date",
#     "numeric", "integer",
#     "numeric", "integer",
#     "integer"
#   ),
#   type = c("id", "date",
#             "duration", "categorical",
#             "duration", "categorical",
#             "categorical"),
#   label_set = list(
#     NULL, NULL,
#     NULL, age_group_labels,
#     NULL, age_group_labels,
#     data.table::data.table(
#       value = 0:3,
#       label = c("student", "labourer", "professional", "entrepreneur")
#     )
#   )
# )

# age_bounds <- list(
#   lo = 0.0, hi = 120.0,
#   lo_inclusive = TRUE, hi_inclusive = FALSE
# )
# value_spaces <- list(
#   # element names such as subject_id, birth_date not necessary ---
#   # I used them for readability.
#   subject_id = list(
#     bounds = list(
#       lo = 1L, hi = 100L,
#       lo_inclusive = TRUE, hi_inclusive = TRUE
#     )
#   ),
#   birth_date = list(
#     bounds = list(
#       lo = as.Date("2001-01-01"),
#       hi = as.Date("2019-12-31"),
#       lo_inclusive = TRUE,
#       hi_inclusive = TRUE
#     )
#   ),
#   dg_age = list(bounds = age_bounds),
#   ex_age = list(bounds = age_bounds),
#   age_group = list(
#     # this is the joint category space of dg_age_group and ex_age_group.
#     # ex_age_group cannot be lower than dg_age_group by definition!
#     dt = data.table::data.table(
#       dg_age_group = 1:18
#     )[
#       j = list(ex_age_group = .SD[["dg_age_group"]]:18),
#       keyby = "dg_age_group",
#       .SDcols = "dg_age_group"
#     ]
#   ),
#   social_class = list(
#     set = 0:3
#   )
# )
# var_set_dt <- data.table::data.table(
#   id = c("subject_id", "birth_date",
#         "dg_age", "ex_age",
#         "age_group",
#         "social_class"),
#   var_nm_set = list(
#     "subject_id", "birth_date",
#     "dg_age", "ex_age",
#     c("dg_age_group", "ex_age_group"),
#     "social_class"
#   ),
#   value_space = value_spaces
# )
# vardef <- vame::VariableMetadata(
#   var_dt = var_dt,
#   var_set_dt = var_set_dt
# )

# # producing and using stratum tables
# sdt <- vardef@vame_category_space_dt(
#   c("dg_age_group", "ex_age_group", "social_class")
# )
# print(sdt)
# ndt <- dt[i = sdt, on = names(sdt), j = .N, keyby = .EACHI]
# print(ndt)

# # retrieving labels
# ndt[, "social_class_label" := vardef@var_labels(ndt[["social_class"]])]

# # writing assertions (checks) on function input data
# my_fun_1 <- function(dataset) {
#   vardef@dbc@assert_is_dataset(dataset)
#   return(nrow(dataset))
# }
# my_fun_2 <- function(dg_age) {
#   vardef@dbc@assert_is_var(dg_age, var_nm = "dg_age")
# }
# ```
# @codedoc_comment_block R_package_description(vame)
