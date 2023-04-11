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
# @codedoc_comment_block R_package_description(vame)

# @codedoc_comment_block news("vame", "2023-04-11", "0.1.0")
# First tagged version. See ?vame::VariableMetadata.
# @codedoc_comment_block news("vame", "2023-04-11", "0.1.0")