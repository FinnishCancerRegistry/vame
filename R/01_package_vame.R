doc_dev_todo__ <- function(df = NULL) {
  if (is.null(df)) {
    df <- codedoc::extract_keyed_comment_blocks(
      detect_allowed_keys = "^dev_todo[(]"
    )
  } else {
    df <- df[grepl("^dev_todo[(]", df[["key"]]), ]
  }  
  priority_type_topic <- gsub(
    "(^dev_todo[(])|([)]$)",
    "",
    df[["key"]]
  )
  priority_type_topic <- strsplit(
    priority_type_topic,
    split = "[ ]*,[ ]*"
  )
  priority_type_topic <- do.call(
    rbind,
    lapply(priority_type_topic, function(ptt) {
      data.frame(
        todo_priority = ptt[1],
        todo_type = ptt[2],
        todo_topic = ptt[3]
      )
    })
  )
  df[, c("todo_priority", "todo_type", "todo_topic")] <- priority_type_topic
  df <- df[
    order(df[["todo_priority"]], df[["todo_type"]], df[["todo_topic"]]),
  ]
  todo_priority_set <- unique(df[["todo_priority"]])
  lines <- unlist(lapply(todo_priority_set, function(priority) {
    idx_set <- which(df[["todo_priority"]] == priority)
    todo_type_set <- unique(df[["todo_type"]][idx_set])
    lines <- unlist(lapply(todo_type_set, function(type) {
      idx_set <- intersect(
        idx_set,
        which(df[["todo_type"]] == type)
      )
      todo_topic_set <- unique(df[["todo_topic"]][idx_set])
      lines <- unlist(lapply(todo_topic_set, function(todo_topic) {
        idx_set <- intersect(
          idx_set,
          which(df[["todo_topic"]] == todo_topic)
        )
        c(
          paste0("`", todo_topic, "`:"),
          "",
          unlist(df[["comment_block"]][idx_set])
        )
      }))
      lines <- c(
        paste0("#### Type: ", type),
        "",
        lines
      )
      return(lines)
    }))
    lines <- c(
      paste0("### Priority: ", priority),
      "",
      lines
    )
    return(lines)
  }))
  lines <- c(
    "## TODO",
    "",
    lines
  )
  return(lines)
}

#' @name vame
#' @title vame: Variable Metadata
#'
#' @eval c(
#'   codedoc::codedoc_R_package_description("vame"),
#'   codedoc::codedoc_news_for_R_package()
#' )
"_PACKAGE"

# @codedoc_comment_block R_package_description(vame)
# `vame` makes it simpler to define and make use of metadata pertaining
# to one more variables (e.g. a tabular dataset). It implements the
# `VariableMetadata` class, which contains the metadata. The various metadata
# are accessed using slot functions such as `vm@var_description_get`.
#
# The `VariableMetadata` class is intended for storing metadata for which there
# is "one right way". For instance, a variable has one correct description in
# text. This philosophy excludes tasks such as creating a manual for a dataset,
# which can take many forms.
#
# See the help page `?vame::VariableMetadata` for more information. In
# particular see the examples.
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
# ${paste0(vame:::doc_dev_todo__(), collapse = "\n")}
# @codedoc_comment_block R_package_description(vame)
