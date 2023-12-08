.__VAME_SLOT_FUN_NMS__ <- local({
  lines <- readLines("./R/09_slot_funs.R")
  fun_def_lines <- lines[grepl(" <- function[(]", lines)]
  slot_fun_nms <- sub(" <- function.+$", "", fun_def_lines)
  return(slot_fun_nms)
})

vame_slot_nms_get__ <- function() {
  return(.__VAME_SLOT_FUN_NMS__)
}

doc_slot_fun_arg__ <- function(df, arg_nm, with_tag = FALSE) {
  key <- paste0("param_", arg_nm)
  if (!key %in% df[["key"]]) {
    return(paste0("TODO: document argument ", arg_nm))
  }
  lines <- unlist(df[["comment_block"]][df[["key"]] == key])
  if (length(lines) == 0) {
    warning("arg_nm = \"", arg_nm, "\" has not been documented")
    return(lines)
  }
  while (lines[1] == "") {
    lines <- lines[-1]
  }
  if (!with_tag) {
    lines <- gsub("^@param +", "", lines)
    lines <- gsub(sprintf("(?<!\\w)%s(?!=\\w)", arg_nm),
                  sprintf("`%s`", arg_nm),
                  lines,
                  perl = TRUE)
    lines <- gsub("``", "`", lines)
  }
  if (lines[length(lines)] != "") {
    lines <- c(lines, "")
  }
  return(lines)
}

doc_slot_fun__ <- function(df, fun_nm) {
  key <- paste0("vm@", fun_nm)
  description <- unlist(df[["comment_block"]][df[["key"]] == key])
  fun_arg_nms <- names(formals(match.fun(fun_nm)))
  slot_fun_arg_nms <- setdiff(fun_arg_nms, "vm")
  slot_call_string <- paste0(
    "vm@", fun_nm, "(", paste0(slot_fun_arg_nms, collapse = ", "), ")"
  )
  lines <- c(
    paste0("@slot ", fun_nm),
    "",
    "*Description*",
    "",
    description,
    "",
    "*Usage*",
    "",
    paste0("`", slot_call_string, "`"),
    ""
  )
  if (length(slot_fun_arg_nms) > 0) {
    lines <- c(
      lines,
      "*Arguments*",
      "",
      unlist(lapply(slot_fun_arg_nms, doc_slot_fun_arg__, df = df))
    )
  }
  return(lines)
}
doc_slot_funs__ <- function(df = NULL, fun_nms = NULL) {
  if (is.null(df)) {
    df <- codedoc::extract_keyed_comment_blocks()
  }
  if (is.null(fun_nms)) {
    fun_nms <- vame_slot_nms_get__()
  }
  out <- lapply(fun_nms, doc_slot_fun__, df = df)
  unlist(out)
}
doc_variablemetadata_features__ <- function(df = NULL) {
  if (is.null(df)) {
    df <- codedoc::extract_keyed_comment_blocks()
  }
  feature_keys <- unique(df[["key"]])
  feature_regex <- "^feature[(]VariableMetadata, "
  feature_keys <- feature_keys[
    grepl(feature_regex, feature_keys)
  ]
  if (length(feature_keys) == 0) {
    return(character(0L))
  }
  c(
    "@section Features:",
    unlist(lapply(feature_keys, function(feature_key) {
      lines <- unlist(df[["comment_block"]][df[["key"]] == feature_key])
      feat_nm <- sub(feature_regex, "", feature_key)
      feat_nm <- gsub("[()]", "", feat_nm)
      feat_nm <- paste0(
        toupper(substr(feat_nm, 1, 1)),
        substr(feat_nm, 2, nchar(feat_nm))
      )
      c(paste0("**", feat_nm, "**"), "", lines, "")
    }))
  ) 
}
doc_variablemetadata_news__ <- function() {
  codedoc::codedoc_roxygen_news_by_version(
    detect_allowed_keys = "(vm@)|(vame::VariableMetadata)"
  )
}

methods::setClass(
  Class = "VariableMetadata",
  slots = structure(
    rep("function", length(vame_slot_nms_get__())),
    names = vame_slot_nms_get__()
  )
)

#' @title Variable Metadata
#' @docType class
#' @description
#' Create and make use of a `VariableMetadata` object. It contains
#' - `var_dt`: A `data.table` containing metadata for variables,
#' - `var_set_dt`: Metadata for variable sets,
#' - A number of functions in S4 slots for making use of the metadata via e.g.
#'   `vm@var_assert` where `vm` is the `VariableMetadata` object.
#' 
#' A `VariableMetadata` object is created via calling the
#' `vame::VariableMetadata` function.
#' See section **Features** for what you can do with `VariableMetadata` objects.
#' @name VariableMetadata-class
#' @aliases VariableMetadata
#' @param var_dt `[data.table]`
#'
#' Contains information for individual variables. Must contain at a minimum
#' column `var_nm`.
#' @param var_set_dt `[data.table]`
#'
#' Contains information for sets of variables --- e.g. a common value space.
#' Must contain at a minimum columns
#'
#' - `id` [`character`]: Identifies each set of variable names. E.g.
#'   `"my_set"`.
#' - `var_nm_set` `[list]`: Each list element contains a character string
#'   vector of variable names. e.g. `list(c("a", "b"))`.
#' @examples
#'
#' # vame::VariableMetadata ----------------------------------------------------
#'
#' # basic example of different kinds of variables
#' value_space_d <- function() 1:3 * 100L
#' vm <- vame::VariableMetadata(
#'   var_dt = data.table::data.table(
#'     var_nm = c("a", "b", "c", "d", "e", "f", "g", "h"),
#'     type = c("categorical", "categorical",
#'              "categorical",
#'              "my_type_1", "my_type_2", "my_type_3", "my_type_4", "my_type_5")
#'   ),
#'   var_set_dt = data.table::data.table(
#'     id = c("ab", "c", "d", "e", "f", "g", "h"),
#'     var_nm_set = list(
#'       ab = c("a", "b"),
#'       c = "c", d = "d", e = "e", f = "f", g = "g", h = "h"),
#'     value_space = list(
#'       ab = list(dt = data.table::data.table(
#'         a = c(1L, 2L, 2L),
#'         b = c(11L, 21L, 22L)
#'       )),
#'       c = list(set = c("a", "b")),
#'       d = list(expr = quote(value_space_d())),
#'       e = list(bounds = list(
#'         lo = 0.0, hi = 10.0,
#'         lo_inclusive = TRUE, hi_inclusive = TRUE
#'       )),
#'       f = list(bounds = list(
#'         lo = as.Date("1901-01-01"), hi = as.Date("2023-12-31"),
#'         lo_inclusive = TRUE, hi_inclusive = TRUE
#'       )),
#'       g = list(unrestricted = list(class_set = c("IDate", "Date"))),
#'       h = list(regex = "^[a-z]$")
#'     )
#'   )
#' )
#' vm@var_assert(1L, var_nm = "a")
#' vm@var_assert(21L, var_nm = "b")
#' vm@var_assert("a", var_nm = "c")
#' vm@var_assert(100L, var_nm = "d")
#' vm@var_assert(c(0.0, 10.0), var_nm = "e")
#' vm@var_assert(as.Date("1901-01-01"), var_nm = "f")
#' vm@var_assert(data.table::as.IDate("1901-01-01"), var_nm = "g")
#' vm@var_assert(letters, var_nm = "h")
#' my_fun <- function(e_values) {
#'   vm@var_assert(e_values, var_nm = "e")
#'   e_values + 1
#' }
#' my_fun(0.0)
#'
#' # renaming, removing variables
#' vm <- vame::VariableMetadata(
#'   var_dt = data.table::data.table(
#'     var_nm = c("a", "b", "c"),
#'     flavour = c("tasty", "rancid", "bitter")
#'   ),
#'   var_set_dt = data.table::data.table(
#'     id = "set_01",
#'     var_nm_set = list(c("a", "b")),
#'     value_space = list(list(dt = data.table::data.table(
#'       a = 1:2,
#'       b = 3:4
#'     )))
#'   )
#' )
#'
#' vm@var_rename("a", "A")
#' stopifnot(
#'   identical(vm@var_meta_get("A", "flavour"), "tasty"),
#'   identical(names(vm@var_set_value_space_get("set_01")[["dt"]]), c("A", "b")),
#'   identical(vm@var_set_meta_get("set_01", "var_nm_set"), c("A", "b"))
#' )
#'
#' vm@var_set_rename("set_01", "Ab")
#' stopifnot(
#'   identical(vm@var_set_meta_get_all("id"), c("Ab" = "Ab"))
#' )
#'
#' vm@var_remove("b")
#' stopifnot(
#'   identical(names(vm@var_set_value_space_get("Ab")[["dt"]]), "A"),
#'   identical(vm@var_set_meta_get("Ab", "var_nm_set"), "A")
#' )
#'
#' vm@var_set_remove("Ab")
#' stopifnot(
#'   identical(length(vm@var_set_meta_get_all("var_nm_set")), 0L)
#' )
#'
#' # retrieving category space data.tables
#' dt_01 <- data.table::CJ(a = 1:3, b = 3:1, c = 1:3)
#' dt_02 <- data.table::CJ(d = 1:2, e = 2:1)
#' vm <- vame::VariableMetadata(
#'   var_dt = data.table::data.table(
#'     var_nm = c("a", "b", "c", "d", "e", "f"),
#'     type = "categorical"
#'   ),
#'   var_set_dt = data.table::data.table(
#'     id = c("abc", "de", "f"),
#'     var_nm_set = list(
#'       abc = c("a", "b", "c"),
#'       de = c("d", "e"),
#'       f = "f"
#'      ),
#'     value_space = list(
#'       abc = list(dt = dt_01),
#'       de = list(expr = quote({
#'         dt_02[
#'           i = !duplicated(dt_02, by = var_nms),
#'           j = .SD,
#'           .SDcols = var_nms
#'         ]
#'       })),
#'       f = list(bounds = list(
#'         lo = 0L, hi = 10L,
#'         lo_inclusive = TRUE, hi_inclusive = TRUE
#'       ))
#'     )
#'   )
#' )
#'
#' stopifnot(
#'   all.equal(
#'     vm@vame_category_space_dt(c("a", "b")),
#'     dt_01[
#'       i = !duplicated(dt_01, by = c("a", "b")),
#'       j = .SD,
#'       .SDcols = c("a", "b")
#'     ],
#'     check.attributes = FALSE
#'   ),
#'   all.equal(
#'     vm@vame_category_space_dt(c("d", "e")),
#'     dt_02,
#'     check.attributes = FALSE
#'   ),
#'   all.equal(
#'     vm@vame_category_space_dt(c("a", "f")),
#'     data.table::CJ(a = 1:3, f = 0:10),
#'     check.attributes = FALSE
#'   )
#' )
#'
#' # getting category space data.tables --- here a variable appears in
#' # two different value spaces. this can be handy for defining joint value
#' # spaces and also conversions & aggregations.
#' dt_01 <- data.table::CJ(a = 1:3, b = 3:1, c = 1:3)
#' dt_02 <- data.table::CJ(a = 0:1, e = 2:1)
#' dt_03 <- data.table::data.table(a = 0:3, a_2 = c(1L,1L, 2L,2L))
#' vm <- vame::VariableMetadata(
#'   var_dt = data.table::data.table(
#'     var_nm = c("a", "b", "c", "e", "a_2"),
#'     type = "categorical"
#'   ),
#'   var_set_dt = data.table::data.table(
#'     id = c("set_01", "set_02", "set_03"),
#'     var_nm_set = list(c("a", "b", "c"), c("a", "e"), c("a", "a_2")),
#'     value_space = list(
#'       list(dt = dt_01),
#'       list(dt = dt_02),
#'       list(dt = dt_03)
#'     )
#'   )
#' )
#'
#' obs <- vm@vame_category_space_dt(c("a", "b", "e"))
#' exp <- data.table::data.table(
#'   a = c(0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L),
#'   b = c(NA, NA, 1L, 1L, 2L, 2L, 3L, 3L, 1L, 2L, 3L, 1L, 2L, 3L),
#'   e = c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, NA, NA, NA, NA, NA, NA)
#' )
#' data.table::setkeyv(obs, names(obs))
#' data.table::setkeyv(exp, names(exp))
#' stopifnot(
#'   all.equal(obs, exp, check.attributes = FALSE)
#' )
#'
#' stopifnot(
#'   vm@var_is_aggregateable_to("a", "a_2"),
#'   identical(vm@var_aggregate(0:1, "a", "a_2"), c(1L,1L))
#' )
#'
#' # getting labels for variable levels
#' dt_01 <- data.table::CJ(a = 1:3, b = 3:1, c = 4:5)
#' vm <- vame::VariableMetadata(
#'   var_dt = data.table::data.table(
#'     var_nm = c("a", "b", "c"),
#'     type = "categorical",
#'     labeler = list(
#'       a = data.table::data.table(
#'         level = 1:3,
#'         en = paste0("a_level_", 1:3)
#'       ),
#'       b = quote({
#'         dt <- data.table::data.table(
#'           level = 1:3,
#'           en = paste0("b_level_", 1:3)
#'         )
#'         dt[[label_col_nm]][match(x, dt[["level"]])]
#'       }),
#'       c = NULL
#'     )
#'   ),
#'   var_set_dt = data.table::data.table(
#'     id = c("set_01"),
#'     var_nm_set = list(c("a", "b", "c")),
#'     value_space = list(
#'       list(dt = dt_01)
#'     )
#'   )
#' )
#'
#' obs <- vm@var_labels_get(x = 1:4, var_nm = "a", label_col_nm = "en")
#' exp <- c(paste0("a_level_", 1:3), NA)
#' stopifnot(
#'   identical(obs, exp)
#' )
#'
#' obs <- tryCatch(
#'   vm@var_labels_get(
#'     x = 1:4,
#'     var_nm = "a",
#'     label_col_nm = "this does not exist"
#'   ),
#'   error = function(e) e[["message"]]
#' )
#' exp <- paste0(
#'   "label_col_nm = \"this does not exist\"",
#'   " not one of the defined label columns: \"en\""
#' )
#' stopifnot(
#'   grepl(exp, obs)
#' )
#'
#' obs <- tryCatch(
#'   vm@var_labels_get(
#'     x = 1:4,
#'     var_nm = "c",
#'     label_col_nm = "en"
#'   ),
#'   error = function(e) e[["message"]]
#' )
#' exp <- "Variable \"c\" has no labeler defined"
#' stopifnot(
#'   grepl(exp, obs)
#' )
#'
#' # adding data to a pre-existing VariableMetadata object
#' vm_1 <- vame::VariableMetadata(
#'   var_dt = data.table::data.table(
#'     var_nm = c("a", "b"),
#'     type = "categorical",
#'     labeler = list(
#'       a = data.table::data.table(level = 1:2, label = c("a_1", "a_2")),
#'       b = NULL
#'     )
#'   ),
#'   var_set_dt = data.table::data.table(
#'     id = "ab",
#'     var_nm_set = list(ab = c("a", "b")),
#'     value_space = list(ab = list(dt = data.table::CJ(a = 1:2, b = 3:4)))
#'   )
#' )
#' # note that vm_2 var_dt does not have columns "type", "labeler" --- those
#' # will be NA / NULL for "c" and "d".
#' vm_2 <- vame::VariableMetadata(
#'   var_dt = data.table::data.table(var_nm = c("c", "d")),
#'   var_set_dt = data.table::data.table(
#'     id = "cd",
#'     var_nm_set = list(cd = c("c", "d")),
#'     value_space = list(cd = list(dt = data.table::CJ(c = 5:6, d = 7:8)))
#'   )
#' )
#' vm_1@vame_union_append(vm_2)
#' stopifnot(
#'   c("ab", "cd") %in% vm_1@var_set_meta_get_all("id")
#' )
#' 
#' # taking a copy of a VariableMetadata object
#' vm_3 <- vm_2@vame_copy()
#' vm_2@var_rename("d", "dd")
#' stopifnot(
#'   "d" %in% vm_3@var_meta_get_all("var_nm"),
#'   !"d" %in% vm_2@var_meta_get_all("var_nm"),
#'   "dd" %in% vm_2@var_meta_get_all("var_nm")
#' )
#' @export
#' @eval local({
#'   df <- codedoc::extract_keyed_comment_blocks()
#'   c(
#'     doc_variablemetadata_features__(df = df),
#'     doc_slot_funs__(df = df),
#'     doc_variablemetadata_news__()
#'   )
#' })
VariableMetadata <- function(var_dt, var_set_dt) {
  # @codedoc_comment_block news("vame::VariableMetadata", "2023-06-30", "0.1.0")
  # First release.
  # @codedoc_comment_block news("vame::VariableMetadata", "2023-06-30", "0.1.0")
  
  assert_is_var_dt(var_dt)
  assert_is_var_set_dt(var_set_dt)
  pkg_env <- environment(VariableMetadata)
  funs <- new.env(parent = pkg_env)
  funs$data <- new.env(parent = emptyenv())
  funs$data$var_dt <- var_dt
  funs$data$var_set_dt <- var_set_dt
  funs$self <- function() {
    get("self_obj", envir = get("data"))
  }
  environment(funs$self) <- funs
  slot_fun_nms <- vame_slot_nms_get__()
  lapply(slot_fun_nms, function(slot_fun_nm) {
    alias_fun <- eval(parse(text = paste0("vame:::", slot_fun_nm)))
    args <- formals(alias_fun)
    arg_lines <- paste0(
      names(args), " = ", vapply(args, deparse1, character(1L))
    )
    arg_lines <- sub(" = $", "", arg_lines)
    arg_lines <- setdiff(arg_lines, "vm")
    arg_lines <- paste0(arg_lines, ",")
    arg_lines[length(arg_lines)] <- sub(",$", "", arg_lines[length(arg_lines)])
    # body_lines <- sprintf(
    #   "vame:::call_slot_fun_alias_in_slot_fun__(\"%s\")",
    #   slot_fun_nm
    # )
    body_arg_lines <- paste0(names(args), " = ", names(args))
    body_arg_lines[names(args) == "vm"] <- "vm = self()"
    body_arg_lines <- paste0(body_arg_lines, ",")
    body_arg_lines[length(body_arg_lines)] <- sub(
      ",$", "", body_arg_lines[length(body_arg_lines)]
    )
    body_lines <- c(
      paste0("vame:::", slot_fun_nm, "("),
      paste0("  ", body_arg_lines),
      ")"
    )
    lines <- c(
      sprintf("%s <- function(", slot_fun_nm), 
      paste0("  ", arg_lines),
      ") {",
      paste0("  ", body_lines),
      "}"
    )
    # browser()
    funs[[slot_fun_nm]] <- eval(parse(text = lines))
    environment(funs[[slot_fun_nm]]) <- funs
    return(NULL)
  })  
  slots <- lapply(slot_fun_nms, function(fun_nm) {
    funs[[fun_nm]]
  })
  names(slots) <- slot_fun_nms
  arg_list <- c(list(Class = "VariableMetadata"), slots)
  out <- do.call(methods::new, arg_list, quote = TRUE)
  funs$data$self_obj <- out
  vd_vsd_intersect(out)
  if (nrow(vd_get(out)) == 0) {
    stop("vame::VariableMetadata call resulted in no variables being defined. ",
         "do var_dt and var_set_dt use the same variable names?")
  }
  return(out)
}

methods::setMethod(
  f = "print",
  signature = "VariableMetadata",
  definition = function(x) {
    ids <- x@var_set_meta_get_all("id")
    sets <- x@var_set_meta_get_all("var_nm_set")
    cat(
      "VariableMetadata object ----\n",

      "Functions:\n",
      vapply(vame_slot_nms_get__(), function(obj_nm) {
        paste0("  @", obj_nm, "()\n")
      }, character(1L)),

      "Variable sets:\n",
      vapply(
        seq_along(ids),
        function(i) {
          set_string <- paste0(
            "\"", utils::head(sets[[i]], 5), "\"",
            collapse = ", "
          )
          if (length(sets[[i]]) > 5) {
            set_string <- paste0(set_string, ", ...")
          }
          set_string <- paste0("c(", set_string, ")")
          paste0("  \"", ids[i], "\": ", set_string, "\n")
        },
        character(1L)
      )
    )
  }
)

methods::setMethod(
  f = "show",
  signature = "VariableMetadata",
  definition = function(object) {
    print(object)
  }
)