
vd_fun_nms <- function() {
  c(
    "rename",
    "subset",
    "remove",
    "union",
    "value_space_dt_subset",
    "value_space_set"
  )
}

methods::setClass(
  Class = "VariableMetadata",
  slots = structure(rep("function", length(vd_fun_nms())), names = vd_fun_nms())
)

#' @title Variable Metadata
#' @description
#' Create a VariableMetadata object.
#' @param var_nm_dt `[data.table]`
#' 
#' Contains information for individual variables.
#' @param var_nm_set_dt `[data.table]`
#' 
#' Contains information for sets of variables --- e.g. a common value space.
#' @name VariableMetadata
NULL

#' @describeIn VariableMetadata
#' Use this function to create a new VariableMetadata object.
VariableMetadata <- function(var_nm_dt, var_nm_set_dt) {
  data <- new.env(parent = emptyenv())
  data$var_nm_dt <- var_nm_dt
  data$var_nm_set_dt <- var_nm_set_dt
  funs <- new.env(parent = asNamespace("data.table"))
  funs$data <- data
  local(
    expr = {
      cnd_get <- function() {
        data[["var_nm_dt"]]
      }
      cnd_set <- function(dt) {
        data[["var_nm_dt"]] <- dt
      }
      cnsd_get <- function() {
        data[["var_nm_set_dt"]]
      }
      cnsd_set <- function(dt) {
        data[["var_nm_set_dt"]] <- dt
      }
      value_space_pos_get <- function(var_nm) {
        data.table::chmatch(var_nm, cnd_get()[["var_nm"]])
      }
      value_space_get <- function(pos) {
        return(cnsd_get()[["value_space"]][[pos]])
      }
      value_space_set <- function(pos, value_space) {
        cnsd_get()[["value_space"]][[pos]] <- value_space
      }
      value_space_dt_subset_expr <- function(var_nm, expr) {
        dbc::assert_is_language_object(expr, assertion_type = "prod_input")
        cnd <- cnd_get()
        pos <- value_space_pos_get(var_nm)
        vs <- value_space_get(pos)
        dt <- vs[["dt"]]
        if (!data.table::is.data.table(dt)) {
          stop("Column \"", var_nm, "\" value space is not a data.table")
        }
        dt_expr <- substitute(dt[i = expr], list(expr = expr))
        dt <- eval(dt_expr)
        value_space_set(pos, dt)
        return(invisible(NULL))
      }
      value_space_dt_subset <- function(var_nm, expr) {
        expr <- substitute(expr)
        value_space_dt_subset_expr(var_nm, expr)
      }
      rename <- function(old, new) {
        cnd <- cnd_get()
        cnsd <- cnsd_get()
        jdt <- data.table::data.table(var_nm = old, new = new)
        i.new <- NULL # appease R CMD CHECK
        cnd[
          i = jdt,
          on = "var_nm",
          j = "var_nm" := i.new
        ]
        tmp <- cnd[
          j = list(var_nm_set = list(.SD[["var_nm"]])),
          keyby = "var_nm_set_dt_pos"
        ]
        data.table::set(cnsd, j = "var_nm_set", value = tmp[["var_nm_set"]])
        invisible(NULL)
      }
      subset_expr <- function(expr) {
        dbc::assert_is_language_object(expr, assertion_type = "prod_input")
        dt_expr <- substitute(cdn[i = expr], list(expr = expr))
        cnd <- eval(dt_expr)
        cnd_set(cnd)

        cnsd <- cnsd_get()
        cnsd[
          j = "var_nm_set" := list(
            lapply(.SD[["var_nm_set"]], intersect, y = cnd[["var_nm"]])
          )
        ]
        cnsd_subset <- vapply(cnsd[["var_nm_set"]], length, integer(1L)) > 0L
        cnsd <- cnsd[cnsd_subset, ]
        cnsd_set(cnd)
      }
      subset <- function(expr) {
        expr <- substitute(expr)
        subset_expr(expr)
        invisible(NULL)
      }
      remove <- function(var_nms) {
        expr <- substitute(!var_nm %in% var_nms, list(var_nms = var_nms))
        subset_expr(expr)
      }
      union <- function(x) {
        e <- environment(x@remove)
        cnd_1 <- cnd_get()
        cnsd_1 <- cnsd_get()
        cnd_2 <- e[["cnd_get"]]()
        cnsd_2 <- e[["cnsd_get"]]()
        cnd <- rbind(cnd_1, cnd_2)
        cnsd <- rbind(cnsd_1, cnsd_2)
        cnsd <- cnsd[!duplicated(cnsd[["var_nm_set"]]), ]
        cnd_implied <- data.table::data.table(
          var_nm = unlist(cnsd[["var_nm_set"]]),
          var_nm_set_dt_pos = vapply(
            seq_along(cnsd[["var_nm_set"]]),
            function(i) {
              rep(i, length(cnsd[["var_nm_set"]]))
            },
            integer(1L)
          )
        )
        i.var_nm_set_dt_pos <- NULL # appease R CMD CHECK
        cnd[
          i = cnd_implied,
          on = "var_nm",
          j = "var_nm_set_dt_pos" := i.var_nm_set_dt_pos
        ]
        data.table::setkeyv(cnd, c("var_nm", "var_nm_set_dt_pos"))
        cnd_set(cnd)
        cnsd_set(cnsd)
        return(invisible(NULL))
      }
    },
    envir = funs
  )
  stopifnot(vd_fun_nms() %in% ls(funs))
  arg_list <- list(
    Class = "VariableMetadata"
  )
  fun_list <- lapply(vd_fun_nms(), function(fun_nm) {
    funs[[fun_nm]]
  })
  names(fun_list) <- vd_fun_nms()
  arg_list <- c(arg_list, fun_list)
  do.call(methods::new, arg_list, quote = TRUE)
}

vd <- VariableMetadata(
  var_nm_dt = data.table::data.table(
    var_nm = c("a", "b"),
    var_nm_set_dt_pos = c(1L, 1L)
  ),
  var_nm_set_dt = data.table::data.table(
    var_nm_set = list(c("a", "b"))
  )
)

methods::setMethod(
  f = "print",
  signature = "VariableMetadata",
  definition = function(x) {
    cat(
      "VariableMetadata object ----\n",
      "Functions:\n",
      vapply(vd_fun_nms(), function(obj_nm) {
        paste0("  @", obj_nm, "()\n")
      }, character(1L))
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
