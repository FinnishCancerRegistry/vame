
vd_fun_nms <- function() {
  c(
    "rename",
    "subset",
    "remove",
    "union_append",
    "value_space_dt_subset",
    "value_space_set",
    "meta_get"
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

#' @describeIn VariableMetadata Use this function to create a new
#' VariableMetadata object.
VariableMetadata <- function(var_nm_dt, var_nm_set_dt) {
  data <- new.env(parent = emptyenv())
  data$var_nm_dt <- var_nm_dt
  data$var_nm_set_dt <- var_nm_set_dt
  funs <- new.env(parent = asNamespace("data.table"))
  funs$data <- data
  local(
    expr = {
      vnd_get <- function() {
        data[["var_nm_dt"]]
      }
      vnd_implied_get <- function() {
        vnsd <- vnsd_get()
        dt <- data.table::data.table(
          var_nm = unlist(vnsd[["var_nm_set"]]),
          var_nm_set_dt_pos = vapply(
            seq_along(vnsd[["var_nm_set"]]),
            function(i) {
              rep(i, length(vnsd[["var_nm_set"]]))
            },
            integer(1L)
          )
        )
        data.table::setkeyv(dt, names(dt))
        return(dt[])
      }
      vnd_vnsd_linkage_refresh <- function() {
        vnd <- vnd_get()
        vndi <- vnd_implied_get()
        i.var_nm_set_dt_pos <- NULL # appease R CMD CHECK
        data.table::setkeyv(vnd, c("var_nm", "var_nm_set_dt_pos"))
        vnd_set(vnd)
        return(invisible(NULL))
      }
      vnd_vnsd_intersect <- function() {
        vnd <- vnd_get()
        vndi <- vnd_implied_get()
        rm_var_nms <- union(
          setdiff(
            vnd[["var_nm"]],
            vndi[["var_nm"]]
          ),
          setdiff(
            vndi[["var_nm"]],
            vnd[["var_nm"]]
          )
        )
        if (length(rm_var_nms) > 0) {
          remove(rm_var_nms)
          vnsd <- vnsd_get()
          vnsd_subset <- vapply(vnsd[["var_nm_set"]], length, integer(1L)) > 0L
          vnsd <- vnsd[vnsd_subset, ]
          vnsd_set(vnsd)
        }
        vnd_vnsd_linkage_refresh()
      }
      vnd_set <- function(dt) {
        data[["var_nm_dt"]] <- dt
      }
      vnsd_get <- function() {
        data[["var_nm_set_dt"]]
      }
      vnsd_set <- function(dt) {
        data[["var_nm_set_dt"]] <- dt
      }
      value_space_pos_get <- function(var_nm) {
        data.table::chmatch(var_nm, vnd_get()[["var_nm"]])
      }
      value_space_get <- function(pos) {
        return(vnsd_get()[["value_space"]][[pos]])
      }
      value_space_set <- function(pos, value_space) {
        vnsd_get()[["value_space"]][[pos]] <- value_space
      }
      value_space_dt_subset_expr <- function(var_nm, expr) {
        dbc::assert_is_language_object(expr, assertion_type = "prod_input")
        vnd <- vnd_get()
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
      meta_get <- function(var_nm, meta_nm) {
        vnd <- vnd_get()
        dbc::assert_is_character_nonNA_atom(var_nm)
        dbc::assert_is_character_nonNA_atom(meta_nm)
        dbc::assert_atom_is_in_set(var_nm, set = vnd[["var_nm"]])
        dbc::assert_atom_is_in_set(
          meta_nm, set = setdiff(names(vnd), "var_nm_set_dt_pos")
        )
        jdt <- data.table::setDT(list(var_nm = var_nm))
        vnd[
          i = jdt,
          on = "var_nm",
          j = .SD[[1]],
          .SDcols = meta_nm
        ]
      }
      rename <- function(old, new) {
        vnd <- vnd_get()
        vnsd <- vnsd_get()
        jdt <- data.table::data.table(var_nm = old, new = new)
        i.new <- NULL # appease R CMD CHECK
        vnd[
          i = jdt,
          on = "var_nm",
          j = "var_nm" := i.new
        ]
        tmp <- vnd[
          j = list(var_nm_set = list(.SD[["var_nm"]])),
          keyby = "var_nm_set_dt_pos"
        ]
        data.table::set(vnsd, j = "var_nm_set", value = tmp[["var_nm_set"]])
        invisible(NULL)
      }
      subset_expr <- function(expr) {
        dbc::assert_is_language_object(expr, assertion_type = "prod_input")
        vnd <- vnd_get()
        dt_expr <- substitute(vnd[i = expr], list(expr = expr))
        vnd <- eval(dt_expr)
        vnd_set(vnd)

        vnd_vnsd_intersect()
        vnsd_set(vnd)
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
      union_append <- function(x) {
        e <- environment(x@remove)
        vnd_1 <- vnd_get()
        vnsd_1 <- vnsd_get()
        vnd_2 <- e[["vnd_get"]]()
        vnsd_2 <- e[["vnsd_get"]]()
        vnd <- rbind(vnd_1, vnd_2)
        vnsd <- rbind(vnsd_1, vnsd_2)
        vnsd <- vnsd[!duplicated(vnsd[["var_nm_set"]]), ]
        vnd_set(vnd)
        vnd_vnsd_linkage_refresh()
        vnsd_set(vnsd)
        return(invisible(NULL))
      }
    },
    envir = funs
  )
  stopifnot(vd_fun_nms() %in% ls(funs))
  funs[["vnd_vnsd_intersect"]]()
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
