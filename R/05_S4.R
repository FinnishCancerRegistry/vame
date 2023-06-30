methods::setClass(
  Class = "VariableMetadata",
  slots = structure(
    rep("function", length(vame_slot_nms_get__())),
    names = vame_slot_nms_get__()
  )
)

#' @title Variable Metadata
#' @description
#' Create a VariableMetadata object.
#' @name VariableMetadata
NULL

#' @rdname VariableMetadata
#' @section Functions:
#' - `vame::VariableMetadata`: Use this function to create a new
#'   VariableMetadata object.
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
#' vd <- vame::VariableMetadata(
#'   var_dt = data.table::data.table(
#'     var_nm = c("a", "b", "c"),
#'     flavour = c("tasty", "rancid", "bitter")
#'   ),
#'   var_set_dt = data.table::data.table(
#'     id = "set_01",
#'     var_nm_set = list(c("a", "b"))
#'   )
#' )
#' vd@var_rename("a", "A")
#' stopifnot(identical(vd@var_meta_get("A", "flavour"), "tasty"))
#' @export
VariableMetadata <- function(var_dt, var_set_dt) {
  pkg_env <- environment(VariableMetadata)
  funs <- new.env(parent = pkg_env)
  funs$data <- new.env(parent = emptyenv())
  funs$data$var_dt <- var_dt
  funs$data$var_set_dt <- var_set_dt
  data <- NULL # appease R CMD CHECK
  local(
    expr = {
      # assertions -------------------------------------------------------------
      assert_is_var_nm <- function(
        var_nm,
        assertion_type = dbc::assertion_type_default()
      ) {
        dbc::assert_is_character_nonNA_atom(
          var_nm,
          assertion_type = assertion_type
        )
        dbc::assert_atom_is_in_set(
          var_nm,
          set = vd_get()[["var_nm"]],
          assertion_type = assertion_type
        )
      }
      assert_is_var_meta_nm <- function(
        meta_nm,
        assertion_type = dbc::assertion_type_default()
      ) {
        dbc::assert_is_character_nonNA_atom(
          meta_nm,
          assertion_type = dbc::assertion_type_default()
        )
        dbc::assert_atom_is_in_set(
          meta_nm,
          set = names(vd_get()),
          assertion_type = dbc::assertion_type_default()
        )
      }
      assert_is_var_set_id <- function(
        id,
        assertion_type = dbc::assertion_type_default()
      ) {
        dbc::assert_is_character_nonNA_atom(
          id,
          assertion_type = dbc::assertion_type_default()
        )
        dbc::assert_atom_is_in_set(
          id,
          set = vsd_get()[["id"]],
          assertion_type = dbc::assertion_type_default()
        )
      }
      assert_is_var_set_meta_nm <- function(
        meta_nm,
        assertion_type = dbc::assertion_type_default()
      ) {
        dbc::assert_is_character_nonNA_atom(
          meta_nm,
          assertion_type = dbc::assertion_type_default()
        )
        dbc::assert_atom_is_in_set(
          meta_nm,
          set = names(vsd_get()),
          assertion_type = dbc::assertion_type_default()
        )
      }
      assert_var_set_value_space_is_defined <- function() {
        if (!var_set_value_space_is_defined()) {
          stop("No value spaces have been defined")
        }
      }
      assert_is_var_label_dt <- function(value) {
        dbc::assert_is_one_of(
          value,
          funs = list(
            dbc::report_is_data_table,
            dbc::report_is_NULL
          )
        )
        if (data.table::is.data.table(value)) {
          dbc::assert_has_names(value, required_names = "level")
        }
      }

      # vd funs ----------------------------------------------------------------
      vd_get <- function(col_nms = NULL) {
        out <- data[["var_dt"]]
        if (is.null(col_nms)) {
          col_nms <- names(out)
        }
        out <- dt_independent_frame_dependent_contents__(out, col_nms)
        return(out[])
      }
      vd_implied_get <- function() {
        vsd <- vsd_get()
        dt <- data.table::data.table(
          var_nm = unlist(vsd[["var_nm_set"]]),
          var_set_dt_pos = unlist(lapply(
            seq_along(vsd[["var_nm_set"]]),
            function(i) {
              rep(i, length(vsd[["var_nm_set"]][[i]]))
            }
          ))
        )
        if (ncol(dt) > 0) {
          data.table::setkeyv(dt, names(dt))
        }
        return(dt[])
      }
      vd_set <- function(dt) {
        data[["var_dt"]] <- dt
      }

      # vsd funs ---------------------------------------------------------------
      vsd_get <- function(col_nms = NULL) {
        out <- data[["var_set_dt"]]
        if (!is.null(col_nms)) {
          dbc::assert_vector_elems_are_in_set(col_nms, set = names(out))
          out <- dt_independent_frame_dependent_contents__(out, col_nms)
        }
        return(out[])
      }
      vsd_set <- function(dt) {
        data[["var_set_dt"]] <- dt
      }

      # vd_vsd funs ------------------------------------------------------------
      vd_vsd_linkage_refresh <- function() {
        vd <- vd_get()
        if (nrow(vd) == 0L) {
          return(invisible(NULL))
        }
        vdi <- vd_implied_get()
        data.table::setkeyv(vd, intersect(names(vdi), names(vd)))
        i.var_set_dt_pos <- NULL # appease R CMD CHECK
        #' @importFrom data.table :=
        vd[
          i = vdi,
          on = "var_nm",
          j = "var_set_dt_pos" := i.var_set_dt_pos
        ]
        data.table::setkeyv(vd, intersect(names(vdi), names(vd)))
        vd_set(vd)
        return(invisible(NULL))
      }
      vd_vsd_intersect <- function() {
        vd <- vd_get()
        vdi <- vd_implied_get()
        rm_var_nms <- union(
          setdiff(
            vd[["var_nm"]],
            vdi[["var_nm"]]
          ),
          setdiff(
            vdi[["var_nm"]],
            vd[["var_nm"]]
          )
        )
        if (length(rm_var_nms) > 0) {
          vd_subset <- !vd[["var_nm"]] %in% rm_var_nms
          vd <- vd[vd_subset, ]
          vd_set(vd)

          vsd <- vsd_get()
          #' @importFrom data.table :=
          vsd[
            j = "var_nm_set" := list(
              var_nm_set = lapply(.SD[["var_nm_set"]], setdiff, y = rm_var_nms)
            )
          ]
          vsd_subset <- vapply(vsd[["var_nm_set"]], length, integer(1L)) > 0L
          vsd <- vsd[vsd_subset, ]
          if ("value_space" %in% names(vsd)) {
            vsd[
              j = "value_space" := list(
                #' @importFrom data.table .N
                value_space = lapply(seq_len(.N), function(i) {
                  vs_i <- .SD[["value_space"]][[i]]
                  if ("dt" %in% names(vs_i)) {
                    dt_i <- vs_i[["dt"]]
                    keep_nms_i <- setdiff(names(dt_i), rm_var_nms)
                    keep_dt_i <- dt_i[
                      i = !duplicated(dt_i, by = keep_nms_i, fromLast = TRUE),
                      j = .SD,
                      .SDcols = keep_nms_i
                    ]
                    vs_i[["dt"]] <- keep_dt_i
                  }
                  vs_i
                })
              )
            ]
          }
          vsd_set(vsd)
        }
        vd_vsd_linkage_refresh()
      }

      # var_set funs -----------------------------------------------------------
      var_set_id_to_pos <- function(id) {
        vsd <- vsd_get()
        data.table::chmatch(id, vsd[["id"]])
      }
      var_set_pos_to_id <- function(pos) {
        vsd_get()[["id"]][pos]
      }
      var_to_var_set_id <- function(var_nm) {
        pos <- var_meta_get(var_nm = var_nm, meta_nm = "var_set_dt_pos")
        id <- var_set_pos_to_id(pos)
        return(id)
      }
      # slot:var_set_list_get
      var_set_list_get <- function() {
        var_set_list <- var_set_meta_get_all("var_nm_set")
        names(var_set_list) <- var_set_meta_get_all("id")
        return(var_set_list)
      }
      # slot:var_set_get
      var_set_get <- function(id) {
        assert_is_var_set_id(id)
        vsd <- vsd_get()
        return(vsd[["var_nm_set"]][[var_set_id_to_pos(id)]])
      }
      # slot:var_set_meta_get
      var_set_meta_get <- function(
        id,
        meta_nm
      ) {
        assert_is_var_set_id(id)
        assert_is_var_set_meta_nm(meta_nm)
        vsd <- vsd_get()
        vsd[[meta_nm]][[var_set_id_to_pos(id)]]
      }
      # slot:var_set_meta_set
      var_set_meta_set <- function(
        id,
        meta_nm,
        value
      ) {
        assert_is_var_set_id(id)
        vsd <- vsd_get()
        data.table::set(
          vsd,
          i = var_set_id_to_pos(id),
          j = meta_nm,
          value = value
        )
        vsd_set(vsd)
      }
      # slot:var_set_meta_get_all
      var_set_meta_get_all <- function(
        meta_nm
      ) {
        assert_is_var_set_meta_nm(meta_nm)
        vsd <- vsd_get()
        vsd[[meta_nm]]
      }
      # slot:var_set_rename
      var_set_rename <- function(old, new) {
        assert_is_var_set_id(old)
        dbc::assert_is_character_nonNA_atom(new)
        var_set_meta_set(id = old, meta_nm = "id", value = new)
      }
      # slot:var_set_remove
      var_set_remove <- function(id) {
        assert_is_var_set_id(id)
        pos <- var_set_id_to_pos(id)
        vsd <- vsd_get()
        vsd_subset <- setdiff(seq_len(nrow(vsd)), pos)
        vsd <- vsd[vsd_subset, ]
        vsd_set(vsd)
        vd_vsd_intersect()
      }

      # var_set_value_space funs -----------------------------------------------
      var_set_value_space_is_defined <- function() {
        vsd <- vsd_get()
        return("value_space" %in% names(vsd))
      }
      # slot:var_set_value_space_get
      var_set_value_space_get <- function(id) {
        assert_is_var_set_id(id)
        assert_var_set_value_space_is_defined()
        vsd <- vsd_get()
        pos <- var_set_id_to_pos(id)
        return(vsd[["value_space"]][[pos]])
      }
      # slot:var_set_value_space_set
      var_set_value_space_set <- function(id, value_space) {
        assert_var_set_value_space_is_defined()
        vsd <- vsd_get()
        pos <- var_set_id_to_pos(id)
        data.table::set(
          vsd,
          i = pos,
          j = "value_space",
          # to ensure value_space remains a list
          value = list(list(value_space))
        )
        vsd_set(vsd)
      }
      var_set_value_set_dt_subset_expr <- function(id, expr) {
        assert_var_set_value_space_is_defined()
        dbc::assert_is_language_object(expr, assertion_type = "prod_input")
        vs <- var_set_value_space_get(id)
        dt <- vs[["dt"]]
        if (!data.table::is.data.table(dt)) {
          stop("Value space for id = \"", id, "\"  is not a data.table.")
        }
        dt_expr <- substitute(dt[i = expr], list(expr = expr))
        dt <- eval(dt_expr)
        var_set_value_space_set(id, dt)
        return(invisible(NULL))
      }
      # slot:var_set_value_space_dt_subset
      var_set_value_space_dt_subset <- function(
        id,
        expr
      ) {
        assert_var_set_value_space_is_defined()
        assert_is_var_set_id(id)
        expr <- substitute(expr)
        var_set_value_set_dt_subset_expr(id, expr)
      }

      # var funs ---------------------------------------------------------------
      # slot:var_meta_get
      var_meta_get <- function(var_nm, meta_nm) {
        assert_is_var_nm(var_nm)
        assert_is_var_meta_nm(meta_nm)
        vd <- vd_get()
        jdt <- data.table::setDT(list(var_nm = var_nm))
        # retrieve "pos" instead of joining directly to ensure that list-type
        # columns are handled correctly.
        pos <- vd[
          i = jdt,
          on = "var_nm",
          which = TRUE
        ]
        out <- vd[[meta_nm]][pos]
        if (inherits(out, "list") && length(out) == 1L) {
          out <- out[[1]]
        }
        return(out)
      }
      # slot:var_meta_set
      var_meta_set <- function(
        var_nm,
        meta_nm,
        value
      ) {
        assert_is_var_nm(var_nm)
        vd <- vd_get()
        data.table::set(
          vd,
          i = data.table::chmatch(var_nm, vd[["var_nm"]]),
          j = meta_nm,
          value = value
        )
        return(invisible(NULL))
      }
      # slot:var_meta_get_all
      var_meta_get_all <- function(meta_nm) {
        assert_is_var_meta_nm(meta_nm)
        vd <- vd_get()
        vd[[meta_nm]]
      }
      # slot:var_rename
      var_rename <- function(old, new) {
        assert_is_var_nm(old)
        dbc::assert_is_character_nonNA_atom(new)
        var_meta_set(var_nm = old, meta_nm = "var_nm", value = new)
        id <- var_to_var_set_id(new)
        if (var_set_value_space_is_defined()) {
          vs <- var_set_value_space_get(id = id)
          if ("dt" %in% names(vs)) {
            data.table::setnames(vs[["dt"]], old, new)
          }
          var_set_value_space_set(id = id, value_space = vs)
        }
        var_set <- var_set_meta_get(id = id, meta_nm = "var_nm_set")
        var_set[var_set == old] <- new
        var_set_meta_set(id = id, meta_nm = "var_nm_set", value = var_set)
        invisible(NULL)
      }
      # slot:var_remove
      var_remove <- function(var_nm) {
        assert_is_var_nm(var_nm)
        expr <- substitute(var_nm != VN, list(VN = var_nm))
        vame_subset_expr(expr)
      }
      # slot:var_label_dt_get
      var_label_dt_get <- function(var_nm) {
        assert_is_var_nm(var_nm)
        var_meta_get(var_nm, "label_dt")
      }
      # slot:var_label_dt_set
      var_label_dt_set <- function(var_nm, value) {
        assert_is_var_nm(var_nm)
        assert_is_var_label_dt(value)
        var_meta_set(var_nm, "label_dt", value)
      }
      # slot:var_labels_get
      var_labels_get <- function(x, var_nm, label_col_nm) {
        assert_is_var_nm(var_nm)
        ldt <- var_label_dt_get(var_nm = var_nm)
        if (is.null(ldt)) {
          stop("Variable \"", var_nm, "\" has no label_dt defined.")
        }
        dbc::assert_is_character_nonNA_atom(label_col_nm)
        label_col_nm_set <- setdiff(names(ldt), "level")
        if (!label_col_nm %in% label_col_nm_set) {
          stop("label_col_nm = \"", label_col_nm, "\" not one of the defined ",
               "label columns: ", deparse1(label_col_nm_set))
        }
        dbc::assert_has_class(x = x, required_class = class(ldt[["level"]]))
        jdt <- data.table::setDT(list(level = x))
        #' @importFrom data.table .SD
        ldt[
          i = jdt,
          on = "level",
          j = .SD[[1]],
          .SDcols = label_col_nm
        ]
      }

      # vame funs --------------------------------------------------------------
      vame_subset_expr <- function(expr) {
        dbc::assert_is_language_object(expr, assertion_type = "prod_input")
        vd <- vd_get()
        vd <- eval(
          substitute(vd[i = expr], list(expr = expr))
        )
        vd_set(vd)
        vd_vsd_intersect()
      }
      # slot:vame_subset
      vame_subset <- function(expr) {
        expr <- substitute(expr)
        vame_subset_expr(expr)
        invisible(NULL)
      }
      # slot:vame_union_append
      vame_union_append <- function(x) {
        e <- environment(x@remove)
        vd_1 <- vd_get()
        vsd_1 <- vsd_get()
        vd_2 <- e[["vd_get"]]()
        vsd_2 <- e[["vsd_get"]]()
        vd <- rbind(vd_1, vd_2)
        vsd <- rbind(vsd_1, vsd_2)
        vsd <- vsd[!duplicated(vsd[["var_nm_set"]]), ]
        vd_set(vd)
        vd_vsd_linkage_refresh()
        vsd_set(vsd)
        return(invisible(NULL))
      }

      # vame_category_space funs -----------------------------------------------
      # slot:vame_category_space_dt_list
      vame_category_space_dt_list <- function(var_nms, env = NULL) {
        dbc::assert_is_one_of(
          env,
          funs = list(dbc::report_is_NULL,
                      dbc::report_is_environment)
        )
        if (is.null(env)) {
          env <- parent.frame(1L)
        }
        vd <- vd_get(c("var_nm", "type"))
        is_categorical <- vd[["type"]] == "categorical"
        dbc::assert_vector_elems_are_in_set(
          var_nms,
          set = vd[["var_nm"]][is_categorical]
        )
        vsd <- vsd_get(c("id", "var_nm_set", "value_space"))
        dtl <- category_space_dt_list__(
          var_nms = var_nms,
          vsd = vsd,
          env = env
        )
        return(dtl)
      }
      # slot:vame_category_space_dt
      vame_category_space_dt <- function(var_nms, env = NULL) {
        if (is.null(env)) {
          env <- parent.frame(1L)
        }
        dtl <- vame_category_space_dt_list(var_nms = var_nms, env = env)
        dt <- category_space_dt_list_to_category_space_dt__(dtl)
        return(dt[])
      }
    },
    envir = funs
  )
  funs[["vd_vsd_intersect"]]()
  arg_list <- list(
    Class = "VariableMetadata"
  )
  fun_list <- lapply(vame_slot_nms_get__(), function(fun_nm) {
    funs[[fun_nm]]
  })
  names(fun_list) <- vame_slot_nms_get__()
  arg_list <- c(arg_list, fun_list)
  do.call(methods::new, arg_list, quote = TRUE)
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
