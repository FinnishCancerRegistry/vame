# var_set funs -----------------------------------------------------------------
var_set_dt_print <- function(vm, ...) {
  # @codedoc_comment_block vm@var_set_dt_print
  # Print `var_set_dt`.
  # @codedoc_comment_block vm@var_set_dt_print
  # @codedoc_comment_block news("vm@var_set_dt_print", "2025-06-26", "1.10.4")
  # New function `vm@var_set_dt_print`.
  # @codedoc_comment_block news("vm@var_set_dt_print", "2025-06-26", "1.10.4")
  # @codedoc_comment_block vame::var_set_dt_print::...
  # **`...`**
  #
  # Arguments passed to `print.data.table`.
  # @codedoc_comment_block vame::var_set_dt_print::...
  print(vsd_get(vm), ...)
}

var_set_dt_copy <- function(vm) {
  # @codedoc_comment_block vm@var_set_dt_copy
  # Returns a deep copy of `var_set_dt`.
  # @codedoc_comment_block vm@var_set_dt_copy
  # @codedoc_comment_block news("vm@var_set_dt_copy", "2023-12-13", "0.2.2")
  # New function `vm@var_set_dt_copy`.
  # @codedoc_comment_block news("vm@var_set_dt_copy", "2023-12-13", "0.2.2")
  # @codedoc_comment_block news("vm@var_set_dt_copy", "2023-12-13", "0.4.1")
  # `vm@var_set_dt_copy` now actually returns `var_set_dt` instead of `var_dt`.
  # @codedoc_comment_block news("vm@var_set_dt_copy", "2023-12-13", "0.4.1")
  data.table::copy(data_obj_get(vm, "var_set_dt"))
}

var_set_meta_is_defined <- function(
  vm,
  id,
  meta_nm
) {
  # @codedoc_comment_block function_example(vm@var_set_meta_is_defined)
  # vm <- vame::VariableMetadata(
  #   var_dt = data.table::data.table(
  #     var_nm = c("a", "b")
  #   ),
  #   var_set_dt = data.table::data.table(
  #     id = c("set_a", "set_b"),
  #     var_nm_set = list("a", "b"),
  #     my_list_meta = list(NULL, 1:3),
  #     my_int_meta = c(1L, NA_integer_)
  #   )
  # )
  # stopifnot(
  #   identical(vm@var_set_meta_is_defined(id = NULL, meta_nm = "my_int_meta"), TRUE),
  #   identical(vm@var_set_meta_is_defined(id = NULL, meta_nm = "x"), FALSE),
  #   identical(vm@var_set_meta_is_defined(id = "set_a", meta_nm = "my_int_meta"), TRUE),
  #   identical(vm@var_set_meta_is_defined(id = "set_b", meta_nm = "my_int_meta"), FALSE),
  #   identical(vm@var_set_meta_is_defined(id = "set_a", meta_nm = "my_list_meta"), FALSE),
  #   identical(vm@var_set_meta_is_defined(id = "set_b", meta_nm = "my_list_meta"), TRUE)
  # )
  # @codedoc_comment_block function_example(vm@var_set_meta_is_defined)

  # @codedoc_comment_block news("vm@var_set_meta_is_defined", "2023-12-12", "0.2.2")
  # New function `vm@var_set_meta_is_defined`.
  # @codedoc_comment_block news("vm@var_set_meta_is_defined", "2023-12-12", "0.2.2")
  # @codedoc_comment_block news("vm@var_set_meta_is_defined", "2024-12-19", "1.5.0")
  # `vm@var_set_meta_is_defined` now also accepts `id = NULL`. Then
  # `vm@var_set_meta_is_defined` tests whether `meta_nm` is a column name of
  # `var_set_dt`.
  # @codedoc_comment_block news("vm@var_set_meta_is_defined", "2024-12-19", "1.5.0")
  assert_is_variablemetadata(vm)
  vsd <- vsd_get(vm)
  # @codedoc_comment_block vm@var_set_meta_is_defined
  # Detects whether metadata has been defined for a variable set.
  #
  # - Detects whether `meta_nm` is a column name of `var_set_dt`.
  #   If not or if `is.null(id)`, return results early.
  # @codedoc_comment_block vm@var_set_meta_is_defined
  out <- meta_nm %in% names(vsd)
  if (is.null(id) || isFALSE(out)) {
    return(out)
  }

  # @codedoc_comment_block vm@var_set_meta_is_defined
  # - Else return `TRUE`/`FALSE`  for whether `var_set_dt[[meta_nm]]`
  #   is defined for the particular `id`. For list-valued column a
  #   `NULL` element will result in `FALSE` and for other column types
  #   a missing (`?is.na`) value results in `FALSE`. Otherwise you get `TRUE`.
  # @codedoc_comment_block vm@var_set_meta_is_defined
  assert_is_id(x = id, vm = vm)
  pos <- var_set_id_to_pos(vm, id)
  if (is.list(vsd[[meta_nm]])) {
    out <- !is.null(vsd[[meta_nm]][[pos]])
  } else {
    out <- !is.na(vsd[[meta_nm]][pos])
  }
  return(out)
}

var_set_meta_get <- function(
  vm,
  id,
  meta_nm
) {
  # @codedoc_comment_block vm@var_set_meta_get
  # Get metadata for a specific variable set.
  # @codedoc_comment_block vm@var_set_meta_get

  # @codedoc_comment_block function_example(vm@var_set_meta_get)
  # vm <- vame::VariableMetadata(
  #   var_dt = data.table::data.table(
  #     var_nm = c("a", "b")
  #   ),
  #   var_set_dt = data.table::data.table(
  #     id = c("set_a", "set_b"),
  #     var_nm_set = list("a", "b"),
  #     my_list_meta = list(NULL, 1:3),
  #     my_int_meta = c(1L, NA_integer_)
  #   )
  # )
  # stopifnot(
  #   identical(vm@var_set_meta_get(id = "set_a", meta_nm = "my_int_meta"), 1L),
  #   identical(vm@var_set_meta_get(id = "set_b", meta_nm = "my_list_meta"), 1:3)
  # )
  # @codedoc_comment_block function_example(vm@var_set_meta_get)
  assert_is_variablemetadata(vm)

  assert_is_var_set_id(vm, id)

  # @codedoc_comment_block doc_slot_fun_arg(meta_nm)
  # **`meta_nm`** `[character]` (no default)
  #
  # Name of a metadata column in `var_set_dt` or `var_dt` (depending on context)
  # of a `VariableMetadata` object.
  # @codedoc_comment_block doc_slot_fun_arg(meta_nm)
  assert_is_var_set_meta_nm(vm, meta_nm)
  vsd <- vsd_get(vm)
  vsd[[meta_nm]][[var_set_id_to_pos(vm, id)]]
}

var_set_meta_set <- function(
  vm,
  id,
  meta_nm,
  value
) {
  # @codedoc_comment_block vm@var_set_meta_set
  # Set metadata for a specific variable set.
  # @codedoc_comment_block vm@var_set_meta_set

  # @codedoc_comment_block function_example(vm@var_set_meta_set)
  # vm <- vame::VariableMetadata(
  #   var_dt = data.table::data.table(
  #     var_nm = c("a", "b")
  #   ),
  #   var_set_dt = data.table::data.table(
  #     id = c("set_a", "set_b"),
  #     var_nm_set = list("a", "b"),
  #     my_list_meta = list(NULL, 1:3),
  #     my_int_meta = c(1L, NA_integer_)
  #   )
  # )
  # vm@var_set_meta_set(
  #   id = "set_a",
  #   meta_nm = "my_int_meta",
  #   value = 2L
  # )
  # vm@var_set_meta_set(
  #   id = "set_a",
  #   meta_nm = "my_new_int_meta",
  #   value = 10L
  # )
  # stopifnot(
  #   identical(vm@var_set_meta_get(id = "set_a", meta_nm = "my_int_meta"), 2L),
  #   identical(vm@var_set_meta_get(id = "set_a", meta_nm = "my_new_int_meta"), 10L)
  # )
  #
  # a_value_space <- list(set = 1:3)
  # vm@var_set_meta_set(
  #   id = "set_a",
  #   meta_nm = "value_space",
  #   value = a_value_space
  # )
  # b_value_space <- list(dt = data.table::data.table(b = 1:3))
  # vm@var_set_meta_set(
  #   id = "set_b",
  #   meta_nm = "value_space",
  #   value = b_value_space
  # )
  # stopifnot(
  #   identical(vm@var_set_value_space_get(id = "set_a"), a_value_space),
  #   identical(vm@var_set_value_space_get(id = "set_b"), b_value_space)
  # )
  # @codedoc_comment_block function_example(vm@var_set_meta_set)

  assert_is_variablemetadata(vm)

  # @codedoc_comment_block doc_slot_fun_arg(id)
  # **`id`** `[any]` (no default)
  #
  # ID, or "name", of a variable set. The class of `id` is defined when you
  # create the `VariableMetadata` object and it can be pretty much anything.
  # @codedoc_comment_block doc_slot_fun_arg(id)
  assert_is_var_set_id(vm, id)

  # @codedoc_comment_block doc_slot_fun_arg(value)
  # **`value`** `[any]` (no default)
  #
  # In `_set` functions the value to set for the specified metadata.
  # @codedoc_comment_block doc_slot_fun_arg(value)

  # @codedoc_comment_block news("vm@var_set_meta_set", "2024-09-02", "0.5.6")
  # `vm@var_set_meta_set` now checks `value` for validity for "officially"
  # defined metadata such as `value_space`. Formerly this was done only by
  # the corresponding wrapper such as `vm@var_set_value_space_set`.
  # @codedoc_comment_block news("vm@var_set_meta_set", "2024-09-02", "0.5.6")
  assert_meta(vm = vm, x = value, meta_nm = meta_nm, must_exist = FALSE)

  # @codedoc_comment_block news("vm@var_set_meta_set", "2023-12-12", "0.2.2")
  # `vm@var_set_meta_set` now wraps `value` into a list if it isn't a list
  # and if the target column is a list.
  # @codedoc_comment_block news("vm@var_set_meta_set", "2023-12-12", "0.2.2")
  # @codedoc_comment_block news("vm@var_set_meta_set", "2024-09-04", "0.5.7")
  # `vm@var_set_meta_set` now always wraps `value` into a list before adding
  # it into `vm` when `meta_nm %in% c("value_space", "maker")` --- those are
  # known `list` columns.
  # @codedoc_comment_block news("vm@var_set_meta_set", "2024-09-04", "0.5.7")
  # @codedoc_comment_block vm@var_set_meta_set
  # `value` is wrapped inside a `list` if `var_set_dt[[meta_nm]]` is a `list`
  # or if `meta_nm %in% c("value_space", "maker")`. This is necessary for
  # correct assignment into a `list`-valued column.
  # @codedoc_comment_block vm@var_set_meta_set
  vsd <- vsd_get(vm)
  if (
    meta_nm %in% names(vsd) && is.list(vsd[[meta_nm]]) ||
      meta_nm %in% c("value_space", "maker")
  ) {
    value <- list(value)
    names(value) <- id
  }
  value <- list(value)
  names(value) <- meta_nm
  data.table::set(
    vsd,
    i = var_set_id_to_pos(vm, id),
    j = meta_nm,
    value = value
  )
  vsd_set(vm, vsd)
}

var_set_meta_get_all <- function(
  vm,
  meta_nm
) {
  # @codedoc_comment_block vm@var_set_meta_get_all
  # Get specific metadata for every variable set.
  # @codedoc_comment_block vm@var_set_meta_get_all

  # @codedoc_comment_block function_example(vm@var_set_meta_get_all)
  # vm <- vame::VariableMetadata(
  #   var_dt = data.table::data.table(
  #     var_nm = c("a", "b")
  #   ),
  #   var_set_dt = data.table::data.table(
  #     id = c("set_a", "set_b"),
  #     var_nm_set = list("a", "b"),
  #     my_list_meta = list(NULL, 1:3),
  #     my_int_meta = c(1L, NA_integer_)
  #   )
  # )
  # stopifnot(
  #   identical(
  #     names(vm@var_set_meta_get_all(meta_nm = "my_list_meta")),
  #     c("set_a", "set_b")
  #   ),
  #   identical(
  #     vm@var_set_meta_get_all(meta_nm = "my_int_meta"),
  #     c(set_a = 1L, set_b = NA_integer_)
  #   ),
  #   identical(
  #     vm@var_set_meta_get_all(meta_nm = "my_list_meta"),
  #     list(set_a = NULL, set_b = 1:3)
  #   )
  # )
  # @codedoc_comment_block function_example(vm@var_set_meta_get_all)

  # @codedoc_comment_block news("vm@var_set_meta_get_all", "2023-12-04", "0.2.0")
  # `vm@var_set_meta_get_all` now always sets `var_set_dt$id` as the names
  # of the output list or vector.
  # @codedoc_comment_block news("vm@var_set_meta_get_all", "2023-12-04", "0.2.0")
  assert_is_var_set_meta_nm(vm, meta_nm)
  vsd <- vsd_get(vm)
  out <- vsd[[meta_nm]]
  names(out) <- vsd[["id"]]
  return(out)
}

var_set_var_nm_set_get_all <- function(
  vm
) {
  # @codedoc_comment_block vm@var_set_var_nm_set_get_all
  # Get every variable name set.
  # @codedoc_comment_block vm@var_set_var_nm_set_get_all

  # @codedoc_comment_block news("vm@var_set_var_nm_set_get_all", "2023-12-04", "0.2.0")
  # Rename `vm@var_set_list_get` to `vm@var_set_var_nm_set_get_all`.
  # @codedoc_comment_block news("vm@var_set_var_nm_set_get_all", "2023-12-04", "0.2.0")

  # @codedoc_comment_block doc_slot_fun_arg(vm)
  # **`vm`** `[VariableMetadata]` (no default)
  #
  # A `VariableMetadata` object.
  # @codedoc_comment_block doc_slot_fun_arg(vm)
  var_set_meta_get_all(vm, meta_nm = "var_nm_set")
}

var_set_var_nm_set_get <- function(
  vm,
  id
) {
  # @codedoc_comment_block vm@var_set_var_nm_set_get
  # Get a specific variable name set.
  # @codedoc_comment_block vm@var_set_var_nm_set_get

  # @codedoc_comment_block news("vm@var_set_var_nm_set_get", "2023-12-04", "0.2.0")
  # Rename `vm@var_set_get` to `vm@var_set_var_nm_set_get`.
  # @codedoc_comment_block news("vm@var_set_var_nm_set_get", "2023-12-04", "0.2.0")
  var_set_meta_get(vm, id = id, meta_nm = "var_nm_set")
}

var_set_var_nm_set_set <- function(
  vm,
  id,
  value
) {
  # @codedoc_comment_block vm@var_set_var_nm_set_set
  # Set a new value for a specific variable name set.
  # @codedoc_comment_block vm@var_set_var_nm_set_set

  # @codedoc_comment_block news("vm@var_set_var_nm_set_set", "2023-12-04", "0.2.0")
  # New function `vm@var_set_var_nm_set_set`.
  # @codedoc_comment_block news("vm@var_set_var_nm_set_set", "2023-12-04", "0.2.0")

  var_set_meta_set(
    vm = vm,
    id = id,
    meta_nm = "var_set_nm",
    value = value
  )
}

var_set_rename <- function(
  vm,
  old_ids,
  new_ids
) {
  # @codedoc_comment_block vm@var_set_rename
  # Rename variable sets --- change `var_set_dt$id` values.
  # @codedoc_comment_block vm@var_set_rename

  # @codedoc_comment_block function_example(vm@var_set_rename)
  # vm <- vame::VariableMetadata(
  #   var_dt = data.table::data.table(var_nm = c("a", "b")),
  #   var_set_dt = data.table::data.table(
  #     id = c("set_a", "set_b"),
  #     var_nm_set = list("a", "b")
  #   )
  # )
  # vm@var_set_rename(old_ids = "set_a", new_ids = "set_A")
  # stopifnot(
  #   identical(
  #     vm@var_set_meta_get_all(meta_nm = "id"),
  #     c("set_A" = "set_A", "set_b" = "set_b")
  #   )
  # )
  # @codedoc_comment_block function_example(vm@var_set_rename)

  # @codedoc_comment_block news("vm@var_set_rename", "2023-12-01", "0.2.0")
  # Rename `old` to `old_ids` and `new` to `new_ids`.
  # @codedoc_comment_block news("vm@var_set_rename", "2023-12-01", "0.2.0")

  # @codedoc_comment_block doc_slot_fun_arg(old_ids)
  # **`old_ids`** `[any]` (no default)
  #
  # Old variable set IDs.
  # @codedoc_comment_block doc_slot_fun_arg(old_ids)
  # @codedoc_comment_block doc_slot_fun_arg(new_ids)
  # **`new_ids`** `[any]` (no default)
  #
  # New variable set IDs.
  # @codedoc_comment_block doc_slot_fun_arg(new_ids)
  dbc::assert_is_nonNA(new_ids)
  dbc::assert_is_vector(new_ids)
  lapply(seq_along(old_ids), function(i) {
    assert_is_var_set_id(vm, old_ids[i])
    var_set_meta_set(vm, id = old_ids[i], meta_nm = "id", value = new_ids[i])
  })
  return(invisible(NULL))
}


var_set_remove <- function(
  vm,
  id
) {
  # @codedoc_comment_block function_example(vm@var_set_remove)
  # vm <- vame::VariableMetadata(
  #   var_dt = data.table::data.table(var_nm = c("a", "b")),
  #   var_set_dt = data.table::data.table(
  #     id = c("set_a", "set_b"),
  #     var_nm_set = list("a", "b")
  #   )
  # )
  # vm@var_set_remove(id = "set_a")
  # stopifnot(
  #   identical(
  #     vm@var_set_meta_get_all(meta_nm = "id"),
  #     c("set_b" = "set_b")
  #   ),
  #   identical(
  #     vm@var_meta_get_all(meta_nm = "var_nm"),
  #     c("b" = "b")
  #   )
  # )
  # @codedoc_comment_block function_example(vm@var_set_remove)
  assert_is_var_set_id(vm, id)
  pos <- var_set_id_to_pos(vm, id)
  vsd <- vsd_get(vm)
  vsd_subset <- setdiff(seq_len(nrow(vsd)), pos)
  # @codedoc_comment_block vm@var_set_remove
  # Remove a variable set by `id`. Performs the following steps:
  #
  # - Drop the row in `var_set_dt` with the supplied `id`.
  # @codedoc_comment_block vm@var_set_remove
  vsd <- vsd[vsd_subset, ]
  vsd_set(vm, vsd)
  # @codedoc_comment_block vm@var_set_remove
  # - "Intersect" data in the `VariableMetadata` object:
  # @codedoc_insert_comment_block vame:::vd_vsd_intersect
  # @codedoc_comment_block vm@var_set_remove
  vd_vsd_intersect(vm)
  return(invisible(NULL))
}

# var_set_maker funs -----------------------------------------------------------
var_set_maker_get <- function(
  vm,
  id
) {
  # @codedoc_comment_block vm@var_set_maker_get
  # Get specific `var_set_dt$maker`.
  # @codedoc_comment_block vm@var_set_maker_get
  # @codedoc_comment_block feature_funs(make)
  # - `vm@var_set_maker_get`
  # @codedoc_comment_block feature_funs(make)
  # @codedoc_comment_block news("vm@var_set_maker_get", "2024-01-19", "0.3.0")
  # New function `vm@var_set_maker_get`.
  # @codedoc_comment_block news("vm@var_set_maker_get", "2024-01-19", "0.3.0")
  var_set_meta_get(vm = vm, id = id, meta_nm = "maker")
}

var_set_maker_set <- function(
  vm,
  id,
  value
) {
  # @codedoc_comment_block vm@var_set_maker_set
  # Assign specific `var_set_dt$maker`.
  # @codedoc_comment_block vm@var_set_maker_set
  # @codedoc_comment_block feature_funs(make)
  # - `vm@var_set_maker_set`
  # @codedoc_comment_block feature_funs(make)
  # @codedoc_comment_block news("vm@var_set_maker_set", "2024-01-19", "0.3.0")
  # New function `vm@var_set_maker_set`.
  # @codedoc_comment_block news("vm@var_set_maker_set", "2024-01-19", "0.3.0")
  var_set_meta_set(
    vm = vm,
    id = id,
    meta_nm = "maker",
    value = value
  )
}

var_set_make <- function(
  vm,
  data,
  id = NULL,
  var_nms = NULL,
  env = NULL
) {
  # @codedoc_comment_block vm@var_set_make
  # Call specific `var_set_dt$maker`.
  # @codedoc_comment_block vm@var_set_make
  # @codedoc_comment_block feature_funs(make)
  # - `vm@var_set_make`
  # @codedoc_comment_block feature_funs(make)
  # @codedoc_comment_block news("vm@var_set_make", "2024-01-19", "0.3.0")
  # New function `vm@var_set_make`.
  # @codedoc_comment_block news("vm@var_set_make", "2024-01-19", "0.3.0")

  # @codedoc_comment_block feature(make)
  # The make feature allows for making variable sets based on other variable
  # sets. For instance, a year column might be "made" based on a data column,
  # say `start_year` from `start_date`. This feature becomes available
  # when one or more `var_set_dt$maker` objects have been defined:
  #
  # @codedoc_insert_comment_block specification(var_set_dt$maker)
  #
  # The following functions are related to this feature:
  #
  # @codedoc_insert_comment_block feature_funs(make)
  # @codedoc_comment_block feature(make)

  # @codedoc_comment_block specification(var_set_dt$maker)
  # The recommended approach to writing a `maker` for a variable set is to
  # write a function and store a call to it as the `maker`. See the example
  # for the `make` feature:
  #
  # ```
  # @codedoc_insert_comment_block feature_example(make)
  # ```
  #
  # @codedoc_comment_block specification(var_set_dt$maker)

  # @codedoc_comment_block feature_example(make)
  # # Example of make feature
  # make_dg_y <- function(
  #   dg_date
  # ) {
  #   out <- data.table::data.table(dg_y = data.table::year(dg_date))
  #   return(out[])
  # }
  # # We do not store `my_fun` here directly because if a `VariableMetadata`
  # # object is written to disk, the entire enclosing env of `my_fun` is
  # # included. This can get very messy when it is read back into R.
  # # It is strongly recommended to store quoted expressions instead
  # # and store any functions you write somewhere else, ideally in an R package.
  # my_vame <- vame::VariableMetadata(
  #   var_dt = data.table::data.table(
  #     var_nm = c("dg_y", "dg_date", "area_01", "area_02", "area_03"),
  #     type = c("year", "date", "categorical", "categorical", "categorical")
  #   ),
  #   var_set_dt = data.table::data.table(
  #     id = c("dg_y", "dg_date", "area", "area_01", "area_02"),
  #     var_nm_set = list(
  #       dg_y = "dg_y",
  #       dg_date = "dg_date",
  #       area = c("area_01", "area_02", "area_03"),
  #       area_01 = "area_01",
  #       area_02 = "area_02"
  #     ),
  #     maker = list(
  #       dg_y = list(
  #         maker = quote(make_dg_y(dg_date = dg_date)),
  #         dep_var_nm_set = "dg_date"
  #       ),
  #       dg_date = NULL,
  #       area = NULL,
  #       area_01 = list(
  #         maker = quote({
  #           # This is an example of re-using data from the `VariableMetadata`
  #           # object itself.
  #           dt <- data.table::setDT(list(
  #             x = vm@var_aggregate(
  #               get(dep_var_nm_set),
  #               from_var_nm = dep_var_nm_set,
  #               to_var_nm = var_nms
  #             )
  #           ))
  #           data.table::setnames(dt, "x", var_nms)
  #           dt[]
  #         }),
  #         dep_var_nm_set = "area_02"
  #       ),
  #       # This example uses the shorthand var_aggregate approach.
  #       area_02 = list(maker = "var_aggregate", dep_var_nm_set = "area_03")
  #     ),
  #     value_space = list(
  #       dg_y = list(set = 1953:2024),
  #       dg_date = list(bounds = list(
  #         lo = as.Date("1953-01-01"),
  #         hi = as.Date("2024-12-31"),
  #         lo_inclusive = TRUE,
  #         hi_inclusive = TRUE
  #       )),
  #       area = list(dt = data.table::data.table(
  #         area_01 = c(1L,1L,1L,2L,2L,2L),
  #         area_02 = c(11L,12L,12L,21L,21L,22L),
  #         area_03 = c(111L,121L,122L,211L,212L,221L)
  #       )),
  #       area_01 = NULL,
  #       area_02 = NULL
  #     )
  #   )
  # )
  # obs <- my_vame@var_set_make(
  #   id = "dg_y",
  #   data = data.table::data.table(dg_date = as.Date("2001-01-01"))
  # )
  # exp <- data.table::data.table(dg_y = 2001L)
  # stopifnot(identical(obs[["dg_y"]], exp[["dg_y"]]))

  # obs <- my_vame@var_set_make(
  #   id = "area_01",
  #   data = data.table::data.table(area_02 = c(11L, 21L))
  # )
  # exp <- data.table::data.table(area_01 = c(1L,2L))
  # stopifnot(identical(obs[["area_01"]], exp[["area_01"]]))

  # obs <- my_vame@var_set_make(
  #   id = "area_02",
  #   data = data.table::data.table(area_03 = c(111L, 211L))
  # )
  # exp <- data.table::data.table(area_02 = c(11L,21L))
  # stopifnot(identical(obs[["area_02"]], exp[["area_02"]]))
  #
  # obs <- my_vame@vame_make(
  #   ids = c("dg_y", "area_02", "area_01"),
  #   data = data.table::data.table(
  #     dg_date = as.Date("2001-01-01"),
  #     area_03 = c(111L,121L,122L,211L,212L,221L)
  #   )
  # )
  # exp <- data.table::data.table(
  #   dg_y = 2001L,
  #   area_02 = my_vame@var_aggregate(
  #     c(111L,121L,122L,211L,212L,221L),
  #     from_var_nm = "area_03",
  #     to_var_nm = "area_02"
  #   )
  # )
  # exp[
  #   j = "area_01" := my_vame@var_aggregate(
  #     exp[["area_02"]],
  #     from_var_nm = "area_02",
  #     to_var_nm = "area_01"
  #   )
  # ]
  # stopifnot(
  #   identical(obs[["dg_y"]], exp[["dg_y"]]),
  #   identical(obs[["area_01"]], exp[["area_01"]]),
  #   identical(obs[["area_02"]], exp[["area_02"]])
  # )
  #
  # obs_2 <- my_vame@vame_make(
  #   var_nms = c("dg_y", "area_02", "area_01"),
  #   data = data.table::data.table(
  #     dg_date = as.Date("2001-01-01"),
  #     area_03 = c(111L,121L,122L,211L,212L,221L)
  #   )
  # )
  # obs_3 <- my_vame@vame_make(
  #   var_nms = c("dg_y", "area_02", "area_01"),
  #   data = list(df = data.table::data.table(
  #     dg_date = as.Date("2001-01-01"),
  #     area_03 = c(111L,121L,122L,211L,212L,221L)
  #   ))
  # )
  # stopifnot(
  #   all.equal(obs, obs_2),
  #   all.equal(obs, obs_3)
  # )
  #
  # # Example with a `maker` which works on multiple different input variable sets
  # vm <- vame::VariableMetadata(
  #   var_dt = data.table::data.table(
  #     var_nm = "a"
  #   ),
  #   var_set_dt = data.table::data.table(
  #     id = "a_set",
  #     var_nm_set = list(a_set = "a"),
  #     maker = list(
  #       a_set = list(
  #         dep_var_nm_sets = list(
  #           "b",
  #           "c"
  #         ),
  #         maker = quote({
  #           if (identical(dep_var_nm_set, "b")) {
  #             out <- b + 1L
  #           } else if (identical(dep_var_nm_set, "c")) {
  #             out <- c - 1L
  #           }
  #           return(data.table::data.table(a = out)[])
  #         })
  #       )
  #     )
  #   )
  # )
  # obs_b_1 <- vm@var_set_make(var_nms = "a", data = list(b = 0L))
  # obs_c <- vm@var_set_make(var_nms = "a", data = list(c = 2L))
  # obs_b_2 <- vm@vame_make(var_nms = "a", data = data.frame(b = 0L))
  # stopifnot(
  #   all.equal(obs_b_1, obs_b_2, check.attributes = FALSE),
  #   all.equal(obs_b_1, obs_c, check.attributes = FALSE),
  #   identical(
  #     attr(obs_b_1, "var_set_make_meta"),
  #     list(id = "a_set", var_nms = "a", dep_var_nm_set = "b")
  #   ),
  #   all.equal(
  #     attr(obs_b_2, "vame_make_meta"),
  #     data.table::data.table(
  #       id = "a_set",
  #       var_nms = list("a"),
  #       dep_var_nm_set = list("b")
  #     ),
  #     check.attributes = FALSE
  #   )
  # )
  #
  # # Example where the same `maker` produces multiple columns
  # vm <- vame::VariableMetadata(
  #   var_dt = data.table::data.table(
  #     var_nm = c("a", "b")
  #   ),
  #   var_set_dt = data.table::data.table(
  #     id = "ab_set",
  #     var_nm_set = list(ab_set = c("a", "b")),
  #     maker = list(
  #       a_set = list(
  #         dep_var_nm_set = "c",
  #         maker = quote({
  #           dt <- data.table::data.table(
  #             a = c + 1L,
  #             b = c - 1L
  #           )
  #           return(dt[])
  #         })
  #       )
  #     )
  #   )
  # )
  # dt <- data.table::data.table(c = 1:5)
  # obs_ab <- vm@var_set_make(id = "ab_set", data = dt)
  # obs_a <- vm@var_set_make(id = "ab_set", var_nms = "a", data = dt)
  # obs_b <- vm@var_set_make(id = "ab_set", var_nms = "b", data = dt)
  # stopifnot(
  #   identical(names(obs_ab), c("a", "b")),
  #   identical(names(obs_a), "a"),
  #   identical(names(obs_b), "b")
  # )
  # @codedoc_comment_block feature_example(make)

  dbc::assert_has_one_of_classes(env, classes = c("NULL", "environment"))
  if (is.null(env)) {
    env <- parent.frame(1L)
  }
  # @codedoc_comment_block news("vm@var_set_make", "2024-05-13", "0.5.2")
  # `vm@var_set_make` gains argument `var_nms`. You can now pass either `id` or
  # `var_nms` or both.
  # @codedoc_comment_block news("vm@var_set_make", "2024-05-13", "0.5.2")
  # @codedoc_comment_block vm@var_set_make
  # `vm@var_set_make` performs the following steps:
  #
  # - Figures out `id` if missing but `var_nms` given and vice versa.
  # @codedoc_comment_block vm@var_set_make
  handle_arg_ids_et_var_nms_inplace__(
    vm = vm,
    required_meta_nms = "maker",
    ids_arg_nm = "id",
    var_nms_arg_nm = "var_nms"
  )
  if (length(id) != 1L) {
    stop("`id` must be of length one but inferred `id` was `",
         deparse1(id), "`. Consider passing `id` explicitly.")
  }
  # @codedoc_comment_block vame::var_set_make::data
  # @codedoc_insert_comment_block specs(vame:::handle_arg_data__("arg_list"))
  # @codedoc_comment_block vame::var_set_make::data
  # @codedoc_comment_block vm@var_set_make
  # - `data` is turned into a list of variables, `arg_list`.
  # @codedoc_insert_comment_block steps(vame:::handle_arg_data__("arg_list"))
  # @codedoc_comment_block vm@var_set_make
  arg_list <- handle_arg_data__(data, output_type = "arg_list")
  maker <- var_set_maker_get(vm = vm, id = id)
  arg_list <- local({
    # @codedoc_comment_block news("vm@var_set_make", "2024-05-13", "0.5.2")
    # `vm@var_set_make` now raises an informative error if `data` did not
    # contain something the `maker` needs.
    # @codedoc_comment_block news("vm@var_set_make", "2024-05-13", "0.5.2")
    # @codedoc_comment_block news("vm@var_set_make", "2024-09-26", "1.2.0")
    # `vm@var_set_make` can now handle a `maker` of type `list` with element
    # `dep_var_nm_sets`.
    # @codedoc_comment_block news("vm@var_set_make", "2024-09-26", "1.2.0")
    req_obj_nm_sets <- var_set_maker_req_obj_nm_sets__(
      vm = vm,
      id = id,
      maker = maker
    )
    data_obj_nm_set <- setdiff(names(data), "df")
    if ("df" %in% names(data)) {
      data_obj_nm_set <- union(data_obj_nm_set, names(data[["df"]]))
    }
    miss_req_obj_nm_sets <- lapply(
      X = req_obj_nm_sets,
      FUN = function(req_obj_nm_set) {
        setdiff(
          req_obj_nm_set,
          data_obj_nm_set
        )
      }
    )
    is_usable <- vapply(miss_req_obj_nm_sets, length, integer(1L)) == 0L
    if (!any(is_usable)) {
      msg <- "Argument `data` did not contain all the required objects. "
      if (length(is_usable) == 1) {
        msg <- c(
          msg,
          paste0(
            "These were missing: `", deparse1(miss_req_obj_nm_sets[[1]]), "`."
          )
        )
      } else {
        msg <- c(
          msg,
          "Make sure `data` contains all of the objects of at least one ",
          "of these sets: ",
          vapply(req_obj_nm_sets, function(set) {
            sprintf("\n- `%s`", deparse1(set))
          }, character(1L))
        )
      }
      stop(msg)
    }
    # @codedoc_comment_block news("vm@var_set_make", "2024-06-19", "0.5.3")
    # `vm@var_set_make` passing `var_nms` fixed. It used to pass the
    # `var_nm_set` for the corresponding variable set, now it passes arg
    # `var_nms` (whether inferred or user-given) as intended.
    # @codedoc_comment_block news("vm@var_set_make", "2024-06-19", "0.5.3")
    # @codedoc_comment_block vm@var_set_make
    # - Arg `var_nms` is added to `arg_list$var_nms`. We also add `arg_list$vm`,
    #   the `VariableMetadata` object itself, and `dep_var_nm_set`, the set of
    #   dependency variables names.
    # @codedoc_comment_block vm@var_set_make
    arg_list[["var_nms"]] <- var_nms
    arg_list[["dep_var_nm_set"]] <- req_obj_nm_sets[is_usable][[1]]
    arg_list[["vm"]] <- vm
    return(arg_list)
  })
  if (is.function(maker)) {
    # @codedoc_comment_block vm@var_set_make
    # - If `maker` is a function, it is called with the objects from the above
    #   list which have a corresponding argument.
    # @codedoc_comment_block vm@var_set_make
    # @codedoc_comment_block news("vm@var_set_maker_set", "2024-06-19", "0.5.3")
    # `vm@var_set_maker_set` now ignores arguments passed via `data` to a
    # `maker` of type `function` that do not correspond to any argument name.
    # @codedoc_comment_block news("vm@var_set_maker_set", "2024-06-19", "0.5.3")
    dt <- do.call(
      maker,
      arg_list[intersect(names(formals(maker)), names(arg_list))],
      quote = TRUE
    )
  } else if (inherits(maker, "list")) {
    # @codedoc_comment_block vm@var_set_make
    # - If `maker` is a `list`, an evaluation env is created with `env` as its
    #   parent. This evaluation environment is populated by the objects in the
    #   list detailed above.
    #   If `maker[["maker"]] == "aggregate"`, we use an internally defined
    #   expression that uses `vm@var_aggregate`.
    #   Else `maker[["maker"]]` must be an R expression. In both cases the
    #   expression is evaluated in the evaluation environment.
    # @codedoc_comment_block vm@var_set_make
    make_env <- new.env(parent = env)
    lapply(names(arg_list), function(obj_nm) {
      make_env[[obj_nm]] <- arg_list[[obj_nm]]
    })
    if (identical(maker[["maker"]], "var_aggregate")) {
      maker_call <- quote({
        dt <- data.table::setDT(list(
          x = vm@var_aggregate(
            get(dep_var_nm_set),
            from_var_nm = dep_var_nm_set,
            to_var_nm = var_nms
          )
        ))
        data.table::setnames(dt, "x", var_nms)
        dt[]
      })
    } else if (is.call(maker[["maker"]])) {
      maker_call <- maker[["maker"]]
    } else {
      stop("No logic defined for situation where element `maker` is not ",
           "a `function`, a `call` object, nor \"var_aggregate\".")
    }
    dt <- eval(maker_call, envir = make_env)
  } else {
    stop("Internal error: invalid var_set_dt$maker for id = ", deparse1(id))
  }

  dbc::assert_prod_output_is_data_table(dt)
  dbc::assert_prod_output_has_names(dt, required_names = var_nms)
  # @codedoc_comment_block news("vm@var_set_make", "2025-04-01", "1.8.0")
  # `vm@var_set_make` now deletes all other columns except `var_nms` from output
  # if such exist. Previously the user could request for e.g.
  # `var_nms = "my_var_1"` only, but if the underlying `maker` was defined for
  # e.g. `c("my_var_1", "my_var_2")`, then both were included in output.
  # So now only `"my_var_1"` would be included.
  # @codedoc_comment_block news("vm@var_set_make", "2025-04-01", "1.8.0")
  # @codedoc_comment_block vm@var_set_make
  # - Delete columns other than `var_nms` if found. E.g. if user supplied
  #   `var_nms = "my_var_1"` but the `maker` produced a `data.table` with
  #   columns `my_var_1` and `my_var_2`, remote the latter. If user supplied
  #   `var_nms = NULL` then nothing is deleted.
  # @codedoc_comment_block vm@var_set_make
  del_col_nms <- setdiff(names(dt), var_nms)
  if (length(del_col_nms) > 0) {
    data.table::set(dt, j = del_col_nms, value = NULL)
  }
  # @codedoc_comment_block news("vm@var_set_make", "2024-10-02", "1.3.0")
  # `vm@var_set_make` now sets attribute `var_set_make_meta` on its output.
  # @codedoc_comment_block news("vm@var_set_make", "2024-10-02", "1.3.0")
  # @codedoc_comment_block vm@var_set_make
  # - The `data.table` resulting from the `maker` call is returned after
  #   setting the `var_set_make_meta` attribute as a list containing the
  #   elements `id`, `var_nms`, and `dep_var_nm_set`.
  # @codedoc_comment_block vm@var_set_make
  data.table::setattr(
    dt,
    "var_set_make_meta",
    list(
      id = id,
      var_nms = arg_list[["var_nms"]],
      dep_var_nm_set = arg_list[["dep_var_nm_set"]]
    )
  )
  return(dt[])
}

var_set_maker_get_dep_var_nm_sets <- function(
  vm,
  id
) {
  # @codedoc_comment_block vm@var_set_maker_get_dep_var_nm_sets
  # Get list of dependency variable sets of a `maker`. Always returns a list of
  # `character` vectors of variable names.
  # @codedoc_comment_block vm@var_set_maker_get_dep_var_nm_sets

  # @codedoc_comment_block news("vm@var_set_maker_get_dep_var_nm_sets", "2024-10-02", "1.3.0")
  # New function `vm@var_set_maker_get_dep_var_nm_sets`.
  # @codedoc_comment_block news("vm@var_set_maker_get_dep_var_nm_sets", "2024-10-02", "1.3.0")

  # @codedoc_comment_block function_example(vm@var_set_maker_get_dep_var_nm_sets)
  # vm <- vame::VariableMetadata(
  #   var_dt = data.table::data.table(
  #     var_nm = c("a", "b")
  #   ),
  #   var_set_dt = data.table::data.table(
  #     id = c("a_set", "b_set"),
  #     var_nm_set = list(a_set = "a", b_set = "b"),
  #     maker = list(
  #       a_set = list(
  #         dep_var_nm_sets = list(
  #           "b",
  #           "c"
  #         ),
  #         maker = quote({
  #           if (identical(dep_var_nm_set, "b")) {
  #             out <- b + 1L
  #           } else if (identical(dep_var_nm_set, "c")) {
  #             out <- c - 1L
  #           }
  #           return(data.table::data.table(a = out)[])
  #         })
  #       ),
  #       b_set = list(
  #         dep_var_nm_set = "b_dep",
  #         maker = quote({data.table::data.table(b = b_dep + 1L)})
  #       )
  #     )
  #   )
  # )
  # stopifnot(
  #   identical(
  #     vm@var_set_maker_get_dep_var_nm_sets(id = "a_set"),
  #     vm@var_set_maker_get(id = "a_set")[["dep_var_nm_sets"]]
  #   ),
  #   identical(
  #     vm@var_set_maker_get_dep_var_nm_sets(id = "b_set"),
  #     list(vm@var_set_maker_get(id = "b_set")[["dep_var_nm_set"]])
  #   )
  # )
  # @codedoc_comment_block function_example(vm@var_set_maker_get_dep_var_nm_sets)
  var_set_maker_req_obj_nm_sets__(vm = vm, id = id)
}

vame_make <- function(
  vm,
  data,
  ids = NULL,
  var_nms = NULL,
  env = NULL,
  optional_steps = NULL
) {
  # @codedoc_comment_block feature_funs(make)
  # - `vm@vame_make`
  # @codedoc_comment_block feature_funs(make)
  # @codedoc_comment_block news("vm@vame_make", "2024-01-19", "0.3.0")
  # New function `vm@vame_make`.
  # @codedoc_comment_block news("vm@vame_make", "2024-01-19", "0.3.0")

  # @codedoc_comment_block news("vm@vame_make", "2024-09-11", "1.0.0")
  # `vm@vame_make` gains argument `callbacks`.
  # @codedoc_comment_block news("vm@vame_make", "2024-09-11", "1.0.0")
  # @codedoc_comment_block news("vm@vame_make", "2025-04-02", "1.9.0")
  # `vm@vame_make` gains arg `optional_steps` to replace `callbacks`.
  # Using `callbacks` throws a warning now, an error starting in 1.10.0
  # and it will be removed in 1.11.0.
  # @codedoc_comment_block news("vm@vame_make", "2025-04-02", "1.9.0")
  # @codedoc_comment_block news("vm@vame_make", "2025-06-30", "1.11.0")
  # `vm@vame_make` arg `callbacks` removed. Use `optional_steps`.
  # You will have to rebuild your `VariableMetadata` object to make
  # `vm@vame_make` work after this update.
  # @codedoc_comment_block news("vm@vame_make", "2025-06-30", "1.11.0")
  assert_is_arg_optional_steps(optional_steps)

  # @codedoc_comment_block news("vm@vame_make", "2024-09-11", "1.0.1")
  # `vm@vame_make` gains `callbacks` element `on_entry`.
  # @codedoc_comment_block news("vm@vame_make", "2024-09-11", "1.0.1")
  # @codedoc_comment_block vm@vame_make
  # Call multiple `var_set_dt$maker`s in sequence. Performs these steps:
  #
  # - Calls `optional_steps[["on_entry"]](env = main_env)` if defined.
  #   `main_env` is the local environment of the main function.
  # @codedoc_comment_block vm@vame_make
  main_env <- environment()
  if ("on_entry" %in% names(optional_steps)) {
    optional_steps[["on_entry"]](env = main_env)
  }
  # @codedoc_comment_block vm@vame_make
  # - Calls `on.exit(optional_steps[["on_exit"]](env = main_env))`
  #   if that is defined.
  # @codedoc_comment_block vm@vame_make
  if ("on_exit" %in% names(optional_steps)) {
    on.exit(optional_steps[["on_exit"]](env = main_env))
  }

  # @codedoc_comment_block vame::vame_make::data
  # @codedoc_insert_comment_block specs(vame:::handle_arg_data__("df_list"))
  # @codedoc_comment_block vame::vame_make::data
  data <- handle_arg_data__(data, output_type = "df_list")
  data[["df"]] <- dt_independent_frame_dependent_contents__(data[["df"]])
  df_start_col_nms <- data.table::copy(names(data[["df"]]))
  # @codedoc_comment_block news("vm@vame_make", "2024-05-13", "0.5.2")
  # `vm@vame_make` gains argument `var_nms`. You can now pass either `ids` or
  # `var_nms` or both.
  # @codedoc_comment_block news("vm@vame_make", "2024-05-13", "0.5.2")
  # @codedoc_comment_block vm@vame_make
  # - Infers `ids` to use if user gave `var_nms` but not `ids` and vice versa.
  # @codedoc_comment_block vm@vame_make
  handle_arg_ids_et_var_nms_inplace__(
    vm = vm,
    required_meta_nms = "maker"
  )
  dbc::assert_has_one_of_classes(env, classes = c("NULL", "environment"))
  if (is.null(env)) {
    env <- parent.frame(1L)
  }

  ids <- local({
    # @codedoc_comment_block news("vm@vame_make", "2024-05-13", "0.5.2")
    # `vm@vame_make` automatically determines the appropriate order of `ids`
    # (whether user-supplied or inferred) in which their `maker`s should be
    # called. For instance if `maker` for `ids[1]` requires the variables
    # created by the `maker` for `ids[2]`, then the latter is called first.
    # @codedoc_comment_block news("vm@vame_make", "2024-05-13", "0.5.2")
    # @codedoc_comment_block vm@vame_make
    # - Determines order in which the `maker`s of `ids` should be called.
    #   For instance if `maker` for `ids[1]` requires the variables
    #   created by the `maker` for `ids[2]`, then the latter is called first.
    # @codedoc_comment_block vm@vame_make
    meta_df <- data.frame(id = ids)
    meta_df[["req_obj_nm_set_set"]] <- lapply(meta_df[["id"]], function(id) {
      var_set_maker_req_obj_nm_sets__(vm = vm, id = id)
    })
    meta_df[["output_var_nm_set"]] <- lapply(meta_df[["id"]], function(id) {
      var_set_meta_get(vm = vm, id = id, meta_nm = "var_nm_set")
    })
    order_of_ids <- rep(NA_integer_, length(ids))
    data_obj_nm_set <- union(
      names(data[["df"]]),
      setdiff(names(data), "df")
    )
    usable_obj_nm_set <- data_obj_nm_set
    for (i in seq_along(order_of_ids)) {
      candidate_ids <- setdiff(ids, ids[order_of_ids])
      makeable_ids <- candidate_ids[vapply(
        candidate_ids,
        function(id) {
          idx <- match(id, meta_df[["id"]])
          has_reqs <- vapply(
            meta_df[["req_obj_nm_set_set"]][[idx]],
            function(req_obj_nm_set) {
              all(req_obj_nm_set %in% usable_obj_nm_set)
            },
            logical(1L)
          )
          any(has_reqs)
        },
        logical(1L)
      )]
      if (length(makeable_ids) == 0) {
        print(meta_df[setdiff(seq_len(nrow(meta_df)), order_of_ids), ])
        stop(
          "Could not determine order in which to call `maker`s: One or more ",
          "variable sets in set of ids (first five): `",
          utils::head(deparse1(unname(ids)), 5L),
          "` has `maker` ",
          "dependencies not satisfied by what was passed via `data` and what ",
          "other `maker`s can produce. Either pass more variable via `data` ",
          "or fix one or more of the `maker`s. See the printed table above ",
          "for more information. Argument `data` contains objects with names ",
          "(first five)`",
          deparse1(utils::head(unname(data_obj_nm_set), 5L)), "`"
        )
      }
      order_of_ids[i] <- match(makeable_ids, ids)[1L]
      usable_obj_nm_set <- union(
        usable_obj_nm_set,
        meta_df[["output_var_nm_set"]][[
          match(ids[order_of_ids[i]], meta_df[["id"]])
        ]]
      )
    }
    ids[order_of_ids]
  })

  # @codedoc_comment_block news("vm@vame_make", "2024-09-11", "1.0.1")
  # `vm@vame_make` gains `callbacks` element `pre_loop`.
  # @codedoc_comment_block news("vm@vame_make", "2024-09-11", "1.0.1")
  # @codedoc_comment_block news("vm@vame_make", "2024-09-11", "1.2.0")
  # Rename `vm@vame_make` `callbacks` element `pre_loop` to `pre_lapply`.
  # @codedoc_comment_block news("vm@vame_make", "2024-09-11", "1.2.0")
  # @codedoc_comment_block vm@vame_make
  # - Calls `optional_steps[["pre_lapply"]](env = main_env)` if defined.
  # @codedoc_comment_block vm@vame_make
  if ("pre_lapply" %in% names(optional_steps)) {
    optional_steps[["pre_lapply"]](env = main_env)
  }
  vame_make_meta_dt <- data.table::rbindlist(lapply(ids, function(id) {
    # @codedoc_comment_block news("vm@vame_make", "2024-09-26", "1.2.0")
    # `vm@vame_make` gains `callbacks` element `lapply_on_entry`.
    # @codedoc_comment_block news("vm@vame_make", "2024-09-26", "1.2.0")
    # @codedoc_comment_block vm@vame_make
    # - Call `lapply` on `ids` with an anonymous function.
    #   For each `ids` element:
    #   * Calls `optional_steps[["lapply_on_entry"]](env = lapply_fun_env)` if
    #     defined. `lapply_fun_env` is the local environment of the anonymous
    #     function that `lapply` calls. `lapply_fun_env` contains object
    #     `id` at this point, an individual `var_set_dt$id` value.
    # @codedoc_comment_block vm@vame_make
    lapply_fun_env <- environment()
    if ("lapply_on_entry" %in% names(optional_steps)) {
      optional_steps[["lapply_on_entry"]](env = lapply_fun_env)
    }
    # @codedoc_comment_block news("vm@vame_make", "2024-09-26", "1.2.0")
    # `vm@vame_make` gains `callbacks` element `lapply_on_exit`.
    # @codedoc_comment_block news("vm@vame_make", "2024-09-26", "1.2.0")
    # @codedoc_comment_block vm@vame_make
    #   * Calls
    #     `on.exit(optional_steps[["lapply_on_exit"]](env = lapply_fun_env))`
    #     if defined.
    # @codedoc_comment_block vm@vame_make
    if ("lapply_on_exit" %in% names(optional_steps)) {
      on.exit(optional_steps[["lapply_on_exit"]](env = lapply_fun_env))
    }
    # this called just in case some make expression makes use of a slot function
    # --- see where the slots are created.
    var_nms_of_id <- NULL
    if (!is.null(var_nms)) {
      var_nms_of_id <- intersect(
        var_set_var_nm_set_get(vm = vm, id = id),
        var_nms
      )
    }
    # @codedoc_comment_block news("vm@vame_make", "2024-09-26", "1.2.0")
    # `vm@vame_make` gains `callbacks` element `lapply_pre_var_set_make`.
    # @codedoc_comment_block news("vm@vame_make", "2024-09-26", "1.2.0")
    # @codedoc_comment_block vm@vame_make
    #   * Calls
    #     `optional_steps[["lapply_pre_var_set_make"]](env = lapply_fun_env)`
    #     if that is defined. `lapply_fun_env` contains objects
    #     `id` and `var_nms_of_id`, where the latter is the intersection of
    #     arg `var_nms` and those variable names that the variable set with
    #     `id` contains.
    # @codedoc_comment_block vm@vame_make
    if ("lapply_pre_var_set_make" %in% names(optional_steps)) {
      optional_steps[["lapply_pre_var_set_make"]](env = lapply_fun_env)
    }
    # @codedoc_comment_block vm@vame_make
    #   * Calls `vm@var_set_make`.
    # @codedoc_comment_block vm@vame_make
    made_dt <- var_set_make(
      vm = vm,
      id = id,
      var_nms = var_nms_of_id,
      data = data,
      env = env
    )
    # @codedoc_comment_block news("vm@vame_make", "2024-09-11", "1.2.0")
    # Rename `vm@vame_make` `callbacks` element `post_var_set_make` to
    # `lapply_post_var_set_make`.
    # @codedoc_comment_block news("vm@vame_make", "2024-09-11", "1.2.0")
    # @codedoc_comment_block vm@vame_make
    #   * Calls
    #     `optional_steps[["lapply_post_var_set_make"]](env = lapply_fun_env)`
    #     if that is defined. `lapply_fun_env` contains objects
    #     `id`, `var_nms_of_id`, and `made_dt` where the `made_dt` is the result
    #     of the `var_set_make` call.
    # @codedoc_comment_block vm@vame_make
    if ("lapply_post_var_set_make" %in% names(optional_steps)) {
      optional_steps[["lapply_post_var_set_make"]](env = lapply_fun_env)
    }
    # @codedoc_comment_block vm@vame_make
    #   * Adds the created columns into `data[["df"]]`. The same object is used
    #     in subsequent calls to `vm@var_set_make`.
    # @codedoc_comment_block vm@vame_make
    data.table::set(
      x = data[["df"]],
      j = names(made_dt),
      value = made_dt
    )
    meta <- attr(made_dt, "var_set_make_meta")
    list_col_nms <- setdiff(names(meta), "id")
    meta[list_col_nms] <- lapply(meta[list_col_nms], list)
    return(meta)
  }))
  data.table::set(data[["df"]], j = df_start_col_nms, value = NULL)

  # @codedoc_comment_block news("vm@vame_make", "2024-10-02", "1.3.0")
  # `vm@vame_make` now sets the attribute `vame_make_meta` on its output.
  # @codedoc_comment_block news("vm@vame_make", "2024-10-02", "1.3.0")
  # @codedoc_comment_block vm@vame_make
  # - Returns a `data.table` containing the columns created by the `maker`s,
  #   i.e. `data[["df"]]` without the original columns.
  #   The `vame_make_meta` attribute is set as a `data.table` containing the
  #   columns `id`, `var_nms`, and `dep_var_nm_set` in the order of execution.
  # @codedoc_comment_block vm@vame_make
  data.table::setattr(
    data[["df"]],
    "vame_make_meta",
    vame_make_meta_dt
  )
  return(data[["df"]][])
}

# var_set_value_space funs -----------------------------------------------------
var_set_value_space_eval <- function(
  vm,
  id,
  var_nms = NULL,
  env = NULL
) {
  # @codedoc_comment_block feature_funs(value spaces)
  # - `vm@var_set_value_space_eval`
  # @codedoc_comment_block feature_funs(value spaces)

  # @codedoc_comment_block function_example(vm@var_set_value_space_eval)
  # # vm@var_set_value_space_eval
  # vm <- vame::VariableMetadata(
  #   var_dt = data.table::data.table(
  #     var_nm = c("a", "b", "c")
  #   ),
  #   var_set_dt = data.table::data.table(
  #     id = c("a_set", "b_set", "c_set"),
  #     var_nm_set = list("a", "b", "c"),
  #     value_space = list(
  #       list(expr = quote(list(set = 1:3))),
  #       list(expr = quote(1:3)),
  #       list(set = 1:3)
  #     )
  #   )
  # )
  # stopifnot(
  #   identical(
  #     vm@var_set_value_space_eval(id = "a_set"),
  #     vm@var_set_value_space_eval(id = "b_set")
  #   ),
  #   identical(
  #     vm@var_set_value_space_eval(id = "a_set"),
  #     vm@var_set_value_space_eval(id = "c_set")
  #   )
  # )
  #
  # # Since v1.6.0, a `labeler` of class `data.table` can also be used.
  # vm <- vame::VariableMetadata(
  #   var_dt = data.table::data.table(
  #     var_nm = "a",
  #     labeler = list(data.table::data.table(x = 1:3, my_label = letters[1:3]))
  #   ),
  #   var_set_dt = data.table::data.table(
  #     id = "a_set",
  #     var_nm_set = list("a")
  #   )
  # )
  # stopifnot(all.equal(
  #   vm@var_set_value_space_eval(id = "a_set"),
  #   list(dt = data.table::data.table(a = 1:3)),
  #   check.attributes = FALSE
  # ))
  # @codedoc_comment_block function_example(vm@var_set_value_space_eval)


  # @codedoc_comment_block news("vm@var_set_value_space_eval", "2023-07-03", "0.1.1")
  # New slot `vm@var_set_value_space_eval`.
  # @codedoc_comment_block news("vm@var_set_value_space_eval", "2023-07-03", "0.1.1")
  # @codedoc_comment_block news("vm@var_set_value_space_eval", "2023-07-17", "0.1.7")
  # New exported fun `vame::var_set_value_space_eval` --- alternative for
  # `vm@var_set_value_space_eval`.
  # @codedoc_comment_block news("vm@var_set_value_space_eval", "2023-07-17", "0.1.7")
  # @codedoc_comment_block news("vm@var_set_value_space_eval", "2023-11-29", "0.1.9")
  # `vame::var_set_value_space_eval` + `vm@var_set_value_space_eval` gain
  # arg `var_nms`. You can now evaluate the value space for only a subset of
  # the variables in the set.
  # @codedoc_comment_block news("vm@var_set_value_space_eval", "2023-11-29", "0.1.9")

  # @codedoc_comment_block feature(value spaces)
  # The value spaces feature allows you to define what values (combinations of)
  # variables can have. This information can then be retrieved from one single
  # place (the `VariableMetadata` object) for use elsewhere, e.g. for computing
  # something by strata. See also the category spaces and assertions features.
  #
  # The value spaces feature becomes available when `var_set_dt` contains column
  # `value_space`. You can include it when `VariableMetadata` is constructed
  # or you can use `vm@var_set_value_space_set` later.
  #
  # @codedoc_insert_comment_block specification(var_set_dt$value_space)
  #
  # The following functions are related to this feature:
  # @codedoc_insert_comment_block feature_funs(value spaces)
  # @codedoc_comment_block feature(value spaces)

  dbc::assert_inherits(vm, required_class = "VariableMetadata")

  assert_is_var_set_id(vm, id)
  assert_is_arg_var_nms(vm = vm, x = var_nms)
  value_space_var_nms <- var_set_meta_get(vm, id = id, meta_nm = "var_nm_set")
  if (is.null(var_nms)) {
    var_nms <- value_space_var_nms
  } else {
    dbc::assert_vector_elems_are_in_set(x = var_nms, set = value_space_var_nms)
  }
  # @codedoc_comment_block doc_slot_fun_arg(env)
  # **`env`** `[NULL, environment]` (default `NULL`)
  #
  # Environment where a value space will be evaluated, if applicable.
  #
  # - `NULL`: Use the environment where the function was called.
  # - `environment`: Use this environment.
  # @codedoc_comment_block doc_slot_fun_arg(env)
  dbc::assert_is_one_of(
    env,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_environment)
  )
  if (is.null(env)) {
    env <- parent.frame(1L)
  }

  # @codedoc_comment_block news("vm@var_set_value_space_eval", "2024-09-11", "1.1.0")
  # `vm@var_set_value_space_eval` now makes variables `id`, `var_nms`, and `vm`
  # available for `labeler` types `function` and `call`.
  # @codedoc_comment_block news("vm@var_set_value_space_eval", "2024-09-11", "1.1.0")
  # @codedoc_comment_block vm@var_set_value_space_eval
  # Retrieve and evaluate value space for a variable set given its `id`.
  #
  # - The variables `id`, `var_nms`, and `vm` are made available to
  #   `value_space` objects of type `function` and `call`.
  #   `vm` is the `vame::VariableMetadata` object itself.
  # @codedoc_comment_block vm@var_set_value_space_eval
  arg_list <- mget(c("vm", "id", "var_nms"))

  if (!var_set_meta_is_defined(vm = vm, id = id, meta_nm = "value_space")) {
    # @codedoc_comment_block news("vm@var_set_value_space_eval", "2025-02-13", "1.6.0")
    # `vm@var_set_value_space_eval` now can make use of a `labeler` of class
    # `data.table` in cases where no `value_space` is defined for the `id`,
    # but `var_nms` is of length one, and that variable has such a `labeler`.
    # @codedoc_comment_block news("vm@var_set_value_space_eval", "2025-02-13", "1.6.0")
    # @codedoc_comment_block vm@var_set_value_space_eval
    # - If `id` (whether inferred or supplied) has no `value_space` defined
    #   an error is usually raised. However, if `var_nms` is of length one,
    #   and that variable has a `labeler` of class `data.table`, then
    #   in effect `list(dt = labeler)` is returned (minus the label columns
    #   themselves, and after renaming `labeler$x` to `labeler[[var_nms]]`).
    # @codedoc_comment_block vm@var_set_value_space_eval
    if (
      length(var_nms) == 1 &&
        var_meta_is_defined(vm = vm, var_nm = var_nms, meta_nm = "labeler") &&
        data.table::is.data.table(var_labeler_get(vm = vm, var_nm = var_nms))
    ) {
      dt <- var_labeler_get(vm = vm, var_nm = var_nms)
      #' @importFrom data.table .SD
      dt <- dt[j  = .SD, .SDcols = "x"]
      data.table::setnames(dt, var_nms)
      value_space <- list(dt = dt)
      assert_is_value_space(
        x = value_space,
        assertion_type = "prod_output"
      )
      return(value_space)
    } else {
      stop(sprintf("No `value_space` defined for `id = %s`", deparse1(id)))
    }
  }

  value_space <- var_set_value_space_get(vm, id = id)
  assert_is_value_space(
    x = value_space,
    x_nm = sprintf("vm@var_set_value_space_get(id = %s)", deparse1(id)),
    assertion_type = "general"
  )
  if ("expr" %in% names(value_space)) {
    # @codedoc_comment_block vm@var_set_value_space_eval
    # - A `value_space` of type `expr` is evaluated in a temporary evaluation
    #   environment whose parent is set to `env`. The evaluation environment
    #   is populated by the variables listed above.
    # @codedoc_comment_block vm@var_set_value_space_eval
    call_eval_env <- as.environment(arg_list)
    parent.env(call_eval_env) <- env
    call_eval_env[["var_nms"]] <- var_nms
    call_eval_env[["vm"]] <- vm
    value_space <- list(
      tmp = eval(value_space[["expr"]], envir = call_eval_env)
    )
  } else if ("fun" %in% names(value_space)) {
    # @codedoc_comment_block vm@var_set_value_space_eval
    # - A `value_space` of type `function` is called with the subset of the
    #   variables listed above which are named arguments of the `function`.
    #   E.g. you are allowed to have only the argument `var_nms` if you want.
    # @codedoc_comment_block vm@var_set_value_space_eval
    value_space <- do.call(
      value_space[["fun"]],
      arg_list[intersect(names(arg_list), names(formals(value_space[["fun"]])))]
    )
    value_space <- list(tmp = value_space)
  }
  # @codedoc_comment_block vm@var_set_value_space_eval
  # - The evaluation result a `value_space` of type `fun` or `expr` is inspected
  #   and attempted to make into a proper `value_space` object.
  #   It is recommended that the evaluation result is directly a proper
  #   `value_space` object, e.g. `list(set = 1:3)`, but the following is also
  #   supported based on the class of the evaluation result (here `tmp`):
  #   + `data.table` -> `list(dt = tmp)`
  #   + `vector` and not `list` -> `list(set = tmp)`
  #   + `list` with element named `lo` -> `list(bounds = tmp)`
  #   + Otherwise an error is raised.
  # @codedoc_comment_block vm@var_set_value_space_eval
  if ("tmp" %in% names(value_space)) {
    tmp <- value_space[["tmp"]]
    if (test_is_value_space(tmp)) {
      # @codedoc_comment_block news("vm@var_set_value_space_eval", "2025-02-14", "1.6.0")
      # `vm@var_set_value_space_eval` now also allows the evaluation result of
      # a `value_space` of type `expr` or `fun` to be a `value_space` object
      # in itself. In fact that is recommended from now on.
      # @codedoc_comment_block news("vm@var_set_value_space_eval", "2025-02-14", "1.6.0")
      value_space <- tmp
    } else if (data.table::is.data.table(tmp)) {
      names(value_space) <- "dt"
    } else if (is.vector(tmp) && !is.list(tmp)) {
      names(value_space) <- "set"
    } else if (is.list(tmp) && "lo" %in% names(tmp)) {
      names(value_space) <- "bounds"
    } else {
      stop("value space for var_set with id = ", deparse1(id),
           " was of type expr or fun, but did not evaluate into ",
           "a proper value_space object nor the contents of value_space types ",
           "dt, set, nor bounds (data.table, vector, and named list, ",
           "respectively). Output had class(es) ",
           deparse1(class(tmp)), ". It is recommended that a value_space ",
           "of type expr or fun evaluates into a proper value_space object, ",
           "e.g. into list(set = 1:3).")
    }
  }
  # @codedoc_comment_block vm@var_set_value_space_eval
  # - If the `value_space` did not need evaluation, it is simply collected
  #   without modification,
  #   except a `value_space` of type `data.table` (or if evaluation produced
  #   a `data.table`) is subset into the requested variables only if
  #   the `data.table` contains more than the requested variables.
  # @codedoc_comment_block vm@var_set_value_space_eval
  if ("dt" %in% names(value_space) &&
        ncol(value_space[["dt"]]) > length(var_nms)) {
    #' @importFrom data.table .SD
    value_space <- list(dt = value_space[["dt"]][
      i = !duplicated(value_space[["dt"]], by = var_nms),
      j = .SD,
      .SDcols = var_nms
    ])
  }

  return(value_space)
}

var_set_value_space_get <- function(
  vm,
  id
) {
  # @codedoc_comment_block vm@var_set_value_space_get
  # Get the `value_space` of a specific variable set without evaluting it.
  # @codedoc_comment_block vm@var_set_value_space_get
  # @codedoc_comment_block feature_funs(value spaces)
  # - `vm@var_set_value_space_get`
  # @codedoc_comment_block feature_funs(value spaces)
  assert_is_var_set_id(vm, id)
  assert_var_set_value_space_is_defined(vm)
  vsd <- vsd_get(vm)
  pos <- var_set_id_to_pos(vm, id)
  return(vsd[["value_space"]][[pos]])
}


var_set_value_space_set <- function(
  vm,
  id,
  value_space
) {
  # @codedoc_comment_block vm@var_set_value_space_set
  # Set the `value_space` of a variable set.
  # @codedoc_comment_block vm@var_set_value_space_set
  # @codedoc_comment_block feature_funs(value spaces)
  # - `vm@var_set_value_space_set`
  # @codedoc_comment_block feature_funs(value spaces)

  # @codedoc_comment_block doc_slot_fun_arg(value_space)
  # **`value_space`** `[list]` (no default)
  #
  # A value space to assign for the specified variable set.
  # @codedoc_comment_block doc_slot_fun_arg(value_space)

  var_set_meta_set(
    vm = vm,
    id = id,
    meta_nm = "value_space",
    value = value_space
  )
}

var_set_value_space_dt_subset <- function(
  vm,
  id,
  expr,
  enclos = NULL
) {
  # @codedoc_comment_block function_example(vm@var_set_value_space_dt_subset)
  # vm <- vame::VariableMetadata(
  #   var_dt = data.table::data.table(var_nm = c("a", "b")),
  #   var_set_dt = data.table::data.table(
  #     id = c("set_a", "set_b"),
  #     var_nm_set = list("a", "b"),
  #     value_space = list(
  #       set_a = list(dt = data.table::data.table(a = 1:5)),
  #       set_b = list(dt = data.table::data.table(b = 4:7))
  #     )
  #   )
  # )
  # allowed_set_a <- 1:3
  # vm@var_set_value_space_dt_subset(
  #   id = "set_a",
  #   expr = a %in% allowed_set_a
  # )
  # allowed_set_b <- 4:5
  # vm@var_set_value_space_dt_subset(
  #   id = "set_b",
  #   expr = dt$b %in% allowed_set_b
  # )
  # vm@var_set_value_space_dt_subset(
  #   id = "set_b",
  #   expr = quote(b %in% allowed_set_b)
  # )
  # stopifnot(
  #   identical(
  #     vm@var_set_value_space_get(id = "set_a")[["dt"]][["a"]],
  #     allowed_set_a
  #   ),
  #   identical(
  #     vm@var_set_value_space_get(id = "set_b")[["dt"]][["b"]],
  #     allowed_set_b
  #   )
  # )
  # @codedoc_comment_block function_example(vm@var_set_value_space_dt_subset)

  # @codedoc_comment_block feature_funs(value spaces)
  # - `vm@var_set_value_space_dt_subset`
  # @codedoc_comment_block feature_funs(value spaces)
  assert_var_set_value_space_is_defined(vm)
  assert_is_var_set_id(vm, id)

  # @codedoc_comment_block vame::var_set_value_space_dt_subset::enclos
  # **`enclos`** `[NULL, environment]` (default `NULL`)
  #
  # Evaluation environment of `expr` will be a child of `enclos`.
  #
  # - `NULL`: Use environment where `vm@var_set_value_space_dt_subset` was
  #   called.
  # - `environment`: Use this environment.
  # @codedoc_comment_block vame::var_set_value_space_dt_subset::enclos
  # @codedoc_comment_block news("vm@var_set_value_space_dt_subset", "2025-06-30", "1.11.0")
  # `vm@var_set_value_space_dt_subset` gains arg `enclos`.
  # @codedoc_comment_block news("vm@var_set_value_space_dt_subset", "2025-06-30", "1.11.0")
  dbc::assert_is_one_of(
    enclos,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_environment)
  )
  if (is.null(enclos)) {
    enclos <- parent.frame(2L)
  }
  vs <- var_set_value_space_get(vm, id)
  if (!"dt" %in% names(vs)) {
    stop("Value space for id = \"", id, "\"  is not of type `dt`.")
  }
  # @codedoc_comment_block vm@var_set_value_space_dt_subset
  # Take a subset of a value space dt for a variable set and set that as the
  # value space. Performs the following steps:
  #
  # - Create new temporary environment `eval_env` as child of `enclos`.
  #   Assign the `data.table` object of the `value_space` into
  #   `eval_env[["dt"]]`.
  # - Collect `expr` using `substitute`.
  # @codedoc_comment_block vm@var_set_value_space_dt_subset
  eval_env <- new.env(parent = enclos)
  eval_env[["dt"]] <- vs[["dt"]]
  # @codedoc_comment_block vame::var_set_value_space_dt_subset::expr
  # **`expr`** `[name, call integer, logical]` (no default)
  #
  # Expression to subset a `data.table` object. Available columns depends on
  # context. Must be or evaluate to `integer` or `logical`.
  # @codedoc_comment_block vame::var_set_value_space_dt_subset::expr
  expr <- substitute(expr, env = parent.frame(1L))
  dt_subset <- expr
  n <- 0L
  # @codedoc_comment_block vm@var_set_value_space_dt_subset
  # - Call `expr <- eval(expr, envir = eval_env[["dt"]], enclos = eval_env)`
  #   up to 10 times until result is not `name` or `call` object.
  # @codedoc_comment_block vm@var_set_value_space_dt_subset
  while (n <= 10 && (is.name(dt_subset) || is.call(dt_subset))) {
    n <- n + 1L
    dt_subset <- eval(
      expr = dt_subset,
      envir = eval_env[["dt"]],
      enclos = eval_env
    )
  }
  # @codedoc_comment_block vm@var_set_value_space_dt_subset
  # - Raise an error if result of `expr` is not `logical` or `integer`.
  # @codedoc_comment_block vm@var_set_value_space_dt_subset
  if (!inherits(dt_subset, c("logical", "integer"))) {
    stop("Result of `expr` was not `logical` nor `integer` vector after ",
         n, " evaluations.")
  }
  # @codedoc_comment_block vm@var_set_value_space_dt_subset
  # - Use result to subset `eval_env[["dt"]]`.
  # - Set result as the new `value_space`.
  # @codedoc_comment_block vm@var_set_value_space_dt_subset
  var_set_value_space_set(vm, id, list(dt = eval_env[["dt"]][dt_subset, ]))
  return(invisible(NULL))
}

var_set_value_space_sampler_get <- function(
  vm,
  id
) {
  # @codedoc_comment_block vm@var_set_value_space_sampler_get
  # Retrieve value space sampler for given `id` from `var_set_dt$sampler`.
  # @codedoc_comment_block vm@var_set_value_space_sampler_get
  # @codedoc_comment_block feature_funs(random sampling)
  # - `vm@var_set_value_space_sampler_get`
  # @codedoc_comment_block feature_funs(random sampling)
  # @codedoc_comment_block news("vm@var_set_value_space_sampler_get", "2023-12-12", "0.2.2")
  # New slot function `var_set_value_space_sampler_get`.
  # @codedoc_comment_block news("vm@var_set_value_space_sampler_get", "2023-12-12", "0.2.2")
  var_set_meta_get(vm, id = id, meta_nm = "sampler")
}

var_set_value_space_sampler_set <- function(
  vm,
  id,
  value
) {
  # @codedoc_comment_block vm@var_set_value_space_sampler_set
  # Assign value space sampler for given `id` in `var_set_dt$sampler`.
  #
  # @codedoc_insert_comment_block specification(var_set_dt$sampler)
  #
  # See the documentation for `var_set_value_space_sample` to understand how
  # the `sampler` object is used.
  # @codedoc_comment_block vm@var_set_value_space_sampler_set
  # @codedoc_comment_block feature_funs(random sampling)
  # - `vm@var_set_value_space_sampler_set`
  # @codedoc_comment_block feature_funs(random sampling)
  # @codedoc_comment_block news("vm@var_set_value_space_sampler_set", "2023-12-12", "0.2.2")
  # New slot function `var_set_value_space_sampler_set`.
  # @codedoc_comment_block news("vm@var_set_value_space_sampler_set", "2023-12-12", "0.2.2")
  var_set_meta_set(
    vm = vm,
    id = id,
    meta_nm = "sampler",
    value = value
  )
}

var_set_value_space_sample <- function(
  vm,
  id = NULL,
  var_nms = NULL,
  n = 1L,
  data = NULL,
  env = NULL
) {
  # @codedoc_comment_block vm@var_set_value_space_sample
  # Take `n` random samples from the value space of `id`.
  #
  # @codedoc_insert_comment_block feature_process(random sampling)
  # @codedoc_comment_block vm@var_set_value_space_sample

  # @codedoc_comment_block function_example(vm@var_set_value_space_sample)
  # c_lo <- as.Date("2001-01-01")
  # c_hi <- as.Date("2020-12-31")
  # ## see what default samplers do with bounds
  # storage.mode(c_lo) <- storage.mode(c_hi) <- "integer"
  # vm <- vame::VariableMetadata(
  #   var_dt = data.table::data.table(
  #     var_nm = c("a", "d", "b", "c"),
  #     type = c("categorical", "categorical", "categorical", "my_type")
  #   ),
  #   var_set_dt = data.table::data.table(
  #     id = c("ad", "b", "c", "e"),
  #     var_nm_set = list(ad = c("a", "d"), b = "b", c = "c", e = "e"),
  #     value_space = list(
  #       ad = list(dt = data.table::CJ(a = 1:2, d = 3:4)),
  #       b = list(set = 3:4),
  #       c = list(bounds = list(
  #         lo = c_lo,
  #         hi = c_hi,
  #         lo_inclusive = TRUE,
  #         hi_inclusive = TRUE
  #       )),
  #       e = list(set = 1:2)
  #     ),
  #     sampler = list(
  #       ad = NULL,
  #       b = quote({
  #         p <- c(0.25, 0.75)
  #         out <- sample(vs[["set"]], size = n, replace = TRUE, p = p)
  #         out <- data.table::setDT(list(b = out))
  #         return(out[])
  #       }),
  #       c = NULL,
  #       e = quote({
  #         stopifnot(
  #           inherits(data, "list"),
  #           "e_cond" %in% names(data),
  #           length(data[["e_cond"]]) == n
  #         )
  #         dist <- data.table::data.table(
  #           e_cond = c(1L, 1L, 2L, 2L),
  #           e = c(1:2, 1:2),
  #           p_cond = c(0.25, 0.75, 0.40, 0.60)
  #         )
  #         join_dt <- data.table::setDT(list(e_cond = data[["e_cond"]]))
  #         out <- dist[
  #           i = join_dt,
  #           j = list(
  #             e = sample(.SD[["e"]], replace = TRUE, prob = .SD[["p_cond"]])
  #           ),
  #           by = .EACHI
  #         ]
  #         data.table::set(out, j = setdiff(names(out), "e"), value = NULL)
  #         return(out[])
  #       })
  #     )
  #   )
  # )
  # vm@var_set_value_space_sampler_set(
  #   id = "c",
  #   value = quote({
  #     pool <- vs[["bounds"]][["lo"]]:vs[["bounds"]][["hi"]]
  #     p <- dnorm(x = pool, mean = mean(pool), sd = 10)
  #     class(pool) <- class(vs[["bounds"]][["lo"]])
  #     out <- pool[sample(length(pool), size = n, replace = FALSE, prob = p)]
  #     out <- data.table::setDT(list(c = out))
  #     return(out[])
  #   })
  # )
  #
  # ad_sample <- vm@var_set_value_space_sample(id = "ad", n = 4L)
  # b_sample <- vm@var_set_value_space_sample(id = "b", n = 4L)
  # c_sample <- vm@var_set_value_space_sample(id = "c", n = 4L)
  # stopifnot(
  #   inherits(ad_sample, "data.table"),
  #   nrow(ad_sample) == 4,
  #   inherits(b_sample, "data.table"),
  #   nrow(b_sample) == 4,
  #   inherits(c_sample, "data.table"),
  #   storage.mode(c_sample$c) == "integer",
  #   nrow(c_sample) == 4
  # )
  # @codedoc_comment_block function_example(vm@var_set_value_space_sample)

  # @codedoc_comment_block feature_example(random sampling)
  # # random sampling
  # @codedoc_insert_comment_block function_example(vm@var_set_value_space_sample)
  #
  # vame_sample <- vm@vame_value_space_sample(
  #   ids = c("ad", "b", "c"),
  #   n = 10L
  # )
  # stopifnot(
  #   inherits(vame_sample, "data.table"),
  #   identical(c("a", "d", "b", "c"), names(vame_sample)),
  #   nrow(vame_sample) == 10
  # )
  #
  # # conditional sampling
  # vm@var_set_value_space_sampler_set(
  #   id = "c",
  #   value = list(
  #     dep_var_nm_set = "a",
  #     sampler = quote({
  #       a <- data[["a"]]
  #       out <- data.table::data.table(c = runif(n = n, min = a - 1, max = a))
  #       return(out[])
  #     })
  #   )
  # )
  # c_sample_data <- list(a = 1:2)
  # c_sample <- vm@var_set_value_space_sample(
  #   id = "c",
  #   data = c_sample_data,
  #   n = 2L
  # )
  # vame_sample <- vm@vame_value_space_sample(
  #   ids = c("ad", "b", "c"),
  #   n = 10L
  # )
  # stopifnot(
  #   inherits(c_sample, "data.table"),
  #   identical("c", names(c_sample)),
  #   nrow(c_sample) == 2,
  #   c_sample[["c"]] < c_sample_data[["a"]],
  #   inherits(vame_sample, "data.table"),
  #   identical(c("a", "d", "b", "c"), names(vame_sample)),
  #   nrow(vame_sample) == 10,
  #   vame_sample[["c"]] < vame_sample[["a"]]
  # )
  #
  # # You may sometimes want to write the `sampler` and `value_space` in
  # # separate variable sets for the same variable.
  # my_vm <- vame::VariableMetadata(
  #   var_dt = data.table::data.table(var_nm = c("a", "b")),
  #   var_set_dt = data.table::data.table(
  #     id = c("a_sampler", "ab_value_space"),
  #     var_nm_set = list("a", c("a", "b")),
  #     value_space = list(
  #       NULL,
  #       list(
  #         dt = data.table::data.table(a = 0:5)[
  #           j = list(b = .SD[["a"]]:5L),
  #           .SDcols = "a",
  #           keyby = "a"
  #         ][]
  #       )
  #     ),
  #     sampler = list(
  #       list(
  #         dep_var_nm_set = "b",
  #         sampler = quote({
  #           pdt <- data.table::data.table(
  #             a = 0:5,
  #             p = 0:5 + 1
  #           )
  #           n # must be used in expression, in this case not useful
  #           out <- vapply(
  #             data[["b"]],
  #             function(b_value) {
  #               pdt[
  #                 i = pdt[["a"]] <= b_value,
  #                 j = sample(.SD[["a"]], size = 1L, prob = .SD[["p"]]),
  #                 .SDcols = c("a", "p")
  #               ]
  #             },
  #             integer(1L)
  #           )
  #           out <- data.table::setDT(list(a = out))
  #           return(out[])
  #         })
  #       ),
  #       NULL
  #     )
  #   )
  # )
  # # `sampler` is used, not a default sampler on `value_space`.
  # RNGversion("4.0.0")
  # set.seed(1337)
  # a_sample <- my_vm@var_set_value_space_sample(
  #   var_nms = "a",
  #   n = 1e6L,
  #   data = data.table::data.table(b = rep(5L, 1e6))
  # )
  # a_sample_distribution <- table(a_sample[["a"]])
  # stopifnot(
  #   a_sample_distribution[1] < a_sample_distribution[6]
  # )
  # @codedoc_comment_block feature_example(random sampling)

  # @codedoc_comment_block news("vm@var_set_value_space_sample", "2023-12-11", "0.2.2")
  # New slot function `var_set_value_space_sample`.
  # @codedoc_comment_block news("vm@var_set_value_space_sample", "2023-12-11", "0.2.2")

  # @codedoc_comment_block feature_funs(random sampling)
  # - `vm@var_set_value_space_sample`
  # @codedoc_comment_block feature_funs(random sampling)

  # @codedoc_comment_block feature(random sampling)
  # The random sampling feature is available when the value spaces feature
  # is available.
  #
  # @codedoc_insert_comment_block feature_process(random sampling)
  #
  # The following functions are related to this feature:
  # @codedoc_insert_comment_block feature_funs(random sampling)
  # @codedoc_comment_block feature(random sampling)

  # @codedoc_comment_block news("vm@var_set_value_space_sample", "2024-07-01", "0.5.3")
  # `vm@var_set_value_space_sample` arg `id` now `NULL` by default.
  # You can supply either `id` or `var_nms` and the other one is inferred,
  # if `NULL`.
  # @codedoc_comment_block news("vm@var_set_value_space_sample", "2024-07-01", "0.5.3")
  # @codedoc_comment_block news("vm@var_set_value_space_sample", "2024-10-14", "1.3.1")
  # `vm@var_set_value_space_sample` arg `id` auto-inference now only finds
  # one `id` which has either `sampler` or `value_space`.
  # @codedoc_comment_block news("vm@var_set_value_space_sample", "2024-10-14", "1.3.1")
  handle_arg_ids_et_var_nms_inplace__(
    required_meta_nms = c("sampler", "value_space"),
    require_meta_style = "or",
    ids_arg_nm = "id",
    var_nms_arg_nm = "var_nms",
    vm = vm,
    n_max_ids = 1L
  )
  dbc::assert_has_one_of_classes(
    env,
    classes = c("NULL", "environment")
  )
  if (is.null(env)) {
    env <- parent.frame(1L)
  }
  # @codedoc_comment_block doc_slot_fun_arg(n)
  # **`n`** `[integer]` (default `1L`)
  #
  # Number of random samples to take.
  # @codedoc_comment_block doc_slot_fun_arg(n)
  dbc::assert_is_integer_nonNA_gtzero_atom(n)

  # @codedoc_comment_block feature_process(random sampling)
  # Random sampling is performed as follows:
  #
  # - Object `vs` is created by calling `vm@var_set_value_space_eval`,
  #   if `value_space` is defined for the `id`. Else `vs` will be `NULL`.
  # @codedoc_comment_block feature_process(random sampling)
  vs <- NULL
  if (var_set_meta_is_defined(vm = vm, id = id, meta_nm = "value_space")) {
    vs <- var_set_value_space_eval(
      vm = vm,
      id = id,
      env = env,
      var_nms = var_nms
    )
  }

  # @codedoc_comment_block feature_process(random sampling)
  # - The appropriate `sampler` is retrieved --- this is either the
  #   corresponding
  #   sampler stored in `var_set_dt$sampler`, or if no sampler exists there,
  #   one of the defaults depending on the type of the value space:
  # @codedoc_insert_comment_block defaults(var_set_dt$sampler)
  # @codedoc_comment_block feature_process(random sampling)
  if (var_set_meta_is_defined(vm = vm, id = id, meta_nm = "sampler")) {
    sampler <- var_set_value_space_sampler_get(vm, id = id)
    assert_is_value_space_sampler(sampler, assertion_type = "general")
  } else if (is.null(vs)) {
    stop("No `sampler` nor `value_space` defined for `id = ", deparse1(id),
         "`. At least one of them must be defined for sampling.")
  } else {
    sampler <- get_value_space_type_fun__(
      value_space_type = names(vs),
      fun_nm = "sampler"
    )
  }
  # @codedoc_comment_block news("vm@var_set_value_space_sample", "2024-02-23", "0.4.0")
  # `vm@var_set_value_space_sample` now has new arg `data`. Pass your data via
  # `data` when you have a conditional sampling method.
  # @codedoc_comment_block news("vm@var_set_value_space_sample", "2024-02-23", "0.4.0")
  # @codedoc_comment_block news("vm@var_set_value_space_sample", "2024-09-16", "1.1.0")
  # `vm@var_set_value_space_sample` object `x` passed to the `sampler` renamed
  # to `vm`. It is the `VariableMetadata` object itself.
  # @codedoc_comment_block news("vm@var_set_value_space_sample", "2024-09-16", "1.1.0")
  # @codedoc_comment_block feature_process(random sampling)
  # - The following variables are collected for use by the `sampler`:
  #   * `n`: The argument
  #   * `vm`: The `VariableMetadata` object itself
  #   * `vs`: The evaluated `value_space` object (`NULL` if none found)
  #   * `data`: The argument after handling --- useful for conditional sampling
  #   * `id`: The argument after handling
  #   * `var_nms`: The argument after handling
  # @codedoc_comment_block feature_process(random sampling)
  arg_list <- list(
    n = n,
    vm = vm,
    vs = vs,
    data = handle_arg_data__(data),
    id = id,
    var_nms = var_nms
  )
  if (inherits(sampler, "list")) {
    miss_dep_var_nm_set <- setdiff(sampler[["dep_var_nm_set"]], names(data))
    if (length(miss_dep_var_nm_set) > 0) {
      stop("`sampler` requires variables ",
           deparse1(sampler[["dep_var_nm_set"]]),
           " but these were not included via `data`: ",
           deparse1(miss_dep_var_nm_set))
    }
    # @codedoc_comment_block feature_process(random sampling)
    #   * `dep_var_nm_set`: only added if `sampler` is a `list` object and has
    #     `sampler[["dep_var_nm_set"]]` --- this is useful for conditional
    #     sampling
    # @codedoc_comment_block feature_process(random sampling)
    arg_list[["dep_var_nm_set"]] <- sampler[["dep_var_nm_set"]]
    sampler <- sampler[["sampler"]]
  }
  if (is.function(sampler)) {
    # @codedoc_comment_block feature_process(random sampling)
    # - If `sampler` is a function, it is called with those objects in the
    #   above variable list which are also named arguments of the function.
    # @codedoc_comment_block feature_process(random sampling)
    arg_list <- arg_list[intersect(names(formals(sampler)), names(arg_list))]
    sample <- do.call(sampler, arg_list)
  } else if (is.call(sampler)) {
    # @codedoc_comment_block feature_process(random sampling)
    # - If `sampler` is an expression, it is evaluated in a temporary evaluation
    #   environment which has been populated with the variables of the above
    #   list.
    # @codedoc_comment_block feature_process(random sampling)
    call_eval_env <- as.environment(arg_list)
    parent.env(call_eval_env) <- env
    sample <- eval(sampler, envir = call_eval_env)
  } else {
    stop("Internal error: no handling defined for sampler with class vector ",
         deparse1(class(sampler)), "; if your sampler object can be set with ",
         "vm@var_set_value_space_sampler_set, this function is broken ",
         "and you should contact the maintainer ",
         as.character(utils::maintainer(pkg = "vame")))
  }

  # @codedoc_comment_block feature_process(random sampling)
  # - The output of sampling is always a `data.table` with `n` rows.
  # @codedoc_comment_block feature_process(random sampling)
  dbc::assert_prod_output_is_data_table(sample)
  dbc::assert_prod_output_is_identical(
    x = nrow(sample),
    y = n
  )
  return(sample)
}

var_is_aggregateable_to <- function(
  vm,
  from_var_nm,
  to_var_nm
) {
  # @codedoc_comment_block vm@var_is_aggregateable_to
  # Returns `TRUE` if `from_var_nm` can be aggregated into `to_var_nm`.
  # @codedoc_comment_block vm@var_is_aggregateable_to
  # @codedoc_comment_block function_example(vm@var_is_aggregateable_to)
  # vm <- vame::VariableMetadata(
  #   var_dt = data.table::data.table(
  #     var_nm = c("a1", "a2")
  #   ),
  #   var_set_dt = data.table::data.table(
  #     id = "a1_&_a2",
  #     var_nm_set = list(c("a1", "a2")),
  #     value_space = list("a1_&_a2" = list(dt = data.table::data.table(
  #       a1 = c(1L, 1L, 2L, 2L),
  #       a2 = c(11L, 12L, 21L, 22L)
  #     )))
  #   )
  # )
  # stopifnot(
  #   vm@var_is_aggregateable_to(from_var_nm = "a2", to_var_nm = "a1"),
  #   !vm@var_is_aggregateable_to(from_var_nm = "a1", to_var_nm = "a2")
  # )
  # @codedoc_comment_block function_example(vm@var_is_aggregateable_to)
  # @codedoc_comment_block feature_funs(category spaces)
  # - `vm@var_is_aggregateable_to`
  # @codedoc_comment_block feature_funs(category spaces)

  # @codedoc_comment_block news("vm@var_is_aggregateable_to", "2023-07-10", "0.1.3")
  # New slot `var_is_aggregateable_to`.
  # @codedoc_comment_block news("vm@var_is_aggregateable_to", "2023-07-10", "0.1.3")

  # @codedoc_comment_block doc_slot_fun_arg(from_var_nm)
  # **`from_var_nm`** `[character]` (no default)
  #
  # Name of a variable. Aggregation from this to another variable.
  # @codedoc_comment_block doc_slot_fun_arg(from_var_nm)
  assert_is_var_nm(vm, from_var_nm)
  # @codedoc_comment_block doc_slot_fun_arg(to_var_nm)
  # **`to_var_nm`** `[character]` (no default)
  #
  # Name of a variable. Aggregation to this from another variable.
  # @codedoc_comment_block doc_slot_fun_arg(to_var_nm)
  assert_is_var_nm(vm, to_var_nm)
  var_is_aggregateable_to__(
    vm,
    from_var_nm = from_var_nm,
    to_var_nm = to_var_nm,
    dt = vame_category_space_dt(vm, c(from_var_nm, to_var_nm))
  )
}


var_aggregate <- function(
  vm,
  x,
  from_var_nm,
  to_var_nm
) {
  # @codedoc_comment_block feature_funs(category spaces)
  # - `vm@var_aggregate`
  # @codedoc_comment_block feature_funs(category spaces)

  # @codedoc_comment_block news("vm@var_aggregate", "2023-07-10", "0.1.3")
  # New slot `var_aggregate`.
  # @codedoc_comment_block news("vm@var_aggregate", "2023-07-10", "0.1.3")
  assert_is_var_nm(vm, from_var_nm)
  assert_is_var_nm(vm, to_var_nm)
  # @codedoc_comment_block doc_slot_fun_arg(x)
  # **`x`** `[any]` (no default)
  #
  # Values of a specified variable.
  # @codedoc_comment_block doc_slot_fun_arg(x)
  dbc::assert_is_vector(x)

  # @codedoc_comment_block news("vm@var_aggregate", "2025-04-24", "1.9.4")
  # `vm@var_aggregate` optimised to simply return `x` if
  # `from_var_nm == to_var_nm`.
  # @codedoc_comment_block news("vm@var_aggregate", "2025-04-24", "1.9.4")
  # @codedoc_comment_block vm@var_aggregate
  # Returns correspoding level of `to_var_nm` for each value in `x`.
  # Performs the following steps:
  #
  # - If `from_var_nm == to_var_nm`, simply return `x` early.
  # @codedoc_comment_block vm@var_aggregate
  if (from_var_nm == to_var_nm) {
    return(x)
  }
  # @codedoc_comment_block vm@var_aggregate
  # - Call `vm@vame_category_space_dt` to get a joint `data.table` with both
  #   `from_var_nm` and `to_var_nm`.
  # @codedoc_comment_block vm@var_aggregate
  dt <- vame_category_space_dt(vm, c(from_var_nm, to_var_nm))
  # @codedoc_comment_block vm@var_aggregate
  # - Check if `from_var_nm` is aggregateable to `to_var_nm`. Raise an
  #   informative error if it isn't.
  # @codedoc_insert_comment_block var_is_aggregateable_to__
  # @codedoc_comment_block vm@var_aggregate
  is_aggregateable <- var_is_aggregateable_to__(
    vm,
    from_var_nm = from_var_nm,
    to_var_nm = to_var_nm,
    dt = dt
  )
  if (!is_aggregateable) {
    stop(
      "cannot aggregate ", from_var_nm, " to ", to_var_nm, "; ",
      "aggregation only possible when there is exactly one ",
      "level of the target variable for each level of the starting ",
      "variable. if e.g. ", from_var_nm, " = 1 can be either ",
      to_var_nm, " = 1 or 2, cannot aggregate."
    )
  }
  # @codedoc_comment_block vm@var_aggregate
  # - Check that `x` and the `value_space` for `from_var_nm` have the same
  #   exact `class`. Raise an informative error if that is not the case.
  # @codedoc_comment_block vm@var_aggregate
  jdt <- data.table::setDT(list(x = x))
  data.table::setnames(jdt, "x", from_var_nm)
  if (!identical(class(x), class(dt[[from_var_nm]]))) {
    # @codedoc_comment_block news("vm@var_aggregate", "2025-04-24", "1.9.4")
    # `vm@var_aggregate` now checks that `x` and the `value_space` of
    # `from_var_nm` have identical class vectors and raises an informative
    # error if that is not the case.
    # @codedoc_comment_block news("vm@var_aggregate", "2025-04-24", "1.9.4")
    stop(
      "`x` and `value_space` for `from_var_nm = \"", from_var_nm, "\"` ",
      "do not match. Join between `x` and its `value_space` not possible. ",
      "`class(x) = ", deparse1(class(x)), "` vs. ",
      "`class(vs_of_from_var_nm) = ", deparse1(dt[[from_var_nm]]), "`."
    )
  }
  # @codedoc_comment_block vm@var_aggregate
  # - Simply do a join on the joint `data.table` using `x` to return values of
  #   `to_var_nm` for each value of `x`.
  # @codedoc_comment_block vm@var_aggregate
  out <- dt[
    i = jdt,
    on = from_var_nm,
    #' @importFrom data.table .SD
    j = .SD[[1L]],
    .SDcols = to_var_nm
  ]
  return(out[])
}


var_value_space_eval <- function(
  vm,
  var_nm,
  env = NULL
) {
  # @codedoc_comment_block feature_funs(value spaces)
  # - `vm@var_value_space_eval`
  # @codedoc_comment_block feature_funs(value spaces)

  # @codedoc_comment_block news("vm@var_value_space_eval", "2023-07-03", "0.1.1")
  # New slot `vm@var_value_space_eval`.
  # @codedoc_comment_block news("vm@var_value_space_eval", "2023-07-03", "0.1.1")

  # @codedoc_comment_block doc_slot_fun_arg(var_nm)
  # **`var_nm`** `[character]` (no default)
  #
  # Name of a variable.
  # @codedoc_comment_block doc_slot_fun_arg(var_nm)
  assert_is_var_nm(vm, var_nm)
  dbc::assert_is_one_of(
    env,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_environment)
  )
  if (is.null(env)) {
    env <- parent.frame(1L)
  }
  # @codedoc_comment_block vm@var_value_space_eval
  # Get and evaluate value space for a variable.
  # This gets tricky when a variable has been defined in more than one
  # `var_set_dt$value_space`. If there is a single `var_set_dt$value_space`
  # for only the variable of interest, that is used. Otherwise:
  # @codedoc_comment_block vm@var_value_space_eval
  pos_set <- var_meta_get(vm, var_nm = var_nm, meta_nm = "var_set_dt_pos_set")
  vsd <- vsd_get(vm, var_nms = c("id", "var_nm_set", "value_space"))
  pos_set <- pos_set[
    !vapply(vsd[["value_space"]][pos_set], is.null, logical(1L))
  ]
  vs_lens <- vapply(vsd[["var_nm_set"]], length, integer(1L))
  is_singular <- vs_lens == 1
  if (sum(is_singular) == 1) {
    pos_set <- pos_set[is_singular]
  }

  # @codedoc_comment_block vm@var_value_space_eval
  # - Every value space for the variable is evaluated via
  #   `vm@var_set_value_space_eval`. If the exact same value space has been
  #   produced multiple times, duplicates are removed.
  # @codedoc_comment_block vm@var_value_space_eval
  value_space <- lapply(vsd[["id"]][pos_set], function(id) {
    var_set_value_space_eval(
      vm = vm,
      id = id,
      var_nms = var_nm,
      env = env
    )
  })
  value_space <- unique(value_space)
  # @codedoc_comment_block vm@var_value_space_eval
  # - If we now have only one value space, all is well and that is returned.
  # @codedoc_comment_block vm@var_value_space_eval
  if (length(value_space) == 1) {
    value_space <- value_space[[1]]
  } else if (var_meta_get(vm, var_nm, "type") == "categorical") {
    # @codedoc_comment_block vm@var_value_space_eval
    # - If the `var_dt$type == "categorical"` for this variable, the union of
    #   all separate value spaces is formed and that will be returned.
    # @codedoc_comment_block vm@var_value_space_eval
    value_space <- lapply(value_space, function(x) {
      if ("set" %in% names(x)) {
        x <- list(dt = data.table::data.table(x = x[["set"]]))
        data.table::setnames(x[["dt"]], "x", var_nm)
      }
      if ("dt" %in% names(x)) {
        x <- x[["dt"]]
      }
      x
    })
    value_space <- data.table::rbindlist(value_space)
    value_space <- list(dt = unique(value_space, by = var_nm))
  } else {
    # @codedoc_comment_block vm@var_value_space_eval
    # - In other cases an error is raised because there does not seem to be
    #   any way to decide which value space to use.
    # @codedoc_comment_block vm@var_value_space_eval
    stop(
      "Internal error: var_nm = \"", var_nm, "\" appears in more than ",
      "one variable set value space, and its value spaces are different ",
      "in the different sets --- don't know which one to take."
    )
  }
  if ("dt" %in% names(value_space)) {
    dt_subset <- !duplicated(value_space[["dt"]], by = var_nm)
    value_space[["dt"]] <- value_space[["dt"]][
      i = dt_subset,
      j = .SD,
      .SDcols = var_nm
    ]
  }
  return(value_space)
}


var_assert <- function(
  vm,
  x,
  var_nm,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL,
  env = NULL
) {
  # @codedoc_comment_block vm@var_assert
  # Assert that values in `x` are proper values of `var_nm`.
  # @codedoc_comment_block vm@var_assert
  # @codedoc_comment_block feature_funs(assertions)
  # - `vm@var_assert`
  # @codedoc_comment_block feature_funs(assertions)

  # @codedoc_comment_block news("vm@var_assert", "2023-07-03", "0.1.1")
  # Fixed `var_assert` handling of a value space based on `bounds`.
  # @codedoc_comment_block news("vm@var_assert", "2023-07-03", "0.1.1")
  # @codedoc_comment_block news("vm@var_assert", "2023-07-04", "0.1.2")
  # Added arguments `x_nm`, `call`.
  # @codedoc_comment_block news("vm@var_assert", "2023-07-04", "0.1.2")

  # @codedoc_comment_block feature(assertions)
  # The assertions feature allows you to check that variables look like what
  # you expect. This feature relies on the value spaces feature --- see that
  # for more information.
  #
  # The following functions are related to this feature:
  # @codedoc_insert_comment_block feature_funs(assertions)
  # @codedoc_comment_block feature(assertions)

  # @codedoc_comment_block doc_slot_fun_arg(x_nm)
  # **`x_nm`** `[NULL, character]` (default `NULL`)
  #
  # See [dbc::handle_arg_x_nm].
  # @codedoc_comment_block doc_slot_fun_arg(x_nm)
  x_nm <- dbc::handle_arg_x_nm(x_nm)
  # @codedoc_comment_block doc_slot_fun_arg(call)
  # **`call`** `[NULL, language]` (default `NULL`)
  #
  # See [dbc::handle_arg_call].
  # @codedoc_comment_block doc_slot_fun_arg(call)
  call <- dbc::handle_arg_call(call)
  # @codedoc_comment_block doc_slot_fun_arg(assertion_type)
  # **`assertion_type`** `[NULL, character]` (default `NULL`)
  #
  # See [dbc::handle_arg_assertion_type].
  # @codedoc_comment_block doc_slot_fun_arg(assertion_type)
  assertion_type <- dbc::handle_arg_assertion_type(assertion_type)

  # @codedoc_comment_block news("vm@var_assert", "2023-07-11", "0.1.3")
  # `vm@var_assert` gains arg `env`. This is passed
  # to `vm@var_value_space_eval`.
  # @codedoc_comment_block news("vm@var_assert", "2023-07-11", "0.1.3")
  dbc::assert_is_one_of(
    env,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_environment)
  )
  if (is.null(env)) {
    env <- parent.frame(1L)
  }

  vs <- var_value_space_eval(vm, var_nm, env = env)
  dbc::assert_prod_interim_is_list(
    vs,
    call = call
  )
  dbc::assert_prod_interim_has_length(
    vs,
    expected_length = 1L,
    call = call
  )
  get_var_value_assertion_fun__(names(vs))(
    x = x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type,
    var_nm = var_nm,
    value_space = vs
  )
}


# var funs ---------------------------------------------------------------------
var_dt_print <- function(vm, ...) {
  # @codedoc_comment_block vm@var_dt_print
  # Print `var_dt`.
  # @codedoc_comment_block vm@var_dt_print
  # @codedoc_comment_block news("vm@var_dt_print", "2025-06-26", "1.10.4")
  # New function `vm@var_dt_print`.
  # @codedoc_comment_block news("vm@var_dt_print", "2025-06-26", "1.10.4")
  # @codedoc_comment_block vame::var_dt_print::...
  # **`...`**
  #
  # Arguments passed to `print.data.table`.
  # @codedoc_comment_block vame::var_dt_print::...

  print(vd_get(vm), ...)
}

var_dt_copy <- function(vm) {
  # @codedoc_comment_block vm@var_dt_copy
  # Returns a deep copy of `var_dt`.
  # @codedoc_comment_block vm@var_dt_copy
  # @codedoc_comment_block news("vm@var_dt_copy", "2023-12-13", "0.2.2")
  # New function `vm@var_dt_copy`.
  # @codedoc_comment_block news("vm@var_dt_copy", "2023-12-13", "0.2.2")
  # @codedoc_comment_block news("vm@var_dt_copy", "2023-12-13", "0.4.1")
  # `vm@var_dt_copy` now actually returns `var_dt` instead of `var_set_dt`.
  # @codedoc_comment_block news("vm@var_dt_copy", "2023-12-13", "0.4.1")
  data.table::copy(data_obj_get(vm, "var_dt"))
}

var_meta_is_defined <- function(
  vm,
  var_nm,
  meta_nm
) {
  # @codedoc_comment_block function_example(vm@var_meta_is_defined)
  # vm <- vame::VariableMetadata(
  #   var_dt = data.table::data.table(
  #     var_nm = c("a", "b"),
  #     my_int_meta = c(1L, NA),
  #     my_list_meta = list(NULL, 1:3)
  #   ),
  #   var_set_dt = data.table::data.table(
  #     id = c("set_a", "set_b"),
  #     var_nm_set = list("a", "b")
  #   )
  # )
  # stopifnot(
  #   identical(vm@var_meta_is_defined(var_nm = NULL, meta_nm = "my_int_meta"), TRUE),
  #   identical(vm@var_meta_is_defined(var_nm = NULL, meta_nm = "x"), FALSE),
  #   identical(vm@var_meta_is_defined(var_nm = "a", meta_nm = "my_int_meta"), TRUE),
  #   identical(vm@var_meta_is_defined(var_nm = "b", meta_nm = "my_int_meta"), FALSE),
  #   identical(vm@var_meta_is_defined(var_nm = "a", meta_nm = "my_list_meta"), FALSE),
  #   identical(vm@var_meta_is_defined(var_nm = "b", meta_nm = "my_list_meta"), TRUE)
  # )
  # @codedoc_comment_block function_example(vm@var_meta_is_defined)

  # @codedoc_comment_block news("vm@var_meta_is_defined", "2023-12-12", "0.2.2")
  # New function `vm@var_meta_is_defined`.
  # @codedoc_comment_block news("vm@var_meta_is_defined", "2023-12-12", "0.2.2")
  # @codedoc_comment_block news("vm@var_meta_is_defined", "2024-01-24", "0.4.0")
  # `vm@var_meta_is_defined` internal problem fixed.
  # @codedoc_comment_block news("vm@var_meta_is_defined", "2024-01-24", "0.4.0")
  # @codedoc_comment_block news("vm@var_meta_is_defined", "2024-12-19", "1.5.0")
  # `vm@var_meta_is_defined` now also allows `var_nm = NULL`. Then it returns
  # `TRUE/FALSE` depending on whether `meta_nm` is a column name of `var_dt`.
  # @codedoc_comment_block news("vm@var_meta_is_defined", "2024-12-19", "1.5.0")
  # @codedoc_comment_block vm@var_meta_is_defined
  # Returns `TRUE`/ `FALSE` to indicate whether a metadatum is considered to be
  # defined.
  #
  # - Detects whether `meta_nm` is a column name of `var_dt`. If not or if
  #   `is.null(var_nm)`, return result early.
  # @codedoc_comment_block vm@var_meta_is_defined
  vd <- vd_get(vm)
  out <- meta_nm %in% names(vd)
  if (is.null(var_nm) || isFALSE(out)) {
    return(out)
  }
  # @codedoc_comment_block vm@var_meta_is_defined
  # - If `var_dt[[meta_nm]]` is a `list`, detects whether the element for
  #   `var_nm` is `NULL` or not. For other column types detects whether the
  #   element is `NA` or not.
  # @codedoc_comment_block vm@var_meta_is_defined
  assert_is_var_nm(vm, var_nm)
  pos <- data.table::chmatch(var_nm, vd[["var_nm"]])
  if (is.list(vd[[meta_nm]])) {
    out <- !is.null(vd[[meta_nm]][[pos]])
  } else {
    out <- !is.na(vd[[meta_nm]][pos])
  }
  return(out)
}

var_meta_get <- function(
  vm,
  var_nm,
  meta_nm
) {
  # @codedoc_comment_block vm@var_meta_get
  # Get metadata for a variable.
  # @codedoc_comment_block vm@var_meta_get
  assert_is_var_nm(vm, var_nm)
  assert_is_var_meta_nm(vm, meta_nm)
  vd <- vd_get(vm)
  pos <- data.table::chmatch(var_nm, vd[["var_nm"]])
  return(vd[[meta_nm]][[pos]])
}

var_meta_set <- function(
  vm,
  var_nm,
  meta_nm,
  value
) {
  # @codedoc_comment_block vm@var_meta_set
  # Set metadata for a variable.
  # @codedoc_comment_block vm@var_meta_set
  assert_is_var_nm(vm, var_nm, must_exist = TRUE)
  # @codedoc_comment_block news("vm@var_meta_set", "2024-09-02", "0.5.6")
  # `vm@var_meta_set` now checks `value` for validity for "officially" defined
  # metadata such as `describer`. Formerly this was done only by
  # the corresponding wrapper such as `vm@var_describer_set`.
  # @codedoc_comment_block news("vm@var_meta_set", "2024-09-02", "0.5.6")
  assert_meta(vm = vm, x = value, meta_nm = meta_nm, must_exist = FALSE)

  # @codedoc_comment_block function_example(vm@var_meta_set)
  # # vm@var_meta_set
  # vm <- vame::VariableMetadata(
  #   var_dt = data.table::data.table(
  #     var_nm = "a"
  #   ),
  #   var_set_dt = data.table::data.table(
  #     id = "A",
  #     var_nm_set = list("a")
  #   )
  # )
  # a_type <- "this is type for a"
  # vm@var_meta_set(
  #   var_nm = "a",
  #   meta_nm = "type",
  #   value = a_type
  # )
  # a_describer <- list(descr = "this is description for a")
  # vm@var_meta_set(
  #   var_nm = "a",
  #   meta_nm = "describer",
  #   value = a_describer
  # )
  # stopifnot(
  #   identical(
  #     vm@var_meta_get(var_nm = "a", meta_nm = "type"),
  #     a_type
  #   ),
  #   identical(
  #     vm@var_meta_get(var_nm = "a", meta_nm = "describer"),
  #     a_describer
  #   )
  # )
  # @codedoc_comment_block function_example(vm@var_meta_set)

  # @codedoc_comment_block news("vm@var_meta_set", "2023-12-12", "0.2.2")
  # `vm@var_meta_set` now wraps `value` into a list if it isn't a list
  # and if the target column is a list.
  # @codedoc_comment_block news("vm@var_meta_set", "2023-12-12", "0.2.2")
  # @codedoc_comment_block news("vm@var_meta_set", "2024-09-04", "0.5.7")
  # `vm@var_meta_set` now always wraps `value` into a list before adding
  # it into `vm` when `meta_nm %in% c("describer", "labeler")` --- those are
  # known `list` columns.
  # @codedoc_comment_block news("vm@var_meta_set", "2024-09-04", "0.5.7")
  # @codedoc_comment_block vm@var_meta_set
  # `value` is wrapped inside a `list` if `var_dt[[meta_nm]]` is a `list`
  # or if `meta_nm %in% c("describer", "labeler")`. This is necessary for
  # correct assignment into a `list`-valued column.
  # @codedoc_comment_block vm@var_meta_set
  vd <- vd_get(vm)
  if (
    meta_nm %in% names(vd) && is.list(vd[[meta_nm]]) ||
      meta_nm %in% c("describer", "labeler")
  ) {
    value <- list(value)
    names(value) <- var_nm
  }
  value <- list(value)
  names(value) <- meta_nm
  data.table::set(
    vd,
    i = data.table::chmatch(var_nm, vd[["var_nm"]]),
    j = meta_nm,
    value = value
  )
  vd_set(vm = vm, dt = vd)
  return(invisible(NULL))
}


var_meta_get_all <- function(
  vm,
  meta_nm
) {
  # @codedoc_comment_block vm@var_meta_get_all
  # Get metadata for all variables.
  # @codedoc_comment_block vm@var_meta_get_all

  assert_is_var_meta_nm(vm, meta_nm)
  vd <- vd_get(vm)
  # @codedoc_comment_block news("vm@var_meta_get_all", "2023-12-04", "0.2.0")
  # `vm@var_meta_get_all` now always sets `var_dt$var_nm` as the names
  # of the output list or vector.
  # @codedoc_comment_block news("vm@var_meta_get_all", "2023-12-04", "0.2.0")
  out <- vd[[meta_nm]]
  names(out) <- vd[["var_nm"]]
  return(out)
}


var_rename <- function(
  vm,
  old_var_nms,
  new_var_nms
) {
  # @codedoc_comment_block news("vm@var_rename", "2023-12-01", "0.2.0")
  # Rename `old` to `old_var_nms` and `new` to `new_var_nms`.
  # @codedoc_comment_block news("vm@var_rename", "2023-12-01", "0.2.0")

  # @codedoc_comment_block doc_slot_fun_arg(old_var_nms)
  # **`old_var_nms`** `[character]` (no default)
  #
  # Variable names to change.
  # @codedoc_comment_block doc_slot_fun_arg(old_var_nms)
  dbc::assert_is_character_nonNA_vector(old_var_nms)
  # @codedoc_comment_block doc_slot_fun_arg(new_var_nms)
  # **`new_var_nms`** `[character]` (no default)
  #
  # Variable names to change to.
  # @codedoc_comment_block doc_slot_fun_arg(new_var_nms)
  dbc::assert_is_character_nonNA_vector(new_var_nms)
  stopifnot(length(old_var_nms) == length(new_var_nms))
  lapply(seq_along(old_var_nms), function(i) {
    old_var_nm <- old_var_nms[i]
    assert_is_var_nm(vm, old_var_nm)
    new_var_nm <- new_var_nms[i]
    # @codedoc_comment_block vm@var_rename
    # Rename variables. The name is changed in the following places:
    #
    # - `var_dt$var_nm`
    # @codedoc_comment_block vm@var_rename
    var_meta_set(
      vm,
      var_nm = old_var_nm,
      meta_nm = "var_nm",
      value = new_var_nm
    )
    # @codedoc_comment_block vm@var_rename
    # - `var_set_dt$value_space`, if the `value_space` object is of type `dt`.
    #   If the `value_space` is of type `expr` or `fun`, `vm@var_rename` emits
    #   a warning because it does not attempt to alter R expressions or
    #   functions --- you will have to do that youself.
    # @codedoc_comment_block vm@var_rename
    # @codedoc_comment_block news("vm@var_rename", "2023-12-07", "0.2.2")
    # Fix renaming multiple variables on one go.
    # @codedoc_comment_block news("vm@var_rename", "2023-12-07", "0.2.2")
    id_set <- var_to_var_set_id(vm, new_var_nm)
    if (var_set_value_space_is_defined(vm)) {
      lapply(id_set, function(id) {
        vs <- var_set_value_space_get(vm, id = id)
        if ("dt" %in% names(vs)) {
          data.table::setnames(vs[["dt"]], old_var_nm, new_var_nm)
        } else if (any(c("expr", "fun") %in% names(vs))) {
          warning(
            "vm@var_rename cannot handle value_space object of type ",
            names(vs)[1], "; you must alter the value_space object for ",
            "id = ", id, " yourself"
          )
        }
        var_set_value_space_set(vm, id = id, value_space = vs)
        var_nm_set <- var_set_meta_get(vm, id = id, meta_nm = "var_nm_set")
        var_nm_set[var_nm_set == old_var_nm] <- new_var_nm
        # @codedoc_comment_block vm@var_rename
        # - `var_set_dt$var_nm_set`
        # @codedoc_comment_block vm@var_rename
        var_set_meta_set(
          vm,
          id = id,
          meta_nm = "var_nm_set",
          value = var_nm_set
        )
      })
    }
  })
  invisible(NULL)
}

var_remove <- function(
  vm,
  var_nm
) {
  # @codedoc_comment_block vm@var_remove
  # Remove a variable.
  # @codedoc_comment_block vm@var_remove

  # @codedoc_comment_block news("vm@var_remove", "2023-08-11", "0.1.9")
  # `vm@var_remove` can now remove multiple variables in one go.
  # @codedoc_comment_block news("vm@var_remove", "2023-08-11", "0.1.9")
  for (vn in var_nm) {
    assert_is_var_nm(vm, vn)
  }
  expr <- substitute(!var_nm %in% VN, list(VN = var_nm))
  vame_subset_expr(vm, var_dt_expr = expr, var_set_dt_expr = NULL)
}

var_labeler_get <- function(
  vm,
  var_nm
) {
  # @codedoc_comment_block vm@var_labeler_get
  # Get the labeler for a variable.
  # @codedoc_comment_block vm@var_labeler_get
  # @codedoc_comment_block feature_funs(labeling)
  # - `vm@var_labeler_get`
  # @codedoc_comment_block feature_funs(labeling)
  assert_is_var_nm(vm, var_nm)
  out <- var_meta_get(vm, var_nm, "labeler")
  if (is.null(out)) {
    stop("Variable \"", var_nm, "\" has no labeler defined.")
  }
  return(out)
}

var_labeler_set <- function(
  vm,
  var_nm,
  value
) {
  # @codedoc_comment_block vm@var_labeler_get
  # Set the labeler for a variable.
  # @codedoc_comment_block vm@var_labeler_get
  # @codedoc_comment_block feature_funs(labeling)
  # - `vm@var_labeler_set`
  # @codedoc_comment_block feature_funs(labeling)
  var_meta_set(vm, var_nm, "labeler", value)
}

var_labels_get <- function(
  vm,
  x,
  var_nm,
  label_nm = NULL,
  labeler_env = NULL
) {
  # @codedoc_comment_block feature(labeling)
  # The labeling feature becomes available if
  # the `var_dt` of a `VariableMetadata` object has a `labeler` column value
  # for a variable. You can include `labeler` in `var_dt` when the
  # `VariableMetadata` object is constructed or use `vm@var_labeler_set` later.
  #
  # @codedoc_insert_comment_block specification(var_dt$labeler)
  #
  # The following functions are related to this feature:
  # @codedoc_insert_comment_block feature_funs(labeling)
  # @codedoc_comment_block feature(labeling)
  # @codedoc_comment_block feature_funs(labeling)
  # - `vm@var_labels_get`
  # @codedoc_comment_block feature_funs(labeling)
  assert_is_var_nm(vm, var_nm)
  labeler <- var_labeler_get(vm, var_nm = var_nm)

  # @codedoc_comment_block doc_slot_fun_arg(label_nm)
  # **`label_nm`** `[NULL, character]` (default `NULL`)
  #
  # Name of a column in the `labeler` that has been assigned for the variable.
  # Labels will be taken from this column.
  #
  # - `NULL`: Use first column name in `labeler` that is not `"x"` --- if
  #   `labeler` is a `data.table`.
  # - `character`: Use this column name.
  # @codedoc_comment_block doc_slot_fun_arg(label_nm)
  # @codedoc_comment_block news("vm@var_labels_get", "2024-01-24", "0.4.0")
  # `vm@var_labels_get` arg `label_col_nm` renamed to `label_nm`.
  # @codedoc_comment_block news("vm@var_labels_get", "2024-01-24", "0.4.0")
  dbc::assert_is_one_of(
    label_nm,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_character_nonNA_atom)
  )

  # @codedoc_comment_block doc_slot_fun_arg(labeler_env)
  # **`labeler_env`** `[NULL, environment]` (default `NULL`)
  #
  # Environment where `labeler` of class `call` is evaluated.
  #
  # - `NULL`: Use the environment where this function is called.
  # - `environment`: Use this environment.
  # @codedoc_comment_block doc_slot_fun_arg(labeler_env)
  dbc::assert_is_one_of(
    labeler_env,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_environment)
  )
  if (is.null(labeler_env)) {
    labeler_env <- parent.frame(1L)
  }

  # @codedoc_comment_block news("vm@var_labels_get", "2024-09-11", "1.1.0")
  # `vm@var_labels_get` now makes variables `x`, `var_nm`, `label_nm`, and `vm`
  # available for `labeler` types `function` and `call`.
  # @codedoc_comment_block news("vm@var_labels_get", "2024-09-11", "1.1.0")
  # @codedoc_comment_block vm@var_labels_get
  # Get label for each value in `x` for `var_nm`.
  #
  # - A `labeler` of type `function` or `call` has  variables
  #  `x`, `label_nm`, `var_nm`, and `vm` available. `vm` is the
  #  `vame::VariableMetadata` object itself.
  # @codedoc_comment_block vm@var_labels_get
  arg_list <- list(x = x, label_nm = label_nm, var_nm = var_nm, vm = vm)
  if (is.function(labeler)) {
    # @codedoc_comment_block news("vm@var_labels_get", "2023-12-01", "0.2.0")
    # `vm@var_labels_get` now can handle `labeler`s of type `function`.
    # @codedoc_comment_block news("vm@var_labels_get", "2023-12-01", "0.2.0")
    if (is.null(label_nm)) {
      stop("label_nm = NULL, but labeler is a function so cannot ",
           "determine label_nm automatically.")
    }
    # @codedoc_comment_block vm@var_labels_get
    # - A `labeler` of type `function` is called with a subset of the
    #   the variables listed above which are arguments of the function.
    #   I.e. your function is allowed to use only some of the variables
    #   as input arguments such as `x` and `label_nm` only if you want.
    # @codedoc_comment_block vm@var_labels_get
    out <- do.call(
      labeler,
      arg_list[intersect(names(arg_list), names(formals(labeler)))]
    )
  } else if (is.call(labeler)) {
    # @codedoc_comment_block news("vm@var_labels_get", "2023-12-01", "0.2.1")
    # `vm@var_labels_get` now can handle `labeler`s of class `call`.
    # Added argument `labeler_env` for this purpose.
    # @codedoc_comment_block news("vm@var_labels_get", "2023-12-01", "0.2.1")
    # @codedoc_comment_block vm@var_labels_get
    # - A `labeler` of type `call` is evaluated in a temporary evaluation
    #   environment that contains the variables listed above. The parent of this
    #   evaluation environment is set to `labeler_env`.
    # @codedoc_comment_block vm@var_labels_get
    eval_env <- as.environment(arg_list)
    parent.env(eval_env) <- labeler_env
    out <- tryCatch(
      eval(labeler, envir = labeler_env),
      error = function(e) {
        stop(
          "Labeler for var_nm = \"", var_nm, "\" was of class 'call', but ",
          "evaluation failed. Error message: \"", e[["message"]], "\""
        )
      }
    )
    if (!is.character(out)) {
      utils::str(out)
      stop(
        "Labeler for var_nm = \"", var_nm, "\" was of class 'call', but ",
        "evaluation did not produce a character string vector. See what ",
        "`str(result)` printed above this error message."
      )
    }
  } else if (inherits(labeler, "data.table")) {
    label_nm_set <- setdiff(names(labeler), "x")
    if (is.null(label_nm)) {
      label_nm <- label_nm_set[1]
    } else if (!label_nm %in% label_nm_set) {
      stop("label_nm = \"", label_nm, "\" not one of the defined ",
           "label names: ", deparse1(label_nm_set))
    }
    dbc::assert_has_class(x = x, required_class = class(labeler[["x"]]))
    jdt <- data.table::setDT(list(x = x))
    # @codedoc_comment_block vm@var_labels_get
    # - If `labeler` is a `data.table`, we get a label for each `x` using a
    #   left join on the `data.table`.
    # @codedoc_comment_block vm@var_labels_get
    #' @importFrom data.table .SD
    out <- labeler[
      i = jdt,
      on = "x",
      j = .SD[[1]],
      .SDcols = label_nm
    ]
  } else {
    stop("no handling defined for labeler of class(es) ",
         deparse1(class(labeler)))
  }
  return(out)
}

var_describer_get <- function(
  vm,
  var_nm
) {
  # @codedoc_comment_block vm@var_describer_get
  # Get the describer for a variable.
  # @codedoc_comment_block vm@var_describer_get
  # @codedoc_comment_block feature_funs(describing)
  # - `vm@var_describer_get`
  # @codedoc_comment_block feature_funs(describing)
  # @codedoc_comment_block news("vm@var_describer_get", "2024-01-22", "0.4.0")
  # New function `vm@var_describer_get`.
  # @codedoc_comment_block news("vm@var_describer_get", "2024-01-22", "0.4.0")
  assert_is_var_nm(vm, var_nm)
  out <- var_meta_get(vm, var_nm, "describer")
  if (is.null(out)) {
    stop("Variable \"", var_nm, "\" has no describer defined.")
  }
  return(out)
}

var_describer_set <- function(
  vm,
  var_nm,
  value
) {
  # @codedoc_comment_block vm@var_describer_get
  # Set the describer for a variable.
  # @codedoc_comment_block vm@var_describer_get
  # @codedoc_comment_block feature_funs(describing)
  # - `vm@var_describer_set`
  # @codedoc_comment_block feature_funs(describing)
  # @codedoc_comment_block news("vm@var_describer_set", "2024-01-22", "0.4.0")
  # New function `vm@var_describer_set`.
  # @codedoc_comment_block news("vm@var_describer_set", "2024-01-22", "0.4.0")
  var_meta_set(vm, var_nm, "describer", value)
}

var_description_get <- function(
  vm,
  var_nm,
  descr_nm = NULL,
  describer_env = NULL
) {
  # @codedoc_comment_block feature_funs(describing)
  # - `vm@var_description_get`
  # @codedoc_comment_block feature_funs(describing)
  # @codedoc_comment_block news("vm@var_description_get", "2024-01-22", "0.4.0")
  # New function `vm@var_description_get`.
  # @codedoc_comment_block news("vm@var_description_get", "2024-01-22", "0.4.0")

  # @codedoc_comment_block feature(describing)
  # The describing feature becomes available if
  # the `var_dt` of a `VariableMetadata` object has a `describer` column value
  # for a variable. You can include `describer` in `var_dt` when the
  # `VariableMetadata` object is constructed or use `vm@var_describer_set`
  # later.
  #
  # @codedoc_insert_comment_block specification(var_dt$describer)
  #
  # The following functions are related to this feature:
  # @codedoc_insert_comment_block feature_funs(describing)
  # @codedoc_comment_block feature(describing)
  # @codedoc_comment_block feature_example(describing)
  #
  # # Example of describing feature
  # my_vame <- vame::VariableMetadata(
  #   var_dt = data.table::data.table(
  #     var_nm = c("sex", "birth_date"),
  #     describer = list(
  #       sex = list(
  #         en = "Sex assigned at birth.",
  #         fi = "Syntymahetken sukupuoli."
  #       ),
  #       birth_date = quote(switch(
  #         descr_nm,
  #         en = "Date of birth.",
  #         fi = "Syntymapaivamaara."
  #       ))
  #     )
  #   ),
  #   var_set_dt = data.table::data.table(
  #     id = c("sex", "birth_date"),
  #     var_nm_set = list(sex = "sex", birth_date = "birth_date")
  #   )
  # )
  # new_descr <- list(en = c("Sex ", "at ", "birth."), fi = "Sukupuoli.")
  # my_vame@var_describer_set(
  #   var_nm = "sex",
  #   value = new_descr
  # )
  # stopifnot(
  #   identical(
  #     my_vame@var_description_get("sex", descr_nm = "en"),
  #     new_descr[["en"]]
  #   ),
  #   inherits(
  #     tryCatch(
  #       my_vame@var_description_get("birth_date"),
  #       error = function(e) e
  #     ),
  #     "error"
  #   ),
  #   identical(
  #     my_vame@var_description_get(
  #       var_nm = "birth_date",
  #       descr_nm = "fi"
  #     ),
  #     "Syntymapaivamaara."
  #   )
  # )
  # @codedoc_comment_block feature_example(describing)
  assert_is_var_nm(vm, var_nm)
  describer <- var_describer_get(vm, var_nm = var_nm)

  # @codedoc_comment_block doc_slot_fun_arg(descr_nm)
  # **`descr_nm`** `[NULL, character]` (default `NULL`)
  #
  # Name of a description in the `describer` that has been assigned for the
  # variable.
  #
  # - `NULL`: If `describer` is a `list`, use first element.
  #   Else raises an error.
  # - `character`: Use this description name.
  # @codedoc_comment_block doc_slot_fun_arg(descr_nm)
  # @codedoc_comment_block news("vm@var_description_get", "2024-01-24", "0.4.0")
  # `vm@var_description_get` arg `description_name` renamed to `descr_nm`.
  # @codedoc_comment_block news("vm@var_description_get", "2024-01-24", "0.4.0")
  dbc::assert_is_one_of(
    descr_nm,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_character_nonNA_atom)
  )

  # @codedoc_comment_block doc_slot_fun_arg(describer_env)
  # **`describer_env`** `[NULL, environment]` (default `NULL`)
  #
  # Parent environment of evaluation environment where `describer` of class
  # `call` is evaluated.
  #
  # - `NULL`: Use the environment where this function is called.
  # - `environment`: Use this environment.
  # @codedoc_comment_block doc_slot_fun_arg(describer_env)
  dbc::assert_is_one_of(
    describer_env,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_environment)
  )
  if (is.null(describer_env)) {
    describer_env <- parent.frame(1L)
  }

  # @codedoc_comment_block news("vm@var_description_get", "2024-09-11", "1.1.0")
  # `vm@var_description_get` now makes variables `var_nm`, `descr_nm`, and `vm`
  # available for `describer` types `function` and `call`.
  # @codedoc_comment_block news("vm@var_description_get", "2024-09-11", "1.1.0")
  # @codedoc_comment_block vm@var_description_get
  # Get description for `var_nm`.
  #
  # - A `describer` of type `function` or `call` has  variables
  #  `var_nm`, `descr_nm`, and `vm` available. `vm` is the
  #  `vame::VariableMetadata` object itself.
  # @codedoc_comment_block vm@var_description_get
  arg_list <- mget(c("var_nm", "descr_nm", "vm"))

  if (is.function(describer)) {
    if (is.null(descr_nm)) {
      stop("descr_nm = NULL, but describer is a function so cannot ",
           "determine descr_nm automatically.")
    }
    # @codedoc_comment_block vm@var_description_get
    # - A `describer` of type `function` is called with a subset of the
    #   the variables listed above which are arguments of the function.
    #   I.e. your function is allowed to use only some of the variables
    #   as input arguments such as `descr_nm` only if you want.
    # @codedoc_comment_block vm@var_description_get
    out <- do.call(
      describer,
      arg_list[intersect(names(arg_list), names(formals(describer)))]
    )
  } else if (is.call(describer)) {
    if (is.null(descr_nm)) {
      stop("descr_nm = NULL, but describer is a call so cannot ",
           "determine descr_nm automatically.")
    }
    eval_env <- as.environment(arg_list)
    parent.env(eval_env) <- describer_env
    # @codedoc_comment_block vm@var_description_get
    # - A `describer` of type `call` is evaluated in a temporary evaluation
    #   environment that has `describer_env` as its parent. The evaluation
    #   environment is populated by the variables listed above.
    # @codedoc_comment_block vm@var_description_get
    out <- tryCatch(
      eval(describer, envir = eval_env),
      error = function(e) {
        stop(
          "describer for var_nm = \"", var_nm, "\" was of class 'call', but ",
          "evaluation failed. Error message: \"", e[["message"]], "\""
        )
      }
    )
  } else if (inherits(describer, "list")) {
    descr_nm_set <- names(describer)
    if (is.null(descr_nm)) {
      descr_nm <- descr_nm_set[1]
    } else if (!descr_nm %in% descr_nm_set) {
      stop("descr_nm = \"", descr_nm,
           "\" not one of the defined ",
           "elements: ", deparse1(descr_nm_set))
    }
    # @codedoc_comment_block vm@var_description_get
    # - If `describer` is a `list`, we simply take the appropriate element from
    #   the list.
    # @codedoc_comment_block vm@var_description_get
    out <- describer[[descr_nm]]
  } else {
    stop("no handling defined for describer of class(es) ",
         deparse1(class(describer)))
  }
  assert_is_description(out, assertion_type = "prod_output")
  return(out)
}

var_value_space_sample <- function(
  vm,
  var_nm,
  env = NULL,
  n = 1L
) {
  # @codedoc_comment_block vm@var_value_space_sample
  # Wrapper for `vm@var_set_value_space_sample` for when you want a random
  # sample for only one variable.
  # @codedoc_comment_block vm@var_value_space_sample

  # @codedoc_comment_block news("vm@var_value_space_sample", "2023-12-12", "0.2.2")
  # New function `vm@var_value_space_sample`.
  # @codedoc_comment_block news("vm@var_value_space_sample", "2023-12-12", "0.2.2")

  # @codedoc_comment_block feature_funs(random sampling)
  # - `vm@var_value_space_sample`
  # @codedoc_comment_block feature_funs(random sampling)
  id <- var_to_var_set_id(vm = vm, var_nm = var_nm)
  out <- var_set_value_space_sample(
    vm = vm,
    id = id,
    n = n
  )
  if (inherits(out, "data.table")) {
    out <- out[[var_nm]]
  }
  # @codedoc_comment_block vm@vame_value_space_sample
  # `vm@var_value_space_sample` always returns a vector of length `n`.
  # @codedoc_comment_block vm@vame_value_space_sample
  dbc::assert_prod_output_is_vector(out)
  dbc::assert_prod_output_is_of_length(out, expected_length = n)
  return(out)
}

var_var_set_dt_pos_set_get <- function(
  vm,
  var_nm
) {
  # @codedoc_comment_block vm@var_var_set_dt_pos_set_get
  # Retrieve positions (indices) of where `var_nm` is part of
  # `var_set_dt$var_nm_set`.
  # @codedoc_comment_block vm@var_var_set_dt_pos_set_get
  # @codedoc_comment_block news("vm@var_var_set_dt_pos_set_get", "2024-12-19", "1.5.0")
  # New function `vm@var_var_set_dt_pos_set_get`.
  # @codedoc_comment_block news("vm@var_var_set_dt_pos_set_get", "2024-12-19", "1.5.0")
  var_meta_get(vm = vm, var_nm = var_nm, meta_nm = "var_set_dt_pos_set")
}

var_var_set_dt_id_set_get <- function(
  vm,
  var_nm
) {
  # @codedoc_comment_block vm@var_var_set_dt_id_set_get
  # Retrieve `var_set_dt$id` values of where `var_nm` is part of
  # `var_set_dt$var_nm_set`.
  # @codedoc_comment_block vm@var_var_set_dt_id_set_get
  # @codedoc_comment_block news("vm@var_var_set_dt_id_set_get", "2024-12-19", "1.5.0")
  # New function `vm@var_var_set_dt_id_set_get`.
  # @codedoc_comment_block news("vm@var_var_set_dt_id_set_get", "2024-12-19", "1.5.0")
  pos_set <- var_var_set_dt_pos_set_get(vm = vm, var_nm = var_nm)
  id_set <- var_set_meta_get_all(vm = vm, meta_nm = "id")[pos_set]
  return(id_set)
}

vame_harmonise_dt <- function(
  vm,
  dt,
  var_nms = NULL,
  inplace = TRUE
) {
  # @codedoc_comment_block vm@vame_harmonise_dt
  # Recode (via `maker` calls) and rename columns. Requires
  # `var_dt$is_harmonised` to be defined. The `maker`s of those variables
  # which have `is_harmonised == TRUE` determine which variables harmonise
  # into the harmonised forms. See **Examples**.
  # @codedoc_comment_block vm@vame_harmonise_dt
  # @codedoc_comment_block feature_funs(make)
  # - `vm@vame_harmonise_dt`
  # @codedoc_comment_block feature_funs(make)
  # @codedoc_comment_block news("vm@vame_harmonise_dt", "2024-12-19", "1.5.0")
  # New function `vm@vame_harmonise_dt`.
  # @codedoc_comment_block news("vm@vame_harmonise_dt", "2024-12-19", "1.5.0")

  # @codedoc_comment_block function_example(vm@vame_harmonise_dt)
  # # vm@vame_harmonise_dt
  # vm <- vame::VariableMetadata(
  #   var_dt = data.table::data.table(
  #     var_nm = c("a1", "a2", "official|A"),
  #     is_harmonised = c(FALSE, FALSE, TRUE)
  #   ),
  #   var_set_dt = data.table::data.table(
  #     id = c("a1", "a2", "official|A"),
  #     var_nm_set = list("a1", "a2", "official|A"),
  #     maker = list(
  #       NULL,
  #       NULL,
  #       list(
  #         dep_var_nm_sets = list("a1", "a2"),
  #         maker = quote({
  #           switch(
  #             intersect(c("a1", "a2"), ls())[1],
  #             a1 = data.table::data.table("official|A" = a1 + 1L),
  #             a2 = data.table::data.table("official|A" = a2 + 2L)
  #           )
  #         })
  #       )
  #     )
  #   )
  # )
  #
  # my_dt_1 <- data.table::data.table(
  #   a1 = 1:2,
  #   some_var = runif(2)
  # )
  # my_dt_1 <- vm@vame_harmonise_dt(dt = my_dt_1)
  # my_dt_2 <- data.table::data.table(
  #   a2 = 1:2,
  #   some_var = runif(2)
  # )
  # my_dt_2 <- vm@vame_harmonise_dt(dt = my_dt_2)
  # my_dt_3 <- data.table::data.table(
  #   a1 = 1:2,
  #   some_var = runif(2)
  # )
  # vm@vame_harmonise_dt(dt = my_dt_3, inplace = TRUE)
  #
  # stopifnot(
  #   !"a1" %in% names(my_dt_1),
  #   identical(names(my_dt_1), c("official|A", "some_var")),
  #   my_dt_1[["official|A"]] == 2:3,
  #
  #   !"a2" %in% names(my_dt_2),
  #   identical(names(my_dt_2), c("official|A", "some_var")),
  #   my_dt_2[["official|A"]] == 3:4,
  #
  #   !"a1" %in% names(my_dt_3),
  #   identical(names(my_dt_3), c("official|A", "some_var")),
  #   my_dt_3[["official|A"]] == 2:3
  # )
  #
  # # multiple harmonisation candidates
  # vm <- vame::VariableMetadata(
  #   var_dt = data.table::data.table(
  #     var_nm = c("a", "official|A1", "official|A2"),
  #     is_harmonised = c(FALSE, TRUE, TRUE)
  #   ),
  #   var_set_dt = data.table::data.table(
  #     id = c("a", "official|A1", "official|A2"),
  #     var_nm_set = list("a", "official|A1", "official|A2"),
  #     maker = list(
  #       NULL,
  #       list(
  #         dep_var_nm_set = "a",
  #         maker = quote({
  #           data.table::data.table("official|A1" = a + 1L)
  #         })
  #       ),
  #       list(
  #         dep_var_nm_set = "a",
  #         maker = quote({
  #           data.table::data.table("official|A2" = a + 2L)
  #         })
  #       )
  #     )
  #   )
  # )
  #
  # my_dt_1 <- data.table::data.table(
  #   a = 1:2,
  #   some_var = runif(2)
  # )
  # # Error because of multiple harmonisation candidates --- cannot automatically
  # # know which one to use.
  # my_dt_1 <- tryCatch(
  #   vm@vame_harmonise_dt(dt = my_dt_1),
  #   error = function(e) e
  # )
  # stopifnot(
  #   inherits(my_dt_1, "error")
  # )
  # my_dt_1 <- data.table::data.table(
  #   a = 1:2,
  #   some_var = runif(2)
  # )
  # my_dt_2 <- data.table::copy(my_dt_1)
  # my_dt_1 <- vm@vame_harmonise_dt(dt = my_dt_1, var_nms = c("a" = "official|A1"))
  # my_dt_2 <- vm@vame_harmonise_dt(dt = my_dt_2, var_nms = c("a" = "official|A2"))
  #
  # stopifnot(
  #   !"a" %in% names(my_dt_1),
  #   identical(names(my_dt_1), c("official|A1", "some_var")),
  #   my_dt_1[["official|A1"]] == 2:3,
  #
  #   !"a" %in% names(my_dt_2),
  #   identical(names(my_dt_2), c("official|A2", "some_var")),
  #   my_dt_2[["official|A2"]] == 3:4
  # )
  # @codedoc_comment_block function_example(vm@vame_harmonise_dt)

  # @codedoc_comment_block vame::vame_harmonise_dt::dt
  # **`dt`** `[data.table]`
  #
  # `data.table` object to harmonise.
  # @codedoc_comment_block vame::vame_harmonise_dt::dt
  dbc::assert_is_data_table(dt)

  # @codedoc_comment_block vame::vame_harmonise_dt::var_nms
  # **`var_nms`** `[NULL, character]` (default `NULL`)
  #
  # - `NULL`: Try to harmonise all columns of `dt`.
  # - `character`: Harmonise these only and raise an error this fails.
  #   A named vector, e.g. `c(old = "new")`, forces a specific replacement
  #   of old with new regardless of `var_dt$is_harmonised`. Mandatory in
  #   (rare) instances where one variable can be harmonised into multiple
  #   variables that have `var_dt$is_harmonised` value `TRUE`.
  # @codedoc_comment_block vame::vame_harmonise_dt::var_nms
  assert_is_arg_var_nms(
    vm = vm,
    x = var_nms,
    must_exist = FALSE,
    allow_null = TRUE
  )
  if (!var_meta_is_defined(vm = vm, var_nm = NULL, meta_nm = "is_harmonised")) {
    stop("Your `vame::VariableMetadata` object does not have ",
         "`var_dt$is_harmonised` defined. This boolean column is needed to ",
         "identify what input data are harmonised to.")
  }
  original_dt_col_nms <- data.table::copy(names(dt))
  # @codedoc_comment_block vame::vame_harmonise_dt::inplace
  # **`inplace`** `[logical]` (default `FALSE`)
  #
  # If `FALSE`, a copy of `dt` is taken. If `FALSE`, `dt` is modified in-place
  # without taking a copy.
  # @codedoc_comment_block vame::vame_harmonise_dt::inplace
  if (!inplace) {
    dt <- data.table::copy(dt)
  }
  meta_dt <- local({
    if (is.null(var_nms)) {
      meta_dt <- data.table::data.table(
        var_nm = names(dt),
        var_nm_harmonised = NA_character_
      )
      forced_harmonisation <- FALSE
    } else {
      if (!is.null(names(var_nms))) {
        meta_dt <- data.table::data.table(
          var_nm = names(var_nms),
          var_nm_harmonised = unname(var_nms)
        )
        forced_harmonisation <- TRUE
      } else {
        meta_dt <- data.table::data.table(
          var_nm = var_nms,
          var_nm_harmonised = NA_character_
        )
      }
    }
    harmonisation_candidate_set <- local({
      if (forced_harmonisation) {
        harmonisation_candidate_set <- meta_dt[["var_nm_harmonised"]]
      } else {
        all_var_nms <- var_meta_get_all(vm = vm, meta_nm = "var_nm")
        is_harmonised_idx <- which(var_meta_get_all(
          vm = vm, meta_nm = "is_harmonised"
        ))
        harmonisation_candidate_set <- all_var_nms[is_harmonised_idx]
      }
      harmonisation_candidate_set
    })
    tgt_meta_dt <- data.table::rbindlist(lapply(
      harmonisation_candidate_set,
      function(harmonisation_candidate) {
        id_set <- var_var_set_dt_id_set_get(
          vm = vm,
          var_nm = harmonisation_candidate
        )
        data.table::rbindlist(lapply(
          id_set,
          function(id) {
            if (!var_set_meta_is_defined(vm = vm, id = id, meta_nm = "maker")) {
              return(NULL)
            }
            dep_var_nm_sets <- var_set_maker_get_dep_var_nm_sets(
              vm = vm,
              id = id
            )
            rm <- vapply(dep_var_nm_sets, length, integer(1L)) != 1L
            dep_var_nm_sets[rm] <- NULL
            var_nms <- unlist(dep_var_nm_sets)
            n_rep <- length(var_nms)
            data.table::data.table(
              id = rep(id, n_rep),
              harmonisation_candidate = rep(harmonisation_candidate, n_rep),
              var_nm = var_nms
            )
          }
        ))
      }
    ))
    if (forced_harmonisation) {
      tgt_meta_dt <- tgt_meta_dt[
        i = meta_dt,
        on = c("var_nm", "harmonisation_candidate" = "var_nm_harmonised"),
        nomatch = NULL
      ]
      miss_var_nms <- setdiff(tgt_meta_dt[["var_nm"]], meta_dt[["var_nm"]])
      if (length(miss_var_nms) > 0) {
        stop("The harmonised forms of the following variable(s) had no ",
             "`var_set_dt$maker`: ",
             deparse1(miss_var_nms), ". Define ",
             "`var_set_dt$maker` for variables you want to harmonise to.")
      }
    }
    dup_var_nms <- tgt_meta_dt[["var_nm"]][duplicated(tgt_meta_dt[["var_nm"]])]
    if (length(dup_var_nms) > 0) {
      print(tgt_meta_dt)
      stop("The following variable(s) have more than possible harmonisation ",
           "candidate: ", deparse1(dup_var_nms), ". It is best to have only ",
           "one harmonisation candidate per variable. If you want to have ",
           "multiple anyway, you must use `vm@vame_harmonise_dt` argument ",
           "`var_nms` to force a specific harmonisation. See the table ",
           "above for more information.")
    }
    dt_join_assign__(
      dt = meta_dt,
      i = tgt_meta_dt,
      on = "var_nm",
      dt_col_nms = c("id", "var_nm_harmonised"),
      i_col_nms = c("id", "harmonisation_candidate")
    )
    meta_dt[]
  })

  # At this point, missing `id` implies no harmonisation will be performed
  # on the `var_nm`
  lapply(which(!is.na(meta_dt[["id"]])), function(i) {
    vn_i <- meta_dt[["var_nm"]][i]
    hc_i <- meta_dt[["var_nm_harmonised"]][i]
    made_dt <- var_set_make(
      vm = vm,
      data = dt,
      id =  meta_dt[["id"]][i],
      var_nms = hc_i
    )
    data.table::setnames(dt, vn_i, hc_i)
    data.table::set(
      dt,
      j = hc_i,
      value = made_dt[[hc_i]]
    )
  })

  data.table::setattr(
    dt,
    "vame_harmonise_dt_meta",
    list(
      dt_col_nms = original_dt_col_nms,
      dt_col_nms_harmonised = names(dt),
      var_nms = meta_dt[["var_nms"]],
      var_nms_harmonised = meta_dt[["var_nm_harmonised"]]
    )
  )
  return(dt[])
}

# vame funs --------------------------------------------------------------------
vame_copy <- function(vm) {
  # @codedoc_comment_block vm@vame_copy
  # Take a deep copy of a VariableMetadata object. See `?data.table::copy`.
  # @codedoc_comment_block vm@vame_copy

  # @codedoc_comment_block news("vm@vame_copy", "2023-08-10", "0.1.8")
  # New slot fun `vm@vame_copy` + new exported fun `vame::vame_copy`.
  # @codedoc_comment_block news("vm@vame_copy", "2023-08-10", "0.1.8")

  # @codedoc_comment_block function_example(vm@vame_copy)
  # # vm@vame_copy
  # vm_1 <- vame::VariableMetadata(
  #   var_dt = data.table::data.table(var_nm = "a"),
  #   var_set_dt = data.table::data.table(id = "a", var_nm_set = list("a")),
  #   vame_list = list(hello = "there")
  # )
  # vm_2 <- vm_1@vame_copy()
  # stopifnot(
  #   all.equal(vm_1@var_dt_copy(), vm_2@var_dt_copy()),
  #   all.equal(vm_1@var_set_dt_copy(), vm_2@var_set_dt_copy())
  # )
  # vm_2@var_rename("a", "b")
  # stopifnot(
  #   identical(unname(vm_1@var_meta_get_all(meta_nm = "var_nm")), "a"),
  #   identical(unname(vm_2@var_meta_get_all(meta_nm = "var_nm")), "b"),
  #   identical(vm_1@vame_list_copy(), vm_2@vame_list_copy())
  # )
  # @codedoc_comment_block function_example(vm@vame_copy)

  # @codedoc_comment_block news("vm@vame_copy", "2023-08-22", "0.5.5")
  # `vm@vame_copy` now also copies `vame_list`.
  # @codedoc_comment_block news("vm@vame_copy", "2023-08-22", "0.5.5")
  out <- vame::VariableMetadata(
    var_dt = var_dt_copy(vm = vm),
    var_set_dt = var_set_dt_copy(vm = vm),
    vame_list = vame_list_copy(vm = vm)
  )
  return(out)
}


vame_subset <- function(
  vm,
  var_dt_expr = NULL,
  var_set_dt_expr = NULL,
  enclos = NULL
) {
  # @codedoc_comment_block function_example(vm@vame_subset)
  # # vm@vame_subset
  # vm <- vame::VariableMetadata(
  #   var_dt = data.table::data.table(
  #     var_nm = c("a", "b", "c"),
  #     flavour = c("tasty", "rancid", "bitter")
  #   ),
  #   var_set_dt = data.table::data.table(
  #     id = c("set_01", "set_02"),
  #     var_nm_set = list(c("a", "b"), "c"),
  #     value_space = list(
  #       set_01 = list(dt = data.table::data.table(
  #         a = 1:2,
  #         b = 3:4
  #       )),
  #       set_02 = list(set = 5:6)
  #     )
  #   )
  # )
  # allowed_value_set <- "a"
  # vm_1 <- vm@vame_copy()
  # vm_1@vame_subset(var_dt_expr = var_nm %in% allowed_value_set)
  # stopifnot(
  #   identical(unname(vm_1@var_meta_get_all(meta_nm = "var_nm")), "a"),
  #   identical(names(vm_1@var_set_value_space_eval("set_01")[["dt"]]), "a")
  # )
  #
  # allowed_id_set <- "set_02"
  # vm_2 <- vm@vame_copy()
  # vm_2@vame_subset(var_set_dt_expr = var_set_dt[["id"]] %in% allowed_id_set)
  # stopifnot(
  #   identical(unname(vm_2@var_meta_get_all(meta_nm = "var_nm")), "c"),
  #   identical(unname(vm_2@var_set_meta_get_all(meta_nm = "id")), "set_02")
  # )
  #
  # allowed_id_set <- "set_02"
  # vm_3 <- vm@vame_copy()
  # vm_3@vame_subset(var_set_dt_expr = quote(id %in% allowed_id_set))
  # stopifnot(
  #   identical(unname(vm_3@var_meta_get_all(meta_nm = "var_nm")), "c"),
  #   identical(unname(vm_3@var_set_meta_get_all(meta_nm = "id")), "set_02")
  # )
  # @codedoc_comment_block function_example(vm@vame_subset)

  # @codedoc_comment_block news("vm@vame_subset", "2023-12-01", "0.2.0")
  # Rename `expr` to `var_dt_expr`. Add arg `var_set_dt_expr`.
  # @codedoc_comment_block news("vm@vame_subset", "2023-12-01", "0.2.0")

  # @codedoc_comment_block vm@vame_subset
  # Subset whole `VariableMetadata` object in-place. This means that your
  # `VariableMetadata` object is modified and no copy is taken.
  # Subset either `var_dt` or
  # `var_set_dt` (or both) and keep only metadata for variables that appear in
  # both `var_dt` and `var_set_dt`. Performs the following steps:
  #
  # - Collect `var_dt_expr` and `var_set_dt_expr` using `substitute`.
  # @codedoc_comment_block vm@vame_subset
  # @codedoc_comment_block doc_slot_fun_arg(var_dt_expr)
  # **`var_dt_expr`** `[NULL, logical, integer, name, call]` (default `NULL`)
  #
  # Subset `var_dt`.
  #
  # - `NULL`: No subsetting.
  # - `logical` / `integer`: Subset to these rows.
  # - `name` / `call`: The expression is evaluated and must result in the other
  #   allowed classes.
  # @codedoc_comment_block doc_slot_fun_arg(var_dt_expr)
  # @codedoc_comment_block news("vm@vame_subset", "2024-08-22", "0.5.4")
  # Fix use of `substitute` in turning args `var_dt_expr` + `var_set_dt_expr`
  # into a quoted expression.
  # @codedoc_comment_block news("vm@vame_subset", "2024-08-22", "0.5.4")
  var_dt_expr <- substitute(var_dt_expr, env = parent.frame(1L))
  # @codedoc_comment_block doc_slot_fun_arg(var_set_dt_expr)
  # **`var_set_dt_expr`** `[NULL, logical, integer, name, call]` (default `NULL`)
  #
  # Subset `var_set_dt`.
  #
  # - `NULL`: No subsetting.
  # - `logical` / `integer`: Subset to these rows.
  # - `name` / `call`: The expression is evaluated and must result in the other
  #   allowed classes.
  # @codedoc_comment_block doc_slot_fun_arg(var_set_dt_expr)
  var_set_dt_expr <- substitute(var_set_dt_expr, env = parent.frame(1L))

  # @codedoc_comment_block doc_slot_fun_arg(enclos)
  # **`enclos`** `[NULL, environment]` (default `NULL`)
  #
  # Context environment for evaluating `var_dt_expr` and `var_set_dt_expr`.
  # Effectively we do e.g. `eval(var_dt_expr, envir = var_dt, enclos = enclos)`.
  #
  # - `NULL`: Use the environment where `vm@vame_subset` was called.
  # - `environment`: Use this environment.
  # @codedoc_comment_block doc_slot_fun_arg(enclos)
  # @codedoc_comment_block news("vm@vame_subset", "2025-06-24", "1.10.0")
  # `vm@vame_subset` gained argument `enclos`.
  # @codedoc_comment_block news("vm@vame_subset", "2025-06-24", "1.10.0")
  dbc::assert_is_one_of(
    enclos,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_environment)
  )
  if (is.null(enclos)) {
    # @codedoc_comment_block vm@vame_subset
    # - If `is.null(enclos)`, use the environment where `vm@vame_subset`
    #   was called.
    # @codedoc_comment_block vm@vame_subset
    # @codedoc_comment_block news("vm@vame_subset", "2025-06-24", "1.10.1")
    # `vm@vame_subset` arg `enclos` default fixed. Now uses the environment
    # where `vm@vame_subset` is called.
    # @codedoc_comment_block news("vm@vame_subset", "2025-06-24", "1.10.1")
    enclos <- parent.frame(2L)
  }
  # @codedoc_comment_block vm@vame_subset
  # @codedoc_insert_comment_block vm@vame_subset_expr
  # @codedoc_comment_block vm@vame_subset
  vame_subset_expr(
    vm = vm,
    var_dt_expr = var_dt_expr,
    var_set_dt_expr = var_set_dt_expr,
    enclos = enclos
  )
  return(invisible(NULL))
}

vame_union_append <- function(
  vm,
  vm_2
) {
  assert_is_variablemetadata(vm)
  # @codedoc_comment_block doc_slot_fun_arg(vm_2)
  # **`vm_2`** `[VariableMetadata]` (no default)
  #
  # `VariableMetadata` object whose metadata will be appended to the current
  # `VariableMetadata` object.
  # @codedoc_comment_block doc_slot_fun_arg(vm_2)
  assert_is_variablemetadata(vm_2)

  # @codedoc_comment_block function_example(vm@vame_union_append)
  # vm_1 <- vame::VariableMetadata(
  #   var_dt = data.table::data.table(
  #     var_nm = c("a", "b"),
  #     my_meta = c("vm_1_a", "vm_1_b")
  #   ),
  #   var_set_dt = data.table::data.table(
  #     id = "ab",
  #     var_nm_set = list(c("a", "b"))
  #   )
  # )
  # vm_2 <- vame::VariableMetadata(
  #   var_dt = data.table::data.table(
  #     var_nm = c("b", "c"),
  #     my_meta = c("vm_2_b", "vm_2_c")
  #   ),
  #   var_set_dt = data.table::data.table(
  #     id = "bc",
  #     var_nm_set = list(c("b", "c"))
  #   )
  # )
  # vm_1@vame_union_append(vm_2)
  # stopifnot(
  #   vm_1@var_meta_get(var_nm = "b", meta_nm = "my_meta") == "vm_1_b",
  #   vm_1@var_meta_get(var_nm = "c", meta_nm = "my_meta") == "vm_2_c"
  # )
  # @codedoc_comment_block function_example(vm@vame_union_append)

  # @codedoc_comment_block news("vm@vame_union_append", "2023-07-14", "0.1.4")
  # Fixed `vame_union_append` --- used to always raise an error due to
  # a misnamed object.
  # @codedoc_comment_block news("vm@vame_union_append", "2023-07-14", "0.1.4")
  # @codedoc_comment_block news("vm@vame_union_append", "2023-07-14", "0.1.5")
  # Robustify `vame_union_append` --- use `use.names = TRUE, fill = TRUE`
  # in `rbind` calls.
  # @codedoc_comment_block news("vm@vame_union_append", "2023-07-14", "0.1.5")
  # @codedoc_comment_block news("vm@vame_union_append", "2023-07-14", "0.1.6")
  # fix `vame_union_append` --- no longer attempt to remove duplicates
  # in rbind'd `var_dt` because some `by` columns may be of type `list`
  # which is not supported by `duplicated`.
  # @codedoc_comment_block news("vm@vame_union_append", "2023-07-14", "0.1.6")
  # @codedoc_comment_block news("vm@vame_union_append", "2023-12-01", "0.2.0")
  # Rename `x` to `vm_2`.
  # @codedoc_comment_block news("vm@vame_union_append", "2023-12-01", "0.2.0")
  # @codedoc_comment_block news("vm@vame_union_append", "2024-11-28", "1.4.0")
  # `vm@vame_union_append` now adds metadata from `vm_2` into `vm` even for
  # pre-existing variables where that particular metadata is missing in
  # `vm`. Previously only new variables were added and pre-existing variables'
  # metadata were untouched.
  # @codedoc_comment_block news("vm@vame_union_append", "2024-11-28", "1.4.0")
  vd_1 <- vd_get(vm)
  vsd_1 <- vsd_get(vm)
  vd_2 <- vd_get(vm_2)
  vsd_2 <- vsd_get(vm_2)
  # @codedoc_comment_block vm@vame_union_append
  # Append new data into `VariableMetadata` object from another.
  # No pre-existing data are overwritten. Performs the following steps:
  #
  # - Combine `var_dt` and `var_set_dt` from `vm` and `vm_2` with `rbind`,
  #   adding only new rows from `vm_2` metadata where `var_dt$var_nm` or
  #   `var_set_dt$id` does not exist in `vm`.
  # @codedoc_comment_block vm@vame_union_append
  vd <- rbind(
    vd_1,
    vd_2[!vd_2[["var_nm"]] %in% vd_1[["var_nm"]], ],
    use.names = TRUE,
    fill = TRUE
  )
  vd_set(vm, vd)
  vsd <- rbind(
    vsd_1,
    vsd_2[!vsd_2[["id"]] %in% vsd_1[["id"]], ],
    use.names = TRUE,
    fill = TRUE
  )
  vsd_set(vm, vsd)
  vd_vsd_linkage_refresh(vm)
  # @codedoc_comment_block vm@vame_union_append
  # - Loop through all metadata in the `var_dt` of `vm_2` and add with
  #   `vm@var_meta_set` those metadata that are not defined
  #   (`vm@var_meta_set`).
  # @codedoc_comment_block vm@vame_union_append
  for (i in seq_len(nrow(vd_2))) {
    vd_2_var_nm <- vd_2[["var_nm"]][i]
    for (vd_2_meta_nm in setdiff(names(vd_2), "var_nm")) {
      if (
        vm_2@var_meta_is_defined(
          var_nm = vd_2_var_nm, meta_nm = vd_2_meta_nm
        ) &&
          !vm@var_meta_is_defined(
            var_nm = vd_2_var_nm, meta_nm = vd_2_meta_nm
          )
      ) {
        vm@var_meta_set(
          var_nm = vd_2_var_nm,
          meta_nm = vd_2_meta_nm,
          value = vd_2[[vd_2_meta_nm]][[i]]
        )
      }
    }
  }
  # @codedoc_comment_block vm@vame_union_append
  # - Perform the same loop with `var_set_dt` metadata.
  # @codedoc_comment_block vm@vame_union_append
  for (i in seq_len(nrow(vsd_2))) {
    vsd_2_id <- vsd_2[["id"]][i]
    for (vsd_2_meta_nm in setdiff(names(vsd_2), "id")) {
      if (
        vm_2@var_set_meta_is_defined(id = vsd_2_id, meta_nm = vsd_2_meta_nm) &&
          !vm@var_set_meta_is_defined(id = vsd_2_id, meta_nm = vsd_2_meta_nm)
      ) {
        vm@var_set_meta_set(
          id = vsd_2_id,
          meta_nm = vsd_2_meta_nm,
          value = vsd_2[[vsd_2_meta_nm]][[i]]
        )
      }
    }
  }
  # @codedoc_comment_block vm@vame_union_append
  # This results in `vm` containing new rows in its metadata tables from
  # `vm_2` and possibly new metadata even for pre-existing rows.
  # The function always returns `NULL` invisibly and the modifications are
  # made into `vm` in-place.
  # @codedoc_comment_block vm@vame_union_append
  return(invisible(NULL))
}

vame_meta_is_defined <- function(
  vm,
  meta_nm
) {
  # @codedoc_comment_block vm@vame_meta_is_defined
  # Returns `TRUE` if `vame_list` has non-`NULL` element named `meta_nm`.
  # @codedoc_comment_block vm@vame_meta_is_defined
  # @codedoc_comment_block news("vm@vame_meta_is_defined", "2023-12-12", "0.2.2")
  # New function `vm@vame_meta_is_defined`.
  # @codedoc_comment_block news("vm@vame_meta_is_defined", "2023-12-12", "0.2.2")
  dbc::assert_is_character_nonNA_vector(meta_nm)
  vl <- vame_list_get(vm = vm)
  return(meta_nm %in% names(vl) && !is.null(vl[[meta_nm]]))
}
vame_meta_get <- function(
  vm,
  meta_nm
) {
  # @codedoc_comment_block vm@vame_meta_get
  # Retrieve an element of `vame_list`.
  # @codedoc_comment_block vm@vame_meta_get
  # @codedoc_comment_block news("vm@vame_meta_get", "2023-12-12", "0.2.2")
  # New function `vm@vame_meta_get`.
  # @codedoc_comment_block news("vm@vame_meta_get", "2023-12-12", "0.2.2")
  dbc::assert_is_character_nonNA_vector(meta_nm)
  vl <- vame_list_get(vm = vm)
  dbc::assert_atom_is_in_set(meta_nm, set = names(vl))
  vl[[meta_nm]]
}
vame_meta_set <- function(
  vm,
  meta_nm,
  value
) {
  # @codedoc_comment_block vm@vame_meta_set
  # Assign an element of `vame_meta_set`.
  # @codedoc_comment_block vm@vame_meta_set
  # @codedoc_comment_block news("vm@vame_meta_set", "2023-12-12", "0.2.2")
  # New function `vm@vame_meta_set`.
  # @codedoc_comment_block news("vm@vame_meta_set", "2023-12-12", "0.2.2")
  dbc::assert_is_character_nonNA_vector(meta_nm)
  # @codedoc_comment_block news("vm@vame_meta_set", "2024-09-02", "0.5.6")
  # `vm@vame_meta_set` now checks `value` for validity for "officially"
  # defined metadata such as `sampler`. Formerly this was done only by
  # the corresponding wrapper such as `vm@vame_value_space_sampler_set`.
  # @codedoc_comment_block news("vm@vame_meta_set", "2024-09-02", "0.5.6")
  assert_meta(vm = vm, x = value, meta_nm = meta_nm, must_exist = FALSE)
  vl <- vame_list_get(vm = vm)
  vl[[meta_nm]] <- value
  vame_list_set(vm = vm, value = vl)
}

vame_value_space_sampler_get <- function(
  vm
) {
  # @codedoc_comment_block vm@vame_value_space_sampler_get
  # Retrieve `meta_list$sampler`.
  # @codedoc_comment_block vm@vame_value_space_sampler_get

  # @codedoc_comment_block news("vm@vame_value_space_sampler_get", "2023-12-12", "0.2.2")
  # New function `vm@vame_value_space_sampler_get`.
  # @codedoc_comment_block news("vm@vame_value_space_sampler_get", "2023-12-12", "0.2.2")

  # @codedoc_comment_block feature_funs(random sampling)
  # - `vm@vame_value_space_sampler_get`
  # @codedoc_comment_block feature_funs(random sampling)
  out <- vame_meta_get(vm = vm, meta_nm = "sampler")
  assert_is_value_space_sampler(out, assertion_type = "prod_output")
  return(out)
}
vame_value_space_sampler_set <- function(
  vm,
  value
) {
  # @codedoc_comment_block vm@vame_value_space_sampler_set
  # Assign `meta_list$sampler`.
  #
  # @codedoc_insert_comment_block specification(vame_list$sampler)
  #
  # See the documentation for `vame_value_space_sample` to understand how
  # the `sampler` object is used.
  # @codedoc_comment_block vm@vame_value_space_sampler_set

  # @codedoc_comment_block news("vm@vame_value_space_sampler_set", "2023-12-12", "0.2.2")
  # New function `vm@vame_value_space_sampler_set`.
  # @codedoc_comment_block news("vm@vame_value_space_sampler_set", "2023-12-12", "0.2.2")

  # @codedoc_comment_block feature_funs(random sampling)
  # - `vm@vame_value_space_sampler_set`
  # @codedoc_comment_block feature_funs(random sampling)
  vame_meta_set(vm = vm, meta_nm = "sampler", value = value)
}
vame_value_space_sample <- function(
  vm,
  ids = NULL,
  n = 1L,
  data = NULL,
  var_nms = NULL,
  env = NULL
) {
  # @codedoc_comment_block news("vm@vame_value_space_sample", "2023-12-12", "0.2.2")
  # New function `vm@var_set_meta_is_defined`.
  # @codedoc_comment_block news("vm@vame_value_space_sample", "2023-12-12", "0.2.2")

  # @codedoc_comment_block feature_funs(random sampling)
  # - `vm@vame_value_space_sample`
  # @codedoc_comment_block feature_funs(random sampling)

  # @codedoc_comment_block news("vm@vame_value_space_sample", "2024-02-27", "0.4.0")
  # `vm@vame_value_space_sample` gains arguments `ids` and `data`.
  # @codedoc_comment_block news("vm@vame_value_space_sample", "2024-02-27", "0.4.0")
  # @codedoc_comment_block news("vm@vame_value_space_sample", "2024-10-14", "1.3.1")
  # `vm@vame_value_space_sample` arg `ids` auto-inference now only finds
  # `ids` which have either `sampler` or `value_space`.
  # @codedoc_comment_block news("vm@vame_value_space_sample", "2024-10-14", "1.3.1")
  handle_arg_ids_et_var_nms_inplace__(
    vm,
    required_meta_nms = c("sampler", "value_space"),
    require_meta_style = "or"
  )
  dbc::assert_has_one_of_classes(
    env,
    classes = c("NULL", "environment")
  )
  if (is.null(env)) {
    env <- parent.frame(1L)
  }
  dbc::assert_is_integer_nonNA_gtzero_atom(n)
  arg_list <- list(
    x = vm,
    ids = ids,
    n = n,
    data = handle_arg_data__(data),
    var_nms = var_nms,
    vm = vm
  )
  # @codedoc_comment_block vm@vame_value_space_sample
  # If `vame_list$sampler` has not been set, `vm@vame_value_space_sample`
  # calls `vm@vame_value_space_sample_default`.
  # @codedoc_comment_block vm@vame_value_space_sample
  sampler <- vame_value_space_sample_default
  if (vame_meta_is_defined(vm = vm, meta_nm = "sampler")) {
    sampler <- vame_value_space_sampler_get(vm = vm)
  }
  if (is.function(sampler)) {
    # @codedoc_comment_block vm@vame_value_space_sample
    # Take `n` random samples of the value spaces of `ids`.
    #
    # If `vame_list$sampler` is a function, it is called via
    # `vame_list$sampler(x = vm, ids = ids, n = n, data = data, var_nms = var_nms)`,
    # where `vm` is the pertinent `VariableMetadata` object itself.
    # @codedoc_comment_block vm@vame_value_space_sample
    arg_list <- arg_list[intersect(names(arg_list), names(formals(sampler)))]
    sample <- do.call(sampler, arg_list, quote = TRUE)
  } else if (is.call(sampler)) {
    # @codedoc_comment_block vm@vame_value_space_sample
    # If `vame_list$sampler` is a `call` object, it is evaluated in a new
    # environment whose parent is `env`. This new environment contains the
    # same objects that would be passed as arguments when calling
    # `vame_list$sampler` as a `function`.
    # @codedoc_comment_block vm@vame_value_space_sample
    call_eval_env <- as.environment(arg_list)
    parent.env(call_eval_env) <- env
    sample <- eval(sampler, envir = call_eval_env)
  }
  # @codedoc_comment_block vm@vame_value_space_sample
  # `vm@vame_value_space_sample` always returns a `data.table` with `n` rows.
  # If `var_nms` was supplied, only those columns are in the output in the
  # given order.
  # Else all columns for each variable set identified by `ids` are present.
  # @codedoc_comment_block vm@vame_value_space_sample
  dbc::assert_prod_output_is_data_table(sample)
  data.table::setcolorder(sample, var_nms)
  dbc::assert_prod_output_is_identical(
    x = names(sample),
    y = var_nms
  )
  dbc::assert_prod_output_is_identical(
    x = nrow(sample),
    y = n
  )
  return(sample[])
}

vame_value_space_sample_default <- function(
  vm,
  ids = NULL,
  env = NULL,
  var_nms = NULL,
  data = NULL,
  n = 1L
) {
  # @codedoc_comment_block news("vm@vame_value_space_sample_default", "2023-12-12", "0.2.2")
  # New function `vm@vame_value_space_sample_default`.
  # @codedoc_comment_block news("vm@vame_value_space_sample_default", "2023-12-12", "0.2.2")

  # @codedoc_comment_block feature_funs(random sampling)
  # - `vm@vame_value_space_sample_default`
  # @codedoc_comment_block feature_funs(random sampling)

  # @codedoc_comment_block news("vm@vame_value_space_sample_default", "2024-02-27", "0.4.0")
  # `vm@vame_value_space_sample_default` gains arguments `ids` and `data`.
  # @codedoc_comment_block news("vm@vame_value_space_sample_default", "2024-02-27", "0.4.0")
  # @codedoc_comment_block news("vm@vame_value_space_sample_default", "2024-10-14", "1.3.1")
  # `vm@vame_value_space_sample_default` arg `ids` auto-inference now only finds
  # `ids` which have either `sampler` or `value_space`.
  # @codedoc_comment_block news("vm@vame_value_space_sample_default", "2024-10-14", "1.3.1")
  handle_arg_ids_et_var_nms_inplace__(
    vm = vm,
    required_meta_nms = c("sampler", "value_space"),
    require_meta_style = "or"
  )
  data <- handle_arg_data__(data)
  dbc::assert_has_one_of_classes(
    env,
    classes = c("NULL", "environment")
  )
  if (is.null(env)) {
    env <- parent.frame(1L)
  }
  dbc::assert_is_integer_nonNA_gtzero_atom(n)

  # @codedoc_comment_block vm@vame_value_space_sample_default
  # `vm@vame_value_space_sample_default`
  # calls `vm@var_set_value_space_sample` for every pertinent variable set
  # and combines the results into one large `data.table`.
  # @codedoc_comment_block vm@vame_value_space_sample_default
  sample_dt <- local({
    if (is.null(data)) {
      data <- list()
    }
    idx_col_nm <- "INDEX_______________________________________________________"
    sample_dt <- data.table::data.table(
      idx = seq_len(n)
    )
    data.table::setnames(sample_dt, "idx", idx_col_nm)
    for (id in ids) {
      data[names(sample_dt)] <- sample_dt
      data[idx_col_nm] <- NULL
      var_nms_id <- intersect(
        var_set_var_nm_set_get(vm = vm, id = id),
        var_nms
      )
      sampled <- var_set_value_space_sample(
        vm = vm,
        id = id,
        env = env,
        n = n,
        var_nms = var_nms_id,
        data = data
      )
      data.table::set(
        sample_dt,
        j = names(sampled),
        value = sampled
      )
    }
    data.table::set(sample_dt, j = idx_col_nm, value = NULL)
    sample_dt[]
  })

  # @codedoc_comment_block vm@vame_value_space_sample_default
  # `vm@vame_value_space_sample_default` always returns a `data.table` with `n`
  # rows.
  # @codedoc_comment_block vm@vame_value_space_sample_default
  dbc::assert_prod_output_is_data_table(sample_dt)
  data.table::setcolorder(sample_dt, var_nms)
  dbc::assert_prod_output_is_identical(
    x = names(sample_dt),
    y = var_nms
  )
  dbc::assert_prod_output_is_identical(
    x = nrow(sample_dt),
    y = n
  )
  return(sample_dt[])
}

# vame_list functions ----------------------------------------------------------
vame_list_print <- function(vm) {
  # @codedoc_comment_block vm@vame_list_print
  # Run `print(summary(meta_list))`.
  # @codedoc_comment_block vm@vame_list_print

  # @codedoc_comment_block news("vm@vame_list_print", "2025-06-26", "1.10.4")
  # New function `vm@vame_list_print`.
  # @codedoc_comment_block news("vm@vame_list_print", "2025-06-26", "1.10.4")
  print(summary(vame_list_get(vm)))
}

vame_list_copy <- function(vm) {
  # @codedoc_comment_block vm@vame_list_copy
  # Get a deep copy of `meta_list`.
  # @codedoc_comment_block vm@vame_list_copy

  # @codedoc_comment_block news("vm@vame_list_copy", "2023-12-13", "0.2.2")
  # New function `vm@vame_list_copy`.
  # @codedoc_comment_block news("vm@vame_list_copy", "2023-12-13", "0.2.2")
  data.table::copy(data_obj_get(vm = vm, "vame_list"))
}

# vame_category_space funs -----------------------------------------------

vame_category_space_dt_list <- function(
  vm,
  var_nms,
  env = NULL
) {
  # @codedoc_comment_block vm@vame_category_space_dt_list
  # Get list of category space `data.table` objects.
  # @codedoc_comment_block vm@vame_category_space_dt_list
  # @codedoc_comment_block feature_funs(category spaces)
  # - `vm@vame_category_space_dt_list`
  # @codedoc_comment_block feature_funs(category spaces)

  dbc::assert_is_one_of(
    env,
    funs = list(dbc::report_is_NULL,
                dbc::report_is_environment)
  )
  if (is.null(env)) {
    env <- parent.frame(1L)
  }
  vd <- vd_get(vm, c("var_nm", "type"))
  is_categorical <- vd[["type"]] == "categorical"
  dbc::assert_vector_elems_are_in_set(
    var_nms,
    set = vd[["var_nm"]][is_categorical]
  )
  dtl <- category_space_dt_list__(
    vm = vm,
    var_nms = var_nms,
    env = env
  )
  return(dtl)
}


vame_category_space_dt <- function(
  vm,
  var_nms,
  env = NULL
) {
  # @codedoc_comment_block vm@vame_category_space_dt
  # Get a category space `data.table`.
  # @codedoc_comment_block vm@vame_category_space_dt

  # @codedoc_comment_block feature(category spaces)
  # The category spaces feature becomes available when the value spaces feature
  # is available and when
  # the `var_dt` of a `VariableMetadata` object has a `type` column, with
  # value `"categorical"` for at least one variable.
  # This features differs from the value spaces feature by being more specific
  # and being able to produce one (large) `data.table` of allowed variable
  # value combinations.
  #
  # @codedoc_insert_comment_block specification(var_dt$type)
  #
  # This feature relies on the value spaces feature. See the documentation
  # for that feature for more information.
  #
  # The following functions are part of this feature:
  # @codedoc_insert_comment_block feature_funs(category spaces)
  #
  # @codedoc_comment_block feature(category spaces)

  # @codedoc_comment_block feature_funs(category spaces)
  # - `vm@vame_category_space_dt`
  # @codedoc_comment_block feature_funs(category spaces)
  if (is.null(env)) {
    env <- parent.frame(1L)
  }
  dtl <- vame_category_space_dt_list(vm, var_nms = var_nms, env = env)
  dt <- category_space_dt_list_to_category_space_dt__(dtl)
  return(dt[])
}
