testthat::test_that("vm@vame_maker_dag_get and vm@vame_maker_dag_print work", {
  vm <- vame::VariableMetadata(
    var_dt = data.table::data.table(
      var_nm = c("x", "y", "z", "w")
    ),
    var_set_dt = data.table::data.table(
      id = c("x", "y", "z", "w"),
      var_nm_set = list(x = "x", y = "y", z = "z", w = "w"),
      maker = list(
        x = NULL,
        y = list(
          maker = quote(data.table::data.table(y = x + 1L)),
          dep_var_nm_set = "x"
        ),
        z = list(
          maker = quote(data.table::data.table(z = y + 1L)),
          dep_var_nm_set = "y"
        ),
        w = function(x, vm) data.table::data.table(w = x - 1L)
      )
    )
  )
  dag <- vm@vame_maker_dag_get()
  testthat::expect_identical(dag[["id"]], c("y", "z", "w"))
  testthat::expect_identical(dag[["from_var_nm"]], c("x", "y", "x"))
  testthat::expect_identical(dag[["to_var_nm"]], c("y", "z", "w"))

  testthat::expect_output(
    lines <- vm@vame_maker_dag_print(),
    "x -> y -> z\nx -> w"
  )
  testthat::expect_identical(lines, c("x -> y -> z", "x -> w"))
  testthat::expect_output(
    lines <- vm@vame_maker_dag_print(ids = "z"),
    "x -> y -> z"
  )
  testthat::expect_identical(lines, "x -> y -> z")
  testthat::expect_output(
    lines <- vm@vame_maker_dag_print(var_nms = "w"),
    "x -> w"
  )
  testthat::expect_identical(lines, "x -> w")
})

testthat::test_that("vm@vame_maker_dag_print handles no makers and cycles", {
  vm <- vame::VariableMetadata(
    var_dt = data.table::data.table(var_nm = "a"),
    var_set_dt = data.table::data.table(id = "a", var_nm_set = list("a"))
  )
  testthat::expect_identical(nrow(vm@vame_maker_dag_get()), 0L)
  testthat::expect_message(
    lines <- vm@vame_maker_dag_print(),
    "No `maker`s"
  )
  testthat::expect_identical(lines, character(0L))

  vm <- vame::VariableMetadata(
    var_dt = data.table::data.table(var_nm = c("a", "b")),
    var_set_dt = data.table::data.table(
      id = c("a", "b"),
      var_nm_set = list("a", "b"),
      maker = list(
        a = list(dep_var_nm_sets = list("b", "q"), maker = quote(f(b))),
        b = list(dep_var_nm_set = "a", maker = quote(f(a)))
      )
    )
  )
  testthat::expect_output(
    lines <- vm@vame_maker_dag_print(),
    "b | q -> a -> b | q -> a (cycle)",
    fixed = TRUE
  )
  testthat::expect_identical(lines, "b | q -> a -> b | q -> a (cycle)")
})

testthat::test_that("vm@vame_maker_dag_print groups and separates dep sets", {
  vm <- vame::VariableMetadata(
    var_dt = data.table::data.table(
      var_nm = c("x1", "x2", "x3", "y", "z", "a", "b")
    ),
    var_set_dt = data.table::data.table(
      id = c("y", "z", "ab"),
      var_nm_set = list(y = "y", z = "z", ab = c("a", "b")),
      maker = list(
        y = list(
          maker = quote(f(x1, x2, x3)),
          dep_var_nm_sets = list(c("x1", "x2"), "x3")
        ),
        z = list(
          maker = quote(f(y, x3)),
          dep_var_nm_set = c("y", "x3")
        ),
        ab = list(
          maker = quote(f(z)),
          dep_var_nm_set = "z"
        )
      )
    )
  )
  testthat::expect_output(
    lines <- vm@vame_maker_dag_print(),
    "(x1, x2) | x3 -> (y, x3) -> z -> a",
    fixed = TRUE
  )
  testthat::expect_identical(
    lines,
    c(
      "(x1, x2) | x3 -> (y, x3) -> z -> a",
      "(x1, x2) | x3 -> (y, x3) -> z -> b",
      "(y, x3) -> z -> a",
      "(y, x3) -> z -> b"
    )
  )
  testthat::expect_output(
    lines <- vm@vame_maker_dag_print(style = "maker"),
    "y <- (x1, x2) | x3",
    fixed = TRUE
  )
  testthat::expect_identical(
    lines,
    c("y <- (x1, x2) | x3", "z <- (y, x3)", "(a, b) <- z")
  )
  testthat::expect_error(vm@vame_maker_dag_print(style = "tree"))
})
