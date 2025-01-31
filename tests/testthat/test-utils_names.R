crew_test("is_named", {
  expect_true(is_named(NULL))
  expect_true(is_named(list()))
  expect_true(is_named(list(a = 1)))
  expect_true(is_named(list(a = 1, b = 1)))
  expect_false(is_named(list(a = 1, a = 1)))
  expect_false(is_named(list(a = 1, 2)))
})

crew_test("eval_tidyselect()", {
  expr <- rlang::new_quosure(quote(tidyselect::any_of(c("b", "c"))))
  out <- eval_tidyselect(expr = expr, choices = letters[seq_len(4L)])
  expect_equal(out, c("b", "c"))
  out <- eval_tidyselect(expr = expr, choices = character(0))
  expect_equal(out, character(0L))
  out <- eval_tidyselect(expr = rlang::new_quosure(NULL), choices = "x")
  expect_equal(out, character(0L))
})

crew_test("name_task_tempfile()", {
  expect_true(is.character(name_task_tempfile()))
})

crew_test("name_task_nanonext()", {
  expect_true(is.character(name_task_nanonext()))
})
