crew_test("mirai_status()", {
  skip_on_cran()
  skip_on_os("windows")
  out <- mirai_status(
    profile = "none",
    seconds_interval = 1L,
    seconds_timeout = 15L
  )
  expect_true(is.list(out))
})

crew_test("mirai_status_error()", {
  skip_on_cran()
  expect_crew_error(mirai_status_error(status = 5L, profile = "default"))
})

crew_test("mirai_resolved()", {
  skip_on_cran()
  skip_on_os("windows")
  expect_true(mirai_resolved(TRUE))
  task <- mirai::mirai(TRUE)
  mirai::call_mirai(task)
  expect_true(mirai_resolved(task))
  ask <- mirai::mirai(stop("abc"))
  mirai::call_mirai(task)
  expect_true(mirai_resolved(task))
  task <- mirai::mirai(Sys.sleep(1))
  expect_false(mirai_resolved(task))
  mirai::call_mirai(task)
})

crew_test("mirai_resolve()", {
  skip_on_cran()
  skip_on_os("windows")
  expect_equal(mirai_resolve(list("abc"), launching = TRUE), list("abc"))
  expect_equal(
    mirai_resolve(mirai::mirai(list(a = 1L, b = 2L)), launching = TRUE),
    list(a = 1L, b = 2L)
  )
  expect_crew_error(
    mirai_resolve(
      task = mirai::mirai({
        Sys.sleep(0.5)
        stop("abc")
      }),
      launching = TRUE
    )
  )
})

crew_test("mirai_wait()", {
  skip_on_cran()
  skip_on_os("windows")
  tasks <- replicate(2L, mirai::mirai(TRUE), simplify = FALSE)
  expect_silent(mirai_wait(tasks, launching = TRUE))
  expect_crew_error(
    mirai_wait(
      tasks = list(
        mirai::mirai(TRUE),
        mirai::mirai({
          Sys.sleep(1)
          stop("abc")
        })
      ),
      launching = TRUE
    )
  )
})

crew_test("mirai_assert()", {
  skip_on_cran()
  skip_on_os("windows")
  expect_silent(mirai_assert(task = "task", launching = TRUE))
  task <- mirai::mirai(TRUE)
  mirai::call_mirai(task)
  expect_silent(mirai_assert(task, launching = TRUE))
  task <- mirai::mirai(stop("message"))
  mirai::call_mirai(task)
  expect_crew_error(mirai_assert(task, launching = TRUE))
  expect_warning(
    mirai_assert(task, launching = FALSE),
    class = "crew_warning"
  )
})
