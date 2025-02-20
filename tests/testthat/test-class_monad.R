crew_test("empty monad is valid", {
  expect_silent(monad_validate(monad_init()))
})

crew_test("bad name", {
  out <- monad_init()
  out$name <- 0
  expect_crew_error(monad_validate(out))
})

crew_test("bad field", {
  out <- monad_init()
  out$nope <- 0
  expect_crew_error(monad_validate(out))
})

crew_test("as_monad()", {
  skip_on_cran()
  on.exit(mirai::daemons(n = 0L))
  task <- structure(list(data = list(2L)), class = c("mirai", "recvAio"))
  expect_equal(as_monad(task = task, name = "x", controller = "y"), list(2L))
  mirai::daemons(n = 1L, autoexit = crew_terminate_signal())
  task <- mirai::mirai(stop("error_message"))
  mirai::call_mirai(task)
  monad <- as_monad(task = task, name = "x", controller = "y")
  expect_equal(monad$name, "x")
  expect_equal(monad$result, list(NA))
  expect_equal(monad$status, "error")
  expect_true(grepl("error_message", monad$error))
  expect_equal(monad$code, -1L)
  task <- mirai::mirai(Sys.sleep(300))
  mirai::stop_mirai(task)
  monad <- as_monad(task = task, name = "x", controller = "y")
  expect_equal(monad$name, "x")
  expect_equal(monad$result, list(NA))
  expect_equal(monad$status, "cancel")
  expect_true(grepl("cancel", tolower(monad$error)))
  expect_equal(monad$code, 20L)
  task <- mirai::mirai(Sys.sleep(300))
  Sys.sleep(0.25)
  mirai::daemons(n = 0L)
  monad <- as_monad(task = task, name = "x", controller = "y")
  expect_equal(monad$name, "x")
  expect_equal(monad$result, list(NA))
  expect_equal(monad$status, "crash")
  expect_true(grepl("connection reset", tolower(monad$error)))
  expect_equal(monad$code, 19L)
})
