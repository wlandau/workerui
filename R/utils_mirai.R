mirai_status <- function(profile, seconds_interval, seconds_timeout) {
  envir <- new.env(parent = emptyenv())
  iterate <- function() {
    status <- mirai::status(.compute = profile)
    valid <- is.list(status)
    retry <- is.numeric(status) && identical(as.integer(status), 5L)
    if_any(
      valid || retry,
      NULL,
      mirai_status_error(status = status, profile = profile)
    )
    envir$status <- status
    envir$valid <- valid
    valid
  }
  crew_retry(
    fun = iterate,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    error = FALSE,
    assertions = FALSE
  )
  status <- .subset2(envir, "status")
  valid <- .subset2(envir, "valid")
  if_any(valid, status, mirai_status_error(status, profile))
}

mirai_status_error <- function(status, profile) {
  message <- sprintf("'errorValue' int %s\n", nanonext::nng_error(status))
  pid <- mirai::nextget("pid", .compute = profile)
  exists <- !is.null(pid) &&
    !inherits(
      try(handle <- ps::ps_handle(pid = pid), silent = TRUE),
      "try-error"
    )
  lines_common <- c(
    "Please also try upgrading R packages {nanonext}, {mirai}, and {crew}",
    "to their latest versions on CRAN. (Likewise with {targets} if you are",
    "using it.) Upgrading these packages solves many kinds of errors.",
    "\n\nAnother possibility is an out-of-memory error.",
    "The dispatcher process can run out of memory if it is overwhelmed with",
    "data objects too large or too many to comfortably fit inside a single",
    "R process. Please read",
    "https://wlandau.github.io/crew/articles/logging.html to learn",
    "about proactive resource usage logging which can provide useful",
    "data in the event of a crash.",
    "\n\nIf memory consumption is a problem, then as a workaround,",
    "you could make each task save/load files instead of using/returning",
    "large R objects in memory.",
    "If you are using {targets}, you might consider",
    "storage = \"worker\", retrieval = \"worker\", and/or",
    "cloud storage as documented at",
    "https://books.ropensci.org/targets/performance.html and",
    "https://books.ropensci.org/targets/cloud-storage.html."
  )
  info <- if_any(
    exists,
    sprintf(
      paste(
        c(
          "A {mirai} dispatcher process is running at pid %s with status",
          "\"%s\". There may be a network connection issue,",
          "or the dispatcher is simply busy managing lots of tasks or",
          "uploading/downloading large datasets. Maybe try increasing",
          "seconds_timeout in your {crew} controller.\n\n",
          lines_common
        ),
        collapse = " "
      ),
      pid,
      ps::ps_status(handle)
    ),
    paste(
      c(
        "The {mirai} dispatcher is not running. If you are using {crew}",
        "without {targets}, be sure to call the start() method of the",
        "controller before doing anything else. If you already did,",
        "or if you are using {targets}, then the dispatcher process probably",
        "started and then failed.\n\n",
        lines_common
      ),
      collapse = " "
    )
  )
  crew_error(paste(message, info))
}

mirai_resolved <- function(task) {
  !is_mirai(task) || !nanonext::.unresolved(task)
}

mirai_resolve <- function(task, launching) {
  if (mirai::is_mirai(task)) {
    mirai::call_mirai(task)
    mirai_assert(task, launching)
    task$data
  } else {
    task
  }
}

mirai_wait <- function(tasks, launching) {
  mirai::call_mirai(tasks)
  lapply(tasks, mirai_assert, launching = launching)
  invisible()
}

mirai_assert <- function(task, launching) {
  if (!mirai::is_mirai(task)) {
    return()
  }
  data <- .subset2(task, "data")
  if (mirai::is_mirai_error(data)) {
    if (launching) {
      crew_error(
        message = paste(
          "Error asynchronously launching a worker:",
          as.character(data)
        )
      )
    } else {
      crew_warning(
        message = paste(
          "Error asynchronously terminating a worker:",
          as.character(data)
        )
      )
    }
  }
}
