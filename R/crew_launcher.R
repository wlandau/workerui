#' @title Create an abstract launcher.
#' @export
#' @family launcher
#' @description This function is useful for inheriting argument documentation
#'   in functions that create custom third-party launchers. See
#'   `@inheritParams crew::crew_launcher` in the source code file of
#'   [crew_launcher_local()].
#' @inheritParams crew_client
#' @inheritParams crew_worker
#' @param name Character string, name of the launcher. If the name is
#'   `NULL`, then a name is automatically generated when the launcher starts.
#' @param workers Maximum number of workers to run concurrently
#'   when auto-scaling, excluding task retries and manual calls to `launch()`.
#'   Special workers allocated for task retries
#'   do not count towards this limit, so the number of workers running at a
#'   given time may exceed this maximum.
#'   A smaller number of workers may run if the number of executing tasks
#'   is smaller than the supplied value of the `workers` argument.
#' @param seconds_interval Number of seconds between
#'   polling intervals waiting for certain internal
#'   synchronous operations to complete. In certain cases, exponential
#'   backoff is used with this argument passed to `seconds_max`
#'   in a [crew_throttle()] object.
#' @param seconds_timeout Number of seconds until timing
#'   out while waiting for certain synchronous operations to complete,
#'   such as checking `mirai::status()`.
#' @param seconds_launch Seconds of startup time to allow.
#'   A worker is unconditionally assumed to be alive
#'   from the moment of its launch until `seconds_launch` seconds later.
#'   After `seconds_launch` seconds, the worker is only
#'   considered alive if it is actively connected to its assign websocket.
#' @param seconds_idle Maximum number of seconds that a worker can idle
#'   since the completion of the last task. If exceeded, the worker exits.
#'   But the timer does not launch until `tasks_timers` tasks
#'   have completed.
#'   See the `idletime` argument of `mirai::daemon()`. `crew` does not
#'   excel with perfectly transient workers because it does not micromanage
#'   the assignment of tasks to workers, so please allow enough idle
#'   time for a new worker to be delegated a new task.
#' @param seconds_wall Soft wall time in seconds.
#'   The timer does not launch until `tasks_timers` tasks
#'   have completed.
#'   See the `walltime` argument of `mirai::daemon()`.
#' @param seconds_exit Deprecated on 2023-09-21 in version 0.5.0.9002.
#'   No longer necessary.
#' @param tasks_max Maximum number of tasks that a worker will do before
#'   exiting. See the `maxtasks` argument of `mirai::daemon()`.
#'   `crew` does not
#'   excel with perfectly transient workers because it does not micromanage
#'   the assignment of tasks to workers, it is recommended to set
#'   `tasks_max` to a value greater than 1.
#' @param tasks_timers Number of tasks to do before activating
#'   the timers for `seconds_idle` and `seconds_wall`.
#'   See the `timerstart` argument of `mirai::daemon()`.
#' @param reset_globals `TRUE` to reset global environment
#'   variables between tasks, `FALSE` to leave them alone.
#' @param reset_packages `TRUE` to unload any packages loaded during
#'   a task (runs between each task), `FALSE` to leave packages alone.
#' @param reset_options `TRUE` to reset global options to their original
#'   state between each task, `FALSE` otherwise. It is recommended to
#'   only set `reset_options = TRUE` if `reset_packages` is also `TRUE`
#'   because packages sometimes rely on options they set at loading time.
#' @param garbage_collection `TRUE` to run garbage collection between
#'   tasks, `FALSE` to skip.
#' @param crashes_error Deprecated on 2025-01-13 (`crew` version 0.10.2.9002).
#' @param launch_max Deprecated on 2024-11-04 (`crew` version 0.10.2.9002).
#'   Use `crashes_error` instead.
#' @param processes `NULL` or positive integer of length 1,
#'   number of local processes to
#'   launch to allow worker launches to happen asynchronously. If `NULL`,
#'   then no local processes are launched. If 1 or greater, then the launcher
#'   starts the processes on `start()` and ends them on `terminate()`.
#'   Plugins that may use these processes should run asynchronous calls
#'   using `launcher$async$eval()` and expect a `mirai` task object
#'   as the return value.
#' @param r_arguments Optional character vector of command line arguments
#'   to pass to `Rscript` (non-Windows) or `Rscript.exe` (Windows)
#'   when starting a worker. Example:
#'   `r_arguments = c("--vanilla", "--max-connections=32")`.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' client <- crew_client()
#' client$start()
#' launcher <- crew_launcher_local()
#' launcher$start(url = client$url, profile = client$profile)
#' launcher$launch()
#' task <- mirai::mirai("result", .compute = client$profile)
#' mirai::call_mirai(task)
#' task$data
#' client$terminate()
#' }
crew_launcher <- function(
  name = NULL,
  workers = 1L,
  seconds_interval = 1,
  seconds_timeout = 60,
  seconds_launch = 30,
  seconds_idle = 300,
  seconds_wall = Inf,
  seconds_exit = NULL,
  tasks_max = Inf,
  tasks_timers = 0L,
  reset_globals = TRUE,
  reset_packages = FALSE,
  reset_options = FALSE,
  garbage_collection = FALSE,
  crashes_error = NULL,
  launch_max = NULL,
  tls = crew::crew_tls(),
  processes = NULL,
  r_arguments = c("--no-save", "--no-restore"),
  options_metrics = crew::crew_options_metrics()
) {
  crew_deprecate(
    name = "seconds_exit",
    date = "2023-09-21",
    version = "0.5.0.9002",
    alternative = "none (no longer necessary)",
    condition = "warning",
    value = seconds_exit
  )
  crew_assert(
    inherits(tls, "crew_class_tls"),
    message = "argument tls must be an object created by crew_tls()"
  )
  crew_deprecate(
    name = "crashes_error",
    date = "2025-01-13",
    version = "0.10.2.9002",
    alternative = "none",
    condition = "message",
    value = crashes_error
  )
  crew_deprecate(
    name = "launch_max",
    date = "2024-11-04",
    version = "0.10.1.9000",
    alternative = "none",
    condition = "warning",
    value = launch_max
  )
  crew_class_launcher$new(
    name = name,
    workers = workers,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    seconds_launch = seconds_launch,
    seconds_idle = seconds_idle,
    seconds_wall = seconds_wall,
    tasks_max = tasks_max,
    tasks_timers = tasks_timers,
    reset_globals = reset_globals,
    reset_packages = reset_packages,
    reset_options = reset_options,
    garbage_collection = garbage_collection,
    tls = tls,
    processes = processes,
    r_arguments = r_arguments,
    options_metrics = options_metrics
  )
}

launcher_empty_instances <- tibble::tibble(
  handle = list(),
  id = integer(0L),
  start = numeric(0L),
  submitted = logical(0L),
  online = logical(0L),
  discovered = logical(0L)
)

#' @title Launcher abstract class
#' @export
#' @family launcher
#' @description `R6` abstract class to build other subclasses
#'   which launch and manage workers.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' client <- crew_client()
#' client$start()
#' launcher <- crew_launcher_local()
#' launcher$start(url = client$url, profile = client$profile)
#' launcher$launch()
#' task <- mirai::mirai("result", .compute = client$profile)
#' mirai::call_mirai(task)
#' task$data
#' client$terminate()
#' }
crew_class_launcher <- R6::R6Class(
  classname = "crew_class_launcher",
  cloneable = FALSE,
  portable = TRUE,
  private = list(
    .name = NULL,
    .workers = NULL,
    .seconds_interval = NULL,
    .seconds_timeout = NULL,
    .seconds_launch = NULL,
    .seconds_idle = NULL,
    .seconds_wall = NULL,
    .tasks_max = NULL,
    .tasks_timers = NULL,
    .reset_globals = NULL,
    .reset_packages = NULL,
    .reset_options = NULL,
    .garbage_collection = NULL,
    .tls = NULL,
    .processes = NULL,
    .r_arguments = NULL,
    .options_metrics = NULL,
    .url = NULL,
    .profile = NULL,
    .instances = launcher_empty_instances,
    .id = NULL,
    .async = NULL,
    .throttle = NULL
  ),
  active = list(
    #' @field name See [crew_launcher()].
    name = function() {
      .subset2(private, ".name")
    },
    #' @field workers See [crew_launcher()].
    workers = function() {
      .subset2(private, ".workers")
    },
    #' @field seconds_interval See [crew_launcher()].
    seconds_interval = function() {
      .subset2(private, ".seconds_interval")
    },
    #' @field seconds_timeout See [crew_launcher()].
    seconds_timeout = function() {
      .subset2(private, ".seconds_timeout")
    },
    #' @field seconds_launch See [crew_launcher()].
    seconds_launch = function() {
      .subset2(private, ".seconds_launch")
    },
    #' @field seconds_idle See [crew_launcher()].
    seconds_idle = function() {
      .subset2(private, ".seconds_idle")
    },
    #' @field seconds_wall See [crew_launcher()].
    seconds_wall = function() {
      .subset2(private, ".seconds_wall")
    },
    #' @field tasks_max See [crew_launcher()].
    tasks_max = function() {
      .subset2(private, ".tasks_max")
    },
    #' @field tasks_timers See [crew_launcher()].
    tasks_timers = function() {
      .subset2(private, ".tasks_timers")
    },
    #' @field reset_globals See [crew_launcher()].
    reset_globals = function() {
      .subset2(private, ".reset_globals")
    },
    #' @field reset_packages See [crew_launcher()].
    reset_packages = function() {
      .subset2(private, ".reset_packages")
    },
    #' @field reset_options See [crew_launcher()].
    reset_options = function() {
      .subset2(private, ".reset_options")
    },
    #' @field garbage_collection See [crew_launcher()].
    garbage_collection = function() {
      .subset2(private, ".garbage_collection")
    },
    #' @field tls See [crew_launcher()].
    tls = function() {
      .subset2(private, ".tls")
    },
    #' @field processes See [crew_launcher()].
    #'   asynchronously.
    processes = function() {
      .subset2(private, ".processes")
    },
    #' @field r_arguments See [crew_launcher()].
    r_arguments = function() {
      .subset2(private, ".r_arguments")
    },
    #' @field options_metrics See [crew_launcher()].
    options_metrics = function() {
      .subset2(private, ".options_metrics")
    },
    #' @field url Websocket URL for worker connections.
    url = function() {
      .subset2(private, ".url")
    },
    #' @field profile `mirai` compute profile of the launcher.
    profile = function() {
      .subset2(private, ".profile")
    },
    #' @field instances Data frame of worker instance information.
    instances = function() {
      .subset2(private, ".instances")
    },
    #' @field id Integer worker ID from the last call to `settings()`.
    id = function() {
      .subset2(private, ".id")
    },
    #' @field async A [crew_async()] object to run low-level launcher tasks
    #'   asynchronously.
    async = function() {
      .subset2(private, ".async")
    },
    #' @field throttle A [crew_throttle()] object to throttle scaling.
    throttle = function() {
      .subset2(private, ".throttle")
    }
  ),
  public = list(
    #' @description Launcher constructor.
    #' @return An `R6` object with the launcher.
    #' @param name See [crew_launcher()].
    #' @param workers See [crew_launcher()].
    #' @param seconds_interval See [crew_launcher()].
    #' @param seconds_timeout See [crew_launcher()].
    #' @param seconds_launch See [crew_launcher()].
    #' @param seconds_idle See [crew_launcher()].
    #' @param seconds_wall See [crew_launcher()].
    #' @param seconds_exit See [crew_launcher()].
    #' @param tasks_max See [crew_launcher()].
    #' @param tasks_timers See [crew_launcher()].
    #' @param reset_globals See [crew_launcher()].
    #' @param reset_packages See [crew_launcher()].
    #' @param reset_options See [crew_launcher()].
    #' @param garbage_collection See [crew_launcher()].
    #' @param crashes_error See [crew_launcher()].
    #' @param launch_max Deprecated.
    #' @param tls See [crew_launcher()].
    #' @param processes See [crew_launcher()].
    #' @param r_arguments See [crew_launcher()].
    #' @param options_metrics See [crew_launcher()].
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' client <- crew_client()
    #' client$start()
    #' launcher <- crew_launcher_local()
    #' launcher$start(url = client$url, profile = client$profile)
    #' launcher$launch()
    #' task <- mirai::mirai("result", .compute = client$profile)
    #' mirai::call_mirai(task)
    #' task$data
    #' client$terminate()
    #' }
    initialize = function(
      name = NULL,
      workers = NULL,
      seconds_interval = NULL,
      seconds_timeout = NULL,
      seconds_launch = NULL,
      seconds_idle = NULL,
      seconds_wall = NULL,
      seconds_exit = NULL,
      tasks_max = NULL,
      tasks_timers = NULL,
      reset_globals = NULL,
      reset_packages = NULL,
      reset_options = NULL,
      garbage_collection = NULL,
      crashes_error = NULL,
      launch_max = NULL, # TODO: remove after deprecation period
      tls = NULL,
      processes = NULL,
      r_arguments = NULL,
      options_metrics = NULL
    ) {
      private$.name <- name %|||% crew_random_name(n = 4L)
      private$.workers <- workers
      private$.seconds_interval <- seconds_interval
      private$.seconds_timeout <- seconds_timeout
      private$.seconds_launch <- seconds_launch
      private$.seconds_idle <- seconds_idle
      private$.seconds_wall <- seconds_wall
      private$.tasks_max <- tasks_max
      private$.tasks_timers <- tasks_timers
      private$.reset_globals <- reset_globals
      private$.reset_packages <- reset_packages
      private$.reset_options <- reset_options
      private$.garbage_collection <- garbage_collection
      private$.tls <- tls
      private$.processes <- processes
      private$.r_arguments <- r_arguments
      private$.options_metrics <- options_metrics
    },
    #' @description Validate the launcher.
    #' @return `NULL` (invisibly).
    validate = function() {
      if (!is.null(private$.name)) {
        crew_assert(
          private$.name,
          is.character(.),
          length(.) == 1L,
          !anyNA(.),
          nzchar(.),
          message = c(
            "launcher name must be a nonempty non-missing character string"
          )
        )
      }
      crew_assert(
        is.function(self$launch_worker),
        message = "launch_worker() must be a function."
      )
      fields <- c("call", "name", "launcher", "worker")
      crew_assert(
        fields %in% names(formals(self$launch_worker)),
        message = paste(
          "launch_worker() must have the following arguments:",
          paste(sprintf("\"%s\"", fields), collapse = ", ")
        )
      )
      if (!is.null(self$terminate_worker)) {
        crew_assert(
          is.function(self$terminate_worker),
          message = "terminate_worker() must be a function or NULL."
        )
        crew_assert(
          "handle" %in% names(formals(self$terminate_worker)),
          message = "terminate_worker() must have a \"handle\" argument."
        )
      }
      fields <- c(
        "seconds_interval",
        "seconds_timeout",
        "seconds_launch",
        "seconds_idle",
        "seconds_wall",
        "tasks_max",
        "tasks_timers"
      )
      # TODO: require non-null workers field when plugins catch up.
      if (!is.null(self[["workers"]])) {
        fields <- c(fields, "workers")
      }
      for (field in fields) {
        crew_assert(
          self[[field]],
          is.numeric(.),
          . >= 0,
          length(.) == 1L,
          !anyNA(.),
          message = paste(field, "must be a non-missing numeric of length 1")
        )
      }
      crew_assert(
        private$.processes %|||% 1L,
        is.numeric(.),
        . > 0L,
        length(.) == 1L,
        !anyNA(.),
        message = "processes must be NULL or a positive integer of length 1"
      )
      fields <- c(
        "reset_globals",
        "reset_packages",
        "reset_options",
        "garbage_collection"
      )
      for (field in fields) {
        crew_assert(
          self[[field]], isTRUE(.) || isFALSE(.),
          message = paste(field, "must be a non-missing logical of length 1")
        )
      }
      # TODO: validate these fields when revdeps catch up.
      # nocov start
      if (FALSE) {
        if (!is.null(private$.url)) {
          crew_assert(
            private$.url,
            is.character(.),
            !anyNA(.),
            length(.) == 1L,
            nzchar(.)
          )
        }
        # nocov end
      }
      crew_assert(private$.instances, is.data.frame(.))
      if (!is.null(private$.r_arguments)) {
        crew_assert(
          is.character(private$.r_arguments),
          message = "r_arguments must be a character vector."
        )
      }
      crew_assert(
        inherits(private$.tls, "crew_class_tls"),
        message = "field 'tls' must be an object created by crew_tls()"
      )
      private$.tls$validate()
      if (!is.null(private$.async)) {
        crew_assert(
          inherits(private$.async, "crew_class_async"),
          message = "field 'async' must be an object created by crew_async()"
        )
        private$.async$validate()
      }
      if (!is.null(private$.throttle)) {
        crew_assert(
          inherits(private$.throttle, "crew_class_throttle"),
          message = "field 'throttle' must be an object from crew_throttle()"
        )
        private$.throttle$validate()
      }
      if (!is.null(private$.options_metrics)) {
        crew_options_metrics_validate(private$.options_metrics)
      }
      invisible()
    },
    #' @description Poll the throttle.
    #' @return `TRUE` to run whatever work comes next, `FALSE` to skip
    #'   until the appropriate time.
    poll = function() {
      private$.throttle$poll()
    },
    #' @description List of arguments for `mirai::daemon()`.
    #' @return List of arguments for `mirai::daemon()`.
    settings = function() {
      private$.id <- private$.id + 1L
      cleanup <- as.integer(isTRUE(private$.reset_globals)) +
        (2L * as.integer(isTRUE(private$.reset_packages))) +
        (4L * as.integer(isTRUE(private$.reset_options))) +
        (8L * as.integer(isTRUE(private$.garbage_collection)))
      list(
        url = private$.url,
        dispatcher = TRUE,
        asyncdial = FALSE,
        autoexit = crew_terminate_signal(),
        cleanup = cleanup,
        output = TRUE,
        maxtasks = private$.tasks_max,
        idletime = private$.seconds_idle * 1000,
        walltime = private$.seconds_wall * 1000,
        timerstart = private$.tasks_timers,
        id = private$.id,
        tls = private$.tls$worker(profile = private$.profile),
        rs = mirai::nextstream(.compute = private$.profile)
      )
    },
    #' @description Create a call to [crew_worker()] to
    #'   help create custom launchers.
    #' @return Character string with a call to [crew_worker()].
    #' @param worker Character string, name of the worker.
    #' @param socket Deprecated on 2025-01-28 (`crew` version 1.0.0).
    #' @param launcher Deprecated on 2025-01-28 (`crew` version 1.0.0).
    #' @param instance Deprecated on 2025-01-28 (`crew` version 1.0.0).
    #' @examples
    #' launcher <- crew_launcher_local()
    #' launcher$start(url = "tcp://127.0.0.1:57000", profile = "profile")
    #' launcher$call(worker = "worker_name")
    #' launcher$terminate()
    call = function(
      worker,
      socket = NULL,
      launcher = NULL,
      instance = NULL
    ) {
      call <- substitute(
        crew::crew_worker(
          settings = settings,
          launcher = launcher,
          worker = worker,
          options_metrics = crew::crew_options_metrics(
            path = path,
            seconds_interval = seconds_interval
          )
        ),
        env = list(
          settings = self$settings(),
          launcher = private$.name,
          worker = worker,
          path = private$.options_metrics$path,
          seconds_interval = private$.options_metrics$seconds_interval %|||% 5
        )
      )
      out <- deparse_safe(expr = call, collapse = " ")
      gsub(pattern = "   *", replacement = " ", x = out)
    },
    #' @description Start the launcher.
    #' @param url Character string, websocket URL for worker connections.
    #' @param profile Character string, `mirai` compute profile.
    #' @param sockets Deprecated on 2025-01-28 (`crew` version 1.0.0).
    #' @return `NULL` (invisibly).
    start = function(url = NULL, profile = NULL, sockets = NULL) {
      crew::crew_deprecate(
        name = "sockets",
        date = "2025-01-28",
        version = "1.0.0",
        alternative = "url and profile",
        condition = "message",
        value = sockets,
        skip_cran = TRUE
      )
      url <- url %|||% sockets[1L]
      profile <- profile %|||% crew_random_name()
      if (!is.null(private$.async)) {
        private$.async$terminate()
      }
      private$.async <- crew_async(workers = private$.processes)
      private$.async$start()
      private$.throttle <- crew_throttle(seconds_max = self$seconds_interval)
      private$.url <- url
      private$.profile <- profile
      private$.instances <- launcher_empty_instances
      private$.id <- 0L
      invisible()
    },
    #' @description Terminate the whole launcher, including all workers.
    #' @return `NULL` (invisibly).
    terminate = function() {
      private$.url <- NULL
      private$.profile <- NULL
      if (!is.null(private$.throttle)) {
        private$.throttle$reset()
      }
      using_async <- !is.null(private$.async) &&
        private$.async$asynchronous() &&
        private$.async$started()
      if (using_async) {
        on.exit(private$.async$terminate())
      }
      self$terminate_workers()
      invisible()
    },
    #' @description Resolve asynchronous worker submissions.
    #' @return `NULL` (invisibly). Throw an error if there were any
    #'   asynchronous worker submission errors.'
    resolve = function() {
      unresolved <- which(!private$.instances$submitted)
      handles <- private$.instances$handle
      for (index in unresolved) {
        handle <- .subset2(handles, index)
        if (mirai_resolved(handle)) {
          handle <- mirai_resolve(handle, launching = TRUE)
          private$.instances$handle[[index]] <- handle %||% crew_null
          private$.instances$submitted[index] <- TRUE
        }
      }
    },
    #' @description Update worker metadata, resolve asynchronous
    #'   worker submissions, and terminate lost workers.
    #' @return `NULL` (invisibly).
    #' @param status A `mirai` status list.
    update = function(status) {
      self$resolve()
      instances <- private$.instances
      id <- instances$id
      events <- as.integer(status$events)
      connected <- id %in% (events[events > 0L])
      disconnected <- id %in% (-events[events < 0L])
      instances$online[connected] <- TRUE
      instances$online[disconnected] <- FALSE
      instances$discovered[connected] <- TRUE
      online <- instances$online
      discovered <- instances$discovered
      start <- instances$start
      awaiting <- (now() - start) < private$.seconds_launch
      active <- online | (!discovered & awaiting)
      lost <- !active & !discovered
      handles <- instances$handle[lost]
      handles <- lapply(handles, mirai_resolve, launching = TRUE)
      mirai_wait(map(handles, self$terminate_worker), launching = FALSE)
      private$.instances <- instances[active, ]
      invisible()
    },
    #' @description Launch a worker.
    #' @return Handle of the launched worker.
    launch = function() {
      launcher <- private$.name
      worker <- crew_random_name(n = 4L)
      call <- self$call(worker = worker)
      name <- name_worker(launcher = launcher, worker = worker)
      handle <- self$launch_worker(
        call = call,
        name = name,
        launcher = launcher,
        worker = worker
      )
      private$.instances <- tibble::add_row(
        private$.instances,
        handle = list(handle) %||% crew_null,
        id = private$.id,
        start = now(),
        submitted = FALSE,
        online = FALSE,
        discovered = FALSE
      )
      invisible()
    },
    #' @description Auto-scale workers out to meet the demand of tasks.
    #' @return Invisibly returns `TRUE` if there was any relevant
    #'   auto-scaling activity (new worker launches or worker
    #'   connection/disconnection events) (`FALSE` otherwise).
    #' @param status A `mirai` status list with worker and task information.
    #' @param throttle Deprecated, only used in the controller
    #'   as of 2025-01-16 (`crew` version 0.10.2.9003).
    scale = function(status, throttle = NULL) {
      self$update(status = status)
      supply <- nrow(private$.instances)
      demand <- status$mirai["awaiting"] + status$mirai["executing"]
      increment <- 0L
      if (!anyNA(demand)) {
        increment <- min(self$workers - supply, max(0L, demand - supply))
        index <- 1L
        while (index <= increment) {
          self$launch()
          index <- index + 1L
        }
      }
      activity <- length(status$events) > 0L || increment > 0L
      private$.throttle$update(activity = activity)
      invisible(activity)
    },
    #' @description Abstract worker launch method.
    #' @details Launcher plugins will overwrite this method.
    #' @return A handle to mock the worker launch.
    #' @param call Character of length 1 with a namespaced call to
    #'   [crew_worker()] which will run in the worker and accept tasks.
    #' @param name Character of length 1 with an informative worker name.
    #' @param launcher Character of length 1, name of the launcher.
    #' @param worker Positive integer of length 1, index of the worker.
    #'   This worker index remains the same even when the current instance
    #'   of the worker exits and a new instance launches.
    #'   It is always between 1 and the maximum number of concurrent workers.
    launch_worker = function(call, name, launcher, worker) {
      list(
        call = call,
        name = name,
        launcher = launcher,
        worker = worker,
        abstract = TRUE
      )
    },
    #' @description Abstract worker termination method.
    #' @details Launcher plugins will overwrite this method.
    #' @return A handle to mock worker termination.
    #' @param handle A handle object previously
    #'   returned by `launch_worker()` which allows the termination
    #'   of the worker.
    terminate_worker = function(handle) {
      list(handle = handle, abstract = TRUE)
    },
    #' @description Terminate all workers.
    #' @return `NULL` (invisibly).
    terminate_workers = function() {
      instances <- .subset2(self, "instances")
      tasks <- list()
      for (index in seq_len(nrow(instances))) {
        handle <- mirai_resolve(instances$handle[[index]], launching = TRUE)
        tasks[[index]] <- self$terminate_worker(handle = handle)
      }
      mirai_wait(tasks = tasks, launching = FALSE)
      private$.instances <- launcher_empty_instances
      invisible()
    },
    #' @description Deprecated on 2025-01-28 (`crew` version 1.0.0).
    #' @param index Unused argument.
    #' @return The integer 1, for compatibility.
    crashes = function(index = NULL) {
      crew_deprecate(
        name = "crashes() method in crew launchers",
        date = "2025-01-28",
        version = "1.0.0",
        alternative = "none",
        condition = "message",
        value = "x",
        skip_cran = TRUE
      )
      1L
    },
    #' @description Deprecated on 2025-01-28 (`crew` version 1.0.0).
    #' @param name Name to set for the launcher.
    #' @return `NULL` (invisibly).
    set_name = function(name) {
      crew_deprecate(
        name = "crashes() method in crew launchers",
        date = "2025-01-28",
        version = "1.0.0",
        alternative = "none",
        condition = "message",
        value = "x",
        skip_cran = TRUE
      )
      private$.name <- name
      invisible()
    }
  )
)
