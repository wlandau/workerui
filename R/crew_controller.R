#' @title Create a controller object from a client and launcher.
#' @export
#' @family controller
#' @description This function is for developers of `crew` launcher plugins.
#'   Users should use a specific controller helper such as
#'   [crew_controller_local()].
#' @param client An `R6` client object created by [crew_client()].
#' @param launcher An `R6` launcher object created by one of the
#'   `crew_launcher_*()` functions such as [crew_launcher_local()].
#' @param crashes_max In rare cases, a worker may exit unexpectedly
#'   before it completes its current task. If this happens, `pop()`
#'   returns a status of `"crash"` instead of `"error"` for the task.
#'   The controller does not automatically retry the task, but
#'   you can retry it manually by calling `push()` again and using the same
#'   task name as before. (However, `targets` pipelines running `crew`
#'   do automatically retry tasks whose workers crashed.)
#'
#'   `crashes_max` is a non-negative integer, and it sets the maximum number of
#'   allowable consecutive crashes for a given task.
#'   If a task's worker crashes more than `crashes_max` times in a row,
#'   then `pop()` throws an error when it tries to return the results
#'   of the task.
#' @param backup An optional `crew` controller object, or `NULL` to omit.
#'   If supplied, the `backup` controller runs any pushed tasks that have
#'   already reached `crashes_max` consecutive crashes.
#'   Using `backup`, you can create
#'   a chain of controllers with different levels of resources
#'   (such as worker memory and CPUs) so that a task that fails on
#'   one controller can retry using incrementally more powerful workers.
#'   All controllers in a backup chain should be part of the same
#'   controller group (see [crew_controller_group()]) so you can call the
#'   group-level `pop()` and `collect()` methods to make sure you get results
#'   regardless of which controller actually ended up running the task.
#'
#'   Limitations of `backup`:
#'     * `crashes_max` needs to be positive in order for `backup` to be used.
#'       Otherwise, every task would always skip the current controller and
#'       go to `backup`.
#'     * `backup` cannot be a controller group. It must be an ordinary
#'       controller.
#' @param auto_scale Deprecated. Use the `scale` argument of `push()`,
#'   `pop()`, and `wait()` instead.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' client <- crew_client()
#' launcher <- crew_launcher_local()
#' controller <- crew_controller(client = client, launcher = launcher)
#' controller$start()
#' controller$push(name = "task", command = sqrt(4))
#' controller$wait()
#' controller$pop()
#' controller$terminate()
#' }
crew_controller <- function(
  client,
  launcher,
  crashes_max = 5L,
  backup = NULL,
  auto_scale = NULL
) {
  crew_deprecate(
    name = "auto_scale",
    date = "2023-05-18",
    version = "0.2.0",
    alternative = "use the scale argument of push(), pop(), and wait()",
    value = auto_scale,
    frequency = "once"
  )
  controller <- crew_class_controller$new(
    client = client,
    launcher = launcher,
    crashes_max = crashes_max,
    backup = backup
  )
  controller$validate()
  controller
}

#' @title Controller class
#' @export
#' @family controller
#' @description `R6` class for controllers.
#' @details See [crew_controller()].
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' client <- crew_client()
#' launcher <- crew_launcher_local()
#' controller <- crew_controller(client = client, launcher = launcher)
#' controller$start()
#' controller$push(name = "task", command = sqrt(4))
#' controller$wait()
#' controller$pop()
#' controller$terminate()
#' }
crew_class_controller <- R6::R6Class(
  classname = "crew_class_controller",
  cloneable = FALSE,
  private = list(
    .client = NULL,
    .launcher = NULL,
    .tasks = new.env(parent = emptyenv(), hash = TRUE),
    .pushed = 0L,
    .popped = 0L,
    .crashes_max = NULL,
    .crash_log = new.env(parent = emptyenv(), hash = TRUE),
    .backup = NULL,
    .summary = NULL,
    .error = NULL,
    .backlog = character(0L),
    .autoscaling = FALSE,
    .queue = NULL,
    .resolved = -1L,
    .resolve = function(force) {
      queue <- .subset2(private, ".queue")
      if ((!force) && .subset2(queue, "nonempty")()) {
        return()
      }
      observed <- .subset2(.subset2(private, ".client"), "resolved")()
      expected <- .subset2(private, ".resolved")
      if ((!force) && (observed == expected)) {
        return()
      }
      tasks <- .subset2(private, ".tasks")
      status <- eapply(
        env = tasks,
        FUN = nanonext::.unresolved,
        all.names = TRUE,
        USE.NAMES = TRUE
      )
      resolved <- names(status)[!as.logical(status)]
      .subset2(queue, "set")(names = resolved)
      private$.resolved <- observed
    },
    .wait_all_once = function() {
      if (.subset2(self, "unresolved")() > 0L) {
        private$.client$relay$wait(throttle = private$.launcher$throttle)
      }
      .subset2(self, "unresolved")() < 1L
    },
    .wait_one_once = function() {
      if (.subset2(self, "unpopped")() < 1L) {
        private$.client$relay$wait(throttle = private$.launcher$throttle)
      }
      .subset2(self, "unpopped")() > 0L
    },
    .scan_crash = function(name, task) {
      code <- .subset2(task, "code")
      log <- .subset2(private, ".crash_log")
      if (code != code_crash) {
        if (!is.null(.subset2(log, name))) {
          private$.crash_log[[name]] <- NULL
        }
        return()
      }
      previous <- .subset2(log, name)
      if (is.null(previous)) {
        previous <- 0L
      }
      count <- previous + 1L
      private$.crash_log[[name]] <- count
      if (count > .subset2(private, ".crashes_max")) {
        private$.summary$crash <- private$.summary$crash + 1L
        crew_error(
          message = paste(
            "the crew worker of task",
            shQuote(name),
            "crashed",
            count,
            sprintf(
              "consecutive time(s) in controller %s.",
              shQuote(private$.launcher$name)
            ),
            "For details and advice, please see the",
            "crashes_max argument of crew::crew_controller(), as well as",
            "https://wlandau.github.io/crew/articles/risks.html#crashes",
            "and https://wlandau.github.io/crew/articles/logging.html."
          )
        )
      }
    }
  ),
  active = list(
    #' @field client Router object.
    client = function() {
      .subset2(private, ".client")
    },
    #' @field launcher Launcher object.
    launcher = function() {
      .subset2(private, ".launcher")
    },
    #' @field tasks A list of `mirai::mirai()` task objects.
    tasks = function() {
      .subset2(private, ".tasks")
    },
    #' @field pushed Number of tasks pushed since the controller was started.
    pushed = function() {
      .subset2(private, ".pushed")
    },
    #' @field popped Number of tasks popped
    #' since the controller was started.
    popped = function() {
      .subset2(private, ".popped")
    },
    #' @field crashes_max See [crew_controller()].
    crashes_max = function() {
      .subset2(private, ".crashes_max")
    },
    #' @field backup See [crew_controller()].
    backup = function() {
      .subset2(private, ".backup")
    },
    #' @field error Tibble of task results (with one result per row)
    #'   from the last call to `map(error = "stop)`.
    error = function() {
      .subset2(private, ".error")
    },
    #' @field backlog Character vector of explicitly backlogged tasks.
    backlog = function() {
      .subset2(private, ".backlog")
    },
    #' @field autoscaling `TRUE` or `FALSE`, whether async `later`-based
    #'   auto-scaling is currently running
    autoscaling = function() {
      .subset2(private, ".autoscaling")
    },
    #' @field queue Queue of resolved unpopped/uncollected tasks.
    queue = function() {
      .subset2(private, ".queue")
    }
  ),
  public = list(
    #' @description `mirai` controller constructor.
    #' @return An `R6` controller object.
    #' @param client Router object. See [crew_controller()].
    #' @param launcher Launcher object. See [crew_controller()].
    #' @param crashes_max See [crew_controller()].
    #' @param backup See [crew_controller()].
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' client <- crew_client()
    #' launcher <- crew_launcher_local()
    #' controller <- crew_controller(client = client, launcher = launcher)
    #' controller$start()
    #' controller$push(name = "task", command = sqrt(4))
    #' controller$wait()
    #' controller$pop()
    #' controller$terminate()
    #' }
    initialize = function(
      client = NULL,
      launcher = NULL,
      crashes_max = NULL,
      backup = NULL
    ) {
      private$.client <- client
      private$.launcher <- launcher
      private$.crashes_max <- crashes_max
      private$.backup <- backup
      invisible()
    },
    #' @description Validate the controller.
    #' @return `NULL` (invisibly).
    validate = function() {
      crew_assert(inherits(private$.client, "crew_class_client"))
      crew_assert(inherits(private$.launcher, "crew_class_launcher"))
      private$.client$validate()
      private$.launcher$validate()
      # TODO: re-enable checks on crashes_max and crashes
      # when reverse dependencies catch up.
      if (!is.null(private$.crashes_max)) {
        crew_assert(
          private$.crashes_max,
          is.numeric(.),
          length(.) == 1L,
          is.finite(.),
          . >= 0L,
          message = c(
            "crashes_max must be a finite non-negative integer scalar."
          )
        )
      }
      if (!is.null(private$.crash_log)) {
        crew_assert(is.environment(private$.crash_log))
      }
      if (!is.null(private$.backup)) {
        crew_assert(
          private$.backup,
          inherits(., "crew_class_controller"),
          !inherits(., "crew_class_controller_group"),
          message = paste(
            "backup must be NULL or a crew controller, and",
            "it must not be a controller group."
          )
        )
        crew_assert(
          private$.crashes_max > 0L,
          message = "crashes_max must be positive if backup is not NULL."
        )
      }
      crew_assert(private$.tasks, is.null(.) || is.environment(.))
      crew_assert(private$.summary, is.null(.) || is.list(.))
      crew_assert(private$.backlog, is.null(.) || is.character(.))
      crew_assert(private$.autoscaling, is.null(.) || isTRUE(.) || isFALSE(.))
      crew_assert(
        private$.resolved,
        is.integer(.),
        length(.) == 1L,
        is.finite(.)
      )
      if (!is.null(private$.queue)) {
        crew_assert(private$.queue, inherits(., "crew_class_queue"))
        private$.queue$validate()
      }
      invisible()
    },
    #' @description Check if the controller is empty.
    #' @details A controller is empty if it has no running tasks
    #'   or completed tasks waiting to be retrieved with `push()`.
    #' @return `TRUE` if the controller is empty, `FALSE` otherwise.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    empty = function(controllers = NULL) {
      .subset2(private, ".pushed") == .subset2(private, ".popped")
    },
    #' @description Check if the controller is nonempty.
    #' @details A controller is empty if it has no running tasks
    #'   or completed tasks waiting to be retrieved with `push()`.
    #' @return `TRUE` if the controller is empty, `FALSE` otherwise.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    nonempty = function(controllers = NULL) {
      .subset2(private, ".pushed") > .subset2(private, ".popped")
    },
    #' @description Number of resolved `mirai()` tasks.
    #' @details `resolved()` is cumulative: it counts all the resolved
    #'   tasks over the entire lifetime of the controller session.
    #' @return Non-negative integer of length 1,
    #'   number of resolved `mirai()` tasks.
    #'   The return value is 0 if the condition variable does not exist
    #'   (i.e. if the client is not running).
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    resolved = function(controllers = NULL) {
      .subset2(.subset2(self, "client"), "resolved")()
    },
    #' @description Number of unresolved `mirai()` tasks.
    #' @return Non-negative integer of length 1,
    #'   number of unresolved `mirai()` tasks.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    unresolved = function(controllers = NULL) {
      .subset2(private, ".pushed") - .subset2(self, "resolved")()
    },
    #' @description Number of resolved `mirai()` tasks available via `pop()`.
    #' @return Non-negative integer of length 1,
    #'   number of resolved `mirai()` tasks available via `pop()`.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    unpopped = function(controllers = NULL) {
      .subset2(self, "resolved")() - .subset2(private, ".popped")
    },
    #' @description Check if the controller is saturated.
    #' @details A controller is saturated if the number of unresolved tasks
    #'   is greater than or equal to the maximum number of workers.
    #'   In other words, in a saturated controller, every available worker
    #'   has a task.
    #'   You can still push tasks to a saturated controller, but
    #'   tools that use `crew` such as `targets` may choose not to.
    #' @return `TRUE` if the controller is saturated, `FALSE` otherwise.
    #' @param collect Deprecated in version 0.5.0.9003 (2023-10-02). Not used.
    #' @param throttle Deprecated in version 0.5.0.9003 (2023-10-02). Not used.
    #' @param controller Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    saturated = function(collect = NULL, throttle = NULL, controller = NULL) {
      crew_deprecate(
        name = "collect",
        date = "2023-10-02",
        version = "0.5.0.9003",
        alternative = "none (no longer necessary)",
        condition = "message",
        value = collect,
        skip_cran = TRUE,
        frequency = "once"
      )
      crew_deprecate(
        name = "throttle",
        date = "2023-10-02",
        version = "0.5.0.9003",
        alternative = "none (no longer necessary)",
        condition = "message",
        value = throttle,
        skip_cran = TRUE,
        frequency = "once"
      )
      .subset2(self, "unresolved")() >=
        .subset2(.subset2(self, "launcher"), "workers")
    },
    #' @description Start the controller if it is not already started.
    #' @details Register the mirai client and register worker websockets
    #'   with the launcher.
    #' @return `NULL` (invisibly).
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    start = function(controllers = NULL) {
      if (!.subset2(.subset2(self, "client"), "started")) {
        private$.client$start()
        private$.launcher$start(
          url = private$.client$url,
          profile = private$.client$profile
        )
        private$.tasks <- new.env(parent = emptyenv(), hash = TRUE)
        private$.pushed <- 0L
        private$.popped <- 0L
        private$.crash_log <- new.env(parent = emptyenv(), hash = TRUE)
        private$.summary <- list(
          controller = private$.launcher$name,
          tasks = 0L,
          seconds = 0,
          success = 0L,
          error = 0L,
          crash = 0L,
          cancel = 0L,
          warning = 0L
        )
        private$.backlog <- character(0L)
        private$.queue <- crew_queue()
        private$.resolved <- -1L
      }
      invisible()
    },
    #' @description Check whether the controller is started.
    #' @details Actually checks whether the client is started.
    #' @return `TRUE` if the controller is started, `FALSE` otherwise.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    started = function(controllers = NULL) {
      .subset2(.subset2(self, "client"), "started")
    },
    #' @description Launch one or more workers.
    #' @return `NULL` (invisibly).
    #' @param n Number of workers to launch.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    launch = function(n = 1L, controllers = NULL) {
      self$start()
      replicate(n, private$.launcher$launch(), simplify = FALSE)
      invisible()
    },
    #' @description Auto-scale workers out to meet the demand of tasks.
    #' @details The `scale()` method re-launches all inactive backlogged
    #'   workers, then any additional inactive workers needed to
    #'   accommodate the demand of unresolved tasks. A worker is
    #'   "backlogged" if it was assigned more tasks than it has completed
    #'   so far.
    #'
    #'   Methods `push()`, `pop()`, and `wait()` already invoke
    #'   `scale()` if the `scale` argument is `TRUE`.
    #'   For finer control of the number of workers launched,
    #'   call `launch()` on the controller with the exact desired
    #'   number of workers.
    #' @return Invisibly returns `TRUE` if there was any relevant
    #'   auto-scaling activity (new worker launches or worker
    #'   connection/disconnection events) (`FALSE` otherwise).
    #' @param throttle `TRUE` to skip auto-scaling if it already happened
    #'   within the last `seconds_interval` seconds. `FALSE` to auto-scale
    #'   every time `scale()` is called. Throttling avoids
    #'   overburdening the `mirai` dispatcher and other resources.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    scale = function(throttle = TRUE, controllers = NULL) {
      if (throttle && !private$.launcher$poll()) {
        return(invisible())
      }
      .subset2(self, "start")()
      status <- private$.client$status()
      activity <- private$.launcher$scale(status = status, throttle = throttle)
      invisible(activity)
    },
    #' @description Run worker auto-scaling in a private `later` loop
    #'   every `controller$client$seconds_interval` seconds.
    #' @details Call `controller$descale()` to terminate the
    #'   auto-scaling loop.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    #' @return `NULL` (invisibly).
    autoscale = function(controllers = NULL) {
      # Tested in tests/interactive/test-promises.R
      # nocov start
      if (isTRUE(private$.autoscaling)) {
        return(invisible())
      }
      poll <- function() {
        if (isTRUE(private$.client$started) && isTRUE(private$.autoscaling)) {
          self$scale(throttle = FALSE)
          later::later(func = poll, delay = self$client$seconds_interval)
        }
      }
      self$start()
      private$.autoscaling <- TRUE
      poll()
      invisible()
      # nocov end
    },
    #' @description Terminate the auto-scaling loop started by
    #'   `controller$autoscale()`.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    #' @return `NULL` (invisibly).
    descale = function(controllers = NULL) {
      private$.autoscaling <- FALSE
      invisible()
    },
    #' @description Report the number of consecutive crashes of a task.
    #' @details See the `crashes_max` argument of [crew_controller()].
    #' @return Non-negative integer, number of consecutive times the task
    #'   crashed.
    #' @param name Character string, name of the task to check.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    crashes = function(name, controllers = NULL) {
      count <- .subset2(.subset2(private, ".crash_log"), name)
      if (is.null(count)) {
        0L
      } else {
        count
      }
    },
    #' @description Push a task to the head of the task list.
    #' @return Invisibly return the `mirai` object of the pushed task.
    #'   This allows you to interact with the task directly, e.g.
    #'   to create a promise object with `promises::as.promise()`.
    #' @param command Language object with R code to run.
    #' @param data Named list of local data objects in the
    #'   evaluation environment.
    #' @param globals Named list of objects to temporarily assign to the
    #'   global environment for the task.
    #'   This list should
    #'   include any functions you previously defined in the global
    #'   environment which are required to run tasks.
    #'   See the `reset_globals` argument
    #'   of [crew_controller_local()].
    #' @param substitute Logical of length 1, whether to call
    #'   `base::substitute()` on the supplied value of the
    #'   `command` argument. If `TRUE` (default) then `command` is quoted
    #'   literally as you write it, e.g.
    #'   `push(command = your_function_call())`. If `FALSE`, then `crew`
    #'   assumes `command` is a language object and you are passing its
    #'   value, e.g. `push(command = quote(your_function_call()))`.
    #'   `substitute = TRUE` is appropriate for interactive use,
    #'   whereas `substitute = FALSE` is meant for automated R programs
    #'   that invoke `crew` controllers.
    #' @param seed Integer of length 1 with the pseudo-random number generator
    #'   seed to set for the evaluation of the task. Passed to the
    #'   `seed` argument of `set.seed()` if not `NULL`.
    #'   If `algorithm` and `seed` are both `NULL`,
    #'   then the random number generator defaults to the
    #'   widely spaced worker-specific
    #'   L'Ecuyer streams as supported by `mirai::nextstream()`.
    #'   See `vignette("parallel", package = "parallel")` for details.
    #' @param algorithm Integer of length 1 with the pseudo-random number
    #'   generator algorithm to set for the evaluation of the task.
    #'   Passed to the `kind` argument of `RNGkind()` if not `NULL`.
    #'   If `algorithm` and `seed` are both `NULL`,
    #'   then the random number generator defaults to the
    #'   recommended widely spaced worker-specific
    #'   L'Ecuyer streams as supported by `mirai::nextstream()`.
    #'   See `vignette("parallel", package = "parallel")` for details.
    #' @param packages Character vector of packages to load for the task.
    #' @param library Library path to load the packages. See the `lib.loc`
    #'   argument of `require()`.
    #' @param seconds_timeout Optional task timeout passed to the `.timeout`
    #'   argument of `mirai::mirai()` (after converting to milliseconds).
    #' @param scale Logical, whether to automatically call `scale()`
    #'   to auto-scale workers to meet the demand of the task load. Also
    #'   see the `throttle` argument.
    #' @param throttle `TRUE` to skip auto-scaling if it already happened
    #'   within the last `seconds_interval` seconds. `FALSE` to auto-scale
    #'   every time `scale()` is called. Throttling avoids
    #'   overburdening the `mirai` dispatcher and other resources.
    #' @param name Character string, name of the task. If `NULL`, then
    #'   a random name is generated automatically.
    #'   The name of the task must not conflict with the name of another
    #'   task pushed to the controller. Any previous task with the same name
    #'   must first be popped before a new task with that name can be pushed.
    #' @param save_command Deprecated on 2025-01-22 (`crew` version
    #'   0.10.2.9004) and no longer used.
    #' @param controller Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    push = function(
      command,
      data = list(),
      globals = list(),
      substitute = TRUE,
      seed = NULL,
      algorithm = NULL,
      packages = character(0),
      library = NULL,
      seconds_timeout = NULL,
      scale = TRUE,
      throttle = TRUE,
      name = NULL,
      save_command = NULL,
      controller = NULL
    ) {
      .subset2(self, "start")()
      if (substitute) {
        command <- substitute(command)
      }
      if (is.null(seconds_timeout)) {
        .timeout <- NULL
      } else {
        .timeout <- seconds_timeout * 1000
      }
      tasks <- .subset2(private, ".tasks")
      if (is.null(name)) {
        name <- name_task_tempfile()
        name <- if_any(
          is.null(.subset2(tasks, name)),
          name,
          name_task_nanonext()
        )
      }
      if (!is.null(.subset2(tasks, name))) {
        crew_error(
          message = paste(
            "crew task name",
            name,
            "already found in the task list. Before pushing a task",
            "of the same name, please wait for it to resolve, then",
            "use pop() or collect() to remove it from the controller."
          )
        )
      }
      backup <- .subset2(private, ".backup")
      if (!is.null(backup)) {
        max <- .subset2(private, ".crashes_max")
        if ((max > 0L) && (.subset2(self, "crashes")(name = name) == max)) {
          return(
            .subset2(backup, "push") (
              command = command,
              data = data,
              globals = globals,
              substitute = FALSE,
              seed = seed,
              algorithm = algorithm,
              packages = packages,
              library = library,
              seconds_timeout = seconds_timeout,
              scale = scale,
              throttle = throttle,
              name = name,
              controller = controller
            )
          )
        }
      }
      task <- mirai::mirai(
        .expr = expr_crew_eval,
        .args = list(
          name = name,
          command = command,
          data = data,
          globals = globals,
          seed = seed,
          algorithm = algorithm,
          packages = packages,
          library = library
        ),
        .timeout = .timeout,
        .compute = .subset2(.subset2(private, ".client"), "profile")
      )
      tasks[[name]] <- task
      private$.pushed <- .subset2(self, "pushed") + 1L
      if (scale) {
        .subset2(self, "scale")(throttle = throttle)
      }
      invisible(task)
    },
    #' @description Apply a single command to multiple inputs,
    #'   and return control to the user without
    #'   waiting for any task to complete.
    #' @details In contrast to `walk()`, `map()` blocks the local R session
    #'   and waits for all tasks to complete.
    #' @return Invisibly returns a list of `mirai` task objects for the
    #'   newly created tasks. The order of tasks in the list matches the
    #'   order of data in the `iterate` argument.
    #' @param command Language object with R code to run.
    #' @param iterate Named list of vectors or lists to iterate over.
    #'   For example, to run function calls
    #'   `f(x = 1, y = "a")` and `f(x = 2, y = "b")`,
    #'   set `command` to `f(x, y)`, and set `iterate` to
    #'   `list(x = c(1, 2), y = c("a", "b"))`. The individual
    #'   function calls are evaluated as
    #'   `f(x = iterate$x[[1]], y = iterate$y[[1]])` and
    #'   `f(x = iterate$x[[2]], y = iterate$y[[2]])`.
    #'   All the elements of `iterate` must have the same length.
    #'   If there are any name conflicts between `iterate` and `data`,
    #'   `iterate` takes precedence.
    #' @param data Named list of constant local data objects in the
    #'   evaluation environment. Objects in this list are treated as single
    #'   values and are held constant for each iteration of the map.
    #' @param globals Named list of constant objects to temporarily
    #'   assign to the global environment for each task. This list should
    #'   include any functions you previously defined in the global
    #'   environment which are required to run tasks.
    #'   See the `reset_globals` argument of [crew_controller_local()].
    #'   Objects in this list are treated as single
    #'   values and are held constant for each iteration of the map.
    #' @param substitute Logical of length 1, whether to call
    #'   `base::substitute()` on the supplied value of the
    #'   `command` argument. If `TRUE` (default) then `command` is quoted
    #'   literally as you write it, e.g.
    #'   `push(command = your_function_call())`. If `FALSE`, then `crew`
    #'   assumes `command` is a language object and you are passing its
    #'   value, e.g. `push(command = quote(your_function_call()))`.
    #'   `substitute = TRUE` is appropriate for interactive use,
    #'   whereas `substitute = FALSE` is meant for automated R programs
    #'   that invoke `crew` controllers.
    #' @param seed Integer of length 1 with the pseudo-random number generator
    #'   seed to set for the evaluation of the task. Passed to the
    #'   `seed` argument of `set.seed()` if not `NULL`.
    #'   If `algorithm` and `seed` are both `NULL`,
    #'   then the random number generator defaults to the
    #'   recommended widely spaced worker-specific
    #'   L'Ecuyer streams as supported by `mirai::nextstream()`.
    #'   See `vignette("parallel", package = "parallel")` for details.
    #' @param algorithm Integer of length 1 with the pseudo-random number
    #'   generator algorithm to set for the evaluation of the task.
    #'   Passed to the `kind` argument of `RNGkind()` if not `NULL`.
    #'   If `algorithm` and `seed` are both `NULL`,
    #'   then the random number generator defaults to the
    #'   recommended widely spaced worker-specific
    #'   L'Ecuyer streams as supported by `mirai::nextstream()`.
    #'   See `vignette("parallel", package = "parallel")` for details.
    #' @param packages Character vector of packages to load for the task.
    #' @param library Library path to load the packages. See the `lib.loc`
    #'   argument of `require()`.
    #' @param seconds_timeout Optional task timeout passed to the `.timeout`
    #'   argument of `mirai::mirai()` (after converting to milliseconds).
    #' @param names Optional character of length 1, name of the element of
    #'   `iterate` with names for the tasks. If `names` is supplied,
    #'   then `iterate[[names]]` must be a character vector.
    #' @param save_command Deprecated on 2025-01-22 (`crew` version
    #'   0.10.2.9004). The command is always saved now.
    #' @param scale Logical, whether to automatically scale workers to meet
    #'   demand. See also the `throttle` argument.
    #' @param throttle `TRUE` to skip auto-scaling if it already happened
    #'   within the last `seconds_interval` seconds. `FALSE` to auto-scale
    #'   every time `scale()` is called. Throttling avoids
    #'   overburdening the `mirai` dispatcher and other resources.
    #' @param controller Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    walk = function(
      command,
      iterate,
      data = list(),
      globals = list(),
      substitute = TRUE,
      seed = NULL,
      algorithm = NULL,
      packages = character(0),
      library = NULL,
      seconds_timeout = NULL,
      names = NULL,
      save_command = NULL,
      scale = TRUE,
      throttle = TRUE,
      controller = NULL
    ) {
      .subset2(self, "start")()
      crew_deprecate(
        name = "save_command",
        date = "2025-01-22",
        version = "0.10.2.9004",
        alternative = "none (no longer needed)",
        condition = "warning",
        value = save_command
      )
      crew_assert(substitute, isTRUE(.) || isFALSE(.))
      if (substitute) {
        command <- substitute(command)
      }
      crew_assert(
        iterate,
        is.list(.),
        rlang::is_named(.),
        message = "the 'iterate' arg of map() must be a nonempty named list"
      )
      crew_assert(
        length(iterate) > 0L,
        message = "the \"iterate\" arg of map() must be a nonempty named list"
      )
      crew_assert(
        length(unique(map_dbl(iterate, length))) == 1L,
        message = "all elements of \"iterate\" must have the same length"
      )
      crew_assert(
        length(iterate[[1L]]) > 0L,
        message = "all elements of \"iterate\" must be nonempty"
      )
      crew_assert(
        data,
        is.list(.),
        rlang::is_named(.) || length(.) < 1L,
        message = "the \"data\" argument of map() must be a named list"
      )
      crew_assert(
        globals,
        is.list(.),
        rlang::is_named(.) || length(.) < 1L,
        message = "the \"globals\" argument of map() must be a named list"
      )
      crew_assert(
        seed %|||% 1L,
        is.numeric(.),
        length(.) == 1L,
        !anyNA(seed),
        message = "seed must be an integer of length 1"
      )
      crew_assert(
        algorithm %|||% "Mersenne-Twister",
        is.character(.),
        length(.) == 1L,
        !anyNA(.),
        nzchar(.),
        message = "algorithm must be a valid RNG algorithm name"
      )
      crew_assert(
        packages,
        is.character(.),
        !anyNA(.),
        message = "packages must be a character vector with no element missing"
      )
      crew_assert(
        library %|||% "local",
        is.character(.),
        !anyNA(.),
        message = "library must be a NULL or a non-missing character vector"
      )
      crew_assert(
        names %|||% names(iterate)[[1L]],
        is.character(.),
        length(.) == 1L,
        !anyNA(.),
        nzchar(.),
        . %in% names(iterate),
        message = "names argument must be NULL or an element of names(iterate)"
      )
      crew_assert(scale, isTRUE(.) || isFALSE(.))
      crew_assert(throttle, isTRUE(.) || isFALSE(.))
      names <- if_any(
        is.null(names),
        paste(
          basename(tempfile(pattern = "unnamed_task_")),
          as.character(seq_along(iterate[[1L]])),
          sep = "_"
        ),
        as.character(iterate[[names]])
      )
      crew_assert(
        names,
        is.character(.),
        !anyNA(.),
        message = "task names in map() must be valid character strings."
      )
      crew_assert(
        anyDuplicated(names) < 1L,
        message = "task names in map() must not have duplicates"
      )
      names_iterate <- names(iterate)
      self$start()
      sign <- if_any(!is.null(seed) && seed > 0L, 1L, -1L)
      if (!is.null(seed)) {
        seed <- 1L
      }
      tasks <- list()
      push <- self$push
      for (index in seq_along(names)) {
        for (name in names_iterate) {
          data[[name]] <- .subset2(.subset2(iterate, name), index)
        }
        if (is.null(seed)) {
          task_seed <- NULL
        } else {
          task_seed <- seed - (sign * index)
        }
        name <- .subset(names, index)
        tasks[[name]] <- push(
          command = command,
          substitute = FALSE,
          data = data,
          globals = globals,
          seed = task_seed,
          algorithm = algorithm,
          packages = packages,
          library = library,
          seconds_timeout = seconds_timeout,
          name = name
        )
      }
      if (scale) {
        self$scale(throttle = throttle)
      }
      invisible(tasks)
    },
    #' @description Apply a single command to multiple inputs,
    #'   wait for all tasks to complete,
    #'   and return the results of all tasks.
    #' @details `map()` cannot be used unless all prior tasks are
    #'   completed and popped. You may need to wait and then pop them
    #'   manually. Alternatively, you can start over: either call
    #'   `terminate()` on the current controller object to reset it, or
    #'   create a new controller object entirely.
    #' @return A `tibble` of results and metadata: one row per task
    #'   and columns corresponding to the output of `pop()`.
    #' @param command Language object with R code to run.
    #' @param iterate Named list of vectors or lists to iterate over.
    #'   For example, to run function calls
    #'   `f(x = 1, y = "a")` and `f(x = 2, y = "b")`,
    #'   set `command` to `f(x, y)`, and set `iterate` to
    #'   `list(x = c(1, 2), y = c("a", "b"))`. The individual
    #'   function calls are evaluated as
    #'   `f(x = iterate$x[[1]], y = iterate$y[[1]])` and
    #'   `f(x = iterate$x[[2]], y = iterate$y[[2]])`.
    #'   All the elements of `iterate` must have the same length.
    #'   If there are any name conflicts between `iterate` and `data`,
    #'   `iterate` takes precedence.
    #' @param data Named list of constant local data objects in the
    #'   evaluation environment. Objects in this list are treated as single
    #'   values and are held constant for each iteration of the map.
    #' @param globals Named list of constant objects to temporarily
    #'   assign to the global environment for each task. This list should
    #'   include any functions you previously defined in the global
    #'   environment which are required to run tasks.
    #'   See the `reset_globals` argument of [crew_controller_local()].
    #'   Objects in this list are treated as single
    #'   values and are held constant for each iteration of the map.
    #' @param substitute Logical of length 1, whether to call
    #'   `base::substitute()` on the supplied value of the
    #'   `command` argument. If `TRUE` (default) then `command` is quoted
    #'   literally as you write it, e.g.
    #'   `push(command = your_function_call())`. If `FALSE`, then `crew`
    #'   assumes `command` is a language object and you are passing its
    #'   value, e.g. `push(command = quote(your_function_call()))`.
    #'   `substitute = TRUE` is appropriate for interactive use,
    #'   whereas `substitute = FALSE` is meant for automated R programs
    #'   that invoke `crew` controllers.
    #' @param seed Integer of length 1 with the pseudo-random number generator
    #'   seed to set for the evaluation of the task. Passed to the
    #'   `seed` argument of `set.seed()` if not `NULL`.
    #'   If `algorithm` and `seed` are both `NULL`,
    #'   then the random number generator defaults to the
    #'   recommended widely spaced worker-specific
    #'   L'Ecuyer streams as supported by `mirai::nextstream()`.
    #'   See `vignette("parallel", package = "parallel")` for details.
    #' @param algorithm Integer of length 1 with the pseudo-random number
    #'   generator algorithm to set for the evaluation of the task.
    #'   Passed to the `kind` argument of `RNGkind()` if not `NULL`.
    #'   If `algorithm` and `seed` are both `NULL`,
    #'   then the random number generator defaults to the
    #'   recommended widely spaced worker-specific
    #'   L'Ecuyer streams as supported by `mirai::nextstream()`.
    #'   See `vignette("parallel", package = "parallel")` for details.
    #' @param packages Character vector of packages to load for the task.
    #' @param library Library path to load the packages. See the `lib.loc`
    #'   argument of `require()`.
    #' @param seconds_interval Deprecated on 2025-01-17 (`crew` version
    #'   0.10.2.9003). Instead, the `seconds_interval` argument passed
    #'   to [crew_controller_group()] is used as `seconds_max`
    #'   in a [crew_throttle()] object which orchestrates exponential
    #'   backoff.
    #' @param seconds_timeout Optional task timeout passed to the `.timeout`
    #'   argument of `mirai::mirai()` (after converting to milliseconds).
    #' @param names Optional character string, name of the element of
    #'   `iterate` with names for the tasks. If `names` is supplied,
    #'   then `iterate[[names]]` must be a character vector.
    #' @param save_command Deprecated on 2025-01-22 (`crew` version
    #'   0.10.2.9004). The command is always saved now.
    #' @param error Character of length 1, choice of action if
    #'   a task was not successful. Possible values:
    #'   * `"stop"`: throw an error in the main R session instead of returning
    #'     a value. In case of an error, the results from the last errored
    #'     `map()` are in the `error` field
    #'     of the controller, e.g. `controller_object$error`. To reduce
    #'     memory consumption, set `controller_object$error <- NULL` after
    #'     you are finished troubleshooting.
    #'   * `"warn"`: throw a warning. This allows the return value with
    #'     all the error messages and tracebacks to be generated.
    #'   * `"silent"`: do nothing special.
    #'   NOTE: the only kinds of errors considered here are errors at the R
    #'   level. A crashed tasks will return a status of `"crash"` in the output
    #'   and not trigger an error in `map()` unless `crashes_max` is reached.
    #' @param warnings Logical of length 1, whether to throw a warning in the
    #'   interactive session if at least one task encounters an error.
    #' @param verbose Logical of length 1, whether to print progress messages.
    #' @param scale Logical, whether to automatically scale workers to meet
    #'   demand. See also the `throttle` argument.
    #' @param throttle `TRUE` to skip auto-scaling if it already happened
    #'   within the last `seconds_interval` seconds. `FALSE` to auto-scale
    #'   every time `scale()` is called. Throttling avoids
    #'   overburdening the `mirai` dispatcher and other resources.
    #' @param controller Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    map = function(
      command,
      iterate,
      data = list(),
      globals = list(),
      substitute = TRUE,
      seed = NULL,
      algorithm = NULL,
      packages = character(0),
      library = NULL,
      seconds_interval = NULL,
      seconds_timeout = NULL,
      names = NULL,
      save_command = NULL,
      error = "stop",
      warnings = TRUE,
      verbose = interactive(),
      scale = TRUE,
      throttle = TRUE,
      controller = NULL
    ) {
      crew_deprecate(
        name = "seconds_interval",
        date = "2025-01-17",
        version = "0.10.2.9003",
        alternative = "none (no longer used)",
        condition = "warning",
        value = seconds_interval
      )
      crew_deprecate(
        name = "save_command",
        date = "2025-01-22",
        version = "0.10.2.9004",
        alternative = "none (no longer needed)",
        condition = "warning",
        value = save_command
      )
      crew_assert(
        length(private$.tasks) < 1L,
        message = "cannot map() until all prior tasks are completed and popped"
      )
      crew_assert(substitute, isTRUE(.) || isFALSE(.))
      crew_assert(error %in% c("stop", "warn", "silent"))
      if (substitute) {
        command <- substitute(command)
      }
      tasks <- self$walk(
        command = command,
        iterate = iterate,
        data = data,
        globals = globals,
        substitute = FALSE,
        seed = seed,
        algorithm = algorithm,
        packages = packages,
        library = library,
        seconds_timeout = seconds_timeout,
        names = names,
        scale = scale,
        throttle = throttle
      )
      names <- names(tasks)
      total <- length(tasks)
      relay <- .subset2(.subset2(self, "client"), "relay")
      throttle_object <- .subset2(.subset2(self, "launcher"), "throttle")
      start <- nanonext::mclock()
      pushed <- private$.pushed
      this_envir <- environment()
      progress_envir <- new.env(parent = this_envir)
      if (verbose) {
        cli::cli_progress_bar(
          total = total,
          type = "custom",
          format = paste(
            "{cli::pb_current}/{cli::pb_total}",
            "{cli::pb_bar}",
            "{cli::pb_percent}"
          ),
          format_done = "{cli::pb_total} tasks in {cli::pb_elapsed}",
          clear = FALSE,
          .envir = progress_envir
        )
      }
      iterate <- function() {
        if (scale) {
          .subset2(self, "scale")(throttle = throttle)
        }
        unresolved <- .subset2(self, "unresolved")()
        if (verbose) {
          cli::cli_progress_update(
            set = total - unresolved,
            .envir = progress_envir
          )
        }
        if (unresolved > 0L) {
          .subset2(relay, "wait")(throttle = throttle_object)
        }
        .subset2(self, "unresolved")() < 1L
      }
      crew_retry(
        fun = iterate,
        seconds_interval = 0,
        seconds_timeout = Inf,
        error = FALSE,
        assertions = FALSE
      )
      if (verbose) {
        cli::cli_progress_done(.envir = progress_envir)
      }
      out <- list()
      controller_name <- .subset2(.subset2(private, ".launcher"), "name")
      for (index in seq_along(tasks)) {
        task <- .subset2(tasks, index)
        name <- .subset(names, index)
        monad <- as_monad(
          task = task,
          name = name,
          controller = controller_name
        )
        .subset2(private, ".scan_crash")(name = name, task = monad)
        out[[length(out) + 1L]] <- monad
      }
      out <- tibble::new_tibble(data.table::rbindlist(out, use.names = FALSE))
      out <- out[match(x = names, table = out$name),, drop = FALSE] # nolint
      out <- out[!is.na(out$name),, drop = FALSE] # nolint
      on.exit({
        private$.tasks <- new.env(parent = emptyenv(), hash = TRUE)
        private$.popped <- .subset2(private, ".popped") + nrow(out)
        summary <- private$.summary
        summary$tasks <- .subset2(summary, "tasks") + nrow(out)
        summary$seconds <- .subset2(summary, "seconds") +
          sum(out$seconds)
        for (status in c("success", "error", "crash", "cancel")) {
          summary[[status]] <- .subset2(summary, status) +
            sum(.subset2(out, "status") == status)
        }
        summary$warning <- .subset2(summary, "warning") +
          sum(!is.na(out$warnings))
        private$.summary <- summary
      })
      warning_messages <- out$warnings
      if (!all(is.na(warning_messages)) && isTRUE(warnings)) {
        message <- sprintf(
          paste(
            "%s tasks encountered warnings.",
            "Warning messages of first such task: \"%s\"."
          ),
          sum(!is.na(warning_messages)),
          warning_messages[min(which(!is.na(warning_messages)))]
        )
        crew_warning(message)
      }
      error_messages <- out$error
      if (!all(is.na(error_messages)) && !identical(error, "silent")) {
        message <- sprintf(
          "%s tasks were not successful. First error message: \"%s\".",
          sum(!is.na(error_messages)),
          error_messages[min(which(!is.na(error_messages)))]
        )
        if (identical(error, "stop")) {
          private$.error <- out
          message <- paste(
            message,
            "\nSee the \"error\" field of your controller object",
            "for all results, including warnings, tracebacks, all",
            "error messages, etc."
          )
          crew_error(message)
        } else {
          crew_warning(message)
        }
      }
      out
    },
    #' @description Pop a completed task from the results data frame.
    #' @details If not task is currently completed, `pop()`
    #'   will attempt to auto-scale workers as needed.
    #' @return If there is no task to collect, return `NULL`. Otherwise,
    #'   return a one-row `tibble` with the following columns.
    #'   * `name`: the task name.
    #'   * `command`: a character string with the R command.
    #'   * `result`: a list containing the return value of the R command.
    #'     `NA` if the task failed.
    #'   * `status`: a character string. `"success"` if the task succeeded,
    #'     `"cancel"` if the task was canceled with
    #'     the `cancel()` controller method,
    #'     `"crash"` if the worker running the task exited before
    #'     it could complete the task, or `"error"`
    #'      for any other kind of error.
    #'   * `error`: the first 2048 characters of the error message if
    #'     the task status is not `"success"`, `NA` otherwise.
    #'     Messages for crashes and cancellations are captured here
    #'     alongside ordinary R-level errors.
    #'   * `code`: an integer code denoting the specific exit status:
    #'     `0` for successful tasks, `-1` for tasks with an error in the R
    #'     command of the task, and another positive integer with an NNG
    #'     status code if there is an error at the NNG/`nanonext` level.
    #'     `nanonext::nng_error()` can interpret these codes.
    #'   * `trace`: the first 2048 characters of the text of the traceback
    #'     if the task threw an error, `NA` otherwise.
    #'   * `warnings`: the first 2048 characters. of the text of
    #'     warning messages that the task may have generated, `NA` otherwise.
    #'   * `seconds`: number of seconds that the task ran.
    #'   * `seed`: the single integer originally supplied to `push()`,
    #'      `NA` otherwise. The pseudo-random number generator state
    #'      just prior to the task can be restored using
    #'      `set.seed(seed = seed, kind = algorithm)`, where `seed` and
    #'      `algorithm` are part of this output.
    #'   * `algorithm`: name of the pseudo-random number generator algorithm
    #'      originally supplied to `push()`,
    #'      `NA` otherwise. The pseudo-random number generator state
    #'      just prior to the task can be restored using
    #'      `set.seed(seed = seed, kind = algorithm)`, where `seed` and
    #'      `algorithm` are part of this output.
    #'   * `controller`: name of the `crew` controller where the task ran.
    #'   * `worker`: name of the `crew` worker that ran the task.
    #' @param scale Logical of length 1,
    #'   whether to automatically call `scale()`
    #'   to auto-scale workers to meet the demand of the task load.
    #'   Scaling up on `pop()` may be important
    #'   for transient or nearly transient workers that tend to drop off
    #'   quickly after doing little work.
    #'   See also the `throttle` argument.
    #' @param collect Deprecated in version 0.5.0.9003 (2023-10-02).
    #' @param throttle `TRUE` to skip auto-scaling if it already happened
    #'   within the last `seconds_interval` seconds. `FALSE` to auto-scale
    #'   every time `scale()` is called. Throttling avoids
    #'   overburdening the `mirai` dispatcher and other resources.
    #' @param error `NULL` or character of length 1, choice of action if
    #'   the popped task threw an error. Possible values:
    #'   * `"stop"`: throw an error in the main R session instead of returning
    #'     a value.
    #'   * `"warn"`: throw a warning.
    #'   * `NULL` or `"silent"`: do not react to errors.
    #'   NOTE: the only kinds of errors considered here are errors at the R
    #'   level. A crashed tasks will return a status of `"crash"` in the output
    #'   and not trigger an error in `pop()` unless `crashes_max` is reached.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    pop = function(
      scale = TRUE,
      collect = NULL,
      throttle = TRUE,
      error = NULL,
      controllers = NULL
    ) {
      if (!.subset2(.subset2(self, "client"), "started")) {
        return(NULL)
      }
      crew_deprecate(
        name = "collect",
        date = "2023-10-02",
        version = "0.5.0.9003",
        alternative = "none (no longer necessary)",
        condition = "message",
        value = collect,
        skip_cran = TRUE,
        frequency = "once"
      )
      if (!is.null(error)) {
        crew_assert(
          error,
          is.character(.),
          !anyNA(.),
          nzchar(.),
          length(.) == 1L,
          error %in% c("stop", "warn", "silent")
        )
      }
      if (scale) {
        .subset2(self, "scale")(throttle = throttle)
      }
      if (.subset2(self, "empty")()) {
        return(NULL)
      }
      .subset2(private, ".resolve")(force = FALSE)
      name <- .subset2(.subset2(private, ".queue"), "pop")()
      if (is.null(name)) {
        return(NULL)
      }
      tasks <- .subset2(self, "tasks")
      task <- .subset2(tasks, name)
      remove(list = name, envir = tasks)
      private$.popped <- .subset2(self, "popped") + 1L
      out <- as_monad(
        task = task,
        name = name,
        controller = .subset2(.subset2(private, ".launcher"), "name")
      )
      .subset2(private, ".scan_crash")(name = name, task = out)
      seconds <- .subset2(out, "seconds")
      summary <- .subset2(private, ".summary")
      summary$tasks <- .subset2(summary, "tasks") + 1L
      if (!anyNA(seconds)) {
        summary$seconds <- .subset2(summary, "seconds") + seconds
      }
      for (status in c("success", "error", "crash", "cancel")) {
        summary[[status]] <- .subset2(summary, status) +
          (.subset2(out, "status") == status)
      }
      summary$warning <- .subset2(summary, "warning") +
        !anyNA(.subset2(out, "warnings"))
      private$.summary <- summary
      has_error <- !is.null(error) &&
        any(.subset2(out, "status") != "success")
      throw_error <- has_error && identical(error, "stop")
      throw_warning <- has_error && identical(error, "warn")
      if_any(
        throw_error,
        crew_error(message = .subset2(out, "error")),
        NULL
      )
      if_any(
        throw_warning,
        crew_warning(message = .subset2(out, "error")),
        NULL
      )
      out
    },
    #' @description Pop all available task results and return them in a tidy
    #'   `tibble`.
    #' @return A `tibble` of results and metadata of all resolved tasks,
    #'   with one row per task. Returns `NULL` if there are no tasks
    #'   to collect. See `pop()` for details on the columns of the
    #'   returned `tibble`.
    #' @param scale Logical of length 1,
    #'   whether to automatically call `scale()`
    #'   to auto-scale workers to meet the demand of the task load.
    #' @param throttle `TRUE` to skip auto-scaling if it already happened
    #'   within the last `seconds_interval` seconds. `FALSE` to auto-scale
    #'   every time `scale()` is called. Throttling avoids
    #'   overburdening the `mirai` dispatcher and other resources.
    #' @param error `NULL` or character of length 1, choice of action if
    #'   the popped task threw an error. Possible values:
    #'     * `"stop"`: throw an error in the main R session instead of
    #'       returning a value.
    #'     * `"warn"`: throw a warning.
    #'     * `NULL` or `"silent"`: do not react to errors.
    #'   NOTE: the only kinds of errors considered here are errors at the R
    #'   level. A crashed tasks will return a status of `"crash"` in the output
    #'   and not trigger an error in `collect()`
    #'   unless `crashes_max` is reached.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    collect = function(
      scale = TRUE,
      throttle = TRUE,
      error = NULL,
      controllers = NULL
    ) {
      if (!is.null(error)) {
        crew_assert(
          error,
          is.character(.),
          !anyNA(.),
          nzchar(.),
          length(.) == 1L,
          error %in% c("stop", "warn", "silent"),
          message = "invalid error argument to collect()"
        )
      }
      if (scale) {
        .subset2(self, "scale")(throttle = throttle)
      }
      if (.subset2(self, "empty")()) {
        return(NULL)
      }
      queue <- .subset2(private, ".queue")
      .subset2(private, ".resolve")(force = TRUE)
      names <- .subset2(queue, "collect")()
      if (!length(names)) {
        return(NULL)
      }
      tasks <- .subset2(private, ".tasks")
      scan_crash <- .subset2(private, ".scan_crash")
      controller_name <- .subset2(.subset2(private, ".launcher"), "name")
      out <- lapply(names, function(name) {
        out <- as_monad(
          task = .subset2(tasks, name),
          name = name,
          controller = controller_name
        )
        scan_crash(name = name, task = out)
        out
      })
      out <- tibble::new_tibble(data.table::rbindlist(out, use.names = FALSE))
      remove(list = names, envir = tasks)
      popped <- length(names)
      private$.popped <- .subset2(self, "popped") + popped
      summary <- .subset2(private, ".summary")
      summary$tasks <- .subset2(summary, "tasks") + popped
      summary$seconds <- .subset2(summary, "seconds") +
        sum(.subset2(out, "seconds"), na.rm = TRUE)
      for (status in c("success", "error", "crash", "cancel")) {
        summary[[status]] <- .subset2(summary, status) +
          sum(.subset2(out, "status") == status)
      }
      summary$warning <- .subset2(summary, "warning") +
        sum(!is.na(.subset2(out, "warnings")))
      private$.summary <- summary
      errors <- .subset2(out, "error")
      errors <- errors[!is.na(errors)]
      if (!is.null(error) && length(errors)) {
        if (identical(error, "stop")) {
          crew_error(message = errors[1L])
        } else if (identical(error, "warn")) {
          crew_warning(message =  errors[1L])
        }
      }
      out
    },
    #' @description Create a `promises::promise()` object to asynchronously
    #'   pop or collect one or more tasks.
    #' @details Please be aware that `pop()` or `collect()` will happen
    #'   asynchronously at a some unpredictable time after the promise object
    #'   is created, even if your local R process appears to be doing
    #'   something completely different. This behavior is highly desirable
    #'   in a Shiny reactive context, but please be careful as it may be
    #'   surprising in other situations.
    #' @return A `promises::promise()` object whose eventual value will
    #'   be a `tibble` with results from one or more popped tasks.
    #'   If `mode = "one"`, only one task is popped and returned (one row).
    #'   If `mode = "all"`, then all the tasks are returned in a `tibble`
    #'   with one row per task (or `NULL` is returned if there are no
    #'   tasks to pop).
    #' @param mode Character of length 1, what kind of promise to create.
    #'   `mode` must be `"one"` or `"all"`. Details:
    #'   * If `mode` is `"one"`, then the promise is fulfilled (or rejected)
    #'     when at least one task is resolved and available to `pop()`.
    #'     When that happens, `pop()` runs asynchronously, pops a result off
    #'     the task list, and returns a value.
    #'     If the task succeeded, then the promise
    #'     is fulfilled and its value is the result of `pop()` (a one-row
    #'     `tibble` with the result and metadata). If the task threw an error,
    #'     the error message of the task is forwarded to any error callbacks
    #'     registered with the promise.
    #'   * If `mode` is `"all"`, then the promise is fulfilled (or rejected)
    #'     when there are no unresolved tasks left in the controller.
    #'     (Be careful: this condition is trivially met in the moment
    #'     if the controller is empty and you have not submitted any tasks,
    #'     so it is best to create this kind of promise only after you
    #'     submit tasks.)
    #'     When there are no unresolved tasks left,
    #'     `collect()` runs asynchronously, pops all available results
    #'     off the task list, and returns a value.
    #'     If the task succeeded, then the promise
    #'     is fulfilled and its value is the result of `collect()`
    #'     (a `tibble` with one row per task result). If any of the tasks
    #'     threw an error, then the first error message detected is forwarded
    #'     to any error callbacks registered with the promise.
    #' @param seconds_interval Positive numeric of length 1, delay in the
    #'   `later::later()` polling interval to asynchronously check if
    #'   the promise can be resolved.
    #' @param scale Deprecated on 2024-04-10 (version 0.9.1.9003)
    #'   and no longer used. Now, `promise()` always turns on auto-scaling
    #'   in a private `later` loop (if not already activated).
    #' @param throttle Deprecated on 2024-04-10 (version 0.9.1.9003)
    #'   and no longer used. Now, `promise()` always turns on auto-scaling
    #'   in a private `later` loop (if not already activated).
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    promise = function(
      mode = "one",
      seconds_interval = 1,
      scale = NULL,
      throttle = NULL,
      controllers = NULL
    ) {
      # Tested in tests/interactive/test-promises.R.
      # nocov start
      crew_deprecate(
        name = "controller$promise()",
        date = "2024-04-19",
        version = "> 0.9.2",
        alternative = paste(
          "see https://wlandau.github.io/crew/articles/shiny.html for the",
          "latest on using {crew} in promise-driven Shiny apps"
        ),
        value = TRUE,
        frequency = "once"
      )
      self$autoscale(controllers = controllers)
      controller_promise(
        controller = self,
        mode = mode,
        seconds_interval = seconds_interval,
        controllers = controllers
      )
      # nocov end
    },
    #' @description Wait for tasks.
    #' @details The `wait()` method blocks the calling R session and
    #'   repeatedly auto-scales workers for tasks that need them.
    #'   The function runs until it either times out or the condition
    #'   in `mode` is met.
    #' @return A logical of length 1, invisibly. `TRUE` if the condition
    #'   in `mode` was met, `FALSE` otherwise.
    #' @param mode Character of length 1: `"all"` to wait for all tasks to
    #'   complete, `"one"` to wait for a single task to complete.
    #' @param seconds_interval Deprecated on 2025-01-17 (`crew` version
    #'   0.10.2.9003). Instead, the `seconds_interval` argument passed
    #'   to [crew_controller_group()] is used as `seconds_max`
    #'   in a [crew_throttle()] object which orchestrates exponential
    #'   backoff.
    #' @param seconds_timeout Timeout length in seconds waiting for tasks.
    #' @param scale Logical, whether to automatically call `scale()`
    #'   to auto-scale workers to meet the demand of the task load.
    #'   See also the `throttle` argument.
    #' @param throttle `TRUE` to skip auto-scaling if it already happened
    #'   within the last `seconds_interval` seconds. `FALSE` to auto-scale
    #'   every time `scale()` is called. Throttling avoids
    #'   overburdening the `mirai` dispatcher and other resources.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    wait = function(
      mode = "all",
      seconds_interval = NULL,
      seconds_timeout = Inf,
      scale = TRUE,
      throttle = TRUE,
      controllers = NULL
    ) {
      crew_deprecate(
        name = "seconds_interval",
        date = "2025-01-17",
        version = "0.10.2.9003",
        alternative = "none (no longer used)",
        condition = "warning",
        value = seconds_interval
      )
      crew_assert(mode, identical(., "all") || identical(., "one"))
      mode_all <- identical(mode, "all")
      if (length(private$.tasks) < 1L) {
        return(invisible(mode_all))
      }
      envir <- new.env(parent = emptyenv())
      envir$result <- FALSE
      iterate <- function() {
        if (!envir$result && scale) {
          self$scale(throttle = throttle)
        }
        envir$result <- if_any(
          mode_all,
          private$.wait_all_once(),
          private$.wait_one_once()
        )
        envir$result
      }
      crew_retry(
        fun = iterate,
        seconds_interval = 0,
        seconds_timeout = seconds_timeout,
        error = FALSE,
        assertions = FALSE
      )
      invisible(envir$result)
    },
    #' @description Push the name of a task to the backlog.
    #' @details `pop_backlog()` pops the tasks that can be pushed
    #'   without saturating the controller.
    #' @param name Character of length 1 with the task name to push to
    #'   the backlog.
    #' @return `NULL` (invisibly).
    #' @param controller Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    push_backlog = function(name, controller = NULL) {
      crew_assert(
        name,
        is.character(.),
        length(.) == 1L,
        !anyNA(.),
        nzchar(.),
        message = "'name' in push_backlog() must be a valid character string"
      )
      n <- length(.subset2(private, ".backlog")) + 1L
      private$.backlog[[n]] <- name
      invisible()
    },
    #' @description Pop the task names from the head of the backlog which
    #'   can be pushed without saturating the controller.
    #' @return Character vector of task names which can be pushed to the
    #'   controller without saturating it. If the controller is saturated,
    #'   `character(0L)` is returned.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    pop_backlog = function(controllers = NULL) {
      n <- .subset2(.subset2(self, "launcher"), "workers") -
        .subset2(self, "unresolved")()
      if (n < 1L) {
        return(character(0L))
      }
      backlog <- .subset2(private, ".backlog")
      out <- utils::head(x = backlog, n = n)
      private$.backlog <- backlog[-seq_len(n)]
      out
    },
    #' @description Summarize the workers and tasks of the controller.
    #' @return A data frame of summary statistics on the tasks
    #'   that ran on a worker and then were returned by `pop()` or
    #'   `collect()`.
    #'   It has one row and the following columns:
    #'   * `controller`: name of the controller.
    #'   * `tasks`: number of tasks.
    #'   * `seconds`: total number of runtime in seconds.
    #'   * `success`: total number of successful tasks.
    #'   * `error`: total number of tasks with errors, either in the R code
    #'     of the task or an NNG-level error that is not a cancellation
    #'     or crash.
    #'   * `crash`: total number of crashed tasks (where the worker exited
    #'     unexpectedly while it was running the task).
    #'   * `cancel`: total number of tasks interrupted with the `cancel()`
    #'     controller method.
    #'   * `warnings`: total number of tasks with one or more warnings.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    summary = function(controllers = NULL) {
      out <- .subset2(private, ".summary")
      if (!is.null(out)) {
        out <- tibble::new_tibble(out)
      }
      out
    },
    #' @description Cancel one or more tasks.
    #' @param names Character vector of names of tasks to cancel.
    #'   Those names must have been manually supplied by `push()`.
    #' @param all `TRUE` to cancel all tasks, `FALSE` otherwise.
    #'   `all = TRUE` supersedes the `names` argument.
    cancel = function(names = character(0L), all = FALSE) {
      crew_assert(
        all,
        isTRUE(.) || isFALSE(.),
        message = "'all' must be TRUE or FALSE"
      )
      crew_assert(
        names,
        is.character(.),
        !anyNA(.),
        nzchar(.),
        message = paste(
          "'names' must be a character vector",
          "with no missing or empty strings"
        )
      )
      tasks <- .subset2(private, ".tasks")
      if (all) {
        mirai::stop_mirai(as.list(tasks))
      }
      names <- intersect(names, names(tasks))
      for (name in names) {
        mirai::stop_mirai(.subset2(tasks, name))
      }
      invisible()
    },
    #' @description Get the process IDs of the local process and the
    #'   `mirai` dispatcher (if started).
    #' @return An integer vector of process IDs of the local process and the
    #'   `mirai` dispatcher (if started).
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    pids = function(controllers = NULL) {
      private$.client$pids()
    },
    #' @description Terminate the workers and the `mirai` client.
    #' @return `NULL` (invisibly).
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    terminate = function(controllers = NULL) {
      # https://github.com/r-lib/covr/issues/445#issuecomment-689032236
      if_any(
        condition = isTRUE(as.logical(Sys.getenv("R_COVR", "false"))),
        true = {
          private$.launcher$terminate()
          private$.client$terminate()
        },
        false = {
          private$.client$terminate() # nocov
          private$.launcher$terminate() # nocov
        }
      )
      private$.tasks <- new.env(parent = emptyenv(), hash = TRUE)
      private$.pushed <- 0L
      private$.popped <- 0L
      private$.crash_log <- new.env(parent = emptyenv(), hash = TRUE)
      private$.autoscaling <- FALSE
      private$.queue <- crew_queue()
      private$.resolved <- -1L
      invisible()
    }
  )
)
