name_worker <- function(launcher, worker) {
  paste("crew", launcher, worker, sep = "-")
}

name_task_tempfile <- function() {
  basename(tempfile(pattern = "unnamed_task_"))
}

name_task_nanonext <- function() {
  paste0("unnamed_task_", nanonext::random(n = 32L))
}

is_named <- function(x) {
  if (!is.list(x) || length(x) < 1L) {
    return(TRUE)
  }
  n <- unique(names(x))
  !anyNA(n) && (length(n) == length(x)) && all(nzchar(n))
}

eval_tidyselect <- function(expr, choices) {
  if (is.null(rlang::quo_squash(expr)) || !length(choices)) {
    return(character(0L))
  }
  choices <- unique(choices)
  names(choices) <- choices
  out <- tidyselect::eval_select(
    expr = expr,
    data = choices,
    strict = FALSE
  )
  out <- names(out)
  crew_assert(
    is.character(out),
    message = paste(
      "tidyselect failed. Please supply a valid tidyselect expression",
      "such as everything() or any_of(\"this_field\")."
    )
  )
  out
}
