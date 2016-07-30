# copies of utility functions needed from other packages that are not currently
# exported or that need tweaking to work in this package namespace

#### scales ####
# this version of as.trans will work without importing all *_trans functions
# from scales (unlike scales::as.trans)
as.trans <- function(x) {
  if (scales::is.trans(x)) return(x)
  get(paste0(x, "_trans"), asNamespace("scales"), mode = "function")()
}

#### ggplot2 ####
`%||%` <- function (a, b)
{
  if (!is.null(a))
    a
  else b
}

Range <- ggplot2::ggproto("Range", NULL,
                 range = NULL,
                 reset = function(self) {
                   self$range <- NULL
                 }
)

RangeContinuous <- ggproto("RangeContinuous", Range,
  train = function(self, x) {
    self$range <- scales::train_continuous(x, self$range)
  }
)

check_breaks_labels <- function(breaks, labels) {
  if (is.null(breaks)) return(TRUE)
  if (is.null(labels)) return(TRUE)

  bad_labels <- is.atomic(breaks) && is.atomic(labels) &&
    length(breaks) != length(labels)
  if (bad_labels) {
    stop("`breaks` and `labels` must have the same length", call. = FALSE)
  }

  TRUE
}

element_render <- function(theme, element, ..., name = NULL) {

  # Get the element from the theme, calculating inheritance
  el <- ggplot2::calc_element(element, theme)
  if (is.null(el)) {
    message("Theme element ", element, " missing")
    return(ggplot2::zeroGrob())
  }

  ggname(paste(element, name, sep = "."), ggplot2::element_grob(el, ...))
}

ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

is.waive <- function(x) inherits(x, "waiver")
