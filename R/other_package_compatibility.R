# This file is part of colorplaner. Code in this file is modified or
# redistributed from scales and ggplot2 as noted below, 2016-09-18.
#
# colorplaner is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, version 2.
#
# colorplaner is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Foobar.  If not, see <http://www.gnu.org/licenses/>.

# copies of utility functions needed from other packages that are not currently
# exported or that need tweaking to work in this package namespace

#### scales ####
# This is a modified version of as.trans from package scales, copyright
# Hadley Wickham, 2010-2014 and distributed under an MIT license
# This version of as.trans will work without importing all *_trans functions
# from scales (unlike scales::as.trans)
as.trans <- function(x) {
  if (scales::is.trans(x)) return(x)
  get(paste0(x, "_trans"), asNamespace("scales"), mode = "function")()
}

#### ggplot2 ####
# The remainder of this file is a portion of ggplot2 that is
#distributed unmodified. Copyright RStudio, 2016 and distributed under the GPL-2
#license.
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
