# This file is part of colorplaner
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
# along with colorplaner.  If not, see <http://www.gnu.org/licenses/>.

#' colorplaner: ggplot2 Extension to Visualize Two Variables Per Color
#' Aesthetic through Color Space Projection
#'
#' Add additional dimensionality to visualizations by using the color and/or
#' fill aesthetics to convey the values of two continuous variables each. By
#' projecting variable values onto YUV color space, a scale is created that
#' allows viewers to intuitively determine the values of both variables from the
#' single displayed color. Includes two new scales and a new guide for ggplot2.
#' See \code{\link{scale_color_colorplane}} for usage.
#'
#' @section Requirement for Package Attachment:
#' At present, \code{guide_colorplane} will only function when the colorplaner
#' package is attached to the search list. For scripting or interactive use,
#' use \code{library(colorplaner)}. For package development, add colorplaner
#' to the Depends list in your DESCRIPTION file.
#'
#' This requirement exists because
#' ggplot2 guides function through the use of S3 generics and methods, but the
#' generic functions are not exported from the ggplot package. Without access
#' to the generics, the methods for the colorplane guide cannot be properly
#' registered and will only be found by the dispatcher if in the search path.
#'
#' Check \url{https://github.com/wmurphyrd/colorplaner/issues/27} for current
#' status and progress towards resolving this issue.
#'
#' @section Warning Message About Ignoring Unknown Aesthetics:
#' Layers now produce a warning message when unrecognized aesthetics are found
#' but have no mechanism for notifying them of aesthetics handled by scales.
#' The warning can be avoided by mapping \code{color2}/\code{fill2} at the plot
#' level (i.e. in the initial \code{ggplot()} statement). If you want to avoid
#' colorplane mapping on all layers, map \code{color}/\code{fill} only on the
#' layers you want, as in the example below.
#'
#' @examples
#' library(ggplot2)
#' ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
#'   colour2 = Petal.Width)) +
#'  geom_point(aes(colour = Petal.Length)) +
#'  geom_line(aes(linetype = Species)) +
#'  scale_color_colorplane()
#'
#'
#' @name colorplaner
#' @import ggplot2
#' @importFrom scales rescale
#' @importFrom scales censor
NULL
