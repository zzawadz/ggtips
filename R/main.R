# This file is part of ggtips (R package to display tooltips on svg ggplot)
#
# @author Pawel Piatkowski
#
# Copyright 2018 Genentech, Inc.
#
# Permission is hereby granted, free of charge, to any person obtaining
# copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


#' Render plot with tooltips
#'
#' @param plot A \link{ggplot} object.
#' @param varDict Variable dictionary in the following format:
#' \code{list(<variable> = <label>, ...)},
#' where \code{<variable>} is a valid name of a variable mapped
#' with \link{aes}, and \code{<label>} is a character string.
#' It defines the composition of information displayed in tooltips.
#' @param plotScales A list with two fields: x and y. Defines axis
#' scales (transformations) for the purpose of displaying original
#' values in tooltips. If NULL (default), values are displayed "as is".
#' @param callback Callback function for adding custom content to the tooltips
#' (see the example app).
#' @param point.size Point size for calibrating hovering accuracy (optional).
#' @param dpi DPI value (optional).
#' @param width Plot width (in inches; optional).
#' @param height Plot height (in inches; optional).
#' @param ... Additional parameters passed to \link{ggsave}.
#'
#' @note Non-numeric variables are only back-transformed to their original
#' values if they are passed to \link{ggplot} as factors.
#'
#' @export
renderWithTooltips <- function(plot,
                               varDict,
                               plotScales = NULL,
                               callback = NULL,
                               point.size = 10,
                               dpi = 72,
                               width = NA,
                               height = NA,
                               ...) {
  if (!requireNamespace("shiny")) {
    stop("renderWithTooltips() requires Shiny")
  }

  shiny::installExprFunction({
    outfile <- tempfile(fileext = ".svg")
    grob <- gridExtra::arrangeGrob(plot)
    data <- ggtips::saveAndGetTooltips(
      plot = plot,
      g = grob[[1]][[1]],
      callback = callback,
      filename = outfile,
      varDict = varDict,
      plotScales = plotScales,
      dpi = dpi,
      width = width,
      height = height,
      limitsize = FALSE,
      ...
    )
    svg <- ggtips:::readSvgAndRemoveTextLength(outfile)
    id <- as.numeric(Sys.time())*1000
    Encoding(svg) <- "UTF-8"
    data <- list(
      width = width,
      height = height,
      data = data,
      size = point.size
    )
    script <- paste0(
      "<script>",
      "$('[data-id=\"%s\"]').closest('.shiny-html-output').ggtips(%s);",
      "</script>"
    )
    shiny::tagList(
      shiny::HTML(svg),
      ggtips:::getDependencies(),
      htmltools::tags$div(`data-id` = id, class = "ggtips-tooltip"),
      shiny::HTML(
        sprintf(script, id, jsonlite::toJSON(data, auto_unbox = TRUE))
      )
    )
  }, name = "func", eval.env = parent.frame(), quoted = FALSE)

  shiny::createRenderFunction(func, function(result, shinysession, name, ...) {
    if (is.null(result) || length(result) == 0) return(NULL)
    shiny:::processDeps(result, shinysession)
  }, shiny::uiOutput, list())
}

#' Run a demo app for ggtips
#'
#' Launches a Shiny app demonstrating the capabilities of ggtips.
#'
#' @export
demo <- function(host = "0.0.0.0", port = 3838L) {
  if (!require("shiny")) {
    stop("Shiny is required to run the demo")
  }
  shiny::runApp(
    appDir = system.file("example", package = "ggtips"),
    host = host,
    port = port
  )
}
