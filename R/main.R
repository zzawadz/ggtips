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
#' @param tooltip.width width of the tooltip
#' @param dpi DPI value (optional).
#' @param width Plot width (in inches; optional).
#' @param height Plot height (in inches; optional).
#' @param customGrob optional grob object. It allows to pass original plot with
#' grob manipulations.
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
                               tooltip.width = 220,
                               dpi = 72,
                               width = NA,
                               height = NA,
                               customGrob = NULL,
                               tolerance = 0.05,
                               follow = FALSE,
                               ...) {
  if (!requireNamespace("shiny")) {
    stop("renderWithTooltips() requires Shiny")
  }

  shiny::installExprFunction({
    res <- ggtips::getSvgAndTooltipdata(
      plot = plot,
      varDict = varDict,
      plotScales = plotScales,
      callback = callback,
      point.size = point.size,
      dpi = dpi,
      width = width,
      height = height,
      customGrob = customGrob,
      ...
    )
    ggtips::htmlWithGivenTooltips(
      svg = res$svg,
      data = res$data,
      height = height,
      width = width,
      tooltip.width = tooltip.width,
      point.size = point.size,
      follow = follow,
      tolerance = tolerance
    )
  }, name = "func", eval.env = parent.frame(), quoted = FALSE)

  shiny::createRenderFunction(func, function(result, shinysession, name, ...) {
    if (is.null(result) || length(result) == 0) return(NULL)
    shiny:::processDeps(result, shinysession)
  }, shiny::uiOutput, list())
}

#' Sets css variables for styling tooltip div
#'
#' @param tooltip.width numeric; can be a single number or two element vector for min and max values
#' @details A vector with mixed units can be passed to the function, e.g. \code{c("220px", "80%")}.
#' In this case the min and max widths will be set by position, i.e. \code{--ggtips-min-width} will
#' be set to \code{220px} and \code{--ggtips-max-width} will be set to \code{80%}. If units are
#' the same, e.g. \code{c("220px", "420px")}, numerical parts of the elements are comapred with use
#' of \code{min} and \code{max} functions. If the input is numeric it will be treated as value
#' expressed in pixels, e.g. 40 will be treated as 40px.
#'
#' @return character;
tooltipStyle <- function(tooltip.width) {
  tooltipLenght <- length(tooltip.width)

  if(tooltipLenght == 0 || any(is.na(tooltip.width)) || any(is.null(tooltip.width))) {
    return()
  }

  if(is.numeric(tooltip.width)) {
    stopifnot(all(tooltip.width >= 0))
    tooltip.width <- paste0(tooltip.width, "px")
  }

  if (length(tooltip.width) == 1) {
    val <- gsub(pattern = "([0-9]+)[a-z%]*", x = tooltip.width, replacement = "\\1")
    stopifnot(val >= 0)
    minWidth <- maxWidth <- tooltip.width
  } else {
    vals <- gsub(pattern = "([0-9]+)[a-z%]*", x = tooltip.width, replacement = "\\1")
    vals <- as.numeric(vals)
    stopifnot(all(vals >= 0))
    units <- gsub(pattern = "[0-9]+([a-z%]*)", x = tooltip.width, replacement = "\\1")

    if(length(unique(units)) == 1) {
      # values are in the same units, e.g. px or % and can be compared
      minWidth <- paste0(min(vals), units[1])
      maxWidth <- paste0(max(vals), units[1])
    } else {
      # units are mixed, e.g. px and %; we cannot use min and max, instead take it by position
      minWidth <- tooltip.width[1]
      maxWidth <- tooltip.width[2]
    }
  }

  paste0("--ggtips-max-width:", maxWidth, "; --ggtips-min-width:", minWidth)
}


#' Render plot with given tooltips data
#'
#' @param svg A SVG plot object.
#' @param data List with tooltip data.
#' @param width Plot width (in inches; optional).
#' @param height Plot height (in inches; optional).
#' @param point.size Point size for calibrating hovering accuracy (optional).
#' @param tooltip.width width of the tooltip; may be a numeric or character two-element vector
#'
#' @export
htmlWithGivenTooltips <- function(svg,
                                  data,
                                  height = NA,
                                  width = NA,
                                  tooltip.width = "220px",
                                  follow = FALSE,
                                  point.size = 10,
                                  tolerance = 0.05) {
  ggtips.arg <- if (length(data) == 0) {
    'unbind'
  } else {
    list(
      data = data,
      width = width,
      height = height,
      size = point.size,
      follow = follow,
      tolerance = tolerance
    )
  }

  id <- as.numeric(Sys.time())*1000

  ## Timeout is to run code on next render (even loop cycle)
  script <- paste0(
    "<script>",
    "setTimeout(function() {",
      "$('[data-id=\"%s\"]').closest('.shiny-html-output').ggtips(%s);",
    "}, 0)",
    "</script>"
  )

  shiny::tagList(
    shiny::HTML(svg),
    getDependencies(),
    htmltools::tags$div(
      `data-id` = id,
      style = tooltipStyle(tooltip.width),
      class = "ggtips-tooltip"
    ),
    shiny::HTML(
      sprintf(script, id, jsonlite::toJSON(ggtips.arg, auto_unbox = TRUE))
    )
  )
}

#' Render SVG object and return it with tooltip data
#'
#' @param plot A \link{ggplot} object.
#' @param varDict Variable dictionary in the following format:
#' \code{list(<variable> = <label>, ...)},
#' where \code{<variable>} is a valid name of a variable mapped
#' with \link{aes}, and \code{<label>} is a character string.
#' It defines the composition of information displayed in tooltips.
#' if argument is NULL it will not generate tooltip data, that may speed up
#' generating of plots.
#' @param plotScales A list with two fields: x and y. Defines axis
#' scales (transformations) for the purpose of displaying original
#' values in tooltips. If NULL (default), values are displayed "as is".
#' @param callback Callback function for adding custom content to the tooltips
#' (see the example app).
#' @param point.size Point size for calibrating hovering accuracy (optional).
#' @param dpi DPI value (optional).
#' @param width Plot width (in inches; optional).
#' @param height Plot height (in inches; optional).
#' @param customGrob optional grob object. It allows to pass original plot with
#' grob manipulations.
#' @param ... Additional parameters passed to \link{ggsave}.
#'
#' @note Non-numeric variables are only back-transformed to their original
#' values if they are passed to \link{ggplot} as factors.
#'
#' @export
getSvgAndTooltipdata <- function(plot,
                                 varDict,
                                 plotScales = NULL,
                                 callback = NULL,
                                 point.size = 10,
                                 dpi = 72,
                                 width = NA,
                                 height = NA,
                                 customGrob = NULL,
                                 addAttributes = FALSE,
                                 ...) {
  outfile <- tempfile(fileext = ".svg")

  # avoid lazy eval to use correct directory
  plot <- plot
  customGrob <- customGrob

  currentDir <- getwd()
  setwd(tempdir())
  # arrangeGrob produces Rplots.pdf which may cause permission issue when run on shiny server
  # to be more precise, ggplot2:::ggplot_gtable.ggplot_built is the origin of the issue
  grob <- gridExtra::arrangeGrob(`if`(is.null(customGrob), plot, customGrob))
  setwd(currentDir)

  data <- if (is.null(varDict)) {
    ggplot2::ggsave(
      plot = grob,
      filename = outfile,
      dpi = dpi,
      width = width,
      height = height,
      limitsize = FALSE,
      ...
    )
    NULL
  } else {
    saveAndGetTooltips(
      plot = grob,
      ggPlotObj = plot,
      g = grob[[1]][[1]],
      callback = callback,
      filename = outfile,
      varDict = varDict,
      plotScales = plotScales,
      dpi = dpi,
      width = width,
      height = height,
      limitsize = FALSE,
      addAttributes = addAttributes,
      ...
    )
  }
  svg <- readSvgAndRemoveTextLength(outfile)
  Encoding(svg) <- "UTF-8"

  list(
    svg = svg,
    data = data
  )
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

#' Helper function that can be used to render GGPlot with ggtips in renderUI
#'
#' @export
plotWithTooltips <- function(plot,
                             varDict,
                             plotScales = NULL,
                             callback = NULL,
                             point.size = 10,
                             dpi = 72,
                             width = NA,
                             height = NA,
                             customGrob = NULL,
                             follow = FALSE,
                             tolerance = 0.05,
                             ...) {
  res <- ggtips::getSvgAndTooltipdata(
    plot = plot,
    varDict = varDict,
    plotScales = plotScales,
    callback = callback,
    point.size = point.size,
    dpi = dpi,
    width = width,
    height = height,
    customGrob = customGrob,
    ...
  )
  ggtips::htmlWithGivenTooltips(
    svg = res$svg,
    data = res$data,
    height = height,
    width = width,
    point.size = point.size,
    follow = follow,
    tolerance = tolerance
  )
}
