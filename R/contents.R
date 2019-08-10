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

#' Parse plot/layer mapping
#'
#' Converts ggplot mapping to a character vector.
#'
#' @param mapping ggplot mapping.
#'
#' @return Character.
parseMapping <- function(mapping) {
  if (is.symbol(mapping)) {
    as.character(mapping)
  } else if (rlang::is_quosure(mapping)) {
    rlang::quo_name(mapping)
  } else {
    as.character(mapping)
  }
}

#' Get layer aesthetics
#'
#' Returns a list of aesthetics for each layer in a ggplot.
#'
#' @param layer A ggplot object.
#'
#' @return A list.
#' @export
getLayerAesthetics <- function(plot) {
  layers <- plot$layers
  plotMapping <- plot$mapping
  lapply(layers, function(layer) {
    layerMapping <- if (layer$inherit.aes) {
      plotMapping
    } else {
      layer$mapping
    }
    lapply(layerMapping, parseMapping)
  })
}

#' Get layer geometries
#'
#' Returns grid-compatible geometry names for a ggplot layer.
#'
#' @param layer A ggplot layer object.
#'
#' @return A character vector.
#' @export
getLayerGeom <- function(layer) {
  geomDict <- list(
    #TODO complete the list
    # GeomLine = "polyline",
    # GeomPath = "polyline",
    GeomPoint = "points"
  )
  classes <- class(layer$geom)
  unique(unlist(geomDict[classes]))
}

#' Unmap factors
#'
unmapFactors <- function(df, origin) {
  # Order factor levels in the original data frame
  origin <- freezeFactorLevels(origin)
  # Select only factor variables
  factors <- Filter(
    function(name) { is.factor(origin[[name]]) },
    names(origin)
  )
  for (name in factors) {
    origColumn <- origin[[name]]
    if (name %in% names(df)) {
      # Map values in the column to the original values
      column <- df[[name]]
      asFactor <- factor(column, levels = unique(column))
      df[[name]] <- levels(origColumn)[asFactor]
    } else {
      if (length(origColumn) == nrow(df)) {
        # Simply add the column from the original data frame
        df[[name]] <- origColumn
      }
    }
  }
  df
}

#' Unmap aesthetics
#'
unmapAes <- function(data, mapping, plot) {
  plotData <- plot$data
  # If it's a trellis, order the input data frame
  # based on the actual order of panels
  if (!is.null(plot$facet$params$facets)) {
    facet <- plot$facet$params$facets[[1]]
    trellisVar <- parseMapping(facet)
    groups <- plotData[[trellisVar]]
    groupLevels <- levels(groups)
    plotData <- plotData[order(match(groups, groupLevels)), ]
  }

  mapply(
    function(df, map) {
      mapNames <- names(map)
      names(df) <- sapply(names(df), function(name) {
        if (name %in% mapNames) { map[[name]] } else { name }
      })
      unmapFactors(df, origin = plotData)
    },
    data,
    mapping,
    SIMPLIFY = FALSE
  )
}

#' Add custom contents to the tooltips
#'
#' For each row of the plot data, applies a callback function that returns
#' an HTML character string to be appended to the contents.
#'
addCustomContents <- function(data, callback) {
  fun <- if (is.null(callback)) {
    NULL
  } else {
    function(x) { c(.custom = callback(x)) }
  }
  lapply(data, function(df) {
    plyr::adply(df, .margins = 1L, .fun = fun)
  })
}

#' Use columns defined in variable dictionary
#'
getNamesFromVarDict <- function(df, varDict, mapping) {
  dfNames <- names(df)
  # If varDict is NULL, use all mapped columns
  if (is.null(varDict)) {
    dictNames <- setdiff(unlist(mapping), ".custom")
    varDict <- structure(as.list(dictNames), names = dictNames)
  }
  customColumn <- if (".custom" %in% dfNames) {
    df[[".custom"]]
  }
  validNames <- intersect(names(varDict), dfNames)
  if (length(validNames) == 0) {
    return(NULL)
  }
  varDict <- varDict[validNames]
  df <- df[names(varDict)]
  names(df) <- varDict

  if (!is.null(customColumn)) {
    cbind(".custom" = customColumn, df)
  } else {
    df
  }
}

#' As trans
#' 
#' Gets a proper trans object from scales package. Original function 
#' scales::as.trans() is not working properly when scales are in Imports
#' 
#' @param x character string, the scale name
#' 
#' @return scale object
as_trans <- function(x){
  trans <- get(paste0(x, "_trans"), asNamespace("scales"))
  trans()
}

#' Untransform scales
#'
untransformScales <- function(data, plotScales) {
  lapply(data, function(df) {
    if (!is.null(plotScales$x) && "x" %in% names(df)) {
      tr <- as_trans(plotScales$x)
      df[["x"]] <- tr$inverse(df[["x"]])
    }
    if (!is.null(plotScales$y) && "y" %in% names(df)) {
      tr <- as_trans(plotScales$y)
      df[["y"]] <- tr$inverse(df[["y"]])
    }
    df
  })
}

#' Get decimal places per each element in x
#'
#' @author Michal Jakubczak
decimalPlaces <- function(x) {
  sapply(
    X = strsplit(
      x = sub(
        pattern = "0+$",
        replacement = "",
        x = format( # uses options("digits") to return significant digits
          x = x,
          trim = TRUE,
          scientific = FALSE
        )
      ),
      split = ".",
      fixed = TRUE
    ),
    FUN = function(x) {
      if (is.na(x[2])) {
        0
      } else {
        nchar(x[2])
      }
    }
  )
}

#' Round data column values
#'
#' @author Michal Jakubczak
roundColumn <- function(column, maxDecimals = 3) {
  if (all(is.double(column))) {
    digits <- decimalPlaces(column)

    nDigits <- min(
      maxDecimals,
      max(digits, na.rm = TRUE)
    )
    column <- sprintf(
      paste0("%.", nDigits, "f"),
      round(column, digits = nDigits)
    )
  }
}

#' Round values
#'
#' @author Michal Jakubczak
roundValues <- function(data) {
  lapply(data, function(df) {
    df[["x"]] <- roundColumn(df[["x"]])
    df[["y"]] <- roundColumn(df[["y"]])
    df
  })
}

#'  Get data for tooltip contents
#'
getTooltipData <- function(plot, built, varDict, plotScales, callback) {
  mapping <- getLayerAesthetics(plot)
  data <- built$data
  data <- untransformScales(data, plotScales = plotScales)
  data <- roundValues(data)
  data <- unmapAes(data, mapping = mapping, plot = plot)
  data <- addCustomContents(data, callback = callback)
  lapply(data, getNamesFromVarDict, varDict = varDict, mapping = mapping)
}

#' Convert tooltip data to character strings
#'
tooltipDataToText <- function(df, width = 50) {
  df <- sapply(names(df), function(varName) {
    text <- if (varName == ".custom") {
      df[[varName]]
    } else {
      wrapped <- wrap(df[[varName]], width = width)
      paste(varName, wrapped, sep = ": ")
    }
    paste0("<li>", text, "</li>")
  })
  if (is.vector(df)) {
    df <- t(df)
  }
  sprintf("<ul>%s</ul>", apply(df, 1, paste, collapse = ""))
}
