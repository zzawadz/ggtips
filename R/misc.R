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

#' Auxiliary function removing `textLength` and `lengthAdjust` from SVG data
#'
readSvgAndRemoveTextLength <- function(filename){
  gsub(
    pattern = paste0(" (textLength|lengthAdjust)='(.*?)'"),
    replacement = '',
    x = readChar(filename, file.info(filename)$size, useBytes = TRUE)
  )
}

#' Freeze factor levels
#'
#' Reorders factor levels in a data frame, and - optionally -
#' converts character columns to factors.
#'
#' @param df A data frame.
#' @param characterToFactor If TRUE, converts strings to factors.
#'
#' @return A data frame.
freezeFactorLevels <- function(df, characterToFactor = FALSE){
  tibble::as_tibble(lapply(
    X = df,
    FUN = function(x){
      if (is.factor(x) || (characterToFactor && is.character(x))){
        factor(x = x, levels = unique(x))
      } else {
        x
      }
    }
  ))
}

#' Wrap
#'
#' @param x character, values to be wrapped
#' @param width numeric, max number of characters
#'
#' @return Wrapped x
wrap <- function(x, width, by = "\n") {
  x <- gsub("[\r\n]", "", x)
  sapply(
    X = x,
    FUN = wrapProportionalString,
    width = width,
    by = by
  )
}

#' Wrap proportional-font string
#'
#' Wraps a string, taking into account different widths of characters in
#' text rendered with proportional fonts ('W' is much wider than 'i').
#'
#' @param string character string.
#' @param width numeric; maximum width of the wrapped text.
#' @param by line separator (default: newline character).
#'
#' @return wrapped character string.
wrapProportionalString <- function(string, width, by = "\n") {
  elements <- splitLongWords(
    stringi::stri_split_boundaries(string)[[1]],
    width
  )
  lineWidth <- 0
  output <- c()
  for (element in elements) {
    elemWidth <- getStringWidth(element)
    lineWidth <- lineWidth + elemWidth
    if (lineWidth > width && length(output) > 0) {
      output <- c(output, by)
      lineWidth <- elemWidth
    }
    output <- c(output, element)
  }
  outputString <- paste(output, collapse = "")
  gsub(" *\\n *", "\n", outputString)
}

#' Split long words
#'
#' Takes a vector of words (character strings) and splits each word
#' longer than the specified length (in characters) using separators
#' passed as a regular expression.
#'
#' @param words a character vector.
#' @param width integer; maximal width (in characters) of a word.
#' @param separators character string with a regular expression.
#' The word is cut *after* each occurrence of the pattern.
#'
#' @return an updated character vector.
splitLongWords <- function(words, width, separators = "[,;:/\\?\\!\\-\\%]") {
  unlist(lapply(
    words,
    function(word) {
      wordLen <- nchar(word)
      if (wordLen <= width) {
        word
      } else {
        # find separators
        breaks <- gregexpr(separators, word)[[1]]
        # if no separators were found, cut the word at width
        if (identical(c(breaks), -1L)) {
          breaks <- seq(from = width, to = wordLen, by = width)
        }
        # if the last character is a separator, remove it from the list
        if (dplyr::last(breaks) == wordLen) {
          breaks <- head(breaks, -1)
        }
        from <- c(1, breaks + 1)
        to <- c(breaks, wordLen)
        substring(word, first = from, last = to)
      }
    }
  ))
}

#' Get width of a proportional string
#'
#' Returns estimated width (in "standard characters") of a string,
#' as if it were rendered using a proportional font.
#' This function is used for the purpose of scaling text containers
#' when the actual width of the text is not yet known (before rendering
#' the text).
#'
#' @param string character string.
#'
#' @return numeric (floating-point) value.
getStringWidth <- function(string) {
  chars <- strsplit(string, split = "", fixed = TRUE)[[1]]
  sum(sapply(chars, getCharWidth, USE.NAMES = FALSE))
}

#' Get width of a proportional character
#'
#' Estimates how the character compares to a "standard character", like
#' \code{a} or \code{s}.
#' Returns a single floating-point value: 1 for characters of standard
#' width, >1 for wider characters, <1 for narrower characters.
#'
#' @param char character string of length 1 (single character).
#'
#' @return numeric (floating-point) value.
getCharWidth <- function(char) {
  charWidths <- c(
    "ijlI()[].,| " = 0.5,
    "rt" = 0.7,
    "wBCDGHKNOQRU" = 1.2,
    "mMW" = 1.5
  )
  whichType <- grep(char, names(charWidths), fixed = TRUE)
  `if`(length(whichType) == 0, 1, unname(charWidths[whichType]))
}

#' load Front-End dependencies (use this function in any rendered content and
#' it will the files only ones)
getDependencies <- function() {
  htmltools::htmlDependency(
    name = "ggtips",
    package = "ggtips",
    version = packageVersion("ggtips"),
    src = "ggtips",
    script = "ggtips.js",
    stylesheet = "ggtips.css"
  )
}
