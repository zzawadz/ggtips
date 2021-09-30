library(ggplot2)
library(ggtips)
library(dplyr)

# where do you want the resulting json files to be stored
SAVE_DIR <- system.file(file.path("barplot_tooltips", "json_output"), package = "ggtips")

singlelayers <- c("p1.rds", "p2.rds", "p3.rds")

lapply(seq_along(singlelayers), function(i) {
  p <- readRDS(system.file(file.path("barplot_tooltips", singlelayers[i]),
                           package = "ggtips"))
  varDict <- list(Species = "Species")
  gt <- gridExtra::grid.arrange(p)[[1]][[1]]

  tooltips <- ggtips:::getTooltips(
    plot = p,
    varDict = varDict,
    plotScales = NULL,
    g = gt,
    callback = NULL,
    addAttributes = TRUE
  )

  single_tooltips <- purrr::imap(tooltips, ~{
    type <- switch (.y,
                    "rect" = "bar",
                    "points" = "points"
    )

    .x[[1]]$type <- type
    .x[[1]]
  })

  names(single_tooltips)[names(single_tooltips) == "rect"] <- "bar"
  jsonlite::write_json(single_tooltips, path = file.path(SAVE_DIR, paste0("singlelayer_", i, ".json")))
})


#### multilayer plot

multilayers <- c("gg2.rds", "gg3.rds")

lapply(seq_along(multilayers), function(i) {
  p <- readRDS(system.file(file.path("barplot_tooltips", multilayers[i]),
                           package = "ggtips"))
  varDict <- list(Species = "Species")
  gt <- gridExtra::grid.arrange(p$plot)[[1]][[1]]

  tooltips <- ggtips:::getTooltips(
    plot = p$plot,
    varDict = varDict,
    plotScales = NULL,
    g = gt,
    callback = NULL,
    addAttributes = TRUE
  )
  tooltips

  multi_tooltips <- purrr::imap(tooltips, ~{
    type <- switch (.y,
                    "rect" = "bar",
                    "points" = "points"
    )

    .x[[1]]$type <- type
    .x[[1]]
  })

  names(multi_tooltips)[names(multi_tooltips) == "rect"] <- "bar"
  jsonlite::write_json(multi_tooltips, path = file.path(SAVE_DIR, paste0("multilayer_", i, ".json")))
})
