SAVE_SVG_DIR <- system.file(file.path("barplot_tooltips", "svg_output"), package = "ggtips")

rds_plots <- c("p1.rds", "p2.rds", "p3.rds", "gg2.rds", "gg3.rds")

lapply(seq_along(rds_plots), function(i) {
  p <- readRDS(system.file(file.path("barplot_tooltips", rds_plots[i]),
                           package = "ggtips"))
  if ("ggplot_built" %in% class(p)) {
    p <- p$plot
  }
  fn <- paste0("p", i, ".svg")
  ggplot2::ggsave(plot = p, filename = fn, device = "svg", path = SAVE_SVG_DIR)
})
