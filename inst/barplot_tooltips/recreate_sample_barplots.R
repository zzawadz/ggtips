library(ggplot2)
library(ggtips)
library(dplyr)

p1 <- ggplot(
  data = mtcars %>% count(cyl, gear),
  aes(x = cyl, y = n, fill = gear)
) + geom_bar(stat = "identity")

p2 <- ggplot(
  data = mtcars %>%
    mutate(gear = as.factor(gear))  %>%
    count(cyl, gear),
  aes(x = cyl, y = n, fill = gear)
) +
  geom_bar(stat = "identity") +
  facet_wrap(facets = "gear")

p3 <- ggplot(
  data = mtcars %>%
    mutate(hp_bin = cut(hp, breaks = 10),
           gear = as.factor(gear)) %>%
    count(gear, hp_bin),
  aes(x = gear, y = n, fill = hp_bin)
) +
  geom_bar(stat = "identity") +
  facet_wrap(facets = "hp_bin")
p3

build_test_plot <- function(p) {
  testPlot <- list(
    plot = p,
    grob = ggplot_build(p)
  )
}

plots <- lapply(list(p1, p2, p3), build_test_plot)
save_names <- c("barplot_single.json", "barplot_facets.json", "barplot_complex.json")

save_dir <-

mapply(
  function(p, file) {
    varDict <- list(cyl = "cylinder", gear = "gear")
    gt <- gridExtra::grid.arrange(p$plot)[[1]][[1]]

    tooltips <- ggtips:::getTooltips(
      plot = p$plot,
      varDict = varDict,
      plotScales = NULL,
      g = gt,
      callback = NULL,
      addAttributes = TRUE
    )
    tooltips$rect[[1]]$type <- "bar"

    save_path <- file.path(save_dir, file)
    jsonlite::write_json(tooltips$rect[[1]] , path = save_path)
  },
  plots,
  save_names
)
