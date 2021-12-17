library(ggplot2)
library(tidyverse)

# helpers: ----
get_children_names <- function(x) {
  sapply(x, function(i) i$name)
}

get_panel_names_and_positions <- function(grobs) {
  grob_names <- get_children_names(grobs)
  panel_names <- grep(pattern = "^panel-[0-9]+\\..*",
                      x = grob_names,
                      value = TRUE)
  panel_positions <- grep(pattern = "^panel-[0-9]+\\..*",
                          x = grob_names,
                          value = FALSE)
  list(names = panel_names, positions = panel_positions)
}

get_rect_coordinates <- function(grobs, regex = "^geom_rect\\..*") {
  names_positions <- get_panel_names_and_positions(grobs)
  panel_positions <- names_positions$positions
  panel_names <- names_positions$names
  geom_rect_coordinates <- lapply(panel_positions, function(i) {
    children <- grobs[[i]]$children
    children_names <- get_children_names(children)
    geom_rect_position <- grep(pattern = regex, children_names)
    geom_rect <- children[[geom_rect_position]]
    list(x = geom_rect$x, y = geom_rect$y)
  })
  names(geom_rect_coordinates) <- panel_names
  geom_rect_coordinates
}

get_sample_plot <- function(polar = FALSE){
  year <- 2020:2021
  month <- 1:3
  PI_name <- c("John Smith", "Adolfo Alvarez", "Patrick Nowak", "Michelle Pfeifer", "Winston Churchil", "George Washington")

  d <- expand.grid(year, month, PI_name)
  d <- as_tibble(d)
  d$value <- sample(1:1000, size = nrow(d), replace = TRUE)

  names(d) <- c("year", "month", "PI_name", "value")
  d <- d %>% mutate_at(vars(year, month, PI_name), as.factor)
  d

  p <- ggplot(
    data = d %>%
      group_by(year, month, PI_name) %>%
      summarise(n = sum(value)) %>%
      group_by(year, PI_name) %>%
      mutate(n = n / sum(n)),
    aes(y = year, x = n, fill = month)
  ) +
    geom_bar(stat = "identity") +
    facet_wrap("PI_name")

  if (polar){
    p <- p + coord_polar("x")
  }

  return(p)
}

get_panels_borders <- function(p) {
  plot(p)
  g <- gridExtra::arrangeGrob(p)
  g <- g$grobs[[1]]
  layoutNames <- ggtips:::assignLayoutNamesToPanels(g)

  totalPlotSize <- ggtips:::getGrobSize(g)
  plotWidth <- totalPlotSize$width
  plotHeight <- totalPlotSize$height
  colWidths <- sapply(
    1:length(g$widths),
    function(i) { ggtips:::gridColWidth(g, i) }
  )
  colWidths <- ggtips:::correctMargins(colWidths, plotWidth)
  rowHeights <- sapply(
    1:length(g$heights),
    function(i) { ggtips:::gridRowHeight(g, i) }
  )
  rowHeights <- ggtips:::correctMargins(rowHeights, plotHeight)

  l <- lapply(layoutNames, function(layout_name) {
    i <- which(g$layout$name == layout_name)
    panel_layout <- g$layout[i, ]
    top <- panel_layout$t
    left <- panel_layout$l
    bottom <- panel_layout$b
    right <- panel_layout$r

    width_from <- sum(colWidths[1:(left - 1)])
    width_to <- sum(colWidths[1:right])

    height_from <- sum(rowHeights[1:(top - 1)])
    height_to <- sum(rowHeights[1:bottom])

    norm_width <- c(width_from, width_to) / totalPlotSize$width
    norm_height <- c(height_from, height_to) / totalPlotSize$height

    list(normalized = c(x = norm_width, y = norm_height),
         panel_center = c(x = mean(norm_width), y = mean(norm_height)))
  })

  names(l) <- layoutNames
  l
}

get_polygons_centers <- function(p) {
  # p has to be in polar coordinates
  stopifnot("CoordPolar" %in% class(p$coordinates))

  grob <- gridExtra::arrangeGrob(p)
  s2 <- grob[[1]][[1]]

  layoutNames <- ggtips:::assignLayoutNamesToPanels(s2)

  panels_cart_to_polar <- lapply(layoutNames, function(panel) {
    i <- which(s2$layout$name == panel)
    print(sprintf("processing %s", panel))
    children_names <- get_children_names(s2$grobs[[i]]$children)
    which_bar.gTree <- grep(pattern = "^bar.gTree.*", x = children_names)


    from_cart_to_polar <- lapply(s2$grobs[[i]]$children[[which_bar.gTree]]$children, function(p) {
      cx <- cy <- 0.5

      x <- as.numeric(p$x) - cx
      y <- as.numeric(p$y) - cy

      which_origin <- intersect(which(x == 0), which(y == 0))
      if (length(which_origin) > 0){
        x <- x[-which_origin]
        y <- y[-which_origin]
      }

      r <- sqrt(x^2 + y^2)
      # force phi to be in [0, 2pi)
      phi <- ifelse(x > 0 & y >= 0,
                    atan(y / x),
                    ifelse(x > 0 & y < 0,
                           atan(y / x) + 2 * pi,
                           ifelse(x < 0,
                                  atan(y / x) + pi,
                                  ifelse(x == 0 & y > 0,
                                         pi / 2,
                                         ifelse(x == 0 & y < 0,
                                                3 * pi / 2,
                                                0)))))

      polars <- data.frame(x = x, y = y, r = r, phi = phi)
      polars <- polars[!(polars$x == 0 & polars$y == 0), ]

      plot(x = 1:length(polars$phi), y = polars$phi, main = panel, sub = p$name)

      phi_limits <- polars
      phi_limits$phi <- round(phi_limits$phi, 6)
      phi_limits <- aggregate(x ~ phi, data = phi_limits, length)
      phi_limits <- phi_limits[phi_limits$x > 1, ]
      phi_limits <- phi_limits[order(phi_limits$x, decreasing = TRUE), ]
      phi_limits <- phi_limits[seq_len(min(nrow(phi_limits), 2)), "phi"]

      phi_from <- min(phi_limits)
      phi_to <- max(phi_limits)

      if (nrow(polars[round(polars$phi, 6) > round(phi_to, 6), ]) > 0){
        phi_to <- phi_from
        polars$phi <- ifelse(
          round(polars$phi, 6) > round(phi_to, 6),
          polars$phi - 2*pi,
          polars$phi
        )
      }

      plot(x = 1:length(polars$phi), y = polars$phi, main = "after")
      polars <- polars[, c("r", "phi")]
      extrema <- apply(polars, MARGIN = 2, FUN = function(x) {
        c(min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE))
      })
      row_min <- which(rownames(extrema) == "min")
      row_max <- which(rownames(extrema) == "max")

      polar_center <- list(r = (extrema[row_min, "r"] + extrema[row_max, "r"]) / 2,
                           phi = (extrema[row_min, "phi"] + extrema[row_max, "phi"]) / 2)
      cartesian_center <- list(x = polar_center$r * cos(polar_center$phi),
                               y = polar_center$r * sin(polar_center$phi))

      plot(x, y, type = "l", asp = 1, main = "cartesian")
      points(x, y)
      points(cartesian_center$x, cartesian_center$y, col = "red", asp = 1)
      plot(polars$r, polars$phi, type = "l", asp = 1, main = "polar")
      points(polar_center$r, polar_center$phi, col = "red")

      list(polar_coordinates = polars,
           polar_vertices = extrema,
           cartesian_center = cartesian_center,
           polar_center = polar_center)
    })

    from_cart_to_polar
  })

  names(panels_cart_to_polar) <- layoutNames
  panels_cart_to_polar
}

calculate_panel_sizes <- function(panels) {
  lapply(panels, function(p) {
    list(
      width = abs(p$normalized["x1"] - p$normalized["x2"]),
      height = abs(p$normalized["y1"] - p$normalized["y2"])
    )
  })
}


# create plots and save svg ----
set.seed(12345)
p <- get_sample_plot()
p
pp <- get_sample_plot(polar = TRUE)
pp

ggsave("output/bar.svg", p)
ggsave("output/pie.svg", pp)

# find polygons centers ----
getPolygonsCenters <- function(pp) {
  par(mfrow=c(2, 2))
  poly_centers <- get_polygons_centers(pp)
  panels_borders <- get_panels_borders(pp)
  panels_sizes <- calculate_panel_sizes(panels_borders)

  polygons_normalized <- lapply(names(poly_centers), function(i) {
    polygons <- poly_centers[[i]]
    panel_width <- panels_sizes[[i]]$width
    panel_height <- panels_sizes[[i]]$height
    panel_center <- panels_borders[[i]]$panel_center

    res <- lapply(polygons, function(p) {
      data.frame(
        x = p$cartesian_center$x * panel_width + panel_center["x"],
        y = -p$cartesian_center$y * panel_height + panel_center["y"]
      )
    })
    res <- do.call(rbind, res)
  })
  out <- do.call(rbind, polygons_normalized)
  rownames(out) <- NULL
  out
}

# check if any value is greater than 1 or less than 0
sapply(polygons_normalized, function(i) {
  sapply(i, function(j) j$cartesian_center)
}) %>% unlist() %>% summary()


##### add tooltips: -----
# warning! this is unsafe method and may not work in more complex plots!
tooltips_data <- pp$data %>%
  arrange(PI_name, year, month)

k <- 1
for (i in 1:6) {
  for (j in 1:6) {
    polygons_normalized[[i]][[j]]$data <- tooltips_data[k, ]
    k <- k + 1
  }
}

#### prepare data for browser / [{x, y, tooltip}]  /: ----
ttips <- lapply(polygons_normalized, function(i) {
  lapply(i, function(j) {
    l <- lapply(j$data, function(d) {
      sprintf("<li>%s</li>", d)
    }) %>% unlist()
    res <- paste(l, collapse = "")
    res <- sprintf("<ul>%s</ul>", res)

    list(x = j$cartesian_center$x,
         y = j$cartesian_center$y,
         tooltip = res)
  })
})

jsonlite::write_json(ttips, path = "output/tooltips.json")
jsonlite::write_json(poly_centers, path = "output/polygon_centers.json")
jsonlite::write_json(panels_borders, path = "output/panels_centers.json")



# test cases (needs work on assertions) ----
if (FALSE) {
  p_test <- ggplot(data = data.frame(x = 0, y = 1),
                   aes(x = x, y = y)) +
    geom_bar(stat = "identity") +
    coord_polar("x")
  p_test
  center <- get_polygons_centers(p_test)
  center[[1]][[1]]$cartesian_center %>% unlist()


  p_test2 <- ggplot(data = data.frame(x = c(0, 0), y = c(1, 1), fac = c("A", "B")),
                    aes(x = x, y = y)) +
    geom_bar(stat = "identity") +
    coord_polar("x") +
    facet_grid("fac")
  p_test2
  center <- get_polygons_centers(p_test2)
  center[[1]][[1]]$cartesian_center %>% unlist()
  center[[2]][[1]]$cartesian_center %>% unlist()

  p_test3 <- ggplot(data = data.frame(x = c(0, 0), y = c(1, 3), fac = c("A", "B")),
                    aes(x = x, y = y)) +
    geom_bar(stat = "identity") +
    coord_polar("y") +
    facet_grid("fac")
  p_test3
  center <- get_polygons_centers(p_test3)
  center[[1]][[1]]$cartesian_center %>% unlist()
  center[[2]][[1]]$cartesian_center %>% unlist()


  p_test4 <- ggplot(data = data.frame(x = c(0, 0, 1), y = c(3, 7, 1000), fac = c("A", "B", "B")),
                    aes(x = x, y = y)) +
    geom_bar(stat = "identity") +
    coord_polar("y") +
    facet_grid("fac")
  p_test4
  center <- get_polygons_centers(p_test4)
  center[[1]][[1]]$cartesian_center %>% unlist()
  center[[2]][[1]]$cartesian_center %>% unlist()
  center[[2]][[2]]$cartesian_center %>% unlist()
}
