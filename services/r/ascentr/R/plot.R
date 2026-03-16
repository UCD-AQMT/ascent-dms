# # Plotting functions
# 

#' A little bespoke map for plotting values at all of the ascent sites
#'
#' @param dfs a simple features data frame with data to plot for each site as "value"
#' @param states a sf of state boundaries
#' @param point_size size of plot point (default is 4)
#'
#' @returns
#' @export
#'
#' @examples
map_ascent <- function(dfs, states = NULL, point_size = 4) {
  
  # base layer
  if (is.null(states)) {
    states_path <- system.file("/data/gis/cb_2018_us_state_5m.shp", package="ascentr")
    states <- sf::st_read(states_path)
  } 
  
  plot_limits <- c(min(dfs$value), max(dfs$value))
  
  # Remove freak states plus all the territories
  lower48 <- states |>
    filter(!NAME %in% c("Alaska", "Hawaii"),
           GEOID <= 56)
  
  sites48 <- dfs |>
    filter(site_code != "DeltaJunction")
  
  l48 <- ggplot() +
    geom_sf(data = lower48) +
    geom_sf(data = sites48, aes(color = value), size = point_size) +
    scale_color_viridis_c(limits = plot_limits) +
    coord_sf(crs = "+proj=laea +y_0=0 +lon_0=-98 +lat_0=40 +ellps=WGS84 +no_defs") +
    theme_void()

  ak <- states |>
    filter(NAME == "Alaska")
  sites_ak <- dfs |>
    filter(site_code == "DeltaJunction")
  
  
  akmap <- ggplot() +
    geom_sf(data = ak) +
    geom_sf(data = sites_ak, aes(color = value), size = point_size) +
    scale_color_viridis_c(limits = plot_limits) +
    coord_sf(crs = "+proj=laea +y_0=0 +lon_0=-140 +lat_0=65 +ellps=WGS84 +no_defs") +
    theme_void() +
    theme(legend.position = "none")
  
  # inset for southern california
  socal_sites <- dfs |>
    filter(site_code %in% c("LosAngeles", "Rubidoux", "JoshuaTree")) |>
    sf::st_transform(crs = "+proj=laea +y_0=0 +lon_0=-98 +lat_0=40 +ellps=WGS84 +no_defs")
  
  
  socal <- ggplot() +
    geom_sf(data = lower48, fill = "gray80") +
    geom_sf(data = socal_sites, aes(color = value), size = point_size) +
    scale_color_viridis_c(limits = plot_limits) +
    coord_sf(crs = "+proj=laea +y_0=0 +lon_0=-98 +lat_0=40 +ellps=WGS84 +no_defs",
             xlim = c(-1.9e6, -1.5e6),
             ylim = c(-6.5e5, -4e5)) +
    theme_void() +
    theme(panel.border = element_rect(color = "black", linewidth = 2, fill = NA),
          panel.background = element_rect(fill = "white"),
          legend.position = "none")
  
  
  p <- cowplot::ggdraw(l48) +
    cowplot::draw_plot(akmap, 0, 0, 0.3, 0.3) +
    cowplot::draw_plot(socal, 0.05, 0.34, 0.2, 0.2)
  p
  
  
}




# # Got this from the internet to crib. Doesn't work out of the box
# ggptable <- function(data, ascolour, breaks, limits, label = label){
#   # load the basic data of element positions: symbol; x; y.
#   ETable <- read.csv(file = "c:/Users/sraffuse/OneDrive - University of California, Davis/Documents/PeriodicTable/periodic_table.csv")
#   # join the as-plot data to the basic data and plot it
#   data[,"ascolour"] <- data[,ascolour]
#   left_join(ETable, data, by = "Symbol") %>%
#     ggplot() +
#     # set the gray background of each element
#     geom_point(aes(y = Graph.Period, x = Graph.Group),size = 14,shape = 15,color = "gray90") +
#     # color the element with the value
#     geom_point(aes(y = Graph.Period, x = Graph.Group, colour = ascolour),size = 13,shape = 15) +
#     # put all the symbol on the table
#     geom_text(colour = "black",size = 4,fontface = "bold",aes(label = Symbol, y = Graph.Period, x = Graph.Group)) +
#     # set the scale
#     scale_x_continuous(breaks = seq(min(ETable$Graph.Group),
#                                     max(ETable$Graph.Group)),
#                        limits = c(min(ETable$Graph.Group) - 1,
#                                   max(ETable$Graph.Group) + 1),
#                        expand = c(0,0)) +
#     scale_y_continuous(trans = "reverse", # y axis is reverse
#                        breaks = seq(min(ETable$Graph.Period),
#                                     max(ETable$Graph.Period)),
#                        limits = c(max(ETable$Graph.Period) + 1,
#                                   min(ETable$Graph.Period) - 1.5),
#                        expand = c(0,0)) +
#     # set breaks and labels in the colourbar legend
#     scale_colour_gradientn( breaks = breaks,
#                             limits = limits,
#                             colours = cm.colors(20),
#                             # colour if value is NA
#                             na.value = "grey70") +
#     # plot title (usually property and unit)
#     annotate("text", x = 8, y = 0.6,
#              vjust = 0,
#              label = label,
#              # parse required if using superscripts or subscripts
#              parse = F) #+
#     # # detail adjust for the scitific theme
#     # theme(panel.grid.major = element_blank(),
#     #       panel.grid.minor = element_blank(),
#     #       plot.margin = unit(c(0, 0, -0.85, -0.85), "line"),
#     #       axis.title = element_blank(),
#     #       axis.text = element_blank(),
#     #       axis.ticks = element_blank(),
#     #       panel.background  = element_blank(),
#     #       plot.background = element_rect(fill="transparent", colour=NA),
#     #       # center (roughly) over transition metal block
#     #       legend.position.inside = c(0.42, 0.91),
#     #       legend.justification = c(0.5, 1),
#     #       legend.direction = "horizontal",
#     #       # make the legend colourbar a little longer
#     #       legend.key.width = unit(2.5, "line"),
#     #       legend.title = element_blank(),
#     #       legend.background = element_rect(fill = "transparent"))
# }

#below code works - putting in function so it does run when loading package
periodic <- function(variables) {
  #per_table <- read.csv("c:/Users/sraffuse/OneDrive - University of California, Davis/Documents/PeriodicTable/periodic_table.csv")

  con <- get_db_connection("dataconnection")
  x <- xact_l1a_df("QueensCollege", "2025-01-01", "2025-12-31", con)
  x <- x$df
  sn <- x |>
    filter(sample_type == "sample",
           element != "Nb") |>
    mutate(sn = concentration_ng_m3 / uncertainty_ng_m3) |>
    summarise(sn = median(sn, na.rm = TRUE), .by = element) |>
    mutate(sn = as.numeric(sn)) |>
    rename(Symbol=element)

  df <- per_table |>
    left_join(sn, by = "Symbol")

  ggplot(df, aes(x = Group, y = Period, fill = sn), color = "gray90") +
    geom_tile() +
    geom_text(aes(label = Symbol)) +
    scale_y_reverse() +
    scale_fill_viridis_c(trans = "log10") +
    coord_equal() +
    labs(title = "Queens College S/N (2025)") +
    theme_void() +
    theme(legend.position = "inside",
          legend.position.inside = c(0.4, 0.7),
          legend.direction = "horizontal",
          legend.key.width = unit(0.05, "npc"))

}
