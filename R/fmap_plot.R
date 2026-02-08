#' Fresnel Map
#'
#' Function for plotting Fresnel Maps, or thematic maps that visualise spatial data to the level of equal-area concentric circular zones (or annuli).
#'
#' @param ncircles Number of concentric circular zones of equal area (i.e. Fresnel circles) including the inner circle and annuli
#' @param radius_inner Radius of innermost Fresnel circle in metres
#' @param radius_outer Radius of outermost Fresnel circle in metres
#' @param lat Latitude of the centre of the Fresnel Map
#' @param lon Longitude of the centre of the Fresnel Map
#' @param geo_centre A spatial dataset containing the coordinates of the centre of the Fresnel Map
#' @param geo_centres A spatial dataset containing the coordinates of the centres of each separate Fresnel Map
#' @param id_var Variable from geo_centres containing the location ID
#' @param geo_points A spatial dataset of points to aggregate
#' @param sum Variable from geo_points for calculating sum
#' @param mean Variable from geo_points for calculating mean
#' @param median Variable from geo_points for calculating median
#' @param count Count the number of points from geo_points. Input TRUE to count points. Defaults to FALSE
#' @importFrom dplyr "%>%"
#' @return A Fresnel Map based on aggregations of points-based data and visualised using tmap.
#' @examples
#' library(sf)
#' library(dplyr)
#'
#' # Load the sf datasets of cholera deaths and Soho pumps
#' data(cholera_deaths, soho_pumps)
#'
#' # Filter the Broad Street Pump from the Soho pumps dataset
#' bstreet_pump <- soho_pumps %>% filter(soho.pump == "Broad Street")
#'
#' # The Fresnel Map based on the Broad Street Pump
#' fmap_plot(radius_inner = 125, ncircles = 8, geo_centre = bstreet_pump, geo_points = cholera_deaths, sum = "cholera.deaths")
#'
#' # Map of multiple Fresnel Maps based on each Soho pump
#' fmap_plot(radius_inner = 125, ncircles = 2, geo_centres = soho_pumps, id_var = "soho.pump", geo_points = cholera_deaths, sum = "cholera.deaths")
#' @export

fmap_plot <- function(ncircles,
                      radius_inner = NULL, radius_outer = NULL,
                      geo_points,
                      geo_centre = NULL, lat = NULL, lon = NULL,
                      geo_centres = NULL, id_var = NULL,
                      sum = NULL, mean = NULL, median = NULL, count = FALSE) {

  df <- fmap_data(ncircles = ncircles,
                  radius_inner = radius_inner, radius_outer = radius_outer,
                  geo_points = geo_points,
                  geo_centre = geo_centre, lat = lat, lon = lon,
                  geo_centres = geo_centres, id_var = id_var,
                  sum = sum, mean = mean, median = median, count = count)

  one_centre <- !is.null(geo_centre) || (!is.null(lat) && !is.null(lon))

  if(count) {
    legend_title <- "Count"

  } else if(!is.null(sum)) {
    legend_title <- paste0("Total ", '("', sum, '")')

  } else if(!is.null(mean)) {
    legend_title <- paste0("Mean ", '("', mean, '")')

  } else {
    legend_title <- paste0("Median ", '("', median, '")')
  }

  fill_col <- colnames(df)[3]

  tmap_v4 <- utils::packageVersion("tmap") >= "4.0"

  if(tmap_v4) {
    map <- .fmap_plot_v4(df, fill_col, legend_title, one_centre)
  } else {
    map <- .fmap_plot_v3(df, fill_col, legend_title, one_centre)
  }

  map
}

#' @noRd
.fmap_plot_v3 <- function(df, fill_col, legend_title, one_centre) {

  if(one_centre) {
    map <- tmap::tm_shape(df, name = "Fresnel Map") +
      tmap::tm_fill(col = fill_col, palette = "viridis",
                    title = legend_title, id = "",
                    popup.vars = c("Zonal Area" = "zonal_area",
                                   "Radius (Metres)" = "radius",
                                   fill_col))

  } else {
    id_col <- df %>%
      dplyr::ungroup() %>%
      sf::st_drop_geometry() %>%
      dplyr::select(dplyr::last_col()) %>%
      colnames()

    map <- tmap::tm_shape(df, name = "Fresnel Map") +
      tmap::tm_fill(col = fill_col, palette = "viridis",
                    title = legend_title, id = "",
                    popup.vars = c("Zonal Area" = "zonal_area",
                                   "Radius (Metres)" = "radius",
                                   fill_col, id_col)) +
      tmap::tm_facets(by = "id", ncol = 2)
  }

  map <- map +
    tmap::tm_borders(col = "black", lwd = 0.8) +
    tmap::tm_basemap(server = c("OpenStreetMap", "Esri.WorldImagery")) +
    tmap::tm_view(view.legend.position = c("right", "top")) +
    tmap::tm_layout(frame = FALSE,
                    frame.lwd = 0,
                    legend.outside = FALSE,
                    legend.text.fontfamily = "Helvetica",
                    legend.text.size = 0.62,
                    legend.title.size = 0.8,
                    legend.title.fontface = "bold",
                    legend.position = c("left", "top"),
                    panel.label.fontfamily = "Helvetica",
                    panel.label.fontface = "bold",
                    panel.label.size = 1.1,
                    panel.label.bg.color = NA) +
    tmap::tmap_options(show.messages = FALSE, show.warnings = FALSE)

  map
}

#' @noRd
.fmap_plot_v4 <- function(df, fill_col, legend_title, one_centre) {

  if(one_centre) {
    map <- tmap::tm_shape(df, name = "Fresnel Map") +
      tmap::tm_fill(fill = fill_col,
                    fill.scale = tmap::tm_scale_continuous(values = "viridis"),
                    fill.legend = tmap::tm_legend(title = legend_title,
                                                  text.size = 0.5,
                                                  title.size = 0.7,
                                                  title.fontface = "bold",
                                                  position = tmap::tm_pos_out("left", "center"),
                                                  frame = FALSE)) +
      tmap::tm_borders(col = "black", lwd = 0.8)

  } else {
    map <- tmap::tm_shape(df, name = "Fresnel Map") +
      tmap::tm_fill(fill = fill_col,
                    fill.scale = tmap::tm_scale_continuous(values = "viridis"),
                    fill.free = FALSE,
                    fill.legend = tmap::tm_legend(title = legend_title,
                                                  text.size = 0.5,
                                                  title.size = 0.7,
                                                  title.fontface = "bold",
                                                  position = tmap::tm_pos_out("left", "center"),
                                                  frame = FALSE)) +
      tmap::tm_borders(col = "black", lwd = 0.8) +
      tmap::tm_facets(by = "id", ncol = 2)
  }

  map <- map +
    tmap::tm_layout(frame = FALSE, panel.label.bg.color = "#ffffff", panel.label.frame = FALSE) +
    tmap::tmap_options(show.messages = FALSE, show.warnings = FALSE)

  map
}