#' Fresnel Map
#'
#' Function for plotting Fresnel Maps, or thematic maps that visualise spatial data to the level of equal-area concentric circular zones (or annuli).
#'
#' @param ncircles Number of concentric circular zones of equal area including the inner circle and annuli
#' @param radius_inner Radius of innermost Fresnel circle in metres
#' @param radius_outer Radius of outermost Fresnel circle in metres
#' @param lat Latitude of the centre of the Fresnel Map
#' @param lon Longitude of the centre of the Fresnel Map
#' @param geo_centre A spatial dataset containing the coordinates of the centre of the Fresnel Map
#' @param geo_points A spatial dataset of points to aggregate
#' @param sum Variable from geo_points for calculating sum
#' @param mean Variable from geo_points for calculating mean
#' @param median Variable from geo_points for calculating median
#' @param count Count the number of points from geo_points. Input TRUE to count points. Defaults to FALSE
#' @return A Fresnel Map based on aggregations of points-based data and visualised using tmap.
#' @examples
#' # Load the sf datasets of cholera deaths and Soho pumps
#' data(cholera_deaths, soho_pumps)
#'
#' # Filter the Broad Street Pump from the Soho pumps dataset
#' bstreet_pump = soho_pumps %>% filter(soho.pump == "Broad Street")
#'
#' # The Fresnel Map
#' fmap_plot(radius_inner = 125, ncircles = 8, geo_centre = bstreet_pump, geo_points = cholera_deaths, sum = "cholera.deaths")
#' @export

fmap_plot = function(ncircles, radius_inner = NULL, radius_outer = NULL, lat = NULL, lon = NULL, geo_centre = NULL, geo_points, sum = NULL, mean = NULL, median = NULL, count = F) {

  df = fmap_data(ncircles, radius_inner = radius_inner, radius_outer = radius_outer, lat = lat, lon = lon, geo_centre = geo_centre, geo_points, sum = sum, mean = mean, median = median, count = count)

  df = df %>%
    dplyr::select(-geometry)

  if(is.null(mean) && is.null(sum) && is.null(median) && count == T) {
    legend_title = "Count"

  } else if(is.null(sum) != T && is.null(mean) && is.null(median) && count == F) {
    legend_title = paste0("Total ", '("', sum, '")')

  } else if(is.null(mean) != T && is.null(sum) && is.null(median) && count == F) {
    legend_title = paste0("Mean ", '("', mean, '")')

  } else if(is.null(median) != T && is.null(sum) && is.null(mean) && count == F) {
    legend_title = paste0("Median ", '("', median, '")')

  } else {
    stop('error in aggregation parameter', call. = F)
  }

  map = tm_shape(df, name = "Fresnel Map") +
    tm_fill(col = colnames(df)[3], palette = "viridis", title = legend_title, id = "", popup.vars = c("Zonal Area" = "zonal_area", "Radius (Metres)" = "radius", colnames(df)[3])) +
    tm_borders(col = "black", lwd = 0.8) +
    tm_basemap(server = c("OpenStreetMap", "Esri.WorldImagery")) +
    tm_view(view.legend.position = c("right", "top")) +
    tm_layout(frame = F,
              legend.outside = F,
              legend.text.fontfamily = "Helvetica",
              legend.text.size = 0.6,
              legend.title.size = 0.8,
              legend.title.fontface = "bold",
              legend.position = c("left", "top")) +
    tmap_options(show.messages = F, show.warnings = F)

  map
}
