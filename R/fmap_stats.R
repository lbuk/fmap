#' Summary Stats from the Fresnel Map
#'
#' Function for calculating summary statistics from Fresnel Maps, or thematic maps that visualise spatial data to the level of equal-area concentric circular zones (or annuli).
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
#' @return Summary statistics from the Fresnel Map based on aggregated data from the Fresnel circles.
#' @examples
#' # Load the sf datasets of cholera deaths and Soho pumps
#' data(cholera_deaths, soho_pumps)
#'
#' # Filter the Broad Street Pump from the Soho pumps dataset
#' bstreet_pump = soho_pumps %>% filter(soho.pump == "Broad Street")
#'
#' # Summary stats from the Fresnel Map for the Broad Street Pump
#' fmap_stats(radius_inner = 125, ncircles = 8, geo_centre = bstreet_pump, geo_points = cholera_deaths, sum = "cholera.deaths")
#'
#' # Summary stats from multiple Fresnel Maps based on each Soho pump
#' fmap_stats(radius_outer = 150, ncircles = 2, geo_centres = soho_pumps, id_var = "soho.pump", geo_points = cholera_deaths, sum = "cholera.deaths")
#' @export

fmap_stats = function(ncircles, radius_inner = NULL, radius_outer = NULL, geo_points, lat = NULL, lon = NULL, geo_centre = NULL, geo_centres = NULL, id_var = NULL, sum = NULL, mean = NULL, median = NULL, count = F) {

  df = fmap_data(ncircles = ncircles, radius_inner = radius_inner, radius_outer = radius_outer, geo_points = geo_points, lat = lat, lon = lon, geo_centre = geo_centre, geo_centres = geo_centres, id_var = id_var, sum = sum, mean = mean, median = median, count = count)

  stats = df %>%
    tibble() %>%
    dplyr::select(-geometry)

  stats
}
