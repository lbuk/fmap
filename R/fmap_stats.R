#' Summary Stats from the Fresnel Map
#'
#' Function for calculating summary statistics from Fresnel Maps, or thematic maps that visualise spatial data to the level of equal-area concentric circles (or annuli).
#'
#' @param ncircles Number of concentric circles of equal area including the inner circle and annuli
#' @param radius_inner Radius of innermost circle in metres
#' @param radius_outer Radius of outermost circle in metres
#' @param lat Latitude of the centre of the Fresnel Map
#' @param lon Longitude of the centre of the Fresnel Map
#' @param geo_centre A spatial dataset containing the coordinates of the centre of the Fresnel Map
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
#' # Summary stats from the Fresnel Map
#' fmap_stats(radius_inner = 125, ncircles = 8, geo_centre = bstreet_pump, geo_points = cholera_deaths, sum = "cholera.deaths")
#' @export

fmap_stats = function(ncircles, radius_inner = NULL, radius_outer = NULL, lat = NULL, lon = NULL, geo_centre = NULL, geo_points, sum = NULL, mean = NULL, median = NULL, count = F) {

  df_fc = fmap::fcircles(ncircles = ncircles, radius_inner = radius_inner, radius_outer = radius_outer, lat = lat, lon = lon, geo_centre = geo_centre)

  crs_aeqd = st_crs(df_fc)

  if(grepl(x = class(geo_points)[1], pattern = "sf", ignore.case = T) != T && grepl(x = class(geo_points)[1], pattern = "sp", ignore.case = T) != T) {
    stop('input geo_points as a points-based spatial dataset', call. = F)

  } else {
    geo_points = geo_points %>%
      st_as_sf() %>%
      st_transform(crs_aeqd)
  }

  if(is.null(sum) && is.null(mean) && is.null(median) && count != T) {
    stop('no aggregation inputted', call. = F)

  } else if(is.null(mean) && is.null(sum) && is.null(median) && count == T) {
    fmap_stats = df_fc %>%
      mutate(count = lengths(st_intersects(., geo_points))) %>%
      tibble() %>%
      dplyr::select(zonal_area, radius, count)

    fmap_stats

  } else if(is.null(sum) != T && is.null(mean) && is.null(median) && count == F) {
    fmap_stats = df_fc %>%
      st_join(geo_points) %>%
      tibble() %>%
      group_by(zonal_area, radius) %>%
      dplyr::summarise(sum = sum(!! sym(sum), na.rm = T)) %>%
      dplyr::select(zonal_area, radius, sum)

    fmap_stats

  } else if(is.null(mean) != T && is.null(sum) && is.null(median) && count == F) {
    fmap_stats = df_fc %>%
      st_join(geo_points) %>%
      tibble() %>%
      group_by(zonal_area, radius) %>%
      dplyr::summarise(mean = mean(!! sym(mean), na.rm = T)) %>%
      dplyr::select(zonal_area, radius, mean)

    fmap_stats

  } else if(is.null(median) != T && is.null(sum) && is.null(mean) && count == F) {
    fmap_stats = df_fc %>%
      st_join(geo_points) %>%
      tibble() %>%
      group_by(zonal_area, radius) %>%
      dplyr::summarise(median = median(!! sym(median), na.rm = T)) %>%
      dplyr::select(zonal_area, radius, median)

    fmap_stats

  } else {
    stop('error in aggregation parameter', call. = F)
  }
}
