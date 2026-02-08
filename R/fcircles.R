#' Fresnel Circles
#'
#' Function for creating Fresnel circles polygons for mapping. Fresnel circles consist of a central circle as well as concentric annuli, all of which are equal in area.
#'
#' @param ncircles Number of concentric circular zones of equal area (i.e. Fresnel circles) including the inner circle and annuli
#' @param radius_inner Radius of innermost Fresnel circle in metres
#' @param radius_outer Radius of outermost Fresnel circle in metres
#' @param lat Latitude of the centre of the Fresnel circles
#' @param lon Longitude of the centre of the Fresnel circles
#' @param geo_centre A spatial dataset containing the coordinates of the centre of the Fresnel circles
#' @importFrom dplyr "%>%"
#' @return An sf dataset of Fresnel circle polygons with a custom Azimuthal Equidistant (AEQD) CRS.
#' @examples
#' library(sf)
#' library(dplyr)
#'
#' # Load the sf dataset of Soho pumps
#' data(soho_pumps)
#'
#' # Filter the Broad Street Pump from the Soho pumps dataset
#' bstreet_pump <- soho_pumps %>% filter(soho.pump == "Broad Street")
#'
#' # Polygonal data from the Fresnel circles
#' fcircles(radius_inner = 125, ncircles = 8, geo_centre = bstreet_pump)
#' @export

fcircles <- function(ncircles, 
                     radius_inner = NULL, radius_outer = NULL, 
                     geo_centre = NULL, lat = NULL, lon = NULL) {

  df_radii <- fcircles_radii(ncircles = ncircles, 
                             radius_inner = radius_inner, radius_outer = radius_outer)
  
  df_radii <- df_radii %>%
    dplyr::select(-zone)
  
  latlon <- !is.null(lat) && !is.null(lon)
  
  one_centre <- !is.null(geo_centre)
  
  if(!latlon && !one_centre) {
    stop('no centre coordinates provided', call. = FALSE)
  }
  
  if(latlon && one_centre) {
    stop('input either geo_centre or lat and lon', call. = FALSE)
  }
  
  if(!is.null(lat) && is.null(lon) || is.null(lat) && !is.null(lon)) {
    stop('input lat and lon', call. = FALSE)
  }
    
  if(one_centre) {
    validate_geo(geo_centre, "geo_centre")
    
    if(nrow(geo_centre) > 1) {
      stop('geo_centre should contain only one point', call. = FALSE)
    }
    
    geo_centre <- geo_centre %>%
      sf::st_as_sf() %>%
      sf::st_transform(4326) %>%
      sf::st_coordinates() %>%
      data.frame() %>%
      dplyr::rename(lat = Y, lon = X)
    
    lat <- geo_centre$lat
    lon <- geo_centre$lon
  }
  
  coords <- data.frame(lat, lon)
  
  crs_aeqd <- sprintf("+proj=aeqd +lat_0=%s +lon_0=%s +x_0=0 +y_0=0", coords$lat, coords$lon)
  
  coords <- coords %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    sf::st_transform(crs_aeqd)
  
  circles <- Map(function(r) {
    coords %>%
      sf::st_buffer(r, nQuadSegs = 1375)
      
  }, df_radii$radius)
  
  inner_circle <- circles[[1]]
  
  outer_fcircles <- Map(function(outer, inner) {
    sf::st_difference(outer, inner)
  }, circles[-1], circles[-length(circles)])
  
  outer_fcircles <- do.call(rbind, outer_fcircles)
  
  data <- inner_circle %>%
    rbind(outer_fcircles) %>%
    dplyr::mutate(zonal_area = 1:ncircles, radius = df_radii$radius) %>%
    dplyr::select(zonal_area, radius, geometry) %>%
    tibble::tibble() %>%
    sf::st_as_sf()
  
  if(any(!sf::st_is_valid(data))) {
    data <- data %>% 
      sf::st_make_valid(TRUE)
  }
  
  data
}