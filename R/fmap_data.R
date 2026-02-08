#' Polygonal Data from the Fresnel Map
#'
#' Function for creating Fresnel Map polygons by aggregating data to the level of equal-area concentric circular zones (or annuli).
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
#' @importFrom rlang sym
#' @return An sf dataset of Fresnel Map polygons based on aggregations of points-based data with a custom Azimuthal Equidistant (AEQD) CRS.
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
#' # Polygonal data from the Fresnel Map for the Broad Street Pump
#' fmap_data(radius_inner = 125, ncircles = 8, geo_centre = bstreet_pump, geo_points = cholera_deaths, sum = "cholera.deaths")
#'
#' # Polygonal data from multiple Fresnel Maps based on each Soho pump
#' fmap_data(radius_outer = 150, ncircles = 2, geo_points = cholera_deaths, geo_centres = soho_pumps, id_var = "soho.pump", sum = "cholera.deaths")
#' @export

fmap_data <- function(ncircles, 
                      radius_inner = NULL, radius_outer = NULL, 
                      geo_points, 
                      geo_centre = NULL, lat = NULL, lon = NULL, 
                      geo_centres = NULL, id_var = NULL, 
                      sum = NULL, mean = NULL, median = NULL, count = FALSE) {
  
  centres <- validate_centres(geo_centre = geo_centre, lat = lat, lon = lon, 
                              geo_centres = geo_centres, id_var = id_var)

  validate_geo(geo_points, "geo_points")
  
  validate_aggregation(sum, mean, median, count)
  
  geo_points <- geo_points %>% 
    sf::st_as_sf()
  
  if(centres$single) {
    df <- fcircles(ncircles = ncircles, 
                   radius_inner = radius_inner, radius_outer = radius_outer, 
                   geo_centre = geo_centre, lat = lat, lon = lon)
    
    crs_aeqd <- sf::st_crs(df)
    
    geo_points <- geo_points %>% 
      sf::st_transform(crs_aeqd)
    
    if(count) {
      data <- df %>%
        dplyr::mutate(count = lengths(sf::st_intersects(., geo_points))) %>%
        dplyr::select(zonal_area, radius, count, geometry) %>%
        tibble::tibble() %>%
        sf::st_as_sf()
      
    } else if(!is.null(sum)) {
      data <- df %>%
        sf::st_join(geo_points) %>%
        dplyr::group_by(zonal_area, radius) %>%
        dplyr::summarise(sum = sum(!!sym(sum), na.rm = TRUE), .groups = 'drop')
      
    } else if(!is.null(mean)) {
      data <- df %>%
        sf::st_join(geo_points) %>%
        dplyr::group_by(zonal_area, radius) %>%
        dplyr::summarise(mean = mean(!!sym(mean), na.rm = TRUE), .groups = 'drop')
      
    } else if(!is.null(median)) {
      data <- df %>%
        sf::st_join(geo_points) %>%
        dplyr::group_by(zonal_area, radius) %>%
        dplyr::summarise(median = median(!!sym(median), na.rm = TRUE), .groups = 'drop')
    }
    
    return(data)
  }
  
  if(centres$multiple) {
    df_radii <- fcircles_radii(ncircles = ncircles, 
                               radius_inner = radius_inner, radius_outer = radius_outer)
    
    crs <- sf::st_crs(geo_points)
    
    if(is.null(id_var)) {
      geo_centres <- geo_centres %>%
        sf::st_as_sf() %>%
        dplyr::mutate(id = dplyr::row_number())

    } else {
      geo_centres <- geo_centres %>%
        sf::st_as_sf() %>%
        dplyr::mutate(id = geo_centres[[id_var]])
    }
    
    geo_centres <- geo_centres %>%
      sf::st_transform(crs) %>%
      sf::st_transform(4326) %>%
      sf::st_coordinates() %>%
      data.frame() %>%
      dplyr::rename(lon = X, lat = Y) %>%
      dplyr::mutate(id = geo_centres$id)
    
    df <- Map(function(lat, lon, id) {
      coords <- data.frame(lat, lon)
      
      crs_aeqd <- sprintf("+proj=aeqd +lat_0=%s +lon_0=%s +x_0=0 +y_0=0", lat, lon)
      
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
      
      fcircles_data <- inner_circle %>%
        rbind(outer_fcircles) %>%
        dplyr::mutate(zonal_area = 1:ncircles, radius = df_radii$radius) %>%
        dplyr::arrange(zonal_area) %>%
        sf::st_make_valid(TRUE)
      
      geo_points <- geo_points %>%
        sf::st_transform(crs_aeqd)
      
      if(count) {
        result <- fcircles_data %>%
          dplyr::mutate(count = lengths(sf::st_intersects(., geo_points)), id = id) %>%
          sf::st_transform(crs)
        
      } else if(!is.null(sum)) {
        result <- fcircles_data %>%
          sf::st_join(geo_points) %>%
          dplyr::group_by(zonal_area, radius) %>%
          dplyr::summarise(sum_calc = sum(!!sym(sum), na.rm = TRUE), .groups = 'drop') %>%
          sf::st_transform(crs) %>%
          dplyr::mutate(id = id) %>%
          dplyr::rename(sum = sum_calc)
        
      } else if(!is.null(mean)) {
        result <- fcircles_data %>%
          sf::st_join(geo_points) %>%
          dplyr::group_by(zonal_area, radius) %>%
          dplyr::summarise(mean_calc = mean(!!sym(mean), na.rm = TRUE), .groups = 'drop') %>%
          sf::st_transform(crs) %>%
          dplyr::mutate(id = id) %>%
          dplyr::rename(mean = mean_calc)
        
      } else if(!is.null(median)) {
        result <- fcircles_data %>%
          sf::st_join(geo_points) %>%
          dplyr::group_by(zonal_area, radius) %>%
          dplyr::summarise(median_calc = median(!!sym(median), na.rm = TRUE), .groups = 'drop') %>%
          sf::st_transform(crs) %>%
          dplyr::mutate(id = id) %>%
          dplyr::rename(median = median_calc)
      }
      
      if(!is.null(id_var)) {
        result <- result %>%
          dplyr::mutate(!!paste(id_var) := id)
      }
      
      result
    }, geo_centres$lat, geo_centres$lon, geo_centres$id)
    
    df <- do.call(rbind, df)
    
    data <- df %>%
      dplyr::relocate(1, 2, 3, geometry, dplyr::everything()) %>%
      tibble::tibble() %>%
      sf::st_as_sf() %>%
      dplyr::select(1, 2, 3, id, dplyr::last_col(), geometry)
    
    return(data)
  }
  
  stop('error in parameter handling', call. = FALSE)
}