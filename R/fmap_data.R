#' Polygonal Data from the Fresnel Map
#'
#' Function for creating Fresnel Map polygons by aggregating data to the level of equal-area concentric circles (or annuli).
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
#' @return An sf dataset of Fresnel Map polygons based on aggregations of points-based data with a custom Azimuthal Equidistant (AEQD) CRS.
#' @examples
#' # Load the sf datasets of cholera deaths and Soho pumps
#' data(cholera_deaths, soho_pumps)
#'
#' # Filter the Broad Street Pump from the Soho pumps dataset
#' bstreet_pump = soho_pumps %>% filter(soho.pump == "Broad Street")
#'
#' # Polygonal data from the Fresnel Map
#' fmap_data(radius_inner = 125, ncircles = 8, geo_centre = bstreet_pump, geo_points = cholera_deaths, sum = "cholera.deaths")
#' @export

fmap_data = function(ncircles, radius_inner = NULL, radius_outer = NULL, lat = NULL, lon = NULL, geo_centre = NULL, geo_points, sum = NULL, mean = NULL, median = NULL, count = F) {

  if(is.null(radius_inner) && is.null(radius_outer)) {
    stop('radius_inner or radius_outer not inputted', call. = F)

  } else if(is.null(radius_inner) != T && is.null(radius_outer) != T) {
    stop('radius_inner and radius_outer inputted', call. = F)

  } else if(ncircles <= 1 && ncircles%%1 != 0) {
    stop('ncircles should not be <= 1 or a decimal number', call. = F)

  } else if(ncircles <= 1) {
    stop('ncircles should not be <= 1', call. = F)

  } else if(ncircles%%1 != 0) {
    stop('ncircles should not be a decimal number', call. = F)

  } else if(is.null(radius_inner) != T && is.null(radius_outer)) {
    area_fcircles = pi * (radius_inner ^ 2)

  } else {
    area_outer = pi * (radius_outer ^ 2)
    area_fcircles = area_outer / ncircles
  }

  radius = sqrt((area_fcircles * 1:ncircles) / pi)

  fcircle_radii = data.frame(radius)

  if(is.null(lat) && is.null(lon) && is.null(geo_centre)) {
    stop('no centre coordinates inputted', call. = F)

  } else if(is.null(lat) && is.null(lon) && grepl(x = class(geo_centre)[1], pattern = "sf", ignore.case = T) != T && grepl(x = class(geo_centre)[1], pattern = "sp", ignore.case = T) != T) {
    stop('input geo_centre as a point-based spatial dataset', call. = F)

  } else if(nrow(geo_centre) > 1) {
    stop('geo_centre contains multiple points', call. = F)

  } else if(is.null(lat) != T && is.null(lon) != T && is.null(geo_centre) != T || is.null(lat) && is.null(lon) != T && is.null(geo_centre) != T || is.null(lat) != T && is.null(lon) && is.null(geo_centre) != T) {
    stop('input geo_centre or lat and lon', call. = F)

  } else if(is.null(lat) && is.null(lon) && is.null(geo_centre) != T) {
    geo_centre = geo_centre %>%
      st_as_sf() %>%
      st_transform(4326) %>%
      st_coordinates() %>%
      data.frame() %>%
      dplyr::rename(lat = Y, lon = X)

    lat = geo_centre$lat
    lon = geo_centre$lon

  } else {
    lat = lat
    lon = lon
  }

  coords = data.frame(lat, lon)

  crs_aeqd = sprintf("+proj=aeqd +lat_0=%s +lon_0=%s +x_0=0 +y_0=0", coords$lat, coords$lon)

  circles = lapply(1:nrow(fcircle_radii), function(i) {
    coords %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
      st_transform(crs_aeqd) %>%
      st_buffer(fcircle_radii[i, "radius"], nQuadSegs = 1375) %>%
      mutate(circle = fcircle_radii[i, "circle"])
  })

  inner_fcircle = circles[[1]]

  outer_fcircles = lapply(2:length(circles), function(i)  {
    st_difference(circles[[i]], circles[[i-1]])
  })

  outer_fcircles = do.call(rbind, outer_fcircles)

  df_fcircles = inner_fcircle %>%
    rbind(outer_fcircles) %>%
    mutate(zonal_area = 1:ncircles, radius = fcircle_radii$radius) %>%
    arrange(zonal_area) %>%
    st_make_valid(T)

  if(grepl(pattern = "sf", x = class(geo_points)[1], ignore.case = T) != T && grepl(pattern = "sp", x = class(geo_points)[1], ignore.case = T) != T) {
    stop('input geo_points as a points-based spatial dataset', call. = F)

  } else {
    geo_points = geo_points %>%
      st_as_sf() %>%
      st_transform(crs_aeqd)
  }

  if(is.null(sum) && is.null(mean) && is.null(median) && count != T) {
    stop('no aggregation inputted', call. = F)

  } else if(is.null(mean) && is.null(sum) && is.null(median) && count == T) {
    fm_data = df_fcircles %>%
      mutate(count = lengths(st_intersects(., geo_points))) %>%
      dplyr::select(zonal_area, radius, count, geometry) %>%
      tibble() %>%
      st_as_sf()

    fm_data

  } else if(is.null(sum) != T && is.null(mean) && is.null(median) && count == F) {
    fm_data = df_fcircles %>%
      st_join(geo_points) %>%
      group_by(zonal_area, radius) %>%
      dplyr::summarise(sum = sum(!! sym(sum), na.rm = T))

    fm_data

  } else if(is.null(mean) != T && is.null(sum) && is.null(median) && count == F) {
    fm_data = df_fcircles %>%
      st_join(geo_points) %>%
      group_by(zonal_area, radius) %>%
      dplyr::summarise(mean = mean(!! sym(mean), na.rm = T))

    fm_data

  } else if(is.null(median) != T && is.null(sum) && is.null(mean) && count == F) {
    fm_data = df_fcircles %>%
      st_join(geo_points) %>%
      group_by(zonal_area, radius) %>%
      dplyr::summarise(median = median(!! sym(median), na.rm = T))

    fm_data

  } else {
    stop('error in aggregation parameter', call. = F)
  }
}
