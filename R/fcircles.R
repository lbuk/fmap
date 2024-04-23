#' Fresnel Circles
#'
#' Function for creating Fresnel circles polygons for mapping.
#'
#' @param ncircles Number of concentric circular zones of equal area including the inner_circle circle and annuli
#' @param radius_inner Radius of inner_circlemost Fresnel circle in metres
#' @param radius_outer Radius of outer_circlesmost Fresnel circle in metres
#' @param lat Latitude of the centre of the Fresnel circles
#' @param lon Longitude of the centre of the Fresnel circles
#' @param geo_centre A spatial dataset containing the coordinates of the centre of the Fresnel circles
#' @return An sf dataset of Fresnel circle polygons with a custom Azimuthal Equidistant (AEQD) CRS.
#' @examples
#' # Load the sf dataset of Soho pumps
#' data(soho_pumps)
#'
#' # Filter the Broad Street Pump from the Soho pumps dataset
#' bstreet_pump = soho_pumps %>% filter(soho.pump == "Broad Street")
#'
#' # Polygonal data from the Fresnel circles
#' fcircles(radius_inner = 125, ncircles = 8, geo_centre = bstreet_pump)
#' @export

fcircles = function(ncircles, radius_inner = NULL, radius_outer = NULL, lat = NULL, lon = NULL, geo_centre = NULL) {

  if(is.null(radius_inner) && is.null(radius_outer)) {
    stop('radius_inner or radius_outer not inputted', call. = F)

  } else if(is.null(radius_inner) != T && is.null(radius_outer) != T) {
    stop('radius_inner and radius_outer inputted', call. = F)

  } else if(radius_inner <= 0 && is.null(radius_outer) || radius_outer <= 0 && is.null(radius_inner)) {
    stop('radius should not be <= 0', call. = F)

  } else if(ncircles <= 1 && ncircles%%1 != 0) {
    stop('ncircles should not be <= 1 or a decimal number', call. = F)

  } else if(ncircles <= 1) {
    stop('ncircles should not be <= 1', call. = F)

  } else if(ncircles%%1 != 0) {
    stop('ncircles should not be a decimal number', call. = F)

  } else if(is.null(radius_inner) != T && is.null(radius_outer)) {
    area_fc = pi * (radius_inner ^ 2)

  } else {
    area_c = pi * (radius_outer ^ 2)
    area_fc = area_c / ncircles
  }

  radius = sqrt((area_fc * 1:ncircles) / pi)

  df_radii = data.frame(radius)

  if(is.null(lat) && is.null(lon) && is.null(geo_centre)) {
    stop('no centre coordinates inputted', call. = F)

  } else if(is.null(lat) && is.null(lon) && grepl(x = class(geo_centre)[1], pattern = "sf", ignore.case = T) != T && grepl(x = class(geo_centre)[1], pattern = "sp", ignore.case = T) != T) {
    stop('input geo_centre as a point-based spatial dataset', call. = F)

  } else if(is.null(lat) && is.null(lon) && nrow(geo_centre) > 1) {
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

  circles = lapply(1:nrow(df_radii), function(i) {
    coords %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
      st_transform(crs_aeqd) %>%
      st_buffer(df_radii[i, "radius"], nQuadSegs = 1375)
  })

  inner_circle = circles[[1]]

  outer_circles = lapply(2:length(circles), function(i)  {
    st_difference(circles[[i]], circles[[i-1]])
  })

  outer_circles = do.call(rbind, outer_circles)

  data = inner_circle %>%
    rbind(outer_circles) %>%
    mutate(zonal_area = 1:ncircles, radius = df_radii$radius) %>%
    dplyr::select(zonal_area, radius, geometry) %>%
    tibble() %>%
    st_as_sf()

  if(any(st_is_valid(data) == FALSE) == T) {
    data = data %>%
      st_make_valid(T)

    data

  } else {
    data
  }
}
