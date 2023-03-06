#' Fresnel Circles
#'
#' Function for creating Fresnel circles polygons for mapping.
#'
#' @param ncircles Number of equal-area concentric circles
#' @param radius_inner Radius of innermost circle in metres
#' @param radius_outer Radius of outermost circle in metres
#' @param lat Latitude of the centre of the Fresnel circles
#' @param lon Longitude of the centre of the Fresnel circles
#' @param geo_centre A spatial dataset containing the coordinates of the centre of the Fresnel circles
#' @return An sf dataset of Fresnel circle polygons with a custom Azimuthal Equidistant (AEQD) CRS.
#' @examples
#' # Load the sf dataset of Soho pumps
#' data(sohopumps)
#'
#' # Filter the Broad Street Pump
#' broadstreetpump =
#'   sohopumps %>%
#'   filter(Soho.Pump == "Broad Street")
#'
#' fcircles(radius_inner = 125, ncircles = 8, geo_centre = broadstreetpump)
#' @export

fcircles = function(ncircles, radius_inner = NULL, radius_outer = NULL, lat = NULL, lon = NULL, geo_centre = NULL) {

  df_fmap_radii =
    lapply(ncircles, function(i) {
      if(is.null(radius_inner) && is.null(radius_outer)) {
        stop('radius_inner or radius_outer not inputted')

      } else if(is.null(radius_inner) != T && is.null(radius_outer) != T) {
        stop('radius_inner and radius_outer inputted')

      } else if(ncircles%%1 != 0 | ncircles <= 1) {
        stop('ncircles should not be <= 1 or a decimal number')

      } else if(is.null(radius_inner) != T && is.null(radius_outer)) {
        inner_circle_area = pi * (radius_inner ^ 2)
        radius = sqrt((inner_circle_area * 1:ncircles) / pi)

      } else {
        outer_circle_area = pi * (radius_outer ^ 2)
        area_circles = outer_circle_area / ncircles
        radius_inner = sqrt(area_circles / pi)
        inner_circle_area = pi * (radius_inner ^ 2)
        radius = sqrt((inner_circle_area * 1:ncircles) / pi)
      }

      data.frame(radius)
    })

  df_fmap_radii = data.frame(df_fmap_radii)

  if(is.null(lat) && is.null(lon) && is.null(geo_centre)) {
    stop('no centre coordinates inputted')

  } else if(is.null(lat) && is.null(lon) && grepl(x = class(geo_centre)[1], pattern = "sf", ignore.case = T) != T && grepl(x = class(geo_centre)[1], pattern = "sp", ignore.case = T) != T) {
    stop('input geo_centre as a point-based geospatial dataset')

  } else if(is.null(lat) != T && is.null(lon) != T && is.null(geo_centre) != T || is.null(lat) && is.null(lon) != T && is.null(geo_centre) != T || is.null(lat) != T && is.null(lon) && is.null(geo_centre) != T) {
    stop('input geo_centre or lat and lon')

  } else if(is.null(lat) && is.null(lon) && is.null(geo_centre) != T) {
    geo_centre =
      geo_centre %>%
      sf::st_as_sf() %>%
      sf::st_transform(4326) %>%
      sf::st_coordinates() %>%
      data.frame() %>%
      rename(lat = Y, lon = X)

    lat = geo_centre$lat
    lon = geo_centre$lon

  } else {
    lat = lat
    lon = lon
  }

  coords = data.frame(lat, lon)

  crs_aeqd = sprintf("+proj=aeqd +lat_0=%s +lon_0=%s +x_0=0 +y_0=0", coords$lat, coords$lon)

  circles =
    lapply(1:nrow(df_fmap_radii), function(i) {
      coords %>%
        sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
        sf::st_transform(crs_aeqd) %>%
        st_buffer(df_fmap_radii[i, "radius"], nQuadSegs = 2175) %>%
        mutate(circle = df_fmap_radii[i, "circle"])
    })

  outer_circles =
    lapply(2:length(circles), function(i)  {
      st_difference(circles[[i]], circles[[i-1]])
    })
  outer_circles = do.call(rbind, outer_circles)
  inner_circle = circles[[1]]

  fcircles =
    inner_circle %>%
    rbind(outer_circles) %>%
    mutate(zonal_area = 1:ncircles, radius = df_fmap_radii$radius) %>%
    arrange(zonal_area) %>%
    st_make_valid(T)

  fcircles
}
