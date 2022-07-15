#' Fresnel Circles
#'
#' Function for creating Fresnel circles for mapping.
#'
#' @param ncircles Number of equal-area concentric circles
#' @param radius_inner Radius of innermost circle in metres
#' @param radius_outer Radius of outermost circle in metres
#' @param lat Latitude of the centre of the Fresnel circles
#' @param lon Longitude of the centre of the Fresnel circles
#' @return An sf dataset of Fresnel circles with a custom Azimuthal Equidistant (AEQD) CRS.
#' @examples
#' fcircles(radius_inner = 125, ncircles = 8, lat = 51.51334, lon = -0.1366678)
#' @export

fcircles = function(ncircles, radius_inner = NA, radius_outer = NA, lat, lon) {

  for (i in ncircles) {

    if(is.na(radius_inner) != TRUE && is.na(radius_outer) != TRUE) {
      warning('Error: radius_inner and radius_outer inputted')

    } else if(is.na(radius_inner) != TRUE && is.na(radius_outer)) {
      inner_circle_area = pi * (radius_inner ^ 2)
      radius = sqrt((inner_circle_area * 1:ncircles) / pi)

    } else if(is.na(radius_inner) && is.na(radius_outer) != TRUE) {
      outer_circle_area = pi * (radius_outer ^ 2)
      area_circles = outer_circle_area / ncircles
      radius_inner = sqrt(area_circles / pi)
      inner_circle_area = pi * (radius_inner ^ 2)
      radius = sqrt((inner_circle_area * 1:ncircles) / pi)
    }

    df_fmap_radii = data.frame(radius)
  }

  coords = data.frame(lat, lon)

  crs_aeqd = sprintf("+proj=aeqd +lat_0=%s +lon_0=%s +x_0=0 +y_0=0", coords$lat, coords$lon)

  circles = list()
  for(i in 1:nrow(df_fmap_radii))  {
    circles[[i]] =
      coords %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
      st_transform(crs_aeqd) %>%
      st_buffer(df_fmap_radii[i, "radius"], nQuadSegs = 2500) %>%
      mutate(circle = df_fmap_radii[i, "circle"])
  }

  inner_circle = circles[[1]]
  outer_circles = list()
  for(i in 2:length(circles))  {
    outer_circles[[i]] = st_difference(circles[[i]], circles[[i-1]])
  }
  outer_circles = do.call(rbind, outer_circles)

  fcircles =
    inner_circle %>%
    rbind(outer_circles) %>%
    st_as_sf() %>%
    mutate(circle = 1:ncircles) %>%
    mutate(radius = df_fmap_radii$radius) %>%
    arrange(circle) %>%
    mutate(poly_shape = case_when(circle == 1 ~ "circle",
                                  circle > 1 ~ "doughnut")) %>%
    st_transform(crs_aeqd) %>%
    st_make_valid(TRUE)

  fcircles
}
