#' Summary Stats from the Fresnel Map
#'
#' Function for calculating summary statistics from Fresnel Maps, or thematic maps that aggregate and visualise spatial data to the level of equal-area concentric circles or doughnuts.
#'
#' @param ncircles Number of equal-area concentric circles
#' @param radius_inner Radius of innermost circle in metres
#' @param radius_outer Radius of outermost circle in metres
#' @param lat Latitude of the centre of the Fresnel Map
#' @param lon Longitude of the centre of the Fresnel Map
#' @param geo_points A spatial dataset of points to aggregate
#' @param sum Variable from geo_points for calculating sum
#' @param mean Variable from geo_points for calculating mean
#' @param median Variable from geo_points for calculating median
#' @return Summary statistics from the Fresnel Maps including aggregated data from the Fresnel circles.
#' @examples
#' data(choleradeaths)
#' choleradeaths = choleradeaths %>% st_transform(27700)
#' fmap_stats(radius_inner = 125, ncircles = 8, lat = 51.51334, lon = -0.1366678, geo_points = choleradeaths, sum = "Cholera.Deaths")
#' @export

fmap_stats = function(ncircles, radius_inner = NA, radius_outer = NA, lat, lon, geo_points, sum = NA, mean = NA, median = NA) {

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

  circles =
    inner_circle %>%
    rbind(outer_circles) %>%
    st_as_sf() %>%
    mutate(circle = 1:ncircles) %>%
    mutate(radius = df_fmap_radii$radius) %>%
    arrange(circle) %>%
    st_transform(crs_aeqd) %>%
    st_make_valid(TRUE)

  geo_points =
    geo_points %>%
    st_as_sf() %>%
    st_transform(crs_aeqd)

  if(is.na(sum) == FALSE && is.na(mean) == FALSE && is.na(median) == FALSE | is.na(sum) == FALSE && is.na(mean) == FALSE | is.na(mean) == FALSE && is.na(median) == FALSE | is.na(sum) == FALSE && is.na(median) == FALSE) {
    warning('Error: multiple aggregations inputted')
  }

  if(is.na(mean) && is.na(sum) && is.na(median)) {

    fmap_count =
      circles %>%
      mutate(circle_count = lengths(st_intersects(., geo_points))) %>%
      as.data.frame() %>%
      dplyr::select(circle, radius, circle_count) %>%
      rename(count = circle_count) %>%
      print()

  } else if (is.na(sum) != TRUE && is.na(mean) && is.na(median)) {

    fmap_sum =
      circles %>%
      st_join(geo_points) %>%
      data.frame() %>%
      group_by(circle) %>%
      summarise(total = sum(!! sym(sum), na.rm = T)) %>%
      mutate(radius = df_fmap_radii$radius) %>%
      dplyr::select(circle, radius, total) %>%
      as.data.frame() %>%
      print()

  } else if (is.na(mean) != TRUE && is.na(sum) && is.na(median)) {

    fmap_mean =
      circles %>%
      st_join(geo_points) %>%
      data.frame() %>%
      group_by(circle) %>%
      summarise(mean = mean(!! sym(mean), na.rm = T)) %>%
      mutate(radius = df_fmap_radii$radius) %>%
      dplyr::select(circle, radius, mean) %>%
      as.data.frame() %>%
      print()

  } else if (is.na(median) != TRUE && is.na(sum) && is.na(mean)) {

    fmap_median =
      circles %>%
      st_join(geo_points) %>%
      data.frame() %>%
      group_by(circle) %>%
      summarise(median = median(!! sym(median), na.rm = T)) %>%
      mutate(radius = df_fmap_radii$radius) %>%
      dplyr::select(circle, radius, median) %>%
      as.data.frame() %>%
      print()

  }
}
