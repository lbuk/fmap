#' Fresnel Map
#'
#' Function for plotting Fresnel Maps, or thematic maps that aggregate and visualise data to the level of equal-area concentric circles or doughnuts.
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
#' @return A Fresnel Map, or a thematic map with equal-area Fresnel circles, based on aggregations of points-based data and mapped using tmap.
#' @examples
#' data(choleradeaths)
#' choleradeaths = choleradeaths %>% st_transform(27700)
#' fmap_plot(radius_inner = 125, ncircles = 8, lat = 51.51334, lon = -0.1366678, geo_points = choleradeaths, sum = "Cholera.Deaths")
#' @export

fmap_plot = function(ncircles, radius_inner = NA, radius_outer = NA, lat, lon, geo_points, sum = NA, mean = NA, median = NA) {

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
      mutate(circle_count = lengths(st_intersects(., geo_points)))

    tm_shape(fmap_count, name = "Fresnel Map") +
      tm_fill(col = "circle_count", palette = "viridis", title = "Count", id = "", popup.vars = c("Circle"="circle", "Radius"="radius", "Count"="circle_count")) +
      tm_borders(col = "black", lwd = 0.8) +
      tm_basemap(server = "OpenStreetMap") +
      tm_view(view.legend.position = c("right", "top")) +
      tm_layout(frame = FALSE,
                legend.outside = F,
                legend.position = c("left","top"),
                legend.title.size = 0.72,
                legend.text.size = 0.52,
                legend.text.fontfamily = "Helvetica",
                legend.title.fontface = "bold") +
      tmap_options(show.messages = F, show.warnings = F)

  } else if (is.na(sum) != TRUE && is.na(mean) && is.na(median)) {

    fmap_sum =
      circles %>%
      st_join(geo_points) %>%
      group_by(circle, radius) %>%
      summarise(sum = sum(!! sym(sum), na.rm = T))

    tm_shape(fmap_sum, name = "Fresnel Map") +
      tm_fill(col = "sum", palette = "viridis", title = paste0("Total ", '("', sum, '")'), id = "", popup.vars = c("Circle"="circle", "Radius"="radius", "Total"="sum")) +
      tm_borders(col = "black", lwd = 0.8) +
      tm_basemap(server = "OpenStreetMap") +
      tm_view(view.legend.position = c("right", "top")) +
      tm_layout(frame = FALSE,
                legend.outside = F,
                legend.position = c("left","top"),
                legend.title.size = 0.72,
                legend.text.size = 0.52,
                legend.text.fontfamily = "Helvetica",
                legend.title.fontface = "bold") +
      tmap_options(show.messages = F, show.warnings = F)

  } else if (is.na(mean) != TRUE && is.na(sum) && is.na(median)) {

    fmap_mean =
      circles %>%
      st_join(geo_points) %>%
      group_by(circle, radius) %>%
      summarise(mean = mean(!! sym(mean), na.rm = T))

    tm_shape(fmap_mean, name = "Fresnel Map") +
      tm_fill(col = "mean", palette = "viridis", title = paste0("Mean ", '("', mean, '")'), id = "", popup.vars = c("Circle"="circle", "Radius"="radius", "Mean"="mean")) +
      tm_borders(col = "black", lwd = 0.8) +
      tm_basemap(server = "OpenStreetMap") +
      tm_view(view.legend.position = c("right", "top")) +
      tm_layout(frame = FALSE,
                legend.position = c("left","top"),
                legend.outside = F,
                legend.title.size = 0.72,
                legend.text.size = 0.52,
                legend.text.fontfamily = "Helvetica",
                legend.title.fontface = "bold") +
      tmap_options(show.messages = F, show.warnings = F)

  } else if (is.na(median) != TRUE && is.na(sum) && is.na(mean)) {

    fmap_median =
      circles %>%
      st_join(geo_points) %>%
      group_by(circle, radius) %>%
      summarise(median = median(!! sym(median), na.rm = T))

    tm_shape(fmap_median, name = "Fresnel Map") +
      tm_fill(col = "median", palette = "viridis", title = paste0("Median ", '("', median, '")'), id = "", popup.vars = c("Circle"="circle", "Radius"="radius", "Median"="median")) +
      tm_borders(col = "black", lwd = 0.8) +
      tm_basemap(server = "OpenStreetMap") +
      tm_view(view.legend.position = c("right", "top")) +
      tm_layout(frame = FALSE,
                legend.position = c("left","top"),
                legend.outside = F,
                legend.title.size = 0.72,
                legend.text.size = 0.52,
                legend.text.fontfamily = "Helvetica",
                legend.title.fontface = "bold") +
      tmap_options(show.messages = F, show.warnings = F)
  }
}
