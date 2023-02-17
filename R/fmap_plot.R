#' Fresnel Map
#'
#' Function for plotting Fresnel Maps, or thematic maps that visualise geospatial data to the level of equal-area concentric circles (or annuli).
#'
#' @param ncircles Number of equal-area concentric circles
#' @param radius_inner Radius of innermost circle in metres
#' @param radius_outer Radius of outermost circle in metres
#' @param lat Latitude of the centre of the Fresnel Map
#' @param lon Longitude of the centre of the Fresnel Map
#' @param geo_centre A geospatial dataset containing the coordinates of the centre of the Fresnel Map
#' @param geo_points A geospatial dataset of points to aggregate
#' @param sum Variable from geo_points for calculating sum
#' @param mean Variable from geo_points for calculating mean
#' @param median Variable from geo_points for calculating median
#' @param count Count the number of points from geo_points. Input TRUE to count points. Defaults to FALSE
#' @return A Fresnel Map based on aggregations of points-based data and visualised using tmap.
#' @examples
#' # Load the sf dataset of Soho pumps
#' data(sohopumps)
#'
#' # Filter the Broad Street Pump
#' broadstpump =
#'   sohopumps %>%
#'   filter(Soho.Pump == "Broad Street")
#'
#' fmap_plot(radius_inner = 125, ncircles = 8, geo_centre = broadstpump, geo_points = choleradeaths, sum = "Cholera.Deaths")
#' @export

fmap_plot = function(ncircles, radius_inner = NULL, radius_outer = NULL, lat = NULL, lon = NULL, geo_centre = NULL, geo_points, sum = NULL, mean = NULL, median = NULL, count = F) {

  for(i in ncircles) {
    if(ncircles <= 1) {
      stop('input concentric circles using the ncircles parameter')

    } else if(is.null(radius_inner) && is.null(radius_outer)) {
      stop('radius_inner or radius_outer not inputted')

    } else if(is.null(radius_inner) != T && is.null(radius_outer) != T) {
      stop('radius_inner and radius_outer inputted')

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

    df_fmap_radii = data.frame(radius)
  }

  if(is.null(lat) && is.null(lon) && is.null(geo_centre)) {
    stop('no centre coordinates inputted')

  } else if(is.null(lat) && is.null(lon) && grepl(x = class(geo_centre)[1], pattern = "sf", ignore.case = T) != T && grepl(x = class(geo_centre)[1], pattern = "sp", ignore.case = T) != T) {
    stop('input geo_centre as a point-based geospatial dataset')

  } else if(is.null(lat) != T && is.null(lon) != T && is.null(geo_centre) != T || is.null(lat) && is.null(lon) != T && is.null(geo_centre) != T || is.null(lat) != T && is.null(lon) && is.null(geo_centre) != T) {
    stop('input geo_centre or lat and lon')

  } else if(is.null(lat) && is.null(lon) && is.null(geo_centre) != T) {
    geo_centre =
      geo_centre %>%
      st_as_sf() %>%
      st_transform(4326) %>%
      st_coordinates() %>%
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

  circles = list()
  for(i in 1:nrow(df_fmap_radii))  {
    circles[[i]] =
      coords %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
      st_transform(crs_aeqd) %>%
      st_buffer(df_fmap_radii[i, "radius"], nQuadSegs = 500) %>%
      mutate(circle = df_fmap_radii[i, "circle"])
  }

  fcircles =
    do.call(rbind, circles) %>%
    st_difference() %>%
    mutate(zonal_area = 1:ncircles) %>%
    mutate(radius = df_fmap_radii$radius) %>%
    arrange(zonal_area) %>%
    st_transform(crs_aeqd) %>%
    st_make_valid(T)

  if(grepl(x = class(geo_points)[1], pattern = "sf", ignore.case = T) != T && grepl(x = class(geo_points)[1], pattern = "sp", ignore.case = T) != T) {
    stop('input geo_points as a geospatial dataset of points')

  } else {
    geo_points =
      geo_points %>%
      st_as_sf() %>%
      st_transform(crs_aeqd)
  }

  if(is.null(sum) && is.null(mean) && is.null(median) && count != T) {
    stop('no aggregation inputted')

  } else if(is.null(mean) && is.null(sum) && is.null(median) && count == T) {
    fmap_count =
      fcircles %>%
      mutate(circle_count = lengths(st_intersects(., geo_points)))

    tm_shape(fmap_count, name = "Fresnel Map") +
      tm_fill(col = "circle_count", palette = "viridis",
              title = "Count", id = "", popup.vars = c("Zonal Area" = "zonal_area", "Radius" = "radius", "Count" = "circle_count")) +
      tm_borders(col = "black", lwd = 0.8) +
      tm_basemap(server = "OpenStreetMap") +
      tm_view(view.legend.position = c("right", "top")) +
      tm_layout(frame = F,
                legend.outside = F,
                legend.position = c("left", "top"),
                legend.title.size = 0.72,
                legend.text.size = 0.52,
                legend.text.fontfamily = "Helvetica",
                legend.title.fontface = "bold") +
      tmap_options(show.messages = F, show.warnings = F)

  } else if(is.null(sum) != T && is.null(mean) && is.null(median) && count == F) {
    fmap_sum =
      fcircles %>%
      st_join(geo_points) %>%
      group_by(zonal_area, radius) %>%
      summarise(sum = sum(!! sym(sum), na.rm = T))

    tm_shape(fmap_sum, name = "Fresnel Map") +
      tm_fill(col = "sum", palette = "viridis",
              title = paste0("Total ", '("', sum, '")'), id = "", popup.vars = c("Zonal Area" = "zonal_area", "Radius" = "radius", "Total" = "sum")) +
      tm_borders(col = "black", lwd = 0.8) +
      tm_basemap(server = "OpenStreetMap") +
      tm_view(view.legend.position = c("right", "top")) +
      tm_layout(frame = F,
                legend.outside = F,
                legend.position = c("left", "top"),
                legend.title.size = 0.72,
                legend.text.size = 0.52,
                legend.text.fontfamily = "Helvetica",
                legend.title.fontface = "bold") +
      tmap_options(show.messages = F, show.warnings = F)

  } else if(is.null(mean) != T && is.null(sum) && is.null(median) && count == F) {
    fmap_mean =
      fcircles %>%
      st_join(geo_points) %>%
      group_by(zonal_area, radius) %>%
      summarise(mean = mean(!! sym(mean), na.rm = T))

    tm_shape(fmap_mean, name = "Fresnel Map") +
      tm_fill(col = "mean", palette = "viridis",
              title = paste0("Mean ", '("', mean, '")'), id = "", popup.vars = c("Zonal Area" = "zonal_area", "Radius" = "radius", "Mean" = "mean")) +
      tm_borders(col = "black", lwd = 0.8) +
      tm_basemap(server = "OpenStreetMap") +
      tm_view(view.legend.position = c("right", "top")) +
      tm_layout(frame = F,
                legend.position = c("left", "top"),
                legend.outside = F,
                legend.title.size = 0.72,
                legend.text.size = 0.52,
                legend.text.fontfamily = "Helvetica",
                legend.title.fontface = "bold") +
      tmap_options(show.messages = F, show.warnings = F)

  } else if(is.null(median) != T && is.null(sum) && is.null(mean) && count == F) {
    fmap_median =
      fcircles %>%
      st_join(geo_points) %>%
      group_by(zonal_area, radius) %>%
      summarise(median = median(!! sym(median), na.rm = T))

    tm_shape(fmap_median, name = "Fresnel Map") +
      tm_fill(col = "median", palette = "viridis",
              title = paste0("Median ", '("', median, '")'), id = "", popup.vars = c("Zonal Area" = "zonal_area", "Radius" = "radius", "Median" = "median")) +
      tm_borders(col = "black", lwd = 0.8) +
      tm_basemap(server = "OpenStreetMap") +
      tm_view(view.legend.position = c("right", "top")) +
      tm_layout(frame = F,
                legend.position = c("left", "top"),
                legend.outside = F,
                legend.title.size = 0.72,
                legend.text.size = 0.52,
                legend.text.fontfamily = "Helvetica",
                legend.title.fontface = "bold") +
      tmap_options(show.messages = F, show.warnings = F)

  } else {
    stop('error in aggregation parameter')
  }
}
