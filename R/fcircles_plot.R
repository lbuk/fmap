#' Map of Fresnel Circles
#'
#' Function for plotting Fresnel circles with overlaying points. This map provides a basis for producing the Fresnel Map, which aggregates the points within the equal-area Fresnel circles
#'
#' @param ncircles Number of concentric circles of equal area
#' @param radius_inner Radius of innermost circle in metres
#' @param radius_outer Radius of outermost circle in metres
#' @param lat Latitude of the centre of the Fresnel circles
#' @param lon Longitude of the centre of the Fresnel circles
#' @param geo_centre A spatial dataset containing the coordinates of the centre of the Fresnel circles
#' @param geo_points A spatial dataset of points
#' @return A map of Fresnel circles with overlaying points and visualised using tmap.
#' @examples
#' # Load the sf datasets of cholera deaths and Soho pumps
#' data(cholera_deaths, soho_pumps)
#'
#' # Filter the Broad Street Pump from the Soho pumps dataset
#' bstreet_pump = soho_pumps %>% filter(soho.pump == "Broad Street")
#'
#' # Map of the Fresnel circles
#' fcircles_plot(radius_inner = 125, ncircles = 8, geo_centre = bstreet_pump, geo_points = cholera_deaths)
#' @export

fcircles_plot = function(ncircles, radius_inner = NULL, radius_outer = NULL, lat = NULL, lon = NULL, geo_centre = NULL, geo_points) {

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
    inner_fcircle_area = pi * (radius_inner ^ 2)
    radius = sqrt((inner_fcircle_area * 1:ncircles) / pi)

  } else {
    outer_fcircle_area = pi * (radius_outer ^ 2)
    area_fcircles = outer_fcircle_area / ncircles
    radius_inner = sqrt(area_fcircles / pi)
    inner_fcircle_area = pi * (radius_inner ^ 2)
    radius = sqrt((inner_fcircle_area * 1:ncircles) / pi)
  }

  df_fcircles_radii = data.frame(radius)

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

  circles = lapply(1:nrow(df_fcircles_radii), function(i) {
    coords %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
      st_transform(crs_aeqd) %>%
      st_buffer(df_fcircles_radii[i, "radius"], nQuadSegs = 1375) %>%
      mutate(circle = df_fcircles_radii[i, "circle"])
  })

  inner_fcircle = circles[[1]]

  outer_fcircles = lapply(2:length(circles), function(i)  {
    st_difference(circles[[i]], circles[[i-1]])
  })

  outer_fcircles = do.call(rbind, outer_fcircles)

  fcircles = inner_fcircle %>%
    rbind(outer_fcircles) %>%
    mutate(zonal_area = 1:ncircles, radius = df_fcircles_radii$radius, title = "Fresnel Circle") %>%
    arrange(zonal_area) %>%
    st_make_valid(T)

  if(grepl(x = class(geo_points)[1], pattern = "sf", ignore.case = T) != T && grepl(x = class(geo_points)[1], pattern = "sp", ignore.case = T) != T) {
    stop('input geo_points as a points-based spatial dataset', call. = F)

  } else {
    dataset = deparse(substitute(geo_points))

    geo_points = geo_points %>%
      st_as_sf() %>%
      st_transform(crs_aeqd) %>%
      mutate(dataset = dataset)
  }

  tm_shape(fcircles, name = "Fresnel Circles") +
    tm_add_legend('line', lwd = 1.225, col = "black", border.col = "white", title = "Fresnel Circles") +
    tm_fill(col = "white", alpha = 0.5, id = "", popup.vars = c("Zonal Area" = "zonal_area", "Radius (Metres)" = "radius")) +
    tm_borders(col = "black", lwd = 1.225) +
    tm_shape(geo_points, name = "geo_points") +
    tm_dots(col = "dataset", title = "geo_points", size = 0.15, palette = c("#3DE2F1"), alpha = 0.75, id = "", legend.show = TRUE, popup.vars = colnames(st_drop_geometry(geo_points[1:ncol(geo_points)-1]))) +
    tm_view(view.legend.position = c("left", "top")) +
    tm_basemap(server = c("OpenStreetMap", "Esri.WorldImagery")) +
    tm_layout(frame = F,
              legend.outside = F,
              legend.title.size = 0.85,
              legend.title.fontface = "bold",
              legend.position = c("left", "top")) +
    tmap_options(show.messages = F, show.warnings = F)
}
