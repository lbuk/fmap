#' Multiple Fresnel Circles
#'
#' Function for plotting or deriving data from multiple Fresnel circles centred on different locations.
#'
#' @param ncircles Number of concentric circles of equal area
#' @param radius_inner Radius of innermost circle in metres
#' @param radius_outer Radius of outermost circle in metres
#' @param geo_centres A spatial dataset containing the coordinates of the centres of the Fresnel circles
#' @param id_var Variable from geo_centres containing the location ID
#' @param output Output of function. Input either 'plot' or 'data'. Defaults to 'data'
#' @return An sf dataset of polygons or a simple map of the multiple Fresnel circles.
#' @examples
#' # Load the sf dataset of Soho pumps
#' data(soho_pumps)
#'
#' # Polygonal data from the multiple Fresnel circles
#' fcircles_multi(radius_outer = 105, ncircles = 2, geo_centres = soho_pumps, id_var = "soho.pump", output = "data")
#'
#' # Multiple Fresnel circles
#' fcircles_multi(radius_outer = 105, ncircles = 2, geo_centres = soho_pumps, id_var = "soho.pump", output = "plot")
#' @export

fcircles_multi = function(ncircles, radius_inner = NULL, radius_outer = NULL, geo_centres, id_var = NULL, output = 'data') {

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

  if(is.null(id_var)) {
    geo_centres = geo_centres %>%
      st_as_sf() %>%
      mutate(id = row_number())

  } else {
    geo_centres = geo_centres %>%
      st_as_sf() %>%
      mutate(id = geo_centres[[id_var]])
  }

  if(grepl(x = class(geo_centres)[1], pattern = "sf", ignore.case = T) != T && grepl(x = class(geo_centres)[1], pattern = "sp", ignore.case = T) != T) {
    stop('input geo_centres as a geospatial dataset of points', call. = F)

  } else {
    crs = st_crs(geo_centres)

    geo_centres = geo_centres %>%
      st_transform(4326) %>%
      st_coordinates() %>%
      data.frame() %>%
      rename(lon = X, lat = Y) %>%
      mutate(id = geo_centres$id)
  }

  fcircles = lapply(1:nrow(geo_centres), function(i) {
    lat = geo_centres[i, "lat"]
    lon = geo_centres[i, "lon"]

    id = geo_centres[i, "id"]

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
      st_transform(crs) %>%
      mutate(zonal_area = 1:ncircles, radius = df_fcircles_radii$radius) %>%
      arrange(zonal_area) %>%
      st_make_valid(T)

    if(is.null(id_var) != T) {fcircles = fcircles %>% mutate(!!paste(id_var) := id, id = id)} else {fcircles = fcircles %>% mutate(id = id)}
  })

  fcircles_multi = do.call(rbind, fcircles)

  if(output == 'plot') {
    tm_shape(fcircles_multi, name = "Fresnel Circles") +
      tm_fill(col = "white", alpha = 0.5, id = "", popup.vars = c("Zonal Area" = "zonal_area", "Radius (Metres)" = "radius", colnames(fcircles_multi)[3])) +
      tm_borders(col = "black", lwd = 1.225) +
      tm_text("id", remove.overlap = TRUE, size = 0.6) +
      tm_add_legend('line', lwd = 1.225, col = "black", border.col = "white", title = "Fresnel Circles") +
      tm_basemap(server = c("OpenStreetMap", "Esri.WorldImagery")) +
      tm_layout(frame = F,
                frame.lwd = 0,
                legend.title.size = 0.85,
                legend.title.fontface = "bold",
                legend.outside = T,
                legend.outside.position = "left",
                legend.outside.size = 0.2) +
      tmap_options(show.messages = F, show.warnings = F)

  } else if(output == 'data') {
    data = fcircles_multi

    data

  } else {
    stop('error in output parameter', call. = F)
  }
}
