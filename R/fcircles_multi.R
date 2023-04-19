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
#' @return An sf dataset of polygons or a simple map of the multiple Fresnel circles centred on different locations.
#' @examples
#' # Load the sf dataset of Soho pumps
#' data(soho_pumps)
#'
#' # Polygonal data from the multiple Fresnel circles centred on different locations
#' fcircles_multi(radius_outer = 105, ncircles = 2, geo_centres = soho_pumps, id_var = "soho.pump", output = "data")
#'
#' # Map of multiple Fresnel circles centred on different locations
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
    area_fcircles = pi * (radius_inner ^ 2)
    radius = sqrt((area_fcircles * 1:ncircles) / pi)

  } else {
    area_outer = pi * (radius_outer ^ 2)
    area_fcircles = area_outer / ncircles
    radius = sqrt((area_fcircles * 1:ncircles) / pi)
  }

  fcircle_radii = data.frame(radius)

  if(is.null(id_var)) {
    geo_centres = geo_centres %>%
      st_as_sf() %>%
      dplyr::mutate(id = row_number())

  } else {
    geo_centres = geo_centres %>%
      st_as_sf() %>%
      mutate(id = geo_centres[[id_var]])
  }

  if(grepl(x = class(geo_centres)[1], pattern = "sf", ignore.case = T) != T && grepl(x = class(geo_centres)[1], pattern = "sp", ignore.case = T) != T) {
    stop('input geo_centres as a points-based spatial dataset', call. = F)

  } else if(nrow(geo_centres) == 1) {
    stop('geo_centres does not contain multiple points', call. = F)

  } else {
    crs = st_crs(geo_centres)

    geo_centres = geo_centres %>%
      st_transform(4326) %>%
      st_coordinates() %>%
      data.frame() %>%
      dplyr::rename(lon = X, lat = Y) %>%
      mutate(id = geo_centres$id)
  }

  df_fcm = lapply(1:nrow(geo_centres), function(i) {
    lat = geo_centres[i, "lat"]
    lon = geo_centres[i, "lon"]

    id = geo_centres[i, "id"]

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
      st_transform(crs) %>%
      mutate(zonal_area = 1:ncircles, radius = fcircle_radii$radius) %>%
      arrange(zonal_area) %>%
      st_make_valid(T)

    if(is.null(id_var) != T) {
      df_fcircles = df_fcircles %>%
        dplyr::mutate(!!paste(id_var) := id, id = id)

    } else {
      df_fcircles = df_fcircles %>%
        mutate(id = id)
    }
  })

  df_fcm = do.call(rbind, df_fcm)

  if(output == 'plot') {
    tm_shape(df_fcm, name = "Fresnel Circles") +
      tm_fill(col = "white", alpha = 0.5, id = "", popup.vars = c("Zonal Area" = "zonal_area", "Radius (Metres)" = "radius", colnames(df_fcm)[3])) +
      tm_borders(col = "black", lwd = 1.225) +
      tm_text("id", remove.overlap = T, size = 0.6) +
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
    fcm_data = df_fcm

    fcm_data

  } else {
    stop('error in output parameter', call. = F)
  }
}
