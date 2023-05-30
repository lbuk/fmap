#' Multiple Fresnel Maps
#'
#' Function for plotting or deriving data or stats from multiple Fresnel Maps, or thematic maps that visualise spatial data to the level of equal-area concentric circles (or annuli), centred on different locations.
#'
#' @param ncircles Number of concentric circles of equal area including the inner circle and annuli
#' @param radius_inner Radius of innermost circle in metres
#' @param radius_outer Radius of outermost circle in metres
#' @param geo_points A spatial dataset of points to aggregate
#' @param geo_centres A spatial dataset containing the coordinates of the centres of each Fresnel Map
#' @param id_var Variable from geo_centres containing the location ID
#' @param sum Variable from geo_points for calculating sum
#' @param mean Variable from geo_points for calculating mean
#' @param median Variable from geo_points for calculating median
#' @param count Count the number of points from geo_points. Input TRUE to count points. Defaults to FALSE
#' @param output Output of function. Input either 'plot', 'data' or 'stats'. Defaults to 'plot'
#' @return A map, sf dataset or summary stats based on multiple Fresnel Maps.
#' @examples
#' # Load the sf datasets of cholera deaths and Soho pumps
#' data(cholera_deaths, soho_pumps)
#'
#' # Map of multiple Fresnel Maps
#' fmap_multi(radius_outer = 150, ncircles = 2, geo_points = cholera_deaths, geo_centres = soho_pumps, id_var = "soho.pump", sum = "cholera.deaths", output = "plot")
#'
#' # Summary stats from multiple Fresnel Maps
#' fmap_multi(radius_outer = 150, ncircles = 2, geo_points = cholera_deaths, geo_centres = soho_pumps, id_var = "soho.pump", sum = "cholera.deaths", output = "stats")
#'
#' # Polygonal data from multiple Fresnel Maps
#' fmap_multi(radius_outer = 150, ncircles = 2, geo_points = cholera_deaths, geo_centres = soho_pumps, id_var = "soho.pump", sum = "cholera.deaths", output = "data")
#' @export

fmap_multi = function(ncircles, radius_inner = NULL, radius_outer = NULL, geo_points, geo_centres, id_var = NULL, sum = NULL, mean = NULL, median = NULL, count = F, output = 'plot') {

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

  radii_fc = data.frame(radius)

  if(grepl(x = class(geo_points)[1], pattern = "sf", ignore.case = T) != T && grepl(x = class(geo_points)[1], pattern = "sp", ignore.case = T) != T) {
    stop('input geo_points as a points-based spatial dataset', call. = F)

  } else {
    geo_points = geo_points %>% st_as_sf()

    crs = st_crs(geo_points)
  }

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
    geo_centres = geo_centres %>%
      st_as_sf() %>%
      st_transform(crs) %>%
      st_transform(4326) %>%
      st_coordinates() %>%
      data.frame() %>%
      dplyr::rename(lon = X, lat = Y) %>%
      mutate(id = geo_centres$id)
  }

  fmap_multi = lapply(1:nrow(geo_centres), function(i) {
    lat = geo_centres[i, "lat"]
    lon = geo_centres[i, "lon"]

    id = geo_centres[i, "id"]

    coords = data.frame(lat, lon)

    crs_aeqd = sprintf("+proj=aeqd +lat_0=%s +lon_0=%s +x_0=0 +y_0=0", coords$lat, coords$lon)

    cs = lapply(1:nrow(radii_fc), function(i) {
      coords %>%
        st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
        st_transform(crs_aeqd) %>%
        st_buffer(radii_fc[i, "radius"], nQuadSegs = 1375) %>%
        mutate(circle = radii_fc[i, "circle"])
    })

    i_fc = cs[[1]]

    o_fc = lapply(2:length(cs), function(i)  {
      st_difference(cs[[i]], cs[[i-1]])
    })

    o_fc = do.call(rbind, o_fc)

    df_fc = i_fc %>%
      rbind(o_fc) %>%
      mutate(zonal_area = 1:ncircles, radius = radii_fc$radius) %>%
      arrange(zonal_area) %>%
      st_make_valid(T)

    geo_points = geo_points %>%
      st_transform(crs_aeqd)

    if(is.null(sum) && is.null(mean) && is.null(median) && count != T) {
      stop('no aggregation inputted', call. = F)

    } else if(is.null(mean) && is.null(sum) && is.null(median) && count == T) {
      fmap_multi = df_fc %>%
        mutate(count = lengths(st_intersects(., geo_points)), id = id) %>%
        st_transform(crs)

    } else if(is.null(sum) != T && is.null(mean) && is.null(median) && count == F) {
      fmap_multi = df_fc %>%
        st_join(geo_points) %>%
        group_by(zonal_area, radius) %>%
        dplyr::summarise(sum_calc = sum(!! sym(sum), na.rm = T)) %>%
        st_transform(crs) %>%
        mutate(id = id) %>%
        dplyr::rename(sum = sum_calc)

    } else if(is.null(mean) != T && is.null(sum) && is.null(median) && count == F) {
      fmap_multi = df_fc %>%
        st_join(geo_points) %>%
        group_by(zonal_area, radius) %>%
        dplyr::summarise(mean_calc = mean(!! sym(mean), na.rm = T)) %>%
        st_transform(crs) %>%
        mutate(id = id) %>%
        dplyr::rename(mean = mean_calc)

    } else if(is.null(median) != T && is.null(sum) && is.null(mean) && count == F) {
      fmap_multi = df_fc %>%
        st_join(geo_points) %>%
        group_by(zonal_area, radius) %>%
        dplyr::summarise(median_calc = median(!! sym(median), na.rm = T)) %>%
        st_transform(crs) %>%
        mutate(id = id) %>%
        dplyr::rename(median = median_calc)

    } else {
      stop('error in aggregation parameter', call. = F)
    }

    if(is.null(id_var) != T) {
      fmap_multi = fmap_multi %>%
        dplyr::mutate(!!paste(id_var) := id)

    } else {
      fmap_multi = fmap_multi
    }
  })

  fmap_multi = do.call(rbind, fmap_multi)

  if(count == T) {
    legend_title = "Count"

  } else if(is.null(sum) != T) {
    legend_title = paste0("Total ", '("', sum, '")')

  } else if(is.null(mean) != T) {
    legend_title = paste0("Mean ", '("', mean, '")')

  } else {
    legend_title = paste0("Median ", '("', median, '")')
  }

  if(output == 'plot') {
    tm_shape(fmap_multi, name = "Fresnel Map") +
      tm_fill(col = colnames(fmap_multi)[3], palette = "plasma", title = legend_title, id = "", popup.vars = c("Zonal Area" = "zonal_area", "Radius (Metres)" = "radius", colnames(fmap_multi)[3], fmap_multi %>% ungroup() %>% st_drop_geometry() %>% dplyr::select(last_col()) %>% colnames())) +
      tm_borders(col = "black", lwd = 0.8) +
      tm_facets(by='id', ncol = 2, free.scales = F) +
      tm_basemap(server = c("OpenStreetMap", "Esri.WorldImagery")) +
      tm_view(view.legend.position = c("right", "top")) +
      tm_layout(frame = F,
                frame.lwd = 0,
                legend.outside = F,
                legend.text.fontfamily = "Helvetica",
                legend.text.size = 0.62,
                legend.title.size = 0.8,
                legend.title.fontface = "bold",
                legend.position = c("left", "top"),
                panel.label.fontfamily = "Helvetica",
                panel.label.fontface = "bold",
                panel.label.size = 1.1,
                panel.label.bg.color = NA) +
      tmap_options(show.messages = F, show.warnings = F)

  } else if(output == 'data') {
    fmap_multi_data = fmap_multi %>%
      relocate(1, 2, 3, geometry, everything()) %>%
      dplyr::select(1, 2, 3, last_col(), id, geometry) %>%
      tibble() %>%
      st_as_sf()

    fmap_multi_data

  } else if(output == 'stats') {
    fmap_multi_stats = fmap_multi %>%
      relocate(1, 2, 3, geometry, everything()) %>%
      tibble() %>%
      dplyr::select(1, 2, 3, last_col(), id)

    fmap_multi_stats

  } else {
    stop('error in output parameter', call. = F)
  }
}
