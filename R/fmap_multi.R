#' Multiple Fresnel Maps
#'
#' Function for plotting or deriving data or stats from multiple Fresnel Maps, or thematic maps that visualise geospatial data to the level of equal-area concentric circles (or annuli), across a range of locations.
#'
#' @param ncircles Number of equal-area concentric circles
#' @param radius_inner Radius of innermost circle in metres
#' @param radius_outer Radius of outermost circle in metres
#' @param geo_points A geospatial dataset of points to aggregate
#' @param geo_centres A geospatial dataset containing the coordinates of the centres of each Fresnel Map
#' @param id_var Variable from geo_centres containing the ID
#' @param sum Variable from geo_points for calculating sum
#' @param mean Variable from geo_points for calculating mean
#' @param median Variable from geo_points for calculating median
#' @param count Count the number of points from geo_points. Input TRUE to count points. Defaults to FALSE
#' @param output Output of function. Input either 'plot', 'data' or 'stats'. Defaults to 'plot'
#' @return A map, sf dataset or stats based on multiple Fresnel Maps.
#' @examples
#' # Load the sf datasets of cholera deaths and Soho pumps
#' data(choleradeaths, sohopumps)
#'
#' fmap_multi(ncircles = 2, radius_outer = 150, geo_points = choleradeaths, geo_centres = sohopumps, id_var = "Soho.Pump", sum = "Cholera.Deaths")
#' @export

fmap_multi = function(ncircles, radius_inner = NULL, radius_outer = NULL, geo_points, geo_centres, id_var = NULL, sum = NULL, mean = NULL, median = NULL, count = F, output = 'plot') {

  if(is.null(radius_inner) && is.null(radius_outer)) {
    stop('radius_inner or radius_outer not inputted')

  } else if(is.null(radius_inner) != T && is.null(radius_outer) != T) {
    stop('radius_inner and radius_outer inputted')

  } else if(ncircles%%1 != 0 | ncircles <= 1) {
    stop('ncircles should not be <= 1 or a decimal number')

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

  df_fmap_radii = data.frame(radius)

  if(grepl(x = class(geo_points)[1], pattern = "sf", ignore.case = T) != T && grepl(x = class(geo_points)[1], pattern = "sp", ignore.case = T) != T) {
    stop('input geo_points as a geospatial dataset of points')

  } else {
    geo_points = geo_points %>% st_as_sf()

    crs = st_crs(geo_points)
  }

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
    stop('input geo_centres as a geospatial dataset of points')

  } else {
    geo_centres = geo_centres %>%
      st_as_sf() %>%
      st_transform(crs) %>%
      st_transform(4326) %>%
      st_coordinates() %>%
      data.frame() %>%
      rename(lon = X, lat = Y) %>%
      mutate(id = geo_centres$id)
  }

  fmaps = lapply(1:nrow(geo_centres), function(i) {
    lat = geo_centres[i, "lat"]
    lon = geo_centres[i, "lon"]

    id = geo_centres[i, "id"]

    coords = data.frame(lat, lon)

    crs_aeqd = sprintf("+proj=aeqd +lat_0=%s +lon_0=%s +x_0=0 +y_0=0", coords$lat, coords$lon)

    circles = lapply(1:nrow(df_fmap_radii), function(i) {
      coords %>%
        st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
        st_transform(crs_aeqd) %>%
        st_buffer(df_fmap_radii[i, "radius"], nQuadSegs = 1375) %>%
        mutate(circle = df_fmap_radii[i, "circle"])
    })

    inner_fcircle = circles[[1]]

    outer_fcircles = lapply(2:length(circles), function(i)  {
      st_difference(circles[[i]], circles[[i-1]])
    })

    outer_fcircles = do.call(rbind, outer_fcircles)

    fcircles = inner_fcircle %>%
      rbind(outer_fcircles) %>%
      mutate(zonal_area = 1:ncircles, radius = df_fmap_radii$radius) %>%
      arrange(zonal_area) %>%
      st_make_valid(T)

    geo_points = geo_points %>% st_transform(crs_aeqd)

    if(is.null(sum) && is.null(mean) && is.null(median) && count != T) {
      stop('no aggregation inputted')

    } else if(is.null(mean) && is.null(sum) && is.null(median) && count == T) {
      fmaps = fcircles %>%
        mutate(circle_count = lengths(st_intersects(., geo_points)), id = id, title = "Count") %>%
        st_transform(crs) %>%
        select(circle_count, radius, zonal_area, id, title)

    } else if(is.null(sum) != T && is.null(mean) && is.null(median) && count == F) {
      fmaps = fcircles %>%
        st_join(geo_points) %>%
        group_by(zonal_area, radius) %>%
        summarise(sum_calc = sum(!! sym(sum), na.rm = T)) %>%
        st_transform(crs) %>%
        mutate(id = id, title = paste0("Total ", '("', sum, '")')) %>%
        rename(sum = sum_calc) %>%
        select(sum, radius, zonal_area, id, title)

    } else if(is.null(mean) != T && is.null(sum) && is.null(median) && count == F) {
      fmaps = fcircles %>%
        st_join(geo_points) %>%
        group_by(zonal_area, radius) %>%
        summarise(mean_calc = mean(!! sym(mean), na.rm = T)) %>%
        st_transform(crs) %>%
        mutate(id = id, title = paste0("Mean ", '("', mean, '")')) %>%
        rename(mean = mean_calc) %>%
        select(mean, zonal_area, radius, id, title)

    } else if(is.null(median) != T && is.null(sum) && is.null(mean) && count == F) {
      fmaps = fcircles %>%
        st_join(geo_points) %>%
        group_by(zonal_area, radius) %>%
        summarise(median_calc = median(!! sym(median), na.rm = T)) %>%
        st_transform(crs) %>%
        mutate(id = id, title = paste0("Median ", '("', median, '")')) %>%
        rename(median = median_calc) %>%
        select(median, zonal_area, radius, id, title)

    } else {
      stop('error in aggregation parameter')
    }
  })

  fmaps = do.call(rbind, fmaps)

  aggregate = colnames(fmaps)[1]

  title = fmaps$title[1]

  if(output == 'plot') {
    tm_shape(fmaps, name = "Fresnel Map") +
      tm_fill(col = aggregate, palette = "plasma", title = title, id = "", popup.vars = c("Zonal Area" = "zonal_area", "Radius" = "radius", aggregate)) +
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
    fmaps_multi_data = fmaps %>% select(zonal_area, radius, 1, id)

    fmaps_multi_data

  } else if(output == 'stats') {
    fmaps_multi_stats = fmaps %>%
      data.frame() %>%
      select(zonal_area, radius, 1, id) %>%
      as_tibble()

    fmaps_multi_stats

  } else {
    stop('error in output parameter')
  }
}
