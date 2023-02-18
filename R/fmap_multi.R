#' Multiple Fresnel Maps
#'
#' Function for creating or deriving data from multiple Fresnel Maps, or thematic maps that visualise geospatial data to the level of equal-area concentric circles (or annuli), across a range of locations.
#'
#' @param ncircles Number of equal-area concentric circles
#' @param radius_inner Radius of innermost circle in metres
#' @param radius_outer Radius of outermost circle in metres
#' @param geo_points A geospatial dataset of points to aggregate
#' @param geo_centres A geospatial dataset of points across a range of locations to centre the Fresnel Maps
#' @param facet Variable from geo_centres for faceting the maps
#' @param sum Variable from geo_points for calculating sum
#' @param mean Variable from geo_points for calculating mean
#' @param median Variable from geo_points for calculating median
#' @param count Count the number of points from geo_points. Input TRUE to count points. Defaults to FALSE
#' @param output Output of function. Input either 'plot', 'data' or 'stats'. Defaults to 'plot'
#' @return Multiple Fresnel Maps in a grid and visualised using tmap.
#' @examples
#' # Load the sf dataset of cholera deaths
#' data(choleradeaths)
#'
#' # Load the sf dataset of Soho pumps
#' data(sohopumps)
#'
#' fmap_multi(ncircles = 6, radius_outer = 200, geo_points = choleradeaths, geo_centres = sohopumps, facet = "Soho.Pump", sum = "Cholera.Deaths")
#' @export

fmap_multi = function(ncircles, radius_inner = NULL, radius_outer = NULL, geo_points, geo_centres, facet = NULL, sum = NULL, mean = NULL, median = NULL, count = F, output = 'plot') {

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

  if(grepl(x = class(geo_points)[1], pattern = "sf", ignore.case = T) != T && grepl(x = class(geo_points)[1], pattern = "sp", ignore.case = T) != T) {
    stop('input geo_points as a geospatial dataset of points')

  } else {
    geo_points =
      geo_points %>%
      st_as_sf()

    crs = st_crs(geo_points)
  }

  if(is.null(facet)) {
    geo_centres =
      geo_centres %>%
      mutate(id = row_number())

  } else {
    geo_centres =
      geo_centres %>%
      mutate(id = geo_centres[[facet]])
  }

  if(grepl(x = class(geo_centres)[1], pattern = "sf", ignore.case = T) != T && grepl(x = class(geo_centres)[1], pattern = "sp", ignore.case = T) != T) {
    stop('input geo_centres as a geospatial dataset of points')

  } else {
    geo_centres =
      geo_centres %>%
      st_as_sf() %>%
      st_transform(crs) %>%
      st_transform(4326) %>%
      st_coordinates() %>%
      data.frame() %>%
      rename(lon = X, lat = Y) %>%
      filter(!is.null(lat)) %>%
      filter(!is.null(lon)) %>%
      mutate(id = geo_centres$id)
  }

  fmaps = data.frame()
  for(i in 1:nrow(geo_centres))  {
    lat = geo_centres[i, "lat"]
    lon = geo_centres[i, "lon"]

    id = geo_centres[i, "id"]

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
      mutate(id = id) %>%
      arrange(zonal_area) %>%
      st_make_valid(T)

    geo_points =
      geo_points %>%
      st_transform(crs_aeqd)

    if(is.null(sum) && is.null(mean) && is.null(median) && count != T) {
      stop('no aggregation inputted')

    } else if(is.null(mean) && is.null(sum) && is.null(median) && count == T) {
      fmap_count =
        fcircles %>%
        mutate(circle_count = lengths(st_intersects(., geo_points))) %>%
        mutate(id = id) %>%
        st_transform(crs) %>%
        dplyr::select(circle_count, radius, zonal_area, id)

      fmaps = rbind(fmaps, fmap_count)

      title = "Count"

    } else if(is.null(sum) != T && is.null(mean) && is.null(median) && count == F) {
      fmap_sum =
        fcircles %>%
        st_join(geo_points) %>%
        group_by(zonal_area, radius) %>%
        summarise(sum = sum(!! sym(sum), na.rm = T)) %>%
        mutate(id = id) %>%
        st_transform(crs) %>%
        dplyr::select(sum, radius, zonal_area, id)

      fmaps = rbind(fmaps, fmap_sum)

      title = paste0("Total ", '("', sum, '")')

    } else if(is.null(mean) != T && is.null(sum) && is.null(median) && count == F) {
      fmap_mean =
        fcircles %>%
        st_join(geo_points) %>%
        group_by(zonal_area, radius) %>%
        summarise(mean = mean(!! sym(mean), na.rm = T)) %>%
        mutate(id = id) %>%
        st_transform(crs) %>%
        dplyr::select(mean, zonal_area, radius, id)

      fmaps = rbind(fmaps, fmap_mean)

      title = paste0("Mean ", '("', mean, '")')

    } else if(is.null(median) != T && is.null(sum) && is.null(mean) && count == F) {
      fmap_median =
        fcircles %>%
        st_join(geo_points) %>%
        group_by(zonal_area, radius) %>%
        summarise(median = median(!! sym(median), na.rm = T)) %>%
        mutate(id = id) %>%
        st_transform(crs) %>%
        dplyr::select(median, zonal_area, radius, id)

      fmaps = rbind(fmaps, fmap_median)

      title = paste0("Median ", '("', median, '")')

    } else {
      stop('error in aggregation parameter')
    }
  }

  aggregate = colnames(fmaps)[1]

  if(output == 'plot') {
    tm_shape(fmaps, name = "Fresnel Map") +
      tm_fill(col = aggregate, palette = "plasma",
              title = title, id = "", popup.vars = c("Zonal Area" = "zonal_area", "Radius" = "radius", aggregate)) +
      tm_borders(col = "black", lwd = 0.8) +
      tm_facets(by='id', ncol = 2, free.scales = F) +
      tm_basemap(server = "OpenStreetMap") +
      tm_view(view.legend.position = c("right", "top")) +
      tm_layout(frame = F,
                legend.text.fontfamily = "Helvetica",
                legend.title.size = 0.8,
                legend.text.size = 0.6,
                legend.outside = F,
                legend.title.fontface = "bold",
                panel.label.fontfamily = "Helvetica",
                panel.label.fontface = "bold",
                panel.label.size = 1.1,
                panel.label.bg.color = NA,
                frame.lwd = 0) +
      tmap_options(show.messages = F, show.warnings = F)

  } else if(output == 'data') {
    fmaps =
      fmaps %>%
      dplyr::select(zonal_area, radius, 1, id)

    fmaps

  } else if(output == 'stats') {
    fmaps %>%
      as.data.frame() %>%
      dplyr::select(zonal_area, radius, 1, id) %>%
      print()

  } else {
    stop('error in output parameter')
  }
}
