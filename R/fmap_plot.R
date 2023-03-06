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
#' # Load the sf dataset of cholera deaths
#' data(choleradeaths)
#'
#' # Load the sf dataset of Soho pumps
#' data(sohopumps)
#'
#' # Filter the Broad Street Pump
#' broadstreetpump =
#'   sohopumps %>%
#'   filter(Soho.Pump == "Broad Street")
#'
#' fmap_plot(radius_inner = 125, ncircles = 8, geo_centre = broadstreetpump, geo_points = choleradeaths, sum = "Cholera.Deaths")
#' @export

fmap_plot = function(ncircles, radius_inner = NULL, radius_outer = NULL, lat = NULL, lon = NULL, geo_centre = NULL, geo_points, sum = NULL, mean = NULL, median = NULL, count = F) {

  for(i in ncircles) {
    if(is.null(radius_inner) && is.null(radius_outer)) {
      stop('radius_inner or radius_outer not inputted')

    } else if(is.null(radius_inner) != T && is.null(radius_outer) != T) {
      stop('radius_inner and radius_outer inputted')

    } else if(ncircles%%1 != 0 | ncircles <= 1) {
      stop('ncircles should not be <= 1 or a decimal number')

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
      sf::st_as_sf() %>%
      sf::st_transform(4326) %>%
      sf::st_coordinates() %>%
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

  circles =
    lapply(1:nrow(df_fmap_radii), function(i) {
      coords %>%
        sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
        sf::st_transform(crs_aeqd) %>%
        st_buffer(df_fmap_radii[i, "radius"], nQuadSegs = 2175) %>%
        mutate(circle = df_fmap_radii[i, "circle"])
    })

  outer_circles =
    lapply(2:length(circles), function(i)  {
      st_difference(circles[[i]], circles[[i-1]])
    })
  outer_circles = do.call(rbind, outer_circles)
  inner_circle = circles[[1]]

  fcircles =
    inner_circle %>%
    rbind(outer_circles) %>%
    mutate(zonal_area = 1:ncircles, radius = df_fmap_radii$radius) %>%
    arrange(zonal_area) %>%
    st_make_valid(T)

  if(grepl(x = class(geo_points)[1], pattern = "sf", ignore.case = T) != T && grepl(x = class(geo_points)[1], pattern = "sp", ignore.case = T) != T) {
    stop('input geo_points as a geospatial dataset of points')

  } else {
    geo_points =
      geo_points %>%
      sf::st_as_sf() %>%
      sf::st_transform(crs_aeqd)
  }

  if(is.null(sum) && is.null(mean) && is.null(median) && count != T) {
    stop('no aggregation inputted')

  } else if(is.null(mean) && is.null(sum) && is.null(median) && count == T) {
    fmap =
      fcircles %>%
      mutate(circle_count = lengths(st_intersects(., geo_points))) %>%
      dplyr::select(circle_count, zonal_area, radius)

    title = "Count"

  } else if(is.null(sum) != T && is.null(mean) && is.null(median) && count == F) {
    fmap =
      fcircles %>%
      st_join(geo_points) %>%
      group_by(zonal_area, radius) %>%
      dplyr::summarise(sum = sum(!! sym(sum), na.rm = T)) %>%
      dplyr::select(sum, zonal_area, radius)

    title = paste0("Total ", '("', sum, '")')

  } else if(is.null(mean) != T && is.null(sum) && is.null(median) && count == F) {
    fmap =
      fcircles %>%
      st_join(geo_points) %>%
      group_by(zonal_area, radius) %>%
      dplyr::summarise(mean = mean(!! sym(mean), na.rm = T)) %>%
      dplyr::select(mean, zonal_area, radius)

    title = paste0("Mean ", '("', mean, '")')

  } else if(is.null(median) != T && is.null(sum) && is.null(mean) && count == F) {
    fmap =
      fcircles %>%
      st_join(geo_points) %>%
      group_by(zonal_area, radius) %>%
      dplyr::summarise(median = median(!! sym(median), na.rm = T)) %>%
      dplyr::select(median, zonal_area, radius)

    title = paste0("Median ", '("', median, '")')

  } else {
    stop('error in aggregation parameter')
  }

  aggregate = colnames(fmap)[1]

  tm_shape(fmap, name = "Fresnel Map") +
    tm_fill(col = aggregate, palette = "viridis", title = title, id = "", popup.vars = c("Zonal Area" = "zonal_area", "Radius" = "radius", aggregate)) +
    tm_borders(col = "black", lwd = 0.8) +
    tm_basemap(server = "OpenStreetMap") +
    tm_view(view.legend.position = c("right", "top")) +
    tm_layout(frame = F,
              legend.text.fontfamily = "Helvetica",
              legend.title.size = 0.8,
              legend.text.size = 0.6,
              legend.outside = F,
              legend.title.fontface = "bold") +
    tmap_options(show.messages = F, show.warnings = F)
}
