#' Polygonal Data from the Fresnel Map
#'
#' Function for creating Fresnel Map polygons by aggregating data to the level of equal-area concentric circular zones (or annuli).
#'
#' @param ncircles Number of concentric circular zones of equal area (i.e. Fresnel circles) including the inner circle and annuli
#' @param radius_inner Radius of innermost Fresnel circle in metres
#' @param radius_outer Radius of outermost Fresnel circle in metres
#' @param lat Latitude of the centre of the Fresnel Map
#' @param lon Longitude of the centre of the Fresnel Map
#' @param geo_centre A spatial dataset containing the coordinates of the centre of the Fresnel Map
#' @param geo_centres A spatial dataset containing the coordinates of the centres of each separate Fresnel Map
#' @param id_var Variable from geo_centres containing the location ID
#' @param geo_points A spatial dataset of points to aggregate
#' @param sum Variable from geo_points for calculating sum
#' @param mean Variable from geo_points for calculating mean
#' @param median Variable from geo_points for calculating median
#' @param count Count the number of points from geo_points. Input TRUE to count points. Defaults to FALSE
#' @return An sf dataset of Fresnel Map polygons based on aggregations of points-based data with a custom Azimuthal Equidistant (AEQD) CRS.
#' @examples
#' # Load the sf datasets of cholera deaths and Soho pumps
#' data(cholera_deaths, soho_pumps)
#'
#' # Filter the Broad Street Pump from the Soho pumps dataset
#' bstreet_pump = soho_pumps %>% filter(soho.pump == "Broad Street")
#'
#' # Polygonal data from the Fresnel Map
#' fmap_data(radius_inner = 125, ncircles = 8, geo_centre = bstreet_pump, geo_points = cholera_deaths, sum = "cholera.deaths")
#' @export

fmap_data = function(ncircles, radius_inner = NULL, radius_outer = NULL, geo_points, lat = NULL, lon = NULL, geo_centre = NULL, geo_centres = NULL, id_var = NULL, sum = NULL, mean = NULL, median = NULL, count = F) {

  if(is.null(lat) && is.null(lon) && is.null(geo_centre) && is.null(geo_centres)) {
    stop('no centre coordinates inputted', call. = F)

  } else if(is.null(lat) != T && is.null(lon) != T && is.null(geo_centre) != T && is.null(geo_centres) != T || is.null(lat) != T &&  is.null(lon) || is.null(lon) != T &&  is.null(lat) || is.null(lat) && is.null(lon) != T && is.null(geo_centre) != T || is.null(lat) != T && is.null(lon) && is.null(geo_centre) != T || is.null(lat) != T && is.null(lon) != T && is.null(geo_centres) != T) {
    stop('input geo_centre or geo_centres or lat and lon', call. = F)

  } else if(is.null(geo_centre) != T && is.null(geo_centres) != T) {
    stop('input geo_centre or geo_centres', call. = F)

  } else if(is.null(geo_centres) != T && nrow(geo_centres) == 1) {
    stop('geo_centres should contain multiple points', call. = F)

  } else if(is.null(geo_centre) != T && nrow(geo_centre) > 1) {
    stop('geo_centre should not contain multiple points', call. = F)

  } else if(is.null(geo_centre) != T || is.null(lat) != T && is.null(lon) != T) {
    df = fcircles(ncircles = ncircles, radius_inner = radius_inner, radius_outer = radius_outer, lat = lat, lon = lon, geo_centre = geo_centre)

    crs_aeqd = st_crs(df)

    if(grepl(x = class(geo_points)[1], pattern = "sf", ignore.case = T) != T && grepl(x = class(geo_points)[1], pattern = "sp", ignore.case = T) != T) {
      stop('input geo_points as a points-based spatial dataset', call. = F)

    } else {
      geo_points = geo_points %>%
        st_as_sf() %>%
        st_transform(crs_aeqd)
    }

    if(is.null(sum) && is.null(mean) && is.null(median) && count != T) {
      stop('no aggregation inputted', call. = F)

    } else if(is.null(mean) && is.null(sum) && is.null(median) && count == T) {
      data = df %>%
        mutate(count = lengths(st_intersects(., geo_points))) %>%
        dplyr::select(zonal_area, radius, count, geometry) %>%
        tibble() %>%
        st_as_sf()

      data

    } else if(is.null(sum) != T && is.null(mean) && is.null(median) && count == F) {
      data = df %>%
        st_join(geo_points) %>%
        group_by(zonal_area, radius) %>%
        dplyr::summarise(sum = sum(!! sym(sum), na.rm = T))

      data

    } else if(is.null(mean) != T && is.null(sum) && is.null(median) && count == F) {
      data = df %>%
        st_join(geo_points) %>%
        group_by(zonal_area, radius) %>%
        dplyr::summarise(mean = mean(!! sym(mean), na.rm = T))

      data

    } else if(is.null(median) != T && is.null(sum) && is.null(mean) && count == F) {
      data = df %>%
        st_join(geo_points) %>%
        group_by(zonal_area, radius) %>%
        dplyr::summarise(median = median(!! sym(median), na.rm = T))

      data

    } else {
      stop('error in aggregation parameter', call. = F)
    }

  } else if(is.null(geo_centres) != T && is.null(geo_centre) && is.null(lat) && is.null(lon)) {
    df_radii = fcircles_radii(ncircles = ncircles, radius_inner = radius_inner, radius_outer = radius_outer)

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

    df = lapply(1:nrow(geo_centres), function(i) {
      lat = geo_centres[i, "lat"]
      lon = geo_centres[i, "lon"]

      id = geo_centres[i, "id"]

      coords = data.frame(lat, lon)

      crs_aeqd = sprintf("+proj=aeqd +lat_0=%s +lon_0=%s +x_0=0 +y_0=0", coords$lat, coords$lon)

      circles = lapply(1:nrow(df_radii), function(i) {
        coords %>%
          st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
          st_transform(crs_aeqd) %>%
          st_buffer(df_radii[i, "radius"], nQuadSegs = 1375)
      })

      inner_circle = circles[[1]]

      outer_circles = lapply(2:length(circles), function(i)  {
        st_difference(circles[[i]], circles[[i-1]])
      })

      outer_circles = do.call(rbind, outer_circles)

      fcircles_data = inner_circle %>%
        rbind(outer_circles) %>%
        mutate(zonal_area = 1:ncircles, radius = df_radii$radius) %>%
        arrange(zonal_area) %>%
        st_make_valid(T)

      geo_points = geo_points %>%
        st_transform(crs_aeqd)

      if(is.null(sum) && is.null(mean) && is.null(median) && count != T) {
        stop('no aggregation inputted', call. = F)

      } else if(is.null(mean) && is.null(sum) && is.null(median) && count == T) {
        df = fcircles_data %>%
          mutate(count = lengths(st_intersects(., geo_points)), id = id) %>%
          st_transform(crs)

      } else if(is.null(sum) != T && is.null(mean) && is.null(median) && count == F) {
        df = fcircles_data %>%
          st_join(geo_points) %>%
          group_by(zonal_area, radius) %>%
          dplyr::summarise(sum_calc = sum(!! sym(sum), na.rm = T)) %>%
          st_transform(crs) %>%
          mutate(id = id) %>%
          dplyr::rename(sum = sum_calc)

      } else if(is.null(mean) != T && is.null(sum) && is.null(median) && count == F) {
        df = fcircles_data %>%
          st_join(geo_points) %>%
          group_by(zonal_area, radius) %>%
          dplyr::summarise(mean_calc = mean(!! sym(mean), na.rm = T)) %>%
          st_transform(crs) %>%
          mutate(id = id) %>%
          dplyr::rename(mean = mean_calc)

      } else if(is.null(median) != T && is.null(sum) && is.null(mean) && count == F) {
        df = fcircles_data %>%
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
        df = df %>%
          dplyr::mutate(!!paste(id_var) := id)

      } else {
        df = df
      }
    })

    df = do.call(rbind, df)

    data = df %>%
      relocate(1, 2, 3, geometry, everything()) %>%
      tibble() %>%
      st_as_sf() %>%
      dplyr::select(1, 2, 3, id, last_col(), geometry)

    data

  } else {
    stop('error in input of parameters', call. = F)
  }
}
