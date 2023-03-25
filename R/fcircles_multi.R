#' Multiple Fresnel Circles
#'
#' Function for creating multiple sets of Fresnel circles polygons across different locations for mapping.
#'
#' @param ncircles Number of equal-area concentric circles
#' @param radius_inner Radius of innermost circle in metres
#' @param radius_outer Radius of outermost circle in metres
#' @param geo_centres A spatial dataset containing the coordinates of the centres of the Fresnel circles
#' @param id_var Variable from geo_centres containing the ID
#' @return An sf dataset of polygons based on multiple sets of Fresnel circles across different locations.
#' @examples
#' # Load the sf dataset of Soho pumps
#' data(sohopumps)
#'
#' fcircles_multi(ncircles = 2, radius_outer = 150, geo_centres = sohopumps, id_var = "Soho.Pump")
#' @export

fcircles_multi = function(ncircles, radius_inner = NULL, radius_outer = NULL, geo_centres, id_var = NULL) {

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

    circles = lapply(1:nrow(df_fmap_radii), function(i) {
      coords %>%
        st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
        st_transform(crs_aeqd) %>%
        st_buffer(df_fmap_radii[i, "radius"], nQuadSegs = 2175) %>%
        mutate(circle = df_fmap_radii[i, "circle"])
    })

    inner_fcircle = circles[[1]]

    outer_fcircles = lapply(2:length(circles), function(i)  {
      st_difference(circles[[i]], circles[[i-1]])
    })

    outer_fcircles = do.call(rbind, outer_fcircles)

    fcircles = inner_fcircle %>%
      rbind(outer_fcircles) %>%
      st_transform(crs) %>%
      st_difference() %>%
      mutate(zonal_area = 1:ncircles, radius = df_fmap_radii$radius, id = id) %>%
      arrange(zonal_area) %>%
      st_make_valid(T)
  })

  fcircles_multi = do.call(rbind, fcircles)

  fcircles_multi
}
