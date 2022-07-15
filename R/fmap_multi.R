#' Multiple Fresnel Maps
#'
#' Function for creating multiple Fresnel Maps, or thematic maps that aggregate and visualise spatial data to the level of equal-area concentric circles or doughnuts, in a grid.
#'
#' @param ncircles Number of equal-area concentric circles
#' @param radius_inner Radius of innermost circle in metres
#' @param radius_outer Radius of outermost circle in metres
#' @param geo_points A spatial dataset of points to aggregate
#' @param geo_centres A spatial dataset of points to centre the Fresnel Maps
#' @param id_var ID column from geo_centres dataset
#' @param sum Variable from geo_points for calculating sum
#' @param mean Variable from geo_points for calculating mean
#' @param median Variable from geo_points for calculating median
#' @return Multiple Fresnel Maps, or thematic maps with equal-area Fresnel circles, in a grid and mapped using tmap.
#' @examples
#' data(choleradeaths)
#' data(sohopumps)
#' choleradeaths = choleradeaths %>% st_transform(27700)
#' sohopumps = sohopumps %>% st_transform(27700)
#' fmap_multi(ncircles = 6, radius_outer = 350, geo_points = choleradeaths, geo_centres = sohopumps, id_var = "ID", sum = "Cholera.Deaths")
#' @export

fmap_multi = function(ncircles, radius_inner = NA, radius_outer = NA, geo_points, geo_centres, id_var = NA, sum = NA, mean = NA, median = NA) {

  for (i in ncircles) {

    if(is.na(radius_inner) != TRUE && is.na(radius_outer) != TRUE) {
      warning('Error: radius_inner and radius_outer inputted')

    } else if(is.na(radius_inner) != TRUE && is.na(radius_outer)) {
      inner_circle_area = pi * (radius_inner ^ 2)
      radius = sqrt((inner_circle_area * 1:ncircles) / pi)

    } else if(is.na(radius_inner) && is.na(radius_outer) != TRUE) {
      outer_circle_area = pi * (radius_outer ^ 2)
      area_circles = outer_circle_area / ncircles
      radius_inner = sqrt(area_circles / pi)
      inner_circle_area = pi * (radius_inner ^ 2)
      radius = sqrt((inner_circle_area * 1:ncircles) / pi)
    }

    df_fmap_radii = data.frame(radius)
  }

  geo_points =
    geo_points %>%
    st_as_sf()

  if (is.na(st_crs(geo_points))) {
    warning('Error: unknown crs')
  } else {
    crs = st_crs(geo_points)
  }

  geo_centres =
    geo_centres %>%
    st_as_sf() %>%
    st_transform(crs) %>%
    st_transform(4326) %>%
    st_coordinates() %>%
    data.frame() %>%
    rename(lon = X, lat = Y) %>%
    filter(!is.na(lat)) %>%
    filter(!is.na(lon)) %>%
    mutate(id_var = geo_centres[[id_var]]) %>%
    mutate(id_number = row_number())

  fmaps = data.frame()
  for(i in 1:nrow(geo_centres))  {

    lat = geo_centres[i, "lat"]
    lon = geo_centres[i, "lon"]

    if(is.na(id_var)) {
      id = geo_centres[i, "id_number"]
    } else{id = geo_centres[i, "id_var"]}

    coords = data.frame(lat, lon)

    crs_aeqd = sprintf("+proj=aeqd +lat_0=%s +lon_0=%s +x_0=0 +y_0=0", coords$lat, coords$lon)

    circles = list()
    for(i in 1:nrow(df_fmap_radii))  {
      circles[[i]] =
        coords %>%
        st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
        st_transform(crs_aeqd) %>%
        st_buffer(df_fmap_radii[i, "radius"], nQuadSegs = 2500) %>%
        mutate(circle = df_fmap_radii[i, "circle"])
    }

    inner_circle = circles[[1]]
    outer_circles = list()
    for(i in 2:length(circles))  {
      outer_circles[[i]] = st_difference(circles[[i]], circles[[i-1]])
    }
    outer_circles = do.call(rbind, outer_circles)

    circles =
      inner_circle %>%
      rbind(outer_circles) %>%
      st_as_sf() %>%
      mutate(circle = 1:ncircles) %>%
      mutate(radius = df_fmap_radii$radius) %>%
      mutate(id = id) %>%
      arrange(circle) %>%
      st_make_valid(TRUE)

    geo_points =
      geo_points %>%
      st_transform(crs_aeqd)

    if(is.na(sum) == FALSE && is.na(mean) == FALSE && is.na(median) == FALSE | is.na(sum) == FALSE && is.na(mean) == FALSE | is.na(mean) == FALSE && is.na(median) == FALSE | is.na(sum) == FALSE && is.na(median) == FALSE) {
      warning('Error: multiple aggregations inputted')
    }

    if(is.na(mean) && is.na(sum) && is.na(median)) {

      fmap_count =
        circles %>%
        mutate(Count = lengths(st_intersects(., geo_points))) %>%
        mutate(id = id) %>%
        st_transform(crs) %>%
        dplyr::select(Count, radius, circle, id)

      fmaps = rbind(fmaps, fmap_count)

      title = "Count"

    } else if (is.na(sum) != TRUE && is.na(mean) && is.na(median)) {

      fmap_sum =
        circles %>%
        st_join(geo_points) %>%
        group_by(circle, radius) %>%
        summarise(Total = sum(!! sym(sum), na.rm = T)) %>%
        mutate(id = id) %>%
        st_transform(crs) %>%
        dplyr::select(Total, radius, circle, id)

      fmaps = rbind(fmaps, fmap_sum)

      title = paste0("Total ", '("', sum, '")')

    } else if (is.na(mean) != TRUE && is.na(sum) && is.na(median)) {

      fmap_mean =
        circles %>%
        st_join(geo_points) %>%
        group_by(circle, radius) %>%
        summarise(Mean = mean(!! sym(mean), na.rm = T)) %>%
        mutate(id = id) %>%
        st_transform(crs) %>%
        dplyr::select(Mean, circle, radius, id)

      fmaps = rbind(fmaps, fmap_mean)

      title = paste0("Mean ", '("', mean, '")')

    } else if (is.na(median) != TRUE && is.na(sum) && is.na(mean)) {

      fmap_median =
        circles %>%
        st_join(geo_points) %>%
        group_by(circle, radius) %>%
        summarise(Median = median(!! sym(median), na.rm = T)) %>%
        mutate(id = id) %>%
        st_transform(crs) %>%
        dplyr::select(Median, circle, radius, id)

      fmaps = rbind(fmaps, fmap_median)

      title = paste0("Median ", '("', median, '")')
    }
  }

  aggregate = colnames(fmaps)[1]

  tm_shape(fmaps, name = "Fresnel Map") +
    tm_fill(col = aggregate, palette = "plasma", title = title, id = "", popup.vars = c("Circle"="circle", "Radius"="radius", aggregate)) +
    tm_borders(col = "black", lwd = 0.8) +
    tm_facets(by='id', ncol = 2, free.scales = FALSE) +
    tm_basemap(server = "OpenStreetMap") +
    tm_view(view.legend.position = c("right", "top")) +
    tm_layout(frame = T,
              legend.text.fontfamily = "Helvetica",
              legend.title.size = 0.64,
              legend.text.size = 0.48,
              legend.outside.position = "right",
              legend.outside.size = 0.22,
              legend.title.fontface = "bold",
              panel.label.fontfamily = "Helvetica",
              panel.label.fontface = "bold",
              panel.label.size = 1.1,
              panel.label.bg.color = NA,
              frame.lwd = 1) +
    tmap_options(show.messages = F, show.warnings = F)
}
