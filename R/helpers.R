#' @noRd
validate_centres <- function(geo_centre, lat, lon, 
                             geo_centres, id_var) {
  latlon <- !is.null(lat) && !is.null(lon)
  one_centre <- !is.null(geo_centre)
  multi_centres <- !is.null(geo_centres)
  centre_inputs <- sum(latlon, one_centre, multi_centres)
  
  if(centre_inputs == 0) {
    stop('no centre coordinates provided. input either geo_centre, (lat and lon) or geo_centres', call. = FALSE)
  }
  
  if(centre_inputs > 1) {
    stop('input either geo_centre, (lat and lon) or geo_centres', call. = FALSE)
  }
  
  if(one_centre && nrow(geo_centre) > 1) {
    stop('geo_centre should contain only one point', call. = FALSE)
  }
  
  if(multi_centres && nrow(geo_centres) == 1) {
    stop('geo_centres should contain multiple points', call. = FALSE)
  }
  
  if((one_centre || latlon) && !is.null(id_var)) {
    stop('id_var only applies to geo_centres', call. = FALSE)
  }
  
  list(latlon = latlon, one_centre = one_centre, multi_centres = multi_centres, single = latlon || one_centre, multiple = multi_centres)
}

#' @noRd
validate_aggregation <- function(sum, mean, median, count) {
  methods <- sum(!is.null(sum), !is.null(mean), !is.null(median), count)
  
  if(methods == 0) {
    stop('no aggregation method specified', call. = FALSE)
  }
  
  if(methods > 1) {
    stop('specify either count, sum, mean or median', call. = FALSE)
  }
  
  invisible(NULL)
}

#' @noRd
validate_geo <- function(geo_data, param) {
  if(!inherits(geo_data, c("sf", "sfc", "Spatial"))) {
    stop(sprintf('%s must be an sf or sp dataset', param), call. = FALSE)
  }

  invisible(NULL)
}