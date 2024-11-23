#' Fresnel Circles Radii
#'
#' Function for creating a dataset of the Fresnel circles radii and zones.
#'
#' @param ncircles Number of concentric circular zones of equal area (i.e. Fresnel circles) including the inner circle and annuli
#' @param radius_inner Radius of innermost Fresnel circle in metres
#' @param radius_outer Radius of outermost Fresnel circle in metres
#' @return A dataset of Fresnel circles radii.
#' @examples
#' # A dataset of the Fresnel circles radii and zones
#' fcircles_radii(ncircles = 7, radius_outer = 300)
#' @export

fcircles_radii <- function(ncircles, radius_inner = NULL, radius_outer = NULL) {

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
    area_fc <- pi * (radius_inner ^ 2)

  } else {
    area_c <- pi * (radius_outer ^ 2)
    area_fc <- area_c / ncircles
  }

  radius <- sqrt((area_fc * 1:ncircles) / pi)

  df_radii <- data.frame(radius, zone = 1:ncircles)

  df_radii
}
