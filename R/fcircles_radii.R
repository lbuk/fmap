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

fcircles_radii <- function(ncircles, 
                           radius_inner = NULL, radius_outer = NULL) {
  
  inner <- !is.null(radius_inner)
  
  outer <- !is.null(radius_outer)
  
  if(!inner && !outer) {
    stop('input either radius_inner or radius_outer', call. = FALSE)
  }
  
  if(inner && outer) {
    stop('input either radius_inner or radius_outer', call. = FALSE)
  }
  
  if(inner && radius_inner <= 0) {
    stop('radius_inner must be > 0', call. = FALSE)
  }
  
  if(outer && radius_outer <= 0) {
    stop('radius_outer must be > 0', call. = FALSE)
  }
  
  if(ncircles <= 1) {
    stop('ncircles must be >= 2', call. = FALSE)
  }
  
  if(ncircles != round(ncircles)) {
    stop('ncircles must be an integer', call. = FALSE)
  }
  
  if(inner) {
    area_fc <- pi * (radius_inner ^ 2)

  } else {
    area_c <- pi * (radius_outer ^ 2)
    area_fc <- area_c / ncircles
  }
  
  radius <- sqrt((area_fc * 1:ncircles) / pi)
  
  df_radii <- data.frame(zone = 1:ncircles, radius = radius)

  df_radii
}