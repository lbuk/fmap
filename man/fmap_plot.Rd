% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fmap_plot.R
\name{fmap_plot}
\alias{fmap_plot}
\title{Fresnel Map}
\usage{
fmap_plot(
  ncircles,
  radius_inner = NULL,
  radius_outer = NULL,
  geo_points,
  lat = NULL,
  lon = NULL,
  geo_centre = NULL,
  geo_centres = NULL,
  id_var = NULL,
  sum = NULL,
  mean = NULL,
  median = NULL,
  count = F
)
}
\arguments{
\item{ncircles}{Number of concentric circular zones of equal area (i.e. Fresnel circles) including the inner circle and annuli}

\item{radius_inner}{Radius of innermost Fresnel circle in metres}

\item{radius_outer}{Radius of outermost Fresnel circle in metres}

\item{geo_points}{A spatial dataset of points to aggregate}

\item{lat}{Latitude of the centre of the Fresnel Map}

\item{lon}{Longitude of the centre of the Fresnel Map}

\item{geo_centre}{A spatial dataset containing the coordinates of the centre of the Fresnel Map}

\item{geo_centres}{A spatial dataset containing the coordinates of the centres of each separate Fresnel Map}

\item{id_var}{Variable from geo_centres containing the location ID}

\item{sum}{Variable from geo_points for calculating sum}

\item{mean}{Variable from geo_points for calculating mean}

\item{median}{Variable from geo_points for calculating median}

\item{count}{Count the number of points from geo_points. Input TRUE to count points. Defaults to FALSE}
}
\value{
A Fresnel Map based on aggregations of points-based data and visualised using tmap.
}
\description{
Function for plotting Fresnel Maps, or thematic maps that visualise spatial data to the level of equal-area concentric circular zones (or annuli).
}
\examples{
# Load the sf datasets of cholera deaths and Soho pumps
data(cholera_deaths, soho_pumps)

# Filter the Broad Street Pump from the Soho pumps dataset
bstreet_pump = soho_pumps \%>\% filter(soho.pump == "Broad Street")

# The Fresnel Map based on the Broad Street Pump
fmap_plot(radius_inner = 125, ncircles = 8, geo_centre = bstreet_pump, geo_points = cholera_deaths, sum = "cholera.deaths")

# Map of multiple Fresnel Maps based on each Soho pump
fmap_plot(radius_inner = 125, ncircles = 2, geo_centres = soho_pumps, id_var = "soho.pump", geo_points = cholera_deaths, sum = "cholera.deaths")
}
