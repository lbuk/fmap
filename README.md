# fmap

### Overview

fmap is an R package for creating [Fresnel Maps](https://www.liamthomasbolton.com/portfolio/FresnelMap/), a new thematic mapping technique in which spatial data is visualised to concentric circular zones (or annuli) of equal area.

### Installation

fmap can be installed from Github.

    library(devtools)
    install_github("lbuk/fmap")

### Use

    library(fmap)
    library(dplyr)
    library(sf)

    # Load the sf datasets of cholera deaths and Soho pumps
    data(cholera_deaths, soho_pumps)

    # Filter the Broad Street Pump from the Soho pumps dataset
    bstreet_pump <- soho_pumps %>% filter(soho.pump == "Broad Street")

    # Create some example Fresnel circles
    zones_example <- fcircles(radius_inner = 125, ncircles = 8, geo_centre = bstreet_pump)

    # Visualise the Fresnel circles
    plot(st_geometry(zones_example))

![](https://github.com/lbuk/fmap/blob/master/img/zones_example.png)

    # Visualise the Fresnel Map
    fmap_plot(radius_inner = 150, ncircles = 6, geo_centre = bstreet_pump, geo_points = cholera_deaths, sum = "cholera.deaths")

![](https://github.com/lbuk/fmap/blob/master/img/map_example.png)