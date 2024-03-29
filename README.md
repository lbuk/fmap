# fmap

### Overview

fmap is an R package for creating [Fresnel Maps](https://www.liamthomasbolton.com/portfolio/FresnelMap/), a new thematic mapping technique in which spatial data is visualised to concentric circles (or annuli) of equal area.

### Installation

    library(devtools)
    install_github("lbuk/fmap")

### Use

    library(fmap)

    # Load the sf datasets of cholera deaths and Soho pumps
    data(cholera_deaths, soho_pumps)

    # Filter the Broad Street Pump from the Soho pumps dataset
    bstreet_pump = soho_pumps %>% filter(soho.pump == "Broad Street")

    # Set to static mapping in tmap. For interactive mapping use tmap_mode("view")
    tmap_mode("plot")

    # Visualise the Fresnel Map
    fmap_plot(radius_inner = 125, ncircles = 8, geo_centre = bstreet_pump, geo_points = cholera_deaths, sum = "cholera.deaths")

![](https://github.com/lbuk/fmap/blob/master/img/fmap_plot_example.png)