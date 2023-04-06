# fmap

### Overview

fmap is an R package for creating [Fresnel Maps](https://www.liamthomasbolton.com/portfolio/FresnelMap/), a new thematic mapping technique in which spatial data is visualised to concentric circles (or annuli) of equal area.

Inspired by Fresnel lenses used in lighthouses and the Codex Atlanticus 221v-b, the geometric technique of creating concentric circles of equal area has been utilised in architecture and civil engineering. The Fresnel Map applies this geometric technique in a spatial context.

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