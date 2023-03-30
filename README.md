# fmap

### Overview

fmap is an R package for creating [Fresnel Maps](https://www.liamthomasbolton.com/portfolio/FresnelMap/), a new thematic mapping technique in which spatial data is visualised to concentric circles (or annuli) of equal area.

### Installation

    library(devtools)
    install_github("lbuk/fmap")

### Use

    library(fmap)

    # Load the sf datasets of cholera deaths and Soho pumps
    data(choleradeaths, sohopumps)

    # Filter the Broad Street Pump from the Soho pumps dataset
    broadstreetpump = sohopumps %>% filter(Soho.Pump == "Broad Street")

    # Set to static mapping in tmap. For interactive mapping use tmap_mode("view")
    tmap_mode("plot")

    # Visualise the Fresnel Map
    fmap_plot(radius_inner = 125, ncircles = 8, geo_centre = broadstreetpump, geo_points = choleradeaths, sum = "Cholera.Deaths")

![](https://github.com/lbuk/fmap/blob/master/img/fmap_johnsnow_example.png)