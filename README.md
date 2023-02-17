# fmap

### Overview

fmap is an R package for creating [Fresnel Maps](https://www.liamthomasbolton.com/portfolio/FresnelMap/), a new equal-area thematic mapping technique in which data is visualised to concentric circles (or annuli) of equal area.

### Installation

    library(devtools)
    install_github("lbuk/fmap")

### Use

    library(fmap)

    # Load the sf dataset of cholera deaths
    data(choleradeaths)
    
    # Load the sf dataset of Soho pumps
    data(sohopumps)

    # Filter the Broad Street Pump from the dataset of Soho pumps
    broadstpump = 
        sohopumps %>%
        filter(Soho.Pump == "Broad Street")

    # Set to static mapping in tmap. For interactive mapping use tmap_mode("view")
    tmap_mode("plot")

    # Visualise the Fresnel Map
    fmap_plot(radius_inner = 125, ncircles = 8, geo_centre = broadstpump, geo_points = choleradeaths, sum = "Cholera.Deaths")

![](https://github.com/lbuk/fmap/blob/master/img/fmap_johnsnow_example.png)

