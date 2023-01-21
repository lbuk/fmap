# fmap

### Overview

fmap is an R package for creating [Fresnel Maps](https://www.liamthomasbolton.com/portfolio/FresnelMap/), a new equal-area thematic mapping technique in which data is visualised to concentric circles of equal area.

### Installation

    library(devtools)
    install_github("lbuk/fmap")

### Use

    library(fmap)

    # Load the dataset and transform the CRS to BNG
    data(choleradeaths)
    choleradeaths = choleradeaths %>% st_transform(27700)

    # Set to static mapping in tmap. For interactive mapping use tmap_mode("view")
    tmap_mode("plot")

    # Visualise the Fresnel Map, which is centred on the Broad Street Pump in Soho in London and based on data from John Snow's seminal analysis
    fmap_plot(radius_inner = 125, ncircles = 8, lat = 51.51334, lon = -0.1366678, geo_points = choleradeaths, sum = "Cholera.Deaths")

![](https://github.com/lbuk/fmap/blob/master/img/fmap_johnsnow_example.png)

