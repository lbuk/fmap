# fmap

### Overview
fmap is an R package for creating [Fresnel Maps](https://www.liamthomasbolton.com/portfolio/FresnelMap/). The Fresnel Map is a new thematic mapping technique in which data is aggregated and visualised to the level of equal-area concentric circles or doughnuts.

### Installation
```
# Load devtools
library(devtools)

# Install fmap from Github
devtools::install_github("lbuk/fmap")
library(fmap)
```

### Use
```
library(fmap)

# Use the fmap_plot function to create a Fresnel Map centred on the Broad Street Pump in Soho in London that utilises data on cholera deaths from John Snow's 1854 analysis

# Load the sf dataset and transform the CRS to BNG
data(choleradeaths)
choleradeaths = choleradeaths %>% st_transform(27700)

# Set to static mapping in tmap. For interactive mapping use tmap_mode("view")
tmap_mode("plot")

# Visualise the Fresnel Map
fmap_plot(radius_inner = 125, ncircles = 8, 
          lat = 51.51334, lon = -0.1366678,
          geo_points = choleradeaths, sum = "Cholera.Deaths")
```
![](https://github.com/lbuk/fmap/blob/master/img/fmap_johnsnow_example.png)
