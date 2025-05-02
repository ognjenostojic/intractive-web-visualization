## Data Description

The precipitation type data used in this application is based on park-specific JSON records. Each value represents a **categorical precipitation type**, coded as follows:

- `1` = Rain  
- `2` = Snow  
- `3` = Freezing Rain  
- `4` = Ice Pellets / Sleet

For visualization purposes (e.g., line charts, plots), these categories are treated numerically and **averaged** when needed (e.g., multiple observations for the same park and date).  

This numerical averaging is a simplification and may not reflect a statistically sound interpretation of categorical data. However, it helps us visualize general precipitation trends across time and parks in a consistent way.

### Gridded Climate Data Source

The precipitation type raster data used for the heatmap is based on:

> **ERA5 monthly averaged reanalysis data on single levels (2005–2025)**  
> Variable: **Precipitation type**  
> Format: **NetCDF**  
> Source: [Copernicus Climate Data Store](https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels-monthly-means)
