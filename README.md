# TEE-dataset
Code for generating the TEE: temperature extremes in Europe dataset.

## Abstract

---
abstract here .....

---

## Code description
Produces three datasets containing measures of extreme temperatures (hot and cold) across Europe at a NUTS2 and NUTS3 spatial resolution. Figure 2 outlines the data analysis pipeline. Briefly, we convert daily temperature available only on a raster grid to the daily mean for each region (polygon data), using daily temperature we calculate seven commonly used metrics of extreme exposure (at a variety of thresholds), and using these define consecutive days of extreme exposure as heatwaves/coldsnaps. 



# Replication notes:

## Steps
- Download data and save in Data folder
- Run script xx to process raster files and generate daily temperature measures (TEEdaily)
- Run the following scripts to compute yearly temperature measures (TEEyearly):
- Run script xx to generate TEEwave dataset 

## R environment

R version 4.4.1 (2024-06-14)
Platform: x86_64-pc-linux-gnu
Running under: AlmaLinux 8.10 (Cerulean Leopard)

Matrix products: default
BLAS/LAPACK: /usr/lib64/libopenblas-r0.3.15.so;  LAPACK version 3.9.0

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8     LC_MONETARY=en_US.UTF-8   
 [6] LC_MESSAGES=en_US.UTF-8    LC_PAPER=en_US.UTF-8       LC_NAME=C                  LC_ADDRESS=C               LC_TELEPHONE=C            
[11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

time zone: America/Chicago
tzcode source: system (glibc)

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] foreign_0.8-86       sf_1.0-16            exactextractr_0.10.0 terra_1.7-78         fs_1.6.4             lubridate_1.9.3     
 [7] forcats_1.0.0        stringr_1.5.1        dplyr_1.1.4          purrr_1.0.2          readr_2.1.5          tidyr_1.3.1         
[13] tibble_3.2.1         ggplot2_3.5.1        tidyverse_2.0.0     

loaded via a namespace (and not attached):
 [1] s2_1.1.6           utf8_1.2.4         generics_0.1.3     lwgeom_0.2-14      class_7.3-22       KernSmooth_2.23-24 stringi_1.8.4     
 [8] lattice_0.22-6     hms_1.1.3          magrittr_2.0.3     grid_4.4.1         timechange_0.3.0   pkgload_1.4.0      e1071_1.7-14      
[15] DBI_1.2.3          fansi_1.0.6        scales_1.3.0       codetools_0.2-20   cli_3.6.3          rlang_1.1.4        crayon_1.5.3      
[22] units_0.8-5        munsell_0.5.1      withr_3.0.0        tools_4.4.1        raster_3.6-26      tzdb_0.4.0         colorspace_2.1-0  
[29] vctrs_0.6.5        R6_2.5.1           zoo_1.8-12         proxy_0.4-27       lifecycle_1.0.4    classInt_0.4-10    pkgconfig_2.0.3   
[36] pillar_1.9.0       gtable_0.3.5       glue_1.7.0         Rcpp_1.0.13        tidyselect_1.2.1   rstudioapi_0.16.0  farver_2.1.2      
[43] wk_0.9.1           compiler_4.4.1     sp_2.1-3          
