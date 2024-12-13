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


