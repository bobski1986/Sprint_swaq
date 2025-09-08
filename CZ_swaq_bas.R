pkg <- c("tidyverse", "fs", "readxl", "terra", "tmap", "OpenStreetMap", "maptiles", "tidyterra", "sf", "data.table", "fuzzyjoin", "openxlsx",
         "EnvStats", "leaflet", "htmltools")

for (i in pkg) {
  if (!requireNamespace(i, quietly = TRUE)) { 
    install.packages(i)
  }
}

lapply(pkg, library, character.only = T)

# Data import for the Czech Republic
# NUTS regions
nuts <- dir_ls(path_home_r(), recurse = T, regexp = "/NUTS_RG_20M_2024_4326.shp$")  |>
  vect()

nuts3_cz <- nuts |>
  filter(CNTR_CODE == "CZ" & LEVL_CODE == 3)

nuts1_cz <- nuts |>
  filter(CNTR_CODE == "CZ" & LEVL_CODE == 1) 

district_cz_select <- nuts3_cz |> select(NUTS_NAME)

# Read soil density map
budens_jrc_cz_path <- dir_ls(path_home_r(),
                             recurse = T,
                             regexp = "budens_jrc_cz.nc$")
budens_jrc_cz <- rast(budens_jrc_cz_path)

# Read Czech river network from the country geoportal
rivers_cz <- dir_ls(path_home_r(), recurse = T, regexp = "WatrcrsL.shp$") |> 
  vect() |> 
  select(NAMN1, HYDROID, WD7, WD8 , SHAPE_Leng ) |> 
  project(nuts1_cz) |>
  intersect(basins_cz["HYBAS_ID"])

# Czech river basins including data on soil, basin and terrain characteristics
basins_cz <- dir_ls(path_home_r(), recurse = T, regexp = "hydrosheds_lvl09_basins_cz.gpkg") |>
  vect() |>
  select("HYBAS_ID",
         "SUB_AREA",
         starts_with(c("pre_mm", "dis_m3", "run_mm")),
         "run_mm_syr",
         "slp_dg_uav",
         "cly_pc_uav",
         "snd_pc_uav",
         "soc_th_uav") |>
  tidyterra::mutate(SUB_AREA_ha = SUB_AREA * 100,
                    HYBAS_ID = as.character(HYBAS_ID)) |>
  mask(vect(RCzechia::republika())) 

budens_jrc_cz_bas <- zonal(budens_jrc_cz, basins_cz, "mean", as.polygons = T, na.rm = T) |>
  select(HYBAS_ID, budens_jrc_CZ)

basins_cz <-  basins_cz |>
  left_join(budens_jrc_cz_bas |>
              values(),
            by = "HYBAS_ID") |>
  tidyterra::mutate(slp_perc_uav = tan((slp_dg_uav/10)/180*pi),
                    vol_soil_m3_ha = 10000 * 0.05,
                    mass_soil_t3_ha = vol_soil_m3_ha * budens_jrc_CZ,
                    oc_perc = (soc_th_uav/mass_soil_t3_ha) * 100,
                    area_basins_cz_ha = basins_cz |>
                      expanse("ha"))

# Add crop specific spray drift values to the "gemap"
# spray_drift <- tibble(
#   sdrift_dist_m =  c(1, 1, 1, 1,
#                      3, 3, 3, 3,
#                      5, 5, 5, 5,
#                      10, 10, 10, 10,
#                      15, 15, 15, 15,
#                      20, 20, 20, 20,
#                      30, 30, 30, 30,
#                      50, 50, 50, 50,
#                      75, 75, 75, 75,
#                      100,100, 100,100),
#   sdrift_90lnorm = c(2.45, NA, NA, NA,
#                      NA, 7.21, 29.90, 19.54,
#                      0.59, 4.07, 26.11, 12.32,
#                      0.32, 1.48, 12.94, 6.23, 
#                      0.13, 0.63, 6.67, 3.72,
#                      0.16, 0.33, 4.53, 2.16,
#                      0.11, 0.03, 2.48, 0.76,
#                      0.10, 0.07, 0.32, 0.18, 
#                      0.03, NA, NA, NA,
#                      0.02, NA, NA, NA),
#   Crop_group_sdrift = c("Arable", "Vines", "Orchards", "Hops",
#                         "Arable", "Vines", "Orchards", "Hops",
#                         "Arable", "Vines", "Orchards", "Hops",
#                         "Arable", "Vines", "Orchards", "Hops",
#                         "Arable", "Vines", "Orchards", "Hops",
#                         "Arable", "Vines", "Orchards", "Hops",
#                         "Arable", "Vines", "Orchards", "Hops",
#                         "Arable", "Vines", "Orchards", "Hops",
#                         "Arable", "Vines", "Orchards", "Hops",
#                         "Arable", "Vines", "Orchards", "Hops")
# ) |> filter(sdrift_dist_m == 100)

# Read Czech gemap
active <- c("Glyphosate" , "Tebuconazole" , "Acetamiprid")

gemap_cz <- dir_ls(path_home_r(), recurse = T, regexp = "gemap1_10_cz_wgs84.gpkg") |> 
  vect() |>
  mutate(acsubst_name = AS |> str_to_title(),
         ARfarm = ARfarm*1000) |> 
  filter(acsubst_name %in% active) |> 
  makeValid()

acsubst_name <- gemap_cz["acsubst_name"] |>
  # mutate(EU_name = str_to_title(EU_name)) |>
  values() |>
  unique() |>
  pull()

# Chemical input data from qsars (vega, epi) and PPDB where available #
source(dir_ls(path_home_r(), recurse = T, regexp = "ppdb scraping"))
chemprop <- chemprop_gen(acsubst_name = c("Glyphosate", "Tebuconazole", "Acetamiprid")) |> 
  select(acsubst_name, DT50_typical_d, Koc_ml.g,  DT50_field_d, Kfoc_ml.g)
  # filter(!is.na(Kfoc_ml.g),
  #        !is.na(Koc_ml.g),
  #        !is.na(DT50_field_d),
  #        !is.na(DT50_typical_d))

# Intersect cz gemap with river basins
gemap_cz_bas <- terra::intersect(gemap_cz |>
                                   select("Crop", "acsubst_name", "ARfarm", "IFav", "ASmass", "ASarea"), basins_cz["HYBAS_ID"])

# Get the number and of all farms, grouped by crop and AS, in each river basin
mass_as_farms_bas_cz <- terra::aggregate(gemap_cz_bas[c("HYBAS_ID", "Crop", "acsubst_name", "ASmass")],
                                      c("HYBAS_ID", "Crop", "acsubst_name"),
                                      fun = "sum") 

area_as_farms_bas_cz <- mass_as_farms_bas_cz |> expanse("ha")

gemap_cz_bas_if <- gemap_cz_bas[c("HYBAS_ID",
                                  "IFav",
                                  "Crop")] |>
  terra::aggregate(c("HYBAS_ID", "Crop"),
                   fun = "mean")

mass_area_as_farms_bas_cz <- cbind(mass_as_farms_bas_cz, area_as_farms_bas_cz |> data.frame()) |>
  terra::merge(basins_cz, by = "HYBAS_ID") |>
  terra::merge(gemap_cz_bas_if[c("HYBAS_ID",
                                 "mean_IFav",
                                 "Crop")],
               by = c("HYBAS_ID", "Crop")) |>
  tidyterra::rename(area_as_farms_bas_cz_ha = area_as_farms_bas_cz,
                    SUB_AREA_km2 = SUB_AREA,
                    nr_farms_as_crop = agg_n) |>
  tidyterra::mutate(area_as_farms_bas_frac = area_as_farms_bas_cz_ha / area_basins_cz_ha,
                    AppRate_kg_ha_corr = ((sum_ASmass*1000)/area_as_farms_bas_cz) * area_as_farms_bas_frac)

####################################################################
########### START: Pesticide Runoff Model Schriever 2007 ###########
####################################################################

# Create new sample for each uncertain input parameter. There are 3 groups of parameters: crop, catchment (soil density, clay, sand, terrain slope etc.), and chemical specific
#....

# Insert sampled data into main dataset

# Run model for new input

conc_acsubst_river_seg_cz[[3]] |>  filter(srunoff_load |> is.na())

# TWA concentration in soil
pest_twc_as_farm_cz <- mass_area_as_farms_bas_cz |>
  rename(IFav = mean_IFav,
         aprate_farm_kg.ha = AppRate_kg_ha_corr,
         bulk_dens_kg.dm3 = budens_jrc_CZ,
         slope_perc = slp_perc_uav) |> 
  mutate(IFav = case_when(Crop == "Not Classified" ~ 0.5,
                          Crop == "ball clover" ~ 0.5,
                          Crop == "purple medick" ~ 0.5,
                          Crop == "frech millet" ~ 0.5,
                          Crop ==  "two-rowed barley" ~ 0.5,
                          Crop ==       "wild carrot" ~ 0.5,
                          Crop ==  "common buckwheat" ~ 0.5,
                          Crop ==       "stone leek" ~ 0.5,
                          Crop ==             "gram" ~ 0.5,
                          Crop ==     "holy thistle" ~ 0.5,
                          Crop == "spring triticale" ~ 0.5,
                                       .default = IFav)) |>
  terra::merge(chemprop, by = "acsubst_name") |>
  # ## AS spray drift loading
  # mutate(sdrift_load = map2_dbl(aprate_farm_kg.ha,
  #                               sdrift_90lnorm,
  #                               ~.x * (.y/100))) |>
  # ## Application rate correction
  # mutate(aprate_farm_kg.ha = map2_dbl(aprate_farm_kg.ha,
  #                                     sdrift_load,
  #                                     ~.x-.y)) |> 
  ## Effect of crop interception factor on runoff
  mutate(infactor_effect = map_dbl(IFav,
                                   ~1-(./100))) |>  
  ## Initial as fraction in soil particles, pore water total soil
  mutate(frac_asubst_soil_water_ini = map2_dbl(oc_perc,
                                               Kfoc_ml.g,
                                               ~1/(1 + (.x*.y)))) |> 
  mutate(frac_asubst_soil_solid_ini = map2_dbl(oc_perc,
                                               Kfoc_ml.g,
                                               ~((.x*.y))/(1 + (.x*.y)))) |> 
  mutate(conc_acsubst_total_soil_ini = pmap_dbl(list(x = aprate_farm_kg.ha,
                                                     y = bulk_dens_kg.dm3,
                                                     z = frac_asubst_soil_water_ini,
                                                     v = frac_asubst_soil_solid_ini),
                                                \(x,y,z,v) (x/y*5)*(z+v))/1000) |> 
  ## lagged as fraction/concentration in soil particles, pore water total soil
  mutate(frac_asubst_soil_water_lag = exp(-1 * log(2) / DT50_typical_d) * frac_asubst_soil_water_ini,
         frac_asubst_soil_solid_lag = exp(-1 * log(2) / DT50_typical_d) * frac_asubst_soil_solid_ini,
         conc_acsubst_total_soil_lag_ug.kg = (frac_asubst_soil_water_lag + frac_asubst_soil_solid_lag) * conc_acsubst_total_soil_ini,
         conc_acsubst_total_soil_56twa_ug.kg = (conc_acsubst_total_soil_ini / (56 * (log(2) / DT50_typical_d))) * (1 - exp(-56 * (log(2) / DT50_typical_d))),
         conc_acsubst_total_soil_365twa_ug.kg = (conc_acsubst_total_soil_ini / (365 * (log(2) / DT50_typical_d))) * (1 - exp(-365 * (log(2) / DT50_typical_d)))) |> 
## Effect of terrain slope
mutate(slope_effect = map_dbl(slope_perc,
                              ~if_else(. <= 20,
                                       0.001423 * .^2 + 0.02153 * .,
                                       1))) |> 
  ## Surface runoff 
  mutate(srunoff_frac = map2_dbl(run_mm_syr,
                                 pre_mm_syr,
                                 ~(.x/.y))) |> 
  ## AS surface runoff loading
  mutate(srunoff_load = pmap_dbl(list(area_as_farms_bas_cz_ha,
                                      aprate_farm_kg.ha,
                                      infactor_effect,
                                      frac_asubst_soil_water_lag,
                                      slope_effect,
                                      srunoff_frac),
                                 prod)) |>
  makeValid()

# Calculate the mean, min and max concentration of the active substance in the river segment
# Aggregate the data to the river segment level and then to the basin level
# conc_acsubst_river_seg_mapinput <- load_acsubst_farm_mapinput |>
#   makeValid() |>
#   terra::intersect(rivers_basin_buff_seg) |>
#   group_by(SHAPE_Leng, HYDROID, acsubst, month, ndays, dis_m3_pyr, dis_m3_pmn, dis_m3_pmx) |>
#   summarise(conc_mean_river_seg = mean(load_acsubst_mean_g.ndays/dis_m3_pyr),
#             conc_min_river_seg = mean(load_acsubst_min_g.ndays/dis_m3_pmn),
#             conc_max_river_seg = mean(load_acsubst_max_g.ndays/dis_m3_pmx))

# Split simulated concentration by substance 
pest_twc_as_basin_cz <- pest_twc_as_farm_cz[c("HYBAS_ID",
                                              "Crop",
                                              "acsubst_name",
                                              "nr_farms_as_crop",
                                              "area_as_farms_bas_cz_ha",
                                              "conc_acsubst_total_soil_ini",
                                              "conc_acsubst_total_soil_56twa_ug.kg",
                                              "conc_acsubst_total_soil_365twa_ug.kg")] |>
  terra::split("acsubst_name")

# Rasterise individual parcel polygons to show AS concentration distribution in individual basins
pest_twc_as_farm_cz_rast <- sprc( rasterize(project(pest_twc_as_basin_cz[[1]] |>
                                                      select("conc_acsubst_total_soil_365twa_ug.kg"),
                                                    crs("EPSG:32633")),
                                            project(budens_jrc_cz, crs("EPSG:32633")),
                                            field = "conc_acsubst_total_soil_365twa_ug.kg",
                                            touches = T) |>
                                    disagg(4) |>
                                    zonal(project(pest_twc_as_basin_cz[[1]], crs("EPSG:32633")), fun = "median", touches = T, as.raster = T),
                                  
                                  rasterize(project(pest_twc_as_basin_cz[[2]] |>
                                                      select("conc_acsubst_total_soil_365twa_ug.kg"),
                                                    crs("EPSG:32633")),
                                            project(budens_jrc_cz, crs("EPSG:32633")),
                                            field = "conc_acsubst_total_soil_365twa_ug.kg",
                                            touches = T) |>
                                    disagg(4) |>
                                    zonal(project(pest_twc_as_basin_cz[[2]], crs("EPSG:32633")), fun = "median", touches = T, as.raster = T),
                                     
                                  rasterize(project(pest_twc_as_basin_cz[[3]] |>
                                                      select("conc_acsubst_total_soil_365twa_ug.kg"),
                                                    crs("EPSG:32633")),
                                            project(budens_jrc_cz, crs("EPSG:32633")),
                                            field = "conc_acsubst_total_soil_365twa_ug.kg",
                                            touches = T) |>
                                      disagg(4) |> 
                                    zonal(project(pest_twc_as_basin_cz[[3]], crs("EPSG:32633")), fun = "median", touches = T, as.raster = T)) 

# Aggregate the data to the basin level
conc_wmean_basin_cz <- list()
area_farm_basin_cz <- list()
nr_farm_basin_cz <- list()
all_as_basin_cz <- list()

for(i in seq_along(pest_twc_as_basin_cz)){
  
  cat("\r", unique(values(pest_twc_as_basin_cz[[i]]["acsubst_name"]))[1,1],
      "is being processed out of",
      seq_along(pest_twc_as_basin_cz) |> max(), 
      "ASs.",
      max(seq_along(pest_twc_as_basin_cz)) - i,
      "ASs left.")
  
  conc_wmean_basin_cz[[i]] <- zonal(pest_twc_as_basin_cz[[i]][,"conc_acsubst_total_soil_365twa_ug.kg"],
                            basins_cz[c("HYBAS_ID")],
                            weighted = T,
                            as.polygons = T)
    
  area_farm_basin_cz[[i]] <- zonal(pest_twc_as_basin_cz[[i]][,"area_as_farms_bas_cz_ha"],
                                basins_cz[c("HYBAS_ID", "SUB_AREA_ha")],
                                fun = "sum",
                                as.polygons = T) 
    
  nr_farm_basin_cz[[i]] <- zonal(pest_twc_as_basin_cz[[i]][,"nr_farms_as_crop"],
                                     basins_cz[c("HYBAS_ID")],
                                     fun = "sum",
                                     as.polygons = T)
    
  all_as_basin_cz[[i]] <- merge(conc_wmean_basin_cz[[i]], area_farm_basin_cz[[i]] |> as.data.table(), by = "HYBAS_ID") |> 
      merge(nr_farm_basin_cz[[i]] |>
              as.data.table(), by = "HYBAS_ID") |>
      # terra::na.omit("HYBAS_ID") |> 
    mutate("Active substance" = unique(values(pest_twc_as_basin_cz[[i]]["acsubst_name"]))[1,1],
           area_as_farms_bas_cz_ha = round(area_as_farms_bas_cz_ha, 1),
           conc_acsubst_total_soil_365twa_ug.kg = round(conc_acsubst_total_soil_365twa_ug.kg, 1)) |> 
    tidyterra::rename("Basin ID" = HYBAS_ID,
                      "Basin area [ha]" = SUB_AREA_ha,
                      "Concentration (TWA) [\u00B5g\u00D7kg\u207B\u00B9]" = conc_acsubst_total_soil_365twa_ug.kg,
                      "Number of parcels" = nr_farms_as_crop,
                      "Agricultural area [ha]" = area_as_farms_bas_cz_ha)
  
}

writeCDF(pest_twc_as_farm_cz_rast[1], "Acetamiprid_pec365_topsoil_farm_cz.nc")
writeCDF(pest_twc_as_farm_cz_rast[2], "Glyphosate_pec365_topsoil_farm_cz.nc")
writeCDF(pest_twc_as_farm_cz_rast[3], "Tebuconazole_pec365_topsoil_farm_cz.nc")
writeVector(vect(all_as_basin_cz[1]), "3chem_pec365_topsoil_basin_cz.gpkg", overwrite = F)

####################################################################
########### END: Pesticide Runoff Model Schriever 2007 #############
####################################################################

#########################################################################
########### START: Pesticide concentration in surface water #############
#########################################################################
# Calculate the mean, min and max concentration of the active substance in the river segment
# Intersect stream buffer with fields
# Aggregate the data to the river segment level and then to the basin level

# River buffers

farm_rivers_buff_bas_cz <- list()
rivers_buff_cz <- list()

# --- Start of Progress Bar Integration ---

# Initialize the progress bar
# min: starting value
# max: ending value (total number of iterations)
# style: 3 gives a percentage and elapsed time
pb <- utils::txtProgressBar(min = 0, max = basins_cz |> length(), style = 3)

# --- End of Progress Bar Integration ---

for(basin_idx in seq_along(basins_cz)) {
  current_hybas_id <- basins_cz[basin_idx, 1] |> values() |> as.vector()

  current_rivers_buff <- rivers_cz |>
    filter(HYBAS_ID == current_hybas_id) |>
    buffer(100)

  current_farm_rivers_buff <- terra::intersect(
    pest_twc_as_farm_cz[c("Crop", "acsubst_name", "srunoff_load", "dis_m3_pyr")],
    current_rivers_buff
  )
  
  if (nrow(current_farm_rivers_buff) == 0) {
  
    message("Warning: Basin ID ", as.character(current_hybas_id), " (Basin nr ", basin_idx, " out of ", basins_cz |> length(), ") has no fields intersecting the buffer. Skipping.")
    next
  }
  
  rivers_buff_cz[[basin_idx]] <- current_rivers_buff
  farm_rivers_buff_bas_cz[[basin_idx]] <- current_farm_rivers_buff

# --- Progress Bar Update ---
# Update the progress bar to the current iteration number
  setTxtProgressBar(pb, basin_idx)
# --- End of Progress Bar Update ---

}

# --- Close Progress Bar ---
# Close the progress bar when the loop is complete
close(pb)
# 
cat("\nProcessing complete.\n")
# 
writeVector(farm_rivers_buff_bas_cz |> svc() |> vect(), "water_network_farmbuff100_12_CZ.gpkg")

# Import intersected river network and parcel data 
rivers_farm_buff_cz <- dir_ls(path_home_r(), recurse = T, regexp = "water_network_farmbuff100_12_CZ.gpkg") |> 
  vect()

# Sum total length of stream for each river basin
river_length_tot_bas_cz <- rivers_cz |> 
  values() |> 
  group_by(HYBAS_ID) |> 
  summarise(length_tot_bas_m = sum(SHAPE_Leng))

# Calculate concentration and weighted concentrations in all individual streams. Split dataset for each pesticide
conc_acsubst_river_seg_cz <- rivers_farm_buff_cz |> 
  terra::merge(river_length_tot_bas_cz, by = "HYBAS_ID") |> 
  select(-HYBAS_ID) |> 
  terra::intersect(basins_cz["HYBAS_ID"]) |> 
  mutate(segment_weight = SHAPE_Leng/length_tot_bas_m) |> 
  mutate(conc_river_seg_ug.dm3 = srunoff_load/(dis_m3_pyr*1000),
         conc_river_seg_w_ug.dm3 = conc_river_seg_ug.dm3*segment_weight) |>
  terra::split("acsubst_name")

# Aggregate pesticide concentration in streams for each crop and river basin
conc_rivseg_agg_cz <- svc(aggregate(conc_acsubst_river_seg_cz[[1]][c("Crop",  "HYBAS_ID" , "conc_river_seg_ug.dm3")],
                                 c("HYBAS_ID", "Crop"),
                                 fun = "sum"),
                       aggregate(conc_acsubst_river_seg_cz[[2]][c("Crop",  "HYBAS_ID" ,  "conc_river_seg_ug.dm3")],
                                 c("HYBAS_ID", "Crop"),
                                 fun = "sum"),
                       aggregate(conc_acsubst_river_seg_cz[[3]][c("Crop",  "HYBAS_ID" , "conc_river_seg_ug.dm3")],
                                 c("HYBAS_ID", "Crop"),
                                 fun = "sum"))

# Rasterise aggregated concentration values for each chemical
conc_rivseg_agg_rast_cz <- sprc(rasterize(project(conc_rivseg_agg_cz[[1]],
                                               crs("EPSG:32633")),
                                       project(budens_jrc_cz, crs("EPSG:32633")),
                                       field = "sum_conc_river_seg_ug.dm3",
                                       touches = T) |> 
                               zonal(project(conc_rivseg_agg_cz[[1]],
                                             crs("EPSG:32633")),
                                     fun = "median",
                                     touches = T,
                                     as.raster = T),
                             rasterize(project(conc_rivseg_agg_cz[[2]],
                                               crs("EPSG:32633")),
                                       project(budens_jrc_cz, crs("EPSG:32633")),
                                       field = "sum_conc_river_seg_ug.dm3",
                                       touches = T) |> 
                               zonal(project(conc_rivseg_agg_cz[[2]],
                                             crs("EPSG:32633")), 
                                     fun = "median",
                                     touches = T,
                                     as.raster = T),
                             rasterize(project(conc_rivseg_agg_cz[[3]],
                                               crs("EPSG:32633")),
                                       project(budens_jrc_cz, crs("EPSG:32633")),
                                       field = "sum_conc_river_seg_ug.dm3",
                                       touches = T) |> 
                               zonal(project(conc_rivseg_agg_cz[[3]],
                                             crs("EPSG:32633")),
                                     fun = "median",
                                     touches = T,
                                     as.raster = T)) 

# Create vector layers of aggregated pesticide concentration in streams for each respective basin level
conc_wmean_river_basin_cz <- list()
lenght_river_buffer_basin_cz <- list()
nr_river_buffer_basin_cz <- list()
conc_rivseg_agg_vect_cz <- list()

for(i in seq_along(conc_acsubst_river_seg_cz)){
  
  cat("\r", unique(values(conc_acsubst_river_seg_cz[[i]]["acsubst_name"]))[1,1],
      "is being processed out of",
      seq_along(conc_acsubst_river_seg_cz) |> max(), 
      "ASs.",
      max(seq_along(conc_acsubst_river_seg_cz)) - i,
      "ASs left.")
  
  conc_wmean_river_basin_cz[[i]] <- zonal(conc_acsubst_river_seg_cz[[i]][,"conc_river_seg_w_ug.dm3"],
                                          basins_cz[c("HYBAS_ID")],
                                          fun = "sum",
                                          as.polygons = T)
  
  lenght_river_buffer_basin_cz[[i]] <- zonal(conc_acsubst_river_seg_cz[[i]]["SHAPE_Leng"],
                                             basins_cz[c("HYBAS_ID")],
                                             fun = "sum",
                                             as.polygons = T) 
  
  nr_river_buffer_basin_cz[[i]] <- conc_acsubst_river_seg_cz[[i]] |>
    values() |> 
    group_by(HYBAS_ID) |>
    summarise(stream_seg_count = n())
  
  conc_rivseg_agg_vect_cz[[i]] <- merge(conc_wmean_river_basin_cz[[i]], lenght_river_buffer_basin_cz[[i]] |>
                                          as.data.table(), by = "HYBAS_ID") |> 
    left_join(nr_river_buffer_basin_cz[[i]], by = "HYBAS_ID") |>
    # terra::na.omit("HYBAS_ID") |> 
    mutate("Active substance" = unique(values(conc_acsubst_river_seg_cz[[i]]["acsubst_name"]))[1,1],
           length_km = round(SHAPE_Leng/1000, 2),
           conc_river_seg_w_ug.dm3 = conc_river_seg_w_ug.dm3*1000) |> 
    tidyterra::rename("Basin ID" = HYBAS_ID,
                      "Concentration [\u00B5g\u00D7dm\u207B\u00B3]" = conc_river_seg_w_ug.dm3,
                      "Total stream length receiving pesticide loadings [km]" = length_km,
                      "Number of streams receiving pesticide loadings" = stream_seg_count)
}

# Save raster and vector data files
writeCDF(conc_rivseg_agg_rast_cz[1], "Acetamiprid_pec365_swater_seg_cz.nc")
writeCDF(conc_rivseg_agg_rast_cz[2], "Glyphosate_pec365_swater_seg_cz.nc")
writeCDF(conc_rivseg_agg_rast_cz[3], "Tebuconazole_pec365_swater_seg_cz.nc")
writeVector(vect(conc_rivseg_agg_vect_cz), "3chem_pec365_swater_seg_basin_cz.gpkg", overwrite = F)

#########################################################################
########### END: Pesticide concentration in surface water ###############
#########################################################################

##########################################################
########### START: Pesticide topsoil Map ##################
##########################################################

# PEC soil maps
tmap_mode("view")

pec_field_to_basin_cz <- function(pest_twc_as_farm_cz_rast, all_as_basin_cz) {
  
  basemap <- tm_scalebar(position = c("right", "bottom")) +
  tm_title(paste0(all_as_basin_cz$Active.substance |> unique(),
                  " 365-day PEC topsoil in Czech Republic")) +
    tm_basemap(providers$Esri.WorldTopoMap, group.control = "check", alpha = NULL)
    
  
  pec_soil_cz <- tm_shape(pest_twc_as_farm_cz_rast,
             name =paste0("PEC topsoil ",all_as_basin_cz$Active.substance |> unique())) +
    tm_raster("conc_acsubst_total_soil_365twa_ug.kg",
              col.scale = tm_scale_intervals(label.na = "Missing values",
                                             values = "brewer.bu_pu"),
              col.legend = tm_legend(title = paste0("Avg (twa) ", all_as_basin_cz$Active.substance |> unique(),
                                                    " [\u00B5g\u00D7kg\u207B\u00B9]"),
                                     bg.alpha = 1,
                                     reverse = T),
              col_alpha = 1,
              group = "Field level",
              group.control = "radio") +
    tm_shape(all_as_basin_cz |> mask(nuts_cz),
             name =paste0("PEC topsoil ",all_as_basin_cz$Active.substance |> unique())) +
    tm_polygons("Concentration.(TWA).[µg×kg⁻¹]",
                popup.vars = c("Concentration.(TWA).[µg×kg⁻¹]",
                               "Number.of.parcels",
                               "Agricultural.area.[ha]",
                               "Basin.area.[ha]"),
                fill.scale = tm_scale_intervals(value.na = "#ffdeaf" ,
                                                label.na = "Missing values",
                                                values = "brewer.bu_pu"),
                fill.legend = tm_legend(title = paste0("Avg (twa) ",
                                                       all_as_basin_cz$Active.substance |> unique(),
                                                       " [\u00B5g\u00D7kg\u207B\u00B9]"),
                                        bg.alpha = 1,
                                        reverse = T),
                group = "River basin level",
                group.control = "radio",
                lwd = 0,
                fill_alpha = 1) +
    tm_shape(basins_cz[,1] |> mask(nuts_cz), name = "River basin borders") +
    tm_lines(col= "black",
             lwd = 0.75) +
    basemap +
    tm_view(control.collapse = F, alpha = NULL)
  
  tmap_save(pec_soil_cz, paste0(all_as_basin_cz$Active.substance |> unique(),"_pec365_topsoil_whole_CZ.html"))
  
}

pec_field_to_basin_cz(pest_twc_as_farm_cz_rast[1], all_as_basin_cz[[1]])
pec_field_to_basin_cz(pest_twc_as_farm_cz_rast[2], all_as_basin_cz[[2]])
pec_field_to_basin_cz(pest_twc_as_farm_cz_rast[3], all_as_basin_cz[[3]])

###########################################################
########### END: Pesticide topsoil map ####################
###########################################################

###################################################################
########### START: Pesticide surface water map ####################
##################################################################
# PEC surface water live maps

pec_streams_to_basin_cz <- function(conc_rivseg_agg_rast_cz, conc_rivseg_agg_vect_cz, conc_acsubst_river_seg_cz) {
  
  raster_values <- conc_rivseg_agg_rast_cz[!is.na(conc_rivseg_agg_rast_cz)]
  raster_breaks <- pretty(raster_values)
  if (min(raster_breaks) > min(raster_values)) raster_breaks <- c(min(raster_values), raster_breaks)
  if (max(raster_breaks) < max(raster_values)) raster_breaks <- c(raster_breaks, max(raster_values))
  color_palette_rast <- colorBin(palette = "BuPu",
                                 domain = range(raster_values, na.rm = T),
                                 bins = raster_breaks,
                                 na.color = NA)
  
  color_palette_rast_rev <- colorBin(palette = "BuPu",
                                     domain = range(raster_values, na.rm = T),
                                     bins = raster_breaks,
                                     reverse = T, 
                                     na.color = NA)
  
  vector_values <- conc_rivseg_agg_vect_cz[,2] |> pull()
  vector_breaks <- pretty(vector_values)
  # if (min(vector_breaks) > min(vector_values)) vector_breaks <- c(min(vector_values), vector_breaks)
  # if (max(vector_breaks) < max(vector_values)) vector_breaks <- c(vector_breaks, max(vector_values))
  color_palette_vect <- colorBin(palette = "BuPu",
                                 domain = vector_values,
                                 bins = vector_breaks,
                                 na.color = "#ffdeaf")
  
  color_palette_vect_rev <- colorBin(palette = "BuPu",
                                     domain = vector_values,
                                     bins = vector_breaks,
                                     reverse = T, 
                                     na.color = "#ffdeaf")
  
  pec_swater_cz <- leaflet() %>%
    addMapPane(name = "vect_bas",
               zIndex = 410) |> 
    addMapPane(name = "rast_conc",
               zIndex = 420) |> 
    addMapPane(name = "vect_conc",
               zIndex = 430) |> 
    addProviderTiles(providers$Esri.WorldTopoMap,
                     options = providerTileOptions(minZoom = 0, maxZoom = 18),
                     group = "Esri World Topo Map") %>%
    addPolygons(data = basins_cz,
                fillColor = "grey",
                fillOpacity = 0.15,
                stroke = T,
                color = "black",
                dashArray = "line",
                weight = 0.5,
                opacity = 1,
                options = pathOptions(pane = "vect_bas")) |> 
    addRasterImage(conc_rivseg_agg_rast_cz,
                   colors = color_palette_rast,
                   opacity = 1,
                   group = "Stream level",
                   options = pathOptions(pane = "rast_conc")) %>%
    addScaleBar(position = "bottomright") %>%
    addLegend(pal = color_palette_rast_rev,
              values = values(conc_rivseg_agg_rast_cz),
              title = paste0(conc_acsubst_river_seg_cz$acsubst_name |> unique(),
                             " [\u00B5g\u00D7dm\u207B\u00B3]\n at stream level"),
              position = "bottomright",
              group = "Stream level",
              labFormat = labelFormat(between = " to ",
                                      digits = 2,
                                      transform = function(x=conc_rivseg_agg_rast_cz) sort(x, decreasing = T))) %>%
    addPolygons(data = conc_rivseg_agg_vect_cz,
                fillColor = ~color_palette_vect(vector_values),
                fillOpacity = 0.95,
                stroke = T,
                dashArray = "line",
                color = "black",
                weight = 0.5,
                opacity = 1,
                group = "Basin level",
                options = pathOptions(pane = "vect_conc"),
                highlightOptions = highlightOptions(color = "black",
                                                    weight = 3,
                                                    bringToFront = TRUE),
                popup = paste0("<div>",
                               "Median concentration in streams receiving pesticides: ",
                               format(conc_rivseg_agg_vect_cz[,2] |>
                                        pull(),
                                      digits = 2,
                                      scientific = F),
                               " [\u00B5g\u00D7dm\u207B\u00B3]",
                               "<div>",
                               "<div>",
                               "# of streams receiving pesticides: ",
                               conc_rivseg_agg_vect_cz[,4] |>
                                 pull(),
                               "<div>",
                               "Total length of streams receiving pesticides: ",
                               conc_rivseg_agg_vect_cz[,6] |>
                                 pull(),
                               " [km]",
                               "<div>")) %>% 
    addScaleBar(position = "bottomright") %>%
    addLegend(pal = color_palette_vect_rev,
              values = values(conc_rivseg_agg_vect_cz[,2]),
              title = paste0(conc_acsubst_river_seg_cz$acsubst_name |> unique(),
                             " [\u00B5g\u00D7dm\u207B\u00B3]\nat basin level"),
              position = "bottomright",
              group = 'Basin level',
              labFormat = labelFormat(between = " to ",
                                      digits = 5,
                                      transform = function(x=conc_rivseg_agg_vect_cz[,2] |> values()) sort(x, decreasing = T))) %>%
    addLayersControl(overlayGroups = c("Stream level", "Basin level"),
                     baseGroups = c("Esri World Topo Map"),
                     options = layersControlOptions(collapsed = F, autoZIndex = T)) %>%
    hideGroup("Stream level") |> 
    addControl(html = paste0("<b>", conc_acsubst_river_seg_cz$acsubst_name |> unique(), " annual PEC surface water in Czech Republic following 1x application in July", "</b>"),
               position = "topleft")
  
  pec_swater_cz |> saveWidget(paste0(conc_acsubst_river_seg_cz$acsubst_name |> unique(), "_pec365_swater_whole_CZ.html"))
  
}

pec_streams_to_basin_cz(conc_rivseg_agg_rast_cz[3], conc_rivseg_agg_vect_cz[[3]], conc_acsubst_river_seg_cz[[3]])

#################################################################
########### END: Pesticide surface water map ####################
################################################################

# Input maps preparation

# basins_eu <- dir_ls(path_home_r(), recurse = T, regexp = "BasinATLAS_v10_lev09.shp$") |> vect()
# basins_cz <- mask(basins_eu, ext(nuts_cz))
# writeVector(basins_cz, "hydrosheds_lvl09_basins_cz.gpkg")
# basins_eu <- dir_ls(path_home_r(), recurse = T, regexp = "hydrosheds_lvl12_basins.gpkg$") |> vect()
# rivers_eu <- dir_ls(path_home_r(), recurse = T, regexp = "hydrosheds_rivers_eu.gpkg") |> vect()
# SAVE eu hydrosheds data as as .gpkg (only once)
# writeVector(basins_eu, "hydrosheds_lvl12_basins.gpkg")
# writeVector(rivers_eu, "hydrosheds_rivers_eu.gpkg")
# basins_cz <- mask(basins_eu, ext(nuts_cz))
# rivers_cz <- mask(rivers_eu_path, ext(nuts_cz))
# Save country hydroshed data as .gpkg (only once)
# writeVector(basins_cz, "hydrosheds_lvl12_basins_cz.gpkg", overwrite = T)
# writeVector(rivers_cz, "hydrosheds_rivers_cz.gpkg")