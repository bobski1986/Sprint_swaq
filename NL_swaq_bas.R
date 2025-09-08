pkg <- c("tidyverse", "fs", "readxl", "terra", "tmap", "OpenStreetMap", "maptiles", "tidyterra", "sf", "data.table", "fuzzyjoin", "openxlsx",
         "EnvStats", "leaflet", "htmltools", "htmlwidgets")

for (i in pkg) {
  if (!requireNamespace(i, quietly = TRUE)) { 
    install.packages(i)
  }
}

lapply(pkg, library, character.only = T)

# Data import for the Netherlands
# NUTS regions
nuts <- dir_ls(path_home_r(), recurse = T, regexp = "/NUTS_RG_20M_2024_4326.shp$")  |>
  vect()

nuts3_nl <- nuts |>
  filter(CNTR_CODE == "NL" & LEVL_CODE == 3)

nuts1_nl <- nuts |>
  filter(CNTR_CODE == "NL" & LEVL_CODE == 1) 

district_nl_select <- nuts3_nl |> select(NUTS_NAME)

# Define the approximate bounding box for the Netherlands in EPSG:32631 coordinates.
# crs_utm31n_nl <- "EPSG:32631"

# xmin <- 525976.598198582  # Approximate western boundary (easting)
# xmax <- 781549.306160638  # Approximate eastern boundary (easting)
# ymin <- 5627507.94888309 # Approximate southern boundary (northing)
# ymax <- 5929938.95158156 # Approximate northern boundary (northing)

# Define the desired pixel resolution (size of each cell). 
# The size of the pixel is defined based on the smallest size of the vector to be rasterised, e.g., 10m buffer polygons around river segments
# res_x <- 10 # 10 meters in the X (easting) direction
# res_y <- 10 # 10 meters in the Y (northing) direction

# # Calculate the number of columns and rows needed for the raster grid.
# ncols <- ceiling((xmax - xmin) / res_x)
# nrows <- ceiling((ymax - ymin) / res_y)

# Create an empty raster. This is a template for rasterisation of river network.
# empty_raster_nl <- rast(
#   ncols = ncols,        # Number of columns
#   nrows = nrows,        # Number of rows
#   xmin = xmin,          # Minimum X coordinate of the extent
#   xmax = xmin + ncols * res_x, # Maximum X coordinate, precisely calculated from xmin and ncols
#   ymin = ymin,          # Minimum Y coordinate of the extent
#   ymax = ymin + nrows * res_y, # Maximum Y coordinate, precisely calculated from ymin and nrows
#   crs = crs_utm31n,     # Coordinate Reference System (EPSG:32631)
#   resolution = c(res_x, res_y) # Resolution in X and Y directions
# )

# Read soil density map
budens_jrc_nl_path <- dir_ls(path_home_r(),
                             recurse = T,
                             regexp = "budens_jrc_nl.nc$")

budens_jrc_nl <- rast(budens_jrc_nl_path)

# Read dutch river network data from the country geoportal
rivers_nl <- dir_ls(path_home_r(), recurse = T, regexp = "water_network_12_NL.gpkg") |> 
  vect()

# Dutch river basins including data on soil, basin and terrain characteristics
basins_nl <- dir_ls(path_home_r(), recurse = T, regexp = "hydrosheds_lvl12_basins_nl.gpkg") |> 
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
  mask(nuts3_nl)

budens_jrc_nl_bas <- zonal(budens_jrc_nl, basins_nl, "mean", as.polygons = T, na.rm = T) |> 
  select(HYBAS_ID, budens_jrc_NL)

basins_nl <-  basins_nl |> 
  left_join(budens_jrc_nl_bas |>
              values(),
            by = "HYBAS_ID") |> 
  tidyterra::mutate(slp_perc_uav = tan((slp_dg_uav/10)/180*pi),
                    vol_soil_m3_ha = 10000 * 0.05,
                    mass_soil_t3_ha = vol_soil_m3_ha * budens_jrc_NL,
                    oc_perc = (soc_th_uav/mass_soil_t3_ha) * 100,
                    area_basins_nl_ha = basins_nl |>
                      expanse("ha"))

basins_nuts_nl <- basins_nl |>  terra::intersect(nuts3_nl[c("NUTS_ID", "NAME_LATN", "NUTS_NAME")])

# Add crop specific spray drift values to the "gemap"
spray_drift <- tibble(
  sdrift_dist_m =  c(1, 1, 1, 1,
                     3, 3, 3, 3,
                     5, 5, 5, 5,
                     10, 10, 10, 10,
                     15, 15, 15, 15,
                     20, 20, 20, 20,
                     30, 30, 30, 30,
                     50, 50, 50, 50,
                     75, 75, 75, 75,
                     100,100, 100,100),
  sdrift_90lnorm = c(2.45, NA, NA, NA,
                     NA, 7.21, 29.90, 19.54,
                     0.59, 4.07, 26.11, 12.32,
                     0.32, 1.48, 12.94, 6.23, 
                     0.13, 0.63, 6.67, 3.72,
                     0.16, 0.33, 4.53, 2.16,
                     0.11, 0.03, 2.48, 0.76,
                     0.10, 0.07, 0.32, 0.18, 
                     0.03, NA, NA, NA,
                     0.02, NA, NA, NA),
  Crop_group_sdrift = c("Arable", "Vines", "Orchards", "Hops",
                        "Arable", "Vines", "Orchards", "Hops",
                        "Arable", "Vines", "Orchards", "Hops",
                        "Arable", "Vines", "Orchards", "Hops",
                        "Arable", "Vines", "Orchards", "Hops",
                        "Arable", "Vines", "Orchards", "Hops",
                        "Arable", "Vines", "Orchards", "Hops",
                        "Arable", "Vines", "Orchards", "Hops",
                        "Arable", "Vines", "Orchards", "Hops",
                        "Arable", "Vines", "Orchards", "Hops")
) |> filter(sdrift_dist_m == 100)

# Read dutch gemap
active <- c("Glyphosate" , "Tebuconazole" , "Acetamiprid")

gemap_nl <- dir_ls(path_home_r(), recurse = T, regexp = "gemap_allchem_nl_wgs84") |> 
  vect() |>
  filter(EU_name %in% active) |> 
  makeValid()

acsubst_name <- gemap_nl["EU_name"] |>
  # mutate(EU_name = str_to_title(EU_name)) |>
  values() |>
  unique() |>
  pull()

# Chemical input data from qsars (vega, epi) and PPDB where available #
source(dir_ls(path_home_r(), recurse = T, regexp = "ppdb scraping"))
chemprop <- chemprop_gen(acsubst_name) |> 
  select(acsubst_name, Kfoc_ml.g, DT50_field_d, DT50_typical_d, Koc_ml.g) 
  # filter(!is.na(Kfoc_ml.g),
  #        !is.na(Koc_ml.g),
  #        !is.na(DT50_field_d),
  #        !is.na(DT50_typical_d))

# Intersect dutch gemap with river basins
gemap_nl_bas <- terra::intersect(gemap_nl, basins_nl["HYBAS_ID"])

# Get the number and are of all farms, grouped by crop and AS, in each river basin
mass_as_farms_bas_nl <- terra::aggregate(gemap_nl_bas[c("HYBAS_ID","EU_name", "EC_trans_n", "gewascode", "Dose_kg_ha")],
                                         c("HYBAS_ID", "EC_trans_n", "gewascode", "EU_name"),
                                         fun = "sum")

area_as_farms_bas_nl <- mass_as_farms_bas_nl |> expanse("ha") 

gemap_nl_bas_if <- gemap_nl_bas[c("HYBAS_ID",
                                  "EC_trans_n",
                                  "inter_frac_bbch.min",
                                  "inter_frac_bbch.max",
                                  "inter_frac_bbch.ave")] |>
  terra::aggregate(c("HYBAS_ID", "EC_trans_n"),
                   fun = "mean")

mass_area_as_farms_bas_nl <- cbind(mass_as_farms_bas_nl, area_as_farms_bas_nl |> data.frame()) |>
  terra::merge(basins_nl, by = "HYBAS_ID") |> 
  terra::merge(gemap_nl_bas_if[c("HYBAS_ID",
                                 "EC_trans_n",
                                 "mean_inter_frac_bbch.min",
                                 "mean_inter_frac_bbch.max",
                                 "mean_inter_frac_bbch.ave")],
               by = c("HYBAS_ID", "EC_trans_n")) |> 
  tidyterra::rename(area_as_farms_bas_nl_ha = area_as_farms_bas_nl,
                    SUB_AREA_km2 = SUB_AREA,
                    nr_farms_as_crop = agg_n) |>
  tidyterra::mutate(area_as_farms_bas_frac = area_as_farms_bas_nl_ha / area_basins_nl_ha,
                    AppRate_g_ha_corr = (sum_Dose_kg_ha/area_as_farms_bas_nl) * area_as_farms_bas_frac) |> 
  select("HYBAS_ID", "EC_trans_n", "gewascode", "EU_name",
         "sum_Dose_kg_ha", "slp_dg_uav", "cly_pc_uav", "snd_pc_uav",
         "oc_perc", "budens_jrc_NL", "slp_perc_uav",
         "area_basins_nl_ha", "mean_inter_frac_bbch.ave", "mean_inter_frac_bbch.min",
         "mean_inter_frac_bbch.max", "area_as_farms_bas_frac", "AppRate_g_ha_corr",
         "dis_m3_pyr", "run_mm_syr", "pre_mm_syr", "area_as_farms_bas_nl_ha", "nr_farms_as_crop")

####################################################################
########### START: Pesticide Runoff Model Schriever 2007 ###########
####################################################################

# Create new sample for each uncertain input parameter. There are 3 groups of parameters: crop, catchment (soil density, clay, sand, terrain slope etc.), and chemical specific

# IF_distr_par <- mass_area_as_farms_bas_nl |> 
#   values() |>
#   select(EC_trans_n,mean_inter_frac_bbch.ave, mean_inter_frac_bbch.min, mean_inter_frac_bbch.max) |> 
#   distinct(EC_trans_n, .keep_all = T) |> 
#   filter(mean_inter_frac_bbch.ave != 0.9)
# 
# IF_sample <- list()
# 
# for(i in 1:nrow(IF_distr_par)) {
#   
#   IF_sample[[i]] <- rtri(1000,
#                 min = IF_distr_par[i , ]$mean_inter_frac_bbch.min,
#                 max = IF_distr_par[i , ]$mean_inter_frac_bbch.max,
#                 mode = IF_distr_par[i , ]$mean_inter_frac_bbch.ave)
# }
# 
# IF_list <- tibble(IF_sample) |>
#   bind_cols(tibble(Crop = IF_distr_par[ , 1])) |>
#   unnest(cols = c(IF_sample, Crop)) |>
#   nest(.by = Crop)
# 
# IF_extracted <- IF_list |> 
#   pull(data) |> 
#   list_rbind() |> 
# do.call(rbind, .) %>%
# as.data.frame()

# colnames(IF_extracted) <- paste0("IF_sample_", 1:ncol(IF_extracted))

# IF_sample_all <- IF_list |> 
#   select(Crop) |> 
#   bind_cols(IF_extracted)

# Insert sampled data into main dataset

# Run model for new input

# TWA concentration in soil
pest_twc_as_farm_nl <- mass_area_as_farms_bas_nl |>
  rename(acsubst_name = EU_name,
         IFmin = mean_inter_frac_bbch.min,
         IFmax = mean_inter_frac_bbch.max,
         IFav = mean_inter_frac_bbch.ave,
         aprate_farm_kg.ha = sum_Dose_kg_ha,
         bulk_dens_kg.dm3 = budens_jrc_NL,
         slope_perc = slp_perc_uav) |> 
  mutate(Crop_group_sdrift = case_when(gewascode == 1014 ~ "Vines",
                                       gewascode == 1099 ~ "Vines",
                                       gewascode == 1091 ~ "Orchards",
                                       gewascode == 1092 ~ "Orchards",
                                       gewascode == 1093 ~ "Orchards",
                                       gewascode == 1077 ~ "Orchards",
                                       gewascode == 1078 ~ "Orchards",
                                       gewascode == 1079 ~ "Orchards",
                                       gewascode == 1874 ~ "Orchards",
                                       gewascode == 375 ~ "Hops",
                                       .default = "Arable")) |> 
  left_join(spray_drift, by = "Crop_group_sdrift") |> 
  terra::merge(chemprop, by = "acsubst_name") |>
  ## AS spray drift loading
  mutate(sdrift_load = map2_dbl(aprate_farm_kg.ha,
                                sdrift_90lnorm,
                                ~.x * (.y/100))) |>
  ## Application rate correction
  mutate(aprate_farm_kg.ha = map2_dbl(aprate_farm_kg.ha,
                                      sdrift_load,
                                      ~.x-.y)) |> 
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
                                                \(x,y,z,v) (x/y*5)*(z+v))) |> 
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
  mutate(srunoff_load = pmap_dbl(list(area_as_farms_bas_nl_ha,
                                      aprate_farm_kg.ha,
                                      infactor_effect,
                                      frac_asubst_soil_water_lag,
                                      slope_effect,
                                      srunoff_frac),
                                 prod)) |>
  makeValid()

# Split simulated concentration by substance 
pest_twc_as_basin_nl <- pest_twc_as_farm_nl[c("HYBAS_ID",
                                              "EC_trans_n",
                                              "acsubst_name",
                                              "nr_farms_as_crop",
                                              "area_as_farms_bas_nl_ha",
                                              "conc_acsubst_total_soil_ini",
                                              "conc_acsubst_total_soil_56twa_ug.kg",
                                              "conc_acsubst_total_soil_365twa_ug.kg")] |>
  tidyterra::mutate(HYBAS_ID = as.character(HYBAS_ID)) |> 
  terra::split("acsubst_name")

# Rasterise individual parcel polygons to show AS concentration distribution in individual basins
pest_twc_as_farm_nl_rast <- sprc(rasterize(project(pest_twc_as_basin_nl[[1]] |>
                                                     select("conc_acsubst_total_soil_365twa_ug.kg"),
                                                   crs("EPSG:32631")),
                                           project(budens_jrc_nl, crs("EPSG:32631")),
                                           field = "conc_acsubst_total_soil_365twa_ug.kg",
                                           touches = T) |> 
                                   disagg(4) |> 
                                   zonal(project(pest_twc_as_basin_nl[[1]], crs("EPSG:32631")), fun = "median", touches = T, as.raster = T),
                                 
                                 rasterize(project(pest_twc_as_basin_nl[[2]] |>
                                                     select("conc_acsubst_total_soil_365twa_ug.kg"),
                                                   crs("EPSG:32631")),
                                           project(budens_jrc_nl, crs("EPSG:32631")),
                                           field = "conc_acsubst_total_soil_365twa_ug.kg",
                                           touches = T) |>
                                   disagg(4) |> 
                                   zonal(project(pest_twc_as_basin_nl[[2]], crs("EPSG:32631")), fun = "median", touches = T, as.raster = T),
                                 
                                 rasterize(project(pest_twc_as_basin_nl[[3]] |>
                                                     select("conc_acsubst_total_soil_365twa_ug.kg"),
                                                   crs("EPSG:32631")),
                                           project(budens_jrc_nl, crs("EPSG:32631")),
                                           field = "conc_acsubst_total_soil_365twa_ug.kg",
                                           touches = T) |>
                                   disagg(4) |> 
                                   zonal(project(pest_twc_as_basin_nl[[3]], crs("EPSG:32631")) ,fun = "median", touches = T, as.raster = T))

# Create vector layers of aggregated pesticide concentration in topsoil to the respective basin level
conc_wmean_farm_basin_nl <- list()
area_farm_basin_nl <- list()
nr_farm_basin_nl <- list()
all_as_farm_basin_nl <- list()

for(i in seq_along(pest_twc_as_basin_nl)){
  
  cat("\r", unique(values(pest_twc_as_basin_nl[[i]]["acsubst_name"]))[1,1],
      "is being processed out of",
      seq_along(pest_twc_as_basin_nl) |> max(), 
      "ASs.",
      max(seq_along(pest_twc_as_basin_nl)) - i,
      "ASs left.")
  
  conc_wmean_farm_basin_nl[[i]] <- zonal(pest_twc_as_basin_nl[[i]][,"conc_acsubst_total_soil_365twa_ug.kg"],
                                         basins_nl[c("HYBAS_ID")],
                                         weighted = T,
                                         as.polygons = T)
  
  area_farm_basin_nl[[i]] <- zonal(pest_twc_as_basin_nl[[i]][,"area_as_farms_bas_nl_ha"],
                                   basins_nl[c("HYBAS_ID", "SUB_AREA_ha")],
                                   fun = "sum",
                                   as.polygons = T) 
  
  nr_farm_basin_nl[[i]] <- zonal(pest_twc_as_basin_nl[[i]][,"nr_farms_as_crop"],
                                 basins_nl[c("HYBAS_ID")],
                                 fun = "sum",
                                 as.polygons = T)
  
  all_as_farm_basin_nl[[i]] <- merge(conc_wmean_farm_basin_nl[[i]], area_farm_basin_nl[[i]] |> as.data.table(), by = "HYBAS_ID") |> 
    merge(nr_farm_basin_nl[[i]] |> as.data.table(), by = "HYBAS_ID") |>
    # terra::na.omit("HYBAS_ID") |> 
    mutate("Active substance" = unique(values(pest_twc_as_basin_nl[[i]]["acsubst_name"]))[1,1],
           area_as_farms_bas_nl_ha = round(area_as_farms_bas_nl_ha, 1),
           conc_acsubst_total_soil_365twa_ug.kg = round(conc_acsubst_total_soil_365twa_ug.kg, 1),
           HYBAS_ID = as.character(HYBAS_ID)) |> 
    tidyterra::rename("Basin ID" = HYBAS_ID,
                      "Basin area [ha]" = SUB_AREA_ha,
                      "Concentration (TWA) [\u00B5g\u00D7kg\u207B\u00B9]"  = conc_acsubst_total_soil_365twa_ug.kg,
                      "Number of parcels" = nr_farms_as_crop,
                      "Agricultural area [ha]" = area_as_farms_bas_nl_ha)
}

# Save raster and vector data files
writeCDF(pest_twc_as_farm_nl_rast[1], "Acetamiprid_pec365_topsoil_farm_nl.nc")
writeCDF(pest_twc_as_farm_nl_rast[2], "Glyphosate_pec365_topsoil_farm_nl.nc")
writeCDF(pest_twc_as_farm_nl_rast[3], "Tebuconazole_pec365_topsoil_farm_nl.nc")
writeVector(vect(all_as_basin_nl), "3chem_pec365_topsoil_basin_nl.gpkg", overwrite = F)

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

# farm_rivers_buff_bas_nl <- list()
# rivers_buff_nl <- list()

# --- Start of Progress Bar Integration ---

# Initialize the progress bar
# min: starting value
# max: ending value (total number of iterations)
# style: 3 gives a percentage and elapsed time
# pb <- utils::txtProgressBar(min = 0, max = basins_nl |> length(), style = 3)

# --- End of Progress Bar Integration ---

# for(basin_idx in seq_along(basins_nl)) {
#   current_hybas_id <- basins_nl[basin_idx, 1] |> values() |> as.vector()
# 
#   current_rivers_buff <- rivers_nl |>
#     filter(HYBAS_ID == current_hybas_id) |>
#     buffer(100)
# 
#   current_farm_rivers_buff <- terra::intersect(
#     pest_twc_as_farm_nl[c("EC_trans_n", "acsubst_name", "srunoff_load", "dis_m3_pyr", "sdrift_dist_m")],
#     current_rivers_buff
#   )
# 
#   if (nrow(current_farm_rivers_buff) == 0) {
# 
#     message("Warning: Basin ID ", as.character(current_hybas_id), " (Basin nr ", basin_idx, " out of ", basins_nl |> length(), ") has no fields intersecting the buffer. Skipping.")
#     next
#   }
# 
#   rivers_buff_nl[[basin_idx]] <- current_rivers_buff
#   farm_rivers_buff_bas_nl[[basin_idx]] <- current_farm_rivers_buff

# --- Progress Bar Update ---
# Update the progress bar to the current iteration number
# setTxtProgressBar(pb, basin_idx)
# --- End of Progress Bar Update ---

# }

# --- Close Progress Bar ---
# Close the progress bar when the loop is complete
# close(pb)
# 
# cat("\nProcessing complete.\n")
# 
# writeVector(farm_rivers_buff_bas_nl |> svc() |> vect(), "water_network_farmbuff100_12_NL.gpkg")


# Import intersected river network and parcel data 
rivers_farm_buff_nl <- dir_ls(path_home_r(), recurse = T, regexp = "water_network_farmbuff100_12_NL.gpkg") |> 
  vect()

# Sum total length of stream for each river basin
river_length_tot_bas <- rivers_nl |> 
  values() |> 
  group_by(HYBAS_ID) |> 
  summarise(length_tot_bas_m = sum(length_m))

# Calculate concentration and weighted concentrations in all individual streams. Split dataset for each pesticide
conc_acsubst_river_seg_nl <- rivers_farm_buff_nl |> 
  terra::merge(river_length_tot_bas, by = "HYBAS_ID") |> 
  select(-HYBAS_ID) |> 
  terra::intersect(basins_nl["HYBAS_ID"]) |> 
  mutate(segment_weight = length_m/length_tot_bas_m) |> 
  mutate(conc_river_seg_ug.dm3 = srunoff_load/(dis_m3_pyr*1000),
         conc_river_seg_w_ug.dm3 = conc_river_seg_ug.dm3*segment_weight) |>
  terra::split("acsubst_name")

# Aggregate pesticide concentration in streams for each crop and river basin
conc_rivseg_agg_nl <- svc(aggregate(conc_acsubst_river_seg_nl[[1]][c("EC_trans_n",  "HYBAS_ID" , "conc_river_seg_ug.dm3")],
                                    c("HYBAS_ID", "EC_trans_n"),
                                    fun = "sum"),
                          aggregate(conc_acsubst_river_seg_nl[[2]][c("EC_trans_n",  "HYBAS_ID" ,  "conc_river_seg_ug.dm3")],
                                    c("HYBAS_ID", "EC_trans_n"),
                                    fun = "sum"),
                          aggregate(conc_acsubst_river_seg_nl[[3]][c("EC_trans_n",  "HYBAS_ID" , "conc_river_seg_ug.dm3")],
                                    c("HYBAS_ID", "EC_trans_n"),
                                    fun = "sum"))

# Rasterise aggregated concentration values for each chemical
conc_rivseg_agg_rast_nl <- sprc(rasterize(project(conc_rivseg_agg_nl[[1]],
                                                  crs("EPSG:32631")),
                                          project(budens_jrc_nl, crs("EPSG:32631")),
                                          field = "sum_conc_river_seg_ug.dm3",
                                          touches = T) |> 
                                  zonal(project(conc_rivseg_agg_nl[[1]],
                                                crs("EPSG:32631")),
                                        fun = "median",
                                        touches = T,
                                        as.raster = T),
                                rasterize(project(conc_rivseg_agg_nl[[2]],
                                               crs("EPSG:32631")),
                                       project(budens_jrc_nl, crs("EPSG:32631")),
                                       field = "sum_conc_river_seg_ug.dm3",
                                       touches = T) |> 
                               zonal(project(conc_rivseg_agg_nl[[2]],
                                             crs("EPSG:32631")), 
                                     fun = "median",
                                     touches = T,
                                     as.raster = T),
                             rasterize(project(conc_rivseg_agg_nl[[3]],
                                               crs("EPSG:32631")),
                                       project(budens_jrc_nl, crs("EPSG:32631")),
                                       field = "sum_conc_river_seg_ug.dm3",
                                       touches = T) |> 
                               zonal(project(conc_rivseg_agg_nl[[3]],
                                             crs("EPSG:32631")),
                                     fun = "median",
                                     touches = T,
                                     as.raster = T)) 

# Create vector layers of aggregated pesticide concentration in streams for each respective basin level
conc_wmean_river_basin_nl <- list()
lenght_river_buffer_basin_nl <- list()
nr_river_buffer_basin_nl <- list()
conc_rivseg_agg_vect_nl <- list()

for(i in seq_along(conc_acsubst_river_seg_nl)){
  
  cat("\r", unique(values(conc_acsubst_river_seg_nl[[i]]["acsubst_name"]))[1,1],
      "is being processed out of",
      seq_along(conc_acsubst_river_seg_nl) |> max(), 
      "ASs.",
      max(seq_along(conc_acsubst_river_seg_nl)) - i,
      "ASs left.")
  
  conc_wmean_river_basin_nl[[i]] <- zonal(conc_acsubst_river_seg_nl[[i]][,"conc_river_seg_w_ug.dm3"],
                                    basins_nl[c("HYBAS_ID")],
                                    fun = "sum",
                                    as.polygons = T)
  
  lenght_river_buffer_basin_nl[[i]] <- zonal(conc_acsubst_river_seg_nl[[i]]["length_m"],
                                   basins_nl[c("HYBAS_ID")],
                                   fun = "sum",
                                   as.polygons = T) 
  
  nr_river_buffer_basin_nl[[i]] <- conc_acsubst_river_seg_nl[[i]] |>
    values() |> 
    group_by(HYBAS_ID) |>
    summarise(stream_seg_count = n())
  
  conc_rivseg_agg_vect_nl[[i]] <- merge(conc_wmean_river_basin_nl[[i]], lenght_river_buffer_basin_nl[[i]] |>
                                        as.data.table(), by = "HYBAS_ID") |> 
    left_join(nr_river_buffer_basin_nl[[i]], by = "HYBAS_ID") |>
    # terra::na.omit("HYBAS_ID") |> 
    mutate("Active substance" = unique(values(conc_acsubst_river_seg_nl[[i]]["acsubst_name"]))[1,1],
           length_km = round(length_m/1000, 2),
           conc_river_seg_w_ug.dm3 = conc_river_seg_w_ug.dm3*1000) |> 
    tidyterra::rename("Basin ID" = HYBAS_ID,
                      "Concentration [\u00B5g\u00D7dm\u207B\u00B3]" = conc_river_seg_w_ug.dm3,
                      "Total stream length receiving pesticide loadings [km]" = length_km,
                      "Number of streams receiving pesticide loadings" = stream_seg_count)
}

# Save raster and vector data files
writeCDF(conc_rivseg_agg_vect_nl[1], "Acetamiprid_pec365_swater_seg_nl.nc")
writeCDF(conc_rivseg_agg_vect_nl[2], "Glyphosate_pec365_swater_seg_nl.nc")
writeCDF(conc_rivseg_agg_vect_nl[3], "Tebuconazole_pec365_swater_seg_nl.nc")
writeVector(vect(conc_rivseg_agg_vect_nl), "3chem_pec365_swater_seg_basin_nl.gpkg", overwrite = F)

#########################################################################
########### END: Pesticide concentration in surface water ###############
#########################################################################

###########################################################
########### START: Pesticide topsoil map ##################
###########################################################

# PEC soil live maps
tmap_mode("view")

#PEC topsoil 
pec_field_to_basin_nl <- function(pest_twc_as_farm_nl_rast, all_as_basin_nl) {

  basemap <- tm_scalebar(position = c("right", "bottom")) +
    tm_basemap("Esri.WorldTopoMap", alpha = 0.5, group.control = "check") +
    tm_title(paste0(all_as_basin_nl$Active.substance |> unique(), 
                    " 365-day PEC topsoil in Netherlands"))
  
  pec_soil_nl <- basemap +
    tm_shape(pest_twc_as_farm_nl_rast,
             name =paste0("PEC topsoil ",all_as_basin_nl$Active.substance |> unique())) +
    tm_raster("conc_acsubst_total_soil_365twa_ug.kg",
              col.scale = tm_scale_intervals(label.na = "Missing values",
                                             values = "brewer.bu_pu"),
              col.legend = tm_legend(title = paste0(all_as_basin_nl$Active.substance |> unique(),
                                                    " [\u00B5g\u00D7kg\u207B\u00B9]"),
                                     bg.alpha = 1,
                                     reverse = T),
              col_alpha = 1,
              group = "Field level",
              group.control = "radio") +
    tm_shape(all_as_basin_nl |> mask(nuts_nl),
             name =paste0("PEC topsoil ",all_as_basin_nl$Active.substance |> unique())) +
    tm_polygons("Concentration.(TWA).[µg×kg⁻¹]",
                popup.vars = c("Concentration.(TWA).[µg×kg⁻¹]",
                               "Number.of.parcels",
                               "Agricultural.area.[ha]",
                               "Basin.area.[ha]",
                               "Basin.ID"),
                fill.scale = tm_scale_intervals(value.na = "#ffdeaf" ,
                                                label.na = "Missing values",
                                                values = "brewer.bu_pu"),
                fill.legend = tm_legend(title = paste0(all_as_basin_nl$Active.substance |> unique(),
                                                       " [\u00B5g\u00D7kg\u207B\u00B9]"),
                                        bg.alpha = 1,
                                        reverse = T),
                group = "River basin level",
                group.control = "radio",
                lwd = 0,
                fill_alpha = 1) +
    tm_shape(all_as_basin_nl[,1], name = "River basin borders") +
    tm_lines(col= "black",
             lwd = 0.75) +
    tm_view(control.collapse = F)

  tmap_save(pec_soil_nl, paste0(all_as_basin_nl$Active.substance |> unique(),"_pec365_topsoil_whole_NL.html"))
  
}

pec_field_to_basin_nl(pest_twc_as_farm_nl_rast[1], all_as_basin_nl[[1]])
pec_field_to_basin_nl(pest_twc_as_farm_nl_rast[2], all_as_basin_nl[[2]])
pec_field_to_basin_nl(pest_twc_as_farm_nl_rast[3], all_as_basin_nl[[3]])

# PEC soil static maps
tmap_mode("plot")

pecini_basin_nl <- dir_ls(path_home_r(), recurse = T, regexp = "3chem_pec365_topsoil_basin_nl.gpkg$") |> vect() |> filter(Active.substance == "Tebuconazole")


# basemap <- tm_shape(basins_nl) +
#   tm_borders(col = "black",
#              lty = "dashed",
#              lwd = 0.75) +
#   tm_basemap("OpenTopoMap" , alpha = 0.5) +
#   # tmap_options(component.autoscale = T) +

layout_map <- tm_layout(legend.position = c("left", "top"),
                        legend.frame = F,
                        legend.text.size = 0.75,
                        legend.title.size = .9, 
                        main.title.size = 1.5)

pecsoil_bas_nl <- layout_map +
  tm_shape(pecini_basin_nl) +
  tm_polygons("Concentration.(TWA).[µg×kg⁻¹]",
              fill.scale = tm_scale_intervals(value.na = "#ffdeaf" ,
                                              label.na = "Missing values",
                                             values = "brewer.bu_pu"),
              fill.legend = tm_legend(title = paste0("365-day concentration [\u00B5g\u00D7kg\u207B\u00B9]"),
                                     bg.alpha = 1,
                                     reverse = T)) +
  tm_title(paste0(pecini_basin_nl$Active.substance |> unique(),
                  " 365-day Predicted Environemental Concentration in 5cm agricultural topsoil \nAggregated for individual river catchments")) +
  tm_add_legend(type = "lines",
                labels = "River catchment borders",
                col = "black",
                lwd = 1.5) +
  tm_scalebar(position = c("right", "bottom"))


tmap_save(pecsoil_bas_nl, "teb_soil365_bas_nl.png", scale = .75, dpi = 300)

# Spatial input overview
tmap_mode("plot")
nl_soil_water_input <- tm_shape(basins_nl) +
  tm_polygons(fill = basins_nl |>
                select(-c(HYBAS_ID,
                          riv_tc_ssu,
                          slp_dg_uav,
                          dis_m3_pmn,
                          dis_m3_pmx)) |>
                names(),
              fill.legend = list(tm_legend(title = "")),
              fill_alpha = 0.85,
              fill.scale = list(tm_scale_continuous(values = "matplotlib.purples"),
                                tm_scale_continuous(values = "tableau.blue_teal"),
                                tm_scale_continuous(values = "matplotlib.blues"),
                                tm_scale_continuous(values = "matplotlib.yl_or_br"),
                                tm_scale_continuous(values = "matplotlib.yl_or_br"),
                                tm_scale_continuous(values = "matplotlib.yl_or_br"),
                                tm_scale_continuous(values = "matplotlib.terrain"))) +
  tm_basemap("OpenStreetMap", alpha = 0.5) +
  tm_layout(panel.labels = c("River discahrge [m3/yr] (average)",
                             "River volume [m3] (long term (1971-2000) average)",
                             "Annual precipitation [mm] (average)",
                             "Clay content [%]",
                             "Sand content [%]",
                             "Organic carbon stock [t/ha]",
                             "Terrain slope [%]")) +
  tm_facets_pagewise()

# tmap_animation(nl_soil_water_input, "nl.gif", fps = 0.25, width=1200, height = 1200, dpi = 200)
tmap_save(pec_soil_nl, "pec_soil_nl.html")

###########################################################
########### END: Pesticide topsoil map ####################
###########################################################

###################################################################
########### START: Pesticide surface water map ####################
###################################################################

conc_rivseg_agg_rast_nl_1 <- dir_ls(path = path_home_r(), regexp = "Acetamiprid_pec365_swater_seg_nl.nc", recurse = T) |> rast()
conc_rivseg_agg_rast_nl_2 <- dir_ls(path = path_home_r(), regexp = "Glyphosate_pec365_swater_seg_nl.nc", recurse = T) |> rast()
conc_rivseg_agg_rast_nl_3 <- dir_ls(path = path_home_r(), regexp = "Tebuconazole_pec365_swater_seg_nl.nc", recurse = T) |> rast()
conc_rivseg_agg_rast_nl <- sprc(conc_rivseg_agg_rast_nl_1, conc_rivseg_agg_rast_nl_1, conc_rivseg_agg_rast_nl_1)
conc_rivseg_agg_vect_nl <- dir_ls(path = path_home_r(), regexp = "3chem_pec365_swater_seg_basin_nl.gpkg", recurse = T) |> vect() |> split("Active.substance")

# PEC surface water live maps

pec_streams_to_basin_nl <- function(conc_rivseg_agg_rast_nl, conc_rivseg_agg_vect_nl) {
  
raster_values <- conc_rivseg_agg_rast_nl[!is.na(conc_rivseg_agg_rast_nl)]
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

vector_values <- conc_rivseg_agg_vect_nl[,2] |> pull()
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

pec_swater_nl <- leaflet(options = leafletOptions(zoomControl = F)) %>%
  addMapPane(name = "vect_bas",
             zIndex = 410) |> 
  addMapPane(name = "rast_conc",
             zIndex = 420) |> 
  addMapPane(name = "vect_conc",
             zIndex = 430) |> 
  addProviderTiles(providers$Esri.WorldTopoMap,
                   options = providerTileOptions(minZoom = 0, maxZoom = 18),
                   group = "Esri World Topo Map") %>%
  addPolygons(data = basins_nl,
              fillColor = "grey",
              fillOpacity = 0.15,
              stroke = T,
              color = "black",
              dashArray = "line",
              weight = 0.5,
              opacity = 1,
              options = pathOptions(pane = "vect_bas")) |> 
  addRasterImage(conc_rivseg_agg_rast_nl,
                 colors = color_palette_rast,
                 opacity = 1,
                 group = "Stream level",
                 options = pathOptions(pane = "rast_conc")) %>%
  addScaleBar(position = "bottomright") %>%
  addLegend(pal = color_palette_rast_rev,
            values = values(conc_rivseg_agg_rast_nl),
            title = paste0(conc_rivseg_agg_vect_nl$Active.substance |> unique(),
                           " [\u00B5g\u00D7dm\u207B\u00B3]\n at stream level"),
            position = "bottomright",
            group = "Stream level",
            labFormat = labelFormat(between = " to ",
                                    digits = 2,
                                    transform = function(x=conc_rivseg_agg_rast_nl) sort(x, decreasing = T))) %>%
  addPolygons(data = conc_rivseg_agg_vect_nl,
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
                             format(conc_rivseg_agg_vect_nl[,2] |>
                                    pull(),
                                    digits = 2,
                                    scientific = F),
                             " [\u00B5g\u00D7dm\u207B\u00B3]",
                             "<div>",
                             "<div>",
                             "# of streams receiving pesticides: ",
                             conc_rivseg_agg_vect_nl[,4] |>
                               pull(),
                             "<div>",
                             "Total length of streams receiving pesticides: ",
                             conc_rivseg_agg_vect_nl[,6] |>
                               pull(),
                             " [km]",
                             "<div>")) %>% 
  addScaleBar(position = "bottomright") %>%
  addLegend(pal = color_palette_vect_rev,
            values = values(conc_rivseg_agg_vect_nl[,2]),
            title = paste0(conc_rivseg_agg_vect_nl$Active.substance |> unique(),
                           " [\u00B5g\u00D7dm\u207B\u00B3]\nat basin level"),
            position = "bottomright",
            group = 'Basin level',
            labFormat = labelFormat(between = " to ",
                                    digits = 5,
                                    transform = function(x=conc_rivseg_agg_vect_nl[,2] |> values()) sort(x, decreasing = T))) %>%
  addLayersControl(overlayGroups = c("Stream level", "Basin level"),
    baseGroups = c("Esri World Topo Map"),
    options = layersControlOptions(collapsed = F, autoZIndex = T)) %>%
  hideGroup("Stream level") |> 
  addControl(html = paste0("<div style='background-color: rgba(255, 255, 255, 0.9);
                             padding: 6px 6px; border-radius: 4px; font-size: 14px; font-weight: bold; color: #333; max-width: 800px;
                             line-height: 1.4;'>",
                             conc_rivseg_agg_vect_nl$Active.substance |> unique(),
                             " annual PEC surface water in Netherlands following 1x application in July", 
                             "</div>"),
               position = "topleft")

pec_swater_nl |> saveWidget(paste0(conc_rivseg_agg_vect_nl$Active.substance |> unique(), "_pec365_swater_whole_NL.html"))
pec_swater_nl
}

pec_streams_to_basin_nl(conc_rivseg_agg_rast_nl[1], conc_rivseg_agg_vect_nl[[1]])


#################################################################
########### END: Pesticide surface water map ####################
#################################################################

######## Whole country AS usage for aggregated for each crop - AS combination ########

gemap_nl_bychem <- gemap_nl |> filter(EU_name %in% c("Glyphosate", "Tebuconazole", "Acetamiprid")) |> terra::split("EU_name")

mass_as_farms_nl <- list()
area_as_farms_nl<- list()
mass_area_as_farms_nl<- list()

for(chem in seq_along(gemap_nl_bychem)){
  
  mass_as_farms_nl[[chem]] <- terra::aggregate(gemap_nl_bychem[[chem]] |>
                                                 select("EC_trans_n",
                                                        "gewascode",
                                                        "EU_name",
                                                        "Dose_kg_ha") |>  makeValid(),
                                               c("EC_trans_n",
                                                 "gewascode",
                                                 "EU_name"),
                                               fun = "sum")
  
  area_as_farms_nl[[chem]] <- mass_as_farms_nl[[chem]] |> expanse("ha") 
  
  mass_area_as_farms_nl[[chem]] <- cbind(mass_as_farms_nl[[chem]], area_as_farms_nl[[chem]] |> data.frame())
  
}

area_crop_nuts_nl <- dir_ls(path_home_r(), recurse = T, regexp = "crop_area_nl.xlsx$") |>
  read.xlsx() |>
  group_by(CNTR_CODE) |>
  summarise(nr_total_parcels_nl=sum(nr_farms),
            area_total_agri_nl = sum(area_crop_ha))

mass_area_as_farms_nl |>
  vect() |>
  values() |>
  as_tibble() |>
  bind_cols(area_crop_nuts_nl) |> 
  rename(sum_apprate_crop_as_kg_ha = sum_Dose_kg_ha,
         nr_parcels_crop_as = agg_n,
         area_parcels_crop_as_ha = area_as_farms_nl..chem..) |> 
  mutate(frac_area_crop_as =area_parcels_crop_as_ha/area_total_agri_nl,
         mass_as_crop_kg = sum_apprate_crop_as_kg_ha * frac_area_crop_as) |> 
  write.xlsx("area_crop_3as_nl.xlsx")
