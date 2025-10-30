pkg <- c("tidyverse",
         "fs", 
         "readxl",
         "terra",
         "OpenStreetMap",
         "tidyterra",
         "sf",
         "data.table",
         "openxlsx",
         'ncdf4',
         "leaflet",
         "htmltools",
         "htmlwidgets",
         "RColorBrewer",
         "cols4all",
         "progress")

for (i in pkg) {
  if (!requireNamespace(i, quietly = TRUE)) { 
    install.packages(i)
  }
}

lapply(pkg, library, character.only = T)

# Switch terra options to show progress bar for spatial operations
terraOptions(progress = 1)

# Data import for the Netherlands
# NUTS regions
nuts <- dir_ls(path_home_r(), recurse = T, regexp = "/NUTS_RG_20M_2024_4326.shp$")  |>
  vect()

nuts3_nl <- nuts |>
  filter(CNTR_CODE == "NL" & LEVL_CODE == 3)

nuts1_nl <- nuts |>
  filter(CNTR_CODE == "NL" & LEVL_CODE == 1) 

districts_nl <- dir_ls(path_home_r(), recurse = T, regexp = "/LAU_RG_01M_2024_4326.gpkg") |> 
  vect() |> 
  filter(CNTR_CODE == "NL")

# Define the approximate bounding box for country in EPSG:32631 coordinates.
crs_utm31_nl <- "EPSG:32631"

xmin <- 525976.598198582  # Approximate western boundary (easting)
xmax <- 781549.306160638  # Approximate eastern boundary (easting)
ymin <- 5627507.94888309 # Approximate southern boundary (northing)
ymax <- 5929938.95158156 # Approximate northern boundary (northing)

# Define the desired pixel resolution (size of each cell). 
# The size of the pixel is defined based on the smallest size of the vector to be rasterised, e.g., 10m buffer polygons around river segments
res_x <- 100 # 10 meters in the X (easting) direction
res_y <- 100 # 10 meters in the Y (northing) direction

# # Calculate the number of columns and rows needed for the raster grid.
ncols <- ceiling((xmax - xmin) / res_x)
nrows <- ceiling((ymax - ymin) / res_y)

# Create an empty raster. This is a template for rasterisation of river network.
empty_raster_nl <- rast(
  ncols = ncols,        # Number of columns
  nrows = nrows,        # Number of rows
  xmin = xmin,          # Minimum X coordinate of the extent
  xmax = xmin + ncols * res_x, # Maximum X coordinate, precisely calculated from xmin and ncols
  ymin = ymin,          # Minimum Y coordinate of the extent
  ymax = ymin + nrows * res_y, # Maximum Y coordinate, precisely calculated from ymin and nrows
  crs = crs_utm31_nl,     # Coordinate Reference System (EPSG:32631)
  resolution = c(res_x, res_y) # Resolution in X and Y directions
)

# Read soil density map
budens_jrc_nl_path <- dir_ls(path_home_r(),
                             recurse = T,
                             regexp = "budens_jrc_nl.nc$")

budens_jrc_nl <- rast(budens_jrc_nl_path)

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
) |> filter(sdrift_dist_m == 50)

# Read dutch gemap
active <- c("Glyphosate" , "Tebuconazole" , "Acetamiprid")

gemap_nl <- dir_ls(path_home_r(), recurse = T, regexp = "gemap_allchem_nl_wgs84") |> 
  vect() |>
  filter(EU_name %in% active) |> 
  makeValid()

acsubst_gemap_nl <- gemap_nl["EU_name"] |>
  # mutate(EU_name = str_to_title(EU_name)) |>
  values() |>
  unique() |>
  pull()

# Chemical input data from qsars (vega, epi) and PPDB where available #
source(dir_ls(path_home_r(), recurse = T, regexp = "ppdb scraping"))
chemprop <- chemprop_gen(Active = acsubst_gemap_nl) |> 
  select(Active,
         DT50_typical_d,
         Koc_ml.g,
         DT50_field_d,
         Kfoc_ml.g) |> 
  mutate(across(2:last_col(), as.numeric)) |> 
  rename(acsubst = Active)

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
  rename(acsubst = EU_name,
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
  terra::merge(chemprop, by = "acsubst") |>
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
                                              "acsubst",
                                              "nr_farms_as_crop",
                                              "area_as_farms_bas_nl_ha",
                                              "conc_acsubst_total_soil_ini",
                                              "conc_acsubst_total_soil_56twa_ug.kg",
                                              "conc_acsubst_total_soil_365twa_ug.kg")] |>
  tidyterra::mutate(HYBAS_ID = as.character(HYBAS_ID)) |> 
  terra::split("acsubst")

# Rasterise individual parcel polygons to show AS concentration distribution in individual basins
pest_twc_as_farm_nl_rast <- sprc(rasterize(project(pest_twc_as_basin_nl[[1]] |>
                                                     select("conc_acsubst_total_soil_365twa_ug.kg"),
                                                   crs("EPSG:32631")),
                                           project(budens_jrc_nl,
                                                   crs("EPSG:32631")),
                                           field = "conc_acsubst_total_soil_365twa_ug.kg",
                                           touches = T) |> 
                                   disagg(4) |> 
                                   zonal(project(pest_twc_as_basin_nl[[1]],
                                                 crs("EPSG:32631")),
                                         fun = "median",
                                         touches = T,
                                         as.raster = T),
                                 
                                 rasterize(project(pest_twc_as_basin_nl[[2]] |>
                                                     select("conc_acsubst_total_soil_365twa_ug.kg"),
                                                   crs("EPSG:32631")),
                                           project(budens_jrc_nl,
                                                   crs("EPSG:32631")),
                                           field = "conc_acsubst_total_soil_365twa_ug.kg",
                                           touches = T) |>
                                   disagg(4) |> 
                                   zonal(project(pest_twc_as_basin_nl[[2]],
                                                 crs("EPSG:32631")),
                                         fun = "median",
                                         touches = T,
                                         as.raster = T),
                                 rasterize(project(pest_twc_as_basin_nl[[3]] |>
                                                     select("conc_acsubst_total_soil_365twa_ug.kg"),
                                                   crs("EPSG:32631")),
                                           project(budens_jrc_nl,
                                                   crs("EPSG:32631")),
                                           field = "conc_acsubst_total_soil_365twa_ug.kg",
                                           touches = T) |>
                                   disagg(4) |> 
                                   zonal(project(pest_twc_as_basin_nl[[3]],
                                                 crs("EPSG:32631")), 
                                         fun = "median",
                                         touches = T,
                                         as.raster = T))

# Create vector layers of aggregated pesticide concentration in topsoil to the respective basin level
conc_wmean_farm_basin_nl <- list()
area_farm_basin_nl <- list()
nr_farm_basin_nl <- list()
all_as_basin_nl <- list()

for(i in seq_along(pest_twc_as_basin_nl)){

  cat("\r", unique(values(pest_twc_as_basin_nl[[i]]["acsubst"]))[1,1],
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
  
  all_as_basin_nl[[i]] <- merge(conc_wmean_farm_basin_nl[[i]], area_farm_basin_nl[[i]] |> as.data.table(), by = "HYBAS_ID") |> 
    merge(nr_farm_basin_nl[[i]] |> as.data.table(), by = "HYBAS_ID") |>
    # terra::na.omit("HYBAS_ID") |>
    mutate("Active substance" = unique(values(pest_twc_as_basin_nl[[i]]["acsubst"]))[1,1],
           area_as_farms_bas_nl_ha = round(area_as_farms_bas_nl_ha, 1),
           HYBAS_ID = as.character(HYBAS_ID)) |> 
    tidyterra::rename("Basin ID" = HYBAS_ID,
                      "Basin area [ha]" = SUB_AREA_ha,
                      "Concentration (TWA) [\u00B5g\u00D7kg\u207B\u00B9]"  = conc_acsubst_total_soil_365twa_ug.kg,
                      "Number of parcels" = nr_farms_as_crop,
                      "Agricultural area [ha]" = area_as_farms_bas_nl_ha)
}

# Save raster and vector data files
writeCDF(pest_twc_as_farm_nl_rast[1], "Acetamiprid_pec365_topsoil_farm_nl.nc", overwrite = T)
writeCDF(pest_twc_as_farm_nl_rast[2], "Glyphosate_pec365_topsoil_farm_nl.nc", overwrite = T)
writeCDF(pest_twc_as_farm_nl_rast[3], "Tebuconazole_pec365_topsoil_farm_nl.nc", overwrite = T)
writeVector(vect(all_as_basin_nl), "3chem_pec365_topsoil_basin_nl.gpkg", overwrite = T)

####################################################################
########### END: Pesticide Runoff Model Schriever 2007 #############
####################################################################

###########################################################
########### START: Pesticide topsoil map ##################
###########################################################

conc_soil_agg_rast_nl_1 <- dir_ls(path = path_home_r(), regexp = "Acetamiprid_pec365_topsoil_farm_nl.nc", recurse = T) |> rast()
conc_soil_agg_rast_nl_2 <- dir_ls(path = path_home_r(), regexp = "Glyphosate_pec365_topsoil_farm_nl.nc", recurse = T) |> rast()
conc_soil_agg_rast_nl_3 <- dir_ls(path = path_home_r(), regexp = "Tebuconazole_pec365_topsoil_farm_nl.nc", recurse = T) |> rast()
conc_soil_agg_rast_nl <- sprc(conc_soil_agg_rast_nl_1, conc_soil_agg_rast_nl_2, conc_soil_agg_rast_nl_3)
conc_soil_agg_vect_nl <- dir_ls(path = path_home_r(), regexp = "3chem_pec365_topsoil_basin_nl.gpkg", recurse = T) |> vect() |> split("Active.substance")

# PEC soil maps

pec_field_to_basin_nl <- function(conc_soil_agg_rast_nl, conc_soil_agg_vect_nl) {
  
  raster_values <- conc_soil_agg_rast_nl[!is.na(conc_soil_agg_rast_nl)]
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
  
  vector_values <- conc_soil_agg_vect_nl[,2] |> pull()
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
  
  # Custom function to format numbers as power of 10 with superscript
  format_power10 <- function(x, digits = 2) {
    # Handle single value
    format_single <- function(val) {
      if (is.na(val)) return("Missing values")
      
      # Format in scientific notation
      sci <- format(val, scientific = TRUE, digits = digits)
      
      # Parse the scientific notation
      parts <- strsplit(sci, "e")[[1]]
      
      if (length(parts) == 1) {
        # No exponent, just return the number
        return(format(val, digits = digits, scientific = FALSE))
      }
      
      mantissa <- as.numeric(parts[1])
      exponent <- as.numeric(parts[2])
      
      # If exponent is 0 or 1, return unformatted value
      if (exponent == 0) {
        return(format(val, digits = digits, scientific = FALSE))
      }
      
      # Create superscript for exponent
      superscripts <- c("⁰", "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹", "⁻")
      
      # Convert exponent to superscript
      exp_str <- as.character(abs(exponent))
      exp_super <- paste0(sapply(strsplit(exp_str, "")[[1]], function(d) {
        superscripts[as.numeric(d) + 1]
      }), collapse = "")
      
      # Add minus sign if negative
      if (exponent < 0) {
        exp_super <- paste0("⁻", exp_super)
      }
      
      # Return formatted string
      paste0(format(mantissa, digits = digits, nsmall = 1), " × 10", exp_super)
    }
    
    # Handle vector or single value
    if (length(x) > 1) {
      return(sapply(x, format_single))
    } else {
      return(format_single(x))
    }
  }
  
  pec_soil_nl <- leaflet(options = leafletOptions()) |> 
    addMapPane(name = "vect_bas",
               zIndex = 410) |> 
    addMapPane(name = "vect_lau",
               zIndex = 420) |> 
    addMapPane(name = "rast_conc",
               zIndex = 430) |> 
    addMapPane(name = "vect_conc",
               zIndex = 440) |> 
    addProviderTiles(providers$Esri.WorldTopoMap,
                     options = providerTileOptions(minZoom = 0, maxZoom = 18),
                     group = "Esri World Topo Map") |> 
    addPolygons(data = basins_nl["HYBAS_ID"],
                weight = 0.5,
                opacity = 0.75,
                fillColor = "grey",
                fillOpacity = 0.1,
                color = "black",
                highlightOptions = highlightOptions(color = "black",
                                                    weight = 3,
                                                    bringToFront = TRUE),
                popup = paste0("<b>Catchment ID: ", basins_nl$HYBAS_ID ,"</b>"),
                options = pathOptions(pane = "vect_bas"),
                group = "River basins") |>
    addPolygons(data = districts_nl["LAU_NAME"],
                weight = 0.5,
                opacity = 0.75,
                fillColor = "grey",
                fillOpacity = 0.1,
                color = "black",
                highlightOptions = highlightOptions(color = "black",
                                                    weight = 3,
                                                    bringToFront = TRUE),
                popup = paste0("<b>District (LAU): ",districts_nl$LAU_NAME,"</b>"),
                options = pathOptions(pane = "vect_lau"),
                group = "Districts (LAU)") |>
    addRasterImage(conc_soil_agg_rast_nl,
                   colors = color_palette_rast,
                   opacity = 1,
                   group = "Field level concentration",
                   options = pathOptions(pane = "rast_conc")) |> 
    addLegend(pal = color_palette_rast_rev,
              values = values(conc_soil_agg_rast_nl),
              title = paste0(conc_soil_agg_vect_nl$Active.substance |> unique(),"\n",
                             " PEC<sub>365-day</sub> soil at field level<br>(time-weighted) (µg × kg⁻¹)"),
              position = "bottomright",
              group = "Field level concentration",
              labFormat = function(type, cuts, p) {
                n <- length(cuts)
                cuts <- sort(cuts, decreasing = TRUE)
                paste0(sapply(cuts[-1], format_power10, digits = 2), 
                       " to ", 
                       sapply(cuts[-n], format_power10, digits = 2))
              }) |> 
    addPolygons(data = conc_soil_agg_vect_nl,
                fillColor = ~color_palette_vect(vector_values),
                fillOpacity = 0.95,
                stroke = T,
                dashArray = "line",
                color = "black",
                weight = 0.5,
                opacity = 1,
                group = "Basin level concentration",
                options = pathOptions(pane = "vect_conc"),
                highlightOptions = highlightOptions(color = "black",
                                                    weight = 3,
                                                    bringToFront = TRUE),
                popup = paste0("<b>",
                               "Median PEC<sub>365-day</sub> soil (µg × kg⁻¹): ",
                               format_power10(conc_soil_agg_vect_nl[,2] |>
                                                pull(),
                                              digits = 3),
                               "</b>",
                               "<b><br>",
                               "# of treated fields: ",
                               conc_soil_agg_vect_nl[,5] |> pull(),
                               "</b><br>",
                               "<b>",
                               "Area of treated fields (ha): ",
                               conc_soil_agg_vect_nl[,4] |>
                                 pull(),
                               "</b>")) |> 
    addLegend(pal = color_palette_vect_rev,
              values = values(conc_soil_agg_vect_nl[,2]),
              title = paste0(conc_soil_agg_vect_nl$Active.substance |> unique(),"\n",
                             " PEC<sub>365-day</sub> soil at basin level<br>(time- and area-weighted) (µg × kg⁻¹)"),
              position = "bottomright",
              group = 'Basin level concentration',
              labFormat = function(type, cuts, p) {
                n <- length(cuts)
                cuts <- sort(cuts, decreasing = TRUE)
                paste0(sapply(cuts[-1], format_power10, digits = 2), 
                       " to ", 
                       sapply(cuts[-n], format_power10, digits = 2))
              }) |> 
    addLayersControl(overlayGroups = c("Basin level concentration", "Field level concentration", "Districts (LAU)", "River basins"),
                     baseGroups = c("Esri World Topo Map"),
                     options = layersControlOptions(collapsed = F, autoZIndex = T)) |> 
    hideGroup(c("Field level concentration", "Districts (LAU)", "River basins")) |> 
    addScaleBar(position = "bottomright") %>%
    addControl(html = paste0("<div style='background-color: rgba(255, 255, 255, 0.9);
                             padding: 6px 6px; border-radius: 4px; font-size: 14px; font-weight: bold; color: #333; max-width: 800px;
                             line-height: 1.4;'>",
                             conc_soil_agg_vect_nl$Active.substance |> unique(),
                             " 365-day PEC soil after 1x application in July in the Netherlands", 
                             "</div>"),
               position = "topleft") |> 
    addControl(html = "", position = "bottomleft") %>%
    htmlwidgets::onRender("
    function(el, x) {
      // Remove default zoom control if it exists
      if (this._controlCorners) {
        var existingZoom = this._controlCorners.topleft.querySelector('.leaflet-control-zoom');
        if (existingZoom) {
          existingZoom.remove();
        }
      }
      
      // Add zoom control to bottom left
      var zoomControl = L.control.zoom({
        position: 'bottomleft'
      });
      zoomControl.addTo(this);
    }
  ")
  
  pec_soil_nl |> saveWidget(paste0("NL_", conc_soil_agg_vect_nl$Active.substance |> unique(), "_PEC_365_Soil.html"))
  # return(pec_soil_nl)
}

pec_field_to_basin_nl(conc_soil_agg_rast_nl[1], conc_soil_agg_vect_nl[[1]])
pec_field_to_basin_nl(conc_soil_agg_rast_nl[2], conc_soil_agg_vect_nl[[2]])
pec_field_to_basin_nl(conc_soil_agg_rast_nl[3], conc_soil_agg_vect_nl[[3]])
###########################################################
########### END: Pesticide topsoil map ####################
###########################################################


#########################################################################
########### START: Pesticide concentration in surface water #############
#########################################################################
# Calculate the mean, min and max concentration of the active substance in the river segment
# Intersect stream buffer with fields
# Aggregate the data to the river segment level and then to the basin level


# Read dutch river network data from the country geoportal
rivers_nl <- dir_ls(path_home_r(), recurse = T, regexp = "water_network_12_NL.gpkg") |>
vect()

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
#     pest_twc_as_farm_nl[c("EC_trans_n", "acsubst", "srunoff_load", "dis_m3_pyr", "sdrift_dist_m")],
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
# writeVector(rivers_buff_nl |> svc() |> vect(), "rivers_buff100_nl.gpkg")

# Import intersected river network and parcel data 
rivers_farm_buff_nl <- dir_ls(path_home_r(), recurse = T, regexp = "water_network_farmbuff100_12_NL.gpkg") |> 
  vect()

rivers_buff_nl <- dir_ls(path_home_r(), recurse = T, regexp = "rivers_buff100_nl.gpkg") |> 
  vect()

# Split all river related data by width and length categories

# All 3-6 meter streams (286600)
rivers_nl_3_6m <- rivers_nl |> filter(breedteklasse == "3 - 6 meter")
# Rasterise river network
rivers_3_6m_rast_nl <- project(rivers_nl_3_6m, empty_raster_nl) |> rasterize(empty_raster_nl)
rivers_buff_3_6m_nl <- rivers_buff_nl |> filter(breedteklasse == "3 - 6 meter")
rivers_farm_buff_nl_3_6m <- rivers_farm_buff_nl |> filter(breedteklasse == "3 - 6 meter")

# All 0.5-3 meter streams (1610623)
rivers_0.5_3m <- rivers_nl |> filter(breedteklasse == "0,5 - 3 meter")

# <= 20 meter streams 422654
rivers_0.5_3m_L20_nl <- rivers_nl |> filter(breedteklasse == "0,5 - 3 meter",
                                            length_m <= 20)
# Rasterise river network
rivers_0.5_3m_L20_rast_nl <- project(rivers_0.5_3m_L20_nl, empty_raster_nl) |> rasterize(empty_raster_nl)
rivers_buff_0.5_3m_L20_nl <- rivers_buff_nl |> filter(breedteklasse == "0,5 - 3 meter",
                                                length_m <= 20)
rivers_farm_buff_0.5_3m_L20_nl <- rivers_farm_buff_nl |> filter(breedteklasse == "0,5 - 3 meter",
                                                          length_m <= 20)
# 20-60 meter stream 433755
rivers_0.5_3m_L60_nl <- rivers_nl |> filter(breedteklasse == "0,5 - 3 meter",
                                            length_m > 20 & length_m <= 60)
# Rasterise river network
rivers_0.5_3m_L60_rast_nl <- project(rivers_0.5_3m_L60_nl, empty_raster_nl) |> rasterize(empty_raster_nl)
rivers_buff_0.5_3m_L60_nl <- rivers_buff_nl |> filter(breedteklasse == "0,5 - 3 meter",
                                                      length_m > 20 & length_m <= 60)
rivers_farm_buff_0.5_3m_L60_nl <- rivers_farm_buff_nl |> filter(breedteklasse == "0,5 - 3 meter",
                                                                length_m > 20 & length_m <= 60)

# 60-150 meter streams 430790
rivers_0.5_3m_L150_nl <- rivers_nl |> filter(breedteklasse == "0,5 - 3 meter",
                                             length_m > 60 & length_m <= 150)
# Rasterise river network
rivers_0.5_3m_L150_nl_rast <- project(rivers_0.5_3m_L150_nl, empty_raster_nl) |> rasterize(empty_raster_nl)
rivers_buff_0.5_3m_L150_nl <- rivers_buff_nl |> filter(breedteklasse == "0,5 - 3 meter",
                                                      length_m > 20 & length_m <= 60)
rivers_farm_buff_0.5_3m_L150_nl <- rivers_farm_buff_nl |> filter(breedteklasse == "0,5 - 3 meter",
                                                                length_m > 20 & length_m <= 60)
# > 150 meter streams 323424
rivers_0.5_3m_Lmax_nl <- rivers_nl |> filter(breedteklasse == "0,5 - 3 meter",
                                             length_m > 150 & max(length_m))
# Rasterise river network
rivers_0.5_3m_Lmax_nl_rast <- project(rivers_0.5_3m_Lmax_nl, empty_raster_nl) |> rasterize(empty_raster_nl)
rivers_buff_0.5_3m_Lmax_nl <- rivers_buff_nl |> filter(breedteklasse == "0,5 - 3 meter",
                                                       length_m > 150 & max(length_m))
rivers_farm_buff_0.5_3m_Lmax_nl <- rivers_farm_buff_nl |> filter(breedteklasse == "0,5 - 3 meter",
                                                                 length_m > 150 & max(length_m))

# Sum total length of stream for each river basin
river_length_tot_bas <- rivers_nl |> 
  values() |> 
  group_by(HYBAS_ID) |> 
  summarise(length_tot_bas_m = sum(length_m))

# Calculate concentration and weighted concentrations in all individual streams. Split dataset for each pesticide
conc_acsubst_river_seg_nl <- rivers_farm_buff_0.5_3m_Lmax_nl |> 
  terra::merge(river_length_tot_bas, by = "HYBAS_ID") |> 
  select(-HYBAS_ID) |> 
  terra::intersect(basins_nl["HYBAS_ID"]) |> 
  mutate(segment_weight = length_m/length_tot_bas_m) |> 
  mutate(conc_river_seg_ug.dm3 = srunoff_load/dis_m3_pyr,
         conc_river_seg_w_ug.dm3 = conc_river_seg_ug.dm3*segment_weight*1000) |>
  terra::split("acsubst")

# Aggregate pesticide concentration in streams for each crop and river basin
conc_rivseg_agg_nl <- svc(aggregate(conc_acsubst_river_seg_nl[[1]][c("EC_trans_n",  "HYBAS_ID" , "conc_river_seg_w_ug.dm3")],
                                    c("HYBAS_ID", "EC_trans_n"),
                                    fun = "median") |> 
                            zonal(rivers_buff_0.5_3m_Lmax_nl, as.polygons = T, weighted = T),
                          aggregate(conc_acsubst_river_seg_nl[[2]][c("EC_trans_n",  "HYBAS_ID" ,  "conc_river_seg_w_ug.dm3")],
                                    c("HYBAS_ID", "EC_trans_n"),
                                    fun = "median") |> 
                            zonal(rivers_buff_0.5_3m_Lmax_nl, as.polygons = T, weighted = T),
                          aggregate(conc_acsubst_river_seg_nl[[3]][c("EC_trans_n",  "HYBAS_ID" , "conc_river_seg_w_ug.dm3")],
                                    c("HYBAS_ID", "EC_trans_n"),
                                    fun = "median") |> 
                            zonal(rivers_buff_0.5_3m_Lmax_nl, as.polygons = T, weighted = T))

# Rasterise aggregated concentration values for each chemical
conc_rivseg_agg_rast_nl <- sprc(rasterize(project(conc_rivseg_agg_nl[[1]],
                                                  crs("EPSG:32631")),
                                          empty_raster_nl,
                                          field = "median_conc_river_seg_w_ug.dm3",
                                          touches = T),
                                rasterize(project(conc_rivseg_agg_nl[[2]],
                                               crs("EPSG:32631")),
                                          empty_raster_nl,
                                       field = "median_conc_river_seg_w_ug.dm3",
                                       touches = T),
                             rasterize(project(conc_rivseg_agg_nl[[3]],
                                               crs("EPSG:32631")),
                                       empty_raster_nl,
                                       field = "median_conc_river_seg_w_ug.dm3",
                                       touches = T)) 

# Create vector layers of aggregated pesticide concentration in streams for each respective basin level
conc_wmean_river_basin_nl <- list()
lenght_river_buffer_basin_nl <- list()
nr_river_buffer_basin_nl <- list()
conc_rivseg_agg_vect_nl <- list()

for(i in seq_along(conc_acsubst_river_seg_nl)){
  
  cat("\r", unique(values(conc_acsubst_river_seg_nl[[i]]["acsubst"]))[1,1],
      "is being processed out of",
      seq_along(conc_acsubst_river_seg_nl) |> max(), 
      "ASs.",
      max(seq_along(conc_acsubst_river_seg_nl)) - i,
      "ASs left.")
  
  conc_wmean_river_basin_nl[[i]] <- zonal(conc_acsubst_river_seg_nl[[i]][,"conc_river_seg_w_ug.dm3"],
                                    basins_nl[c("HYBAS_ID")],
                                    weighted = T,
                                    as.polygons = T)
  
  lenght_river_buffer_basin_nl[[i]] <- zonal(conc_acsubst_river_seg_nl[[i]]["length_m"],
                                   basins_nl[c("HYBAS_ID")],
                                   fun = "sum",
                                   as.polygons = T)
  
  nr_river_buffer_basin_nl[[i]] <- conc_acsubst_river_seg_nl[[i]] |>
    values() |> 
    group_by(HYBAS_ID) |>
    summarise(stream_seg_count = n())
  
  conc_rivseg_agg_vect_nl[[i]] <- merge(conc_wmean_river_basin_nl[[1]], lenght_river_buffer_basin_nl[[i]] |>
                                        as.data.table(), by = "HYBAS_ID") |> 
    left_join(nr_river_buffer_basin_nl[[i]], by = "HYBAS_ID") |>
    # terra::na.omit("HYBAS_ID") |> 
    mutate("Active substance" = unique(values(conc_acsubst_river_seg_nl[[i]]["acsubst"]))[1,1],
           length_km = round(length_m/1000, 2),
           conc_river_seg_w_ug.dm3 = conc_river_seg_w_ug.dm3*1000) |> 
    tidyterra::rename("Basin ID" = HYBAS_ID,
                      "Concentration [\u00B5g\u00D7dm\u207B\u00B3]" = conc_river_seg_w_ug.dm3,
                      "Total stream length receiving pesticide loadings [km]" = length_km,
                      "Number of streams receiving pesticide loadings" = stream_seg_count)
}

# Save raster and vector data files
writeCDF(conc_rivseg_agg_rast_nl[1], "Acetamiprid_pec365_swater_seg_0.5_3m_Lmax_nl.nc", overwrite = T)
writeCDF(conc_rivseg_agg_rast_nl[2], "Glyphosate_pec365_swater_seg_0.5_3m_Lmax_nl.nc", overwrite = T)
writeCDF(conc_rivseg_agg_rast_nl[3], "Tebuconazole_pec365_swater_seg_0.5_3m_Lmax_nl.nc", overwrite = T)
writeVector(vect(conc_rivseg_agg_vect_nl), "3chem_pec365_swater_seg_basin_0.5_3m_Lmax_nl.gpkg", overwrite = T)

#########################################################################
########### END: Pesticide concentration in surface water ###############
#########################################################################

###################################################################
########### START: Pesticide surface water map ####################
###################################################################

conc_rivseg_agg_rast_nl_1 <- dir_ls(path = path_home_r(), regexp = "Acetamiprid_pec365_swater_seg_0.5_3m_Lmax_nl.nc", recurse = T) |> rast()
conc_rivseg_agg_rast_nl_2 <- dir_ls(path = path_home_r(), regexp = "Glyphosate_pec365_swater_seg_0.5_3m_Lmax_nl.nc", recurse = T) |> rast()
conc_rivseg_agg_rast_nl_3 <- dir_ls(path = path_home_r(), regexp = "Tebuconazole_pec365_swater_seg_0.5_3m_Lmax_nl.nc", recurse = T) |> rast()
conc_rivseg_agg_rast_nl <- sprc(conc_rivseg_agg_rast_nl_1, conc_rivseg_agg_rast_nl_2, conc_rivseg_agg_rast_nl_3)
conc_rivseg_agg_vect_nl <- dir_ls(path = path_home_r(), regexp = "3chem_pec365_swater_seg_basin_0.5_3m_Lmax_nl.gpkg", recurse = T) |> vect() |> split("Active.substance")

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
  
  # Custom function to format numbers as power of 10 with superscript
  format_power10 <- function(x, digits = 2) {
    # Handle single value
    format_single <- function(val) {
      if (is.na(val)) return("Missing values")
      
      # Format in scientific notation
      sci <- format(val, scientific = TRUE, digits = digits)
      
      # Parse the scientific notation
      parts <- strsplit(sci, "e")[[1]]
      
      if (length(parts) == 1) {
        # No exponent, just return the number
        return(format(val, digits = digits, scientific = FALSE))
      }
      
      mantissa <- as.numeric(parts[1])
      exponent <- as.numeric(parts[2])
      
      # If exponent is 0 or 1, return unformatted value
      if (exponent == 0) {
        return(format(val, digits = digits, scientific = FALSE))
      }
      
      # Create superscript for exponent
      superscripts <- c("⁰", "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹", "⁻")
      
      # Convert exponent to superscript
      exp_str <- as.character(abs(exponent))
      exp_super <- paste0(sapply(strsplit(exp_str, "")[[1]], function(d) {
        superscripts[as.numeric(d) + 1]
      }), collapse = "")
      
      # Add minus sign if negative
      if (exponent < 0) {
        exp_super <- paste0("⁻", exp_super)
      }
      
      # Return formatted string
      paste0(format(mantissa, digits = digits, nsmall = 1), " × 10", exp_super)
    }
    
    # Handle vector or single value
    if (length(x) > 1) {
      return(sapply(x, format_single))
    } else {
      return(format_single(x))
    }
  }
  
  pec_swater_nl <- leaflet(options = leafletOptions()) |> 
    addMapPane(name = "rast_riv",
               zIndex = 410) |> 
    addMapPane(name = "vect_bas",
               zIndex = 420) |> 
    addMapPane(name = "vect_lau",
               zIndex = 430) |> 
    addMapPane(name = "rast_conc",
               zIndex = 440) |> 
    addMapPane(name = "vect_conc",
               zIndex = 450) |> 
    addProviderTiles(providers$Esri.WorldTopoMap,
                     options = providerTileOptions(minZoom = 0, maxZoom = 18),
                     group = "Esri World Topo Map") |> 
    addRasterImage(rivers_0.5_3m_Lmax_nl_rast,
                 color = "blue",
                 opacity = 0.5,
                 options = pathOptions(pane = "rast_riv"),
                 group = "River network") |> 
    addPolygons(data = basins_nl["HYBAS_ID"],
                weight = 0.5,
                opacity = 0.5,
                fillColor = "grey",
                fillOpacity = 0.25,
                color = "black",
                highlightOptions = highlightOptions(color = "black",
                                                    weight = 3,
                                                    bringToFront = TRUE),
                popup = paste0("<b>Catchment ID: ", basins_nl$HYBAS_ID ,"</b>"),
                options = pathOptions(pane = "vect_bas"),
                group = "River basins") |>
    addPolygons(data = districts_nl["LAU_NAME"],
                weight = 0.5,
                opacity = 0.5,
                fillColor = "grey",
                fillOpacity = 0.25,
                color = "black",
                highlightOptions = highlightOptions(color = "black",
                                                    weight = 3,
                                                    bringToFront = TRUE),
                popup = paste0("<b>District (LAU): ",districts_nl$LAU_NAME,"</b>"),
                options = pathOptions(pane = "vect_lau"),
                group = "Districts (LAU)") |>
    addRasterImage(conc_rivseg_agg_rast_nl,
                   colors = color_palette_rast,
                   opacity = 1,
                   group = "Stream level concentration",
                   options = pathOptions(pane = "rast_conc")) |> 
    addLegend(pal = color_palette_rast_rev,
              values = values(conc_rivseg_agg_rast_nl),
              title = paste0(conc_rivseg_agg_vect_nl$Active.substance |> unique(),
                             " PEC<sub>annual</sub> surface water<br>at stream level (\u00B5g\u00D7dm\u207B\u00B3)"),
              position = "bottomright",
              group = "Stream level concentration",
              labFormat = function(type, cuts, p) {
                n <- length(cuts)
                cuts <- sort(cuts, decreasing = TRUE)
                paste0(sapply(cuts[-1], format_power10, digits = 2), 
                       " to ", 
                       sapply(cuts[-n], format_power10, digits = 2))
              }) |> 
    addPolygons(data = conc_rivseg_agg_vect_nl,
                fillColor = ~color_palette_vect(vector_values),
                fillOpacity = 0.95,
                stroke = T,
                dashArray = "line",
                color = "black",
                weight = 0.5,
                opacity = 1,
                group = "Basin level concentration",
                options = pathOptions(pane = "vect_conc"),
                highlightOptions = highlightOptions(color = "black",
                                                    weight = 3,
                                                    bringToFront = TRUE),
                popup = paste0("<b>Median PEC<sub>annual</sub> surface water (\u00B5g\u00D7dm\u207B\u00B3): ",
                               format_power10(conc_rivseg_agg_vect_nl[,2] |>
                                                pull(),
                                              digits = 3),
                               "<br></b>",
                               "<b># of streams receiving pesticides: ",
                               conc_rivseg_agg_vect_nl[,4] |>
                                 pull(), 
                               "<br></b>",
                               "<b>Total length of streams receiving pesticides (km): ",
                               conc_rivseg_agg_vect_nl[,6] |>
                                 pull(),
                               "</b>")) |> 
    addLegend(pal = color_palette_vect_rev,
              values = values(conc_rivseg_agg_vect_nl[,2]),
              title = paste0(conc_rivseg_agg_vect_nl$Active.substance |> unique(),
                             " PEC<sub>annual</sub> surface water<br>at basin level (\u00B5g\u00D7dm\u207B\u00B3)"),
              position = "bottomright",
              group = 'Basin level concentration',
              labFormat = function(type, cuts, p) {
                n <- length(cuts)
                cuts <- sort(cuts, decreasing = TRUE)
                paste0(sapply(cuts[-1], format_power10, digits = 2), 
                       " to ", 
                       sapply(cuts[-n], format_power10, digits = 2))
              }) |> 
    addLayersControl(overlayGroups = c("Basin level concentration", "Stream level concentration", "Districts (LAU)", "River basins", "River network"),
                     baseGroups = c("Esri World Topo Map"),
                     options = layersControlOptions(collapsed = F, autoZIndex = T)) |> 
    hideGroup(c("Stream level concentration", "Districts (LAU)", "River basins concentration", "River basins", "River network")) |> 
    addScaleBar(position = "bottomright") |> 
    addControl(html = paste0("<div style='",
                             # "background-color: rgba(255, 255, 255, 0.95); ",
                             "padding: 8px 12px; ",
                             # "border-radius: 4px; ",
                             "box-shadow: 0 2px 4px rgba(0,0,0,0.2); ",
                             "font-size: 14px; ",
                             "font-weight: bold; ",
                             # "color: #333; ",
                             "max-width: calc(100vw - 350px); ",  # Leave space for layers control
                             "width: fit-content; ",
                             "line-height: 1.5; ",
                             "word-wrap: break-word; ",
                             # "white-space: normal; ",
                             "z-index: 1000;",
                             "'>",
                             conc_rivseg_agg_vect_nl$Active.substance |> unique(),
                             " annual PEC surface water after 1x application in July in the Netherlands",
                             "</div>"),
    position = "topleft") |> 
    addControl(html = "", position = "bottomleft") |> 
    htmlwidgets::onRender("
    function(el, x) {
    
      // Remove default zoom control if it exists
      if (this._controlCorners) {
        var existingZoom = this._controlCorners.topleft.querySelector('.leaflet-control-zoom');
        if (existingZoom) {
          existingZoom.remove();
        }
      }
      
      // Add zoom control to bottom left
      var zoomControl = L.control.zoom({
        position: 'bottomleft'
      });
      zoomControl.addTo(this);
      
    // Ensure title stays within bounds on window resize
      var titleControl = document.querySelector('.leaflet-top.leaflet-left .leaflet-control');
      if (titleControl) {
        window.addEventListener('resize', function() {
          var mapWidth = el.offsetWidth;
          var maxWidth = Math.max(200, mapWidth - 350); // Min 200px, leave 350px for controls
          titleControl.style.maxWidth = maxWidth + 'px';
        });
      }
    }
  ")

  # pec_swater_nl |> saveWidget(paste0("NL_", conc_rivseg_agg_vect_nl$Active.substance |> unique(), "_PEC_365_Water_B0.5_3_Lmax.html"))
  return(pec_swater_nl)
}

pec_streams_to_basin_nl(conc_rivseg_agg_rast_nl[1], conc_rivseg_agg_vect_nl[[1]])
pec_streams_to_basin_nl(conc_rivseg_agg_rast_nl[2], conc_rivseg_agg_vect_nl[[2]])
pec_streams_to_basin_nl(conc_rivseg_agg_rast_nl[3], conc_rivseg_agg_vect_nl[[3]])

#################################################################
########### END: Pesticide surface water map ####################
#################################################################

######## Whole country AS usage for aggregated for each crop - AS combination ########

# gemap_nl_bychem <- gemap_nl |> filter(EU_name %in% c("Glyphosate", "Tebuconazole", "Acetamiprid")) |> terra::split("EU_name")
# 
# mass_as_farms_nl <- list()
# area_as_farms_nl<- list()
# mass_area_as_farms_nl<- list()
# 
# for(chem in seq_along(gemap_nl_bychem)){
#   
#   mass_as_farms_nl[[chem]] <- terra::aggregate(gemap_nl_bychem[[chem]] |>
#                                                  select("EC_trans_n",
#                                                         "gewascode",
#                                                         "EU_name",
#                                                         "Dose_kg_ha") |>  makeValid(),
#                                                c("EC_trans_n",
#                                                  "gewascode",
#                                                  "EU_name"),
#                                                fun = "sum")
#   
#   area_as_farms_nl[[chem]] <- mass_as_farms_nl[[chem]] |> expanse("ha") 
#   
#   mass_area_as_farms_nl[[chem]] <- cbind(mass_as_farms_nl[[chem]], area_as_farms_nl[[chem]] |> data.frame())
#   
# }
# 
# area_crop_nuts_nl <- dir_ls(path_home_r(), recurse = T, regexp = "crop_area_nl.xlsx$") |>
#   read.xlsx() |>
#   group_by(CNTR_CODE) |>
#   summarise(nr_total_parcels_nl=sum(nr_farms),
#             area_total_agri_nl = sum(area_crop_ha))
# 
# mass_area_as_farms_nl |>
#   vect() |>
#   values() |>
#   as_tibble() |>
#   bind_cols(area_crop_nuts_nl) |> 
#   rename(sum_apprate_crop_as_kg_ha = sum_Dose_kg_ha,
#          nr_parcels_crop_as = agg_n,
#          area_parcels_crop_as_ha = area_as_farms_nl..chem..) |> 
#   mutate(frac_area_crop_as =area_parcels_crop_as_ha/area_total_agri_nl,
#          mass_as_crop_kg = sum_apprate_crop_as_kg_ha * frac_area_crop_as) |> 
#   write.xlsx("area_crop_3as_nl.xlsx")
