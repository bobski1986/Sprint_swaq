# Install and load packages
pkg <- c("tidyverse", "readxl", "fs", "terra", "tmap", "tmaptools", "data.table", "openxlsx", "OpenStreetMap", "tidyterra", "sf", "leafgl", "leaflet", "rvest")

for (i in pkg) {
  if (!requireNamespace(i, quietly = TRUE)) {
    install.packages(i)
  }
}

lapply(pkg, library, character.only = T)

# Function for displaying ASs concnetrtion in river water and topsoil in buffers using dynamic HTML map for one selected district level#

AS_dynmap_riverwater <- function(district_name,
                                    acsubst_name,
                                    app_month,
                                    app_startday,
                                    endday,
                                    rivbuff_width){

## Some initial values for testing
  district_name <- "Overig Groningen"
  basins_nrmin <- 1
  basins_nrmax <- 5
  acsubst_name <- "Glyphosate"
  app_month <- "July"
  app_startday <- 1
  endday <- 30
  rivbuff_width <- 100


######################################################################
############# START: Import and transform input data sets ############
######################################################################

# Districts borders #
nuts <- dir_ls(path_home_r(), recurse = T, regexp = "/NUTS_RG_20M_2024_4326.shp$")  |>
    vect()
  
districts <- nuts |>
    filter(CNTR_CODE == "NL" & LEVL_CODE == 3)

district_select <- districts |> filter(NUTS_NAME %in% district_name)

# River basins #

###Read basin polygons for the selected district
basins_nl_distr <- dir_ls(path_home_r(), recurse = T, regexp = "hydrosheds_lvl12_basins_nl.gpkg") |>
  select("HYBAS_ID",
         "SUB_AREA",
         "pre_mm_uyr",
         "slp_dg_uav",
         "cly_pc_uav",
         "snd_pc_uav",
         "soc_th_uav") |> 
  tidyterra::mutate(SUB_AREA_ha = SUB_AREA * 100) |> 
  mask(district_select)
# Select the maximum of basin areas to be included

basins_nrmax <- readline(prompt = paste0("There are ", nrow(basins_nl_distr), " river basins intersecting this district. Select number of river basins: "))

basins_nl_distr_max <- basins_nl_distr[1:basins_nrmax,] |> terra::split("HYBAS_ID")

# River network #

## River network CUZK
watrcrsL <- dir_ls(path_home_r(), recurse = T, regexp = "WatrcrsL.shp$") |>
  vect() |>
  project(districts)

# GeMAP for the selected district  and Active substance #
gemap_loc <- list()

for (basin in seq_along(basins_cz_distr_max)) {
  
  gemap_loc[[basin]] <- dir_ls(path_home_r(), recurse = T, regexp = "gemap100") |>
    vect(extent = ext(basins_cz_distr_max[[basin]])) |>
    mask(basins_cz_distr_max[[basin]]) |>
    filter(Active == acsubst_name |> str_to_lower()) |>
    # select("ZKOD", "CTVEREC", "Crop", "EPPO", "EAGRI", "FieldAr", "Active", "ASmass", "ASarea", "ARfarm", "IFav", "IFmin", "IFmax", "District", "ARmin", "ARmax", "BBCHmin", "BBCHmax", "ApFreq") |>
    mutate(District = str_to_title((gsub("_", " ", District, fixed = TRUE)))) |>
    filter(!if_any("IFav", is.na)) |>
    rename(infactor_av = IFav,
           infactor_min = IFmin,
           infactor_max = IFmax,
           acsubst = Active,
           acsubst_mass_kg = ASmass,
           acsubst_area_ha = ASarea,
           field_area = FieldAr,
           aprate_farm_g.ha = ARfarm,
           aprate_min = ARmin,
           aprate_mmax = ARmax,
           apfreq = ApFreq) |>
    mutate(Crop = str_to_sentence(Crop),
           acsubst = str_to_title(acsubst))
  
  cat("\r", "Gemap for", basin, "basin" ,"(out of", basins_cz_distr_max |> length(),")", "in", district_name, "is being processed")
}

gemap_loc <- gemap_loc |> vect() |> terra::intersect(basins_cz_distr["HYBAS_ID"])


# Surface runoff map #
# srunoff_path <- paste0(water_spatial_dir, "v3_runoff_year.tif")
#
# srunoff_cz <- srunoff_path |>
#   rast() |>
#   crop(districts |>
#          project("EPSG:3035")) |>
#   project(districts)
#
# writeCDF(srunoff_cz, filename = paste0(water_spatial_dir,"/srunoff_CZ.nc"), overwrite = T)

# srunoff_basin <- paste0(water_spatial_dir, "srunoff_CZ.nc") |>
#   rast() |>
#   crop(basins_cz_distr)

# CHMU Rainfall #
# For the moment: rainfall data remain separate dataset to be later connected with the crop BBCH and scheduled in PPP registry AS application
meteo_stations <- read.csv(dir_ls(path_home_r(),
                                  recurse = T,
                                  regexp = "stanice_souradnice.csv"),
                           sep = ";") |>
  as_tibble() |>
  mutate(across(3:5,
                \(x) str_replace_all(x,
                                     pattern = ",",
                                     replacement =  ".")),
         Elevation = as.numeric(Elevation),
         Geogr1 = as.numeric(Geogr1),
         Geogr2 = as.numeric(Geogr2)) |>
  add_column(Elevation_unit = "m")

Sys.setlocale("LC_TIME", "uk")

meteo_stations_prec_basins <- dir_ls(path_home_r(),
                                     recurse = T,
                                     regexp = "srazky_SRA.csv") |>
  read.csv(sep = ";",
           dec = ",") |>
  as_tibble() |>
  mutate(value = str_replace(value, ",", "."),
                value = as.numeric(value),
         month = make_datetime(month = month) |>
           lubridate::month(label = T,
                       abbr = F,
                       locale = Sys.getlocale("LC_TIME"))) |>
  rename("rain_mm.day" = "value") |>
  full_join(meteo_stations,
            by = join_by(id == id),
            relationship = "many-to-many") |>
  filter(!is.na(rain_mm.day),
         rain_mm.day >= 0,
# Filter the month for which to run daily simulations
# Filter number of days: from first day of AS application to several day after
         month == app_month, between(day, app_startday, endday)) |>
# Average daily rainfall in selected basins calculated from stations within basin polygons
vect(crs = "WGS84", geom = c("Geogr1", "Geogr2")) |>
  mask(basins_cz_distr) |>
  as_tibble() |>
  group_by(month, day) |>
  summarise(rain_mean_mm.day = mean(rain_mm.day),
            rain_min_mm.day = min(rain_mm.day),
            rain_max_mm.day = max(rain_mm.day))

# Terrain slope #
# Data cropped to include only country of interest and saved. Done only once
## FAO
### 5min resolution
# slope5min_path <- dir_info(water_spatial_dir, regexp = "_5min.asc")
# slopesCl1_8_5min <- map(slope5min_path$path, rast)
# slopesCl1_8_5min_nl <- map(slopesCl1_8_5min, crop, basins_nl)
# slopes_all5min_nl <- sprc(slopesCl1_8_5min_nl) |> terra::mosaic(fun = "mean")
# writeCDF(slopes_all5min_nl,filename = paste0(water_spatial_dir,"/TerrainSlope_5min_nl.nc"), overwrite = T)
slope_nl_5min_path <- dir_ls(path_home_r(),
                             recurse = T,
                             regexp = "TerrainSlope_5min_nl.nc$")
slope_nl_5min <- rast(slope_nl_5min_path) |>
  crop(basins_nl_distr) |> 
  rename("slope_perc" = "TerrainSlope_5min_nl") |>
  mutate(slope_perc = slope_perc/1000/100)

### 30as resolution
# slope30as_path <- dir_info(path_home_r(), recurse = T, regexp = "_30as.asc")
# slopesCl1_8_30as <- map(slope30as_path$path, rast, .progress = T)
# slopesCl1_8_30as_nl <- map(slopesCl1_8_30as, crop, ext(nl_basins), .progress = T)
# slopes_all_perc_nl <- map(slopesCl1_8_30as_nl, ~tan(./3600)*100) |> 
#   rast() |> median() |> project(crs(orcarb_jrc_nl)) |> rename(terrain_slope_med_perc = sum)
# terra::writeCDF(slopes_all_perc_nl, filename = paste0(path_home_r(),"/TerrainSlope_30as_nl.nc"))
slope_nl_30as <- dir_ls(path_home_r(), recurse = T, regexp = "/TerrainSlope_30as_nl.nc$") |>
  rast() |> crop(basins_nl_distr)

# ESDAC Organic carbon content in topsoil #
# Data cropped to include only country of interest and saved. Done only once

# oc_jrc <- rast(oc_jrc_path)
# crs(oc_jrc) <- crs(srunoff_path |> rast())
# oc_jrc <- oc_jrc |> project(srunoff_path |> rast() |> project(basins_cz))
# oc_jrc_cz <- crop(oc_jrc, basins_cz)
# writeCDF(oc_jrc_cz, filename = paste0(water_spatial_dir ,"/OC_jrc_CZ.nc"), overwrite = T)

orcarb_jrc_cz_path <- dir_ls(path_home_r(),
                             recurse = T,
                             regexp ="OC_jrc_CZ.nc$")
orcarb_jrc_cz <- rast(orcarb_jrc_cz_path) |>
  select("OC_jrc_CZ") |>
  crop(gemap_loc)

# ESDAC topsoil physical properties for Europe (based on LUCAS topsoil data) #

# sand_jrc_path <- paste0(water_spatial_dir, "Sand1.tif")
# sand_jrc_laea <- rast(sand_jrc_path)
# sand_jrc_cz_wgs84 <- sand_jrc_laea |>  project(basins_cz) |> crop(basins_cz)
# writeCDF(sand_jrc_cz_wgs84, filename = paste0(water_spatial_dir ,"sand_jrc_CZ.nc"), overwrite = T)

sand_jrc_cz_path <- dir_ls(path_home_r(),
                           recurse = T,
                           regexp = "sand_jrc_CZ.nc$")
sand_jrc_cz <- rast(sand_jrc_cz_path) |>
  crop(gemap_loc)

# clay_jrc_path <- paste0(water_spatial_dir, "Clay.tif")
# clay_jrc_laea <- rast(clay_jrc_path)
# clay_jrc_cz_wgs84 <- clay_jrc_laea |>  project(basins_cz) |> crop(basins_cz)
# writeCDF(clay_jrc_cz_wgs84, filename = paste0(water_spatial_dir ,"clay_jrc_CZ.nc"), overwrite = T)

clay_jrc_cz_path <- dir_ls(path_home_r(),
                           recurse = T,
                           regexp = "clay_jrc_CZ.nc$")
clay_jrc_cz <- rast(clay_jrc_cz_path) |>
  crop(gemap_loc)

# budens_jrc_path <- paste0(water_spatial_dir, "Bulk_density.tif")
# budens_jrc_laea <- rast(budens_jrc_path)
# budens_jrc_cz_wgs84 <- budens_jrc_laea |>  project(basins_cz) |> crop(basins_cz)
# writeCDF(budens_jrc_cz_wgs84, filename = paste0(water_spatial_dir ,"budens_jrc_CZ.nc"), overwrite = T)

budens_jrc_cz_path <- dir_ls(path_home_r(),
                             recurse = T,
                             regexp = "budens_jrc_cz.nc$")
budens_jrc_cz <- rast(budens_jrc_cz_path) |>
  crop(gemap_loc)

# Chemical input data from qsars (vega, epi) and PPDB where available #
source(dir_ls(path_home_r(), recurse = T, regexp = "ppdb scraping"))
chemprop <- chemprop_gen(acsubst_name) |> 
  select(acsubst_name, Kfoc_ml.g, DT50_field_d) |> 
  filter(!is.na(Kfoc_ml.g),
         !is.na(DT50_field_d))

####################################################################
############# END: Import and transform input data sets ############
####################################################################

########################################################################################
############ START: Spatial input data intersected with river segment ##################
########################################################################################
# Intersect river network with river basin(s)
rivers_basin <- terra::intersect(watrcrsL[c("HYDROID", "NAMN1", "SHAPE_Leng", "WD7")], basins_cz_distr)
  # Indicate buffer width around a river segment
  rivers_basin_buff_seg <- rivers_basin |> terra::buffer(rivbuff_width+rivers_basin$WD7)
  # Intersect gemap with selected river segments so the GeMAP is expanded to include hydrography of the selected river basin(s)
  gemap_loc_rivers_buff <- gemap_loc[rivers_basin_buff_seg]

  if(gemap_loc_rivers_buff |> nrow() == 0) {

    "Warning: There are no fields in the buffer, increase buffer width or select another basin"

  }  else {

  # Extract data from raster maps intersected by river segments
  orcarb_river_seg <- terra::zonal(orcarb_jrc_cz,
                                   gemap_loc_rivers_buff,
                               fun = "mean",
                               # weights = T,
                               # exact = T,
                               as.polygons = T) |>
    rename("orcarb_perc" = "OC_jrc_CZ") |>
    mutate(orcarb_perc = orcarb_perc/100)

  # srunoff_river_seg <- terra::zonal(srunoff_basin,
  #                                   gemap_loc_rivers_seg,
  #                                   fun = "mean",
  #                                   # weights = T,
  #                                   # exact = T,
  #                                   as.polygons = T) |>
  #   rename("SR" = "srunoff_CZ")
  
  slope_river_seg <- terra::zonal(slope_cz_5min,
                                  gemap_loc_rivers_buff,
                                  fun = "mean",
                                  # weights = T,
                                  # exact = T,
                                  as.polygons = TRUE) |>
    rename("slope_perc" = "TerrainSlope_5min_CZ") |>
    mutate(slope_perc = slope_perc/1000/100)

  sand_river_seg <- terra::zonal(sand_jrc_cz,
                                 gemap_loc_rivers_buff,
                                  fun = "mean",
                                  # weights = T,
                                  # exact = T,
                                  as.polygons = TRUE) |>
    rename("sand_perc" = "sand_jrc_CZ")


  clay_river_seg <- terra::zonal(clay_jrc_cz,
                                 gemap_loc_rivers_buff,
                                 fun = "mean",
                                 # weights = T,
                                 # exact = T,
                                 as.polygons = TRUE) |>
    rename("clay_perc" = "clay_jrc_CZ")

  budens_river_seg <- terra::zonal(budens_jrc_cz,
                                   gemap_loc_rivers_buff,
                                 fun = "mean",
                                 # weights = T,
                                 # exact = T,
                                 as.polygons = TRUE) |>
    rename("bulk_dens" = "budens_jrc_CZ")

  # 4th order river basins crossing with the river segment
  # basin4 <- terra::mask(basins4ord, rivers_basin3_buff_seg)
  # gemap_rivers_basin4_seg <- gemap |>
  #     terra::intersect(basin4)

   # Patch size of arable land in a river segment
   district_rivers_basins <- mask(districts, rivers_basin_buff_seg) |>
        values() |>
        select(NAZ_LAU1)

   crop_area_tot_river_buff <- gemap_loc_rivers_buff |>
        values() |>
        group_by(District, Crop, acsubst) |>
        summarise(crop_acsubst_totarea_river_buff_ha = sum(field_area)) |>
        ungroup()

   crop_area_tot_distr <- gemap_loc |>
        values() |>
        filter(District %in% district_rivers_basins$NAZ_LAU1) |>
        filter(Crop %in% crop_area_tot_river_buff$Crop) |>
        group_by(District, acsubst, Crop) |>
        summarise(crop_acsubst_totarea_distr_ha = sum(field_area)) |>
        ungroup()

   # District and crop specific application rate per river segment
   acsubst_application <- gemap_loc_rivers_buff |>
        left_join(crop_area_tot_distr, by = join_by(acsubst, Crop, District)) |>
        left_join(crop_area_tot_river_buff, by = join_by(acsubst, Crop, District)) |>
        mutate(acsubst_mass_g = acsubst_mass_kg * 1000,
          crop_acsubst_area_frac_distr = acsubst_area_ha / crop_acsubst_totarea_distr_ha,
               crop_acsubst_area_river_buff_ha = crop_acsubst_totarea_river_buff_ha * crop_acsubst_area_frac_distr,
               acsubst_mass_river_buff_g = crop_acsubst_area_river_buff_ha * aprate_farm_g.ha,
               crop_acsubst_mass_frac_river_buff = acsubst_mass_river_buff_g / acsubst_mass_g) |>
      left_join(chemprop,
               by = c("acsubst" = "acsubst_name"))

#######################################################################################
############ END: Spatial input data intersected with river segment ###################
#######################################################################################

####################################################################
########### START: Pesticide Runoff Model Schriever 2007 ###########
####################################################################

# Model subroutines related to pesticide runoff from individual farms
load_acsubst_farm <- acsubst_application |>
     mutate(apfreq = case_when(apfreq == NA ~ 1, .default = 1)) |>
     cbind(orcarb_river_seg$orcarb_perc |>
             as.data.frame()) |>
     cbind(sand_river_seg$sand_perc |>
             as.data.frame()) |>
     cbind(budens_river_seg$bulk_dens |>
             as.data.frame()) |>
     cbind(clay_river_seg$clay_perc |>
             as.data.frame()) |>
     cbind(slope_river_seg$slope_perc |>
             as.data.frame()) |>
     rename("clay_perc" = "clay_river_seg$clay_perc",
            "sand_perc" = "sand_river_seg$sand_perc",
            "bulk_dens_kg.dm3" = "budens_river_seg$bulk_dens",
            "oc_perc" = "orcarb_river_seg$orcarb_perc",
            "slope_perc" = "slope_river_seg$slope_perc") |>
  ## Effect of crop interception factor
  mutate(infactor_effect = map_dbl(infactor_av,
                               ~1-(./100))) |>
  ## Soil and chemical interaction
  mutate(frac_asubst_soil_water_ini = map2_dbl(oc_perc,
                                               Kfoc_ml.g,
                                         ~1/(1 + (.x*.y)/100))) |>
  mutate(frac_asubst_soil_solid_ini = map2_dbl(oc_perc,
                                               Kfoc_ml.g,
                                             ~((.x*.y)/100)/(1 + (.x*.y/1000)/100))) |>
  ## Initial concentration in soil
  mutate(conc_acsubst_total_soil_ini = pmap_dbl(list(x = aprate_farm_g.ha,
                                             y = bulk_dens_kg.dm3,
                                             z = frac_asubst_soil_water_ini,
                                             v = frac_asubst_soil_solid_ini),
                                        \(x,y,z,v) (x/y*20)*(z+v))) |>
  ## Effect of terrain slope
  mutate(slope_effect = map_dbl(slope_perc,
                                ~if_else(. <= 20,
                                         0.001423 * .^2 + 0.02153 * .,
                                         1))) |>
  ## Active substance application rate in a river segment
  mutate(aprate_river_buff = pmap_dbl(list(x = acsubst_mass_river_buff_g,
                                           y = crop_acsubst_area_river_buff_ha,
                                           z = apfreq),
                                      \(x,y,z) ((x/y)/z)))

   ## Fraction of daily generated surface runoff. Mean, min, max are calculated over meteorological station within the basins
   ## Add code to match rainfall to AS application period for a given crop!!!!
   srunoff_mean_sandy_mm.day <- map_dbl(meteo_stations_prec_basins$rain_mean_mm.day,
                                 ~ (-0.016427-0.011377*.+0.0026284*.^2-5.8564*10^-6*.^3))
   srunoff_mean_loamy_mm.day  <- map_dbl(meteo_stations_prec_basins$rain_mean_mm.day,
                                 ~ (-0.061108-0.0041626*.+0.0040395*.^2-9.0361*10^-6*.^3))
   srunoff_min_sandy_mm.day  <- map_dbl(meteo_stations_prec_basins$rain_min_mm.day,
                                ~ (-0.016427-0.011377*.+0.0026284*.^2-5.8564*10^-6*.^3))
   srunoff_min_loamy_mm.day  <- map_dbl(meteo_stations_prec_basins$rain_min_mm.day,
                                ~ (-0.061108-0.0041626*.+0.0040395*.^2-9.0361*10^-6*.^3))
   srunoff_max_sandy_mm.day  <- map_dbl(meteo_stations_prec_basins$rain_max_mm.day,
                                ~ (-0.016427-0.011377*.+0.0026284*.^2-5.8564*10^-6*.^3))
   srunoff_max_loamy_mm.day  <- map_dbl(meteo_stations_prec_basins$rain_max_mm.day,
                                ~ (-0.061108-0.0041626*.+0.0040395*.^2-9.0361*10^-6*.^3))

   srunoffs <- tibble(srunoff_mean_sandy_mm.day ,
                      srunoff_mean_loamy_mm.day ,
                      srunoff_min_sandy_mm.day ,
                      srunoff_min_loamy_mm.day ,
                      srunoff_max_sandy_mm.day ,
                      srunoff_max_loamy_mm.day) |>
     bind_cols(meteo_stations_prec_basins) |>
     mutate(across(starts_with("srunoff"), ~ case_when(.x < 0 ~ 0, .default = .x)))
     # filter(srunoff_max_loamy_mm.day > 0 & srunoff_max_sandy_mm.day > 0)

  ## Daily amount of pesticide potentially reaching a stream from individual farms
srunoff_acsubst_farm <- list()
   for (i in seq_along(load_acsubst_farm)) {

     srunoff_acsubst_farm[[i]] <-  merge(load_acsubst_farm[i] , srunoffs) |>
       mutate(srunoff_mean_fraction = case_when(sand_perc > clay_perc ~ srunoff_mean_sandy_mm.day/rain_mean_mm.day,
                                                sand_perc < clay_perc ~ srunoff_mean_loamy_mm.day/rain_mean_mm.day),
              srunoff_min_fraction = case_when(sand_perc > clay_perc ~ srunoff_min_sandy_mm.day/rain_min_mm.day,
                                                sand_perc < clay_perc ~ srunoff_min_loamy_mm.day/rain_min_mm.day),
              srunoff_max_fraction = case_when(sand_perc > clay_perc ~ srunoff_max_sandy_mm.day/rain_max_mm.day,
                                                sand_perc < clay_perc ~ srunoff_max_loamy_mm.day/rain_max_mm.day),
              across(starts_with("srunoff"), ~ case_when(.x == "NaN" ~ 0, .default = .x)),
              ndays = max(meteo_stations_prec_basins$day),
              frac_asubst_soil_water_lag = exp(-day * log(2) / DT50_field_d) * frac_asubst_soil_water_ini,
              frac_asubst_soil_solid_lag = exp(-day * log(2) / DT50_field_d) * frac_asubst_soil_solid_ini,
              conc_acsubst_total_soil_lag_g.kg = (frac_asubst_soil_water_lag + frac_asubst_soil_solid_lag) * conc_acsubst_total_soil_ini,
              conc_acsubst_total_soil_twa_g.kg = (conc_acsubst_total_soil_ini / (ndays * (log(2) / DT50_field_d))) * (1 - exp(-ndays * (log(2) / DT50_field_d))),
              ## Product of AS runoff components
              load_acsubst_prod = pmap_dbl(list(crop_acsubst_area_river_buff_ha,
                                                aprate_river_buff,
                                                infactor_effect,
                                                frac_asubst_soil_water_lag,
                                                slope_effect),
                                           prod),
              rain_mean_mm.ndays = mean(rain_mean_mm.day),
              rain_min_mm.ndays = mean(rain_min_mm.day),
              rain_max_mm.ndays = mean(rain_max_mm.day),
              load_acsubst_mean_g.day = load_acsubst_prod * srunoff_mean_fraction,
              load_acsubst_min_g.day = load_acsubst_prod * srunoff_min_fraction,
              load_acsubst_max_g.day = load_acsubst_prod * srunoff_max_fraction,
              load_acsubst_mean_g.ndays = mean(load_acsubst_mean_g.day),
              load_acsubst_min_g.ndays = mean(load_acsubst_min_g.day),
              load_acsubst_max_g.ndays = mean(load_acsubst_max_g.day)) |> slice(1)

       # rename("srunoff_day" = "day")

     cat("\r", i ,"farm out of", nrow(load_acsubst_farm), "is being processed")

   }

# Keep unique rows, but slice() can be keep only the first row
# load_acsubst_farm_mapinput <- srunoff_acsubst_farm |>
#   map(\(x) distinct(x,
#                     # aprate_river_buff,
#                     load_acsubst_mean_g.ndays,
#                     load_acsubst_min_g.ndays,
#                     load_acsubst_max_g.ndays,
#                     conc_acsubst_total_soil_twa_g.kg,
#                     acsubst,
#                     month,
#                     ndays),
#       .progress = T) |>
#   vect()

# Aggregate concentration for a river segment. Connecting farm pesticide loads to the respective river segments

load_acsubst_farm_mapinput <- srunoff_acsubst_farm |> vect()
conc_acsubst_river_seg_mapinput <- load_acsubst_farm_mapinput |>
  makeValid() |>
  terra::intersect(rivers_basin_buff_seg) |>
  group_by(SHAPE_Leng, HYDROID, acsubst, month, ndays, dis_m3_pyr, dis_m3_pmn, dis_m3_pmx) |>
  summarise(conc_acsubst_mean_river_seg_g.m3.ndays = mean(load_acsubst_mean_g.ndays/dis_m3_pyr),
            conc_acsubst_min_river_seg_g.m3.ndays = mean(load_acsubst_min_g.ndays/dis_m3_pmn),
            conc_acsubst_max_river_seg_g.m3.ndays = mean(load_acsubst_max_g.ndays/dis_m3_pmx))

# # Loops to debug some error with intersecting farms and river buffers
# farms_buffers <- list()
#
# for(farm in seq_along(load_acsubst_farm_mapinput)){
#
#   farms_buffers[[farm]] <- load_acsubst_farm_mapinput[farm, ] |>
#     terra::intersect(rivers_basin_buff_seg)
#
#   cat("\r", farm ,"farm out of", nrow(load_acsubst_farm_mapinput), "is being processed")
#
# }
#
# rivseg_buffers <- list()
#
# for(rivseg in seq_along(rivers_basin_buff_seg)){
#
#   rivseg_buffers[[rivseg]] <- load_acsubst_farm_mapinput |>
#     terra::intersect(rivers_basin_buff_seg[rivseg])
#
#   cat("\r", rivseg ,"river segment out of", nrow(rivers_basin_buff_seg), "is being processed")
#
# }
# #Minimal example of aggregating vectors.
# load_acsubst_farm_mapinput_seg1 <- load_acsubst_farm_mapinput[rivers_basin_buff_seg1]
# rivers_basin_buff_seg1 <- rivers_basin_buff_seg |> filter(HYDROID == "N.CZ.WATRCRS.128870000100")
#
# # rivers_basin_buff_seg |> plot()
# aggregate(rivers_basin_buff_seg, "SHAPE_Leng") |> plot(border = "blue")
# intersect(load_acsubst_farm_mapinput) |> plot(col = "red", add = T)
#
# # Under one HYDROID there are multiple polygons having different lengths. Caution, the same concentration may be spread over many polygons. Aggregate with the unique key, not HYDROID
# plot(rivers_basin_buff_seg1[1, ])
# plot(rivers_basin_buff_seg1[2, ])
# plot(rivers_basin_buff_seg1[3, ])
#
# # Quick check how fields are intersected with the river buffer
# rivers_basin_buff_seg1 |> plot(border= "blue")
# plot(load_acsubst_farm_mapinput_seg1 |> terra::intersect(rivers_basin_buff_seg1[c("HYDROID", "dis_m3_pyr", "dis_m3_pmn", "dis_m3_pmx")]), border ="green4", add = T)
# symdif(load_acsubst_farm_mapinput_seg1, intersect(rivers_basin_buff_seg1, load_acsubst_farm_mapinput_seg1)) |> plot(col = "red", add = T)
# plot(rivers_basin_buff_seg1[load_acsubst_farm_mapinput_seg1], border="green", add = T)
# plot(gemap_loc_rivers_buff, col = "green", add = T)

####################################################################
########### END: Pesticide Runoff Model Schriever 2007 #############
####################################################################

##########################################################
########### START: Pesticide Runoff Map ##################
##########################################################
sf_use_s2(F)
tmap_mode("view")

# districts_bb <- st_bbox(basins_cz_distr)
# districts_osmap <- read_osm(districts_bb)

# Admin and river basins borders, river segments and farms within
riverbuff_basemap <- tm_shape(district_select,
                       name = "District borders") +
  tm_borders(col = "black",
             lwd = 1.5) +
# tm_shape(basins_cz_distr,
#            name = "Basin borders (Hydrosheds 1.0)") +
#   tm_borders(col = "blue",
#              lwd = 1,
#              lty = "dashed") +
# tm_shape(rivers_basin |> select(HYBAS_ID),
#            name = "River network (CUZK)") +
#   tm_lines(col = "steelblue1",
#            lwd = 1) +
tm_shape(rivers_basin_buff_seg |> select(NAMN1))+
  tm_polygons(fill_alpha = 0.25,
              popup.vars = c("River name" = "NAMN1"),
              fill = "steelblue1",
              lwd = 0.75,
              group = paste0(rivbuff_width, " m buffer around river segments"),
              group.control = "check") +
# tm_shape(gemap_loc_rivers_buff |> select(Crop),
#            name = "Crops grown in the buffer (EAGRI)") +
#   tm_polygons("Crop",
#               id = "Crop",
#               fill.scale = tm_scale_categorical(value.na = "#f9f9f9", label.na = "Missing", values = "brewer.set3")) +
  tm_scalebar(position = c("right", "bottom")) +
  tm_basemap("Esri.WorldTopoMap", alpha = 0.5, group.control = "check")

# AS concentration in river segments, soil, and loads (shown for entire buffers for visual clarity)
riverseg_conc_acsubst_map <- riverbuff_basemap +
  # tm_shape(load_acsubst_farm_mapinput |> select(aprate_river_buff),
  #          name = "Application rate") +
  # tm_polygons("aprate_river_buff" ,
  #             fill.scale = tm_scale_intervals(value.na = "#f9f9f9", label.na = "Missing", values = "brewer.bu_pu"),
  #             fill.legend = tm_legend(title = "Application rate (g/ha)"),
  #             lwd = 0.2) +
# tm_shape(load_acsubst_farm_mapinput |>
#            select(load_acsubst_mean_g.ndays),
#            name = "Simulated runoff") +
#   tm_polygons("load_acsubst_mean_g.ndays",
#               fill.scale = tm_scale_intervals(value.na = "#f9f9f9",
#                                               label.na = "Missing values",
#                                               values = "brewer.bu_pu"),
#               fill.legend = tm_legend(title = paste0("Load (mean) g/days"),
#                                       bg.color = "#f0f0f0",
#                                       bg.alpha = 1),
#               lwd = 0.2,
#               group = "Simulated load from agricultural fields", 
#               group.control = "radio") +
# tm_shape(load_acsubst_farm_mapinput |>  select(conc_acsubst_total_soil_twa_g.kg),
#            name = "Simulated concentration in soil") +
#   tm_polygons("conc_acsubst_total_soil_twa_g.kg",
#               fill.scale = tm_scale_intervals(value.na = "#f9f9f9" ,
#                                               label.na = "Missing values",
#                                               values = "brewer.bu_pu"),
#               fill.legend = tm_legend(title = "Concentration in top soil (time-wieghted) g/kg",
#                                       bg.color = "#f0f0f0",
#                                       bg.alpha = 1),
#               lwd = 0.2,
#               group = "Simulated concentration in topsoil",
#               group.control = "radio") +
tm_shape(rivers_basin_buff_seg |>
           merge(conc_acsubst_river_seg_mapinput) |>
           select(conc_acsubst_mean_river_seg_g.m3.ndays),
           name = "Simulated concentration in river water") +
  tm_polygons("conc_acsubst_mean_river_seg_g.m3.ndays",
              popup.vars = c("Concentration \u00B5g \u00D7 dm\u207B\u00B3" = "conc_acsubst_mean_river_seg_g.m3.ndays"),
              fill.scale = tm_scale_intervals(value.na = "#f9f9f9" ,
                                              label.na = "Missing values",
                                              values = "brewer.bu_pu"),
              fill.legend = tm_legend(title = "Concentration in river water (mean) \u00B5g \u00D7 dm\u207B\u00B3",
                                      bg.color = "#f0f0f0",
                                      bg.alpha = 1),
              lwd = 0.75, 
              group = "Simulated concentration in river water",
              group.control = "check") +
  tm_title(paste0(district_name,
                  ": ", acsubst_name,
                  " application on ",
                  paste(app_startday, app_month[1]),
                  ". ",
                  endday,
                  "-day simulation period using crop data and ",
                  acsubst_name |> str_to_lower(),
                  " usage for 2021.")) +
  tm_view(control.collapse = F)

########################################################
########### END: Pesticide Runoff Map ##################
########################################################

  }


 tmap_save(riverseg_conc_acsubst_map, paste0(district_name, "_", acsubst_name, "_water", ".html"))
 # writeVector(rivers_basin_buff_seg |>
 #              merge(conc_acsubst_river_seg_mapinput) |>
 #              select(HYDROID, acsubst, month, ndays, conc_acsubst_mean_river_seg_g.m3.ndays), paste0(district_name, "_", acsubst_name, "_water", ".gpkg"))

}

AS_dynmap_riverwater("Břeclav","Tebuconazole", "July", 1, 30 , 100)


# Function for displaying ASs concentration in topsoil on all fields using dynamic HTML map for one selected district #

AS_dynmap_topsoil <- function(district_name,
                                         acsubst_name,
                                         app_month,
                                         app_startday,
                                         endday,
                                         rivbuff_width){
  
  ## Some initial values for testing
  # district_name <- "Benešov"
  # basins_nrmin <- 1
  # basins_nrmax <- 21
  # acsubst_name <- "Tebuconazole"
  # app_month <- "July"
  # app_startday <- 1
  # endday <- 30
  
  ######################################################################
  ############# START: Import and transform input data sets ############
  ######################################################################
  
  # Districts borders #
  districts <- okresy("low")|>
    mutate(NAZ_LAU1 = str_to_title((gsub("-", " ", NAZ_LAU1, fixed = TRUE)))) |>
    vect()
  
  district_select <- districts |> filter(NAZ_LAU1 %in% district_name)
  
  # River basins #
  ## Hydrosheds river basins. Prepared trimmed file to include only the country of interest
  # basins_eu_path <- dir_ls(path_home_r(), recurse = T, regexp = "BasinATLAS_v10_lev10.gpkg")
  # basins_cz <- vect(basins_eu_path, extent = ext(districts))
  # writeVector(basins_cz, "hydrosheds_lvl10_basins_cz.gpkg")
  
  ### Read basin polygons from Hydrosheds for the selected district
  basins_cz_distr <- dir_ls(path_home_r(), recurse = T, regexp = "hydrosheds_lvl10_basins_cz.gpkg") |>
    vect() |>
    select("HYBAS_ID", "dis_m3_pyr", "dis_m3_pmn", "dis_m3_pmx", "riv_tc_ssu", "riv_tc_usu") |>
    mask(district_select)
  # Select the maximum of basin areas to be included in the analysis
  
  basins_nrmax <- readline(prompt = paste0("There are ", nrow(basins_cz_distr), " river basins intersecting this district. Select number of river basins: "))
  
  basins_cz_distr_max <- basins_cz_distr[1:basins_nrmax,] |> terra::split("HYBAS_ID")
  
  # GeMAP for the selected district  and Active substance #
  gemap_loc <- list()

  for (basin in seq_along(basins_cz_distr_max)) {
    
    gemap_loc[[basin]] <- dir_ls(path_home_r(), recurse = T, regexp = "gemap100") |>
      vect(extent = ext(basins_cz_distr_max[[basin]])) |>
      mask(district_select) |>
      # mask(basins_cz_distr_max[[basin]]) |>
      filter(Active == acsubst_name |> str_to_lower()) |>
      # select("ZKOD", "CTVEREC", "Crop", "EPPO", "EAGRI", "FieldAr", "Active", "ASmass", "ASarea", "ARfarm", "IFav", "IFmin", "IFmax", "District", "ARmin", "ARmax", "BBCHmin", "BBCHmax", "ApFreq") |>
      mutate(District = str_to_title((gsub("_", " ", District, fixed = TRUE)))) |>
      filter(!if_any("IFav", is.na)) |>
      rename(infactor_av = IFav,
             infactor_min = IFmin,
             infactor_max = IFmax,
             acsubst = Active,
             acsubst_mass_kg = ASmass,
             acsubst_area_ha = ASarea,
             field_area = FieldAr,
             aprate_farm_g.ha = ARfarm,
             aprate_min = ARmin,
             aprate_mmax = ARmax,
             apfreq = ApFreq) |>
      mutate(Crop = str_to_sentence(Crop),
             acsubst = str_to_title(acsubst))
    
    cat("\r", "Gemap for", basin, "basin" ,"(out of", basins_cz_distr_max |> length(),")", "in", district_name, "is being processed")
  }

  gemap_loc <- gemap_loc |> vect() |> terra::intersect(basins_cz_distr["HYBAS_ID"])
  
  # CHMU Rainfall #
  # For the moment: rainfall data remain separate dataset to be later connected with the crop BBCH and scheduled in PPP registry AS application
  meteo_stations <- read.csv(dir_ls(path_home_r(),
                                    recurse = T,
                                    regexp = "stanice_souradnice.csv"),
                             sep = ";") |>
    as_tibble() |>
    mutate(across(3:5,
                  \(x) str_replace_all(x,
                                       pattern = ",",
                                       replacement =  ".")),
           Elevation = as.numeric(Elevation),
           Geogr1 = as.numeric(Geogr1),
           Geogr2 = as.numeric(Geogr2)) |>
    add_column(Elevation_unit = "m")
  
  Sys.setlocale("LC_TIME", "uk")
  
  meteo_stations_prec_basins <- dir_ls(path_home_r(),
                                       recurse = T,
                                       regexp = "srazky_SRA.csv") |>
    read.csv(sep = ";",
             dec = ",") |>
    as_tibble() |>
    mutate(value = str_replace(value, ",", "."),
           value = as.numeric(value),
           month = make_datetime(month = month) |>
             lubridate::month(label = T,
                              abbr = F,
                              locale = Sys.getlocale("LC_TIME"))) |>
    rename("rain_mm.day" = "value") |>
    full_join(meteo_stations,
              by = join_by(id == id),
              relationship = "many-to-many") |>
    filter(!is.na(rain_mm.day),
           rain_mm.day >= 0,
           # Filter the month for which to run daily simulations
           # Filter number of days: from first day of AS application to several day after
           month == app_month, between(day, app_startday, endday)) |>
    # Average daily rainfall in selected basins calculated from stations within basin polygons
    vect(crs = "WGS84", geom = c("Geogr1", "Geogr2")) |>
    mask(basins_cz_distr) |>
    as_tibble() |>
    group_by(month, day) |>
    summarise(rain_mean_mm.day = mean(rain_mm.day),
              rain_min_mm.day = min(rain_mm.day),
              rain_max_mm.day = max(rain_mm.day))
  
  # Terrain slope #
  # Data cropped to include only country of interest and saved. Done only once
  ## FAO
  ### 5min resolution
  # slope5min_path <- dir_info(water_spatial_dir, regexp = "_5min.asc")
  # slopesCl1_8_5min <- map(slope5min_path$path, rast)
  # slopesCl1_8_5min_cz <- map(slopesCl1_8_5min, crop, basins_cz)
  # slopes_all5min_cz <- sprc(slopesCl1_8_5min_cz) |> terra::mosaic(fun = "mean")
  # writeCDF(slopes_all5min_cz,filename = paste0(water_spatial_dir,"/TerrainSlope_5min_CZ.nc"), overwrite = T)
  slope_cz_5min_path <- dir_ls(path_home_r(),
                               recurse = T,
                               regexp = "TerrainSlope_5min_CZ.nc$")
  slope_cz_5min <- rast(slope_cz_5min_path) |>
    crop(gemap_loc)
  
  ### 30as resolution
  # slope30as_path <- dir_info(water_spatial_dir, regexp = "_30as.asc")
  # slopesCl1_8_30as <- map(slope30as_path$path, rast)
  # slopesCl1_8_30as_cz <- map(slopesCl1_8_30as, crop, basins_cz)
  # slopes_all30as_cz <- sprc(slopesCl1_8_30as_cz) |> terra::mosaic()
  # writeCDF(slopes_all30as_cz, filename = paste0(water_spatial_dir,"/TerrainSlope_30as_CZ.nc"), overwrite = T)
  # slope_cz_30as_path <- paste0(water_spatial_dir, "/TerrainSlope_30as_CZ.nc")
  # slope_cz_30as <- rast(slope_cz_30as_path)
  
  ### Terrain slope maps
  # slope_class <- c("0% ≤ slope ≤ 0.5%", "0.5% ≤ slope ≤ 2%", "2% ≤ slope ≤ 5%", "5% ≤ slope ≤ 10%", "10% ≤ slope ≤ 15%", "15% ≤ slope ≤ 30%",
  #                  "30% ≤ slope ≤ 45%", "Slope > 45%")
  
  # slope5min <- tm_shape(mask(slopes_all5min_cz/100, st_as_sf(districts))) +
  #   tm_raster(col = "GloSlopesCl1_5min",
  #             palette = "Greens",
  #             style = "equal",
  #             n = 8,
  #             labels = slope_class) +
  # tm_shape(st_as_sf(districts)) +
  #   tm_borders(col="black",
  #              lwd = 1) +
  #   tm_layout(legend.outside = T) +
  #   tm_scale_bar(position = "left")
  #
  # slope30as <- tm_shape(mask(slopes_all30as_cz, st_as_sf(districts))) +
  #   tm_raster(col ="GloSlopesCl1_30as",
  #             palette = "Greens",
  #             style = "equal",
  #             labels = slope_class,
  #             n = 8) +
  #   tm_shape(st_as_sf(districts)) +
  #   tm_borders(col="black",
  #              lwd = 1) +
  #   tm_layout(legend.outside = T) +
  #   tm_scale_bar(position = "left")
  
  # ESDAC Organic carbon content in topsoil #
  # Data cropped to include only country of interest and saved. Done only once
  
  # oc_jrc <- rast(oc_jrc_path)
  # crs(oc_jrc) <- crs(srunoff_path |> rast())
  # oc_jrc <- oc_jrc |> project(srunoff_path |> rast() |> project(basins_cz))
  # oc_jrc_cz <- crop(oc_jrc, basins_cz)
  # writeCDF(oc_jrc_cz, filename = paste0(water_spatial_dir ,"/OC_jrc_CZ.nc"), overwrite = T)
  
  orcarb_jrc_cz_path <- dir_ls(path_home_r(),
                               recurse = T,
                               regexp ="OC_jrc_CZ.nc$")
  orcarb_jrc_cz <- rast(orcarb_jrc_cz_path) |>
    select("OC_jrc_CZ") |>
    crop(gemap_loc)
  
  # ESDAC topsoil physical properties for Europe (based on LUCAS topsoil data) #
  
  # sand_jrc_path <- paste0(water_spatial_dir, "Sand1.tif")
  # sand_jrc_laea <- rast(sand_jrc_path)
  # sand_jrc_cz_wgs84 <- sand_jrc_laea |>  project(basins_cz) |> crop(basins_cz)
  # writeCDF(sand_jrc_cz_wgs84, filename = paste0(water_spatial_dir ,"sand_jrc_CZ.nc"), overwrite = T)
  
  sand_jrc_cz_path <- dir_ls(path_home_r(),
                             recurse = T,
                             regexp = "sand_jrc_CZ.nc$")
  sand_jrc_cz <- rast(sand_jrc_cz_path) |>
    crop(gemap_loc)
  
  # clay_jrc_path <- paste0(water_spatial_dir, "Clay.tif")
  # clay_jrc_laea <- rast(clay_jrc_path)
  # clay_jrc_cz_wgs84 <- clay_jrc_laea |>  project(basins_cz) |> crop(basins_cz)
  # writeCDF(clay_jrc_cz_wgs84, filename = paste0(water_spatial_dir ,"clay_jrc_CZ.nc"), overwrite = T)
  
  clay_jrc_cz_path <- dir_ls(path_home_r(),
                             recurse = T,
                             regexp = "clay_jrc_CZ.nc$")
  clay_jrc_cz <- rast(clay_jrc_cz_path) |>
    crop(gemap_loc)
  
  # budens_jrc_path <- paste0(water_spatial_dir, "Bulk_density.tif")
  # budens_jrc_laea <- rast(budens_jrc_path)
  # budens_jrc_cz_wgs84 <- budens_jrc_laea |>  project(basins_cz) |> crop(basins_cz)
  # writeCDF(budens_jrc_cz_wgs84, filename = paste0(water_spatial_dir ,"budens_jrc_CZ.nc"), overwrite = T)
  
  budens_jrc_cz_path <- dir_ls(path_home_r(),
                               recurse = T,
                               regexp = "budens_jrc_cz.nc$")
  budens_jrc_cz <- rast(budens_jrc_cz_path) |>
    crop(gemap_loc)
  
  # Chemical input data from qsars (vega, epi) and PPDB where available #
  source(dir_ls(path_home_r(), recurse = T, regexp = "ppdb scraping"))
  chemprop <- chemprop_gen(acsubst_name) |> 
    select(acsubst_name, Kfoc_ml.g, DT50_field_d) |> 
    filter(!is.na(Kfoc_ml.g),
           !is.na(DT50_field_d))
  
  ####################################################################
  ############# END: Import and transform input data sets ############
  ####################################################################
  
  ########################################################################################
  ############ START: Spatial input data intersected with river basin ##################
  ########################################################################################

    # Extract data from raster maps intersected by river segments
    orcarb_river_seg <- terra::zonal(orcarb_jrc_cz,
                                     gemap_loc,
                                     fun = "mean",
                                     # weights = T,
                                     # exact = T,
                                     as.polygons = T) |>
      rename("orcarb_perc" = "OC_jrc_CZ") |>
      mutate(orcarb_perc = orcarb_perc/100)
    
    # srunoff_river_seg <- terra::zonal(srunoff_basin,
    #                                   gemap_loc_rivers_seg,
    #                                   fun = "mean",
    #                                   # weights = T,
    #                                   # exact = T,
    #                                   as.polygons = T) |>
    #   rename("SR" = "srunoff_CZ")
    
    slope_river_seg <- terra::zonal(slope_cz_5min,
                                    gemap_loc,
                                    fun = "mean",
                                    # weights = T,
                                    # exact = T,
                                    as.polygons = TRUE) |>
      rename("slope_perc" = "TerrainSlope_5min_CZ") |>
      mutate(slope_perc = slope_perc/1000/100)
    
    sand_river_seg <- terra::zonal(sand_jrc_cz,
                                   gemap_loc,
                                   fun = "mean",
                                   # weights = T,
                                   # exact = T,
                                   as.polygons = TRUE) |>
      rename("sand_perc" = "sand_jrc_CZ")
    
    
    clay_river_seg <- terra::zonal(clay_jrc_cz,
                                   gemap_loc,
                                   fun = "mean",
                                   # weights = T,
                                   # exact = T,
                                   as.polygons = TRUE) |>
      rename("clay_perc" = "clay_jrc_CZ")
    
    budens_river_seg <- terra::zonal(budens_jrc_cz,
                                     gemap_loc,
                                     fun = "mean",
                                     # weights = T,
                                     # exact = T,
                                     as.polygons = TRUE) |>
      rename("bulk_dens" = "budens_jrc_CZ")
    
    # Patch size of arable land in a river basins
    district_basin <- mask(districts, basins_cz_distr) |>
      values() |>
      select(NAZ_LAU1)
    
    crop_area_tot_distr_basin <- gemap_loc |>
      values() |>
      group_by(District, HYBAS_ID, Crop, acsubst) |>
      summarise(crop_acsubst_totarea_dsitr_basin_ha = sum(field_area)) |>
      ungroup()
    
    crop_area_tot_basin_distr <- gemap_loc |>
      values() |>
      # filter(District %in% district_basin$NAZ_LAU1) |>
      filter(Crop %in% crop_area_tot_distr_basin$Crop) |>
      group_by(District, acsubst, Crop) |>
      summarise(crop_acsubst_totarea_distr_ha = sum(field_area)) |>
      ungroup()
    
    # District and crop specific application rate per river segment
    acsubst_application <- gemap_loc |>
      left_join(crop_area_tot_distr_basin, by = join_by(District, HYBAS_ID, Crop, acsubst)) |>
      left_join(crop_area_tot_basin_distr, by = join_by(acsubst, Crop, District)) |>
      mutate(acsubst_mass_g = acsubst_mass_kg * 1000,
             crop_acsubst_area_frac_distr_basin = crop_acsubst_totarea_dsitr_basin_ha / acsubst_area_ha,
             crop_acsubst_area_basin_ha = crop_acsubst_totarea_dsitr_basin_ha * crop_acsubst_area_frac_distr_basin,
             crop_acsubst_mass_basin_g = crop_acsubst_area_basin_ha * aprate_farm_g.ha,
             crop_acsubst_mass_frac_basin = crop_acsubst_mass_basin_g / acsubst_mass_g) |>
      left_join(chemprop,
                by = c("acsubst" = "acsubst_name"))
    
    #######################################################################################
    ############ END: Spatial input data intersected with river basin ###################
    #######################################################################################
    
    ####################################################################
    ########### START: Pesticide Runoff Model Schriever 2007 ###########
    ####################################################################
    
    # Model subroutines related to pesticide runoff from individual farms
    load_acsubst_farm <- acsubst_application |>
      mutate(apfreq = case_when(apfreq == NA ~ 1, .default = 1)) |>
      cbind(orcarb_river_seg$orcarb_perc |>
              as.data.frame()) |>
      cbind(sand_river_seg$sand_perc |>
              as.data.frame()) |>
      cbind(budens_river_seg$bulk_dens |>
              as.data.frame()) |>
      cbind(clay_river_seg$clay_perc |>
              as.data.frame()) |>
      cbind(slope_river_seg$slope_perc |>
              as.data.frame()) |>
      rename("clay_perc" = "clay_river_seg$clay_perc",
             "sand_perc" = "sand_river_seg$sand_perc",
             "bulk_dens_kg.dm3" = "budens_river_seg$bulk_dens",
             "oc_perc" = "orcarb_river_seg$orcarb_perc",
             "slope_perc" = "slope_river_seg$slope_perc") |>
      ## Effect of crop interception factor
      mutate(infactor_effect = map_dbl(infactor_av,
                                       ~1-(./100))) |>
      ## Soil and chemical interaction
      mutate(frac_asubst_soil_water_ini = map2_dbl(oc_perc,
                                                   Kfoc_ml.g,
                                                   ~1/(1 + (.x*.y)/100))) |>
      mutate(frac_asubst_soil_solid_ini = map2_dbl(oc_perc,
                                                   Kfoc_ml.g,
                                                   ~((.x*.y)/100)/(1 + (.x*.y/1000)/100))) |>
      ## Initial concentration in soil
      mutate(conc_acsubst_total_soil_ini = pmap_dbl(list(x = aprate_farm_g.ha,
                                                         y = bulk_dens_kg.dm3,
                                                         z = frac_asubst_soil_water_ini,
                                                         v = frac_asubst_soil_solid_ini),
                                                    \(x,y,z,v) (x/(y*20))*(z+v))) |>
      ## Effect of terrain slope
      mutate(slope_effect = map_dbl(slope_perc,
                                    ~if_else(. <= 20,
                                             0.001423 * .^2 + 0.02153 * .,
                                             1))) |>
      ## Active substance application rate in a river segment
      mutate(aprate_basin = pmap_dbl(list(x = crop_acsubst_mass_basin_g,
                                               y = crop_acsubst_area_basin_ha,
                                               z = apfreq),
                                          \(x,y,z) ((x/y)/z)))
    
    ## Fraction of daily generated surface runoff. Mean, min, max are calculated over meteorological station within the basins
    ## Add code to match rainfall to AS application period for a given crop!!!!
    srunoff_mean_sandy_mm.day <- map_dbl(meteo_stations_prec_basins$rain_mean_mm.day,
                                         ~ (-0.016427-0.011377*.+0.0026284*.^2-5.8564*10^-6*.^3))
    srunoff_mean_loamy_mm.day  <- map_dbl(meteo_stations_prec_basins$rain_mean_mm.day,
                                          ~ (-0.061108-0.0041626*.+0.0040395*.^2-9.0361*10^-6*.^3))
    srunoff_min_sandy_mm.day  <- map_dbl(meteo_stations_prec_basins$rain_min_mm.day,
                                         ~ (-0.016427-0.011377*.+0.0026284*.^2-5.8564*10^-6*.^3))
    srunoff_min_loamy_mm.day  <- map_dbl(meteo_stations_prec_basins$rain_min_mm.day,
                                         ~ (-0.061108-0.0041626*.+0.0040395*.^2-9.0361*10^-6*.^3))
    srunoff_max_sandy_mm.day  <- map_dbl(meteo_stations_prec_basins$rain_max_mm.day,
                                         ~ (-0.016427-0.011377*.+0.0026284*.^2-5.8564*10^-6*.^3))
    srunoff_max_loamy_mm.day  <- map_dbl(meteo_stations_prec_basins$rain_max_mm.day,
                                         ~ (-0.061108-0.0041626*.+0.0040395*.^2-9.0361*10^-6*.^3))
    
    srunoffs <- tibble(srunoff_mean_sandy_mm.day ,
                       srunoff_mean_loamy_mm.day ,
                       srunoff_min_sandy_mm.day ,
                       srunoff_min_loamy_mm.day ,
                       srunoff_max_sandy_mm.day ,
                       srunoff_max_loamy_mm.day ) |>
      bind_cols(meteo_stations_prec_basins) |>
      mutate(across(starts_with("srunoff"), ~ case_when(.x < 0 ~ 0, .default = .x)))
    # filter(srunoff_max_loamy_mm.day > 0 & srunoff_max_sandy_mm.day > 0)
    
    ## Daily amount of pesticide potentially reaching a stream from individual farms
    srunoff_acsubst_farm <- list()
    for (i in seq_along(load_acsubst_farm)) {
      
      srunoff_acsubst_farm[[i]] <-  merge(load_acsubst_farm[i] , srunoffs) |>
        mutate(srunoff_mean_fraction = case_when(sand_perc > clay_perc ~ srunoff_mean_sandy_mm.day/rain_mean_mm.day,
                                                 sand_perc < clay_perc ~ srunoff_mean_loamy_mm.day/rain_mean_mm.day),
               srunoff_min_fraction = case_when(sand_perc > clay_perc ~ srunoff_min_sandy_mm.day/rain_min_mm.day,
                                                sand_perc < clay_perc ~ srunoff_min_loamy_mm.day/rain_min_mm.day),
               srunoff_max_fraction = case_when(sand_perc > clay_perc ~ srunoff_max_sandy_mm.day/rain_max_mm.day,
                                                sand_perc < clay_perc ~ srunoff_max_loamy_mm.day/rain_max_mm.day),
               across(starts_with("srunoff"), ~ case_when(.x == "NaN" ~ 0, .default = .x)),
               ndays = max(meteo_stations_prec_basins$day),
               frac_asubst_soil_water_lag = exp(-day * log(2) / DT50_field_d) * frac_asubst_soil_water_ini,
               frac_asubst_soil_solid_lag = exp(-day * log(2) / DT50_field_d) * frac_asubst_soil_solid_ini,
               conc_acsubst_total_soil_lag_g.kg = (frac_asubst_soil_water_lag + frac_asubst_soil_solid_lag) * conc_acsubst_total_soil_ini,
               conc_acsubst_total_soil_twa_g.kg = (conc_acsubst_total_soil_ini / (ndays * (log(2) / DT50_field_d))) * (1 - exp(-ndays * (log(2) / DT50_field_d))),
               ## Product of AS runoff components
               load_acsubst_prod = pmap_dbl(list(crop_acsubst_area_basin_ha,
                                                 aprate_basin,
                                                 infactor_effect,
                                                 frac_asubst_soil_water_lag,
                                                 slope_effect),
                                            prod),
               rain_mean_mm.ndays = mean(rain_mean_mm.day),
               rain_min_mm.ndays = mean(rain_min_mm.day),
               rain_max_mm.ndays = mean(rain_max_mm.day),
               load_acsubst_mean_g.day = load_acsubst_prod * srunoff_mean_fraction,
               load_acsubst_min_g.day = load_acsubst_prod * srunoff_min_fraction,
               load_acsubst_max_g.day = load_acsubst_prod * srunoff_max_fraction,
               load_acsubst_mean_g.ndays = mean(load_acsubst_mean_g.day),
               load_acsubst_min_g.ndays = mean(load_acsubst_min_g.day),
               load_acsubst_max_g.ndays = mean(load_acsubst_max_g.day)) |> slice(1)
      
      # rename("srunoff_day" = "day")
      
      cat("\r", i ,"farm out of", nrow(load_acsubst_farm), "is being processed")
      
    }
    
    load_acsubst_farm_mapinput <- srunoff_acsubst_farm |> vect()
    
    
    # Save the results
    # writeVector(load_acsubst_farm_mapinput, paste0(district_name, "_topsoil_allchem_farm.gpkg"))
    # fwrite(load_acsubst_farm_mapinput |>
    #          select("HYBAS_ID",
    #                 "ZKOD",
    #                 "CTVEREC",
    #                 "acsubst",
    #                 "Crop",
    #                 "month",
    #                 "ndays",
    #                 "conc_acsubst_total_soil_twa_g.kg")  |> 
    #          values(),
    #        paste0(district_name, "_topsoil_allchem_farm.csv"))
    
    
    # Keep unique rows, but slice() can be keep only the first row
    # load_acsubst_farm<- srunoff_acsubst_farm[[1]] |> view()
    # Remove invalid polygons
    # load_acsubst_farm_mapinput <- srunoff_acsubst_farm |>
    #   map(\(x) distinct(x,
    #                     # aprate_river_buff,
    #                     load_acsubst_mean_g.ndays,
    #                     load_acsubst_min_g.ndays,
    #                     load_acsubst_max_g.ndays,
    #                     conc_acsubst_total_soil_twa_g.kg,
    #                     acsubst,
    #                     month,
    #                     ndays),
    #       .progress = T) |>
    #   vect()
    
    ####################################################################
    ########### END: Pesticide Runoff Model Schriever 2007 #############
    ####################################################################
    
    ##########################################################
    ########### START: Pesticide Runoff Map ##################
    ##########################################################
    sf_use_s2(F)
    tmap_mode("view")
    
    # Admin and river basins borders
    basin_basemap <- tm_shape(district_select,
                                  name = "District borders") +
      tm_borders(col = "black",
                 lwd = 1.5) +
      # tm_shape(basins_cz_distr,
      #            name = "Basin borders (Hydrosheds 1.0)") +
      #   tm_borders(col = "blue",
      #              lwd = 1,
      #              lty = "dashed") +
      # tm_shape(rivers_basin |> select(HYBAS_ID),
      #          name = "River network (CUZK)") +
      # tm_lines(col = "steelblue1",
      #          lwd = 1) +
      # tm_shape(rivers_basin_buff_seg |> select(NAMN1),
      #          name = paste0(rivbuff_width, " m buffer around river segments (CUZK)")) +
      # tm_polygons(fill_alpha = 0.1,
      #             col = "steelblue3",
      #             lwd = 0.2) +
      # tm_shape(gemap_loc_rivers_buff |> select(Crop),
      #            name = "Crops grown in the buffer (EAGRI)") +
      #   tm_polygons("Crop",
      #               id = "Crop",
      #               fill.scale = tm_scale_categorical(value.na = "#f9f9f9", label.na = "Missing", values = "brewer.set3")) +
      tm_scalebar(position = c("right", "bottom")) +
      tm_basemap("Esri.WorldTopoMap", alpha = 0.5, group.control = "check")
    
    # AS concentration in soil, and loads from all agricultural fields
    soil_basin_conc_acsubst_map <- basin_basemap +
      # tm_shape(load_acsubst_farm_mapinput |> select(aprate_river_buff),
      #          name = "Application rate") +
      # tm_polygons("aprate_river_buff" ,
      #             fill.scale = tm_scale_intervals(value.na = "#f9f9f9", label.na = "Missing", values = "brewer.bu_pu"),
      #             fill.legend = tm_legend(title = "Application rate (g/ha)"),
      #             lwd = 0.2) +
      tm_shape(load_acsubst_farm_mapinput |>  select(conc_acsubst_total_soil_twa_g.kg),
               name = "Simulated concentration in soil") +
      tm_polygons("conc_acsubst_total_soil_twa_g.kg",
                  popup.vars = c("Concentration \u00B5g \u00D7 kg\u207B\u00B9" = "conc_acsubst_total_soil_twa_g.kg"),
                  fill.scale = tm_scale_intervals(value.na = "#f9f9f9" ,
                                                  label.na = "Missing values",
                                                  values = "brewer.bu_pu"),
                  fill.legend = tm_legend(title = "Concentration in top soil (time-wieghted) \u00B5g \u00D7 kg\u207B\u00B9",
                                          bg.color = "#f0f0f0",
                                          bg.alpha = 1),
                  lwd = 0.75,
                  group = "Simulated concentration in topsoil",
                  group.control = "check") +
      # tm_shape(load_acsubst_farm_mapinput |>
      #            select(load_acsubst_mean_g.ndays),
      #          name = "Simulated runoff") +
      # tm_polygons("load_acsubst_mean_g.ndays",
      #             fill.scale = tm_scale_intervals(value.na = "#f9f9f9",
      #                                             label.na = "Missing values",
      #                                             values = "brewer.bu_pu"),
      #             fill.legend = tm_legend(title = paste0("Load (mean) g/days"),
      #                                     bg.color = "#f0f0f0",
      #                                     bg.alpha = 1),
      #             lwd = 0.2,
      #             group = "Simulated load from agricultural fields", 
      #             group.control = "radio") +
      # tm_shape(rivers_basin_buff_seg |>
      #            merge(conc_acsubst_river_seg_mapinput) |>
      #            select(conc_acsubst_mean_river_seg_g.m3.ndays),
      #          name = "Simulated concentration in river water") +
      # tm_polygons("conc_acsubst_mean_river_seg_g.m3.ndays",
      #             fill.scale = tm_scale_intervals(value.na = "#f9f9f9" ,
      #                                             label.na = "Missing values",
      #                                             values = "brewer.bu_pu"),
      #             fill.legend = tm_legend(title = "Concentration in river water (mean) g/m\u00B3",
      #                                     bg.color = "#f0f0f0",
      #                                     bg.alpha = 1),
      #             lwd = 0.2, 
      #             group = "Simulated concentration in river water",
      #             group.control = "radio") +
      tm_title(paste0(district_name,
                      ": ", acsubst_name,
                      " application on ",
                      paste(app_startday, app_month[1]),
                      ". ",
                      endday,
                      "-day simulation period using crop data and ",
                      acsubst_name |> str_to_lower(),
                      " usage for 2021.")) +
      tm_view(control.collapse = F)
    
    ########################################################
    ########### END: Pesticide Runoff Map ##################
    ########################################################
  
  soil_basin_conc_acsubst_map
  tmap_save(soil_basin_conc_acsubst_map, paste0(district_name, "_", acsubst_name, "_soil", ".html"))
  # writeVector(load_acsubst_farm_mapinput |>
  #               select(HYBAS_ID,ZKOD,CTVEREC,acsubst,Crop, conc_acsubst_total_soil_twa_g.kg), paste0(district_name, "_", acsubst_name, "_soil", ".gpkg"))
  
}

AS_dynmap_topsoil("Břeclav","Acetamiprid", "July", 1, 30 , 100)


# Function for creating static png/jpeg map showing simulated ASs in river water and topsoil in buffers for one selected district #

AS_statmap_riverwater <- function(district_name,
                                         acsubst_name,
                                         app_month,
                                         app_startday,
                                         endday,
                                         rivbuff_width){

## Some initial values for testing
  district_name <- "Benešov"
  basins_nrmin <- 1
  basins_nrmax <- 7
  acsubst_name <- "Glyphosate"
  app_month <- "August"
  app_startday <- 1
  endday <- 10
  rivbuff_width <- 100

  ######################################################################
  ############# START: Import and transform input data sets ############
  ######################################################################
  # Districts borders #
  # Districts borders #
  districts <- okresy("low")|>
    mutate(NAZ_LAU1 = str_to_title((gsub("-", " ", NAZ_LAU1, fixed = TRUE)))) |>
    vect()
  
  district_select <- districts |> filter(NAZ_LAU1 %in% district_name)
  
  # River basins #
  ## Hydrosheds river basins. Prepared trimmed file to include only the country of interest
  # basins_eu_path <- dir_ls(path_home_r(), recurse = T, regexp = "BasinATLAS_v10_lev10.gpkg")
  # basins_cz <- vect(basins_eu_path, extent = ext(districts))
  # writeVector(basins_cz, "hydrosheds_lvl10_basins_cz.gpkg")
  
  ### Read basin polygons for the selected district
  basins_cz_distr <- dir_ls(path_home_r(), recurse = T, regexp = "hydrosheds_lvl10_basins_cz.gpkg") |>
    vect() |>
    select("HYBAS_ID", "dis_m3_pyr", "dis_m3_pmn", "dis_m3_pmx", "riv_tc_ssu", "riv_tc_usu") |>
    mask(district_select)
  # Select the maximum of basin areas to be included
  
  basins_nrmax <- readline(prompt = paste0("There are ", nrow(basins_cz_distr), " river basins intersecting this district. Select number of river basins: "))
  
  basins_cz_distr_max <- basins_cz_distr |> slice(1:basins_nrmax) |> terra::split("HYBAS_ID")
  
  # River network #

  ## River network CUZK
  watrcrsL <- paste0(water_spatial_dir, "/CUZK250/WatrcrsL.shp") |>
    vect() |>
    project(districts)

  # GeMAP for the selected district  and Active substance #
  gemap_loc <- list()
  for (basin in seq_along(basins_cz_distr_max)) {
    
    gemap_loc[[basin]] <- dir_ls(path_home_r(), recurse = T, regexp = "gemap100") |>
      vect(extent = ext(basins_cz_distr_max[[basin]])) |>
      filter(Active == acsubst_name |> str_to_lower()) |>
      # select("ZKOD", "CTVEREC", "Crop", "EPPO", "EAGRI", "FieldAr", "Active", "ASmass", "ASarea", "ARfarm", "IFav", "IFmin", "IFmax", "District", "ARmin", "ARmax", "BBCHmin", "BBCHmax", "ApFreq") |>
      mutate(District = str_to_title((gsub("_", " ", District, fixed = TRUE)))) |>
      intersect(basins_cz_distr_max[[basin]]["HYBAS_ID"]) |>
      filter(!if_any("IFav", is.na)) |>
      rename(infactor_av = IFav,
             infactor_min = IFmin,
             infactor_max = IFmax,
             acsubst = Active,
             acsubst_mass_kg = ASmass,
             acsubst_area_ha = ASarea,
             field_area = FieldAr,
             aprate_farm_g.ha = ARfarm,
             aprate_min = ARmin,
             aprate_mmax = ARmax,
             apfreq = ApFreq) |>
      mutate(Crop = str_to_sentence(Crop),
             acsubst = str_to_title(acsubst))
    
    cat("\r", basin ,"basin out of", basins_nrmax, "is being processed")
  }
  
  gemap_loc <- gemap_loc |> vect()

  # CHMU Rainfall #
  # For the moment: rainfall data remain separate dataset to be later connected with the crop BBCH and scheduled in PPP registry AS application
  meteo_stations <- read.csv(dir_ls(path_home_r(),
                                    recurse = T,
                                    regexp = "stanice_souradnice.csv"),
                             sep = ";") |>
    as_tibble() |>
    mutate(across(3:5,
                  \(x) str_replace_all(x,
                                       pattern = ",",
                                       replacement =  ".")),
           Elevation = as.numeric(Elevation),
           Geogr1 = as.numeric(Geogr1),
           Geogr2 = as.numeric(Geogr2)) |>
    add_column(Elevation_unit = "m")
  
  Sys.setlocale("LC_TIME", "uk")
  
  meteo_stations_prec_basins <- dir_ls(path_home_r(),
                                       recurse = T,
                                       regexp = "srazky_SRA.csv") |>
    read.csv(sep = ";",
             dec = ",") |>
    as_tibble() |>
    mutate(value = str_replace(value, ",", "."),
           value = as.numeric(value),
           month = make_datetime(month = month) |>
             lubridate::month(label = T,
                              abbr = F,
                              locale = Sys.getlocale("LC_TIME"))) |>
    rename("rain_mm.day" = "value") |>
    full_join(meteo_stations,
              by = join_by(id == id),
              relationship = "many-to-many") |>
    filter(!is.na(rain_mm.day),
           rain_mm.day >= 0,
           # Filter the month for which to run daily simulations
           # Filter number of days: from first day of AS application to several day after
           month == app_month, between(day, app_startday, endday)) |>
    # Average daily rainfall in selected basins calculated from stations within basin polygons
    vect(crs = "WGS84", geom = c("Geogr1", "Geogr2")) |>
    mask(basins_cz_distr) |>
    as_tibble() |>
    group_by(month, day) |>
    summarise(rain_mean_mm.day = mean(rain_mm.day),
              rain_min_mm.day = min(rain_mm.day),
              rain_max_mm.day = max(rain_mm.day))

  # Terrain slope #
  # Data cropped to include only country of interest and saved. Done only once
  ## FAO
  ### 5min resolution
  # slope5min_path <- dir_info(water_spatial_dir, regexp = "_5min.asc")
  # slopesCl1_8_5min <- map(slope5min_path$path, rast)
  # slopesCl1_8_5min_cz <- map(slopesCl1_8_5min, crop, basins_cz)
  # slopes_all5min_cz <- sprc(slopesCl1_8_5min_cz) |> terra::mosaic(fun = "mean")
  # writeCDF(slopes_all5min_cz,filename = paste0(water_spatial_dir,"/TerrainSlope_5min_CZ.nc"), overwrite = T)
  slope_cz_5min_path <- dir_ls(path_home_r(),
                               recurse = T,
                               regexp = "TerrainSlope_5min_CZ.nc$")
  slope_cz_5min <- rast(slope_cz_5min_path) |>
    mask(gemap_loc)
  
  ### 30as resolution
  # slope30as_path <- dir_info(water_spatial_dir, regexp = "_30as.asc")
  # slopesCl1_8_30as <- map(slope30as_path$path, rast)
  # slopesCl1_8_30as_cz <- map(slopesCl1_8_30as, crop, basins_cz)
  # slopes_all30as_cz <- sprc(slopesCl1_8_30as_cz) |> terra::mosaic()
  # writeCDF(slopes_all30as_cz, filename = paste0(water_spatial_dir,"/TerrainSlope_30as_CZ.nc"), overwrite = T)
  # slope_cz_30as_path <- paste0(water_spatial_dir, "/TerrainSlope_30as_CZ.nc")
  # slope_cz_30as <- rast(slope_cz_30as_path)
  
  ### Terrain slope maps
  # slope_class <- c("0% ≤ slope ≤ 0.5%", "0.5% ≤ slope ≤ 2%", "2% ≤ slope ≤ 5%", "5% ≤ slope ≤ 10%", "10% ≤ slope ≤ 15%", "15% ≤ slope ≤ 30%",
  #                  "30% ≤ slope ≤ 45%", "Slope > 45%")
  
  # slope5min <- tm_shape(mask(slopes_all5min_cz/100, st_as_sf(districts))) +
  #   tm_raster(col = "GloSlopesCl1_5min",
  #             palette = "Greens",
  #             style = "equal",
  #             n = 8,
  #             labels = slope_class) +
  # tm_shape(st_as_sf(districts)) +
  #   tm_borders(col="black",
  #              lwd = 1) +
  #   tm_layout(legend.outside = T) +
  #   tm_scale_bar(position = "left")
  #
  # slope30as <- tm_shape(mask(slopes_all30as_cz, st_as_sf(districts))) +
  #   tm_raster(col ="GloSlopesCl1_30as",
  #             palette = "Greens",
  #             style = "equal",
  #             labels = slope_class,
  #             n = 8) +
  #   tm_shape(st_as_sf(districts)) +
  #   tm_borders(col="black",
  #              lwd = 1) +
  #   tm_layout(legend.outside = T) +
  #   tm_scale_bar(position = "left")
  
  # ESDAC Organic carbon content in topsoil #
  # Data cropped to include only country of interest and saved. Done only once
  
  # oc_jrc <- rast(oc_jrc_path)
  # crs(oc_jrc) <- crs(srunoff_path |> rast())
  # oc_jrc <- oc_jrc |> project(srunoff_path |> rast() |> project(basins_cz))
  # oc_jrc_cz <- crop(oc_jrc, basins_cz)
  # writeCDF(oc_jrc_cz, filename = paste0(water_spatial_dir ,"/OC_jrc_CZ.nc"), overwrite = T)
  
  orcarb_jrc_cz_path <- dir_ls(path_home_r(),
                               recurse = T,
                               regexp ="OC_jrc_CZ.nc$")
  orcarb_jrc_cz <- rast(orcarb_jrc_cz_path) |>
    select("OC_jrc_CZ") |>
    crop(gemap_loc)
  
  # ESDAC topsoil physical properties for Europe (based on LUCAS topsoil data) #
  
  # sand_jrc_path <- paste0(water_spatial_dir, "Sand1.tif")
  # sand_jrc_laea <- rast(sand_jrc_path)
  # sand_jrc_cz_wgs84 <- sand_jrc_laea |>  project(basins_cz) |> crop(basins_cz)
  # writeCDF(sand_jrc_cz_wgs84, filename = paste0(water_spatial_dir ,"sand_jrc_CZ.nc"), overwrite = T)
  
  sand_jrc_cz_path <- dir_ls(path_home_r(),
                             recurse = T,
                             regexp = "sand_jrc_CZ.nc$")
  sand_jrc_cz <- rast(sand_jrc_cz_path) |>
    crop(gemap_loc)
  
  # clay_jrc_path <- paste0(water_spatial_dir, "Clay.tif")
  # clay_jrc_laea <- rast(clay_jrc_path)
  # clay_jrc_cz_wgs84 <- clay_jrc_laea |>  project(basins_cz) |> crop(basins_cz)
  # writeCDF(clay_jrc_cz_wgs84, filename = paste0(water_spatial_dir ,"clay_jrc_CZ.nc"), overwrite = T)
  
  clay_jrc_cz_path <- dir_ls(path_home_r(),
                             recurse = T,
                             regexp = "clay_jrc_CZ.nc$")
  clay_jrc_cz <- rast(clay_jrc_cz_path) |>
    crop(gemap_loc)
  
  # budens_jrc_path <- paste0(water_spatial_dir, "Bulk_density.tif")
  # budens_jrc_laea <- rast(budens_jrc_path)
  # budens_jrc_cz_wgs84 <- budens_jrc_laea |>  project(basins_cz) |> crop(basins_cz)
  # writeCDF(budens_jrc_cz_wgs84, filename = paste0(water_spatial_dir ,"budens_jrc_CZ.nc"), overwrite = T)
  
  budens_jrc_cz_path <- dir_ls(path_home_r(),
                               recurse = T,
                               regexp = "budens_jrc_cz.nc$")
  budens_jrc_cz <- rast(budens_jrc_cz_path) |>
    crop(gemap_loc)
  
  # Chemical input data from qsars (vega, epi) and PPDB where available #
  chemprop <- dir_ls(path_home_r(),
                     recurse = T,
                     regexp = "chem_prop_ppdb.csv$") |>
    fread() |>
    rename(acsubst_name = active_name) |> 
    mutate(across(4:last_col(), as.numeric)) |>
    select(acsubst_name, Kfoc_ml.g, DT50_field_d) |> 
    filter(!is.na(Kfoc_ml.g),
           !is.na(DT50_field_d))
  
  ####################################################################
  ############# END: Import and transform input data sets ############
  ####################################################################

  ########################################################################################
  ############ START: Spatial input data intersected with river segment ##################
  ########################################################################################
  # Intersect river network with river basin(s)
  rivers_basin <- terra::intersect(watrcrsL[c("HYDROID", "NAMN1", "SHAPE_Leng", "WD7")], basins_cz_distr)
  # Indicate buffer width around a river segment
  rivers_basin_buff_seg <- rivers_basin |> terra::buffer(rivbuff_width+rivers_basin$WD7)
  # Intersect gemap with selected river segments so the GeMAP is expanded to include hydrography of the selected river basin(s)
  gemap_loc_rivers_buff <- gemap_loc[rivers_basin_buff_seg]

  if(gemap_loc_rivers_buff |> nrow() == 0) {

    "Warning: There are no fields in the buffer, increase buffer width or select another basin"

  }  else {

    # Extract data from raster maps intersected by river segments
    orcarb_river_seg <- terra::zonal(orcarb_jrc_cz,
                                     gemap_loc,
                                     fun = "mean",
                                     # weights = T,
                                     # exact = T,
                                     as.polygons = T) |>
      rename("orcarb_perc" = "OC_jrc_CZ") |>
      mutate(orcarb_perc = orcarb_perc/100)

    # srunoff_river_seg <- terra::zonal(srunoff_basin,
    #                                   gemap_loc_rivers_seg,
    #                                   fun = "mean",
    #                                   # weights = T,
    #                                   # exact = T,
    #                                   as.polygons = T) |>
    #   rename("SR" = "srunoff_CZ")

    slope_river_seg <- terra::zonal(slope_cz_5min,
                                    gemap_loc,
                                    fun = "mean",
                                    # weights = T,
                                    # exact = T,
                                    as.polygons = TRUE) |>
      rename("slope_perc" = "TerrainSlope_5min_CZ") |>
      mutate(slope_perc = slope_perc/1000/100)

    sand_river_seg <- terra::zonal(sand_jrc_cz,
                                   gemap_loc,
                                   fun = "mean",
                                   # weights = T,
                                   # exact = T,
                                   as.polygons = TRUE) |>
      rename("sand_perc" = "sand_jrc_CZ")


    clay_river_seg <- terra::zonal(clay_jrc_cz,
                                   gemap_loc,
                                   fun = "mean",
                                   # weights = T,
                                   # exact = T,
                                   as.polygons = TRUE) |>
      rename("clay_perc" = "clay_jrc_CZ")

    budens_river_seg <- terra::zonal(budens_jrc_cz,
                                     gemap_loc,
                                     fun = "mean",
                                     # weights = T,
                                     # exact = T,
                                     as.polygons = TRUE) |>
      rename("bulk_dens" = "budens_jrc_CZ")

    # 4th order river basins crossing with the river segment
    # basin4 <- terra::mask(basins4ord, rivers_basin3_buff_seg)
    # gemap_rivers_basin4_seg <- gemap |>
    #     terra::intersect(basin4)

    # Patch size of arable land in a river segment
    district_rivers_basins <- mask(districts, rivers_basin_buff_seg) |>
      values() |>
      select(NAZ_LAU1)

    crop_area_tot_river_buff <- gemap_loc_rivers_buff |>
      values() |>
      group_by(District, Crop, acsubst) |>
      summarise(crop_area_river_buff_ha = sum(field_area)) |>
      ungroup()

    crop_area_tot_distr <- gemap_loc |>
      values() |>
      filter(District %in% district_rivers_basins$NAZ_LAU1) |>
      filter(Crop %in% crop_area_tot_river_buff$Crop) |>
      group_by(District, acsubst, Crop) |>
      summarise(crop_area_distr_ha = sum(field_area)) |>
      ungroup()

    # District and crop specific application rate per river segment
    acsubst_application <- gemap_loc |>
      left_join(crop_area_tot_distr, by = join_by(acsubst, Crop, District)) |>
      left_join(crop_area_tot_river_buff, by = join_by(acsubst, Crop, District)) |>
      mutate(acsubst_mass_g = acsubst_mass * 1000,
             acsubst_area_ha = acsubst_area,
             aprate_farm_g.ha = aprate_farm,
             crop_acsubst_area_frac_distr = acsubst_area_ha / crop_area_distr_ha,
             crop_acsubst_area_river_buff_ha = crop_area_river_buff_ha * crop_acsubst_area_frac_distr,
             mass_acsubst_river_buff_g = crop_acsubst_area_river_buff_ha * aprate_farm_g.ha) |>
      left_join(chemprop,
                by = c("acsubst" = "acsubst_name"))

  #######################################################################################
  ############ END: Spatial input data intersected with river segment ###################
  #######################################################################################

  ####################################################################
  ########### START: Pesticide Runoff Model Schriever 2007 ###########
  ####################################################################

    # Model subroutines related to pesticide runoff from individual farms in the whole district
    load_acsubst_farm <- acsubst_application |>
      cbind(orcarb_river_seg$orcarb_perc |>
              as.data.frame()) |>
      cbind(sand_river_seg$sand_perc |>
              as.data.frame()) |>
      cbind(budens_river_seg$bulk_dens |>
              as.data.frame()) |>
      cbind(clay_river_seg$clay_perc |>
              as.data.frame()) |>
      cbind(slope_river_seg$slope_perc |>
              as.data.frame()) |>
      rename("clay_perc" = "clay_river_seg$clay_perc",
             "sand_perc" = "sand_river_seg$sand_perc",
             "bulk_dens_kg.dm3" = "budens_river_seg$bulk_dens",
             "oc_perc" = "orcarb_river_seg$orcarb_perc",
             "slope_perc" = "slope_river_seg$slope_perc") |>
      ## Effect of crop interception factor
      mutate(infactor_effect = map_dbl(infactor_av,
                                       ~1-(./100))) |>
      ## Soil and chemical interaction
      mutate(frac_asubst_soil_water_ini = map2_dbl(oc_perc,
                                                   Kfoc_ml.g,
                                                   ~1/(1 + (.x*.y)/100))) |>
      mutate(frac_asubst_soil_solid_ini = map2_dbl(oc_perc,
                                                   Kfoc_ml.g,
                                                   ~((.x*.y)/100)/(1 + (.x*.y/1000)/100))) |>
      ## Initial concentration in soil
      mutate(conc_acsubst_total_soil_ini = pmap_dbl(list(x = aprate_farm_g.ha,
                                                         y = bulk_dens_kg.dm3,
                                                         z = frac_asubst_soil_water_ini,
                                                         v = frac_asubst_soil_solid_ini),
                                                    \(x,y,z,v) (x/(y*1000*20))*(z+v))) |>
      ## Effect of terrain slope
      mutate(slope_effect = map_dbl(slope_perc,
                                    ~if_else(. <= 20,
                                             0.001423 * .^2 + 0.02153 * .,
                                             1))) |>
      ## Active substance application rate in a river segment
      mutate(aprate_river_buff = map2_dbl(mass_acsubst_river_buff_g,
                                          crop_area_river_buff_ha,
                                          ~.x/.y)) |>
      ## Product of AS runoff components
      mutate(load_acsubst_prod = pmap_dbl(list(crop_area_river_buff_ha,
                                               aprate_river_buff,
                                               infactor_effect,
                                               frac_asubst_soil_water_ini,
                                               slope_effect),
                                          prod))

    ## Fraction of daily generated surface runoff. Mean, min, max are calculated over meteorological station within the basins
    ## Add code to match rainfall to AS application period for a given crop!!!!
    srunoff_mean_sandy_mm.day <- map_dbl(meteo_stations_prec_basins$rain_mean_mm.day,
                                         ~ (-0.016427-0.011377*.+0.0026284*.^2-5.8564*10^-6*.^3))
    srunoff_mean_loamy_mm.day  <- map_dbl(meteo_stations_prec_basins$rain_mean_mm.day,
                                          ~ (-0.061108-0.0041626*.+0.0040395*.^2-9.0361*10^-6*.^3))
    srunoff_min_sandy_mm.day  <- map_dbl(meteo_stations_prec_basins$rain_min_mm.day,
                                         ~ (-0.016427-0.011377*.+0.0026284*.^2-5.8564*10^-6*.^3))
    srunoff_min_loamy_mm.day  <- map_dbl(meteo_stations_prec_basins$rain_min_mm.day,
                                         ~ (-0.061108-0.0041626*.+0.0040395*.^2-9.0361*10^-6*.^3))
    srunoff_max_sandy_mm.day  <- map_dbl(meteo_stations_prec_basins$rain_max_mm.day,
                                         ~ (-0.016427-0.011377*.+0.0026284*.^2-5.8564*10^-6*.^3))
    srunoff_max_loamy_mm.day  <- map_dbl(meteo_stations_prec_basins$rain_max_mm.day,
                                         ~ (-0.061108-0.0041626*.+0.0040395*.^2-9.0361*10^-6*.^3))

    srunoffs <- tibble(srunoff_mean_sandy_mm.day ,
                       srunoff_mean_loamy_mm.day ,
                       srunoff_min_sandy_mm.day ,
                       srunoff_min_loamy_mm.day ,
                       srunoff_max_sandy_mm.day ,
                       srunoff_max_loamy_mm.day ) |>
      bind_cols(meteo_stations_prec_basins) |>
      mutate(across(starts_with("srunoff"), ~ case_when(.x < 0 ~ 0, .default = .x)))
    # filter(srunoff_max_loamy_mm.day > 0 & srunoff_max_sandy_mm.day > 0)


    ## Daily amount of pesticide potentially reaching a stream from individual farms
    srunoff_acsubst_farm <- list()
    for (i in seq_along(load_acsubst_farm)) {

      srunoff_acsubst_farm[[i]] <-  merge(load_acsubst_farm[i] , srunoffs) |>
        mutate(srunoff_mean_fraction = case_when(sand_perc > clay_perc ~ srunoff_mean_sandy_mm.day/rain_mean_mm.day,
                                                 sand_perc < clay_perc ~ srunoff_mean_loamy_mm.day/rain_mean_mm.day),
               srunoff_min_fraction = case_when(sand_perc > clay_perc ~ srunoff_min_sandy_mm.day/rain_min_mm.day,
                                                sand_perc < clay_perc ~ srunoff_min_loamy_mm.day/rain_min_mm.day),
               srunoff_max_fraction = case_when(sand_perc > clay_perc ~ srunoff_max_sandy_mm.day/rain_max_mm.day,
                                                sand_perc < clay_perc ~ srunoff_max_loamy_mm.day/rain_max_mm.day),
               across(starts_with("srunoff"), ~ case_when(.x == "NaN" ~ 0, .default = .x)),
               ndays = max(meteo_stations_prec_basins$day),
               frac_asubst_soil_water_lag = exp(-day * log(2) / DT50_field_d) * frac_asubst_soil_water_ini,
               frac_asubst_soil_solid_lag = exp(-day * log(2) / DT50_field_d) * frac_asubst_soil_solid_ini,
               conc_acsubst_total_soil_lag_g.kg = (frac_asubst_soil_water_lag + frac_asubst_soil_solid_lag) * conc_acsubst_total_soil_ini,
               conc_acsubst_total_soil_twa_g.kg = (conc_acsubst_total_soil_ini / (ndays * (log(2) / DT50_field_d))) * (1 - exp(-ndays * (log(2) / DT50_field_d))),
               load_acsubst_prod = pmap_dbl(list(crop_area_river_buff_ha,
                                                 aprate_river_buff,
                                                 infactor_effect,
                                                 frac_asubst_soil_water_lag,
                                                 slope_effect),
                                            prod),
               rain_mean_mm.ndays = mean(rain_mean_mm.day),
               rain_min_mm.ndays = mean(rain_min_mm.day),
               rain_max_mm.ndays = mean(rain_max_mm.day),
               load_acsubst_mean_g.day = load_acsubst_prod * srunoff_mean_fraction,
               load_acsubst_min_g.day = load_acsubst_prod * srunoff_min_fraction,
               load_acsubst_max_g.day = load_acsubst_prod * srunoff_max_fraction,
               load_acsubst_mean_g.ndays = mean(load_acsubst_mean_g.day),
               load_acsubst_min_g.ndays = mean(load_acsubst_min_g.day),
               load_acsubst_max_g.ndays = mean(load_acsubst_max_g.day))

      # rename("srunoff_day" = "day")

      cat("\r", i ,"farm in", district_name, "out of", nrow(load_acsubst_farm), "is being processed")

    }

    # Selecting only unique mean, max min load values for each farm
    load_acsubst_farm_mapinput <- srunoff_acsubst_farm |>
      map(\(x) distinct(x,
                        load_acsubst_mean_g.ndays,
                        load_acsubst_min_g.ndays,
                        load_acsubst_max_g.ndays,
                        conc_acsubst_total_soil_twa_g.kg,
                        acsubst,
                        month,
                        ndays),
          .progress = T) |>
      vect()

    # Aggregate concentration for a river segment. Connecting farm pesticide loads to the respective river segments
    conc_acsubst_river_seg_mapinput <- load_acsubst_farm_mapinput |>
      makeValid() |>
      terra::intersect(rivers_basin_buff_seg) |>
      group_by(SHAPE_Leng, HYDROID, acsubst, month, ndays, dis_m3_pyr, dis_m3_pmn, dis_m3_pmx) |>
      summarise(conc_acsubst_mean_river_seg_g.m3.ndays = mean(load_acsubst_mean_g.ndays/dis_m3_pyr),
                conc_acsubst_min_river_seg_g.m3.ndays = mean(load_acsubst_min_g.ndays/dis_m3_pmn),
                conc_acsubst_max_river_seg_g.m3.ndays= mean(load_acsubst_max_g.ndays/dis_m3_pmx))
    

    # # Loops to debug some error with intersecting farms and river buffers
    # farms_buffers <- list()
    #
    # for(farm in seq_along(load_acsubst_farm_mapinput)){
    #
    #   farms_buffers[[farm]] <- load_acsubst_farm_mapinput[farm, ] |>
    #     terra::intersect(rivers_basin_buff_seg)
    #
    #   cat("\r", farm ,"farm out of", nrow(load_acsubst_farm_mapinput), "is being processed")
    #
    #
    # }
    #
    # rivseg_buffers <- list()
    #
    # for(rivseg in seq_along(rivers_basin_buff_seg)){
    #
    #   rivseg_buffers[[rivseg]] <- load_acsubst_farm_mapinput |>
    #     terra::intersect(rivers_basin_buff_seg[rivseg])
    #
    #   cat("\r", rivseg ,"river segment out of", nrow(rivers_basin_buff_seg), "is being processed")
    #
    #
    # }
    # #Minimal example of aggregating vectors.
    # load_acsubst_farm_mapinput_seg1 <- load_acsubst_farm_mapinput[rivers_basin_buff_seg1]
    # rivers_basin_buff_seg1 <- rivers_basin_buff_seg |> filter(HYDROID == "N.CZ.WATRCRS.128870000100")
    #
    # rivers_basin_buff_seg |> plot()
    # aggregate(rivers_basin_buff_seg, "SHAPE_Leng") |> plot(border = "blue")
    # intersect(load_acsubst_farm_mapinput) |> plot(col = "red", add = T)
    #
    # # Quick check how fields are intersected with the river buffer
    # rivers_basin_buff_seg1 |> plot(border= "blue")
    # plot(load_acsubst_farm_mapinput_seg1 |> terra::intersect(rivers_basin_buff_seg1[c("HYDROID", "dis_m3_pyr", "dis_m3_pmn", "dis_m3_pmx")]), border ="green4", add = T)
    # symdif(load_acsubst_farm_mapinput_seg1, intersect(rivers_basin_buff_seg1, load_acsubst_farm_mapinput_seg1)) |> plot(col = "red", add = T)
    # plot(rivers_basin_buff_seg1[load_acsubst_farm_mapinput_seg1], border="green", add = T)
    # plot(gemap_loc_rivers_buff, col = "green", add = T)

  ####################################################################
  ########### END: Pesticide Runoff Model Schriever 2007 #############
  ####################################################################

  ##########################################################
  ########### START: Pesticide Runoff Map ##################
  ##########################################################
    
    sf_use_s2(F)
    tmap_mode("plot")

    districts_bb <- st_bbox(district_select)
    districts_osmap <- read_osm(districts_bb)

  # Crop map
   crop_map <- tm_shape(districts_osmap) +
      tm_rgb(alpha = 0.5) +
      tm_shape(st_as_sf(district_select)) +
      tm_borders(col = "black",
                 lwd = 2) +
     tm_shape(st_as_sf(gemap_loc)) +
       tm_polygons("Crop",
                   id = "Crop name",
                   palette = "Set2",
                   lwd = 0.2) +
     tm_layout(frame = FALSE,
               legend.outside = TRUE,
               legend.outside.position = "right",
               legend.outside.size = 0.4,
               legend.text.size = 0.6,
               legend.title.size = 0.75,
               main.title = paste0("Crops treated with ",
                                   acsubst_name,
                                   " in ",
                                   district_select$NAZ_LAU1,
                                   " district"),
               main.title.fontface = "bold",
               main.title.size = 1,
               main.title.position = "centre") +
     tm_add_legend(title = paste0(district_select$NAZ_LAU1, " district border"),
                   "line",
                   col = "black",
                   lwd = 2) +
     tm_scale_bar(position = c("right", "bottom")) +
     tmap_options(basemaps = "Esri.WorldTopoMap")
   
  # Map showing AS application rate map

  apprate_as_map <- tm_shape(districts_osmap) +
      tm_rgb(alpha = 0.5) +
    tm_shape(st_as_sf(district_select)) +
      tm_borders(col = "black",
               lwd = 2) +
    tm_shape(st_as_sf(acsubst_application),
             name = "Application rate (g/ha)") +
    tm_polygons("aprate_farm_g.ha",
                title = paste0(acsubst_name,
                               " application rate (g/ha)"),
                palette = "YlOrBr",
                lwd = 0.2) +
      tm_layout(frame = FALSE,
              legend.outside = TRUE,
              legend.outside.position = "right",
              legend.outside.size = 0.4,
              legend.text.size = 0.6,
              legend.title.size = 0.75,
              main.title = paste0(acsubst_name," application rate in ",
                                  district_name,
                                  " district"),
              main.title.fontface = "bold",
              main.title.size = 1,
              main.title.position = "centre") +
      tm_add_legend(title = paste0(district_name, " district border"),
                  "line",
                  col = "black",
                  lwd = 2) +
    tm_scale_bar(position = c("right", "bottom")) +
    tmap_options(basemaps = "Esri.WorldTopoMap")

  # Map showing AS concentration in topsoil map
  soil_as_conc <- tm_shape(districts_osmap) +
    tm_rgb(alpha = 0.5) +
    tm_shape(st_as_sf(district_select)) +
    tm_borders(col = "black",
               lwd = 2) +
    tm_shape(st_as_sf(load_acsubst_farm_mapinput),
            name = "Concentration in soil") +
     tm_polygons("conc_acsubst_total_soil_twa_g.kg",
                 title = paste0(acsubst_name,
                                " concentration in soil (twa) g/kg\nover ",
                                meteo_stations_prec_basins$day |> max(),
                                " days in ",
                                meteo_stations_prec_basins$month |> unique()) ,
                 palette = "YlOrBr",
                 lwd = 0.2) +
     tm_layout(frame = FALSE,
               legend.outside = TRUE,
               legend.outside.position = "right",
               legend.outside.size = 0.4,
               legend.text.size = 0.6,
               legend.title.size = 0.75,
               main.title = paste0(acsubst_name," PECsoil in the ",
                                   district_select$NAZ_LAU1,
                                   " district"),
               main.title.fontface = "bold",
               main.title.size = 1,
               main.title.position = "centre") +
    tm_add_legend(title = paste0(district_select$NAZ_LAU1, " district border"),
                  "line",
                  col = "black",
                  lwd = 2) +
     tm_scale_bar(position = c("right", "bottom")) +
     tmap_options(basemaps = "Esri.WorldTopoMap")

  # Mao showing AS concentration in water
  riverwater_as_conc <- tm_shape(districts_osmap) +
    tm_rgb(alpha = 0.5) +
  tm_shape(st_as_sf(district_select)) +
    tm_borders(col = "black",
               lwd = 2) +
  tm_shape(st_as_sf(rivers_basin)) +
    tm_lines(col = "steelblue1",
             lwd = 1) +
  tm_shape(st_as_sf(rivers_basin_buff_seg)) +
    tm_polygons(alpha = 0.5,
                col = "steelblue3",
                lwd = 0.2) +
  tm_shape(st_as_sf(rivers_basin_buff_seg |> merge(conc_acsubst_river_seg_mapinput)),
           name = "Concentration in river water") +
    tm_polygons("conc_acsubst_mean_river_seg_g.m3.ndays",
                title = paste0(acsubst_name, " concentration in river water (mean) g/kg\nover ",
                               meteo_stations_prec_basins$day |> max(),
                               " days in ",
                               meteo_stations_prec_basins$month |> unique()) ,
                palette = "YlOrBr",
                lwd = 0.2) +
    tm_layout(frame = FALSE,
              legend.outside = TRUE,
              legend.outside.position = "right",
              legend.outside.size = 0.4,
              legend.text.size = 0.6,
              legend.title.size = 0.75,
              main.title = paste0(acsubst_name," PECsw in the ",
                                  district_select$NAZ_LAU1,
                                  " district"),
              main.title.fontface = "bold",
              main.title.size = 1,
              main.title.position = "centre") +
    tm_add_legend(title = paste0(district_select$NAZ_LAU1, " district border"),
                  "line",
                  col = "black",
                  lwd = 2) +
    tm_add_legend(title = "River network (CUZK)",
                  col = "steelblue1",
                  lwd = 1) +
    tm_add_legend(title = paste0(rivbuff_width, " m buffer around river segments (CUZK)"),
                  alpha = 0.1,
                  col = "steelblue3",
                  lwd = 0.2) +
    tm_scale_bar(position = c("right", "bottom")) +
    tmap_options(basemaps = "Esri.WorldTopoMap")

  }


 tmap_save(crop_map,
           paste0(district_name,
                  "_",
                  acsubst_name,
                  "_crop_map.png"),
           dpi = 600,
           units = "cm",
           width = 20,
           height = 10)

 tmap_save(soil_as_conc,
           paste0(district_select$NAZ_LAU1,
                  "_",
                  acsubst_name,
                  "_PECsoil_map.png"),
           dpi = 600,
           units = "cm",
           width = 20,
           height = 10)

 tmap_save(riverwater_as_conc,
           paste0(district_name,
                  "_",
                  acsubst_name,
                  "_PECsw_map.png"),
           dpi = 600,
           units = "cm",
           width = 20,
           height = 10)
 
 tmap_save(apprate_as_map,
           paste0(district_name,
                  "_",
                  acsubst_name,
                  "_apprate_map.png"),
           dpi = 600,
           units = "cm",
           width = 20,
           height = 10)
 
 ########################################################
 ########### END: Pesticide Runoff Map ##################
 ########################################################

}

AS_statmap_topsoil_riverwater("Benešov", "Tebuconazole", "August", 1, 20, 100)


# !!!!Function for creating datasets of simulated ASs concentration in topsoil on all fields for one selected district !!!!!!#

AS_statmap_topsoil <- function(district_name,
                                          acsubst_name,
                                          app_month,
                                          app_startday,
                                          endday,
                                          rivbuff_width){
  
  ## Some initial values for testing
  # district_name <- "Benešov"  
  # basins_nrmin <- 1
  # basins_nrmax <- 7
  # acsubst_name <- "Glyphosate"
  # app_month <- "August"
  # app_startday <- 1
  # endday <- 10
  # rivbuff_width <- 100
  
  ######################################################################
  ############# START: Import and transform input data sets ############
  ######################################################################
  # Districts borders #
  # Districts borders #
  districts <- okresy("low")|>
    mutate(NAZ_LAU1 = str_to_title((gsub("-", " ", NAZ_LAU1, fixed = TRUE)))) |>
    vect()
  
  district_select <- districts |> filter(NAZ_LAU1 %in% district_name)
  
  # River basins #
  ## Hydrosheds river basins. Prepared trimmed file to include only the country of interest
  # basins_eu_path <- dir_ls(path_home_r(), recurse = T, regexp = "BasinATLAS_v10_lev10.gpkg")
  # basins_cz <- vect(basins_eu_path, extent = ext(districts))
  # writeVector(basins_cz, "hydrosheds_lvl10_basins_cz.gpkg")
  
  ### Read basin polygons for the selected district
  basins_cz_distr <- dir_ls(path_home_r(), recurse = T, regexp = "hydrosheds_lvl10_basins_cz.gpkg") |>
    vect() |>
    select("HYBAS_ID", "dis_m3_pyr", "dis_m3_pmn", "dis_m3_pmx", "riv_tc_ssu", "riv_tc_usu") |>
    mask(district_select)
  # Select the maximum of basin areas to be included
  
  basins_nrmax <- readline(prompt = paste0("There are ", nrow(basins_cz_distr), " river basins intersecting this district. Select number of river basins: "))
  
  basins_cz_distr_max <- basins_cz_distr |> slice(1:basins_nrmax) |> terra::split("HYBAS_ID")
  
  # River network #
  ## River network CUZK
  watrcrsL <- paste0(water_spatial_dir, "/CUZK250/WatrcrsL.shp") |>
    vect() |>
    project(districts)
  
  # GeMAP for the selected district  and Active substance #
  gemap_loc <- list()
  for (basin in seq_along(basins_cz_distr_max)) {
    
    gemap_loc[[basin]] <- dir_ls(path_home_r(), recurse = T, regexp = "gemap100") |>
      vect(extent = ext(basins_cz_distr_max[[basin]])) |>
      filter(Active == acsubst_name |> str_to_lower()) |>
      # select("ZKOD", "CTVEREC", "Crop", "EPPO", "EAGRI", "FieldAr", "Active", "ASmass", "ASarea", "ARfarm", "IFav", "IFmin", "IFmax", "District", "ARmin", "ARmax", "BBCHmin", "BBCHmax", "ApFreq") |>
      mutate(District = str_to_title((gsub("_", " ", District, fixed = TRUE)))) |>
      intersect(basins_cz_distr_max[[basin]]["HYBAS_ID"]) |>
      filter(!if_any("IFav", is.na)) |>
      rename(infactor_av = IFav,
             infactor_min = IFmin,
             infactor_max = IFmax,
             acsubst = Active,
             acsubst_mass_kg = ASmass,
             acsubst_area_ha = ASarea,
             field_area = FieldAr,
             aprate_farm_g.ha = ARfarm,
             aprate_min = ARmin,
             aprate_mmax = ARmax,
             apfreq = ApFreq) |>
      mutate(Crop = str_to_sentence(Crop),
             acsubst = str_to_title(acsubst))
    
    cat("\r", basin ,"basin out of", basins_nrmax, "is being processed")
  }
  
  gemap_loc <- gemap_loc |> vect()
  
  # CHMU Rainfall #
  # For the moment: rainfall data remain separate dataset to be later connected with the crop BBCH and scheduled in PPP registry AS application
  meteo_stations <- read.csv(dir_ls(path_home_r(),
                                    recurse = T,
                                    regexp = "stanice_souradnice.csv"),
                             sep = ";") |>
    as_tibble() |>
    mutate(across(3:5,
                  \(x) str_replace_all(x,
                                       pattern = ",",
                                       replacement =  ".")),
           Elevation = as.numeric(Elevation),
           Geogr1 = as.numeric(Geogr1),
           Geogr2 = as.numeric(Geogr2)) |>
    add_column(Elevation_unit = "m")
  
  Sys.setlocale("LC_TIME", "uk")
  
  meteo_stations_prec_basins <- dir_ls(path_home_r(),
                                       recurse = T,
                                       regexp = "srazky_SRA.csv") |>
    read.csv(sep = ";",
             dec = ",") |>
    as_tibble() |>
    mutate(value = str_replace(value, ",", "."),
           value = as.numeric(value),
           month = make_datetime(month = month) |>
             lubridate::month(label = T,
                              abbr = F,
                              locale = Sys.getlocale("LC_TIME"))) |>
    rename("rain_mm.day" = "value") |>
    full_join(meteo_stations,
              by = join_by(id == id),
              relationship = "many-to-many") |>
    filter(!is.na(rain_mm.day),
           rain_mm.day >= 0,
           # Filter the month for which to run daily simulations
           # Filter number of days: from first day of AS application to several day after
           month == app_month, between(day, app_startday, endday)) |>
    # Average daily rainfall in selected basins calculated from stations within basin polygons
    vect(crs = "WGS84", geom = c("Geogr1", "Geogr2")) |>
    mask(basins_cz_distr) |>
    as_tibble() |>
    group_by(month, day) |>
    summarise(rain_mean_mm.day = mean(rain_mm.day),
              rain_min_mm.day = min(rain_mm.day),
              rain_max_mm.day = max(rain_mm.day))
  
  # Terrain slope #
  # Data cropped to include only country of interest and saved. Done only once
  ## FAO
  ### 5min resolution
  # slope5min_path <- dir_info(water_spatial_dir, regexp = "_5min.asc")
  # slopesCl1_8_5min <- map(slope5min_path$path, rast)
  # slopesCl1_8_5min_cz <- map(slopesCl1_8_5min, crop, basins_cz)
  # slopes_all5min_cz <- sprc(slopesCl1_8_5min_cz) |> terra::mosaic(fun = "mean")
  # writeCDF(slopes_all5min_cz,filename = paste0(water_spatial_dir,"/TerrainSlope_5min_CZ.nc"), overwrite = T)
  slope_cz_5min_path <- dir_ls(path_home_r(),
                               recurse = T,
                               regexp = "TerrainSlope_5min_CZ.nc$")
  slope_cz_5min <- rast(slope_cz_5min_path) |>
    mask(gemap_loc)
  
  ### 30as resolution
  # slope30as_path <- dir_info(water_spatial_dir, regexp = "_30as.asc")
  # slopesCl1_8_30as <- map(slope30as_path$path, rast)
  # slopesCl1_8_30as_cz <- map(slopesCl1_8_30as, crop, basins_cz)
  # slopes_all30as_cz <- sprc(slopesCl1_8_30as_cz) |> terra::mosaic()
  # writeCDF(slopes_all30as_cz, filename = paste0(water_spatial_dir,"/TerrainSlope_30as_CZ.nc"), overwrite = T)
  # slope_cz_30as_path <- paste0(water_spatial_dir, "/TerrainSlope_30as_CZ.nc")
  # slope_cz_30as <- rast(slope_cz_30as_path)
  
  ### Terrain slope maps
  # slope_class <- c("0% ≤ slope ≤ 0.5%", "0.5% ≤ slope ≤ 2%", "2% ≤ slope ≤ 5%", "5% ≤ slope ≤ 10%", "10% ≤ slope ≤ 15%", "15% ≤ slope ≤ 30%",
  #                  "30% ≤ slope ≤ 45%", "Slope > 45%")
  
  # slope5min <- tm_shape(mask(slopes_all5min_cz/100, st_as_sf(districts))) +
  #   tm_raster(col = "GloSlopesCl1_5min",
  #             palette = "Greens",
  #             style = "equal",
  #             n = 8,
  #             labels = slope_class) +
  # tm_shape(st_as_sf(districts)) +
  #   tm_borders(col="black",
  #              lwd = 1) +
  #   tm_layout(legend.outside = T) +
  #   tm_scale_bar(position = "left")
  #
  # slope30as <- tm_shape(mask(slopes_all30as_cz, st_as_sf(districts))) +
  #   tm_raster(col ="GloSlopesCl1_30as",
  #             palette = "Greens",
  #             style = "equal",
  #             labels = slope_class,
  #             n = 8) +
  #   tm_shape(st_as_sf(districts)) +
  #   tm_borders(col="black",
  #              lwd = 1) +
  #   tm_layout(legend.outside = T) +
  #   tm_scale_bar(position = "left")
  
  # ESDAC Organic carbon content in topsoil #
  # Data cropped to include only country of interest and saved. Done only once
  
  # oc_jrc <- rast(oc_jrc_path)
  # crs(oc_jrc) <- crs(srunoff_path |> rast())
  # oc_jrc <- oc_jrc |> project(srunoff_path |> rast() |> project(basins_cz))
  # oc_jrc_cz <- crop(oc_jrc, basins_cz)
  # writeCDF(oc_jrc_cz, filename = paste0(water_spatial_dir ,"/OC_jrc_CZ.nc"), overwrite = T)
  
  orcarb_jrc_cz_path <- dir_ls(path_home_r(),
                               recurse = T,
                               regexp ="OC_jrc_CZ.nc$")
  orcarb_jrc_cz <- rast(orcarb_jrc_cz_path) |>
    select("OC_jrc_CZ") |>
    crop(gemap_loc)
  
  # ESDAC topsoil physical properties for Europe (based on LUCAS topsoil data) #
  
  # sand_jrc_path <- paste0(water_spatial_dir, "Sand1.tif")
  # sand_jrc_laea <- rast(sand_jrc_path)
  # sand_jrc_cz_wgs84 <- sand_jrc_laea |>  project(basins_cz) |> crop(basins_cz)
  # writeCDF(sand_jrc_cz_wgs84, filename = paste0(water_spatial_dir ,"sand_jrc_CZ.nc"), overwrite = T)
  
  sand_jrc_cz_path <- dir_ls(path_home_r(),
                             recurse = T,
                             regexp = "sand_jrc_CZ.nc$")
  sand_jrc_cz <- rast(sand_jrc_cz_path) |>
    crop(gemap_loc)
  
  # clay_jrc_path <- paste0(water_spatial_dir, "Clay.tif")
  # clay_jrc_laea <- rast(clay_jrc_path)
  # clay_jrc_cz_wgs84 <- clay_jrc_laea |>  project(basins_cz) |> crop(basins_cz)
  # writeCDF(clay_jrc_cz_wgs84, filename = paste0(water_spatial_dir ,"clay_jrc_CZ.nc"), overwrite = T)
  
  clay_jrc_cz_path <- dir_ls(path_home_r(),
                             recurse = T,
                             regexp = "clay_jrc_CZ.nc$")
  clay_jrc_cz <- rast(clay_jrc_cz_path) |>
    crop(gemap_loc)
  
  # budens_jrc_path <- paste0(water_spatial_dir, "Bulk_density.tif")
  # budens_jrc_laea <- rast(budens_jrc_path)
  # budens_jrc_cz_wgs84 <- budens_jrc_laea |>  project(basins_cz) |> crop(basins_cz)
  # writeCDF(budens_jrc_cz_wgs84, filename = paste0(water_spatial_dir ,"budens_jrc_CZ.nc"), overwrite = T)
  
  budens_jrc_cz_path <- dir_ls(path_home_r(),
                               recurse = T,
                               regexp = "budens_jrc_cz.nc$")
  budens_jrc_cz <- rast(budens_jrc_cz_path) |>
    crop(gemap_loc)
  
  # Chemical input data from qsars (vega, epi) and PPDB where available #
  chemprop <- dir_ls(path_home_r(),
                     recurse = T,
                     regexp = "chem_prop_ppdb.csv$") |>
    fread() |>
    rename(acsubst_name = active_name) |> 
    mutate(across(4:last_col(), as.numeric)) |>
    select(acsubst_name, Kfoc_ml.g, DT50_field_d) |> 
    filter(!is.na(Kfoc_ml.g),
           !is.na(DT50_field_d))
  
  ####################################################################
  ############# END: Import and transform input data sets ############
  ####################################################################
  
  ########################################################################################
  ############ START: Spatial input data intersected with river segment ##################
  ########################################################################################
  # Intersect river network with river basin(s)
  rivers_basin <- terra::intersect(watrcrsL[c("HYDROID", "NAMN1", "SHAPE_Leng", "WD7")], basins_cz_distr)
  # Indicate buffer width around a river segment
  rivers_basin_buff_seg <- rivers_basin |> terra::buffer(rivbuff_width+rivers_basin$WD7)
  # Intersect gemap with selected river segments so the GeMAP is expanded to include hydrography of the selected river basin(s)
  gemap_loc_rivers_buff <- gemap_loc[rivers_basin_buff_seg]
  
  if(gemap_loc_rivers_buff |> nrow() == 0) {
    
    "Warning: There are no fields in the buffer, increase buffer width or select another basin"
    
  }  else {
    
    # Extract data from raster maps intersected by river segments
    orcarb_river_seg <- terra::zonal(orcarb_jrc_cz,
                                     gemap_loc,
                                     fun = "mean",
                                     # weights = T,
                                     # exact = T,
                                     as.polygons = T) |>
      rename("orcarb_perc" = "OC_jrc_CZ") |>
      mutate(orcarb_perc = orcarb_perc/100)
    
    # srunoff_river_seg <- terra::zonal(srunoff_basin,
    #                                   gemap_loc_rivers_seg,
    #                                   fun = "mean",
    #                                   # weights = T,
    #                                   # exact = T,
    #                                   as.polygons = T) |>
    #   rename("SR" = "srunoff_CZ")
    
    slope_river_seg <- terra::zonal(slope_cz_5min,
                                    gemap_loc,
                                    fun = "mean",
                                    # weights = T,
                                    # exact = T,
                                    as.polygons = TRUE) |>
      rename("slope_perc" = "TerrainSlope_5min_CZ") |>
      mutate(slope_perc = slope_perc/1000/100)
    
    sand_river_seg <- terra::zonal(sand_jrc_cz,
                                   gemap_loc,
                                   fun = "mean",
                                   # weights = T,
                                   # exact = T,
                                   as.polygons = TRUE) |>
      rename("sand_perc" = "sand_jrc_CZ")
    
    
    clay_river_seg <- terra::zonal(clay_jrc_cz,
                                   gemap_loc,
                                   fun = "mean",
                                   # weights = T,
                                   # exact = T,
                                   as.polygons = TRUE) |>
      rename("clay_perc" = "clay_jrc_CZ")
    
    budens_river_seg <- terra::zonal(budens_jrc_cz,
                                     gemap_loc,
                                     fun = "mean",
                                     # weights = T,
                                     # exact = T,
                                     as.polygons = TRUE) |>
      rename("bulk_dens" = "budens_jrc_CZ")
    
    # 4th order river basins crossing with the river segment
    # basin4 <- terra::mask(basins4ord, rivers_basin3_buff_seg)
    # gemap_rivers_basin4_seg <- gemap |>
    #     terra::intersect(basin4)
    
    # Patch size of arable land in a river segment
    district_rivers_basins <- mask(districts, rivers_basin_buff_seg) |>
      values() |>
      select(NAZ_LAU1)
    
    crop_area_tot_river_buff <- gemap_loc_rivers_buff |>
      values() |>
      group_by(District, Crop, acsubst) |>
      summarise(crop_area_river_buff_ha = sum(field_area)) |>
      ungroup()
    
    crop_area_tot_distr <- gemap_loc |>
      values() |>
      filter(District %in% district_rivers_basins$NAZ_LAU1) |>
      filter(Crop %in% crop_area_tot_river_buff$Crop) |>
      group_by(District, acsubst, Crop) |>
      summarise(crop_area_distr_ha = sum(field_area)) |>
      ungroup()
    
    # District and crop specific application rate per river segment
    acsubst_application <- gemap_loc |>
      left_join(crop_area_tot_distr, by = join_by(acsubst, Crop, District)) |>
      left_join(crop_area_tot_river_buff, by = join_by(acsubst, Crop, District)) |>
      mutate(acsubst_mass_g = acsubst_mass * 1000,
             acsubst_area_ha = acsubst_area,
             aprate_farm_g.ha = aprate_farm,
             crop_acsubst_area_frac_distr = acsubst_area_ha / crop_area_distr_ha,
             crop_acsubst_area_river_buff_ha = crop_area_river_buff_ha * crop_acsubst_area_frac_distr,
             mass_acsubst_river_buff_g = crop_acsubst_area_river_buff_ha * aprate_farm_g.ha) |>
      left_join(chemprop,
                by = c("acsubst" = "acsubst_name"))
    
    #######################################################################################
    ############ END: Spatial input data intersected with river segment ###################
    #######################################################################################
    
    ####################################################################
    ########### START: Pesticide Runoff Model Schriever 2007 ###########
    ####################################################################
    
    # Model subroutines related to pesticide runoff from individual farms in the whole district
    load_acsubst_farm <- acsubst_application |>
      cbind(orcarb_river_seg$orcarb_perc |>
              as.data.frame()) |>
      cbind(sand_river_seg$sand_perc |>
              as.data.frame()) |>
      cbind(budens_river_seg$bulk_dens |>
              as.data.frame()) |>
      cbind(clay_river_seg$clay_perc |>
              as.data.frame()) |>
      cbind(slope_river_seg$slope_perc |>
              as.data.frame()) |>
      rename("clay_perc" = "clay_river_seg$clay_perc",
             "sand_perc" = "sand_river_seg$sand_perc",
             "bulk_dens_kg.dm3" = "budens_river_seg$bulk_dens",
             "oc_perc" = "orcarb_river_seg$orcarb_perc",
             "slope_perc" = "slope_river_seg$slope_perc") |>
      ## Effect of crop interception factor
      mutate(infactor_effect = map_dbl(infactor_av,
                                       ~1-(./100))) |>
      ## Soil and chemical interaction
      mutate(frac_asubst_soil_water_ini = map2_dbl(oc_perc,
                                                   Kfoc_ml.g,
                                                   ~1/(1 + (.x*.y)/100))) |>
      mutate(frac_asubst_soil_solid_ini = map2_dbl(oc_perc,
                                                   Kfoc_ml.g,
                                                   ~((.x*.y)/100)/(1 + (.x*.y/1000)/100))) |>
      ## Initial concentration in soil
      mutate(conc_acsubst_total_soil_ini = pmap_dbl(list(x = aprate_farm_g.ha,
                                                         y = bulk_dens_kg.dm3,
                                                         z = frac_asubst_soil_water_ini,
                                                         v = frac_asubst_soil_solid_ini),
                                                    \(x,y,z,v) (x/(y*1000*20))*(z+v))) |>
      ## Effect of terrain slope
      mutate(slope_effect = map_dbl(slope_perc,
                                    ~if_else(. <= 20,
                                             0.001423 * .^2 + 0.02153 * .,
                                             1))) |>
      ## Active substance application rate in a river segment
      mutate(aprate_river_buff = map2_dbl(mass_acsubst_river_buff_g,
                                          crop_area_river_buff_ha,
                                          ~.x/.y)) |>
      ## Product of AS runoff components
      mutate(load_acsubst_prod = pmap_dbl(list(crop_area_river_buff_ha,
                                               aprate_river_buff,
                                               infactor_effect,
                                               frac_asubst_soil_water_ini,
                                               slope_effect),
                                          prod))
    
    ## Fraction of daily generated surface runoff. Mean, min, max are calculated over meteorological station within the basins
    ## Add code to match rainfall to AS application period for a given crop!!!!
    srunoff_mean_sandy_mm.day <- map_dbl(meteo_stations_prec_basins$rain_mean_mm.day,
                                         ~ (-0.016427-0.011377*.+0.0026284*.^2-5.8564*10^-6*.^3))
    srunoff_mean_loamy_mm.day  <- map_dbl(meteo_stations_prec_basins$rain_mean_mm.day,
                                          ~ (-0.061108-0.0041626*.+0.0040395*.^2-9.0361*10^-6*.^3))
    srunoff_min_sandy_mm.day  <- map_dbl(meteo_stations_prec_basins$rain_min_mm.day,
                                         ~ (-0.016427-0.011377*.+0.0026284*.^2-5.8564*10^-6*.^3))
    srunoff_min_loamy_mm.day  <- map_dbl(meteo_stations_prec_basins$rain_min_mm.day,
                                         ~ (-0.061108-0.0041626*.+0.0040395*.^2-9.0361*10^-6*.^3))
    srunoff_max_sandy_mm.day  <- map_dbl(meteo_stations_prec_basins$rain_max_mm.day,
                                         ~ (-0.016427-0.011377*.+0.0026284*.^2-5.8564*10^-6*.^3))
    srunoff_max_loamy_mm.day  <- map_dbl(meteo_stations_prec_basins$rain_max_mm.day,
                                         ~ (-0.061108-0.0041626*.+0.0040395*.^2-9.0361*10^-6*.^3))
    
    srunoffs <- tibble(srunoff_mean_sandy_mm.day ,
                       srunoff_mean_loamy_mm.day ,
                       srunoff_min_sandy_mm.day ,
                       srunoff_min_loamy_mm.day ,
                       srunoff_max_sandy_mm.day ,
                       srunoff_max_loamy_mm.day ) |>
      bind_cols(meteo_stations_prec_basins) |>
      mutate(across(starts_with("srunoff"), ~ case_when(.x < 0 ~ 0, .default = .x)))
    # filter(srunoff_max_loamy_mm.day > 0 & srunoff_max_sandy_mm.day > 0)
    
    
    ## Daily amount of pesticide potentially reaching a stream from individual farms
    srunoff_acsubst_farm <- list()
    for (i in seq_along(load_acsubst_farm)) {
      
      srunoff_acsubst_farm[[i]] <-  merge(load_acsubst_farm[i] , srunoffs) |>
        mutate(srunoff_mean_fraction = case_when(sand_perc > clay_perc ~ srunoff_mean_sandy_mm.day/rain_mean_mm.day,
                                                 sand_perc < clay_perc ~ srunoff_mean_loamy_mm.day/rain_mean_mm.day),
               srunoff_min_fraction = case_when(sand_perc > clay_perc ~ srunoff_min_sandy_mm.day/rain_min_mm.day,
                                                sand_perc < clay_perc ~ srunoff_min_loamy_mm.day/rain_min_mm.day),
               srunoff_max_fraction = case_when(sand_perc > clay_perc ~ srunoff_max_sandy_mm.day/rain_max_mm.day,
                                                sand_perc < clay_perc ~ srunoff_max_loamy_mm.day/rain_max_mm.day),
               across(starts_with("srunoff"), ~ case_when(.x == "NaN" ~ 0, .default = .x)),
               ndays = max(meteo_stations_prec_basins$day),
               frac_asubst_soil_water_lag = exp(-day * log(2) / DT50_field_d) * frac_asubst_soil_water_ini,
               frac_asubst_soil_solid_lag = exp(-day * log(2) / DT50_field_d) * frac_asubst_soil_solid_ini,
               conc_acsubst_total_soil_lag_g.kg = (frac_asubst_soil_water_lag + frac_asubst_soil_solid_lag) * conc_acsubst_total_soil_ini,
               conc_acsubst_total_soil_twa_g.kg = (conc_acsubst_total_soil_ini / (ndays * (log(2) / DT50_field_d))) * (1 - exp(-ndays * (log(2) / DT50_field_d))),
               load_acsubst_prod = pmap_dbl(list(crop_area_river_buff_ha,
                                                 aprate_river_buff,
                                                 infactor_effect,
                                                 frac_asubst_soil_water_lag,
                                                 slope_effect),
                                            prod),
               rain_mean_mm.ndays = mean(rain_mean_mm.day),
               rain_min_mm.ndays = mean(rain_min_mm.day),
               rain_max_mm.ndays = mean(rain_max_mm.day),
               load_acsubst_mean_g.day = load_acsubst_prod * srunoff_mean_fraction,
               load_acsubst_min_g.day = load_acsubst_prod * srunoff_min_fraction,
               load_acsubst_max_g.day = load_acsubst_prod * srunoff_max_fraction,
               load_acsubst_mean_g.ndays = mean(load_acsubst_mean_g.day),
               load_acsubst_min_g.ndays = mean(load_acsubst_min_g.day),
               load_acsubst_max_g.ndays = mean(load_acsubst_max_g.day))
      
      # rename("srunoff_day" = "day")
      
      cat("\r", i ,"farm in", district_name, "out of", nrow(load_acsubst_farm), "is being processed")
      
    }
    
    # Selecting only unique mean, max min load values for each farm
    load_acsubst_farm_mapinput <- srunoff_acsubst_farm |>
      map(\(x) distinct(x,
                        load_acsubst_mean_g.ndays,
                        load_acsubst_min_g.ndays,
                        load_acsubst_max_g.ndays,
                        conc_acsubst_total_soil_twa_g.kg,
                        acsubst,
                        month,
                        ndays),
          .progress = T) |>
      vect()
    
    # Aggregate concentration for a river segment. Connecting farm pesticide loads to the respective river segments
    conc_acsubst_river_seg_mapinput <- load_acsubst_farm_mapinput |>
      makeValid() |>
      terra::intersect(rivers_basin_buff_seg) |>
      group_by(SHAPE_Leng, HYDROID, acsubst, month, ndays, dis_m3_pyr, dis_m3_pmn, dis_m3_pmx) |>
      summarise(conc_acsubst_mean_river_seg_g.m3.ndays = mean(load_acsubst_mean_g.ndays/dis_m3_pyr),
                conc_acsubst_min_river_seg_g.m3.ndays = mean(load_acsubst_min_g.ndays/dis_m3_pmn),
                conc_acsubst_max_river_seg_g.m3.ndays= mean(load_acsubst_max_g.ndays/dis_m3_pmx))
    
    # # Loops to debug some error with intersecting farms and river buffers
    # farms_buffers <- list()
    #
    # for(farm in seq_along(load_acsubst_farm_mapinput)){
    #
    #   farms_buffers[[farm]] <- load_acsubst_farm_mapinput[farm, ] |>
    #     terra::intersect(rivers_basin_buff_seg)
    #
    #   cat("\r", farm ,"farm out of", nrow(load_acsubst_farm_mapinput), "is being processed")
    #
    #
    # }
    #
    # rivseg_buffers <- list()
    #
    # for(rivseg in seq_along(rivers_basin_buff_seg)){
    #
    #   rivseg_buffers[[rivseg]] <- load_acsubst_farm_mapinput |>
    #     terra::intersect(rivers_basin_buff_seg[rivseg])
    #
    #   cat("\r", rivseg ,"river segment out of", nrow(rivers_basin_buff_seg), "is being processed")
    #
    #
    # }
    # #Minimal example of aggregating vectors.
    # load_acsubst_farm_mapinput_seg1 <- load_acsubst_farm_mapinput[rivers_basin_buff_seg1]
    # rivers_basin_buff_seg1 <- rivers_basin_buff_seg |> filter(HYDROID == "N.CZ.WATRCRS.128870000100")
    #
    # rivers_basin_buff_seg |> plot()
    # aggregate(rivers_basin_buff_seg, "SHAPE_Leng") |> plot(border = "blue")
    # intersect(load_acsubst_farm_mapinput) |> plot(col = "red", add = T)
    #
    # # Quick check how fields are intersected with the river buffer
    # rivers_basin_buff_seg1 |> plot(border= "blue")
    # plot(load_acsubst_farm_mapinput_seg1 |> terra::intersect(rivers_basin_buff_seg1[c("HYDROID", "dis_m3_pyr", "dis_m3_pmn", "dis_m3_pmx")]), border ="green4", add = T)
    # symdif(load_acsubst_farm_mapinput_seg1, intersect(rivers_basin_buff_seg1, load_acsubst_farm_mapinput_seg1)) |> plot(col = "red", add = T)
    # plot(rivers_basin_buff_seg1[load_acsubst_farm_mapinput_seg1], border="green", add = T)
    # plot(gemap_loc_rivers_buff, col = "green", add = T)
    
    ####################################################################
    ########### END: Pesticide Runoff Model Schriever 2007 #############
    ####################################################################
    
    ##########################################################
    ########### START: Pesticide Runoff Map ##################
    ##########################################################
    
    sf_use_s2(F)
    tmap_mode("plot")
    
    districts_bb <- st_bbox(district_select)
    districts_osmap <- read_osm(districts_bb)
    
    # Crop map
    crop_map <- tm_shape(districts_osmap) +
      tm_rgb(alpha = 0.5) +
      tm_shape(st_as_sf(district_select)) +
      tm_borders(col = "black",
                 lwd = 2) +
      tm_shape(st_as_sf(gemap_loc)) +
      tm_polygons("Crop",
                  id = "Crop name",
                  palette = "Set2",
                  lwd = 0.2) +
      tm_layout(frame = FALSE,
                legend.outside = TRUE,
                legend.outside.position = "right",
                legend.outside.size = 0.4,
                legend.text.size = 0.6,
                legend.title.size = 0.75,
                main.title = paste0("Crops treated with ",
                                    acsubst_name,
                                    " in ",
                                    district_select$NAZ_LAU1,
                                    " district"),
                main.title.fontface = "bold",
                main.title.size = 1,
                main.title.position = "centre") +
      tm_add_legend(title = paste0(district_select$NAZ_LAU1, " district border"),
                    "line",
                    col = "black",
                    lwd = 2) +
      tm_scale_bar(position = c("right", "bottom")) +
      tmap_options(basemaps = "Esri.WorldTopoMap")
    
    # Map showing AS application rate map
    
    apprate_as_map <- tm_shape(districts_osmap) +
      tm_rgb(alpha = 0.5) +
      tm_shape(st_as_sf(district_select)) +
      tm_borders(col = "black",
                 lwd = 2) +
      tm_shape(st_as_sf(acsubst_application),
               name = "Application rate (g/ha)") +
      tm_polygons("aprate_farm_g.ha",
                  title = paste0(acsubst_name,
                                 " application rate (g/ha)"),
                  palette = "YlOrBr",
                  lwd = 0.2) +
      tm_layout(frame = FALSE,
                legend.outside = TRUE,
                legend.outside.position = "right",
                legend.outside.size = 0.4,
                legend.text.size = 0.6,
                legend.title.size = 0.75,
                main.title = paste0(acsubst_name," application rate in ",
                                    district_name,
                                    " district"),
                main.title.fontface = "bold",
                main.title.size = 1,
                main.title.position = "centre") +
      tm_add_legend(title = paste0(district_name, " district border"),
                    "line",
                    col = "black",
                    lwd = 2) +
      tm_scale_bar(position = c("right", "bottom")) +
      tmap_options(basemaps = "Esri.WorldTopoMap")
    
    # Map showing AS concentration in topsoil map
    soil_as_conc <- tm_shape(districts_osmap) +
      tm_rgb(alpha = 0.5) +
      tm_shape(st_as_sf(district_select)) +
      tm_borders(col = "black",
                 lwd = 2) +
      tm_shape(st_as_sf(load_acsubst_farm_mapinput),
               name = "Concentration in soil") +
      tm_polygons("conc_acsubst_total_soil_twa_g.kg",
                  title = paste0(acsubst_name,
                                 " concentration in soil (twa) g/kg\nover ",
                                 meteo_stations_prec_basins$day |> max(),
                                 " days in ",
                                 meteo_stations_prec_basins$month |> unique()) ,
                  palette = "YlOrBr",
                  lwd = 0.2) +
      tm_layout(frame = FALSE,
                legend.outside = TRUE,
                legend.outside.position = "right",
                legend.outside.size = 0.4,
                legend.text.size = 0.6,
                legend.title.size = 0.75,
                main.title = paste0(acsubst_name," PECsoil in the ",
                                    district_select$NAZ_LAU1,
                                    " district"),
                main.title.fontface = "bold",
                main.title.size = 1,
                main.title.position = "centre") +
      tm_add_legend(title = paste0(district_select$NAZ_LAU1, " district border"),
                    "line",
                    col = "black",
                    lwd = 2) +
      tm_scale_bar(position = c("right", "bottom")) +
      tmap_options(basemaps = "Esri.WorldTopoMap")
    
    # Mao showing AS concentration in water
    riverwater_as_conc <- tm_shape(districts_osmap) +
      tm_rgb(alpha = 0.5) +
      tm_shape(st_as_sf(district_select)) +
      tm_borders(col = "black",
                 lwd = 2) +
      tm_shape(st_as_sf(rivers_basin)) +
      tm_lines(col = "steelblue1",
               lwd = 1) +
      tm_shape(st_as_sf(rivers_basin_buff_seg)) +
      tm_polygons(alpha = 0.5,
                  col = "steelblue3",
                  lwd = 0.2) +
      tm_shape(st_as_sf(rivers_basin_buff_seg |> merge(conc_acsubst_river_seg_mapinput)),
               name = "Concentration in river water") +
      tm_polygons("conc_acsubst_mean_river_seg_g.m3.ndays",
                  title = paste0(acsubst_name, " concentration in river water (mean) g/kg\nover ",
                                 meteo_stations_prec_basins$day |> max(),
                                 " days in ",
                                 meteo_stations_prec_basins$month |> unique()) ,
                  palette = "YlOrBr",
                  lwd = 0.2) +
      tm_layout(frame = FALSE,
                legend.outside = TRUE,
                legend.outside.position = "right",
                legend.outside.size = 0.4,
                legend.text.size = 0.6,
                legend.title.size = 0.75,
                main.title = paste0(acsubst_name," PECsw in the ",
                                    district_select$NAZ_LAU1,
                                    " district"),
                main.title.fontface = "bold",
                main.title.size = 1,
                main.title.position = "centre") +
      tm_add_legend(title = paste0(district_select$NAZ_LAU1, " district border"),
                    "line",
                    col = "black",
                    lwd = 2) +
      tm_add_legend(title = "River network (CUZK)",
                    col = "steelblue1",
                    lwd = 1) +
      tm_add_legend(title = paste0(rivbuff_width, " m buffer around river segments (CUZK)"),
                    alpha = 0.1,
                    col = "steelblue3",
                    lwd = 0.2) +
      tm_scale_bar(position = c("right", "bottom")) +
      tmap_options(basemaps = "Esri.WorldTopoMap")
    
  }
  
  
  tmap_save(crop_map,
            paste0(district_name,
                   "_",
                   acsubst_name,
                   "_crop_map.png"),
            dpi = 600,
            units = "cm",
            width = 20,
            height = 10)
  
  tmap_save(soil_as_conc,
            paste0(district_select$NAZ_LAU1,
                   "_",
                   acsubst_name,
                   "_PECsoil_map.png"),
            dpi = 600,
            units = "cm",
            width = 20,
            height = 10)
  
  tmap_save(riverwater_as_conc,
            paste0(district_name,
                   "_",
                   acsubst_name,
                   "_PECsw_map.png"),
            dpi = 600,
            units = "cm",
            width = 20,
            height = 10)
  
  tmap_save(apprate_as_map,
            paste0(district_name,
                   "_",
                   acsubst_name,
                   "_apprate_map.png"),
            dpi = 600,
            units = "cm",
            width = 20,
            height = 10)
  
  ########################################################
  ########### END: Pesticide Runoff Map ##################
  ########################################################
  
}

AS_statmap_topsoil_riverwater("Benešov", "Tebuconazole", "August", 1, 20, 100)


# Risk map 
soil_rq_map <- dir_ls(path_home_r(),
                        recurse = T,
                        regexp = "Benešov_RQ_soil.gpkg") |>
  vect() |> 
  select(HYBAS_ID,  
         ZKOD, 
         CTVEREC,  
         acsubst,    
         Crop, 
         month, 
         ndays,
         conc_acsubst_total_soil_twa_g.kg)

rq <- dir_ls(path_home_r(),
             recurse = T,
             regexp = "Benešov_topsoil_allchem_farm_RQ") |> fread() |> select(-1)


soil_rq_map <- merge(soil_conc_map, rq)
writeVector(soil_rq_map, "Benešov_RQ_soil.gpkg")


# AS RQ for soil map
sf_use_s2(F)
tmap_mode("plot")

districts_bb <- st_bbox(RCzechia::okresy() |> filter(NAZ_LAU1 == "Benešov"))
districts_osmap <- read_osm(districts_bb)

rq_bene_gly <- tm_shape(districts_osmap) +
  tm_rgb(alpha = 0.5) +
tm_shape(RCzechia::okresy() |> filter(NAZ_LAU1 == "Benešov")) +
  tm_borders(col = "black",
             lwd = 2) +
tm_shape(soil_rq_map |>
           filter(acsubst == "Glyphosate") |>
           mask(vect(RCzechia::okresy() |> filter(NAZ_LAU1 == "Benešov"))))+
  tm_polygons("RQ",
              name = "RQ soil",
              fill.scale = tm_scale_intervals(value.na = "#f9f9f9" ,
                                              label.na = "Missing values",
                                              values = "brewer.bu_pu"),
              fill.legend = tm_legend(position = c("top", "right")))  +
tm_title("Topsoil risk quotient (RQ soil) for individual fields applying glyphosate in Benešov") +
tm_scale_bar(position = c("right", "bottom")) +
  tmap_options(basemaps = "Esri.WorldTopoMap")

tmap_save(rq_bene_gly,
          "Benešov_Glyphosate_soil.png",
          dpi = 600,
          units = "cm",
          scale = 0.75)

##################################################################
########### START: Surface water monitoring stations #############
##################################################################

crop_soilconc_plt <- function(as) {
  
  pec_soil <- dir_ls(path_home_r(),
                     recurse = T,
                     regexp = "Benešov_RQ_soil.gpkg") |>
    vect() |> 
    filter(acsubst %in% as) |> 
    mutate(conc_soil_pec_ug.kg = conc_acsubst_total_soil_twa_g.kg*1000) |>
    values() |> 
    group_by(acsubst, Crop) |> 
    summarise(conc_soil_pec_ug.kg_mean_crop =  conc_soil_pec_ug.kg |> mean(),
              std_pec = sd(conc_soil_pec_ug.kg),
              n_field_pred = n())
  
  # Add PECsoil data collected by Vera
  
  obs_soil <- bind_rows(
    tibble(acsubst = "Glyphosate",
           conc_ug.kg = c(21.3, 46.7, 22.2, 42.1)),
    tibble(acsubst = "Tebuconazole",
           conc_ug.kg = c(88.6, 54.1, 27.2,  23.3,  27.5, 27.4, 47.4, 109.1,24.8)),
    tibble(acsubst = "Acetamiprid",
           conc_ug.kg = 20),
  ) |> cbind(tibble(Crop = "Oilseed (whole country)",
                    Source = "Knuth et al. 2024")) |> 
    filter(acsubst %in% as) |> 
    mutate(conc_soil_obs_ug.kg = conc_ug.kg) |>
    group_by(acsubst, Crop) |> 
    summarise(conc_soil_obs_ug.kg_mean_crop =  conc_soil_obs_ug.kg |> mean(),
              std_obs = sd(conc_soil_obs_ug.kg),
              n_field_obs = n())
  
  pec_obs_soil <- bind_rows(pec_soil, obs_soil)
  
  soilconc.plt <- ggplot(pec_obs_soil) +
    geom_pointrange(aes(x = conc_soil_pec_ug.kg_mean_crop,
                        y = Crop,
                        xmin = conc_soil_pec_ug.kg_mean_crop - std_pec,
                        xmax = conc_soil_pec_ug.kg_mean_crop + std_pec)) +
    geom_text(aes(x = conc_soil_pec_ug.kg_mean_crop,
                  y = Crop,
                  label = paste0("(",n_field_pred,")"),
                  fontface = "italic"),
              nudge_y = 0.3,
              size = 3) +
    geom_pointrange(aes(conc_soil_obs_ug.kg_mean_crop,
                        y = Crop,
                        xmin = conc_soil_obs_ug.kg_mean_crop - std_obs,
                        xmax = conc_soil_obs_ug.kg_mean_crop + std_obs,
                   colour = "salmon")) +
    geom_text(aes(x = conc_soil_obs_ug.kg_mean_crop,
                  y = Crop,
                  label = paste0("(",n_field_obs,")"),
                  fontface = "italic",
                  colour = "salmon"),
              nudge_y = 0.3,
              size = 3) +
    scale_colour_identity() +
    ggtitle(paste0("Benešov: PEC topsoil individual fields"))+
    xlab(expression("\u00B5g \u00D7 kg"^-1)) +
    ylab("") +
    facet_wrap(~acsubst, scales = "free_x") +
    theme_bw()
    
  soilconc.plt
  
}

crop_riverconc_plt <- function(as) {
  
  pec_riv <- dir_ls(path_home_r(),
                    recurse = T,
                    regexp = "Benesov_river_allchem_farm") |>
    vect() |>
    filter(acsubst %in% as) |> 
    rename(conc_river_mg.dm3_pred = conc_acsubst_mean_river_seg_g.m3.ndays) |> 
    values() |> 
    select(acsubst, conc_river_mg.dm3_pred) |> 
    bind_cols(tibble(type = "Predicted")) 
  
  
  obs_riv <- dir_ls(path_home_r(),
                    recurse = T,
                    regexp = "2021 water monitoring data CZ") |>
    read_excel() |>
    filter(!is.na(lon)) |>
    select(1:15) |>
    rename(acsubst = Active) |> 
    filter(acsubst %in% as) |> 
    # mutate(across(starts_with("conc"), ~.x/1000), 
    #        unit = "mg/dm3") |>
    vect(geom = c("lon",
                  "lat"), crs = "EPSG:32633") |> 
    project(vect(RCzechia::okresy())) |> 
    terra::mask(vect(RCzechia::okresy() |>
                       filter(NAZ_LAU1 == "Benešov"))) |> 
    values() |> 
    rename(conc_river_mg.dm3_obs = conc_mean) |> 
    select(acsubst, conc_river_mg.dm3_obs) |> 
    bind_cols(tibble(type = "Measured")) 
  
  pec_obs_soil <- bind_rows(pec_riv, obs_riv) |>
    group_by(acsubst, type) |>
    summarise(conc_river_mg.dm3_obs_distr = conc_river_mg.dm3_obs |> mean(),
              conc_river_mg.dm3_pred_distr = conc_river_mg.dm3_pred |> mean(),
              std_obs = sd(conc_river_mg.dm3_obs),
              std_pred = sd(conc_river_mg.dm3_pred),
              n = n())
  
  riverconc.plt <-  ggplot(pec_obs_soil) + 
    geom_pointrange(aes(conc_river_mg.dm3_obs_distr,
                        y = type,
                        xmin = conc_river_mg.dm3_obs_distr - std_obs,
                        xmax = conc_river_mg.dm3_obs_distr + std_obs,
                        colour = "salmon")) +
    geom_text(aes(x = conc_river_mg.dm3_obs_distr,
                  y = type,
                  label = paste0("(",n,")"),
                  fontface = "italic",
                  colour = "salmon"),
              nudge_y = 0.05,
              size = 3) +
    geom_pointrange(aes(conc_river_mg.dm3_pred_distr,
                        y = type,
                        xmin = conc_river_mg.dm3_pred_distr - std_pred,
                        xmax = conc_river_mg.dm3_pred_distr + std_pred)) +
    geom_text(aes(x = conc_river_mg.dm3_pred_distr,
                  y = type,
                  label = paste0("(",n,")"),
                  fontface = "italic"),
              nudge_y = 0.05,
              size = 3) +
    scale_colour_identity() +
    ggtitle(paste0("Benešov: PEC river water"))+
    xlab(expression("\u00B5g \u00D7 dm"^-3)) +
    ylab("") +
    facet_wrap(~acsubst, scales = "free_x") +
    theme_bw()
  
  riverconc.plt
}

as = c("Glyphosate", "Acetamiprid", "Tebuconazole")

crop_soilconc_plt(as = as) 
crop_riverconc_plt(as =  as)


watrcrsL <- dir_ls(path_home_r(), recurse = T, regexp = "WatrcrsL.shp$") |>
  vect() |>
  project(vect(RCzechia::okresy())) |> 
  terra::mask(vect(RCzechia::okresy() |>
                     filter(NAZ_LAU1 == "Benešov"))) |> buffer(100)

river_obs <- dir_ls(path_home_r(),
       recurse = T,
       regexp = "2021 water monitoring data CZ") |>
  read_excel() |>
  filter(!is.na(lon)) |>
  select(1:15) |>
  rename(acsubst = Active) |> 
  # filter(acsubst %in% as) |> 
  filter(acsubst == "Glyphosate") |> 
  # mutate(across(starts_with("conc"), ~.x/1000),
  #        unit = "mg/dm3") |>
  vect(geom = c("lon",
                "lat"), crs = "EPSG:32633") |> 
  project(vect(RCzechia::okresy())) |> 
  terra::mask(vect(RCzechia::okresy() |>
                     filter(NAZ_LAU1 == "Benešov")))
river_pred <- dir_ls(path_home_r(),
                      recurse = T,
                      regexp = "Benesov_river_allchem_farm.gpkg") |>
  vect() |> 
  # filter(acsubst %in% as) |> 
  filter(acsubst == "Glyphosate") |> 
  select(-HYDROID) |> 
  terra::zonal(watrcrsL,  fun = "mean", as.polygons = T) |> 
  select(conc_acsubst_mean_river_seg_g.m3.ndays)

tmap_mode("plot")
tm_shape(watrcrsL,
           name = "River network (CUZK)") +
  tm_lines(col = "steelblue1",
           lwd = 1) +
tm_shape(RCzechia::okresy() |> filter(NAZ_LAU1 == "Benešov"), name = "District border") +
  tm_borders(col = "black",
             lwd = 2) +
tm_shape(river_pred) +
  tm_polygons("conc_acsubst_mean_river_seg_g.m3.ndays",
              fill.scale = tm_scale_continuous(value.na = "#f9f9f9" ,
                                              label.na = "Missing values",
                                              values = "brewer.bu_pu"),
              fill.legend = tm_legend(title = "PEC (mean) \u00B5g \u00D7 dm\u207B\u00B3",
                                      bg.color = "#f0f0f0",
                                      bg.alpha = 1),
              group = "Simulated concentration in river water",
              group.control = "radio") +
tm_shape(river_obs) +
  tm_dots("conc_mean", 
          size = 0.95,
          fill.scale = tm_scale_intervals(values = "brewer.or_rd"),
          fill.legend = tm_legend(title = "MEC (mean) \u00B5g \u00D7 dm\u207B\u00B3",
                                  bg.color = "#f0f0f0",
                                  bg.alpha = 1)) +
          # group = "Observed concentration in river water",
          # group.control = "radio") +
tm_shape(river_obs) +
  tm_text(text = "site_id", size = 1, options = opt_tm_text(just = 1.25)) +
tm_scalebar(position = c("right", "bottom")) +
  tm_title(paste0("Predicted and observed ", river_obs[["acsubst"]] |> unique() , " concentrations in 2021 in Benešov")) +
  tm_basemap("Esri.WorldTopoMap", alpha = 0.95, group.control = "check") 

################################################################
############ END: Surface water monitoring stations ############
################################################################

