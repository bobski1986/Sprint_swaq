# Install and load packages
pkg <- c("tidyverse", "fs", "readxl", "terra", "tmap", "OpenStreetMap", "tidyterra", "sf", "data.table", "fuzzyjoin", "openxlsx", 'ncdf4',
         "leaflet", "htmltools", "htmlwidgets", "RCzechia", "RColorBrewer", "progress")
for (i in pkg) {
  if (!requireNamespace(i, quietly = TRUE)) {
    install.packages(i)
  }
}

lapply(pkg, library, character.only = T)

# Function for simulating and visualising ASs concentration in river water using dynamic HTML map for one selected district#

AS_dynmap_riverwater <- function(district_name,
                                    acsubst_name,
                                    app_month,
                                    app_startday,
                                    endday,
                                    rivbuff_width){

## Some initial values for testing
  # district_name <- "Břeclav"
  # basins_nrmin <- 1
  # basins_nrmax <- 21
  # acsubst_name <- c("Glyphosate", "Acetamiprid", "Tebuconazole")
  # app_month <- "July"
  # app_startday <- 1
  # endday <- 30
  # rivbuff_width <- 100

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

### Read basin polygons for the selected district
basins_cz_distr <- dir_ls(path_home_r(), recurse = T, regexp = "hydrosheds_lvl10_basins_cz.gpkg") |>
  vect() |>
  select("HYBAS_ID", "dis_m3_pyr", "dis_m3_pmn", "dis_m3_pmx", "riv_tc_ssu", "riv_tc_usu") |>
  mask(district_select)
# Select the maximum of basin areas to be included

basins_nrmax <- readline(prompt = paste0("There are ", nrow(basins_cz_distr), " river basins intersecting this district. Select number of river basins: "))

basins_cz_distr_max <- basins_cz_distr[1:basins_nrmax,] |> terra::split("HYBAS_ID")

## River basins CHMI
# basins4ord <- paste0(water_spatial_dir, "4BasinsCZ.shp") |>
#   vect() |>
#   project(districts) |>
#   rename("Basin_ID" = CHP_14_S)

# basins3ord <- paste0(water_spatial_dir, "3BasinsCZ.shp") |>
#   vect() |>
#   project(districts) |>
#   rename("Basin_ID" = CHP_3R)

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
  summarise(conc_mean_river_seg = mean(load_acsubst_mean_g.ndays/dis_m3_pyr),
            conc_min_river_seg = mean(load_acsubst_min_g.ndays/dis_m3_pmn),
            conc_max_river_seg = mean(load_acsubst_max_g.ndays/dis_m3_pmx))

conc_acsubst_river_seg_mapinput <- dir_ls(path_home_r(), recurse = T, regexp = "Benesov_river_3chem_farm.gpkg") |>
  vect()

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
           select(conc_mean_river_seg),
           name = "Simulated concentration in river water") +
  tm_polygons("conc_mean_river_seg",
              popup.vars = c("Concentration \u00B5g \u00D7 dm\u207B\u00B3" = "conc_mean_river_seg"),
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
 #              select(acsubst, month, ndays, conc_acsubst_mean_river_seg_g.m3.ndays), paste0(district_name, "_", acsubst_name, "_water", ".gpkg"))

}

AS_dynmap_riverwater("Břeclav","Tebuconazole", "July", 1, 30 , 100)


# Function for simulating and visualising ASs concentration in topsoil on all fields using dynamic HTML map for one selected district #

AS_dynmap_topsoil <- function(district_name,
                                         acsubst_name,
                                         app_month,
                                         app_startday,
                                         endday,
                                         rivbuff_width){
  
  ## Some initial values for testing
  district_name <- "Benešov"
  basins_nrmin <- 1
  basins_nrmax <- 21
  acsubst_name <-  "Glyphosate"
  app_month <- "July"
  app_startday <- 1
  endday <- 56
  
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
    
    gemap_loc[[basin]] <- dir_ls(path_home_r(), recurse = T, regexp = "gemap100_model_cz.gpkg") |>
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
    crop(district_select)
  
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
    crop(district_select)
  
  # ESDAC topsoil physical properties for Europe (based on LUCAS topsoil data) #
  
  # sand_jrc_path <- paste0(water_spatial_dir, "Sand1.tif")
  # sand_jrc_laea <- rast(sand_jrc_path)
  # sand_jrc_cz_wgs84 <- sand_jrc_laea |>  project(basins_cz) |> crop(basins_cz)
  # writeCDF(sand_jrc_cz_wgs84, filename = paste0(water_spatial_dir ,"sand_jrc_CZ.nc"), overwrite = T)
  
  sand_jrc_cz_path <- dir_ls(path_home_r(),
                             recurse = T,
                             regexp = "sand_jrc_CZ.nc$")
  sand_jrc_cz <- rast(sand_jrc_cz_path) |>
    crop(district_select)
  
  # clay_jrc_path <- paste0(water_spatial_dir, "Clay.tif")
  # clay_jrc_laea <- rast(clay_jrc_path)
  # clay_jrc_cz_wgs84 <- clay_jrc_laea |>  project(basins_cz) |> crop(basins_cz)
  # writeCDF(clay_jrc_cz_wgs84, filename = paste0(water_spatial_dir ,"clay_jrc_CZ.nc"), overwrite = T)
  
  clay_jrc_cz_path <- dir_ls(path_home_r(),
                             recurse = T,
                             regexp = "clay_jrc_CZ.nc$")
  clay_jrc_cz <- rast(clay_jrc_cz_path) |>
    crop(district_select)
  
  # budens_jrc_path <- paste0(water_spatial_dir, "Bulk_density.tif")
  # budens_jrc_laea <- rast(budens_jrc_path)
  # budens_jrc_cz_wgs84 <- budens_jrc_laea |>  project(basins_cz) |> crop(basins_cz)
  # writeCDF(budens_jrc_cz_wgs84, filename = paste0(water_spatial_dir ,"budens_jrc_CZ.nc"), overwrite = T)
  
  budens_jrc_cz_path <- dir_ls(path_home_r(),
                               recurse = T,
                               regexp = "budens_jrc_cz.nc$")
  budens_jrc_cz <- rast(budens_jrc_cz_path) |>
    crop(district_select)
  
  # Chemical input data from qsars (vega, epi) and PPDB where available #
  source(dir_ls(path_home_r(), recurse = T, regexp = "ppdb scraping"))
  chemprop <- chemprop_gen(acsubst_name) |> 
    select(acsubst_name, Kfoc_ml.g, DT50_field_d) |> 
    filter(!is.na(Kfoc_ml.g),
           !is.na(Koc_ml.g),
           !is.na(DT50_field_d),
           !is.na(DT50_typical_d))
  
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
    
    
    load_acsubst_farm_mapinput <- dir_ls(path_home_r(), recurse = T, regex = "Benešov_topsoil_3chem_farm.gpkg") |> vect()
    
   rasterize(project(load_acsubst_farm_mapinput |> filter(acsubst == "Glyphosate") |> mask(district_select),
                     crs("EPSG:32633")) |>
                         select(conc_total_soil_twa),
                       project(orcarb_jrc_cz, crs("EPSG:32633")),
                       field = "conc_total_soil_twa", touches = T) |> disagg(2) |> plot()
   
   project(project(load_acsubst_farm_mapinput |> mask(district_select) |> filter(acsubst == "Glyphosate"), crs("EPSG:32633")) |>
             select(conc_total_soil_twa),
           crs("EPSG:32633")) |> plot(border = "red",
                                      add = T)
  
    # Keep unique rows
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
      tm_basemap("Esri.WorldTopoMap", alpha = 1, group.control = "check")
      
    # Other spatial input 
    soil_basin_conc_acsubst_rastmap <-  basin_basemap +
    tm_shape(rasterize(project(load_acsubst_farm_mapinput, crs("EPSG:32633")) |>
                         select(conc_acsubst_total_soil_twa_g.kg),
                       project(orcarb_jrc_cz, crs("EPSG:32633"))|> disagg(2),
                       field = "conc_acsubst_total_soil_twa_g.kg",touches = T)) +
      tm_raster("conc_acsubst_total_soil_twa_g.kg",
                # popup.vars = c("Concentration \u00B5g \u00D7 kg\u207B\u00B9" = "conc_acsubst_total_soil_twa_g.kg"),
                col.scale = tm_scale_intervals(label.na = "Missing values",
                                                values = "brewer.bu_pu"),
                col.legend = tm_legend(title = "Concentration in topsoil (time-weighted) \u00B5g \u00D7 kg\u207B\u00B9",
                                        bg.color = "#f0f0f0",
                                        bg.alpha = 1),
                # lwd = 0.75,
                group = "Simulated concentration in topsoil",
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
    
    # AS concentration in soil, and loads from all agricultural fields vector map
    soil_basin_conc_acsubst_vectmap <- basin_basemap +
      tm_shape(load_acsubst_farm_mapinput,
               name = "Application rate") +
      tm_polygons("aprate_river_buff" ,
                  fill.scale = tm_scale_intervals(value.na = "#f9f9f9", label.na = "Missing", values = "brewer.bu_pu"),
                  fill.legend = tm_legend(title = "Application rate (g/ha)"),
                  lwd = 0.2) +
      tm_shape(load_acsubst_farm_mapinput |>  select(conc_acsubst_total_soil_twa_g.kg),
               name = "Simulated concentration in soil") +
      tm_polygons("conc_acsubst_total_soil_twa_g.kg",
                  popup.vars = c("Concentration \u00B5g \u00D7 kg\u207B\u00B9" = "conc_acsubst_total_soil_twa_g.kg"),
                  fill.scale = tm_scale_intervals(value.na = "#f9f9f9" ,
                                                  label.na = "Missing values",
                                                  values = "brewer.bu_pu"),
                  fill.legend = tm_legend(title = "Concentration in topsoil (time-weighted) \u00B5g \u00D7 kg\u207B\u00B9",
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
  tmap_save(soil_basin_conc_acsubst_rastmap, paste0(district_name, "_", acsubst_name, "_soil", ".html"))
  # writeVector(load_acsubst_farm_mapinput |>
  #               select(HYBAS_ID,ZKOD,CTVEREC,acsubst,Crop, conc_acsubst_total_soil_twa_g.kg), paste0(district_name, "_", acsubst_name, "_soil", ".gpkg"))
  
}

AS_dynmap_topsoil("Benešov","Acetamiprid", "July", 1, 30 , 100)

# Function for simulating and visualising ASs concentration in topsoil on all fields and in riverwater buffer for one selected district #

AS_statmap_topsoil_riverwater <- function(district_name,
                                          acsubst_name,
                                          app_month,
                                          app_startday,
                                          endday,
                                          rivbuff_width){
  
  ## Some initial values for testing
  district_name <- "Beroun"
  # basins_nrmin <- 1
  # basins_nrmax <- 21
  acsubst_name <- c(
    "Bixafen",
    "Diflufenican",
    "Chlorotoluron",
    "Imazamox",
    "Mandestrobin",
    "Dimoxystrobin",
    # "Difenoconazole",
    # "Doscalid",
    # "Fluazinam",
    # "Prochloraz",
    # "Diquat",
    # "Azoxystrobin",
    # "Pethoxamid",
    "Benzovindiflupyr",
    "Tebuconazole",
    # "Quinmerac",
    # "Mefentrifluconazole",
    # "Cyproconazole",
    # "Tefluthrin",
    # "Picloram",
    "Metazachlor",
    # "Pendimethalin",
    # "Gamma-cyhalothrin",
    # "Deltamethrin",
    "Epoxiconazole"
    # "Glyphosate",
    # "Spiroxamine",
    # "Terbuthylazine"
    )
  sim_yr <- 2021
  app_month <- "July"
  app_startday <- 1
  endday <- 56
  rivbuff_width <- 100
  
  ######################################################################
  ############# START: Import and transform input data sets ############
  ######################################################################
  
  # Districts borders #
  districts <- okresy("low")|>
    mutate(NAZ_LAU1 = str_to_title((gsub("-", " ", NAZ_LAU1, fixed = TRUE)))) |>
    vect()
  
  district_select <- districts |>  filter(NAZ_LAU1 %in% district_name)
  
  # River basins #
  ## Hydrosheds river basins. Prepared trimmed file to include only the country of interest
  # basins_eu_path <- dir_ls(path_home_r(), recurse = T, regexp = "BasinATLAS_v10_lev10.gpkg")
  # basins_cz <- vect(basins_eu_path, extent = ext(districts))
  # writeVector(basins_cz, "hydrosheds_lvl10_basins_cz.gpkg")
  
  ### Read basin polygons from Hydrosheds for the selected district
  basins_cz_distr <- dir_ls(path_home_r(), recurse = T, regexp = "hydrosheds_lvl10_basins_cz.gpkg") |>
    vect() |>
    select("HYBAS_ID", "dis_m3_pyr", "dis_m3_pmn", "dis_m3_pmx", "riv_tc_ssu", "riv_tc_usu", "pre_mm_uyr") |>
    mask(district_select)
  
  # whole country basins
  # cz_basins <-  mask(dir_ls(path_home_r(), recurse = T, regexp = "hydrosheds_lvl10_basins_cz.gpkg") |> vect(), districts)
  
  # Select the maximum of basin areas to be included in the analysis
  
  basins_nrmax <- readline(prompt = paste0("There are ", nrow(basins_cz_distr), " river basins intersecting this district. Select number of river basins: "))
  
  basins_cz_distr_max <- basins_cz_distr[1:basins_nrmax,] |> terra::split("HYBAS_ID")
  
  # River network #
  ## River network CUZK
  watrcrsL <- dir_ls(path_home_r(), recurse = T, regexp = "WatrcrsL.shp$") |>
    vect() |>
    project(districts)

  # Get the CZ crop-district map
  # parcel_id <- read_excel(dir_ls(path_home_r(), recurse = T, regexp = "GPZ_Plodiny_2021_12_31.xlsx"), sheet = 1)
  # crop_map_cz_dist <- dir_ls(path_home_r(), recurse = T, regexp = "crop_district_map2021_cz.gpkg$") |> 
  #   vect(extent = ext(basins_cz_distr_max[[basin]])) |>
  #   mask(basins_cz_distr_max[[basin]]) |> 
  #   select("crop_name_eagri_map", "crop_code_eagri", "ZKOD", "CTVEREC") |> 
  #   terra::merge(parcel_id[c("ZKOD", "CTVEREC", "FID")], by = c("ZKOD", "CTVEREC"))

  # GeMAP for the selected district  and Active substance #
  gemap_loc <- list()
  
  for (basin in seq_along(basins_cz_distr_max)) {
    
    gemap_loc[[basin]] <- dir_ls(path_home_r(), recurse = T, regexp = "gemap100_model_cz") |>
      vect(extent = ext(basins_cz_distr_max[[basin]])) |>
      mask(basins_cz_distr_max[[basin]]) |>
      mutate(District = str_to_title((gsub("_", " ", District, fixed = TRUE))),
             Active = str_to_title(Active)) |>
      filter(Active %in% acsubst_name) |> 
      # select("ZKOD", "CTVEREC", "Crop", "EPPO", "EAGRI", "FieldAr", "Active", "ASmass", "ASarea", "ARfarm", "IFav", "IFmin", "IFmax", "District", "ARmin", "ARmax", "BBCHmin", "BBCHmax", "ApFreq") |>
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

  # CHMU Rainfall#
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
  
  start_date <- make_date(year = sim_yr, month = match(app_month, month.name), day = app_startday)
  end_date <- start_date + days(endday)
  sim_months <- seq(start_date, end_date, by = "day") |>
    lubridate::month(label = TRUE, abbr = FALSE, locale = Sys.getlocale("LC_TIME")) |>
    unique()

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
           # Filter months for which to run daily simulations
           month %in% sim_months) |>
    # Average daily rainfall in selected basins calculated from stations within basin polygons
    vect(crs = "WGS84", geom = c("Geogr1", "Geogr2")) |>
    mask(basins_cz_distr) |>
    as_tibble() |>
    group_by(month, day) |>
    summarise(rain_mean_mm.day = mean(rain_mm.day),
              rain_min_mm.day = min(rain_mm.day),
              rain_max_mm.day = max(rain_mm.day)) |> 
    ungroup() |> 
    # Filter number of days: from first day of AS application to several day after
   slice(1:endday) |> 
    mutate(ndays = n())

  # Terrain slope #
  # Data cropped to include only country of interest and saved. Done only once
  ## FAO
  ### 5min resolution
  # slope5min_path <- dir_info(water_spatial_dir, regexp = "_5min.asc")
  # slopesCl1_8_5min <- map(slope5min_path$path, rast)
  # slopesCl1_8_5min_cz <- map(slopesCl1_8_5min, crop, basins_cz)
  # slopes_all5min_cz <- sprc(slopesCl1_8_5min_cz) |> terra::mosaic(fun = "mean")
  # writeCDF(slopes_all5min_cz,filename = paste0(water_spatial_dir,"/TerrainSlope_5min_CZ.nc"), overwrite = T)
  # slope_cz_5min_path <- dir_ls(path_home_r(),
  #                              recurse = T,
  #                              regexp = "TerrainSlope_5min_CZ.nc$")
  # slope_cz_5min <- rast(slope_cz_5min_path) |>
  #   crop(basins_cz_distr) |> 
  #   rename("slope_perc" = "TerrainSlope_5min_CZ") |>
  #   mutate(slope_perc = slope_perc/1000/100)
  
  ### 30as resolution
  # slope30as_path <- dir_info(path_home_r(), recurse = T, regexp = "_30as.asc")
  # slopesCl1_8_30as <- map(slope30as_path$path, rast, .progress = T)
  # slopesCl1_8_30as_cz <- map(slopesCl1_8_30as, crop, ext(cz_basins), .progress = T)
  # slopes_all_perc_cz <- map(slopesCl1_8_30as_cz, ~tan(./3600)*100) |> 
  #   rast() |> median() |> project(crs(orcarb_jrc_cz)) |> rename(terrain_slope_med_perc = sum)
    # terra::writeCDF(slopes_all_perc_cz, filename = paste0(path_home_r(),"/TerrainSlope_30as_cz.nc"))
  slope_cz_30as <- dir_ls(path_home_r(), recurse = T, regexp = "/TerrainSlope_30as_cz.nc$") |>
    rast() |> crop(basins_cz_distr_max)

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
    crop(basins_cz_distr_max)
  
  # ESDAC topsoil physical properties for Europe (based on LUCAS topsoil data) #
  
  # sand_jrc_path <- paste0(water_spatial_dir, "Sand1.tif")
  # sand_jrc_laea <- rast(sand_jrc_path)
  # sand_jrc_cz_wgs84 <- sand_jrc_laea |>  project(basins_cz) |> crop(basins_cz)
  # writeCDF(sand_jrc_cz_wgs84, filename = paste0(water_spatial_dir ,"sand_jrc_CZ.nc"), overwrite = T)
  
  sand_jrc_cz_path <- dir_ls(path_home_r(),
                             recurse = T,
                             regexp = "sand_jrc_CZ.nc$")
  sand_jrc_cz <- rast(sand_jrc_cz_path) |>
    crop(basins_cz_distr_max)
  
  # clay_jrc_path <- paste0(water_spatial_dir, "Clay.tif")
  # clay_jrc_laea <- rast(clay_jrc_path)
  # clay_jrc_cz_wgs84 <- clay_jrc_laea |>  project(basins_cz) |> crop(basins_cz)
  # writeCDF(clay_jrc_cz_wgs84, filename = paste0(water_spatial_dir ,"clay_jrc_CZ.nc"), overwrite = T)
  
  clay_jrc_cz_path <- dir_ls(path_home_r(),
                             recurse = T,
                             regexp = "clay_jrc_CZ.nc$")
  clay_jrc_cz <- rast(clay_jrc_cz_path) |>
    crop(basins_cz_distr_max)
  
  # budens_jrc_path <- paste0(water_spatial_dir, "Bulk_density.tif")
  # budens_jrc_laea <- rast(budens_jrc_path)
  # budens_jrc_cz_wgs84 <- budens_jrc_laea |>  project(basins_cz) |> crop(basins_cz)
  # writeCDF(budens_jrc_cz_wgs84, filename = paste0(water_spatial_dir ,"budens_jrc_CZ.nc"), overwrite = T)
  
  budens_jrc_cz_path <- dir_ls(path_home_r(),
                               recurse = T,
                               regexp = "budens_jrc_cz.nc$")
  budens_jrc_cz <- rast(budens_jrc_cz_path) |>
    crop(basins_cz_distr_max)
  
  # Chemical input data from qsars (vega, epi) and PPDB where available #
  # In case of error e.g., "Error in ppdb_df_values ! Can't extract rows past the end",
  # first check if the names of properties are the same in the script and PPDB, 
  # PPDB gets updated every now and then, so must be the scraping script.
  source(dir_ls(path_home_r(), recurse = T, regexp = "ppdb scraping"))
  chemprop <- chemprop_gen(Active = acsubst_name) |> 
    select(Active,
           DT50_typical_d,
           Koc_ml.g,
           DT50_field_d,
           Kfoc_ml.g, 
           NOEC_earthworm_chron_repr_mg.kg,
           LC50_earthworm_acute_14d_mg.kg,
           NOEC_fish_21_mg.L,
           LC50_fish_acute_96h_mg.kg) |> 
    mutate(across(2:last_col(), as.numeric),
           NOEC_earthworm_chron_repr_mg.kg = coalesce(NOEC_earthworm_chron_repr_mg.kg, LC50_earthworm_acute_14d_mg.kg/10),
           NOEC_fish_21_mg.L = coalesce(NOEC_fish_21_mg.L, LC50_fish_acute_96h_mg.kg),
           Kfoc_ml.g = coalesce(Kfoc_ml.g, Koc_ml.g))


  ####################################################################
  ############# END: Import and transform input data sets ############
  ####################################################################
  
  ########################################################################################
  ############ START: Spatial input data intersected with river basin ##################
  ########################################################################################
  
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
  
  slope_river_seg <- terra::zonal(slope_cz_30as,
                                  gemap_loc,
                                  fun = "mean",
                                  # weights = T,
                                  # exact = T,
                                  as.polygons = TRUE) |> 
  rename("slope_perc" = "TerrainSlope_30as_cz")
  
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
  
  # Patch size of arable land in a river basins and district
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
  
  # District and crop specific application rate per river basins
  acsubst_application_basin_distr <- gemap_loc |>
    left_join(crop_area_tot_distr_basin, by = join_by(District, HYBAS_ID, Crop, acsubst)) |>
    left_join(crop_area_tot_basin_distr, by = join_by(acsubst, Crop, District)) |>
    mutate(acsubst_mass_g = acsubst_mass_kg * 1000,
           crop_acsubst_area_frac_distr_basin = crop_acsubst_totarea_dsitr_basin_ha / acsubst_area_ha,
           crop_acsubst_area_basin_ha = crop_acsubst_totarea_dsitr_basin_ha * crop_acsubst_area_frac_distr_basin,
           crop_acsubst_mass_basin_g = crop_acsubst_area_basin_ha * aprate_farm_g.ha,
           crop_acsubst_mass_frac_basin = crop_acsubst_mass_basin_g / acsubst_mass_g) |>
    left_join(chemprop,
              by = c("acsubst" = "Active"))
  
#######################################################################################
############ END: Spatial input data intersected with river basin ###################
#######################################################################################
  
####################################################################
########### START: Pesticide Runoff Model Schriever 2007 ###########
####################################################################

    # Model subroutines related to pesticide runoff from individual farms
  load_acsubst_farm <- acsubst_application_basin_distr |>
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
                                                  \(x,y,z,v) (x/(y*5))*(z+v))) |>
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

  # Calculate daily amount of pesticide load potentially reaching a stream from individual farms
  # Calculate lagged and time-weighted soil concentration
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
             frac_asubst_soil_water_lag = exp(-day * log(2) / DT50_typical_d) * frac_asubst_soil_water_ini,
             frac_asubst_soil_solid_lag = exp(-day * log(2) / DT50_typical_d) * frac_asubst_soil_solid_ini,
             conc_acsubst_total_soil_lag_g.kg = (frac_asubst_soil_water_lag + frac_asubst_soil_solid_lag) * conc_acsubst_total_soil_ini,
             conc_acsubst_total_soil_twa_g.kg = (conc_acsubst_total_soil_ini / (ndays * (log(2) / DT50_typical_d))) * (1 - exp(-ndays * (log(2) / DT50_typical_d))),
             rq_acsubst_total_soil_twa = conc_acsubst_total_soil_twa_g.kg/NOEC_earthworm_chron_repr_mg.kg,
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
             load_acsubst_max_g.ndays = mean(load_acsubst_max_g.day)) |> 
      slice(1)
    
    # rename("srunoff_day" = "day")
    
    cat("\r", i ,"farm out of", nrow(load_acsubst_farm), "is being processed")
    
  }
  
  # Combine AS concentration in soil results on individual farms
  # Calculate RQ for individual farms and individual AS
   soil_farm_mapinput <- srunoff_acsubst_farm |>
     vect() |>
     select(conc_acsubst_total_soil_twa_g.kg,
            rq_acsubst_total_soil_twa,
            load_acsubst_mean_g.ndays,
            Crop,
            acsubst,
            month,
            ndays,
            District,
            ZKOD,
            CTVEREC,
            EAGRI) |>
     mutate(conc_acsubst_total_soil_twa_g.kg = conc_acsubst_total_soil_twa_g.kg/10,
            load_acsubst_mean_g.ndays = load_acsubst_mean_g.ndays,
            rq_acsubst_total_soil_twa = rq_acsubst_total_soil_twa/1000) |> 
     mask(district_select)
   
   # writeVector(soil_farm_mapinput, "Berun_map_input.gpkg")
  # soil_farm_mapinput <- dir_ls(path_home_r(), recurse = T, regexp = "Břeclav_map_input") |> vect()
   # Calculate cumulative RQ in soil for individual fields
   # Create a character type column showing a list of ASs and RQs for each field
   soil_RQ_nest <- soil_farm_mapinput |>   
     values() |> 
     group_by(ZKOD, CTVEREC, Crop) |> 
     distinct(acsubst, .keep_all = T) |>
     arrange(desc(rq_acsubst_total_soil_twa), .by_group = T) |>
     mutate(nr_as = n(),
            rq_acsubst_total_soil_twa =  round(rq_acsubst_total_soil_twa, 3),
            rq_acsubst_total_soil_twa = case_when(rq_acsubst_total_soil_twa < 0.01 ~ 0.01,
                                              .default = rq_acsubst_total_soil_twa)) |> 
     # filter(rq_acsubst_total_soil_twa >= 0.01) |> 
     unite("AS_rq", acsubst, rq_acsubst_total_soil_twa, sep = " | ", remove = FALSE) |>
     select(ZKOD, CTVEREC, Crop, nr_as, AS_rq) |>
     nest("AS_rq" = AS_rq) |> 
     ungroup()
   
  # Calculate summed RQs for individual fields
   soil_cumRQ <- soil_farm_mapinput |> 
     makeValid() |> 
     select("ZKOD", "CTVEREC", "Crop", "acsubst", "rq_acsubst_total_soil_twa") |> 
     group_by(ZKOD, CTVEREC, Crop) |> 
     summarise(sum_rq_acsubst_total_soil_twa = sum(rq_acsubst_total_soil_twa),
               .groups = "keep")
     
   # Merge nested RQs and summed RQs  datasets by unique field IDs
   soil_cumRQ_all <- terra::merge(soil_cumRQ, soil_RQ_nest, by = c("Crop", "ZKOD", "CTVEREC")) |> 
     mutate(AS_rq = str_remove_all(AS_rq, "\""),
            AS_rq = str_remove_all(AS_rq, "list"),
            AS_rq = str_remove_all(AS_rq, "AS_rq"),
            AS_rq = str_remove_all(AS_rq, "c"),
            AS_rq = str_remove_all(AS_rq, "="),
            AS_rq = str_remove_all(AS_rq, "\\("),
            AS_rq = str_remove_all(AS_rq, "\\)"),
            AS_rq = str_replace_all(AS_rq, "0.01", "<0.01")) |>
     mask(district_select)
   
  # Intersect farm loadings with river buffer segments
  # Calculate RQ for individual fields intersected with buffer around river segments
   river_seg_mapinput <- soil_farm_mapinput[c("load_acsubst_mean_g.ndays",
                                              "acsubst",
                                              "month",
                                              "ndays",
                                              "District")] |>
    makeValid() |>
    terra::intersect(rivers_basin_buff_seg[c("HYDROID", "NAMN1", "SHAPE_Leng", "dis_m3_pyr", "dis_m3_pmn", "dis_m3_pmx")]) |>
    terra::merge(chemprop[c("Active", "NOEC_earthworm_chron_repr_mg.kg", "NOEC_fish_21_mg.L")], by.x = "acsubst", by.y = "Active") |>
    mutate(conc_mean_river_seg = (load_acsubst_mean_g.ndays/dis_m3_pyr),
           rq_mean_river_seg_twa = conc_mean_river_seg/NOEC_fish_21_mg.L/1000) |> 
     mask(district_select)
   
  farm_area_buff <- terra::merge(rivers_basin_buff_seg,
  cbind(river_seg_mapinput[c("HYDROID", "NAMN1", "SHAPE_Leng")] |> 
                 terra::aggregate(c("HYDROID", "NAMN1", "SHAPE_Leng")),
        river_seg_mapinput[c("HYDROID", "NAMN1", "SHAPE_Leng")] |> 
          terra::aggregate(c("HYDROID", "NAMN1", "SHAPE_Leng")) |> 
          expanse()) |> values(),
  by = c("HYDROID", "NAMN1", "SHAPE_Leng")) |> 
    rename(farm_buff_area.m = y)
  
  buff_area <- cbind(rivers_basin_buff_seg[c("HYDROID", "NAMN1", "SHAPE_Leng", "dis_m3_pyr")] |> 
    aggregate(c("HYDROID", "NAMN1", "SHAPE_Leng")),
    rivers_basin_buff_seg[c("HYDROID", "NAMN1", "SHAPE_Leng", "dis_m3_pyr")] |> 
    aggregate(c("HYDROID", "NAMN1", "SHAPE_Leng")) |> 
    expanse(unit = "m")) |> 
    rename(buffer_area.m = y)
  
  river_weight <- terra::merge(farm_area_buff, buff_area |>
                 values(),
               by = c("HYDROID", "NAMN1", "SHAPE_Leng")) |> 
    mutate(river_w = (SHAPE_Leng*WD7)/buffer_area.m*(farm_buff_area.m/buffer_area.m))
  
  # Calculate cumulative RQ for fields in river buffer for individual river segments
  # Create a character type column showing a list of ASs and RQs for each river segment 
  river_RQ_nest <- terra::merge(river_seg_mapinput,
                                river_weight[c("HYDROID", "NAMN1", "SHAPE_Leng", "river_w")] |> 
                                values(),
                                by =  c("HYDROID", "NAMN1", "SHAPE_Leng")) |>
    values() |> 
    group_by(SHAPE_Leng, HYDROID, NAMN1) |> 
     arrange(desc(rq_mean_river_seg_twa), .by_group = T) |>
     distinct(acsubst, .keep_all = T) |>
     mutate(nr_fields = n(),
            rq_mean_river_seg_twa = round(rq_mean_river_seg_twa*river_w, 3),
            rq_mean_river_seg_twa = case_when(rq_mean_river_seg_twa < 0.001 ~ 0.001,
                                              .default = rq_mean_river_seg_twa)) |> 
     # filter(rq_mean_river_seg_twa >= 0.001) |>
    unite("AS_rq", acsubst, rq_mean_river_seg_twa, sep = " | ", remove = FALSE) |>
     select(SHAPE_Leng, NAMN1, HYDROID, nr_fields, AS_rq) |>
     nest("AS_rq" = AS_rq) |> 
     ungroup() 
  
  # Calculate summed weighted RQs for fields in each unique river segment
  river_cumRQ <-terra::merge(river_seg_mapinput,
                               river_weight[c("HYDROID", "NAMN1", "SHAPE_Leng", "river_w")] |> 
                                 values(),
                               by =  c("HYDROID", "NAMN1", "SHAPE_Leng")) |> 
     group_by(HYDROID, NAMN1, SHAPE_Leng) |>
     summarise(sum_rq_mean_river_seg_twa = max(rq_mean_river_seg_twa * river_w), .groups = "keep")
  
  # Merge nested RQs and summed RQs  datasets by unique river segments and drop geometry
  river_cumRQ_df <- terra::merge(river_cumRQ, river_RQ_nest,
                               by = c("HYDROID", "NAMN1", "SHAPE_Leng")) |> 
    mutate(AS_rq = str_remove_all(AS_rq, "\""),
           AS_rq = str_remove_all(AS_rq, "list"),
           AS_rq = str_remove_all(AS_rq, "AS_rq"),
           AS_rq = str_remove_all(AS_rq, "c"),
           AS_rq = str_remove_all(AS_rq, "="),
           AS_rq = str_remove_all(AS_rq, "\\("),
           AS_rq = str_remove_all(AS_rq, "\\)"),
           AS_rq = str_replace_all(AS_rq, "0.001", "<0.001")) |> 
    mask(district_select) |> 
    values()
  
  # Merge all values from the RQ dataframe with river buffer polygons
  river_cumRQ_all <- terra::merge(rivers_basin_buff_seg[c("HYDROID", "NAMN1", "SHAPE_Leng")],
                                  river_cumRQ_df,
                                 by = c("HYDROID", "NAMN1", "SHAPE_Leng")) 
  

# writeVector(conc_acsubst_river_seg_mapinput,
#              paste0(district_name, "_", acsubst_name, "_riverwater.gpkg"),
#              overwrite = T)
  
# Save the results to an Excel file
# write_excel_csv(conc_acsubst_river_seg_mapinput |> 
#            values(),
#          "Benesov_23chem_PEC56_riverwater.csv")
  
# Save the results to an Excel file
# write_excel_csv(load_acsubst_farm_mapinput |>
#                select("acsubst",
#                       District,
#                       HYBAS_ID,
#                       "ZKOD",
#                       "CTVEREC", 
#                       "Crop",
#                       month,
#                       day,
#                       conc_acsubst_total_soil_ini, 
#                       conc_acsubst_total_soil_twa_g.kg) |> 
#              values(),
#            "Benesov_23chem_PEC56_topsoil.csv")
  
  # Keep unique rows
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
########## START: Pesticide concentration Maps ###########
##########################################################

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

pb <- progress_bar$new(
  format = "[:bar] :percent | :map_type | Substance: :substance | ETA: :eta",
  total = length(acsubst_name) * 4,  # 4 maps per substance
  clear = FALSE,
  width = 100
)

actual_soil_acsubst_name <- soil_farm_mapinput$acsubst |> unique()
actual_river_acsubst_name <- river_seg_mapinput$acsubst |> unique()

# Soil individual RQ maps 
for (as in seq_along(acsubst_name <- actual_soil_acsubst_name)) {
  
  # Store current substance name for progress updates
  current_substance <- acsubst_name[as]
  
  # Prepare data
  # Prepare soil data
  soil_data <- soil_farm_mapinput |>
    filter(acsubst %in% acsubst_name[as]) 

  # Color palettes
  # Define color palette functions
  # RQ Using reversed Purple-Green scheme, skipping middle (white) color
  # For the categories of risk, it is important to have a cut at 0.2 (defined as safe level - exposure must be 5 times lower than NOEC). So, the first three categories would be <0.2, ≥ 0.2, ≥ 1... then, it could be ≥ 2, ≥ 3, ≥ 4...
  
  # Concentration maps palettes
  # Soil concentration palette
  
  soil_conc_pal <- colorBin(
    palette = "BuPu",
    domain = soil_data$conc_acsubst_total_soil_twa_g.kg,
    bins = pretty(soil_data$conc_acsubst_total_soil_twa_g.kg),
    na.color = "#ffdeaf"
  )
  
  soil_conc_pal_rev <- colorBin(
    palette = "BuPu",
    domain = soil_data$conc_acsubst_total_soil_twa_g.kg,
    bins = pretty(soil_data$conc_acsubst_total_soil_twa_g.kg),
    na.color = "#ffdeaf",
    reverse = T,
  )
  
  # RQ maps palettes
  # Soil RQ palette
  soil_rq_colors <- brewer.pal(11, "PRGn")[c(1,2,3,4,5,8,9,10)]
  
  soil_rq_pal <- colorBin(
    palette = soil_rq_colors |> rev(),
    domain = soil_data$rq_acsubst_total_soil_twa,
    bins = c(0, 0.2, 1, 2, 3, 5, 500)
    )
  
  # Map titles
  # Concentration maps titles
  # Soil concentration map title
  soil_conc_title <- paste0(
    acsubst_name[as], " ",
    endday |> unique(),
    "-day PEC soil after 1x application in ",
    app_month,
    " ",
    "(",
    sim_yr,
    ")",
    " in ",
    district_select$NAZ_LAU1,
    " district"
  )
  
  # RQ maps titles
  # Soil RQ map title
  soil_rq_title <- paste0(
    acsubst_name[as], " ",
    endday |> unique(),
    "-day RQ<sub>earthworm</sub> soil after 1x application in ",
    app_month,
    " ",
    "(",
    sim_yr,
    ")",
    " in ",
    district_select$NAZ_LAU1,
    " district"
  )
  
  # AS concentration in topsoil
  if (!pb$finished) pb$tick(tokens = list(map_type = "Creating Soil PEC map", substance = current_substance))
  
  soil_as_conc <- leaflet() |>
    addTiles(options = tileOptions(opacity = 0.5)) |> 
    
   # Add district borders
   addPolylines(
      data = district_select,
      color = "black",
      weight = 0.75,
      opacity = 1,
      fillOpacity = 0,
      group = "District borders"
    ) |>
    
   # Add soil concentration polygons
   addPolygons(
      data = soil_data,
      fillColor = ~soil_conc_pal(conc_acsubst_total_soil_twa_g.kg),
      fillOpacity = 0.85,
      color = "black",
      weight = 0.25,
      opacity = 1,
      popup = ~paste0("<b>Field ID: </b>", "<b>", ZKOD, " (ZKOD)", ", ", CTVEREC, " (CTVEREC)", "</b>", "<br>",
                      "<b>Crop: </b>", "<b>", Crop, "</b>", "<br>",
        "<b>Individual PEC soil (µg × kg⁻¹): </b>", "<b>",
        ifelse(is.na(conc_acsubst_total_soil_twa_g.kg), 
               "Missing values",
               format_power10(conc_acsubst_total_soil_twa_g.kg,  digits = 2)), "</b>"),
      highlightOptions = highlightOptions(color = "black",
                                          weight = 3,
                                          bringToFront = TRUE),
      group = "Individual fields"
    ) |> 
    
  # Add layer controls
  addLayersControl(
    overlayGroups = c("District borders", "Individual fields"),
    options = layersControlOptions(collapsed = FALSE)
  ) |>

  # Add legend
  addLegend(
    pal = soil_conc_pal_rev,
    values = soil_data$conc_acsubst_total_soil_twa_g.kg,
    title = "Individual PEC soil<br>(time-weighted) (µg × kg⁻¹)",
    group = "Individual fields",
    position = "bottomright",
    opacity = 0.75,
    na.label = "Missing values",
    labFormat = function(type, cuts, p) {
      n <- length(cuts)
      cuts <- sort(cuts, decreasing = TRUE)
      paste0(sapply(cuts[-1], format_power10, digits = 2), 
             " to ", 
             sapply(cuts[-n], format_power10, digits = 2))
    }
  ) |>
    
  # Add scale bar
  addScaleBar(position = "bottomleft") |>
    
  # Add title using custom HTML control
  addControl(html = paste0("<div style='background-color: rgba(255, 255, 255, 0.9);
                         padding: 6px 6px; border-radius: 4px; font-size: 14px; font-weight: bold; color: #333; max-width: 800px;
                         line-height: 1.4;'>",
                           soil_conc_title, 
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
  
saveWidget(soil_as_conc, file = paste0(dir_create(path_home_r(),
                                                "/Soil/PEC"),
                                     "/", 
                                                paste0(district_select$NAZ_LAU1,
                                                       "_",
                                                       acsubst_name[as],
                                                       "_PEC_Soil.html")), selfcontained = T)

# Soil RQ maps for individual AS
# Create the leaflet map
if (!pb$finished) pb$tick(tokens = list(map_type = "Creating Soil RQ map", substance = current_substance))

soil_ind_rq_dist <- leaflet() |>
  # Add OpenStreetMap tiles with transparency
  addTiles(options = tileOptions(opacity = 0.5)) |> 
  
  # Add district borders
  addPolylines(
    data = district_select,
    color = "black",
    weight = 0.75,
    opacity = 1,
    fillOpacity = 0,
    group = "District borders"
  ) |>
  
  # Add soil RQ polygons
  addPolygons(
    data = soil_data,
    fillColor = ~soil_rq_pal(rq_acsubst_total_soil_twa),
    fillOpacity = 0.85,
    color = "black",
    weight = 0.25,
    opacity = 1,
    popup = ~paste0("<b>Field ID: </b>", "<b>", ZKOD, " (ZKOD)", ", ", CTVEREC, " (CTVEREC)", "</b>", "<br>",
                    "<b>Crop: </b>", "<b>", Crop, "</b>", "<br>",
                    "<b>Individual RQ soil: </b>", "<b>", 
                    ifelse(is.na(rq_acsubst_total_soil_twa), 
                           "Missing values", 
                           format_power10(rq_acsubst_total_soil_twa, digits = 3)), "</b>"),
    highlightOptions = highlightOptions(color = "black",
                                        weight = 3,
                                        bringToFront = TRUE),
    group = "Individual fields"
    ) |>
    
    # Add layer controls
    addLayersControl(
      overlayGroups = c("District borders", "Individual fields"),
      options = layersControlOptions(collapsed = FALSE)
    ) |>
    
    # Add scale bar
    addScaleBar(position = "bottomleft") |> 
    
    # Add custom legend
    addLegend(
      colors = soil_rq_colors,
      labels = c("\u2265 5", "\u2265 4", "\u2265 3", "\u2265 2", "\u2265 1", "\u003c 1", "\u2265 0.2", "\u003c 0.2"),
      title = "Individual RQ soil",
      position = "bottomright",
      group = "Individual fields",
      opacity = 1
      ) |> 
    addControl(html = paste0("<div style='background-color: rgba(255, 255, 255, 0.9);
                         padding: 6px 6px; border-radius: 4px; font-size: 14px; font-weight: bold; color: #333; max-width: 800px;
                         line-height: 1.4;'>",
                             soil_rq_title, 
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

saveWidget(soil_ind_rq_dist, file = paste0(dir_create(path_home_r(),
                                                "/Soil/RQ"),
                                     "/", 
                                     paste0(district_select$NAZ_LAU1,
                                            "_",
                                            acsubst_name[as],
                                            "_RQ_Soil.html")), selfcontained = T)
}

# Water individual RQ maps 
for (as in seq_along(acsubst_name <- actual_river_acsubst_name)) {
  
  # Store current substance name for progress updates
  current_substance <- acsubst_name[as]
  
  # Prepare data
  # Prepare river water data
  
  river_data <- terra::merge(rivers_basin_buff_seg[c("HYDROID", "NAMN1", "SHAPE_Leng")],
                             terra::merge(river_seg_mapinput[c("HYDROID", "NAMN1", "SHAPE_Leng",
                                                               "rq_mean_river_seg_twa",
                                                               "conc_mean_river_seg",
                                                               "acsubst")] |> 
                                            filter(acsubst %in% acsubst_name[as]),
                                          river_weight[c("HYDROID", "NAMN1", "SHAPE_Leng", "river_w")] |> 
                                            values(),
                                          by =  c("HYDROID", "NAMN1", "SHAPE_Leng")) |>
                               group_by(SHAPE_Leng, HYDROID, NAMN1) |> 
                               arrange(desc(rq_mean_river_seg_twa), .by_group = T) |>
                               mutate(nr_fields = n(),
                                      rq_mean_river_seg_twa = round(sum(rq_mean_river_seg_twa*river_w), 4),
                                      conc_mean_river_seg = mean(conc_mean_river_seg*river_w),
                                      .groups = "keep") |> 
                               distinct(acsubst, .keep_all = T) |>
                               values(),
                             by = c("HYDROID", "NAMN1", "SHAPE_Leng"))

  
  river_net <- watrcrsL |> 
    mask(district_select)
  
  # Color palettes
  # Define color palette functions
  # RQ Using reversed Purple-Green scheme, skipping middle (white) color
  # For the categories of risk, it is important to have a cut at 0.2 (defined as safe level - exposure must be 5 times lower than NOEC). So, the first three categories would be <0.2, ≥ 0.2, ≥ 1... then, it could be ≥ 2, ≥ 3, ≥ 4...
  
  # Concentration maps palettes
  # River water concentration palette
  river_conc_pal <- colorBin(
    palette = "BuPu",
    domain = river_data$conc_mean_river_seg,
    bins = pretty(river_data$conc_mean_river_seg),
    na.color = "#ffdeaf"
  )
  
  river_conc_pal_rev <- colorBin(
    palette = "BuPu",
    domain = river_data$conc_mean_river_seg,
    bins = pretty(river_data$conc_mean_river_seg),
    na.color = "#ffdeaf",
    reverse = T
  )
  
  # RQ maps palettes
  # River RQ palette
  river_rq_colors <- brewer.pal(11, "PRGn")[c(1,2,3,4,5,8,9,10)]
  
  river_rq_pal <- colorBin(
    palette = river_rq_colors |> rev(),
    domain = river_data$rq_mean_river_seg_twa,
    bins = c(0, 0.2, 1, 2, 3, 5, 500)
  )
  
  # Map titles
  # Concentration maps titles
  # River water concentration map title
  river_conc_title <- paste0(
    acsubst_name[as], " ",
    endday |> max(),
    "-day PEC<sub>time- and area-weighted</sub> surface water after 1x application in ",
    app_month,
    " ",
    "(",
    sim_yr,
    ")",
    " in ",
    district_select$NAZ_LAU1,
    " district"
  )
  
  # RQ maps titles
  # River water RQ map title
  river_rq_title <- paste0(
    acsubst_name[as], " ",
    endday |> unique(),
    "-day individual RQ<sub>fish, area-weighted</sub> surface water after 1x application in ",
    app_month,
    " ",
    "(",
    sim_yr,
    ")",
    " in ",
    district_select$NAZ_LAU1,
    " district"
  )
  
  # AS concentration in river water
  if (!pb$finished) pb$tick(tokens = list(map_type = "Creating Water PEC map", substance = current_substance))
  
  river_as_conc <- leaflet() |>
    addTiles(options = tileOptions(opacity = 0.5)) |> 
    
    # Add district borders
    addPolylines(
      data = district_select,
      color = "black",
      weight = 0.75,
      opacity = 1,
      fillOpacity = 0,
      group = "District borders"
    ) |>
    
    # Add river water network
    addPolylines(data = river_net,
                 color = "blue",
                 weight = 0.5,
                 opacity = 1,
                 group = "River network" 
    ) |> 
    
    # Add river water concentration polygons
    addPolygons(
      data = river_data,
      fillColor = ~river_conc_pal(conc_mean_river_seg),
      fillOpacity = 0.85,
      color = "black",
      weight = 0.25,
      opacity = 1,
      popup = ~paste0("<b>River name: </b>", "<b>", NAMN1, "</b><br>",
                      "<b># of fields in </b>", "<b>", rivbuff_width, " meter</b>", "<b>", " buffer: ", "</b>",  "<b>", nr_fields, "</b><br>",
                      "<b>Individual PEC surface water (\u00B5g\u00D7dm\u207B\u00B3): </b>",
                      "<b>",
                      ifelse(is.na(conc_mean_river_seg), 
                             "Missing values", 
                             format_power10(conc_mean_river_seg, digits = 3)), "</b>"),
      highlightOptions = highlightOptions(color = "black",
                                          weight = 3,
                                          bringToFront = TRUE),
      group = "Individual river segments"
    ) |> 
    
    # Add river basins polgons
    addPolygons(data = basins_cz_distr,
                color = "black",
                weight = 0.5,
                opacity = 0.25,
                fillColor = "#1f78b4",
                popup = ~paste0("<b>Catchment ID: ",HYBAS_ID |> as.character(),"</b><br>",
                                "<b>Annual discharge m\u00B3: ", format_power10(dis_m3_pyr, 3),"</b><br>"),
                highlightOptions = highlightOptions(color = "black",
                                                    weight = 3,
                                                    bringToFront = TRUE),
                group = "River basins" 
    )  |> 
    
    # Add layer controls
    addLayersControl(
      overlayGroups = c("District borders", "Individual river segments", "River network", "River basins"),
      options = layersControlOptions(collapsed = FALSE)
    ) |>
    
    hideGroup("River basins") |> 
    
    # Add legend
    addLegend(
      pal = river_conc_pal_rev,
      values = river_data$conc_mean_river_seg,
      title = "Individual PEC surface water<br>(time- and area-weighted) (\u00B5g\u00D7dm\u207B\u00B3)",
      group = "Individual river segments",
      position = "bottomright",
      opacity = 0.75,
      na.label = "Missing values",
      labFormat = function(type, cuts, p) {
        n <- length(cuts)
        cuts <- sort(cuts, decreasing = TRUE)
        paste0(sapply(cuts[-1], format_power10, digits = 3), 
               " to ", 
               sapply(cuts[-n], format_power10, digits = 3))
      }
    ) |>
    
    # Add scale bar
    addScaleBar(position = "bottomleft") |>
    
    # Add title using custom HTML control
    addControl(html = paste0("<div style='background-color: rgba(255, 255, 255, 0.9);
                         padding: 6px 6px; border-radius: 4px; font-size: 14px; font-weight: bold; color: #333; max-width: 800px;
                         line-height: 1.4;'>",
                             river_conc_title, 
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
  
  saveWidget(river_as_conc, file = paste0(dir_create(path_home_r(),
                                                     "/Water/PEC"),
                                          "/", 
                                          paste0(district_select$NAZ_LAU1,
                                                 "_",
                                                 acsubst_name[as],
                                                 "_PEC_Water.html")), selfcontained = T)
  
  # River water RQ maps for individual AS
  # Create the leaflet map
  if (!pb$finished) pb$tick(tokens = list(map_type = "Creating Water RQ map", substance = current_substance))
  
  river_ind_rq_dist <- leaflet() |>
    # Add OpenStreetMap tiles with transparency
    addTiles(options = tileOptions(opacity = 0.5)) |>
    
    # Add district borders
    addPolylines(
      data = district_select,
      color = "black",
      weight = 0.75,
      opacity = 1,
      fillOpacity = 0,
      group = "District borders"
    ) |>
    
    # Add river water network
    addPolylines(data = river_net,
                 color = "blue",
                 weight = 0.5,
                 opacity = 1,
                 group = "River network" 
    ) |> 
    
    # Add river water RQ polygons
    addPolygons(
      data = river_data,
      fillColor = ~river_rq_pal(rq_mean_river_seg_twa),
      fillOpacity = 0.85,
      color = "black",
      weight = 0.25,
      opacity = 1,
      popup = ~paste0("<b>River name: </b>", "<b>", NAMN1, "</b><br>",
                      "<b># of fields in </b>", "<b>", rivbuff_width, " meter</b>", "<b>", " buffer: ", "</b>",  "<b>", nr_fields, "</b><br>",
                      "<b>Individual RQ Surface water: </b>",
                      "<b>",
                      ifelse(is.na(rq_mean_river_seg_twa), 
                             "Missing values", 
                             format_power10(rq_mean_river_seg_twa, digits = 3)), "<b>"),
      highlightOptions = highlightOptions(color = "black",
                                          weight = 3,
                                          bringToFront = TRUE),
      group = "Individual river segments"
    ) |>
    
    # Add river basins polgons
    addPolygons(data = basins_cz_distr,
                color = "black",
                weight = 0.5,
                opacity = 0.25,
                fillColor = "#1f78b4",
                popup = ~paste0("<b>Catchment ID: ",HYBAS_ID |> as.character(),"</b><br>",
                                "<b>Annual discharge m\u00B3: ", format_power10(dis_m3_pyr, digits = 3),"</b><br>"),
                highlightOptions = highlightOptions(color = "black",
                                                    weight = 3,
                                                    bringToFront = TRUE),
                group = "River basins" 
    )  |> 
    
    # Add layer controls
    addLayersControl(
      overlayGroups = c("District borders", "Individual river segments", "River network", "River basins"),
      options = layersControlOptions(collapsed = FALSE)
    ) |>
    
    hideGroup("River basins") |> 
    
    # Add scale bar
    addScaleBar(position = "bottomleft") |> 
    
    # Add custom legend
    addLegend(
      colors = river_rq_colors,
      labels = c("\u2265 5", "\u2265 4", "\u2265 3", "\u2265 2", "\u2265 1", "\u003c 1", "\u2265 0.2", "\u003c 0.2"),
      title = "Individual RQ surface water<br>(area-weighted)",
      position = "bottomright",
      group = "Individual river segments",
      opacity = 1
    ) |> 
    addControl(html = paste0("<div style='background-color: rgba(255, 255, 255, 0.9);
                         padding: 6px 6px; border-radius: 4px; font-size: 14px; font-weight: bold; color: #333; max-width: 800px;
                         line-height: 1.4;'>",
                             river_rq_title, 
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
  
  saveWidget(river_ind_rq_dist, file = paste0(dir_create(path_home_r(),
                                                         "/Water/RQ"),
                                              "/", 
                                              paste0(district_select$NAZ_LAU1,
                                                     "_",
                                                     acsubst_name[as],
                                                     "_RQ_Water.html")), selfcontained = T)
}

# Cumulative RQ maps
    
# Define color palette function
# Using reversed Purple-Green scheme, skipping middle (white) color
# For the categories of risk, it is important to have a cut at 0.2 (defined as safe level - exposure must be 5 times lower than NOEC). So, the first three categories would be <0.2, ≥ 0.2, ≥ 1... then, it could be ≥ 2, ≥ 3, ≥ 4...
soil_RQcum_colors <- brewer.pal(11, "PRGn")[c(1,2,3,4,5,8,9,10)]
river_RQcum_colors <- brewer.pal(11, "PRGn")[c(1,2,3,4,5,8,9,10)]

soil_RQcum_pal <- colorBin(
  palette = rev(soil_RQcum_colors),
  domain = soil_cumRQ_all$sum_rq_acsubst_total_soil_twa,
  bins = c(0, 0.2, 1, 2, 3, 5, 10000)
)

river_RQcum_pal <- colorBin(
  palette = river_RQcum_colors |> rev(),
  domain = river_cumRQ_all$sum_rq_mean_river_seg_twa,
  bins = c(0, 0.2, 1, 2, 3, 5, 10000),
)

# Cumulative RQ maps titles
# Cumulative RQ soil map title
soil_rqcum_title <- paste0(
  "Cumulative RQ<sub>earthworm</sub> soil after ",
  endday |> unique(),
  "-day exposure following 1x application in ",
  app_month,
  " ",
  "(",
  sim_yr,
  ")",
  " in ",
  district_select$NAZ_LAU1,
  " district"
)

# Cumulative RQ water map title
river_rqcum_title <- paste0(
  "Cumulative RQ<sub>fish, area weighted</sub> surface water after ",
  endday |> unique(),
  "-day exposure following 1x application in ",
  app_month,
  " ",
  "(",
  sim_yr,
  ")",
  " in ",
  district_select$NAZ_LAU1,
  " district"
)

# Cumulative RQ soil map
soil_cum_rq_dist <- leaflet() |>
  # Add OpenStreetMap tiles with transparency
  addTiles(options = tileOptions(opacity = 0.5)) |>
  
  # Add district borders
  addPolylines(
    data = district_select,
    color = "black",
    weight = 0.5,
    opacity = 1,
    fillOpacity = 0,
    group = "District borders"
  ) |>
  
  # Add soil RQ polygons
  addPolygons(
    data = soil_cumRQ_all,
    fillColor = ~soil_RQcum_pal(sum_rq_acsubst_total_soil_twa),
    fillOpacity = 0.85,
    color = "black",
    weight = 0.25,
    opacity = 1,
    popup = ~paste0("<b>Field ID: </b>", "<b>", ZKOD, " (ZKOD)", ", ", CTVEREC, " (CTVEREC)", "</b>", "<br>",
                    "<b>Crop: </b>", "<b>", Crop, "</b>", "<br>",
                    "<b>Cumulative RQ soil: </b>", "<b>",
                    ifelse(is.na(sum_rq_acsubst_total_soil_twa), 
                           "Missing values",
                           format_power10(sum_rq_acsubst_total_soil_twa, digits = 3)), "</b><br>",
                    "<b>Number of AS used: </b>","<b>", nr_as, "</b>", "<br>",
                    "<b>Top contributors (RQ \u003e 0.01):</b>" ,"<br>", AS_rq),
    highlightOptions = highlightOptions(color = "black",
                                        weight = 3,
                                        bringToFront = TRUE),
    group = "Individual fields"
  ) |>
  
  # Add layer controls
  addLayersControl(
    overlayGroups = c("District borders", "Individual fields"),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  
  # Add scale bar
  addScaleBar(position = "bottomleft") |>
  
  # Add custom legend
  addLegend(
    colors = soil_RQcum_colors,
    labels = c("\u2265 5", "\u2265 4", "\u2265 3", "\u2265 2", "\u2265 1", "\u003c 1", "\u2265 0.2", "\u003c 0.2"),
    title = "Cumulative RQ soil",
    position = "bottomright",
    group = "Individual fields",
    opacity = 1
  ) |> 
  addControl(html = paste0("<div style='background-color: rgba(255, 255, 255, 0.9);
                             padding: 6px 6px; border-radius: 4px; font-size: 14px; font-weight: bold; color: #333; max-width: 800px;
                             line-height: 1.4;'>",
                           soil_rqcum_title, 
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

saveWidget(soil_cum_rq_dist, file = paste0(path_home_r(),
                                           "/Soil/RQ/",
                                           district_select$NAZ_LAU1,
                                           "_RQcum_Soil.html"), selfcontained = T)

# Cumulative RQ river water
river_cum_rq_dist <- leaflet() |>
  # Add OpenStreetMap tiles with transparency
  addTiles(options = tileOptions(opacity = 0.5)) |>
  
  # Add district borders
  addPolylines(
    data = district_select,
    color = "black",
    weight = 0.75,
    opacity = 1,
    fillOpacity = 0,
    group = "District borders"
  ) |>
  
  # Add river water network
  addPolylines(data = river_net,
               color = "blue",
               weight = 0.5,
               opacity = 1,
               group = "River network" 
  ) |> 
  
  # Add river water RQ polygons
  addPolygons(
    data = river_cumRQ_all,
    fillColor = ~river_RQcum_pal(sum_rq_mean_river_seg_twa),
    fillOpacity = 0.85,
    color = "black",
    weight = 0.25,
    opacity = 1,
    popup = ~paste0("<b>River name: </b>", "<b>", NAMN1, "</b><br>",
                    "<b># of fields in </b>", "<b>", rivbuff_width, " meter</b>", "<b>", " buffer: ", "</b>",  "<b>", nr_fields, "</b><br>",
                    "<b>Cumulative RQ surface water: </b>", "<b>",
                    ifelse(is.na(sum_rq_mean_river_seg_twa), 
                           "Missing values",
                           format_power10(sum_rq_mean_river_seg_twa, digits = 3)), "</b><br>",
                    "<b>Top contributors (RQ \u003e 0.001): </b>" ,"<br>", AS_rq),
    highlightOptions = highlightOptions(color = "black",
                                        weight = 3,
                                        bringToFront = TRUE),
    group = "Individual river segments"
  ) |>
  
  # Add river basins polgons
  addPolygons(data = basins_cz_distr,
              color = "black",
              weight = 0.5,
              opacity = 0.25,
              fillColor = "#1f78b4",
              popup = ~paste0("<b>Catchment ID: ",HYBAS_ID |> as.character(),"</b><br>",
                              "<b>Annual discharge m\u00B3: ", format_power10(dis_m3_pyr, 3),"</b><br>"),
              highlightOptions = highlightOptions(color = "black",
                                                  weight = 3,
                                                  bringToFront = TRUE),
              group = "River basins" 
  )  |> 
  
  # Add layer controls
  addLayersControl(
    overlayGroups = c("District borders", "Individual river segments", "River network", "River basins"),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  
  hideGroup("River basins") |> 
  
  # Add scale bar
  addScaleBar(position = "bottomleft") |> 
  
  # Add custom legend
  addLegend(
    colors = river_RQcum_colors,
    labels = c("\u2265 5", "\u2265 4", "\u2265 3", "\u2265 2", "\u2265 1", "\u003c 1", "\u2265 0.2", "\u003c 0.2"),
    title = "Cumulative RQ surface water<br>(area-weighted)",
    position = "bottomright",
    group = "Individual river segments",
    opacity = 1
  ) |> 
  addControl(html = paste0("<div style='background-color: rgba(255, 255, 255, 0.9);
                             padding: 6px 6px; border-radius: 4px; font-size: 14px; font-weight: bold; color: #333; max-width: 800px;
                             line-height: 1.4;'>",
                           river_rqcum_title, 
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

saveWidget(river_cum_rq_dist, file = paste0(path_home_r(),
                                            "/Water/RQ/",
                                            district_select$NAZ_LAU1,
                                            "_RQcum_Water.html"), selfcontained = T)
  }    
      
#################################################################
########## END: Pesticide concentration and risk maps ###########
#################################################################
  
  # Plots using tmap package need to be converted to leaflet for interactive viewing
  # and saved as hmtl files for reporting 
  
  # sf_use_s2(F)
  # tmap_mode("view")
  
  # basemap_district <- tm_shape(district_select) +
  #   tm_shape(district_select) +
  #   tm_borders(col = "black",
  #              lty = "dashed",
  #              lwd = 0.75) +
  #   tm_basemap("Esri.WorldTopoMap", alpha = 0.5) +
  #   # tmap_options(component.autoscale = T) +
  #   tm_scalebar(position = c("right", "bottom"))
  # 
  # layout_map <- tm_layout(frame = FALSE,
  #                         legend.outside = TRUE,
  #                         legend.outside.position = "right",
  #                         legend.outside.size = 0.2,
  #                         legend.text.size = 0.4,
  #                         legend.title.size = 0.5,
  #                         main.title.fontface = "bold"
  #                         # main.title.size = 0.5
  #                         )
  
  # Create and save district dependent maps
  ## Terrain slope
  # tslope_map <- basemap_district +
  #   layout_map +
  #   tm_shape(slope_cz_30as |> mask(district_select)) +
  #   tm_raster("TerrainSlope_30as_cz", 
  #             col_alpha = 0.85,
  #             col.scale = tm_scale_continuous(values = "-brewer.rd_yl_gn"),
  #             col.legend = tm_legend(title = "Terrain slope (median) (%)",
  #                                    bg.color = "#f0f0f0",
  #                                    bg.alpha = 0.75)) +
  #   tm_title(paste0("Terrain slope in ",district_select$NAZ_LAU1,
  #                   " district"),
  #            size = 1) +
  #   tm_add_legend(title = "District border",
  #                 type = "lines",
  #                 col = "black",
  #                 lty = "dashed",
  #                 lwd = 1) +
  #   tm_shape(district_select) +
  #   tm_borders(col = "black",
  #              lty = "dashed",
  #              lwd = 0.5)
  
  ## Save terrain slope maps
  # tmap_save(tslope_map,
  #           paste0(district_name,
  #                  "_terrain_slope_map.png"),
  #           dpi = 600,
  #           units = "cm",
  #           width = 20,
  #           height = 15)
  
  ## Organic carbon content
  # ocarb_map <- basemap_district +
  #   layout_map +
  #   tm_shape(orcarb_jrc_cz |> mask(district_select)) +
  #   tm_raster("OC_jrc_CZ", 
  #             col_alpha = 0.85,
  #             col.scale = tm_scale_continuous(values = "-brewer.rd_yl_gn"),
  #             col.legend = tm_legend(title = "Organic carbon (%)",
  #                                    bg.color = "#f0f0f0",
  #                                    bg.alpha = 0.75)) +
  #   tm_title(paste0("Topsoil organic carbon content in ",district_select$NAZ_LAU1,
  #                   " district"),
  #            size = 1) +
  #   tm_add_legend(title = "District border",
  #                 type = "lines",
  #                 col = "black",
  #                 lty = "dashed",
  #                 lwd = 1) +
  #   tm_shape(district_select) +
  #   tm_borders(col = "black",
  #              lty = "dashed",
  #              lwd = 0.5)
  
  ## Save Organic carbon content map
  # tmap_save(ocarb_map,
  #           paste0(district_name,
  #                  "_ocarb_map.png"),
  #           dpi = 600,
  #           units = "cm",
  #           width = 20,
  #           height = 15)
  
  ## Sand content
  # sand_map <-basemap_district +
  #   layout_map +
  #   tm_shape(sand_jrc_cz |> mask(district_select)) +
  #   tm_raster("sand_jrc_CZ", 
  #             col_alpha = 0.85,
  #             col.scale = tm_scale_continuous(values = "-brewer.rd_yl_gn"),
  #             col.legend = tm_legend(title = "Sand (%)",
  #                                    bg.color = "#f0f0f0",
  #                                    bg.alpha = 0.75)) +
  #   tm_title(paste0("Topsoil sand content in ",district_select$NAZ_LAU1,
  #                   " district"),
  #            size = 1) +
  #   tm_add_legend(title = "District border",
  #                 type = "lines",
  #                 col = "black",
  #                 lty = "dashed",
  #                 lwd = 1) +
  #   tm_shape(district_select) +
  #   tm_borders(col = "black",
  #              lty = "dashed",
  #              lwd = 0.5)
  
  ## Save Sand content map
  # tmap_save(sand_map,
  #           paste0(district_name,
  #                  "_sand_map.png"),
  #           dpi = 600,
  #           units = "cm",
  #           width = 20,
  #           height = 15)
  
  ## Bulk soil density map
  # bdens_map <-basemap_district +
  #   layout_map +
  #   tm_shape(budens_jrc_cz|> mask(district_select)) +
  #   tm_raster("budens_jrc_CZ", 
  #             col_alpha = 0.85,
  #             col.scale = tm_scale_continuous(values = "-brewer.rd_yl_gn"),
  #             col.legend = tm_legend(title = "Soil bulk density (kg\u00D7dm\u207B\u00B3)",
  #                                    bg.color = "#f0f0f0",
  #                                    bg.alpha = 0.75)) +
  #   tm_title(paste0("Topsoil bulk density in ",district_select$NAZ_LAU1,
  #                   " district"),
  #            size = 1) +
  #   tm_add_legend(title = "District border",
  #                 type = "lines",
  #                 col = "black",
  #                 lty = "dashed",
  #                 lwd = 1) +
  #   tm_shape(district_select) +
  #   tm_borders(col = "black",
  #              lty = "dashed",
  #              lwd = 0.5)
  
  ## Save Bulk soil density map
  # tmap_save(bdens_map,
  #           paste0(district_name,
  #                  "_bdens_map.png"),
  #           dpi = 600,
  #           units = "cm",
  #           width = 20,
  #           height = 15)
  
  ## Precipitation map
  
  # prec_map <- basemap_district +
  #   layout_map +
  #   tm_shape(disagg(rasterize(basins_cz_distr|>
  #                               select(pre_mm_uyr), rast(basins_cz_distr), "pre_mm_uyr", touches = T), fact=c(100, 100)) |>
  #              mask(district_select)) +
  #   tm_raster("pre_mm_uyr", 
  #             col_alpha = 0.85,
  #             col.scale = tm_scale_continuous(values = "brewer.bu_pu"),
  #             col.legend = tm_legend(title = "Precipitation (mm)",
  #                                    bg.color = "#f0f0f0",
  #                                    bg.alpha = 0.75)) +
  #   tm_title(paste0("Annual precipitation in ",district_select$NAZ_LAU1,
  #                   " district"),
  #            size = 1) +
  #   tm_add_legend(title = "District border",
  #                 type = "lines",
  #                 col = "black",
  #                 lty = "dashed",
  #                 lwd = 1) 
  #   tm_shape(district_select) +
  #   tm_borders(col = "black",
  #              lty = "dashed",
  #              lwd = 0.5)
  
  ## Save preciptation map
  # tmap_save(prec_map,
  #           paste0(district_name,
  #                  "_prec_map.png"),
  #           dpi = 600,
  #           units = "cm",
  #           width = 20,
  #           height = 15)
  
  ## River discharge map
  # dis_map <- basemap_district +
  #   layout_map +
  #   tm_shape(rivers_basin_buff_seg |> select(dis_m3_pyr)) +
  #   tm_polygons("dis_m3_pyr", 
  #               fill.scale = tm_scale_continuous(values = "brewer.bu_pu"),
  #               fill.legend = tm_legend(title = "Discharge (mean) (m\u00B3)",
  #                                       bg.color = "#f0f0f0",
  #                                       bg.alpha = 0.75),
  #               lwd= 0) +
  #   tm_title(paste0("Annual river discharge in ",district_select$NAZ_LAU1,
  #                   " district"),
  #            size = 1) +
  #   tm_add_legend(title = "District border",
  #                 type = "lines",
  #                 col = "black",
  #                 lty = "dashed",
  #                 lwd = 1) +
  #   tm_shape(district_select) +
  #   tm_borders(col = "black",
  #              lty = "dashed",
  #              lwd = 0.5)
  
  ## Save river discharge map
  # tmap_save(dis_map,
  #           paste0(district_name,
  #                  "_dis_map.png"),
  #           dpi = 600,
  #           units = "cm",
  #           width = 20,
  #           height = 15)
  
  # Create chemical- and district- dependent concentration maps
  ## Crop map
  # crop_map <- basemap_district +
  #   layout_map +
  #   tm_shape(gemap_loc |> filter(acsubst %in% acsubst_name[as]) |> mask(district_select)) +
  #   tm_polygons("Crop",
  #               fill.scale = tm_scale_categorical(value.na = "#ffdeaf" ,
  #                                                 label.na = "Missing values",
  #                                                 values = "poly.palette36"),
  #               fill.legend = tm_legend(title = "Crop name",
  #                                       bg.color = "#f0f0f0",
  #                                       bg.alpha = 0.75),
  #               lwd = 0.2) +
  #   tm_title(paste0("Crops treated with ",
  #                   acsubst_name[as] |> str_to_lower(),
  #                   " in ",
  #                   district_select$NAZ_LAU1,
  #                   " district"),
  #            size = 1) +
  #   tm_add_legend(title = "District border",
  #                 type = "lines",
  #                 col = "black",
  #                 lty = "dashed",
  #                 lwd = 1) 
  
  ## AS application rate map
  # apprate_as_map <- basemap_district +
  #   layout_map +
  #   tm_shape(acsubst_application_basin_distr |> filter(acsubst %in% acsubst_name[as]) |> mask(district_select)) +
  #   tm_polygons("aprate_farm_g.ha",
  #               fill.scale = tm_scale_continuous(value.na = "#ffdeaf" ,
  #                                                 label.na = "Missing values",
  #                                                 values = "brewer.bu_pu"),
  #               fill.legend = tm_legend(title = "Application rate (g\u00D7ha\u207B\u00B9)",
  #                                       bg.color = "#f0f0f0",
  #                                       bg.alpha = 0.75),
  #               lwd = 0.2) +
  #   tm_title(paste0(acsubst_name[as]," application rate in ",
  #                   district_name,
  #                   " district"),
  #            size = 1) +
  #   tm_add_legend(title = "District border",
  #                 type = "lines",
  #                 col = "black",
  #                 lty = "dashed",
  #                 lwd = 1) 
  
  # Save chemical and district dependent maps
  ## Save Crop map
  # tmap_save(crop_map,
  #           paste0(district_name,
  #                  "_",
  #                  acsubst_name[as],
  #                  "_crop_map.png"),
  #           dpi = 600,
  #           units = "cm",
  #           width = 20,
  #           height = 15)
  # 
  # tmap_save(apprate_as_map,
  #           paste0(district_name,
  #                  "_",
  #                  acsubst_name[as],
  #                  "_apprate_map.png"),
  #           dpi = 600,
  #           units = "cm",
  #           width = 20,
  #           height = 15)

# ASs <- c("Acetamiprid", "Tebuconazole", "Glyphosate")

# AS_statmap_topsoil_riverwater("Benešov", acsubst_name = ASs, "July", 1, 30, 100)

# AS RQ for soil map

# rq_map <- function(ASs, district){
#   
#   soil_rq_map <- dir_ls(path_home_r(),
#                         recurse = T,
#                         regexp = "CZdistricts_topsoil_3chem_farm_RQ.gpkg") |>
#     vect() 
#   
#   sf_use_s2(F)
#   tmap_mode("view")
#   
#   rq_dist <- tm_shape(RCzechia::okresy() |> filter(NAZ_LAU1 == district),
#                       name = "District borders") +
#     tm_borders(col = "black",
#                lwd = 1.5) +
#     tm_shape(soil_rq_map |>
#                filter(acsubst == ASs | District == district) |>
#                select(RQ, acsubst) |>
#                mask(vect(RCzechia::okresy() |> 
#                            filter(NAZ_LAU1 == district)))) +
#     tm_polygons("RQ",
#                 popup.vars = c("RQ topsoil" = "RQ"),
#                 fill.scale = tm_scale_intervals(value.na = "#ffdeaf",
#                                                 label.na = "Missing values",
#                                                 style = "fixed",
#                                                 breaks = c(0:3),
#                                                 labels = c("<1", "1", ">1"),
#                                                 values = "-brewer.prgn",
#                                                 midpoint = 1),
#                 fill.legend = tm_legend(title = "RQ topsoil",
#                                         bg.color = "#f0f0f0",
#                                         bg.alpha = 1),
#                 lwd = 0.75,
#                 group = "RQ topsoil for individual fields",
#                 group.control = "check") +
#     tm_title(paste0(RCzechia::okresy()[,3] |>
#                       filter(NAZ_LAU1 == district) |>
#                       st_drop_geometry() |> 
#                       unique(),
#                     ": 30-day ",
#                     soil_rq_map |>
#                       select(acsubst) |> 
#                       filter(acsubst == ASs) |> 
#                       values() |> 
#                       unique(),
#                    " topsoil risk quotient (RQ topsoil) following 1x application in July.")) +
#     # tm_scalebar(position = c("right", "bottom")) +
#     tm_basemap("Esri.WorldTopoMap", alpha = 0.5, group.control = "check")
#   
#   tmap_save(rq_dist,
#             paste0(RCzechia::okresy()[,3] |>
#                     filter(NAZ_LAU1 == district) |>
#                     st_drop_geometry() |> 
#                     unique(),
#                   "_",
#                   soil_rq_map |>
#                     select(acsubst) |> 
#                     filter(acsubst == ASs) |> 
#                       values() |> 
#                       unique(),
#                   "_soil_RQ.html"))
# 
# }
# 
# rq_map("Glyphosate", "Břeclav")
# 
# soil_rq_map <- dir_ls(path_home_r(),
#                       recurse = T,
#                       regexp = "") |> vect()
# 
# tmap_mode("plot")
# 
# # animated rq gif
# 
# rq_soil_gif <- function(district) {
#   
#   rq_soil <- tm_shape(RCzechia::okresy() |> filter(NAZ_LAU1 == district),
#                       name = "District borders") +
#     tm_borders(col = "black",
#                lwd = 1.5) +
#     tm_shape(soil_rq_map|> 
#                # mutate(RQ = RQ*1000) |> 
#                filter(District == district) |>
#                mask(vect(RCzechia::okresy() |>
#                            filter(NAZ_LAU1 == district)))) +
#     tm_polygons("RQ",
#                 popup.vars = c("RQ topsoil" = "RQ"),
#                 fill.scale = tm_scale_intervals(value.na = "#ffdeaf",
#                                                 label.na = "Missing values",
#                                                 style = "fixed",
#                                                 breaks = c(0:3),
#                                                 labels = c("<1", "1", ">1"),
#                                                 values = "-brewer.prgn",
#                                                 midpoint = 1),
#                 fill.legend = tm_legend(title = "RQ topsoil",
#                                         bg.color = "#f0f0f0",
#                                         bg.alpha = 1),
#                 lwd = 0.75,
#                 group = "RQ topsoil for individual fields",
#                 group.control = "check") +
#     tm_basemap("OpenStreetMap", alpha = 0.5) +
#     tm_layout(panel.labels = c(paste0("Acetamiprid RQ topsoil in ", district),
#                                paste0("Glyphosate RQ topsoil in ", district),
#                                paste0( "Tebuconazole RQ topsoil in ", district)),
#               legend.outside = TRUE,
#               legend.outside.position = "right") +
#     tm_facets_pagewise(by = "acsubst")
#   
#   tmap_animation(rq_soil, paste0(district,"_rq_soil.gif"), fps = 0.45, scale = 1.25, dpi = 200)
#   
# }
# 
# rq_soil_gif("Benešov")

  
# Risk map data preparation
  
# AS_top <- dir_ls(path = path_home_r(),
#                  recurse = T,
#                  regexp = "Rank_selection_CZ_earthw.xlsx") |>
#   read_xlsx() |>
#   slice(1:23) |> 
#   mutate(ACTIVE = str_to_title(ACTIVE))
# 
#  parcel_id <- terra::merge(crop_map_cz_dist |>
#                select(ZKOD, CTVEREC, crop_name_eagri_map),
#                dir_ls(path_home_r(),
#                       recurse = T,
#                       regexp = "GPZ_Plodiny_2021_12_31.xlsx") |> 
#                  read.xlsx(sheet = 1),
#                by.x = c("ZKOD", "CTVEREC", "crop_name_eagri_map"),
#                by.y = c("ZKOD", "CTVEREC", "PLODINA_NA")) |> 
#    select(ZKOD, FID, CTVEREC, FID, crop_name_eagri_map)
 
# soil_RQ_as_nest <- dir_ls(path = "C:\\Users\\253120\\Documents",
#                       recurse = T,
#                       regexp = "Benešov_allchem_RQ_soil.gpkg") |>
#   vect() |>
#   values() |> 
#   filter(acsubst %in% AS_top$ACTIVE) |> 
#   filter(!is.na(RQ)) |> 
#   mutate(RQ = round(RQ/1000,2)) |> 
#   # unite("AS_crop_rq", acsubst, Crop, RQ, sep = "/", remove = FALSE) |> 
#   select(-month, -ndays, -"NOEC mg.kg", -conc_acsubst_total_soil_twa_g.kg, -Crop, -RQ) |> 
#   nest("AS" = acsubst)
# 
# soil_RQ_crop_nest <- dir_ls(path = "C:\\Users\\253120\\Documents",
#                       recurse = T,
#                       regexp = "Benešov_allchem_RQ_soil.gpkg") |>
#   vect() |>
#   values() |> 
#   filter(acsubst %in% AS_top$ACTIVE) |> 
#   filter(!is.na(RQ)) |> 
#   mutate(RQ = round(RQ/1000,2)) |> 
#   # unite("AS_crop_rq", acsubst, Crop, RQ, sep = "/", remove = FALSE) |> 
#   select(-month, -ndays, -"NOEC mg.kg", -conc_acsubst_total_soil_twa_g.kg, -acsubst, -RQ) |> 
#   nest("Crop" = Crop)
# 
# soil_RQ_rq_nest <- dir_ls(path = "C:\\Users\\253120\\Documents",
#                       recurse = T,
#                       regexp = "Benešov_allchem_RQ_soil.gpkg") |>
#   vect() |>
#   values() |> 
#   filter(acsubst %in% AS_top$ACTIVE) |> 
#   filter(!is.na(RQ)) |> 
#   mutate(RQ = round(RQ/1000,2)) |> 
#   # unite("AS_crop_rq", acsubst, Crop, RQ, sep = "/", remove = FALSE) |> 
#   select(-month, -ndays, -"NOEC mg.kg", -conc_acsubst_total_soil_twa_g.kg, -acsubst, -Crop) |> 
#   nest("RQ" = RQ)
# 
# soil_RQ <- dir_ls(path = "C:\\Users\\253120\\Documents",
#                       recurse = T,
#                       regexp = "Benešov_allchem_RQ_soil.gpkg") |>
#   vect() |>
#   filter(acsubst %in% AS_top$ACTIVE) |> 
#   filter(!is.na(RQ)) |> 
#   makeValid() |> 
#    mask(vect(RCzechia::okresy() |>
#                            filter(NAZ_LAU1 == "Benešov"))) |> 
#   select("HYBAS_ID", "ZKOD", "CTVEREC", "Crop", "acsubst", "RQ") |> 
#   mutate(RQ = round(RQ/1000,2) ,
#       HYBAS_ID = as.character(HYBAS_ID))
# 
# soil_cumRQ_crop <- dir_ls(path = "C:\\Users\\253120\\Documents",
#                       recurse = T,
#                       regexp = "Benešov_allchem_RQ_soil.gpkg") |>
#   vect() |>
#   filter(acsubst %in% AS_top$ACTIVE) |> 
#   filter(!is.na(RQ)) |> 
#   makeValid() |> 
#   mask(vect(RCzechia::okresy() |>
#                            filter(NAZ_LAU1 == "Benešov"))) |> 
#   select("HYBAS_ID", "ZKOD", "CTVEREC", "Crop", "acsubst", "RQ") |> 
#   aggregate(c("HYBAS_ID", "ZKOD", "CTVEREC", "Crop"), fun = "sum") |> 
#   mutate(sum_RQ = round(sum_RQ/1000,2) ,
#       HYBAS_ID = as.character(HYBAS_ID))
  
  
  
# soil_rq_map <- merge(soil_conc_map[c("acsubst",
#                                      "ZKOD", 
#                                      "CTVEREC",
#                                      "Crop",
#                                      "field_area", 
#                                      "District",
#                                      "HYBAS_ID",
#                                      "aprate_farm_g.ha",
#                                      "conc_total_soil_twa",
#                                      "frac_asubst_soil_water_ini",
#                                      "frac_asubst_soil_solid_ini",
#                                      "load_acsubst_mean_g.ndays",
#                                      "clay_perc",
#                                      "oc_perc",
#                                      "sand_perc",
#                                      "bulk_dens_kg.dm3",
#                                      "day")] |> 
#                        filter(acsubst %in% soil_rq[[1]]), soil_rq, by.x = "acsubst", by.y = "ACTIVE")

# write.xlsx(soil_rq_map |> values(), "benesov_RQ_cz.xlsx")

# writeVector(bene_rq_map, "Benešov_topsoil_3chem_farm_RQ.gpkg")
# 
# bene_rq_map <- dir_ls(path_home_r(),
#                         recurse = T,
#                         regexp = "Benešov_topsoil_3chem_farm_RQ.gpkg") |>
#   vect() |>
#   mutate(RQ = RQ/1000,
#          District = "Benešov") |>
#   filter(acsubst == c("Acetamiprid", "Tebuconazole", "Glyphosate"))
# 
# brec_rq_map <- dir_ls(path_home_r(),
#                       recurse = T,
#                       regexp = "Břeclav_topsoil_3chem_farm_RQ.gpkg") |>
#   vect() |>
#   select(-c(conc_total_soil_twa ,  unit)) |>
#   mutate(District = "Břeclav")
# 
# bene_conc_map <- dir_ls(path_home_r(),
#        recurse = T,
#        regexp = "Benešov_23chem_topsoil.gpkg") |>
#   vect() |> 
#   mask(vect(RCzechia::okresy() |>
#               filter(NAZ_LAU1 == "Benešov")))
  
# brec_conc_map <- dir_ls(path_home_r(),
#                         recurse = T,
#                         regexp = "Břeclav_topsoil_3chem_farm") |>
#   vect() |> 
#   mutate(unit = "\u00B5g\u00D7kg\u207B\u00B9",
#          conc_total_soil_twa= conc_total_soil_twa/1000)
#  
# writeVector(bene_conc_map, "Benešov_topsoil_3chem_farm.gpkg")


##################################################################
########### START: Surface water monitoring stations #############
##################################################################
 
# Evaluation dataset

# RUN SIMULATION FOR OILSEED FOR THE WHOLE COUNRTY!!!!
pec_soil <- dir_ls(path_home_r(),
                    recurse = T,
                    regexp = "2dist_3chem_topsoil_farm.gpkg") |> vect()
 
gemap100 <-  dir_ls(path_home_r(), recurse = T, regexp = "gemap100") |> 
  vect() |>
  filter(Active %in% c("acetamiprid", "tebuconazole", "glyphosate")) |> 
  mutate(acsubst = Active |> str_to_title())

gemap100_oilseed <- gemap100 |> 
  filter(str_detect(Crop,  regex("rape")))

writeVector(gemap3_pecsoil, "gemap3chem_allcrop_2distr.gpkg")

gemap3_pecsoil <-merge(gemap100 |> 
                         select(acsubst, Crop, ZKOD, CTVEREC, District), pec_soil |> 
                         values(), all.y = T, by = c("ZKOD", "CTVEREC", "acsubst")) |>
  filter(District %in% c("benešov", "břeclav"))

# PLOTS
crop_soilconc_plt <- function(as) {
  
  pec30_soil <-  dir_ls(path_home_r(), recurse = T, regexp = "gemap3chem_allcrop_2distr.gpkg") |> 
    vect() |>
    # filter(acsubst %in% as) |> 
    values() |> 
    mutate(conc_total_soil_twa= conc_total_soil_twa*1000000) |> 
    group_by(acsubst, Crop) |> 
    summarise(PEC30_mean_crop_ug.kg =  conc_total_soil_twa |> mean(),
              std_pec = sd(conc_total_soil_twa),
              n = n()) |> 
    filter(!is.na(PEC30_mean_crop_ug.kg))
  
  # Add PEC and MEC soil data collected by Vera and Knuth et al. 2024
  
  Knuth14 <- bind_rows(tibble(acsubst = "Glyphosate",
                              PEC_ug.kg = c(21.3, 46.7, 22.2, 42.1)),
                       tibble(acsubst = "Tebuconazole",
                              PEC_ug.kg = c(88.6, 54.1, 27.2,  23.3,  27.5, 27.4, 47.4, 109.1,24.8)),
                       tibble(acsubst = "Acetamiprid",
                              PEC_ug.kg = 20))  |>
    cbind(tibble(Source = "Knuth et al. 2024",
                 Crop = "Oilseed rape"))
  
  EFSA16 <- tibble(acsubst = "Acetamiprid",
                   # PECini_mg.kg = c(0.0295, 0.029, 0.045),
                   PEC_ug.kg = c(0.001, 0.001, 0.001)*1000,
                   # PECaccu_mg.kg = c(0.0295, 0.029, 0.045),
                   Crop = c("Apples, pears", "Potatoes", "Tomato")) |>
    cbind(tibble(Source = "EFSA 2016"))
  
  
  EFSA15 <- tibble(acsubst = "Glyphosate",
                   # PECini_mg.kg = c(0.288, 0.576, 2.549, 3.8235, 2.549),
                   PEC_ug.kg = c(0.088, 0.1761, 1.0422, 1.5633, 1.0422)*1000,
                   # PECaccu_mg.kg = c(0.2987,  0.5974, 3.0649, 4.5973, 3.0649),
                   Crop = c("Cereals (wheat, rye, triticale, barley and oats)",
                            "Oilseed rape",
                            "Orchard crops, including citrus & tree nuts",
                            "Orchard crops, including citrus & tree nuts",
                            "Grapes")) |>
    cbind(tibble(Source = "EFSA 2015"))
  
  EFSA14 <- tibble(acsubst = "Tebuconazole",
                   # PECini_mg.kg = c(0.185, 0.067, 0.119, 0.01),
                   PEC_ug.kg = c(0.087, 0.031, 0.056, 0.005)*1000,
                   # PECaccu_mg.kg = c(0.1881, 0.0681, 0.127, 0.01),
                   Crop = c("Cereals (Wheat, barley, oat, rye)",
                            "Oilseed rape",
                            "Grapes",
                            "Cereal (Barley)")) |>
    cbind(tibble(Source = "EFSA 2014"))
  
  obs_soil <- bind_rows(Knuth14,EFSA14, EFSA15, EFSA16) |>
    # filter(acsubst %in% as) |> 
    group_by(acsubst, Crop, Source) |>
    summarise(PEC_ug.kg_mean_crop =  PEC_ug.kg |> mean(),
              std_PEC_ug.kg = sd(PEC_ug.kg),
              n = n()) |> view()
  
  pec_obs_soil <- bind_rows(pec30_soil, obs_soil)

  ggplot(pec_obs_soil |> filter(acsubst == "Glyphosate" , Crop != "Orchard crops, including citrus & tree nuts", Crop != "Grapes")) +
    geom_point(aes(PEC30_mean_crop_ug.kg, Crop), size = 3) +
    ggrepel::geom_text_repel(aes(x = PEC30_mean_crop_ug.kg,
                                 y = Crop,
                                 label = paste0("(",n,")"),
                                 size = 3)) +
    geom_point(aes(PEC_ug.kg_mean_crop, Crop), colour = "salmon", size = 3) +
    ggrepel::geom_text_repel(aes(x = PEC_ug.kg_mean_crop,
                                 y = Crop,
                                 label = paste0("(",n,", ", Source, ")"),
                                 size = 3)) +
    scale_colour_identity() +
    ggtitle("Glyphosate PEC30 topsoil vs literature data") +
    xlab(expression("\u00B5g \u00D7 kg"^-1)) +
    ylab("") + 
    theme(axis.text = element_text(size = 10),
          axis.title.y = element_text(size = 14),
          title = element_text(size = 14),
          legend.position = "none")

  ggplot(pec_obs_soil |> filter(acsubst == "Tebuconazole")) +
    geom_point(aes(PEC30_mean_crop_ug.kg, Crop), size = 3) +
    ggrepel::geom_text_repel(aes(x = PEC30_mean_crop_ug.kg,
                                 y = Crop,
                                 label = paste0("(",n,")"),
                                 size = 3)) +
    geom_point(aes(PEC_ug.kg_mean_crop, Crop), colour = "salmon", size = 3) +
    ggrepel::geom_text_repel(aes(x = PEC_ug.kg_mean_crop,
                                 y = Crop,
                                 label = paste0("(",n,", ", Source, ")"),
                                 size = 3)) +
    scale_colour_identity() +
    ggtitle("Tebuconazole PEC30 topsoil vs literature data") +
    xlab(expression("\u00B5g \u00D7 kg"^-1)) +
    ylab("") + 
    theme(axis.text = element_text(size = 10),
          axis.title.y = element_text(size = 14),
          title = element_text(size = 14),
          legend.position = "none")
  
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
    rename(conc_river_ug.dm3_pred = conc_mean_river_seg) |> 
    values() |> 
    select(acsubst, River_name, conc_river_ug.dm3_pred) |> 
    group_by(River_name, acsubst) |> 
    summarise(conc_river_ug.dm3_pred = conc_river_ug.dm3_pred |> mean(),
              std_pred = sd(conc_river_ug.dm3_pred),
              n_segment_pred = n()) |>
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
    rename(conc_river_ug.dm3_obs = conc_mean) |> 
    select(acsubst, conc_river_ug.dm3_obs) |> 
    bind_cols(tibble(type = "Measured")) 
  
  pec_obs_river <- bind_rows(pec_riv, obs_riv) |>
    group_by(acsubst, type) |>
    summarise(conc_river_ug.dm3_obs_distr = conc_river_ug.dm3_obs |> mean() ,
              conc_river_ug.dm3_pred_distr = conc_river_ug.dm3_pred |> mean(),
              std_obs = sd(conc_river_ug.dm3_obs),
              std_pred = sd(conc_river_ug.dm3_pred),
              n = n())
  
  riverconc.plt <-  ggplot(pec_obs_river) + 
    geom_pointrange(aes(conc_river_ug.dm3_obs_distr,
                        y = type,
                        xmin = conc_river_ug.dm3_obs_distr - std_obs,
                        xmax = conc_river_ug.dm3_obs_distr + std_obs,
                        colour = "salmon")) +
    geom_text(aes(x = conc_river_ug.dm3_obs_distr,
                  y = type,
                  label = paste0("(",n,"**)"),
                  fontface = "italic",
                  colour = "salmon"),
              nudge_y = 0.1,
              size = 5) +
    geom_pointrange(aes(conc_river_ug.dm3_pred_distr,
                        y = type,
                        xmin = conc_river_ug.dm3_pred_distr - std_pred,
                        xmax = conc_river_ug.dm3_pred_distr + std_pred)) +
    geom_text(aes(x = conc_river_ug.dm3_pred_distr,
                  y = type,
                  label = paste0("(",n,"*)"),
                  fontface = "italic"),
              nudge_y = 0.1,
              size = 5) +
    scale_colour_identity() +
    ggtitle(paste0("Benešov: 30-day PEC river water in July 2021"))+
    xlab(expression("\u00B5g \u00D7 dm"^-3)) +
    ylab("") +
    facet_wrap(~acsubst, scales = "free_x") +
    cowplot::theme_cowplot() +
    labs(caption = "*Nr of river segments (Lumped PEC)\n**Nr of sampling sites (National monitoring programme)") +
    theme(plot.caption = element_text(hjust = 0, vjust = 5))
  
  riverconc.plt
}

crop_soilconc_plt(as = as) 
crop_riverconc_plt(as =  c("Acetamiprid", "Tebuconazole", "Glyphosate"))

# MAPS
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

