#############################
# Install and load packages #
#############################
pkg <- c("tidyverse",
         "fs",
         "readxl",
         "terra",
         "tmap",
         "OpenStreetMap",
         "tidyterra",
         "sf",
         "data.table",
         "leaflet",
         "htmltools",
         "htmlwidgets",
         "RCzechia",
         "cols4all",
         "RColorBrewer",
         "progress")

for (i in pkg) {
  if (!requireNamespace(i, quietly = TRUE)) {
    install.packages(i)
  }
}

lapply(pkg, library, character.only = T)

#################
# Load datasets #
#################

# To load the data listed below, they should placed in the Documents on local storage

# LAU data #
# LAUs 2024. 6258 administrative units, Gisco 2024, https://gisco-services.ec.europa.eu/distribution/v2/.
# These LAUs show territorial areas based on the degree of urbanisation (DEGURBA) classes: 1) urban areas: cities, towns and suburbs, and 2) rural areas
lau_degurba_cz <- dir_ls(path_home_r(), recurse = T, regexp = "/LAU_RG_01M_2024_4326.gpkg") |>
  vect() |> 
  filter(CNTR_CODE == "CZ")

# LAUs 2025. 77 administrative units (Okresy) ČÚZK 2025, https://vdp.cuzk.cz/.
# Old LAU_1 categories according to Eurostat
lau_okres_cz <- RCzechia::okresy(resolution = "high") |>
  select(NAZ_LAU1) |>
  rename(LAU_NAME = NAZ_LAU1) |>
  vect()

# Lapis map 2021 #
# Used for selecting parcels based on conventional or ecological farming classes.
# This light version of the original data, contains only conventional farming (EKO = 0) parcels
lapis_cz <- dir_ls(path_home_r(), recurse = T, regexp = "lapis_slim.gpkg") |> 
  vect()


# River basin, Hydrosheds #
# Polygons and attributes from the Hydrosheds dataset
basins_cz <- dir_ls(path_home_r(), recurse = T, regexp = "hydrosheds_lvl10_basins_cz.gpkg") |>
  vect()

# River network, CUZK #
# River network 
river_net <- dir_ls(path_home_r(), recurse = T, regexp = "WatrcrsL.shp$") |>
  vect() |>
  project(lau_okres_cz)

# Terrain slope, FAO #
terrain_slope <- dir_ls(path_home_r(), recurse = T, regexp = "/TerrainSlope_30as_cz.nc$") |>
  rast()

# Organic carbon, ESDAC #
organic_carbon <- dir_ls(path_home_r(),
                             recurse = T,
                             regexp ="OC_jrc_CZ.nc$") |> 
  rast()

# Sand content, ESDAC # 
sand <- dir_ls(path_home_r(),
                           recurse = T,
                           regexp = "sand_jrc_CZ.nc$") |> 
  rast()

# Clay content, ESDAC # 
clay <- dir_ls(path_home_r(),
                           recurse = T,
                           regexp = "clay_jrc_CZ.nc$") |> 
  rast()
  
# Bulk density, ESDAC #
bulk_dens <- dir_ls(path_home_r(),
                             recurse = T,
                             regexp = "budens_jrc_cz.nc$") |> 
  rast()

# GEMUP #
# data are read from within the function for each LAU
# This is lighter version of GEMUP, contains only data required to run the model

###########################
# Main function arguments #
###########################
# The arguments below can be used to define environmental scenarios and to run map_topsoil_riverwater() function
#  map_topsoil_riverwater() is executed for each LAU and generates: 
# - Individual maps per substance (soil PEC, soil RQ, water PEC, water RQ)
# - Cumulative maps (soil RQ, water RQ)
# - .csv files

# List active substances#
acsubst_name <- c("dimoxystrobin", "difenoconazole", "boscalid", "fluazinam", 
                  "prochloraz", "diquat", "azoxystrobin", "pethoxamid", 
                  "benzovindiflupyr", "tebuconazole", "quinmerac", 
                  "mefentrifluconazole", "cyproconazole", "tefluthrin", 
                  "picloram", "metazachlor", "pendimethalin", 
                  "gamma-cyhalothrin", "deltamethrin", "epoxiconazole", 
                  "glyphosate", "spiroxamine", "terbuthylazine", 
                  "flutriafol", "prothioconazole") |> 
  unique() |> 
  str_to_sentence()

# Check if all substances are in the PPDB script
# ASs[which(!acsubst_name %in% ASs)]

# Spatial extent #
# Use lau_okres_cz or lau_derugba_cz
# Change numbers in brackets to select area(s) 
lau_name <- lau_okres_cz[11, "LAU_NAME"]

# Year of simulation #
# Remains unchanged
sim_yr <- 2021

# Starting month of simulation #
# Can be changed
app_month <- "July"

# Simulation start date #
# Used only for daily precipitation data
# app_startday <- 1

# End of simulation #
# Can be changed
# Does not depend on month
endday <- 56

# Buffer area around rivers #
# Can be changed
rivbuff_width <- 100

# Main function for simulating and visualising ASs concentration in topsoil on all fields and in riverwater buffer #
# Run it only once to create "function object"
# Don't change content in the curly brackets 
map_topsoil_riverwater <- function(lau_name,
                                   acsubst_name,
                                   app_month,
                                   endday,
                                   rivbuff_width){
  
  for (name in seq_along(lau_name)) {
    

    ######################################################################
    ############# START: Import and transform input data sets ############
    ######################################################################
    terraOptions(progress = 1)
    
    acsubst_water <- c("tebuconazole",
                       "benzovindiflupyr",
                       "epoxiconazole",
                       "dimoxystrobin",
                       "diflufenican",
                       "chlorotoluron",
                       "imazamox",
                       "bixafen",
                       "metazachlor",
                       "aminopyralid",
                       "fluopyram",
                       "terbuthylazine",
                       "tefluthrin",
                       "picloram") |> unique() |> str_to_sentence()
    
    acsubst_soil <- c("dimoxystrobin",
                      "difenoconazole",
                      "boscalid",
                      "fluazinam",
                      "prochloraz",
                      "diquat",
                      "azoxystrobin",
                      "pethoxamid",
                      "benzovindiflupyr",
                      "tebuconazole",
                      "quinmerac",
                      "mefentrifluconazole",
                      "cyproconazole",
                      "tefluthrin",
                      "picloram",
                      "metazachlor",
                      "pendimethalin",
                      "gamma-cyhalothrin",
                      "deltamethrin",
                      "epoxiconazole",
                      "glyphosate",
                      "spiroxamine",
                      "terbuthylazine",
                      "flutriafol",
                      "prothioconazole") |> unique() |> str_to_sentence()
    
    # Select the maximum of basin areas to be included in the analysis
    # basins_nrmax <- readline(prompt = paste0("There are ", nrow(basins_cz_distr), " river basins intersecting this district. Select number of river basins: "))
    # 
    # basins_cz_distr_max <- basins_cz_distr[1:basins_nrmax,] |> terra::split("HYBAS_ID")

    # gemup for the selected district and Active substance #
    # gemup_lau <- list()
    
    # for (name in seq_along(lau_name)) {
    
    cat("\r", "Gemup for", lau_name[name]$LAU_NAME, "is being processed.", lau_name |> length() - name, "LAUs remaining.")
    
    gemup_lau <- dir_ls(path_home_r(), recurse = T, regexp = "gemup100_slim_cz") |>
      vect(extent = ext(lau_name[name])) |> 
      # vect(extent = ext(basins_cz_distr_max[[basin]])) |>
      mask(lau_name[name]) |>
      mutate(District = str_to_title((gsub("_", " ", District, fixed = TRUE))),
             Active = str_to_title(Active),
             Crop = str_to_sentence(Crop)) |>
      filter(Active %in% acsubst_name) |> 
      filter(!is.na(Crop)) |>
      filter(!if_any("IFav", is.na)) |> 
      # select("ZKOD", "CTVEREC", "Crop", "EPPO", "EAGRI", "FieldAr", "Active", "ASmass", "ASarea", "ARfarm", "IFav", "IFmin", "IFmax", "District", "ARmin", "ARmax", "BBCHmin", "BBCHmax", "ApFreq") |>
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
      tidyterra::bind_spat_cols(lau_name[name] |> values()) |> 
      terra::merge(lapis_cz |> values(),
                   by = c("ZKOD", "CTVEREC")) |> 
      makeValid()
    
    acsubst_gemup <- gemup_lau |> select(acsubst) |> values() |> unique() |> as.vector()
    acsubst_water <- acsubst_water[which(acsubst_water %in% acsubst_gemup$acsubst)]
    acsubst_soil <- acsubst_soil[which(acsubst_soil %in% acsubst_gemup$acsubst)]
    
    # Read basin polygons from Hydrosheds for the selected LAU
    basins_lau_cz <- basins_cz |> 
      select("HYBAS_ID", "dis_m3_pyr", "dis_m3_pmn", "dis_m3_pmx", "riv_tc_ssu", "riv_tc_usu", starts_with("pre_mm_")) |>
      mask(gemup_lau)
    
    # Precipitation data from HYDROSHEDS database#
    
    # Precipitation data from HYDROSHEDS database
    # Create month mapping function
    month_to_column <- function(month_name) {
      # Define month name to number mapping
      month_map <- c(
        "January" = "01", "February" = "02", "March" = "03",
        "April" = "04", "May" = "05", "June" = "06",
        "July" = "07", "August" = "08", "September" = "09",
        "October" = "10", "November" = "11", "December" = "12"
      )
      
      # Get the month number
      month_num <- month_map[month_name]
      
      if (is.na(month_num)) {
        stop(sprintf("Invalid month name: %s. Must be full month name (e.g., 'July')", month_name))
      }
      
      # Construct column name
      col_name <- paste0("pre_mm_s", month_num)
      
      return(col_name)
    }
    
    # Get the precipitation column name for the application month
    prec_col_name <- month_to_column(app_month)
    
    # Check if column exists in basins_lau_cz
    if (!prec_col_name %in% names(basins_lau_cz)) {
      stop(sprintf("Precipitation column '%s' not found in basins_lau_cz. Available columns: %s",
                   prec_col_name,
                   paste(names(basins_lau_cz)[grepl("^pre_mm_s", names(basins_lau_cz))], collapse = ", ")))
    }
    
    # cat(sprintf("Using precipitation data from column: %s (for %s)\n", prec_col_name, app_month))
    
    ### Extract precipitation data for the specified month
    prec_lau <- basins_lau_cz[prec_col_name] |> makeValid()
    
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
    slope_cz_30as <- terrain_slope |> 
      crop(lau_name[name])
    
    # ESDAC Organic carbon content in topsoil #
    # Data cropped to include only country of interest and saved. Done only once
    
    # oc_jrc <- rast(oc_jrc_path)
    # crs(oc_jrc) <- crs(srunoff_path |> rast())
    # oc_jrc <- oc_jrc |> project(srunoff_path |> rast() |> project(basins_cz))
    # oc_jrc_cz <- crop(oc_jrc, basins_cz)
    # writeCDF(oc_jrc_cz, filename = paste0(water_spatial_dir ,"/OC_jrc_CZ.nc"), overwrite = T)
    
    orcarb_jrc_cz <- organic_carbon |>
      select("OC_jrc_CZ") |>
      crop(lau_name[name])
    
    # ESDAC topsoil physical properties for Europe (based on LUCAS topsoil data) #
    
    # sand_jrc_path <- paste0(water_spatial_dir, "Sand1.tif")
    # sand_jrc_laea <- rast(sand_jrc_path)
    # sand_jrc_cz_wgs84 <- sand_jrc_laea |>  project(basins_cz) |> crop(basins_cz)
    # writeCDF(sand_jrc_cz_wgs84, filename = paste0(water_spatial_dir ,"sand_jrc_CZ.nc"), overwrite = T)
    
    sand_jrc_cz <- sand |>
      crop(lau_name[name])
    
    # clay_jrc_path <- paste0(water_spatial_dir, "Clay.tif")
    # clay_jrc_laea <- rast(clay_jrc_path)
    # clay_jrc_cz_wgs84 <- clay_jrc_laea |>  project(basins_cz) |> crop(basins_cz)
    # writeCDF(clay_jrc_cz_wgs84, filename = paste0(water_spatial_dir ,"clay_jrc_CZ.nc"), overwrite = T)
    
    clay_jrc_cz <- clay |>
      crop(lau_name[name])
    
    # budens_jrc_path <- paste0(water_spatial_dir, "Bulk_density.tif")
    # budens_jrc_laea <- rast(budens_jrc_path)
    # budens_jrc_cz_wgs84 <- budens_jrc_laea |>  project(basins_cz) |> crop(basins_cz)
    # writeCDF(budens_jrc_cz_wgs84, filename = paste0(water_spatial_dir ,"budens_jrc_CZ.nc"), overwrite = T)
    
    budens_jrc_cz <- bulk_dens |>
      crop(lau_name[name])
    
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
    
    rivers_lau <- terra::intersect(river_net[c("HYDROID", "NAMN1", "SHAPE_Leng", "WD7")], basins_lau_cz)
    # Indicate buffer width around a river segment
    rivers_lau_buff_seg <- rivers_lau |> terra::buffer(rivbuff_width+rivers_lau$WD7)
    # Intersect gemup with selected river segments so the gemup is expanded to include hydrography of the selected river basin(s)
    gemup_lau_rivers_buff <- gemup_lau[rivers_lau_buff_seg]
    
    # if(gemup_lau_rivers_buff |> nrow() == 0) {
    
    #   "Warning: There are no fields in the buffer, increase buffer width or select another basin"
    
    # }  else {
    
    # Extract data from raster maps intersected by river segments
    prec_field <- terra::zonal(prec_lau,
                               gemup_lau,
                               fun = "mean",
                               as.polygons = T) |>
      rename(!!paste0("prec_", app_month, "_mm") := !!prec_col_name)
    
    orcarb_field <- terra::zonal(orcarb_jrc_cz,
                                 gemup_lau,
                                 fun = "mean",
                                 # weights = T,
                                 # exact = T,
                                 as.polygons = T) |>
      rename("orcarb_perc" = "OC_jrc_CZ") |>
      mutate(orcarb_perc = orcarb_perc/100)
    
    # srunoff_river_seg <- terra::zonal(srunoff_basin,
    #                                   gemup_loc_rivers_seg,
    #                                   fun = "mean",
    #                                   # weights = T,
    #                                   # exact = T,
    #                                   as.polygons = T) |>
    #   rename("SR" = "srunoff_CZ")
    
    slope_field <- terra::zonal(slope_cz_30as,
                                gemup_lau,
                                fun = "mean",
                                # weights = T,
                                # exact = T,
                                as.polygons = TRUE) |> 
      rename("slope_perc" = "TerrainSlope_30as_cz")
    
    sand_field <- terra::zonal(sand_jrc_cz,
                               gemup_lau,
                               fun = "mean",
                               # weights = T,
                               # exact = T,
                               as.polygons = TRUE) |>
      rename("sand_perc" = "sand_jrc_CZ")
    
    
    clay_field <- terra::zonal(clay_jrc_cz,
                               gemup_lau,
                               fun = "mean",
                               # weights = T,
                               # exact = T,
                               as.polygons = TRUE) |>
      rename("clay_perc" = "clay_jrc_CZ")
    
    budens_field <- terra::zonal(budens_jrc_cz,
                                 gemup_lau,
                                 fun = "mean",
                                 # weights = T,
                                 # exact = T,
                                 as.polygons = TRUE) |>
      rename("bulk_dens" = "budens_jrc_CZ")
    
    # Patch size of arable land in a river basins and district
    
    crop_area_tot_lau <- gemup_lau |>
      values() |>
      group_by(LAU_NAME, Crop, acsubst) |>
      summarise(crop_acsubst_totarea_lau.ha = sum(field_area)) |>
      ungroup()
    
    crop_area_tot_distr <- gemup_lau |>
      values() |>
      # filter(District %in% district_basin$NAZ_LAU1) |>
      filter(Crop %in% crop_area_tot_lau$Crop) |>
      group_by(District, acsubst, Crop) |>
      summarise(crop_acsubst_totarea_distr.ha = sum(field_area)) |>
      ungroup()
    
    # District and crop specific application rate per lau
    acsubst_application_lau <- gemup_lau |>
      left_join(crop_area_tot_lau, by = join_by(LAU_NAME, Crop, acsubst)) |>
      left_join(crop_area_tot_distr, by = join_by(acsubst, Crop, District)) |>
      mutate(acsubst_mass.g = acsubst_mass_kg * 1000,
             crop_acsubst_area_frac_lau = crop_acsubst_totarea_lau.ha / acsubst_area_ha,
             crop_acsubst_area_lau.ha = crop_acsubst_totarea_lau.ha * crop_acsubst_area_frac_lau,
             crop_acsubst_mass_lau.g = crop_acsubst_area_lau.ha * aprate_farm_g.ha,
             crop_acsubst_mass_frac_lau = crop_acsubst_mass_lau.g / acsubst_mass.g) |>
      left_join(chemprop,
                by = c("acsubst" = "Active"))
    
    #######################################################################################
    ############ END: Spatial input data intersected with river basin ###################
    #######################################################################################
    
    ####################################################################
    ########### START: Pesticide Runoff Model Schriever 2007 ###########
    ####################################################################
    
    # Get the precipitation column name for binding
    prec_field_col <- paste0("prec_", app_month, "_mm")
    
    # Model subroutines related to pesticide runoff from individual farms
    load_acsubst_farm <- acsubst_application_lau |>
      mutate(apfreq = case_when(apfreq == NA ~ 1, .default = 1)) |>
      cbind(prec_field[prec_field_col] |>
              as.data.frame()) |>
      cbind(orcarb_field$orcarb_perc |>
              as.data.frame()) |>
      cbind(sand_field$sand_perc |>
              as.data.frame()) |>
      cbind(budens_field$bulk_dens |>
              as.data.frame()) |>
      cbind(clay_field$clay_perc |>
              as.data.frame()) |>
      cbind(slope_field$slope_perc |>
              as.data.frame()) |>
      rename("clay_perc" = "clay_field$clay_perc",
             "sand_perc" = "sand_field$sand_perc",
             "bulk_dens_kg.dm3" = "budens_field$bulk_dens",
             "oc_perc" = "orcarb_field$orcarb_perc",
             "slope_perc" = "slope_field$slope_perc",
             "prec_month_mm" = !!prec_field_col) |>
      ## Effect of crop interception factor
      mutate(infactor_effect = map_dbl(infactor_av,
                                       ~1-(./100))) |>
      ## Soil and chemical interaction
      mutate(frac_asubst_soil_water_ini = map2_dbl(oc_perc,
                                                   Kfoc_ml.g,
                                                   ~1/(1 + (.x*.y)/100)),
             frac_asubst_soil_solid_ini = map2_dbl(oc_perc,
                                                   Kfoc_ml.g,
                                                   ~((.x*.y)/100)/(1 + (.x*.y/1000)/100)),
             frac_asubst_soil_water_lag = exp(-endday * log(2) / DT50_typical_d) * frac_asubst_soil_water_ini,
             frac_asubst_soil_solid_lag = exp(-endday * log(2) / DT50_typical_d) * frac_asubst_soil_solid_ini,) |>
      ## Initial concentration in soil
      mutate(conc_acsubst_total_soil_ini_ug.kg = pmap_dbl(list(x = aprate_farm_g.ha,
                                                               y = bulk_dens_kg.dm3,
                                                               z = frac_asubst_soil_water_ini,
                                                               v = frac_asubst_soil_solid_ini),
                                                          \(x,y,z,v) (x/(y*5))*(z+v))) |>
      ## TWA concentration in soil
      mutate(conc_acsubst_total_soil_56twa_ug.kg = (conc_acsubst_total_soil_ini_ug.kg / (endday * (log(2) / DT50_typical_d))) * (1 - exp(-56 * (log(2) / DT50_typical_d)))) |> 
      ## Effect of terrain slope
      mutate(slope_effect = map_dbl(slope_perc,
                                    ~if_else(. <= 20,
                                             0.001423 * .^2 + 0.02153 * .,
                                             1))) |>
      ## Active substance application rate in a river segment
      mutate(aprate_farm_kg.ha = pmap_dbl(list(x = crop_acsubst_mass_lau.g,
                                               y = crop_acsubst_area_lau.ha,
                                               z = apfreq),
                                          \(x,y,z) ((x/y)/z)/1000)) |> 
      ##Soil chronic RQ
      mutate(rq_acsubst_total_soil_twa = conc_acsubst_total_soil_56twa_ug.kg/NOEC_earthworm_chron_repr_mg.kg) |> 
      ## Fraction of daily generated surface runoff. Mean, min, max are calculated over meteorological station within the basins
      ## Add code to match rainfall to AS application period for a given crop!!!!
      mutate(srunoff_month_sandy.mm = map_dbl(prec_month_mm,
                                              ~(-0.016427-0.011377*.+0.0026284*.^2-5.8564*10^-6*.^3)),
             srunoff_month_loamy.mm = map_dbl(prec_month_mm,
                                              ~(-0.061108-0.0041626*.+0.0040395*.^2-9.0361*10^-6*.^3)),
             srunoff_sandy_frac = map2_dbl(srunoff_month_sandy.mm,
                                           prec_month_mm,
                                           ~(.x/.y)),
             srunoff_loamy_frac = map2_dbl(srunoff_month_loamy.mm,
                                           prec_month_mm,
                                           ~(.x/.y)),
             srunoff_tot_frac = map2_dbl(srunoff_sandy_frac,
                                         srunoff_loamy_frac,
                                         ~(.x + .y))) |> 
      ## AS surface runoff loading
      mutate(srunoff_as_load = pmap_dbl(list(acsubst_area_ha,
                                             aprate_farm_kg.ha,
                                             infactor_effect,
                                             frac_asubst_soil_water_lag,
                                             slope_effect,
                                             srunoff_tot_frac),
                                        prod)) |> 
      makeValid()
    
    # srunoffs <- tibble(srunoff_mean_sandy_mm.day ,
    #                    srunoff_mean_loamy_mm.day ,
    #                    srunoff_min_sandy_mm.day ,
    #                    srunoff_min_loamy_mm.day ,
    #                    srunoff_max_sandy_mm.day ,
    #                    srunoff_max_loamy_mm.day ) |>
    #   bind_cols(meteo_stations_prec_basins) |>
    #   mutate(across(starts_with("srunoff"), ~ case_when(.x < 0 ~ 0, .default = .x)))
    # filter(srunoff_max_loamy_mm.day > 0 & srunoff_max_sandy_mm.day > 0)
    
    # Calculate daily amount of pesticide load potentially reaching a stream from individual farms
    # Calculate lagged and time-weighted soil concentration
    # srunoff_acsubst_farm <- list()
    # for (i in seq_along(load_acsubst_farm)) {
    
    #   srunoff_acsubst_farm[[i]] <-  merge(load_acsubst_farm[i] , srunoffs) |>
    #     mutate(srunoff_mean_fraction = case_when(sand_perc > clay_perc ~ srunoff_mean_sandy_mm.day/rain_mean_mm.day,
    #                                              sand_perc < clay_perc ~ srunoff_mean_loamy_mm.day/rain_mean_mm.day),
    #            srunoff_min_fraction = case_when(sand_perc > clay_perc ~ srunoff_min_sandy_mm.day/rain_min_mm.day,
    #                                             sand_perc < clay_perc ~ srunoff_min_loamy_mm.day/rain_min_mm.day),
    #            srunoff_max_fraction = case_when(sand_perc > clay_perc ~ srunoff_max_sandy_mm.day/rain_max_mm.day,
    #                                             sand_perc < clay_perc ~ srunoff_max_loamy_mm.day/rain_max_mm.day),
    #            across(starts_with("srunoff"), ~ case_when(.x == "NaN" ~ 0, .default = .x)),
    #            frac_asubst_soil_water_lag = exp(-day * log(2) / DT50_typical_d) * frac_asubst_soil_water_ini,
    #            frac_asubst_soil_solid_lag = exp(-day * log(2) / DT50_typical_d) * frac_asubst_soil_solid_ini,
    #            conc_acsubst_total_soil_lag_g.kg = (frac_asubst_soil_water_lag + frac_asubst_soil_solid_lag) * conc_acsubst_total_soil_ini,
    #            conc_acsubst_total_soil_twa_g.kg = (conc_acsubst_total_soil_ini / (ndays * (log(2) / DT50_typical_d))) * (1 - exp(-ndays * (log(2) / DT50_typical_d))),
    #            # Product of AS runoff components
    #            load_acsubst_prod = pmap_dbl(list(crop_acsubst_area_basin_ha,
    #                                              aprate_basin,
    #                                              infactor_effect,
    #                                              frac_asubst_soil_water_lag,
    #                                              slope_effect),
    #                                         prod),
    #            rain_mean_mm.ndays = mean(rain_mean_mm.day),
    #            rain_min_mm.ndays = mean(rain_min_mm.day),
    #            rain_max_mm.ndays = mean(rain_max_mm.day),
    #            load_acsubst_mean_g.day = load_acsubst_prod * srunoff_mean_fraction,
    #            load_acsubst_min_g.day = load_acsubst_prod * srunoff_min_fraction,
    #            load_acsubst_max_g.day = load_acsubst_prod * srunoff_max_fraction,
    #            load_acsubst_mean_g.ndays = mean(load_acsubst_mean_g.day),
    #            load_acsubst_min_g.ndays = mean(load_acsubst_min_g.day),
    #            load_acsubst_max_g.ndays = mean(load_acsubst_max_g.day)) |> 
    #     slice(1)
    
    #   rename("srunoff_day" = "day")
    
    #   cat("\r", i ,"farm out of", nrow(load_acsubst_farm), "is being processed")
    
    # }
    
    # Combine AS concentration in soil results on individual farms
    # Calculate RQ for individual farms and individual AS
    
    # soil_farm_mapinput <- dir_ls(path_home_r(), recurse = T, regexp = "Břeclav_map_input") |> vect()
    
    soil_farm_mapinput <- load_acsubst_farm |> 
      select(conc_acsubst_total_soil_56twa_ug.kg,
             rq_acsubst_total_soil_twa,
             srunoff_as_load,
             Crop,
             acsubst,
             District,
             ZKOD,
             CTVEREC) |> 
      mutate(conc_acsubst_total_soil_twa_g.kg = conc_acsubst_total_soil_56twa_ug.kg,
             load_acsubst_mean_g.ndays = srunoff_as_load,
             rq_acsubst_total_soil_twa = rq_acsubst_total_soil_twa/1000) |>
      mask(lau_name[name])
    
    # Calculate cumulative RQ in soil for individual fields
    # Create a character type column showing a list of ASs and RQs for each field
    soil_RQ_nest <- soil_farm_mapinput |>  
      filter(acsubst %in% acsubst_soil) |> 
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
      select("ZKOD", "CTVEREC", "Crop", "acsubst", "rq_acsubst_total_soil_twa") |> 
      group_by(ZKOD, CTVEREC, Crop) |> 
      summarise(sum_rq_acsubst_total_soil_twa = sum(rq_acsubst_total_soil_twa),
                .groups = "keep") |> 
      makeValid()
    
    # Merge nested RQs and summed RQs  datasets by unique field IDs
    soil_cumRQ_all <- terra::merge(soil_cumRQ, soil_RQ_nest, by = c("Crop", "ZKOD", "CTVEREC")) |> 
      mutate(AS_rq = str_remove_all(AS_rq, "\""),
             AS_rq = str_remove_all(AS_rq, "list"),
             AS_rq = str_remove_all(AS_rq, "AS_rq"),
             AS_rq = str_remove_all(AS_rq, "c"),
             AS_rq = str_remove_all(AS_rq, "="),
             AS_rq = str_remove_all(AS_rq, "\\("),
             AS_rq = str_remove_all(AS_rq, "\\)"),
             AS_rq = str_replace_all(AS_rq, "0.01$", "<0.01")) |>
      mask(lau_name[name])
    
    # Intersect farm loadings with river buffer segments
    # Calculate RQ for individual fields intersected with buffer around river segments
    river_seg_mapinput <- soil_farm_mapinput[c("srunoff_as_load",
                                               "acsubst",
                                               "District")] |>
      filter(acsubst %in% acsubst_soil) |> 
      terra::intersect(rivers_lau_buff_seg[c("HYDROID", "NAMN1", "SHAPE_Leng", "dis_m3_pyr", "dis_m3_pmn", "dis_m3_pmx")]) |>
      terra::merge(chemprop[c("Active", "NOEC_fish_21_mg.L")], by.x = "acsubst", by.y = "Active") |>
      mutate(conc_mean_river_seg = (srunoff_as_load/dis_m3_pyr),
             rq_mean_river_seg_twa = conc_mean_river_seg/NOEC_fish_21_mg.L/1000) |> 
      mask(lau_name[name])
    
    farm_area_buff <- terra::merge(rivers_lau_buff_seg,
                                   cbind(river_seg_mapinput[c("HYDROID", "NAMN1", "SHAPE_Leng")] |> 
                                           terra::aggregate(c("HYDROID", "NAMN1", "SHAPE_Leng")),
                                         river_seg_mapinput[c("HYDROID", "NAMN1", "SHAPE_Leng")] |> 
                                           terra::aggregate(c("HYDROID", "NAMN1", "SHAPE_Leng")) |> 
                                           expanse()) |> values(),
                                   by = c("HYDROID", "NAMN1", "SHAPE_Leng")) |> 
      rename(farm_buff_area.m = y)
    
    buff_area <- cbind(rivers_lau_buff_seg[c("HYDROID", "NAMN1", "SHAPE_Leng", "dis_m3_pyr")] |> 
                         aggregate(c("HYDROID", "NAMN1", "SHAPE_Leng")),
                       rivers_lau_buff_seg[c("HYDROID", "NAMN1", "SHAPE_Leng", "dis_m3_pyr")] |> 
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
      summarise(sum_rq_mean_river_seg_twa = max(rq_mean_river_seg_twa * river_w), .groups = "keep") |> 
      makeValid()
    
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
      mask(lau_name[name]) |> 
      values()
    
    # Merge all values from the RQ dataframe with river buffer polygons
    river_cumRQ_all <- terra::merge(rivers_lau_buff_seg[c("HYDROID", "NAMN1", "SHAPE_Leng")],
                                    river_cumRQ_df,
                                    by = c("HYDROID", "NAMN1", "SHAPE_Leng")) 
    
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
    
    # Soil PEC and RQ individual maps 
    for (as in seq_along(acsubst_name <- actual_soil_acsubst_name)) {
      
      # Store current substance name for progress updates
      current_substance <- acsubst_name[as]
      
      # Prepare data
      # Prepare soil data
      soil_data <- soil_farm_mapinput |> 
        filter(acsubst %in% acsubst_name[as])
      
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
        lau_name[name]$LAU_NAME
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
        lau_name[name]$LAU_NAME
      )
      
      # AS concentration in topsoil
      if (!pb$finished) pb$tick(tokens = list(map_type = "Creating Soil PEC map", substance = current_substance))
      
      soil_as_conc <- leaflet() |>
        addTiles(options = tileOptions(opacity = 0.5)) |> 
        
        # Add district borders
        addPolylines(
          data = lau_name[name],
          color = "black",
          weight = 0.75,
          opacity = 1,
          fillOpacity = 0,
          group = "Administrative borders"
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
          overlayGroups = c("Administrative borders", "Individual fields"),
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
        addControl(html = paste0("<div style='padding: 8px 12px; box-shadow: 0 2px 4px rgba(0,0,0,0.2); ",
                                 "font-size: 14px; font-weight: bold; max-width: calc(100vw - 350px); ",
                                 "width: fit-content; line-height: 1.5; word-wrap: break-word; z-index: 1000;'>",
                                 soil_conc_title, 
                                 "</div>"),
                   position = "topleft") |> 
        addControl(html = "", position = "bottomleft") %>%
        htmlwidgets::onRender("
    function(el, x) {
      if (this._controlCorners) {
        var existingZoom = this._controlCorners.topleft.querySelector('.leaflet-control-zoom');
        if (existingZoom) {
          existingZoom.remove();
        }
      }
      
      var zoomControl = L.control.zoom({
        position: 'bottomleft'
      });
      zoomControl.addTo(this);
      
      var titleControl = document.querySelector('.leaflet-top.leaflet-left .leaflet-control');
        if (titleControl) {
          window.addEventListener('resize', function() {
            var mapWidth = el.offsetWidth;
            var maxWidth = Math.max(200, mapWidth - 350);
            titleControl.style.maxWidth = maxWidth + 'px';
          });
        }
    }
  ")
      
      saveWidget(soil_as_conc, file = paste0(dir_create(path_home_r(),
                                                        "/Soil/PEC"),
                                             "/", 
                                             paste0(lau_name[name]$LAU_NAME,
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
          data = lau_name[name],
          color = "black",
          weight = 0.75,
          opacity = 1,
          fillOpacity = 0,
          group = "Administrative borders"
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
          overlayGroups = c("Administrative borders", "Individual fields"),
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
        addControl(html = paste0("<div style='padding: 8px 12px; box-shadow: 0 2px 4px rgba(0,0,0,0.2); ",
                                 "font-size: 14px; font-weight: bold; max-width: calc(100vw - 350px); ",
                                 "width: fit-content; line-height: 1.5; word-wrap: break-word; z-index: 1000;'>",
                                 soil_rq_title, 
                                 "</div>"),
                   position = "topleft") |> 
        addControl(html = "", position = "bottomleft") %>%
        htmlwidgets::onRender("
    function(el, x) {
      if (this._controlCorners) {
        var existingZoom = this._controlCorners.topleft.querySelector('.leaflet-control-zoom');
        if (existingZoom) {
          existingZoom.remove();
        }
      }
      
      var zoomControl = L.control.zoom({
        position: 'bottomleft'
      });
      zoomControl.addTo(this);
      
     var titleControl = document.querySelector('.leaflet-top.leaflet-left .leaflet-control');
        if (titleControl) {
          window.addEventListener('resize', function() {
            var mapWidth = el.offsetWidth;
            var maxWidth = Math.max(200, mapWidth - 350);
            titleControl.style.maxWidth = maxWidth + 'px';
          });
        }
    }
  ")
      
      saveWidget(soil_ind_rq_dist, file = paste0(dir_create(path_home_r(),
                                                            "/Soil/RQ"),
                                                 "/", 
                                                 paste0(lau_name[name]$LAU_NAME,
                                                        "_",
                                                        acsubst_name[as],
                                                        "_RQ_Soil.html")), selfcontained = T)
    }
    
    # Water PEC and RQ individual maps 
    for (as in seq_along(acsubst_name <- actual_river_acsubst_name)) {
      
      # Store current substance name for progress updates
      current_substance <- acsubst_name[as]
      
      # Prepare data
      # Prepare river water data
      
      river_data <- terra::merge(rivers_lau_buff_seg[c("HYDROID", "NAMN1", "SHAPE_Leng")],
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
                                   ungroup() |> 
                                   values(),
                                 by = c("HYDROID", "NAMN1", "SHAPE_Leng"))
      
      river_net_lau <- river_net |> 
        mask(lau_name[name])
      
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
        lau_name[name]$LAU_NAME
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
        lau_name[name]$LAU_NAME
      )
      
      # AS concentration in river water
      if (!pb$finished) pb$tick(tokens = list(map_type = "Creating Water PEC map", substance = current_substance))
      
      river_as_conc <- leaflet() |>
        addTiles(options = tileOptions(opacity = 0.5)) |> 
        
        # Add district borders
        addPolylines(
          data = lau_name[name],
          color = "black",
          weight = 0.75,
          opacity = 1,
          fillOpacity = 0,
          group = "Administrative borders"
        ) |>
        
        # Add river water network
        addPolylines(data = river_net_lau,
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
        addPolygons(data = basins_lau_cz,
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
          overlayGroups = c("Administrative  borders", "Individual river segments", "River network", "River basins"),
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
        addControl(html = paste0("<div style='padding: 8px 12px; box-shadow: 0 2px 4px rgba(0,0,0,0.2); ",
                                 "font-size: 14px; font-weight: bold; max-width: calc(100vw - 350px); ",
                                 "width: fit-content; line-height: 1.5; word-wrap: break-word; z-index: 1000;'>",
                                 river_conc_title, 
                                 "</div>"),
                   position = "topleft") |> 
        addControl(html = "", position = "bottomleft") %>%
        htmlwidgets::onRender("
    function(el, x) {
      if (this._controlCorners) {
        var existingZoom = this._controlCorners.topleft.querySelector('.leaflet-control-zoom');
        if (existingZoom) {
          existingZoom.remove();
        }
      }
      
      var zoomControl = L.control.zoom({
        position: 'bottomleft'
      });
      zoomControl.addTo(this);
      
      var titleControl = document.querySelector('.leaflet-top.leaflet-left .leaflet-control');
        if (titleControl) {
          window.addEventListener('resize', function() {
            var mapWidth = el.offsetWidth;
            var maxWidth = Math.max(200, mapWidth - 350);
            titleControl.style.maxWidth = maxWidth + 'px';
          });
        }
    }
  ")
      
      saveWidget(river_as_conc, file = paste0(dir_create(path_home_r(),
                                                         "/Water/PEC"),
                                              "/", 
                                              paste0(lau_name[name]$LAU_NAME,
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
          data = lau_name[name],
          color = "black",
          weight = 0.75,
          opacity = 1,
          fillOpacity = 0,
          group = "Administrative  borders"
        ) |>
        
        # Add river water network
        addPolylines(data = river_net_lau,
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
        addPolygons(data = basins_lau_cz,
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
          overlayGroups = c("Administrative  borders", "Individual river segments", "River network", "River basins"),
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
        addControl(html = paste0("<div style='padding: 8px 12px; box-shadow: 0 2px 4px rgba(0,0,0,0.2); ",
                                 "font-size: 14px; font-weight: bold; max-width: calc(100vw - 350px); ",
                                 "width: fit-content; line-height: 1.5; word-wrap: break-word; z-index: 1000;'>",
                                 river_rq_title, 
                                 "</div>"),
                   position = "topleft") |> 
        addControl(html = "", position = "bottomleft") %>%
        htmlwidgets::onRender("
    function(el, x) {
      if (this._controlCorners) {
        var existingZoom = this._controlCorners.topleft.querySelector('.leaflet-control-zoom');
        if (existingZoom) {
          existingZoom.remove();
        }
      }
      
      var zoomControl = L.control.zoom({
        position: 'bottomleft'
      });
      zoomControl.addTo(this);
      
       var titleControl = document.querySelector('.leaflet-top.leaflet-left .leaflet-control');
        if (titleControl) {
          window.addEventListener('resize', function() {
            var mapWidth = el.offsetWidth;
            var maxWidth = Math.max(200, mapWidth - 350);
            titleControl.style.maxWidth = maxWidth + 'px';
          });
        }
    }
  ")
      
      saveWidget(river_ind_rq_dist, file = paste0(dir_create(path_home_r(),
                                                             "/Water/RQ"),
                                                  "/", 
                                                  paste0(lau_name[name]$LAU_NAME,
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
      lau_name[name]$LAU_NAME
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
      lau_name[name]$LAU_NAME
    )
    
    # Cumulative RQ soil map
    soil_cum_rq_dist <- leaflet() |>
      # Add OpenStreetMap tiles with transparency
      addTiles(options = tileOptions(opacity = 0.5)) |>
      
      # Add district borders
      addPolylines(
        data = lau_name[name],
        color = "black",
        weight = 0.5,
        opacity = 1,
        fillOpacity = 0,
        group = "Administrative borders"
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
                        "<b>Top contributors (RQ \u2265 0.01):</b>" ,"<br>", AS_rq),
        highlightOptions = highlightOptions(color = "black",
                                            weight = 3,
                                            bringToFront = TRUE),
        group = "Individual fields"
      ) |>
      
      # Add layer controls
      addLayersControl(
        overlayGroups = c("Administrative borders", "Individual fields"),
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
      addControl(html = paste0("<div style='padding: 8px 12px; box-shadow: 0 2px 4px rgba(0,0,0,0.2); ",
                               "font-size: 14px; font-weight: bold; max-width: calc(100vw - 350px); ",
                               "width: fit-content; line-height: 1.5; word-wrap: break-word; z-index: 1000;'>",
                               soil_rqcum_title, 
                               "</div>"),
                 position = "topleft") |> 
      addControl(html = "", position = "bottomleft") %>%
      htmlwidgets::onRender("
    function(el, x) {
      if (this._controlCorners) {
        var existingZoom = this._controlCorners.topleft.querySelector('.leaflet-control-zoom');
        if (existingZoom) {
          existingZoom.remove();
        }
      }
      
      var zoomControl = L.control.zoom({
        position: 'bottomleft'
      });
      zoomControl.addTo(this);
      
       var titleControl = document.querySelector('.leaflet-top.leaflet-left .leaflet-control');
        if (titleControl) {
          window.addEventListener('resize', function() {
            var mapWidth = el.offsetWidth;
            var maxWidth = Math.max(200, mapWidth - 350);
            titleControl.style.maxWidth = maxWidth + 'px';
          });
        }
    }
  ")
    
    saveWidget(soil_cum_rq_dist, file = paste0(path_home_r(),
                                               "/Soil/RQ/",
                                               lau_name[name]$LAU_NAME,
                                               "_RQcum_Soil.html"), selfcontained = T)
    
    write_excel_csv(soil_farm_mapinput |> values(), paste0(path_home_r(),
                                                           "/Soil/PEC/",
                                                           lau_name[name]$LAU_NAME,
                                                           "_PEC_Soil.csv")) 
    
    write_excel_csv(soil_cumRQ |> values(), paste0(path_home_r(),
                                                   "/Soil/RQ/",
                                                   lau_name[name]$LAU_NAME,
                                                   "_",
                                                   "RQcum_Soil.csv")) 
    
    # Cumulative RQ river water
    river_cum_rq_dist <- leaflet() |>
      # Add OpenStreetMap tiles with transparency
      addTiles(options = tileOptions(opacity = 0.5)) |>
      
      # Add district borders
      addPolylines(
        data = lau_name[name],
        color = "black",
        weight = 0.75,
        opacity = 1,
        fillOpacity = 0,
        group = "Administrative borders"
      ) |>
      
      # Add river water network
      addPolylines(data = river_net_lau,
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
                        "<b>Top contributors (RQ \u2265 0.001): </b>" ,"<br>", AS_rq),
        highlightOptions = highlightOptions(color = "black",
                                            weight = 3,
                                            bringToFront = TRUE),
        group = "Individual river segments"
      ) |>
      
      # Add river basins polgons
      addPolygons(data = basins_lau_cz,
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
        overlayGroups = c("Administrative borders", "Individual river segments", "River network", "River basins"),
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
      addControl(html = paste0("<div style='padding: 8px 12px; box-shadow: 0 2px 4px rgba(0,0,0,0.2); ",
                               "font-size: 14px; font-weight: bold; max-width: calc(100vw - 350px); ",
                               "width: fit-content; line-height: 1.5; word-wrap: break-word; z-index: 1000;'>",
                               river_rqcum_title, 
                               "</div>"),
                 position = "topleft") |> 
      addControl(html = "", position = "bottomleft") %>%
      htmlwidgets::onRender("
    function(el, x) {
      if (this._controlCorners) {
        var existingZoom = this._controlCorners.topleft.querySelector('.leaflet-control-zoom');
        if (existingZoom) {
          existingZoom.remove();
        }
      }
      
      var zoomControl = L.control.zoom({
        position: 'bottomleft'
      });
      zoomControl.addTo(this);
      
       var titleControl = document.querySelector('.leaflet-top.leaflet-left .leaflet-control');
        if (titleControl) {
          window.addEventListener('resize', function() {
            var mapWidth = el.offsetWidth;
            var maxWidth = Math.max(200, mapWidth - 350);
            titleControl.style.maxWidth = maxWidth + 'px';
          });
        }
    }
  ")
    
    saveWidget(river_cum_rq_dist, file = paste0(path_home_r(),
                                                "/Water/RQ/",
                                                lau_name[name]$LAU_NAME,
                                                "_RQcum_Water.html"), selfcontained = T)  
    
    write_excel_csv(river_seg_mapinput |> values(), paste0(path_home_r(),
                                                           "/Water/PEC/",
                                                           lau_name[name]$LAU_NAME,
                                                           "_PEC_Water.csv"))
    
    write_excel_csv(river_cumRQ |> values(), paste0(path_home_r(),
                                                    "/Water/RQ/",
                                                    lau_name[name]$LAU_NAME,
                                                    "_RQcum_Water.csv")) 
  }
  #################################################################
  ########## END: Pesticide concentration and risk maps ###########
  #################################################################
}

# Main function call #
# Use it for each new scenario
# It might take up to several minutes to run this function and to generate all the maps for a single LAU. It depends on how many fields and rivers are in the LAU
map_topsoil_riverwater(lau_name = lau_name,
                         acsubst_name = acsubst_name,
                         app_month = app_month,
                         endday = endday,
                         rivbuff_width = rivbuff_width)


#########################################################
########### START: Model results evaluation #############
#########################################################
 
# CZ sampling site cooridnates. It is needed for model evaluation. It can be also used to select LAUs where sampling took place #
# site_id_cz <- c("F01", "F02", "F03", "F04", "F05", "F06", "F07", "F08", "F09", "F10",
#                 "F11", "F12", "F13", "F14", "F15", "F16", "F17", "F18", "F19", "F20",
#                 "F21", "F22", "F23", "F24")
# 
# site_id_lat_cz <- c(48.75428, 48.7651997, 49.5882283, 48.9971858, 48.8974494, 
#                     48.9203217, 49.5573031, 49.7089406, 49.7190786, 50.5149308,
#                     50.5392117, 50.4087597, 49.0598786, 49.0573903, 50.1286728,
#                     50.2487525, 50.22116, 49.7082947, 48.9869569, 49.6619561,
#                     49.7321864, 48.8276081, 48.8326775, 48.7364833)
# 
# site_id_lon_cz <- c(16.9401214, 16.9324394, 17.1860597, 16.9157722, 16.0395308,
#                     15.9784944, 13.9029903, 14.8711725, 14.8851656, 16.196935,
#                     16.1935875, 14.1494492, 15.4676047, 15.4676261, 16.2425311,
#                     17.6263658, 15.9933244, 14.7895919, 17.0273617, 12.9795581,
#                     12.9605036, 16.5171839, 16.4857483, 16.9331583)
# 
# site_coord_cz <- cbind(tibble(
#   site_id = site_id_cz,
#   long = site_id_lon_cz,
#   lat = site_id_lat_cz)) |> 
#   sf::st_as_sf(coords = c("long", "lat"), crs = st_crs(lau_cz)) |> 
#   vect() |> 
#   terra::intersect(lau_cz)


# Evaluation dataset
pec_soil <- dir_ls(path_home_r(),
                    recurse = T,
                    regexp = "2dist_3chem_topsoil_farm.gpkg") |> vect()
 
gemup100 <-  dir_ls(path_home_r(), recurse = T, regexp = "gemup100") |> 
  vect() |>
  filter(Active %in% c("acetamiprid", "tebuconazole", "glyphosate")) |> 
  mutate(acsubst = Active |> str_to_title())

gemup100_oilseed <- gemup100 |> 
  filter(str_detect(Crop,  regex("rape")))

writeVector(gemup3_pecsoil, "gemup3chem_allcrop_2distr.gpkg")

gemup3_pecsoil <-merge(gemup100 |> 
                         select(acsubst, Crop, ZKOD, CTVEREC, District), pec_soil |> 
                         values(), all.y = T, by = c("ZKOD", "CTVEREC", "acsubst")) |>
  filter(District %in% c("benešov", "břeclav"))


# Add PEC and MEC soil data collected by Vera and Knuth et al. 2024
Knuth14 <- terra::merge(site_coord_cz,
   bind_rows(tibble(acsubst = "Glyphosate",
                            PEC_ug.kg = c(NA, 21.3, NA, NA, 46.7, 22.2, NA, NA, NA, NA, NA, NA, 42.1, NA, 59.69, NA, NA, NA, NA, NA, NA, NA, NA, NA)),
                     tibble(acsubst = "Tebuconazole",
                            PEC_ug.kg = c(88.6, NA, 54.1, 27.2,  23.3,  27.5, NA, 27.4, 47.4, 109.1, 24.8, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)),
                     tibble(acsubst = "Acetamiprid",
                            PEC_ug.kg = c(NA, 20, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))) |>
  cbind(tibble(Source = "Knuth et al. 2024",
               Crop = "Oilseed rape",
               site_id = site_coord_cz$site_id
              ), by = "site_id"))


eval_data <- terra::merge(Knuth14 |> 
                             filter(!is.na(PEC_ug.kg)),
                           load_acsubst_farm |>
                             filter(Crop %in% c("Winter rape", "Spring rape")) |>
                             select(LAU_NAME,
                                    acsubst, 
                                    aprate_farm_g.ha,
                                    conc_acsubst_total_soil_ini_ug.kg,
                                    conc_acsubst_total_soil_56twa_ug.kg),
                           by = c("acsubst", "LAU_NAME"))

eval_data_df <- eval_data |> group_by(LAU_NAME, acsubst) |> 
  summarise(soil_conc_sample = max(PEC_ug.kg),
            soil_conc_ini_med = mean(conc_acsubst_total_soil_ini_ug.kg),
            soil_conc_ini_sd = sd(conc_acsubst_total_soil_ini_ug.kg),
            soil_conc_56d_med = mean(conc_acsubst_total_soil_56twa_ug.kg),
            soil_conc_56d_sd = sd(conc_acsubst_total_soil_56twa_ug.kg),
            nr_field = n()) |>
  values() |> 
  bind_cols("Concentration units" = "µg × kg⁻¹") |> 
  rename("Location" = LAU_NAME,
         "Active substance" = acsubst,
         "Conentration sample" = soil_conc_sample,
         "Concentration intial" = soil_conc_ini_med,
        "Concentration intial SD" = soil_conc_ini_sd,
         "Concentration 56 days" = soil_conc_56d_med,
         "Concentration 56 days SD" = soil_conc_56d_sd,
         "# of fields" = nr_field)

write_excel_csv(eval_data_df, "soil concentration comparison.xlsx")

# Read the data
soil_conc <- read.csv(dir_ls(path_home_r(), recurse = T, regexp = "soil concentration comparison.csv"))

# Prepare data for plotting
soil_conc_long <- soil_conc %>%
  # Create location label with number of fields
  mutate(Location_label = paste0(Location, "\n(n=", X..of.fields, ")")) %>%
  # Reshape data to long format
  pivot_longer(
    cols = c(Conentration.sample, Concentration.intial, Concentration.56.days),
    names_to = "Measurement_type",
    values_to = "Concentration"
  ) %>%
  # Add error bar values
  mutate(
    SD = case_when(
      Measurement_type == "Concentration.intial" ~ Concentration.intial.SD,
      Measurement_type == "Concentration.56.days" ~ Concentration.56.days.SD,
      TRUE ~ NA_real_
    ),
    # Clean up measurement type labels
    Measurement_type = case_when(
      Measurement_type == "Conentration.sample" ~ "Sample (measured)",
      Measurement_type == "Concentration.intial" ~ "Initial (modeled)",
      Measurement_type == "Concentration.56.days" ~ "56-day TWA (modeled)"
    )
  ) %>%
  # Reorder factor levels for plotting
  mutate(
    Measurement_type = factor(Measurement_type, 
                              levels = c("Sample (measured)", 
                                         "Initial (modeled)", 
                                         "56-day TWA (modeled)")),
    Location_label = factor(Location_label, 
                            levels = unique(Location_label[order(Location, Active.substance)]))
  )

# Create a data frame for vertical lines (separating locations)
# We need to add lines between each unique location
unique_locations <- soil_conc_long %>%
  distinct(Location_label, Active.substance) %>%
  arrange(Location_label) %>%
  group_by(Active.substance) %>%
  mutate(location_num = row_number()) %>%
  ungroup()

vline_data <- unique_locations %>%
  group_by(Active.substance) %>%
  filter(location_num < max(location_num)) %>%
  mutate(xintercept = location_num + 0.5) %>%
  ungroup()

# Create the plot
p <- ggplot(soil_conc_long, 
            aes(x = Location_label, y = Concentration, fill = Measurement_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  geom_errorbar(aes(ymin = Concentration - SD, ymax = Concentration + SD),
                position = position_dodge(width = 0.9),
                width = 0.25,
                na.rm = TRUE) +
  # Add vertical dashed lines to separate locations
  geom_vline(data = vline_data, 
             aes(xintercept = xintercept), 
             linetype = "dashed", 
             color = "gray50", 
             linewidth = 0.5) +
  facet_wrap(~ Active.substance, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("Sample (measured)" = "#E69F00", 
                               "Initial (modeled)" = "#56B4E9", 
                               "56-day TWA (modeled)" = "#009E73")) +
  labs(
    title = "Comparison of Measured and Modeled Pesticide Concentrations in Soil",
    subtitle = "Error bars represent standard deviation (SD) for modeled values",
    x = "Location (n = number of fields growing oilseed rape)",
    y = paste0("Concentration \u00B5g\u00D7kg\u207B\u00B9"),
    fill = "Measurement Type"
  ) +
  theme_minimal() +
  theme(
    # Remove all grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Keep axis lines
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "lightgray", color = NA),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Display the plot
print(p)

p_glyphosate <- soil_conc_long %>%
  filter(Active.substance == "Glyphosate") %>%
  ggplot(aes(x = Location_label, y = Concentration, fill = Measurement_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  geom_errorbar(aes(ymin = Concentration - SD, ymax = Concentration + SD),
                position = position_dodge(width = 0.9),
                width = 0.25,
                na.rm = TRUE) +
  # Add vertical dashed lines
  geom_vline(data = vline_data %>% filter(Active.substance == "Glyphosate"), 
             aes(xintercept = xintercept), 
             linetype = "dashed", 
             color = "gray50", 
             linewidth = 0.5) +
  scale_fill_manual(values = c("Sample (measured)" = "#E69F00", 
                               "Initial (modeled)" = "#56B4E9", 
                               "56-day TWA (modeled)" = "#009E73")) +
  labs(
    title = "Glyphosate: Comparison of Measured and Modeled Concentrations in Soil",
    subtitle = "Error bars represent standard deviation (SD) for modeled values",
    x = "Location (n = number of fields growing oilseed rape)",
    y = paste0("(\u00B5g\u00D7kg\u207B\u00B9)"),
    fill = "Measurement Type"
  ) +
  theme_minimal() +
  theme(
    # Remove all grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Keep axis lines
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "lightgray", color = NA),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

p_tebuconazole <- soil_conc_long %>%
  filter(Active.substance == "Tebuconazole") %>%
  ggplot(aes(x = Location_label, y = Concentration, fill = Measurement_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  geom_errorbar(aes(ymin = Concentration - SD, ymax = Concentration + SD),
                position = position_dodge(width = 0.9),
                width = 0.25,
                na.rm = TRUE) +
  # Add vertical dashed lines
  geom_vline(data = vline_data %>% filter(Active.substance == "Tebuconazole"), 
             aes(xintercept = xintercept), 
             linetype = "dashed", 
             color = "gray50", 
             linewidth = 0.5) +
  scale_fill_manual(values = c("Sample (measured)" = "#E69F00", 
                               "Initial (modeled)" = "#56B4E9", 
                               "56-day TWA (modeled)" = "#009E73")) +
  labs(
    title = "Tebuconazole: Comparison of Measured and Modeled Concentrations in Soil",
    subtitle = "Error bars represent standard deviation (SD) for modeled values",
    x = "Location (n = number of fields growing oilseed rape)",
    y = paste0("(\u00B5g\u00D7kg\u207B\u00B9)"),
    fill = "Measurement Type"
  ) +
  theme_minimal() +
  theme(
    # Remove all grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Keep axis lines
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "lightgray", color = NA),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )


tmap_mode("plot")
basemap_lau <- tm_shape(lau_cz) +
  tm_borders(col = "black",
             lty = "dashed",
             lwd = 0.75) +
  tm_basemap("Esri.WorldTopoMap", alpha = 0.75) +
  # tmap_options(component.autoscale = T) +
  tm_scalebar(position = c("right", "bottom"))

layout_map <- tm_layout(legend.outside = T,
                        legend.outside.position = "right",
                        legend.outside.size = 0.5,
                        legend.text.size = 0.5,
                        legend.title.size = 0.75,
                        main.title.fontface = "bold",
                        scale = 1.25, 
                        # main.title.size = 0.5
                        )

basemap_lau +
layout_map +
  tm_shape(lau_cz) +
    tm_borders(lwd = 0.15) +
  tm_shape(lau_cz |> filter(LAU_NAME %in% lau_name$LAU_NAME)) +
  tm_polygons(fill = "LAU_NAME",
             fill.scale = tm_scale(values = "poly.alphabet2"),
             fill.legend = tm_legend("LAU (2024)", frame = F)) +
  tm_shape(Knuth14["site_id"]) +
    tm_symbols(fill = "magenta",
               size = .35,) +
    tm_add_legend(type = "symbols",
                  fill = "magenta",
                  labels = "Sampling site",
                  frame = F) + 
  tm_title("Soil sampling locations (# of sites 24)")

# PLOTS
crop_soilconc_plt <- function(as) {
  
  pec30_soil <-  dir_ls(path_home_r(), recurse = T, regexp = "gemup3chem_allcrop_2distr.gpkg") |> 
    vect() |>
    # filter(acsubst %in% as) |> 
    values() |> 
    mutate(conc_total_soil_twa= conc_total_soil_twa*1000000) |> 
    group_by(acsubst, Crop) |> 
    summarise(PEC30_mean_crop_ug.kg =  conc_total_soil_twa |> mean(),
              std_pec = sd(conc_total_soil_twa),
              n = n()) |> 
    filter(!is.na(PEC30_mean_crop_ug.kg))

  
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

######################################################
########### END: Model results evaluation ############
######################################################

# CHMU Rainfall#
# For the moment: rainfall data remain separate dataset to be later connected with the crop BBCH and scheduled in PPP registry AS application
# meteo_stations <- read.csv(dir_ls(path_home_r(),
#                                   recurse = T,
#                                   regexp = "stanice_souradnice.csv"),
#                            sep = ";") |>
#   as_tibble() |>
#   mutate(across(3:5,
#                 \(x) str_replace_all(x,
#                                      pattern = ",",
#                                      replacement =  ".")),
#          Elevation = as.numeric(Elevation),
#          Geogr1 = as.numeric(Geogr1),
#          Geogr2 = as.numeric(Geogr2)) |>
#   add_column(Elevation_unit = "m")
# 
# Sys.setlocale("LC_TIME", "uk")
# 
# start_date <- make_date(year = sim_yr, month = match(app_month, month.name), day = app_startday)
# end_date <- start_date + days(endday)
# sim_months <- seq(start_date, end_date, by = "day") |>
#   lubridate::month(label = TRUE, abbr = FALSE, locale = Sys.getlocale("LC_TIME")) |>
#   unique()
# 
# meteo_stations_prec_basin <- dir_ls(path_home_r(),
#                                     recurse = T,
#                                     regexp = "srazky_SRA.csv") |>
#   read.csv(sep = ";",
#            dec = ",") |>
#   as_tibble() |>
#   mutate(value = str_replace(value, ",", "."),
#          value = as.numeric(value),
#          month = make_datetime(month = month) |>
#            lubridate::month(label = T,
#                             abbr = F,
#                             locale = Sys.getlocale("LC_TIME"))) |>
#   rename("rain_mm.day" = "value") |>
#   full_join(meteo_stations,
#             by = join_by(id == id),
#             relationship = "many-to-many") |>
#   filter(!is.na(rain_mm.day),
#          rain_mm.day >= 0,
#          # Filter months for which to run daily simulations
#          month %in% sim_months) |>
#   # Average daily rainfall in selected basins calculated from stations within basin polygons
#   vect(crs = "WGS84", geom = c("Geogr1", "Geogr2")) |>
#   mask(district_name[1]) |>
#   as_tibble() |>
#   group_by(month, day) |>
#   summarise(rain_mean_mm.day = mean(rain_mm.day),
#             rain_min_mm.day = min(rain_mm.day),
#             rain_max_mm.day = max(rain_mm.day)) |> 
#   ungroup() |> 
#   # Filter number of days: from first day of AS application to several day after
#   slice(1:endday) |> 
  #   mutate(ndays = n())
