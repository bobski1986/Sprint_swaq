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
# LAUs 2024. Administrative units, Gisco 2024, https://gisco-services.ec.europa.eu/distribution/v2/.
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

# lpis map 2021 #
# Used for selecting parcels based on conventional or ecological farming classes.
# This light version of the original data, contains only conventional farming (EKO = 0) parcels
lpis_cz <- dir_ls(path_home_r(), recurse = T, regexp = "lpis_slim.gpkg") |> 
  vect()

# River basin, Hydrosheds #
# Polygons and attributes from the Hydrosheds dataset
basins_cz <- dir_ls(path_home_r(), recurse = T, regexp = "hydrosheds_lvl10_basins_cz.gpkg") |>
  vect()

# River network, CUZK #
# River network 
river_net <- dir_ls(path_home_r(), recurse = T, regexp = "WatrcrsL.shp$") |>
  vect() |>
  project(lau_degurba_cz)

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

# List of priority active substances in soil and water#
acsubst_name <- c("dimoxystrobin", "difenoconazole", "boscalid", "fluazinam", 
                  "prochloraz", "diquat", "azoxystrobin", "pethoxamid", 
                  "benzovindiflupyr", "tebuconazole", "quinmerac", 
                  "mefentrifluconazole", "cyproconazole",  "tefluthrin", 
                  "picloram", "metazachlor", "pendimethalin", 
                  "gamma-cyhalothrin", "deltamethrin", "epoxiconazole", 
                  "glyphosate", "spiroxamine", "terbuthylazine", 
                  "flutriafol", "prothioconazole", "diflufenican", "chlorotoluron",
                  "imazamox", "bixafen", "aminopyralid", "fluopyram") |> 
  unique() |> 
  str_to_sentence()

# Check if all substances are in the PPDB script
# ASs[which(!acsubst_name %in% ASs)]

# Spatial extent #
# Use lau_okres_cz or lau_derugba_cz
# Change numbers/names in brackets to select area(s). 1) Use single value to select one LAU,
# 2) use single values in brackets separated with commas to select multiple LAUs e.g., c(1,3,77),
# 3) use ranges to select consecutive LAUs e.g., c(1:5, 64:73), 
# 4) use correctly spelled LAU names instead of integers
lau_name <- lau_sample_cz
  # lau_okres_cz[1, "LAU_NAME"]

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

# Main function for simulating and visualising ASs concentration in topsoil on individual fields and in river segments #
# Run it only once to create "function object"
# Don't change content in the curly brackets 
map_topsoil_riverwater_cz <- function(lau_name,
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
      terra::merge(lpis_cz |> values(),
                   by = c("ZKOD", "CTVEREC")) |> 
      makeValid()
    
    acsubst_gemup <- gemup_lau |> select(acsubst) |> values() |> unique() |> as.vector()
    acsubst_water <- acsubst_water[which(acsubst_water %in% acsubst_gemup$acsubst)]
    acsubst_soil <- acsubst_soil[which(acsubst_soil %in% acsubst_gemup$acsubst)]
    
    # Read basin polygons from Hydrosheds for the selected LAU
    basins_lau_cz <- basins_cz |> 
      select("HYBAS_ID", "dis_m3_pyr", "dis_m3_pmn", "dis_m3_pmx", "riv_tc_ssu", "riv_tc_usu", starts_with("pre_mm_")) |>
      mask(lau_name[name]) |> 
      simplifyGeom(0.005)
    
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
      stop(sprintf("Precipitation column '%s' not found in czech basin atlas. Available columns: %s",
                   prec_col_name,
                   paste(names(basins_lau_cz)[grepl("^pre_mm_s", names(basins_lau_cz))], collapse = ", ")))
    }
    
    # cat(sprintf("Using precipitation data from column: %s (for %s)\n", prec_col_name, app_month))
  
    # Alternatively use CHMU Rainfall. Code needs to be updated to run over all rainy days for each field !!! #
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
    
    ### Extract precipitation data for the specified month
    prec_lau <- basins_lau_cz[prec_col_name]
    
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
      crop(ext(gemup_lau))
    
    # ESDAC Organic carbon content in topsoil #
    # Data cropped to include only country of interest and saved. Done only once
    
    # oc_jrc <- rast(oc_jrc_path)
    # crs(oc_jrc) <- crs(srunoff_path |> rast())
    # oc_jrc <- oc_jrc |> project(srunoff_path |> rast() |> project(basins_cz))
    # oc_jrc_cz <- crop(oc_jrc, basins_cz)
    # writeCDF(oc_jrc_cz, filename = paste0(water_spatial_dir ,"/OC_jrc_CZ.nc"), overwrite = T)
    
    orcarb_jrc_cz <- organic_carbon |>
      select("OC_jrc_CZ") |>
      crop(ext(gemup_lau))
    
    # ESDAC topsoil physical properties for Europe (based on LUCAS topsoil data) #
    
    # sand_jrc_path <- paste0(water_spatial_dir, "Sand1.tif")
    # sand_jrc_laea <- rast(sand_jrc_path)
    # sand_jrc_cz_wgs84 <- sand_jrc_laea |>  project(basins_cz) |> crop(basins_cz)
    # writeCDF(sand_jrc_cz_wgs84, filename = paste0(water_spatial_dir ,"sand_jrc_CZ.nc"), overwrite = T)
    
    sand_jrc_cz <- sand |>
      crop(ext(gemup_lau))
    
    # clay_jrc_path <- paste0(water_spatial_dir, "Clay.tif")
    # clay_jrc_laea <- rast(clay_jrc_path)
    # clay_jrc_cz_wgs84 <- clay_jrc_laea |> project(basins_cz) |> crop(basins_cz)
    # writeCDF(clay_jrc_cz_wgs84, filename = paste0(water_spatial_dir ,"clay_jrc_CZ.nc"), overwrite = T)
    
    clay_jrc_cz <- clay |>
      crop(ext(gemup_lau))
    
    # budens_jrc_path <- paste0(water_spatial_dir, "Bulk_density.tif")
    # budens_jrc_laea <- rast(budens_jrc_path)
    # budens_jrc_cz_wgs84 <- budens_jrc_laea |>  project(basins_cz) |> crop(basins_cz)
    # writeCDF(budens_jrc_cz_wgs84, filename = paste0(water_spatial_dir ,"budens_jrc_CZ.nc"), overwrite = T)
    
    budens_jrc_cz <- bulk_dens |>
      crop(ext(gemup_lau))
    
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
    
    #####################################################################################
    ############ START: Spatial input data intersected with river basin #################
    #####################################################################################
    
    rivers_lau <- terra::intersect(river_net[c("HYDROID", "NAMN1", "SHAPE_Leng", "WD7")], basins_lau_cz)
    # Indicate buffer width around a river segment
    rivers_lau_buff_seg <- rivers_lau |> terra::buffer(rivbuff_width+rivers_lau$WD7)
    # Intersect gemup with selected river segments so the gemup is expanded to include hydrography of the selected river basin(s)
    gemup_lau_rivers_buff <- rivers_lau_buff_seg[gemup_lau]
  
    if(gemup_lau_rivers_buff |> nrow() == 0) {

    "Warning: There are no fields in the buffer. Only PEC and RQ soil maps will be generated."
      
      ##############################################################
      # In case of empty river buffers only soil data processesing #
      ##############################################################
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
        terra::merge(crop_area_tot_lau, by.x = c("Crop", "acsubst"), by.y = c("Crop", "acsubst")) |>
        terra::merge(crop_area_tot_distr, by = c("acsubst", "Crop", "District")) |>
        mutate(acsubst_mass.g = acsubst_mass_kg * 1000,
               crop_acsubst_area_frac_lau = crop_acsubst_totarea_lau.ha / acsubst_area_ha,
               crop_acsubst_area_lau.ha = crop_acsubst_totarea_lau.ha * crop_acsubst_area_frac_lau,
               crop_acsubst_mass_lau.g = crop_acsubst_area_lau.ha * aprate_farm_g.ha,
               crop_acsubst_mass_frac_lau = crop_acsubst_mass_lau.g / acsubst_mass.g) |>
        terra::merge(chemprop,
                     by.x = c("acsubst"),
                     by.y = c("Active"))
      
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
               frac_asubst_soil_solid_lag = exp(-endday * log(2) / DT50_typical_d) * frac_asubst_soil_solid_ini) |>
        ## Initial concentration in soil
        mutate(conc_acsubst_total_soil_ini_ug.kg = pmap_dbl(list(x = aprate_farm_g.ha,
                                                                 y = bulk_dens_kg.dm3,
                                                                 z = frac_asubst_soil_water_ini,
                                                                 v = frac_asubst_soil_solid_ini),
                                                            \(x,y,z,v) (x/(y*5))*(z+v))) |>
        ## TWA concentration in soil
        mutate(conc_acsubst_total_soil_56twa_ug.kg = (conc_acsubst_total_soil_ini_ug.kg / (56 * (log(2) / DT50_typical_d))) * (1 - exp(-56 * (log(2) / DT50_typical_d))),
               conc_acsubst_total_soil_365twa_ug.kg = (conc_acsubst_total_soil_ini_ug.kg / (365 * (log(2) / DT50_typical_d))) * (1 - exp(-365 * (log(2) / DT50_typical_d)))) |> 
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
        mutate(rq_acsubst_total_soil_twa = conc_acsubst_total_soil_56twa_ug.kg/(NOEC_earthworm_chron_repr_mg.kg*1000)) |> 
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
                                          prod))
      
      # Combine AS concentration in soil results on individual farms
      # Calculate RQ for individual farms and individual AS
      
      soil_farm_mapinput <- load_acsubst_farm |> 
        select(District,
               ZKOD,
               CTVEREC,
               Crop,
               acsubst,
               DT50_typical_d,
               aprate_farm_kg.ha,
               oc_perc,
               bulk_dens_kg.dm3,
               clay_perc,
               sand_perc,
               slope_perc,
               conc_acsubst_total_soil_56twa_ug.kg,
               conc_acsubst_total_soil_ini_ug.kg,
               srunoff_as_load,
               rq_acsubst_total_soil_twa) |> 
        mask(lau_name[name])
      
      # Calculate cumulative RQ in soil for individual fields
      # Create a character type column showing a list of ASs and RQs for each field
      soil_RQ_nest <- soil_farm_mapinput |>  
        # filter(acsubst %in% acsubst_soil) |> 
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
        select("District", "ZKOD", "CTVEREC", "Crop", "acsubst", "rq_acsubst_total_soil_twa") |> 
        group_by(District, ZKOD, CTVEREC, Crop) |> 
        summarise(sum_rq_acsubst_total_soil_twa = sum(rq_acsubst_total_soil_twa),
                  .groups = "keep") |> 
        makeValid()
      
      # Merge nested RQs and summed RQs  datasets by unique field IDs
      soil_cumRQ_all <- terra::merge(soil_cumRQ, soil_RQ_nest, by = c("Crop", "ZKOD", "CTVEREC")) |> 
        mutate(AS_rq = str_remove_all(AS_rq, "\""),
               AS_rq = str_remove_all(AS_rq, "list"),
               AS_rq = str_remove_all(AS_rq, "AS_rq"),
               AS_rq = str_remove(AS_rq, "c"),
               AS_rq = str_remove_all(AS_rq, "="),
               AS_rq = str_remove_all(AS_rq, "\\("),
               AS_rq = str_remove_all(AS_rq, "\\)"),
               AS_rq = str_replace_all(AS_rq, "0.01$", "<0.01")) |>
        mask(lau_name[name])
      
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
      

      
      actual_soil_acsubst_name <- soil_farm_mapinput$acsubst |> unique()
      
      pb <- progress_bar$new(
        format = "[:bar] :percent | :map_type | Substance: :substance | ETA: :eta",
        total = (length(actual_soil_acsubst_name) * 4),
        clear = FALSE,
        width = 100
      )

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
          domain = soil_data$conc_acsubst_total_soil_56twa_ug.kg,
          bins = pretty(soil_data$conc_acsubst_total_soil_56twa_ug.kg),
          na.color = "#ffdeaf"
        )
        
        soil_conc_pal_rev <- colorBin(
          palette = "BuPu",
          domain = soil_data$conc_acsubst_total_soil_56twa_ug.kg,
          bins = pretty(soil_data$conc_acsubst_total_soil_56twa_ug.kg),
          na.color = "#ffdeaf",
          reverse = T,
        )
        
        # RQ maps palettes
        # Soil RQ palette
        soil_rq_colors <- brewer.pal(11, "PRGn")[c(1,2,3,4,5,8,9,10)]
        
        soil_rq_pal <- colorBin(
          palette = soil_rq_colors |> rev(),
          domain = soil_data$rq_acsubst_total_soil_twa,
          bins = c(0, 0.2, 1, 2, 3, 5, 500),
          na.color = "#ffdeaf"
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
            fillColor = ~soil_conc_pal(conc_acsubst_total_soil_56twa_ug.kg),
            fillOpacity = 0.85,
            color = "black",
            weight = 0.25,
            opacity = 1,
            popup = ~paste0("<b>Field ID: </b>", "<b>", ZKOD, " (ZKOD)", ", ", CTVEREC, " (CTVEREC)", "</b>", "<br>",
                            "<b>Crop: </b>", "<b>", Crop, "</b>", "<br>",
                            "<b>Individual PEC soil (µg × kg⁻¹): </b>", "<b>",
                            ifelse(is.na(conc_acsubst_total_soil_56twa_ug.kg), 
                                   "Missing values",
                                   format_power10(conc_acsubst_total_soil_56twa_ug.kg,  digits = 2)), "</b>"),
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
            values = soil_data$conc_acsubst_total_soil_56twa_ug.kg,
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
                                                      "_PEC_Soil_CZ.html")), selfcontained = T)
        
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
                                                          "_RQ_Soil_CZ.html")), selfcontained = T)
      }

      # Cumulative RQ maps
      # Define color palette function
      # Using reversed Purple-Green scheme, skipping middle (white) color
      # For the categories of risk, it is important to have a cut at 0.2 (defined as safe level - exposure must be 5 times lower than NOEC). So, the first three categories would be <0.2, ≥ 0.2, ≥ 1... then, it could be ≥ 2, ≥ 3, ≥ 4...
      soil_RQcum_colors <- brewer.pal(11, "PRGn")[c(1,2,3,4,5,8,9,10)]
      
      soil_RQcum_pal <- colorBin(
        palette = rev(soil_RQcum_colors),
        domain = soil_cumRQ_all$sum_rq_acsubst_total_soil_twa,
        bins = c(0, 0.2, 1, 2, 3, 5, 10000),
        na.color = "#ffdeaf"
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
      
      if (!pb$finished) pb$tick(tokens = list(map_type = "Creating cumulative Soil RQ map"))
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
                          "<b>Number of pesticides used: </b>","<b>", nr_as, "</b>", "<br>",
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
      
      saveWidget(soil_cum_rq_dist,
                 file = paste0(path_home_r(),
                               "/Soil/RQ/",
                               lau_name[name]$LAU_NAME,
                               "_RQcum_Soil_CZ.html"),
                 selfcontained = T)
      
      write_excel_csv(soil_farm_mapinput |>
                        values()  |> 
                        select(-c(District, rq_acsubst_total_soil_twa)) |> 
                        cbind(endday |>
                                as.data.frame()) |> 
                        cbind(lau_name[name] |> 
                                as.data.frame()) |> 
                        rename(simtime_day = endday),
                      paste0(path_home_r(),
                             "/Soil/PEC/",
                             lau_name[name]$LAU_NAME,
                             "_PEC_Soil_CZ.csv"))
      
      write_excel_csv(soil_farm_mapinput |>
                        values()  |> 
                        select(-c(srunoff_as_load, District)) |> 
                        cbind(endday |>
                                as.data.frame()) |> 
                        cbind(lau_name[name] |> 
                                as.data.frame()) |> 
                        rename(simtime_day = endday),
                      paste0(path_home_r(),
                             "/Soil/RQ/",
                             lau_name[name]$LAU_NAME,
                             "_RQ_Soil_CZ.csv"))
      
      write_excel_csv(soil_cumRQ |>
                        values() |> 
                        select(-District) |> 
                        cbind(endday |>
                                as.data.frame()) |> 
                        cbind(lau_name[name] |> 
                                as.data.frame()) |> 
                        rename(simtime_day = endday),
                      paste0(path_home_r(),
                             "/Soil/RQ/",
                             lau_name[name]$LAU_NAME,
                             "_",
                             "RQcum_Soil_CZ.csv")) 
      
    }
  
    else {
    
      #######################################################################################
      ############ END: Spatial input data intersected with river basin ###################
      #######################################################################################
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
      terra::merge(crop_area_tot_lau, by.x = c("Crop", "acsubst"), by.y = c("Crop", "acsubst")) |>
      terra::merge(crop_area_tot_distr, by = c("acsubst", "Crop", "District")) |>
      mutate(acsubst_mass.g = acsubst_mass_kg * 1000,
             crop_acsubst_area_frac_lau = crop_acsubst_totarea_lau.ha / acsubst_area_ha,
             crop_acsubst_area_lau.ha = crop_acsubst_totarea_lau.ha * crop_acsubst_area_frac_lau,
             crop_acsubst_mass_lau.g = crop_acsubst_area_lau.ha * aprate_farm_g.ha,
             crop_acsubst_mass_frac_lau = crop_acsubst_mass_lau.g / acsubst_mass.g) |>
      terra::merge(chemprop,
                by.x = c("acsubst"),
                by.y = c("Active"))
    
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
      mutate(conc_acsubst_total_soil_56twa_ug.kg = (conc_acsubst_total_soil_ini_ug.kg / (56 * (log(2) / DT50_typical_d))) * (1 - exp(-56 * (log(2) / DT50_typical_d))),
             conc_acsubst_total_soil_365twa_ug.kg = (conc_acsubst_total_soil_ini_ug.kg / (365 * (log(2) / DT50_typical_d))) * (1 - exp(-365 * (log(2) / DT50_typical_d)))) |> 
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
      mutate(rq_acsubst_total_soil_twa = conc_acsubst_total_soil_56twa_ug.kg/(NOEC_earthworm_chron_repr_mg.kg*1000)) |> 
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
                                        prod))
    
    # Combine AS concentration in soil results on individual farms
    # Calculate RQ for individual farms and individual AS
    
    soil_farm_mapinput <- load_acsubst_farm |> 
      select(District,
             ZKOD,
             CTVEREC,
             Crop,
             acsubst,
             DT50_typical_d,
             aprate_farm_kg.ha,
             oc_perc,
             bulk_dens_kg.dm3,
             clay_perc,
             sand_perc,
             slope_perc,
             conc_acsubst_total_soil_56twa_ug.kg,
             conc_acsubst_total_soil_ini_ug.kg,
             srunoff_as_load,
             rq_acsubst_total_soil_twa) |> 
      mask(lau_name[name])
    
    # Calculate cumulative RQ in soil for individual fields
    # Create a character type column showing a list of ASs and RQs for each field
    soil_RQ_nest <- soil_farm_mapinput |>  
      # filter(acsubst %in% acsubst_soil) |> 
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
      select("District", "ZKOD", "CTVEREC", "Crop", "acsubst", "rq_acsubst_total_soil_twa") |> 
      group_by(District, ZKOD, CTVEREC, Crop) |> 
      summarise(sum_rq_acsubst_total_soil_twa = sum(rq_acsubst_total_soil_twa),
                .groups = "keep") |> 
      makeValid()
    
    # Merge nested RQs and summed RQs  datasets by unique field IDs
    soil_cumRQ_all <- terra::merge(soil_cumRQ, soil_RQ_nest, by = c("Crop", "ZKOD", "CTVEREC")) |> 
      mutate(AS_rq = str_remove_all(AS_rq, "\""),
             AS_rq = str_remove_all(AS_rq, "list"),
             AS_rq = str_remove_all(AS_rq, "AS_rq"),
             AS_rq = str_remove(AS_rq, "c"),
             AS_rq = str_remove_all(AS_rq, "="),
             AS_rq = str_remove_all(AS_rq, "\\("),
             AS_rq = str_remove_all(AS_rq, "\\)"),
             AS_rq = str_replace_all(AS_rq, "0.01$", "<0.01")) |>
      mask(lau_name[name])

    # Intersect farm loadings with river buffer segments
    # Calculate RQ for individual fields intersected with buffer around river segments
    river_seg_mapinput <- soil_farm_mapinput[c("srunoff_as_load",
                                               "ZKOD",
                                               "CTVEREC",
                                               "Crop",      
                                               "acsubst",
                                               "aprate_farm_kg.ha",
                                               "District")] |>
      filter(acsubst %in% acsubst_water) |>
      terra::intersect(rivers_lau_buff_seg[c("HYDROID", "NAMN1", "SHAPE_Leng", "dis_m3_pyr")]) |>
      terra::merge(chemprop[c("Active", "NOEC_fish_21_mg.L")], by.x = "acsubst", by.y = "Active") |>
      group_by(HYDROID, NAMN1, SHAPE_Leng, Crop) |>
      mutate(conc_mean_river_seg = srunoff_as_load/dis_m3_pyr,
             rq_mean_river_seg_twa = conc_mean_river_seg/(NOEC_fish_21_mg.L*1000)) |> 
      mask(gemup_lau)
    
    farm_area_buff <- terra::merge(rivers_lau_buff_seg,
                                   cbind(river_seg_mapinput[c("HYDROID", "NAMN1", "SHAPE_Leng")] |> 
                                           terra::aggregate(c("HYDROID", "NAMN1", "SHAPE_Leng")),
                                         river_seg_mapinput[c("HYDROID", "NAMN1", "SHAPE_Leng")] |> 
                                           terra::aggregate(c("HYDROID", "NAMN1", "SHAPE_Leng")) |> 
                                           expanse(unit = "m")) |> values(),
                                   by = c("HYDROID", "NAMN1", "SHAPE_Leng")) |>
      rename(farm_buff_area.m = y,
             nr_farms_buff = agg_n)

    buff_area <- cbind(rivers_lau_buff_seg[c("HYDROID", "NAMN1", "SHAPE_Leng", "dis_m3_pyr")] |> 
                         aggregate(c("HYDROID", "NAMN1", "SHAPE_Leng")),
                       rivers_lau_buff_seg[c("HYDROID", "NAMN1", "SHAPE_Leng", "dis_m3_pyr")] |> 
                         aggregate(c("HYDROID", "NAMN1", "SHAPE_Leng")) |> 
                         expanse(unit = "m")) |> 
      rename(buffer_area.m = y) |> 
      select(-agg_n) |> 
      mask(gemup_lau)
    
    river_weight <- terra::merge(farm_area_buff,
                                 buff_area |>
                                   values(),
                                 by = c("HYDROID", "NAMN1", "SHAPE_Leng")) |> 
      mutate(river_w = (SHAPE_Leng*WD7/buffer_area.m)*(farm_buff_area.m/buffer_area.m))
    
    # Calculate cumulative RQ for fields in river buffer for individual river segments
    # Create a character type column showing a list of ASs and RQs for each river segment 
    river_RQ_nest <- terra::merge(river_seg_mapinput,
                                  river_weight[c("HYDROID", "NAMN1", "SHAPE_Leng", "river_w")] |> 
                                    values(),
                                  by =  c("HYDROID", "NAMN1", "SHAPE_Leng")) |>
      values() |> 
      group_by(HYDROID, NAMN1, SHAPE_Leng) |> 
      arrange(desc(rq_mean_river_seg_twa), .by_group = T) |>
      distinct(acsubst, .keep_all = T) |>
      mutate(nr_fields = n(),
             rq_mean_river_seg_twa = round(rq_mean_river_seg_twa*river_w, 6),
             rq_mean_river_seg_twa = case_when(rq_mean_river_seg_twa < 0.000001 ~ 0.000001,
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
      group_by(District, HYDROID, NAMN1, SHAPE_Leng) |> 
      summarise(sum_rq_mean_river_seg_twa = sum(round(rq_mean_river_seg_twa*river_w, 10)), .groups = "keep") |> 
      ungroup() |>
      select(-District) |>
      makeValid()
   
    # Merge nested RQs and summed RQs  datasets by unique river segments and drop geometry
    river_cumRQ_df <- terra::merge(river_cumRQ, river_RQ_nest,
                                   by = c("HYDROID", "NAMN1", "SHAPE_Leng")) |> 
      mutate(AS_rq = str_remove_all(AS_rq, "\""),
             AS_rq = str_remove_all(AS_rq, "list"),
             AS_rq = str_remove_all(AS_rq, "AS_rq"),
             AS_rq = str_remove(AS_rq, "c"),
             AS_rq = str_remove_all(AS_rq, "="),
             AS_rq = str_remove_all(AS_rq, "\\("),
             AS_rq = str_remove_all(AS_rq, "\\)"),
             AS_rq = str_replace_all(AS_rq, "0.000001", "<0.000001")) |> 
      # mask(lau_name[name]) |> 
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
    
    actual_soil_acsubst_name <- soil_farm_mapinput$acsubst |> unique()
    actual_river_acsubst_name <- river_seg_mapinput$acsubst |> unique()
    
    pb <- progress_bar$new(
      format = "[:bar] :percent | :map_type | Substance: :substance | ETA: :eta",
      total = (length(actual_soil_acsubst_name) + length(actual_river_acsubst_name) * 4) + 2,
      clear = FALSE,
      width = 100
    )
    

  # Soil PEC and RQ individual maps 
    
    for (as in seq_along(as_soil <- actual_soil_acsubst_name)) {

      # Store current substance name for progress updates
      current_substance <- as_soil[as]

      # Prepare data
      # Prepare soil data
      soil_data <- soil_farm_mapinput |>
        filter(acsubst %in% as_soil[as])

      # Concentration maps palettes
      # Soil concentration palette

      soil_conc_pal <- colorBin(
        palette = "BuPu",
        domain = soil_data$conc_acsubst_total_soil_56twa_ug.kg,
        bins = pretty(soil_data$conc_acsubst_total_soil_56twa_ug.kg),
        na.color = "#ffdeaf"
      )

      soil_conc_pal_rev <- colorBin(
        palette = "BuPu",
        domain = soil_data$conc_acsubst_total_soil_56twa_ug.kg,
        bins = pretty(soil_data$conc_acsubst_total_soil_56twa_ug.kg),
        na.color = "#ffdeaf",
        reverse = T,
      )

      # RQ maps palettes
      # Soil RQ palette
      soil_rq_colors <- brewer.pal(11, "PRGn")[c(1,2,3,4,5,8,9,10)]

      soil_rq_pal <- colorBin(
        palette = soil_rq_colors |> rev(),
        domain = soil_data$rq_acsubst_total_soil_twa,
        bins = c(0, 0.2, 1, 2, 3, 5, 500),
        na.color = "#ffdeaf"
      )

      # Map titles
      # Concentration maps titles
      # Soil concentration map title
      soil_conc_title <- paste0(
        as_soil[as], " ",
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
        as_soil[as], " ",
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
          fillColor = ~soil_conc_pal(conc_acsubst_total_soil_56twa_ug.kg),
          fillOpacity = 0.85,
          color = "black",
          weight = 0.25,
          opacity = 1,
          popup = ~paste0("<b>Field ID: </b>", "<b>", ZKOD, " (ZKOD)", ", ", CTVEREC, " (CTVEREC)", "</b>", "<br>",
                          "<b>Crop: </b>", "<b>", Crop, "</b>", "<br>",
                          "<b>Individual PEC soil (µg × kg⁻¹): </b>", "<b>",
                          ifelse(is.na(conc_acsubst_total_soil_56twa_ug.kg),
                                 "Missing values",
                                 format_power10(conc_acsubst_total_soil_56twa_ug.kg,  digits = 2)), "</b>"),
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
          values = soil_data$conc_acsubst_total_soil_56twa_ug.kg,
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
                                                    as_soil[as],
                                                    "_PEC_Soil_CZ.html")), selfcontained = T)

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
                                                        as_soil[as],
                                                        "_RQ_Soil_CZ.html")), selfcontained = T)
    }
    
  # Water PEC and RQ individual maps 
    
    for (as in seq_along(as_river <- actual_river_acsubst_name)) {
      
      # Store current substance name for progress updates
      current_substance <- as_river[as]
      
      # Prepare data
      # Prepare river water data

      river_data <- terra::merge(rivers_lau_buff_seg[c("HYDROID",
                                                       "NAMN1",
                                                       "SHAPE_Leng")],
                                 terra::merge(river_seg_mapinput[c("HYDROID",
                                                                   "NAMN1",
                                                                   "SHAPE_Leng",
                                                                   "rq_mean_river_seg_twa",
                                                                   "conc_mean_river_seg",
                                                                   "acsubst")] |> 
                                                filter(acsubst %in% as_river[as]),
                                              river_weight[c("HYDROID", "NAMN1", "SHAPE_Leng", "river_w")] |> 
                                                values(),
                                              by =  c("HYDROID", "NAMN1", "SHAPE_Leng")) |>
                                   group_by(HYDROID, NAMN1, SHAPE_Leng) |> 
                                   arrange(desc(rq_mean_river_seg_twa), .by_group = T) |>
                                   mutate(nr_fields = n(),
                                          rq_mean_river_seg_twa = round(rq_mean_river_seg_twa*river_w, 10),
                                          conc_mean_river_seg = round(median(conc_mean_river_seg*river_w), 10)) |>
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
        bins = c(0, 0.2, 1, 2, 3, 5, 500),
        na.color = "#ffdeaf"
      )
      
      # Map titles
      # Concentration maps titles
      # River water concentration map title
      river_conc_title <- paste0(
        as_river[as], " ",
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
        as_river[as], " ",
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
                          "<b>River segment length (km): </b>", "<b>", round(SHAPE_Leng/1000, 2), "</b><br>",
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
                                                     as_river[as],
                                                     "_PEC_Water_CZ.html")), selfcontained = T)
      
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
                          "<b>River segment length (km): </b>", "<b>", round(SHAPE_Leng/1000, 2), "</b><br>",
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
                                                         as_river[as],
                                                         "_RQ_Water_CZ.html")), selfcontained = T)
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
      bins = c(0, 0.2, 1, 2, 3, 5, 10000),
      na.color = "#ffdeaf"
    )
    
    river_RQcum_pal <- colorBin(
      palette = river_RQcum_colors |> rev(),
      domain = river_cumRQ_all$sum_rq_mean_river_seg_twa,
      bins = c(0, 0.2, 1, 2, 3, 5, 10000),
      na.color = "#ffdeaf"
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
    
    if (!pb$finished) pb$tick(tokens = list(map_type = "Creating cumulative Soil RQ map"))
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
                        "<b>Number of pesticides used: </b>","<b>", nr_as, "</b>", "<br>",
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
    
    saveWidget(soil_cum_rq_dist,
               file = paste0(path_home_r(),
                             "/Soil/RQ/",
                             lau_name[name]$LAU_NAME,
                             "_RQcum_Soil_CZ.html"),
               selfcontained = T)
    
    write_excel_csv(soil_farm_mapinput |>
                      values()  |> 
                      select(-c(District,
                                rq_acsubst_total_soil_twa)) |> 
                      cbind(endday |>
                              as.data.frame()) |> 
                      cbind(lau_name[name] |> 
                              as.data.frame()) |> 
                      rename(simtime_day = endday),
                    paste0(path_home_r(),
                      "/Soil/PEC/",
                    lau_name[name]$LAU_NAME,
                    "_PEC_Soil_CZ.csv"))
    
    write_excel_csv(soil_farm_mapinput |>
                      values()  |> 
                      select(-c(srunoff_as_load,
                                District,
                                conc_acsubst_total_soil_56twa_ug.kg,
                                conc_acsubst_total_soil_ini_ug.kg)) |> 
                      cbind(endday |>
                              as.data.frame()) |> 
                      cbind(lau_name[name] |> 
                              as.data.frame()) |> 
                      rename(simtime_day = endday),
                    paste0(path_home_r(),
                           "/Soil/RQ/",
                           lau_name[name]$LAU_NAME,
                           "_RQ_Soil_CZ.csv"))
    
    write_excel_csv(soil_cumRQ |>
                      values() |> 
                      select(-District) |> 
                      cbind(endday |>
                              as.data.frame()) |> 
                      cbind(lau_name[name] |> 
                              as.data.frame()) |> 
                      rename(simtime_day = endday),
                    paste0(path_home_r(),
                           "/Soil/RQ/",
                           lau_name[name]$LAU_NAME,
                           "_",
                           "RQcum_Soil_CZ.csv")) 
    
    if (!pb$finished) pb$tick(tokens = list(map_type = "Creating cumulative Water RQ map"))
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
                        "<b>River segment length (km): </b>", "<b>", round(SHAPE_Leng/1000, 2), "</b><br>",
                        "<b># of fields in </b>", "<b>", rivbuff_width, " meter</b>", "<b>", " buffer: ", "</b>",  "<b>", nr_fields, "</b><br>",
                        "<b>Cumulative RQ surface water: </b>", "<b>",
                        ifelse(is.na(sum_rq_mean_river_seg_twa), 
                               "Missing values",
                               format_power10(sum_rq_mean_river_seg_twa, digits = 3)), "</b><br>",
                        "<b>Top contributors (RQ \u2265 0.000001): </b>" ,"<br>", AS_rq),
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
                                                "_RQcum_Water_CZ.html"), selfcontained = T)  

    write_excel_csv(terra::merge(river_seg_mapinput,
                                 river_weight[c("HYDROID",
                                                "NAMN1",
                                                "SHAPE_Leng",
                                                "WD7",
                                                "mean_dis_m3_pyr",
                                                "buffer_area.m",
                                                "river_w",
                                                "pre_mm_s07",
                                                "nr_farms_buff")] |> 
                                   values(),
                                 by =  c("HYDROID", "NAMN1", "SHAPE_Leng")) |>
                      values() |> 
                      mutate(conc_mean_river_seg  = median(conc_mean_river_seg  * river_w)) |> 
                      select(-c(rq_mean_river_seg_twa,
                                NOEC_fish_21_mg.L,
                                District,
                                dis_m3_pyr)) |>
                      cbind(endday |>
                              as.data.frame()) |>
                      cbind(lau_name[name] |>
                              as.data.frame()) |>
                      rename(conc_river_ug.dm3 = conc_mean_river_seg,
                             srunoff_as_load_kg = srunoff_as_load,
                             simtime_day = endday),
                    paste0(path_home_r(),
                           "/Water/PEC/",
                           lau_name[name]$LAU_NAME,
                           "_PEC_Water_CZ.csv"))
    
    write_excel_csv(terra::merge(river_seg_mapinput,
                                 river_weight[c("HYDROID",
                                                "NAMN1",
                                                "SHAPE_Leng",
                                                "WD7",
                                                "mean_dis_m3_pyr",
                                                "buffer_area.m",
                                                "river_w",
                                                "pre_mm_s07",
                                                "nr_farms_buff")] |> 
                                   values(),
                                 by =  c("HYDROID", "NAMN1", "SHAPE_Leng")) |>
                      values() |> 
                      mutate(rq_river_seg_twa  = rq_mean_river_seg_twa  * river_w) |> 
                      select(-c(conc_mean_river_seg,
                                rq_mean_river_seg_twa,
                                dis_m3_pyr,
                                District)) |>
                      cbind(endday |>
                              as.data.frame()) |>
                      cbind(lau_name[name] |>
                              as.data.frame()) |>
                      rename(simtime_day = endday,
                             srunoff_as_load_kg = srunoff_as_load),
                    paste0(path_home_r(),
                           "/Water/RQ/",
                           lau_name[name]$LAU_NAME,
                           "_RQ_Water_CZ.csv"))
    
    write_excel_csv(river_cumRQ |>
                      values() |> 
                      cbind(endday |>
                              as.data.frame()) |> 
                      cbind(lau_name[name] |> 
                              as.data.frame()) |> 
                      rename(simtime_day = endday),
                    paste0(path_home_r(),
                           "/Water/RQ/",
                           lau_name[name]$LAU_NAME,
                           "_RQcum_Water_CZ.csv"))
    }
  }
  #################################################################
  ########## END: Pesticide concentration and risk maps ###########
  #################################################################
}

# Main function call #
# Use it for each new scenario given by a new set of parameters 
# It might take up to several minutes to process all input files, run the model and to generate all files for a single LAU.
map_topsoil_riverwater_cz(lau_name = lau_name,
                         acsubst_name = acsubst_name,
                         app_month = app_month,
                         endday = endday,
                         rivbuff_width = rivbuff_width)
