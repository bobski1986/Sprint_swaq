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
lau_degurba_nl <- dir_ls(path_home_r(), recurse = T, regexp = "/LAU_RG_01M_2024_4326.gpkg") |>
  vect() |> 
  filter(CNTR_CODE == "NL")

# River basin, Hydrosheds #
# Polygons and attributes from the Hydrosheds dataset
basins_nl <- dir_ls(path_home_r(), recurse = T, regexp = "hydrosheds_lvl12_basins_nl.gpkg") |>
  vect() |> 
  mutate(HYBAS_ID = as.character(HYBAS_ID)) |> 
  mask(lau_degurba_nl)

# River network #
river_net <- dir_ls(path_home_r(), recurse = T, regexp = "water_network_12_NL.gpkg") |>
  vect()

## Selection of dutch river class: "3_6", "0.5_3_L20", "0.5_3_L60", "0.5_3_L150", "0.5_3_Lmax"
river_class_name <- "0.5_3_L60"

# Terrain slope, FAO #
terrain_slope <- dir_ls(path_home_r(),
                        recurse = T,
                        regexp = "/TerrainSlope_30as_nl.nc$") |>
  rast()

# Bulk density, ESDAC #
bulk_dens <- dir_ls(path_home_r(),
                    recurse = T,
                    regexp = "budens_jrc_nl.nc$") |> 
  rast()

# Organic carbon Hydroshed, to be substituted with ESDAC octop #
organic_carbon <- basins_nl |> 
  select("soc_th_uav") |> 
  rasterize(bulk_dens,
            field = "soc_th_uav",
            touches = T) |> 
  rename(OC_jrc_NL = soc_th_uav)

# Sand content, ESDAC # 
# sand_jrc_path <- dir_ls(path_home_r(),
#                         recurse = T,
#                         regexp = "Sand1.tif$")
# sand_jrc_laea <- rast(sand_jrc_path)
# sand_jrc_nl_wgs84 <- sand_jrc_laea |>  project(basins_nl) |> crop(basins_nl)
# writeCDF(sand_jrc_nl_wgs84, filename = paste0(path_home_r(), "/sand_jrc_NL.nc"), overwrite=TRUE)

sand <- dir_ls(path_home_r(),
               recurse = T,
               regexp = "sand_jrc_NL.nc$") |> 
  rast()

# Clay content, ESDAC # 
# clay_jrc_path <- dir_ls(path_home_r(),
#                         recurse = T,
#                         regexp = "Clay.tif$")
# clay_jrc_laea <- rast(clay_jrc_path)
# clay_jrc_nl_wgs84 <- clay_jrc_laea |> project(basins_nl) |> crop(basins_nl)
# writeCDF(clay_jrc_nl_wgs84, filename = paste0(path_home_r(), "/clay_jrc_NL.nc"), overwrite = T)

clay <- dir_ls(path_home_r(),
               recurse = T,
               regexp = "clay_jrc_NL.nc$") |> 
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
acsubst_name <- c("aclonifen",
                  "azoxystrobin",
                  "benzovindiflupyr",
                  "boscalid",
                  "cyazofamid",
                  "cyproconazole",
                  "deltamethrin",
                  "difenoconazole",
                  "epoxiconazole",
                  "esfenvalerate",
                  "ethofumesate",
                  "fenpropidin",
                  "fluazinam",
                  "fludioxonil",
                  "fluopyram",
                  "glyphosate",
                  "isoxaben",
                  "mandipropamid",
                  "metamitron",
                  "methoxyfenozide",
                  "pendimethalin",
                  "prochloraz",
                  "propyzamide",
                  "prosulfocarb",
                  "terbuthylazine",
                  "tebuconazole",
                  "fluopicolide",
                  "penflufen",
                  "bixafen",
                  "tembotrione",
                  "metobromuron",
                  "flutolanil",
                  "fluoxastrobin",
                  "dimethomorph",
                  "chlorantraniliprole") |> 
  unique() |> 
  str_to_sentence()

# Check if all substances are in the PPDB script
# acsubst_name[which(!acsubst_name %in% ASs)]

# Spatial extent #
lau_name <- lau_degurba_nl[2 , "LAU_NAME"]

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
    
    acsubst_water <- c("aclonifen",
                       "benzovindiflupyr",
                       "epoxiconazole",
                       "ethofumesate",
                       "fluazinam",
                       "fluopyram",
                       "mandipropamid",
                       "pendimethalin",
                       "terbuthylazine",
                       "tebuconazole",
                       "fluopicolide",
                       "penflufen",
                       "bixafen",
                       "tembotrione",
                       "metobromuron",
                       "flutolanil",
                       'fluoxastrobin',
                       "dimethomorph",
                       "chlorantraniliprole") |> unique() |> str_to_sentence()
    
    acsubst_soil <- c("azoxystrobin",
                      "boscalid",
                      "cyazofamid",
                      "cyproconazole",
                      "deltamethrin",
                      "difenoconazole",
                      "esfenvalerate",
                      "fenpropidin",
                      "fludioxonil",
                      "glyphosate",
                      "isoxaben",
                      "metamitron",
                      "methoxyfenozide",
                      "prochloraz",
                      "propyzamide",
                      "prosulfocarb") |> unique() |> str_to_sentence()
    
    cat("\r", "Gemup for", lau_name[name]$LAU_NAME, "is being processed.", lau_name |> length() - name, "LAUs remaining.")
    
    gemup_lau <- dir_ls(path_home_r(), recurse = T, regexp = "gemap_slim_nl") |> 
      vect(extent = ext(lau_name[1])) |> 
      mask(lau_name[1]) |>
      filter(EU_name %in% acsubst_name) |> 
      rename(infactor_av = inter_frac_bbch.ave,
             infactor_min = inter_frac_bbch.min,
             infactor_max = inter_frac_bbch.max,
             acsubst = EU_name,
             field_area = area,
             aprate_farm_g.ha = Dose_kg_ha,
             Crop = EC_hcat_n) |> 
      mutate(field_area = field_area * 0.0001) |> 
      makeValid() 
    
    acsubst_gemup <- gemup_lau |> select(acsubst) |> values() |> unique() |> as.vector()
    acsubst_water <- acsubst_water[which(acsubst_water %in% acsubst_gemup$acsubst)]
    acsubst_soil <- acsubst_soil[which(acsubst_soil %in% acsubst_gemup$acsubst)]
    
    # Read basin polygons from Hydrosheds for the selected LAU
    basins_lau_nl <- basins_nl |> 
      select("HYBAS_ID", "dis_m3_pyr", "dis_m3_pmn", "dis_m3_pmx", "riv_tc_ssu", "riv_tc_usu", starts_with("pre_mm_")) |>
      mask(lau_name[1]) |> 
      simplifyGeom(0.005)
    
    # Precipitation data from HYDROSHEDS database #
    
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
    if (!prec_col_name %in% names(basins_lau_nl)) {
      stop(sprintf("Precipitation column '%s' not found in dutch basin atlas. Available columns: %s",
                   prec_col_name,
                   paste(names(basins_lau_nl)[grepl("^pre_mm_s", names(basins_lau_nl))], collapse = ", ")))
    }
    
    ## Extract precipitation data for the specified month
    prec_lau <- basins_lau_nl[prec_col_name]
    
    # Terrain slope #
    slope_nl_30as <- terrain_slope |> 
      crop(lau_name[1])
    
    # HYDROSHEDS (to be changed to ESDAC octop) Organic carbon content in topsoil #
    organic_carbon <- (organic_carbon  / (bulk_dens * 10000 * 0.05)) * 100
    organic_carbon <- organic_carbon |> rename(oc_perc = OC_jrc_NL)
    
    orcarb_nl <- organic_carbon |>
      select("oc_perc") |>
      crop(lau_name[1])
    
    # ESDAC topsoil physical properties for Europe (based on LUCAS topsoil data) #
    sand_jrc_nl <- sand |>
      mask(lau_name[1], touches = T)

    clay_jrc_nl <- clay |>
      mask(lau_name[1], touches = T)
    
    budens_jrc_nl <- bulk_dens |>
      crop(lau_name[1], touches = T)
    
    # Crop specific spray drift values to the "gemup"
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
    
    # Chemical input data from qsars (vega, epi) and PPDB where available #
    # In case of error e.g., "Error in ppdb_df_values ! Can't extract rows past the end",
    # first check if the names of properties are the same in the script and PPDB, 
    # PPDB gets updated every now and then, so must be the scraping script.
    source(dir_ls(path_home_r(), recurse = T, regexp = "ppdb scraping"))
    chemprop <- chemprop_gen(Active = acsubst_gemup$acsubst) |> 
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
    
    river_classes <- list(
      "3_6" = list(
        width = "3 - 6 meter",
        length_min = NULL,
        length_max = NULL
      ),
      "0.5_3_L20" = list(
        width = "0,5 - 3 meter",
        length_min = NULL,
        length_max = 20
      ),
      "0.5_3_L60" = list(
        width = "0,5 - 3 meter",
        length_min = 20,
        length_max = 60
      ),
      "0.5_3_L150" = list(
        width = "0,5 - 3 meter",
        length_min = 60,
        length_max = 150
      ),
      "0.5_3_Lmax" = list(
        width = "0,5 - 3 meter",
        length_min = 150,
        length_max = Inf
      )
    )
    
    # Function to filter rivers by classification
    filter_rivers <- function(rivers_data, class_def) {
      filtered <- rivers_data |> filter(breedteklasse == class_def$width)
      
      if (!is.null(class_def$length_min)) {
        filtered <- filtered |> filter(length_m > class_def$length_min)
      }
      if (!is.null(class_def$length_max) && is.finite(class_def$length_max)) {
        filtered <- filtered |> filter(length_m <= class_def$length_max)
      }
      
      return(filtered)
    }
    
    # Intersect filtered rivers with selected LAU
    
    rivers_lau <- terra::intersect(river_net, lau_name[1]) |> 
      terra::merge(basins_lau_nl[c("HYBAS_ID", "dis_m3_pyr")] |>
                     values(), by = "HYBAS_ID")
    
    # basins_lau_nl |> values(), by = "HYBAS_ID"
    # Filter rivers present in the LAU based on selected river class
    rivers_class_lau <- filter_rivers(rivers_lau, river_classes[[river_class_name]]) 
    
    # Indicate buffer width around a river segment
    rivers_class_lau_buff <- rivers_class_lau |> terra::buffer(rivbuff_width)
    
    # Intersect gemup with selected river segments so the gemup is expanded to include hydrography of the selected river basin(s)
    gemup_lau_rivers_class_buff <- gemup_lau[rivers_class_lau_buff]
    
    if(rivers_net_buff_farm_lau |> nrow() == 0) {
      
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
      
      orcarb_field <- terra::zonal(orcarb_nl,
                                   gemup_lau,
                                   fun = "mean",
                                   # weights = T,
                                   # exact = T,
                                   as.polygons = T) |>
        rename("orcarb_perc" = "oc_perc") |>
        mutate(orcarb_perc = orcarb_perc/100)
      
      # srunoff_river_seg <- terra::zonal(srunoff_basin,
      #                                   gemup_loc_rivers_seg,
      #                                   fun = "mean",
      #                                   # weights = T,
      #                                   # exact = T,
      #                                   as.polygons = T) |>
      #   rename("SR" = "srunoff_CZ")
      
      slope_field <- terra::zonal(slope_nl_30as,
                                  gemup_lau,
                                  fun = "mean",
                                  # weights = T,
                                  # exact = T,
                                  as.polygons = TRUE) |> 
        rename("slope_perc" = "TerrainSlope_30as_nl")
      
      sand_field <- terra::zonal(sand_jrc_nl,
                                 gemup_lau,
                                 fun = "mean",
                                 # weights = T,
                                 # exact = T,
                                 as.polygons = TRUE) |>
        rename("sand_perc" = "sand_jrc_NL")
      

      clay_field <- terra::zonal(clay_jrc_nl,
                                 gemup_lau,
                                 fun = "mean",
                                 # weights = T,
                                 # exact = T,
                                 as.polygons = TRUE) |>
        rename("clay_perc" = "clay_jrc_NL")
      
      budens_field <- terra::zonal(budens_jrc_nl,
                                   gemup_lau,
                                   fun = "mean",
                                   # weights = T,
                                   # exact = T,
                                   as.polygons = TRUE) |>
        rename("bulk_dens" = "budens_jrc_NL")
    
      # Patch size of arable land in a river basins and district
      crop_area_tot_lau <- gemup_lau |>
        values() |>
        group_by(LAU_NAME, Crop, acsubst) |>
        summarise(crop_acsubst_totarea_lau.ha = sum(field_area)) |>
        ungroup()
      
      # District and crop specific application rate per lau
      acsubst_application_lau <- gemup_lau |>
        terra::merge(crop_area_tot_lau, by.x = c("Crop", "acsubst", 'LAU_NAME'), by.y = c("Crop", "acsubst", "LAU_NAME")) |>
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
        mutate(conc_acsubst_total_soil_twa_ug.kg = (conc_acsubst_total_soil_ini_ug.kg / (endday * (log(2) / DT50_typical_d))) * (1 - exp(-56 * (log(2) / DT50_typical_d)))) |> 
        ## Effect of terrain slope
        mutate(slope_effect = map_dbl(slope_perc,
                                      ~if_else(. <= 20,
                                               0.001423 * .^2 + 0.02153 * .,
                                               1))) |>
        ##Soil chronic RQ
        mutate(rq_acsubst_total_soil_twa = conc_acsubst_total_soil_twa_ug.kg/NOEC_earthworm_chron_repr_mg.kg) |> 
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
        ## AS spray drift loading
        mutate(sdrift_as_load = map2_dbl(aprate_farm_g.ha,
                                      sdrift_90lnorm,
                                      ~.x * (.y/100))) |>
        ## Application rate correction
        mutate(aprate_farm_kg.ha = map2_dbl(aprate_farm_g.ha,
                                            sdrift_load,
                                            ~.x-.y)/1000) |> 
        ## AS surface runoff loading
        mutate(srunoff_as_load = pmap_dbl(list(field_area,
                                               aprate_farm_kg.ha,
                                               infactor_effect,
                                               frac_asubst_soil_water_lag,
                                               slope_effect,
                                               srunoff_tot_frac),
                                          prod))
      
      # Combine AS concentration in soil results on individual farms
      # Calculate RQ for individual farms and individual AS
      soil_farm_mapinput <- load_acsubst_farm |>
        select(LAU_NAME,
               objectid,
               Crop,
               acsubst,
               conc_acsubst_total_soil_twa_ug.kg,
               rq_acsubst_total_soil_twa,
               srunoff_as_load,
               sdrift_load) |> 
        mutate(Crop = str_replace_all(Crop, "_", " "),
              Crop = str_to_sentence(Crop)) |> 
        # mutate(rq_acsubst_total_soil_twa = rq_acsubst_total_soil_twa) |>
        mask(lau_name[1])
      
      # Calculate cumulative RQ in soil for individual fields
      # Create a character type column showing a list of ASs and RQs for each field
      soil_RQ_nest <- soil_farm_mapinput |>  
        # filter(acsubst %in% acsubst_soil) |> 
        values() |> 
        group_by(objectid, Crop) |> 
        distinct(objectid, acsubst, .keep_all = T) |>
        arrange(desc(rq_acsubst_total_soil_twa), .by_group = T) |>
        mutate(nr_as = n(),
               rq_acsubst_total_soil_twa =  round(rq_acsubst_total_soil_twa, 3),
               rq_acsubst_total_soil_twa = case_when(rq_acsubst_total_soil_twa < 0.01 ~ 0.01,
                                                     .default = rq_acsubst_total_soil_twa)) |> 
        # filter(rq_acsubst_total_soil_twa >= 0.01) |>
        unite("AS_rq", acsubst, rq_acsubst_total_soil_twa, sep = " | ", remove = FALSE) |>
        select(objectid, Crop, nr_as, AS_rq) |>
        nest("AS_rq" = AS_rq) |> 
        ungroup()
      
      # Calculate summed RQs for individual fields
      soil_cumRQ <- soil_farm_mapinput |> 
        select("LAU_NAME", "objectid", "Crop", "acsubst", "rq_acsubst_total_soil_twa") |> 
        group_by(objectid, Crop) |> 
        summarise(sum_rq_acsubst_total_soil_twa = sum(rq_acsubst_total_soil_twa),
                  .groups = "keep") |> 
        makeValid()
      
      # Merge nested RQs and summed RQs  datasets by unique field IDs
      soil_cumRQ_all <- terra::merge(soil_cumRQ, soil_RQ_nest, by = c("Crop", "objectid")) |> 
        mutate(AS_rq = str_remove_all(AS_rq, "\""),
               AS_rq = str_remove_all(AS_rq, "list"),
               AS_rq = str_remove_all(AS_rq, "AS_rq"),
               AS_rq = str_remove(AS_rq, "c"),
               AS_rq = str_remove_all(AS_rq, "="),
               AS_rq = str_remove_all(AS_rq, "\\("),
               AS_rq = str_remove_all(AS_rq, "\\)"),
               AS_rq = str_replace_all(AS_rq, "0.01$", "<0.01"),
               Crop = str_replace_all(Crop, "_", " "),
               Crop = str_to_sentence(Crop)) |>
        mask(lau_name[1])
      
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
        total = (length(acsubst_name) * 4) + 2,
        clear = FALSE,
        width = 100
      )
      
      actual_soil_acsubst_name <- soil_farm_mapinput$acsubst[1] |> unique()
      
      # Soil PEC and RQ individual maps 
      for (as in seq_along(acsubst_name <- actual_soil_acsubst_name)) {
        
        # Store current substance name for progress updates
        current_substance <- acsubst_name[1]
        
        # Prepare data
        # Prepare soil data
        soil_data <- soil_farm_mapinput |> 
          filter(acsubst %in% acsubst_name[1])
        
        # Concentration maps palettes
        # Soil concentration palette
        
        soil_conc_pal <- colorBin(
          palette = "BuPu",
          domain = soil_data$conc_acsubst_total_soil_twa_ug.kg,
          bins = pretty(soil_data$conc_acsubst_total_soil_twa_ug.kg),
          na.color = "#ffdeaf"
        )
        
        soil_conc_pal_rev <- colorBin(
          palette = "BuPu",
          domain = soil_data$conc_acsubst_total_soil_twa_ug.kg,
          bins = pretty(soil_data$conc_acsubst_total_soil_twa_ug.kg),
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
          acsubst_name[1], " ",
          endday |> unique(),
          "-day PEC soil after 1x application in ",
          app_month,
          " ",
          "(",
          sim_yr,
          ")",
          " in ",
          lau_name[1]$LAU_NAME
        )
        
        # RQ maps titles
        # Soil RQ map title
        soil_rq_title <- paste0(
          acsubst_name[1], " ",
          endday |> unique(),
          "-day RQ<sub>earthworm</sub> soil after 1x application in ",
          app_month,
          " ",
          "(",
          sim_yr,
          ")",
          " in ",
          lau_name[1]$LAU_NAME
        )
        
        # AS concentration in topsoil
        if (!pb$finished) pb$tick(tokens = list(map_type = "Creating Soil PEC map", substance = current_substance))
        
        soil_as_conc <- leaflet() |>
          addTiles(options = tileOptions(opacity = 0.5)) |> 
          
          # Add district borders
          addPolylines(
            data = lau_name[1],
            color = "black",
            weight = 0.75,
            opacity = 1,
            fillOpacity = 0,
            group = "Administrative borders"
          ) |>
          
          # Add soil concentration polygons
          addPolygons(
            data = soil_data,
            fillColor = ~soil_conc_pal(conc_acsubst_total_soil_twa_ug.kg),
            fillOpacity = 0.85,
            color = "black",
            weight = 0.25,
            opacity = 1,
            popup = ~paste0("<b>Field ID: </b>", "<b>", objectid, "</b>", "<br>",
                            "<b>Crop: </b>", "<b>", Crop, "</b>", "<br>",
                            "<b>Individual PEC soil (µg × kg⁻¹): </b>", "<b>",
                            ifelse(is.na(conc_acsubst_total_soil_twa_ug.kg), 
                                   "Missing values",
                                   format_power10(conc_acsubst_total_soil_twa_ug.kg,  digits = 2)), "</b>"),
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
            values = soil_data$conc_acsubst_total_soil_twa_ug.kg,
            title = "Individual PEC soil<br>(time-weighted) (µg × kg⁻¹)",
            group = "Individual fields",
            position = "bottomright",
            opacity = 1,
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
                                               paste0(lau_name[1]$LAU_NAME,
                                                      "_",
                                                      acsubst_name[1],
                                                      "_PEC_Soil_NL.html")), selfcontained = T)
        
        # Soil RQ maps for individual AS
        # Create the leaflet map
        if (!pb$finished) pb$tick(tokens = list(map_type = "Creating Soil RQ map", substance = current_substance))
        
        soil_ind_rq_dist <- leaflet() |>
          # Add OpenStreetMap tiles with transparency
          addTiles(options = tileOptions(opacity = 0.5)) |> 
          
          # Add district borders
          addPolylines(
            data = lau_name[1],
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
            popup = ~paste0("<b>Field ID: </b>", "<b>", objectid, "</b>", "<br>",
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
                                                   paste0(lau_name[1]$LAU_NAME,
                                                          "_",
                                                          acsubst_name[1],
                                                          "_RQ_Soil_NL.html")), selfcontained = T)
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
        lau_name[1]$LAU_NAME
      )
      
      if (!pb$finished) pb$tick(tokens = list(map_type = "Creating cumulative Soil RQ map"))
      # Cumulative RQ soil map
      soil_cum_rq_dist <- leaflet() |>
        # Add OpenStreetMap tiles with transparency
        addTiles(options = tileOptions(opacity = 0.5)) |>
        
        # Add district borders
        addPolylines(
          data = lau_name[1],
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
          popup = ~paste0("<b>Field ID: </b>", "<b>", objectid, "</b>", "<br>",
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
                               lau_name[1]$LAU_NAME,
                               "_RQcum_Soil_NL.html"),
                 selfcontained = T)
      
      write_excel_csv(soil_farm_mapinput |>
                        values()  |> 
                        select(-c(srunoff_as_load, LAU_NAME, rq_acsubst_total_soil_twa)) |> 
                        cbind(endday |>
                                as.data.frame()) |> 
                        cbind(lau_name[1] |> 
                                as.data.frame()) |> 
                        rename(simtime_day = endday),
                      paste0(path_home_r(),
                             "/Soil/PEC/",
                             lau_name[1]$LAU_NAME,
                             "_PEC_Soil_NL.csv"))
      
      write_excel_csv(soil_farm_mapinput |>
                        values()  |> 
                        select(-c(srunoff_as_load, LAU_NAME, conc_acsubst_total_soil_twa_ug.kg)) |> 
                        cbind(endday |>
                                as.data.frame()) |> 
                        cbind(lau_name[1] |> 
                                as.data.frame()) |> 
                        rename(simtime_day = endday),
                      paste0(path_home_r(),
                             "/Soil/RQ/",
                             lau_name[1]$LAU_NAME,
                             "_RQ_Soil_NL.csv"))
      
      write_excel_csv(soil_cumRQ |>
                        values() |> 
                        select(-LAU_NAME) |> 
                        cbind(endday |>
                                as.data.frame()) |> 
                        cbind(lau_name[1] |> 
                                as.data.frame()) |> 
                        rename(simtime_day = endday),
                      paste0(path_home_r(),
                             "/Soil/RQ/",
                             lau_name[1]$LAU_NAME,
                             "_",
                             "RQcum_Soil_NL.csv")) 
      
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
      
      orcarb_field <- terra::zonal(orcarb_nl,
                                   gemup_lau,
                                   fun = "mean",
                                   # weights = T,
                                   # exact = T,
                                   as.polygons = T) |>
        rename("orcarb_perc" = "oc_perc") |>
        mutate(orcarb_perc = orcarb_perc/100)
      
      # srunoff_river_seg <- terra::zonal(srunoff_basin,
      #                                   gemup_loc_rivers_seg,
      #                                   fun = "mean",
      #                                   # weights = T,
      #                                   # exact = T,
      #                                   as.polygons = T) |>
      #   rename("SR" = "srunoff_CZ")
      
      slope_field <- terra::zonal(slope_nl_30as,
                                  gemup_lau,
                                  fun = "mean",
                                  # weights = T,
                                  # exact = T,
                                  as.polygons = TRUE) |> 
        rename("slope_perc" = "TerrainSlope_30as_nl")
      
      sand_field <- terra::zonal(sand_jrc_nl,
                                 gemup_lau,
                                 fun = "mean",
                                 # weights = T,
                                 # exact = T,
                                 as.polygons = TRUE) |>
        rename("sand_perc" = "sand_jrc_NL")
      
      
      clay_field <- terra::zonal(clay_jrc_nl,
                                 gemup_lau,
                                 fun = "mean",
                                 # weights = T,
                                 # exact = T,
                                 as.polygons = TRUE) |>
        rename("clay_perc" = "clay_jrc_NL")
      
      budens_field <- terra::zonal(budens_jrc_nl,
                                   gemup_lau,
                                   fun = "mean",
                                   # weights = T,
                                   # exact = T,
                                   as.polygons = TRUE) |>
        rename("bulk_dens" = "budens_jrc_NL")
      
      # Patch size of arable land in a river basins and district
      crop_area_tot_lau <- gemup_lau |>
        values() |>
        group_by(LAU_NAME, Crop, acsubst) |>
        summarise(crop_acsubst_totarea_lau.ha = sum(field_area)) |>
        ungroup()
      
      # District and crop specific application rate per lau
      acsubst_application_lau <- gemup_lau |>
        terra::merge(crop_area_tot_lau, by.x = c("Crop", "acsubst", 'LAU_NAME'), by.y = c("Crop", "acsubst", "LAU_NAME")) |>
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
        mutate(conc_acsubst_total_soil_twa_ug.kg = (conc_acsubst_total_soil_ini_ug.kg / (endday * (log(2) / DT50_typical_d))) * (1 - exp(-56 * (log(2) / DT50_typical_d)))) |> 
        ## Effect of terrain slope
        mutate(slope_effect = map_dbl(slope_perc,
                                      ~if_else(. <= 20,
                                               0.001423 * .^2 + 0.02153 * .,
                                               1))) |>
        ##Soil chronic RQ
        mutate(rq_acsubst_total_soil_twa = conc_acsubst_total_soil_twa_ug.kg/NOEC_earthworm_chron_repr_mg.kg) |> 
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
        ## AS spray drift loading
        mutate(sdrift_as_load = map2_dbl(aprate_farm_g.ha,
                                      sdrift_90lnorm,
                                      ~.x * (.y/100))) |>
        ## Application rate correction
        mutate(aprate_farm_kg.ha = map2_dbl(aprate_farm_g.ha,
                                            sdrift_as_load,
                                            ~.x-.y)/1000) |> 
        ## AS surface runoff loading
        mutate(srunoff_as_load = pmap_dbl(list(field_area,
                                               aprate_farm_kg.ha,
                                               infactor_effect,
                                               frac_asubst_soil_water_lag,
                                               slope_effect,
                                               srunoff_tot_frac),
                                          prod))
      
      # Combine AS concentration in soil results on individual farms
      # Calculate RQ for individual farms and individual AS
      soil_farm_mapinput <- load_acsubst_farm |>
        select(LAU_NAME,
               objectid,
               Crop,
               acsubst,
               conc_acsubst_total_soil_twa_ug.kg,
               rq_acsubst_total_soil_twa,
               srunoff_as_load,
               sdrift_as_load) |> 
        mutate(Crop = str_replace_all(Crop, "_", " "),
               Crop = str_to_sentence(Crop)) |> 
        # mutate(rq_acsubst_total_soil_twa = rq_acsubst_total_soil_twa) |>
        mask(lau_name[1])
      
      # Calculate cumulative RQ in soil for individual fields
      # Create a character type column showing a list of ASs and RQs for each field
      soil_RQ_nest <- soil_farm_mapinput |>  
        # filter(acsubst %in% acsubst_soil) |> 
        values() |> 
        group_by(objectid, Crop) |> 
        distinct(objectid, acsubst, .keep_all = T) |>
        arrange(desc(rq_acsubst_total_soil_twa), .by_group = T) |>
        mutate(nr_as = n(),
               rq_acsubst_total_soil_twa =  round(rq_acsubst_total_soil_twa, 3),
               rq_acsubst_total_soil_twa = case_when(rq_acsubst_total_soil_twa < 0.01 ~ 0.01,
                                                     .default = rq_acsubst_total_soil_twa)) |> 
        # filter(rq_acsubst_total_soil_twa >= 0.01) |>
        unite("AS_rq", acsubst, rq_acsubst_total_soil_twa, sep = " | ", remove = FALSE) |>
        select(objectid, Crop, nr_as, AS_rq) |>
        nest("AS_rq" = AS_rq) |> 
        ungroup()
      
      # Calculate summed RQs for individual fields
      soil_cumRQ <- soil_farm_mapinput |> 
        select("LAU_NAME", "objectid", "Crop", "acsubst", "rq_acsubst_total_soil_twa") |> 
        group_by(objectid, Crop) |> 
        summarise(sum_rq_acsubst_total_soil_twa = sum(rq_acsubst_total_soil_twa),
                  .groups = "keep") |> 
        makeValid()
      
      # Merge nested RQs and summed RQs  datasets by unique field IDs
      soil_cumRQ_all <- terra::merge(soil_cumRQ, soil_RQ_nest, by = c("Crop", "objectid")) |> 
        mutate(AS_rq = str_remove_all(AS_rq, "\""),
               AS_rq = str_remove_all(AS_rq, "list"),
               AS_rq = str_remove_all(AS_rq, "AS_rq"),
               AS_rq = str_remove(AS_rq, "c"),
               AS_rq = str_remove_all(AS_rq, "="),
               AS_rq = str_remove_all(AS_rq, "\\("),
               AS_rq = str_remove_all(AS_rq, "\\)"),
               AS_rq = str_replace_all(AS_rq, "0.01$", "<0.01"),
               Crop = str_replace_all(Crop, "_", " "),
               Crop = str_to_sentence(Crop)) |>
        mask(lau_name[1])

      # Intersect farm loadings with river buffer segments
      # Calculate RQ for individual fields intersected with buffer around river segments
      river_seg_mapinput <- soil_farm_mapinput[c("srunoff_as_load",
                                                 "sdrift_as_load",
                                                 "acsubst",
                                                 "LAU_NAME")] |>
        # filter(acsubst %in% acsubst_soil) |> 
        terra::intersect(rivers_class_lau_buff[c("id", "length_m", "dis_m3_pyr")]) |>
        terra::merge(chemprop[c("Active", "NOEC_fish_21_mg.L")], by.x = "acsubst", by.y = "Active") |>
        mutate(conc_mean_river_seg = srunoff_as_load+sdrift_as_load/dis_m3_pyr,
               rq_mean_river_seg_twa = conc_mean_river_seg/NOEC_fish_21_mg.L/1000) |> 
        mask(lau_name[1])
      
      farm_area_buff <- terra::merge(rivers_class_lau_buff,
                                     cbind(river_seg_mapinput[c("id", "length_m")] |> 
                                             terra::aggregate(c("id", "length_m")),
                                           river_seg_mapinput[c("id", "length_m")] |> 
                                             terra::aggregate(c("id", "length_m")) |> 
                                             expanse(unit = "m")) |> values(),
                                     by = c("id", "length_m")) |> 
        rename(farm_buff_area.m = y,
               nr_fields = agg_n)
      
      buff_area <- cbind(rivers_class_lau_buff[c("id", "length_m", "dis_m3_pyr")] |> 
                           aggregate(c("id", "length_m", "dis_m3_pyr")),
                         rivers_class_lau_buff[c("id", "length_m", "dis_m3_pyr")] |> 
                           aggregate(c("id", "length_m", "dis_m3_pyr")) |> 
                           expanse(unit = "m")) |> 
        rename(buffer_area.m = y,
               nr_buffers = agg_n)
      
      river_weight <- terra::merge(farm_area_buff, buff_area |>
                                     values(),
                                   by = c("id", "length_m")) |> 
        mutate(river_w = case_when(river_class_name == "3_6" ~ (length_m*4)/(buffer_area.m*(farm_buff_area.m/buffer_area.m)),
               river_class_name == "0.5_3" ~ (length_m*1.5)/(buffer_area.m*(farm_buff_area.m/buffer_area.m))))
      
      # Calculate cumulative RQ for fields in river buffer for individual river segments
      # Create a character type column showing a list of ASs and RQs for each river segment
      river_RQ_nest <- terra::merge(river_seg_mapinput,
                                    river_weight[c("id", "length_m", "river_w")] |> 
                                      values(),
                                    by =  c("id", "length_m")) |>
        values() |> 
        group_by(id, length_m) |> 
        arrange(desc(rq_mean_river_seg_twa), .by_group = T) |>
        distinct(acsubst, .keep_all = T) |>
        mutate(nr_fields = n(),
               rq_mean_river_seg_twa = round(rq_mean_river_seg_twa*river_w, 3),
               rq_mean_river_seg_twa = case_when(rq_mean_river_seg_twa < 0.001 ~ 0.001,
                                                 .default = rq_mean_river_seg_twa)) |> 
        # filter(rq_mean_river_seg_twa >= 0.001) |>
        unite("AS_rq", acsubst, rq_mean_river_seg_twa, sep = " | ", remove = FALSE) |>
        select(id, length_m, nr_fields, AS_rq) |>
        nest("AS_rq" = AS_rq) |> 
        ungroup() 
      
      # Calculate summed weighted RQs for fields in each unique river segment
      river_cumRQ <-terra::merge(river_seg_mapinput,
                                 river_weight[c("id", "length_m", "river_w")] |> 
                                   values(),
                                 by =  c("id", "length_m")) |> 
        group_by(id, length_m) |>
        summarise(sum_rq_mean_river_seg_twa = max(rq_mean_river_seg_twa * river_w), .groups = "keep") |> 
        ungroup() |>
        makeValid()
      
      # Merge nested RQs and summed RQs  datasets by unique river segments and drop geometry
      river_cumRQ_df <- terra::merge(river_cumRQ, river_RQ_nest,
                                     by = c("id", "length_m")) |> 
        mutate(AS_rq = str_remove_all(AS_rq, "\""),
               AS_rq = str_remove_all(AS_rq, "list"),
               AS_rq = str_remove_all(AS_rq, "AS_rq"),
               AS_rq = str_remove(AS_rq, "c"),
               AS_rq = str_remove_all(AS_rq, "="),
               AS_rq = str_remove_all(AS_rq, "\\("),
               AS_rq = str_remove_all(AS_rq, "\\)"),
               AS_rq = str_replace_all(AS_rq, "0.001", "<0.001")) |> 
        mask(lau_name[1]) |>
        values()
        
        # Merge all values from the RQ dataframe with river buffer polygons
        river_cumRQ_all <- terra::merge(rivers_class_lau_buff[c("id", "length_m")],
                                        river_cumRQ_df,
                                        by = c("id", "length_m")) 
        
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
          total = (length(acsubst_name) * 4) + 2,
          clear = FALSE,
          width = 100
        )
        
        actual_soil_acsubst_name <- soil_farm_mapinput$acsubst |> unique()
        actual_river_acsubst_name <- river_seg_mapinput$acsubst |> unique()
        
        # Soil PEC and RQ individual maps 
        for (as in seq_along(acsubst_name <- actual_soil_acsubst_name[1])) {
          
          # Store current substance name for progress updates
          current_substance <- acsubst_name[1]
          
          # Prepare data
          # Prepare soil data
          soil_data <- soil_farm_mapinput |> 
            filter(acsubst %in% acsubst_name[1])
          
          # Concentration maps palettes
          # Soil concentration palette
          
          soil_conc_pal <- colorBin(
            palette = "BuPu",
            domain = soil_data$conc_acsubst_total_soil_twa_ug.kg,
            bins = pretty(soil_data$conc_acsubst_total_soil_twa_ug.kg),
            na.color = "#ffdeaf"
          )
          
          soil_conc_pal_rev <- colorBin(
            palette = "BuPu",
            domain = soil_data$conc_acsubst_total_soil_twa_ug.kg,
            bins = pretty(soil_data$conc_acsubst_total_soil_twa_ug.kg),
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
            acsubst_name[1], " ",
            endday |> unique(),
            "-day PEC soil after 1x application in ",
            app_month,
            " ",
            "(",
            sim_yr,
            ")",
            " in ",
            lau_name[1]$LAU_NAME
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
            lau_name[1]$LAU_NAME
          )
          
          # AS concentration in topsoil
          if (!pb$finished) pb$tick(tokens = list(map_type = "Creating Soil PEC map", substance = current_substance))
          
          soil_as_conc <- leaflet() |>
            addTiles(options = tileOptions(opacity = 0.5)) |> 
            
            # Add district borders
            addPolylines(
              data = lau_name[1],
              color = "black",
              weight = 0.75,
              opacity = 1,
              fillOpacity = 0,
              group = "Administrative borders"
            ) |>
            
            # Add soil concentration polygons
            addPolygons(
              data = soil_data,
              fillColor = ~soil_conc_pal(conc_acsubst_total_soil_twa_ug.kg),
              fillOpacity = 0.85,
              color = "black",
              weight = 0.25,
              opacity = 1,
              popup = ~paste0("<b>Field ID: </b>", "<b>", objectid, "</b>", "<br>",
                              "<b>Crop: </b>", "<b>", Crop, "</b>", "<br>",
                              "<b>Individual PEC soil (µg × kg⁻¹): </b>", "<b>",
                              ifelse(is.na(conc_acsubst_total_soil_twa_ug.kg), 
                                     "Missing values",
                                     format_power10(conc_acsubst_total_soil_twa_ug.kg,  digits = 2)), "</b>"),
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
              values = soil_data$conc_acsubst_total_soil_twa_ug.kg,
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
                                                 paste0(lau_name[1]$LAU_NAME,
                                                        "_",
                                                        acsubst_name[1],
                                                        "_PEC_Soil_NL.html")), selfcontained = T)
          
          # Soil RQ maps for individual AS
          # Create the leaflet map
          if (!pb$finished) pb$tick(tokens = list(map_type = "Creating Soil RQ map", substance = current_substance))
          
          soil_ind_rq_dist <- leaflet() |>
            # Add OpenStreetMap tiles with transparency
            addTiles(options = tileOptions(opacity = 0.5)) |> 
            
            # Add district borders
            addPolylines(
              data = lau_name[1],
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
              popup = ~paste0("<b>Field ID: </b>", "<b>", objectid, "</b>", "<br>",
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
                                                     paste0(lau_name[1]$LAU_NAME,
                                                            "_",
                                                            acsubst_name[1],
                                                            "_RQ_Soil_NL.html")), selfcontained = T)
        }
        
        # Water PEC and RQ individual maps 
        for (as in seq_along(acsubst_name <- actual_river_acsubst_name[1])) {
          
          # Store current substance name for progress updates
          current_substance <- acsubst_name[1]
          
          # Prepare data
          # Prepare river water data
          river_data <- terra::merge(rivers_class_lau_buff[c("id",
                                                           "length_m",
                                                           "breedteklasse")],
                                     terra::merge(river_seg_mapinput[c("id",
                                                                       "length_m",
                                                                       "rq_mean_river_seg_twa",
                                                                       "conc_mean_river_seg",
                                                                       "acsubst")] |> 
                                                    filter(acsubst %in% acsubst_name[1]),
                                                  river_weight[c("id",
                                                                 "length_m",
                                                                 "river_w")] |> 
                                                    values(),
                                                  by =  c("id",
                                                          "length_m")) |>
                                       group_by(id,
                                                length_m) |> 
                                       arrange(desc(rq_mean_river_seg_twa), .by_group = T) |>
                                       mutate(nr_fields = n(),
                                              rq_mean_river_seg_twa = round(sum(rq_mean_river_seg_twa*river_w), 4),
                                              conc_mean_river_seg = mean(conc_mean_river_seg*river_w),
                                              .groups = "keep") |> 
                                       distinct(acsubst, .keep_all = T) |>
                                       ungroup() |> 
                                       values(),
                                     by = c("id",
                                            "length_m"))
          
          rivers_class_lau_buff_corr <- rivers_class_lau |> filter(id %in% river_data$id) |> buffer(20)
          river_data_corr <- terra::intersect(rivers_class_lau_buff_corr[""], river_data)
          river_length_class <- paste0(river_classes[[river_class_name]]$length_min, " - ", river_classes[[river_class_name]]$length_max)
          
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
            acsubst_name[1], " ",
            endday |> max(),
            "-day PEC<sub>time- and area-weighted</sub> surface water after 1x application in ",
            app_month,
            " ",
            "(",
            sim_yr,
            ")",
            " in ",
            lau_name[1]$LAU_NAME
          )
          
          # RQ maps titles
          # River water RQ map title
          river_rq_title <- paste0(
            acsubst_name[1], " ",
            endday |> unique(),
            "-day individual RQ<sub>fish, area-weighted</sub> surface water after 1x application in ",
            app_month,
            " ",
            "(",
            sim_yr,
            ")",
            " in ",
            lau_name[1]$LAU_NAME
          )
          
          # AS concentration in river water
          if (!pb$finished) pb$tick(tokens = list(map_type = "Creating Water PEC map", substance = current_substance))
          
          river_as_conc <- leaflet() |>
            addTiles(options = tileOptions(opacity = 0.5)) |> 
            
            # Add district borders
            addPolylines(
              data = lau_name[1],
              color = "black",
              weight = 0.75,
              opacity = 1,
              fillOpacity = 0,
              group = "Administrative borders"
            ) |>
            
            # Add river water network
            addPolylines(data = rivers_class_lau,
                         color = "blue",
                         weight = 0.5,
                         opacity = 1,
                         group = "River network" 
            ) |> 
            
            # Add river water concentration polygons
            addPolygons(
              data = river_data_corr,
              fillColor = ~river_conc_pal(conc_mean_river_seg),
              fillOpacity = 0.85,
              color = "black",
              weight = 0.25,
              opacity = 1,
              popup = ~paste0("<b>River ID: </b>", "<b>", id, "</b><br>",
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
            addPolygons(data = basins_lau_nl,
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
                                                  paste0(lau_name[1]$LAU_NAME,
                                                         "_",
                                                         acsubst_name[1],
                                                         "_PEC_Water_NL.html")), selfcontained = T)
          
          # River water RQ maps for individual AS
          # Create the leaflet map
          if (!pb$finished) pb$tick(tokens = list(map_type = "Creating Water RQ map", substance = current_substance))
          
          river_ind_rq_dist <- leaflet() |>
            # Add OpenStreetMap tiles with transparency
            addTiles(options = tileOptions(opacity = 0.5)) |>
            
            # Add district borders
            addPolylines(
              data = lau_name[1],
              color = "black",
              weight = 0.75,
              opacity = 1,
              fillOpacity = 0,
              group = "Administrative  borders"
            ) |>
            
            # Add river water network
            addPolylines(data = rivers_class_lau,
                         color = "blue",
                         weight = 0.5,
                         opacity = 1,
                         group = paste0("River network<br>",
                                        "Breadth class: ",
                                        river_classes[[river_class_name]]$width,
                                        "<br>",
                                        "Length class: ",
                                        ifelse(is.null(river_classes[[river_class_name]]$length_min) == T && is.null(river_classes[[river_class_name]]$length_max) == T,
                                               "ALL",
                                               ifelse(is.null(river_classes[[river_class_name]]$length_min) == T && !is.null(river_classes[[river_class_name]]$length_max) == T,
                                                      paste0("0 - ", river_classes[[river_class_name]]$length_max),
                                                      river_length_class)),
                                        " meter")
            ) |> 
            
            # Add river water RQ polygons
            addPolygons(
              data = river_data_corr,
              fillColor = ~river_rq_pal(rq_mean_river_seg_twa),
              fillOpacity = 0.85,
              color = "black",
              weight = 0.25,
              opacity = 1,
              popup = ~paste0("<b>River name: </b>", "<b>", id, "</b><br>",
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
            addPolygons(data = basins_lau_nl,
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
              overlayGroups = c("Administrative  borders",
                                "Individual river segments",
                                paste0("River network<br>",
                                       "Breadth class: ",
                                       river_classes[[river_class_name]]$width,
                                       "<br>",
                                       "Length class: ",
                                       ifelse(is.null(river_classes[[river_class_name]]$length_min) == T && is.null(river_classes[[river_class_name]]$length_max) == T,
                                              "ALL",
                                              ifelse(is.null(river_classes[[river_class_name]]$length_min) == T && !is.null(river_classes[[river_class_name]]$length_max) == T,
                                                     paste0("0 - ", river_classes[[river_class_name]]$length_max),
                                                     river_length_class)),
                                       " meter"),
                                "River basins"),
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
                                                      paste0(lau_name[1]$LAU_NAME,
                                                             "_",
                                                             acsubst_name[1],
                                                             "_RQ_Water_NL.html")), selfcontained = T)
        }
      

    }
  }
}
