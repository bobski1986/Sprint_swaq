####################################################
# Generalised Mapped Application Patterns (GeMAPs) #
####################################################

# Create a vector of packages
pkg <- c("fs" ,"sf", "terra", "RCzechia", "tidyverse", "tidygeocoder", "tmap", "data.table", "fuzzyjoin", "readxl")

# Download required packages
for (i in pkg) {
  if (!requireNamespace(i, quietly = TRUE)) {
    install.packages(i, repos = c("https://r-tmap.r-universe.dev", "https://cloud.r-project.org"))
  }
}

# Load packages
lapply(pkg, library, character.only = T)

# NUTS regions
nuts <- dir_ls(path_home_r(), recurse = T, regexp = "/NUTS_RG_20M_2024_4326.shp$")  |>
  vect()
nuts_cz <- nuts |>
  filter(CNTR_CODE == "CZ")

# Get the CZ borders etc.
districts <- okresy("low")|>
  mutate(NAZ_LAU1 = tolower((gsub("-", "_", NAZ_LAU1, fixed = TRUE))))

district_names <- districts$NAZ_LAU1

# Get the CZ crop-district map
crop_map_cz_dist <- dir_ls(path_home_r(), recurse = T, regexp = "crop_district_map2021_cz.gpkg$") |> 
  st_read()

# Mass of used individual ai and areas under application for whole district# Mass of used individual ai and areas under application for whDistrictole district
ai_usage_mass_dist <- list()
for (i in seq_along(district_names)) {
  ai_usage_mass_dist[[i]] <- ai_mass_crop_type_list |> 
    filter(District %in% district_names[i]) |> 
    unnest(ai_mass_usage_det) |> 
    select(-year) |>
    filter(!if_any("value", is.na)) |> 
    filter(crop_code_eagri %in% crop_map_all[["crop_code_eagri"]]) |> 
    rename(ai_mass_tot_dist = value,
           mass_unit = unit)
  
  cat("\rAS mass table for district",i,"joined")
}


ai_usage_area_dist <- list()
for (i in seq_along(district_names)) {
  ai_usage_area_dist[[i]] <- ai_area_crop_type_list|>
    filter(District %in% district_names[i])|> 
    unnest(ai_area_usage_det)|> 
    select(-year) |> 
    filter(!if_any("value", is.na)) |>
    filter(crop_code_eagri %in% crop_map_all[["crop_code_eagri"]])|>
    rename(ai_area_tot_dist = value,
           area_unit = unit)
  
  cat("\rAS area table for district",i,"joined")
}


# Total application rate per district
ai_usage_rate <- list()
for(i in seq_along(ai_usage_mass_dist)){
  for(i in seq_along(ai_usage_area_dist)){
    ai_usage_rate[[i]] <- left_join(ai_usage_mass_dist[[i]],
                                    ai_usage_area_dist[[i]][c("crop_name_eagri", "ai", "ai_area_tot_dist", "area_unit", "crop_code_eagri", "crop_name_focus")],
                                    by = c("crop_name_eagri", "ai", "crop_code_eagri", "crop_name_focus")) |> 
      mutate(ai_app_rate_tot_dist = (ai_usage_mass_dist[[i]]$ai_mass_tot_dist/ai_usage_area_dist[[i]]$ai_area_tot_dist)*1000,
             ai_app_rate_unit = "g/ha")
    
    cat("\rAS application rate table for district nr",i,"joined")
    
  }
}


# Join GenAP with usage data
ai_usage_genap_rate <- list()
for (i in seq_along(ai_usage_rate)) {
  
  ai_usage_genap_rate[[i]] <- full_join(ai_usage_rate[[i]],
                                  genap,
                                  by = c("ai", "crop_code_eagri", "crop_code_eppo")) |> 
    mutate(ai_app_rate_tot_dist = round(ai_app_rate_tot_dist, 2),    
           ai_max_apprate_check = case_when(ai_app_rate_tot_dist > max_app_rate ~ "OUT",
                                            .default = "OK"))
  
  cat("\rGenAP table joined with district usage rate table nr.",i)
  
}

# Use fuzzy matching on AS names to join AS usage data with substance codebook (move to the function calculating usage rate for each district). fuzzyjoin package instead of stringdist.
# Pay attention when using EU_name or PPDB_name for filtering/selecting chemicals!!

ai_usage_chemcode_rate <- list()
for (i in seq_along(ai_usage_genap_rate)) {
  ai_usage_chemcode_rate[[i]] <- stringdist_left_join(ai_usage_genap_rate[[i]],
                                             substance_codebook,
                                             by = c("ai" = "PPDB_name"),
                                             method = "lv",
                                             max_dist = 1,
                                             ignore_case = TRUE) |> 
    filter(!is.na(District))
  
  cat("\rSubstance codebook table joined with district", ai_usage_chemcode_rate[[i]]$District |> unique(),  "usage rate table nr.",i)
  
}


# Full spatial object with geographic information, crop usage attributes, GenAP
crop_map_cz_dist_41_100 <- crop_map_cz_dist |>
  left_join(ai_usage_chemcode_rate |> rbindlist(),
            by = c("crop_code_eagri", "District"))

# Save shapefiles for all districts
st_write(crop_map_cz_dist_41_100 |>
           st_as_sf() |> 
           rename(Year = Year,
         Crop = crop_name_eppo_short,
         EPPO = crop_code_eppo,
         FOCUS = crop_name_focus,
         FOCUS_com = crop_focus_comment,
         EAGRI = crop_code_eagri,
         District = District,
         CTVEREC = CTVEREC,
         ZKOD = ZKOD,
         FieldAr = DEKL_VYMER,
         Active = ai,
         CAS = CAS_number,
         AS_eu_name = EU_name,
         AS_ppdb_name = PPDB_name,
         ASmass = ai_mass_tot_dist,
         ASmassunit = mass_unit,
         ASarea = ai_area_tot_dist,
         ASareaunit = area_unit,
         ARfarm = ai_app_rate_tot_dist,
         ARmin = min_app_rate,
         ARmax = max_app_rate,
         ARunit = ai_app_rate_unit,
         ARcheckmax = ai_max_apprate_check,
         BBCHmin = min_bbch,
         BBCHmax = max_bbch,
         ApFreq = max_app_per_year,
         IFav = inter_frac_bbch.ave,
         IFmin = inter_frac_bbch.min,
         IFmax = inter_frac_bbch.max),
"gemap41_100_cz_wgs84.gpkg", append = F)

# Visual check

dist_name <- districts |> tidyterra::filter(NAZ_LAU1 == "benešov")
gemap_ben <- dir_ls(path_home_r(), recurse = T, regexp = "gemap41_100_cz_wgs84") |>
  vect(extent = ext(dist_name)) |> 
  mask(vect(dist_name)) |> 
  tidyterra::select(District, AS_eu_name, AS_ppdb_name, ARfarm, ARmin, ARmax)
  
tmap_mode("plot")

gemap41_100_cz <- tm_shape(dist_name) +
  tm_borders() + 
tm_shape(gemap_ben |> filter(AS_ppdb_name %in% ASs[42:50])) +
  tm_polygons(fill = "ARfarm",
              fill.scale = tm_scale_intervals(label.na = F , values = "matplotlib.yl_or_br"),
              fill.legend = tm_legend(position = tm_pos_in(pos.h = "right",
                                                            pos.v = "top"),
                                      bg.color = "white",
                                      title = "AppRate (g/ha)",
                                      title.size = 0.5,
                                      width = 8, height = 9),
              fill.free = T) +
  # tm_title(paste0("Active susbtance applcation rates in ", dist_name$NAZ_LAU1 |> str_to_title())) +
  tm_facets_wrap(by = "AS_ppdb_name", ncol = 3) +
  tm_basemap("OpenStreetMap")

tmap_save(gemap41_100_cz, "gemap41_100_cz.png", dpi = 600, scale = 0.5)
# tmap_animation(gemap11_20_cz, "gemap11_20_cz.gif", fps = 0.25, width=1200, height = 1200, dpi = 200)



# Get the crop cover distribution for CZ
# crop_map_cz <- dir_ls(path_home_r(), recurse = T, regexp = "GEOPROSTOR_ZADOSTI_2021-12-31.shp$") |> 
#   st_read() |> 
#   rename(crop_name_eagri_map = PLODINA_NA,
#          crop_code_eagri = PLODINA_ID) |> 
#   mutate(crop_code_eagri = as.numeric(crop_code_eagri))

# districts <- st_transform(districts, st_crs(crop_map_cz))

# crop_map_cz_dist <- st_intersection(crop_map_cz, districts)|>
#   rename(District = NAZ_LAU1,
#          Region = NAZ_CZNUTS3) |>
#   filter(!if_any("geometry", st_is_empty)) |>
#   st_transform(crs(nuts_cz)) |>
#   nest(.by = District)

# st_write(crop_map_cz_dist, "crop_district_map2021_cz.gpkg")

# Get the borders for the EU and other administrative units
## Download the data if needed
### All countries
# cntr_url <- "https://gisco-services.ec.europa.eu/distribution/v2/countries/download/ref-countries-2020-01m.shp.zip"
# 
# download.file(cntr_url,
#               file.path(set_dir,
#                         basename(cntr_url)),
#               mode = "wb")
# 
# unzip(zipfile = file.path(set_dir,
#                         "ref-countries-2020-01m.shp.zip"))
# unzip(zipfile = file.path(set_dir,
#                           "CNTR_RG_01M_2020_4326.shp.zip"))

### European communes
# cmm_url <- "https://gisco-services.ec.europa.eu/distribution/v2/communes/download/ref-communes-2016-01m.shp.zip"
# 
# download.file(cmm_url,
#               file.path(set_dir,
#                         basename(cmm_url)),
#               mode = "wb")
# 
# unzip(zipfile = file.path(set_dir,
#                           "ref-communes-2016-01m.shp.zip"))
# unzip(zipfile = file.path(set_dir,
#                           "COMM_RG_01M_2016_4326.shp.zip"))

## Load countries' boarders shape file
# cntrs <- st_read(paste0(set_crop_spatial_dir, "CNTR_RG_01M_2020_4326.shp"), 
#                   stringsAsFactors = FALSE) |>
#   st_transform(4326) |>
#   st_as_sf()
# 
# ## Filter countries
# cntrs_eu <- cntrs |> 
#   filter(EU_STAT == "T" | NAME_ENGL == "United Kingdom")
#   # filter(NAME_ENGL == "Czechia")
# 
# ## Remove extra EU territories and center on the EU
# extra_terr_eu <- subset(cntrs_eu, CNTR_ID %in% c("FR", "PT", "ES", "RU"))
# 
# buffer <- geo("Bern", quiet = T) |> 
#   st_as_sf(coords = c("long", "lat"), crs = 4326) |>  
#   st_buffer(as_units(1750, "km")) |>  
#   st_geometry()
# 
# extra_terr_eu <- extra_terr_eu |>
#   st_intersection(buffer)
# 
# cntrs_eu <- cntrs_eu |> 
#   filter(!CNTR_ID %in% c("RU", "SJ", "FR", "PT", "ES")) |> 
#   rbind(extra_terr_eu) |>
#   st_transform(3035)

##Load EU communities' boarders shapefile
# comms <- st_read("COMM_RG_01M_2016_4326.shp", 
#                stringsAsFactors = FALSE) %>% 
#   st_transform(4326) %>% 
#   st_as_sf() %>%
#   filter(!CNTR_ID%in%"GL")

## EU Crop Map
# eu_crop_map_url <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EUCROPMAP/2022/EU_CropMap_22_v1_stratum_EU27-HR.tif"
# Check for raster files and pick one
# download.file(cmm_url,
#               file.path(set_dir,
#                         basename(cmm_url)),
#               mode = "wb")
# list.files(path = paste0(set_dir, "/Spatial"),
#            pattern = ".tif$",
#            all.files=T,
#            full.names=F)
#  crop_map_eu <- paste0(set_crop_spatial_dir, "/EU_CropMap_22_v1_stratum_EU27-HR.tif") |> 
# rast() |> 
#   rename(EU_crop_class = "EU_CropMap_22_v1_stratum_EU27-HR")
# 
## Mapping AS usage using EU crop map 2022
# crop_map_distr <- crop(crop_map, districts_AS_usage_full )
# 
# crop_map_distr_df <- terra::extract(crop_map,
#                                     districts_AS_usage_full,
#                                           xy = TRUE) |> 
#   mutate(across(EU_crop_class, as.character)) |> 
#   mutate(EU_crop_class = case_match(EU_crop_class,
#                                  "100" ~ "Artificial land",
#                                  "200" ~ "Arable land",
#                                  "211" ~ "Common wheat",
#                                  "213" ~ "Barley",
#                                  "214" ~ "Rye",
#                                  "215" ~ "Oats",
#                                  "216" ~ "Maize",
#                                  "217" ~ "Rice",
#                                  "218" ~ "Triticale",
#                                  "219" ~ "Other cereals",
#                                  "221" ~ "Potato",
#                                  "222" ~ "Sugar beet",
#                                  "223" ~ "Other root crops",
#                                  "230" ~ "Other non permanent industrial crops",
#                                  "231" ~ "Sunlower",
#                                  "232" ~ "Rape and turnip rape",
#                                  "233" ~ "Soya",
#                                  "240" ~ "Dry pulses, vegetables and flowers",
#                                  "250" ~ "Other fodder crops",
#                                  "290" ~ "Bare arable land",
#                                  "300" ~ "Woodland and shrubland",
#                                  "500" ~ "Grassland",
#                                  "600" ~ "Bare land and lichens/moss"))
#                                 
# crop_map_distr_rast <- crop_map_distr_df |>
#   filter(EU_crop_class == "Maize") |>
#   as_spatraster(xycols = 3:4,
#                 crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
#              
#This part is used to extract and write data from raster into .csv files.
# crop_map_districts_list <- list()
# crop_map_districts_extracted_list <- list()
# for(i in 1:nrow(districts)){
# 
#   districts_boundaries <- districts[i , ]
# 
#   crop_map_districts <- crop(crop_map, districts_boundaries)
# 
#   crop_map_districts_list[[i]] <- crop_map_districts
#   # 
#   # crop_map_districts_extracted_list[[i]] <- terra::extract(crop_map_districts,
#   #                                         districts_boundaries,
#   #                                         xy = T)|>
#   #     rename(Crop_class = "EU_CropMap_22_v1_stratum_EU27-HR") |> 
#     
#   # 
#   # write_csv(crop_map_districts_extracted,
#   #            file = paste0(set_dir, "district_",
#   #                          i,
#   #                          ".csv"))
# }
# save( crop_map_districts_extracted_list,
#       .file = "cropped_raster_list.RData")

# This loop was used for joining nested by district and crop two lists Crop map and application rates
# crop_map_cz_dist_full <- list()
# for (i in seq_along(ai_usage_chemcode_rate)) {
#   for(i in 1:nrow(crop_map_cz_dist)) {
#   crop_map_cz_dist_full[[i]] <- crop_map_cz_dist[i,] |>
#     # filter(crop_code_eagri %in% crop_codes_map[["crop_code_eagri"]])|>
#     left_join(ai_usage_chemcode_rate |> rbindlist(),
#               by = c("crop_code_eagri", "District"))|>
#     filter(!if_any("geometry", st_is_empty), !is.na(ai))
# 
#   cat("\rTable",i,"created for district", crop_map_cz_dist_full[[i]]$District |> unique())
#   }
#   return(crop_map_cz_dist_full)
# }
