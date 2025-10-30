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
         "progress")

for (i in pkg) {
  if (!requireNamespace(i, quietly = TRUE)) { 
    install.packages(i)
  }
}

lapply(pkg, library, character.only = T)

# Data import for Denmark
# NUTS regions
nuts <- dir_ls(path_home_r(), recurse = T, regexp = "/NUTS_RG_20M_2024_4326.shp$")  |>
  vect()

nuts3_dk <- nuts |>
  filter(CNTR_CODE == "DK" & LEVL_CODE == 3)

districts_dk <- dir_ls(path_home_r(), recurse = T, regexp = "/LAU_RG_01M_2024_4326.gpkg") |> 
  vect() |> 
  filter(CNTR_CODE == "DK")

# Define the approximate bounding box for country in EPSG:32631 coordinates.
crs_utm31n_dk <- "EPSG:32631"

project(nuts3_dk, crs_utm31n_dk)

xmin <- 815665.9  # Approximate western boundary (easting)
xmax <- 1273097  # Approximate eastern boundary (easting)
ymin <- 6083472 # Approximate southern boundary (northing)
ymax <- 6419198   # Approximate northern boundary (northing)

# Define the desired pixel resolution (size of each cell). 
# The size of the pixel is defined based on the smallest size of the vector to be rasterised, e.g., 10m buffer polygons around river segments
res_x <- 100 # n meters in the X (easting) direction
res_y <- 100 # n meters in the Y (northing) direction

# # Calculate the number of columns and rows needed for the raster grid.
ncols <- ceiling((xmax - xmin) / res_x)
nrows <- ceiling((ymax - ymin) / res_y)

# Create an empty raster. This is a template for rasterisation of river network.
empty_raster_dk <- rast(
  ncols = ncols,        # Number of columns
  nrows = nrows,        # Number of rows
  xmin = xmin,          # Minimum X coordinate of the extent
  xmax = xmin + ncols * res_x, # Maximum X coordinate, precisely calculated from xmin and ncols
  ymin = ymin,          # Minimum Y coordinate of the extent
  ymax = ymin + nrows * res_y, # Maximum Y coordinate, precisely calculated from ymin and nrows
  crs = crs_utm31n_dk,     # Coordinate Reference System (EPSG:32631)
  resolution = c(res_x, res_y) # Resolution in X and Y directions
)

basins_dk <- dir_ls(path_home_r(), recurse = T, regexp = "hydrosheds_lvl12_basins_DK.gpkg") |>
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
                    HYBAS_ID = as.character(HYBAS_ID))

# Read soil density map
budens_jrc_dk_path <- dir_ls(path_home_r(),
                             recurse = T,
                             regexp = "budens_jrc_dk.nc$")

budens_jrc_dk <- rast(budens_jrc_dk_path)

budens_jrc_dk_bas <- zonal(budens_jrc_dk, basins_dk, "mean", as.polygons = T, na.rm = T) |>
  select(HYBAS_ID, budens_jrc_DK) |> 
  tidyterra::mutate(HYBAS_ID = as.character(HYBAS_ID))

# Danish river basins including data on soil, basin and terrain characteristics
basins_dk <- basins_dk |> 
  tidyterra::mutate(SUB_AREA_ha = SUB_AREA * 100,
                    HYBAS_ID = as.character(HYBAS_ID)) |> 
  left_join(budens_jrc_dk_bas |> 
              values(),
            by = "HYBAS_ID") |>
  tidyterra::mutate(slp_perc_uav = tan((slp_dg_uav/10)/180*pi),
                    vol_soil_m3_ha = 10000 * 0.05,
                    mass_soil_t3_ha = vol_soil_m3_ha * budens_jrc_DK,
                    oc_perc = (soc_th_uav/mass_soil_t3_ha) * 100,
                    area_basins_dk_ha = basins_dk |> expanse("ha"))

# Read danish gemap
crop_focus_eppo_intercept <- read_xlsx(dir_ls(path_home_r(), recurse = T, regexp = "/EFSA_FOCUS crop interception factor.xlsx"), sheet = 2)

gemap_dk <- dir_ls(path_home_r(), recurse = T, regexp = "gemap_3chem_dk_wgs84") |> 
  vect() |>
  mutate(Active = case_when(
    Active == "acetamiprid" ~ "Acetamiprid",
    Active == "glyphosat" ~ "Glyphosate",
    Active == "tebuconazol" ~ "Tebuconazole")) |>
  merge(crop_focus_eppo_intercept |>
                         select("crop_code_eppo",
                         "inter_frac_bbch.ave",
                        "inter_frac_bbch.min",
                         "inter_frac_bbch.max"), by.x = "CROP_EPPO_CODE", by.y = "crop_code_eppo") |> 
  makeValid()

acitve_gemap_dk <- gemap_dk["Active"] |> values() |> unique() |> pull()

# Chemical input data from qsars (vega, epi) and PPDB where available #
# In case of error e.g., "Error in ppdb_df_values ! Can't extract rows past the end",
# first check if the names of properties are the same in the script and PPDB, 
# PPDB gets updated every now and then, so must be the scraping script.
source(dir_ls(path_home_r(), recurse = T, regexp = "ppdb scraping"))
chemprop <- chemprop_gen(Active = acitve_gemap_dk) |> 
  select(Active,
         DT50_typical_d,
         Koc_ml.g,
         DT50_field_d,
         Kfoc_ml.g) |> 
  mutate(across(2:last_col(), as.numeric)) |> 
  rename(acsubst = Active)

# Intersect danish gemap with river basins
gemap_dk_bas <- terra::intersect(gemap_dk, basins_dk["HYBAS_ID"])

# Get the number of all farms, grouped by crop and AS, in each river basin
mass_as_farms_bas_dk <- terra::aggregate(gemap_dk_bas[c("HYBAS_ID","Active", "EC_trans_n", "App_dose_kg.ha")],
                                         c("HYBAS_ID", "EC_trans_n", "Active"),
                                         fun = "sum")

area_as_farms_bas_dk <- mass_as_farms_bas_dk |> expanse("ha") 

gemap_dk_bas_if <- gemap_dk_bas[c("HYBAS_ID",
                                  "EC_trans_n",
                                   "inter_frac_bbch.ave",
                                    "inter_frac_bbch.min",
                                    "inter_frac_bbch.max")] |>
  terra::aggregate(c("HYBAS_ID", "EC_trans_n"),
                   fun = "mean")

mass_area_as_farms_bas_dk <- cbind(mass_as_farms_bas_dk, area_as_farms_bas_dk |>
  data.frame()) |>
  terra::merge(basins_dk, by = "HYBAS_ID") |> 
  terra::merge(gemap_dk_bas_if[c("HYBAS_ID",
                                 "EC_trans_n",
                                 "mean_inter_frac_bbch.ave",
                                    "mean_inter_frac_bbch.min",
                                    "mean_inter_frac_bbch.max")],
               by = c("HYBAS_ID", "EC_trans_n")) |> 
  tidyterra::rename(area_as_farms_bas_dk_ha = area_as_farms_bas_dk,
                    SUB_AREA_km2 = SUB_AREA,
                    nr_farms_as_crop = agg_n) |>
  tidyterra::mutate(area_as_farms_bas_frac = area_as_farms_bas_dk_ha / area_basins_dk_ha,
                    AppRate_g_ha_corr = (sum_App_dose_kg.ha/area_as_farms_bas_dk) * area_as_farms_bas_frac)

###################################################################
########### START: Pesticide Runoff Model Schriever 2007 ###########
####################################################################

# TWA concentration in soil
pest_twc_as_farm_dk <- mass_area_as_farms_bas_dk |>
  rename(acsubst = Active, 
         IFav = mean_inter_frac_bbch.ave,
         aprate_farm_kg.ha = sum_App_dose_kg.ha,
         bulk_dens_kg.dm3 = budens_jrc_DK,
         slope_perc = slp_perc_uav) |> 
  terra::merge(chemprop, by = "acsubst") |>
  ## Effect of crop interception factor on runoff
  mutate(infactor_effect = map_dbl(IFav,
                                   ~1-(./100))) |>  
  ## Initial as fraction in soil and porewater
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
  mutate(srunoff_load = pmap_dbl(list(area_as_farms_bas_dk_ha,
                                      aprate_farm_kg.ha,
                                      infactor_effect,
                                      frac_asubst_soil_water_lag,
                                      slope_effect,
                                      srunoff_frac),
                                 prod)) |> 
  makeValid()

# Split simulated concentration by substance 
pest_twc_as_basin_dk <- pest_twc_as_farm_dk[c("HYBAS_ID",
                                              "EC_trans_n",
                                              "acsubst",
                                              "nr_farms_as_crop",
                                              "area_as_farms_bas_dk_ha",
                                              "conc_acsubst_total_soil_ini",
                                              "conc_acsubst_total_soil_56twa_ug.kg",
                                              "conc_acsubst_total_soil_365twa_ug.kg")] |>
  tidyterra::mutate(HYBAS_ID = as.character(HYBAS_ID)) |> 
  terra::split("acsubst")

# Rasterise individual parcel polygons to show AS concentration distribution in individual basins
pest_twc_as_farm_dk_rast <- sprc(rasterize(project(pest_twc_as_basin_dk[[1]] |>
                    select("conc_acsubst_total_soil_ini"),
                  crs("EPSG:32631")),
          project(budens_jrc_dk, crs("EPSG:32631")),
          field = "conc_acsubst_total_soil_ini",
          touches = T) |> 
            disagg(4) |> 
            zonal(project(pest_twc_as_basin_dk[[1]], crs("EPSG:32631")), fun = "median", touches = T, as.raster = T),
          
rasterize(project(pest_twc_as_basin_dk[[2]] |>
                    select("conc_acsubst_total_soil_ini"),
                  crs("EPSG:32631")),
          project(budens_jrc_dk, crs("EPSG:32631")),
          field = "conc_acsubst_total_soil_ini",
                                   touches = T) |>
  disagg(4) |> 
zonal(project(pest_twc_as_basin_dk[[2]], crs("EPSG:32631")), fun = "median", touches = T, as.raster = T),

rasterize(project(pest_twc_as_basin_dk[[3]] |>
                    select("conc_acsubst_total_soil_ini"),
                  crs("EPSG:32631")),
          project(budens_jrc_dk, crs("EPSG:32631")),
          field = "conc_acsubst_total_soil_ini",
          touches = T) |>
  disagg(4) |> 
  zonal(project(pest_twc_as_basin_dk[[3]], crs("EPSG:32631")) ,fun = "median", touches = T, as.raster = T))

# Aggregate the simulated concentration to the respective basin level
conc_wmean_basin_dk <- list()
area_farm_basin_dk <- list()
nr_farm_basin_dk <- list()
all_as_basin_dk <- list()

for(i in seq_along(pest_twc_as_basin_dk)){
  
  cat("\r", unique(values(pest_twc_as_basin_dk[[i]]["acsubst"]))[1,1],
      "is being processed out of",
      seq_along(pest_twc_as_basin_dk) |> max(), 
      "ASs.",
      max(seq_along(pest_twc_as_basin_dk)) - i,
      "ASs left.")
  
  conc_wmean_basin_dk[[i]] <- zonal(pest_twc_as_basin_dk[[i]][,"conc_acsubst_total_soil_ini"],
                                    basins_dk[c("HYBAS_ID")],
                                    weighted = T,
                                    as.polygons = T)
  
  area_farm_basin_dk[[i]] <- zonal(pest_twc_as_basin_dk[[i]][,"area_as_farms_bas_dk_ha"],
                                   basins_dk[c("HYBAS_ID", "SUB_AREA_ha")],
                                   fun = "sum",
                                   as.polygons = T) 
  
  nr_farm_basin_dk[[i]] <- zonal(pest_twc_as_basin_dk[[i]][,"nr_farms_as_crop"],
                                 basins_dk[c("HYBAS_ID")],
                                 fun = "sum",
                                 as.polygons = T)

  all_as_basin_dk[[i]] <- merge(conc_wmean_basin_dk[[i]], area_farm_basin_dk[[i]] |> as.data.table(), by = "HYBAS_ID") |> 
    merge(nr_farm_basin_dk[[i]] |> as.data.table(), by = "HYBAS_ID") |>
    # terra::na.omit("HYBAS_ID") |> 
    mutate("Active substance" = unique(values(pest_twc_as_basin_dk[[i]]["acsubst"]))[1,1],
           area_as_farms_bas_dk_ha = round(area_as_farms_bas_dk_ha, 1),
           HYBAS_ID = as.character(HYBAS_ID)) |> 
    tidyterra::rename("Basin ID" = HYBAS_ID,
                      "Basin area [ha]" = SUB_AREA_ha,
                      "Concentration (TWA) [\u00B5g\u00D7kg\u207B\u00B9]"  = conc_acsubst_total_soil_ini,
                      "Number of parcels" = nr_farms_as_crop,
                      "Agricultural area [ha]" = area_as_farms_bas_dk_ha)
}

writeCDF(pest_twc_as_farm_dk_rast[1], "Acetamiprid_pecini_topsoil_farm_dk.nc")
writeCDF(pest_twc_as_farm_dk_rast[2], "Glyphosate_pecini_topsoil_farm_dk.nc")
writeCDF(pest_twc_as_farm_dk_rast[3], "Tebuconazole_pecini_topsoil_farm_dk.nc")
writeVector(vect(all_as_basin_dk), "3chem_pecini_topsoil_basin_dk.gpkg", overwrite = F)

####################################################################
########### END: Pesticide Runoff Model Schriever 2007 #############
####################################################################

##########################################################
########### START: Pesticide topsoil Map #################
##########################################################

conc_soil_agg_rast_dk_1 <- dir_ls(path = path_home_r(), regexp = "Acetamiprid_pec365_topsoil_farm_dk.nc", recurse = T) |> rast()
conc_soil_agg_rast_dk_2 <- dir_ls(path = path_home_r(), regexp = "Glyphosate_pec365_topsoil_farm_dk.nc", recurse = T) |> rast()
conc_soil_agg_rast_dk_3 <- dir_ls(path = path_home_r(), regexp = "Tebuconazole_pec365_topsoil_farm_dk.nc", recurse = T) |> rast()
conc_soil_agg_rast_dk <- sprc(conc_soil_agg_rast_dk_1, conc_soil_agg_rast_dk_2, conc_soil_agg_rast_dk_3)
conc_soil_agg_vect_dk <- dir_ls(path = path_home_r(), regexp = "3chem_pec365_topsoil_basin_dk.gpkg", recurse = T) |> vect() |> split("Active.substance")

# PEC soil maps
pec_field_to_basin_dk <- function(conc_soil_agg_rast_dk, conc_soil_agg_vect_dk) {
  
  raster_values <- conc_soil_agg_rast_dk[!is.na(conc_soil_agg_rast_dk)]
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
  
  vector_values <- conc_soil_agg_vect_dk[,2] |> pull()
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
  
  pec_soil_dk <- leaflet(options = leafletOptions()) |> 
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
    addPolygons(data = basins_dk["HYBAS_ID"],
                weight = 0.5,
                opacity = 0.75,
                fillColor = "grey",
                fillOpacity = 0.1,
                color = "black",
                highlightOptions = highlightOptions(color = "black",
                                                    weight = 3,
                                                    bringToFront = TRUE),
                popup = paste0("<b>Catchment ID: ", basins_dk$HYBAS_ID ,"</b>"),
                options = pathOptions(pane = "vect_bas"),
                group = "River basins") |>
    addPolygons(data = districts_dk["LAU_NAME"],
                weight = 0.5,
                opacity = 0.75,
                fillColor = "grey",
                fillOpacity = 0.1,
                color = "black",
                highlightOptions = highlightOptions(color = "black",
                                                    weight = 3,
                                                    bringToFront = TRUE),
                popup = paste0("<b>District (LAU): ",districts_dk$LAU_NAME,"</b>"),
                options = pathOptions(pane = "vect_lau"),
                group = "Districts (LAU)") |>
    addRasterImage(conc_soil_agg_rast_dk,
                   colors = color_palette_rast,
                   opacity = 1,
                   group = "Field level concentration",
                   options = pathOptions(pane = "rast_conc")) |> 
    addLegend(pal = color_palette_rast_rev,
              values = values(conc_soil_agg_rast_dk),
              title = paste0(conc_soil_agg_vect_dk$Active.substance |> unique(),"\n",
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
    addPolygons(data = conc_soil_agg_vect_dk,
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
                               format_power10(conc_soil_agg_vect_dk[,2] |>
                                                pull(),
                                              digits = 3),
                               "</b>",
                               "<b><br>",
                               "# of treated fields: ",
                               conc_soil_agg_vect_dk[,5] |> pull(),
                               "</b><br>",
                               "<b>",
                               "Area of treated fields (ha): ",
                               conc_soil_agg_vect_dk[,4] |>
                                 pull(),
                               "</b>")) |> 
    addLegend(pal = color_palette_vect_rev,
              values = values(conc_soil_agg_vect_dk[,2]),
              title = paste0(conc_soil_agg_vect_dk$Active.substance |> unique(),"\n",
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
                             conc_soil_agg_vect_dk$Active.substance |> unique(),
                             " 365-day PEC soil after 1x application in July in Denmark", 
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
  
  pec_soil_dk |> saveWidget(paste0("DK_", conc_soil_agg_vect_dk$Active.substance |> unique(), "_PEC_365_Soil.html"))
  # return(pec_soil_dk)
}

pec_field_to_basin_dk(conc_soil_agg_rast_dk[1], conc_soil_agg_vect_dk[[1]])
pec_field_to_basin_dk(conc_soil_agg_rast_dk[2], conc_soil_agg_vect_dk[[2]])
pec_field_to_basin_dk(conc_soil_agg_rast_dk[3], conc_soil_agg_vect_dk[[3]])

###########################################################
########### END: Pesticide topsoil map ####################
###########################################################

#########################################################################
########### START: Pesticide concentration in surface water #############
#########################################################################

# Calculate the mean, min and max concentration of the active substance in the river segment
# Intersect stream buffer with fields
# Aggregate the data to the river segment level and then to the basin level

# Danish river network from the HYDROSHEDS database
rivers_dk <- dir_ls(path_home_r(), recurse = T, regexp = "hydrosheds_rivers_eu.gpkg") |>
  vect() |>
  terra::mask(nuts3_dk) |> 
  select("HYRIV_ID",
         "HYBAS_L12",
         "LENGTH_KM",
         "ria_ha_csu",
         "DIS_AV_CMS",
         "dis_m3_pyr",
         "dis_m3_pmn",
         "dis_m3_pmx",
         "ORD_FLOW",
         "sgr_dk_rav",
         "riv_tc_csu") |>
  tidyterra::rename(HYBAS_ID = HYBAS_L12) |> 
  tidyterra::mutate(WIDTH_M = (ria_ha_csu * 10000) / (LENGTH_KM *1000),
                    HYBAS_ID = as.character(HYBAS_ID))


# River buffers

# farm_rivers_buff_bas_dk <- list()
# rivers_buff_dk <- list()

# --- Start of Progress Bar Integration ---

# Initialize the progress bar
# min: starting value
# max: ending value (total number of iterations)
# style: 3 gives a percentage and elapsed time
# pb <- utils::txtProgressBar(min = 0, max = basins_dk |> length(), style = 3)

# --- End of Progress Bar Integration ---

# for(basin_idx in seq_along(basins_dk)) {
#   current_hybas_id <- basins_dk[basin_idx, 1] |> values() |> as.vector()
# 
#   current_rivers_buff <- rivers_dk |>
#     filter(HYBAS_ID == current_hybas_id) |>
#     buffer(100)
# 
#   current_farm_rivers_buff <- terra::intersect(
#     pest_twc_as_farm_dk[c("EC_trans_n", "acsubst", "srunoff_load", "dis_m3_pyr")],
#     current_rivers_buff
#   )
# 
#   if (nrow(current_farm_rivers_buff) == 0) {
# 
#     message("Warning: Basin ID ", as.character(current_hybas_id), " (Basin nr ", basin_idx, " out of ", basins_dk |> length(), ") has no fields intersecting the buffer. Skipping.")
#     next
#   }
# 
#   rivers_buff_dk[[basin_idx]] <- current_rivers_buff
#   farm_rivers_buff_bas_dk[[basin_idx]] <- current_farm_rivers_buff

# --- Progress Bar Update ---
# Update the progress bar to the current iteration number
# setTxtProgressBar(pb, basin_idx)
# --- End of Progress Bar Update ---

# }

# --- Close Progress Bar ---
# Close the progress bar when the loop is complete
# close(pb)

# cat("\nProcessing complete.\n")

# writeVector(farm_rivers_buff_bas_dk |> svc() |> vect(), "water_network_farmbuff100_12_DK.gpkg")
# writeVector(rivers_buff_dk |> svc() |> vect(), "rivers_buff100_dk.gpkg")

# Import intersected river network and parcel data 
rivers_farm_buff_dk <- dir_ls(path_home_r(), recurse = T, regexp = "water_network_farmbuff100_12_DK.gpkg") |> 
  vect()|> 
  mutate(HYBAS_ID = as.character(HYBAS_ID))

rivers_buff_dk <- dir_ls(path_home_r(), recurse = T, regexp = "rivers_buff100_dk.gpkg") |> 
  vect() |> 
  mutate(HYBAS_ID = as.character(HYBAS_ID))

rivers_dk_rast <- project(rivers_dk, empty_raster_dk) |> rasterize(empty_raster_dk)

# Sum total length of stream for each river basin
river_length_tot_bas_dk <- rivers_dk |> 
  values() |> 
  group_by(HYBAS_ID) |> 
  summarise(length_tot_bas_km = sum(LENGTH_KM))

# Calculate concentration and weighted concentrations in all individual streams. Split dataset for each pesticide
conc_acsubst_river_seg_dk <- rivers_farm_buff_dk |> 
  terra::merge(river_length_tot_bas_dk, by = "HYBAS_ID") |>
  mutate(segment_weight = LENGTH_KM/length_tot_bas_km,
    conc_river_seg_ug.dm3 = srunoff_load/(dis_m3_pyr_2),
         conc_river_seg_w_ug.dm3 = conc_river_seg_ug.dm3*segment_weight,
                    HYBAS_ID = as.character(HYBAS_ID)) |>
  terra::split("acsubst")

# Aggregate pesticide concentration in streams for each crop and river basin
conc_rivseg_agg_dk <- svc(aggregate(conc_acsubst_river_seg_dk[[1]][c("acsubst", "EC_trans_n",  "HYBAS_ID" , "conc_river_seg_w_ug.dm3")],
                                    c("HYBAS_ID", "EC_trans_n"),
                                    fun = "median") |> 
                            zonal(rivers_buff_dk, as.polygons = T, weighted = T),
                          aggregate(conc_acsubst_river_seg_dk[[2]][c("acsubst", "EC_trans_n",  "HYBAS_ID" ,  "conc_river_seg_w_ug.dm3")],
                                    c("HYBAS_ID", "EC_trans_n"),
                                    fun = "median") |> 
                            zonal(rivers_buff_dk, as.polygons = T, weighted = T),
                          aggregate(conc_acsubst_river_seg_dk[[3]][c("acsubst", "EC_trans_n",  "HYBAS_ID" , "conc_river_seg_w_ug.dm3")],
                                    c("HYBAS_ID", "EC_trans_n"),
                                    fun = "median") |> 
                            zonal(rivers_buff_dk, as.polygons = T, weighted = T))

# Rasterise aggregated concentration values in each river segment
conc_rivseg_agg_rast_dk <- sprc(rasterize(project(conc_rivseg_agg_dk[[1]],
                                                  crs("EPSG:32631")),
                                          empty_raster_dk,
                                          field = "median_conc_river_seg_w_ug.dm3",
                                          touches = T),
                                  rasterize(project(conc_rivseg_agg_dk[[2]],
                                                    crs("EPSG:32631")),
                                            empty_raster_dk,
                                            field = "median_conc_river_seg_w_ug.dm3",
                                            touches = T),
                                rasterize(project(conc_rivseg_agg_dk[[3]],
                                                  crs("EPSG:32631")),
                                          empty_raster_dk,
                                          field = "median_conc_river_seg_w_ug.dm3",
                                                   touches = T)) 

# Create vector layers of aggregated pesticide concentration in streams for each respective basin level
conc_wmean_river_basin_dk <- list()
lenght_river_buffer_basin_dk <- list()
nr_river_buffer_basin_dk <- list()
conc_rivseg_agg_vect_dk <- list()

for(i in seq_along(conc_acsubst_river_seg_dk)){
  
  cat("\r", unique(values(conc_acsubst_river_seg_dk[[i]]["acsubst"]))[1,1],
      "is being processed out of",
      seq_along(conc_acsubst_river_seg_dk) |> max(), 
      "ASs.",
      max(seq_along(conc_acsubst_river_seg_dk)) - i,
      "ASs left.")
  
  conc_wmean_river_basin_dk[[i]] <- zonal(conc_acsubst_river_seg_dk[[i]][,"conc_river_seg_w_ug.dm3"],
                                    basins_dk[c("HYBAS_ID")],
                                    weighted = T,
                                    as.polygons = T) 
  
  lenght_river_buffer_basin_dk[[i]] <- zonal(conc_acsubst_river_seg_dk[[i]]["LENGTH_KM"],
                                   basins_dk[c("HYBAS_ID")],
                                   fun = "sum",
                                   as.polygons = T) 
  
  nr_river_buffer_basin_dk[[i]] <- conc_acsubst_river_seg_dk[[i]] |>
    values() |> 
    group_by(HYBAS_ID) |>
    summarise(stream_seg_count = n())
  
  conc_rivseg_agg_vect_dk[[i]] <- merge(conc_wmean_river_basin_dk[[i]], lenght_river_buffer_basin_dk[[i]] |>
                                        as.data.table(), by = "HYBAS_ID") |> 
    left_join(nr_river_buffer_basin_dk[[i]], by = "HYBAS_ID") |>
    # terra::na.omit("HYBAS_ID") |> 
    mutate("Active substance" = unique(values(conc_acsubst_river_seg_dk[[i]]["acsubst"]))[1,1],
           LENGTH_KM = round(LENGTH_KM, 2)) |> 
    tidyterra::rename("Basin ID" = HYBAS_ID,
                      "Concentration [\u00B5g\u00D7dm\u207B\u00B3]" = conc_river_seg_w_ug.dm3,
                      "Total stream length receiving pesticide loadings [km]" = LENGTH_KM,
                      "Number of streams receiving pesticide loadings" = stream_seg_count)
}

# Save raster and vector data files
writeCDF(conc_rivseg_agg_rast_dk[1], "Acetamiprid_pec365_swater_seg_dk.nc",  overwrite = T)
writeCDF(conc_rivseg_agg_rast_dk[2], "Glyphosate_pec365_swater_seg_dk.nc",  overwrite = T)
writeCDF(conc_rivseg_agg_rast_dk[3], "Tebuconazole_pec365_swater_seg_dk.nc",  overwrite = T)
writeVector(vect(conc_rivseg_agg_vect_dk), "3chem_pec365_swater_seg_basin_dk.gpkg", overwrite = T)

#########################################################################
########### END: Pesticide concentration in surface water ###############
#########################################################################

###################################################################
########### START: Pesticide surface water map ####################
###################################################################

conc_rivseg_agg_rast_dk_1 <- dir_ls(path = path_home_r(), regexp = "Acetamiprid_pec365_swater_seg_dk.nc", recurse = T) |> rast()
conc_rivseg_agg_rast_dk_2 <- dir_ls(path = path_home_r(), regexp = "Glyphosate_pec365_swater_seg_dk.nc", recurse = T) |> rast()
conc_rivseg_agg_rast_dk_3 <- dir_ls(path = path_home_r(), regexp = "Tebuconazole_pec365_swater_seg_dk.nc", recurse = T) |> rast()
conc_rivseg_agg_rast_dk <- sprc(conc_rivseg_agg_rast_dk_1, conc_rivseg_agg_rast_dk_2, conc_rivseg_agg_rast_dk_3)
conc_rivseg_agg_vect_dk <- dir_ls(path = path_home_r(), regexp = "3chem_pec365_swater_seg_basin_dk.gpkg", recurse = T) |> vect() |> split("Active.substance")

# PEC surface water live maps

pec_streams_to_basin_dk <- function(conc_rivseg_agg_rast_dk, conc_rivseg_agg_vect_dk) {
  
  raster_values <- conc_rivseg_agg_rast_dk[!is.na(conc_rivseg_agg_rast_dk)]
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
  
  vector_values <- conc_rivseg_agg_vect_dk[,2] |> pull()
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
  
  
  pec_swater_dk <- leaflet(options = leafletOptions()) |> 
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
    addRasterImage(rivers_dk_rast,
                   color = "blue",
                   opacity = 0.5,
                   options = pathOptions(pane = "rast_riv"),
                   group = "River network") |> 
    addPolygons(data = basins_dk["HYBAS_ID"],
                weight = 0.5,
                opacity = 0.5,
                fillColor = "grey",
                fillOpacity = 0.25,
                color = "black",
                highlightOptions = highlightOptions(color = "black",
                                                    weight = 3,
                                                    bringToFront = TRUE),
                popup = paste0("<b>Catchment ID: ", basins_dk$HYBAS_ID ,"</b>"),
                options = pathOptions(pane = "vect_bas"),
                group = "River basins") |>
    addPolygons(data = districts_dk["LAU_NAME"],
                weight = 0.5,
                opacity = 0.5,
                fillColor = "grey",
                fillOpacity = 0.25,
                color = "black",
                highlightOptions = highlightOptions(color = "black",
                                                    weight = 3,
                                                    bringToFront = TRUE),
                popup = paste0("<b>District (LAU): ",districts_dk$LAU_NAME,"</b>"),
                options = pathOptions(pane = "vect_lau"),
                group = "Districts (LAU)") |>
    addRasterImage(conc_rivseg_agg_rast_dk,
                   colors = color_palette_rast,
                   opacity = 1,
                   group = "Stream level concentration",
                   options = pathOptions(pane = "rast_conc")) |> 
    addLegend(pal = color_palette_rast_rev,
              values = values(conc_rivseg_agg_rast_dk),
              title = paste0(conc_rivseg_agg_vect_dk$Active.substance |> unique(),
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
    addPolygons(data = conc_rivseg_agg_vect_dk,
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
                               format_power10(conc_rivseg_agg_vect_dk[,2] |>
                                                pull(),
                                              digits = 3),
                               "<br></b>",
                               "<b># of streams receiving pesticides: ",
                               conc_rivseg_agg_vect_dk[,4] |>
                                 pull(), 
                               "<br></b>",
                               "<b>Total length of streams receiving pesticides (km): ",
                               conc_rivseg_agg_vect_dk[,3] |>
                                 pull(),
                               "</b>")) |> 
    addLegend(pal = color_palette_vect_rev,
              values = values(conc_rivseg_agg_vect_dk[,2]),
              title = paste0(conc_rivseg_agg_vect_dk$Active.substance |> unique(),
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
    addControl(html = paste0("<div style='background-color: rgba(255, 255, 255, 0.9);
                             padding: 6px 6px; border-radius: 4px; font-size: 14px; font-weight: bold; color: #333; max-width: 800px;
                             line-height: 1.4;'>",
                             conc_rivseg_agg_vect_dk$Active.substance |> unique(),
                             " annual PEC surface water after 1x application in July in Denmark"
    ),
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
    }
  ")
  
  pec_swater_dk |> saveWidget(paste0("DK_", conc_rivseg_agg_vect_dk$Active.substance |> unique(), "_PEC_365_Water.html"))
  # return(pec_swater_dk)
}

pec_streams_to_basin_dk(conc_rivseg_agg_rast_dk[1], conc_rivseg_agg_vect_dk[[1]])
pec_streams_to_basin_dk(conc_rivseg_agg_rast_dk[2], conc_rivseg_agg_vect_dk[[2]])
pec_streams_to_basin_dk(conc_rivseg_agg_rast_dk[3], conc_rivseg_agg_vect_dk[[3]])

#################################################################
########### END: Pesticide surface water map ####################
#################################################################

