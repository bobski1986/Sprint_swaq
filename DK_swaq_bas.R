pkg <- c("tidyverse", "fs", "readxl", "terra", "tmap", "OpenStreetMap", "tidyterra", "sf", "data.table", "fuzzyjoin", "openxlsx")

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

# Read soil density map
budens_jrc_dk_path <- dir_ls(path_home_r(),
                             recurse = T,
                             regexp = "budens_jrc_dk.nc$")

budens_jrc_dk <- rast(budens_jrc_dk_path)

# Danish river basins including data on soil, basin and terrain characteristics
basins_dk <- dir_ls(path_home_r(), recurse = T, regexp = "hydrosheds_lvl10_basins_dk.gpkg") |>
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
  mask(nuts3_dk)

budens_jrc_dk_bas <- zonal(budens_jrc_dk, basins_dk, "mean", as.polygons = T, na.rm = T) |> 
  select(HYBAS_ID, budens_jrc_DK)

basins_dk <-  basins_dk |> 
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
                         select("crop_code_eppo", "inter_frac_bbch.ave"), by.x = "CROP_EPPO_CODE", by.y = "crop_code_eppo") |> 
  makeValid()

acsubst_name <- gemap_dk["Active"] |> values() |> unique() |> pull()

# Chemical input data from qsars (vega, epi) and PPDB where available #
source(dir_ls(path_home_r(), recurse = T, regexp = "ppdb scraping"))
chemprop <- chemprop_gen(acsubst_name) |> 
  select(acsubst_name, Kfoc_ml.g, DT50_field_d, DT50_typical_d, Koc_ml.g) 
  # filter(!is.na(Kfoc_ml.g),
  #        !is.na(Koc_ml.g),
  #        !is.na(DT50_field_d),
  #        !is.na(DT50_typical_d))

# Intersect danish gemap with river basins
gemap_dk_bas <- terra::intersect(gemap_dk, basins_dk["HYBAS_ID"])

# Get the number of all farms, grouped by crop and AS, in each river basin
mass_as_farms_bas_dk <- terra::aggregate(gemap_dk_bas[c("HYBAS_ID","Active", "EC_trans_n", "App_dose_kg.ha")],
                                         c("HYBAS_ID", "EC_trans_n", "Active"),
                                         fun = "sum")

area_as_farms_bas_dk <- mass_as_farms_bas_dk |> expanse("ha") 

gemap_dk_bas_if <- gemap_dk_bas[c("HYBAS_ID",
                                  "EC_trans_n",
                                  "inter_frac_bbch.ave")] |>
  terra::aggregate(c("HYBAS_ID", "EC_trans_n"),
                   fun = "mean")

mass_area_as_farms_bas_dk <- cbind(mass_as_farms_bas_dk, area_as_farms_bas_dk |>
  data.frame()) |>
  terra::merge(basins_dk, by = "HYBAS_ID") |> 
  terra::merge(gemap_dk_bas_if[c("HYBAS_ID",
                                 "EC_trans_n",
                                 "mean_inter_frac_bbch.ave")],
               by = c("HYBAS_ID", "EC_trans_n")) |> 
  tidyterra::rename(area_as_farms_bas_dk_ha = area_as_farms_bas_dk,
                    SUB_AREA_km2 = SUB_AREA,
                    nr_farms_as_crop = agg_n) |>
  tidyterra::mutate(area_as_farms_bas_frac = area_as_farms_bas_dk_ha / area_basins_dk_ha,
                    AppRate_g_ha_corr = (sum_App_dose_kg.ha/area_as_farms_bas_dk) * area_as_farms_bas_frac)

# Intersect danish gemap with river network

# Danish river network from the HYDROSHEDS database
rivers_nl <- dir_ls(path_home_r(), recurse = T, regexp = "hydrosheds_lvl12_rivers_dk.gpkg") |>
  vect() |>
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
  tidyterra::mutate(WIDTH_M = (ria_ha_csu * 10000) / (LENGTH_KM *1000)) |>
  tidyterra::rename(HYBAS_ID = HYBAS_L12) |>
  terra::mask(basins_nl)

####################################################################
########### START: Pesticide Runoff Model Schriever 2007 ###########
####################################################################

# TWA concentration in soil
pest_twc_as_farm_dk <- mass_area_as_farms_bas_dk |>
  rename(acsubst_name = Active,
         mean_IFav = mean_inter_frac_bbch.ave,
         aprate_farm_kg.ha = sum_App_dose_kg.ha,
         bulk_dens_kg.dm3 = budens_jrc_DK) |> 
  terra::merge(chemprop, by = "acsubst_name") |>
  ## Effect of crop interception factor on runoff
  mutate(infactor_effect = map_dbl(mean_IFav,
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
         conc_acsubst_total_soil_365twa_ug.kg = (conc_acsubst_total_soil_ini / (365 * (log(2) / DT50_typical_d))) * (1 - exp(-365 * (log(2) / DT50_typical_d))))

# Calculate the mean, min and max concentration of the active substance in the river segment
# Aggregate the data to the river segment level and then to the basin level

# pest_twc_as_farm_river_nl <- pest_twc_as_farm_nl |> intersect(rivers_bas_buff_nl) |> 
#   group_by(HYBAS_ID, HYRIV_ID, LENGTH_KM, DIS_AV_CMS, dis_m3_pyr, dis_m3_pmn, dis_m3_pmx) |>
# conc_acsubst_river_seg_mapinput <- load_acsubst_farm_mapinput |>
#   makeValid() |>
#   terra::intersect(rivers_basin_buff_seg) |>
#   group_by(SHAPE_Leng, HYDROID, acsubst, month, ndays, dis_m3_pyr, dis_m3_pmn, dis_m3_pmx) |>
#   summarise(conc_mean_river_seg = mean(load_acsubst_mean_g.ndays/dis_m3_pyr),
#             conc_min_river_seg = mean(load_acsubst_min_g.ndays/dis_m3_pmn),
#             conc_max_river_seg = mean(load_acsubst_max_g.ndays/dis_m3_pmx))

# pest_twc_as_farm_river_nl |> names()
# 
# plot(rivers_bas_nl, col = "blue")
# plot(rivers_bas_buff_nl, border = "lightblue", lwd = 0.25, add = T)
# plot(pest_twc_as_farm_river_nl, "conc_acsubst_total_soil_365twa_ug.kg")

# Split simulated concentration by substance 
pest_twc_as_basin_dk <- pest_twc_as_farm_dk[c("HYBAS_ID",
                                              "EC_trans_n",
                                              "acsubst_name",
                                              "nr_farms_as_crop",
                                              "area_as_farms_bas_dk_ha",
                                              "conc_acsubst_total_soil_ini",
                                              "conc_acsubst_total_soil_56twa_ug.kg",
                                              "conc_acsubst_total_soil_365twa_ug.kg")] |>
  tidyterra::mutate(HYBAS_ID = as.character(HYBAS_ID)) |> 
  terra::split("acsubst_name")

# Rasterise individual parcel polygons to show AS concentration distribution in individual basins
pest_twc_as_farm_dk_rast <- sprc(rasterize(project(pest_twc_as_basin_dk[[1]] |>
                    select("conc_acsubst_total_soil_365twa_ug.kg"),
                  crs("EPSG:32631")),
          project(budens_jrc_dk, crs("EPSG:32631")),
          field = "conc_acsubst_total_soil_365twa_ug.kg",
          touches = T) |> 
            disagg(4) |> 
            zonal(project(pest_twc_as_basin_dk[[1]], crs("EPSG:32631")), fun = "median", touches = T, as.raster = T),
          
rasterize(project(pest_twc_as_basin_dk[[2]] |>
                    select("conc_acsubst_total_soil_365twa_ug.kg"),
                  crs("EPSG:32631")),
          project(budens_jrc_dk, crs("EPSG:32631")),
          field = "conc_acsubst_total_soil_365twa_ug.kg",
                                   touches = T) |>
  disagg(4) |> 
zonal(project(pest_twc_as_basin_dk[[2]], crs("EPSG:32631")), fun = "median", touches = T, as.raster = T),

rasterize(project(pest_twc_as_basin_dk[[3]] |>
                    select("conc_acsubst_total_soil_365twa_ug.kg"),
                  crs("EPSG:32631")),
          project(budens_jrc_dk, crs("EPSG:32631")),
          field = "conc_acsubst_total_soil_365twa_ug.kg",
          touches = T) |>
  disagg(4) |> 
  zonal(project(pest_twc_as_basin_dk[[3]], crs("EPSG:32631")) ,fun = "median", touches = T, as.raster = T))

# Aggregate the simulated concentration to the respective basin level
conc_wmean_basin_dk <- list()
area_farm_basin_dk <- list()
nr_farm_basin_dk <- list()
all_as_basin_dk <- list()

for(i in seq_along(pest_twc_as_basin_dk)){
  
  cat("\r", unique(values(pest_twc_as_basin_dk[[i]]["acsubst_name"]))[1,1],
      "is being processed out of",
      seq_along(pest_twc_as_basin_dk) |> max(), 
      "ASs.",
      max(seq_along(pest_twc_as_basin_dk)) - i,
      "ASs left.")
  
  conc_wmean_basin_dk[[i]] <- zonal(pest_twc_as_basin_dk[[i]][,"conc_acsubst_total_soil_365twa_ug.kg"],
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
    mutate("Active substance" = unique(values(pest_twc_as_basin_dk[[i]]["acsubst_name"]))[1,1],
           area_as_farms_bas_dk_ha = round(area_as_farms_bas_dk_ha, 1),
           conc_acsubst_total_soil_365twa_ug.kg = round(conc_acsubst_total_soil_365twa_ug.kg, 1),
           HYBAS_ID = as.character(HYBAS_ID)) |> 
    tidyterra::rename("Basin ID" = HYBAS_ID,
                      "Basin area [ha]" = SUB_AREA_ha,
                      "Concentration (TWA) [\u00B5g\u00D7kg\u207B\u00B9]"  = conc_acsubst_total_soil_365twa_ug.kg,
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
########### START: Pesticide Runoff Map ##################
##########################################################

# PEC soil maps
tmap_mode("view")

#PEC topsoil 
pec_field_to_basin_dk <- function(pest_twc_as_farm_dk_rast, all_as_basin_dk) {

  basemap <- tm_scalebar(position = c("right", "bottom")) +
    tm_basemap("Esri.WorldTopoMap", alpha = 0.5, group.control = "check") +
    tm_title(paste0(all_as_basin_dk$Active.substance |> unique(), 
                    " 365-day PEC topsoil in Denmark"))
  
  pec_soil_dk <- basemap +
    tm_shape(pest_twc_as_farm_dk_rast,
             name =paste0("PEC topsoil ",all_as_basin_dk$Active.substance |> unique())) +
    tm_raster("conc_acsubst_total_soil_365twa_ug.kg",
              col.scale = tm_scale_intervals(label.na = "Missing values",
                                             values = "brewer.bu_pu"),
              col.legend = tm_legend(title = paste0(all_as_basin_dk$Active.substance |> unique(),
                                                    " [\u00B5g\u00D7kg\u207B\u00B9]"),
                                     bg.alpha = 1,
                                     reverse = T),
              col_alpha = 1,
              group = "Field level",
              group.control = "radio") +
    tm_shape(all_as_basin_dk |> mask(nuts_dk),
             name =paste0("PEC topsoil ",all_as_basin_dk$Active.substance |> unique())) +
    tm_polygons("Concentration.(TWA).[µg×kg⁻¹]",
                popup.vars = c("Concentration.(TWA).[µg×kg⁻¹]",
                               "Number.of.parcels",
                               "Agricultural.area.[ha]",
                               "Basin.area.[ha]",
                               "Basin.ID"),
                fill.scale = tm_scale_intervals(value.na = "#ffdeaf" ,
                                                label.na = "Missing values",
                                                values = "brewer.bu_pu"),
                fill.legend = tm_legend(title = paste0(all_as_basin_dk$Active.substance |> unique(),
                                                       " [\u00B5g\u00D7kg\u207B\u00B9]"),
                                        bg.alpha = 1,
                                        reverse = T),
                group = "River basin level",
                group.control = "radio",
                lwd = 0,
                fill_alpha = 1) +
    tm_shape(all_as_basin_dk[,1], name = "River basin borders") +
    tm_lines(col= "black",
             lwd = 0.75) +
    tm_view(control.collapse = F)

  # Save the map
  tmap_save(pec_soil_dk, paste0(all_as_basin_dk$Active.substance |> unique(),"_pec365_topsoil_whole_DK.html"))
  
}

pec_field_to_basin_dk(pest_twc_as_farm_dk_rast[1], all_as_basin_dk[[1]])
pec_field_to_basin_dk(pest_twc_as_farm_dk_rast[2], all_as_basin_dk[[2]])
pec_field_to_basin_dk(pest_twc_as_farm_dk_rast[3], all_as_basin_dk[[3]])



# Spatial input overview
tmap_mode("plot")
nl_soil_water_input <- tm_shape(basins_dk) +
  tm_polygons(fill = basins_dk |>
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
tmap_save(pec_soil_dk, "pec_soil_dk.html")

########################################################
########### END: Pesticide Runoff Map ##################
########################################################
