library(tidyverse)
library(fs)
library(terra)

#########################################################
########### START: Model results evaluation #############
#########################################################

# Sampling coordinates needed for model evaluation. It can be also used to select LAUs where sampling took place #
# CZ sampling site cooridnates
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
# DK sampling site cooridnates
# site_id_dk <- c("F01", "F02", "F03", "F04", "F05", "F06", "F07", "F08", "F09", "F10",
#                 "F11", "F12", "F13", "F14", "F15", "F16", "F17", "F18", "F19", "F20")
# site_id_lat_dk <- c()
# site_id_lon_dk <- c()


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
