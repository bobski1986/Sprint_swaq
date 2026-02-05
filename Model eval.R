library(tidyverse)
library(fs)
library(sf)
library(terra)
library(tmap)
library(readxl)


#########################################################
########### START: Model results evaluation #############
#########################################################

# CZ sampling site coordinates
lau_degurba_cz <- dir_ls(path_home_r(), recurse = T, regexp = "/LAU_RG_01M_2024_4326.gpkg") |>
  vect() |> 
  tidyterra::filter(CNTR_CODE == "CZ")

# Add PEC and MEC soil data collected by Vera and Knuth et al. 2024
soil_mec_cz <- read_excel(dir_ls(path_home_r(), regexp = "Knuth soil sampling", recurse = T), range = "A1:GO65") |> 
  mutate(across(6:197, ~as.numeric(.))) |> 
  rename(Sample_code = `SPRINT Sample code`) |> 
  pivot_longer(cols = -c(Sample_code, Country, Management, Region, Crop),
               names_to = "Active",
               values_to = "MEC_µg.kg") |> 
  filter(!is.na(MEC_µg.kg), Country == "CZ") 

# Evaluation dataset
soil_pec_cz_path <- dir_ls(path_home_r(), regexp = "PEC_Soil_CZ.csv", recurse = T)
soil_pec_cz <- map(soil_pec_cz_path, read_csv) |>
  bind_rows() |>
  rename(Active = acsubst)|>
  filter(str_detect(Crop, "rape"))

# Sampling coordinates needed for model evaluation. It can be also used to select LAUs where sampling took place #
site_id_cz <- c("CZ_EF01_S_P", "CZ_EF02_S_P", "CZ_EF03_S_P", "CZ_EF04_S_P", "CZ_EF05_S_P", "CZ_EF06_S_P", "CZ_EF07_S_P", "CZ_EF08_S_P", "CZ_EF09_S_P", "CZ_EF10_S_P", "CZ_EF11_S_P", "CZ_EF12_S_P", "CZ_EF13_S_P", "CZ_EF14_S_P", "CZ_EF15_S_P", "CZ_EF16_S_P", "CZ_EF17_S_P", "CZ_EF18_S_P", "CZ_EF19_S_P", "CZ_EF20_S_P", "CZ_EF21_S_P", "CZ_EF22_S_P", "CZ_EF23_S_P", "CZ_EF24_S_P")
site_id_lat_cz <- c(48.75428, 48.7651997, 49.5882283, 48.9971858, 48.8974494, 48.9203217, 49.5573031, 49.7089406, 49.7190786, 50.5149308, 50.5392117, 50.4087597, 49.0598786, 49.0573903, 50.1286728, 50.2487525, 50.22116, 49.7082947, 48.9869569, 49.6619561, 49.7321864, 48.8276081, 48.8326775, 48.7364833)
site_id_lon_cz <- c(16.9401214, 16.9324394, 17.1860597, 16.9157722, 16.0395308, 15.9784944, 13.9029903, 14.8711725, 14.8851656, 16.196935, 16.1935875, 14.1494492, 15.4676047, 15.4676261, 16.2425311, 17.6263658, 15.9933244, 14.7895919, 17.0273617, 12.9795581, 12.9605036, 16.5171839, 16.4857483, 16.9331583)

site_coord_cz <- cbind(tibble(
  site_id = site_id_cz,
  long = site_id_lon_cz,
  lat = site_id_lat_cz)) |>
  sf::st_as_sf(coords = c("long", "lat"), crs = st_crs(lau_degurba_cz)) |>
  vect() |>
  terra::intersect(lau_degurba_cz)

lau_sample_cz <- lau_degurba_cz |> filter(LAU_NAME %in% site_coord_cz$LAU_NAME) |> distinct(LAU_NAME)

eval_data_pec <- soil_pec_cz |>
  group_by(LAU_NAME, Active) |> 
  summarise(soil_conc_ini_med = mean(conc_acsubst_total_soil_ini_ug.kg),
            soil_conc_ini_sd = sd(conc_acsubst_total_soil_ini_ug.kg),
            soil_conc_56d_med = mean(conc_acsubst_total_soil_56twa_ug.kg),
            soil_conc_56d_sd = sd(conc_acsubst_total_soil_56twa_ug.kg),
            nr_field = n()) |>
  bind_cols("Concentration units" = "µg × kg⁻¹") |> 
  rename("Location" = LAU_NAME,
         "Active substance" = Active,
         "Concentration intial" = soil_conc_ini_med,
         "Concentration intial SD" = soil_conc_ini_sd,
         "Concentration 56 days" = soil_conc_56d_med,
         "Concentration 56 days SD" = soil_conc_56d_sd,
         "# of fields" = nr_field)

eval_data_mec <- soil_mec_cz |>
  left_join(site_coord_cz |> values(), by = join_by("Sample_code" == "site_id")) |> 
  group_by(LAU_NAME, Active, Sample_code, Country, Management) |> 
  summarise(soil_conc_sample = max(MEC_µg.kg),
            nr_samples = n()) |>
  bind_cols("Concentration units" = "µg × kg⁻¹") |> 
  rename("Location" = LAU_NAME,
         "Concentration sample" = soil_conc_sample,
         "Active substance" = Active,
         "# of fields" = nr_samples)


# NL sampling site coordinates
lau_degurba_nl <- dir_ls(path_home_r(), recurse = T, regexp = "/LAU_RG_01M_2024_4326.gpkg") |>
  vect() |> 
  tidyterra::filter(CNTR_CODE == "NL")

# Add PEC and MEC soil data collected by Vera and Knuth et al. 2024
soil_mec_nl <- read_excel(dir_ls(path_home_r(), regexp = "Knuth soil sampling", recurse = T), range = "A1:GO65") |> 
  mutate(across(6:197, ~as.numeric(.))) |> 
  rename(Sample_code = `SPRINT Sample code`) |> 
  pivot_longer(cols = -c(Sample_code, Country, Management, Region, Crop),
               names_to = "Active",
               values_to = "MEC_µg.kg") |> 
  filter(!is.na(MEC_µg.kg), Country == "NL")

# Evaluation dataset
soil_pec_nl_path <- dir_ls(path_home_r(), regexp = "PEC_Soil_NL.csv", recurse = T)
soil_pec_nl <- map(soil_pec_nl_path, read_csv) |>
  bind_rows() |>
  rename(Active = acsubst) |> 
  filter(str_detect(Crop, "Potat"))

# Sampling coordinates needed for model evaluation. It can be also used to select LAUs where sampling took place #
site_id_nl <- c("NL_EF01_S_P", "NL_EF02_S_P", "NL_EF03_S_P", "NL_EF04_S_P", "NL_EF05_S_P", "NL_EF06_S_P", "NL_EF07_S_P", "NL_EF08_S_P", "NL_EF09_S_P", "NL_EF10_S_P", "NL_EF11_S_P", "NL_EF12_S_P", "NL_EF13_S_P", "NL_EF14_S_P", "NL_EF15_S_P", "NL_EF16_S_P")
site_id_lat_nl <- c(53.31, 53.32, 53.39, 53.38, 53.22, 53.4, 53.36, 53.39, 53.33, 53.28, 53.42, 53.35, 53.43, 53.38, 53.19, 53.32)
site_id_lon_nl <- c(6.27, 6.13, 6.37, 6.33, 5.47, 6.42, 6.41, 6.03, 6.32, 6.26, 6.6, 6.46, 6.63, 6.34, 5.46, 6.33)

site_coord_nl <- cbind(tibble(
site_id = site_id_nl,
long = site_id_lon_nl,
lat = site_id_lat_nl)) |> 
  sf::st_as_sf(coords = c("long", "lat"), crs = st_crs(lau_degurba_nl)) |>
  vect() |>
  terra::intersect(lau_degurba_nl)

lau_sample_nl <- lau_degurba_nl |> filter(LAU_NAME %in% site_coord_nl$LAU_NAME) |> distinct(LAU_NAME)

eval_data_pec_nl <- soil_pec_nl |>
  group_by(LAU_NAME, Active) |> 
  summarise(soil_conc_ini_med = mean(conc_acsubst_total_soil_ini_ug.kg),
            soil_conc_ini_sd = sd(conc_acsubst_total_soil_ini_ug.kg),
            soil_conc_56d_med = mean(conc_acsubst_total_soil_56twa_ug.kg),
            soil_conc_56d_sd = sd(conc_acsubst_total_soil_56twa_ug.kg),
            nr_field = n()) |>
  bind_cols("Concentration units" = "µg × kg⁻¹") |> 
  rename("Location" = LAU_NAME,
         "Active substance" = Active,
         "Concentration intial" = soil_conc_ini_med,
         "Concentration intial SD" = soil_conc_ini_sd,
         "Concentration 56 days" = soil_conc_56d_med,
         "Concentration 56 days SD" = soil_conc_56d_sd,
         "# of fields" = nr_field)

eval_data_mec_nl <- soil_mec_nl |>
  left_join(site_coord_nl |> values(), by = join_by("Sample_code" == "site_id")) |> 
  group_by(LAU_NAME, Active, Sample_code, Country, Management) |> 
  summarise(soil_conc_sample = max(MEC_µg.kg),
            nr_samples = n()) |>
  bind_cols("Concentration units" = "µg × kg⁻¹") |> 
  rename("Location" = LAU_NAME,
         "Concentration sample" = soil_conc_sample,
         "Active substance" = Active,
         "# of fields" = nr_samples)

eval_data_df_nl <- left_join(eval_data_pec_nl, eval_data_mec_nl, by = c("Active substance", "Location", "Concentration units")) |> na.omit()
write_excel_csv(eval_data_df_nl, "NL soil concentration comparison.csv")

# DK sampling site coordinates
lau_degurba_dk <- dir_ls(path_home_r(), recurse = T, regexp = "/LAU_RG_01M_2024_4326.gpkg") |>
  vect() |> 
  filter(CNTR_CODE == "DK")

# Sampling coordinates needed for model evaluation. It can be also used to select LAUs where sampling took place #
site_id_dk <- c("F01", "F02", "F03", "F04", "F05", "F06", "F07", "F08", "F09", "F10", "F11", "F12", "F13", "F14", "F15", "F16", "F17", "F18", "F19", "F20")
site_id_lat_dk <- c(57, 57, 56.5, 56.51, 56.43, 56.44, 56.95, 56.97, 56.57, 56.57, 56.34, 56.35, 56.81, 56.81, 56.2, 56.23, 56.75, 56.78, 56.53,56.53)
site_id_lon_dk <- c(8.75, 8.73, 8.17, 8.28, 8.98, 8.94, 10.12, 10.16, 8.21, 8.19, 10.35, 10.37, 9.35, 9.35, 9.39, 9.4, 9.96, 9.89, 8.19, 8.21)

site_coord_dk <- cbind(tibble(
  site_id = site_id_dk,
  long = site_id_lon_dk,
  lat = site_id_lat_dk)) |>
  mutate(site_id = paste0(site_id, "_dk")) |> 
  sf::st_as_sf(coords = c("long", "lat"), crs = st_crs(lau_degurba_dk)) |>
  vect() |>
  terra::intersect(lau_degurba_dk)

lau_sample_dk <- lau_degurba_dk |> filter(LAU_NAME %in% site_coord_dk$LAU_NAME) |> distinct(LAU_NAME)

# Sample site map
sampling_sites_map <- function(lau, site_coord){
  
  tmap_mode("plot")
  basemap_lau <- tm_shape(lau) +
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
    tm_shape(lau) +
    tm_borders(lwd = 0.15) +
    tm_shape(lau |> filter(LAU_NAME %in% site_coord$LAU_NAME)) +
    tm_polygons(fill = "LAU_NAME",
                fill.scale = tm_scale(values = "poly.alphabet2"),
                fill.legend = tm_legend("LAU (2024)", frame = F)) +
    tm_shape(site_coord["site_id"]) +
    tm_symbols(fill = "magenta",
               size = .35,) +
    tm_add_legend(type = "symbols",
                  fill = "magenta",
                  labels = "Sampling site",
                  frame = F) + 
    tm_title(paste0("Soil sampling locations (# of sites ", site_coord$site_id |> unique() |> length(), ")"))
  
}

sampling_sites_map(lau_degurba_cz, site_coord_cz)


# gemup_cz <- dir_ls(path_home_r(), recurse = T, regexp = "gemup100") |> 
#   vect(extent = ext(lau_sample_cz[1])) |>
#   filter(Active %in% c("acetamiprid", "tebuconazole", "glyphosate")) |> 
#   filter(str_detect(Crop,  regex("rape"))) |> 
#   mutate(acsubst = Active |> str_to_title()) |> 
#   mask(lau_degurba_cz |> filter(LAU_NAME %in% lau_sample_cz$LAU_NAME))

# gemup_nl <- dir_ls(path_home_r(), recurse = T, regexp = "gemap_slim_nl") |> 
#   vect(extent = ext(lau_sample_nl[1])) |> 
#   filter(EU_name %in% c("Acetamiprid", "Tebuconazole", "Glyphosate")) |> 
#   filter(str_detect(EC_hcat_n,  regex("pota"))) |> 
#   mutate(acsubst = EU_name) |> 
#   mask(lau_degurba_nl |> filter(LAU_NAME %in% lau_sample_nl$LAU_NAME))
# 
# gemup_dk <- dir_ls(path_home_r(), recurse = T, regexp = "gemap_3chem_dk") |> 
#   vect(extent = ext(lau_sample_dk[1])) |>
#   mutate(Active = case_when(
#     Active == "acetamiprid" ~ "Acetamiprid",
#     Active == "glyphosat" ~ "Glyphosate",
#     Active == "tebuconazol" ~ "Tebuconazole")) |> 
# filter(EC_trans_n %in% c("Winter wheat", "Spring barley", "Spring oats","Green grain of spring oats", "Winter rye", "Winter triticale", "Spring barley wholecrop", "Spring wheat")) |> 
#   mask(lau_degurba_dk |> filter(LAU_NAME %in% lau_sample_dk$LAU_NAME))

# Read the data
soil_conc <- dir_ls(path_home_r(), recurse = T, regexp = "NL soil concentration comparison.csv") |> read_csv()

# Prepare data for plotting
soil_conc_long <- soil_conc %>%
  # Create location label with number of fields
  mutate(Location_label = paste0(Location, "\n(n=", X..of.fields.x, ")")) %>%
  # Reshape data to long format
  pivot_longer(
    cols = c(Concentration.sample, Concentration.intial, Concentration.56.days),
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
      Measurement_type == "Concentration.sample" ~ "Sample (measured)",
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


# BAR PLOT
as_soil.plt <- function(soil_conc_long, as, Location_label, Concentration, Measurement_type, Country){
  
  unique_locations <- soil_conc_long %>%
    filter(Active.substance == as) %>%
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
    
  p <- soil_conc_long %>%
    filter(Active.substance == as) %>%
    ggplot(aes(x = Location_label, y = Concentration, fill = Measurement_type)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
    geom_errorbar(aes(ymin = Concentration - SD, ymax = Concentration + SD),
                  position = position_dodge(width = 0.9),
                  width = 0.25,
                  na.rm = TRUE) +
    # Add vertical dashed lines
    geom_vline(data = vline_data, 
               aes(xintercept = xintercept), 
               linetype = "dashed", 
               color = "gray50", 
               linewidth = 0.5) +
    # facet_wrap(~ Active.substance, scales = "free_y", ncol = 1) +
    scale_fill_manual(values = c("Sample (measured)" = "#E69F00", 
                                 "Initial (modeled)" = "#56B4E9", 
                                 "56-day TWA (modeled)" = "#009E73")) +
    labs(
      title = paste0(as, ": Comparison of Measured and Modeled Concentrations in Soil in ", Country),
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
  
  print(p)
  
}

as_soil.plt(soil_conc_long = soil_conc_long,
               as = as_soil_unique,
               Location_label = soil_conc_long_cz$Location_label,
               Concentration = soil_conc_long_cz$Concentration,
               Measurement_type = soil_conc_long_cz$Measurement_type,
               Country = soil_conc_long$Country)

# Scatter Plots

# IMPORTANT: These colors are fixed to ensure consistency across all plots
# Each substance always gets the same color regardless of which dataset it appears in
# Colors are organized by pesticide type for better interpretability

substance_colors <- c(
  # Fungicides (reds, oranges, purples, teals)
  'Azoxystrobin' = '#E41A1C',      # Red
  'Dimoxystrobin' = '#999999',     # Gray
  'Difenoconazole' = '#984EA3',    # Purple
  'Fluopicolide' = '#FF7F00',      # Orange
  'Fluopyram' = '#8DA0CB',         # Light Blue
  'Mandipropamid' = '#FFFF33',     # Yellow
  'Tebuconazole' = '#66C2A5',      # Teal
  
  # Herbicides (blues, greens, browns)
  'Glyphosate' = '#377EB8',        # Blue
  'Metobromuron' = '#A65628',      # Brown
  'Pendimethalin' = '#FC8D62',     # Salmon
  'Prosulfocarb' = '#4DAF4A',      # Green
  
  # Insecticides (pinks)
  'Deltamethrin' = '#F781BF'       # Pink
)

# ============================================================================
# Define markers (pch values) for locations
# ============================================================================
location_markers <- c(
  'Harlingen' = 16,          # filled circle
  'Het Hogeland' = 15,       # filled square
  'Westerkwartier' = 17,     # filled triangle up
  'Kostice' = 25,            # filled triangle down
  'Kravsko' = 18,            # filled diamond
  'Krumvíř' = 11,            # star
  'Olomouc' = 8,             # asterisk
  'Postupice' = 23,          # filled diamond (alternative)
  'Velké Petrovice' = 22,    # filled square (alternative)
  'Vlašim' = 4,              # cross
  'Vševily' = 3,             # plus
  'Znojmo' = 60,             # left triangle
  'Česká Metuje' = 62        # right triangle
)

# ============================================================================
# Function to create scatterplot for a specific timepoint
# ============================================================================
create_scatterplot_timepoint <- function(data, country_name, output_prefix, timepoint) {
  
  # Prepare data based on timepoint
  if (timepoint == "initial") {
    data <- data %>%
      mutate(
        Model_concentration = `Concentration intial`,
        Model_SD = `Concentration intial SD`,
        Timepoint_label = "Initial Concentrations"
      )
    file_suffix <- "initial"
  } else {  # 56days
    data <- data %>%
      mutate(
        Model_concentration = `Concentration 56 days`,
        Model_SD = `Concentration 56 days SD`,
        Timepoint_label = "56-Day Concentrations"
      )
    file_suffix <- "56day"
  }
  
  # Calculate plot limits
  all_values <- c(data$`Concentration sample`, data$Model_concentration)
  all_values <- all_values[all_values > 0]
  min_val <- min(all_values)
  max_val <- max(all_values)
  plot_min <- min_val * 0.5
  plot_max <- max_val * 1.5
  
  # Get unique substances and locations
  unique_substances <- sort(unique(data$`Active substance`))
  unique_locations <- sort(unique(data$Location))
  
  # Assign colors and markers to data
  data <- data %>%
    mutate(
      Color = substance_colors[`Active substance`],
      Marker = location_markers[Location]
    )
  
  # Create output filename
  png_file <- paste0(output_prefix, "_", file_suffix, "_comparison.png")
  pdf_file <- paste0(output_prefix, "_", file_suffix, "_comparison.pdf")
  
  # ============================================================================
  # Create PNG plot
  # ============================================================================
  png(png_file, width = 10, height = 8, units = "in", res = 300)
  
  # Set up plotting area with margins for legends
  par(mar = c(5, 5, 4, 2), family = "sans")
  
  # Create empty plot with log scale
  plot(1, type = "n", 
       xlim = c(plot_min, plot_max), 
       ylim = c(plot_min, plot_max),
       log = "xy",
       xlab = expression(paste("Measured concentration (µg ", kg^{-1}, ")")),
       ylab = expression(paste("Modelled concentration (µg ", kg^{-1}, ")")),
       main = paste0(country_name, " - ", unique(data$Timepoint_label)),
       cex.lab = 1.2, cex.axis = 1.0, cex.main = 1.3, font.main = 2)
  
  # Add grid
  grid(col = "gray90", lty = 1)
  
  # Add reference lines
  x_ref <- c(plot_min, plot_max)
  
  # 1:1 line
  lines(x_ref, x_ref, lwd = 1.5, col = "black")
  
  # ±1 SD (factor of 2)
  lines(x_ref, x_ref * 2, lwd = 1, lty = 2, col = "gray40")
  lines(x_ref, x_ref * 0.5, lwd = 1, lty = 2, col = "gray40")
  
  # ±1 order of magnitude (factor of 10)
  lines(x_ref, x_ref * 10, lwd = 1, lty = 3, col = "gray40")
  lines(x_ref, x_ref * 0.1, lwd = 1, lty = 3, col = "gray40")
  
  # Plot data points with error bars
  for (i in 1:nrow(data)) {
    # Error bars
    arrows(
      x0 = data$`Concentration sample`[i],
      y0 = data$Model_concentration[i] - data$Model_SD[i],
      x1 = data$`Concentration sample`[i],
      y1 = data$Model_concentration[i] + data$Model_SD[i],
      angle = 90, code = 3, length = 0, lwd = 1,
      col = data$Color[i]
    )
    
    # Data points
    points(
      x = data$`Concentration sample`[i],
      y = data$Model_concentration[i],
      pch = data$Marker[i],
      col = data$Color[i],
      bg = data$Color[i],
      cex = 1.2
    )
  }
  
  # ============================================================================
  # Add legends
  # ============================================================================
  
  # Legend 1: Reference lines (top left)
  legend("topleft", 
         legend = c("1:1 line", "±1 SD (2×)", "±1 OoM (10×)"),
         lty = c(1, 2, 3), lwd = c(1.5, 1, 1),
         col = c("black", "gray40", "gray40"),
         title = "Reference lines",
         bty = "o", bg = "white", cex = 0.8, box.lwd = 1)
  
  # Legend 2: Active substances (bottom right)
  legend("bottomright",
         legend = unique_substances,
         pch = 16,  # filled circle for all
         col = substance_colors[unique_substances],
         pt.cex = 1.2,
         title = "Active substance",
         bty = "o", bg = "white", cex = 0.7, box.lwd = 1,
         ncol = 1)
  
  # Legend 3: Locations (top right)
  legend("topright",
         legend = unique_locations,
         pch = location_markers[unique_locations],
         col = "gray40",
         pt.bg = "gray40",
         pt.cex = 1.2,
         title = "Location",
         bty = "o", bg = "white", cex = 0.7, box.lwd = 1,
         ncol = 1)
  
  dev.off()
  
  # ============================================================================
  # Create PDF plot (same code but for PDF)
  # ============================================================================
  pdf(pdf_file, width = 10, height = 8)
  
  par(mar = c(5, 5, 4, 2), family = "sans")
  
  plot(1, type = "n", 
       xlim = c(plot_min, plot_max), 
       ylim = c(plot_min, plot_max),
       log = "xy",
       xlab = expression(paste("Measured concentration (µg ", kg^{-1}, ")")),
       ylab = expression(paste("Modelled concentration (µg ", kg^{-1}, ")")),
       main = paste0(country_name, " - ", unique(data$Timepoint_label)),
       cex.lab = 1.2, cex.axis = 1.0, cex.main = 1.3, font.main = 2)
  
  grid(col = "gray90", lty = 1)
  
  lines(x_ref, x_ref, lwd = 1.5, col = "black")
  lines(x_ref, x_ref * 2, lwd = 1, lty = 2, col = "gray40")
  lines(x_ref, x_ref * 0.5, lwd = 1, lty = 2, col = "gray40")
  lines(x_ref, x_ref * 10, lwd = 1, lty = 3, col = "gray40")
  lines(x_ref, x_ref * 0.1, lwd = 1, lty = 3, col = "gray40")
  
  for (i in 1:nrow(data)) {
    arrows(
      x0 = data$`Concentration sample`[i],
      y0 = data$Model_concentration[i] - data$Model_SD[i],
      x1 = data$`Concentration sample`[i],
      y1 = data$Model_concentration[i] + data$Model_SD[i],
      angle = 90, code = 3, length = 0, lwd = 1,
      col = data$Color[i]
    )
    
    points(
      x = data$`Concentration sample`[i],
      y = data$Model_concentration[i],
      pch = data$Marker[i],
      col = data$Color[i],
      bg = data$Color[i],
      cex = 1.2
    )
  }
  
  legend("topleft", 
         legend = c("1:1 line", "±1 SD (2×)", "±1 OoM (10×)"),
         lty = c(1, 2, 3), lwd = c(1.5, 1, 1),
         col = c("black", "gray40", "gray40"),
         title = "Reference lines",
         bty = "o", bg = "white", cex = 0.8, box.lwd = 1)
  
  legend("bottomright",
         legend = unique_substances,
         pch = 16,
         col = substance_colors[unique_substances],
         pt.cex = 1.2,
         title = "Active substance",
         bty = "o", bg = "white", cex = 0.7, box.lwd = 1,
         ncol = 1)
  
  legend("topright",
         legend = unique_locations,
         pch = location_markers[unique_locations],
         col = "gray40",
         pt.bg = "gray40",
         pt.cex = 1.2,
         title = "Location",
         bty = "o", bg = "white", cex = 0.7, box.lwd = 1,
         ncol = 1)
  
  dev.off()
  
  cat(paste0("Created: ", png_file, " and ", pdf_file, "\n"))
}

nl_conc <- dir_ls(path_home_r(), recurse = T, regexp = "NL soil concentration comparison.csv") |> read_csv()
cz_conc <- dir_ls(path_home_r(), recurse = T, regexp = "CZ soil concentration comparison.csv")|> read_csv()


create_scatterplot_timepoint(nl_conc, "Netherlands", "NL", "initial")
create_scatterplot_timepoint(nl_conc, "Netherlands", "NL", "56days")

create_scatterplot_timepoint(cz_conc, "Czech Republic", "CZ", "initial")
create_scatterplot_timepoint(cz_conc, "Czech Republic", "CZ", "56days")


# Alternative faceted plots
# Create a data frame for vertical lines (separating locations)
# We need to add lines between each unique location

# Create the plot
# p <- ggplot(soil_conc_long, 
#             aes(x = Location_label, y = Concentration, fill = Measurement_type)) +
#   geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
#   geom_errorbar(aes(ymin = Concentration - SD, ymax = Concentration + SD),
#                 position = position_dodge(width = 0.9),
#                 width = 0.25,
#                 na.rm = TRUE) +
# Add vertical dashed lines to separate locations
# geom_vline(data = vline_data, 
#            aes(xintercept = xintercept), 
#            linetype = "dashed", 
#            color = "gray50", 
#            linewidth = 0.5) +
# facet_wrap(~ Active.substance, scales = "free_y", ncol = 1) +
# scale_fill_manual(values = c("Sample (measured)" = "#E69F00", 
#                              "Initial (modeled)" = "#56B4E9", 
#                              "56-day TWA (modeled)" = "#009E73")) +
# labs(
#   title = "Comparison of Measured and Modeled Pesticide Concentrations in Soil",
#   subtitle = "Error bars represent standard deviation (SD) for modeled values",
#   x = "Location (n = number of fields growing oilseed rape)",
#   y = paste0("Concentration \u00B5g\u00D7kg\u207B\u00B9"),
#   fill = "Measurement Type"
# ) +
# theme_minimal() +
# theme(
#   # Remove all grid lines
#   panel.grid.major = element_blank(),
#   panel.grid.minor = element_blank(),
#   # Keep axis lines
#   axis.line = element_line(color = "black", linewidth = 0.5),
#   axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
#   legend.position = "bottom",
#   legend.title = element_text(face = "bold"),
#   strip.text = element_text(face = "bold", size = 12),
#   strip.background = element_rect(fill = "lightgray", color = NA),
#   plot.title = element_text(face = "bold", size = 14),
#   plot.subtitle = element_text(size = 10, color = "gray40"),
#   panel.background = element_rect(fill = "white", color = NA),
#   plot.background = element_rect(fill = "white", color = NA)
# )

# Display the plot
# print(p)

# OLD PLOTS
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
