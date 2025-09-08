pkg <- c("tidyverse", "fs", "readxl", "terra", "tmap", "OpenStreetMap", "tidyterra", "sf", "data.table", "fuzzyjoin", "openxlsx")

for (i in pkg) {
  if (!requireNamespace(i, quietly = TRUE)) { 
    install.packages(i)
  }
}

lapply(pkg, library, character.only = T)

# INITIAL CODE TO GENERATE INPUT MAPS AND TABELS

# Dutch PPP registry
# 
# ppp_nl <- paste0(getwd(), "/sprint/Crop data/PPP usage/NL_export-10-12-2024.xlsx") |>
#   read_xlsx(na = " ") |>
#   separate_wider_delim("Minimale interval tussen toepassingen",
#                        delim = " ",
#                        names = c("Minimale interval tussen toepassingen_value",
#                                  "Minimale interval tussen toepassingen_unit")) |>
#   separate_wider_regex("Maximum middeldosis",
#                        c("Maximum middeldosis_value" = "\\d+\\.\\d+", "\\s",
#                          "Maximum middeldosis_unit" = "(?<=\\d\\s)[:print:]+")) |>
#   separate_wider_regex("Maximum middeldosis per gewasseizoen",
#                        c("Maximum middeldosis per gewasseizoen_value" = "\\d+\\.\\d+", "\\s",
#                          "Maximum middeldosis per gewasseizoen_unit" = "(?<=\\d\\s)[:print:]+")) |>
#   dplyr::rename(acsubst_name = "Werkzame stof(fen)",
#                 crop_name = Toepassingsgebied) |>
#   dplyr::mutate(acsubst_name = case_match(acsubst_name,
#                                           "glyfosaat" ~ "glyphosate",
#                                           "tebuconazool" ~ "tebuconazol")) |>
#   # Extract separate crops from single cells and transpose into rows. Remove crop group names
#   filter(acsubst_name == "glyphosate" & Middelnaam == "AZURAL") |>
#   # One application method
#   head(1) |>
#   separate_longer_delim(crop_name, delim = ",") |>
#   # Single crop name per row. No group names
#   mutate(crop_name = str_remove(crop_name, pattern = "\\w+[:punct:]\\s")) |>
#   mutate(crop_name = str_remove(crop_name, pattern = "\\w+[:punct:]\\s")) |>
#   mutate(crop_name = str_remove(crop_name, pattern = " "))

# Crop map preparation
# 
# nuts <- dir_ls(path_home_r(), recurse = T, regexp = "/NUTS_RG_20M_2024_4326.shp$")  |>
#   vect()
# nuts_nl <- nuts |>
#   filter(CNTR_CODE == "NL" & LEVL_CODE == 3)

# parcels_nl <- dir_ls(path_home_r(), recurse = T, regexp = "NL_2020_EC21.shp") |> vect() |> makeValid()
# crop_map_nuts_nl <- terra::intersect(parcels_nl,
#                                      nuts_nl)
# writeVector(crop_map_nuts_nl, "NL_EC_crop_nuts_map.gpkg")

# crop_nuts_grouped <- terra::aggregate(crop_map_nuts_nl, c("gewascode", "NUTS_NAME"), fun = "sum")
# area_crop_nuts <- terra::aggregate(crop_map_nuts_nl, c("gewascode", "NUTS_NAME"), fun = "sum") |> expanse(unit = "ha")
#
# crop_map_nuts_agg_nl <- cbind(crop_nuts_grouped, area_crop_nuts) |>
#   select(-c(sum_jaar, sum_LEVL_CODE, sum_MOUNT_TYPE, sum_URBN_TYPE, sum_COAST_TYPE)) |>
#   rename(area_crop_ha = y,
#          nr_farms = agg_n)

# crop_map_nuts_agg_nl |> writeVector("crop_agg_map.gpkg")

# Prepare pesticide usage data

# fungi <- fread(dir_ls(path_home_r(),
#                       recurse = T,
#                       regexp = "NL_Fungicides_CBS.csv")) |>
#   tibble() |>
#   rename(Dose_kg_ha = "Dose20,") |>
#   mutate(Dose_kg_ha = str_remove(Dose_kg_ha, ","),
#          Dose_kg_ha = as.numeric(Dose_kg_ha)) |>
#   select(Crop, Pesticide, Dose_kg_ha) |>
#   filter(Pesticide != "Natrium-p-tolueensulfonchloramide") |>
#   separate_wider_regex(Pesticide, c(Pesticide_ID = "^[A-Z]\\d+", "\\s",
#                                     Pesticide_Name = "(?<=\\s)[:print:]+$")) |>
#   add_column(Pesticide_class = "Fungicide")
# 
# herbi<- fread(dir_ls(path_home_r(),
#                      recurse = T,
#                      regexp = "NL_Herbicides_CBS.csv"), encoding = "UTF-8") |>
#   as_tibble() |>
#   rename(Dose_kg_ha = "Dose20") |>
#   select(Crop, Pesticide, Dose_kg_ha) |>
#   separate_wider_regex(Pesticide, c(Pesticide_ID = "^[A-Z]\\d+", "\\s",
#                                     Pesticide_Name = "(?<=\\s)[:print:]+$")) |>
#   add_column(Pesticide_class = "Herbicide")
# 
# insect <- fread(dir_ls(path_home_r(),
#                        recurse = T,
#                        regexp = "NL_Insecticides_CBS.csv")) |>
#   tibble() |>
#   rename(Dose_kg_ha = "Dose20") |>
#   select(Crop, Pesticide, Dose_kg_ha) |>
#   separate_wider_regex(Pesticide, c(Pesticide_ID = "^[A-Z]\\d+", "\\s",
#                                     Pesticide_Name = "(?<=\\s)[:print:]+$")) |>
#   add_column(Pesticide_class = "Insecticide")
# 
# pesti_full <- bind_rows(fungi, herbi) |> bind_rows(insect) |>
#   mutate(Crop = str_replace_all(Crop, "^Pap\\w+\\'\\w+\\,\\s\\w+", "Other fresh vegetables (tomato, pepper)"),
#          Crop = str_replace_all(Crop, "^Tom\\w+\\,\\s\\w+", "Other fresh vegetables (tomato, pepper)"))

# write.xlsx(pesti_full, "pesti_dose_crop_nl.xlsx")

# Filter and join chemical codebook
pesti_full <- dir_ls(path_home_r(),
                     recurse = T,
                     regexp = "pesti_dose_crop_nl.xlsx") |>
  read_excel() |>
  filter(Dose_kg_ha > 0)

# Create substance codebook df
substance_codebook <- read_excel(dir_ls(path_home_r(), recurse = T, regexp = "Substances codebook with CSS - corr 2024-01-30.xlsx"), 
                                 sheet = "Data",
                                 range = "A1:V1552") |> 
  mutate(STAT_code = str_replace_all(STAT_code, "_", ""))

pesti_full <- pesti_full |>
  left_join(substance_codebook, by = c(Pesticide_ID = "STAT_code")) |> 
  select("Crop", "Dose_kg_ha", "EU_name", "CAS_number")

# Load excel file with interception factors and eppo codes
crop_focus_eppo_intercept <- read_xlsx(dir_ls(path_home_r(), recurse = T, regexp = "/EFSA_FOCUS crop interception factor.xlsx"), sheet = 2)

# write.xlsx(pesti_full, "nl_ppp_usage.xlsx")

# Read unique crops - pesiticide combinations. EPPO - FOCUS crops connection was done manually in excel

pest_crops <- dir_ls(path_home_r(),
                     recurse = T,
                     regexp = "crop_map_nl_en_uniq.xlsx") |>
  read_excel(sheet = 1)

map_crops <- dir_ls(path_home_r(),
                    recurse = T,
                    regexp = "crop_map_nl_en_uniq.xlsx") |>
  read_excel(sheet = 2)

# Add crop specific spray drift values to the "map_crops"

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
)

map_crops_drift <- map_crops |> 
  mutate(Crop_group_sdrift = case_when(Crop_code_rvo == 1014 ~ "Vines",
                                       Crop_code_rvo == 1099 ~ "Vines",
                                       Crop_code_rvo == 1091  ~ "Orchards",
                                       Crop_code_rvo == 1092 ~ "Orchards",
                                       Crop_code_rvo == 1093 ~ "Orchards",
                                       Crop_code_rvo == 1077 ~ "Orchards",
                                       Crop_code_rvo == 1078 ~ "Orchards",
                                       Crop_code_rvo == 1079 ~ "Orchards",
                                       Crop_code_rvo == 1874 ~ "Orchards",
                                       Crop_code_rvo == 375 ~ "Hops",
                                       .default = "Arable")) |> 
  left_join(spray_drift, by = "Crop_group_sdrift")


# Join unique pesticide - crop pairs with crops from the dutch parcel map

pest_crop <- pest_crops |>
  left_join(map_crops_drift,
            by = join_by(Crop_code_rvo)) |>
  left_join(pesti_full,
            by = join_by(Crop_name_pestdose_nl == Crop))

# Read dutch crop map
crop_map_nuts_nl <- dir_ls(path_home_r(), recurse = T, regexp = "NL_EC_crop_nuts_map.gpkg") |>
  vect() |>
  tidyterra::mutate(gewascode = as.numeric(gewascode)) |> 
  select("gewas", "gewascode", "length", "area", "EC_trans_n","EC_hcat_n", "EC_hcat_c", "NUTS_ID",
         "CNTR_CODE", "NAME_LATN", "NUTS_NAME")
# crop_agg_map_nuts_nl <- dir_ls(path_home_r(), recurse = T, regexp = "crop_agg_map.gpkg") |> vect()

gemap_nl <- tidyterra::left_join(crop_map_nuts_nl,
                                 pest_crop,
                                 by = join_by("EC_hcat_n",
                                              "EC_trans_n",
                                              "gewascode" == "Crop_code_rvo")) |> 
  tidyterra::left_join(crop_focus_eppo_intercept,
                       by = join_by("Crop_pestdose_code_eppo_focus" == "crop_code_eppo"))

writeVector(gemap_nl, "gemap_allchem_nl_wgs84.gpkg")

# Hydrosheds river basins. Prepare masked river basin map to include only country of interest. This data include also info on river discharge, precipitation, river volume, terrain slope and more.

# basins_eu_path <- dir_ls(path_home_r(), recurse = T, regexp = "BasinATLAS_v10_lev12.shp")
# rivers_eu_path <- dir_ls(path_home_r(), recurse = T, regexp = "RiverATLAS_v10_eu.shp")
# basins_nl <- vect(basins_eu_path, extent = ext(nuts_nl))
# rivers_nl <- vect(rivers_eu_path, extent = ext(nuts_nl))
# writeVector(basins_nl, "hydrosheds_lvl12_basins_nl.gpkg", overwrite = T)
# writeVector(rivers_nl, "hydrosheds_lvl10_rivers_nl.gpkg")

# Prepare soil density map

# budens_jrc_laea <- dir_ls(path_home_r(), recurse = T, regexp = "Bulk_density.tif$") |> rast()
# budens_jrc_nl_wgs84 <- budens_jrc_laea |> project(nuts_nl) |> crop(nuts_nl)
# writeCDF(budens_jrc_cz_wgs84, "budens_jrc_NL.nc", overwrite = T)

# Prepare list of RVO cereals, names and codes

# crops_nl_rvo <- tribble(
#   ~BRP_code, ~crop_name_rvo, ~crop_group_rvo,
#   235, "Wintergerst", "Rustgewassen", 
#   236, "Zomergerst", "Rustgewassen", 
#   383, "Graszaad" , "Rustgewassen", 
#   238, "Haver", "Rustgewassen", 
#   944, "Hennepvezel","Rustgewassen", 
#   246, "Karwijzaad","Rustgewassen", 
#   1922, "Winterkoolzaad","Rustgewassen", 
#   1923, "Zomer koolzaad","Rustgewassen", 
#   666, "Lijnzaad","Rustgewassen", 
#   258, "Luzerne","Rustgewassen",
#   664, "Raapzaad","Rustgewassen",
#   237, "Rogge","Rustgewassen",
#   3519, "Sorghum", "Rustgewassen",
#   233, "Winter tarwe", "Rustgewassen",
#   234, "Zomer tarwe","Rustgewassen",
#   381, "Teff","Rustgewassen",
#   314, "Triticale","Rustgewassen",
#   3736, "Vezelvlas","Rustgewassen",
#   1037, "Peterselie","Rustgewassen",
#   247, "Blauwmaanzaad","Rustgewassen",
#   799, "Rode klaver","Rustgewassen",
#   3524, "Witte klaver","Rustgewassen",
#   516, "Miscanthus","Rustgewassen",
#   382, "Spelt","Rustgewassen",
#   1022, "Quinoa","Rustgewassen",
#   2652, "Overige granen","Rustgewassen",
#   265, "Grasland blijvend", "Gras/klaver",
#   266, "Grasland tijdelijk", "Gras/klaver",
#   331, "Grasland natuurlijk", "Gras/klaver",
#   333, "Rand, grenzend aan blijvend grasland of een blijvende teelt, hoofdzakelijk bestaand uit blijvend gras", "Gras/klaver",
#   334, "Rand, grenzend aan bouwland, hoofdzakelijk bestaand uit blijvend gras", "Gras/klaver",
#   370, "Rand, grenzend aan blijvend grasland of een blijvende teelt, hoofdzakelijk bestaand uit tijdelijk grasland", "Gras/klaver",
#   372, "Rand, grenzend aan bouwland, hoofdzakelijk bestaand uit tijdelijk gras","Gras/klaver",
#   3500, "Klaver, Alexandrijnse", "Gras/klaver",
#   3511, "Klaver, incarnaat", "Gras/klaver",
#   3515, "Klaver, Perzische", "Gras/klaver",
#   799, "Klaver, rode", "Gras/klaver",
#   3524, "Klaver, witte", "Gras/klaver",
#   800, "Rolklaver", "Gras/klaver",
#   265, "Grasland blijvend", "Grasland met kruiden",
#   266, "Grasland tijdelijk", "Grasland met kruiden",
#   331, "Grasland natuurlijk", "Grasland met kruiden",
#   333, "Rand, grenzend aan blijvend grasland of een blijvende teelt, hoofdzakelijk bestaand uit blijvend gras", "Grasland met kruiden",
#   334, "Rand, grenzend aan bouwland, hoofdzakelijk bestaand uit blijvend gras", "Grasland met kruiden",
#   370, "Rand, grenzend aan blijvend grasland of een blijvende teelt, hoofdzakelijk bestaand uit tijdelijk grasland", "Grasland met kruiden",
#   372, "Rand, grenzend aan bouwland, hoofdzakelijk bestaand uit tijdelijk gras", "Grasland met kruiden",
#   265, "Grasland blijvend", "Langjarig Grasland",
#   331, "Grasland natuurlijk", "Langjarig Grasland",
#   333, "Rand, grenzend aan blijvend grasland of een blijvende teelt, hoofdzakelijk bestaand uit blijvend gras", "Langjarig Grasland",
#   334, "Rand, grenzend aan bouwland, hoofdzakelijk bestaand uit blijvend gras", "Langjarig Grasland",
#   258, "Luzerne", "Meerjarige teelt",
#   383, "Graszaad", "Meerjarige teelt",
#   516, "Miscantus (olifantsgras)", "Meerjarige teelt",
#   656, "Zonnekroon", "Meerjarige teelt",
#   6520, "Azola", "Natte teelt",
#   3055, "Lisdodde", "Natte teelt",
#   6521, "Wilde rijst", "Natte teelt",
#   6522, "Riet", "Natte teelt", 
#   242, "Bonen, bruine", "Stikstofbindend gewas",
#   853, "Bonen, tuin- (droog te oogsten) (geen consumptie)", "Stikstofbindend gewas",
#   854, "Bonen, tuin- (groen te oogsten)","Stikstofbindend gewas",
#   311, "Bonen, veld- (onder andere duiven-, paarden-, wierbonen)","Stikstofbindend gewas",
#   308, "Erwten (droog te oogsten)","Stikstofbindend gewas",
#   244, "Erwten, groene/gele, groen te oogsten","Stikstofbindend gewas",
#   241, "Kapucijners (en grauwe erwten)","Stikstofbindend gewas",
#   663, "Lupinen, niet bittere-","Stikstofbindend gewas",
#   258, "Luzerne","Stikstofbindend gewas",
#   665, "Sojabonen","Stikstofbindend gewas",
#   2747, "Peulen, productie","Stikstofbindend gewas",
#   2751, "Pronkbonen, productie","Stikstofbindend gewas",
#   2779, "Stamsperziebonen (=stamslabonen), productie","Stikstofbindend gewas",
#   799, "Rode klaver","Stikstofbindend gewas",
#   3524, "Witte klaver - Linzen","Stikstofbindend gewas",
#   800, "Rolklaver","Stikstofbindend gewas",
#   801, "Esparcette","Stikstofbindend gewas",
#   803, "Voederwikke","Stikstofbindend gewas",
#   3500, "Klaver, Alexandrijnse","Stikstofbindend gewas",
#   3511, "Klaver, incarnaat","Stikstofbindend gewas",
#   3515, "Klaver, Perzische","Stikstofbindend gewas",
#   804, "Klaverzaad","Stikstofbindend gewas",
#   426, "Overige groenbemesters, vlinderbloemige-","Stikstofbindend gewas",
#   2748, "Peule, zaden en opkweekmateriaal","Stikstofbindend gewas",
#   2752, "Pronkbonen, zaden en opkweekmateriaal","Stikstofbindend gewas",
#   2780, "Stamsperziebonen (= stamslabonen, zaden en opkweekmateriaal","Stikstofbindend gewas",
#   2014, "Consumptieaardappelen", "Vroeg oogsten rooigewas 31 augustus",
#   2017, "Aardappelen, zetmeel", "Vroeg oogsten rooigewas 31 augustus",
#   6660, "Uien, gele, zaai","Vroeg oogsten rooigewas 31 augustus",
#   6664, "Uien, rode, zaai","Vroeg oogsten rooigewas 31 augustus",
#   1934, "Sjalotten","Vroeg oogsten rooigewas 31 augustus",
#   2717, "Bospeen","Vroeg oogsten rooigewas 31 augustus",
#   2741, "Rode bieten","Vroeg oogsten rooigewas 31 augustus",
#   2783, "Waspeen" ,"Vroeg oogsten rooigewas 31 augustus",
#   2801, "Zomerprei","Vroeg oogsten rooigewas 31 augustus",
#   256, "Suikerbiet","Vroeg oogsten rooigewas 31 augustus",
#   257, "Voederbiet","Vroeg oogsten rooigewas 31 augustus",
#   511, "Cichorei" ,"Vroeg oogsten rooigewas 31 augustus",
#   2785, "Winterpeen","Vroeg oogsten rooigewas 31 augustus",
#   2787, "Witlofwortel","Vroeg oogsten rooigewas 31 augustus",
#   3521, "Stoppelknollen", "Vroeg oogsten rooigewas 31 augustus",
#   2725, "Knolselderij, productie","Vroeg oogsten rooigewas 31 augustus",
#   2726, "Knolselderij, zaden en opkweekmateriaal","Vroeg oogsten rooigewas 31 augustus",
#   256, "Suikerbiet", "Vroeg oogsten rooigewas 31 oktober",
#   257, "Voederbiet", "Vroeg oogsten rooigewas 31 oktober",
#   511, "Cichorei", "Vroeg oogsten rooigewas 31 oktober",
#   2785, "Winterpeen", "Vroeg oogsten rooigewas 31 oktober",
#   2787, "Witlofwortel", "Vroeg oogsten rooigewas 31 oktober",
#   3521, "Stoppelknollen", "Vroeg oogsten rooigewas 31 oktober",
#   2725, "Knolselderij, productie", "Vroeg oogsten rooigewas 31 oktober",
#   2726, "Knolselderij, zaden en opkweekmateriaal", "Vroeg oogsten rooigewas 31 oktober"
# )

