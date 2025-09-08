pkg <- c("tidyverse", "fs", "readxl", "terra", "tmap", "OpenStreetMap", "tidyterra", "sf", "data.table", "openxlsx", "giscoR")

for (i in pkg) {
  if (!requireNamespace(i, quietly = TRUE)) { 
    install.packages(i)
  }
}

lapply(pkg, library, character.only = T)

crop_code_dk <- dir_ls(path_home_r(), recurse = T, regexp = "Denmark_final_version_Hom_EUcrop_matches2") |>
  read_xlsx() |>
  rename("crop_code" = "Crop_code") |>
  mutate(crop_code = as.double(crop_code))
as_usage_dk <- dir_ls(path_home_r(), recurse = T, regexp = "DK_AS_UsageRate") |> read_xlsx() |>
  select("Code", "Aktivstofnavn(e)", "Enhed(er)", "CAS-nr.", "App_Dose_AS_kg.per.ha", "PostCodeIdentifier",	"City") |>
  rename(
  "crop_code" = "Code",
  "Active" = "Aktivstofnavn(e)",
  "Unit" = "Enhed(er)",
  "CAS_nr" = "CAS-nr.",
  "App_dose_kg.ha" = "App_Dose_AS_kg.per.ha",
  "postal_code" ="PostCodeIdentifier"
) |>
  mutate(crop_code = as.double(crop_code),
         postal_code = as.double(postal_code))

as_usage_crop_dk <- as_usage_dk |>
  left_join(crop_code_dk, by = "crop_code") |> 
  group_by(Active, crop_code, CROP_EPPO_CODE, EUcrop.EC_hcat_n, Homologa.CROP_NAME) |> 
  summarise(App_dose_kg.ha = mean(App_dose_kg.ha, na.rm = T),
            n = n()) |> 
  ungroup() |>
  filter(!is.na(App_dose_kg.ha) & !is.na(Homologa.CROP_NAME)) |> 
  select(-c(EUcrop.EC_hcat_n, Homologa.CROP_NAME, n))

nuts <- dir_ls(path_home_r(), recurse = T, regexp = "/NUTS_RG_20M_2024_4326.shp$") |> vect()
nuts_dk <- nuts |> filter(CNTR_CODE == "DK" & LEVL_CODE == 3)
crop_map_dk <- dir_ls(path_home_r(), recurse = T, regexp = "eucrop_dk2021") |> vect() |> project(crs(nuts_dk))
crop_map_nuts_dk <- terra::intersect(crop_map_dk |> makeValid(), nuts_dk) |> select(c("field_id", "farm_id", "crop_code", "crop_name", "EC_trans_n", "EC_hcat_n",  "EC_hcat_c",  "organic", "NUTS_ID"))
crop_as_map_nuts_dk <- crop_map_nuts_dk |> tidyterra::left_join(as_usage_crop_dk, by = join_by("crop_code"))

# writeVector(crop_as_map_nuts_dk, "gemap_allchem_dk_wgs84.gpkg")
# writeVector(crop_as_map_nuts_dk |> filter(Active %in% c("acetamiprid", "glyphosat", "tebuconazol")), "gemap_3chem_dk_wgs84.gpkg")

# postal_code_dk <- dir_ls(path_home_r(), recurse = T, regexp = "PCODE_2024_PT.shp$") |>
#   vect() |>
#   select("POSTCODE", "LAU_NAT", "CNTR_ID") |>
#   rename("postal_code" = "POSTCODE") |>
#   mutate(postal_code = as.double(postal_code)) |>
#   filter(CNTR_ID == "DK") |>
#   project(crs(nuts_dk))
