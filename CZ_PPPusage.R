# Required packages
pkg <- c("tidyverse", "readxl", "fs", "janitor", "RCzechia")

# Download, install and load required packages

for (i in pkg) {
  if (!requireNamespace(i, quietly = TRUE)) {
    install.packages(i)
  }
}

lapply(pkg, library, character.only = T)

#########################################################################
# Script to extract various data: crop codes, crop names, ppp registry, #
#########################################################################

# Create crop codes df
eagri <- read_xlsx(dir_ls(path_home_r(), recurse = T, regexp = "/Plodiny_skupiny_plodin.xlsx$"),
                   col_names = TRUE) |>
  select(!starts_with("...") & !"Název skupiny plodin SRS" & !"Kód skupiny plodin ÚKZÚZ") |>
  fill(1) |>
  rename("crop_name_eagri" = "Název plodiny EAGRI",
         "crop_code_eagri" = "Kód plodiny EAGRI") |> 
  filter(crop_code_eagri != "XNA") |> 
  mutate(crop_code_eagri = as.numeric(crop_code_eagri))

eagri_eppo <- read.csv(dir_ls(path_home_r(), recurse = T, regexp = "/crop_code_eagri_eppo.csv$"),
                       col.names = c("crop_code_eagri", "crop_code_eppo", "crop_group_eagri"),
                       colClasses = c("numeric", "character", "numeric"),
                       na.strings = "") |>
  as_tibble()

eppo_en <- read.csv(dir_ls(path_home_r(), recurse = T, regexp = "/eppo_translate_en.csv$"),
                    colClasses = c("character", "character", "character"),
                    col.names = c("crop_code_eppo", "crop_name_eppo_full", "crop_name_eppo_short")) |> 
  as_tibble()

crop_codes_all <- full_join(eagri_eppo, eppo_en, by = "crop_code_eppo") |> 
  full_join(eagri, by = "crop_code_eagri") |> 
  # mutate(crop_code_eagri = as_factor(crop_code_eagri)) |> 
  replace_na(list(crop_name_eppo_full = "Not Classified",
                  crop_name_eppo_short = "Not Classified",
                  crop_code_eppo = "Not Classified"))

# Create GenAP df
genap <- read_xlsx(dir_ls(path_home_r(), recurse = T, regexp = "/GENAP version 3")) |> 
  filter(source_of_crop_cz_id_and_name == "EAGRI") |> 
  rename(ai = "as_name",
         crop_name_eagri = "crop_name_cz",
         crop_code_eagri = "crop_cz_id",
         crop_code_eppo = "crop_eppo_code",
         crop_name_eppo_full = "crop_eppo_name_eng",
         min_app_rate = "min_as_appl_rate_g_ha",
         max_app_rate = "max_as_appl_rate_g_ha",
         min_bbch = "min_bbch_from",
         max_bbch = "max_bbch_till",
         max_app_per_year = "max_number_appl_per_year") |> 
  select(-c(2,4,6,7)) |>
  mutate(ai = tolower(ai),
         crop_code_eagri = as.numeric(crop_code_eagri),
         min_app_rate = as.numeric(min_app_rate),
         max_app_rate = as.numeric(max_app_rate),
         min_bbch = as.numeric(min_bbch),
         max_bbch  = as.numeric(max_bbch),
         max_app_per_year = as.numeric(max_app_per_year))

# Create interception factor df (done once)
## Calculated summary statistics of interception factors extracted from PERSAM. Save it seperate file and add manually the eppo codes
# crop_focus_intercept <- read_xlsx(paste0(set_crop_table_dir, "/EFSA_FOCUS crop interception factor.xlsx"),
#                             sheet = 1) |>
#   separate_wider_regex(crop_name_focus,
#                        c(crop_name_focus = ".+", " \\(",
#                          crop_focus_comment = ".+(?=\\))"),
#                        too_few = "align_start")
# 
# crop_intercept_summary <- crop_focus_intercept |>
#   group_by(crop_name_focus, crop_focus_comment) |>
#   summarise(inter_frac_bbch.ave = mean(interception_factor),
#             inter_frac_bbch.min = min(interception_factor),
#             inter_frac_bbch.max = max(interception_factor)) |>
#   ungroup() |>
#   add_row(crop_name_focus = c("wheat", "oats", "rye", "triticale"),
#           crop_focus_comment = c("winter", "winter", "winter", "winter"),
#           inter_frac_bbch.ave = c(0.5 , 0.5, 0.5, 0.5),
#           inter_frac_bbch.min = c(0.0, 0.0,0.0, 0.0),
#           inter_frac_bbch.max = c(0.9, 0.9, 0.9, 0.9)) |>
#   add_row(crop_name_focus = c("wheat", "oats", "rye", "triticale"),
#           crop_focus_comment = c("spring", "spring", "spring", "spring"),
#           inter_frac_bbch.ave = c(0.5, 0.5, 0.5, 0.5),
#           inter_frac_bbch.min = c(0.0, 0.0,0.0, 0.0),
#           inter_frac_bbch.max = c(0.9, 0.9, 0.9, 0.9)) |>
#   filter(crop_name_focus != "winter cereals",
#          crop_name_focus != "spring cereals") |> gt()

# Load excel file with interception factors and eppo codes
crop_focus_eppo_intercept <- read_xlsx(dir_ls(path_home_r(), recurse = T, regexp = "/EFSA_FOCUS crop interception factor.xlsx"), sheet = 2) |> 
  filter(crop_code_eppo %in% crop_codes_all$crop_code_eppo)

# Check what eppo codes from map are missing in the interception factor table
# dplyr::setdiff(crop_map_all[,2],
#                crop_focus_eppo_intercept[ , 2]) |> view()

# Read crop codes and names from EAGRI crop distribution map into data frame
crop_codes_map <- read_xlsx(dir_ls(path_home_r(), recurse = T, regexp =
                                   "/GPZ_Plodiny_2021_12_31.xlsx"),
                            col_names = TRUE,
                            sheet = 2,
                            col_types = c("text", "text", "numeric"))|>
  rename(crop_code_eagri = "ID plodiny",
         crop_name_eagri_map = "Plodina",
         declared_tot_dist_sum.ha = "Deklarovaná výměra (ha)")|> 
  mutate(crop_code_eagri = as.numeric(crop_code_eagri))

# Missing EPPO codes are substituted with genus of the generic FOCUS group with the interception factors. EAGRI individual crop and crop group codes are used to verify if plants can be tentatively grouped in order to attribute interception factors and eppo codes.
# Join interception factor df to crop codes

crop_map_all <- crop_codes_all |>
  filter(crop_code_eagri %in% crop_codes_map$crop_code_eagri) |> 
  full_join(crop_focus_eppo_intercept , by =  "crop_code_eppo") |>
  filter(!if_any(crop_code_eagri, is.na)) |> 
  filter(!if_any(crop_group_eagri, is.na)) |>
  replace_na(list(crop_name_eppo_full = "Not Classified",
                  crop_name_eppo_short = "Not Classified",
                  crop_code_eppo = "Not Classified")) |> 
  mutate(crop_code_eppo = case_when(crop_code_eppo == "Not Classified" & crop_group_eagri == 1 ~ "1SOLG",
                                    crop_code_eppo == "Not Classified" & crop_group_eagri == 3 ~ "1MALG",
                                    crop_code_eppo == "Not Classified" & crop_group_eagri == 4 ~ "1UMBF",
                                    crop_code_eppo == "Not Classified" & crop_group_eagri == 5 ~ "1VICG",
                                    crop_code_eppo == "Not Classified" & crop_group_eagri == 7 ~ "1ALLG",
                                    crop_code_eppo == "Not Classified" & crop_group_eagri == 8 ~ "1PAPG",
                                    crop_code_eppo == "Not Classified" & crop_group_eagri == 9 ~ "1MSPG",
                                    crop_code_eppo == "Not Classified" & crop_group_eagri == 10 ~ "1VITG",
                                    crop_code_eppo == "Not Classified" & crop_group_eagri == 11 ~ "1GRAF",
                                    crop_code_eppo == "Not Classified" & crop_group_eagri == 14 ~ "1LEGF",
                                    crop_code_eppo == "Not Classified" & crop_group_eagri == 16 ~ "1MSPG",
                                    crop_code_eppo == "Not Classified" & crop_group_eagri == 17 ~ "1RUBG",
                                    crop_code_eppo == "Not Classified" & crop_group_eagri == 23 ~ "1COMF",
                                    crop_code_eppo == "Not Classified" & crop_group_eagri == 33 ~ "1GRAF",
                                    .default = crop_code_eppo))

# Create substance codebook df
substance_codebook <- read_excel(dir_ls(path_home_r(), recurse = T, regexp = "Substances codebook with CSS - corr 2024-01-30.xlsx"), 
                                 sheet = "Data",
                                 range = "A1:V1552") |> 
  mutate(STAT_code = str_replace_all(STAT_code, "_", "")) |> 
  filter(!is.na(PPDB_name))

###########################################################
# Script to extract detailed AI usage for individual crops#
###########################################################
read_ai_usage_path <- dir_ls(path_home_r(), recurse = T, regexp = "Spotreba_a_osetrena_vymera")

unit_name <- c("kg", "ha")
year <- str_match(read_ai_usage_path[6], "(?<=okresu_)[0-9]+")
ASs <- c("dimoxystrobin",
         "difenoconazole",
         "fluazinam",
         "epoxiconazole",
         "azoxystrobin",
         "pethoxamid",
         "benzovindiflupyr",
         "tebuconazole",
         "chlorantraniliprole",
         "quinmerac",
         "flufenacet",
         "mefentrifluconazole",
         "beflubutamid",
         "tefluthrin",
         "nicosulfuron",
         "picloram",
         "metazachlor",
         "pendimethalin",
         "gamma-cyhalothrin",
         "deltamethrin",
         "cyflufenamid",
         "glyphosate",
         "spiroxamine",
         "clomazone",
         "prothioconazole",
         "fluopyram",
         "esfenvalerate",
         "lambda-cyhalothrin",
         "bixafen",
         "chlorotoluron",
         "fenpropidin",
         "metconazole",
         "clopyralid",
         "propyzamide",
         "acetamiprid",
         "s-metolachlor",
         "fluroxypyr",
         "imazamox",
         "ethofumesate",
         "metamitron",
         "mandestrobin",
         "paclobutrazol",
         "fluxapyroxad",
         "foramsulfuron",
         "sulfosulfuron",
         "tembotrione",
         "dimethenamid-p",
         "pyrimethanil",
         "metrafenone",
         "pyraclostrobin",
         "tetraconazole",
         "aclonifen",
         "sulfoxaflor",
         "terbuthylazine",
         "florasulam",
         "folpet",
         "chlormequat",
         "bromuconazole",
         "mesotrione",
         "phenmedipham",
         "sulphur",
         "napropamide",
         "mandipropamid",
         "cypermethrin",
         "propaquizafop",
         "pirimicarb",
         "2,4-d",
         "dichlorprop-p",
         "fluopicolide",
         "flurochloridone",
         "diflufenican",
         "iprovalicarb",
         "pyroxsulam",
         "propoxycarbazone",
         "prosulfocarb",
         "isofetamid",
         "tritosulfuron",
         "tribenuron-methyl",
         "cyazofamid",
         "fludioxonil",
         "potassium phosphonates",
         "metsulfuron-methyl",
         "proquinazid",
         "metribuzin",
         "picolinafen",
         "dimethomorph",
         "metobromuron",
         "tau-fluvalinate",
         "ethephon",
         "dimethachlor",
         "captan",
         "dithianon",
         "etofenprox",
         "fenpyroximate",
         "ferric phosphate",
         "dicamba",
         "fluoxastrobin",
         "benalaxyl-m",
         "metaldehyde",
         "cyprodinil",
         "fenpyrazamine",
         "mcpa",
         "tebufenpyrad",
         "penthiopyrad",
         "oxathiapiprolin",
         "cymoxanil",
         "trifloxystrobin",
         "metalaxyl-m",
         "halauxifen-methyl",
         "amidosulfuron",
         "bentazone",
         "isoxaflutole",
         "mecoprop-p",
         "penoxsulam",
         "metiram",
         "trinexapac-ethyl",
         "amisulbrom",
         "tebufenozide",
         "beta-cyfluthrin",
         "iodosulfuron-methyl-sodium",
         "ametoctradin",
         "lenacil",
         "quizalofop-p-ethyl",
         "aminopyralid",
         "bromoxynil",
         "copper oxychloride",
         "flazasulfuron",
         "zoxamide",
         "hexythiazox",
         "fenhexamid",
         "penconazole",
         "sulcotrione",
         "fenoxaprop-p-ethyl",
         "thiencarbazone-methyl",
         "kresoxim-methyl",
         "bifenazate",
         "flupyradifurone",
         "clethodim",
         "pyridate",
         "chlorsulfuron",
         "zinc phosphide",
         "mcpb",
         "triclopyr",
         "1-methylcyclopropene",
         "zeta-cypermethrin",
         "meptyldinocap",
         "maleic hydrazide",
         "thifensulfuron-methyl",
         "cyantraniliprole",
         "pinoxaden",
         "triflusulfuron",
         "rimsulfuron",
         "fluazifop-p-butyl",
         "prohexadione-calcium",
         "spirotetramat",
         "pyriproxyfen",
         "cycloxydim",
         "quizalofop-p-tefuryl",
         "pyraflufen-ethyl",
         "acequinocyl",
        "1-naphthylacetic acid sodium salt",
         "6-benzyladenine",
         "mesosulfuron-methyl",
        "mepiquat",
        "propamocarb")

# Create data frame of amount of AS applied
ai_usage_mass_df <- read_xlsx(read_ai_usage_path[6], 
                              skip = 4,
                              col_names = TRUE) |> 
  select(!starts_with("...") & c(1:80)) |> 
  fill(1) |>
  filter(!if_any(2 , is.na)) |> 
  bind_cols(year,
            unit_name[1]) |> 
  rename(year = "...80",
         unit = "...81",
         crop_name_eagri = "Název plodiny",
         ai = "Jméno účinné látky EN") |> 
  rename_with(~tolower(gsub("-", "_", .x, fixed = TRUE))) |> 
  mutate(ai = tolower(ai)) |> 
  filter(ai %in% ASs)

# Join AI mass usage table to the crop code on EAGRI map using crop code (EAGRI)
ai_usage_mass_df <- full_join(ai_usage_mass_df,
                              crop_map_all,
                              by = join_by(crop_name_eagri))|>
  relocate(c(crop_name_eagri,
             crop_code_eagri,
             ai),
           c(1, 2, 3))

# save PPP mass data frame to excel
# write_excel_csv(ppp_usage_mass_df,
# file = paste0(set_crop_table_dir,"/ppp_usage_mass.csv"),
# delim = ",")

# Create a list of PPP mass applied in each district
ai_mass_crop_type_list <- ai_usage_mass_df |>
  pivot_longer(cols = !c(crop_name_eagri,
                         crop_code_eagri,
                         ai,
                         year,
                         unit,
                         crop_code_eppo,
                         crop_name_eppo_full ,
                         crop_name_eppo_short ,
                         crop_group_eagri,
                         crop_name_focus,
                         crop_focus_comment,
                         inter_frac_bbch.ave,
                         inter_frac_bbch.min,
                         inter_frac_bbch.max),
               names_to = "District") |> 
  nest(.by = District) |>
  bind_cols(year) |> 
  rename(ai_mass_usage_det = data,
         Year = ...3) |> 
  relocate(ai_mass_usage_det,
           .after = last_col())

# Create data frame of area PPPs are applied on
ai_usage_area_df <- read_excel(read_ai_usage_path[6],
                               skip = 4,
                               col_names = TRUE,
                               na = "")|>
  select(!starts_with("...") & c(1:2, 81:last_col()))|>
  fill(1)|>
  filter(!if_any(2 , is.na)) |> 
  bind_cols(year,
            unit_name[2]) |> 
  rename(year = "...80",
         unit = "...81",
         crop_name_eagri = "Název plodiny",
         ai = "Jméno účinné látky EN") |> 
  rename_with(~tolower(gsub("-", "_", .x, fixed = TRUE)))|> 
  mutate(ai = tolower(ai)) |> 
  filter(ai %in% ASs)

# Join PPP area usage table to the crop code (EAGRI)
ai_usage_area_df <- full_join(ai_usage_area_df,
                              crop_map_all,
                              by = join_by(crop_name_eagri)) |>
  relocate(c(crop_name_eagri,
             crop_code_eagri,
             ai),
           c(1, 2, 3))

# save PPP mass data frame to excel
# write_excel_csv(ppp_usage_area_df,
#                 file = paste0(set_crop_table_dir,"/ppp_usage_area.csv"),
#                 delim = ",")

# Create a list of PPP application area in each district
ai_area_crop_type_list <- ai_usage_area_df |>
  pivot_longer(cols = !c(crop_name_eagri,
                         crop_code_eagri,
                         ai,
                         year,
                         unit,
                         crop_code_eppo,
                         crop_name_eppo_full ,
                         crop_name_eppo_short ,
                         crop_group_eagri,
                         crop_name_focus,
                         crop_focus_comment,
                         inter_frac_bbch.ave,
                         inter_frac_bbch.min,
                         inter_frac_bbch.max),
               names_to = "District") |> 
  nest(.by = District) |> 
  bind_cols(year) |> 
  rename(ai_area_usage_det = data,
         Year = ...3) |> 
  relocate(ai_area_usage_det,
           .after = last_col())

############################################################################
# The script below extracts AI usage for main crop groups in each district#
############################################################################

# Get the paths to the PPP usage files
ai_usage_file_paths <- dir_info(paste0(set_ai_table_dir, "/2021"), 
                                recurse = TRUE,
                                type = "file",
                                regexp = "Okres")

# Read PPP usage files
read_usage_file <- function(file_paths){
  
  tables <- list()
  
  for(i in seq_along(file_paths$path)){
    
    tables[[i]] <- read_xlsx(file_paths$path[i],
                             col_names = FALSE,
                             progress =  TRUE)
  }
  
  return(tables)
  
}

ai_usage_tables <- read_usage_file(ai_usage_file_paths)

# Create new columns with the district name and year
## First extract unique year and district name
## Create function to do these operations. Should be universal enough to deal with different formatting of excel tables
## For the moment the script works fine with tables formatted as they are for instance for year 2021.
ai_usage_per_district_year <- function(usage_tables){
  
  get_usage_year <- vector()
  get_district_name <- vector()
  
  # Extract year and name of the district
  for(i in seq_along(usage_tables)){
    
    
    get_usage_year[i] <- usage_tables[[i]][c(1:15),c(1:2)]|> 
      pivot_longer(cols = c(1,2))|>
      select(value)|> 
      as.character()|> 
      str_extract("(?<=in )\\d+")
    
    
    get_district_name[i] <- usage_tables[[i]][c(1:15),c(1:2)]|> 
      pivot_longer(cols = c(1,2))|>
      select(value)|>
      as.character()|>
      str_extract(regex("[\\s\\w\\-]+(?=\\sDistrict?)"))
    
    # get_usage_year[i] <- usage_tables[[i]][1 , 1] |> 
    #   str_extract("\\d+") 
    # 
    # get_district_name[i] <- usage_tables[[i]][1 , 1] |> 
    #   str_extract(".+(?=\\sDistrict)") |> 
    
    # Clean tables, remove all rows down to some keyword
    usage_tables[[i]] <- usage_tables[[i]] |>
      slice(which(usage_tables[[i]] == "ACTIVE"):n())
    
    colnames(usage_tables[[i]]) <- usage_tables[[i]][1 , ]
    
    usage_tables[[i]] <- usage_tables[[i]] |>
      select(!starts_with("NA")) |>
      slice(-1) |>
      mutate(across(!ACTIVE, as.numeric)) |> 
      clean_names()
  }
  
  return(tibble(District = get_district_name,
                Year = get_usage_year,
                ai_usage = usage_tables))
}

ai_usage_crop_group_list <- ai_usage_per_district_year(ai_usage_tables)


## generic crop groups
# districts_AS_usage_gen_full <- districts |> 
#   rename(District = NAZ_LAU1,
#          Region = NAZ_CZNUTS3) |> 
#   full_join(AS_usage_crop_group_list,
#             by =join_by(District)) |> 
#   relocate(c(District, Region, Year, AS_usage),
#            c(1, 2, 3, 4)) |> 
#   rename(AS_usage_gen = AS_usage)
# 
# districts_AS_usage_gen_full |> view()
# 
# ## detailed crop types
# districts_AS_usage_det_full <- districts |> 
#   rename(District = NAZ_LAU1,
#          Region = NAZ_CZNUTS3) |> 
#   full_join(AS_mass_crop_type_list,
#             by =join_by(District))|> 
#   relocate(c(District, Region, Year, AS_usage_det),
#            c(1, 2, 3, 4))
