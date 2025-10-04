chemprop_gen <- function(Active) {
  
  # Load the necessary libraries
  pkg <- c("rvest", "tidyverse", "stringr", "readxl")

  for (i in pkg) {
    if (!requireNamespace(i, quietly = TRUE)) {
      install.packages(i)
    }
  }

  lapply(pkg, library, character.only = T)

  
  # Extract the http addresses from the indexed chemicals in the PPDB
  ppdb_url <- "https://sitem.herts.ac.uk/aeru/ppdb/en/atoz.htm"
  ppdb_html <- read_html(ppdb_url)
  ppdb_repo_chem <-  cbind(tibble(report_nr = as.character(ppdb_html |> html_elements("p a") |> html_attr("href"))),
                           tibble(chem_name = ppdb_html |> html_elements("p a") |> html_text2())) |> as_tibble() |>
    mutate(chem_http = paste0("https://sitem.herts.ac.uk/aeru/ppdb/en/", report_nr)) |>
    select(-report_nr) |>
    slice(-1) |>
    # It seems that some chems are not indexed although available in the database, and some bacterial-based and other are not included in the PPDB: Spinosad, Abamectin, Pelargonic acid
    full_join(tibble(chem_name = c("Sulphur", "6-benzyladenine"),
                     chem_http = c("https://sitem.herts.ac.uk/aeru/ppdb/en/Reports/605.htm",
                                     "https://sitem.herts.ac.uk/aeru/ppdb/en/Reports/1324.htm")),
              by = join_by(chem_name, chem_http))
  
  # Create List of active substances from the priority list. For the moment it is list based on usage data in CZ
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
           "propamocarb") |>
    str_to_sentence() |> 
    str_replace("-p-", "-P-") |>
    str_replace("-p", "-P") |>
    str_replace("-d", "-D") |>
    str_replace("-m$", "-M")
  
  ASs[which(ASs == "Mcpb")] <- "MCPB"
  ASs[which(ASs == "Mcpa")] <- "MCPA"
  
  ASs <- ASs[which(ASs %in% Active)]

  # Extract the properties of the chemicals
  ppdb_df_prop <- list()
  for (as in seq_along(ASs)) {
    
    # Create empty lists to store indexes of the properties to be extracted
    dt50field_index <- list()
    dt50typical_index <- list()
    CAS_index <- list()
    smiles_index<- list()
    sol_water_20_index <- list()
    koc_index<- list()
    kfoc_index <- list()
    kd_index <- list()
    logP_index <- list()
    noec_eworm_ch_repro_index <- list()
    lc50_eworm_ac_14d_index <- list()
    noec_fish_ch_21d_index <- list()
    lc50_fish_ac_96h_index <- list()
    ppdb_df <- list()
    ppdb_df_values <- list()
    
    # Extract all the properties for each pesticide into one table
    ppdb_df[[as]] <- ppdb_repo_chem |>
      filter(str_detect(chem_name, paste0("^",ASs[as], "$"))) |>
      pull(2) |> 
      read_html() |>
      html_elements(".row_header, .data1, .data3") |> 
      html_text2() |> 
      as_tibble()
    
    # Read index number for the properties based on table/row headers
    dt50field_index[[as]] <- which(ppdb_df[[as]] == regex("DT₅₀ (field)")) + 1
    dt50typical_index[[as]] <- which(ppdb_df[[as]] == regex("DT₅₀ (typical)")) + 1
    CAS_index[[as]] <- which(ppdb_df[[as]] == regex("CAS RN")) + 1
    smiles_index[[as]] <- which(ppdb_df[[as]] == regex("Canonical SMILES")) + 1
    sol_water_20_index[[as]] <- which(ppdb_df[[as]] == regex("Solubility - In water at 20 °C (mg l⁻¹)")) + 1
    koc_index[[as]] <- which(ppdb_df[[as]] == regex("Koc (mL g⁻¹)")) + 1
    kfoc_index[[as]] <- which(ppdb_df[[as]] == regex("Kfoc (mL g⁻¹)")) + 1
    kd_index[[as]] <- which(ppdb_df[[as]] == regex("Kd (mL g⁻¹)")) + 1
    logP_index[[as]] <- which(ppdb_df[[as]] == regex("Log P")) + 1
    noec_eworm_ch_repro_index[[as]] <- which(ppdb_df[[as]] == regex("Earthworms - Chronic NOEC, reproduction (mg kg⁻¹ dw soil)")) + 1
    lc50_eworm_ac_14d_index[[as]] <-  which(ppdb_df[[as]] == regex("Earthworms - Acute 14 day LC₅₀ (mg kg⁻¹ dw soil)")) + 1
    noec_fish_ch_index[[as]] <- which(ppdb_df[[as]] == regex("Temperate Freshwater Fish - Chronic 21 day NOEC (mg l⁻¹)")) + 1
    lc50_fish_ac_96h_index[[as]] <-  which(ppdb_df[[as]] == regex("Temperate Freshwater Fish - Acute 96 hour LC₅₀ (mg l⁻¹)")) + 1
    
    # Extract the values of the selected properties
    ppdb_df_values[[as]] <- ppdb_df[[as]] |> slice(CAS_index[[as]],
                                                   smiles_index[[as]],
                                                   sol_water_20_index[[as]],
                                                   dt50field_index[[as]],
                                                   dt50typical_index[[as]],
                                                   logP_index[[as]],
                                                   koc_index[[as]],
                                                   kfoc_index[[as]],
                                                   kd_index[[as]],
                                                   noec_eworm_ch_repro_index[[as]],
                                                   lc50_eworm_ac_14d_index[[as]],
                                                   noec_fish_ch_index[[as]],
                                                   lc50_fish_ac_96h_index[[as]])

        # Create a table with the extracted properties
    ppdb_df_prop[[as]] <- tibble(Active = ASs[as],
                                 "CAS" = ppdb_df_values[[as]][[1,1]],
                                 "SMILES" = ppdb_df_values[[as]][[2,1]],
                                 "solub_water_20_mg.L" = ppdb_df_values[[as]][[3,1]],
                                 "DT50_field_d" = ppdb_df_values[[as]][[4,1]],
                                 "DT50_typical_d" = ppdb_df_values[[as]][[5,1]],
                                 "logP_L.L" = ppdb_df_values[[as]][[6,1]],
                                 "Koc_ml.g" = ppdb_df_values[[as]][[7,1]],
                                 "Kfoc_ml.g" = ppdb_df_values[[as]][[8,1]],
                                 "Kd_soil_ml.g" = ppdb_df_values[[as]][[9,1]],
                                 "NOEC_earthworm_chron_repr_mg.kg" = ppdb_df_values[[as]][[10,1]],
                                 "LC50_earthworm_acute_14d_mg.kg" = ppdb_df_values[[as]][[11,1]],
                                 "NOEC_fish_21_mg.L" = ppdb_df_values[[as]][[12,1]],
                                 "LC50_fish_acute_96h_mg.kg" = ppdb_df_values[[as]][[13,1]])
    
    
    cat("\r", ASs[as], "properties are being extracted.", length(ASs) - which(ASs == ASs[as]), "chemicals left.")
    
    
  }
  
  chemprop_sel <- ppdb_df_prop |>
    bind_rows() |>
    mutate(NOEC_earthworm_chron_repr_mg.kg = str_remove_all(NOEC_earthworm_chron_repr_mg.kg, pattern = "[><=\\s]"),
           LC50_earthworm_acute_14d_mg.kg = str_remove_all(LC50_earthworm_acute_14d_mg.kg, pattern = "[><=\\s]"),
           NOEC_fish_21_mg.L = str_remove_all(NOEC_fish_21_mg.L, pattern = "[><=\\s]"),
           LC50_fish_acute_96h_mg.kg = str_remove_all(LC50_fish_acute_96h_mg.kg, pattern = "[><=\\s]"))

    
  return(chemprop_sel)
  
  # Save poperties extracted from the PPDB for all pesticides
  # write_excel_csv2(ppdb_df_prop |> bind_rows(),"chem_prop_ppdb.csv")
  
}





