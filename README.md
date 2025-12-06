# Soil-WAter Quality model (SWAQ)

## There are three main types of files:

1) *_AppPatt.R
Used for building country specific Gemup (GEnaralised Mapped Usage Patterns). Only for CZ there is also CZ_PPPusage.R used for cleaning PPP usage data.

2) *_swaq_farm.R
Used for calculating PEC and RQ at field level. Only CZ script is completed for the moment but NL and DK should follow similar template.

3) *_swaq_bas.R
Used for calculating PEC and RQ at river catchment level. All scripts are completed. Only NL includes spray drift tables, also, only NL results are split by river length and breadth groups.
