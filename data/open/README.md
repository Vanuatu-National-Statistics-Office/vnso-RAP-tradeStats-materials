# Open datasets to be used in VNSO trade statistics RAP

## Suggested naming convention

**TYPE_STATUS_SOURCE_DESC_DATE.suffix**

- **TYPE**: [OPN|SEC] to show whether this is an open (OPN) or secure (SEC) dataset
- **STATUS**: [RAW|PROC|FINAL] to show whether this is a raw (RAW), processed (PROC) or final (FINAL) dataset
- **SOURCE**: where the data came from (ASY for Asycuda, for example)
- **DESC**: brief [`camelCase`](https://en.wikipedia.org/wiki/Camel_case) description of dataset
- **DATE**: [dd-mm-YY] formatted date of when dataset last updated
- **suffix**: [csv|xlsx|tsv|txt|etc.] file format abbreviation

## Dataset dictionary

Datasets currently present in secure folder:

- `OPN_FINAL_ASY_BECClassifications_31-01-20.xlsx` - **B**road **E**conomic **C**lassification (**BEC**) code classification table
- `OPN_FINAL_ASY_CountryDescriptionExportClassifications_31-01-20.xlsx` - **C**ountry **desc**ription (**C-desc**) code classification table for **EXPORTS**
- `OPN_FINAL_ASY_CountryDescriptionImportClassifications_31-01-20.xlsx` - **C**ountry **desc**ription (**C-desc**) code classification table for **IMPORTS**
- `OPN_FINAL_ASY_HSCodeClassifications_31-01-20.xlsx` - **H**armonized **S**ystem (**HS**) code classification table
- `OPN_FINAL_ASY_ModeOfTransportClassifications_31-01-20.xlsx` - **M**ode **o**f **T**ransport (**MoT**) code classifications table
- `OPN_FINAL_ASY_PrincipleCommoditiesClassifications_31-01-20.xlsx` - Principle commodeities list
- `OPN_FINAL_ASY_SITCCodeClassifications_31-01-20.xlsx` - **S**tandard **I**nternational **T**rade **C**lassification (**SITC**) code classification table
