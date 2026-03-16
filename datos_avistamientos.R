#install.packages("rgbif")
library(rgbif)
Sys.setenv(GBIF_USER = "tu usuario",
           GBIF_PWD = "tu contraseña",
           GBIF_EMAIL = "tu correo")

#datos liebre
liebre <- occ_search(scientificName = "Lepus granatensis")
data_liebre <- occ_download(pred_and(
                                      pred("taxonKey", 2436790),
                                      pred("country","ES"),
                                      pred_gte("year", 2010),
                                      pred_lte("year", 2024),
                                      pred("hasCoordinate",TRUE)
                                      ),
                            format = "SIMPLE_CSV")

occ_download_wait(data_liebre)

data_liebre <- occ_download_get(data_liebre) |>
  occ_download_import()

#metadatos liebre
metadatos_liebre <- occ_download_meta("0009923-260221153910048")
#Download key: 0009923-260221153910048
#DOI: 10.15468/dl.2hp4hw

#datos lince
lince <- occ_search(scientificName = "Lynx pardinus")
data_linces <- occ_download(pred_and(
                                      pred("taxonKey", 2435261),
                                      pred("country","ES"),
                                      pred_gte("year", 2010),
                                      pred_lte("year", 2024),
                                      pred("hasCoordinate",TRUE)
                                      ),
                            format = "SIMPLE_CSV")

occ_download_wait(data_linces)

data_linces <- occ_download_get(data_linces) |>
  occ_download_import()

#metadatos lince
metadatos_lince <- occ_download_meta("0000084-260225131425191")
#Download key: 0000084-260225131425191
#DOI: 10.15468/dl.rn7kba

#datos conejo
conejo <- occ_search(scientificName = "Oryctolagus cuniculus")

data_conejo <- occ_download(pred_and(
                              pred("taxonKey", 2436940),
                              pred("country","ES"),
                              pred_gte("year", 2010),
                              pred_lte("year", 2024),
                              pred("hasCoordinate",TRUE)
                              ),
                              format = "SIMPLE_CSV")
occ_download_wait(data_conejo)

data_conejo <- occ_download_get(data_conejo) |>
  occ_download_import()

#metadatos conejo
metadatos_conejo <- occ_download_meta("0029506-260226173443078")
#DOI: 10.15468/dl.perdd7
#Download key: 0029506-260226173443078

#citas y referencias
lepus <- read.csv("1_data/avistamientos_lepus.csv", sep = "\t")
lynx <- read.csv("1_data/avistamientos_lynx.csv", sep = "\t")
oryctolagus <- read.csv("1_data/avistamientos_oryctolagus.csv", sep = "\t")
gbif_citation("0009923-260221153910048") #liebre
gbif_citation("0000084-260225131425191") #lince
gbif_citation("0029506-260226173443078") #conejo