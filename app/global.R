library(tidyverse)
library(here)
library(qs)
library(echarts4r)
library(shinyjs)
library(lubridate)
library(slider)
library(shinycssloaders)
library(sf)
library(leaflet)
library(plotly)

reConfig <- config::get(value = "re")

# set up translation
library(shiny.i18n)
translator <- Translator$new(translation_json_path = file.path("data/covid19reTranslations.json"))
languageSelect <- translator$get_languages()
names(languageSelect) <- c("EN", "IT", "FR", "DE")

# load data
allData <- qread("data/serialized/allCountryData.qs")

continents <- read_csv("data/continents.csv", col_types = cols(.default = col_character()))
countryList <- qread("data/serialized/countryList.qs")

allVaccinationData <- qread("data/serialized/vaccinationData.qs") %>%
  filter(str_detect(data_type, "_per_hundred")) %>%
  mutate(data_type = data_type %>%
    str_remove("_per_hundred") %>%
    str_replace_all("_", " ") %>%
    str_to_title())

updateDataRaw <-  qread("data/serialized/updateDataRaw.qs")
dataSources <-  qread("data/serialized/dataSources.qs")

# Map
worldMapData <-  qread("data/serialized/worldMapData.qs")
countriesShape <-  qread("data/serialized/countriesShape.qs")
CHEregionsShape <-  qread("data/serialized/CHEregionsShape.qs")
cheCasesLabels <-  qread("data/serialized/cheCasesLabels.qs")
cheReLabels <-  qread("data/serialized/cheReLabels.qs")

ZAFregionsShape <-  qread("data/serialized/ZAFregionsShape.qs")
zafCasesLabels <-  qread("data/serialized/zafCasesLabels.qs")
zafReLabels <-  qread("data/serialized/zafReLabels.qs")

## New Stuff -> integrate into data gathering scripts

allIncidenceData <- allData$caseData %>%
  filter(data_type != "Stringency Index") %>%
  select(-local_infection)

allEstimateData <- allData$estimates

# source("R/dataFunctions.R")
# countriesWithRegions <- allData$caseData %>%
#   filter(data_type != "Stringency Index") %>%
#   select(-local_infection) %>%
#   select(countryIso3, region) %>%
#   group_by(countryIso3) %>%
#   summarise(nRegions = length(unique(region))) %>%
#   filter(nRegions > 1)

# allMobilityDataGoogle <- getMobilityDataGoogle(tempFile = "data/temp/mobilityDataGoogle.csv", tReload = 8 * 60 * 60) %>%
#   mutate(data_type = placeCategory %>%
#     str_replace_all("_", " ") %>%
#     str_to_title(),
#     change = change * 100) %>%
#   filter(countryIso3 == region | countryIso3 %in% countriesWithRegions$countryIso3) %>%
#   select(-placeCategory)
# qsave(allMobilityDataGoogle, "data/serialized/allMobilityDataGoogle.qs")

# allMobilityDataApple <- getMobilityDataApple(tempFile = "data/temp/mobilityDataApple.csv", tReload = 8 * 60 * 60) %>%
#   mutate(
#     data_type = str_to_title(transportationType),
#     change = change * 100) %>%
#   select(-percent, -transportationType) %>%
#   filter(countryIso3 == region | countryIso3 %in% countriesWithRegions$countryIso3) 
# qsave(allMobilityDataApple, "data/serialized/allMobilityDataApple.qs")

allMobilityDataGoogle <- qread("data/serialized/allMobilityDataGoogle.qs")
allMobilityDataApple <- qread("data/serialized/allMobilityDataApple.qs")

allStringencyData <- allData$caseData %>%
  select(-c(populationSize, local_infection:testPositivity)) %>%
  filter(data_type == "Stringency Index")
