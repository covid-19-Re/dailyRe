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

# interventionsData <- qread("data/serialized/interventionsData.qs")

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
# allMobilityData <- getMobilityDataGoogle(tempFileName = "data/temp/mobilityDataGoogle.csv", tReload = 24*60*60)
# qsave(mobilityData, "data/serialized/allMobilityData.qs")
allMobilityData <- qread("data/serialized/allMobilityData.qs") %>%
  mutate(placeCategory = placeCategory %>% 
    str_replace_all("_", " ") %>%
    str_to_title(),
    change = change * 100 )

allStringencyData <- allData$caseData %>%
  select(-c(populationSize, local_infection:testPositivity)) %>%
  filter(data_type == "Stringency Index")
