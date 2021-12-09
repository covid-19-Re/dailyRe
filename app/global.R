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

reConfig <- config::get()

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
cheRegionsShape <-  qread("data/serialized/cheRegionsShape.qs")
cheCasesLabels <-  qread("data/serialized/cheCasesLabels.qs")
cheReLabels <-  qread("data/serialized/cheReLabels.qs")

zafRegionsShape <-  qread("data/serialized/zafRegionsShape.qs")
zafCasesLabels <-  qread("data/serialized/zafCasesLabels.qs")
zafReLabels <-  qread("data/serialized/zafReLabels.qs")

## New Stuff -> integrate into data gathering scripts

allIncidenceData <- allData$caseData %>%
  filter(data_type != "Stringency Index") %>%
  select(-local_infection)

plotMinX <- as_datetime("2020-02-01", tz = "UTC")
plotMaxX <- as_datetime(max(allIncidenceData$date))

allEstimateData <- allData$estimates

googleMobilityData <- qread("data/serialized/allMobilityDataGoogle.qs") %>%
    mutate(source = "Google Mobility Data")
googleMobilityDataTypes <- unique(googleMobilityData$data_type)

appleMobilityData <- qread("data/serialized/allMobilityDataApple.qs") %>%
    mutate(source = "Apple Mobility Data")
appleMobilityDataTypes <- unique(appleMobilityData$data_type)

allMobilityData <- bind_rows(
  googleMobilityData,
  appleMobilityData
)

allStringencyData <- allData$caseData %>%
  select(-c(populationSize, local_infection:testPositivity)) %>%
  filter(data_type == "Stringency Index")
