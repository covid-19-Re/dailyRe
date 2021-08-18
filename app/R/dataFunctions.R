getLOESSCases <- function(dates, count_data, days_incl = 21, degree = 1, truncation = 0) {

  if (truncation != 0) {
    dates <- dates[1:(length(dates) - truncation)]
    count_data <- count_data[1:(length(count_data) - truncation)]
  }

  n_points <- length(unique(dates))
  sel_span <- days_incl / n_points

  n_pad <- round(length(count_data) * sel_span * 0.5)

  c_data <- data.frame(value = c(rep(0, n_pad), count_data),
                       date_num = c(seq(as.numeric(dates[1]) - n_pad, as.numeric(dates[1]) - 1),
                                    as.numeric(dates)))
  c_data.lo <- loess(value ~ date_num, data = c_data, span = sel_span, degree = degree)
  smoothed <- predict(c_data.lo)
  smoothed[smoothed < 0] <- 0
  raw_smoothed_counts <- smoothed[(n_pad + 1):length(smoothed)]
  normalized_smoothed_counts <-
    raw_smoothed_counts * sum(count_data, na.rm = T) / sum(raw_smoothed_counts, na.rm = T)

  if (truncation != 0) {
    normalized_smoothed_counts <- append(normalized_smoothed_counts, rep(NA, truncation))
  }
  return(normalized_smoothed_counts)
}

dataUpdatesString <- function(latestData, name = "Data Source", dateFormat = "%Y-%m-%d") {
  latestDataSum <- latestData %>%
    mutate(lastChanged = format(lastChanged, dateFormat)) %>%
    group_by(source, lastChanged) %>%
    summarize(
      data_type = str_c(data_type, collapse = ", "),
      .groups = "keep")

  outList <- list(str_c(name, ": "))
  nSources <- dim(latestDataSum)[1]
  if (nSources == 1) {
    outList[[2]] <- str_c(
      latestDataSum$source, ", ", latestDataSum$lastChanged,
      "; ")
  } else {
    for (i in seq_len(nSources)) {
      outList[[i + 1]] <- str_c(
        latestDataSum[i, ]$source, ", ", latestDataSum[i, ]$lastChanged,
        " (", latestDataSum[i, ]$data_type, ")",
        "; ")
    }
  }
  return(str_sub(str_c(outList, collapse = ""), end = -3))
}


# new data function -> move to data repo
downloadTemporaryFile <- function(url, tempFile, tReload = 300) {
  if (is.null(tempFile)) {
    path <- url
  } else {
    fileMod <- file.mtime(tempFile)
    fileReload <- if_else(file.exists(tempFile), now() > fileMod + minutes(tReload), TRUE)
    if (fileReload) {
      downloadOK <- try(download.file(url, destfile = tempFile))
      if ("try-error" %in% class(downloadOK)) {
        if (file.exists(tempFile)) {
          warning("Couldn't fetch file from ", url, ". Using data from ", fileMod)
        } else {
          warning("Couldn't fetch file from ", url, ".")
          return(NULL)
        }
      }
    }
    path <- tempFile
  }
  return(path)
}

getMobilityDataGoogle <- function(countries = NULL, tempFile = NULL, tReload = 300) {
  url <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
  csvPath <- downloadTemporaryFile(url, tempFile, tReload)

  rawData <- try(read_csv(csvPath,
    na = c(""),
    col_types = cols_only(
      country_region_code = col_character(),
      sub_region_1 = col_character(),
      sub_region_2 = col_character(),
      metro_area = col_character(),
      date = col_date(format = ""),
      retail_and_recreation_percent_change_from_baseline = col_double(),
      grocery_and_pharmacy_percent_change_from_baseline = col_double(),
      parks_percent_change_from_baseline = col_double(),
      transit_stations_percent_change_from_baseline = col_double(),
      workplaces_percent_change_from_baseline = col_double(),
      residential_percent_change_from_baseline = col_double()
    )
  ))

  if ("try-error" %in% class(rawData)) {
    warning(str_c("couldn't get Google Mobility Data data from ", url, "."))
    return(NULL)
  }

  longData <- rawData %>%
    filter(is.na(sub_region_2), is.na(metro_area)) %>%
    mutate(
      countryIso3 = countrycode::countrycode(country_region_code, origin = "iso2c", destination = "iso3c")
    ) %>%
    left_join(
      tribble(
        ~sub_region_1, ~sub_region_new,
        "Aargau", "AG",
        "Appenzell Ausserrhoden", "AR",
        "Appenzell Innerrhoden", "AI",
        "Basel City", "BS",
        "Basel-Landschaft", "BL",
        "Canton of Bern", "BE",
        "Canton of Zug", "ZG",
        "Fribourg", "FR",
        "Geneva", "GE",
        "Glarus", "GL",
        "Grisons", "GR",
        "Jura", "JU",
        "Lucerne", "LU",
        "Neuchâtel", "NE",
        "Nidwalden", "NW",
        "Obwalden", "OW",
        "Schaffhausen", "SH",
        "Schwyz", "SZ",
        "Solothurn", "SO",
        "St. Gallen", "SG",
        "Thurgau", "TG",
        "Ticino", "TI",
        "Uri", "UR",
        "Valais", "VS",
        "Vaud", "VD",
        "Zurich", "ZH"
      ),
      by = "sub_region_1"
    ) %>%
    mutate(
      sub_region_1 = if_else(is.na(sub_region_new), sub_region_1, sub_region_new )
    ) %>%
    transmute(
      date,
      countryIso3,
      region = if_else(is.na(sub_region_1), countryIso3, sub_region_1),
      retail_and_recreation = retail_and_recreation_percent_change_from_baseline / 100,
      grocery_and_pharmacy = grocery_and_pharmacy_percent_change_from_baseline / 100,
      parks = parks_percent_change_from_baseline / 100,
      transit_stations = transit_stations_percent_change_from_baseline / 100,
      workplaces = workplaces_percent_change_from_baseline / 100,
      residential = residential_percent_change_from_baseline / 100
    ) %>%
    pivot_longer(retail_and_recreation:residential, names_to = "placeCategory", values_to = "change")

  if (!is.null(countries)) {
    longData <- longData %>%
      filter(countryIso3 %in% countries)
  }
  return(longData)
}

getMobilityDataApple <- function(countries = NULL, tempFile = NULL, tReload = 300) {
  urlsJson <- jsonlite::fromJSON("https://covid19-static.cdn-apple.com/covid19-mobility-data/current/v3/index.json")
  url <- str_c("https://covid19-static.cdn-apple.com", urlsJson$basePath, urlsJson$regions$`en-us`$csvPath)
  csvPath <- downloadTemporaryFile(url, tempFile, tReload)

  rawData <- try(read_csv(csvPath,
    na = c(""),
    col_types = cols(
      geo_type = col_character(),
      region = col_character(),
      transportation_type = col_character(),
      alternative_name = col_character(),
      `sub-region` = col_character(),
      country = col_character(),
      .default = col_double()
    )
  ))

  if ("try-error" %in% class(rawData)) {
    warning(str_c("couldn't get Apple Mobility data from ", url, "."))
    return(NULL)
  }

  longData <- rawData %>%
    filter(geo_type %in% c("country/region", "sub-region")) %>%
    select(-alternative_name, -`sub-region`) %>%
    mutate(
      country = if_else(geo_type == "country/region", region, country),
      countryIso3 = countrycode::countrycode(country, origin = "country.name", destination = "iso3c"),
      region = if_else(geo_type == "country/region", countryIso3, region)) %>%
    left_join(
      tribble(
        ~region, ~region_new,
        "Canton of Aargau", "AG",
        "Canton of Appenzell Ausserrhoden", "AR",
        "Canton of Appenzell Innerrhoden", "AI",
        "Canton of Basel-Stadt", "BS",
        "Canton of Basel-Landschaft", "BL",
        "Canton of Bern", "BE",
        "Canton of Zug", "ZG",
        "Canton of Fribourg", "FR",
        "Canton of Geneva", "GE",
        "Canton of Glarus", "GL",
        "Canton of Graubünden", "GR",
        "Canton of Jura", "JU",
        "Canton of Lucerne", "LU",
        "Canton of Neuchâtel", "NE",
        "Canton of Nidwalden", "NW",
        "Canton of Obwalden", "OW",
        "Canton of Schaffhausen", "SH",
        "Canton of Schwyz", "SZ",
        "Canton of Solothurn", "SO",
        "Canton of St. Gallen", "SG",
        "Canton of Thurgau", "TG",
        "Canton of Ticino", "TI",
        "Canton of Uri", "UR",
        "Canton of Valais", "VS",
        "Canton of Vaud", "VD",
        "Canton of Zürich", "ZH"
      ),
      by = "region"
    ) %>%
    mutate(
      region = if_else(is.na(region_new), region, region_new)
    ) %>%
    select(-region_new) %>%
    pivot_longer(
      cols = !c(geo_type, region, transportation_type, country, countryIso3),
      names_to = "date",
      names_transform = list(date = as.Date),
      values_to = "percent") %>%
    transmute(
      date,
      countryIso3,
      region,
      transportationType = transportation_type,
      percent,
      change = (percent - 100) / 100
    )

  if (!is.null(countries)) {
    longData <- longData %>%
      filter(countryIso3 %in% countries)
  }

  return(longData)
}
