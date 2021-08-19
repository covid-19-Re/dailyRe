server <- function(input, output, session) {

# Translation
  i18n <- reactive({
    selected <- input$lang
    if (length(selected) > 0 && selected %in% translator$get_languages()) {
      translator$set_translation_language(selected)
    }
    # restore selected tabs
    updateNavbarPage(session, "tabs", selected = stateVals$tabs)
    if (stateVals$tabs == "map") {
      mapPlot <- leafletProxy("mapPlot")
      mapPlot %>%
        setView(
          lng = stateVals$mapViewCenter$lng, lat = stateVals$mapViewCenter$lat,
          zoom = stateVals$mapViewZoom)
    }
    return(translator)
  })

  stateVals <- reactiveValues(
    lang = "en-gb",
    tabs = "timeseries",
    mapViewCenter = list(lng = 24.78515, lat =  33.72436),
    mapViewZoom = 2,
    regionCountrySelect = NULL)

  observeEvent(input$lang, {
    stateVals$lang <- input$lang
    stateVals$tabs <- input$tabs
    if (!is.null(input$mapPlot_center)) {
      stateVals$mapViewCenter <- input$mapPlot_center
      stateVals$mapViewZoom <- input$mapPlot_zoom
      stateVals$mapViewGroups <- input$mapPlot_groups
      stateVals$regionCountrySelect <- input$regionCountrySelect
    }
  })

# reactive data

  selectedRegion <- reactive({
    if (is.null(input$regionSelect)) {
      return(input$countrySelect)
    } else {
      return(input$regionSelect)
    }
  })

  multipleRegions <- reactive({
    return(length(selectedRegion()) > 1)
  })

  incidenceData <- reactive({
    req(input$countrySelect)

    incidenceData <- filter(allIncidenceData,
      countryIso3 %in% input$countrySelect)

    return(incidenceData)
  })

  incidenceDataPlot <- reactive({

    incidenceData <- incidenceData()
    selectedRegion <- selectedRegion()

    if (multipleRegions()) {
      req(input$dataTypeSelect)
      incidenceData <- incidenceData %>%
        filter(
          data_type == input$dataTypeSelect,
          region %in% selectedRegion
        ) %>%
        transmute(
          date = date,
          countryIso3 = countryIso3,
          populationSize = populationSize,
          region = region,
          data_type = data_type,
          series = region,
          value = value,
          deconvoluted = deconvoluted,
          deconvolutedLow = deconvolutedLow,
          deconvolutedHigh = deconvolutedHigh
        )
    } else {
      incidenceData <- incidenceData %>%
        filter(
          region %in% selectedRegion
        ) %>%
        transmute(
          date = date,
          countryIso3 = countryIso3,
          populationSize = populationSize,
          region = region,
          data_type = data_type,
          series = data_type,
          value = value,
          deconvoluted = deconvoluted,
          deconvolutedLow = deconvolutedLow,
          deconvolutedHigh = deconvolutedHigh
          )
    }

    if (input$incidenceSmoothing > 0) {
      incidenceData <- incidenceData %>%
        group_by(region, data_type) %>%
        mutate(
          value = slide_index_dbl(value, date, mean, .before = days(as.integer(input$incidenceSmoothing) - 1))
        )
    }

    if (input$incidenceNormalization) {
      incidenceData <- incidenceData %>%
        mutate(value = value / populationSize * 100000)
    }

    return(incidenceData)
  })

  rightTruncation <- reactive({
    req(input$countrySelect)
    incidenceData <- incidenceDataPlot()

    rightTruncation <- lapply(input$countrySelect, function(iso3) {
      if (iso3 %in% c("CHE", "LIE") && reConfig$CHErightTruncation) {
        max_date <- max(incidenceData$date)
        additionalTruncation <- case_when(
          lubridate::wday(max_date) == 3 ~ 1, # 3 = Tue, exclude Sat,
          lubridate::wday(max_date) == 4 ~ 2, # 4 = Wed, exclude Sun and Sat,
          lubridate::wday(max_date) == 5 ~ 3, # 5 = Thu, exclude Mon, Sun and Sat,
          TRUE ~ 0                            # otherwise don't exclude more days
        )

        rt <- list(
          "Confirmed cases" = 3 + additionalTruncation,
          "Confirmed cases / tests" = 3 + additionalTruncation,
          "Hospitalized patients" = 5,
          "Deaths" = 5)
      } else {
        rt <- list(
          "Confirmed cases" = 3,
          "Confirmed cases / tests" = 3,
          "Hospitalized patients" = 3,
          "Deaths" = 3)
      }
      return(rt)
    })

    names(rightTruncation) <- input$countrySelect
    return(rightTruncation)
  })

  updateData <- reactive({
      updateData <- bind_rows(updateDataRaw[input$countrySelect]) %>%
        ungroup()
      return(updateData)
    })

  estimateData <- reactive({
    req(input$countrySelect)

    selectedCountry <- input$countrySelect
    estimateData <- filter(allEstimateData,
      countryIso3 %in% selectedCountry)

    return(estimateData)
  })

  estimateDataPlot <- reactive({
    estimateData <- estimateData()
    selectedRegion <- selectedRegion()

    estimateDataPlot <- estimateData %>%
      filter(estimate_type == input$estimationType)

    if (multipleRegions()) {
      req(input$dataTypeSelect)
      estimateDataPlot <- estimateDataPlot %>%
        filter(
          data_type == input$dataTypeSelect,
          region %in% selectedRegion
        ) %>%
        transmute(
          date = date,
          countryIso3 = countryIso3,
          region = region,
          data_type = data_type,
          series = region,
          mean = median_R_mean,
          low = median_R_lowHPD,
          high = median_R_highHPD
        ) %>%
        group_by(series)
    } else {
      estimateDataPlot <- estimateDataPlot %>%
        filter(
          region %in% selectedRegion
        ) %>%
        transmute(
          date = date,
          countryIso3 = countryIso3,
          region = region,
          data_type = data_type,
          series = data_type,
          mean = median_R_mean,
          low = median_R_lowHPD,
          high = median_R_highHPD
        ) %>%
        group_by(series)
    }
    return(estimateDataPlot)
  })

  mobilityDataGoogle <- reactive({
    req(input$countrySelect)

    selectedCountry <- input$countrySelect
    mobilityDataGoogle <- allMobilityDataGoogle %>%
      filter(countryIso3 %in% selectedCountry)

    return(mobilityDataGoogle)
  })

  mobilityDataGooglePlot <- reactive({
    mobilityDataGoogle <- mobilityDataGoogle()
    selectedRegion <- selectedRegion()

    if (multipleRegions()) {
      mobilityDataGoogle <- mobilityDataGoogle %>%
        filter(
          data_type == input$googleMobilityType[1],
          region %in% selectedRegion
        ) %>%
        transmute(
          date = date,
          region = region,
          data_type = data_type,
          header = header_formatter(str_c("Mobility: ", input$googleMobilityType[1])),
          series = region,
          value = change
        ) %>%
        group_by(series)
    } else {
      mobilityDataGoogle <- mobilityDataGoogle %>%
        filter(
          data_type %in% input$googleMobilityType,
          region %in% selectedRegion
        ) %>%
        transmute(
          date = date,
          region = region,
          header = header_formatter(i18n()$t("Mobility")),
          data_type = data_type,
          series = data_type,
          value = change
          ) %>%
        group_by(series)
    }

    if (input$mobilitySmoothingGoogle > 0) {
      mobilityDataGoogle <- mobilityDataGoogle %>%
        group_by(region, data_type) %>%
        mutate(
          value = slide_index_dbl(value, date, mean, .before = days(as.integer(input$mobilitySmoothingGoogle) - 1))
        )
    }

    return(mobilityDataGoogle)
  })

  mobilityDataApple <- reactive({
    req(input$countrySelect)

    selectedCountry <- input$countrySelect
    mobilityDataApple <- allMobilityDataApple %>%
      filter(countryIso3 %in% selectedCountry)

    return(mobilityDataApple)
  })

  mobilityDataApplePlot <- reactive({
    mobilityDataApple <- mobilityDataApple()
    selectedRegion <- selectedRegion()

    if (multipleRegions()) {
      mobilityDataApple <- mobilityDataApple %>%
        filter(
          data_type == input$appleMobilityType[1],
          region %in% selectedRegion
        ) %>%
        transmute(
          date = date,
          region = region,
          data_type = data_type,
          header = header_formatter(str_c("Mobility: ", input$appleMobilityType[1])),
          series = region,
          value = change
        ) %>%
        group_by(series)
    } else {
      mobilityDataApple <- mobilityDataApple %>%
        filter(
          data_type %in% input$appleMobilityType,
          region %in% selectedRegion
        ) %>%
        transmute(
          date = date,
          region = region,
          header = header_formatter(i18n()$t("Mobility")),
          data_type = data_type,
          series = data_type,
          value = change
          ) %>%
        group_by(series)
    }

    if (input$mobilitySmoothingApple > 0) {
      mobilityDataApple <- mobilityDataApple %>%
        group_by(region, data_type) %>%
        mutate(
          value = slide_index_dbl(value, date, mean, .before = days(as.integer(input$mobilitySmoothingApple) - 1))
        )
    }

    return(mobilityDataApple)
  })

  vaccinationData <- reactive({
    req(input$countrySelect)

    selectedCountry <- input$countrySelect
    vaccinationData <- allVaccinationData %>%
      filter(
        countryIso3 %in% selectedCountry
        )

    return(vaccinationData)
  })

  vaccinationDataPlot <- reactive({
    vaccinationData <- vaccinationData()
    selectedRegion <- selectedRegion()
    hasVaccDataForRegions <- length(unique(vaccinationData$region)) > 1

    if (multipleRegions() && hasVaccDataForRegions) {
      vaccinationData <- vaccinationData %>%
        filter(
          data_type == input$vaccinationDataType[1],
          region %in% selectedRegion
        ) %>%
        transmute(
          date = date,
          countryIso3 = countryIso3,
          region = region,
          data_type = data_type,
          header = header_formatter(i18n()$t(data_type)),
          series = region,
          value = value
        ) %>%
        group_by(series)
    } else {
      vaccinationData <- vaccinationData %>%
        filter(
          region %in% selectedRegion,
          data_type %in% input$vaccinationDataType,
        ) %>%
        transmute(
          date = date,
          countryIso3 = countryIso3,
          region = region,
          data_type = data_type,
          header = "",
          series = data_type,
          value = value
          ) %>%
        group_by(series)
    }
    return(vaccinationData)
  })

  stringencyData <- reactive({
    req(input$countrySelect)

    selectedCountry <- input$countrySelect
    stringencyData <- allStringencyData %>%
      filter(
        countryIso3 %in% selectedCountry
        )

    return(stringencyData)
  })

  stringencyDataPlot <- reactive({
    incidenceData <- incidenceDataPlot()
    stringencyData <- stringencyData()
    selectedRegion <- selectedRegion()
    hasVaccDataForRegions <- length(unique(stringencyData$region)) > 1

    if (multipleRegions() && hasVaccDataForRegions) {
      stringencyData <- stringencyData %>%
        filter(
          region %in% selectedRegion
        ) %>%
        transmute(
          date = date,
          countryIso3 = countryIso3,
          region = region,
          data_type = data_type,
          header = header_formatter(i18n()$t("Stringency Index")),
          series = region,
          value = value
        ) %>%
        group_by(series)
    } else {
      stringencyData <- stringencyData %>%
        filter(
          region %in% selectedRegion,
        ) %>%
        transmute(
          date = date,
          countryIso3 = countryIso3,
          region = region,
          data_type = data_type,
          header = "",
          series = data_type,
          value = value
          ) %>%
        group_by(series)
    }
    return(stringencyData)
  })

  availableRegions <- eventReactive(input$countrySelect, {

    incidenceData <- incidenceData()
    availableRegions <- unique(incidenceData$region)

    if (length(input$countrySelect) == 1) {
      if (input$countrySelect == "CHE") {
        cantons <- availableRegions[str_detect(availableRegions, "^.{2}$")]
        names(cantons) <- cantons

        grR <- availableRegions[str_detect(availableRegions, "^grR ")]
        grRnames <- str_remove(grR, "grR ")
        names(grR) <- grRnames

        seR <- availableRegions[str_detect(availableRegions, "^seR ")]
        seRnames <- str_remove(seR, "seR ")
        names(seR) <- seRnames

        availableRegions <- list(
          Country = c("Whole country" = input$countrySelect),
          Canton = cantons,
          "Greater Region" = grR,
          "Sentinella Regions" = seR
        )
      } else if (input$countrySelect == "ZAF") {
        availableRegions <- list(
          Country = c("Whole country" = input$countrySelect),
          Province = availableRegions[!(availableRegions %in% c("ZAF", "Unknown"))]
        )
      } else {
        availableRegions <- list(
          Country = c("Whole country" = input$countrySelect),
          Subdivision = availableRegions[!(availableRegions %in% input$countrySelect)]
        )
      }
    } else {
      countryVector <- input$countrySelect
      names(countryVector) <- input$countrySelect
      availableRegions <- list(
        CountryIso3 = countryVector,
        Subdivision = "")
    }
    return(availableRegions)
  })

  # plots
  rightMarginP <- "5%" # right margin in %
  axisLabelSize <- 15
  axisLabelWeight <- 300


  output$incidencePlot <- renderEcharts4r({
    incidenceData <- incidenceDataPlot()
    selectedRegion <- selectedRegion()
    rightTruncation <- rightTruncation()

    incidenceDataTruncated <- incidenceData %>%
      group_by(countryIso3, series, data_type) %>%
      dplyr::filter(date <= (max(date) - rightTruncation[[unique(countryIso3)]][[unique(data_type)]])) %>%
      arrange(date, countryIso3, region, data_type) %>%
      group_by(series)

    incidenceDataRest <- incidenceData %>%
      group_by(countryIso3, series, data_type) %>%
      dplyr::filter(date > (max(date) - rightTruncation[[unique(countryIso3)]][[unique(data_type)]])) %>%
      arrange(date, countryIso3, region, data_type) %>%
      group_by(series)

    yAxisLabel <- i18n()$t("New observations")

    if (input$incidenceLogAxis) {
      yAxisType <- "log"
      ymin <- 10 ^ round(log10(min(incidenceData$value[incidenceData$value != 0])))
      ymax <- max(incidenceData$value)
    } else {
      yAxisType <- "value" 
      ymin <- NULL
      ymax <- NULL
    }

    if (input$incidenceNormalization) {
      yAxisLabel <- str_c(yAxisLabel, " / 100'000")
    }

    plot <- incidenceDataTruncated %>%
      group_by(series) %>%
      e_charts(x = date) %>%
      e_bar(serie = value, cursor = "default") %>%
      e_data(incidenceDataRest) %>%
      e_bar(serie = value, cursor = "default", itemStyle = list(opacity = 0.3))

    if (input$incidenceDeconvolution) {
      plot <- plot %>%
        e_data(incidenceDataTruncated) %>%
        e_line(serie = deconvoluted, symbol = "none", cursor = "default",
          tooltip = list(show = FALSE), selectedMode = FALSE) %>%
        e_band2(
          lower = deconvolutedLow, upper = deconvolutedHigh, cursor = "default",
          itemStyle = list(borderWidth = 0, opacity = 0.5),
          tooltip = list(show = FALSE), selectedMode = FALSE)
    }

    if (input$incidenceLoess) {
      incidenceDataTruncatedLoess <- incidenceDataTruncated %>%
        group_by(countryIso3, region, data_type) %>%
        mutate(value = getLOESSCases(date, value)) %>%
        group_by(series)

      plot <- plot %>%
        e_data(incidenceDataTruncatedLoess) %>%
        e_line(serie = value, symbol = "none", cursor = "default",
          tooltip = list(show = FALSE), selectedMode = FALSE)
    }

    plot <- plot %>%
      e_grid(right = rightMarginP) %>%
      e_tooltip(
        trigger = "axis",
        formatter = e_tooltip_incidence_formatter(
          style = "decimal", digits = 2, locale = input$lang)) %>%
      e_y_axis(
        type = yAxisType,
        scale = FALSE,
        min = ymin,
        max = ymax,
        name = yAxisLabel,
        nameTextStyle = list(fontSize = axisLabelSize, fontWeight = axisLabelWeight),
        nameGap = 50,
        nameLocation = "middle") %>%
      e_datazoom(x_index = 0) %>%
      e_zoom(
        dataZoomIndex = 0,
        start = 0,
        end = 100
      ) %>%
      e_toolbox(show = FALSE) %>%
      e_group("grp") %>%
      e_connect_group("grp")

    return(plot)
  })

  output$estimatePlot <- renderEcharts4r({
    estimateData <- estimateDataPlot()
    incidenceData <- incidenceDataPlot()
    selectedRegion <- selectedRegion()

    if (multipleRegions()) {
      plotDataType <- estimateData$data_type[1]
    } else {
      plotDataType <- ""
    }

    minX <- as_datetime(min(incidenceData$date))
    maxX <- as_datetime(max(incidenceData$date))

    estimateData %>%
      group_by(series) %>%
      e_charts(x = date) %>%
      e_tooltip(
        trigger = "axis",
        formatter = e_tooltip_estimate_formatter(
          style = "decimal", digits = 2, locale = input$lang,
          data_type = plotDataType)) %>%
      e_x_axis(
        min = minX,
        max = maxX
      ) %>%
      e_y_axis(
        name = i18n()$t("Reproductive number Re (95% CI)"),
        nameTextStyle = list(fontSize = axisLabelSize, fontWeight = axisLabelWeight),
        nameGap = 50,
        nameLocation = "middle") %>%
      e_line(serie = mean, symbol = "none", cursor = "default") %>%
      e_band2(lower = low, upper = high,
        itemStyle = list(borderWidth = 0, opacity = 0.5),
        tooltip = list(show = TRUE), cursor = "default") %>%
      e_mark_line(
        data = list(name = "Re", yAxis = 1),
        label = list(show = FALSE, formatter = "{b} = {c}"),
        symbol = c("none", "none"), cursor = "default",
        emphasis = list(label = list(show = TRUE))) %>%
      e_grid(right = rightMarginP) %>%
      e_datazoom(x_index = 0, show = FALSE) %>%
      e_zoom(
        dataZoomIndex = 0,
        start = 0,
        end = 100
      ) %>%
      e_legend(show = FALSE) %>%
      e_toolbox(show = FALSE) %>%
      e_group("grp") %>%
      e_connect_group("grp")
  })

  output$independentVarPlot <- renderEcharts4r({
    incidenceData <- incidenceDataPlot()
    vaccinationData <- vaccinationDataPlot()

    yAxisLabel <- ""
    if (dim(vaccinationData)[1] > 0) {
      yAxisLabel <- i18n()$t("Vaccinations")
    }

    plot <- vaccinationData %>%
      group_by(series) %>%
      e_charts(x = date) %>%
      e_line(serie = value,
        lineStyle = list(type = "solid"),
        symbol = "none", cursor = "default", bind = header)

    if (input$showStringency) {
      plot <- plot %>%
        e_data(
          stringencyDataPlot() %>%
            group_by(series),
          x = date) %>%
        e_line(serie = value,
          lineStyle = list(type = "dashed"),
          symbol = "none", cursor = "default", bind = header)
      yAxisLabel <- str_c(i18n()$t("Oxford Stringency Index"), yAxisLabel, sep = " /\n")
    }

    if (length(input$googleMobilityType) > 0) {
      plot <- plot %>%
        e_data(
          mobilityDataGooglePlot() %>%
            group_by(series),
          x = date) %>%
        e_line(serie = value,
          lineStyle = list(type = "dotted"),
          symbol = "none", cursor = "default", bind = header)

      yAxisLabel <- str_c(yAxisLabel, i18n()$t("% mobility change"), sep = " /\n")
    }

    if (length(input$appleMobilityType) > 0) {
      plot <- plot %>%
        e_data(
          mobilityDataApplePlot() %>%
            group_by(series),
          x = date) %>%
        e_line(serie = value,
          lineStyle = list(type = "dotted"),
          symbol = "none", cursor = "default", bind = header)
      if (!(length(input$googleMobilityType) > 0)) {
        yAxisLabel <- str_c(yAxisLabel, i18n()$t("% mobility change"), sep = " /\n")
      }
    }

    plot <- plot %>%
      e_grid(right = rightMarginP) %>%
      e_x_axis(
        min = as_datetime(min(incidenceData$date)),
        max = as_datetime(max(incidenceData$date))
      ) %>%
      e_y_axis(
        name = yAxisLabel,
        nameTextStyle = list(fontSize = axisLabelSize, fontWeight = axisLabelWeight),
        nameGap = 50,
        nameLocation = "middle") %>%
      e_legend(show = TRUE) %>%
      e_datazoom(x_index = 0, show = FALSE) %>%
      e_zoom(
        dataZoomIndex = 0,
        start = 0,
        end = 100
      ) %>%
      e_tooltip(
        trigger = "axis",
        formatter = e_tooltip_independentvar_formatter(
          style = "decimal", digits = 2, locale = input$lang)) %>%
      e_toolbox(show = FALSE) %>%
      e_group("grp") %>%
      e_connect_group("grp")

    return(plot)
  })

  ## UI
  ### Country Select
  output$countryChoiceUI <- renderUI({
    selectizeInput(
      inputId = "countrySelect",
      label = i18n()$t("Country"),
      choices = countryList,
      selected = "CHE",
      options = list(
        placeholder = i18n()$t("Select Country"),
        plugins = list("remove_button"),
        hideSelected = TRUE,
        sortField = "label"),
      multiple = TRUE, width = "100%", size = NULL
    )
  })

  ### Region Select
  output$regionChoiceUI <- renderUI({
    availableRegions <- availableRegions()
    if (availableRegions[[2]][1] == "") {
      return(NULL)
    } else {
      selectizeInput(
        inputId = "regionSelect", label = i18n()$t("Subdivisions"),
        choices = availableRegions(),
        selected = "",
        options = list(
          placeholder = i18n()$t("Select subdivisions"),
          plugins = list("remove_button"),
          hideSelected = TRUE),
        multiple = TRUE, width = "100%", size = NULL
      )
    }
  })

  ### Data Type
  output$dataTypeChoiceUI <- renderUI({
    incidenceData <- incidenceData()
    availableDataTypes <- unique(incidenceData$data_type)
    if (multipleRegions()) {
      selectizeInput(
        inputId = "dataTypeSelect", label = i18n()$t("Select incidence type"),
        choices = availableDataTypes,
        selected = availableDataTypes[1],
        options = list(
          placeholder = i18n()$t("Select incidence type"),
          hideSelected = TRUE,
          sortField = "label"),
        multiple = FALSE, width = "100%", size = NULL
      )
    } else {
      return(NULL)
    }
  })

  output$incidenceSmoothingUI <- renderUI({

    choices <- c(0, 7)
    names(choices) <- c(i18n()$t("daily incidence"), i18n()$t("7 day average"))

    ui <- selectizeInput(
      inputId = "incidenceSmoothing", label = i18n()$t("Display incidence data as"),
      choices = choices,
      selected = 0,
      options = list(
        hideSelected = TRUE,
        sortField = "label"),
      multiple = FALSE, width = "100%", size = NULL
    )
    return(ui)
  })

  output$moreIncidenceOptions <- renderUI({
    tagList(
      HTML(
        "<label class=\"control-label\">",
        i18n()$t("More Options"),
        "</label>"
        ),
      checkboxInput(
        inputId = "incidenceNormalization",
        label = i18n()$t("Normalize incidence to per 100'000 inhabitants"),
        value = FALSE,
        width = NULL),
      checkboxInput(
        inputId = "incidenceLoess",
        label = i18n()$t("Show smoothed data (Loess Fit)"),
        value = FALSE,
        width = NULL),
      checkboxInput(
        inputId = "incidenceDeconvolution",
        label = i18n()$t("Show estimated infection times (deconvolution)"),
        value = FALSE,
        width = NULL),
      checkboxInput(
        inputId = "incidenceLogAxis",
        label = i18n()$t("Logarithmic axis for Incidence"),
        value = FALSE,
        width = NULL)
    )
  })

  ### Estimation Type
  estimationTypeChoices <- reactive({
    estimationTypeChoices <- c("Sliding window" = "Cori_slidingWindow",
                               "Step-wise constant" = "Cori_step"
                               )
    names(estimationTypeChoices) <- sapply(names(estimationTypeChoices), i18n()$t,  USE.NAMES = FALSE)
    return(estimationTypeChoices)
  })

  output$estimationTypeChoiceUI <- renderUI({
    tagList(
      selectizeInput(
        inputId = "estimationType",
        label = i18n()$t("Select estimation type to show"),
        choices = estimationTypeChoices(),
        selected = "Cori_slidingWindow",
        options = list(
          placeholder = i18n()$t("Select estimation type"),
          hideSelected = TRUE,
          sortField = "label"),
        multiple = FALSE, width = "100%", size = NULL
      ),
      helpText(
        HTML(
          i18n()$t("'Sliding Window' estimates R<sub>e</sub> using a 3 day sliding window."),
          "<br>",
          i18n()$t("'Step-wise constant' estimates R<sub>e</sub> assumes constant R<sub>e</sub> when Oxford Stringency Index is constant.")
        )
      )
    )
  })

  ### Independent Variables
  output$showStringencyUI <- renderUI({
    tagList(
      HTML(
        "<label class=\"control-label\">",
        i18n()$t("Oxford Stringency Index"),
        "</label>"
        ),
      checkboxInput(
        inputId = "showStringency",
        label = i18n()$t("Show / Hide Index"),
        value = TRUE,
        width = NULL)
    )
  })

  output$googleMobilityTypeChoiceUI <- renderUI({
    mobilityDataGoogle <- mobilityDataGoogle()
    availableMobilityDataGoogle <- c("", unique(mobilityDataGoogle$data_type))

    choices <- c(0, 7, 14)
    names(choices) <- c(i18n()$t("no smoothing"), i18n()$t("7 day average"), i18n()$t("14 day average"))

    ui <- tagList(
      selectizeInput(
        inputId = "googleMobilityType", label = i18n()$t("Show Google mobility data"),
        choices = availableMobilityDataGoogle,
        selected = "",
        options = list(
          placeholder = i18n()$t("Select mobility category"),
          plugins = list("remove_button"),
          hideSelected = TRUE,
          sortField = "label"),
        multiple = !multipleRegions(), width = "100%", size = NULL
      ),
      selectizeInput(
        inputId = "mobilitySmoothingGoogle", label = i18n()$t("Smooth Google mobility data"),
        choices = choices,
        selected = 7,
        options = list(
          hideSelected = TRUE,
          sortField = "label"),
        multiple = FALSE, width = "100%", size = NULL
      )
    )

    return(ui)
  })

  output$appleMobilityTypeChoiceUI <- renderUI({
    mobilityDataApple <- mobilityDataApple()
    availableMobilityDataApple <- c("", unique(mobilityDataApple$data_type))

    choices <- c(0, 7, 14)
    names(choices) <- c(i18n()$t("no smoothing"), i18n()$t("7 day average"), i18n()$t("14 day average"))

    ui <- tagList(
      selectizeInput(
        inputId = "appleMobilityType", label = i18n()$t("Show Apple mobility data"),
        choices = availableMobilityDataApple,
        selected = "",
        options = list(
          placeholder = i18n()$t("Select mobility category"),
          plugins = list("remove_button"),
          hideSelected = TRUE,
          sortField = "label"),
        multiple = !multipleRegions(), width = "100%", size = NULL
      ),
      selectizeInput(
        inputId = "mobilitySmoothingApple", label = i18n()$t("Smooth Apple mobility data"),
        choices = choices,
        selected = 7,
        options = list(
          hideSelected = TRUE,
          sortField = "label"),
        multiple = FALSE, width = "100%", size = NULL
      )
    )

    return(ui)
  })

  output$vaccinationDataTypeChoiceUI <- renderUI({
    vaccinationData <- vaccinationData()
    availableVaccinationData <- unique(vaccinationData$data_type)

    selectizeInput(
      inputId = "vaccinationDataType", label = i18n()$t("Show vaccination data (per 100 people)"),
      choices = availableVaccinationData,
      selected = "People Fully Vaccinated",
      options = list(
        placeholder = i18n()$t("Select vaccination data"),
        plugins = list("remove_button"),
        hideSelected = TRUE,
        sortField = "label"),
      multiple = !multipleRegions(), width = "100%", size = NULL
    )
  })

  # Data sources
  output$dataSourcesUI <- renderUI({

    if (multipleRegions()) {
      req(input$dataTypeSelect)
      dataTypeSelect <- input$dataTypeSelect
    } else {
      dataTypeSelect <- unique(incidenceDataPlot()$data_type)
    }

    if (input$showStringency) {
      dataTypeSelect <- c(dataTypeSelect, "Stringency Index")
    }

    if (length(input$appleMobilityType) > 0 && input$appleMobilityType[1] != "") {
      dataTypeSelect <- c(dataTypeSelect, "Apple Mobility Data")
    }

    if (length(input$googleMobilityType) > 0 && input$googleMobilityType[1] != "") {
      dataTypeSelect <- c(dataTypeSelect, "Google Mobility Data")
    }

    updateDataPlot <- updateData() %>%
      filter(
        data_type %in% dataTypeSelect,
        region %in% selectedRegion()) %>%
      ungroup() %>%
      dplyr::select(-region) %>%
      group_by(countryIso3, country, source, data_type) %>%
      dplyr::summarize(
        lastChanged = max(lastChanged),
        .groups = "keep") %>%
      ungroup()

    updateDataString <- dataUpdatesString(updateDataPlot,
      name = i18n()$t("Data Source"), dateFormat = i18n()$t("%Y-%m-%d"))

    ui <- helpText(updateDataString, style = "text-align: center; padding-left: 5% !important; padding-right: 5% !important;")
    return(ui)
  })

  ## Map
  # palettes
    cases14pal <- reactive({
      cases14pal <- divergentColorPal(
        palette = c("RdYlGn"),
        domain = c(0, input$casesCutoff),
        midpoint = input$casesMidpoint,
        reverse = TRUE)
      return(cases14pal)
    }) %>% debounce(1000)

    repal <- reactive({
      repal <- divergentColorPal(
        palette = c("RdYlGn"),
        domain = c(0, input$reCutoff),
        midpoint = input$reMidpoint,
        reverse = TRUE)
      return(repal)
    }) %>% debounce(1000)

  output$mapPlot <- renderLeaflet({
    countriesShapePlot <- countriesShape
    cases14pal <- cases14pal()
    repal <- repal()

    map <- leaflet(options = leafletOptions(minZoom = 2)) %>%
      addTiles() %>%
      addMapPane("countries", zIndex = 410) %>%
      addMapPane("region", zIndex = 420)

    if ("CHE" %in% input$regionCountrySelect) {
      # remove CHE from countries
      countriesShapePlot <- filter(countriesShape, ADM0_A3_IS != "CHE")

      map <- map %>%
        addPolygonLayer(
          shapeFile = CHEregionsShape,
          fillColor = ~cases14pal(cases14d), group = "Cases / 100'000 / 14 d",
          labels = cheCasesLabels,
          options = pathOptions(pane = "region")) %>%
        addPolygonLayer(
          shapeFile = CHEregionsShape,
          fillColor = ~repal(median_R_mean), group = "median Re",
          labels = cheReLabels,
          options = pathOptions(pane = "region"))
    }

    if ("ZAF" %in% input$regionCountrySelect) {
      # remove ZAF from countries
      countriesShapePlot <- filter(countriesShape, ADM0_A3_IS != "ZAF")

      map <- map %>%
        addPolygonLayer(
          shapeFile = ZAFregionsShape,
          fillColor = ~cases14pal(cases14d), group = "Cases / 100'000 / 14 d",
          labels = zafCasesLabels,
          options = pathOptions(pane = "region")) %>%
        addPolygonLayer(
          shapeFile = ZAFregionsShape,
          fillColor = ~repal(median_R_mean),
          group = "median Re",
          labels = zafReLabels,
          options = pathOptions(pane = "region"))
    }

    countryCasesLabels <- mapLabels(shapeFileData = countriesShapePlot, mainLabel = "cases14d")
    countryReLabels <- mapLabels(shapeFileData = countriesShapePlot, mainLabel = "re")

    map <- map %>%
      addPolygonLayer(
        shapeFile = countriesShapePlot,
        fillColor = ~cases14pal(cases14d), group = "Cases / 100'000 / 14 d",
        labels = countryCasesLabels) %>%
      addPolygonLayer(
        shapeFile = countriesShapePlot,
        fillColor = ~repal(median_R_mean), group = "median Re",
        labels = countryReLabels) %>%
      addEasyButton(easyButton(
        icon = "fa-globe", title = "Reset Zoom",
        onClick = JS("function(btn, map) { map.setZoom(2); }"))) %>%
      addEasyButton(easyButton(
        icon = "fa-crosshairs", title = "Locate Me",
        onClick = JS("function(btn, map) { map.locate({setView: true}); }"))) %>%
      setMaxBounds(lng1 = 272.1094, lat1 = 84.73839, lng2 = -222.5391, lat2 = -71.74643) %>%
      addScaleBar(position = "bottomleft") %>%
      addLayersControl(
        baseGroups = c("Cases / 100'000 / 14 d", "median Re"),
        options = layersControlOptions(collapsed = FALSE, hideSingleBase = TRUE)
      ) %>%
      setView(
        lng = stateVals$mapViewCenter$lng, lat = stateVals$mapViewCenter$lat,
        zoom = stateVals$mapViewZoom) %>%
      hideGroup(isolate(selectedMapGroup$groups)) %>%
      showGroup(isolate(selectedMapGroup$group))

    if (isolate(selectedMapGroup$group) == "Cases / 100'000 / 14 d") {
      map <- map %>%
        addLegend(
          pal = cases14pal, opacity = 0.9, title = "Cases / 100'000 / 14 d",
          values = seq(0, input$casesCutoff, 100),
          labFormat = casesLegendLabels,
          data = countriesShape,
          position = "bottomright", group = "Cases / 100'000 / 14 d", layerId = "casesLegend")
    } else {
      map <- map %>%
        addLegend(pal = repal, opacity = 0.9, title = "Most recent R<sub>e</sub> estimate",
          values = c(seq(0, input$reCutoff, 0.2)),
          data = countriesShape,
          position = "bottomright", group = "median Re", layerId = "reLegend")
    }

    return(map)
  })

  output$mapPlotUI <- renderUI({
    tabBox(width = 12,
      title = tagList(shiny::icon("map"),
      HTML(i18n()$t(str_c("Map")))),
      tabPanel(
        title = "World Map",
        value = "worldMap",
        leafletOutput("mapPlot", width = "100%", height = 800) %>% withSpinner()
      )
    )
  })

  output$mapHist <- renderPlotly({
    req(input$mapPlot_groups)

    if (input$mapPlot_groups == "Cases / 100'000 / 14 d") {
      midpoint <- input$casesMidpoint
      cutoff <- input$casesCutoff
      histPal <- cases14pal()
      binwidth <- 10
      histDataRaw <- countriesShape %>%
        as_tibble() %>%
        rename(variable = cases14d)
      title <- "Cases / 100'000 / 14 d"
    } else {
      midpoint <- input$reMidpoint
      cutoff <- input$reCutoff
      histPal <- repal()
      binwidth <- 0.1
      histDataRaw <- countriesShape %>%
        as_tibble() %>%
        rename(variable = median_R_mean)
      title <- "median R<sub>e</sub>"
    }

    histData <- histDataRaw %>%
      mutate(bins = cut(variable,
        breaks = seq(0, max(variable, na.rm = TRUE) + binwidth, binwidth))) %>%
      group_by(bins) %>%
      summarize(
        n = n(),
        countries = str_c(ADM0_A3_IS, collapse = ", "),
        .groups = "keep"
      ) %>%
      ungroup() %>%
      complete(bins, fill = list(n = 0, countries = "", color = "gray")) %>%
      mutate(
        midpoint = seq(binwidth / 2, by = binwidth, length.out = length(bins)),
        color = histPal(midpoint)) %>%
      filter(!is.na(bins))

    quantiles <- quantile(histDataRaw$variable, na.rm = TRUE)
    quantilesText <- glue::glue(
      "<b>Quantiles</b><br>",
      " min: {round(quantiles[1], 2)}<br>",
      "0.25: {round(quantiles[2], 2)}<br>",
      "0.50: {round(quantiles[3], 2)}<br>",
      "0.75: {round(quantiles[4], 2)}<br>",
      " max: {round(quantiles[5], 2)}<br>"
    )

    plot <- plot_ly(data = histData) %>%
      add_bars(x = ~midpoint, y = ~n, color = ~bins, colors = ~color,
        text = ~str_trunc(countries, 50),
        hoverinfo = "text",
        showlegend = FALSE) %>%
        add_segments(x = midpoint, xend = midpoint, y = 0, yend = max(histData$n),
          showlegend = FALSE) %>%
        add_segments(x = cutoff, xend = cutoff, y = 0, yend = max(histData$n),
          showlegend = FALSE) %>%
      layout(
        xaxis = list(range = c(0, 2 * cutoff), fixedrange = TRUE, title = title),
        yaxis = list(fixedrange = TRUE),
        annotations = list(list(
          x = 1, y = 1, xref = "paper", yref = "paper",
          width = 100,
          height = 100,
          text = quantilesText,
          valign = "top",
          showarrow = FALSE,
          xanchor = "right", yanchor = "top", align = "left",
          # xshift = helpBoxShift[1], yshift = helpBoxShift[2],
          font = list(size = 12, color = "black")
        ))) %>%
      config(displaylogo = FALSE, modeBarButtons = list(list("toImage")),
        toImageButtonOptions = list(format = "png", width = 1200, height = 800, scale = 1, filename = "histogram"))

    return(plot)
  })

  output$mapMethodsUI <- renderUI({
    methodsFileName <- "md/methodsOnly_"
    ui <- box(width = 8, includeMarkdown(str_c(methodsFileName, input$lang, ".md")))
    return(ui)
  })

# map observers
  # switch legend (workaround for baseGroup limitation)
    selectedMapGroup <- reactiveValues(
      group = "Cases / 100'000 / 14 d",
      groups = c("Cases / 100'000 / 14 d", "median Re")
    )

    observeEvent(input$mapPlot_groups, {
      mapPlot <- leafletProxy("mapPlot")
      selectedMapGroup$group <- input$mapPlot_groups[1]
      if (selectedMapGroup$group == "Cases / 100'000 / 14 d") {
        mapPlot %>%
          removeControl("reLegend") %>%
          addLegend(
            pal = cases14pal(), opacity = 0.9, title = "Cases / 100'000 / 14 d",
            values = c(seq(0, input$casesCutoff, 100)),
            labFormat = casesLegendLabels,
            data = countriesShape,
            position = "bottomright", group = "Cases / 100'000 / 14 d", layerId = "casesLegend") %>%
          hideGroup(selectedMapGroup$groups) %>%
          showGroup(selectedMapGroup$group)
      }
      else if (selectedMapGroup$group == "median Re") {
        mapPlot %>%
          removeControl("casesLegend") %>%
          addLegend(pal = repal(), opacity = 0.9, title = "Most recent R<sub>e</sub> estimate",
            values = c(seq(0, input$reCutoff, 0.2)),
            data = countriesShape,
            position = "bottomright", group = "median Re", layerId = "reLegend") %>%
          hideGroup(selectedMapGroup$groups) %>%
          showGroup(selectedMapGroup$group)
      }
    })
  
  # additional UI

  output$mapOptionsUI <- renderUI({
    fluidRow(
        column(6,
          regionCheckboxInput("regionCountrySelect", label = i18n()$t("Display regional data"),
            choices = c("Switzerland" = "CHE", "South Africa" = "ZAF"), selected = "", zoomLabel = "Zoom")
        ),
        column(6,
         HTML(
          "<label class=\"control-label\">",
          i18n()$t("Color scale options"),
          "</label>"
          ),
          plotlyOutput("mapHist", height = "250px") %>% withSpinner(),
          column(12,
            conditionalPanel(
              condition = "input.mapPlot_groups == \"Cases / 100'000 / 14 d\"",
              div(
                div(style = "display: inline-block;vertical-align:top;width:40%",
                  numericInput("casesMidpoint", "Breakpoint", value = 60,
                    min = 0, max = 1500, step = 1)
                ),
                div(style = "display: inline-block;vertical-align:top;width:40%",
                  numericInput("casesCutoff", "Cutoff",
                    value = 300,
                    min = 0, max = 1500, step = 50)
                )
              )
            ),
            conditionalPanel(
              condition = "input.mapPlot_groups == 'median Re'",
              div(
                div(style = "display: inline-block;vertical-align:top;width:40%",
                  numericInput("reMidpoint", "Breakpoint", value = 1,
                    min = 0, max = 20, step = 0.1)
                ),
                div(style = "display: inline-block;vertical-align:top;width:40%",
                  numericInput("reCutoff", "Cutoff",
                    value = 2,
                    min = 0, max = 20, step = 0.1)
                )
              )
            )
          )
        )
      )
  })

  # region zoom buttons
    observeEvent(input$zoomCHE, {
      mapPlot <- leafletProxy("mapPlot")
      mapPlot %>% setView(lng = 8.360596, lat = 46.84141, zoom = 8)
    })

    observeEvent(input$zoomZAF, {
      mapPlot <- leafletProxy("mapPlot")
      mapPlot %>% setView(lng = 25.53223, lat = -28.38174, zoom = 6)
    })

    observeEvent(input$regionCountrySelect, {
      if (length(input$regionCountrySelect) == 1) {
        mapPlot <- leafletProxy("mapPlot")
        if (input$regionCountrySelect == "CHE") {
          mapPlot %>% setView(lng = 8.360596, lat = 46.84141, zoom = 8)
        } else if (input$regionCountrySelect == "ZAF") {
          mapPlot %>% setView(lng = 25.53223, lat = -28.38174, zoom = 6)
        }
      }
    })

  # methods text
    output$methodsUI <- renderUI({
      req(input$countrySelect)
      methodsFileName <- "md/methodsOnly_"
      if (length(input$countrySelect) == 1) {
        if (input$countrySelect == "CHE") {
          methodsFileName <- "md/methodsCH_"
        }
      }

      ui <- fluidRow(
        column(10, style = "padding-left: 5% !important; padding-right: 5% !important;",
          includeMarkdown(str_c(methodsFileName, input$lang, ".md"))
          ),
        column(2)
        )
      return(ui)
    })


  # about Page

  output$sourcesTable <- renderDataTable({
      dataSourcesTable <- dataSources
      names(dataSourcesTable) <- sapply(unique(names(dataSources)), i18n()$t,  USE.NAMES = FALSE)
      return(dataSourcesTable)
    }, escape = FALSE, options = list(paging = FALSE, searching = FALSE))

    output$aboutUI <- renderUI({
      fluidPage(
        fluidRow(
          column(12,
            includeMarkdown("md/about.md")
          )
        ),
        fluidRow(
          column(12,
            h3(i18n()$t("Data Sources")),
            dataTableOutput("sourcesTable")
          )
        )
      )
    })

}
