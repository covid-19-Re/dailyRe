navbarPageWithInputs <- function(..., inputs) {
  navbar <- navbarPage(...)
  form <- tags$form(class = "navbar-form navbar-right", inputs)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], form)
  navbar
}

ui <- function(request) {
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "customSelectize.css")
    ),
    useShinyjs(),
    navbarPageWithInputs(
      id = "tabs",
      title = HTML("COVID-19 R<sub>e</sub>"),
      windowTitle = "COVID-19 Re",
      tabPanel(
        id = "timeseries",
        title = "Timeseries",
        icon = icon("chart-area"),
        uiOutput("infoBanner"),
        fluidRow(class = "selectCol",
          column(5,
            uiOutput("countryChoiceUI")
          ),
          column(5,
            uiOutput("regionChoiceUI")
          ),
          column(2,
            uiOutput("currentReUI")
          )
        ),
        fluidRow(
          column(10,
            echarts4rOutput("incidencePlot")
          ),
          column(2,
            uiOutput("dataTypeChoiceUI"),
            uiOutput("dateRangeUI"),
            uiOutput("incidenceSmoothingUI"),
            uiOutput("moreIncidenceOptions")
          )
        ),
        fluidRow(
          column(10,
            echarts4rOutput("estimatePlot")
          ),
          column(2,
            uiOutput("estimationTypeChoiceUI")
          )
        ),
        fluidRow(
          column(10,
            echarts4rOutput("independentVarPlot"),
            uiOutput("dataSourcesUI")
          ),
          column(2,
            uiOutput("showStringencyUI"),
            uiOutput("vaccinationDataTypeChoiceUI"),
            uiOutput("mobilityTypeChoiceUI")
          )
        ),
        uiOutput("methodsUI")
      ),
      tabPanel(
        id = "map",
        title = "Map", icon = icon("map"),
        fluidRow(style = "padding-bottom: 20px;",
          leafletOutput("mapPlot", width = "100%", height = 800) %>% withSpinner()
        ),
        uiOutput("mapOptionsUI")
      ),
      tabPanel(is = "about", title = "About", icon = icon("circle-question", class = "far"),
        uiOutput("aboutUI")
      ),
      inputs = selectInput("lang", NULL,
        languageSelect, selected = languageSelect[1], multiple = FALSE, selectize = TRUE,
        width = "100px")
    )
  )
}
