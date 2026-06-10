#UI.R for Shepherd

library(shiny)
library(tidyverse)
library(dplyr)
library(leaflet)
library(geojsonio)
library(sf)
library(ggrepel)
library(plotly)
library(shinythemes)

#Loading all datasets
obese_overweight_adults <- read.csv("obese_overweight_adults.csv")
obese_overweight_adults$year <- as.integer(obese_overweight_adults$year)
GDP_tidy <- read.csv("GDP_tidy.csv")
Gini_Inequality_Index_tidy <- read.csv("Gini_Inequality_Index_tidy.csv")
happiness_index_tidy <- read.csv("happiness_index_tidy.csv")


#Create the UI

shinyUI(
  navbarPage(
    title = tags$span(
      tags$img(src = "https://cdn-icons-png.flaticon.com/512/3079/3079170.png",
               height = "24px", style = "margin-right:8px; margin-top:-3px;"),
      strong("Happy Meals"),
      tags$span(" | Global Factors Related to Obesity",
                style = "font-weight:300; font-size:90%; color:#ccddcc;")
    ),
    theme = shinytheme("flatly"),

    # Custom CSS for a little extra polish
    header = tags$head(
      tags$style(HTML("
        body { background-color: #f8f9fa; }
        .navbar { border-bottom: 3px solid #18bc9c; }
        .well { background-color: #ffffff; border: 1px solid #e0e0e0; border-radius: 8px; box-shadow: 0 1px 4px rgba(0,0,0,0.07); }
        .nav-tabs { margin-bottom: 16px; }
        h2, h3 { color: #2c3e50; }
        .section-card {
          background: #ffffff;
          border-radius: 8px;
          padding: 20px 24px;
          margin-bottom: 18px;
          border-left: 4px solid #18bc9c;
          box-shadow: 0 1px 4px rgba(0,0,0,0.06);
        }
        pre { background-color: #f4f6f7; border-radius: 6px; font-size: 13px; }
        .leaflet-container { border-radius: 8px; }
        .selectize-input { border-radius: 6px !important; }
      "))
    ),

    #FIRST PANEL: DEFINITION OVERVIEW
    tabPanel(
      "Definition Overview",
      br(),
      fluidRow(
        column(10, offset = 1,
          div(class = "section-card",
            tags$h2(strong("Introduction")),
            htmlOutput("headerText")
          ),
          div(class = "section-card",
            tags$h3(strong("Adult Obesity")),
            htmlOutput("obesityText")
          ),
          div(class = "section-card",
            tags$h3(strong("Gross Domestic Product (GDP)")),
            htmlOutput("gdpText")
          ),
          div(class = "section-card",
            tags$h3(strong("Gini Inequality Index")),
            htmlOutput("giniText")
          ),
          div(class = "section-card",
            tags$h3(strong("Happiness Index")),
            htmlOutput("happinessText")
          )
        )
      )
    ),

    #SECOND PANEL: LONGITUDINAL CHLOROPLETHS
    tabPanel(
      "Longitudinal Chloropleths",
      br(),
      sidebarLayout(
        sidebarPanel(
          wellPanel(
            tags$h4(strong("Map Controls"), style = "margin-top:0; color:#18bc9c;"),
            sliderInput(
              "year",
              "Year:",
              min = as.Date(paste(min(obese_overweight_adults$year), "01", "01", sep = "-")),
              max = as.Date(paste(max(obese_overweight_adults$year), "01", "01", sep = "-")),
              value = as.Date(paste(min(obese_overweight_adults$year), "01", "01", sep = "-")),
              step = 365,
              timeFormat = "%Y"
            ),
            selectInput("GlobalFactor",
              label = "Choose a Global Factor",
              choices = list(
                "Adult Obesity"        = "obese_overweight_adults",
                "Gross GDP"            = "GDP_tidy",
                "Gini Inequality Index"= "Gini_Inequality_Index_tidy",
                "Happiness Index"      = "happiness_index_tidy"
              )
            ),
            tags$h5(strong("Available Years per Factor"),
                    style = "color:#7f8c8d; margin-top:12px;"),
            htmlOutput("year_info")
          )
        ),
        mainPanel(
          leafletOutput(outputId = "map", height = "600px")
        )
      )
    ),

    #THIRD PANEL: CORRELATION ANALYSIS
    tabPanel(
      "Correlation Analysis",
      br(),
      fluidRow(
        column(10, offset = 1,
          div(class = "section-card",
            tags$h2(strong("Pearson Correlations")),
            htmlOutput("pearsoncorrelationText")
          ),
          div(class = "section-card",
            tags$h3(strong("What are Pearson Correlations?")),
            htmlOutput("correlationdefinitionText")
          )
        )
      ),
      fluidRow(
        column(10, offset = 1,
          sidebarLayout(
            sidebarPanel(
              wellPanel(
                tags$h4(strong("Select Correlation"), style = "margin-top:0; color:#18bc9c;"),
                selectInput("correlation_type",
                  label = "",
                  choices = c(
                    "GDP per Capita vs Obesity"       = "Obesity_GDPpercapita_plot",
                    "Gross GDP vs Obesity"            = "Obesity_GDP_plot",
                    "GDP per Capita vs Happiness"     = "Happiness_GDPpercapita_plot",
                    "Gross GDP vs Happiness"          = "Happiness_GDP_plot",
                    "Obesity Rate vs Happiness"       = "Obesity_Happiness_plot"
                  ),
                  selected = "obesity_gdp_per_capita"
                )
              )
            ),
            mainPanel(
              plotOutput("dynamic_correlation_plot")
            )
          )
        )
      )
    ),

    #FOURTH PANEL: FAST FOOD MAP MANIA
    tabPanel(
      "Fast Food Map Mania",
      br(),
      sidebarLayout(
        sidebarPanel(
          wellPanel(
            tags$h4(strong("Explore Global Locations"), style = "margin-top:0; color:#18bc9c;"),
            p("Use the dropdown to explore worldwide locations of popular fast food chains.
              Click clusters to zoom in, or individual markers to view addresses.",
              style = "font-size:13px; color:#7f8c8d;"),
            selectInput("fastFoodDataset",
              label = "Select a Fast Food Chain",
              choices = list(
                "Domino's"   = "coordinates",
                "Starbucks"  = "coordinates_Starbucks",
                "McDonald's" = "coordinates_McDonalds"
              ),
              selected = "coordinates"
            )
          )
        ),
        mainPanel(
          leafletOutput(outputId = "map2", height = "600px")
        )
      )
    ),

    #FIFTH PANEL: RAW DATA
    tabPanel(
      "Raw Data",
      br(),
      fluidRow(
        column(10, offset = 1,
          div(class = "section-card",
            tags$h3(strong("Raw Data Tables")),
            htmlOutput("rawdescText")
          ),
          wellPanel(
            selectInput("GlobalFactor2",
              label = "Choose a Global Factor",
              choices = list(
                "Adult Obesity"         = "obese_overweight_adults",
                "Gross GDP"             = "GDP_tidy",
                "Gini Inequality Index" = "Gini_Inequality_Index_tidy",
                "Happiness Index"       = "happiness_index_tidy"
              )
            ),
            uiOutput("checkbox"),
            textOutput("null_message"),
            tableOutput("data_table")
          )
        )
      )
    )
  )
)
