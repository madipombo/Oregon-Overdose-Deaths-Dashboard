library(shiny)
library(bslib)
library(readxl)
library(tidyverse)
library(patchwork)
library(shinyWidgets)
# Load in data
annual <- read_excel("data/SUDORS-Fatal-Overdose-Data.xlsx",
                     sheet = 3)
# Filter for just Oregon
oregon_annual <- annual %>% 
  filter(
    jurisdiction == "Oregon"
  )

source("tabs/year_selector.R")
source("tabs/overview.R")
source("tabs/drugs.R")
source("tabs/demographics.R")
source("tabs/circumstances.R")
source("tabs/intervention.R")

ui <- page_fillable(
  
  year_selector_ui("year_selector"),
  
  page_navbar(
    theme = bs_theme(version = 5, bootswatch = "flatly"),
    
    tags$style(HTML("
    .navbar-brand {
      font-size: 28px !important;
      font-weight: bold;
    }

    .nav-link {
      font-size: 28px !important;
    }
  ")),
    title = "Oregon Overdose Death Dashboard",
    
    nav_panel("Overview", overview_ui("overview")),
    nav_panel("Drugs", drugs_ui("drugs")),
    nav_panel("Demographics", demographics_ui("demographics")),
    nav_panel("Circumstances", circumstances_ui("circ")),
    nav_panel("Setting & Intervention", intervention_ui("int"))
  )
)

server <- function(input, output, session) {
  
  selected_year <- reactiveVal(2024)
  
  year_selector_server(
    "year_selector",
    selected_year
  )
  
  overview_server(
    "overview",
    annual_data = oregon_annual,
    selected_year = selected_year
  )
  
  demographics_server(
    "demographics",
    annual_data = oregon_annual,
    selected_year = selected_year
  )
  
  circumstances_server(
    "circ",
    annual_data = oregon_annual,
    selected_year = selected_year
  )
  
  intervention_server(
    "int",
    annual_data = oregon_annual,
    selected_year = selected_year
  )
  
  drugs_server(
    "drugs",
    annual_data = oregon_annual,
    selected_year = selected_year
  )
  
}

shinyApp(ui, server)
