
# Intervention Tab UI

intervention_ui <- function(id) {
  
  ns <- NS(id)
  
  layout_columns(
    col_widths = c(6, 6),
# Naloxone plot card   
    card(
      full_screen = TRUE,
      
      card_header(
        radioButtons(
          ns("naloxone_metric"),
          NULL,
          choices = c(
            "Count" = "deaths",
            "Percent" = "percent"
          ),
          selected = "percent",
          inline = TRUE
        )
      ),
      
      plotOutput(
        ns("naloxone_plot"),
        height = "400px"
      )
    ),
# Bystancer plot card    
    card(
      full_screen = TRUE,
      
      card_header(
        
        div(
          style = "
        display:flex;
        justify-content:space-between;
        align-items:center;
        gap:16px;
        flex-wrap:wrap;
      ",
          
          radioButtons(
            ns("byst_metric"),
            NULL,
            choices = c(
              "Count" = "deaths",
              "Percent" = "percent"
            ),
            selected = "percent",
            inline = TRUE
          ),
          
          selectInput(
            ns("byst_measure"),
            NULL,
            choices = c(
              "Potential Bystander Present" = "present",
              "No or Delayed Response" = "noresp",
              "Spatially Separated" = "space",
              "Unaware Substance Use Occurring" = "unaware",
              "Did Not Recognize Symptoms" = "notrecog",
          "Did Not Recognize Overdose" = "notrecog_od",
          "Bystander Using Alcohol or Drugs" = "sub_alc",
          "Public Space / Strangers Did Not Intervene" = "publicspace"
        ),
        selected = "present",
        width = "100%"
      )

    )

  ),

  plotOutput(
    ns("bystander_plot"),
    height = "400px"
  )
)
)
  
}


# Intervention Server

intervention_server <- function(
    id,
    annual_data,
    selected_year
) {
  
  moduleServer(id, function(input, output, session) {
    
# Data for selected year
    selected_row <- reactive({
      
      annual_data[
        annual_data$data_year == selected_year(),
      ]
      
    })
    
# Naloxone Plot
    output$naloxone_plot <- renderPlot({
      
      if (input$naloxone_metric == "deaths") {
        
        df <- annual_data |>
          dplyr::select(
            data_year,
            naloxone_deaths
          ) |>
          dplyr::rename(value = naloxone_deaths)
        
        y_lab <- "Deaths"
        title <- "Naloxone Administered"
        
      } else {
        
        df <- annual_data |>
          dplyr::select(
            data_year,
            naloxone_percent
          ) |>
          dplyr::rename(value = naloxone_percent)
        
        y_lab <- "Percent"
        title <- "Naloxone Administered"
        
      }
      
      df <- df |>
        dplyr::mutate(
          highlighted = data_year == selected_year()
        )
      
      ggplot(
        df,
        aes(
          x = data_year,
          y = value
        )
      ) +
        
        geom_line(
          color = "grey75",
          linewidth = 1.2
        ) +
        
        geom_point(
          color = "grey75",
          size = 3
        ) +
        
        geom_point(
          data = subset(df, highlighted),
          color = "#2C7FB8",
          size = 5
        ) +
        
        geom_text(
          aes(
            label = if (input$naloxone_metric == "deaths")
              scales::comma(round(value))
            else
              paste0(round(value, 1), "%")
          ),
          vjust = -0.7,
          size = 4
        ) +
        
        scale_x_continuous(
          breaks = df$data_year
        ) +
        
        scale_y_continuous(
          labels = if (input$naloxone_metric == "deaths")
            scales::comma
          else
            function(x) paste0(x, "%"),
          expand = expansion(mult = c(0.05, 0.20))
        ) +
        
        labs(
          title = title,
          subtitle = paste(
            "Selected Year:",
            selected_year()
          ),
          x = "Year",
          y = y_lab
        ) +
        
        theme_minimal(base_size = 14) +
        
        theme(
          plot.title = element_text(
            face = "bold",
            size = 18
          ),
          axis.text.x = element_text(
            face = "bold"
          ),
          panel.grid.minor = element_blank()
        )
      
    })
    
# Bystander Plot
    output$bystander_plot <- renderPlot({
      
      var <- switch(
        
        input$byst_measure,
        
        "present" =
          if (input$byst_metric == "deaths")
            "bystander_deaths"
        else
          "bystander_percent",
        
        "noresp" =
          if (input$byst_metric == "deaths")
            "byst_noresp_deaths"
        else
          "byst_noresp_percent",
        
        "space" =
          if (input$byst_metric == "deaths")
            "byst_space_deaths"
        else
          "byst_space_percent",
        
        "unaware" =
          if (input$byst_metric == "deaths")
            "byst_unaware_deaths"
        else
          "byst_unaware_percent",
        
        "notrecog" =
          if (input$byst_metric == "deaths")
            "byst_notrecog_deaths"
        else
          "byst_notrecog_percent",
        
        "notrecog_od" =
          if (input$byst_metric == "deaths")
            "byst_notrecog_od_deaths"
        else
          "byst_notrecog_od_percent",
        
        "sub_alc" =
          if (input$byst_metric == "deaths")
            "byst_sub_alc_deaths"
        else
          "byst_sub_alc_percent",
        
        "publicspace" =
          if (input$byst_metric == "deaths")
            "byst_publicspace_deaths"
        else
          "byst_publicspace_percent"
      )
      
      title <- switch(
        
        input$byst_measure,
        
        "present" = "Potential Bystander Present",
        
        "noresp" = "No or Delayed Bystander Response",
        
        "space" = "Bystander Spatially Separated",
        
        "unaware" = "Bystander Unaware Substance Use Was Occurring",
        
        "notrecog" = "Bystander Did Not Recognize Symptoms",
        
        "notrecog_od" = "Bystander Did Not Recognize Overdose",
        
        "sub_alc" = "Bystander Using Alcohol or Drugs",
        
        "publicspace" = "Public Space / Strangers Did Not Intervene"
      )
      
      df <- annual_data |>
        dplyr::select(data_year, all_of(var)) |>
        dplyr::rename(value = all_of(var)) |>
        dplyr::mutate(
          highlighted = data_year == selected_year()
        )
      
      ggplot(df, aes(data_year, value)) +
        
        geom_line(
          color = "grey75",
          linewidth = 1.2
        ) +
        
        geom_point(
          color = "grey75",
          size = 3
        ) +
        
        geom_point(
          data = subset(df, highlighted),
          color = "#2C7FB8",
          size = 5
        ) +
        
        geom_text(
          aes(
            label =
              if (input$byst_metric == "deaths")
                scales::comma(round(value))
            else
              paste0(round(value, 1), "%")
          ),
          vjust = -0.7,
          size = 4
        ) +
        
        scale_x_continuous(
          breaks = df$data_year
        ) +
        
        scale_y_continuous(
          labels =
            if (input$byst_metric == "deaths")
              scales::comma
          else
            function(x) paste0(x, "%"),
          expand = expansion(mult = c(0.05, 0.20))
        ) +
        
        labs(
          title = title,
          subtitle = paste(
            "Selected Year:",
            selected_year()
          ),
          x = "Year",
          y = ifelse(
            input$byst_metric == "deaths",
            "Deaths",
            "Percent"
          )
        ) +
        
        theme_minimal(base_size = 14) +
        
        theme(
          plot.title = element_text(
            face = "bold",
            size = 18
          ),
          axis.text.x = element_text(face = "bold"),
          panel.grid.minor = element_blank()
        )
      
    })
    
  })
  
}