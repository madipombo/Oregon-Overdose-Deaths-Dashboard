
# Circumstances UI

circumstances_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    

# Top Homelessness Card

    card(
      full_screen = TRUE,
      
      card_header(
        radioButtons(
          ns("homeless_metric"),
          NULL,
          choices = c(
            "Count" = "deaths",
            "Percent" = "percent"
          ),
          selected = "percent",
          inline = TRUE
        )
      ),
      
      layout_columns(
        
        col_widths = c(8, 4),
        
        plotOutput(
          ns("homelessness_plot"),
          height = "450px"
        ),
# Homelessness summary card        
        card(
          card_header("Summary"),
          style = "
    background-color:#F8FAFC;
    border-left:6px solid #2C7FB8;
  ",
          htmlOutput(ns("homeless_summary"))
        )
      )
    ),
    

# Bottom Circumstances Card
    card(
      full_screen = TRUE,
      
      card_header(
        
        div(
          style = "
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 16px;
      flex-wrap: wrap;
    ",
          
# Left metric toggle
          div(
            style = "display: flex; align-items: center; gap: 10px;",
            radioButtons(
              ns("circ_metric"),
              NULL,
              choices = c(
                "Count" = "deaths",
                "Percent" = "percent"
              ),
              selected = "percent",
              inline = TRUE
            )
          ),
          
# Right dropdown menu
          div(
            style = "flex: 1; min-width: 320px;",
            selectInput(
              ns("circ_measure"),
              NULL,
              choices = c(
                "Previous Mental Health Diagnosis" = "mhdiag",
                "Recently Released From an Institutional Setting" = "release",
                "Current Substance Use Treatment" = "sud_curr",
                "Current Mental Health or Substance Use Treatment" = "mh_sud_curr",
                "History of Mental Health or Substance Use Treatment" = "mh_sud_ever",
                "History of Suicide Attempt, Suicidal Ideation, or Self-Harm" = "selfharm"
              ),
              selected = "mhdiag",
              width = "100%"
            )
          )
        )
      ),
      
      layout_columns(
        
        col_widths = c(8,4),
        
        plotOutput(
          ns("circ_plot"),
          height = "450px"
        ),
# Circumstances Summary Card     
        card(
          card_header("Summary"),
          style = "
    background-color:#F8FAFC;
    border-left:6px solid #2C7FB8;
  ",
          htmlOutput(ns("circ_summary"))
        )
      )
    )
  )
}



# Circumstances tab server


circumstances_server <- function(id, annual_data, selected_year) {
  
  moduleServer(id, function(input, output, session) {
    

# Homelessness Plot

    output$homelessness_plot <- renderPlot({
      
      if (input$homeless_metric == "deaths") {
        
        df <- annual_data |>
          dplyr::select(data_year, homeless_deaths) |>
          dplyr::rename(value = homeless_deaths)
        
        y_lab <- "Deaths"
        title <- "Homelessness Among Overdose Decedents"
        
      } else {
        
        df <- annual_data |>
          dplyr::select(data_year, homeless_percent) |>
          dplyr::rename(value = homeless_percent)
        
        y_lab <- "Percent"
        title <- "Homelessness Among Overdose Decedents"
      }
      
      df <- df |>
        dplyr::mutate(highlighted = data_year == selected_year())
      
      ggplot(df, aes(data_year, value)) +
        geom_line(color = "grey75", linewidth = 1.2) +
        geom_point(color = "grey75", size = 3) +
        geom_point(data = subset(df, highlighted),
                   color = "#2C7FB8", size = 5) +
        geom_text(
          aes(label = if (input$homeless_metric == "deaths")
            scales::comma(value)
            else
              paste0(round(value, 1), "%")),
          vjust = -0.7,
          size = 4
        ) +
        scale_x_continuous(breaks = df$data_year) +
        scale_y_continuous(
          labels = if (input$homeless_metric == "deaths")
            scales::comma
          else
            function(x) paste0(x, "%"),
          expand = expansion(mult = c(0.05, 0.2))
        ) +
        labs(
          title = title,
          subtitle = paste("Selected Year:", selected_year()),
          x = "Year",
          y = y_lab
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", size = 18),
          axis.text.x = element_text(face = "bold"),
          panel.grid.minor = element_blank()
        )
      
    })
# Homelessness Summary    
    output$homeless_summary <- renderUI({
      
      row <- annual_data |>
        dplyr::filter(data_year == selected_year())
      
      if (input$homeless_metric == "deaths") {
        
        value <- row$homeless_deaths
        
        value_text <- scales::comma(value)
        
        summary_text <- paste0(
          value_text,
          " drug overdose decedents were experiencing homelessness."
        )
        
      } else {
        
        value <- row$homeless_percent
        
        value_text <- paste0(round(value, 1), "%")
        
        summary_text <- paste0(
          value_text,
          " of overdose decedents were experiencing homelessness."
        )
        
      }
      
      HTML(
        paste0(
          "<div style='padding:20px;'>",
          
          "<div style='font-size:42px;
                   font-weight:700;
                   color:#2C7FB8;
                   line-height:1;'>",
          value_text,
          "</div>",
          
          "<div style='font-size:18px;
                   color:#666;
                   margin-top:5px;'>",
          selected_year(),
          "</div>",
          
          "<hr style='margin-top:15px;
                  margin-bottom:15px;'>",
          
          "<div style='font-size:16px;
                   line-height:1.6;'>",
          summary_text,
          "</div>",
          
          "</div>"
        )
      )
      
    })
    

# Circumstances Plot

    output$circ_plot <- renderPlot({
      
      var <- switch(
        input$circ_measure,
        
        "mhdiag" = if (input$circ_metric == "deaths")
          "mhdiag_deaths" else "mhdiag_percent",
        
        "release" = if (input$circ_metric == "deaths")
          "recentrelease_deaths" else "recentrelease_percent",
        
        "sud_curr" = if (input$circ_metric == "deaths")
          "curr_SUDtrt_deaths" else "curr_SUDtrt_percent",
        
        "mh_sud_curr" = if (input$circ_metric == "deaths")
          "curr_MHSUDtrt_deaths" else "curr_MHSUDtrt_percent",
        
        "mh_sud_ever" = if (input$circ_metric == "deaths")
          "ever_MHSUDtrt_deaths" else "ever_MHSUDtrt_percent",
        
        "selfharm" = if (input$circ_metric == "deaths")
          "hx_selfharm_deaths" else "hx_selfharm_percent"
      )
      
      title <- switch(
        input$circ_measure,
        
        "mhdiag" = "Previous Mental Health Diagnosis",
        
        "release" = "Recently Released From an Institutional Setting",
        
        "sud_curr" = "Current Substance Use Treatment",
        
        "mh_sud_curr" = "Current Mental Health or Substance Use Treatment",
        
        "mh_sud_ever" = "History of Mental Health or Substance Use Treatment",
        
        "selfharm" = "History of Suicide Attempt, Suicidal Ideation, or Self-Harm"
      )
      
      df <- annual_data |>
        dplyr::select(data_year, all_of(var)) |>
        dplyr::rename(value = all_of(var)) |>
        dplyr::mutate(highlighted = data_year == selected_year())
      
      ggplot(df, aes(data_year, value)) +
        geom_line(color = "grey75", linewidth = 1.2) +
        geom_point(color = "grey75", size = 3) +
        geom_point(data = subset(df, highlighted),
                   color = "#2C7FB8", size = 5) +
        geom_text(
          aes(label = if (input$circ_metric == "deaths")
            scales::comma(round(value))
            else
              paste0(round(value, 1), "%")),
          vjust = -0.7,
          size = 4
        ) +
        scale_x_continuous(breaks = df$data_year) +
        scale_y_continuous(
          labels = if (input$circ_metric == "deaths")
            scales::comma
          else
            function(x) paste0(x, "%"),
          expand = expansion(mult = c(0.05, 0.2))
        ) +
        labs(
          title = title,
          subtitle = paste("Selected Year:", selected_year()),
          x = "Year",
          y = ifelse(input$circ_metric == "deaths", "Deaths", "Percent")
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", size = 18),
          axis.text.x = element_text(face = "bold"),
          panel.grid.minor = element_blank()
        )
      
    })

# Circumstances Summary
    
    output$circ_summary <- renderUI({
      
      metric_suffix <- ifelse(
        input$circ_metric == "deaths",
        "_deaths",
        "_percent"
      )
      
      var <- switch(
        input$circ_measure,
        
        "mhdiag" = paste0("mhdiag", metric_suffix),
        
        "release" = paste0("recentrelease", metric_suffix),
        
        "sud_curr" = paste0("curr_SUDtrt", metric_suffix),
        
        "mh_sud_curr" = paste0("curr_MHSUDtrt", metric_suffix),
        
        "mh_sud_ever" = paste0("ever_MHSUDtrt", metric_suffix),
        
        "selfharm" = paste0("hx_selfharm", metric_suffix)
      )
      
      row <- annual_data |>
        dplyr::filter(data_year == selected_year())
      
      value <- row[[var]]
      
      summary_text <- switch(
        
        input$circ_measure,
        
        "mhdiag" =
          "had a documented mental health diagnosis.",
        
        "release" =
          "had recently been released from an institutional setting.",
        
        "sud_curr" =
          "were currently receiving treatment for a substance use disorder.",
        
        "mh_sud_curr" =
          "were currently receiving mental health and/or substance use treatment.",
        
        "mh_sud_ever" =
          "had a history of mental health and/or substance use treatment.",
        
        "selfharm" =
          "had a history of suicide attempt, suicidal ideation, or self-harm."
      )
      
      value_text <- if (input$circ_metric == "deaths") {
        
        scales::comma(round(value))
        
      } else {
        
        paste0(round(value,1), "%")
        
      }
      
      HTML(
        paste0(
          "<div style='padding:20px;'>",
          
          "<div style='font-size:42px;
                 font-weight:700;
                 color:#2C7FB8;
                 line-height:1;'>",
          value_text,
          "</div>",
          
          "<div style='font-size:18px;
                 color:#666;
                 margin-top:5px;'>",
          selected_year(),
          "</div>",
          
          "<hr style='margin-top:15px;
                margin-bottom:15px;'>",
          
          "<div style='font-size:16px;
                 line-height:1.6;'>",
          if (input$circ_metric == "deaths") {
            paste0(
              value_text,
              " overdose decedents ",
              summary_text
            )
          } else {
            paste0(
              value_text,
              " of overdose decedents ",
              summary_text
            )
          },
          "</div>",
          
          "</div>"
        )
      )
      
    })
    
  })
}