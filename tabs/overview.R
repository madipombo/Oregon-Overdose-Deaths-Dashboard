library(dplyr)
library(tidyr)
library(ggplot2)
# UI
overview_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
# KPI ROW
    
    layout_columns(
      fill = TRUE,
      col_widths = c(2,2,2,2,2,2),
      
      card(full_screen = FALSE, uiOutput(ns("total_deaths_box"))),
      card(full_screen = FALSE, uiOutput(ns("fentanyl_box"))),
      card(full_screen = FALSE, uiOutput(ns("stimulants_box"))),
      card(full_screen = FALSE, uiOutput(ns("naloxone_box"))),
      card(full_screen = FALSE, uiOutput(ns("trt_hist_box"))),
      card(full_screen = FALSE, uiOutput(ns("homeless_box")))
    ),
    
# PLOTS ROW
    layout_columns(
      col_widths = c(6, 6),
# Monthly Trend Card     
      card(
        full_screen = TRUE,
        
        card_header(
          radioButtons(
            ns("time_scale"),
            NULL,
            choices = c(
              "Month" = "month",
              "Quarter" = "quarter"
            ),
            selected = "month",
            inline = TRUE
          )
        ),
        
        plotOutput(ns("monthly_trend"), height = "400px")
      ),
# Heatmap Card      
      card(
        full_screen = TRUE,
        
        card_header(
          radioButtons(
            ns("heatmap_scale"),
            NULL,
            choices = c(
              "Month" = "month",
              "Quarter" = "quarter"
            ),
            selected = "month",
            inline = TRUE
          )
        ),
        
        plotOutput(ns("quarter_heatmap"), height = "400px")
      )
    )
  )
}
# Server
overview_server <- function(
    id,
    annual_data,
    selected_year
) {
  
  moduleServer(id, function(input, output, session) {
    kpi_box <- function(value,
                        label,
                        color = "#2C7FB8",
                        suffix = "") {
      
      display_value <- if (suffix == "%") {
        scales::number(value, accuracy = 0.1)
      } else {
        scales::comma(round(as.numeric(value), 0))
      }
      
      div(
        style = "
      text-align:center;
      padding:15px;
      border-radius:10px;
      background:#f8f9fa;
      border:1px solid #e9ecef;

      height:100%;
      display:flex;
      flex-direction:column;
      justify-content:center;
    ",
        
        div(
          style = paste0(
            "font-size:60px;
         font-weight:800;
         color:", color, ";"
          ),
          paste0(display_value, suffix)
        ),
        
        div(
          style = "font-size:20px; font-weight:500; color:#495057;",
          label
        )
      )
    }
    

# REACTIVE YEAR STATE

    selected_row <- reactive({
      annual_data[
        annual_data$data_year == selected_year(),
      ]
    })
    
    trend_data <- reactive({
      
      if (input$time_scale == "month") {
        
        annual_data |>
          select(
            data_year,
            alldrug_jan_deaths,
            alldrug_feb_deaths,
            alldrug_mar_deaths,
            alldrug_apr_deaths,
            alldrug_may_deaths,
            alldrug_jun_deaths,
            alldrug_jul_deaths,
            alldrug_aug_deaths,
            alldrug_sep_deaths,
            alldrug_oct_deaths,
            alldrug_nov_deaths,
            alldrug_dec_deaths
          ) |>
          pivot_longer(
            -data_year,
            names_to = "period",
            values_to = "deaths"
          ) |>
          mutate(
            period = factor(
              c(
                "Jan","Feb","Mar","Apr","May","Jun",
                "Jul","Aug","Sep","Oct","Nov","Dec"
              )[match(
                period,
                c(
                  "alldrug_jan_deaths",
                  "alldrug_feb_deaths",
                  "alldrug_mar_deaths",
                  "alldrug_apr_deaths",
                  "alldrug_may_deaths",
                  "alldrug_jun_deaths",
                  "alldrug_jul_deaths",
                  "alldrug_aug_deaths",
                  "alldrug_sep_deaths",
                  "alldrug_oct_deaths",
                  "alldrug_nov_deaths",
                  "alldrug_dec_deaths"
                )
              )],
              levels = c(
                "Jan","Feb","Mar","Apr","May","Jun",
                "Jul","Aug","Sep","Oct","Nov","Dec"
              )
            )
          )
        
      } else {
        
        annual_data |>
          select(
            data_year,
            alldrug_q1_deaths,
            alldrug_q2_deaths,
            alldrug_q3_deaths,
            alldrug_q4_deaths
          ) |>
          pivot_longer(
            -data_year,
            names_to = "period",
            values_to = "deaths"
          ) |>
          mutate(
            period = factor(
              c("Q1","Q2","Q3","Q4")[match(
                period,
                c(
                  "alldrug_q1_deaths",
                  "alldrug_q2_deaths",
                  "alldrug_q3_deaths",
                  "alldrug_q4_deaths"
                )
              )],
              levels = c("Q1","Q2","Q3","Q4")
            )
          )
        
      }
      
    })
    
    trend_plot_data <- reactive({
      
      trend_data() |>
        mutate(
          highlighted = data_year == selected_year()
        )
      
    })
    
    heatmap_data <- reactive({
      
      if (input$heatmap_scale == "month") {
        
        annual_data |>
          select(
            data_year,
            alldrug_jan_deaths,
            alldrug_feb_deaths,
            alldrug_mar_deaths,
            alldrug_apr_deaths,
            alldrug_may_deaths,
            alldrug_jun_deaths,
            alldrug_jul_deaths,
            alldrug_aug_deaths,
            alldrug_sep_deaths,
            alldrug_oct_deaths,
            alldrug_nov_deaths,
            alldrug_dec_deaths
          ) |>
          pivot_longer(
            -data_year,
            names_to = "period",
            values_to = "deaths"
          ) |>
          mutate(
            period = factor(
              c(
                "Jan","Feb","Mar","Apr","May","Jun",
                "Jul","Aug","Sep","Oct","Nov","Dec"
              )[match(
                period,
                c(
                  "alldrug_jan_deaths",
                  "alldrug_feb_deaths",
                  "alldrug_mar_deaths",
                  "alldrug_apr_deaths",
                  "alldrug_may_deaths",
                  "alldrug_jun_deaths",
                  "alldrug_jul_deaths",
                  "alldrug_aug_deaths",
                  "alldrug_sep_deaths",
                  "alldrug_oct_deaths",
                  "alldrug_nov_deaths",
                  "alldrug_dec_deaths"
                )
              )],
              levels = c(
                "Jan","Feb","Mar","Apr","May","Jun",
                "Jul","Aug","Sep","Oct","Nov","Dec"
              )
            )
          )
        
      } else {
        
        annual_data |>
          select(
            data_year,
            alldrug_q1_deaths,
            alldrug_q2_deaths,
            alldrug_q3_deaths,
            alldrug_q4_deaths
          ) |>
          pivot_longer(
            -data_year,
            names_to = "period",
            values_to = "deaths"
          ) |>
          mutate(
            period = factor(
              c("Q1","Q2","Q3","Q4")[match(
                period,
                c(
                  "alldrug_q1_deaths",
                  "alldrug_q2_deaths",
                  "alldrug_q3_deaths",
                  "alldrug_q4_deaths"
                )
              )],
              levels = c("Q1","Q2","Q3","Q4")
            )
          )
        
      }
      
    })
# Heat map color bins  
    heatmap_data_binned <- reactive({
      
      df <- heatmap_data()
      
      if (input$heatmap_scale == "month") {
        
        df <- df |>
          mutate(
            death_group = cut(
              deaths,
              breaks = c(0, 50, 75, 100, 125, 150, Inf),
              labels = c(
                "<50",
                "50-74",
                "75-99",
                "100-124",
                "125-149",
                "150+"
              ),
              include.lowest = TRUE
            )
          )
        
      } else {
        
        df <- df |>
          mutate(
            death_group = cut(
              deaths,
              breaks = c(0, 200, 300, 400, 500, 600, Inf),
              labels = c(
                "<200",
                "200-299",
                "300-399",
                "400-499",
                "500-599",
                "600+"
              ),
              include.lowest = TRUE
            )
          )
        
      }
      
      df
      
    })
# Summary Box 1    
output$total_deaths_box <- renderUI({
      
      kpi_box(
        selected_row()$alldrug_deaths,
        "Total Deaths"
      )
      
    })
# Summary Box 2
    output$fentanyl_box <- renderUI({
      
      kpi_box(
        selected_row()$opioids_percent,
        "Involved Fentanyl",
        suffix = "%"
      )
      
    })
# Summary Box 3
    output$stimulants_box <- renderUI({
      
      kpi_box(
        selected_row()$stimulant_percent,
        "Involved Stimulants",
        suffix = "%"
      )
      
    })
# Summary Box 4
    output$naloxone_box <- renderUI({
      
      kpi_box(
        selected_row()$naloxone_percent,
        "Had Naloxone Administered",
        suffix = "%"
      )
      
    })
# Summary Box 5
    output$trt_hist_box <- renderUI({
      
      kpi_box(
        selected_row()$ever_SUDtrt_percent,
        "Had a History of Substance Abuse Treatment",
        suffix = "%"
      )
      
    })
# Summary Box 6
    output$homeless_box <- renderUI({
      
      kpi_box(
        selected_row()$homeless_percent,
        "Were Experiencing Homelessness",
        suffix = "%"
      )
      
    })
    

# PLOTS
# Monthly trend line plot
    output$monthly_trend <- renderPlot({
      
      ggplot(
        trend_plot_data(),
        aes(
          x = period,
          y = deaths,
          group = data_year
        )
      ) +
        
        # Other years
        geom_line(
          data = subset(
            trend_plot_data(),
            !highlighted
          ),
          color = "grey75",
          linewidth = 1
        ) +
        
        geom_point(
          data = subset(
            trend_plot_data(),
            !highlighted
          ),
          color = "grey75",
          size = 2
        ) +
        
        # Selected year
        geom_line(
          data = subset(
            trend_plot_data(),
            highlighted
          ),
          color = "#2C7FB8",
          linewidth = 2
        ) +
        
        geom_point(
          data = subset(
            trend_plot_data(),
            highlighted
          ),
          color = "#2C7FB8",
          size = 4
        ) +
        
        # Year labels at end of lines
        geom_text(
          data = trend_plot_data() |>
            dplyr::group_by(data_year) |>
            dplyr::slice_tail(n = 1),
          
          aes(label = data_year),
          
          hjust = -0.3,
          size = 4.5,
          fontface = "bold"
        ) +
        
        coord_cartesian(clip = "off") +
        
        scale_y_continuous(
          labels = scales::comma
        ) +
        
        labs(
          title = "All Drug Overdose Deaths",
          subtitle = paste(
            "Selected Year:",
            selected_year()
          ),
          x = NULL,
          y = "Number of Deaths"
        ) +
        
        theme_minimal(base_size = 14) +
        
        theme(
          plot.title = element_text(
            face = "bold",
            size = 18
          ),
          
          plot.subtitle = element_text(
            size = 13
          ),
          
          axis.text.x = element_text(
            face = "bold"
          ),
          
          panel.grid.minor = element_blank(),
          
          plot.margin = margin(
            10,
            50,
            10,
            10
          )
        )
      
    })
# Heat map    
    output$quarter_heatmap <- renderPlot({
      
      ggplot(
        heatmap_data_binned(),
        aes(
          x = period,
          y = factor(data_year),
          fill = death_group
        )
      ) +
        
        geom_tile(
          color = "white",
          linewidth = 0.5
        ) +
        
        geom_tile(
          data = subset(
            heatmap_data(),
            data_year == selected_year()
          ),
          color = "#6C757D",
          linewidth = 2,
          fill = NA
        ) +
        
        geom_text(
          aes(label = scales::comma(deaths)),
          size = 4
        ) +
        
        scale_fill_brewer(
          palette = "Blues",
          name = "Deaths"
        ) +
        
        labs(
          title = "Overdose Death Heatmap",
          subtitle = ifelse(
            input$heatmap_scale == "month",
            "Monthly deaths by year",
            "Quarterly deaths by year"
          ),
          x = NULL,
          y = "Year",
          fill = "Deaths"
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
          
          axis.text.y = element_text(
            face = "bold"
          ),
          
          panel.grid = element_blank()
        )
      
    })
  })
}