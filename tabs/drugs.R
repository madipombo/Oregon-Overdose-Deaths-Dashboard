
#Drugs Tab UI
drugs_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
# Top row
# Full Width Trends
    card(
      full_screen = TRUE,
      
      layout_sidebar(
        
        sidebar = sidebar(
          
          width = 300,
          
          radioButtons(
            ns("trend_time"),
            "Time Scale",
            choices = c(
              "Month" = "month",
              "Quarter" = "quarter"
            ),
            selected = "month"
          ),
          
          checkboxGroupInput(
            ns("trend_drugs"),
            "Drug Categories",
            choices = c(
              "All Drugs" = "alldrug",
              "Opioids" = "opioids",
              "Illicit Fentanyl" = "imfs",
              "Heroin" = "heroin",
              "Prescription Opioids" = "rxopioids",
              "Stimulants" = "stimulant",
              "Cocaine" = "cocaine",
              "Methamphetamine" = "meth",
              "Non-Opioid Sedatives" = "nonopioid_sedatives",
              "Benzodiazepines" = "benzodiazepines"
            ),
            selected = c(
              "alldrug",
              "opioids"
            )
          )
        ),
        
        plotOutput(
          ns("drug_trend"),
          height = "650px"
        )
      )
    ),

# BOTTOM ROW

    layout_columns(
      col_widths = c(6, 6),
# Upset Plot 
      card(
        full_screen = TRUE,
        
        card_header(
          layout_columns(
            col_widths = c(3, 3, 6),
            
            radioButtons(
              ns("upset_metric"), NULL,
              choices = c("Count"="deaths","Percent"="percent"),
              selected = "deaths",
              inline = TRUE
            ),
            
            radioButtons(
              ns("upset_view"), NULL,
              choices = c(
                "Single Year" = "single",
                "All Years Trend" = "trend"
              ),
              selected = "single",
              inline = TRUE
            ),
            
            conditionalPanel(
              condition = sprintf("input['%s'] == 'trend'", ns("upset_view")),
              
              shinyWidgets::pickerInput(
                ns("upset_interactions"),
                label = NULL,
                choices = c(
                  "Opioids + Stimulants",
                  "Opioids + Cocaine",
                  "Opioids + Meth",
                  "Opioids + Benzos",
                  "Opioids + Non-Op Sed.",
                  "Opioids Only"
                ),
                selected = c(
                  "Opioids + Stimulants",
                  "Opioids Only"
                ),
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE,
                  `selected-text-format` = "count > 2",
                  size = 6
                )
              )
            )
          )
        ),
        
        plotOutput(ns("drug_upset"))
      ),
# Routes of Administration Plot      
      card(
        full_screen = TRUE,
        
        card_header(
          
          layout_columns(
            col_widths = c(3, 3, 6),
            
            radioButtons(
              ns("route_metric"),
              NULL,
              choices = c(
                "Count" = "deaths",
                "Percent" = "percent"
              ),
              selected = "percent",
              inline = TRUE
            ),
            
            radioButtons(
              ns("route_view"),
              NULL,
              choices = c(
                "Single Year" = "single",
                "Year Comparison" = "compare"
              ),
              selected = "single",
              inline = TRUE
            ),
            
            conditionalPanel(
              condition = sprintf(
                "input['%s'] == 'compare'",
                ns("route_view")
              ),
              
              selectInput(
                ns("route_compare_year"),
                "Compare To:",
                choices = NULL
              )
            )
          )
        ),
        
        plotOutput(
          ns("route_plot"),
          height = "500px"
        )
      )
    )
  )
}

# DRUGS TAB SERVER

drugs_server <- function(
    id,
    annual_data,
    selected_year
) {
  
  moduleServer(id, function(input, output, session) {
    
    upset_data_long <- reactive({
      
      yrs <- sort(unique(annual_data$data_year))
      
      purrr::map_dfr(yrs, function(y) {
        
        row <- annual_data[annual_data$data_year == y, ]
        
        tibble(
          year = y,
          interaction = c(
            "Opioids + Stimulants",
            "Opioids + Cocaine",
            "Opioids + Meth",
            "Opioids + Benzos",
            "Opioids + Non-Op Sed.",
            "Opioids Only"
          ),
          deaths = c(
            row$opioids_anystim_deaths,
            row$opioids_cocaine_deaths,
            row$opioids_meth_deaths,
            row$opioids_benzo_deaths,
            row$opioids_nonop_sed_deaths,
            row$opioids_only_deaths
          ),
          percent = c(
            row$opioids_anystim_percentall,
            row$opioids_cocaine_percentall,
            row$opioids_meth_percentall,
            row$opioids_benzo_percentall,
            row$opioids_nonop_sed_percentall,
            row$opioids_only_percentall
          )
        )
      })
    })
    single_data <- reactive({
      df <- upset_data_long()
      df |> dplyr::filter(year == selected_year())
    })
    trend_data <- reactive({
      
      if (is.null(input$upset_interactions) ||
          length(input$upset_interactions) == 0) {
        
        return(
          upset_data_long()[0, ]
        )
        
      }
      
      upset_data_long() |>
        dplyr::filter(
          interaction %in% input$upset_interactions
        )
      
    })
    
    observe({
      
      yrs <- sort(unique(annual_data$data_year))
      
      current <- selected_year()
      
      comparison_default <- if (
        current < max(yrs)
      ) {
        min(yrs[yrs > current])
      } else {
        max(yrs[yrs < current])
      }
      
      updateSelectInput(
        session,
        "route_compare_year",
        choices = yrs[yrs != current],
        selected = comparison_default
      )
      
    })
    
    observeEvent(TRUE, {
      updatePickerInput(
        session,
        "upset_interactions",
        selected = c(
          "Opioids + Stimulants",
          "Opioids Only"
        )
      )
    }, once = TRUE)
    
# Drug involvement bar chart
    output$drug_bar <- renderPlot({
      
      barplot(
        1:5,
        main = "Drug Bar"
      )
      
    })
    
    # Drug trend plot
    output$drug_trend <- renderPlot({
      
      req(input$trend_drugs)
      
      if (input$trend_time == "month") {
        
        suffixes <- c(
          "jan","feb","mar","apr","may","jun",
          "jul","aug","sep","oct","nov","dec"
        )
        
        time_labels <- month.abb
        
      } else {
        
        suffixes <- c(
          "q1","q2","q3","q4"
        )
        
        time_labels <- c(
          "Q1","Q2","Q3","Q4"
        )
        
      }
      
      trend_df <- purrr::map_dfr(
        input$trend_drugs,
        function(drug){
          
          vars <- paste0(
            drug,
            "_",
            suffixes,
            "_deaths"
          )
          
          vals <- annual_data |>
            dplyr::filter(
              data_year == selected_year()
            ) |>
            dplyr::select(
              all_of(vars)
            ) |>
            unlist(use.names = FALSE)
          
          tibble(
            Drug = drug,
            Time = factor(
              time_labels,
              levels = time_labels
            ),
            Deaths = as.numeric(vals)
          )
          
        }
      )
      
      drug_labels <- c(
        alldrug = "All Drugs",
        opioids = "Opioids",
        imfs = "Illicit Fentanyl",
        heroin = "Heroin",
        rxopioids = "Prescription Opioids",
        stimulant = "Stimulants",
        cocaine = "Cocaine",
        meth = "Methamphetamine",
        nonopioid_sedatives = "Non-Opioid Sedatives",
        benzodiazepines = "Benzodiazepines"
      )
      
      trend_df$Drug <- factor(
        trend_df$Drug,
        levels = names(drug_labels),
        labels = drug_labels
      )
      
      drug_colors <- c(
        "All Drugs" = "#1F77B4",
        "Opioids" = "#D62728",
        "Illicit Fentanyl" = "#9467BD",
        "Heroin" = "#8C564B",
        "Prescription Opioids" = "#E377C2",
        "Stimulants" = "#FF7F0E",
        "Cocaine" = "#2CA02C",
        "Methamphetamine" = "#17BECF",
        "Non-Opioid Sedatives" = "#7F7F7F",
        "Benzodiazepines" = "#BCBD22"
      )
      
      ggplot(
        trend_df,
        aes(
          x = Time,
          y = Deaths,
          color = Drug,
          group = Drug
        )
      ) +
        
        geom_line(
          linewidth = 1.3
        ) +
        
        geom_point(
          size = 3
        ) +
        
        scale_color_manual(
          values = drug_colors
        ) +
        
        labs(
          title = "Drug Trends",
          subtitle = paste(
            "Selected Year:",
            selected_year()
          ),
          x = NULL,
          y = "Deaths",
          color = NULL
        ) +
        
        theme_minimal(base_size = 14) +
        
        theme(
          plot.title = element_text(
            face = "bold",
            size = 18
          ),
          legend.position = "bottom",
          legend.title = element_blank(),
          plot.title.position = "plot"
        ) +
        guides(
          color = guide_legend(
            nrow = 2,
            byrow = TRUE
          )
        )
    })
    
# Drug co-occurrence plot
    output$drug_upset <- renderPlot({
      
      value_col <- input$upset_metric
      
      interaction_colors <- c(
        "Opioids + Stimulants" = "#1F77B4",
        "Opioids + Cocaine" = "#2CA02C",
        "Opioids + Meth" = "#FF7F0E",
        "Opioids + Benzos" = "#9467BD",
        "Opioids + Non-Op Sed." = "#7F7F7F",
        "Opioids Only" = "#D62728"
      )
      
      df <- if (input$upset_view == "single") {
        single_data()
      } else {
        trend_data()
      }
      
# No interactions selected
      if (input$upset_view == "trend" && nrow(df) == 0) {
        
        return(
          ggplot() +
            annotate(
              "text",
              x = 0.5,
              y = 0.5,
              label = "Please select at least one interaction",
              size = 7,
              color = "#666666",
              fontface = "bold"
            ) +
            coord_cartesian(
              xlim = c(0, 1),
              ylim = c(0, 1)
            ) +
            theme_void()
        )
        
      }
      
      if (input$upset_view == "single") {
        
        ggplot(df, aes(
          x = reorder(interaction, .data[[value_col]]),
          y = .data[[value_col]]
        )) +
          geom_col(fill = "#2C7FB8") +
          coord_flip() +
          labs(
            title = "Opioid Co-Involvement",
            subtitle = paste(
              "Selected Year:",
              selected_year()
            ),
            x = NULL,
            y = ifelse(
              value_col == "deaths",
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
            plot.subtitle = element_text(
              size = 13
            ),
            panel.grid.minor = element_blank(),
            plot.title.position = "plot"
          )
        
      } else {
        
        ggplot(df, aes(
          x = year,
          y = .data[[value_col]],
          color = interaction,
          group = interaction
        )) +
          geom_line(linewidth = 1.2) +
          geom_point(size = 2) +
          scale_color_manual(values = interaction_colors) +
          labs(
            title = "Opioid Co-Involvement Trends",
            subtitle = "All Years",
            x = "Year",
            y = ifelse(
              value_col == "deaths",
              "Deaths",
              "Percent"
            ),
            color = "Interaction"
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
            panel.grid.minor = element_blank(),
            legend.position = "right",
            plot.title.position = "plot"
          )
      }
    })

# Drug Route Plot
output$route_plot <- renderPlot({
  vars <- if (input$route_metric == "deaths") {
    c( "Ingestion" = "ingestion_deaths",
       "Injection" = "injection_deaths", 
       "Smoking" = "smoking_deaths", 
       "Snorting" = "snorting_deaths", 
       "Other Route" = "otherroute_deaths", 
       "Unknown" = "noroute_deaths" ) 
    } else {
      c( "Ingestion" = "ingestion_percent", 
         "Injection" = "injection_percent", 
         "Smoking" = "smoking_percent", 
         "Snorting" = "snorting_percent", 
         "Other Route" = "otherroute_percent", 
         "Unknown" = "noroute_percent" ) 
    } 
# Single Year View
  if (input$route_view == "single") {
    df <- annual_data |>
      dplyr::filter(data_year == selected_year()) |> 
      dplyr::select(all_of(vars)) |> 
      dplyr::mutate( 
        dplyr::across( 
          everything(),
          ~ as.numeric(.) 
          ) 
        ) |> 
      tidyr::pivot_longer(
        everything(), 
        names_to = "route", 
        values_to = "value" ) 
    df$route <- names(vars) 
    ggplot( 
      df, 
      aes( x = reorder(route, 
                       value), 
           y = value 
           ) 
      ) + 
      geom_col( 
        fill = "#2C7FB8" ) + 
      geom_text( 
        aes( 
          label = if (input$route_metric == "deaths") 
            scales::comma(round(value)) 
          else 
            paste0(round(value, 1), "%") 
          ),
        hjust = -0.1,
        size = 4 
        ) + 
      coord_flip() + 
      labs( title = "Route of Administration", 
            subtitle = paste( 
              "Selected Year:",
              selected_year()
              ),
            x = NULL,
            y = ifelse(
              input$route_metric == "deaths",
              "Deaths",
              "Percent"
              )
            ) + 
      theme_minimal(base_size = 14) + 
      theme( 
        plot.title = element_text(
          face = "bold",
          size = 18 ),
        panel.grid.minor = element_blank(),
        plot.title.position = "plot"
        ) 
    } 
# Year Comparison View
  else { 
    compare_year <- as.numeric( 
      input$route_compare_year ) 
    years_to_show <- c( 
      selected_year(), 
      compare_year ) 
    df <- annual_data |> 
      dplyr::filter(
        data_year %in% years_to_show 
        ) |>
      dplyr::select(
        data_year,
        all_of(vars) ) |> 
      dplyr::mutate(
        dplyr::across(
          -data_year,
          ~ as.numeric(.)
          )
        ) |> 
      tidyr::pivot_longer(
        -data_year,
        names_to = "route",
        values_to = "value" )
    route_labels <- names(vars)
    df$route <- factor( rep(route_labels, times = 2), levels = route_labels ) 
    df$data_year <- factor( df$data_year, levels = years_to_show ) 
    fill_colors <- c(
      "#2C7FB8",  # selected year
      "#A6CEE3"   # comparison year
    )
    
    names(fill_colors) <- c(
      as.character(selected_year()),
      as.character(compare_year)
    )
    ggplot( df, aes( x = route, y = value, fill = data_year ) ) + 
      geom_col( position = position_dodge(width = 0.8) ) + 
      geom_text( aes( label = 
                        if (input$route_metric == "deaths") 
                          scales::comma(round(value)) 
                      else paste0(round(value, 1), "%") ),
                 position = position_dodge(width = 0.8), vjust = -0.3, size = 3 ) + 
      labs( title = "Route of Administration Comparison",
            subtitle = paste( selected_year(), "vs", compare_year ),
            x = NULL,
            y = ifelse( input$route_metric == "deaths", "Deaths", "Percent" ),
            fill = "Year" ) + 
      theme_minimal(base_size = 14) + 
      theme( plot.title = element_text( face = "bold", size = 18 ),
             axis.text.x = element_text( hjust = 1 ),
             panel.grid.minor = element_blank(),
             plot.title.position = "plot") + 
      scale_y_continuous(
        limits = if (input$route_metric == "percent") c(0, 60) else c(0, NA), 
        labels = if (input$route_metric == "percent") function(x) paste0(x, "%") 
        else scales::comma, expand = expansion(mult = c(0, 0.15)) ) +
      scale_fill_manual(
        values = fill_colors
      )
    } 
  })
  }) 
}