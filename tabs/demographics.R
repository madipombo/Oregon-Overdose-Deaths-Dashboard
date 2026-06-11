# DEMOGRAPHICS TAB UI

demographics_ui <- function(id,
                            annual_data,
                            selected_year) {
  
  ns <- NS(id)
  
  layout_columns(
    col_widths = c(6, 6),
# Age group card    
    card(
      full_screen = TRUE,
      
      card_header(
        radioButtons(
          ns("age_view"),
          NULL,
          choices = c(
            "Selected Year" = "single",
            "Year Comparison" = "compare"
          ),
          selected = "single",
          inline = TRUE
        )
      ),
      
      plotOutput(ns("age_group_plot"))
    ),
# Sex plot card    
    card(
      full_screen = TRUE,
      
      card_header(
        
        div(
          style = "display:flex; gap:30px; flex-wrap:wrap;",
          
          radioButtons(
            ns("sex_metric"),
            NULL,
            choices = c(
              "Counts" = "deaths",
              "Rate" = "rate",
              "Percent" = "percent"
            ),
            selected = "deaths",
            inline = TRUE
          ),
          
          radioButtons(
            ns("sex_view"),
            NULL,
            choices = c(
              "Selected Year" = "single",
              "Year Comparison" = "compare"
            ),
            selected = "single",
            inline = TRUE
          )
          
        )
        
      ),
      
      plotOutput(ns("sex_plot"))
    ),
# Race plot card
    card(
      full_screen = TRUE,
      
      card_header(
        tagList(
          radioButtons(
            ns("race_view"),
            NULL,
            choices = c("Single Year" = "single", "Year Comparison" = "compare"),
            selected = "single",
            inline = TRUE
          ),
          
          radioButtons(
            ns("race_metric"),
            NULL,
            choices = c(
              "Deaths" = "deaths",
              "Rate" = "rate"
            ),
            selected = "deaths",
            inline = TRUE
          )
        )
      ),
      
      plotOutput(ns("race_plot"), height = "420px")
    ),
# Age-sex heatmap card
    card(
      full_screen = TRUE,
      
      card_header(
        radioButtons(
          ns("age_sex_metric"),
          NULL,
          choices = c(
            "Deaths" = "deaths",
            "Rate" = "rate"
          ),
          selected = "deaths",
          inline = TRUE
        )
      ),
      
      plotOutput(ns("age_sex_heatmap"))
    )
  )
  
}

# Demographics tab server


demographics_server <- function(
    id,
    annual_data,
    selected_year
) {moduleServer(id, function(input, output, session) {
    
    # Data for selected year
    selected_row <- reactive({
      
      annual_data[
        annual_data$data_year == selected_year(),
      ]
      
    })
    
    sex_data <- reactive({
      
      if (input$sex_metric == "deaths") {
        
        annual_data |>
          select(
            data_year,
            male_deaths,
            female_deaths
          ) |>
          pivot_longer(
            -data_year,
            names_to = "sex",
            values_to = "value"
          ) |>
          mutate(
            sex = recode(
              sex,
              male_deaths = "Male",
              female_deaths = "Female"
            )
          )
        
      } else if (input$sex_metric == "rate") {
        
        annual_data |>
          select(
            data_year,
            male_rate,
            female_rate
          ) |>
          pivot_longer(
            -data_year,
            names_to = "sex",
            values_to = "value"
          ) |>
          mutate(
            sex = recode(
              sex,
              male_rate = "Male",
              female_rate = "Female"
            )
          )
        
      } else {
        
        annual_data |>
          select(
            data_year,
            male_percent,
            female_percent
          ) |>
          pivot_longer(
            -data_year,
            names_to = "sex",
            values_to = "value"
          ) |>
          mutate(
            sex = recode(
              sex,
              male_percent = "Male",
              female_percent = "Female"
            )
          )
        
      }
      
    })
    
    sex_titles <- reactive({
      
      switch(
        input$sex_metric,
        
        deaths = list(
          title = "Overdose Deaths by Sex",
          ylab = "Deaths"
        ),
        
        rate = list(
          title = "Overdose Death Rates by Sex",
          ylab = "Rate per 100,000"
        ),
        
        percent = list(
          title = "Sex Distribution of Overdose Deaths",
          ylab = "Percent"
        )
      )
      
    })

# Age Groups Plot
    output$age_group_plot <- renderPlot({
      

# Single Year Bar Chart
      
      if (input$age_view == "single") {
        
        df <- selected_row()
        
        age_df <- tibble(
          age_group = factor(
            c(
              "<15",
              "15-24",
              "25-34",
              "35-44",
              "45-54",
              "55-64",
              "65+"
            ),
            levels = c(
              "<15",
              "15-24",
              "25-34",
              "35-44",
              "45-54",
              "55-64",
              "65+"
            )
          ),
          deaths = c(
            df$age_under15_deaths,
            df$age_15_24_deaths,
            df$age_25_34_deaths,
            df$age_35_44_deaths,
            df$age_45_54_deaths,
            df$age_55_64_deaths,
            df$age_65plus_deaths
          )
        )
        
        ggplot(
          age_df,
          aes(
            x = age_group,
            y = deaths
          )
        ) +
          geom_col(
            fill = "#2C7FB8",
            width = .75
          ) +
          geom_text(
            aes(label = scales::comma(deaths)),
            vjust = -0.4,
            size = 4
          ) +
          scale_y_continuous(
            labels = scales::comma,
            expand = expansion(mult = c(0, .1))
          ) +
          labs(
            title = "Overdose Deaths by Age Group",
            subtitle = paste("Selected Year:", selected_year()),
            x = "Age Group",
            y = "Deaths"
          ) +
          theme_minimal(base_size = 14) +
          theme(
            plot.title = element_text(
              face = "bold",
              size = 18
            )
          )
        
      } else {
        

# Year Comparison Heat map

        
        age_heatmap <- annual_data |>
          transmute(
            data_year,
            
            `<15`   = age_under15_deaths,
            `15-24` = age_15_24_deaths,
            `25-34` = age_25_34_deaths,
            `35-44` = age_35_44_deaths,
            `45-54` = age_45_54_deaths,
            `55-64` = age_55_64_deaths,
            `65+`   = age_65plus_deaths
          ) |>
          pivot_longer(
            -data_year,
            names_to = "age_group",
            values_to = "deaths"
          ) |>
          mutate(
            death_cat = case_when(
              deaths < 50  ~ "<50",
              deaths < 100 ~ "50–99",
              deaths < 200 ~ "100–199",
              deaths < 350 ~ "200–349",
              TRUE         ~ "350+"
            ),
            death_cat = factor(
              death_cat,
              levels = c(
                "<50",
                "50–99",
                "100–199",
                "200–349",
                "350+"
              )
            )
          )
        
        ggplot(
          age_heatmap,
          aes(
            x = factor(data_year),
            y = factor(
              age_group,
              levels = c(
                "<15",
                "15-24",
                "25-34",
                "35-44",
                "45-54",
                "55-64",
                "65+"
              )
            ),
            fill = death_cat
          )
        ) +
          
          geom_tile(
            color = "white",
            linewidth = .5
          ) +
          
          # Highlight selected year
          geom_tile(
            data = subset(
              age_heatmap,
              data_year == selected_year()
            ),
            fill = NA,
            color = "#6C757D",
            linewidth = 2
          ) +
          
          scale_fill_manual(
            values = c(
              "<50"     = "#85C1E9",
              "50–99"   = "#D6EAF8",
              "100–199" = "#F7F7F7",
              "200–349" = "#F5B7B1",
              "350+"    = "#C0392B"
            ),
            name = "Deaths"
          ) +
          
          labs(
            title = "Age Group Comparison Across Years",
            subtitle = paste("Selected Year:", selected_year()),
            x = "Year",
            y = "Age Group",
            fill = "Deaths"
          ) +
          geom_text(
            aes(label = scales::comma(deaths)),
            size = 3.5
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
        
      }
      
    })
  
# Sex plot  
    output$sex_plot <- renderPlot({
      
      plot_df <- sex_data()
      
      plot_df <- plot_df |>
        mutate(
          highlighted = data_year == selected_year()
        )
      
      titles <- sex_titles()
      

# Single Year

      
      if (input$sex_view == "single") {
        
        plot_df <- plot_df |>
          filter(
            data_year == selected_year()
          )
        
        if (input$sex_metric == "percent") {
          
          ggplot(
            plot_df,
            aes(
              x = "All Deaths",
              y = value,
              fill = sex
            )
          ) +
            scale_fill_manual(
              values = c(
                "Male" = "#4C78A8",
                "Female" = "#E15759"
              )
            ) +
            geom_col(width = .6) +
            geom_text(
              aes(
                label = paste0(
                  round(value, 1),
                  "%"
                )
              ),
              position = position_stack(vjust = .5),
              color = "white",
              fontface = "bold",
              size = 5
            ) +
            labs(
              title = titles$title,
              subtitle = paste("Selected Year:", selected_year()),
              x = NULL,
              y = titles$ylab
            ) +
            theme_minimal(base_size = 14) +
            theme(
              plot.title = element_text(
                face = "bold",
                size = 18
              )
            )
          
        } else {
          
          ggplot(
            plot_df,
            aes(
              x = sex,
              y = value,
              fill = sex
            )
          ) +
            geom_col(width = .7) +
            scale_fill_manual(
              values = c(
                "Male" = "#4C78A8",
                "Female" = "#E15759"
              )
            ) +
            geom_text(
              aes(
                label =
                  if(input$sex_metric == "deaths")
                    scales::comma(value)
                else
                  round(value,1)
              ),
              vjust = -0.4,
              size = 5
            ) +
            labs(
              title = titles$title,
              subtitle = paste("Selected Year:", selected_year()),
              x = NULL,
              y = titles$ylab
            ) +
            theme_minimal(base_size = 14) +
            theme(
              legend.position = "none",
              plot.title = element_text(
                face = "bold",
                size = 18
              )
            ) +
            scale_y_continuous(
              expand = expansion(mult = c(0, 0.15))
            )
          
        }
        

# Year Comparison

        
      } else {
        
        if (input$sex_metric == "percent") {
          
          ggplot(
            plot_df,
            aes(
              x = factor(data_year),
              y = value,
              fill = sex
            )
          ) +
            scale_fill_manual(
              values = c(
                "Male" = "#4C78A8",
                "Female" = "#E15759"
              )
            ) +
            
            geom_col() +
            
            geom_col(
              data = subset(
                plot_df,
                highlighted
              ),
              fill = NA,
              color = "#5F6B6D",
              linewidth = 2
            ) +
            
            geom_text(
              aes(
                label = paste0(
                  round(value,1),
                  "%"
                )
              ),
              position = position_stack(vjust = .5),
              color = "white",
              size = 4
            ) +
            
            labs(
              title = paste(titles$title, "Across Years"),
              subtitle = paste("Selected Year:", selected_year()),
              x = "Year",
              y = titles$ylab
            ) +
            
            theme_minimal(base_size = 14) +
            theme(
              plot.title = element_text(
                face = "bold",
                size = 18
              )
            )
          
        } else {
          
          ggplot(
            plot_df,
            aes(
              x = factor(data_year),
              y = value,
              fill = sex
            )) +
              scale_fill_manual(
                values = c(
                  "Male" = "#4C78A8",
                  "Female" = "#E15759"
                )
              ) +
            geom_col() +
            geom_col(
              data = subset(
                plot_df,
                highlighted
              ),
              fill = NA,
              color = "#5F6B6D",
              linewidth = 2
            ) +
            
            facet_wrap(~sex) +
            
            geom_text(
              aes(
                label =
                  if(input$sex_metric == "deaths")
                    scales::comma(value)
                else
                  round(value,1)
              ),
              vjust = -0.3,
              size = 3.5
            ) +
            
            labs(
              title = paste(titles$title, "Across Years"),
              subtitle = paste("Selected Year:", selected_year()),
              x = "Year",
              y = titles$ylab
            ) +
            
            theme_minimal(base_size = 14) +
            theme(
              plot.title = element_text(
                face = "bold",
                size = 18
              )
            )
          
        }
        
      }
      
    })
    
# Race plot
    output$race_plot <- renderPlot({

  df <- selected_row()

  race_data <- tibble(
    race = c(
      "White",
      "Black",
      "Hispanic",
      "Asian",
      "Native American",
      "Pacific Islander",
      "Multi-racial"
    )
  )


# Build Metric Column Set


  race_data <- race_data |>
    mutate(
      deaths = c(
        df$white_nh_deaths,
        df$black_nh_deaths,
        df$hisp_deaths,
        df$asian_nh_deaths,
        df$aian_nh_deaths,
        df$nhpi_nh_deaths,
        df$multi_nh_deaths
      ),
      rate = c(
        df$white_nh_rate,
        df$black_nh_rate,
        df$hisp_rate,
        df$asian_nh_rate,
        df$aian_nh_rate,
        df$nhpi_nh_rate,
        df$multi_nh_rate
      )
    )
  
  clean_value <- function(x) {
    ifelse(x == 9999, NA, x)
  }

# Choose metric
  metric <- input$race_metric
  race_data$value <- race_data[[metric]]

  race_data <- race_data |>
    mutate(
      value = clean_value(value),
      missing = is.na(value)
    )

# Single Year
  
  if (input$race_view == "single") {

    ggplot(race_data, aes(x = reorder(race, value), y = value)) +

# Data Bars

    geom_col(
      data = subset(race_data, !missing),
      fill = "#2C7FB8",
      width = 0.7
    ) +
      
# Missing bars

    geom_col(
      data = subset(race_data, missing),
      aes(y = 0),
      fill = "grey90",
      color = "grey70",
      width = 0.7
    ) +
      
# Value Labels
    geom_text(
      data = subset(race_data, !missing),
      aes(label = round(value, 1)),
      vjust = -0.4,
      size = 4
    ) +
      
# Missing Labels

    geom_text(
      data = subset(race_data, missing),
      aes(y = 0, label = "Missing data"),
      color = "grey50",
      fontface = "italic",
      vjust = -0.4,
      size = 4
    ) +
      
      scale_y_continuous(
        expand = expansion(mult = c(0, 0.2))
      ) +
      
      labs(
        title = paste(
          "Overdose Deaths by Race/Ethnicity",
          ifelse(metric == "rate", "Rate", "")
        ),
        subtitle = paste("Selected Year:", selected_year()),
        x = NULL,
        y = tools::toTitleCase(metric)
      ) +
      
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(
          face = "bold",
          size = 18
        )
      )

  } else {

# Year Comparison

    race_long <- annual_data |>
      transmute(
        data_year,
        
        White_deaths = white_nh_deaths,
        Black_deaths = black_nh_deaths,
        Hispanic_deaths = hisp_deaths,
        Asian_deaths = asian_nh_deaths,
        AIAN_deaths = aian_nh_deaths,
        NHPI_deaths = nhpi_nh_deaths,
        Multi_deaths = multi_nh_deaths,
        
        White_rate = white_nh_rate,
        Black_rate = black_nh_rate,
        Hispanic_rate = hisp_rate,
        Asian_rate = asian_nh_rate,
        AIAN_rate = aian_nh_rate,
        NHPI_rate = nhpi_nh_rate,
        Multi_rate = multi_nh_rate
      ) |>
      pivot_longer(
        -data_year,
        names_to = "var",
        values_to = "value"
      ) |>
      mutate(
        race = case_when(
          str_detect(var, "White") ~ "White",
          str_detect(var, "Black") ~ "Black",
          str_detect(var, "Hispanic") ~ "Hispanic",
          str_detect(var, "Asian") ~ "Asian",
          str_detect(var, "AIAN") ~ "Native American",
          str_detect(var, "NHPI") ~ "Pacific Islander",
          str_detect(var, "Multi") ~ "Multi-racial"
        ),
        
        metric = case_when(
          str_detect(var, "_rate") ~ "rate",
          TRUE ~ "deaths"
        ),
        
        value = na_if(value, 9999),
        
        fill_bin = case_when(
          is.na(value) ~ "Data Unavailable",
          value < 50 ~ "<50",
          value < 100 ~ "50–99",
          value < 200 ~ "100–199",
          value < 350 ~ "200–349",
          TRUE ~ "350+"
        ),
        
        fill_bin = factor(
          fill_bin,
          levels = c("<50", "50–99", "100–199", "200–349", "350+", "Data Unavailable")
        ),
        
        missing = is.na(value),
        highlighted = data_year == selected_year()
      ) |>
      filter(metric == input$race_metric)
    
    race_long <- race_long |>
      mutate(
        value = na_if(value, 9999),
        
        fill_bin = case_when(
          is.na(value) ~ "Data Unavailable",
          
          metric == "deaths" & value < 50 ~ "<50",
          metric == "deaths" & value < 100 ~ "50-99",
          metric == "deaths" & value < 200 ~ "100-199",
          metric == "deaths" & value < 1000 ~ "200-999",
          metric == "deaths" ~ "1000+",
          
          metric == "rate" & value < 10 ~ "<10",
          metric == "rate" & value < 20 ~ "10-19",
          metric == "rate" & value < 30 ~ "20-29",
          metric == "rate" & value < 50 ~ "30-49",
          metric == "rate" ~ "50+"
        ),
        
        fill_bin = factor(
          fill_bin,
          levels = c(
            "<50","50-99","100-199","200-999","1000+",
            "<10","10-19","20-29","30-49","50+",
            "Data Unavailable"
          )
        ),
        
        highlighted = data_year == selected_year()
      )

    ggplot(
      race_long,
      aes(
        x = factor(data_year),
        y = race,
        fill = fill_bin
      )
    ) +
      
      geom_tile(
        color = "white",
        linewidth = 0.6
      ) +
      
      geom_tile(
        data = subset(race_long, highlighted),
        fill = NA,
        color = "#6C757D",
        linewidth = 2
      ) +
      
      geom_text(
        data = subset(race_long, !is.na(value)),
        aes(label = round(value, 1)),
        size = 3.5
      ) +
      
      scale_fill_manual(
        values = c(
          "<50" = "#85C1E9",
          "50-99" = "#D6EAF8",
          "100-199" = "#F7F7F7",
          "200-999" = "#F5B7B1",
          "1000+" = "#C0392B",
          
          "<10" = "#85C1E9",
          "10-19" = "#D6EAF8",
          "20-29" = "#F7F7F7",
          "30-49" = "#F5B7B1",
          "50+" = "#C0392B",
          
          "Data Unavailable" = "grey90"
        ),
        name = tools::toTitleCase(input$race_metric)
      ) +
      
      labs(
        title = paste(
          "Race/Ethnicity Comparison Across Years",
          "-",
          tools::toTitleCase(input$race_metric)
        ),
        subtitle = paste("Selected Year:", selected_year()),
        x = "Year",
        y = NULL,
        fill = tools::toTitleCase(input$race_metric)
      ) +
      
      theme_minimal(base_size = 13) +
      theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(hjust = 1),
        legend.position = "right",
        plot.title = element_text(
          face = "bold",
          size = 18
        )
      )
  }

})

# Age × Sex Heat Map
    
output$age_sex_heatmap <- renderPlot({
      
      df <- selected_row()
      
      if (input$age_sex_metric == "deaths") {
        
        heatmap_df <- tibble(
          sex = rep(c("Male", "Female"), each = 7),
          
          age_group = rep(
            c(
              "<15",
              "15-24",
              "25-34",
              "35-44",
              "45-54",
              "55-64",
              "65+"
            ),
            times = 2
          ),
          
          value = c(
            df$male_under15_deaths,
            df$male_15_24_deaths,
            df$male_25_34_deaths,
            df$male_35_44_deaths,
            df$male_45_54_deaths,
            df$male_55_64_deaths,
            df$male_65plus_deaths,
            
            df$female_under15_deaths,
            df$female_15_24_deaths,
            df$female_25_34_deaths,
            df$female_35_44_deaths,
            df$female_45_54_deaths,
            df$female_55_64_deaths,
            df$female_65plus_deaths
          )
        )
        
      } else {
        
        heatmap_df <- tibble(
          sex = rep(c("Male", "Female"), each = 7),
          
          age_group = rep(
            c(
              "<15",
              "15-24",
              "25-34",
              "35-44",
              "45-54",
              "55-64",
              "65+"
            ),
            times = 2
          ),
          
          value = c(
            df$male_under15_rate,
            df$male_15_24_rate,
            df$male_25_34_rate,
            df$male_35_44_rate,
            df$male_45_54_rate,
            df$male_55_64_rate,
            df$male_65plus_rate,
            
            df$female_under15_rate,
            df$female_15_24_rate,
            df$female_25_34_rate,
            df$female_35_44_rate,
            df$female_45_54_rate,
            df$female_55_64_rate,
            df$female_65plus_rate
          )
        )
        
      }
      
      heatmap_df <- heatmap_df %>%
        mutate(
          value = na_if(value, 9999),
          
          fill_bin = case_when(
            
            is.na(value) ~ "Data Unavailable",
            
            input$age_sex_metric == "deaths" & value < 25  ~ "<25",
            input$age_sex_metric == "deaths" & value < 50  ~ "25-49",
            input$age_sex_metric == "deaths" & value < 100 ~ "50-99",
            input$age_sex_metric == "deaths" & value < 150 ~ "100-149",
            input$age_sex_metric == "deaths" ~ "150+",
            
            input$age_sex_metric == "rate" & value < 10 ~ "<10",
            input$age_sex_metric == "rate" & value < 20 ~ "10-19",
            input$age_sex_metric == "rate" & value < 30 ~ "20-29",
            input$age_sex_metric == "rate" & value < 50 ~ "30-49",
            input$age_sex_metric == "rate" ~ "50+"
          )
        )
      
      heatmap_df <- heatmap_df %>%
        mutate(
          fill_bin = factor(
            fill_bin,
            levels = c(
              "<25","25-49","50-99","100-149","150+",
              "<10","10-19","20-29","30-49","50+",
              "Data Unavailable"
            )
          )
        )
      
      ggplot(
        heatmap_df,
        aes(
          x = factor(
            age_group,
            levels = c(
              "<15",
              "15-24",
              "25-34",
              "35-44",
              "45-54",
              "55-64",
              "65+"
            )
          ),
          y = sex,
          fill = fill_bin
        )
      ) +
        
        geom_tile(
          color = "white",
          linewidth = 0.8
        ) +
        
        geom_text(
          aes(
            label = ifelse(
              is.na(value),
              "N/A",
              round(value, 1)
            )
          ),
          size = 4
        ) +
        
        scale_fill_manual(
          values = c(
            
            # Death bins
            "<25" = "#85C1E9",
            "25-49" = "#D6EAF8",
            "50-99" = "#F7F7F7",
            "100-149" = "#F5B7B1",
            "150+" = "#C0392B",
            
            # Rate bins
            "<10" = "#85C1E9",
            "10-19" = "#D6EAF8",
            "20-29" = "#F7F7F7",
            "30-49" = "#F5B7B1",
            "50+" = "#C0392B",
            
            "Data Unavailable" = "grey90"
          ),
          name = tools::toTitleCase(input$age_sex_metric)
        ) +
        
        labs(
          title = paste(
            "Age × Sex Heatmap -",
            tools::toTitleCase(input$age_sex_metric)
          ),
          subtitle = paste(
            "Selected Year:",
            selected_year()
          ),
          x = "Age Group",
          y = NULL,
          fill = tools::toTitleCase(input$age_sex_metric)
        ) +
        
        theme_minimal(base_size = 14) +
        
        theme(
          panel.grid = element_blank(),
          axis.text.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"),
          plot.title = element_text(
            face = "bold",
            size = 18
          )
        )
      
    })
  
})
  
}