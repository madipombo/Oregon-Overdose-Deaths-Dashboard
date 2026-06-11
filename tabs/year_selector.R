year_selector_ui <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("year_buttons"))
  
}

year_selector_server <- function(id, selected_year) {
  
  moduleServer(id, function(input, output, session) {
    
    output$year_buttons <- renderUI({
      
      div(
        style = "
          display:flex;
          justify-content:center;
          gap:15px;
          padding:20px 10px;
          flex-wrap:wrap;
        ",
        
        lapply(2020:2024, function(y){
          
          btn_class <- if(y == selected_year()) {
            "btn btn-primary btn-lg"
          } else {
            "btn btn-outline-primary btn-lg"
          }
          
          actionButton(
            session$ns(paste0('year_', y)),
            label = y,
            class = btn_class,
            style = "min-width:200px;"
          )
          
        })
        
      )
      
    })
    
    lapply(2020:2024, function(y){
      
      observeEvent(
        input[[paste0("year_", y)]],
        {
          selected_year(y)
        },
        ignoreInit = TRUE
      )
      
    })
    
  })
  
}