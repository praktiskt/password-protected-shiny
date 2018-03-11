library(shiny)
library(shinyalert)
library(shinymaterial)
library(rhandsontable)
library(tidyverse)
source("./credentials.R") # only exists locally, sets environment variable "secret_password"

df = data.frame(
  stringsAsFactors = F, 
  int = sample(x = 1:15, size = 15, replace = F), 
  let = sample(x = letters, size = 15, replace = F),
  LET = sample(x = LETTERS, size = 15, replace = F), 
  date = seq.Date(
    from = Sys.Date()-14, 
    to = Sys.Date(), 
    by = "day"
  )
) %>% 
  mutate(date = date %>% as.character())

ui <- material_page(
  # Setup
  useShinyalert(),
  
  # Main UI
  title = "Shiny Dummy",
  tags$br(),
  tags$style(c("nav { 
                  color: #fff; 
                  background-color: #26a69a; 
                  width: 100%; 
                  height: 56px; 
                  line-height: 56px; 
                }")),
  material_row(
    material_column(
      width = 6, 
      material_card(
        title = "", 
        depth = 4,
        rHandsontableOutput(
          outputId = "table", 
          height = "400px"
        )
      )
    ), 
    material_column(
      width = 2, 
      material_card(
        title = "", 
        depth = 4,
        uiOutput(outputId = "controls")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # -- Ensure user is authenticated ----
  
  # Password prompt
  shinyalert(
    title = "Log in",
    text = "Password",
    type = "input",
    closeOnEsc = F,
    closeOnClickOutside = F,
    showCancelButton = F,
    showConfirmButton = T,
    inputType = "password",
    inputPlaceholder = "password",
    inputValue = "Password",
    confirmButtonCol = "#26a69a"
  )
  
  # Wait until user has entered password.
  observeEvent(input$shinyalert, {
    
    if (input$shinyalert == Sys.getenv("secret_password")) {
      # If user successfully authenticated.
    
      output$table = renderRHandsontable(
        rhandsontable(
          data = df,
          rowHeaders = F
        ) %>% 
          #fix column format
          hot_col(
            col = "date", 
            dateFormat = "YYYY-MM-DD", 
            type = "date"
          )
        
      )
      
      output$controls = renderUI({
        actionButton(
          inputId = "update",
          label = "Update"
        )
      })
      
    } else {
      # Bad password.
      shinyalert(
        title = "Unauthorized", 
        text = "Bad password. Try again?", 
        type = "error",
        showConfirmButton = T,
        showCancelButton = T, 
        closeOnClickOutside = F, 
        closeOnEsc = F,
        confirmButtonCol = "#26a69a", 
        callbackJS = "function(x) { if(x !== false) history.go(0); }" #refresh page on OK
      )
    }
  })
  
  observeEvent(input$update, {
    material_spinner_show(session = session, 
                          output_id = "table")
    
    Sys.sleep(1) #Simulate loading time
    # INSERT NEW ROWS
    
    #DUMMY
    df = hot_to_r(input$table) #parse rhandsontable
    material_spinner_hide(session = session, 
                          output_id = "table")
  })
  
}

shinyApp(ui = ui, server = server)
