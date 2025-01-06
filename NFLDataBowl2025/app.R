library(shiny)
library(DT)
library(shinythemes)
library(gt)
library(nflplotR)
library(shinyWidgets)
library(shinybusy)

main_df = read.csv('tell_df.csv')
names(main_df) = c("Team","Variable","Value","Total Plays", "Pass Rate", "Difference From Baseline")

# Define UI for application
ui <- fluidPage(
  
  shinybusy::add_busy_spinner(spin = 'half-circle', color = 'white', position = 'bottom-right'),
  
  theme = shinytheme("superhero"),
  
  div(h1("NFL Big Data Bowl 2025"), align = 'center'),
  div(h4("Analysis by Nick Amato"), align = 'center'),
  hr(),

  sidebarLayout(
    sidebarPanel(
      h4("Introduction:"),
      paste0("This app simply provides an overview of the entire 'Team-Specific Keys' data from the presentation.",
             " Use the dropdowns to filter for specific teams/data that you're interested in!",
             " Keep in mind, these values are based on plays with motion into the boundary."),
      hr(),
      
      virtualSelectInput(inputId = "selectTeam", label = "Filter Team(s)",
                         choices = unique(main_df$Team), multiple = TRUE),
      virtualSelectInput(inputId = "selectKey", label = "Filter Key(s)",
                         choices = unique(main_df$Variable), multiple = TRUE),
      virtualSelectInput(inputId = "selectValue", label = "Filter Value(s)",
                         choices = unique(main_df$Value), multiple = TRUE),
      div(actionBttn("submitButton", label = "Apply Filters"), align = 'center')
      
      
    ),
    mainPanel(
      gt_output('tellGT')
    )
  )

)

# Define server logic
server <- function(input, output) {

  output$tellGT = render_gt({
    pal <- function(x) {
      f_neg <- scales::col_numeric(
        palette = c('red', 'white'),
        domain = c(min(main_df$`Difference From Baseline`), 0)
      )
      f_pos <- scales::col_numeric(
        palette = c('white', 'lightgreen'),
        domain = c(0, max(main_df$`Difference From Baseline`))
      )
      ifelse(x < 0, f_neg(x), f_pos(x))
    }
    
    # main_df = main_df %>%
    #   filter(Team %in% c('MIN'))
    
    publication_table <- main_df %>%
      gt() %>%
      tab_header(
        title = "Team Specific Keys",
        subtitle = "2022 Through 9 Weeks"
      ) %>%
      data_color(columns = c("Difference From Baseline"), colors = pal) %>%
      gt_nfl_logos(columns = "Team") %>%
      fmt_number(
        columns = c("Pass Rate", "Difference From Baseline"),
        decimals = 1,
        pattern = "{x}%"
      )
    
    publication_table
  })
  
  observeEvent(input$submitButton, {

    df = main_df
    assign('df_testing',df,.GlobalEnv)

    pal <- function(x) {
      f_neg <- scales::col_numeric(
        palette = c('red', 'white'),
        domain = c(min(df$`Difference From Baseline`), 0)
      )
      f_pos <- scales::col_numeric(
        palette = c('white', 'lightgreen'),
        domain = c(0, max(df$`Difference From Baseline`))
      )
      ifelse(x < 0, f_neg(x), f_pos(x))
    }



    if(!is.null(input$selectTeam)){
      df = df %>%
        filter(Team %in% c(input$selectTeam))
    }

    if(!is.null(input$selectKey)){
      df = df %>%
        filter(Variable %in% c(input$selectKey))
    }
    if(!is.null(input$selectValue)){
      df = df %>%
        filter(Value %in% c(input$selectValue))
    }
    
    publication_table <- df %>%
      gt() %>%
      tab_header(
        title = "Team Specific Keys",
        subtitle = "2022 Through 9 Weeks"
      ) %>%
      data_color(columns = c("Difference From Baseline"), colors = pal) %>%
      gt_nfl_logos(columns = "Team") %>%
      fmt_number(
        columns = c("Pass Rate", "Difference From Baseline"),
        decimals = 1,
        pattern = "{x}%"
      )

    output$tellGT = render_gt(publication_table)
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
