library(shiny)
library(dplyr)
library(DT)
library(shinythemes)
library(gt)
library(nflplotR)
library(shinyWidgets)
library(shinybusy)

main_df = read.csv('tell_df.csv')
boun_pass = read.csv('pub_df.csv')

names(main_df) = c("Team","Variable","Value","Total Plays", "Pass Rate", "Difference From Baseline")
names(boun_pass) = c("Team","Total Plays", "Pass Rate", "Difference From Baseline")

# Define UI for application
ui <- fluidPage(
  theme = shinytheme("darkly"),
  
  navbarPage(title = 'NFL Big Data Bowl 2025',
             tabPanel('Team-Specific Keys',
                      shinybusy::add_busy_spinner(spin = 'half-circle', color = 'white', position = 'bottom-right'),
                      
                      
                      # div(h1("NFL Big Data Bowl 2025"), align = 'center'),
                      div(h4("Analysis by Nick Amato"), align = 'center'),
                      hr(),
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          h4("Introduction:"),
                          paste0("This tab simply provides an overview of the entire 'Team-Specific Keys' data from the presentation.",
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
             ),
             tabPanel("Boundary Motion Pass %",
                      shinybusy::add_busy_spinner(spin = 'half-circle', color = 'white', position = 'bottom-right'),
                      
                      div(h4("Analysis by Nick Amato"), align = 'center'),
                      hr(),
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          h4("Introduction:"),
                          paste0("This tab provides an overview of each NFL teams pass rate with boundary motion.",
                                 " Use the dropdown to filter for specific teams that you're interested in!"),
                          hr(),
                          
                          virtualSelectInput(inputId = "selectTeam2", label = "Filter Team(s)",
                                             choices = unique(main_df$Team), multiple = TRUE),
                          div(actionBttn("submitButton2", label = "Apply Filters"), align = 'center')
                          
                          
                        ),
                        mainPanel(
                          gt_output('tellGT2')
                          
                          
                        )
                      )
             )
  ),
  
  
  
)

# Define server logic
server <- function(input, output) {
  
  rv = reactiveValues(df = main_df,
                      pub_df = boun_pass)
  print(boun_pass)
  
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
  
  output$tellGT2 = render_gt({
    pal <- function(x) {
      f_neg <- scales::col_numeric(
        palette = c('red', 'white'),
        domain = c(min(boun_pass$`Difference From Baseline`), 0)
      )
      f_pos <- scales::col_numeric(
        palette = c('white', 'lightgreen'),
        domain = c(0, max(boun_pass$`Difference From Baseline`))
      )
      ifelse(x < 0, f_neg(x), f_pos(x))
    }
    
    publication_table <- boun_pass %>%
      gt() %>%
      gt_nfl_logos(columns = "Team") %>%
      tab_header(
        title = "Boundary Motion Pass Percentage",
        subtitle = "2022 Through 9 Weeks"
      ) %>%
      data_color(columns = c("Difference From Baseline"), colors = pal) %>%
      fmt_number(
        columns = c("Pass Rate", "Difference From Baseline"),
        decimals = 1,
        pattern = "{x}%"
      )
    
    publication_table
  })
  
  output$ggplot2 = renderPlot({
    ggplot(df_gg, aes(x = scaled_baseline, y = scaled_pass)) +
      xlim(c(-0.75,0.75)) +
      ylim(c(-0.75,0.75)) +
      geom_nfl_logos(aes(team_abbr = Team), width = 0.065) +
      geom_hline(yintercept = 0, color = "black", linetype = "dashed") +  # Horizontal axis line
      geom_vline(xintercept = 0, color = "black", linetype = "dashed") +  # Vertical axis line
      theme_minimal() +  # Use a clean theme
      theme(panel.grid.major = element_line(color = "grey", linetype = "dotted"),
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            axis.text = element_text(size = 20),
            plot.title = element_text(size = 20),
            plot.subtitle = element_text(size = 15)) +  # Customize grid lines
      labs(
        title = "Pass Threat Rating",
        subtitle = "By Type of Motion",
        x = "No Boundary Motion",
        y = "With Boundary Motion"
      )
  })
  
  observeEvent(input$submitButton, {
    
    rv$df = main_df
    
    pal <- function(x) {
      f_neg <- scales::col_numeric(
        palette = c('red', 'white'),
        domain = c(min(rv$df$`Difference From Baseline`), 0)
      )
      f_pos <- scales::col_numeric(
        palette = c('white', 'lightgreen'),
        domain = c(0, max(rv$df$`Difference From Baseline`))
      )
      ifelse(x < 0, f_neg(x), f_pos(x))
    }
    
    
    
    if(!is.null(input$selectTeam)){
      print(names(rv$df))
      rv$df = rv$df %>%
        filter(Team %in% c(input$selectTeam))
    }
    
    if(!is.null(input$selectKey)){
      rv$df = rv$df %>%
        filter(Variable %in% c(input$selectKey))
    }
    if(!is.null(input$selectValue)){
      rv$df = rv$df %>%
        filter(Value %in% c(input$selectValue))
    }
    
    publication_table <- rv$df %>%
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
  
  observeEvent(input$submitButton2, {
    
    
    rv$pub_df = boun_pass
    
    pal <- function(x) {
      f_neg <- scales::col_numeric(
        palette = c('red', 'white'),
        domain = c(min(rv$pub_df$`Difference From Baseline`), 0)
      )
      f_pos <- scales::col_numeric(
        palette = c('white', 'lightgreen'),
        domain = c(0, max(rv$pub_df$`Difference From Baseline`))
      )
      ifelse(x < 0, f_neg(x), f_pos(x))
    }
    
    if(!is.null(input$selectTeam2)){
      rv$pub_df = rv$pub_df %>%
        filter(Team %in% c(input$selectTeam2))
    }
    
    publication_table <- rv$pub_df %>%
      gt() %>%
      gt_nfl_logos(columns = "Team") %>%
      tab_header(
        title = "Boundary Motion Pass Percentage",
        subtitle = "2022 Through 9 Weeks"
      ) %>%
      data_color(columns = c("Difference From Baseline"), colors = pal) %>%
      fmt_number(
        columns = c("Pass Rate", "Difference From Baseline"),
        decimals = 1,
        pattern = "{x}%"
      )
    
    output$tellGT2 = render_gt(publication_table)
    
    
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
