library(shiny)
library(dplyr)
library(DT)
library(shinythemes)
library(gt)
library(nflplotR)
library(shinyWidgets)
library(shinybusy)
library(ggplot2)
library(aws.s3)
library(gganimate)
library(gifski)

Sys.setenv(
  "AWS_ACCESS_KEY_ID" = "##",
  "AWS_SECRET_ACCESS_KEY" = "##",
  "AWS_DEFAULT_REGION" = "##"
)


main_df = read.csv('tell_df.csv')
boun_pass = read.csv('pub_df.csv')
team_specific_keys_plays = read.csv('team_specific_keys_plays.csv')

names(main_df) = c("Team","Variable","Value","Total Plays", "Pass Rate", "Difference From Baseline")
names(boun_pass) = c("Team","Total Plays", "Pass Rate", "Difference From Baseline")

team_imgs = main_df %>%
  left_join(nflfastR::teams_colors_logos, join_by("Team" == "team_abbr")) %>%
  select(Team, team_logo_wikipedia)

main_df$teamAbbr = main_df$Team
main_df$`Difference From Baseline` = paste0(main_df$`Difference From Baseline`,"%")
main_df$Team = paste0('<img src="',team_imgs$team_logo_wikipedia,'" height="30" />')

# Define UI for application
ui <- fluidPage(
  theme = shinytheme("darkly"),
  
  navbarPage(title = 'NFL Big Data Bowl 2025', id = 'navbar',
             tabPanel('Team-Specific Keys',
                      #shinybusy::add_busy_spinner(spin = 'half-circle', color = 'white', position = 'bottom-right'),
                      
                      
                      # div(h1("NFL Big Data Bowl 2025"), align = 'center'),
                      div(h4("Analysis by Nick Amato"), align = 'center'),
                      hr(),
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          h4("Introduction:"),
                          paste0("This tab simply provides an overview of the entire 'Team-Specific Keys' data from the presentation.",
                                 " Use the dropdowns to filter for specific teams/data that you're interested in!",
                                 " Keep in mind, these values are based on plays with motion into the boundary."),
                          br(),
                          br(),
                          paste0("Selecting a row in the dataframe will load all of the plays for that team-specific key."),
                          hr(),
                          
                          virtualSelectInput(inputId = "selectTeam", label = "Filter Team(s)",
                                             choices = unique(main_df$teamAbbr), multiple = TRUE),
                          virtualSelectInput(inputId = "selectKey", label = "Filter Key(s)",
                                             choices = unique(main_df$Variable), multiple = TRUE),
                          virtualSelectInput(inputId = "selectValue", label = "Filter Value(s)",
                                             choices = unique(main_df$Value), multiple = TRUE),
                          div(actionBttn("submitButton", label = "Apply Filters"), align = 'center')
                          
                          
                        ),
                        mainPanel(
                          # gt_output('tellGT'),
                          dataTableOutput('tellDT')
                          
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
                                             choices = unique(main_df$teamAbbr), multiple = TRUE),
                          div(actionBttn("submitButton2", label = "Apply Filters"), align = 'center')
                          
                          
                        ),
                        mainPanel(
                          gt_output('tellGT2')
                          
                          
                        )
                      )
             ),
             tabPanel("Play Viewer",
                      
                      div(uiOutput('gif_ui'), align = 'center'),
                      hr(),
                      div(paste0("Select a play in the table to view how the offensive/defensive players moved during that play"), align = 'center'),
                      div(dataTableOutput('gif_plays'), align = 'center')

             )
  ),
  
  
  
)

# Define server logic
server <- function(input, output, session) {
  
  rv = reactiveValues(df = main_df,
                      pub_df = boun_pass,
                      play_df = NULL,
                      team_plays = NULL,
                      team = NULL,
                      key = NULL,
                      value = NULL)
  
  output$tellDT = renderDataTable({
    
    # Render the DataTable
    datatable(main_df[, 1:6], style = 'bootstrap',
              selection = 'single',
              escape = FALSE,
              rownames = FALSE)
  })
  
  
  # output$tellGT = render_gt({
  #   pal <- function(x) {
  #     f_neg <- scales::col_numeric(
  #       palette = c('red', 'white'),
  #       domain = c(min(main_df$`Difference From Baseline`), 0)
  #     )
  #     f_pos <- scales::col_numeric(
  #       palette = c('white', 'lightgreen'),
  #       domain = c(0, max(main_df$`Difference From Baseline`))
  #     )
  #     ifelse(x < 0, f_neg(x), f_pos(x))
  #   }
  #   
  #   # main_df = main_df %>%
  #   #   filter(Team %in% c('MIN'))
  #   
  #   publication_table <- main_df %>%
  #     gt() %>%
  #     tab_header(
  #       title = "Team Specific Keys",
  #       subtitle = "2022 Through 9 Weeks"
  #     ) %>%
  #     data_color(columns = c("Difference From Baseline"), colors = pal) %>%
  #     gt_nfl_logos(columns = "Team") %>%
  #     fmt_number(
  #       columns = c("Pass Rate", "Difference From Baseline"),
  #       decimals = 1,
  #       pattern = "{x}%"
  #     )
  #   
  #   publication_table
  # })
  
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
  
  observeEvent(input$tellDT_rows_selected, {
    shinybusy::show_modal_spinner(spin = 'semipolar',color = 'white', text = "Loading Visualizations...")
    
    ind = input$tellDT_rows_selected
    
    rv$team = rv$df$teamAbbr[ind]
    rv$key = rv$df$Variable[ind]
    rv$value = rv$df$Value[ind]
    
    
    updateNavbarPage(session, 'navbar', selected = 'Play Viewer')
    
    if(rv$key == 'Receiver Alignment at Snap'){
      team_plays = team_specific_keys_plays[which(team_specific_keys_plays$teamAbbr == rv$team &
                                                    team_specific_keys_plays$receiverAlignment == rv$value),]
    }
    
    if(rv$key == 'In Motion at Snap'){
      team_plays = team_specific_keys_plays[which(team_specific_keys_plays$teamAbbr == rv$team &
                                                    team_specific_keys_plays$inMotionAtBallSnap == rv$value),]
    }
    
    if(rv$key == 'Position in Motion'){
      team_plays = team_specific_keys_plays[which(team_specific_keys_plays$teamAbbr == rv$team &
                                                    team_specific_keys_plays$position == rv$value),]
    }
    
    if(rv$key == 'Offensive Formation'){
      team_plays = team_specific_keys_plays[which(team_specific_keys_plays$teamAbbr == rv$team &
                                                    team_specific_keys_plays$offenseFormation == rv$value),]
    }
    
    rv$team_plays = team_plays
    rv$play_df = aws.s3::s3read_using(read.csv, bucket = 'nflbigdatabowl', object = paste0(rv$team,'_df.csv'))
    # rv$play_df = read.csv(paste0(rv$team,"_df.csv"))
    
    updateVirtualSelect(inputId = "selectPlay", label = "Select a Play", choices = 1:length(unique(team_plays$playId)))
    
    examine = rv$play_df %>%
      filter(gameId == rv$team_plays$gameId[1]) %>%
      filter(playId == rv$team_plays$playId[1])
    
    # Plot the field and player movements
    field_plot <- ggplot(examine, aes(x = x, y = y, group = nflId, color = factor(club))) +
      geom_rect(aes(xmin = 0, xmax = 120, ymin = 0, ymax = 50), fill = "black", color = "white") +
      geom_point(size = 4) +
      geom_hline(yintercept = 23, linetype = "dashed", color = "white", size = 1) +   # Left hash mark
      geom_hline(yintercept = 30, linetype = "dashed", color = "white", size = 1) +   # Right hash mark
      transition_time(frameId) +
      labs(title = paste0(rv$team," (",rv$key, ": ",rv$value,")"," PLAY 1"), x = "X Position", y = "Y Position") +
      theme_minimal() +
      theme(legend.position = "none")
    
    
    # Render the GIF in memory and encode it to base64
    temp_gif <- tempfile(fileext = ".gif")
    animate(field_plot, fps = 7, duration = 15, renderer = gifski_renderer(temp_gif))
    
    # Convert the GIF to base64 encoding
    gif_base64 <- base64enc::dataURI(file = temp_gif)
    
    # Update the UI with the base64-encoded GIF
    output$gif_ui = renderUI({
      img(src = gif_base64, height = "600px", width = "800px")
    })
    
    to_display = rv$team_plays %>%
      select(teamAbbr, position, offenseFormation, receiverAlignment)
    
    names(to_display) = c("Team", "Position in Motion", "Offensive Formation", "Receiver Alignment")
    
    to_display = to_display %>%
      left_join(nflfastR::teams_colors_logos, join_by("Team" == "team_abbr")) %>%
      select("Team", "Position in Motion", "Offensive Formation", "Receiver Alignment", "team_logo_wikipedia")
    
    to_display$Team = paste0('<img src="',to_display$team_logo_wikipedia,'" height="30" />')
    
    output$gif_plays = renderDataTable({
      datatable(to_display[,1:4], style = 'bootstrap',
                escape = FALSE, selection = 'single')
    })
    
    shinybusy::remove_modal_spinner()
    
  })
  
  observeEvent(input$gif_plays_rows_selected, {
    shinybusy::show_modal_spinner(spin = 'semipolar',color = 'white', text = "Loading Play...")
    
    play = as.numeric(input$gif_plays_rows_selected)
    examine = rv$play_df %>%
      filter(gameId == rv$team_plays$gameId[play]) %>%
      filter(playId == rv$team_plays$playId[play])
    
    # Plot the field and player movements
    field_plot <- ggplot(examine, aes(x = x, y = y, group = nflId, color = factor(club))) +
      geom_rect(aes(xmin = 0, xmax = 120, ymin = 0, ymax = 50), fill = "black", color = "white") +
      geom_hline(yintercept = 23, linetype = "dashed", color = "white", size = 1) +   # Left hash mark
      geom_hline(yintercept = 30, linetype = "dashed", color = "white", size = 1) +   # Right hash mark
      geom_point(size = 4) +
      transition_time(frameId) +
      labs(title = paste0(rv$team," (",rv$key, ": ",rv$value,")"," PLAY ",play), x = "X Position", y = "Y Position") +
      theme_minimal() +
      theme(legend.position = "none")
    
    # Render the GIF in memory and encode it to base64
    temp_gif <- tempfile(fileext = ".gif")
    animate(field_plot, fps = 7, duration = 15, renderer = gifski_renderer(temp_gif))
    
    # Convert the GIF to base64 encoding
    gif_base64 <- base64enc::dataURI(file = temp_gif)
    
    # Update the UI with the base64-encoded GIF
    output$gif_ui = renderUI({
      img(src = gif_base64, height = "600px", width = "800px")
    })
    
    shinybusy::remove_modal_spinner()
    
  })
  
  observeEvent(input$submitButton, {
    
    rv$df = main_df
    
    
    
    if(!is.null(input$selectTeam)){
      print(names(rv$df))
      rv$df = rv$df %>%
        filter(teamAbbr %in% c(input$selectTeam))
    }
    
    if(!is.null(input$selectKey)){
      rv$df = rv$df %>%
        filter(Variable %in% c(input$selectKey))
    }
    if(!is.null(input$selectValue)){
      rv$df = rv$df %>%
        filter(Value %in% c(input$selectValue))
    }
    
    
    output$tellDT = renderDataTable({
      datatable(rv$df[,1:6], style = 'bootstrap',
                selection = 'single',
                escape = FALSE,
                rownames = FALSE)
    })
    
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
