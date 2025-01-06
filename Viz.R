library(gt)
library(nflplotR)
library(ggimage)

non_motion_b_df = plays_players_unique %>%
  anti_join(boundary_cross, by = c('playId', 'gameId'))

non_motion_b_df = data.frame(table(non_motion_b_df$possessionTeam, non_motion_b_df$type)) %>%
  pivot_wider(
    names_from = Var2,
    values_from = Freq,
    values_fill = 0
  ) %>%
  rename(Team = Var1,
         Runs = RUN,
         Passes = PASS) %>%
  mutate("Pass Percentage" = round(Passes / (Passes + Runs) * 100, digits = 1)) %>%
  select(Team, `Pass Percentage`)


pub_df = teams_b_mo %>%
  arrange(desc(TotalPlays)) %>%
  left_join(non_motion_b_df, by = "Team")


# pub_df$total_pass_perc = NA
# for(i in 1:nrow(pub_df)){
#   tmp_df = data.frame(table(plays_players_unique$type[plays_players_unique$possessionTeam == pub_df$Team[i]]))
#   
#   total_pass_perc = paste0(round(tmp_df$Freq[tmp_df$Var1 == 'PASS'] / (sum(tmp_df$Freq)) * 100, digits = 1), "%")
#   
#   pub_df$total_pass_perc[i] = total_pass_perc
# }

pub_df = pub_df[,c(1,5,2,3,4,6)]

colnames(pub_df) = c("Team","Boundary Motion Plays","Passes","Runs","Pass %","Baseline Pass Percentage")

#pub_df$`Pass %` = paste0(round(pub_df$`Pass %`, digits = 1), "%")

#pub_df = pub_df[1:10,]

pub_df = pub_df %>%
  mutate("Difference From Baseline" = `Pass %` - `Baseline Pass Percentage`) %>%
  select(-`Baseline Pass Percentage`, -Passes, -Runs)

pal <- function(x) {
  f_neg <- scales::col_numeric(
    palette = c('red', 'white'),
    domain = c(min(pub_df$`Difference From Baseline`), 0)
  )
  f_pos <- scales::col_numeric(
    palette = c('white', 'lightgreen'),
    domain = c(0, max(pub_df$`Difference From Baseline`))
  )
  ifelse(x < 0, f_neg(x), f_pos(x))
}

# Create a publication-ready table
publication_table <- pub_df %>%
  gt() %>%
  gt_nfl_logos(columns = "Team") %>%
  tab_header(
    title = "Boundary Motion Pass Percentage",
    subtitle = "2022 Through 9 Weeks"
  ) %>%
  data_color(columns = c("Difference From Baseline"), colors = pal) %>%
  # data_color(
  #   columns = c('Difference From Baseline'),
  #   fn = scales::col_numeric(
  #     palette = c('darkred','white','darkgreen'),
  #     domain = c(min(pub_df$`Difference From Baseline`, na.rm = TRUE),
  #                0,
  #                max(pub_df$`Difference From Baseline`, na.rm = TRUE)),
  #     na.color = 'grey'
  #   )
  # ) %>%
  fmt_number(
    columns = c("Pass %", "Difference From Baseline"),
    decimals = 1,
    pattern = "{x}%"
  )
# tab_style(
#   style = list(
#     cell_fill(color = 'lightgreen')
#   ),
#   locations = cells_body(columns=c('Total Team Pass %'))
# )

publication_table




#########


team_var = position_df %>%
  select(var1, RUN, PASS, PASS_RATE, TEAM) %>%
  mutate(PLAYS = RUN + PASS) %>%
  left_join(non_motion_b_df, join_by(TEAM == Team)) %>%
  mutate("Difference From Baseline" = PASS_RATE * 100 - `Pass Percentage`) %>%
  select(-RUN, -PASS, - `Pass Percentage`) %>%
  filter(var1 == 'TE')

team_var = team_var[,c(3,1,4,2,5)]

team_var$PASS_RATE = team_var$PASS_RATE * 100

colnames(team_var) = c("Team","Receiver Alignment", "Plays", "Pass %", "Difference From Baseline")


pal <- function(x) {
  f_neg <- scales::col_numeric(
    palette = c('red', 'white'),
    domain = c(min(team_var$`Difference From Baseline`), 0)
  )
  f_pos <- scales::col_numeric(
    palette = c('white', 'lightgreen'),
    domain = c(0, max(team_var$`Difference From Baseline`))
  )
  ifelse(x < 0, f_neg(x), f_pos(x))
}

publication_table <- head(team_var,9) %>%
  gt() %>%
  tab_header(
    title = "TE Boundary Motion Pass Rate",
    subtitle = "2022 Through 9 Weeks"
  ) %>%
  data_color(columns = c("Difference From Baseline"), colors = pal) %>%
  gt_nfl_logos(columns = "Team") %>%
  fmt_number(
    columns = c("Pass %", "Difference From Baseline"),
    decimals = 1,
    pattern = "{x}%"
  )

publication_table

gtsave(publication_table, filename = "exports/position.png")

##########

team_var = offenseFormation_df %>%
  select(var1, RUN, PASS, PASS_RATE)

team_var$PASS_RATE = paste0(round(team_var$PASS_RATE * 100, digits = 2), "%")

colnames(team_var) = c("Position", "Runs", "Passes", "Pass %")

publication_table <- team_var %>%
  gt() %>%
  tab_header(
    title = "Offensive Formation",
    subtitle = "By Position in Motion"
  )
publication_table

gtsave(publication_table, filename = "exports/formation.png")

##########

team_var = inMotionAtBallSnap_df %>%
  filter(TEAM == 'CIN') %>%
  select(var1, RUN, PASS, PASS_RATE)

team_var$PASS_RATE = paste0(round(team_var$PASS_RATE * 100, digits = 2), "%")

colnames(team_var) = c("Position", "Runs", "Passes", "Pass %")

publication_table <- team_var %>%
  gt() %>%
  tab_header(
    title = "In Motion at Snap",
    subtitle = "By Position in Motion"
  )
publication_table

gtsave(publication_table, filename = "exports/motion.png")

library(ggplot2)
library(patchwork)

img1 <- png::readPNG("exports/formation.png")
img2 <- png::readPNG("exports/position.png")
img3 <- png::readPNG("exports/formation.png")

ggplot() +
  annotation_custom(grid::rasterGrob(img1), xmin = 0, xmax = 1, ymin = 0.5, ymax = 1) +
  annotation_custom(grid::rasterGrob(img2), xmin = 0, xmax = 1, ymin = 0, ymax = 0.5) +
  annotation_custom(grid::rasterGrob(img3), xmin = 0, xmax = 1, ymin = 0, ymax = 0.5)

###########

pub_tell_df = tell_df %>%
  select(TEAM, var1, TOTAL_PLAYS, PASS_RATE, `Pass Percentage`) %>%
  mutate('Difference From Baseline' = PASS_RATE*100 - `Pass Percentage`) %>%
  select(-`Pass Percentage`) %>%
  rename("Team" = TEAM,
         'Total Plays' = TOTAL_PLAYS,
         'Pass %' = PASS_RATE,
  ) %>%
  mutate(`Pass %` = `Pass %`*100)

pal <- function(x) {
  f_neg <- scales::col_numeric(
    palette = c('red', 'white'),
    domain = c(min(pub_tell_df$`Difference From Baseline`), 0)
  )
  f_pos <- scales::col_numeric(
    palette = c('white', 'lightgreen'),
    domain = c(0, max(pub_tell_df$`Difference From Baseline`))
  )
  ifelse(x < 0, f_neg(x), f_pos(x))
}

publication_table <- pub_tell_df %>%
  gt() %>%
  tab_header(
    title = "Tell Table",
    subtitle = "2022 Through 9 Weeks"
  ) %>%
  data_color(columns = c("Difference From Baseline"), colors = pal) %>%
  gt_nfl_logos(columns = "Team") %>%
  fmt_number(
    columns = c("Pass %", "Difference From Baseline"),
    decimals = 1,
    pattern = "{x}%"
  )

publication_table

######## table for inspection variables and their unique values

var_df = data.frame("Variable" = c("Receiver Alignment", "In Motion at Snap?",
                                   "Position in Motion", "Offensive Formation"),
                    "Unique Values" = c(paste(unique(receiverAlignment_df$var1), collapse = ", "),
                                        paste(unique(inMotionAtBallSnap_df$var1), collapse = ", "),
                                        paste(unique(position_df$var1), collapse = ", "),
                                        paste(unique(offenseFormation_df$var1), collapse = ", "))
)
names(var_df) = c("Variable","Unique Values")
publication_table = var_df %>%
  gt() %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

publication_table

###### visual for teams pass rate with boundary motion

non_motion_b_df = plays_players_unique %>%
  anti_join(boundary_cross, by = c('playId', 'gameId'))

non_motion_b_df = data.frame(table(non_motion_b_df$possessionTeam, non_motion_b_df$type)) %>%
  pivot_wider(
    names_from = Var2,
    values_from = Freq,
    values_fill = 0
  ) %>%
  rename(Team = Var1,
         Runs = RUN,
         Passes = PASS) %>%
  mutate("Pass Percentage" = round(Passes / (Passes + Runs) * 100, digits = 1)) %>%
  select(Team, `Pass Percentage`)


pub_df = teams_b_mo %>%
  arrange(desc(TotalPlays)) %>%
  left_join(non_motion_b_df, by = "Team")


# pub_df$total_pass_perc = NA
# for(i in 1:nrow(pub_df)){
#   tmp_df = data.frame(table(plays_players_unique$type[plays_players_unique$possessionTeam == pub_df$Team[i]]))
#   
#   total_pass_perc = paste0(round(tmp_df$Freq[tmp_df$Var1 == 'PASS'] / (sum(tmp_df$Freq)) * 100, digits = 1), "%")
#   
#   pub_df$total_pass_perc[i] = total_pass_perc
# }

pub_df = pub_df[,c(1,5,2,3,4,6)]

colnames(pub_df) = c("Team","Boundary Motion Plays","Passes","Runs","Pass %","Baseline Pass Percentage")

# pub_df$scaled_baseline = 2 * (pub_df$`Baseline Pass Percentage` - min(pub_df$`Baseline Pass Percentage`)) / (max(pub_df$`Baseline Pass Percentage`) - min(pub_df$`Baseline Pass Percentage`)) - 1
# pub_df$scaled_pass = 2 * (pub_df$`Pass %` - min(pub_df$`Pass %`)) / (max(pub_df$`Pass %`) - min(pub_df$`Pass %`)) - 1

pub_df$scaled_baseline = 2 * (pub_df$`Baseline Pass Percentage` - 0) / (100 - 0) - 1
pub_df$scaled_pass = 2 * (pub_df$`Pass %` - 0) / (100- 0) - 1

ggplot(pub_df, aes(x = scaled_baseline, y = scaled_pass)) +
  xlim(c(-0.4,0.4)) +
  ylim(c(-0.75,0.75)) +
  geom_nfl_logos(aes(team_abbr = Team), width = 0.075) +
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

################################### TELL DF
tell_df$Variable = NA
tell_df$Variable[tell_df$var1 == TRUE | tell_df$var1 == FALSE] = "In Motion at Snap"
tell_df$Variable[tell_df$var1 == 'TE' | tell_df$var1 == 'WR' | tell_df$var1 == 'FB' |tell_df$var1 == 'RB'] = "Position in Motion"
tell_df$Variable[tell_df$var1 == 'SHOTGUN'] = "Offensive Formation"
tell_df$Variable[tell_df$var1 == '2x2' | tell_df$var1 == '3x1'] = "Receiver Alignment at Snap"

tell_df_select = tell_df %>%
  select(Variable, var1, PASS_RATE, `Pass Percentage`, TOTAL_PLAYS, TEAM) %>%
  mutate("Difference From Baseline" = round((PASS_RATE * 100) - `Pass Percentage`, digits = 1)) %>%
  select(TEAM,Variable, var1,TOTAL_PLAYS,PASS_RATE, "Difference From Baseline") %>%
  arrange(TEAM)

tell_df_select$PASS_RATE = paste0(round(tell_df_select$PASS_RATE * 100, 1), "%")

names(tell_df_select) = c("Team","Variable", "Value", "Total Plays", "Pass Rate", "Difference From Baseline")

pal <- function(x) {
  f_neg <- scales::col_numeric(
    palette = c('red', 'white'),
    domain = c(min(tell_df_select$`Difference From Baseline`), 0)
  )
  f_pos <- scales::col_numeric(
    palette = c('white', 'lightgreen'),
    domain = c(0, max(tell_df_select$`Difference From Baseline`))
  )
  ifelse(x < 0, f_neg(x), f_pos(x))
}

# tell_df_select = tell_df_select %>%
#   filter(Team %in% c('MIN'))

publication_table <- tell_df_select %>%
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

################## 

ggplot(data = tell_df, aes(x = Variable, group = var1)) + geom_bar(stat = 'count', position = 'dodge', fill = 'lightgreen', color = 'black') +
  geom_text(stat = 'count',
            aes(label = var1),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  theme_minimal() +
  theme(legend.position = 'none',
        panel.grid.major = element_line(color = rgb(0, 0, 0, alpha = 0.5)),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 18, angle = 15, hjust = 0.9),
        axis.text.y = element_text(size = 20),
        title = element_text(size = 20)) +
  labs(
    title = "Distribution of Keys",
    x = "Key",
    y = "Number of Teams With Key"
  )

########

unique_keys = data.frame(table(tell_df$Variable))
names(unique_keys) = c("Key", "Frequency")

unique_keys %>% 
  gt() %>%
  tab_header(
    title = "Frequency of Keys"
  )
