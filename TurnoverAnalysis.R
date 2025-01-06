plays_w_turnovers = plays

plays_w_turnovers$fumble = 0
plays_w_turnovers$fumble[grep(pattern = 'fumble', plays$playDescription, ignore.case = TRUE)] = 1

plays_w_turnovers$interception = 0
plays_w_turnovers$interception[grep(pattern = 'interc', plays$playDescription, ignore.case = TRUE)] = 1

plays_w_turnovers$any_turnover = 0
plays_w_turnovers$any_turnover[plays_w_turnovers$interception == 1 | plays_w_turnovers$fumble == 1] = 1

games_select = games %>%
  select(gameId, homeTeamAbbr, visitorTeamAbbr, homeFinalScore, visitorFinalScore)

plays_w_turnovers = plays_w_turnovers %>%
  left_join(games_select, by = "gameId")

plays_w_turnovers$turnover_home = 0
plays_w_turnovers$turnover_away = 0

plays_w_turnovers$turnover_home[plays_w_turnovers$interception == 1 &
                                  plays_w_turnovers$possessionTeam == plays_w_turnovers$homeTeamAbbr] = 1
plays_w_turnovers$turnover_away[plays_w_turnovers$interception == 1 &
                                  plays_w_turnovers$possessionTeam == plays_w_turnovers$visitorTeamAbbr] = 1
plays_w_turnovers$winner_home = 0
plays_w_turnovers$winner_home[plays_w_turnovers$homeFinalScore > plays_w_turnovers$visitorFinalScore] = 1

## group by game and find turnover sums

df_turnover = plays_w_turnovers %>%
  group_by(gameId) %>%
  summarize("home_turnovers" = sum(turnover_home),
            "away_turnovers" = sum(turnover_away),
            "home_winner" = mean(winner_home))

df_turnover = df_turnover[-which(df_turnover$home_turnovers == df_turnover$away_turnovers),]

df_turnover$home_team_wins_turnovers = 0  
df_turnover$home_team_wins_turnovers[df_turnover$home_turnovers < df_turnover$away_turnovers] = 1  

length(which(df_turnover$home_winner == df_turnover$home_team_wins_turnovers))  

viz_turnover = data.frame("var" = c("Won", "Lost"),
                          "won" = c(74.1, 25.9)) 
ggplot(data = viz_turnover, aes(x = var, y = won, fill = var)) + geom_bar(stat = 'identity') +
  ggtitle("Interception Battle Win Rate") +
  xlab("Won/Lost Interception Battle") +
  ylab("% of Games Won") +
  theme_minimal() +
  theme(legend.position = 'none',
        panel.grid.major = element_line(color = "black"),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 15))
  
  