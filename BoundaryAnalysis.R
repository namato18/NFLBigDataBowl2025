library(dplyr)
library(ggplot2)
library(gganimate)
library(tidyr)

######################################
######################################
###################################### LOAD IN DATA
######################################
######################################

# Read in data
players = read.csv('nfl-big-data-bowl-2025/players.csv')
player_play = read.csv('nfl-big-data-bowl-2025/player_play.csv')
plays = read.csv('nfl-big-data-bowl-2025/plays.csv')
games = read.csv("nfl-big-data-bowl-2025/games.csv")

# Read in tracking data
tracking = readRDS('nfl-big-data-bowl-2025/tracking.rds')

######################################
######################################
######################################
######################################
######################################

# Identify play type as run or pass
plays$type = NA
plays$type[is.na(plays$passLocationType)] = 'RUN'
plays$type[!is.na(plays$passLocationType)] = 'PASS'

plays_players = left_join(player_play, plays, by = c('playId', 'gameId'))
plays_players = left_join(players, plays_players, by = c('nflId'))
plays_players = left_join(plays_players, games, by = 'gameId')

plays_players$HomeAway = 'AWAY'
plays_players$HomeAway[plays_players$teamAbbr == plays_players$homeTeamAbbr] = 'HOME'

plays_players$ScoreDifference = NA
plays_players$ScoreDifference[plays_players$HomeAway == 'AWAY'] = plays_players$preSnapVisitorScore[plays_players$HomeAway == 'AWAY'] - plays_players$preSnapHomeScore[plays_players$HomeAway == 'AWAY']
plays_players$ScoreDifference[plays_players$HomeAway == 'HOME'] = plays_players$preSnapHomeScore[plays_players$HomeAway == 'HOME'] - plays_players$preSnapVisitorScore[plays_players$HomeAway == 'HOME']

plays_players$PossessionDifference[plays_players$ScoreDifference == 0] = "TIE"

plays_players$PossessionDifference[plays_players$ScoreDifference < 0 & plays_players$ScoreDifference >= -8] = "DOWN 0-8"
plays_players$PossessionDifference[plays_players$ScoreDifference <= -9] = "DOWN 9+"

plays_players$PossessionDifference[plays_players$ScoreDifference > 0 & plays_players$ScoreDifference <= 8] = "UP 0-8"
plays_players$PossessionDifference[plays_players$ScoreDifference >= 9] = "UP 9+"



# Identify if the play starts in the redzone or not

plays_players$redzone = FALSE
plays_players$redzone[plays_players$yardlineNumber <= 20 & plays_players$possessionTeam != plays_players$yardlineSide] = TRUE

# Track ball on line-set and trim outliers
tracking_ball = tracking %>% filter(event == 'line_set', club == 'football')
# plot(tracking_ball$x, tracking_ball$y)

trim_outside_hash = tracking_ball %>% filter(y >= 23 & y <= 30.5)
# plot(trim_outside_hash$x, trim_outside_hash$y)

# Determine boundary side
trim_outside_hash$boundary = NA
trim_outside_hash$boundary[trim_outside_hash$playDirection == 'right' & trim_outside_hash$y <= 25] = 'right'
trim_outside_hash$boundary[trim_outside_hash$playDirection == 'right' & trim_outside_hash$y >= 29] = 'left'

trim_outside_hash$boundary[trim_outside_hash$playDirection == 'left' & trim_outside_hash$y <= 25] = 'left'
trim_outside_hash$boundary[trim_outside_hash$playDirection == 'left' & trim_outside_hash$y >= 29] = 'right'

trim_outside_hash = trim_outside_hash %>% select(gameId, playId, boundary)

# merge the boundary side back into the full tracking data
tracking = left_join(tracking, trim_outside_hash, by = c('gameId', 'playId'))


#### NOW THAT BOUNDARY HAS BEEN DETERMINED FOR EACH PLAY,
#### LETS GRAB EACH PLAY THAT HAS MOTION

motion_df = plays_players %>% filter(inMotionAtBallSnap == TRUE | motionSinceLineset == TRUE) %>%
  filter(position == "FB" | position == "WR" | position == "TE" | position == "RB")

# Now I want to add the ball position at line-set, player position and start of motion,
# and player position at ball_snap

# attatch player position and ball position
pos_player_snap = tracking %>% filter(event == 'ball_snap') %>%
  mutate(x_snap = x) %>%
  mutate(y_snap = y) %>%
  mutate(s_snap = s) %>%
  select(gameId, playId, nflId, x_snap, y_snap, s_snap)

pos_player_motion = tracking %>% filter(event == 'line_set') %>%
  mutate(x_start_motion = x) %>%
  mutate(y_start_motion = y) %>%
  mutate(s_start_motion = s) %>%
  select(gameId, playId, nflId, x_start_motion, y_start_motion, s_start_motion)

pos_ball_snap = tracking %>% filter(event == 'line_set') %>%
  filter(club == 'football') %>%
  mutate(x_ball = x) %>%
  mutate(y_ball = y) %>%
  select(gameId, playId, x_ball, y_ball, playDirection, boundary)

motion_df = left_join(motion_df, pos_player_snap, by = c("nflId",'gameId','playId'))
motion_df = left_join(motion_df, pos_player_motion, by = c("nflId",'gameId','playId'))
motion_df = left_join(motion_df, pos_ball_snap, by = c('gameId','playId'))

# Lets also add which direction the play ended up going for pass/run
motion_df$run_direction = NA
motion_df$pass_direction = NA

motion_df$run_direction[grep(pattern = 'right', x = motion_df$rushLocationType, ignore.case = TRUE)] = 'right'
motion_df$run_direction[grep(pattern = 'left', x = motion_df$rushLocationType, ignore.case = TRUE)] = 'left'

motion_df$pass_direction[grepl(pattern = 'right', x = motion_df$playDescription, ignore.case = TRUE) &
                           grepl(pattern = 'pass', x = motion_df$playDescription, ignore.case = TRUE)] = 'right'
motion_df$pass_direction[grepl(pattern = 'left', x = motion_df$playDescription, ignore.case = TRUE) &
                           grepl(pattern = 'pass', x = motion_df$playDescription, ignore.case = TRUE)] = 'left'
motion_df$pass_direction[grepl(pattern = 'inside', x = motion_df$playDescription, ignore.case = TRUE) &
                           grepl(pattern = 'pass', x = motion_df$playDescription, ignore.case = TRUE)] = 'inside'

# Lets add which direction the player is going in motion
motion_df$motion_direction = NA

motion_df$motion_direction[motion_df$y_start_motion > motion_df$y_snap & motion_df$playDirection == 'right'] = 'right'
motion_df$motion_direction[motion_df$y_start_motion < motion_df$y_snap & motion_df$playDirection == 'right'] = 'left'

motion_df$motion_direction[motion_df$y_start_motion > motion_df$y_snap & motion_df$playDirection == 'left'] = 'left'
motion_df$motion_direction[motion_df$y_start_motion < motion_df$y_snap & motion_df$playDirection == 'left'] = 'right'

# Add column to check if motion crossed formation
motion_df$cross_formation = FALSE
motion_df$cross_formation[sign(motion_df$y_start_motion - motion_df$y_ball) != sign(motion_df$y_snap - motion_df$y_ball) &
                            abs(motion_df$y_snap - motion_df$y_ball) >= 0.5] = TRUE


# Create DF for motion into boundary cross formation
boundary_cross = motion_df %>%
  filter(cross_formation == TRUE) %>%
  filter(boundary == motion_direction)

length(which(duplicated(boundary_cross[,c('playId', 'gameId')])))
which(duplicated(boundary_cross[,c('playId', 'gameId')]))

# Check out duplicate
duplicate_df = boundary_cross[boundary_cross$playId == boundary_cross$playId[350] &
                                boundary_cross$gameId == boundary_cross$gameId[350] ,]

# flip coordinates of left direction throws
boundary_cross$targetY[boundary_cross$playDirection == 'left'] = 53 - boundary_cross$targetY[boundary_cross$playDirection == 'left']

# Visual to check Y location of throws with motion into boundary
boundary_cross$adjusted_targetX = NA
boundary_cross$adjusted_targetX[boundary_cross$playDirection == 'left'] = abs(boundary_cross$targetX[boundary_cross$playDirection == 'left'] - boundary_cross$x_ball[boundary_cross$playDirection == 'left'])
boundary_cross$adjusted_targetX[boundary_cross$playDirection == 'right'] = boundary_cross$targetX[boundary_cross$playDirection == 'right'] - boundary_cross$x_ball[boundary_cross$playDirection == 'right']

table(boundary_cross$pass_direction)

boundary_cross_hash = boundary_cross %>%
  filter(boundary == 'right') %>%
  filter(type == 'PASS')

plot(boundary_cross_hash$adjusted_targetX,
     boundary_cross_hash$targetY)
# For a heatmap I need to create new variables that store the distance of throw
# from a 0 starting point
heat_df = data.frame(
  throw_dist_x = boundary_cross_hash$adjusted_targetX,
  throw_dist_y = boundary_cross_hash$targetY
)

# Heatmap using geom_bin2d
ggplot(heat_df, aes(x = throw_dist_x, y = throw_dist_y)) +
  geom_bin2d(bins = 10) +  # Adjust 'bins' for finer/coarser grid
  scale_fill_gradient(low = "white", high = "red") +
  geom_hline(yintercept = 30, color = "black", linetype = "dashed") +
  geom_hline(yintercept = 23, color = "black", linetype = "dashed") +
  annotate("point", x = 0, y = 23, color = 'green', size = 5) +
  labs(x = "X Difference", y = "Y Difference", fill = "Count") +
  theme_minimal()


plays_players_unique = plays_players[!duplicated(plays_players[,c('playId','gameId')]),]
table(plays_players_unique$type)

plays_players_unique = left_join(plays_players_unique, pos_ball_snap, by = c('gameId','playId'))
plays_players_unique = plays_players_unique %>% filter(!is.na(playDirection))

plays_players_unique$adjusted_targetX = NA
plays_players_unique$adjusted_targetX[plays_players_unique$playDirection == 'left'] = abs(plays_players_unique$targetX[plays_players_unique$playDirection == 'left'] - plays_players_unique$x_ball[plays_players_unique$playDirection == 'left'])
plays_players_unique$adjusted_targetX[plays_players_unique$playDirection == 'right'] = plays_players_unique$targetX[plays_players_unique$playDirection == 'right'] - plays_players_unique$x_ball[plays_players_unique$playDirection == 'right']

motion_boundary_plays_remove = unique(boundary_cross$playId)

all_other_heat = plays_players_unique %>%
  filter(boundary == 'right') %>%
  filter(!playId %in% motion_boundary_plays_remove)

# Check general dist of throws
ggplot(all_other_heat, aes(x = adjusted_targetX, y = targetY)) +
  geom_bin2d(bins = 10) +  # Adjust 'bins' for finer/coarser grid
  scale_fill_gradient(low = "white", high = "red") +
  geom_hline(yintercept = 30, color = "black", linetype = "dashed") +
  geom_hline(yintercept = 23, color = "black", linetype = "dashed") +
  annotate("point", x = 0, y = 23, color = 'green', size = 5) +
  labs(x = "X Difference", y = "Y Difference", fill = "Count") +
  theme_minimal()

# Check how many plays are in the redzone
table(boundary_cross$redzone)

# Check table of plays on 3rd down
table(boundary_cross$down)

# Add variable for down and distance
boundary_cross$DownDistance = NA

boundary_cross$DownDistance[boundary_cross$down == 1 & boundary_cross$yardsToGo >= 5] = 'FIRST AND LONG'
boundary_cross$DownDistance[boundary_cross$down == 1 & boundary_cross$yardsToGo < 5] = 'FIRST AND SHORT'

boundary_cross$DownDistance[boundary_cross$down == 2 & boundary_cross$yardsToGo >= 5] = 'SECOND AND LONG'
boundary_cross$DownDistance[boundary_cross$down == 2 & boundary_cross$yardsToGo < 5] = 'SECOND AND SHORT'

boundary_cross$DownDistance[boundary_cross$down == 3 & boundary_cross$yardsToGo >= 5] = 'THIRD AND LONG'
boundary_cross$DownDistance[boundary_cross$down == 3 & boundary_cross$yardsToGo < 5] = 'THIRD AND SHORT'

boundary_cross$DownDistance[boundary_cross$down == 4 & boundary_cross$yardsToGo >= 5] = 'FOURTH AND LONG'
boundary_cross$DownDistance[boundary_cross$down == 4 & boundary_cross$yardsToGo < 5] = 'FOURTH AND SHORT'

boundary_cross$QuarterScore = NA

boundary_cross$QuarterScore[boundary_cross$quarter == 1 & boundary_cross$ScoreDifference > 0] = '1ST QRT WINNING'
boundary_cross$QuarterScore[boundary_cross$quarter == 1 & boundary_cross$ScoreDifference < 0] = '1ST QRT LOSING'
boundary_cross$QuarterScore[boundary_cross$quarter == 1 & boundary_cross$ScoreDifference == 0] = '1ST QRT TIE'

boundary_cross$QuarterScore[boundary_cross$quarter == 2 & boundary_cross$ScoreDifference > 0] = '2ND QRT WINNING'
boundary_cross$QuarterScore[boundary_cross$quarter == 2 & boundary_cross$ScoreDifference < 0] = '2ND QRT LOSING'
boundary_cross$QuarterScore[boundary_cross$quarter == 2 & boundary_cross$ScoreDifference == 0] = '2ND QRT TIE'

boundary_cross$QuarterScore[boundary_cross$quarter == 3 & boundary_cross$ScoreDifference > 0] = '3RD QRT WINNING'
boundary_cross$QuarterScore[boundary_cross$quarter == 3 & boundary_cross$ScoreDifference < 0] = '3RD QRT LOSING'
boundary_cross$QuarterScore[boundary_cross$quarter == 3 & boundary_cross$ScoreDifference == 0] = '3RD QRT TIE'

boundary_cross$QuarterScore[boundary_cross$quarter == 4 & boundary_cross$ScoreDifference > 0] = '3RD QRT WINNING'
boundary_cross$QuarterScore[boundary_cross$quarter == 4 & boundary_cross$ScoreDifference < 0] = '3RD QRT LOSING'
boundary_cross$QuarterScore[boundary_cross$quarter == 4 & boundary_cross$ScoreDifference == 0] = '3RD QRT TIE'


# I want to specifically check the plays that are within 2yards of ball snap
within_2 = boundary_cross[abs(boundary_cross$y_snap - boundary_cross$y_ball) < 2, ]

# Check how many of each position
table(boundary_cross$position)

# Create table to check direction of plays by boundary side
table(boundary_cross$boundary, boundary_cross$pass_direction)
table(boundary_cross$boundary, boundary_cross$run_direction)

# Check average time to throw since most of throws are in box
# and compare to overall average time to throw
# (they're about the same, not much here )

mean(boundary_cross$timeToThrow, na.rm = TRUE)
mean(plays$timeToThrow, na.rm = TRUE)

# Check dist of teams in df
sort(table(boundary_cross$teamAbbr), decreasing = TRUE)

# Dataframe for each positions motion
WR_cross = boundary_cross[boundary_cross$position == 'WR',]
TE_cross = boundary_cross[boundary_cross$position == 'TE',]
RB_cross = boundary_cross[boundary_cross$position == 'RB',]

table(TE_cross$boundary, TE_cross$pass_direction)
table(TE_cross$boundary, TE_cross$run_direction)

# Dataframe for only 1st down plays
boundary_cross_1st = boundary_cross[boundary_cross$down == 1,]
boundary_cross_2nd = boundary_cross[boundary_cross$down == 2,]
boundary_cross_3rd = boundary_cross[boundary_cross$down == 3,]

# Create table to see how many times teams throw/pass
# (with motion into the boundary)
teams_b_mo = data.frame(table(boundary_cross$teamAbbr, boundary_cross$type)) %>%
  pivot_wider(
    names_from = Var2,
    values_from = Freq,
    values_fill = 0
  ) %>%
  rename(Team = Var1,
         Runs = RUN,
         Passes = PASS)

teams_b_mo$pass_ratio = teams_b_mo$Passes / (teams_b_mo$Runs + teams_b_mo$Passes) * 100
teams_b_mo$TotalPlays = teams_b_mo$Passes + teams_b_mo$Runs

## After finding the teams with the most threat of pass/run overall
## I want to identify the percentages by team/position
## lets check out dallas for starters
boundary_cross_team = boundary_cross %>% filter(teamAbbr == 'KC') %>% filter(receiverAlignment == '2x2')
table(boundary_cross_team$receiverAlignment, boundary_cross_team$type)

table(boundary_cross_team$down, boundary_cross_team$type)

# Adjust pass rating using bayesian smoothing
# (DOWNS WITH MANY PLAYS: OBSERVED HAVE MORE INFLUENCE)
# (DOWNS WITH FEW PLAYS: BASELINE HAS MORE INFLUENCE)
teams = sort(table(boundary_cross$teamAbbr), decreasing = TRUE)
teams = names(teams[teams >= 30])

#teams = unique(boundary_cross$teamAbbr)
target_vars = c("DownDistance", "receiverAlignment","QuarterScore",
                "inMotionAtBallSnap", "position", "offenseFormation",'redzone')

for(i in 1:length(target_vars)){
  for(j in 1:length(teams)){
    
    tmp_df = boundary_cross %>% filter(teamAbbr == teams[j]) %>%
      select(all_of(c(target_vars[i], "type")))
    
    team_baseline_pass_rating = teams_b_mo$pass_ratio[teams_b_mo$Team == teams[j]] / 100
    k = 2
    
    adjusted_team_df = data.frame(table(tmp_df[[1]], tmp_df$type)) %>%
      pivot_wider(
        names_from = Var2,
        values_from = Freq,
        values_fill = 0
      ) %>%
      mutate(PASS_RATE = PASS / (PASS + RUN),
             TOTAL_PLAYS = PASS + RUN,
             ADJUSTED_PASS_RATE = (TOTAL_PLAYS * PASS_RATE + k  * team_baseline_pass_rating) / (TOTAL_PLAYS + k)
      )
    
    colnames(adjusted_team_df)[1] = target_vars[i]
    colnames(adjusted_team_df)[1] = 'var1'
    
    adjusted_team_df$TEAM = teams[j]
    
    if(j == 1){
      comb_df = adjusted_team_df
    }else{
      comb_df = rbind(comb_df, adjusted_team_df)
    }
    
  }
  assign(paste0(target_vars[i],"_df"),comb_df, .GlobalEnv)
}
names_df = data.frame(
  Variable = c(unique(DownDistance_df$var1), unique(receiverAlignment_df$var1), unique(QuarterScore_df$var1),
               unique(inMotionAtBallSnap_df$var1), unique(position_df$var1), unique(offenseFormation_df$var1))
)

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
  mutate("non_b_mo_plays" = Passes + Runs) %>%
  select(Team, `non_b_mo_plays`, `Pass Percentage`)

bind_df = bind_rows(receiverAlignment_df,
                    inMotionAtBallSnap_df, position_df, offenseFormation_df) %>%
  left_join(non_motion_b_df, join_by('TEAM' == 'Team'))

# FIND OUTLIERS THAT ARE TELLS FOR TEAMS
ind = which(bind_df$TOTAL_PLAYS >= 18 &
              bind_df$PASS_RATE*100 - bind_df$`Pass Percentage` >= 15 &
              bind_df$PASS_RATE*100 >= 70)

tell_df = bind_df[ind,]


boundary_cross_filtered = boundary_cross %>%
  filter(teamAbbr %in% teams) %>%
  filter(quarter != 5)

rating_df = data.frame(
  DownDist_pass_rating = rep(NA, nrow(boundary_cross_filtered)),
  receiver_alignment_rating = rep(NA, nrow(boundary_cross_filtered)),
  quarter_rating = rep(NA, nrow(boundary_cross_filtered)),
  motion_ball_snap_rating = rep(NA, nrow(boundary_cross_filtered)),
  position_rating = rep(NA, nrow(boundary_cross_filtered)),
  formation_rating = rep(NA, nrow(boundary_cross_filtered)),
  redzone_rating = rep(NA, nrow(boundary_cross_filtered))
)



for(i in 1:nrow(boundary_cross_filtered)){
  
  rating_df$DownDist_pass_rating[i] = DownDistance_df$ADJUSTED_PASS_RATE[boundary_cross_filtered$DownDistance[i] == DownDistance_df$DownDistance &
                                                                           boundary_cross_filtered$teamAbbr[i] == DownDistance_df$TEAM]
  rating_df$receiver_alignment_rating[i] = receiverAlignment_df$ADJUSTED_PASS_RATE[boundary_cross_filtered$receiverAlignment[i] == receiverAlignment_df$receiverAlignment &
                                                                                     boundary_cross_filtered$teamAbbr[i] == receiverAlignment_df$TEAM]
  rating_df$quarter_rating[i] = QuarterScore_df$ADJUSTED_PASS_RATE[boundary_cross_filtered$QuarterScore[i] == QuarterScore_df$QuarterScore &
                                                                     boundary_cross_filtered$teamAbbr[i] == QuarterScore_df$TEAM]
  rating_df$motion_ball_snap_rating[i] = inMotionAtBallSnap_df$ADJUSTED_PASS_RATE[boundary_cross_filtered$inMotionAtBallSnap[i] == inMotionAtBallSnap_df$inMotionAtBallSnap &
                                                                                    boundary_cross_filtered$teamAbbr[i] == inMotionAtBallSnap_df$TEAM]
  rating_df$position_rating[i] = position_df$ADJUSTED_PASS_RATE[boundary_cross_filtered$position[i] == position_df$position &
                                                                  boundary_cross_filtered$teamAbbr[i] == position_df$TEAM]
  rating_df$formation_rating[i] = offenseFormation_df$ADJUSTED_PASS_RATE[boundary_cross_filtered$offenseFormation[i] == offenseFormation_df$offenseFormation &
                                                                           boundary_cross_filtered$teamAbbr[i] == offenseFormation_df$TEAM]
  rating_df$redzone_rating[i] = redzone_df$ADJUSTED_PASS_RATE[boundary_cross_filtered$redzone[i] == redzone_df$redzone &
                                                                boundary_cross_filtered$teamAbbr[i] == redzone_df$TEAM]
  print(i)
}

rating_df$MBP_RATING = (rating_df$DownDist_pass_rating * 0.2 + rating_df$receiver_alignment_rating * 0.1 + rating_df$quarter_rating * 0.1 +
                          rating_df$motion_ball_snap_rating * 0.1 + rating_df$position_rating * 0.1 + rating_df$formation_rating* 0.3 + rating_df$redzone_rating * 0.1)

rating_df$type = boundary_cross_filtered$type

rating_df$prediction = 'PASS'
rating_df$prediction[rating_df$MBP_RATING < 0.55] = 'RUN'

length(which(rating_df$prediction == rating_df$type)) / nrow(rating_df)

rating_df$defense_scheme = boundary_cross_filtered$pff_manZone
rating_df$yardsGained = boundary_cross_filtered$yardsGained
rating_df$correct_prediction = 0
rating_df$correct_prediction[rating_df$prediction == rating_df$type] = 1

rating_df$teamAbbr = boundary_cross_filtered$teamAbbr

# Group by teamAbbr and calculate accuracy
team_accuracy = rating_df %>%
  group_by(teamAbbr) %>%
  summarise(
    total_plays = n(),
    correct_predictions = sum(correct_prediction, na.rm = TRUE),
    accuracy = correct_predictions / total_plays
  ) %>%
  arrange(desc(accuracy)) # Sort by accuracy



# 
# team_baseline_pass_rating = teams_b_mo$pass_ratio[teams_b_mo$Team == 'SF'] / 100
# k = 2
# adjusted_team_df = data.frame(table(boundary_cross_team$PossessionDifference, boundary_cross_team$type)) %>%
#   pivot_wider(
#     names_from = Var2,
#     values_from = Freq,
#     values_fill = 0
#   ) %>%
#   rename(Down = Var1) %>%
#   mutate(PASS_RATE = PASS / (PASS + RUN),
#          TOTAL_PLAYS = PASS + RUN,
#          ADJUSTED_PASS_RATE = (TOTAL_PLAYS * PASS_RATE + k  * team_baseline_pass_rating) / (TOTAL_PLAYS + k)
#   )
# 
# str(adjusted_team_df)





# check total pass/run ratio
sum(teams_b_mo$Passes) / (sum(teams_b_mo$Runs) + sum(teams_b_mo$Passes)) * 100 # 70% throw chance

# Let's check the same but for plays between the 30 yard lines
boundary_cross_30 = boundary_cross %>%
  filter(yardlineNumber >= 30)

teams_b_mo_30 = data.frame(table(boundary_cross_30$teamAbbr, boundary_cross_30$type)) %>%
  pivot_wider(
    names_from = Var2,
    values_from = Freq,
    values_fill = 0
  ) %>%
  rename(Team = Var1,
         Runs = RUN,
         Passes = PASS)

teams_b_mo_30$pass_ratio = teams_b_mo_30$Passes / (teams_b_mo_30$Runs + teams_b_mo_30$Passes) * 100

# Create table to see how throws/runs by position motion
# (with motion into the boundary)
positions_b_mo = data.frame(table(boundary_cross$position, boundary_cross$type)) %>%
  pivot_wider(
    names_from = Var2,
    values_from = Freq,
    values_fill = 0
  ) %>%
  rename(Team = Var1,
         Runs = RUN,
         Passes = PASS)

teams_b_mo$pass_ratio = teams_b_mo$Passes / (teams_b_mo$Runs + teams_b_mo$Passes) * 100



##################################################################### ANIMATE

examine = tracking %>%
  filter(gameId == duplicate_df$gameId[2]) %>%
  filter(playId == duplicate_df$playId[2])

# Plot the field and player movements
field_plot <- ggplot(examine, aes(x = x, y = y, group = nflId, color = factor(club))) +
  # Draw field
  geom_rect(aes(xmin = 0, xmax = 120, ymin = 0, ymax = 50), fill = "black", color = "white") +
  # Plot player positions
  geom_point(size = 4) +
  # Add frame animation
  transition_time(frameId) +
  labs(title = "Player Movements: Frame {frame_time}", x = "X Position", y = "Y Position") +
  theme_minimal() +
  theme(legend.position = "none")

# Render and display animation directly in RStudio viewer
animate(field_plot, fps = 7, duration = 15, renderer = gifski_renderer())
