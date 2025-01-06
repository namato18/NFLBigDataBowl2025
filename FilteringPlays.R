library(dplyr)

all_alignment_gameplays = c()
all_motion_gameplays = c()
all_position_gameplays = c()
all_formation_gameplays = c()

for(i in 1:nrow(tell_df)){
  
  team = tell_df$TEAM[i]
  key = tell_df$Variable[i]
  value = tell_df$var1[i]
  

  
  if(key == 'Receiver Alignment at Snap'){
    alignment_gameplays = which(boundary_cross$teamAbbr == team &
            boundary_cross$receiverAlignment == value)
    all_alignment_gameplays = c(all_alignment_gameplays, alignment_gameplays)
    
  }
  
  if(key == 'In Motion at Snap'){
    motion_gameplays = which(boundary_cross$teamAbbr == team &
                                  boundary_cross$inMotionAtBallSnap == value)
    all_motion_gameplays = c(all_motion_gameplays, motion_gameplays)
    
  }
  
  if(key == 'Position in Motion'){
    position_gameplays = which(boundary_cross$teamAbbr == team &
                               boundary_cross$position == value)
    all_position_gameplays = c(all_position_gameplays, position_gameplays)
    
  }
  
  if(key == 'Offensive Formation'){
    formation_gameplays = which(boundary_cross$teamAbbr == team &
                               boundary_cross$offenseFormation == value)
    all_formation_gameplays = c(all_formation_gameplays, formation_gameplays)
    
  }
  
}

all_gameplays = unique(c(all_alignment_gameplays, all_formation_gameplays, all_motion_gameplays, all_position_gameplays))

team_specific_keys_plays = boundary_cross[all_gameplays,]
team_specific_keys_plays = team_specific_keys_plays %>%
  select(gameId, playId, inMotionAtBallSnap, position, offenseFormation, receiverAlignment, teamAbbr)

write.csv(team_specific_keys_plays, 'NFLDataBowl2025/team_specific_keys_plays.csv', row.names = FALSE)


tracking_team_spec_key_plays = tracking %>%
  semi_join(team_specific_keys_plays, by = c("gameId", "playId"))
  
plays_games_teams = team_specific_keys_plays %>%
  select('teamAbbr','gameId','playId')

tracking_team_spec_key_plays = tracking_team_spec_key_plays %>%
  left_join(plays_games_teams, by = c("gameId", "playId"))


for(i in 1:length(unique(team_specific_keys_plays$teamAbbr))){
  team_specific_keys_tracking = tracking_team_spec_key_plays %>%
    filter(teamAbbr == unique(team_specific_keys_plays$teamAbbr)[i])
  
  assign(paste0(unique(team_specific_keys_plays$teamAbbr)[i],"_df"), team_specific_keys_tracking, .GlobalEnv)
  write.csv(team_specific_keys_tracking, paste0('NFLDataBowl2025/',unique(team_specific_keys_plays$teamAbbr)[i],"_df.csv"))
}

write.csv(tracking_team_spec_key_plays, 'NFLDataBowl2025/tracking_team_spec_key_plays.csv', row.names = FALSE)
