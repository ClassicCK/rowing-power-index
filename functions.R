# Placeholder for weather adjustment function
# Returns a multiplier between 0 and 1, with 1 being fair weather and <1 for poor conditions
adjust_for_conditions <- function(conditions) {
  if(conditions == "Good") return(1)
  if(conditions == "Poor") return(0.5)  # Example adjustment
}

adjust_for_league <- function(league) {
  if(league == "Youth") return(1.1)
  if(league == "Scholastic") return(1)
}

adjust_for_distance <- function(distance) {
  standard_distance <- 2000
  adjustment_factor <- 1 - ((distance - standard_distance) / 10000)  # Example adjustment
  adjustment_factor <- max(adjustment_factor, 0.5)  # Ensure it doesn't go below 0.5
  return(adjustment_factor)
}

# Natural logarithm of (absolute margin of victory + 1)
mov_multiplier <- function(margin_of_victory, elo_winner, elo_loser) {
  LN_MoV <- log(abs(margin_of_victory) + 1)
  
  # MoV Multiplier adjusted for autocorrelation
  # Using the formula provided, with adjustment for Elo difference
  MoV_Multiplier <- LN_MoV * (2.2 / ((elo_winner - elo_loser) * 0.001 + 2.2))
  
  return(MoV_Multiplier)
}

# Placeholder for race impact scaling function
# Returns a multiplier based on the race's impact level
adjust_impact <- function(impact_level) {
  if(impact_level == "Championship") return(1.2)
  if(impact_level %in% c("Final", "Semi-Final")) return(1.0)
  if(impact_level %in% c("Regatta", "Heat", "Time Trial")) return(0.9)
}

# Normalize race times to a standard distance (e.g., 2000 meters)
# Finish time is expected to be in MM:SS.millisecond format
normalize_time <- function(finish_time, race_distance) {
  # Convert MM:SS.millisecond to total seconds
  parts <- strsplit(finish_time, ":") # Split the time into minutes and seconds
  minutes <- as.numeric(sapply(parts, `[`, 1))
  seconds <- as.numeric(sapply(parts, function(x) x[2]))
  total_seconds <- minutes * 60 + seconds
  
  standard_distance <- 2000
  # Assuming linear scaling of time with distance
  adjusted_time <- total_seconds * (standard_distance / race_distance)
  return(adjusted_time)
}

calculate_time_diffs <- function(race_results) {
  race_results <- race_results %>%
    mutate(time_diff = abs(NormalizedTime - max(NormalizedTime))) %>%
    return(race_results)
}

# Function to adjust k based on race type
adjust_k_for_race_type <- function(race_type) {
  if (race_type %in% c("Time Trial")) {
    return(10)  # Higher sensitivity for earlier stages
  } else if (race_type == "Semi-Final") {
    return(24)  # Less sensitivity for final stages to not penalize as much
  } else if (race_type %in% c("Final", "Championship")) {
    return(20)  # Less sensitivity for final stages to not penalize as much
  } else {
    return(16)  # Default sensitivity
  }
}

# Rowing Power Index
RPI <- function(db) {
  # Fetch race results
  race_results_query <- "
      SELECT 
        Teams.TeamName, Teams.League, Teams.Division, Teams.Country, 
        Races.RaceID, Races.RaceDate, Races.Distance, Races.Conditions, Races.Type, 
        Results.FinishTime, Results.FinishPosition
      FROM Results
      JOIN Races ON Results.RaceID = Races.RaceID
      JOIN Teams ON Results.TeamName = Teams.TeamName
      ORDER BY Races.RaceDate, Races.RaceID, Results.FinishPosition;"
  race_results <- dbGetQuery(db, race_results_query)
  race_results$NormalizedTime <- mapply(normalize_time, race_results$FinishTime, race_results$Distance)
  
  # Initialize Elo ratings
  unique_team_names <- unique(race_results$TeamName)
  elo_adjustments <- setNames(rep(1500, length(unique_team_names)), unique_team_names)
  
  # Prepare a DataFrame to store Elo rating updates
  elo_updates <- data.frame(TeamName=character(), RPIDate=as.Date(character()), RPI=double(), stringsAsFactors=FALSE)
  
  # Process each unique race date
  for (date in unique(race_results$RaceDate)) {
    unique_races_on_date <- unique(race_results[race_results$RaceDate == date,]$RaceID)
    
    # Process each race on that date
    for (race_id in unique_races_on_date) {
      current_race_results <- race_results[race_results$RaceID == race_id,]
      current_race_results <- calculate_time_diffs(current_race_results)
      
      # Adjust k based on race type
      k <- adjust_k_for_race_type(current_race_results$Type[1])
      
      # Assuming time_diff represents the team's time difference from the best performance
      median_time_diff <- median(current_race_results$time_diff)
      
      is_final <- any(current_race_results$Type == "Championship")
      
      for (i in 1:nrow(current_race_results)) {
        team_i <- current_race_results$TeamName[i]
        teams <- current_race_results$TeamName[-i]
        elo_i <- elo_adjustments[team_i]
        
        # Calculate the performance score relative to the median
        performance_score_i <- ifelse(current_race_results$time_diff[i] <= median_time_diff, 0, 1)
        
        # Assuming a simplified calculation for expected performance
        expected_i <- 1 / (1 + 10 ^ ((mean(elo_racers) - elo_i) / 400))
        
        elo_racers <- elo_adjustments[teams]
        
        # Implementing MoV adjustments and autocorrelation corrections as discussed
        MoV_Multiplier <- mov_multiplier(current_race_results$time_diff[i], elo_i, mean(elo_racers))
        
        # Calculate the rating difference between the current team and the average of other teams in the race
        rating_diff <- elo_i - mean(elo_racers)
        league_adjustment_i <- adjust_for_league(current_race_results$Division[i])
        
        # Apply a dynamic factor based on the rating difference
        # Underdogs gain more from outperforming expectations, and top teams are protected from drastic drops
        if (rating_diff < 0) {
          # Team i is an underdog
          dynamic_factor <- 1 + abs(rating_diff) / 400*league_adjustment_i  # Example scaling factor; adjust based on your league's dynamics
        } else {
          # Team i is the higher-ranked team
          dynamic_factor <- 1 - abs(rating_diff) / 1000  # Smaller adjustment for higher-ranked teams
        }
        
        dynamic_factor <- max(0.5, min(dynamic_factor, 1.5))
        
        # Aggregate adjustments
        condition_adjustment_i <- adjust_for_conditions(current_race_results$Conditions[i])
        impact_adjustment_i <- adjust_impact(current_race_results$Type[i])
        distance_adjustment_i <- adjust_for_distance(current_race_results$Distance[i])
        
        # Adjust Elo change using MoV Multiplier
        elo_change_i <- k * MoV_Multiplier * (performance_score_i - expected_i) * condition_adjustment_i * distance_adjustment_i * league_adjustment_i * impact_adjustment_i
        
        # Now, check if the team won a final and apply the boost
        if (is_final && current_race_results$FinishPosition[i] == 1) {
          elo_change_i <- elo_change_i + 40*current_race_results$time_diff[1]/current_race_results$time_diff[2]
        }
        
        # Now, check if the team won a final and apply the boost
        if (is_final && current_race_results$FinishPosition[i] == 2) {
          elo_change_i <- elo_change_i + 15*current_race_results$time_diff[2]/current_race_results$time_diff[3]
        }
        
        # Now, check if the team won a final and apply the boost
        if (is_final && current_race_results$FinishPosition[i] == 3) {
          elo_change_i <- elo_change_i + 7.5*current_race_results$time_diff[3]/current_race_results$time_diff[4]
        }
        
        elo_adjustments[team_i] <- elo_i + elo_change_i
      }
      
      
      # Store the updated Elo ratings for each team from this race
      for (team_name in names(elo_adjustments)) {
        elo_updates <- rbind(elo_updates, data.frame(TeamName=team_name, RPIDate=date, RPI=elo_adjustments[team_name]))
      }
    }
  }
  
  # Bulk insert the aggregated elo_updates into the database, replacing 'TeamRPI' with your table name
  dbWriteTable(db, "TeamRPI", elo_updates, row.names=FALSE, overwrite=TRUE)
}

elo_column <- function(maxWidth = 55, ...) {
  colDef(maxWidth = maxWidth, align = "center", class = "cell number", ...)
}

champion_column <- function(maxWidth = 175, class = NULL, ...) {
  colDef(
    cell = format_pct,
    maxWidth = maxWidth,
    class = paste("cell number", class),
    style = function(value) {
      # Lighter color for <1%
      if (value < 0.01) {
        list(color = "#aaa")
      } else {
        list(color = "#111", background = knockout_pct_color(value))
      }
    },
    ...
  )
}

format_pct <- function(value) {
  if (value == 0) "  \u2013 "    # en dash for 0%
  #else if (value == 1) "\u2713"  # checkmark for 100%
  else if (value < 0.01) " <1%"
  else if (value > 0.99) ">99%"
  else formatC(paste0(round(value * 100), "%"), width = 4)
}

make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}

simulate_stotesbury <- function(teams_info, n_simulations = 10000) {
  n_teams <- nrow(teams_info)
  
  # Initialize matrices to track advancement
  semifinals_matrix <- matrix(0, nrow = n_teams, ncol = n_simulations)
  finals_matrix <- matrix(0, nrow = n_teams, ncol = n_simulations)
  champion_matrix <- matrix(0, nrow = n_teams, ncol = n_simulations)
  
  for (sim in 1:n_simulations) {
    # Simulate variability in performance using RPI
    simulated_rpis <- teams_info$RPI + rnorm(n_teams, mean = 0, sd = 36)
    
    # Determine advancement based on simulated RPI
    # Top 18 teams advance to semifinals
    semifinalists <- order(simulated_rpis, decreasing = TRUE)[1:18]
    semifinals_matrix[semifinalists, sim] <- 1
    
    # Top 6 teams advance to finals
    finalists <- semifinalists[order(simulated_rpis[semifinalists], decreasing = TRUE)[1:6]]
    finals_matrix[finalists, sim] <- 1
    
    # Winner of the final
    champion <- finalists[which.max(simulated_rpis[finalists])]
    champion_matrix[champion, sim] <- 1
  }
  
  # Calculate probabilities
  teams_info$Semifinals_Prob <- rowMeans(semifinals_matrix)
  teams_info$Finals_Prob <- rowMeans(finals_matrix)
  teams_info$Champion_Prob <- rowMeans(champion_matrix)
  
  return(teams_info)
}

simulate_youths <- function(teams_info, n_simulations = 10000) {
  n_teams <- nrow(teams_info)
  
  # Initialize matrices to track advancement
  semifinals_matrix <- matrix(0, nrow = n_teams, ncol = n_simulations)
  finals_matrix <- matrix(0, nrow = n_teams, ncol = n_simulations)
  champion_matrix <- matrix(0, nrow = n_teams, ncol = n_simulations)
  
  for (sim in 1:n_simulations) {
    simulated_rpis <- teams_info$RPI + rnorm(n_teams, mean = 0, sd = 36)
    
    # Top 16 teams advance to two semifinals, top 8 from those advance to final
    semifinalists <- order(simulated_rpis, decreasing = TRUE)[1:16]
    semifinals_matrix[semifinalists, sim] <- 1
    
    finalists <- semifinalists[order(simulated_rpis[semifinalists], decreasing = TRUE)[1:8]]
    finals_matrix[finalists, sim] <- 1
    
    champion <- finalists[which.max(simulated_rpis[finalists])]
    champion_matrix[champion, sim] <- 1
  }
  
  teams_info$Semifinals_Prob <- rowMeans(semifinals_matrix)
  teams_info$Finals_Prob <- rowMeans(finals_matrix)
  teams_info$Champion_Prob <- rowMeans(champion_matrix)
  
  return(teams_info)
}

simulate_schools <- function(teams_info, n_simulations = 10000) {
  n_teams <- nrow(teams_info)
  
  # Initialize matrices to track advancement
  first_round_matrix <- matrix(0, nrow = n_teams, ncol = n_simulations)
  semifinals_matrix <- matrix(0, nrow = n_teams, ncol = n_simulations)
  finals_matrix <- matrix(0, nrow = n_teams, ncol = n_simulations)
  
  for (sim in 1:n_simulations) {
    simulated_rpis <- teams_info$RPI + rnorm(n_teams, mean = 0, sd = 36)
    
    # Top 8 advance straight to the semifinal
    direct_semifinalists <- order(simulated_rpis, decreasing = TRUE)[1:8]
    semifinals_matrix[direct_semifinalists, sim] <- 1
    
    # The remainder compete again, top 8 of that remainder also advance to semifinal
    remainder_semifinalists <- order(simulated_rpis, decreasing = TRUE)[9:n_teams]
    remainder_semifinalists <- remainder_semifinalists[1:8]
    semifinals_matrix[remainder_semifinalists, sim] <- 1
    
    # Top 8 of the 16 in the semifinal advance to the final
    semifinalists <- c(direct_semifinalists, remainder_semifinalists)
    finalists <- semifinalists[order(simulated_rpis[semifinalists], decreasing = TRUE)[1:8]]
    finals_matrix[finalists, sim] <- 1
    
    # Winner of the final
    champion <- finalists[which.max(simulated_rpis[finalists])]
    champion_matrix[champion, sim] <- 1
  }
  
  # Example adjustment assuming 'teams_info' is already defined correctly but needs matching probabilities
  teams_info$Semifinals_Prob <- rowMeans(semifinals_matrix[1:nrow(teams_info), ])
  teams_info$Finals_Prob <- rowMeans(finals_matrix[1:nrow(teams_info), ])
  teams_info$Champion_Prob <- rowMeans(champion_matrix[1:nrow(teams_info), ])
  
  return(teams_info)
}

simulate_henley <- function(teams_info, n_simulations = 10000) {
  n_teams <- nrow(teams_info)
  champion_matrix <- matrix(0, nrow = n_teams, ncol = n_simulations)
  
  for (sim in 1:n_simulations) {
    simulated_rpis <- teams_info$RPI + rnorm(n_teams, mean = 0, sd = 36)
    
    # Seed the teams
    seeded_teams <- order(simulated_rpis, decreasing = TRUE)[1:32]
    
    # Simulate head-to-head matchups
    for (round in 1:5) { # 5 rounds: 32 -> 16 -> 8 -> 4 -> 2 -> 1
      winners <- numeric()
      for (i in seq(1, length(seeded_teams), by = 2)) {
        match_up <- seeded_teams[i:(i+1)]
        winner <- match_up[which.max(simulated_rpis[match_up])]
        winners <- c(winners, winner)
      }
      seeded_teams <- winners
    }
    
    champion_matrix[seeded_teams, sim] <- 1
  }
  
  teams_info$Champion_Prob <- rowMeans(champion_matrix)
  
  return(teams_info)
}

