###############################################################################################
# This script produces an animation of the 30-60s mark of the 1st quarter of the 2021 AFL GF.
# It combines GPS data from @AFLLab and Event Data from @databyjosh.
# The GPS data can be accessed via 
# https://theafllab.wordpress.com/2021/10/22/full-afl-gps-data-a-taste/#the-data
# and event data is imported directly via the script.
# The animation produced contains convex hulls of both teams ahead and behind the ball.
#
# Elements such as venue length and width are hard coded as are the teams and colors.
# As it isn't necessary for this animation, no consideration has been given as to the
# direction the teams are kicking. The Event Data would need to be adjusted to account for
# possession chain ownership and kicking direction for other time periods to be considered.
#
# Syncing between GPS and Event Data can be hit or miss. In general a 3s lag has been
# allowed for here.
###############################################################################################

library(tidyverse)
library(gganimate) 
library(ggforce)
library(ggtext)
library(emo) # used for the ball icon (install via devtools::install_github("hadley/emo"))

# import the GPS csv (remember to set the working directory)
gps <- read.csv("GPSsample.csv", stringsAsFactors = F) 

# import the event location data
location_data <- read.csv(paste0("https://raw.githubusercontent.com/DataByJosh/AFL-Data/main/AFLM_Match_Chains/csvs/match_chains_2021_27.csv"), stringsAsFactors = F)


gps_edited <- gps %>%
  mutate(x = x / (0.5 / 82.5), # convert the x coordinate to metres
         y = y / (0.4 / 65), # convert the y coordinate to metres
         period = if_else(countdown > 1000 & lag(countdown) == 1, 1, 0), # find the start of the quarter
         period = cumsum(period),
         eoq = if_else(countdown == 0 & lag(countdown) == 1, 1, 0), # find the end of the quarter
         eoq = cumsum(eoq)) %>%
  ungroup() %>%
  filter(eoq + 1 == period) %>% # remove the data between quarters
  rename(periodSeconds = countup) %>% # rename to match location data
  group_by(name, period, periodSeconds) %>%
  # remove any outliers from gps trace and downsample to 1Hz
  mutate(mean_x = mean(x),
         sd_x = sd(x),
         x = if_else(x < mean_x - 2 * sd_x | x > mean_x + 2 * sd_x, mean_x, x),
         mean_y = mean(y),
         sd_y = sd(y),
         y = if_else(y < mean_y - 2 * sd_y | y > mean_y + 2 * sd_y, mean_y, y),
         x = mean(x),
         y = mean(y)) %>%
  filter(row_number() == 1) %>%
  group_by(name, period) %>%
  mutate(next_x = lead(x),
         next_y = lead(y)) %>%
  ungroup() %>%
  select(-mean_x, -mean_y, -sd_x, -sd_y) %>%
  # create variables for 1/10 of a second
  mutate(m0 = "",
         m1 = "",
         m2 = "",
         m3 = "",
         m4 = "",
         m5 = "",
         m6 = "",
         m7 = "",
         m8 = "",
         m9 = "") %>%
  pivot_longer(cols = starts_with("m"), names_to = "tenths", values_to = "values") %>%
  select(-values) %>%
  # smooth the 1Hz values back over 10Hz
  mutate(tenths = as.numeric(str_remove(tenths, "m")),
         x = (next_x - x) * (tenths / 10) + x,
         y = (next_y - y) * (tenths / 10) + y) %>%
  arrange(period, periodSeconds, tenths)
  
gf_stats <- location_data %>%
  select(playerName.givenName, playerName.surname, team = team.teamName, period, periodSeconds, stats = description, x, y) %>%
  mutate(name = paste0(str_sub(playerName.givenName, 1, 1), playerName.surname), # match the gps name convention
         name = if_else(name == "NANA", "", name),
         home = if_else(team == "Melbourne", 1, 0)) %>% # create Melbourne as home team as per gps data
  filter(stats != "Kick Inside 50 Result") %>% # remove this as the location will be out of sync with the other events
  select(name, home, period, periodSeconds, stats, x, y) %>%
  arrange(period, periodSeconds, name) %>%
  # sync the event data with gps times
  mutate(periodSeconds = case_when(periodSeconds > 3 ~ periodSeconds - 3,
                                   periodSeconds == 3 ~ 1,
                                   periodSeconds == 2 ~ 1,
                                   periodSeconds == 1 ~ 1,
                                   TRUE ~ 0),
         # designate the type of stat - required to find the ball location while in possession
         statType = case_when(stats %in% c("Hard Ball Get", "Loose Ball Get", "Free For", "Free For: In Possession", "Free For: Off The Ball",
                                           "Free Advantage", "Hard Ball Get Crumb", "Uncontested Mark", "Gather From Hit Out", "Handball Received",
                                           "Gather", "Kickin play on", "Loose Ball Get Crumb", "Mark On Lead", "Gather From Opposition", "Bounce",
                                           "Ruck Hard Ball Get", "Contested Mark", "Kickin Short", "Pack Mark (P)", "OOF Kick In") ~ "Possession",
                              stats %in% c("Kick", "Handball") ~ "Disposal",
                              stats %in% c("Centre Bounce", "Out of Bounds", "Ball Up Call") ~ "Umpire",
                              TRUE ~ "In Play")) %>%
  group_by(name, period, periodSeconds) %>%
  mutate(tenths = row_number() - 1) %>% # differentiate between possession and disposal time if they occur in the same second time interval
  ungroup() %>%
  rename(ball_x = x,
         ball_y = y) %>%
  # remove double possession entries that occur due to possession gains from opposition
  mutate(keep = if_else(name == lag(name) & period == lag(period) & periodSeconds == lag(periodSeconds) & stats == lag(stats), 0, 1),
         keep = if_else(row_number() == 1, 1, keep)) %>%
  filter(keep == 1) %>%
  select(-keep)

# join gps and location data
gf_gps <- full_join(gps_edited, gf_stats, by = c("name", "period", "periodSeconds", "tenths", "home")) %>%
  arrange(period, periodSeconds, tenths) %>%
  mutate(possessionPlayer = if_else(statType == "Possession", name, NULL),
         possessionPlayer = if_else(statType %in% c("Disposal", "Umpire", "In Play"), "Cut", possessionPlayer)) %>%
  fill(possessionPlayer) %>%
  mutate(x = if_else(is.na(x), as.numeric(ball_x), x), # if no name associated with event use ball location
         y = if_else(is.na(y), as.numeric(ball_y), y))

# create a df with complete time data to join with event gps data to find the ball location at each timestamp
gf_time <- gf_gps %>%
  select(period, periodSeconds, tenths) %>%
  distinct()

# create df of only the ball location at each timestamp  
ball_xy <- gf_gps %>%
  group_by(period, periodSeconds, tenths) %>%
  summarise(x = x[which(name == possessionPlayer | !is.na(statType))], # ball location is either player in posession or non na stat type
            y = y[which(name == possessionPlayer | !is.na(statType))]) %>%
  left_join(gf_time, .) %>% # join with the complete time data df
  mutate(time = periodSeconds + tenths / 10, # create time variable
         fill_x_up = x, # create variables to be filled (used to smooth the ball location when not in possession)
         fill_y_up = y,
         fill_x_down = x,
         fill_y_down = y,
         fill_time_up = if_else(!is.na(x), time, NULL),
         fill_time_down = if_else(!is.na(x), time, NULL)) %>%
  fill(c(fill_x_up, fill_y_up, fill_time_up), .direction = "up") %>%
  fill(c(fill_x_down, fill_y_down, fill_time_down), .direction = "down") %>%
  ungroup() %>%
  # smooth the ball location when not in posession
  mutate(x_current = (fill_x_up - fill_x_down) / (fill_time_up - fill_time_down) * (time - fill_time_down) + fill_x_down,
         y_current = (fill_y_up - fill_y_down) / (fill_time_up - fill_time_down) * (time - fill_time_down) + fill_y_down,
         x = if_else(is.na(x), x_current, x),
         y = if_else(is.na(y), y_current, y),
         name = "ball",
         home = 2) %>%
  select(name, home, period, periodSeconds, tenths, x, y)

# combine the ball and player location data
gf_data <- bind_rows(ball_xy, gf_gps) %>%
  arrange(period, periodSeconds, tenths) %>%
  filter(name != "") %>%
  mutate(time = periodSeconds + tenths / 10) %>%
  filter(period == 1 & between(periodSeconds, 30, 60)) %>% # filter the time period for animation
  group_by(period, periodSeconds, tenths) %>%
  # remove interchange players (this may not work well if play is close to the IC)
  mutate(IC = rank(y)) %>%
  filter(IC > 10) %>%
  group_by(time) %>%
  mutate(ball_x = if_else(name == "ball", x, NULL)) %>% # find the x location of the ball
  fill(ball_x) %>%
  # identify and count the number of players more than 5m ahead or behind the ball for each team
  mutate(Melb_def = if_else(home == 1 & x > ball_x + 5, 1, 0),
         Melb_def = sum(Melb_def),
         WB_attack = if_else(home == 0 & x > ball_x + 5, 1, 0),
         WB_attack = sum(WB_attack),
         Melb_attack = if_else(home == 1 & x < ball_x - 5, 1, 0),
         Melb_attack = sum(Melb_attack),
         WB_def = if_else(home == 0 & x < ball_x - 5, 1, 0),
         WB_def = sum(WB_def)) 
  
# create df of convex hull for players ahead or behind the ball (ahead and behind required for each team)
home_def_hull <- gf_data %>%
  group_by(period, periodSeconds, tenths) %>%
  mutate(ball_x = if_else(name == "ball", x, NULL)) %>%
  fill(ball_x) %>%
  filter(home == 1 & x > ball_x + 5) %>%
  slice(chull(x, y)) # chull function identifies players based on x, y coordinates and slice removes the unrequired ones

away_def_hull <- gf_data %>%
  group_by(period, periodSeconds, tenths) %>%
  mutate(ball_x = if_else(name == "ball", x, NULL)) %>%
  fill(ball_x) %>%
  filter(home == 0 & x < ball_x - 5) %>%
  slice(chull(x, y)) %>%
  mutate(rows = n())

home_attack_hull <- gf_data %>%
  group_by(period, periodSeconds, tenths) %>%
  mutate(ball_x = if_else(name == "ball", x, NULL)) %>%
  fill(ball_x) %>%
  filter(home == 1 & x < ball_x - 5) %>%
  slice(chull(x, y)) %>%
  mutate(rows = n())

away_attack_hull <- gf_data %>%
  group_by(period, periodSeconds, tenths) %>%
  mutate(ball_x = if_else(name == "ball", x, NULL)) %>%
  fill(ball_x) %>%
  filter(home == 0 & x > ball_x + 5) %>%
  slice(chull(x, y)) %>%
  mutate(rows = n())

p <- ggplot(gf_data) +
  geom_ellipse(data = filter(gf_data, name == "ball"), aes(x0 = 0, y0 = 0, a = 165 / 2, b = 130 / 2, angle = 0), size = 1.2) + # oval
  geom_rect(data = filter(gf_data, name == "ball"), aes(xmin = -25, xmax = 25, ymin = -25, ymax = 25), 
            fill = "#ffffff00", col = "black", size = 1.2) + # centre square
  geom_rect(data = filter(gf_data, name == "ball"), aes(xmin = 165 / 2 - 9, xmax = 165 / 2, ymin = 3.2, ymax = -3.2), 
            fill = "#ffffff00", col = "black", size = 1.2) + # right goal square
  geom_rect(data = filter(gf_data, name == "ball"), aes(xmin = -165 / 2 + 9, xmax = -165 / 2, ymin = 3.2, ymax = -3.2), 
            fill = "#ffffff00", col = "black", size = 1.2) + # left goal square
  geom_ellipse(data = filter(gf_data, name == "ball"), aes(x0 = 0, y0 = 0, a = 3, b = 3, angle = 0), size = 1.2) + # centre circle
  geom_arc(data = filter(gf_data, name == "ball"), aes(x0 = 165 / 2, y0 = 0, r = 50, start = -0.478, end = -(3.142 - 0.478)), size = 1.2) + # right 50m arc
  geom_arc(data = filter(gf_data, name == "ball"), aes(x0 = -165 / 2, y0 = 0, r = 50, start = 0.478, end = 3.142 - 0.478), size = 1.2) + # left 50m arc
  geom_point(data = filter(gf_data, name != "ball"), aes(x, y, col = as.factor(home)), size = 4) + # players
  geom_point(data = filter(gf_data, name == "ball"), aes(x, y), size = 3, shape = ji("rugby"), col = "brown") + # ball
  # convex hulls 
  geom_polygon(data = home_def_hull, aes(x, y, group = interaction(time, as.factor(home))), fill = "blue", alpha = 0.3) +
  geom_polygon(data = home_attack_hull, aes(x, y, group = interaction(time, as.factor(home))), fill = "blue", alpha = 0.3) +
  geom_polygon(data = away_def_hull, aes(x, y, group = interaction(time, as.factor(home))), fill = "red", alpha = 0.3) +
  geom_polygon(data = away_attack_hull, aes(x, y, group = interaction(time, as.factor(home))), fill = "red", alpha = 0.3) +
  # numbers ahead and behind the ball
  geom_text(data = filter(gf_data, name == "ball"), aes(label = paste("Melbourne players behind the ball:", Melb_def), 
                                                        x = 65, y = 60), size = 6, col = "blue") +
  geom_text(data = filter(gf_data, name == "ball"), aes(label = paste("Melbourne players ahead of the ball:", Melb_attack), 
                                                        x = -65, y = 60), size = 6, col = "blue") +
  geom_text(data = filter(gf_data, name == "ball"), aes(label = paste("WB players ahead of the ball:", WB_attack), 
                                                        x = 65, y = 55), size = 6, col = "red") +
  geom_text(data = filter(gf_data, name == "ball"), aes(label = paste("WB players behind the ball:", WB_def), 
                                                        x = -65, y = 55), size = 6, col = "red") +
  scale_color_manual(values = c("red", "blue")) +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_markdown(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 20),
        plot.caption = element_text(hjust = 0.5, size = 12)) +
  coord_fixed() +
  labs(title = "<span style='color:blue'>**Melbourne**</span> vs <span style='color:red'>**Western Bulldogs**</span> 2021 AFL GF",
       subtitle = "Player positioning ahead and behind the ball for 30-60s mark of Q1",
       caption = "Source: @AFLLab (GPS data) & @databyjosh (Event data)") +
  transition_time(time) 

# animate the plot (nframes should change depend on the start and finish time)
animate(p, fps = 10, nframes = 300, height = 850, width = 1600) 
anim_save("numbers.gif") # save the gif


