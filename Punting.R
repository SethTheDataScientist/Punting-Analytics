
# Special Teams -----------------------------------------------------------

WinsFill1 <- pbpCurrent %>% 
  filter(!is.na(posteam), !is.na(season), posteam != "") %>% 
  group_by(season, posteam, game_id) %>%
  slice_tail(n = 1) %>% 
  group_by(season, posteam) %>% 
  summarise(Wins = sum(PosteamWin, na.rm = T),
            PointsOff = sum(if_else(posteam == home_team, home_score, away_score), na.rm = T),
            PointsDef = sum(if_else(posteam == away_score, home_score, away_score), na.rm = T),
            PythagoreanWins = ((PointsOff^2.37)/(PointsOff^2.37+PointsDef^2.37))*17)


Punting <- pbpCurrent %>% 
  filter(!is.na(posteam), !is.na(punter_player_name)) %>%
  left_join(WinsFill1, by = c("season", "posteam")) %>% 
  group_by(punter_player_name) %>% 
  arrange((season)) %>% 
  summarise(LastTeam = tail(posteam, 1),
            LastSeason = tail(season, 1),
            Wins = head(Wins, 1),
            PythagoreanWins = head(PythagoreanWins, 1),
            Punts = sum(punt_attempt, na.rm = T),
            AvgStart = mean(yardline_100, na.rm = T),
            YPP = mean(kick_distance, na.rm = T),
            RYPP = mean(return_yards, na.rm = T),
            NetYPP = YPP - RYPP,
            EPA = mean(epa, na.rm = T),
            NonTDEPA = mean(epa[touchdown == 0], na.rm = T),
            NonReturnEPA = mean(epa[return_yards == 0 | punt_fair_catch == 1], na.rm = T),
            WPA = mean(wpa, na.rm = T),
            NonTDWPA = mean(wpa[touchdown == 0], na.rm = T),
            NonReturnWPA = mean(wpa[return_yards == 0 | punt_fair_catch == 1], na.rm = T),
            TD = sum(touchdown, na.rm = T),
            Punt20 = sum(punt_inside_twenty, na.rm = T),
            TwentyPerc = Punt20/Punts) %>% 
  filter(Punts >= 20) %>% 
  group_by() %>% 
  mutate(ID = case_when(punter_player_name == "B.Kern" ~ punter_player_name,
                        punter_player_name == "S.Martin" ~ punter_player_name,
                        punter_player_name == "T.Long" ~punter_player_name,
                        punter_player_name == "M.Haack" ~punter_player_name,
                        punter_player_name == "J.Berry" ~punter_player_name,
                        punter_player_name == "M.Palardy" ~punter_player_name,
                        punter_player_name == "J.Fox" ~ punter_player_name,
                        punter_player_name == "J.Hekker" ~ punter_player_name,
                        T ~ ""),
         ID2 = case_when(punter_player_name == "B.Kern" ~ punter_player_name,
                         punter_player_name == "S.Martin" ~punter_player_name,
                         punter_player_name == "T.Long" ~punter_player_name,
                         punter_player_name == "M.Haack" ~punter_player_name,
                         punter_player_name == "J.Berry" ~punter_player_name,
                         punter_player_name == "M.Palardy" ~punter_player_name,
                         punter_player_name == "J.Fox" ~ punter_player_name,
                         punter_player_name == "J.Hekker" ~punter_player_name,
                         T ~ ""))

ggplot(Punting, aes(x = TwentyPerc, y = NonReturnWPA))+
  geom_point(aes(alpha = if_else(ID == "", 0.8, 1), color = ID2))+
  geom_smooth(method = "lm", se = F)+
  theme_reach() +
  ggrepel::geom_text_repel(aes(label = ID, color = ID2), size = 4, box.padding = 0.2,
                           force = 1, max.overlaps = Inf,
                           min.segment.length = 0)

PuntingCor = Punting %>% 
  group_by() %>% 
  summarise(EPAYPPcor = cor(EPA, YPP),
            NonReturnEPAYPPcor =  cor(NonReturnEPA, YPP),
            EPARYPcor = cor(EPA, RYPP),
            EPANetcor = cor(EPA, NetYPP),
            WPAYPPcor = cor(WPA, YPP),
            NonReturnWPAYPPcor =  cor(NonReturnWPA, YPP),
            WPARYPcor = cor(WPA, RYPP),
            WPANetcor = cor(WPA, NetYPP),
            NonReturnWPANetcor = cor(NonReturnWPA, NetYPP),
            TwentyPercEPAcor = cor(EPA, TwentyPerc),
            TwentyPercWPAcor = cor(WPA, TwentyPerc),
            TwentyPercNonReturnWPAcor = cor(NonReturnWPA, TwentyPerc),
            EPAWinsCor = cor(EPA, Wins),
            NonReturnEPAWinscor =  cor(NonReturnEPA, Wins),
            WPAWinscor = cor(WPA, Wins),
            NonReturnWPAWinscor =  cor(NonReturnWPA, Wins),
            EPAPythagoreanWinsCor = cor(EPA, PythagoreanWins),
            NonReturnEPAPythagoreanWinscor =  cor(NonReturnEPA, PythagoreanWins),
            WPAPythagoreanWinscor = cor(WPA, PythagoreanWins),
            NonReturnWPAPythagoreanWinscor =  cor(NonReturnWPA, PythagoreanWins)
  )


CollegePunters <- pbpCollege %>% 
  filter(!is.na(pos_team), !is.na(punter_player_name)) %>%
  group_by(pos_team, drive_number) %>% 
  mutate(NextPlayLine = lead(yard_line, n = 1),
         PlayDistance = yard_line - (100-NextPlayLine),
         PuntInsideTwenty = case_when(punt == 1 & NextPlayLine <= 20 ~ 1,
                                      T ~ 0)) %>% 
  group_by(pos_team, punter_player_name) %>% 
  arrange((season)) %>% 
  summarise(LastTeam = tail(pos_team, 1),
            LastSeason = tail(season, 1),
            Punts = sum(punt, na.rm = T),
            AvgStart = mean(yard_line, na.rm = T),
            YPP = mean(PlayDistance, na.rm = T),
            RYPP = mean(yds_punt_return, na.rm = T),
            NetYPP = YPP - RYPP,
            EPA = mean(EPA, na.rm = T),
            NonTDEPA = mean(EPA[touchdown == 0], na.rm = T),
            NonReturnEPA = mean(EPA[yds_punt_return == 0 | punt_fair_catch == 1], na.rm = T),
            WPA = mean(wpa, na.rm = T),
            NonTDWPA = mean(wpa[touchdown == 0], na.rm = T),
            NonReturnWPA = mean(wpa[yds_punt_return == 0 | punt_fair_catch == 1], na.rm = T),
            TD = sum(touchdown, na.rm = T),
            Punt20 = sum(PuntInsideTwenty, na.rm = T),
            TwentyPerc = Punt20/Punts) %>% 
  filter(Punts >= 40, LastSeason == 2021) %>% 
  group_by() %>% 
  mutate(ID = case_when(punter_player_name == "Matt Araiza" ~ punter_player_name,
                        punter_player_name == "Ryan Stonehouse" ~ punter_player_name,
                        punter_player_name == "Jordan Stout" ~ punter_player_name,
                        T ~ ""))



ggplot(CollegePunters, aes(x = TwentyPerc, y = NonReturnWPA))+
  geom_point(aes(size = Punts, color = ID))+
  geom_smooth(method = "lm", se = F)+
  theme_reach() +
  ggrepel::geom_text_repel(aes(label = ID), size = 4, box.padding = 0.2,
                           force = 10, max.overlaps = Inf,
                           min.segment.length = 1)+
  scale_y_reverse()+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5),
                     labels = scales::percent_format(accuracy = 1))+
  labs(
    y= "Mean Win Probability Added on Punts without Returns",
    x= "Percent of Punts inside the 20 yard line",
    title= "Career Punting Values for College Punters",
    subtitle = "WPA (From Offense's Perspective) Without Returns has the highest Correlation with Pythagorean Wins 
    and Punts within the 20 is a common metric for gauging punters
    Minimum of 40 Career punts, 2021 Being Last Season of Punting, Size is # of Punts", 
    caption = "@SethDataScience"
    ) +
  theme_reach() +
ggsave("CollegePunters2021.png", height =8, width = 13, units = "in", dpi = 350)
