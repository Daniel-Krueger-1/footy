###################################################################################
##Build visualization tool

###################################################################################
########################### install fc.rstats #####################################
devtools::install_github("FCrSTATS/fc.rstats")

###################################################################################


###################################################################################
############################ load libraries #######################################
require(FC.rSTATS)
require(rvest)
require(dplyr)
require(tnet)

eventsDB <- as.tibble(parse_f24("f24-98-2012-429860-eventdetails.xml"))
events <- eventsDB


## add next.player 
events <- eventsDB
edges <- 
  events %>% 
  mutate(receiver_id = lead(player_id, 1)) %>% 
  filter(outcome == 1 & type_id == 1 & team_id == "436") %>% 
  select(id, player_id, receiver_id, team_id, period_id, min, sec, x, y, `140`, `141`) %>%
  rename(x.end = `140`, y.end = `141`) %>% 
  mutate(edge = paste0(player_id, "-", receiver_id)) %>% 
  mutate(alpha.value = case_when(
    x.end < 33.3 ~ 0.5,
    x.end < 66.6 ~ 1,
    TRUE ~ 1.5) 
  ) %>% 
  group_by(edge) %>% 
  summarise(edgeDict = n()) %>% 
  separate(edge, c("player_id", "receiver_id")) %>% 
  mutate(edge = paste0(player_id, "-", receiver_id))


## Create average passing positions for each passer and receiver   
average.passing.positions.per.player <- 
  events %>% 
  filter(outcome == 1 & type_id == 1 & team_id == "436") %>%
  group_by(player_id) %>% 
  summarise(x = mean(x), y = mean(y))

average.passing.positions.per.player

average.passing.positions.per.player.receiver <- 
  average.passing.positions.per.player %>% 
  rename(receiver_id = player_id, xend = x, yend = y)

average.passing.positions.per.player.receiver

## add x,y for both passer and receiver 
edges <- merge(edges, average.passing.positions.per.player, by = "player_id")
edges <- merge(edges, average.passing.positions.per.player.receiver, by = "receiver_id")

create_OPTA_pitch() + 
  geom_segment(data = edges %>% filter(edgeDict >=1), 
               aes(x = x, color = x, y = y, xend = xend, yend = yend, size = edgeDict)) + 
  xlim(c(0,100)) + ylim(c(0,100)) + 
  geom_point(data = average.passing.positions.per.player, aes(x = x, y =y))


