library(shiny)
library(worldfootballR)
library(tidyverse)


team_url<-tm_league_team_urls("England", 2022)

team_transfers<-tm_team_transfers(team_url, transfer_window = "all")



th<-team_transfers %>% filter(team_name=="Tottenham Hotspur"&transfer_type=="Arrivals")

  th %>% filter(transfer_fee>0) %>% ggplot(aes(player_name,transfer_fee))+geom_col(fill="green")+coord_flip()
