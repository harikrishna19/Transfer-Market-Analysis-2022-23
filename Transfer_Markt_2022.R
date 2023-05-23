# Work on transfer markt data 2022 Big 5 European Leagues

library(worldfootballR)
library(tidyverse)
library(shiny)
library(reactable)
library(reactablefmtr)
library(rvest)
library(purrr)



# Fetching Team URLS ------------------------------------------------------
fetch_team_urls <- function(country,year) {
  tm_urls<-list()
   for(i in 1:length(country)){
     print(i)
  tm_urls[[i]]<-tm_league_team_urls(country_name = country[i],start_year =year) %>% as.data.frame()
  }
  return(tm_urls)
}
tm_urls<-fetch_team_urls(c("England","Italy","Spain","Germany","France"),year=2022)
tm_urls<-do.call(rbind,tm_urls)
colnames(tm_urls)[1]<-"team_urls"



# Transfer Activity-Top 5 European Leagues --------------------------------


# English Premier League --------------------------------------------------

# Transfer DataSet --------------------------------------------------------
euro_league_Transfers<-list()
for(i in 1:length(tm_urls$team_urls)){
print(i)
euro_league_Transfers[[i]] <- tm_team_transfers(team_url = tm_urls$team_urls[i], transfer_window = "all")
}

euro_league_Transfers<-do.call(rbind,euro_league_Transfers)







# Club Image URL ----------------------------------------------------------

club_img<-list()
for(i in 1:nrow(tm_urls)){
  print(i)
  club_img[[i]]=read_html(tm_urls$team_urls[i]) %>% html_elements("img") %>% html_attr("src") %>% .[[22]]
}
club_img<-club_img %>% unlist()

tm_urls$club_img_url<-club_img

new_list<-list()
for(i in 1:nrow(tm_urls)){
  new_list[[i]]<- gsub("tiny","head",tm_urls$club_img_url[i])
}
new_list<-new_list %>% unlist()
tm_urls$club_img_url<-new_list





club_name<-list()
for(i in 1:nrow(tm_urls)){
  print(i)
  club_name[[i]]=read_html(tm_urls$team_urls[i]) %>% html_elements("h1") %>% html_text2()
}
club_name<-club_name %>% unlist()
tm_urls$club_name<-club_name

write.csv(euro_league_Transfers,"Transfer_Data.csv")





# Loading Transfer Data ---------------------------------------------------

data=read.csv('Transfer_Data.csv')


colnames(data)









# big5<- euro_league_Transfers %>% 
#   select("player_name",
#          "player_position","transfer_type",
#          "transfer_fee","is_loan",
#          "minutes_played","goals","window",everything())
# 
# 
# reactable(big5 %>% filter(team_name=="Tottenham Hotspur")
#           , pagination = FALSE, highlight = TRUE, height = 500,bordered = T,searchable = T,compact = T,striped = T,
#           theme = reactableTheme(
#             headerStyle = list(
#               "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
#               "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
#               borderColor = "#555"
#             )
#           ),
#           defaultColDef = colDef(
#             header = function(value) gsub(".", " ", value, fixed = TRUE),
#             cell = function(value) format(value, nsmall = 1),
#             align = "center",
#             minWidth = 170,
#             headerStyle = list(background = "#f7f7f8")
#           ),
#           columns = list(
#             #club_img_url = colDef(cell = embed_img(width = 100,height = 100)),
#           #img = colDef(cell = embed_img(width = 100,height = 100)),
#           is_loan = colDef(cell = function(value) {
#             # Render as an X mark or check mark
#             if (value == "FALSE") "\u274c" else "\u2714\ufe0f"
#           }),
#           in_squad = colDef(
#             cell = data_bars(
#               data = big5
#             )
#           )
#           )
# )




