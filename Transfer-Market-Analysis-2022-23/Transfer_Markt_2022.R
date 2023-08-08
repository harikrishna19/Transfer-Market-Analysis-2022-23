# Work on transfer markt data 2022 Big 5 European Leagues

library(worldfootballR)
library(tidyverse)
library(shiny)
library(reactable)
library(reactablefmtr)
library(rvest)
library(purrr)
library(ggthemes)


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


#Formatting nos to currncy
val<- function(x) {
  dplyr::case_when(
    x < 1e3 ~ as.character(x),
    x < 1e6 ~ paste0(as.character(x/1e3), "K"),
    x < 1e9 ~ paste0(as.character(x/1e6), "M"),
    TRUE ~ "To be implemented..."
  )
}

# dat %>% View()

data <- data %>% mutate(
  Transfer_Fee=val(transfer_fee)
)


#Chelsea Signings 2022-23 season
chelsea=data %>% filter(team_name == "Chelsea FC" & transfer_type=="Arrivals" & transfer_fee>0)
chelsea$Transfer_Fee<-as.numeric(sub("M", "", chelsea$Transfer_Fee))

ggplot(data = chelsea, aes(x = reorder(player_name, Transfer_Fee), y = Transfer_Fee)) +
  geom_col(fill = "blue") + # Add text labels to the bars
  coord_flip() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(labels = function(x) paste0(x, "M")) +  # Add "M" to y-axis labels
  theme_wsj() +
  labs(title="Chelsea Football Club Transfer Spend 2022/23 Season",caption ="By Hari Krishna")+xlab("")




# Number of Signings per team in PL this season ---------------------------

data




# Arsenal vs City Title Race ----------------------------------------------

results=worldfootballR::fb_match_results(country = "ENG",season_end_year = 2023,gender = "M",tier = "1st")


#mancity and Arsenal Trand for all 38 weeks

ars_mci=results %>% filter(Home=="Arsenal"|Home=="Manchester City"|Away=="Arsenal"|Away=="Manchester City")


# Creating a new variable using case_when() and mutate() function
ars_mci<-ars_mci %>%
  mutate(
    Match_Result = case_when(
      HomeGoals == AwayGoals ~ "Draw",
      HomeGoals > AwayGoals ~ paste0(Home,  "Win"),
      AwayGoals > HomeGoals ~ paste0(Away,  "Win")
    )
  )


ars_mci<-ars_mci %>% group_by(Wk) %>%   arrange(Wk)

ars_mci %>% ggplot(aes(x=Wk))





library(ggplot2)

library(ggplot2)

# Load the required libraries
library(ggplot2)
library(patchwork)

# Your data
data <- data.frame(
  X = c(1, 1, 2, 2, 3, 3),
  Y = c(3, 3, 1, 1, 0, 0),
  Cat = c("Arsenal Win", "City Win", "Arsenal Draw", "City Draw", "Arsenal Lose", "City Lose")
)

# Separate the data for Arsenal and City
arsenal_data <- data[data$Cat %in% c("Arsenal Win", "Arsenal Draw", "Arsenal Lose"), ]
city_data <- data[data$Cat %in% c("City Win", "City Draw", "City Lose"), ]

# Create a ggplot line graph for Arsenal
gg_arsenal <- ggplot(arsenal_data, aes(x = X, y = Y, color = "Arsenal")) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(1, 2, 3)) +
  scale_y_continuous(breaks = c(0, 1, 3)) +
  labs(x = "Match", y = "Points", color = "Team") +
  scale_color_manual(values = c("Arsenal" = "red")) +
  theme_minimal()

# Create a ggplot line graph for City
gg_city <- ggplot(city_data, aes(x = X, y = Y, color = "City")) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(1, 2, 3)) +
  scale_y_continuous(breaks = c(0, 1, 3)) +
  labs(x = "Match", y = "Points", color = "Team") +
  scale_color_manual(values = c("City" = "blue")) +
  theme_minimal()

# Combine the plots using the patchwork package
combined_plot <- gg_arsenal + gg_city

# Print the combined plot
# 
# 
# 
# 
print(combined_plot)




library(ggplot2)
library(ggplot2)

# Your data
data <- data.frame(
  X = c(1, 1, 2, 2, 3, 3),
  Y = c(3, 3, 1, 1, 0, 0),
  Cat = c("Arsenal Win", "City Win", "Arsenal Draw", "City Draw", "Arsenal Lose", "City Lose")
)


library(ggplot2)

ggplot(data, aes(x = X, y = Y, 
               color = factor(Cat), group = factor(Cat))) + 
  geom_line()












