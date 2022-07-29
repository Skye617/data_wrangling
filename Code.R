
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyverse)
library(radiant)
library(plotly)

Market_Share_Banks <- read_excel("C:/Alexis learning materials/R/Interterm 1/project/Arrange/Market Share Banks.xls", 
                                 col_types = c("text", "text", "text", 
                                               "numeric", "numeric"))


################ Chart 1: Market Share ################

Market_Share_Banks <- Market_Share_Banks %>% 
  rename("Deposits" = "Total Deposits 2021 ($000)", 
         "Branches" = "Total Active Branches 2021",
         "Bank_Name" = "Parent Company Name") %>% 
  mutate(Market_Share = (Deposits / sum(Deposits))*100) %>% 
  arrange(desc("Deposits")) 


top_10 <-  Market_Share_Banks %>% 
  head(10)

# Market Share Chart for top 10 Banks by Deposits

pct <- lapply(top_10$Market_Share,
              function(x){
                paste(round(x,2), "%", sep = " ")
              })

lbls <- paste(top_10$Bank_Name, pct) 

Market_share_pie <- pie(top_10$Market_Share,labels = lbls, col=rainbow(length(lbls)),
                        main="Bank Market Share (Top 10 = 51%)")

Market_share_pie

# Explore replationship between Branches and Deposits
visualize(
  Market_Share_Banks, 
  xvar = "Branches", 
  yvar = "Deposits", 
  type = "scatter", 
  nrobs = -1, 
  smooth = 0.8, 
  check = "loess", 
  custom = FALSE
)

# Calculating Average Deposit per Branch
# --- Necessary?
Market_Share_Banks <- Market_Share_Banks %>%
  mutate(Average_Deposit = Deposits / Branches)

# Re-ranking the top 10 banks acc. to Average Deposits per Branch
top_10_Average_Deposit <- Market_Share_Banks %>% 
  head(10) %>%
  arrange(desc(Average_Deposit))

# Does Average Deposit have a relationship with Market Share? Do banks with higher Average Deposits
# have a higher market share?
ggplot(top_10_Average_Deposit, aes(x = Bank_Name, y = Average_Deposit, color = Bank_Name, main="Average Deposit")) +
  geom_point() +  
  expand_limits(y = 0) + 
  theme(axis.text.x = element_text(angle = -45, hjust = 0))


################ Chart 2: JP across states ################

# JPM Analysis
JPM_State_Wise <- read_excel("C:/Alexis learning materials/R/Interterm 1/project/Arrange/JPM State-Wise.xls", 
                             col_types = c("text", "text", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric"))
# View(JPM_State_Wise)
# save(JPM_State_Wise, file = "JPM_State_Wise.rda")

JPM_State_Wise$Region[1] = 'East' # Cleaned the data from Northeast to East

JPM_State_Wise <- JPM_State_Wise %>% 
  rename("Deposits" = "Deposits In Market ($000)", 
         "Branches" = "Number of Branches",
         "HH_Income" = "Median Household Income ($) 2021",
         "Total_Population" = "Total Population (Actual) 2021" ) %>% 
  arrange(desc("Deposits"))

result <- regress(
  JPM_State_Wise, 
  rvar = "Deposits In Market ($000)", 
  evar = "Number of Branches", 
  data_filter = "Branches > 20"
)
summary(result)
plot(result, plots = "dashboard", nrobs = -1, custom = FALSE)

JPM_State_Wise <- JPM_State_Wise %>%
  mutate(Average_Deposit = Deposits / Branches)


# Plot for Average Deposit and HH Income for JPM
ggplot(JPM_State_Wise, aes(x = Average_Deposit, y = )) +
  geom_point() +
  geom_abline() +
  expand_limits(y = 0)

library(plotly)
plot <- ggplot(JPM_State_Wise, aes(x = Deposits, y = Total_Population)) +
  geom_point()
ggplotly(plot)

Region_Data <- JPM_State_Wise %>%
  group_by(Region) %>%
  summarise(Deposit = sum(Deposits), Branches = sum(Branches))

ggplot(Region_Data, aes(x = Deposit, y = Branches, color = Region)) +
  geom_point() 
