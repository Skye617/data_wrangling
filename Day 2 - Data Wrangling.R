# SELECT columns: value, position, boolean


library(lavaan)

testData = HolzingerSwineford1939

names(testData)

########## SELECT & VISUAL ###########

library(dplyr)

practice1 = haven::read_dta(file = "https://www3.nd.edu/~sberry5/data/stataExample.dta")

practice1 %>% 
  select(starts_with("lvi"),
         starts_with("effect"),
         starts_with("leader"),
         starts_with("cred")
         ) %>% 
  ##  names(.) view column names
  select(starts_with("lvi")) %>% 
  cor(use = "complete.obs") %>% 
  corrplot()
 
library(readr)
sdc = read_delim("https://www3.nd.edu/~sberry5/data/sdcTest.txt", 
                 delim="^", quote="",
                 col_types="DDDDDDcccnnnDDccccccccnl",
                 locale = locale(date_format = "%m/%d/%y"))
  
practice1 %>% 
  select(starts_with("lvi"),
         starts_with("effect"),
         starts_with("leader"),
         starts_with("cred"),
         "Rater", "Gender"
  ) %>% 
  filter(Gender == 0, Rater == 0) %>% 
  summary()

########## RESHAPE -- Base ###########

library(ggplot2)
library(dplyr)

data("starwars")

as.data.frame(starwars) %>% 
  filter(species == "Human" & grepl("(Skywalker)|(Rey)|(Vader)|(Kylo)", .$name)) %>%
  # ??????grepl????????????????????????
  select(name, height, mass) %>% 
  reshape(., idvar = "name", v.names = "values", varying = list(2:3), 
          times = c("height", "mass"), direction = "long") %>% 
  ggplot(., aes(x = name, y = values, color = time)) + 
  geom_point(size = 3.5) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal()

########## RESHAPE -- reshape ###########

# install.packages("reshape")

starwars %>% 
  as.data.frame() %>% 
  filter(species == "Human" & 
           grepl("(Skywalker)|(Rey)|(Vader)|(Kylo)", 
                 .$name)) %>% 
  select(name, height, mass) %>% 
  reshape::melt.data.frame(., id.vars = "name", 
                           measure.vars = 2:3, 
                           variable_name = "type", na.rm = TRUE) %>% 
  ggplot(., aes(x = name, y = value, color = type)) + 
  geom_point(size = 3.5) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal()

########## RESHAPE -- reshape2 ###########

starwars %>% 
  filter(species == "Human" & grepl("(Skywalker)|(Rey)|(Vader)|(Kylo)", .$name)) %>% 
  select(name, height, mass) %>% 
  reshape2::melt(., id.vars = "name", 
                 measure.vars = 2:3, variable.name = "type", 
                 value.name = "value", na.rm = TRUE) %>% 
  ggplot(., aes(x = name, y = value, color = type)) + 
  geom_point(size = 3.5) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal()

########## RESHAPE -- tdyr pivot ###########

starwarsLong = starwars %>% 
  filter(species == "Human" & grepl("(Skywalker)|(Rey)|(Vader)|(Kylo)", .$name)) %>% 
  select(name, height, mass) %>% 
  tidyr::pivot_longer(., names_to="type", values_to="value", -name)

starwarsLong %>% 
  tidyr::pivot_wider(., names_from = type, values_from=value)

########## RESHAPE -- merging ###########

merge1 = haven::read_dta("https://www3.nd.edu/~sberry5/data/merge1Company.dta")

merge1 %>% 
  names(.)

sasExample = haven::read_sas("https://www3.nd.edu/~sberry5/data/wciklink_gvkey.sas7bdat")

leftTestEqual = left_join(merge1, sasExample, by = c("gvkey", 
                                                     "coname", 
                                                     "datadate" = "DATADATE1"))

innerTest = inner_join(merge1, sasExample, by = c("gvkey"))
innerTest

semiTest = semi_join(merge1, sasExample, by = c("gvkey"))
semiTest

antiTest = anti_join(merge1, sasExample, by = c("gvkey"))
