install.packages('tidyverse')
library(tidyverse)
library(ggplot2)

######### Summary Tables ##########
# diamonds is a data set that comes packaged with ggplot2
plotDat = aggregate(diamonds$cut, by = list(cut = diamonds$cut), 
                    FUN = length)

PracAgg = aggregate(diamonds$price, 
                    by = list(cut = diamonds$cut, color = diamonds$color), 
                    FUN = mean)

# what's happening here? 
colnames(plotDat)[2] = "n"

plotDat

######### Visual ##########
# ggplot uses +
ggplot(plotDat, aes(x = cut, y = n)) +
  geom_point(aes(size = n)) +
  theme_minimal()

# (Im)Proper Plotting: legend isn't suitable for binomial variables
# R automatically decides color according to data type
ggplot(mtcars, aes(x = wt, y = mpg, color = am)) + 
  geom_point() +
  theme_minimal()

# Proper Plotting
library(dplyr)

mtcars$amFactor = as.factor(mtcars$am) 

ggplot(mtcars, aes(x = wt, y = mpg, color = amFactor)) + 
  geom_point() +
  theme_minimal()

######### Pipe ##########
# follows a more logical thought process
# no intermediate objects between what we have (diamonds) and what we want (plot) 
diamonds %>% 
  group_by(cut) %>% 
  summarize(n = n()) %>% 
  ggplot(., aes(x = cut, y = n)) +
  geom_point(aes(size = n)) +
  theme_minimal()

mtcars %>% 
  # mutate will overwrite when column uses the same name, add when different, delete when = NULL 
  mutate(am = as.factor(am)) %>%  
  ggplot(., aes(x = wt, y = mpg, color = am)) + 
  geom_point() +
  theme_minimal()

######### Data Import ##########
# Delimited Files
# \t is tap
read.table("https://download.bls.gov/pub/time.series/ce/ce.data.42a.RetailTrade.Employment", 
           header = TRUE, sep = "\t")

read.delim("https://download.bls.gov/pub/time.series/ce/ce.data.42a.RetailTrade.Employment")

# ???Question???Why is quote necessary? 
la <- read.table("https://jplalor.github.io/files/sdcTestSmall.txt", 
           header = TRUE, sep = "^", quote = "")

# ???Question???Why no difference? 
lala <- read.delim("https://jplalor.github.io/files/sdcTestSmall.txt",
           sep = "^", quote = "")

lalala <- read.delim("https://jplalor.github.io/files/sdcTestSmall.txt",
                   sep = "^")

######### Alternative ##########
# notice that we have to load the library before we can use functions in it
library(readr)

readrTest = read_delim("https://download.bls.gov/pub/time.series/ce/ce.data.42a.RetailTrade.Employment", 
                       delim = "\t")

sdc = read_delim("https://www3.nd.edu/~sberry5/data/sdcTest.txt", 
                 delim="^", quote="",
                 col_types="DDDDDDcccnnnDDccccccccnl",
                 locale = locale(date_format = "%m/%d/%y"))

# why are different columns colored differently in the status output?
# https://rdrr.io/github/tidyverse/readr/src/R/col_types.R 

library(data.table)

dtTest = fread("https://download.bls.gov/pub/time.series/ce/ce.data.42a.RetailTrade.Employment", 
               sep = "\t")



musicalInstruments = readLines("https://www3.nd.edu/~sberry5/data/reviews_Musical_Instruments_5.json")

class(musicalInstruments)

typeof(musicalInstruments)
