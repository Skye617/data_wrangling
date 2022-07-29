library(ggplot2)

library(lavaan)

testData = HolzingerSwineford1939

ggplot(testData, aes(x7, ageyr)) +
  geom_point()

ggplot(testData, aes(x7, ageyr)) +
  geom_point(aes(color = as.factor(grade)), alpha = .75)


########### BUILD MY OWN FUNCTION ###########

pass_on_love = function(x){
  cat(x, "will do great!")
}

pass_on_love("Angel")

########### DT - TABLE ###########
library(ggplot2)

library(lavaan)

testData = HolzingerSwineford1939

library(dplyr)
library(DT)
datatable(testData)

lm(x7 ~ ageyr + school, data = testData) %>% 
  broom::tidy() %>% 
  mutate_if(is.numeric, round, 4) %>% 
  datatable()

########### Connect Database ###########

install.packages("dbplyr")

install.packages("RSQLite")

install.packages("DBI")

download.file(url = "http://jplalor.github.io/files/albumsales.db",
              destfile = "albumsales.db", mode = "wb")
library(dbplyr)
library(dplyr)
library(RSQLite)
library(DBI)

db <- DBI::dbConnect(RSQLite::SQLite(), "albumsales.db")

src_dbi(db) # Use dplyr verbs with a remote database table

tbl(db, sql("SELECT * FROM albums"))

tbl(db, sql("SELECT Title, ArtistId  FROM albums"))

albums <- tbl(db, "albums") 
# why is the result a list instead of a table? 
albums %>%
  select(Title, ArtistId)

tbl(db, sql("SELECT * FROM invoice_items"))

invoice_items <- tbl(db, "invoice_items")

invoice_items %>%
  group_by(InvoiceId) %>%
  summarize(
    num_items = n(),
    total_price = sum(UnitPrice * Quantity)
  ) %>%
  filter(num_items > 10) 

