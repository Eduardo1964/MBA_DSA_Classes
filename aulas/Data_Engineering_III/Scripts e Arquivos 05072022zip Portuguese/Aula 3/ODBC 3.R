#Pacotes
library("DBI")
library("odbc")
library("dplyr")
library("dbplyr")
library("RMySQL")

# Realiza uma conexão com o MySql
conn <- dbConnect(
  drv = RMySQL::MySQL(),
  dbname = "world",
  host = "localhost",
  username = "root",
  password = "Mysql_2022") #seu password

dbListTables(conn)
dbListFields(conn,"countrylanguage")


table1 <- tbl(conn, "countrylanguage") %>% select(Percentage)

show_query(table1)


result <- tbl(conn, "countrylanguage") %>% select(CountryCode,Language,Percentage) %>% filter(Percentage> 50)%>% collect()
