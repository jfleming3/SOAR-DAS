library(RODBC)

con <- odbcConnect("SOAR")

query <- "SELECT [Intake Date] FROM Clients"

data <- sqlQuery(con, query)

odbcClose(con)

write.csv(data, file = "intake_data.csv")

q("no")


