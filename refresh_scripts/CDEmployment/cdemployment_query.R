library(RODBC)

con <- odbcConnect("SOAR")
query <- "SELECT [Client Id], [Last Name], [First Name], [Chemical Dependence] FROM [Clients] WHERE [Chemical Dependence] = -1"
query2 <- "SELECT [Client Id], [Hire Date], [Termination Date] FROM [Jobs]"


clients <- sqlQuery(con, query)
employment <- sqlQuery(con, query2)

employment['Hire Date'] <- lapply(employment["Hire Date"], as.character)
employment['Termination Date'] <- lapply(employment["Termination Date"], as.character)
merged <- merge(clients, employment, by.x = "Client Id", by.y = "Client Id", all.x = T, all.y = F)

write.csv(merged, "cd.csv")

q("no")