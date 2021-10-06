library(RODBC)


con <- odbcConnect("SOAR")
query <- 'SELECT "Client Id", "First Name", "Last Name", Gender, "Ethnicity Id", "Education Status", "Birth Date", "Last Employment" FROM Clients'

clients <- sqlQuery(con, query)
query <- 'SELECT * FROM "Education Status"'
education_lov <- sqlQuery(con, query)
query <- 'SELECT * FROM "DD Ethnicity"'
ethnicity_lov <- sqlQuery(con, query)
query <- 'SELECT "Client Id", Wage, "Hire Date", "Job Number", "Termination Date" FROM Jobs'
jobs <- sqlQuery(con, query)
odbcClose(con)

table <- merge(clients, ethnicity_lov, by.x = "Ethnicity Id", by.y = "Ethnic Id", all.x = T)
table <- merge(table, education_lov, by.x = "Education Status", by.y = "Education Completed Id", all.x = T)
#table <- merge(table, education_lov, by.x = "Education Status", by.y = "Education Completed Id", all.x = T)
table <- merge(table, jobs, by.x = "Client Id", by.y = "Client Id", all.x = T)
table <- as.data.frame(table)

write.csv(table,"demographic.csv")
q("no")

