library(RODBC)

#con <- odbcConnect("SOAR", rows_at_time = 1)

#con <- odbcConnectAccess()

con <- odbcConnect("SOAR")
query <- 'SELECT "Client Id", "First Name", "Last Name", Gender,"Intake Date", "Employed","Employed Status", "Ethnicity Id", "Education Status", "Birth Date" FROM Clients'
clients <- sqlQuery(con, query)

query <- 'SELECT * FROM "Education Status"'
education_lov <- sqlQuery(con, query)

query <- 'SELECT * FROM "DD Ethnicity"'
ethnicity_lov <- sqlQuery(con, query)


#odbcClose(con)
#con <- odbcConnect("SOAR", believeNRows = FALSE, rows_at_time = 1)

#query <- 'SELECT * FROM "Employment Status"'
#employment_lov <- sqlQuery(con, query)
query <- 'SELECT "Client Id","Job Id","Hire Date", Employer, Wage FROM Jobs'
#query <- 'SELECT * FROM Jobs'
jobs <- sqlQuery(channel = con, query = query)
#jobs <- sql_select(con,c("Client Id","Job Id","Hire Date", "Employer", "Wage"),"Jobs")
odbcClose(con)







table <- merge(clients, ethnicity_lov, by.x = "Ethnicity Id", by.y = "Ethnic Id", all.x = T)
table <- merge(table, education_lov, by.x = "Education Status", by.y = "Education Completed Id", all.x = T)
#table <- merge(table, education_lov, by.x = "Education Status", by.y = "Education Completed Id", all.x = T)
table <- merge(table, jobs, by.x = "Client Id", by.y = "Client Id", all.x = T)

table <- as.data.frame(table)
#table
#colnames(table)


#table <- clients
table$Employed <- ifelse(table$Employed == -1,"Employed","Unemployed")
#table <- table[!is.na(table$Employment.Status),] 


write.csv(table,"employment.csv")


q("no")
