library(RODBC)

con <- odbcConnect("SOAR")

query <- "SELECT [Intake Date],[Last Name],[First Name] ,Homeless, [Rent Evicted],[Education Status], [Transportation Reliable],[Parenting Time],[Limited English],[Chemical Dependence], [Criminal Record],[Criminal Legal Issues], [Diagnosed Disability], [Mental Health], [Medical Condition],[Employed Status] FROM Clients"
education_status_lov_query <- 'SELECT * FROM "Education Status"'

data <- sqlQuery(con, query)
education_lov <- sqlQuery(con, education_status_lov_query)



odbcClose(con)

table <- merge(data, education_lov, by.x = "Education Status", by.y = "Education Completed Id", all.x = T)
#table

write.csv(table,"barrier.csv")

q("no")