@echo off

title employment dashboard query

cd ..\..\dashboards\employment-time"

IF EXIST employment.csv DEL /F employment.csv

"..\..\R\R-4.0.3\bin\Rscript.exe" "..\..\refresh_scripts\employment\employment_query.R"
attrib +R  employment.csv
echo employment dashboard query complete


