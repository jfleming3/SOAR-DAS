@echo off

title main-menu intake query

cd ..\..\dashboards\main-menu"

IF EXIST intake_data.csv DEL /F intake_data.csv

"..\..\R\R-4.0.3\bin\Rscript.exe" "..\..\refresh_scripts\main-menu\main-menu_query.R"
attrib +R  intake_data.csv
echo intake data query complete


