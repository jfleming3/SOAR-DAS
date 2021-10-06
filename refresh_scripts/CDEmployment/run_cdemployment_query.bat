@echo off

title cd dashboard query


cd ..\..\dashboards\CD_Employment

IF EXIST cd.csv DEL /F cd.csv

"..\..\R\R-4.0.3\bin\Rscript.exe" "..\..\refresh_scripts\CDEmployment\cdemployment_query.R"
attrib +R  cd.csv
echo demographic dash query complete

