@echo off

title demographic dashboard query


cd ..\..\dashboards\demographic-employment

IF EXIST demographic.csv DEL /F demographic.csv

"..\..\R\R-4.0.3\bin\Rscript.exe" "..\..\refresh_scripts\demographic\demographic_query.R"
attrib +R  demographic.csv
echo demographic dash query complete
