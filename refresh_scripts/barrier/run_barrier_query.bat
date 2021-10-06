@echo off

title barrier dashboard query


cd ..\..\dashboards\barrier

IF EXIST barrier.csv DEL /F barrier.csv

"..\..\R\R-4.0.3\bin\Rscript.exe" "..\..\refresh_scripts\barrier\barrier_query.R"
attrib +R  barrier.csv
echo barrier dash query complete

