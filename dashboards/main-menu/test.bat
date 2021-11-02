@echo off
title batch
cd C:\SOAR_DAS\dashboards\main-menu
C:\SOAR_DAS\R\R-4.0.3\bin\Rscript.exe C:\SOAR_DAS\dashboards\main-menu\app.R --silent
pause