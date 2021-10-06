@echo off

title Task Scheduling

echo Scheduling tasks

schtasks /create /sc daily /tn "SDAS Demographic Dashboard Database Query" /tr "demographic\run_demographic_query.bat" /st 02:00

schtasks /create /sc daily /tn "SDAS Employment Dashboard Database Query" /tr "employment\run_employment_query.bat" /st 02:00

schtasks /create /sc daily /tn "SDAS Main-Menu Intake Database Query" /tr "main-menu\run_main-menu_query.bat" /st 02:00

schtasks /create /sc daily /tn "SDAS CDEmployment Dashboard Database Query" /tr "CDEmployment\run_cdemployment_query.bat" /st 02:00

schtasks /create /sc daily /tn "SDAS Barrier Dashboard Database Query" /tr "barrier\run_barrier_query.bat" /st 02:00

echo done! Open windows task scheduler to change refresh times from default: daily 02:00
timeout /t 30