@echo off

set prog=%1
set /a total_ok=0
set /a total=0

for %%f in (good\*) do (
	echo | set /p=%%f...
	%prog% %%f > nul
	if errorlevel 1 (
		echo ERROR
	) else (
		echo OK
		set /a total_ok+=1
	)
	set /a total+=1
)

for %%f in (bad\*) do (
	echo | set /p=%%f...
	%prog% %%f > nul
	if errorlevel 1 (
		echo OK
		set /a total_ok+=1
	) else (
		echo ERROR
	)
	set /a total+=1
)

echo TOTAL: %total_ok% / %total%

