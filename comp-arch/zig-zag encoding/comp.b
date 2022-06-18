@echo off
set "path=%path%;%~dp1"
rem echo %path%

if "%~dpnx1" equ "" goto :usage
if "%~x1" equ ".class" goto :run
if not "%~x1" equ ".java" goto :usage

echo.
echo Processing file: %~nx1

:compile
if not exist "%~dpnx1" echo File doesn't exist && goto :eof
if exist "%~dpn1.class" goto recompile

echo Compiling...
echo.

javac -encoding utf8 -Xlint:unchecked -d "%~dp1\" -cp "%~dp1\" "%~dpnx1"
if not %ERRORLEVEL% equ 0 echo. && pause && goto :eof
choice /M "Run compiled file" /C yn
if %errorlevel% equ 1 goto :run
goto :eof

:recompile
choice /M "Recompile, start, cancel" /C rsc
if %errorlevel% equ 1 del /F /Q "%~dpn1.class" && goto :compile
if %errorlevel% equ 2 goto :run
goto :eof

:run
cls
if exist "%~dp1\_args.txt" set /p args="" < "%~dp1\_args.txt"
if defined args (if exist "%~dp1\_in.txt" (java -cp "%~dp1\" "%~n1" %args% < "%~dp1\_in.txt"
) else java -cp "%~dp1\" "%~n1" %args%) else (if exist "%~dp1\_in.txt" (java -cp "%~dp1\" "%~n1" < "%~dp1\_in.txt"
) else java -cp "%~dp1\" "%~n1")
echo.
pause
goto :eof

:usage
echo Usage: comp.bat file.java
echo. %*
pause