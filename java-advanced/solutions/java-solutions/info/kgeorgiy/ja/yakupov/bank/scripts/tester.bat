@echo off

:start

choice /M "Y - run JUnit, N - run BaseTests directly and return exit code"
set mode=%errorlevel%

IF %mode%==0 (
    cls
    goto start
    )

set initial_dir=%cd%
set tests_repo_name=shared

set script_directory=%~dp0
cd /d %script_directory%

set java_solutions=..\..\..\..\..\..\..\java-solutions
set my_repo=%java_solutions%\..\
set lib_dir=%java_solutions%\..\..\%tests_repo_name%\lib

set class_path="%lib_dir%\*;%my_repo%\bank-build;"
set test_class=info.kgeorgiy.ja.yakupov.bank.BankTests

if %mode%==1 (
    java -cp %class_path% org.junit.runner.JUnitCore %test_class%
    cd /d %initial_dir%
    )

if %mode%==2 (
    java -cp %class_path% %test_class%
    set errlvl=%errorlevel%
    cd /d %initial_dir%
    exit /b %errlvl%
    )
