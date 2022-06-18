@echo off

set initial_dir=%cd%

set script_directory=%~dp0
cd /d %script_directory%

set java_solutions=..\..\..\..\..\..\..\java-solutions
set my_repo=%java_solutions%\..\
set lib_dir=%my_repo%\lib
set sourcepath="%java_solutions%\info\kgeorgiy\ja\yakupov\bank"

rem remove old build
rmdir /s /q "%my_repo%\bank-build"

rem create build folder again
mkdir "%my_repo%\bank-build"

javac -d "%my_repo%\bank-build" -encoding "UTF-8" -cp "%lib_dir%\*;" %sourcepath%\*.java %sourcepath%\account\*.java %sourcepath%\bank\*.java %sourcepath%\person\*.java %sourcepath%\utils\*.java

cd /d %initial_dir%