@echo off

set initial_dir=%cd%
set tests_repo_name=java-advanced-2022

rem \solutions\scripts\
set script_directory=%~dp0
cd /d %script_directory%

set java_solutions=..\java-solutions
set my_repo=..
set tests_repo=..\..\%tests_repo_name%

set modules_path="%tests_repo%\artifacts;%tests_repo%\lib;"
set sourcepath="%java_solutions%;"
set package_tree="info\kgeorgiy\ja\yakupov\implementor"

rem remove old build
rmdir /s /q "%my_repo%\implementor-build"

rem create build folder again
mkdir "%my_repo%\implementor-build"

javac -d "%my_repo%\implementor-build" -sourcepath "%sourcepath%" -p "%modules_path%" %java_solutions%\\%package_tree%\Implementor.java

cd "%my_repo%\implementor-build"
jar -c -f "implementor.jar" -m "%script_directory%\MANIFEST.MF" -p "%modules_path%" "."

cd /d %initial_dir%
