@echo off

set initial_dir=%cd%
set tests_repo_name=java-advanced-2022

rem \solutions\scripts\
set script_directory=%~dp0
cd /d %script_directory%

set java_solutions=..\java-solutions
set my_repo=..
set tests_repo=..\..\%tests_repo_name%

set docs="https://docs.oracle.com/en/java/javase/17/docs/api/"
set cp="%tests_repo%\artifacts;%tests_repo%\lib;"
set dependency_package="%tests_repo%\modules\info.kgeorgiy.java.advanced.implementor"
set dependency_dir=%dependency_package%\info\kgeorgiy\java\advanced\implementor
set out="%my_repo%\javadoc"

rem remove old javadocs
rmdir /s /q "%out%" 2>nul

rem create javadoc folder again
mkdir "%out%"

set args_part1=-d "%out%" -link "%docs%" -author -private
set args_part2=%java_solutions%\info\kgeorgiy\ja\yakupov\implementor\*.java
set args_part3="%dependency_dir%\package-info.java"
set args_part4="%dependency_dir%\Impler.java" "%dependency_dir%\ImplerException.java" "%dependency_dir%\JarImpler.java"

javadoc %args_part1% %args_part2% %args_part3% %args_part4%

cd /d %initial_dir%
