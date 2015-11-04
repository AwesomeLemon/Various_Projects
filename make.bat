@echo off
if not exist out goto :proceed
ECHO A current build exists.
set /p delBuild=Do you want to overwrite it [y/n]?: 
if %delBuild% == n goto :EOF
rmdir /s /q out
goto :proceed

:proceed
echo Git pulling in progress.
rem git.exe clone --progress -v "https://github.com/AwesomeLemon/Various_Projects" %cd%/builderProject >> log.txt 2>&1
git.exe pull --progress -v "https://github.com/AwesomeLemon/Various_Projects" >> log.txt 2>&1
mkdir out
echo Solution building in progress.
msbuild %cd%\KeyboardTrainer\KeyboardTrainer.sln /p:Configuration=Release /p:OutDir=..\..\out /p:TargetFramework=v4.0 >> log.txt 2>&1
pause