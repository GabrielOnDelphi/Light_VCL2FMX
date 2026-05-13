REM TEMPLATE: Update Project1 variable below with actual .dproj path

@echo off
setlocal enabledelayedexpansion
prompt $p
cls

echo Hint: Build without TESTINSIGHT to enable console output

REM NOTE TO CLAUDE! Do not kill the program! Beep me instead!
REM NOTE TO CLAUDE! When compiling put the dcus in Win32_Debug folder
REM NOTE TO CLAUDE! Documentatio for MsBuild for Delphi: https://docwiki.embarcadero.com/RADStudio/Athens/en/Building_a_Project_Using_an_MSBuild_Command

call "c:\Delphi\Delphi 13\bin\rsvars.bat"

set "MSBuild=c:\Windows\Microsoft.NET\Framework64\v4.0.30319\MSBuild.exe"

REM Log goes to global Temp folder (not project dir). Use a project-unique filename to avoid collisions.
set "LogFile=c:\AI\Claude Code\Temp\Light_VCL2FMX_build.log"

REM Single project. For multi-package builds, see BuildPackage.cmd template.
set "Project1=C:\Projects\FMX\VCL to FMX converter\Light_VCL2FMX.dproj"

if not exist "!Project1!" (
    echo.
    echo ERROR: Project file not found: !Project1!
    echo.
    echo Fix: Edit this script and set Project1 variable to your actual .dproj file path
    echo Example: set "Project1=C:\Projects\MyApp\MyApp.dproj"
    echo.
    exit /b 1
)

echo Build started %time% > !LogFile!
echo.
echo Compiling !Project1! ...
echo === !Project1! === >> !LogFile!

"!MSBuild!" "!Project1!" /t:Clean;Build /p:platform=Win32 /p:Config=Debug >> !LogFile! 2>&1

if errorlevel 1 (
    echo Exit code: !errorlevel! >> !LogFile!
    echo.
    echo BUILD FAILED
    echo See !LogFile! for details
    exit /b 1
)

echo Build finished %time% >> !LogFile!
echo Exit code: 0 >> !LogFile!

echo.
echo BUILD OK
