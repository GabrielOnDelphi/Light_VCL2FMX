@echo off
setlocal enabledelayedexpansion
prompt $p
cls

REM Build script for Light_VCL2FMX_CLI (sibling of Light_VCL2FMX GUI).

call "c:\Delphi\Delphi 13\bin\rsvars.bat"

set "MSBuild=c:\Windows\Microsoft.NET\Framework64\v4.0.30319\MSBuild.exe"
set "LogFile=c:\AI\Claude Code\Temp\Light_VCL2FMX_CLI_build.log"
set "Project1=C:\Projects\FMX\VCL to FMX converter\Light_VCL2FMX_CLI.dproj"

if not exist "!Project1!" (
    echo ERROR: Project file not found: !Project1!
    exit /b 1
)

echo Build started %time% > !LogFile!
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
