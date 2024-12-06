@echo off
@setlocal enabledelayedexpansion
pushd %~dp0

set ARG=%~1
if not defined ARG (
    set DAY=0
    for %%f in (src\day*.hs) do (
        call :validate %%~nf
        if !errorlevel! EQU 0 (
            if !INPUT! GEQ !DAY! (
                set DAY=!INPUT!
            )
        )
    )
    if !DAY! LEQ 0 (
        echo No days exist 2>&1
        exit /B 1
    )
    goto :build
)

if /I "%ARG:"=A%" EQU "ALL" (
    for %%f in (src\day*.hs) do (
        call :validate %%~nf
        if !errorlevel! EQU 0 (
            set DAY=!INPUT!
            call :build
        )
    )
    exit /B 0
)

if /I "%ARG:"=A%" EQU "CLEAN" (
    echo Cleaning
    if exist build (
        rmdir /S /Q build
        mkdir build
    )
    exit /B 0
)

:loop

set ARG=%~1
if not defined ARG (
    exit /B 0
)

call :validate %~1
if %errorlevel% NEQ 0 (
    echo Invalid argument 1>&2
    exit /B 1
)
set DAY=%INPUT%

if not exist src\day%DAY%.hs (
    echo Day %DAY% does not exist 1>&2
    exit /B 1
)
:build

if not exist build (
    mkdir build
)
if not exist build\day%DAY% (
    mkdir build\day%DAY%
)
echo Day %DAY%:

ghc src\day%DAY%.hs -odir build\day%DAY% -hidir build\day%DAY% -o build\day%DAY%.exe
if %errorlevel% NEQ 0 (
    exit /B %errorlevel%
)

build\day%DAY%.exe

shift
goto :loop

:validate
set INPUT=%~1
if not defined INPUT (
    exit /B 1
)
if "%INPUT:"=A%" NEQ "%INPUT:"=B%" (
    exit /B 1
)
if /I "%INPUT:~0,3%" EQU "DAY" (
    set INPUT=%INPUT:~3,100%
)
if not defined INPUT (
    exit /B 1
)
set /A INPUT=%INPUT%

if %INPUT% LEQ 0 (
    exit /B 1
)
if %INPUT% GEQ 26 (
    exit /B 1
)
exit /B 0

