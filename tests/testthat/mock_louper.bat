@echo off

echo "-----------RUNNING MOCK LOUPER.BAT EXECUTABLE--------------"

setlocal enabledelayedexpansion

set "force=false"

REM Check if the correct number of arguments is provided
if "%~1" neq "create" (
    echo Error: First argument must be 'create'
    goto :eof
)
shift

REM Parse and validate the input and output options
set "input="
set "output="

:parse_args
if "%~1" equ "" goto :check_paths

if /i "%~1" equ "--input" (
    set "input=%~2"
    shift
    shift
    goto :parse_args
)

if /i "%~1" equ "--output" (
    set "output=%~2"
    shift
    shift
    goto :parse_args
)

if /i "%~1" equ "--force" (
    set "force=true"
    shift
    goto :parse_args
)

echo Error: Unknown option '%1'
goto :eof

:check_paths
REM Check if input and output paths are provided
if not defined input (
    echo Error: --input option must be provided
    goto :eof
)

if not defined output (
    echo Error: --output option must be provided
    goto :eof
)

REM Additional processing with the arguments can be added here

echo Action: create
echo Input path: !input!
echo Output path: !output!
echo Force: !force!

:end
echo "-----------------------------------------------------------"
