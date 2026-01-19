@echo off
REM Zed Editor Setup Script for Windows
REM This script sets up the Zed development environment

setlocal enabledelayedexpansion

echo.
echo ==========================================
echo Zed Editor - Windows Setup
echo ==========================================
echo.

REM Check if Rust is installed
where cargo >nul 2>nul
if %errorlevel% neq 0 (
    echo Error: Rust is not installed or not in PATH
    echo Please install Rust from https://rustup.rs/
    pause
    exit /b 1
)

echo Rust installation found.
echo.

REM Navigate to workspace
cd /d "%~dp0"

echo Installing dependencies...
cargo build --release

if %errorlevel% neq 0 (
    echo Error: Failed to build Zed
    pause
    exit /b 1
)

echo.
echo ==========================================
echo Setup completed successfully!
echo ==========================================
echo.
echo To run Zed, execute: cargo run --release
echo.
pause
