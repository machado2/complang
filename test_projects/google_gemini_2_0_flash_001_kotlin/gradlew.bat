@echo off
setlocal

set GRADLE_USER_HOME=%USERPROFILE%/.gradle

if "%JAVA_HOME%"=="" (
  echo Getting JAVA_HOME...
  for /f "delims=" %%a in ('"%JAVA_HOME%"') do (
    set JAVA_HOME=%%a
  )
)

call gradle %*

endlocal
