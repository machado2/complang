@echo off
setlocal

set APP_HOME=%~dp0
set CLASSPATH=%APP_HOME%\.mvn\wrapper\maven-wrapper.jar

java -Dmaven.home="%APP_HOME%" -classpath "%CLASSPATH%" org.apache.maven.wrapper.MavenWrapperMain %*
