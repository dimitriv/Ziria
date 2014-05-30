@REM Command line: compile <DIR> [/b|/r] [chk|fre] [x64 WIN7|x86 WXP]
@REM Use chk to get checked compilation (debug) and fre to get free (release)
@REM Use x64 WIN7 to compile for x64 architecture (but DebugPlot will not work)
@REM Use x86 WXP to compile for 

call C:\WinDDK\7600.16385.1\bin\setenv.bat C:\WinDDK\7600.16385.1\ %3 %4 %5

cd %1

if "%2"=="/r" goto Rebuild

:Build
build /z /w
goto exit

:Rebuild
build /c /z /w
goto exit

:exit
exit /b 0
