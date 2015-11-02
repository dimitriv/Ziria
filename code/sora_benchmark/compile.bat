@REM Copyright (c) Microsoft Corporation
@REM All rights reserved.
@REM 
@REM Licensed under the Apache License, Version 2.0 (the ""License""); you
@REM may not use this file except in compliance with the License. You may
@REM obtain a copy of the License at
@REM 
@REM http://www.apache.org/licenses/LICENSE-2.0
@REM 
@REM THIS CODE IS PROVIDED ON AN *AS IS* BASIS, WITHOUT WARRANTIES OR
@REM CONDITIONS OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING WITHOUT
@REM LIMITATION ANY IMPLIED WARRANTIES OR CONDITIONS OF TITLE, FITNESS FOR
@REM A PARTICULAR PURPOSE, MERCHANTABLITY OR NON-INFRINGEMENT.
@REM 
@REM See the Apache Version 2.0 License for specific language governing
@REM permissions and limitations under the License.


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
