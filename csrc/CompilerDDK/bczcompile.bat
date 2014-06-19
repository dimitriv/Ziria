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

SET tempcurdir=%cd%
REM call C:\WinDDK\7600.16385.1\bin\setenv.bat C:\WinDDK\7600.16385.1\ fre x64 WIN7
call %WINDDK_ROOT%\bin\setenv.bat %WINDDK_ROOT%\ fre x64 WIN7

cd %tempcurdir%
REM build with /c rebuilds everything. But we don't need it anymore so removed
REM build /c /z /w
copy sources-noinline sources

REM We need to clear MAKEFLAGS from make (http://www.cygwin.com/ml/cygwin/1999-10/msg00354.html)
set MAKEFLAGS=

build /z /w

if %errorlevel% NEQ 0 (
set errorlev=1
del target\amd64\compilerddk.exe
) else (
set errorlev=0
)
exit /B %errorlev%

REM target\amd64\compilerddk.exe %1 %2 %3 %4 %5 %6 %7 %8
