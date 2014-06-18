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
call "C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\vcvarsall.bat" x86
cd %tempcurdir%

REM We need to clear MAKEFLAGS from make (http://www.cygwin.com/ml/cygwin/1999-10/msg00354.html)
set MAKEFLAGS=

msbuild 

if %errorlevel% NEQ 0 (
set errorlev=1
del Debug\CompilerVS13.exe 
) else (
set errorlev=0
)
exit /B %errorlev%

