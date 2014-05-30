/* 
   Copyright (c) Microsoft Corporation
   All rights reserved. 

   Licensed under the Apache License, Version 2.0 (the ""License""); you
   may not use this file except in compliance with the License. You may
   obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   THIS CODE IS PROVIDED ON AN *AS IS* BASIS, WITHOUT WARRANTIES OR
   CONDITIONS OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING WITHOUT
   LIMITATION ANY IMPLIED WARRANTIES OR CONDITIONS OF TITLE, FITNESS FOR
   A PARTICULAR PURPOSE, MERCHANTABLITY OR NON-INFRINGEMENT.

   See the Apache Version 2.0 License for specific language governing
   permissions and limitations under the License.
*/
#include "sora_ext_visual.h"


static bool DbgPlotInited = false;


FINL void initDbgPlot()
{
	if (!DbgPlotInited)
	{
		DebugPlotInit();
		DbgPlotInited = true;
	}
}

int32 __ext_dbgplot_line(int16 item)
{ 
#ifdef BUILDARCHX86
	initDbgPlot();
	PlotLine("Line", (int*)(&item), 1);
	return 0;
#else
	printf("DbgPlot not supported in X64 mode.\n");
	exit(1);
#endif
}

int32 __ext_dbgplot_spectrum(int16 item)
{ 
#ifdef BUILDARCHX86
	initDbgPlot();
	PlotSpectrum("Spectrum", (int*)(&item), 1);
	return 0;
#else
	printf("DbgPlot not supported in X64 mode.\n");
	exit(1);
#endif
}

int32 __ext_dbgplot_dots(complex16 *data, int len, int toPlot)
{ 
#ifdef BUILDARCHX86
	initDbgPlot();
	PlotDots("Dots", (COMPLEX16 *)data, toPlot);
	return 0;
#else
	printf("DbgPlot not supported in X64 mode.\n");
	exit(1);
#endif
}

