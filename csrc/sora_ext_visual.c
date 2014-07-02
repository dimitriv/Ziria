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
#ifdef WIN32
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

int32 __ext_dbgplot_real_line(int16 *line, int len)
{ 
#ifdef BUILDARCHX86
	int *buf = (int*)malloc(len * sizeof(int));
	if (buf == NULL)
	{
		printf("Memory allocation error!\n");
		exit(1);
	}
	for (int i = 0; i < len; i++)
	{
		buf[i] = (int)line[i];
	}
	initDbgPlot();
	PlotLine("Line", (int*)(buf), len);
	return 0;
#else
	printf("DbgPlot not supported in X64 mode.\n");
	exit(1);
#endif
}

int32 __ext_dbgplot_complex_line(complex16 *line, int len, int16 type)
{
#ifdef BUILDARCHX86
	int *buf = (int*) malloc(len * sizeof(int));
	if (buf == NULL)
	{
		printf("Memory allocation error!\n");
		exit(1);
	}
	for (int i = 0; i < len; i++)
	{
		switch (type) {
		case 1:
			buf[i] = (int)line[i].re;
			break;
		case 2:
			buf[i] = (int)line[i].im;
			break;
		case 3:
			buf[i] = (int)line[i].re * (int)line[i].re + (int)line[i].im * (int)line[i].im;
			break;
		}
	}
	initDbgPlot();
	PlotLine("Line", (int*)(buf), len);
	return 0;
#else
	printf("DbgPlot not supported in X64 mode.\n");
	exit(1);
#endif
}

int32 __ext_dbgplot_spectrum(complex16 *line, int len)
{ 
#ifdef BUILDARCHX86
	int *buf = (int*)malloc(len * sizeof(int));
	if (buf == NULL)
	{
		printf("Memory allocation error!\n");
		exit(1);
	}
	for (int i = 0; i < len; i++)
	{
		buf[i] = (int)line[i].re * (int)line[i].re + (int)line[i].im * (int)line[i].im;
	}
	initDbgPlot();
	PlotSpectrum("Spectrum", buf, len);
	return 0;
#else
	printf("DbgPlot not supported in X64 mode.\n");
	exit(1);
#endif
}

int32 __ext_dbgplot_dots(complex16 *data, int len)
{ 
#ifdef BUILDARCHX86
	initDbgPlot();
	PlotDots("Dots", (COMPLEX16 *)data, len);
	return 0;
#else
	printf("DbgPlot not supported in X64 mode.\n");
	exit(1);
#endif
}

int32 __ext_dbgplot_dot(complex16 data)
{
#ifdef BUILDARCHX86
	initDbgPlot();
	PlotDots("Dots", (COMPLEX16 *)(&data), 1);
	return 0;
#else
	printf("DbgPlot not supported in X64 mode.\n");
	exit(1);
#endif
}

#endif
