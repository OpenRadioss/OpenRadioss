/*Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2025 Altair Engineering Inc.
//Copyright>
//Copyright>    This program is free software: you can redistribute it and/or modify
//Copyright>    it under the terms of the GNU Affero General Public License as published by
//Copyright>    the Free Software Foundation, either version 3 of the License, or
//Copyright>    (at your option) any later version.
//Copyright>
//Copyright>    This program is distributed in the hope that it will be useful,
//Copyright>    but WITHOUT ANY WARRANTY; without even the implied warranty of
//Copyright>    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//Copyright>    GNU Affero General Public License for more details.
//Copyright>
//Copyright>    You should have received a copy of the GNU Affero General Public License
//Copyright>    along with this program.  If not, see <https://www.gnu.org/licenses/>.
//Copyright>
//Copyright>
//Copyright>    Commercial Alternative: Altair Radioss Software
//Copyright>
//Copyright>    As an alternative to this open-source version, Altair also offers Altair Radioss
//Copyright>    software under a commercial license.  Contact Altair to discuss further if the
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.*/

#include "GlobalModelSDI.h"

#include <stdio.h>
#include <dll_settings.h>

using namespace std;

extern "C" 
{

CDECL void cpp_model_open_file_(char *fileName,int *s_fileName)
{
     GlobalModelOpenFile(fileName,s_fileName);
}

CDECL void CPP_MODEL_OPEN_FILE(char *fileName,int *s_fileName)
{cpp_model_open_file_ (fileName,s_fileName);}

CDECL void cpp_model_open_file__(char *fileName,int *s_fileName)
{cpp_model_open_file_ (fileName,s_fileName);}

CDECL void cpp_model_open_file(char *fileName,int *s_fileName)
{cpp_model_open_file_ (fileName,s_fileName);}


}
