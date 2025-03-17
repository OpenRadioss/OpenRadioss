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
#include <UTILS/win32_utils.h>  

#include "mv_descriptor.h"
#include "mv_data_function_feature.h"




/* --------- Constructors & destructor --------- */



MvDataFunctionFeature_t::MvDataFunctionFeature_t(MvDataFeatureType_e dft,const string &title,
						 int func_ikw,int x_scal_ikw,int y_scal_ikw, int z_scal_ikw) :
  MvDataMultiFeature_t(dft,title,1+(x_scal_ikw==END_ARGS ? 0 : 1)+(y_scal_ikw==END_ARGS ? 0 : 1)),
  myXScaleInd(x_scal_ikw==END_ARGS ? 0 : 1),
  myYScaleInd(y_scal_ikw==END_ARGS ? 0 : (x_scal_ikw==END_ARGS ? 1 : 2)),
  myZScaleInd(z_scal_ikw == END_ARGS ? 0 : (x_scal_ikw == END_ARGS ? 1 : 2)),
  myFuncType(FFTY_FUNCTION), 
  myXCanBeNegative(true),
  myFilter(NULL)
{
  setFeature(0,func_ikw,"");
  if(myXScaleInd>0) setFeature(myXScaleInd,x_scal_ikw,"");
  if(myYScaleInd>0) setFeature(myYScaleInd,y_scal_ikw,"");
  if (myZScaleInd > 0) setFeature(myZScaleInd, z_scal_ikw, "");
}



MvDataFunctionFeature_t::~MvDataFunctionFeature_t()
{
    if (myFilter)
    {
        delete myFilter;
        myFilter = NULL;
    }
}

void MvDataFunctionFeature_t::createFilter(int nb, StringVect_t &attribs, StringVect_t &values, StringVect_t &criterias, StringVect_t &units, StringVect_t& messgs)
{
    if (nb)
    {
        myFilter = new EntityFilter(nb, attribs, values, criterias, units, messgs);
    }
}

