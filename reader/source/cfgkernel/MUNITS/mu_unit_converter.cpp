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

//

#include <UTILS/win32_utils.h>


#include <UTILS/error.h>


#include "mu_dimension.h"
#include "mu_unit_converter.h"


/* --------- Constructors and destructor --------- */


MuUnitConverter_t::MuUnitConverter_t(const MuUnitMap_t &input_units,
				     const MuUnitMap_t &output_units) :
  myInputUnits(MU_get_dimension_end()),
  myOutputUnits(MU_get_dimension_end())
{
  //MuUnit_t a_none(0,0); 
  //
  MuUnitMap_t a_input_units,a_output_units;
  input_units.getExpanded(&a_input_units);
  output_units.getExpanded(&a_output_units);
  //
  MuDimension_e a_dim_begin = MU_get_dimension_first();
  MuDimension_e a_dim_end   = MU_get_dimension_end();
  for(int a_dim=a_dim_begin;a_dim<a_dim_end;++a_dim) {
    MuDimension_e a_dimension=(MuDimension_e)a_dim;
    
    myInputUnits[a_dimension]  = a_input_units[a_dimension];
    myOutputUnits[a_dimension] = a_output_units[a_dimension];
    /*
    myInputUnits[a_dimension]  = MU_search_unit(a_dimension,a_input_units[a_dimension]);
    myOutputUnits[a_dimension] = MU_search_unit(a_dimension,a_output_units[a_dimension]);
    //
    if(myInputUnits[a_dimension]==a_none) {
      throw MvError_t("ERROR: \"%s\" is not a valid unit for \"%s\"",
		      a_input_units[a_dimension].c_str(),
		      MU_get_dimension(a_dimension).c_str());
    }
    if(myOutputUnits[a_dimension]==a_none) {
      throw MvError_t("ERROR: \"%s\" is not a valid unit for \"%s\"",
		      a_output_units[a_dimension].c_str(),
		      MU_get_dimension(a_dimension).c_str());
    }
    */
    
  }
}



/* --------- Convertion --------- */


double MuUnitConverter_t::convert(MuDimension_e dimension,double value) const {
  return myInputUnits[dimension].convertTo(value,myOutputUnits[dimension]);
}

