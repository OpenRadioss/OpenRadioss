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


#ifndef MU_DIMENSION_H
#define MU_DIMENSION_H

#include <UTILS/mv_string.h>
#include <UTILS/mv_stl_various.h>
#include <MUNITS/mu_quantity.h>
#include <MUNITS/mu_unit.h>
#include <HCDI/hcdi.h>

/**@name Dimension */
//@{

/// Dimension  
enum MuDimension_s {
  /** */ UDI_UNKNOWN                       =  0,
  /** */ UDI_DIMENSIONLESS                 =  1,
  /** */ UDI_MASS                          =  2,
  /** */ UDI_LENGTH                        =  3,
  /** */ UDI_TIME                          =  4,
  /** */ UDI_ELECTRIC_CURRENT              =  5,
  /** */ UDI_THERMODYNAMIC_TEMPERATURE     =  6,
  /** */ UDI_LUMINOUS_INTENSITY            =  7,
  /** */ UDI_AMOUNT_OF_SUBSTANCE           =  8,
  /** */ UDI_PLANE_ANGLE                   =  9,
  /** */ UDI_SOLID_ANGLE                   = 10,
  /** */ UDI_STRAIN_RATE                   = 11,
  /** */ UDI_FREQUENCY                     = 12,
  /** */ UDI_AREA                          = 13,
  /** */ UDI_VOLUME                        = 14,
  /** */ UDI_VOLUMIC_MASS                  = 15,
  /** */ UDI_SPEED                         = 16,
  /** */ UDI_ACCELERATION                  = 17,
  /** */ UDI_FORCE                         = 18,
  /** */ UDI_PRESSURE                      = 19,
  /** */ UDI_ENERGY                        = 20,
//JL_198_13_4_6 beg
  /** */ UDI_TRANSLATION_STIFFNESS         = 21,
  /** */ UDI_ROTATION_STIFFNESS            = 22, 
  /** */ UDI_TORSION_DAMPING               = 23, 
  /** */ UDI_SPECIFIC_HEAT_PER_UNIT_VOLUME = 24, 
  /** */ UDI_INERTIA                       = 25, 
  /** */ UDI_MASS_FLOW                     = 26, 
  /** */ UDI_ANGULAR_SPEED                 = 27,
  /** */ UDI_SURFACIC_MASS                 = 28                    
};

/// Dimension
typedef enum MuDimension_s MuDimension_e;

/// Dimensions
typedef set<MuDimension_e> MuDimensions_t;

/**@name Init & close */
//@{
/// Initializing dimension toolbox
void MU_init_dimensions(const string &fullname);
/// Closing dimension toolbox
void MU_close_dimensions();
//@}

/**@name Dimensions */
//@{
/// Getting dimension from keyword
HC_DATA_DLL_API MuDimension_e  MU_get_dimension(const string &keyword);
/// Getting keyword from dimension
HC_DATA_DLL_API const string  &MU_get_dimension(MuDimension_e dimension);
/// Getting the first dimension
MuDimension_e  MU_get_dimension_first();
/// Getting the last dimension
MuDimension_e  MU_get_dimension_last();
/// Getting the end dimension (last+1)
MuDimension_e  MU_get_dimension_end();
//@}

/**@name Quantities */
//@{
/// Getting the quantity of a dimension
HC_DATA_DLL_API const MuQuantity_t &MU_get_quantity(MuDimension_e dimension);
/// Getting the dimensions of a quantity
MuDimensions_t *MU_get_dimensions(const MuQuantity_t &quantity,MuDimensions_t *dimensions_p=NULL);
/// Getting the dimension of an elementary quantity
HC_DATA_DLL_API MuDimension_e MU_get_dimension(MuEQuantity_e equantity);
/// Check custom dimension information, whether number of arg matches with defined in units.cfg file
int MU_check_custom_dimension_info(MuDimension_e dim, vector<string>& args);
//@}

/**@name Units */
//@{
/// Getting the name of a unit, for a given dimension
string   MU_search_unit(MuDimension_e dimension,const MuUnit_t &unit);
/// Getting the unit of given formula, for a given dimension
HC_DATA_DLL_API MuUnit_t MU_search_unit(MuDimension_e dimension,const string &formula);

HC_DATA_DLL_API bool     MU_is_valid_unit(MuDimension_e dimension,const string &formula);
/// Getting the SI unit of a dimension
string   MU_get_SI_unit(MuDimension_e dimension);
//@}


//@}


#endif //MV_DIMENSION_H



