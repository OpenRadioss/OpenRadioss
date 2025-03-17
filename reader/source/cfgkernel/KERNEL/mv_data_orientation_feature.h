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
#ifndef MV_DATA_ORIENTATION_FEATURE_H
#define MV_DATA_ORIENTATION_FEATURE_H

#include "mv_orientation.h"
#include "mv_data_single_feature.h"


/// Data orientation feature
class MvDataOrientationFeature_t {

public: /** @name Constructors & destructor */
//@{
  /// Constructor
  MvDataOrientationFeature_t(MvOrientation_e orientation=ORI_COLUMN,int nb_rows=0,int nb_columns=0);
  /// Destructor
  virtual ~MvDataOrientationFeature_t();
//@}

public: /** @name Accessors */
//@{
  /// Gets the orientation
  inline MvOrientation_e getOrientation() const { return myOrientation; }
  /// Gets the number of rows (ORI_MATRIX)
  inline int             getNbRows()    const { return myNbRows; }
  /// Gets the number of columns (ORI_MATRIX)
  inline int             getNbColumns() const { return myNbColumns; }
//@}

protected: // Datas
  MvOrientation_e myOrientation;
  int             myNbRows,myNbColumns;
};

#endif //MV_DATA_ORIENTATION_FEATURE_H




