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
#ifndef MV_DRAWABLE_H
#define MV_DRAWABLE_H


#include <UTILS/mv_string.h>
#include <UTILS/mv_stl_various.h>

#include <HCDI/hcdi_drawableinf.h>



/** @name Drawable */
//@{


/// Drawable types
enum MvDrawableType_s {
  /** */ DRT_UNKNOWN,
  /** */ DRT_SCALAR,
  /** */ DRT_SUBDRAWABLE,
  /** */ DRT_WHILE_ZERO,
  /** */ DRT_EVAL,
  /** */ DRT_TIME_STEP,
  /** */ DRT_MAX,
  /** */ DRT_MIN,
  /** */ DRT_VOLUME,      
  /** */ DRT_AREA,        
  /** */ DRT_LAST
}; 
/// Drawable types
typedef enum MvDrawableType_s MvDrawableType_e;

/// Drawable access
enum MvDrawableAccess_s {
  /** */ DRA_UNKNOWN,
  /** */ DRA_PUBLIC,
  /** */ DRA_PRIVATE,
  /** */ DRA_LAST
}; 
/// Drawable access
typedef enum MvDrawableAccess_s MvDrawableAccess_e;


/// Drawable class
class MvDrawable_t {

public:    /** @name Constructors and destructor */
  //@{
  /// Constructor
  inline MvDrawable_t(const string &name,MvDrawableAccess_e access) : myName(name), myAccess(access) {}
  /// Destructor
  virtual inline ~MvDrawable_t() {}
  //@}

public:    /** @name Accessors */
  //@{
  /// Gets the type
  virtual inline MvDrawableType_e  getType()   const { return DRT_UNKNOWN; }
  /// Gets the access (DRA_PUBLIC or DRA_PRIVATE)
  inline MvDrawableAccess_e        getAccess() const { return myAccess; }
  /// Gets the name
  inline const string             &getName()   const { return myName; }
  /// Evaluation on an object
  virtual double evaluate(const hwCFGDrawableInf* hwcfg_draw_inf) const = 0;

  virtual int getIKeyword() const { return 0; }
  //@}
protected: // Data
  string             myName;
  MvDrawableAccess_e myAccess;

};


/// Set of drawables
typedef set<const MvDrawable_t *> MvDrawablePtrSet_t;
/// Set of drawable names
typedef set<string>               MvDrawableNameSet_t;


//@}


#endif //MV_DRAWABLE_H




