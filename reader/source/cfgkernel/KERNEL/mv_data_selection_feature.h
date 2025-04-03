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
#ifndef MV_DATA_SELECTION_FEATURE_H
#define MV_DATA_SELECTION_FEATURE_H

#ifdef __cplusplus

#include "mv_data_feature.h"

#define MvPseudoTypeIKeywordMap_t        void

class MvDescriptor_t;
/// Data Selection feature
class MvDataSelectionFeature_t : public MvDataFeature_t {

public: /** @name Constructors & destructor */
//@{
  /// Constructor
  MvDataSelectionFeature_t(const MvDescriptor_t *descr_p,
                                    int default_ikeyword);
  /// Destructor
  virtual ~MvDataSelectionFeature_t();
//@}

public: /** @name Accessors */
//@{
  /**Returns the default Ikeyword */
  inline int getDefaultIKeyword() const {return p_default_ikeyword;}
  /** Gets the number of possible values of the TYPE attribute.
  This allows to get the possibilities between which the selector can choose. */
  int getTypeNumber() const;
  /** Gets the i-th possible value of the TYPE attribute. */
  int getTypeValue(int i) const;
  /** Gets the ikeyword of the attribute for a possible value of TYPE .*/
  int getTypeAttribute(int type_value) const;
  /** Get the type number from the ikeyword. */
  int getTypefromIkeyword(int ikeyword) const;
  /** Get the TYPE Ikeyword*/
  inline int GetTypeIKeyword()const {return p_type_ikeyword;};
  /**Gets the skeyword of the attribute for a possible value of TYPE */
  //string getTypeAttribute(int type_value);

//@}
private:
    void AddSupportFeatureMap(const MvDescriptor_t *descr_p);

    int p_default_ikeyword;

    int p_type_ikeyword;
    ///store Type vs. IKeyword map
    MvPseudoTypeIKeywordMap_t        *myTypeIKeywordMapPtr;
};

extern "C" {
#endif

#ifndef MvPseudoDataSelectionFeature_t
#define MvPseudoDataSelectionFeature_t void
#endif
/** Get the TYP keyword for the given data selection feature*/
int MV_dataselfeature_get_group_type_ikeyword(const MvPseudoDataSelectionFeature_t *datasel_p);

/** Get the associated ikeyword for the given typevalue*/
int MV_get_group_ikeyword_for_typevalue(const MvPseudoDataSelectionFeature_t *datasel_p,int typevalue);

#ifdef __cplusplus
}
#endif
#endif //MV_DATA_SELECTION_FEATURE_H

