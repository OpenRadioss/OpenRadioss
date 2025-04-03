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
#ifndef MV_ATTRIBUTE_EXPRESSION_H
#define MV_ATTRIBUTE_EXPRESSION_H

#include "mv_comparator.h"
#include "mv_expression.h"


/// Attribute expression
class MvAttributeExpression_t : public MvExpression_t {

public: /** @name Constructors and destructors */
  //@{
  /// Constructor
  MvAttributeExpression_t(expression_t *expr_p,bool do_delete=true);
  /// Constructor
  MvAttributeExpression_t(const MvDescriptor_t *descr_p,
			  int ikeyword,const string &comparator,const string &value,
			  bool do_delete=true);
  /// Destructor
  virtual ~MvAttributeExpression_t();
  //@}

public:  // Public hidden functions 
  ostream &display(ostream &os,const MvDescriptor_t &descr) const;


//private: // Private functions
//  void setValue(const MvDescriptor_t *descr_p,const string &value);
private:
/*
  int            myIKeyword;
  int            myIndex;
  MvComparator_e myComparator;
  bool           myIsString;
  union {
    int          myIntValue;
    double       myFloatValue;
    char        *myStringValue;
  };
*/


};

#endif //MV_ATTRIBUTE_EXPRESSION_H




