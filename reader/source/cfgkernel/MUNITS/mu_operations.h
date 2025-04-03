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


#ifndef MU_OPERATIONS_H
#define MU_OPERATIONS_H


#include <UTILS/mv_stl_various.h>
#include <UTILS/mv_operator_manager.h>
#include <MUNITS/mu_unit.h>
#include <MUNITS/mu_quantity.h>


typedef enum MuOperandType_s {
  MUOPT_UNKNOWN,
  MUOPT_SCALAR,
  MUOPT_QUANTITIES_UNITS,
  MUOPT_LAST
} MuOperandType_e;


class MuScalarOperand_t : public MvOperand_t {
 public:
  inline MuScalarOperand_t(double value=0.) : MvOperand_t(), myValue(value) {}
  virtual inline ~MuScalarOperand_t() {}
 public:
  virtual int getType() const { return MUOPT_SCALAR; }
  virtual string getTitleOperand() const { return ""; }
  virtual inline bool isDeletable() const { return true; }
 public:
  inline double getValue() const { return myValue; }
 private:
  double myValue;
};


class MuQuantitiesUnitsOperand_t : public MvOperand_t {
 public:
  inline MuQuantitiesUnitsOperand_t(int nb_quantities) : MvOperand_t(), myQuantitiesUnits(nb_quantities) {}
  virtual inline ~MuQuantitiesUnitsOperand_t() {}
public:
  virtual int getType() const { return MUOPT_QUANTITIES_UNITS; }
  virtual string getTitleOperand() const { return ""; }
  virtual inline bool isDeletable() const { return true; }
  public:
  inline void setQuantity(int i,const MuQuantity_t &q) { myQuantitiesUnits[i].first=q; }
  inline void setUnit(int i,const MuUnit_t &u)         { myQuantitiesUnits[i].second=u; }
 public:
  inline int getNbQuantities() const { return (int)(myQuantitiesUnits.size()); }
  inline const MuQuantity_t &getQuantity(int i) const { return myQuantitiesUnits[i].first; }
  inline const MuUnit_t     &getUnit(int i)     const { return myQuantitiesUnits[i].second; }
 private:
  typedef pair<MuQuantity_t,MuUnit_t> MyQuantityUnit_t;
  typedef vector<MyQuantityUnit_t>    MyQuantitiesUnits_t;
 private:
  MyQuantitiesUnits_t myQuantitiesUnits;
};


typedef MuQuantity_t (*MuQuantityQuantityFunction_t)(const MuQuantity_t &,const MuQuantity_t &);
typedef MuQuantity_t (*MuQuantityScalarFunction_t)(const MuQuantity_t &,double);
typedef MuUnit_t     (*MuUnitUnitFunction_t)(const MuUnit_t &,const MuUnit_t &);
typedef MuUnit_t     (*MuUnitScalarFunction_t)(const MuUnit_t &,double);


class MuQuantitiesUnitsVsQuantitiesUnitsOperator_t : public MvOperator_t {
 public:
  inline MuQuantitiesUnitsVsQuantitiesUnitsOperator_t(const MuQuantityQuantityFunction_t &qfunc,
						      const MuUnitUnitFunction_t         &ufunc) : 
    MvOperator_t(),
    myQuantityFunc(qfunc),
    myUnitFunc(ufunc)
    {}
public:
  virtual inline int getResultType() const { return MUOPT_QUANTITIES_UNITS; }
  virtual MvOperand_t *evaluate(const MvOperandPtrArray_t &operands,
				bool                       is_deletable=false) const;
 private:
  MuQuantityQuantityFunction_t myQuantityFunc;
  MuUnitUnitFunction_t         myUnitFunc;
};


class MuQuantitiesUnitsVsScalarOperator_t : public MvOperator_t {
 public:
  inline MuQuantitiesUnitsVsScalarOperator_t(const MuQuantityScalarFunction_t &qfunc,
					     const MuUnitScalarFunction_t     &ufunc) : 
    MvOperator_t(),
    myQuantityFunc(qfunc),
    myUnitFunc(ufunc)
    {}
public:
  virtual inline int getResultType() const { return MUOPT_QUANTITIES_UNITS; }
  virtual MvOperand_t *evaluate(const MvOperandPtrArray_t &operands,
				bool                       is_deletable=false) const;
 private:
  MuQuantityScalarFunction_t myQuantityFunc;
  MuUnitScalarFunction_t         myUnitFunc;
};


class MuQuantitiesUnitsOperatorManager_t : public MvOperatorManager_t {
 public:
  MuQuantitiesUnitsOperatorManager_t();
 public:
  void addBinaryOperator(const string                       &skeyword,
			 const MuQuantityQuantityFunction_t &qfunc,
			 const MuUnitUnitFunction_t         &ufunc);
  void addBinaryOperator(const string                     &skeyword,
			 const MuQuantityScalarFunction_t &qfunc,
			 const MuUnitScalarFunction_t     &ufunc);
};


#endif //MU_OPERATIONS_H
