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

#include <UTILS/str_utils.h>
#include <UTILS/mv_cstring.h>

#include "mu_dimension.h"
#include "mu_prefixes.h"
#include "mu_operations.h"
#include "mu_units_parser.h"


typedef pair<MuQuantity_t,MuUnit_t> LocQuantityUnit_t;
typedef vector<LocQuantityUnit_t>   LocQuantitiesUnits_t;

static MuQuantityManager_t                *loc_quantity_manager_p;
static MuPrefixes_t                       *loc_prefixes_p;
static MuQuantitiesUnitsOperatorManager_t *loc_operator_manager_p = nullptr;

static const MuQuantityManager_t          &loc_get_quantity_manager();
static const MuPrefixes_t                 &loc_get_prefixes();
static MuQuantitiesUnitsOperatorManager_t &loc_get_operator_manager();

static bool                                loc_is_operator_begin(char op);
static bool                                loc_is_scalar(const string &s);
static string                              loc_get_unit(const string &unit,double power);
static LocQuantitiesUnits_t               *loc_get_quantities_units_from_multipled_basic_unit(const string         &basic_unit,
											      double                multiplyer,
											      LocQuantitiesUnits_t *quantities_units_p=NULL);
static LocQuantitiesUnits_t               *loc_get_quantities_units_from_prefixed_basic_unit(const string         &unit,
											     LocQuantitiesUnits_t *quantities_units_p=NULL);
static MvOperand_t                        *loc_get_operand(const string &formula,int ind0,int *ind_p);
static MuQuantitiesUnitsOperand_t         *loc_evaluate_next_operator(MuQuantitiesUnitsOperand_t *operand_p,
								      const string               &formula,
								      int                         ind0,
								      int                        *ind_p,
								      int                         level);
static MvOperand_t                        *loc_evaluate_formula(const string &formula);
static LocQuantitiesUnits_t               *loc_get_quantities_units(const string         &formula,
								    LocQuantitiesUnits_t *quantities_units_p=NULL);


/* --------- Init & close --------- */

void MU_init_dimensions(const string &fullname) {
  /* Quantity manager */
  if (loc_operator_manager_p)
     return;
  MuUnitsParser_t a_units_parser(fullname);
  loc_quantity_manager_p=a_units_parser.read();
  // Adding UNKNOWN
  MuQuantity_t a_unknown;
  loc_quantity_manager_p->setQuantity(0,"UNKNOWN",a_unknown);
  /* Prefixes */
  loc_prefixes_p=new MuPrefixes_t();
  /* Operator manager */
  loc_operator_manager_p=new MuQuantitiesUnitsOperatorManager_t();
}

void MU_close_dimensions() {
  delete loc_quantity_manager_p;
  loc_quantity_manager_p = NULL;
  delete loc_prefixes_p;
  loc_prefixes_p= NULL;
  delete loc_operator_manager_p;
  loc_operator_manager_p = NULL;
}


/* --------- Names --------- */

MuDimension_e MU_get_dimension(const string &dimension) {
  const MuQuantityManager_t &a_quantity_manager = loc_get_quantity_manager();
  int                        a_dimension        = a_quantity_manager[dimension];
  //
  return a_dimension<0 ? UDI_UNKNOWN : (MuDimension_e)a_dimension;
}

const string &MU_get_dimension(MuDimension_e dimension) {
  const MuQuantityManager_t &a_quantity_manager = loc_get_quantity_manager();
  //
  return a_quantity_manager[dimension];
}

MuDimension_e MU_get_dimension_first() {
  return (MuDimension_e)(UDI_UNKNOWN+1);
}

MuDimension_e MU_get_dimension_last() {
  return (MuDimension_e)(MU_get_dimension_end()-1);
}

MuDimension_e MU_get_dimension_end() {
  const MuQuantityManager_t &a_quantity_manager = loc_get_quantity_manager();
  //
  return (MuDimension_e)(a_quantity_manager.getNbQuantities());
}


/* --------- Quantities --------- */

const MuQuantity_t &MU_get_quantity(MuDimension_e dimension) {
  const MuQuantityManager_t &a_quantity_manager = loc_get_quantity_manager();
  //
  return a_quantity_manager.getQuantity(dimension);
}

MuDimensions_t *MU_get_dimensions(const MuQuantity_t &quantity,MuDimensions_t *dimensions_p) {
  
  const MuQuantityManager_t &a_quantity_manager = loc_get_quantity_manager();
  //
  MuDimensions_t *a_dimensions_p=(dimensions_p==NULL ? new MuDimensions_t() : dimensions_p);
  //
  MuQuantityIndexes_t a_indexes;
  a_quantity_manager.searchQuantities(quantity,&a_indexes);
  //
  MuQuantityIndexes_t::iterator a_it_begin = a_indexes.begin();
  MuQuantityIndexes_t::iterator a_it_end   = a_indexes.end();
  MuQuantityIndexes_t::iterator a_it;
  for(a_it=a_it_begin;a_it!=a_it_end;++a_it) {
    MuDimension_e a_dimension = (MuDimension_e)(*a_it);
    a_dimensions_p->insert(a_dimension);
  }    
  //
  return a_dimensions_p;
  
}

MuDimension_e MU_get_dimension(MuEQuantity_e equantity) {
  MuQuantity_t a_quantity;
  a_quantity[equantity]=1;
  //
  MuDimensions_t a_dimensions;
  MU_get_dimensions(a_quantity,&a_dimensions);
  //
  MuDimension_e a_dimension=(*(a_dimensions.begin()));
  return a_dimension;
}


/* --------- Units --------- */

string MU_search_unit(MuDimension_e dimension,const MuUnit_t &unit) {
  const MuQuantityManager_t &a_quantity_manager = loc_get_quantity_manager();
  const MuPrefixes_t        &a_prefixes         = loc_get_prefixes();

  int a_nb_basic_units=a_quantity_manager.getNbBasicUnits(dimension);
  // Second step: other basic units
  int i;
  for(i=0;i<a_nb_basic_units;++i) {
    const MuUnit_t &a_unit=a_quantity_manager.getBasicUnit(dimension,i);
    if(a_unit==unit) return a_quantity_manager.getBasicUnitName(dimension,i);
  }
  // First step: comparing to first basic unit 
  if(a_nb_basic_units>0) {
    const MuUnit_t &a_unit = a_quantity_manager.getBasicUnit(dimension,0);
    if(a_unit==unit) return a_quantity_manager.getBasicUnitName(dimension,0);
    //
    double a_zero = a_unit.convertFrom(0.,unit);
    if(a_zero==0.) {
      double        a_one    = a_unit.convertFrom(1.,unit);
      const string &a_prefix = a_prefixes.getName(a_one);
      //
      if(a_prefix!="") return a_prefix+a_quantity_manager.getBasicUnitName(dimension,0);
    }    
  }
 // Third step: multiple of other basic units  
  for(i=1;i<a_nb_basic_units;++i) {
    const MuUnit_t &a_unit = a_quantity_manager.getBasicUnit(dimension,i);
    double          a_zero = a_unit.convertFrom(0.,unit);
    //
    if(a_zero==0.) {
      double        a_one    = a_unit.convertFrom(1.,unit);
      const string &a_prefix = a_prefixes.getName(a_one);
      //
      if(a_prefix!="") return a_prefix+a_quantity_manager.getBasicUnitName(dimension,i);
    }
  }
  //
  return "";
}

MuUnit_t MU_search_unit(MuDimension_e dimension,const string &formula) {
  const MuQuantityManager_t &a_quantity_manager=loc_get_quantity_manager();
  //
  LocQuantitiesUnits_t a_quantities_units;
  loc_get_quantities_units(formula,&a_quantities_units);
  //
  MuQuantityIndexes_t a_dimensions;
  int                 a_nb_quantities=(int)(a_quantities_units.size());
  for(int i=0;i<a_nb_quantities;++i) {
    a_dimensions.clear();
    a_quantity_manager.searchQuantities(a_quantities_units[i].first,&a_dimensions);
    //
    bool a_found=(a_dimensions.find((int)dimension)!=a_dimensions.end());
    if(a_found) return a_quantities_units[i].second;
  }
  //
  return MuUnit_t(dimension==UDI_DIMENSIONLESS ? 1. : 0.,0.);
}

bool MU_is_valid_unit(MuDimension_e dimension,const string &formula) {
  MuUnit_t a_none(0,0);
  MuUnit_t a_found=MU_search_unit(dimension,formula);
  //
  return a_found!=a_none;
}

string MU_get_SI_unit(MuDimension_e dimension) {
  MuUnit_t a_si_unit(1,0);
  string   a_si_name = MU_search_unit(dimension,a_si_unit);
  //
  if(a_si_name=="") {
    typedef vector<MuEQuantity_e> LocEQuantites_t;
    LocEQuantites_t a_neg_exponents;
    a_neg_exponents.reserve(QTY_LAST-QTY_UNKNOWN-1);
    //
    const MuQuantity_t &a_quantity         = MU_get_quantity(dimension);
    int                 a_nb_pos_exponents = 0;
    //
    for(int i=QTY_UNKNOWN+1;i<QTY_LAST;++i) {
      MuEQuantity_e a_equantity = (MuEQuantity_e)i;
      double        a_exp       = a_quantity[a_equantity];
      //
      if(a_exp>0) {
	MuDimension_e a_loc_dimension = MU_get_dimension(a_equantity);
	string        a_loc_unit      = MU_get_SI_unit(a_loc_dimension);
	//
	if((a_nb_pos_exponents++)>0) a_si_name+="*";
	a_si_name+=loc_get_unit(a_loc_unit,a_exp);
      } else if(a_exp<0) {
	a_neg_exponents.push_back(a_equantity);
      }
    }
    //
    int a_nb_neg_exponents=(int)(a_neg_exponents.size());
    //
    if(a_nb_neg_exponents>0 && a_nb_pos_exponents>0) {
      a_si_name+="/";
      if(a_nb_neg_exponents>1) a_si_name+="(";
    }
    //
    for(int j=0;j<a_nb_neg_exponents;++j) {
      MuEQuantity_e a_equantity = a_neg_exponents[j];
      double        a_exp       = a_quantity[a_equantity];
      //
      MuDimension_e a_loc_dimension = MU_get_dimension(a_equantity);
      string        a_loc_unit      = MU_get_SI_unit(a_loc_dimension);
      //
      if(j>0) a_si_name+="*";
      if(a_nb_neg_exponents>0) a_exp=(-a_exp);
      a_si_name+=loc_get_unit(a_loc_unit,a_exp);
    }
    //
    if(a_nb_neg_exponents>1) a_si_name+=")";
  }
  //
  return a_si_name;
}

/* --------- Static functions --------- */

static const MuQuantityManager_t &loc_get_quantity_manager() {
  return *loc_quantity_manager_p;
}

static const MuPrefixes_t &loc_get_prefixes() {
  return *loc_prefixes_p;
}

static MuQuantitiesUnitsOperatorManager_t &loc_get_operator_manager() {
  return *loc_operator_manager_p;
}

static bool loc_is_operator_begin(char op) {
  typedef set<char> LocOperators_t;
  static LocOperators_t a_operators;
  //
  if(a_operators.empty()) {
    a_operators.insert('*');
    a_operators.insert('/');
    a_operators.insert('^');
    a_operators.insert('�');
    a_operators.insert('�');  }
  //
  return a_operators.find(op)!=a_operators.end();
}

static bool loc_is_scalar(const string &s) {
  char *a_res;
  strtod(s.c_str(),&a_res);
  return (a_res!=s.c_str());
}

static string loc_get_unit(const string &unit,double power) {
  if(power==1) return unit;
  if(power==2) return unit+"�";
  if(power==3) return unit+"�";
  return str_printf("%s^%lg",unit.c_str(),power);
}

//#include <mv_iostream.h> 
static LocQuantitiesUnits_t *loc_get_quantities_units_from_multipled_basic_unit(const string         &basic_unit,
										double                multiplyer,
										LocQuantitiesUnits_t *quantities_units_p)
{
  
  /*
  cout << "--------- loc_get_quantities_units_from_multipled_basic_unit beg ---------" << endl;
  cout << "FORMULA = \"" << basic_unit << "\"" << endl;
  */
  
  LocQuantitiesUnits_t *a_quantities_units_p=(quantities_units_p==NULL ? new LocQuantitiesUnits_t() : quantities_units_p);
  //
  const MuQuantityManager_t &a_quantity_manager=loc_get_quantity_manager();
  //
  MuQuantityIndexes_t a_quantity_indexes;
  a_quantity_manager.searchQuantitiesFromBasicUnit(basic_unit,&a_quantity_indexes);
  //
  MuQuantityIndexes_t::iterator a_it_begin = a_quantity_indexes.begin();
  MuQuantityIndexes_t::iterator a_it_end   = a_quantity_indexes.end();
  MuQuantityIndexes_t::iterator a_it;
  for(a_it=a_it_begin;a_it!=a_it_end;++a_it) {
    MuDimension_e       a_dimension  = (MuDimension_e)(*a_it);
    LocQuantityUnit_t   a_quantity_unit;
    //
    a_quantity_unit.first  = a_quantity_manager.getQuantity(a_dimension);
    a_quantity_unit.second = MuUnit_t(multiplyer,0,a_quantity_manager.getBasicUnit(a_dimension,basic_unit));
    a_quantities_units_p->push_back(a_quantity_unit);
  }
  //
  
  /*
  int a_nb_quantities=a_quantities_units_p->size();
  for(int i=0;i<a_nb_quantities;++i) {
    MuQuantity_t &a_quantity = (*a_quantities_units_p)[i].first;
    MuUnit_t     &a_unit     = (*a_quantities_units_p)[i].second;
    cout << "RESULT_QUANTITY[" << i << "] = " << a_quantity << endl;
    cout << "RESULT_UNIT["     << i << "] = " << a_unit     << endl;
  }
  cout << "--------- loc_get_quantities_units_from_multipled_basic_unit end ---------" << endl;
  */
  
  return a_quantities_units_p;
}

static LocQuantitiesUnits_t *loc_get_quantities_units_from_prefixed_basic_unit(const string         &unit,
										     LocQuantitiesUnits_t *quantities_units_p)
{
  
  /*
  cout << "--------- loc_get_quantities_units_from_prefixed_basic_unit beg ---------" << endl;
  cout << "FORMULA = \"" << unit << "\"" << endl;
  */
  
  LocQuantitiesUnits_t *a_quantities_units_p=(quantities_units_p==NULL ? new LocQuantitiesUnits_t() : quantities_units_p);
  //
  const MuPrefixes_t &a_prefixes=loc_get_prefixes();
  //
  loc_get_quantities_units_from_multipled_basic_unit(unit,1.,a_quantities_units_p);
  //
  int a_unit_length = (int)(unit.length());
  int a_nb_prefixes = a_prefixes.getNbPrefixes();
  //
  for(int i=0;i<a_nb_prefixes;++i) {
    const string &a_prefix        = a_prefixes[i];
    int           a_prefix_length = (int)(a_prefix.length());
    //
    if(a_prefix_length<a_unit_length && !strncmp(unit.c_str(),a_prefix.c_str(),a_prefix_length)) {
      string a_basic_unit = unit.substr(a_prefix_length,(size_t)(a_unit_length-a_prefix_length));
      double a_multiplyer = a_prefixes.getMultiplyer(a_prefix);
      //
      loc_get_quantities_units_from_multipled_basic_unit(a_basic_unit,a_multiplyer,a_quantities_units_p);
    }
  }
  
  /*
  int a_nb_quantities=a_quantities_units_p->size();
  for(int i=0;i<a_nb_quantities;++i) {
    MuQuantity_t &a_quantity = (*a_quantities_units_p)[i].first;
    MuUnit_t     &a_unit     = (*a_quantities_units_p)[i].second;
    cout << "RESULT_QUANTITY[" << i << "] = " << a_quantity << endl;
    cout << "RESULT_UNIT["     << i << "] = " << a_unit     << endl;
  }
  cout << "--------- loc_get_quantities_units_from_prefixed_basic_unit end ---------" << endl;
  */
  
  //
  return a_quantities_units_p;
}

static MvOperand_t *loc_get_operand(const string &formula,int ind0,int *ind_p) {
  
  /*
  cout << "--------- loc_get_operand beg ---------" << endl;
  cout << "FORMULA = \"" << formula.substr(ind0,formula.length()-ind0) << "\"" << endl;
  */
  
  MvOperand_t *a_operand_p = NULL;
  int          a_length    = (int)(formula.length());
  int          a_ind       = ind0;
  //
  while(a_operand_p==NULL && a_ind<a_length) {
    char a_char=formula[a_ind++];
    //
    if(a_char=='(') {
      string a_formula        = "";
      int    a_nb_parenthesis = 1;
      //
      while(a_nb_parenthesis>0 && a_ind<a_length) {
	a_char=formula[a_ind++];
	if(a_char=='(') ++a_nb_parenthesis; else if(a_char==')') --a_nb_parenthesis;
	if(a_nb_parenthesis>0) a_formula+=a_char;
      }
      //
      if(a_nb_parenthesis>0 || a_formula.size()==0) return NULL;
      a_operand_p=loc_evaluate_formula(a_formula);
    } else {
      string a_operand="";
      while(a_ind<a_length && (!loc_is_operator_begin(a_char))) {
	a_operand+=a_char;
	a_char=formula[a_ind++];
      }
      if(loc_is_operator_begin(a_char)) --a_ind; else a_operand+=a_char;
      //
      //cout << "a_operand = \"" << a_operand  << "\"" << endl; 
      if(loc_is_scalar(a_operand)) {
	double a_value=0.;
#ifdef WIN32
	sscanf_s(a_operand.c_str(),"%lg",&a_value);
#else
	sscanf(a_operand.c_str(),"%lg",&a_value);
#endif /* WIN32 */
	a_operand_p=new MuScalarOperand_t(a_value);
      } else {
	LocQuantitiesUnits_t a_quantities_units;
	a_quantities_units.reserve(5);
	loc_get_quantities_units_from_prefixed_basic_unit(a_operand,&a_quantities_units);
	//
	int a_nb_quantities=(int)(a_quantities_units.size());
	if(a_nb_quantities<=0) return NULL;
	//
	MuQuantitiesUnitsOperand_t *a_qu_operand_p=new MuQuantitiesUnitsOperand_t(a_nb_quantities);
	for(int i=0;i<a_nb_quantities;++i) {
	  a_qu_operand_p->setQuantity(i,a_quantities_units[i].first);
	  a_qu_operand_p->setUnit(i,a_quantities_units[i].second);
	}
	a_operand_p=a_qu_operand_p;
      }
    }
  }
  //
  *ind_p=a_ind;
  
  /*
  cout << "OPERAND       = \"" << formula.substr(ind0,a_ind-ind0)    << "\"" << endl;
  cout << "RIGHT_FORMULA = \"" << formula.substr(a_ind,a_length-a_ind) << "\"" << endl;
  //
  if(a_operand_p->getType()==MUOPT_SCALAR) {
    MuScalarOperand_t *a_scalar_p=(MuScalarOperand_t*)a_operand_p;
    //
    cout << "RESULT_SCALAR = " << a_scalar_p->getValue() << endl;
  } else {
    MuQuantitiesUnitsOperand_t *a_qu_operand_p  = (MuQuantitiesUnitsOperand_t *)a_operand_p;
    int                         a_nb_quantities = a_qu_operand_p->getNbQuantities();
    for(int i=0;i<a_nb_quantities;++i) {
      const MuQuantity_t &a_quantity = a_qu_operand_p->getQuantity(i);
      const MuUnit_t     &a_unit     = a_qu_operand_p->getUnit(i);
      cout << "RESULT_QUANTITY[" << i << "] = " << a_quantity << endl;
      cout << "RESULT_UNIT["     << i << "] = " << a_unit     << endl;
    }
  }
  cout << "--------- loc_get_operand end ---------" << endl;
  */
  
  return a_operand_p;
}

static MuQuantitiesUnitsOperand_t *loc_evaluate_next_operator(MuQuantitiesUnitsOperand_t *operand_p,
							      const string               &formula,
							      int                         ind0,
							      int                        *ind_p,
							      int                         level)
{
  
  /*
  cout << "--------- loc_evaluate_next_operator " << level << " beg ---------" << endl;
  cout << "FORMULA = \"" << formula.substr(ind0,formula.length()-ind0) << "\"" << endl;
  int a_nb_quantities = operand_p->getNbQuantities();
  for(int i=0;i<a_nb_quantities;++i) {
    const MuQuantity_t &a_quantity = operand_p->getQuantity(i);
    const MuUnit_t     &a_unit     = operand_p->getUnit(i);
    cout << "INPUT_QUANTITY[" << i << "] = " << a_quantity << endl;
    cout << "INPUT_UNIT["     << i << "] = " << a_unit     << endl;
  }
  */
  
  static MuQuantitiesUnitsOperatorManager_t &a_operator_manager=loc_get_operator_manager();
  //
  MuQuantitiesUnitsOperand_t *a_result_p   = operand_p;
  int                         a_length     = (int)(formula.length());
  int                         a_ind        = ind0;
  string                      a_operator   = ""; 
  MvOperand_t                 *a_operand_p = NULL;
  //
  char a_char=formula[a_ind++];
  if(a_char=='*' && formula[a_ind]=='*') { a_char='^'; ++a_ind; }
  //
  //cout << "a_char = '" << a_char << "'" << endl; 
  switch(a_char) {
  case '*':
  case '/':
    if(level==1) {
      a_operand_p=loc_get_operand(formula,a_ind,&a_ind);
      if(a_operand_p==NULL) return NULL;
      //
      if(a_operand_p->getType()!=MUOPT_QUANTITIES_UNITS) {
	delete a_operand_p;
	return NULL;
      } else {
	MuQuantitiesUnitsOperand_t *a_cur_operand_p = NULL;
	MuQuantitiesUnitsOperand_t *a_new_operand_p = (MuQuantitiesUnitsOperand_t *)a_operand_p;
	//
	while(a_ind<a_length && a_new_operand_p!=a_cur_operand_p) {
	  a_cur_operand_p=a_new_operand_p;
	  a_new_operand_p=loc_evaluate_next_operator(a_cur_operand_p,formula,a_ind,&a_ind,0);
	  if(a_new_operand_p==NULL) { delete a_cur_operand_p; return NULL; }
	}	
	//
	a_operand_p=a_new_operand_p;
	a_operator+=a_char;
      }
    } else {
      //cout << "ECHEC (level must be 1)" << endl; 
      a_ind=ind0;
    }
    break;
    case '�':
    // case '�':
    case '^':
    if(level==0) {
      if(a_char=='^') {
	a_operand_p=loc_get_operand(formula,a_ind,&a_ind);
	//
	if(a_operand_p->getType()!=MUOPT_SCALAR) {
	  delete a_operand_p;	    
	  return NULL;
	}
      } else {
        double a_exp=(a_char=='�' ? 2. : 3.);
        a_operand_p=new MuScalarOperand_t(a_exp);
      }
      //
      a_operator+='^';
    } else {
      //cout << "ECHEC (level must be 0)" << endl; 
      a_ind=ind0;
    }
    break;
  default:
    return NULL;
    //break;
  }
  //
  if(a_operand_p!=NULL) {
    
    MvOperation_t *a_operation_pf = a_operator_manager.newBinaryOperation(a_operator,a_result_p,a_operand_p);
    a_result_p=(MuQuantitiesUnitsOperand_t *)(a_operation_pf->evaluate());
    delete a_operation_pf;
    
    /*
    a_nb_quantities = a_result_p->getNbQuantities();
    for(int i=0;i<a_nb_quantities;++i) {
      const MuQuantity_t &a_quantity = a_result_p->getQuantity(i);
      const MuUnit_t     &a_unit     = a_result_p->getUnit(i);
      cout << "RESULT_QUANTITY[" << i << "] = " << a_quantity << endl;
      cout << "RESULT_UNIT["     << i << "] = " << a_unit     << endl;
    }
    */
    
  }
  //
  
  //cout << "--------- loc_evaluate_next_operator " << level << " end ---------" << endl;
  
  *ind_p=a_ind;
  return a_result_p;
}

static MvOperand_t *loc_evaluate_formula(const string &formula) {
  
  /*
  cout << "--------- loc_evaluate_formula beg ---------" << endl;
  cout << "FORMULA = \"" << formula << "\"" << endl;
  */
  
  int a_formula_length = (int)(formula.length());
  int a_ind            = 0;
  //
  MvOperand_t *a_operand_p=loc_get_operand(formula,a_ind,&a_ind);
  if(a_operand_p==NULL) return NULL;
  //
  MuOperandType_e a_operand_type=(MuOperandType_e)(a_operand_p->getType());
  if(a_operand_type==MUOPT_SCALAR && a_ind<a_formula_length) {
    delete a_operand_p;
    return NULL;
  }
  //
  MuQuantitiesUnitsOperand_t *a_result_p=NULL;
  while(a_ind<a_formula_length && a_result_p!=a_operand_p) {
    a_result_p  = (MuQuantitiesUnitsOperand_t *)a_operand_p;
    a_operand_p = (MvOperand_t *)loc_evaluate_next_operator(a_result_p,formula,a_ind,&a_ind,0);
    if(a_operand_p==NULL) { delete a_result_p; return NULL; }
  }
  //
  a_result_p=(MuQuantitiesUnitsOperand_t *)a_operand_p;
  //
  while(a_ind<a_formula_length) {
    MuQuantitiesUnitsOperand_t *a_loc_result_p=(MuQuantitiesUnitsOperand_t *)loc_evaluate_next_operator(a_result_p,formula,a_ind,&a_ind,1);
    if(a_loc_result_p==NULL) { delete a_result_p; return NULL; }
    a_result_p = a_loc_result_p;
  }
  //
  
  /*
  int a_nb_quantities = a_result_p->getNbQuantities();
  for(int i=0;i<a_nb_quantities;++i) {
    const MuQuantity_t &a_quantity = a_result_p->getQuantity(i);
    const MuUnit_t     &a_unit     = a_result_p->getUnit(i);
    cout << "RESULT_QUANTITY[" << i << "] = " << a_quantity << endl;
    cout << "RESULT_UNIT["     << i << "] = " << a_unit     << endl;
  }
  cout << "--------- loc_evaluate_formula end ---------" << endl;
  */
  
  return a_result_p;
}

static LocQuantitiesUnits_t *loc_get_quantities_units(const string         &formula,
						      LocQuantitiesUnits_t *quantities_units_p)
{
  LocQuantitiesUnits_t *a_quantities_units_p=(quantities_units_p==NULL ? new LocQuantitiesUnits_t() : quantities_units_p);
  //
  MvOperand_t *a_operand_pf=loc_evaluate_formula(formula);
  //
  if(a_operand_pf!=NULL && a_operand_pf->getType()==MUOPT_QUANTITIES_UNITS) {
    MuQuantitiesUnitsOperand_t *a_result_p      = (MuQuantitiesUnitsOperand_t *)a_operand_pf;
    int                         a_nb_quantities = a_result_p->getNbQuantities();
    //
    if(a_quantities_units_p->capacity()==0) a_quantities_units_p->reserve(a_nb_quantities);
    //
    LocQuantityUnit_t a_quantity_unit;
    for(int i=0;i<a_nb_quantities;++i) {
      a_quantity_unit.first  = a_result_p->getQuantity(i);
      a_quantity_unit.second = a_result_p->getUnit(i);
      a_quantities_units_p->push_back(a_quantity_unit);
    }
  }
  //
  delete a_operand_pf;
  return a_quantities_units_p;
}

int MU_check_custom_dimension_info(MuDimension_e dim, vector<string>& args)
{
    // validation for number of args
    int nb_args = (int)args.size();
    const MuQuantity_t& quantity = MU_get_quantity(dim);
    const vector<string>& stored_args = quantity.getArgsVect();
    int nb_stored_args = (int)stored_args.size();
    if (nb_args != nb_stored_args)
    {
        return -1;
    }
    return 0;
}
