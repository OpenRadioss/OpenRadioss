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

#include <UTILS/error.h>
#include <UTILS/str_utils.h>

#include "mu_units_parser.h"

#include <cstring>


MuQuantityManager_t *MuUnitsParser_t::read(MuQuantityManager_t *quantity_manager_p) {
  MuQuantityManager_t *a_quantity_manager_p=(quantity_manager_p==NULL ? new MuQuantityManager_t() : quantity_manager_p);
  //
  try {
    string a_keyword="";
    while(!seof(false)) {
      a_keyword=getNextString();
      if(a_keyword=="QUANTITY") {
	readQuantity(a_quantity_manager_p);
      } else {
	throwError(getMsg(1),a_keyword.c_str());
      }
    }
  } catch(MvError_t &err) {
    throw MvError_t(err.GetMessage()+"\n"+myBuffer+"\n"+getMsg(0));
  }
  //
  return a_quantity_manager_p;
}

void MuUnitsParser_t::readQuantity(MuQuantityManager_t *quantity_manager_p) {
  typedef vector<string> LocUnitNames_t;   LocUnitNames_t   a_unit_names;   a_unit_names.reserve(25);
  typedef vector<double> LocUnitCoeffs_t;  LocUnitCoeffs_t  a_unit_coeffs;  a_unit_coeffs.reserve(25);
  typedef vector<double> LocUnitOffsets_t; LocUnitOffsets_t a_unit_offsets; a_unit_offsets.reserve(25);
  //
  if(getNextChar()!='(') throwError(getMsg(2),"(");
  string a_quantity_name=getNextQuotedString();
  if(getNextChar()!=')') throwError(getMsg(2),")");
  //
  int a_quantity_ind=quantity_manager_p->getNbQuantities();
  MuQuantity_t a_quantity;
  // extract arguments
  int pos = (int)a_quantity_name.find("<");
  int pos_close = (int)a_quantity_name.find(">");
  if (pos != string::npos)
  {
      if (pos_close == string::npos)
          throwError(getMsg(2), ">");
      int length = (int)a_quantity_name.size();
      string arg_list = a_quantity_name.substr(pos + 1, length - pos - 2);
      vector<string> key_vect;
      StringTokenize(arg_list, key_vect, ",");
      a_quantity.storeArgs(key_vect);
      //truncate
      a_quantity_name = a_quantity_name.substr(0, pos);
  }
  char a_char=getNextChar();
  if(a_char=='{') {
    a_unit_names.resize(0);
    a_unit_coeffs.resize(0);
    a_unit_offsets.resize(0);
    //
    a_char=getNextChar();
    while(a_char!='}') {
      unreadChar();
      //
      string a_keyword=getNextString();
      if(a_keyword=="ID") {               // Reading the index
	if(getNextChar()!='=') throwError(getMsg(2),"=");
	a_quantity_ind=getNextInt();
	if(getNextChar()!=';') throwError(getMsg(2),";");
      } else if(a_keyword=="POW") {      // Reading an elementary quantity
	if(getNextChar()!='[') throwError(getMsg(2),"[");
	MuEQuantity_e a_equantity=getNextEQuantity();
	if(getNextChar()!=']') throwError(getMsg(2),"]");
	//
	if(getNextChar()!='=') throwError(getMsg(2),"=");

    char c = getNextChar();
    string a_str = "";
    while (c != ';')
    {
        a_str += c;
        c = getNextChar();
    }
    char* a_res = NULL;
    double a_pow = strtod(a_str.c_str(), &a_res);
    if ((a_res == a_str.c_str()) || (strcmp(a_res, "") != 0)) {
        a_quantity.storeExpr(a_equantity, a_str);
    }
    else
    {
        a_quantity[a_equantity] = a_pow;
    }
	if(c!=';') throwError(getMsg(2),";");
	//

      } else if(a_keyword=="ADD_UNIT") { // Reading a basic unit
	if(getNextChar()!='(') throwError(getMsg(2),"(");
	string a_unit_name=getNextQuotedString();
	if(getNextChar()!=',') throwError(getMsg(2),",");
	//
	double a_unit_coeff  = getNextFloat();
	double a_unit_offset = 0.;
	a_char=getNextChar();
	if(a_char==',') {
	  a_unit_offset=getNextFloat();
	  a_char=getNextChar();
	}
	if(a_char!=')') throwError(getMsg(2),")");
	if(getNextChar()!=';') throwError(getMsg(2),";");
	//
	a_unit_names.push_back(a_unit_name);
	a_unit_coeffs.push_back(a_unit_coeff);
	a_unit_offsets.push_back(a_unit_offset);
      }
      //
      a_char=getNextChar();
    }
    //
    int a_nb_units=(int)(a_unit_names.size());
    quantity_manager_p->setQuantity(a_quantity_ind,a_quantity_name,a_quantity,a_nb_units);
    for(int i=0;i<a_nb_units;++i) {
      MuUnit_t a_unit(a_unit_coeffs[i],a_unit_offsets[i]);
      quantity_manager_p->addBasicUnit(a_quantity_ind,a_unit_names[i],a_unit);
    }
  } else if(a_char==';') {
    quantity_manager_p->setQuantity(a_quantity_ind,a_quantity_name,a_quantity);
  } else {
    throwError(getMsg(2),";");
  }
}

MuEQuantity_e MuUnitsParser_t::getNextEQuantity() {
  string        a_equantity_name = getNextString();
  MuEQuantity_e a_equantity      = MuQuantity_t::getEQuantity(a_equantity_name);
  //
  if(a_equantity==QTY_UNKNOWN) throwError(getMsg(3),a_equantity_name.c_str());
  //
  return a_equantity;
}
