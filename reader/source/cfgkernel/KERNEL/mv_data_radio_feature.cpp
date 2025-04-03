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


#include <UTILS/mv_cstdio.h> 
#include <UTILS/error.h>


#include "mv_descriptor.h"
#include "mv_data_radio_feature.h"


/* --------- Constructors & destructor --------- */


MvDataRadioFeature_t::MvDataRadioFeature_t(const string &name,int ikeyword,value_type_e vtype,int nb_radios,
					   MvOrientation_e orient,int nbr,int nbc) :
  MvDataSingleFeature_t(DFT_RADIO,name,ikeyword),
  MvDataOrientationFeature_t(orient,nbr,nbc),
  myValueType(vtype),
  myNumber(nb_radios),
  myNameArray(myNumber>0   ? new string[myNumber]     : NULL),
  myDomainArray(myNumber>0 ? new MvDomain_e[myNumber] : NULL),
  myEnumValueFlag(false)
{
  myIntValueArray = NULL;
  myDoPrefixValue = true;
  switch(myValueType) {
  case VTYPE_INT:    myIntValueArray    = new int[myNumber];    break;
  case VTYPE_UINT:   myUIntValueArray   = new unsigned int[myNumber];    break;
  case VTYPE_FLOAT:  myFloatValueArray  = new double[myNumber]; break;
  case VTYPE_STRING: myStringValueArray = new string[myNumber]; break;
  default:           break;
  }
}


MvDataRadioFeature_t::~MvDataRadioFeature_t() {
  
  if(myNameArray!=NULL)   delete [] myNameArray;
  //
  switch(myValueType) {
  case VTYPE_INT:    delete [] myIntValueArray;    break;
  case VTYPE_UINT:    delete [] myUIntValueArray;    break;
  case VTYPE_FLOAT:  delete [] myFloatValueArray;  break;
  case VTYPE_STRING: delete [] myStringValueArray; break;
  default:           break;
  }  
  //  
  if(myDomainArray!=NULL) delete [] myDomainArray; 
  
}


/* --------- Accessors --------- */


void MvDataRadioFeature_t::setRadio(int ind,const string &name,int value,MvDomain_e domain) {
  // Error management
  if(ind<0 || ind>=myNumber) {
    throw MvError_t("MvDataRadioFeature_t::setRadio -> index %i out of bounds",ind);
  }
  // Setting the radio button
  myNameArray[ind]     = name;
  myIntValueArray[ind] = value;  
  myDomainArray[ind]   = domain;
}


void MvDataRadioFeature_t::setRadio(int ind,const string &name,unsigned int value,MvDomain_e domain) {
  // Error management
  if(ind<0 || ind>=myNumber) {
    throw MvError_t("MvDataRadioFeature_t::setRadio -> index %i out of bounds",ind);
  }
  // Setting the radio button
  myNameArray[ind]     = name;
  myUIntValueArray[ind] = value;  
  myDomainArray[ind]   = domain;
}




void MvDataRadioFeature_t::setRadio(int ind,const string &name,double value,MvDomain_e domain) {
  // Error management
  if(ind<0 || ind>=myNumber) {
    throw MvError_t("MvDataRadioFeature_t::setRadio -> index %i out of bounds",ind);
  }
  // Setting the radio button
  myNameArray[ind]       = name;
  myFloatValueArray[ind] = value;
  myDomainArray[ind]     = domain;
}



void MvDataRadioFeature_t::setRadio(int ind,const string &name,const string &value,MvDomain_e domain) {
  // Error management
  if(ind<0 || ind>=myNumber) {
    throw MvError_t("MvDataRadioFeature_t::setRadio -> index %i out of bounds",ind);
  }
  // Setting the radio button
  myNameArray[ind]        = name;
  myStringValueArray[ind] = value;
  myDomainArray[ind]      = domain;
}



/* --------- Output in an output stream --------- */

ostream &MvDataRadioFeature_t::display(ostream &os,const MvDescriptor_t &descr,int level) const {
  int i;
  for(i=0;i<level;i++) os << "  ";
  display_props(os); 
  //
  os << "RADIO(TITLE=\"" << getTitle() << "\""
     << ",KEYWORD="      << descr.getSKeyword(getIKeyword()) 
     << ",ORIENTATION="  << MV_get_orientation(getOrientation());
  if(getOrientation()==ORI_MATRIX) os << "(" << getNbRows() << "," << getNbColumns() << ")";
  os << ") {" << endl;
  //
  for(int j=0;j<getNumber();j++) {
    
   for(i=0;i<level;i++) os << "  ";
    os << "  BUTTON[" << j << "]=(";
    //
    switch(myValueType) {
    case VTYPE_INT:    os << getRadioIntValue(j);                    break;
    case VTYPE_UINT:    os << getRadioUIntValue(j);                    break;
    case VTYPE_FLOAT:  os << getRadioFloatValue(j);                  break;
    case VTYPE_STRING: os << "\"" << getRadioStringValue(j) << "\""; break;
    default:           break;
    }  
    //
    os << ",\""       << getRadioName(j) << "\""
       << ","         << MV_get_domain(getRadioDomain(j)) 
       << ")"         << endl;
    
 }
  //
  for(i=0;i<level;i++) os << "  ";
  os << "}";
  return os;
}
