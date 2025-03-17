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
#include <UTILS/memory_utils.h>
#include <UTILS/error.h>
#include "mv_data_single_feature.h"
#include "mv_data_orientation_feature.h"
#include "mv_data_size_radio_feature.h"
#include "mv_descriptor.h"

MvDataSizeRadioFeature_t::MvDataSizeRadioFeature_t(const string &name,int ikeyword, value_type_e vtype,int nb_radios,
                                                   MvOrientation_e orient, int nbr,int nbc) :
  MvDataSingleFeature_t(DFT_SIZE_RADIO,name,ikeyword),
  MvDataOrientationFeature_t(orient,nbr,nbc),
  myValueType(vtype),
  myNumber(nb_radios),
  myNameArray(myNumber>0   ? new string[myNumber]     : NULL),
  myDomainArray(myNumber>0 ? new MvDomain_e[myNumber] : NULL),
  myNbArrays(0),
  myArrayTab(NULL),
  mySizeRadioFeature(false),
  myEnumValueFlag(false)
{
    myIntValueArray = NULL;
    myDoPrefixValue = true;
    switch(myValueType) {
    case VTYPE_INT:    myIntValueArray    = new int[myNumber];    break;
    case VTYPE_FLOAT:  myFloatValueArray  = new double[myNumber]; break;
    case VTYPE_STRING: myStringValueArray = new string[myNumber]; break;
    default:           break;
    }
}

MvDataSizeRadioFeature_t::~MvDataSizeRadioFeature_t() {

  if(myNameArray!=NULL)   delete [] myNameArray;
  //
  switch(myValueType) {
  case VTYPE_INT:    delete [] myIntValueArray;    break;
  case VTYPE_FLOAT:  delete [] myFloatValueArray;  break;
  case VTYPE_STRING: delete [] myStringValueArray; break;
  default:           break;
  }  
  //  
  if(myDomainArray!=NULL) delete [] myDomainArray;
  if(myArrayTab!=NULL) myfree(myArrayTab);
}

void MvDataSizeRadioFeature_t::setRadio(int ind,const string &name,int value,MvDomain_e domain) {
  // Error management
  if(ind<0 || ind>=myNumber) {
    throw MvError_t("MvDataSizeRadioFeature_t::setRadio -> index %i out of bounds",ind);
  }
  // Setting the radio button
  myNameArray[ind]     = name;
  myIntValueArray[ind] = value;  
  myDomainArray[ind]   = domain;
}

void MvDataSizeRadioFeature_t::setRadio(int ind,const string &name,double value,MvDomain_e domain) {
  // Error management
  if(ind<0 || ind>=myNumber) {
    throw MvError_t("MvDataSizeRadioFeature_t::setRadio -> index %i out of bounds",ind);
  }
  // Setting the radio button
  myNameArray[ind]       = name;
  myFloatValueArray[ind] = value;
  myDomainArray[ind]     = domain;
}



void MvDataSizeRadioFeature_t::setRadio(int ind,const string &name,const string &value,MvDomain_e domain) {
  // Error management
  if(ind<0 || ind>=myNumber) {
    throw MvError_t("MvDataSizeRadioFeature_t::setRadio -> index %i out of bounds",ind);
  }
  // Setting the radio button
  myNameArray[ind]        = name;
  myStringValueArray[ind] = value;
  myDomainArray[ind]      = domain;
}

/* --------- Associated dynamic arrays --------- */
void MvDataSizeRadioFeature_t::setNbArrays(int value)
{
    myNbArrays = value;
    myArrayTab=(MvDataDynamicArraySizeRadioFeature_t **)mymalloc(myNbArrays*sizeof(MvDataDynamicArraySizeRadioFeature_t *));
    for (int i = 0; i < myNbArrays; i++)
    {
        myArrayTab[i] = NULL;
    }
}

void MvDataSizeRadioFeature_t::addArray(MvDataDynamicArraySizeRadioFeature_t *daf_p) {
    myArrayTab=(MvDataDynamicArraySizeRadioFeature_t **)myrealloc(myArrayTab,
        (myNbArrays+1)*sizeof(MvDataDynamicArraySizeRadioFeature_t *));
    for (int i = 0; i < (myNbArrays + 1); i++)
    {
        myArrayTab[i] = NULL;
    }
    myArrayTab[myNbArrays++]=daf_p;
    daf_p->setSizeFeature(this);
}


/* --------- Output in an output stream --------- */

ostream &MvDataSizeRadioFeature_t::display(ostream &os,const MvDescriptor_t &descr,int level) const {
    int i;
    for(i=0;i<level;i++) os << "  ";
    display_props(os); 
    //
    os << "SIZE(TITLE=\"" << getTitle() << "\""
        << ",KEYWORD="     << descr.getSKeyword(getIKeyword()) 
        << ") {"           << endl;
    //
    int a_nb_arrays=getNbArrays();
    ++level;
    for (i = 0; i < a_nb_arrays; ++i)
    {
        MvDataDynamicArraySizeRadioFeature_t* an_array_feature = getArrayPtr(i);
        if (an_array_feature)
        {
            an_array_feature->display(os, descr, level) << endl;
        }
    }
    --level;
    //
    for(i=0;i<level;i++) os << "  ";
    os << "}";
    //
    return os;
}

