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

//

#include <UTILS/win32_utils.h>

#include <UTILS/set_utils.h>
#include <UTILS/str_utils.h>


#include "mv_descriptor.h" 
#include "mv_data_cond_scalar_feature.h"

typedef MvExpression_t *MvExpressionPtr_t;


/* --------- Constructors & destructor --------- */

MvDataCondScalarFeature_t::MvDataCondScalarFeature_t(int nb_tests,const string &name,int ikeyword) :
  MvDataScalarFeature_t(DFT_COND_SCALAR,name,ikeyword),
  myNbTests(nb_tests>0  ? nb_tests : 0),
  myExpressionPtrTab(myNbTests>0 ? new MvExpressionPtr_t[myNbTests] : NULL),
  myDimensionTab(new MvDimension_e[myNbTests+1]),
  myNameTab(new string[myNbTests+1]),
  myTestFeatures(),
  myArgVectTab(new vector<string>[myNbTests+1])
{}

MvDataCondScalarFeature_t::~MvDataCondScalarFeature_t() {
  if(myExpressionPtrTab!=NULL) {
    for(int i=0;i<myNbTests;++i) delete myExpressionPtrTab[i];
    delete [] myExpressionPtrTab;
  }
  delete [] myDimensionTab;
  delete [] myNameTab;
  delete [] myArgVectTab;
}


/* --------- Creation --------- */

void MvDataCondScalarFeature_t::setConditionalData(int                       i,
					       const MvDataFeatureSet_t &tests,
					       MvExpression_t           *expr_p,
					       MvDimension_e             dimension,
					       const string             &name,
                           vector<string>& arg_vect)
{
  myExpressionPtrTab[i] = expr_p;
  myDimensionTab[i]     = dimension;
  myNameTab[i]          = name;
  myArgVectTab[i]       = arg_vect;
  //
  MvDataFeatureSet_t::const_iterator a_it_begin = tests.begin();
  MvDataFeatureSet_t::const_iterator a_it_end   = tests.end();
  MvDataFeatureSet_t::const_iterator a_it;
  for(a_it=a_it_begin;a_it!=a_it_end;++a_it) myTestFeatures.insert(*a_it);  
}

void MvDataCondScalarFeature_t::setDefaultData(MvDimension_e dimension,const string &name, vector<string>& arg_vect) {
  myDimensionTab[myNbTests] = dimension;
  myNameTab[myNbTests]      = name;
  myArgVectTab[myNbTests]   = arg_vect;
}


/* --------- Consulting (high level) --------- */

MvDataFeatureSet_t *MvDataCondScalarFeature_t::getConditionFeatures(MvDataFeatureSet_t *cond_features_p) const {
  MvDataFeatureSet_t *a_cond_features_p=(cond_features_p==NULL ? new MvDataFeatureSet_t() : cond_features_p) ;
  //
  *a_cond_features_p+=getTestFeatures();
  //
  return a_cond_features_p;
}








/* --------- Consulting (low level) --------- */





/* --------- Output in an output stream --------- */
ostream &MvDataCondScalarFeature_t::display(ostream &os,const MvDescriptor_t &descr,int level) const {
  int i,j;
  string arg_list = "";
  vector<string> arg_vect;
  for(j=0;j<level;j++) os << "  ";
  display_props(os); 
  //
  os << "SCALAR(KEYWORD=" << descr.getSKeyword(getIKeyword()) << ") {" << endl;
  //
  ++level;
  for(i=0;i<getNbTests();++i) {
    for(j=0;j<level;j++) os << "  ";
    if(i!=0) os << "else ";
    os << "if(";
    myExpressionPtrTab[i]->display(os,descr) << ") {" << endl;
    //
    ++level;
    for(j=0;j<level;j++) os << "  ";
    os << "TITLE     = \"" << getConditionalTitle(i) << "\";" << endl;
    for(j=0;j<level;j++) os << "  ";
    os << "DIMENSION = \"" << MV_get_dimension(getConditionalDimension(i));
    arg_list = "";
    arg_vect = getConditionalArgVect(i);

    if (!arg_vect.empty())
    {
        convert_vect_to_string(arg_list, arg_vect);
        os << arg_list;
    }
    os << "\";" << endl;

    --level;
    for(j=0;j<level;j++) os << "  ";
    os << "} ";
  }
  //
  os << "else {" << endl;
  ++level;
  for(j=0;j<level;j++) os << "  ";
  os << "TITLE     = \"" << getDefaultTitle() << "\";" << endl;
  for(j=0;j<level;j++) os << "  ";
  os << "DIMENSION = \"" << MV_get_dimension(getDefaultDimension());
  arg_list = "";
  arg_vect = getDefaultArgVect();

  if (!arg_vect.empty())
  {
      convert_vect_to_string(arg_list, arg_vect);
      os << arg_list;
  }
  os << "\";" << endl;

  --level;
  for(j=0;j<level;j++) os << "  ";
  os << "}" << endl;
  //
  --level;
  for(j=0;j<level;j++) os << "  ";
  os << "}";
  //
  return os;
}
