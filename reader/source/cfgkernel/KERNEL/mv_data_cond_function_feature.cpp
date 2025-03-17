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
#include "mv_data_cond_function_feature.h"

typedef MvExpression_t *MvExpressionPtr_t;


/* --------- Constructors & destructor --------- */


MvDataCondFunctionFeature_t::MvDataCondFunctionFeature_t(const string &title,int nb_tests,int func_ikw) :
  MvDataFunctionFeature_t(DFT_COND_FUNCTION,title,func_ikw),
  myNbTests(nb_tests>0 ? nb_tests : 0),
  myExpressionPtrTab(myNbTests>0 ? new MvExpressionPtr_t[myNbTests] : NULL),
  myNameTab(new string[myNbTests+1]),
  myXScalingNameTab(new string[myNbTests+1]),         
  myYScalingNameTab(new string[myNbTests+1]),         
  myZScalingNameTab(new string[myNbTests + 1]),
  myXTitleTab(new string[myNbTests+1]),
  myYTitleTab(new string[myNbTests+1]),
  myZTitleTab(new string[myNbTests + 1]),
  mySubXTitleTab(new string[myNbTests+1]),            
  mySubYTitleTab(new string[myNbTests+1]),            
  mySubZTitleTab(new string[myNbTests + 1]),
  myXDimensionTab(new MvDimension_e[myNbTests+1]),
  myYDimensionTab(new MvDimension_e[myNbTests+1]),
  myZDimensionTab(new MvDimension_e[myNbTests + 1]),
  myXArgVectTab(new vector<string>[myNbTests + 1]),
  myYArgVectTab(new vector<string>[myNbTests + 1]),
  myZArgVectTab(new vector<string>[myNbTests + 1]),
  mySubXDimensionTab(new MvDimension_e[myNbTests+1]), 
  mySubYDimensionTab(new MvDimension_e[myNbTests+1]), 
  mySubZDimensionTab(new MvDimension_e[myNbTests + 1]),
  mySubXArgVectTab(new vector<string>[myNbTests + 1]),
  mySubYArgVectTab(new vector<string>[myNbTests + 1]),
  mySubZArgVectTab(new vector<string>[myNbTests + 1]),
  myTestFeatures()
{
  setFeature(0,func_ikw,"");
}




MvDataCondFunctionFeature_t::MvDataCondFunctionFeature_t(const string &title,int nb_tests,
							 int func_ikw,int x_scal_ikw,int y_scal_ikw, int z_scal_ikw) :
  MvDataFunctionFeature_t(DFT_COND_FUNCTION,title,func_ikw,x_scal_ikw,y_scal_ikw,z_scal_ikw),
  myNbTests(nb_tests>0 ? nb_tests : 0),
  myExpressionPtrTab(myNbTests>0 ? new MvExpressionPtr_t[myNbTests] : NULL),
  myNameTab(new string[myNbTests+1]),
  myXScalingNameTab(new string[myNbTests+1]),
  myYScalingNameTab(new string[myNbTests+1]),
  myZScalingNameTab(new string[myNbTests + 1]),
  myXTitleTab(new string[myNbTests+1]),
  myYTitleTab(new string[myNbTests+1]),
  myZTitleTab(new string[myNbTests + 1]),
  mySubXTitleTab(new string[myNbTests+1]),            
  mySubYTitleTab(new string[myNbTests+1]),            
  mySubZTitleTab(new string[myNbTests + 1]),
  myXDimensionTab(new MvDimension_e[myNbTests+1]),
  myYDimensionTab(new MvDimension_e[myNbTests+1]),
  myZDimensionTab(new MvDimension_e[myNbTests + 1]),
  myXArgVectTab(new vector<string>[myNbTests + 1]),
  myYArgVectTab(new vector<string>[myNbTests + 1]),
  myZArgVectTab(new vector<string>[myNbTests + 1]),
  mySubXDimensionTab(new MvDimension_e[myNbTests+1]), 
  mySubYDimensionTab(new MvDimension_e[myNbTests+1]), 
  mySubZDimensionTab(new MvDimension_e[myNbTests + 1]),
  mySubXArgVectTab(new vector<string>[myNbTests + 1]),
  mySubYArgVectTab(new vector<string>[myNbTests + 1]),
  mySubZArgVectTab(new vector<string>[myNbTests + 1]),
  myTestFeatures()
{
  setFeature(0,func_ikw,"");
  if(myXScaleInd>0) setFeature(myXScaleInd,x_scal_ikw,"");
  if(myYScaleInd>0) setFeature(myYScaleInd,y_scal_ikw,"");
  if(myZScaleInd>0) setFeature(myZScaleInd,z_scal_ikw, "");
}



MvDataCondFunctionFeature_t::~MvDataCondFunctionFeature_t() {
  if(myExpressionPtrTab!=NULL) {
    for(int i=0;i<myNbTests;++i) delete myExpressionPtrTab[i];
    delete [] myExpressionPtrTab;
  }
  
  
  delete [] myNameTab;
  delete[] myXScalingNameTab;   delete[] myYScalingNameTab;   delete[] myZScalingNameTab;
  delete [] myXTitleTab;        delete [] myYTitleTab;   delete[] myZTitleTab;
  delete [] mySubXTitleTab;     delete [] mySubYTitleTab;  delete[] mySubZTitleTab;
  delete [] myXDimensionTab;    delete [] myYDimensionTab;  delete[] myZDimensionTab;
  delete [] myXArgVectTab;      delete [] myYArgVectTab;      delete [] myZArgVectTab;
  delete [] mySubXDimensionTab; delete [] mySubYDimensionTab; delete[] mySubZDimensionTab;
  delete [] mySubXArgVectTab;   delete [] mySubYArgVectTab;   delete [] mySubZArgVectTab;
  
  
}


/* --------- Creation --------- */



void MvDataCondFunctionFeature_t::setConditionalData(int                       i,
						     const MvDataFeatureSet_t &tests,
						     MvExpression_t           *expr_p,
						     const string             &func_name,
						     const string             &func_xtitle,
						     const string             &func_ytitle,
                             const string             &func_ztitle,
                             vector<string>           &x_arg_vect,
                             vector<string>           &y_arg_vect,
                             vector<string>           &z_arg_vect,
						     MvDimension_e             func_xdimension,
						     MvDimension_e             func_ydimension,
                             MvDimension_e             func_zdimension,
						     const string             &x_scaling_name,
						     const string             &y_scaling_name,
                             const string             &z_scaling_name,
						     const string             &sub_func_xtitle,
						     const string             &sub_func_ytitle,
                             const string             &sub_func_ztitle,
						     MvDimension_e             sub_func_xdimension,
						     MvDimension_e             sub_func_ydimension,
                             MvDimension_e             sub_func_zdimension,
                             vector<string>* sub_x_arg_vect,
                             vector<string>* sub_y_arg_vect,
                             vector<string>* sub_z_arg_vect)
{
  myExpressionPtrTab[i] = expr_p;
  myNameTab[i]          = func_name;
  myXTitleTab[i]        = func_xtitle;
  myYTitleTab[i]        = func_ytitle;
  myZTitleTab[i]        = func_ztitle;

  myXDimensionTab[i]    = func_xdimension;
  myYDimensionTab[i]    = func_ydimension;
  myZDimensionTab[i]    = func_zdimension;

  myXArgVectTab[i]      = x_arg_vect;
  myYArgVectTab[i]      = y_arg_vect;
  myZArgVectTab[i]      = z_arg_vect;

  myXScalingNameTab[i]  = x_scaling_name;
  myYScalingNameTab[i]  = y_scaling_name;
  myZScalingNameTab[i]  = z_scaling_name;

  mySubXTitleTab[i]     = sub_func_xtitle;
  mySubYTitleTab[i]     = sub_func_ytitle;
  mySubZTitleTab[i]     = sub_func_ztitle;
  mySubXDimensionTab[i] = sub_func_xdimension;
  mySubYDimensionTab[i] = sub_func_ydimension;
  mySubZDimensionTab[i] = sub_func_zdimension;

  mySubXArgVectTab[i] = *sub_x_arg_vect;
  mySubYArgVectTab[i] = *sub_y_arg_vect;
  mySubZArgVectTab[i] = *sub_z_arg_vect;
  //
  MvDataFeatureSet_t::const_iterator a_it_begin = tests.begin();
  MvDataFeatureSet_t::const_iterator a_it_end   = tests.end();
  MvDataFeatureSet_t::const_iterator a_it;
  for(a_it=a_it_begin;a_it!=a_it_end;++a_it) myTestFeatures.insert(*a_it);  
}





void MvDataCondFunctionFeature_t::setDefaultData(const string  &func_name,
						 const string  &func_xtitle,
						 const string  &func_ytitle,
                         const string  &func_ztitle,
                         vector<string>& x_arg_vect,
                         vector<string>& y_arg_vect,
                         vector<string>& z_arg_vect,
						 MvDimension_e  func_xdimension,
						 MvDimension_e  func_ydimension,
                         MvDimension_e  func_zdimension,
						 const string  &x_scaling_name,
						 const string  &y_scaling_name,
                         const string  &z_scaling_name,
						 const string  &sub_func_xtitle,
						 const string  &sub_func_ytitle,
                         const string  &sub_func_ztitle,
						 MvDimension_e  sub_func_xdimension,
						 MvDimension_e  sub_func_ydimension,
                         MvDimension_e  sub_func_zdimension,
                         vector<string>* sub_x_arg_vect,
                         vector<string>* sub_y_arg_vect,
                         vector<string>* sub_z_arg_vect)
{
  myNameTab[myNbTests]          = func_name;
  myXTitleTab[myNbTests]        = func_xtitle;
  myYTitleTab[myNbTests]        = func_ytitle;
  myZTitleTab[myNbTests]        = func_ztitle;

  myXDimensionTab[myNbTests]    = func_xdimension;
  myYDimensionTab[myNbTests]    = func_ydimension;
  myZDimensionTab[myNbTests]    = func_zdimension;

  myXArgVectTab[myNbTests]      = x_arg_vect;
  myYArgVectTab[myNbTests]      = y_arg_vect;
  myZArgVectTab[myNbTests]      = z_arg_vect;

  myXScalingNameTab[myNbTests]  = x_scaling_name;
  myYScalingNameTab[myNbTests]  = y_scaling_name;
  myZScalingNameTab[myNbTests]  = z_scaling_name;
  mySubXTitleTab[myNbTests]     = sub_func_xtitle;
  mySubYTitleTab[myNbTests]     = sub_func_ytitle;
  mySubZTitleTab[myNbTests]     = sub_func_ztitle;
  mySubXDimensionTab[myNbTests] = sub_func_xdimension;
  mySubYDimensionTab[myNbTests] = sub_func_ydimension;
  mySubZDimensionTab[myNbTests] = sub_func_zdimension;

  mySubXArgVectTab[myNbTests] = *sub_x_arg_vect;
  mySubYArgVectTab[myNbTests] = *sub_y_arg_vect;
  mySubZArgVectTab[myNbTests] = *sub_z_arg_vect;

}




/* --------- Consulting (high level) --------- */


MvDataFeatureSet_t *MvDataCondFunctionFeature_t::getConditionFeatures(MvDataFeatureSet_t *cond_features_p) const {
  MvDataFeatureSet_t *a_cond_features_p=(cond_features_p==NULL ? new MvDataFeatureSet_t() : cond_features_p) ;
  //
  *a_cond_features_p+=getTestFeatures();
  //
  return a_cond_features_p;
}































/* --------- Output in an output stream --------- */

ostream &MvDataCondFunctionFeature_t::display(ostream &os,const MvDescriptor_t &descr,int level) const {
  int i,j;
  string arg_list = "";
  vector<string> arg_vect;
  for(i=0;i<level;i++) os << "  ";
  display_props(os);
  //
  os << "FUNCTION(KEYWORD=" << descr.getSKeyword(getFunctionIKeyword()) << ") {" << endl;
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
    os << "TITLE       = \"" << getConditionalFunctionName(i) << "\";" << endl;
    for(j=0;j<level;j++) os << "  ";
    os << "X_TITLE     = \"" << getConditionalFunctionXTitle(i) << "\";" << endl;
    for(j=0;j<level;j++) os << "  ";
    os << "Y_TITLE     = \"" << getConditionalFunctionYTitle(i) << "\";" << endl;
    for (j = 0; j < level; j++) os << "  ";
    os << "Z_TITLE     = \"" << getConditionalFunctionZTitle(i) << "\";" << endl;
    for(j=0;j<level;j++) os << "  ";
    os << "X_DIMENSION = \"" << MV_get_dimension(getConditionalFunctionXDimension(i));
    arg_vect = getConditionalFunctionXArgVect(i);

    if (!arg_vect.empty())
    {
        convert_vect_to_string(arg_list, arg_vect);
        os << arg_list;
    }
    os << "\";" << endl;

    for(j=0;j<level;j++) os << "  ";
    os << "Y_DIMENSION = \"" << MV_get_dimension(getConditionalFunctionYDimension(i));
    arg_list = "";
    arg_vect = getConditionalFunctionYArgVect(i);

    if (!arg_vect.empty())
    {
        convert_vect_to_string(arg_list, arg_vect);
        os << arg_list;
    }
    os << "\";" << endl;

    for (j = 0; j < level; j++) os << "  ";
    os << "Z_DIMENSION = \"" << MV_get_dimension(getConditionalFunctionZDimension(i));
    arg_list = "";
    arg_vect = getConditionalFunctionZArgVect(i);

    if (!arg_vect.empty())
    {
        convert_vect_to_string(arg_list, arg_vect);
        os << arg_list;
    }
    os << "\";" << endl;

    //
    if(isScaled()) {
      for(j=0;j<level;j++) os << "  ";  
      os << "SCALING     = (KEYWORD=" << descr.getSKeyword(getScalingIKeyword()) 
	 << ",TITLE=\""               << getConditionalScalingName(i)
	 << "\");"                    << endl;
    }
    //
    
    MvFunctionType_e a_func_type=getFunctionType();
    if(a_func_type==FFTY_TABLE || a_func_type==FFTY_FUNCTION_OR_TABLE) { /* PM:0268:27/02/2006 */
      for(j=0;j<level;j++) os << "  ";
      os << "SUB_X_TITLE     = \"" << getConditionalSubFunctionXTitle(i) << "\";" << endl;
      for(j=0;j<level;j++) os << "  ";
      os << "SUB_Y_TITLE     = \"" << getConditionalSubFunctionYTitle(i) << "\";" << endl;
      for(j=0;j<level;j++) os << "  ";
      os << "SUB_X_DIMENSION = \"" << MV_get_dimension(getConditionalSubFunctionXDimension(i));
      arg_list = "";
      arg_vect = getConditionalSubFunctionXArgVect(i);

      if (!arg_vect.empty())
      {
          convert_vect_to_string(arg_list, arg_vect);
          os << arg_list;
      }
      os << "\";" << endl;

      for(j=0;j<level;j++) os << "  ";
      os << "SUB_Y_DIMENSION = \"" << MV_get_dimension(getConditionalSubFunctionYDimension(i));
      arg_list = "";
      arg_vect = getConditionalSubFunctionYArgVect(i);

      if (!arg_vect.empty())
      {
          convert_vect_to_string(arg_list, arg_vect);
          os << arg_list;
      }
      os << "\";" << endl;

      for (j = 0; j < level; j++) os << "  ";
      os << "SUB_Z_DIMENSION = \"" << MV_get_dimension(getConditionalSubFunctionZDimension(i));
      arg_list = "";
      arg_vect = getConditionalSubFunctionZArgVect(i);

      if (!arg_vect.empty())
      {
          convert_vect_to_string(arg_list, arg_vect);
          os << arg_list;
      }
      os << "\";" << endl;

    }
    
    //
    --level;
    for(j=0;j<level;j++) os << "  ";
    os << "} ";
  }
  //
  os << "else {" << endl;
  ++level;
  for(j=0;j<level;j++) os << "  ";
  os << "TITLE       = \"" << getDefaultFunctionName() << "\";" << endl;
  for(j=0;j<level;j++) os << "  ";
  os << "X_TITLE     = \"" << getDefaultFunctionXTitle() << "\";" << endl;
  for(j=0;j<level;j++) os << "  ";
  os << "Y_TITLE     = \"" << getDefaultFunctionYTitle() << "\";" << endl;
  for (j = 0; j < level; j++) os << "  ";
  os << "Z_TITLE     = \"" << getDefaultFunctionZTitle() << "\";" << endl;
  for(j=0;j<level;j++) os << "  ";
  os << "X_DIMENSION = \"" << MV_get_dimension(getDefaultFunctionXDimension());
  arg_list = "";
  arg_vect = getDefaultFunctionXArgVect();

  if (!arg_vect.empty())
  {
      convert_vect_to_string(arg_list, arg_vect);
      os << arg_list;
  }
  os << "\";" << endl;

  for(j=0;j<level;j++) os << "  ";
  os << "Y_DIMENSION = \"" << MV_get_dimension(getDefaultFunctionYDimension());
  arg_list = "";
  arg_vect = getDefaultFunctionYArgVect();

  if (!arg_vect.empty())
  {
      convert_vect_to_string(arg_list, arg_vect);
      os << arg_list;
  }
  os << "\";" << endl; 

  for (j = 0; j < level; j++) os << "  ";
  os << "Z_DIMENSION = \"" << MV_get_dimension(getDefaultFunctionZDimension());
  arg_list = "";
  arg_vect = getDefaultFunctionZArgVect();

  if (!arg_vect.empty())
  {
      convert_vect_to_string(arg_list, arg_vect);
      os << arg_list;
  }
  os << "\";" << endl;

  //
  if(isScaled()) {
    for(j=0;j<level;j++) os << "  ";  
    os << "SCALING     = (KEYWORD=" << descr.getSKeyword(getScalingIKeyword()) 
       << ",TITLE=\""               << getDefaultScalingName()
       << "\");"                    << endl;
  }
  //
  
  MvFunctionType_e a_func_type=getFunctionType();
  if(a_func_type==FFTY_TABLE || a_func_type==FFTY_FUNCTION_OR_TABLE) { /* PM:0268:27/02/2006 */
    for(j=0;j<level;j++) os << "  ";
    os << "SUB_X_TITLE     = \"" << getDefaultSubFunctionXTitle() << "\";" << endl;
    for(j=0;j<level;j++) os << "  ";
    os << "SUB_Y_TITLE     = \"" << getDefaultSubFunctionYTitle() << "\";" << endl;
    for(j=0;j<level;j++) os << "  ";
    os << "SUB_X_DIMENSION = \"" << MV_get_dimension(getDefaultSubFunctionXDimension());
    arg_list = "";
    arg_vect = getDefaultSubFunctionXArgVect();

    if (!arg_vect.empty())
    {
        convert_vect_to_string(arg_list, arg_vect);
        os << arg_list;
    }
    os << "\";" << endl;

    for(j=0;j<level;j++) os << "  ";
    os << "SUB_Y_DIMENSION = \"" << MV_get_dimension(getDefaultSubFunctionYDimension());
    arg_list = "";
    arg_vect = getDefaultSubFunctionYArgVect();

    if (!arg_list.empty())
    {
        convert_vect_to_string(arg_list, arg_vect);
        os << arg_list;
    }
    os << "\";" << endl;

    for (j = 0; j < level; j++) os << "  ";
    os << "SUB_Z_DIMENSION = \"" << MV_get_dimension(getDefaultSubFunctionZDimension());
    arg_list = "";
    arg_vect = getDefaultSubFunctionZArgVect();

    if (!arg_list.empty())
    {
        convert_vect_to_string(arg_list, arg_vect);
        os << arg_list;
    }
    os << "\";" << endl;

  }
  
  //
  --level;
  for(j=0;j<level;j++) os << "  ";
  os << "}" << endl;
  //
  --level;
  for(j=0;j<level;j++) os << "  ";
  os << "}";
  return os;
}
