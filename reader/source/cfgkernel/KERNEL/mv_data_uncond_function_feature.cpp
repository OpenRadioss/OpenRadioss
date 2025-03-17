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
#include <UTILS/str_utils.h>

#include "mv_descriptor.h"
#include "mv_data_uncond_function_feature.h"


/* --------- Constructors & destructor --------- */



MvDataUncondFunctionFeature_t::MvDataUncondFunctionFeature_t(const string &title,int func_ikw,const string &func_name,
                                 string x_name, MvDimension_e x_dim, vector<string>& x_arg_vect,
                                 string y_name, MvDimension_e y_dim, vector<string>& y_arg_vect,
                                 string z_name, MvDimension_e z_dim, vector<string>& z_arg_vect,
                                 string sub_x_name, MvDimension_e sub_x_dim, vector<string>* sub_x_arg_vect,
                                 string sub_y_name, MvDimension_e sub_y_dim, vector<string>* sub_y_arg_vect,
                                 string sub_z_name, MvDimension_e sub_z_dim, vector<string>* sub_z_arg_vect) :
  MvDataFunctionFeature_t(DFT_FUNCTION,title,func_ikw),
  myXTitle(x_name),
  myYTitle(y_name),
  myZTitle(z_name),
  myXDimension(x_dim),
  myYDimension(y_dim),
  myZDimension(z_dim),
  myXArgVect(x_arg_vect),
  myYArgVect(y_arg_vect),
  myZArgVect(z_arg_vect),
  mySubXTitle(sub_x_name),
  mySubYTitle(sub_y_name),
  mySubZTitle(sub_z_name),
  mySubXDimension(sub_x_dim),
  mySubYDimension(sub_y_dim),
  mySubZDimension(sub_z_dim),
  mySubXArgVect(*sub_x_arg_vect),
  mySubYArgVect(*sub_y_arg_vect),
  mySubZArgVect(*sub_z_arg_vect)
{
  myTitle=func_name;
  setFeature(0,func_ikw,func_name);
}





MvDataUncondFunctionFeature_t::MvDataUncondFunctionFeature_t(const string &title,int func_ikw,const string &func_name,
                                 string x_name, MvDimension_e x_dim, vector<string>& x_arg_vect,
                                 string y_name, MvDimension_e y_dim, vector<string>& y_arg_vect,
                                 string z_name, MvDimension_e z_dim, vector<string>& z_arg_vect,
                                 int x_scal_ikw, const string& x_scal_name,
                                 int y_scal_ikw, const string& y_scal_name,
                                 int z_scal_ikw, const string& z_scal_name) :
  MvDataFunctionFeature_t(DFT_FUNCTION,title,func_ikw,x_scal_ikw,y_scal_ikw,z_scal_ikw),
  myXTitle(x_name),
  myYTitle(y_name),
  myZTitle(z_name),
  myXDimension(x_dim),
  myYDimension(y_dim),
  myZDimension(z_dim),
  myXArgVect(x_arg_vect),
  myYArgVect(y_arg_vect),
  myZArgVect(z_arg_vect)
{
  myTitle=func_name;
  setFeature(0,func_ikw,func_name);
  if(myXScaleInd>0) setFeature(myXScaleInd,x_scal_ikw,x_scal_name);
  if(myYScaleInd>0) setFeature(myYScaleInd,y_scal_ikw,y_scal_name);
  if(myZScaleInd>0) setFeature(myZScaleInd,z_scal_ikw,z_scal_name);
}



MvDataUncondFunctionFeature_t::~MvDataUncondFunctionFeature_t() {
}


/* --------- Output in an output stream --------- */

ostream &MvDataUncondFunctionFeature_t::display(ostream &os,const MvDescriptor_t &descr,int level) const {
  int i;
  string arg_list = "";
  for(i=0;i<level;i++) os << "  ";
  display_props(os);
  //
  os << "FUNCTION(TITLE=\"" << getTitle() << "\""
     << ",KEYWORD="         << descr.getSKeyword(getFunctionIKeyword())
     << ") {"               << endl;
  //
  for(i=0;i<=level;i++) os << "  ";
  os << "X_TITLE     = \"" << myXTitle << "\"" << endl;
  //
  for(i=0;i<=level;i++) os << "  ";
  os << "Y_TITLE     = \"" << myYTitle << "\"" << endl;
  //
  for (i = 0; i <= level; i++) os << "  ";
  os << "Z_TITLE     = \"" << myZTitle << "\"" << endl;
  //
  for(i=0;i<=level;i++) os << "  ";  
  os << "X_DIMENSION = \"" << MV_get_dimension(myXDimension);
  arg_list = "";
  vector<string> arg_vect = getFunctionXArgVect();

  if (!arg_vect.empty())
  {
      convert_vect_to_string(arg_list, arg_vect);
      os << arg_list;
  }
  os << "\";" << endl;

  //
  for(i=0;i<=level;i++) os << "  ";  
  os << "Y_DIMENSION = \"" << MV_get_dimension(myYDimension);
  arg_list = "";
  arg_vect = getFunctionYArgVect();
  if (!arg_list.empty())
  {
      convert_vect_to_string(arg_list, arg_vect);
      os << arg_list;
  }
  os << "\";" << endl;

  //
  for (i = 0; i <= level; i++) os << "  ";
  os << "Z_DIMENSION = \"" << MV_get_dimension(myZDimension);
  arg_list = "";
  arg_vect = getFunctionZArgVect();

  if (!arg_list.empty())
  {
      convert_vect_to_string(arg_list, arg_vect);
      os << arg_list;
  }
  os << "\";" << endl;

  //
  if(isScaled()) {
    for(i=0;i<=level;i++) os << "  ";  
    os << "SCALING     = (KEYWORD=" << descr.getSKeyword(getScalingIKeyword()) 
       << ",TITLE=\""               << getName(1) 
       << "\");"                    << endl;
  }
  //
  
  MvFunctionType_e a_func_type=getFunctionType();
  if(a_func_type==FFTY_TABLE || a_func_type==FFTY_FUNCTION_OR_TABLE) { /* PM:0268:27/02/2006 */
    for(i=0;i<=level;i++) os << "  ";
    os << "SUB_X_TITLE     = \"" << mySubXTitle << "\"" << endl;
    //
    for(i=0;i<=level;i++) os << "  ";
    os << "SUB_Y_TITLE     = \"" << mySubYTitle << "\"" << endl;
    //
    for (i = 0; i <= level; i++) os << "  ";
    os << "SUB_Z_TITLE     = \"" << mySubZTitle << "\"" << endl;
    //
    for(i=0;i<=level;i++) os << "  ";  
    os << "SUB_X_DIMENSION = \"" << MV_get_dimension(mySubXDimension);
    arg_list = "";
    arg_vect = mySubXArgVect;

    if (!arg_vect.empty())
    {
        convert_vect_to_string(arg_list, arg_vect);
        os << arg_list;
    }
    os << "\";" << endl;
    //
    for(i=0;i<=level;i++) os << "  ";  
    os << "SUB_Y_DIMENSION = \"" << MV_get_dimension(mySubYDimension);
    arg_list = "";
    arg_vect = mySubYArgVect;

    if (!arg_vect.empty())
    {
        convert_vect_to_string(arg_list, arg_vect);
        os << arg_list;
    }
    os << "\";" << endl;

    //
    for (i = 0; i <= level; i++) os << "  ";
    os << "SUB_Z_DIMENSION = \"" << MV_get_dimension(mySubZDimension);
    arg_list = "";
    arg_vect = mySubZArgVect;

    if (!arg_vect.empty())
    {
        convert_vect_to_string(arg_list, arg_vect);
        os << arg_list;
    }
    os << "\";" << endl;
  }
  
  //
  for(i=0;i<level;i++) os << "  ";  
  os << "}";
  return os;
}
