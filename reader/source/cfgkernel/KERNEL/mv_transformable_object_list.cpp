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


#include <PARSER/mv_parser_base.h>

#include <assert.h>
#include "mv_transformable_object_list.h"

//initialize the static member
MvTransformableObject * MvTransformableObject::_instance = NULL;

class MvTransformableObjectParser_t: public MvParserBase_t
{
public:    // Constructor and destructor
  MvTransformableObjectParser_t(const string &fullname):MvParserBase_t(fullname){}
  virtual ~MvTransformableObjectParser_t(){}
  inline string GetNextString(){ return getNextString();}
  inline bool Seof(){return seof(false);}
};

//++////////////////////////////////////////////////////////////////////////////
//Function Descriptions:
//     get the only instance of this class
//Parameters:
//Return value:
//     MvTransformableObject *: pointer to the instance
//Modification History:

//--///////////////////////////////////////////////////////////////////////////
MvTransformableObject * MvTransformableObject::GetInstance()
{
   if(_instance==NULL)
   {
      _instance = new MvTransformableObject();
   }

   return _instance;
}

//++////////////////////////////////////////////////////////////////////////////
//Function Descriptions:
//     Contructor
//Parameters:
//Modification History:

//--///////////////////////////////////////////////////////////////////////////
MvTransformableObject::MvTransformableObject()
{
}
//++////////////////////////////////////////////////////////////////////////////
//Function Descriptions:
//     Destructor, clear the _transformableObjVector
//Parameters:
//Modification History:

//--///////////////////////////////////////////////////////////////////////////
MvTransformableObject::~MvTransformableObject()
{
   _transformableObjVector.clear();
}

//++////////////////////////////////////////////////////////////////////////////
//Function Descriptions:
//     fill the transformable object vector with transformableObjectList.cfg
//Parameters:
//Return value:
//Modification History:

//--///////////////////////////////////////////////////////////////////////////
void MvTransformableObject::InitTransformableObjectVector()
{
   string a_fullname = GetFullName();
   class MvTransformableObjectParser_t a_file = MvTransformableObjectParser_t(a_fullname);
   while(!a_file.Seof()){
      string a_string = a_file.GetNextString();
      _transformableObjVector.push_back(a_string);
   }
}

//++////////////////////////////////////////////////////////////////////////////
//Function Descriptions:
//     get full name (full path +filename) of the transformableObjectList.cfg
//Parameters:
//Return value:
//     string fullname
//Modification History:

//--///////////////////////////////////////////////////////////////////////////
string MvTransformableObject::GetFullName()
{
    string a_fullname = "";
    string a_config_dir = MV_get_config_dir();

    //if it's relative path
    if(a_config_dir.substr(0,1)==".")
    {
       string a_cur_dir = MV_get_current_dir();

       //string a_filename = MV_get_cfg_file("transformableObjectList.cfg");
       string a_filename = a_config_dir + "/transformableObjectList.cfg";

       //remove the "." from the relative path like ./aaaa/bbbb
       a_fullname = a_cur_dir + a_filename.substr(1);   
    }
    else
    {
       a_fullname = a_config_dir + "/transformableObjectList.cfg";
    }

    return a_fullname;
}

//++////////////////////////////////////////////////////////////////////////////
//Function Descriptions:
//     Get Transformable Object Vector
//Parameters:
//Return value:
//     transformableObjectVector 
//Modification History:

//--///////////////////////////////////////////////////////////////////////////
const transformableObjectVector & MvTransformableObject::GetTransformableObjectVector()
{
   return _transformableObjVector;
}

