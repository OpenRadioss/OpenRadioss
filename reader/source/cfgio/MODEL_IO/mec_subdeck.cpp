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
#include <string>
#include <UTILS/mv_stl_various.h>
#include <UTILS/memory_utils.h>
#include <KERNEL/mv_utils.h>
#include <KERNEL/mv_type.h>
#include <UTILS/set_utils.h>
#include <HCDI/hcdi_utils.h>
#include <MODEL_IO/cdr_reserveattribs.h>
#include <MODEL_IO/hcioi_utils.h>
#include "mec_subdeck.h"
#include <cstring>

subdeckVector MECSubdeck::mySubdeckVector;
int MECSubdeck::curSubdeckIdx = -1;

MECSubdeck::MECSubdeck(string name, object_type_e subtype,int Id, int uId, int vNbr, IMECPreObject* PreObject, string header, const std::vector<const char*>* vec, io_types::format_type_e deckformat):
myName(name),
mySubtype(subtype),
submodelId(Id),
unitId(uId),
versionNbr(vNbr),
myPreObject(PreObject),
myheader(header),
mydeckFormat(deckformat)
{
  parentIdx = curSubdeckIdx;

  mySubdeckVector.push_back(this);

  if (myPreObject)
  {
      submodelId = myPreObject->GetId();
      myPreObject->SetSubdeckIndex(parentIdx);
      myPreObject->SetFileIndex(parentIdx);
  }
  else
  { 
      if (subtype == HCDI_OBJ_TYPE_INCLUDEFILES)
      {
          // Creating pre-object
          const char *str_f;
          string header_ori = header;
          bool is_header_present = false;
          if (vec)
          {
              for (int i=0; i < vec->size(); i++)
              {
                  if (!strncmp(header.c_str(), (*vec)[i], strlen((*vec)[i])))
                  {
                      is_header_present = true;
                      str_f = (*vec)[i];
                      break;
                  }
              }
          }
          if (is_header_present)
              header.erase(0, std::strlen(str_f));

          string fulltype = string("/INCLUDEFILE") + string("/") + header;

          myPreObject = HCDI_GetPreObjectHandle(fulltype.c_str(), header_ori.c_str(), name.c_str(), Id, 0);
          if (myPreObject)
          {
              myPreObject->SetEntityType(subtype);
              //myPreObject->SetSubdeckIndex(parentIdx);
              myPreObject->SetFileIndex(parentIdx);
              //StorePreObject(subtype, myPreObject);
              IDescriptor* pdescrp = HCDI_GetDescriptorHandle(myPreObject->GetKernelFullType());
              if (pdescrp)
              {
                  string filemname_skey = GetAttribNameFromDrawable(pdescrp, cdr::g_AttribFileName);
                  myPreObject->AddStringValue(filemname_skey.c_str(), name.c_str());
                  filemname_skey = GetAttribNameFromDrawable(pdescrp, cdr::g_AttribFullFileName);
                  myPreObject->AddStringValue(filemname_skey.c_str(), name.c_str());

              }
          }
      }
      else if (subtype == HCDI_OBJ_TYPE_SOLVERSUBMODELS)
      {
          // Creating pre-object
          string fulltype = string("/INCLUDEFILE") + string("/") + header;

          myPreObject = HCDI_GetPreObjectHandle(fulltype.c_str(), header.c_str(), name.c_str(), Id, 0);
          if (myPreObject)
          {
              myPreObject->SetEntityType(subtype);
              //myPreObject->SetSubdeckIndex(parentIdx);
              myPreObject->SetFileIndex(parentIdx);
              //StorePreObject(subtype, myPreObject);
          }
      }
  }
  curSubdeckIdx = ((int)(mySubdeckVector.size()) -1);
}


MECSubdeck::~MECSubdeck()
{

}
//++////////////////////////////////////////////////////////////////////////////
//Function Descriptions:
//       Current subdeck idx will be changed after calling this function and
//the current subdeck idx is the precedent current subdeck idx's parent.
//Parameters:
//Return value:  
//      Int: current subdeck's idx
//Modification History:

//--////////////////////////////////////////////////////////////////////////////
int MECSubdeck::PopSubDeck()
{
   //curSubdeckIdx
   if(curSubdeckIdx>=0)
   {
      curSubdeckIdx = mySubdeckVector[curSubdeckIdx]->parentIdx;
   }

   return curSubdeckIdx;
}


void MECSubdeck::clearSubdeckVector()
//++////////////////////////////////////////////////////////////////////////////
//Function Descriptions:
//      clean the subdeckVector and initialize curSubdeckIdx to -1
//Parameters:
//Return value:  
//Modification History:

//--////////////////////////////////////////////////////////////////////////////
{
     if(!mySubdeckVector.empty())
     {
          subdeckVector::iterator It= mySubdeckVector.begin();
          for(It = mySubdeckVector.begin();It!=mySubdeckVector.end();++It)
          {
                 delete (*It);
          }
          mySubdeckVector.clear();
     }

     curSubdeckIdx = -1;
}

string MECSubdeck::getSubdeckInfo() 
{
    string str;
    str = string("Name:               ") + GetName() + string("\n");

    if (GetSubtype() == HCDI_OBJ_TYPE_INCLUDEFILES)
        string("Type                 INCLUDE\n");
    else if (GetSubtype() == HCDI_OBJ_TYPE_SOLVERSUBMODELS)
        string("Type                 SUBMODEL\n");


    str += string("File Relative Name: ") + GetFileRelativeName() + string("\n");
    str += string("Submodel ID:        ") + std::to_string(GetSubmodelId()) + string("\n");
    str += string("Parent ID:          ") + std::to_string(GetParentIdx()) + string("\n");
    str += string("Unit ID:            ") + std::to_string(GetUnitID()) + string("\n");
    str += string("Version:            ") + std::to_string(GetVersNbr()) + string("\n");
    if (GetPreObject())
        str += string(GetPreObject()->GetReport()) + string("\n");
    return str;
}
