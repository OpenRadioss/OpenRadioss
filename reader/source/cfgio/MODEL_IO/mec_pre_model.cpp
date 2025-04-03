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


#include "mec_pre_model.h"


/* --------- Constructors & destructor --------- */

MECPreModel::MECPreModel() :
  myPreObjects(),
  myCurrentOType(""),
  myCurrentIt(),
  myCurrentCOType(""),
  myCurrentCIt()
{}

MECPreModel::~MECPreModel() {
}


/* --------- Adding new objects --------- */

void MECPreModel::Reserve(const string &otype,int nb_pre_objects) {
  GetObjectArray(otype).reserve(nb_pre_objects);
}

void MECPreModel::AddPreObject(const string &otype,const IMECPreObject &pre_object) {
  const IMECPreObject* p_pre_object = &pre_object;
  IMECPreObject *a_pre_object_p=HCDI_GetPreObjectHandleFromOtherPreObject((IMECPreObject *)p_pre_object);
  GetObjectArray(otype).push_back(a_pre_object_p);
}


/* --------- Getting objects --------- */

int MECPreModel::GetNbPreObjects(const string &otype) const {
  return (int)(GetObjectArray(otype)).size();
}

const IMECPreObject &MECPreModel::GetPreObject(const string &otype,int ind) const {
  return *((GetObjectArray(otype))[ind]);
}

IMECPreObject *MECPreModel::GetPreObjectData(const string &otype,int ind,IMECPreObject *pre_object_p) const {
  IMECPreObject *a_pre_object_p=(pre_object_p==NULL ? HCDI_GetPreObjectHandle("","","",0,0) : pre_object_p);
  //
  const IMECPreObject &a_pre_object=GetPreObject(otype,ind);
  a_pre_object_p->Reserve(a_pre_object);
  a_pre_object_p->Copy(a_pre_object);
  //
  return a_pre_object_p;
}


/* --------- Access to arrays of objects --------- */

MECPreModel::MyPreObjectArray_t &MECPreModel::GetObjectArray(const string &otype) {
  if(otype!=myCurrentOType) {
    myCurrentIt=myPreObjects.find(myCurrentOType=otype);
    //
    if(myCurrentIt==myPreObjects.end()) {
      myPreObjects[myCurrentOType]=MyPreObjectArray_t();
      myCurrentIt=myPreObjects.find(myCurrentOType);
    }
  }
  //
  return (*myCurrentIt).second;
}

const MECPreModel::MyPreObjectArray_t &MECPreModel::GetObjectArray(const string &otype) const {
  if(otype!=myCurrentCOType) myCurrentCIt=myPreObjects.find(myCurrentCOType=otype);
  //
  return (*myCurrentCIt).second;
}


