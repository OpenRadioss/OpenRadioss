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


#ifndef MEC_PRE_MODEL_H
#define MEC_PRE_MODEL_H

#include <UTILS/mv_string.h>
#include <UTILS/mv_stl_various.h>

#include <HCDI/hcdi_mec_pre_object.h>


class MECPreModel {
 private: // Member types
  typedef vector<IMECPreObject *>         MyPreObjectArray_t;
  typedef map<string,MyPreObjectArray_t> MyPreObjects_t;
 public: // Constructors & destructor
  MECPreModel();
  ~MECPreModel();
 public: // Adding new objects
  void Reserve(const string &otype,int nb_pre_objects);
  void AddPreObject(const string &otype,const IMECPreObject &pre_object);
 public: // Getting objects
  int GetNbPreObjects(const string &otype) const;
  const IMECPreObject &GetPreObject(const string &otype,int ind) const;
  IMECPreObject *GetPreObjectData(const string &otype,int ind,IMECPreObject *pre_object_p=NULL) const;
 private: // Access to arrays of objects
  MyPreObjectArray_t &GetObjectArray(const string &otype);
  const MyPreObjectArray_t &GetObjectArray(const string &otype) const;
 private:
  MyPreObjects_t myPreObjects;
  //
  string                   myCurrentOType;
  MyPreObjects_t::iterator myCurrentIt;
  //
  mutable string                         myCurrentCOType;
  mutable MyPreObjects_t::const_iterator myCurrentCIt;
};


#endif //MEC_PRE_MODEL_H


