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

#include "mv_descriptor.h"
#include "mv_dependence.h"


/* --------- Output into an output stream --------- */

ostream &MvDependence_t::display(ostream &os,const MvDescriptor_t &descr) const {
  int a_nb_conds=getNbConditions();
  //
  for(int i=0;i<a_nb_conds;++i) {
    if(i) os << " else ";
    os << "if(";
    getExpressionPtr(i)->display(os,descr) << ") {" << endl;
    //
    const MvIKeywordSet_t &a_ikw_set=getIKeywords(i);
    MvIKeywordSet_t::const_iterator a_it_begin = a_ikw_set.begin();
    MvIKeywordSet_t::const_iterator a_it_end   = a_ikw_set.end();
    MvIKeywordSet_t::const_iterator a_it;
    for(a_it=a_it_begin;a_it!=a_it_end;++a_it) os << "  " << descr.getSKeyword(*a_it) << ";" << endl;
    //
    os << "}";
  }
  // Default
  const MvIKeywordSet_t &a_default_set=getDefaultIKeywords();
  if(!a_default_set.empty()) {
    os << " else {" << endl;
    MvIKeywordSet_t::const_iterator a_it_begin = a_default_set.begin();
    MvIKeywordSet_t::const_iterator a_it_end   = a_default_set.end();
    MvIKeywordSet_t::const_iterator a_it;
    for(a_it=a_it_begin;a_it!=a_it_end;++a_it) os << "  " << descr.getSKeyword(*a_it) << ";" << endl;
    os << "}";
  }
  //
  return os;
}




