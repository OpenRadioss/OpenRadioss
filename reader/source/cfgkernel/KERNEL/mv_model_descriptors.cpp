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

#include <UTILS/mv_stl_various.h>
#include <UTILS/error.h>

#include "mv_model_descriptors.h" 
#include "mv_pre_descriptor.h"
#include "HCDI/hcdi_multicfgkernelmgr.h"

typedef map<MvControlCardType_e,string> LocCCT2S_t;
typedef map<string,MvControlCardType_e> LocS2CCT_t;

 
typedef map<MvEngineControlCardType_e,string> LocECCT2S_t;
typedef map<string,MvEngineControlCardType_e> LocS2ECCT_t;


typedef map<int,string>                 LocIkw2Skw_t;
typedef map<string,int>                 LocSkw2Ikw_t;

class MvDescriptorMap_t;





static const LocIkw2Skw_t      &loc_get_ikw2skw_map();
static const LocSkw2Ikw_t      &loc_get_skw2ikw_map();


//static MvDescriptorMap_t *g_descriptors_p[FF_LAST]={ NULL };

/* --------- Model descriptors (control cards) --------- */

void CFGKernel::init_model_descriptors() {
  loc_manage_map(0);
}

void CFGKernel::close_model_descriptors() {
  loc_manage_map(1);
}

int MV_get_model_ikeyword(const string &skeyword) {
  const LocSkw2Ikw_t &a_keywords=loc_get_skw2ikw_map();
  LocSkw2Ikw_t::const_iterator a_it=a_keywords.find(skeyword);
  if(a_it==a_keywords.end()) return END_ARGS;
  return (*a_it).second;
}

const string &MV_get_model_skeyword(int ikeyword) {
  static string a_unknown="UNKNOWN";
  const LocIkw2Skw_t &a_keywords=loc_get_ikw2skw_map();
  LocIkw2Skw_t::const_iterator a_it=a_keywords.find(ikeyword);
  if(a_it==a_keywords.end()) return a_unknown;
  return (*a_it).second;  
}




/* --------- Static functions --------- */


static const LocIkw2Skw_t &loc_get_ikw2skw_map() {
  static LocIkw2Skw_t a_ikw2skw;
  static bool         a_first=true;
  //
  if(a_first) {
    // Model general attributes
    a_first=false;
  }
  //
  return a_ikw2skw;
}

static const LocSkw2Ikw_t &loc_get_skw2ikw_map() {
  static LocSkw2Ikw_t a_skw2ikw;
  static bool       a_first=true;
  //
  if(a_first) {
    const LocIkw2Skw_t &a_ikw2skw=loc_get_ikw2skw_map();
    //
    LocIkw2Skw_t::const_iterator a_it_begin = a_ikw2skw.begin();
    LocIkw2Skw_t::const_iterator a_it_end   = a_ikw2skw.end();
    LocIkw2Skw_t::const_iterator a_it;
    for(a_it=a_it_begin;a_it!=a_it_end;++a_it) a_skw2ikw[(*a_it).second]=(*a_it).first;
    //
    a_first=false;
  }
  //
  return a_skw2ikw;
}

const MvDescriptorMap_t * CFGKernel::loc_manage_map(int option) {
 // const MvDescriptorMap_t *a_descriptors_p= p_descriptors_p;
  //
  switch(option) {
  case 0:
    if(p_descriptors_p ==NULL) p_descriptors_p =new MvDescriptorMap_t();
    break;
  case 1:
    if(p_descriptors_p !=NULL) { delete p_descriptors_p; p_descriptors_p =NULL; }
    break;
  }
  //
  return p_descriptors_p;
}


MvDescriptorMap_t::~MvDescriptorMap_t() {
    MvPreDescriptorMap_t::iterator a_it_begin = myPreDescriptors.begin();
    MvPreDescriptorMap_t::iterator a_it_end = myPreDescriptors.end();
    MvPreDescriptorMap_t::iterator a_it;
    for (a_it = a_it_begin; a_it != a_it_end; ++a_it) {
        MvPreDescriptor_t* a_pre_descriptor_pf = (*a_it).second;
        a_pre_descriptor_pf->deleteDescriptor();
        delete a_pre_descriptor_pf;
    }
}

