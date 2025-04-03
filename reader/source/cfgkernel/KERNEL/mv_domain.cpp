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



#include <UTILS/mv_string.h>
#include <UTILS/mv_stl_various.h>

#include "mv_domain.h"

typedef map<string,MvDomain_e> MvStrDomainMap_t;
typedef map<MvDomain_e,string> MvDomainStrMap_t;


class MvDomainMap_t {
public:
  MvDomainMap_t();
  ~MvDomainMap_t();
public:
  MvDomain_e    getDomain(const string &keyword) const;
  const string &getDomain(MvDomain_e domain)     const;
private:
  void InitStrDomainMap();
  void InitDomainStrMap();
private:
  MvStrDomainMap_t myStrDomainMap;
  MvDomainStrMap_t myDomainStrMap;
};

MvDomainMap_t::MvDomainMap_t() {
  InitStrDomainMap();
  InitDomainStrMap();
}

MvDomainMap_t::~MvDomainMap_t() {
}

void MvDomainMap_t::InitStrDomainMap() {
  myStrDomainMap["UNKNOWN"]    = DOM_UNKNOWN;
  myStrDomainMap["COMMON"]     = DOM_COMMON;
  myStrDomainMap["ALE"]        = DOM_ALE;
  myStrDomainMap["PARAMETERS"] = DOM_PARAMETERS;
  myStrDomainMap["LAST"]       = DOM_LAST;
}

MvDomain_e MvDomainMap_t::getDomain(const string &keyword) const {
  MvStrDomainMap_t::const_iterator it=myStrDomainMap.find(keyword);
  if(it!=myStrDomainMap.end()) return (*it).second;
  return DOM_UNKNOWN;
}

const string &MvDomainMap_t::getDomain(MvDomain_e domain) const {
  MvDomainStrMap_t::const_iterator it=myDomainStrMap.find(domain);
  if(it!=myDomainStrMap.end()) return (*it).second;
  return (*(myDomainStrMap.find(DOM_UNKNOWN))).second;
}

void MvDomainMap_t::InitDomainStrMap() {
  for(MvStrDomainMap_t::const_iterator it=myStrDomainMap.begin();it!=myStrDomainMap.end();++it) {
    myDomainStrMap[(*it).second]=(*it).first;
  }
}

static const MvDomainMap_t &get_domain_map() {
  static const MvDomainMap_t MV_DOMAIN_MAP;
  return MV_DOMAIN_MAP;
}

MvDomain_e MV_get_domain(const string &keyword) {
  return get_domain_map().getDomain(keyword);
}

const string &MV_get_domain(MvDomain_e domain) {
  return get_domain_map().getDomain(domain);
}


int MV_get_domains(const string &domains) {
  int a_domains=0;
  //
  unsigned int a_pos=(unsigned int)(domains.find("|"));
  if(a_pos==domains.npos) {
    MvDomain_e a_domain=MV_get_domain(domains);
    if(a_domain!=DOM_UNKNOWN) a_domains|=a_domain;
  } else {
    string     a_first_domain = domains.substr(0,a_pos++);
    string     a_last_domains = domains.substr(a_pos,domains.size()-a_pos);
    MvDomain_e a_domain       = MV_get_domain(a_first_domain);
    if(a_domain!=DOM_UNKNOWN) a_domains|=a_domain;
    a_domains|=MV_get_domains(a_last_domains);
  }
  //
  return a_domains;
}



string MV_get_str_domains(int domains) {
  string a_result="";
  //
  MvDomain_e a_domain_begin = MV_get_domain_begin();
  MvDomain_e a_domain_end   = MV_get_domain_end();
  MvDomain_e a_domain;
  for(a_domain=a_domain_begin;a_domain!=a_domain_end;a_domain=MV_get_domain_next(a_domain)) {
    if(domains & a_domain) {
      if(!a_result.empty()) a_result+="|";
      a_result+=MV_get_domain(a_domain);
    }
  }//
  return a_result;
}


int MV_get_all_domains() {
  int a_all_domains=0;
  for(int i=(int)(DOM_UNKNOWN+1);i<(int)DOM_LAST;i<<=1) a_all_domains|=i;
  return a_all_domains;
}

MvDomain_e MV_get_domain_begin() {
  return (MvDomain_e)(DOM_UNKNOWN+1);
}

MvDomain_e MV_get_domain_next(MvDomain_e domain) {
  return (MvDomain_e)(domain<<1);
}

MvDomain_e MV_get_domain_end() {
  return DOM_LAST;
}

MvIntDomainSet_t *MV_get_int_domains(int domain, MvIntDomainSet_t *idom_set_p) {
  if(idom_set_p==NULL) idom_set_p = new MvIntDomainSet_t();
  //  
  for(int i=(int)(DOM_UNKNOWN+1);i<(int)DOM_LAST;i<<=1) {
    if( domain & i ) idom_set_p->insert(i);
  }
  return idom_set_p;
}

MvDomainSet_t *MV_get_domains(int domain, MvDomainSet_t *dom_set_p) {
  if(dom_set_p==NULL) dom_set_p = new MvDomainSet_t();
  //  
  for(int i=(int)(DOM_UNKNOWN+1);i<(int)DOM_LAST;i<<=1) {
    if( domain & i ) dom_set_p->insert((MvDomain_e) i);
  }
  return dom_set_p;
}

int MV_set_int_domains(const MvIntDomainSet_t *idom_set_p) {
  int a_domain=0;
  MvIntDomainSet_t::const_iterator a_ids_begin=idom_set_p->begin();
  MvIntDomainSet_t::const_iterator a_ids_end  =idom_set_p->end();
  MvIntDomainSet_t::const_iterator a_ids_it;
  for(a_ids_it=a_ids_begin;a_ids_it!=a_ids_end;++a_ids_it) {
    a_domain|=(*a_ids_it);
  }
  return a_domain;
}

int MV_set_int_domains(const MvDomainSet_t *dom_set_p) {
  int a_domain=0;
  MvDomainSet_t::const_iterator a_ds_begin=dom_set_p->begin();
  MvDomainSet_t::const_iterator a_ds_end  =dom_set_p->end();
  MvDomainSet_t::const_iterator a_ds_it;
  for(a_ds_it=a_ds_begin;a_ds_it!=a_ds_end;++a_ds_it) {
    a_domain|=((int) *a_ds_it);
  }
  return a_domain;
}




