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
#ifndef MV_DOMAIN_H
#define MV_DOMAIN_H


#include <UTILS/mv_string.h>
#include <UTILS/mv_stl_various.h>


#include <HCDI/hcdi.h>
/**@name Domain*/
//@{

/// Domain
enum MvDomain_s {
  /** */ DOM_UNKNOWN    =      0,
  /** */ DOM_COMMON     =    0x1,
  /** */ DOM_ALE        =    0x8,
  /** */ DOM_PARAMETERS = 0x4000,
  /** */ DOM_LAST       = 0x8000
};

/// Domain
typedef enum MvDomain_s MvDomain_e;
/// Set of integer domains
typedef set<int> MvIntDomainSet_t;
/// Set of domains
typedef set<MvDomain_e> MvDomainSet_t;

/// Getting domain from keyword
MvDomain_e     MV_get_domain(const string &keyword);
/// Getting keyword from domain
const string  &MV_get_domain(MvDomain_e domain);



/// Getting int domains from string domains
int    MV_get_domains(const string &domains);
/// Getting string domains from int domains
string MV_get_str_domains(int domains);


/// Getting all domains
int MV_get_all_domains();

/// Gets the first domain after DOM_UNKNOWN in the enum
HC_DATA_DLL_API MvDomain_e MV_get_domain_begin();
/// Incerments a domain (*=2)
HC_DATA_DLL_API MvDomain_e MV_get_domain_next(MvDomain_e domain);
/// Gets the first domain after the last one in the enum (always returns DOM_LAST)
HC_DATA_DLL_API MvDomain_e MV_get_domain_end();

/// Getting a set of int domains from an int combination
MvIntDomainSet_t *MV_get_int_domains(int domain, MvIntDomainSet_t *idom_set_p=NULL);
/// Getting a set of MvDomain_e from an int combination
MvDomainSet_t    *MV_get_domains(int domain, MvDomainSet_t *dom_set_p=NULL);

/// Getting an int combination from a set of int domain
int MV_set_int_domains(const MvIntDomainSet_t *idom_set_p);
/// Getting an int combination from a set of MvDomain_e
int MV_set_int_domains(const MvDomainSet_t *dom_set_p);

/// Getting an int combination from a set of int domain
inline int MV_set_int_domains(const MvIntDomainSet_t &idom_set) { return MV_set_int_domains(&idom_set); }
/// Getting an int combination from a set of MvDomain_e
inline int MV_set_int_domains(const MvDomainSet_t &dom_set)     { return MV_set_int_domains(&dom_set); }

//@}

#endif //MV_DOMAIN_H




