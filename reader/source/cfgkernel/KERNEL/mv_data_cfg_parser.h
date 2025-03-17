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
#ifndef MV_DATA_CFG_PARSER_H
#define MV_DATA_CFG_PARSER_H


#include <PARSER/mv_parser_base.h>


#include <KERNEL_BASE/Structure_expression.h>
#include <unordered_map>
#include "mv_pre_datas_hierarchy.h"
#include "cfg_kernel.h"
class MvDataCfgParser_t : public MvParserBase_t {
public:  // Constructor and destructor
  MvDataCfgParser_t(const string &fullname);
public:  // Parsing the file and getting the hierarchy
  MvPreDatasHierarchy_t *getDatasHierarchyPtr(CFGKernel& cfgkernel, const string &title);
  void readFlagInformation(CFGKernel& cfgkernel, int *flag);
  SyntaxInfo* getSyntaxInfoPtr();
private: // Private parsing methods
  
  MvPreDatasHierarchy_t *getDatasHierarchyPtr(CFGKernel& cfgkernel, object_type_e current_type,MvDomain_e current_domain=DOM_COMMON, int parent_flag =0, char un_rep_char= '\0');
  MvDomain_e             getNextDomain();
  
public:
  static void StoreFullTypesForPostTreat(void *dft, vector< pair<string, comparator_e> > &fulltype);
protected: // Messages
  inline const char *getMsg(int i) const { return MV_get_msg_array(MSGT_KERNEL)[i]; }

};

#endif //MV_DATA_CFG_PARSER_H




