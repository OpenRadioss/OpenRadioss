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


#ifndef MU_UNITS_PARSER_H
#define MU_UNITS_PARSER_H

#include <PARSER/mv_parser_base.h>
#include <MUNITS/mu_quantity_manager.h>


class MuUnitsParser_t : public MvParserBase_t {
 public:
  inline MuUnitsParser_t(const string &fullname) : MvParserBase_t(fullname) {}
  inline ~MuUnitsParser_t() {}
 public:
  MuQuantityManager_t *read(MuQuantityManager_t *quantity_manager_p=NULL);
 private:
  void readQuantity(MuQuantityManager_t *quantity_manager_p);
  MuEQuantity_e getNextEQuantity();
 protected: // Messages
  inline const char *getMsg(int i) const { return MV_get_msg_array(MSGT_MUNITS)[i]; }
};


#endif //MU_UNITS_PARSER_H
