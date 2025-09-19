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

#ifndef SDId2R_CONVERTRWALLS_H
#define SDId2R_CONVERTRWALLS_H


#include <dyna2rad/convertentities.h>
#include <dyna2rad/convertutils.h>
#include <typedef.h>

namespace sdiD2R
{
    class ConvertRwall : private ConvertEntity
    {
    public:
        ConvertRwall(sdi::ModelViewRead* lsdynaModel, sdi::ModelViewEdit* radiossModel) :
            ConvertEntity(
                "*RIGIDWALL",
                "/RWALL",
                lsdynaModel->GetEntityType("*RIGIDWALL"),
                radiossModel->GetEntityType("/RWALL"),
                lsdynaModel,
                radiossModel
            ),
            p_ConvertUtils(lsdynaModel, radiossModel)
        {
        }
        void ConvertRigidwalls();

        ~ConvertRwall() {}

    private:

        ConvertUtils p_ConvertUtils;

        std::map<unsigned int, sdi::HandleEdit> RwallIdVsImpHandleMap;

        void ConvertEntities() override;

        void p_ConvertRwallPlanar();

        void p_ConvertRwallGeometric();

        void p_ConvertRwallGeomPRISM(const sdi::EntityRead& dynaRwallRead, const sdi::EntityEdit& radRwallEdit, const sdiTripleList& tripleList);

        void p_ConvertRwallSets(const sdi::EntityRead& dynaRwallRead, sdi::EntityEdit& radRwallEdit);

        void p_ConvertRwallMotion(const sdi::EntityRead& dynaRwallRead, sdi::EntityEdit& radRwallEdit);

        void p_CreateThRwall();

    };
}

#endif // !SDId2R_CONVERTRWALLS_H
