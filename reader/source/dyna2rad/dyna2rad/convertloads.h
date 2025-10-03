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

#ifndef SDId2R_CONVERTLOADS_H
#define SDId2R_CONVERTLOADS_H
#include <dyna2rad/convertentities.h>
#include <dyna2rad/convertutils.h>
#include <typedef.h>

namespace sdiD2R
{
    class ConvertLoad : private ConvertEntity
    {
    public:
        ConvertLoad(sdi::ModelViewRead* lsdynaModel, sdi::ModelViewEdit* radiossModel) :
            ConvertEntity(
                "*LOAD",
                "/LOAD",
                lsdynaModel->GetEntityType("*LOAD"),
                radiossModel->GetEntityType("/LOAD"),
                lsdynaModel,
                radiossModel
            ),
            p_ConvertUtils(lsdynaModel, radiossModel)
        {
        }
        void ConvertAllLoads();

        ~ConvertLoad() {}

    private:

        ConvertUtils p_ConvertUtils;

        void ConvertEntities() override;

        void ConvertLoadNode();

        void ConvertLoadBody();

        void ConvertLoadShell();

        void ConvertLoadGravity();

        void ConvertLoadRigidBody();

        void ConvertLoadSegment();

    };
}

#endif // !SDId2R_CONVERTLOADS_H



