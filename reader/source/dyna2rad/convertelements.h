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

#ifndef SDID2R_CONVERTELEMENTS_H
#define SDID2R_CONVERTELEMENTS_H
#include <dyna2rad/convertentities.h>
#include <dyna2rad/convertutils.h>

namespace sdiD2R
{
    class ConvertElem : private ConvertEntity
    {
    public:
        ConvertElem(sdi::ModelViewRead* lsdynaModel, sdi::ModelViewEdit* radiossModel) :
            ConvertEntity(
                "*ELEMENT",
                "/SHELL",
                lsdynaModel->GetEntityType("*ELEMENT"),
                radiossModel->GetEntityType("/SHELL"),
                lsdynaModel,
                radiossModel
            ),
            p_ConvertUtils(lsdynaModel, radiossModel)
        {
        }
        void ConvertElems();

        ~ConvertElem() {}

    private:
        ConvertUtils p_ConvertUtils;

        void ConvertEntities() override;

        void ConvertSeatbeltAccelerometer();

        void p_CreateThAccel();

        void ConvertSeatbeltSlipring();

        void ConvertSeatbeltSensor();

        void ConvertSeatbeltRetractor();

        void p_UpdateVIDForSprings(const sdi::ElementRead& lsdElemRead, const sdiUIntList& elemNodes, sdi::HandleElementEdit& radElemHEdit);

        void p_CreateThSlipring();
    };
}

#endif // !SDID2R_CONVERTELEMENTS_H
