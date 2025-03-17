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

#ifndef SDID2R_CONVERTTIMEHISTORY_H
#define SDID2R_CONVERTTIMEHISTORY_H

#include <dyna2rad/convertentities.h>
#include <dyna2rad/convertutils.h>

namespace sdiD2R
{
    class ConvertTimeHistory : private ConvertEntity
    {
    public:
        ConvertTimeHistory(sdi::ModelViewRead* lsdynaModel, sdi::ModelViewEdit* radiossModel) :
            ConvertEntity(
                "*DATABASE_HISTORY",
                "/TH",
                lsdynaModel->GetEntityType("*DATABASE_HISTORY"),
                radiossModel->GetEntityType("/TH"),
                lsdynaModel,
                radiossModel
            ),
            p_ConvertUtils(lsdynaModel, radiossModel)
        {
        }
        void ConvertTimeHistories();
        ~ConvertTimeHistory() {}

    private:
        ConvertUtils p_ConvertUtils;

        void ConvertEntities() override;

        void p_CreateDefaultTHPart();

        void p_ConvertAllDBHistroy();

        void p_UpdateThBasedOnREFFlag(const sdi::EntityRead& dynaDH, const int& idArrSIze, const sdiValueEntityList& skewList, sdi::EntityEdit& radTHEdit);
    };
}


#endif // !SDID2R_CONVERTTIMEHISTORY_H

