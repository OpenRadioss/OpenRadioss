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

#ifndef SDID2R_CONVERTRGDBODIES_H
#define SDID2R_CONVERTRGDBODIES_H

#include <dyna2rad/convertentities.h>
#include <dyna2rad/convertutils.h>

namespace sdiD2R
{
    class ConvertRigid : private ConvertEntity
    {
    public:
        ConvertRigid(sdi::ModelViewRead* lsdynaModel, sdi::ModelViewEdit* radiossModel) :
            ConvertEntity(
                "*CONSTRAINED_NODAL_RIGID_BODY",
                "/RBODY",
                lsdynaModel->GetEntityType("*CONSTRAINED_NODAL_RIGID_BODY"),
                radiossModel->GetEntityType("/RBODY"),
                lsdynaModel,
                radiossModel
            ),
            p_ConvertUtils(lsdynaModel, radiossModel)
        {
        }
        void ConvertAllRigids();

        ~ConvertRigid() {}

    private:
        ConvertUtils p_ConvertUtils;

        sdiConvert::ContainUintVsHandleRead mapPartIdVsRbodyHandle;

        sdiUIntList p_IprtRbodies;

        void ConvertEntities() override;

        void ConvertConstrainedNodalRbodies();

        void ConvertMatsRigid();

        void ConvertRigidMaterials(sdi::SelectionRead& selMatRigid);

        void ConvertConstrainedExtraNodes();

        void ConvertRigidBodies();

        void ConvertConstrainedGeneralizedWeldSpot();

        void HandleInertiaOption(const sdi::EntityRead& lsdRgdBody, sdi::EntityEdit& nsidHEdit, sdi::EntityEdit& radRbody);

        void HandleSPCOption(const sdi::EntityRead& lsdRgdBody, sdi::EntityEdit& radRbody);

        void p_CreateTHRBody();

        void p_CreateNewSkewForIRCSFlag(const sdi::EntityRead& lsdRgdBody, sdi::EntityEdit& radRbodyEdit );

        void p_PushPnodeIntoSlaveSet(const unsigned int& pnodeId, const sdi::HandleRead lsdSlaveSet, sdi::EntityEdit& radSlaveSet);
    };
}

#endif // !SDID2R_CONVERTRGDBODIES_H
