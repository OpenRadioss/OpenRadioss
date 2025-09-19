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

#ifndef SDID2R_CONVERTPROPS_H
#define SDID2R_CONVERTPROPS_H

#include <dyna2rad/convertentities.h>
#include <dyna2rad/convertutils.h>
#include <dyna2rad/propertymaterialrelation.h>
#include <dyna2rad/unitsystemdefaults.h>

namespace sdiD2R
{
    class ConvertProp : private ConvertEntity
    {
    public:
        ConvertProp(sdi::ModelViewRead* lsdynaModel, sdi::ModelViewEdit* radiossModel, PropertyMaterialRelation& propMatRelationDB, const Units& unitSyst) :
            ConvertEntity(
                "*SECTION",
                "/PROP",
                lsdynaModel->GetEntityType("*SECTION"),
                radiossModel->GetEntityType("/PROP"),
                lsdynaModel,
                radiossModel
            ),
            p_PropMatRelationDB(propMatRelationDB),
            p_ConvertUtils(lsdynaModel, radiossModel),
            p_CurrentUnitSyst(unitSyst)
        {
        }
        void ConvertProperties();
        ~ConvertProp() {}
    private:
        PropertyMaterialRelation p_PropMatRelationDB;
        sdiUIntList p_convertedProps;
        sdi::HandleRead p_PartBeingConverted;
        ConvertUtils p_ConvertUtils;
        Units p_CurrentUnitSyst;

        void ConvertEntities() override;

        void p_ConvertPropBasedOnCard(const sdi::EntityRead& dynaProp, const sdiString& sourceCard, sdi::HandleEdit& radProp, sdiString &destCard);

        void p_SetDefaultValuesForTYPE13(sdi::HandleEdit& radProp);

        void p_ConvertSectionShell(const sdi::EntityRead& matEntityRead, const sdi::EntityRead& dynaProp, const sdiString& sourceCard, sdiString& destCard, sdi::HandleEdit& radProp);

        void p_ConvertSectionTShell(const sdi::EntityRead& matEntityRead, const sdi::EntityRead& dynaProp, const sdiString& sourceCard, sdiString& destCard, sdi::HandleEdit& radProp);

        void p_ConvertSectionDiscrete(const sdi::EntityRead& matEntityRead, const sdi::EntityRead& dynaProp, sdiString& destCard, sdi::HandleEdit& radProp);

        void p_ConvertSectionBeam(const sdi::EntityRead& matEntityRead, const sdi::EntityRead& dynaProp, sdiString& destCard, sdi::HandleEdit& radProp);
        
        void ConvertSecShellsRelatedMatFabric(const sdi::EntityRead& matEntityRead, const sdi::EntityRead& dynaProp, sdiString& destCard, sdi::HandleEdit& radProp);

        void ConvertSecShells16RelatedMatFabric(const sdi::EntityRead& matEntityRead, const sdi::EntityRead& dynaProp, sdiString& destCard, sdi::HandleEdit& radProp);

        void ConvertSecShellsRelatedMatLaminate(const sdi::EntityRead& matEntityRead, const sdi::EntityRead& dynaProp, sdiString& destCard, sdi::HandleEdit& radProp);

        void ConvertSecShellsRelatedIntegrationShell(const sdiString& sourceCard,const sdi::EntityRead& matEntityRead,const sdi::EntityRead& IridRead,
                                                     const  sdi::EntityRead& dynaProp, sdiString& destCard, sdi::HandleEdit& radProp);

        void ConvertSectionBeamToSpringBeam(const sdi::EntityRead& matEntityRead, const sdi::EntityRead& dynaProp, sdiString& destCard, sdi::HandleEdit& radProp);

        void CreateDefSolidForRadSolidProp();

        void p_CopyNumIntPtsFromMatAddErosion(const sdi::EntityRead& matEntityRead, sdi::HandleEdit& radProp,const sdi::EntityRead& dynaProp, int nip);

        void p_CopyNumIntPtsFromMatPlasticity(const sdi::EntityRead& matEntityRead, sdi::HandleEdit& radProp, int nip);

        void p_ConvertSectionSeatbeltToProp23(const sdi::EntityRead& matEntityRead, const sdi::EntityRead& dynaProp, sdiString& destCard, sdi::HandleEdit& radProp);

        void ConvertSecBeamRelatedMatMuscle(const sdi::EntityRead& matEntityRead, const sdi::EntityRead& dynaProp, sdiString& destCard, sdi::HandleEdit& radProp);

        void p_ConvertPartComposites(const sdi::EntityRead& partEntRead, sdi::HandleEdit& radProp);

        void ConvertSolidOrthType6(const sdi::EntityRead& matEntityRead, const sdi::EntityRead& dynaProp, sdiString& destCard, sdi::HandleEdit& radProp);

        void ConvertSecShellsRelatedMatOrthotropic(const sdi::EntityRead& matEntityRead, const sdi::EntityRead& dynaProp, sdiString& destCard, sdi::HandleEdit& radProp);

        void ConvertSecShellsRelatedMatSeatbelt2D(const sdi::EntityRead& matEntityRead, const sdi::EntityRead& dynaProp, sdiString& destCard, sdi::HandleEdit& radProp);

        void p_ConvertSectionDiscreteMuscle(const sdi::EntityRead& matEntityRead, const sdi::EntityRead& dynaProp, sdiString& destCard, sdi::HandleEdit& radProp);

        void p_ConvertSectionSph(const sdi::EntityRead& matEntityRead, const sdi::EntityRead& dynaProp, sdiString& destCard, sdi::HandleEdit& radProp);

        void ConvertSecTShellsRelatedMatOrthotropic(const sdi::EntityRead& matEntityRead, const sdi::EntityRead& dynaProp, sdiString& destCard, sdi::HandleEdit& radProp);

        void ConvertSecTShellsRelatedMatIsotropic(const sdi::EntityRead& matEntityRead, const sdi::EntityRead& dynaProp, sdiString& destCard, sdi::HandleEdit& radProp);

        void ConvertSecTShellsRelatedMatComposite(const sdi::EntityRead& matEntityRead, const sdi::EntityRead& dynaProp, sdiString& destCard, sdi::HandleEdit& radProp);

    };
}
#endif // !SDID2R_CONVERTPROPS_H
