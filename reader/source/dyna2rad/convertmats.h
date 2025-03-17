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

#ifndef SDID2R_CONVERTMATS_H
#define SDID2R_CONVERTMATS_H

#include <dyna2rad/convertentities.h>
#include <dyna2rad/convertutils.h>
#include <dyna2rad/propertymaterialrelation.h>
#include <dyna2rad/unitsystemdefaults.h>

namespace sdiD2R
{
    class ConvertMat : private ConvertEntity
    {
    public:
        ConvertMat(sdi::ModelViewRead* lsdynaModel, sdi::ModelViewEdit* radiossModel, 
            PropertyMaterialRelation& matPropRelationDB, const Units& unitSyst) :
            ConvertEntity(
                "*MAT",
                "/MAT",
                lsdynaModel->GetEntityType("*MAT"),
                radiossModel->GetEntityType("/MAT"),
                lsdynaModel,
                radiossModel
            ),
            p_MatPropRelationDB(matPropRelationDB),
            p_ConvertUtils(lsdynaModel, radiossModel),
            p_CurrentUnitSyst(unitSyst)
        {
        }
        void ConvertMaterials();
        ~ConvertMat() {}
    private:
        PropertyMaterialRelation p_MatPropRelationDB;
        sdiUIntList p_convertedMats;
        sdi::HandleRead p_PartBeingConverted;
        ConvertUtils p_ConvertUtils;
        Units p_CurrentUnitSyst;
        std::map<unsigned int, std::map<unsigned int, sdi::HandleEdit>> mapMatIdVsMapPropIdNewMatHandle;

        void ConvertEntities() override;

        void p_ConvertAllMatsAssociatedWithParts();

        void p_ConvertMatBasedOnCard(const sdi::EntityRead& dynaMat, sdi::HandleEdit& radMat, sdi::HandleRead& lsdEosHRead);

        void p_ConvertMatL2(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL3(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL5(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL6(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL15(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL15EOS(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL18(const sdi::EntityRead& dynaMat, sdiString& destCard, sdi::HandleEdit& radMat);

        void p_ConvertMatL19(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL24(const sdi::EntityRead& dynaMat, sdiString &destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat,
                             unsigned short int &matLawNum, unsigned short int &matLawNumChoice);

        void p_ConvertMatL26(const sdi::EntityRead& dynaMat, sdi::HandleEdit& radMat);

        void p_ConvertMatL27(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL30(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL32(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL34(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL54(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL57(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL58(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL61(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL66(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL67(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL68(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void ConvertMatL181ToMatL70(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL71(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL73(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL74(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL76(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);
       
        void p_ConvertMatL77(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL81(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat,
                             unsigned short int &matLawNum, unsigned short int &matLawNumChoice);

        void p_ConvertMatL83(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void ConvertMatL181ToMatL88(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL91_92(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL99(const sdi::EntityRead& dynaMat, sdiString& destCard, sdi::HandleEdit& radMat);

        void p_ConvertMatL100(const sdi::EntityRead& dynaMat, sdiString &destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL111(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL119(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL121(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL122(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL123(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat,
                             unsigned short int &matLawNum, unsigned short int &matLawNumChoice);

        void p_ConvertMatL138(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL154(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL169(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL181(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL183(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL196(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL224(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);
        
        void p_ConvertMatL240(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL252(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertToMatLAW44(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatAddErosion();

        void p_ConvertMatL801Seatbelt(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL63(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL77H(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL177H(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL124(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatAddDamageDiem();

        void p_ConvertMatL105(const sdi::EntityRead& dynaMat, sdiString &destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat,
                             unsigned short int &matLawNum);

        void p_ConvertMatL801Shell(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL120(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);

        void p_ConvertMatL187(const sdi::EntityRead& dynaMat, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);
        
        void p_ConvertMatL1_FLUID(const sdi::EntityRead& dynaMat, sdiString& destCard, sdi::HandleEdit& radMat);

        void p_ConvertEOS1(const sdi::EntityRead& lsdEosEntRead, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, const sdi::EntityRead& dynaMat, sdi::HandleEdit& radMat);

        void p_ConvertEOS4(const sdi::EntityRead& lsdEosEntRead, sdiString& destCard, std::multimap<std::string, std::string>& attribMap, sdi::HandleEdit& radMat);
        
        void p_ConvertMatAddThermalExpansion();

        void p_ConvertMatAddDamageGissmo();

    };
}

#endif // !SDID2R_CONVERTMATS_H
