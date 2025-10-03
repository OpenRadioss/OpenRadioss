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

#ifndef SDId2R_CONVERTSETS_H
#define SDId2R_CONVERTSETS_H

#include <dyna2rad/convertentities.h>
#include <dyna2rad/convertutils.h>
#include <convert.h>
#include <typedef.h>

namespace sdiD2R
{
    class ConvertSet : private ConvertEntity
    {
    public:
        ConvertSet(sdi::ModelViewRead* lsdynaModel, sdi::ModelViewEdit* radiossModel) :
            ConvertEntity(
                "*SET",
                "/SET/GENERAL",
                lsdynaModel->GetEntityType("*SET"),
                radiossModel->GetEntityType("/SET/GENERAL"),
                lsdynaModel,
                radiossModel
            ),
            p_ConvertUtils(lsdynaModel, radiossModel)
        {
        }
        void ConvertSets();
        ~ConvertSet() {}

    private:
        ConvertUtils p_ConvertUtils;
        sdi::EntityType radPARTType = p_radiossModel->GetEntityType("/PART");
        sdi::EntityType radBEAMType = p_radiossModel->GetEntityType("/BEAM");
        sdi::EntityType radSPRINGType = p_radiossModel->GetEntityType("/SPRING");
        sdi::EntityType radTRUSSType = p_radiossModel->GetEntityType("/TRUSS");
        sdi::EntityType radSHELLType = p_radiossModel->GetEntityType("/SHELL");
        sdi::EntityType radSH3NType = p_radiossModel->GetEntityType("/SH3N");
        sdi::EntityType radSOLIDType = p_radiossModel->GetEntityType("/BRICK");
        sdi::EntityType radBOXType = p_radiossModel->GetEntityType("/BOX");
        sdiConvert::ContainUIntVsStr p_MapShelElemIdVsElemType;
        sdiConvert::ContainUIntVsStr p_MapBeamElemIdVsElemType;
        int maxSegId = 0;

        void ConvertEntities() override;

        void p_ConvertAllSets();

        void p_ConvertSetPartADD(const sdi::EntityRead& dynaSetRead, sdi::EntityEdit& SetEntityEdit);

        void p_ConvertSetShell(const int& index, const sdi::EntityRead& dynaSetRead, sdi::EntityEdit& SetEntityEdit, int& addClauses);

        void p_ConvertSetBeam(const int& index, const sdi::EntityRead& dynaSetRead, sdi::EntityEdit& SetEntityEdit, int& addClauses);

        void p_ConvertSetSegments();

        void p_ConvertSetSegmentGeneral();

        void p_GetRad1DElemIdElemTypeMap();

        void p_GetRadSetIdListForLsdSetIdList(const sdiUIntList& lsdSetIdList, const sdiString& setKeyWord, sdiUIntList& radSetIdLIst);

    };

    class ConvertRadiossSetsToGroups
    {
    public:
        ConvertRadiossSetsToGroups(sdi::ModelViewEdit* pModelView,
            sdiConvert::ClientInfo* isUsed = nullptr); // isUsed->GetEntityInfo(..., bool&) called
        virtual ~ConvertRadiossSetsToGroups() {}
        void ConvertSets();

    protected:
        enum ClauseType
        {
            CLAUSETYPE_BEAM  ,
            CLAUSETYPE_BOX   ,
            CLAUSETYPE_BOX2  ,
            CLAUSETYPE_NODE  ,
            CLAUSETYPE_PART  ,
            CLAUSETYPE_QUAD  ,
            CLAUSETYPE_RBODY ,
            CLAUSETYPE_SEG   ,
            CLAUSETYPE_SET   ,
            CLAUSETYPE_SETCOL,
            CLAUSETYPE_SH3N  ,
            CLAUSETYPE_SHELL ,
            CLAUSETYPE_SOLID ,
            CLAUSETYPE_SPRING,
            CLAUSETYPE_SUBM  ,
            CLAUSETYPE_SUBS  ,
            CLAUSETYPE_TRIA  ,
            CLAUSETYPE_TRUSS ,
            CLAUSETYPE_LAST
        };

        struct GroupInfo
        {
            sdiString keyword;
            sdiString childKeyword; // for groups which need a second level
            std::vector<sdiString> parentKeyword; // for clause keys
            sdi::EntityType type = sdi::ENTITY_TYPE_NONE;

            GroupInfo() {}
            GroupInfo(const GroupInfo& other) :
                keyword(other.keyword), childKeyword(other.childKeyword), parentKeyword(other.parentKeyword) {}
            GroupInfo(sdiString _keyword, sdiString _childKeyword, const std::vector<sdiString>& _parentKeyword,
                sdi::ModelViewRead* pModelView) :
                keyword(_keyword), childKeyword(_childKeyword), parentKeyword(_parentKeyword)
            {
                if(parentKeyword.size() <= CLAUSETYPE_LAST) parentKeyword.resize(CLAUSETYPE_LAST + 1);
                if(nullptr != pModelView) type = pModelView->GetEntityType(keyword);
            }
        };

        bool ConvertSet(const sdi::EntityRead& set, const sdiString& groupKeyword);

        ClauseType GetClauseType(const sdiString& key) const;

        static bool CompKeyword(const sdiString& keyword1, const sdiString& keyword2);

        sdi::ModelViewEdit* pModelView;
        // data describing the Radioss sets or "groups", constructed in the beginning, could be made static
        sdi::EntityType setType;
        std::map<sdiString, GroupInfo, bool(*)(const sdiString& keyword1, const sdiString& keyword2)> groupInfos;
        std::map<sdiString, ClauseType> clauseTypes;
        // data updated while converting
        std::map<sdiString, unsigned int> maxIds;
        std::map<sdiString, std::set<unsigned int>> convertedGroupsByType;
        std::map<sdiString, std::set<unsigned int>> unconvertibleGroupsByType;
        // pointer to an interface to check whether a group is used
        sdiConvert::ClientInfo* pIsUsed = nullptr;
    };

}

#endif // !SDId2R_CONVERTSETS_H
