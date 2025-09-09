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

#ifndef SDIDYNATORAD_H
#define SDIDYNATORAD_H

// *************************************************************************************
// Windows export macro.
// *************************************************************************************
#if defined(OS_WIN) && !defined(NO_DECLS)
#ifdef SDIDYNATORAD_EXPORT
#undef SDIDYNATORAD_DECLS
#define SDIDYNATORAD_DECLS __declspec(dllexport)
#else
#undef SDIDYNATORAD_DECLS
#define SDIDYNATORAD_DECLS __declspec(dllimport)
#endif  //! SDIDYNATORAD_EXPORT
#else
#undef SDIDYNATORAD_DECLS
#define SDIDYNATORAD_DECLS
#endif

#include <sdiMessageHandler.h>
#include <convert.h>
#include <convertRuleMap.h>

namespace sdiD2R
{
    class SDIDYNATORAD_DECLS DynaToRad :public sdiConvert::Convert
    {
    private:
        sdi::ModelViewRead *&p_lsdynaModel = this->srcModelViewSDI;

        sdi::ModelViewEdit *&p_radiossModel = this->destModelViewSDI;

        sdiString p_RunName;

        bool p_UseMapping;
        bool p_useSubmodelOffsets = false;
        bool p_doCreateClassicGroups = false;
        sdiConvert::ClientInfo* p_pClassicGroupIsUsed = nullptr;

        static const sdiMessageHandler* p_pMessageHandler;

        static std::map<unsigned int, sdiConvert::ContainStrVsUInt> setsMappingDetails;

        static sdiConvert::ContainUIntVsUInt storeLsdVIDVsRadSkewId;

        
        static std::vector<std::string> p_parameterizedValues;

        void p_GetCurrentUnitSystem(sdiString& unitSyst);

        void p_CreateTHNodeBasedonLsdDatabaseCards();

        void p_UpdateTHNodeBasedonLsdDatabaseCards();

        void p_CreateTHCardsBasedonLsdDatabaseCards();

        void p_AutoConvert();

        void UpdateRunNameForCards();

    public:
        DynaToRad(sdi::ModelViewRead* dynaModelViewSDI, sdi::ModelViewEdit* radiossModelViewSDI, sdiString& modelName,
            const sdiMessageHandler* pMessageHandler = nullptr);

        void SetUseSubmodelOffsets(bool useSubmodelOffsets);

        void SetDoConvertParameters(bool doConvertParameters);

        void SetDoCreateClassicGroups(bool doCreateClassicGroups,
            sdiConvert::ClientInfo* pClassicGroupIsUsed = nullptr);

        static sdiConvert::ContainUIntVsUInt storeLsdSDorientIdVsSkewId;

        static sdiConvert::ContainUIntVsUInt storeRbodyPIDVsMasterNode;

        static sdiConvert::ContainUIntVsUInt storeRigidPartVsMasterNode;

        static void ShowMessage(const sdiMessageHandler ::Level &level, int code, ...);

        static void PushToSetsMappingDetails(const unsigned int& LsdSetId, const sdiString& lsdKeyWord, const unsigned int& radSetId);

        static unsigned int GetRadiossSetIdFromLsdSet(const unsigned int& LsdSetId, const sdiString& keyWord);

        static void PushIntoStoreLsdVIDVsRadSkewId(const unsigned int& LsdVId, const unsigned int& radSkewId);

        static unsigned int GetRadiossSkewIdFromLsdVID(const unsigned int& LsdVId);

        void GetConversionLog(sdiConvert::LogQueryHandle& conversionLog) const override;

        void PrintLog(sdiConvert::LogQueryHandle& conversionLog) const override;
        
        static void BuildConversionMap();
        
        static const convertRuleMap&  GetConversionMap();

        void CallConvert() override;

        static std::vector<std::string>& GetParameterizedValues();

        static convertRuleMap p_conversionMap;

        ~DynaToRad() {}
    };
    

}
#endif // SDIDYNATORAD_H
