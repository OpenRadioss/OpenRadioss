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

#ifndef SDID2R_CONVERTUTILS_H
#define SDID2R_CONVERTUTILS_H

#include <convertutilsbase.h>
#include <typeinfo>

#ifdef _DEBUG
#define GetAttribValues(enityRead,atribNames,attribVals) \
        GetAttribValues_(enityRead,atribNames,attribVals,__FILE__,__LINE__)
#endif


namespace sdiD2R
{
    // "private" function for degugging
    void LogParameterizedValue(const sdi::EntityBaseRead &entity, const sdiString& dataname,
        unsigned int row=UINT_MAX, unsigned int column=UINT_MAX,
        const char* filename = __FILE__, int line = __LINE__);

    // Some utility functions outside of ConvertUtils, but they can be called through
    // ConvertUtils.GetValue as well.
    // Our current linux compiler doesn't allow "explicit specialization in non-namespace scope"

    template<typename attribT>
    inline bool GetValue(attribT &val, const sdi::EntityBaseRead &entity, const sdiString& dataname,
        unsigned int row=UINT_MAX, unsigned int column=UINT_MAX)
    {
        sdiValue value;
        bool isOk = entity.GetValue(sdiIdentifier(dataname, 0, row, column), value);
        value.GetValue(val);
        return isOk;
    }

    template<typename attribT>
    inline attribT GetValue(const sdi::EntityBaseRead &entity, const sdiString& dataname,
        unsigned int row=UINT_MAX, unsigned int column=UINT_MAX
#ifdef _DEBUG
        , const char* filename = __FILE__, int line = __LINE__
#endif
    )
    {
        attribT val;
        bool isOk = GetValue(val, entity, dataname, row, column);
#ifdef _DEBUG
        LogParameterizedValue(entity, dataname, row, column, filename, line);
#endif
        return val;
    }

    // template specializations for basic types, which need initialization
#ifndef _DEBUG

    #define SDID2R_GETVALUE(TYPE)                                                             \
    template<>                                                                                \
    inline TYPE GetValue<TYPE>(const sdi::EntityBaseRead &entity, const sdiString& dataname, \
        unsigned int row, unsigned int column)                                                \
    {                                                                                         \
        TYPE val = 0;                                                                         \
        bool isOk = GetValue(val, entity, dataname, row, column);                             \
        return val;                                                                           \
    }

#else

    #define SDID2R_GETVALUE(TYPE)                                                             \
    template<>                                                                                \
    inline TYPE GetValue<TYPE>(const sdi::EntityBaseRead &entity, const sdiString& dataname, \
        unsigned int row, unsigned int column, const char* filename, int line)                \
    {                                                                                         \
        TYPE val = 0;                                                                         \
        bool isOk = GetValue(val, entity, dataname, row, column);                             \
        LogParameterizedValue(entity, dataname, row, column, filename, line);                 \
        return val;                                                                           \
    }

#endif

    SDID2R_GETVALUE(int);
    SDID2R_GETVALUE(unsigned int);
    SDID2R_GETVALUE(double);
    SDID2R_GETVALUE(bool);

    class ConvertUtils : public sdiConvert::ConvertUtilsBase
    {
    private:
        sdi::EntityType p_radiossIncludeType;

        sdi::SelectionElementRead p_GetElements(const sdiString& keyWord, const sdiConvert::EntityId& entityId) const;

    public:
        ConvertUtils(sdi::ModelViewRead* lsdynaModel, sdi::ModelViewEdit* radiossModel) :
            ConvertUtilsBase(lsdynaModel, radiossModel)
        {
            p_radiossIncludeType = p_radiossModel->GetEntityType("#include");
            p_radiossParameterType = p_radiossModel->GetEntityType("/PARAMETER");
        }
        int GetComponentType(const sdi::EntityRead& compHRead) const;

        void GetElemKeywords(const sdiString& keyWord, const unsigned int& entityId, sdiStringList& keyWordList) const;

        void CreateCurve( const sdiString& crvName,
                            const int & numPoints,
                            const sdiDoubleList& crvPoints, 
                            sdi::HandleEdit& crvHEdit,
                            const double scaleX = 1.0,
                            const double scaleY = 1.0,
                            const double offsetX = 0.0,
                            const double offsetY = 0.0 ) const;

        sdiConvert::EntityId GetDynaMaxEntityID(const sdi::EntityType& entityTyp) const;

        sdiConvert::EntityId GetRadiossMaxEntityID(const sdi::EntityType& entityTyp) const;

        void FindRadElement(const sdiValueEntity& entity, sdiString& keyWord, sdi::HandleElementRead& elemHandleRead) const;

        void GetCentroid(const sdi::HandleRead& nsidHRead, sdiTriple& centroid);

        void GetCentroid(const sdi::EntityRead& PartRead, sdiTriple& centroid);

        void GetNodeIdsFromNodeSet(const unsigned int& setId, sdiUIntList& nodeIdList);

        void GetPartIdsFromPartSet(const sdiString& setType, const unsigned int& setId, sdiUIntList& partIdList);

        void GetPartIdsFromSetGeneral(const sdiString& setType, const unsigned int& setId, sdiUIntList& partIdList);

        void GetCurveDetails(const sdi::HandleRead curveHandleRead,
                               int& numPoints,
                               sdiDoubleList& crvPoints,
                               std::vector<std::reference_wrapper<double>>& otherDetailsList);

        void GetNodesOfParts(const sdiUIntList& partList, sdiUIntList& nodeList);

        void GetNodesOfParts(const unsigned int & partList, sdiUIntList& nodeList);

        void ExtractNodesFromRadiossSet(const sdi::HandleRead& setHread, sdiUIntList& nodeList);

        void ExtractDPartsFromSetWithClauseALL(const sdi::HandleRead& setHread, sdiUIntList& partList);

        void PropagateInclude(const sdi::EntityRead& entitySource); // preferred over version with handle
        void PropagateInclude(const sdi::HandleRead& handleSource);

        void InversConnectivityNode2Elem(sdiString& elemType, int elemnNbNode, int *knod2elemList, int *nod2elemList, int *elementNodesList);

        void ShellThickPosLayerList(sdiString& propCard, int NIP, int IPOS, sdiDoubleList& ThickPosLayerList, sdiDoubleList& ThickLayerList);

        void Convert1To1(sdi::SelectionRead& selection, sdiString& keyWord);
        
        void Convert1To1(const sdi::EntityRead& optionEntity, sdiString& OptionKey, unsigned int& OptionNumber, unsigned int& newOptionId);

        sdiTriple rotateVector(sdiTriple& u,sdiTriple& x,sdiTriple& y,sdiTriple& z);

        void ExtractShellsFromSet(const sdi::HandleRead& setHread, sdiUIntList& shellList);

        void ExtractSh3nsFromSet(const sdi::HandleRead& setHread, sdiUIntList& sh3nList);

#ifndef _DEBUG
        template <typename attribT>
        void GetAttribValues(const sdi::EntityRead& enityRead, const std::vector<sdiString>& atribNames,
            std::vector <std::reference_wrapper<attribT>>& attribVals) const;
#else
        template <typename attribT>
        void GetAttribValues_(const sdi::EntityRead& enityRead, const std::vector<sdiString>& atribNames,
            std::vector <std::reference_wrapper<attribT>>& attribVals
            , const char* filename = __FILE__, int line = __LINE__) const;
#endif

        template<typename attribT>
        attribT GetValue(const sdi::EntityBaseRead &entity, const sdiString& dataname,
            unsigned int row=UINT_MAX, unsigned int column=UINT_MAX) const;

        template<typename attribT>
        bool GetValue(attribT &val, const sdi::EntityBaseRead &entity, const sdiString& dataname,
            unsigned int row=UINT_MAX, unsigned int column=UINT_MAX) const;

        
        static std::vector<std::string> parameterizedValues;

    };

#ifndef _DEBUG
    template<typename attribT>
    void ConvertUtils::GetAttribValues(const sdi::EntityRead& enityRead,
        const std::vector<sdiString>& atribNames, std::vector<std::reference_wrapper<attribT>>& attribVals) const
#else
    template <typename attribT>
    void ConvertUtils::GetAttribValues_(const sdi::EntityRead& enityRead,
        const std::vector<sdiString>& atribNames, std::vector<std::reference_wrapper<attribT>>& attribVals,
        const char* filename, int line) const
#endif
    {
        if (atribNames.size() != attribVals.size())
            return;
        std::size_t size = atribNames.size();
        for (std::size_t i = 0; i < size; ++i)
        {
            attribVals[i].get() = sdiD2R::GetValue<attribT>(enityRead, atribNames[i]
#ifdef _DEBUG
                , UINT_MAX, UINT_MAX, filename, line
#endif
            );
        }
    }

    template<typename attribT>
    attribT ConvertUtils::GetValue(const sdi::EntityBaseRead &entity, const sdiString& dataname,
        unsigned int row, unsigned int column) const
    {
        return sdiD2R::GetValue<attribT>(entity, dataname, row, column);
    }

    template<typename attribT>
    inline bool ConvertUtils::GetValue(attribT &val, const sdi::EntityBaseRead &entity, const sdiString& dataname,
        unsigned int row, unsigned int column) const
    {
        return sdiD2R::GetValue<attribT>(val, entity, dataname, row, column);
    }
}


#endif // !SDID2R_CONVERTUTILS_H
