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




////////////////////////////////////////////////////////////////////

#ifndef SDICONVERT_CONVERTUTILSBASE_H
#define SDICONVERT_CONVERTUTILSBASE_H

#include <convert.h>
#include <functional>

namespace sdiConvert
{

class SDICONVERT_DECLS ConvertUtilsBase
{
protected:
    sdi::ModelViewRead* p_lsdynaModel;
    sdi::ModelViewEdit* p_radiossModel;
    sdi::EntityType p_radiossParameterType;

public:
    ConvertUtilsBase(sdi::ModelViewRead* lsdynaModel, sdi::ModelViewEdit* radiossModel) :
        p_lsdynaModel(lsdynaModel),
        p_radiossModel(radiossModel)
    {
        assert(nullptr != lsdynaModel && nullptr != p_radiossModel);
    }

    void GetElemKeywords(const sdi::EntityRead& enityRead, sdiStringList& keyWordList) const;

    void GetEntityHandles(const sdi::EntityRead& enityRead, const std::vector<sdiString>& attribNames, std::vector<std::reference_wrapper<sdi::HandleRead>>& handleReadList);

    sdiStringList SplitString(const sdiString& strToSplit, const sdiString& delim);

    void IdListRemove(sdiUIntList& idList1, const sdiUIntList& idList2);

    bool CopyValue(
        const sdi::EntityRead& srcEntity, const sdi::EntityEdit& destEntity,
        const sdiString& srcDataname, const sdiString& destDataname,
        unsigned int srcRow = UINT_MAX, unsigned int destRow = UINT_MAX,
        unsigned int srcColumn = UINT_MAX, unsigned int destColumn = UINT_MAX);

    bool SetExpressionValue(
        const sdi::EntityRead& srcEntity, const sdi::EntityEdit& destEntity,
        const sdiString& srcExpression, const sdiString& destDataname,
        bool isReal = true, // pass false if int value
        unsigned int srcRow = UINT_MAX, unsigned int destRow = UINT_MAX,
        unsigned int srcColumn = UINT_MAX, unsigned int destColumn = UINT_MAX);

    static std::map<sdiString, sdiString> p_expressionParameters;
};

} // namespace sdiConvert


#endif // !SDICONVERT_CONVERTUTILSBASE_H
