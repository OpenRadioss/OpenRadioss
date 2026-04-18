//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2026 Altair Engineering Inc.
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
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.
#ifndef MECI_MODEL_FACTORY_PARAMETER_EVALUATOR_H
#define MECI_MODEL_FACTORY_PARAMETER_EVALUATOR_H

#include "mec_expression_evaluator.h"
#include <vector>
#include <string>
class MECIModelFactory;
class MECMsgManager;

class HCIO_DATA_DLL_API MECIModelFactoryParameterEvaluator : public IVariableExpressionEvaluator
{
public:

    MECIModelFactoryParameterEvaluator(MECIModelFactory* modelFactory, int fileIndex,
                                       const IExpressionEvaluator* pEvaluator,
                                       const MECMsgManager* pMsgManager,
                                       const std::vector<std::string>& paramStack = std::vector<std::string>());

    virtual bool GetValue(const char* name, double& value) const;

private:


private:
    MECIModelFactory*           p_pModelFactory;
    int                         p_fileIndex;
    const MECMsgManager*        p_pMsgManager;
    mutable std::vector<std::string> p_paramStack;
};

//@}

#endif //MECI_MODEL_FACTORY_PARAMETER_EVALUATOR_H
