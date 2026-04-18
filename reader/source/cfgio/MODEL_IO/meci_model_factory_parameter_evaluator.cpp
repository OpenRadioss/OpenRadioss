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
#include "meci_model_factory_parameter_evaluator.h"

#include "meci_model_factory.h"
#include "mec_msg_manager.h"

MECIModelFactoryParameterEvaluator::MECIModelFactoryParameterEvaluator(
    MECIModelFactory* pModelFactory, int fileIndex,
    const IExpressionEvaluator* pEvaluator,
    const MECMsgManager* pMsgManager,
    const std::vector<std::string>& paramStack):
    p_pModelFactory(pModelFactory), p_fileIndex(fileIndex),
    IVariableExpressionEvaluator(pEvaluator), p_pMsgManager(pMsgManager), p_paramStack(paramStack)
{}

bool MECIModelFactoryParameterEvaluator::GetValue(const char* name, double& value) const
{
    // get parameter
    if(!p_pModelFactory) return false;
    IParameter *iparam_obj = p_pModelFactory->GetParameterObject(name, p_fileIndex);
    if(!iparam_obj) return false;

    // get parameter value
    IParameter::Type type = iparam_obj->GetType();
    if(type == IParameter::TYPE_DOUBLE || type == IParameter::TYPE_DOUBLE_EXPRESSION)
        value = iparam_obj->GetDoubleValue();
    else if(type == IParameter::TYPE_INTEGER || type == IParameter::TYPE_INTEGER_EXPRESSION)
        value = (double) iparam_obj->GetIntValue();

    // if parameter expression value == 0 assume it is not evaluated and evaluate
    if(0 == value &&
       (type == IParameter::TYPE_INTEGER_EXPRESSION || type == IParameter::TYPE_DOUBLE_EXPRESSION))
    {
        // we check for cyclic dependencies here, to avoid infinite loops
        if(std::find(p_paramStack.begin(), p_paramStack.end(), name) != p_paramStack.end())
        {
            if(p_pMsgManager) p_pMsgManager->displayMessage(MECMsgManager::MSG_ERROR, 108, name);
            return false;
        }
        string expr = iparam_obj->GetExpression();
        int fileIndex = iparam_obj->GetFileIndex();
        p_paramStack.push_back(name);
        MECIModelFactoryParameterEvaluator evaluator(p_pModelFactory, fileIndex, p_pEvaluator,
                                                     p_pMsgManager, p_paramStack);
        int err = 0;
        value = evaluator.Evaluate(expr.c_str(), &err);
        p_paramStack.pop_back();
        if(err != 0)
        {
            if(p_pMsgManager) p_pMsgManager->displayMessage(MECMsgManager::MSG_ERROR, 109, name);
            return false;
        }
        // hack: recursive calls of this method might have set another parameter in the static data
        // in *iparam_obj, so we reset it:
        iparam_obj = p_pModelFactory->GetParameterObject(name, p_fileIndex);
        iparam_obj->SetExpressionValue(value);
    }

    return true;
}
