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
#ifndef MEC_PRE_OBJECT_ATTRIBUTE_EXPRESSION_EVALUATOR_H
#define MEC_PRE_OBJECT_ATTRIBUTE_EXPRESSION_EVALUATOR_H

#include "mec_expression_evaluator.h"
class IMECPreObject;
class IDescriptor;

class HCIO_DATA_DLL_API MECPreObjectExpressionEvaluator : public IValueExpressionEvaluator
{
public:

    MECPreObjectExpressionEvaluator(const IMECPreObject* preobject_p, const IDescriptor *descriptor_p,
                                    const IExpressionEvaluator* pEvaluator, int ind = -1);

    virtual bool GetValue(const char* name, double& value) const;

private:
    //!index of array attribute
    int index;
    const IMECPreObject *myPreObjectPtr;
    const IDescriptor *myDescriptorPtr;
    const IExpressionEvaluator* myExpressionEvaluator;
};

//@}

#endif //MEC_PRE_OBJECT_ATTRIBUTE_EXPRESSION_EVALUATOR_H
