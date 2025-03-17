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
#ifndef MEC_EXPRESSION_EVALUATOR_H
#define MEC_EXPRESSION_EVALUATOR_H

#include "hcio.h"

/// Interface that evaluates a mathematical expression.
class HCIO_DATA_DLL_API IExpressionEvaluator
{
public:
    virtual ~IExpressionEvaluator() {}
    virtual double Evaluate(const char* expression, int* pError = nullptr) const = 0;
};

/// Interface that evaluates a mathematical expression with variables.
/// The Evaluate() method replaces variable with their values by calling GetValue(),
/// and then calls pEvaluator->Evaluate() to evaluate the resulting expression.
class HCIO_DATA_DLL_API IValueExpressionEvaluator : public IExpressionEvaluator
{
protected:
    IValueExpressionEvaluator() {} // just to avoid compiler errors, doesn't make sense
    IValueExpressionEvaluator(const IExpressionEvaluator* pEvaluator);
    IValueExpressionEvaluator(IExpressionEvaluator* pEvaluator, bool doDelete);
    virtual bool GetValue(const char* name, double& value) const = 0;
public:
    virtual ~IValueExpressionEvaluator();
    /// Evaluate a mathematical expression with variables.
    virtual double Evaluate(const char* expression, int* pError = nullptr) const;

protected:
    IExpressionEvaluator* p_pEvaluator = nullptr;
private:
    bool p_doDelete = false;
};

// implementation using www.partow.net/programming/exprtk
class HCIO_DATA_DLL_API ExpressionEvaluatorExprTk : public IExpressionEvaluator
{
public:
    virtual double Evaluate(const char* expression, int* pError = nullptr) const;
};

#endif // MEC_EXPRESSION_EVALUATOR_H
