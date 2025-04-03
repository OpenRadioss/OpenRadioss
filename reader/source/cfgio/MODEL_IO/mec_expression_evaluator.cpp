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

#include "mec_expression_evaluator.h"
#include <string>
#include <boost/lexical_cast.hpp>
using std::string;

IValueExpressionEvaluator::IValueExpressionEvaluator(const IExpressionEvaluator* pEvaluator) :
    p_pEvaluator(const_cast<IExpressionEvaluator*>(pEvaluator)), p_doDelete(false)
{}

IValueExpressionEvaluator::IValueExpressionEvaluator(IExpressionEvaluator* pEvaluator,bool doDelete) :
    p_pEvaluator(pEvaluator), p_doDelete(doDelete)
{}

IValueExpressionEvaluator::~IValueExpressionEvaluator()
{
    if(p_doDelete) delete p_pEvaluator;
}

double IValueExpressionEvaluator::Evaluate(const char* expression, int* pError) const
{
    if(nullptr == p_pEvaluator)
    {
        if(nullptr != pError) *pError = -2;
        return 0;
    }

    // replace variables by their values
    string srcExpression(expression), newExpression;
    size_t posTokenStart = 0, posTokenEnd = 0;
    while(posTokenStart < srcExpression.size())
    {
        posTokenEnd = srcExpression.find_first_of("+-*/ (){}[]^|&!=<>%", posTokenStart);

        if(posTokenEnd == posTokenStart &&
           (srcExpression[posTokenStart] != '&' ||
            (posTokenStart < srcExpression.size() - 2 &&
             srcExpression[posTokenStart + 1] == '&')))
        {
            if(srcExpression[posTokenStart] != '&')
            { // it's a one-character "separator", so just copy
                newExpression += srcExpression[posTokenStart];
                ++posTokenStart;
            }
            else
            { // "&&" is also a separator, but not a single "&", because it might be a parameter prefix
                newExpression += "&&";
                posTokenStart += 2;
            }
        }
        else
        {
            string token = srcExpression.substr(posTokenStart, posTokenEnd - posTokenStart);
            double tokenValue = 0;

            if(posTokenEnd != srcExpression.npos && srcExpression[posTokenEnd] == '(')
            { 
                newExpression += token;
            }
            else if(sscanf(token.c_str(), "%lg", &tokenValue) == 1)
            { // the token is a number, so just copy
                newExpression += token;
            }
            else if(GetValue(token.c_str(), tokenValue))
            { // the token is a variable, replace with value
                newExpression += boost::lexical_cast<string>(tokenValue);
            }
            else
            { // it's an unknown token, so just copy
                newExpression += token;
            }

            posTokenStart = posTokenEnd;
        }
    }

    // replace '**' with '^' in case p_pEvaluator doesn't support this
    while((posTokenStart = newExpression.find("**",0)) != newExpression.npos)
    {
        newExpression.replace(posTokenStart, 2, "^");
    }

    // evaluate newExpression
    int error = 0;
    double value = p_pEvaluator->Evaluate(newExpression.c_str(), &error);
    if(0 != error) value = 0;

    // return error if desired (potential shifting to be done!)
    if(nullptr != pError) *pError = error;

    return value;
};

#include <exprtk.hpp>
double ExpressionEvaluatorExprTk::Evaluate(const char* expression, int* pError) const
{
    exprtk::expression<double> exprtkexpression;
    exprtk::parser<double> parser;

    if (!parser.compile(std::string(expression), exprtkexpression))
    {
        if(nullptr != pError) *pError = -1;
        return 0;
    }

    if(nullptr != pError) *pError = 0;
    return exprtkexpression.value();
}

