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
#ifndef HCDI_DRAWABLEINF_H
#define HCDI_DRAWABLEINF_H

#include "hcdi.h"

#include <string>
#include <vector>

using namespace std;

class HC_DATA_DLL_API hwCFGDrawableInf
{
public:
    //@{
    /** Constructor */
    hwCFGDrawableInf() {};
    /** Destructor */
    virtual ~hwCFGDrawableInf() {};
    //@}

public:
    //@{
    virtual int EvaluateDrawable(const std::string& drawable_name, double& value) const = 0;
    virtual double GetFloatAttribute(int ikeyword) const = 0;
    virtual bool GetFloatArrayAttribute(int ikeyword, std::vector<double>& val_array) const = 0;
    virtual int EvaluateSubDrawableObjectAttrib(int ikeyword, const std::string& sub_drawable_name, double& value) const = 0;
};

#endif /* HCDI_DRAWABLEINF_H */
