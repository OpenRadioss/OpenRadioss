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
#ifndef HCDI_DRAWABLEINFPREOBJECT_H
#define HCDI_DRAWABLEINFPREOBJECT_H

#include "hcdi_drawableinf.h"
#include "hcdi_mec_pre_object.h"
#include "hcdi_mv_descriptor.h"

class HC_DATA_DLL_API hwCFGDrawableInfPreObject : public hwCFGDrawableInf
{
public:
    //@{
    /** Constructor */
    hwCFGDrawableInfPreObject(const IMECPreObject *preobj_p, const IDescriptor *descr_p = nullptr);
    /** Destructor */
    virtual ~hwCFGDrawableInfPreObject() {}
    //@}

public:
    virtual int EvaluateDrawable(const std::string& drawable_name, double& value) const;
    virtual double GetFloatAttribute(int ikeyword) const;
    virtual bool GetFloatArrayAttribute(int ikeyword, std::vector<double>& val_array) const;
    virtual int EvaluateSubDrawableObjectAttrib(int ikeyword, const std::string& sub_drawable_name, double& value) const;

protected:
    const IMECPreObject *p_preobj_p = nullptr;
    const IDescriptor *p_descr_p = nullptr;
};

#endif /* HCDI_DRAWABLEINFPREOBJECT_H */
