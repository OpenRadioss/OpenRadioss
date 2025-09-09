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

#include <dyna2rad/convertnodetransforms.h>
#include <dyna2rad/convertdefinetransform.h>
#include <dyna2rad/dyna2rad.h>

using namespace sdiD2R;
using namespace std;
using namespace sdi;
using namespace sdiConvert;

void sdiD2R::ConvertNodeTransform::ConvertNodeTransforms()
{
    ConvertEntities();
}

void sdiD2R::ConvertNodeTransform::ConvertEntities()
{
    EntityType radTransformType = p_radiossModel->GetEntityType("/TRANSFORM");
    EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");
    SelectionRead selNodeTransform(p_lsdynaModel, "*NODE_TRANSFORM");

    while (selNodeTransform.Next())
    {
        unsigned int NodeTransformId = selNodeTransform->GetId();
        sdiString NodeTransformName = selNodeTransform->GetName();

        int defineTransformId = 0;
        sdiValue tempVal(defineTransformId);
        selNodeTransform->GetValue(sdiIdentifier("TRSID"), tempVal);
        tempVal.GetValue(defineTransformId);

        HandleRead setNodeHandle;
        selNodeTransform->GetEntityHandle(sdiIdentifier("NSID"), setNodeHandle);
        EntityRead setNodeHandleRead(p_lsdynaModel, setNodeHandle);
        unsigned int SetNodeReadId = setNodeHandleRead.GetId();

        /*convert define transform*/
        ConvertDefineTransform cnvrtDefineTransform(p_lsdynaModel, p_radiossModel);
        cnvrtDefineTransform.ConvertSelectedDefineTransform(defineTransformId,SetNodeReadId,0);
    }

}
