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

#include <dyna2rad/convertboxes.h>
#include <dyna2rad/dyna2rad.h>

using namespace std;
using namespace sdi;

void sdiD2R::ConvertBox::ConvertBoxes()
{
    ConvertEntities();
}

void sdiD2R::ConvertBox::ConvertEntities()
{
    SelectionRead selectDefBox(p_lsdynaModel, srcCard);
    while (selectDefBox.Next())
    {

        sdiString keyWord = selectDefBox->GetKeyword();
        if (keyWord.find("ADAPTIVE") != keyWord.npos ||
            keyWord.find("COARSEN") != keyWord.npos ||
            keyWord.find("DRAWBEAD") != keyWord.npos ||
            keyWord.find("SPH") != keyWord.npos)
        {
            continue;
        }
        double lsdXMN;
        double lsdXMX;
        double lsdYMN;
        double lsdYMX;
        double lsdZMN;
        double lsdZMX;
        double lsdXX;
        double lsdYX;
        double lsdZX;
        double lsdXV;
        double lsdYV;
        double lsdZV;
        double lsdCX;
        double lsdCY;
        double lsdCZ;
        sdiStringList attribNames({ "XMN", "XMX", "YMN", "YMX", "ZMN", "ZMX", "XX", "YX", "ZX", "XV", "YV", "ZV", "CX", "CY", "CZ" });
        vector<reference_wrapper<double>> doubleAttrVals({ lsdXMN, lsdXMX, lsdYMN, lsdYMX, lsdZMN, lsdZMX, lsdXX, lsdYX, lsdZX, lsdXV, lsdYV, lsdZV, lsdCX, lsdCY, lsdCZ });
        p_ConvertUtils.GetAttribValues(*selectDefBox, attribNames, doubleAttrVals);

        HandleEdit radBoxHEdit;
        p_radiossModel->CreateEntity(radBoxHEdit, "/BOX/RECTA", selectDefBox->GetName(), selectDefBox->GetId());
        EntityEdit radBoxEdit(p_radiossModel, radBoxHEdit);
        sdiStringList radDoubleAttrNames({ "Xp1", "Xp2", "Yp1", "Yp2", "Zp1","Zp2" });

        sdiConvert::SDIHandlReadList sourceList = { {selectDefBox->GetHandle()} };
        if (keyWord.find("LOCAL") != keyWord.npos)
        {
            /*p_ConvertUtils.SetExpressionValue(*selectDefBox, radBoxEdit, "XMN+CX", "Xp1");
            p_ConvertUtils.SetExpressionValue(*selectDefBox, radBoxEdit, "XMX+CX", "Xp2");
            p_ConvertUtils.SetExpressionValue(*selectDefBox, radBoxEdit, "YMN+CY", "Yp1");
            p_ConvertUtils.SetExpressionValue(*selectDefBox, radBoxEdit, "YMX+CY", "Yp2");
            p_ConvertUtils.SetExpressionValue(*selectDefBox, radBoxEdit, "ZMN+CZ", "Zp1");
            p_ConvertUtils.SetExpressionValue(*selectDefBox, radBoxEdit, "ZMX+CZ", "Zp2");*/


            sdiTriple origin(lsdCX, lsdCY, lsdCZ);
            sdiTriple vector2(sdiTriple(lsdXX, lsdYX, lsdZX)* sdiTriple(lsdXV, lsdYV, lsdZV));
            sdiTriple vector1(vector2 * sdiTriple(lsdXX, lsdYX, lsdZX));
            vector1 = vector1.Normalize();
            vector2 = vector2.Normalize();

            HandleEdit radSkewHEdit;
            p_radiossModel->CreateEntity(radSkewHEdit, "/SKEW/FIX", "SKEW_FIX_DEFINE_BOX_" + to_string(selectDefBox->GetId()));
            EntityEdit radSkewEdit(p_radiossModel, radSkewHEdit);
            vector<sdiString> attribOrigin({ "Ox", "Oy", "Oz" });
            vector<sdiString> attribVect1({ "X1", "Y1", "Z1" });
            vector<sdiString> attribVect2({ "X2", "Y2", "Z2" });
            for (size_t i = 0; i < 3; ++i)
            {
                radSkewEdit.SetValue(sdiIdentifier(attribOrigin[i]), sdiValue(origin[i]));
                radSkewEdit.SetValue(sdiIdentifier(attribVect1[i]), sdiValue(vector1[i]));
                radSkewEdit.SetValue(sdiIdentifier(attribVect2[i]), sdiValue(vector2[i]));
            }
            radSkewEdit.SetValue(sdiIdentifier("titlestr"), sdiValue(sdiString("SKEW_FIX_DEFINE_BOX_" + to_string(selectDefBox->GetId()))));
            radBoxEdit.SetEntityHandle(sdiIdentifier("Iskew"), radSkewHEdit);
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radSkewHEdit, sourceList));
                
            sdiTriple lsdMN = sdiTriple(lsdXMN,lsdYMN,lsdZMN);
            sdiTriple lsdMX = sdiTriple(lsdXMX,lsdYMX,lsdZMX);
            sdiTriple lsdX = sdiTriple(lsdXX,lsdYX,lsdZX);

            lsdX   = lsdX.Normalize();

            sdiTriple N1 = p_ConvertUtils.rotateVector(lsdMN,lsdX,vector1,vector2);
            sdiTriple N2 = p_ConvertUtils.rotateVector(lsdMX,lsdX,vector1,vector2);
            radBoxEdit.SetValue(sdiIdentifier("Xp1"), sdiValue(N1[0]+lsdCX));
            radBoxEdit.SetValue(sdiIdentifier("Xp2"), sdiValue(N2[0]+lsdCX));
            radBoxEdit.SetValue(sdiIdentifier("Yp1"), sdiValue(N1[1]+lsdCY));
            radBoxEdit.SetValue(sdiIdentifier("Yp2"), sdiValue(N2[1]+lsdCY));
            radBoxEdit.SetValue(sdiIdentifier("Zp1"), sdiValue(N1[2]+lsdCZ));
            radBoxEdit.SetValue(sdiIdentifier("Zp2"), sdiValue(N2[2]+lsdCZ));
        }
        else
        {
            for (size_t i = 0; i < 6; ++i)
                radBoxEdit.SetValue(sdiIdentifier(radDoubleAttrNames[i]), sdiValue(doubleAttrVals[i]));
        }
        sdiConvert::Convert::PushToConversionLog(std::make_pair(radBoxHEdit, sourceList));
    }
}
