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

#include <dyna2rad/convertconstrainednode.h>
#include <dyna2rad/dyna2rad.h>

using namespace std;
using namespace sdi;

void sdiD2R::ConvertConstrainedNode::ConvertAllConstrainedNode()
{
    ConvertEntities();
}

void sdiD2R::ConvertConstrainedNode::ConvertEntities()
{
    SelectionRead selConstNode(p_lsdynaModel, "*CONSTRAINED_NODE_SET");

    EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");

    while (selConstNode.Next())
    {
        sdiString keyWord = selConstNode->GetKeyword();
        if (keyWord.find("SET") != keyWord.npos)
        {
            HandleEdit rlinkHEdit;
            p_radiossModel->CreateEntity(rlinkHEdit, "/RLINK", "CONSTRAINED_NODE_SET_"+ to_string(selConstNode->GetId()), selConstNode->GetId());
            if (rlinkHEdit.IsValid())
            {
                EntityEdit rlinkEdit(p_radiossModel, rlinkHEdit);

                double lsdTF = 0;
                sdiValue tempVal(lsdTF);
                selConstNode->GetValue(sdiIdentifier("TF"), tempVal);
                tempVal.GetValue(lsdTF);

                int lsdDOF = 0;
                int lsdDOF_X = 0;
                int lsdDOF_Y = 0;
                int lsdDOF_Z = 0;
                int lsdDOF_RX = 0;
                int lsdDOF_RY = 0;
                int lsdDOF_RZ = 0;
                tempVal = sdiValue(lsdDOF);
                selConstNode->GetValue(sdiIdentifier("DOF"), tempVal);
                tempVal.GetValue(lsdDOF);

                HandleRead nsidHRead;
                selConstNode->GetEntityHandle(sdiIdentifier("NSID"), nsidHRead);
                if (nsidHRead.IsValid())
                {
                    EntityRead nsidRead(p_lsdynaModel, nsidHRead);
                    rlinkEdit.SetValue(sdiIdentifier("grnod_ID"), sdiValue(sdiValueEntity(radSetType,
                        DynaToRad::GetRadiossSetIdFromLsdSet(nsidRead.GetId(), "*SET_NODE" ))));
                }
                
                switch (lsdDOF)
                {

                case 0:
                    lsdDOF_X = 0;
                    lsdDOF_Y = 0;
                    lsdDOF_Z = 0;
                    lsdDOF_RX = 0;
                    lsdDOF_RY = 0;
                    lsdDOF_RZ = 0;
                    break; 
                case 1:
                    lsdDOF_X = 1;
                    lsdDOF_Y = 0;
                    lsdDOF_Z = 0;
                    lsdDOF_RX = 0;
                    lsdDOF_RY = 0;
                    lsdDOF_RZ = 0;
                    break; 
                case 2:
                    lsdDOF_X = 0;
                    lsdDOF_Y = 1;
                    lsdDOF_Z = 0;
                    lsdDOF_RX = 0;
                    lsdDOF_RY = 0;
                    lsdDOF_RZ = 0;
                    break;
                case 3:
                    lsdDOF_X = 0;
                    lsdDOF_Y = 0;
                    lsdDOF_Z = 1;
                    lsdDOF_RX = 0;
                    lsdDOF_RY = 0;
                    lsdDOF_RZ = 0;
                    break;
                case 4:
                    lsdDOF_X = 1;
                    lsdDOF_Y = 1;
                    lsdDOF_Z = 0;
                    lsdDOF_RX = 0;
                    lsdDOF_RY = 0;
                    lsdDOF_RZ = 0;
                    break;
                case 5:
                    lsdDOF_X = 0;
                    lsdDOF_Y = 1;
                    lsdDOF_Z = 1;
                    lsdDOF_RX = 0;
                    lsdDOF_RY = 0;
                    lsdDOF_RZ = 0;
                    break;
                case 6:
                    lsdDOF_X = 1;
                    lsdDOF_Y = 0;
                    lsdDOF_Z = 1;
                    lsdDOF_RX = 0;
                    lsdDOF_RY = 0;
                    lsdDOF_RZ = 0;
                    break;
                case 7:
                    lsdDOF_X = 1;
                    lsdDOF_Y = 1;
                    lsdDOF_Z = 1;
                    lsdDOF_RX = 0;
                    lsdDOF_RY = 0;
                    lsdDOF_RZ = 0;
                    break;
                default:
                    break;
                }

                rlinkEdit.SetValue(sdiIdentifier("Tx"), sdiValue(lsdDOF_X));
                rlinkEdit.SetValue(sdiIdentifier("Ty"), sdiValue(lsdDOF_Y));
                rlinkEdit.SetValue(sdiIdentifier("Tz"), sdiValue(lsdDOF_Z));
                rlinkEdit.SetValue(sdiIdentifier("OmegaX"), sdiValue(lsdDOF_RX));
                rlinkEdit.SetValue(sdiIdentifier("OmegaY"), sdiValue(lsdDOF_RY));
                rlinkEdit.SetValue(sdiIdentifier("OmegaZ"), sdiValue(lsdDOF_RZ));


                sdiConvert::SDIHandlReadList sourceConstNode = { {selConstNode->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(rlinkHEdit, sourceConstNode));
            }
        }
    }
}
