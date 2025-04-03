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

#include <dyna2rad/dyna2rad.h>
#include <dyna2rad/convertelementmasses.h>

using namespace sdiD2R;
using namespace std;
using namespace sdi;


void sdiD2R::ConvertElemMass::ConvertElemMasses()
{
    ConvertElementMasses();
}

void ConvertElemMass::ConvertEntities()
{

}

void sdiD2R::ConvertElemMass::ConvertElementMasses()
{
    SelectionElementRead elemMassSelect(p_lsdynaModel, "*ELEMENT_MASS");
    EntityType setEntType = p_radiossModel->GetEntityType("/SET/GENERAL");
    while (elemMassSelect.Next())
    {
        HandleEdit admasHEdit;
        p_radiossModel->CreateEntity(admasHEdit, "/ADMAS",elemMassSelect->GetName());
        if (admasHEdit.IsValid())
        {
            EntityEdit admasEdit(p_radiossModel, admasHEdit);
            sdiConvert::SDIHandlReadList sourcehandleList = { {elemMassSelect->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(admasHEdit, sourcehandleList));

            sdiValueEntity setEntity;
            unsigned int setEntityId;
            sdiValue tempValue(setEntity);
            elemMassSelect->GetValue(sdiIdentifier("ID"), tempValue);
            tempValue.GetValue(setEntity);
            setEntityId = setEntity.GetId();

            int type = 0;

            sdiString keyWord = elemMassSelect->GetKeyword();
            if (keyWord.find("NODE_SET") != keyWord.npos)
            {
                double mass = 0.0;
                tempValue = sdiValue(mass);
                elemMassSelect->GetValue(sdiIdentifier("MASS"), tempValue);
                tempValue.GetValue(mass);

                admasEdit.SetValue(sdiIdentifier("type"), sdiValue(1));
                admasEdit.SetValue(sdiIdentifier("setid_type"), sdiValue(sdiString("/SETS/GRNOD"))); // workaround for internal stuff that should be automatic, but isn't yet
                admasEdit.SetValue(sdiIdentifier("grnd_ID"), sdiValue(sdiValueEntity(setEntType, setEntity.GetId())));
                admasEdit.SetValue(sdiIdentifier("MASS"), sdiValue(mass));

            }
            else if (keyWord.find("PART") != keyWord.npos)
            {
                int lsdMWD = 0;
                double lsdMass = 0.0;
                double lsdFinmass = 0.0;
                sdiValue tempValue(lsdMass);
                elemMassSelect->GetValue(sdiIdentifier("ADDMASS"), tempValue);
                tempValue.GetValue(lsdMass);

                tempValue = sdiValue(lsdFinmass);
                elemMassSelect->GetValue(sdiIdentifier("FINMASS"), tempValue);
                tempValue.GetValue(lsdFinmass);

                tempValue = sdiValue(lsdMWD);
                elemMassSelect->GetValue(sdiIdentifier("MWD"), tempValue);
                tempValue.GetValue(lsdMWD);

                if (keyWord.find("SET") != keyWord.npos)
                {
                    type = 3;
                    admasEdit.SetValue(sdiIdentifier("type"), sdiValue(type));
                    if (lsdMWD == 0)
                        admasEdit.SetValue(sdiIdentifier("IFLAG"), sdiValue(1));
                    else if (lsdMWD == 1)
                        admasEdit.SetValue(sdiIdentifier("IFLAG"), sdiValue(0));
                    if (lsdFinmass != 0.0)
                    {
                        lsdMass = lsdFinmass;
                        type = 4;
                        admasEdit.SetValue(sdiIdentifier("type"), sdiValue(type));
                    }
                    admasEdit.SetValue(sdiIdentifier("MASS"), sdiValue(lsdMass));
                    admasEdit.SetValue(sdiIdentifier("setid_type"), sdiValue(sdiString("/SETS/GRPART"))); // workaround for internal stuff that should be automatic, but isn't yet
                    admasEdit.SetValue(sdiIdentifier("grpart_ID"), sdiValue(sdiValueEntity(setEntType, DynaToRad::GetRadiossSetIdFromLsdSet(setEntity.GetId(), "*SET_PART" ))));
                }
                else
                {
                    sdiValueEntityList partEntityList;
                    sdiUIntList partIdList;

                    type = 6;
                    admasEdit.SetValue(sdiIdentifier("type"), sdiValue(type));
                    if (lsdMWD == 0)
                        admasEdit.SetValue(sdiIdentifier("IFLAG"), sdiValue(sdiIntList(1, 1)));
                    else if (lsdMWD == 1)
                        admasEdit.SetValue(sdiIdentifier("IFLAG"), sdiValue(sdiIntList(1, 0)));
                    if (lsdFinmass != 0.0)
                    {
                        lsdMass = lsdFinmass;
                        type = 7;
                    }
                    tempValue = sdiValue(partEntityList);
                    elemMassSelect->GetValue(sdiIdentifier("ID"), tempValue);
                    tempValue.GetValue(partEntityList);
                    partEntityList.GetIdList(partIdList);

                    admasEdit.SetValue(sdiIdentifier("entityidsmax"), sdiValue((int)partIdList.size()));
                    admasEdit.SetValue(sdiIdentifier("entityids_type"), sdiValue(sdiString("COMPONENT")));
                    admasEdit.SetValue(sdiIdentifier("MASS"), sdiValue(sdiDoubleList(1, lsdMass)));
                    admasEdit.SetValue(sdiIdentifier("part_ID"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/PART"), partIdList)));
                }
            }
            else
            {
                sdiDoubleList massList;
                sdiValueEntityList nodeEntityList;
                sdiUIntList nodeIdList;

                tempValue = sdiValue(massList);
                elemMassSelect->GetValue(sdiIdentifier("MASS"), tempValue);
                tempValue.GetValue(massList);

                tempValue = sdiValue(nodeEntityList);
                elemMassSelect->GetValue(sdiIdentifier("ID"), tempValue);
                tempValue.GetValue(nodeEntityList);
                nodeEntityList.GetIdList(nodeIdList);

                admasEdit.SetValue(sdiIdentifier("type"), sdiValue(5));
                int idsMax = (int)nodeIdList.size();
                admasEdit.SetValue(sdiIdentifier("entityids_type"), sdiValue(sdiString("NODE")));
                admasEdit.SetValue(sdiIdentifier("entityidsmax"), sdiValue(idsMax));
                admasEdit.SetValue(sdiIdentifier("MASS"), sdiValue(massList));
                admasEdit.SetValue(sdiIdentifier("node_ID"), sdiValue(sdiValueEntityList(p_radiossModel->GetEntityType("/NODE"), nodeIdList)));
            }
        }
    }
}
