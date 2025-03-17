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

#include <dyna2rad/convertcontacts.h>
#include <dyna2rad/dyna2rad.h>

using namespace sdi;
using namespace std;


void sdiD2R::ConvertContact::ConvertContacts()
{
    ConvertEntities();

    ConvertContactInterior();

    ConvertContactForceTranducerPenalty();

    p_CreateThInter();
}

void sdiD2R::ConvertContact::ConvertEntities()
{
    EntityType radPartType = p_radiossModel->GetEntityType("/PART");
    EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");

    sdiConvert::ContainStrVsMapStrVsInt interTypeVsMapDefaultVals;
    interTypeVsMapDefaultVals["TYPE25"] = { {"Idel", 1}, {"Inacti", 5}, {"IGAP", 2} };
    interTypeVsMapDefaultVals["TYPE2"] = { {"Ignore", 2}, { "Idel2", 1 }, { "Spotflag", 28 } };
    interTypeVsMapDefaultVals["TYPE10"] = { { "Idel", 1 }, {"INACTI", 0}};
    interTypeVsMapDefaultVals["TYPE19"] = { {"Idel", 1}, {"Inacti", 6}, {"Igap", 0} };
    interTypeVsMapDefaultVals["TYPE7"] = { {"Idel", 2}, {"Inacti", 6}, {"Igap", 2} };
    interTypeVsMapDefaultVals["TYPE11"] = { {"Idel", 2}, {"Inacti", 6}, {"Igap", 0} };

    SelectionRead selContact(p_lsdynaModel, srcCard);
    SelectionRead selFriction(p_lsdynaModel, "*DEFINE_FRICTION");

    while (selContact.Next())
    {
        sdiString interType;
        double dSearch = 0.0;
        sdiValueEntity ssidEntity;
        sdiValueEntity msidEntity;
        sdiString dynaContactName = selContact->GetName();
        unsigned int dynaContactId = selContact->GetId();
        sdiString keyWord = selContact->GetKeyword();
        sdiConvert::SDIHandlReadList sourceHandleList = { {selContact->GetHandle()} };
        EntityRead contactRead(p_lsdynaModel, selContact->GetHandle());
        sdiStringList surfAttrNames({ "surf_ID1", "surf_ID2" });

        vector<reference_wrapper<sdiValueEntity>> contactSetEntities({ ssidEntity, msidEntity });
        sdiStringList contactSetAttrNames({ "SSID", "MSID" });
        p_ConvertUtils.GetAttribValues(*selContact, contactSetAttrNames, contactSetEntities);
        unsigned int ssid = ssidEntity.GetId();
        unsigned int msid = msidEntity.GetId();

        int lsdSSTYP;
        int lsdMSTYP;
        int lsdSOFT;
        if(keyWord.find("CONTACT_AIRBAG_SINGLE_SURFACE") != keyWord.npos)  // TYPE19
        {
            vector<reference_wrapper<int>> contactSetTypes({lsdSSTYP, lsdMSTYP, lsdSOFT});
            sdiStringList contactSetTypeAttrNames({ "SSTYP", "MSTYP", "SOFT" });
            p_ConvertUtils.GetAttribValues(*selContact, contactSetTypeAttrNames, contactSetTypes);
        }
        else if(keyWord.find("AUTOMATIC_BEAMS_TO_SURFACE") != keyWord.npos)  // TYPE19
        {
            vector<reference_wrapper<int>> contactSetTypes({lsdSSTYP, lsdMSTYP, lsdSOFT});
            sdiStringList contactSetTypeAttrNames({ "SSTYPE", "MSTYPE", "SOFT" });
            p_ConvertUtils.GetAttribValues(*selContact, contactSetTypeAttrNames, contactSetTypes);
        }
        else{
            vector<reference_wrapper<int>> contactSetTypes({lsdSSTYP, lsdMSTYP, lsdSOFT});
            sdiStringList contactSetTypeAttrNames({ "SSTYP", "MSTYP", "LSDYNA_SOFT" });
            p_ConvertUtils.GetAttribValues(*selContact, contactSetTypeAttrNames, contactSetTypes);
        }

        double lsdSFS =  0.0;
        double lsdSFM =  0.0;
        double lsdSST =  0.0;
        double lsdMST =  0.0;
        double lsdSFST = 0.0;
        double lsdSFMT = 0.0;
        double lsdFSF =  0.0;
        if(keyWord.find("CONTACT_AIRBAG_SINGLE_SURFACE") != keyWord.npos)  // TYPE19
        {
            sdiStringList tiedAttribNames({ "SFS", "SFM", "SST", "SFST", "SFMT", "FSF" });
            vector<reference_wrapper<double>> tiedAttribVals({ lsdSFS, lsdSFM, lsdSST, lsdSFST, lsdSFMT, lsdFSF });
            p_ConvertUtils.GetAttribValues(*selContact, tiedAttribNames, tiedAttribVals);
        }
        else
        {
            sdiStringList tiedAttribNames({ "LSDYNA_SFS", "LSDYNA_SFM", "LSDYNA_SST", "LSDYNA_MST", "LSDYNA_SFST", "LSDYNA_SFMT", "LSDYNA_FSF" });
            vector<reference_wrapper<double>> tiedAttribVals({ lsdSFS, lsdSFM, lsdSST, lsdMST, lsdSFST, lsdSFMT, lsdFSF });
            p_ConvertUtils.GetAttribValues(*selContact, tiedAttribNames, tiedAttribVals);
        }

        if (keyWord.find("AUTOMATIC_SURFACE_TO_SURFACE") != keyWord.npos ||
            keyWord.find("ERODING_SURFACE_TO_SURFACE")   != keyWord.npos ||
            keyWord.find("AUTOMATIC_GENERAL")            != keyWord.npos ||
            keyWord.find("AUTOMATIC_NODES_TO_SURFACE")   != keyWord.npos ||
            keyWord.find("AUTOMATIC_SINGLE_SURFACE")     != keyWord.npos ||
            keyWord.find("ERODING_SINGLE_SURFACE")       != keyWord.npos ||
            keyWord.find("AUTOMATIC_BEAMS_TO_SURFACE")   != keyWord.npos)
        {
            if (keyWord.find("AUTOMATIC_NODES_TO_SURFACE") != keyWord.npos || keyWord.find("AUTOMATIC_BEAMS_TO_SURFACE")   != keyWord.npos)
                surfAttrNames[0] = "grnd_IDs";
            interType = "TYPE25";


            if(keyWord.find("ERODING_SINGLE_SURFACE") != keyWord.npos || keyWord.find("AUTOMATIC_GENERAL") != keyWord.npos) 
            {
                    if(lsdSOFT == -7)
                    {
                        interType = "TYPE7";
                        surfAttrNames = sdiStringList( { "grnod_id", "surf_id" } );
                        if (msid == 0) 
                        {
                            msid = ssid;
                            lsdMSTYP = lsdSSTYP;
                        }
                    }
                    else if(lsdSOFT == -11)
                    {
                        interType = "TYPE11";
                        surfAttrNames = sdiStringList( { "line_IDs", "line_IDm" } );  
                        if (msid == 0) 
                        {
                            msid = ssid;
                            lsdMSTYP = lsdSSTYP;
                        }
                    }
                    else if(lsdSOFT == -19)
                    {
                        interType = "TYPE19";
                        surfAttrNames = sdiStringList( { "surf_IDs", "surf_IDm" } );  
                        if (msid == 0) 
                        {
                            msid = ssid;
                            lsdMSTYP = lsdSSTYP;
                        }
                    }
            }
        }
        else if (keyWord.find("CONTACT_AIRBAG_SINGLE_SURFACE") != keyWord.npos)
        {
            int IFLAG = GetValue<int>(*selContact, "IFLAG");

            //if (IFLAG == 0)
            if(lsdSOFT == -19)
            {
                interType = "TYPE19";
                surfAttrNames = sdiStringList( { "surf_IDs", "surf_IDm" } );
            }
            //else if(IFLAG == 1)
            else
            {
                interType = "TYPE25";
            }
        }
        else if (keyWord.find("SPOTWELD")                   != keyWord.npos ||
                 keyWord.find("TIED_NODES_TO_SURFACE")      != keyWord.npos ||
                 keyWord.find("TIED_SHELL_EDGE_TO_SURFACE") != keyWord.npos ||
                 keyWord.find("TIEBREAK_NODES") != keyWord.npos)
        {
            interType = "TYPE2";
            surfAttrNames = sdiStringList( { "grnd_IDs", "surf_IDm" } );
            if (keyWord.find("TIED_SHELL_EDGE_TO_SURFACE") != keyWord.npos)
            {
                if(lsdSST > 0 && lsdMST > 0)
                  dSearch = 0.6*(lsdSST + lsdMST);
                else if(lsdSST < 0 && lsdMST < 0)
                  dSearch = fabs(0.6*(lsdSST + lsdMST));
                else
                  dSearch = 0.0;
                if (dSearch < 0)
                    continue;
            }
            else if (keyWord.find("TIED_NODES_TO_SURFACE") != keyWord.npos ||
                     keyWord.find("TIEBREAK_NODES") != keyWord.npos)
            {
                if(lsdSST > 0 && lsdMST > 0)
                  dSearch = 0.6*(lsdSST + lsdMST);
                else if(lsdSST < 0 && lsdMST < 0)
                  dSearch = fabs(0.6*(lsdSST + lsdMST));
                else
                  dSearch = 0.0;
            }
        } 
        else if (keyWord.find("NODES_TO_SURFACE")      != keyWord.npos)
        {
            surfAttrNames[0] = "grnd_IDs";
            interType = "TYPE25";
        }

        else if (keyWord.find("TIED_SURFACE_TO_SURFACE")      != keyWord.npos)
        {
           dSearch = (lsdSFST*lsdSST+lsdSFMT*lsdMST)/2.0;
           if(dSearch >= 0)
           {
             interType = "TYPE2";
             surfAttrNames = sdiStringList ({ "grnd_IDs", "surf_IDm" });
             dSearch = fabs((lsdSFS*lsdSST+lsdSFM*lsdMST))/2.0;
           }
           else
           {
             interType = "TYPE10";
             surfAttrNames = sdiStringList ({ "grnod_id", "surf_id" });
           }
        }

        if (interType.empty())
            continue;
        HandleEdit radInterHEdit;
        p_radiossModel->CreateEntity(radInterHEdit, "/INTER/" + interType, dynaContactName, dynaContactId);
        EntityEdit radInterEdit(p_radiossModel, radInterHEdit);
        sdiConvert::Convert::PushToConversionLog(std::make_pair(radInterHEdit, sourceHandleList));

        vector<int> contacSetType = { {lsdSSTYP, lsdMSTYP} };
        sdiUIntList setIds = { { ssid, msid } };

        for (size_t i = 0; i < 2; ++i)
        {
            unsigned int surfId = setIds[i];
            HandleEdit radSetHEdit;
            if (surfId)
            {
                switch (contacSetType[i])
                {
                case 0:
                    radInterEdit.SetValue(sdiIdentifier(surfAttrNames[i]), sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(surfId, "*SET_SEGMENT"))));
                    break;
                case 1:
                    radInterEdit.SetValue(sdiIdentifier(surfAttrNames[i]), sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(surfId, "*SET_SHELL"))));
                    break;
                case 2:
                    radInterEdit.SetValue(sdiIdentifier(surfAttrNames[i]), sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(surfId, "*SET_PART"))));
                    break;
                case 4:
                    radInterEdit.SetValue(sdiIdentifier(surfAttrNames[i]), sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(surfId, "*SET_NODE"))));
                    break;
                case 3:
                {
                    p_radiossModel->CreateEntity(radSetHEdit, "/SET/GENERAL", surfAttrNames[i] + "_" + dynaContactName);
                    EntityEdit radSetEdit(p_radiossModel, radSetHEdit);
                    radSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                    radSetEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
                    radSetEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(radPartType, sdiUIntList(1, surfId))));
                    radSetEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("PART")));
                    radInterEdit.SetEntityHandle(sdiIdentifier(surfAttrNames[i]), radSetHEdit);
                    break;
                }
                case 5:
                {
                    p_radiossModel->CreateEntity(radSetHEdit, "/SET/GENERAL", surfAttrNames[i] + "_" + dynaContactName);
                    EntityEdit radSetEdit(p_radiossModel, radSetHEdit);
                    radSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                    radSetEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("ALL")));
                    radInterEdit.SetEntityHandle(sdiIdentifier(surfAttrNames[i]), radSetHEdit);
                    break;
                }
                case 6:
                {
                    p_radiossModel->CreateEntity(radSetHEdit, "/SET/GENERAL", surfAttrNames[i] + "_" + dynaContactName);
                    EntityEdit radSetEdit(p_radiossModel, radSetHEdit);
                    radSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(2));
                    radSetEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("ALL")));
                    radSetEdit.SetValue(sdiIdentifier("opt_D", 0, 1), sdiValue(1));
                    radSetEdit.SetValue(sdiIdentifier("KEY_type", 0, 1), sdiValue(sdiString("SET")));
                    radSetEdit.SetValue(sdiIdentifier("idsmax", 0, 1), sdiValue(1));
                    radSetEdit.SetValue(sdiIdentifier("ids", 0, 1), sdiValue(sdiValueEntityList(radSetType, 
                        sdiUIntList(1, DynaToRad::GetRadiossSetIdFromLsdSet(surfId, "*SET_PART")))));
                    radInterEdit.SetEntityHandle(sdiIdentifier(surfAttrNames[i]), radSetHEdit);
                    break;
                }
                default:
                    break;
                }
            }
            else if (!ssid && i == 0)
            {
                p_radiossModel->CreateEntity(radSetHEdit, "/SET/GENERAL", surfAttrNames[i] + "_" + dynaContactName);
                EntityEdit radSetEdit(p_radiossModel, radSetHEdit);
                radSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                radSetEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("ALL")));
                radInterEdit.SetEntityHandle(sdiIdentifier(surfAttrNames[i]), radSetHEdit);
            }
            if (radSetHEdit.IsValid())
                sdiConvert::Convert::PushToConversionLog(std::make_pair(radSetHEdit, sourceHandleList));
        }

        for (auto tempPair : interTypeVsMapDefaultVals[interType])
            radInterEdit.SetValue(sdiIdentifier(tempPair.first), sdiValue(tempPair.second));
        if (interType == "TYPE2")
        {
            radInterEdit.SetValue(sdiIdentifier("dsearch"), sdiValue(dSearch));
            continue;
        }
        double lsdFS;
        double lsdFD;
        double lsdDC;
        double lsdBT;// Tstart
        double lsdDT;// Tstop
        if(keyWord.find("CONTACT_AIRBAG_SINGLE_SURFACE") != keyWord.npos)  // TYPE19
        {
            sdiStringList fricAttrNames({ "FS", "FD", "DC", "BT", "DT"});
            vector< reference_wrapper<double> > fricAttrVals({ lsdFS, lsdFD, lsdDC, lsdBT, lsdDT });
            p_ConvertUtils.GetAttribValues(*selContact, fricAttrNames, fricAttrVals);
        }
        else
        {
            sdiStringList fricAttrNames({ "LSDYNA_FS", "LSDYNA_FD", "LSDYNA_DC", "BT", "DT"});
            vector< reference_wrapper<double> > fricAttrVals({ lsdFS, lsdFD, lsdDC, lsdBT, lsdDT });
            p_ConvertUtils.GetAttribValues(*selContact, fricAttrNames, fricAttrVals);
        }

        // check for *DEFINE_FRICTION connected to contact interfaces

        sdiValue tempValue;
        sdiValueEntity ContactFricEntity;
        int ContactFric_ID;
        selContact->GetValue(sdiIdentifier("LSDYNA_FD_DefineFriction"), tempValue);
        tempValue.GetValue(ContactFricEntity);
        ContactFric_ID = ContactFricEntity.GetId();

        int numFriction = selFriction.Count();
        EntityType radFricType = p_radiossModel->GetEntityType("/FRICTION");

        int frictionId = 0;
        if(lsdFS == -2)
        {
          if(numFriction == 1)
          {
            // same friction to apply to all interfaces in the model
            while (selFriction.Next())
            {
              frictionId = selFriction->GetId();
              radInterEdit.SetValue(sdiIdentifier("Fric_ID"), sdiValue(sdiValueEntity(radFricType, frictionId)));
            }
          }
          else if(numFriction > 1)
          {
            int cpt = 0;
            while (selFriction.Next())
            {
              frictionId = selFriction->GetId();
              if(frictionId == ContactFric_ID)
              {
                radInterEdit.SetValue(sdiIdentifier("Fric_ID"), sdiValue(sdiValueEntity(radFricType, frictionId)));
                cpt++;
              }
            }
            if(cpt == 0)
            {
            // no *DEFINE_FRICTION, then set FS=0, FD=0
            DynaToRad::ShowMessage(sdiMessageHandler::Level::Warning, 29,keyWord.c_str(), dynaContactId, dynaContactName.c_str());
            lsdFS = 0;
            lsdFD = 0;
            }
          }
          else
          {
            // no *DEFINE_FRICTION, then set FS=0, FD=0
            DynaToRad::ShowMessage(sdiMessageHandler::Level::Warning, 29,keyWord.c_str(), dynaContactId, dynaContactName.c_str());
            lsdFS = 0;
            lsdFD = 0;
          }
        }
        //

        lsdSFST = (lsdSFST == 0.0) ? 1.0 : lsdSFST;
        lsdSFMT = (lsdSFMT == 0.0) ? 1.0 : lsdSFMT;
        lsdSFS = (lsdSFS == 0.0) ? 1.0 : lsdSFS;
        lsdSFM = (lsdSFM == 0.0) ? 1.0 : lsdSFM;
        lsdFSF = (lsdFSF == 0.0) ? 1.0 : lsdFSF;

        double stfac = 0.0;
        double gapScale = 0.0;
        double slaveGapMax = lsdSST / 2.0; //Gap_max_s
        double masterGapMax = 0.0;
        double gap = 0.0;
        double GapMax = 0.0;
        double Gapmin = 0.0;

        if (keyWord.find("AUTOMATIC_NODES_TO_SURFACE") != keyWord.npos ||
            keyWord.find("AUTOMATIC_SURFACE_TO_SURFACE") != keyWord.npos||
            keyWord.find("ERODING_SURFACE_TO_SURFACE") != keyWord.npos ||
            keyWord.find("AUTOMATIC_BEAMS_TO_SURFACE") != keyWord.npos)
        {
            stfac = min(lsdSFS, lsdSFM); //Stfac
            gapScale = (lsdSFST + lsdSFMT ) / 2.0; //Gap_scale
            masterGapMax = lsdMST / 2.0; // Gap_max_m
        }
        else if (keyWord.find("TIED_NODES_TO_SURFACE") != keyWord.npos ||
            keyWord.find("TIEBREAK_NODES_TO_SURFACE") != keyWord.npos )
        {
            stfac = lsdSFS;
            gapScale = lsdSFST;
            masterGapMax = slaveGapMax; // Gap_max_m
        }
        else if (keyWord.find("NODES_TO_SURFACE") != keyWord.npos)
        {
            stfac = min(lsdSFS, lsdSFM); //Stfac
            gapScale = (lsdSFST + lsdSFMT) / 2.0; //Gap_scale
            masterGapMax = lsdMST / 2.0; // Gap_max_m
        }
        else if (keyWord.find("TIED_SURFACE_TO_SURFACE") != keyWord.npos)
        {
            stfac = 0.0;
            gap = fabs(lsdSFS*lsdSST+lsdSFM*lsdMST)/2.0;
        }
        else if (keyWord.find("CONTACT_AIRBAG_SINGLE_SURFACE") != keyWord.npos)  // TYPE19
        {
            stfac = lsdSFS;
            gapScale = lsdSFST;
            masterGapMax = slaveGapMax; // Gap_max_m
        }
        else if(keyWord.find("ERODING_SINGLE_SURFACE") != keyWord.npos && interType == "TYPE7")
        {
            GapMax = slaveGapMax;
            Gapmin = slaveGapMax;
        }
        else if(keyWord.find("AUTOMATIC_GENERAL") != keyWord.npos)
        {
            stfac = lsdSFS;
            gapScale = lsdSFST;
            if(interType == "TYPE7" || interType == "TYPE11" || interType == "TYPE19")
            {
                //Gapmin = fabs(lsdSFS*lsdSST+lsdSFM*lsdMST)/2.0;
                Gapmin = fabs(lsdSST*lsdSFST+lsdMST*lsdSFMT)/2.0;
                GapMax = Gapmin;
            }
            else if(interType == "TYPE25")
            {
                //masterGapMax = slaveGapMax; // Gap_max_m
            }
        }
        else
        {
            stfac = lsdSFS;
            gapScale = lsdSFST;
            masterGapMax = slaveGapMax; // Gap_max_m
            if(interType != "TYPE25")masterGapMax = slaveGapMax; // Gap_max_m
        }
        
        if  (interType == "TYPE25")
        {
            masterGapMax = 0.0;
            slaveGapMax  = 0.0;
        }


        double fric = lsdFD * lsdFSF; //Fric
        double radC5 = (lsdFS - lsdFD) * lsdFSF; //C5
        double radC6 = -lsdDC; // C6

        if (interType == "TYPE10")
        {
            sdiConvert::ContainStrVsDouble mapAttrNameVsAttrVal({ {"Tstart", lsdBT}, {"Tstop", lsdDT}, {"GAP", gap},
                                                                  {"STFAC", stfac} });
            for (pair<sdiString, double> tempPair : mapAttrNameVsAttrVal)
            {
                radInterEdit.SetValue(sdiIdentifier(tempPair.first), sdiValue(tempPair.second));
            }
        }
        else if(interType == "TYPE7")
        {
            sdiConvert::ContainStrVsDouble mapAttrNameVsAttrVal({ {"Tstart", lsdBT}, {"Tstop", lsdDT}, {"Fric", fric}, {"C5", radC5},{"Fscalegap", gapScale},
                                                                  {"C6", radC6}, {"Stfac", stfac}, {"Gap_max", GapMax}, {"GAPmin", Gapmin}  });
            for (pair<sdiString, double> tempPair : mapAttrNameVsAttrVal)
            {
                radInterEdit.SetValue(sdiIdentifier(tempPair.first), sdiValue(tempPair.second));
            }
            radInterEdit.SetValue(sdiIdentifier("Ifric"), sdiValue(2));
        }
        else if(interType == "TYPE11")
        {
            fric = lsdFS;
            sdiConvert::ContainStrVsDouble mapAttrNameVsAttrVal({ {"Tstart", lsdBT}, {"Tstop", lsdDT}, {"Fric", fric},
                                                                  {"Stfac", stfac},  {"GAPmin", Gapmin}  });
            for (pair<sdiString, double> tempPair : mapAttrNameVsAttrVal)
            {
                radInterEdit.SetValue(sdiIdentifier(tempPair.first), sdiValue(tempPair.second));
            }
        }
        else
        {
            sdiConvert::ContainStrVsDouble mapAttrNameVsAttrVal({ {"Tstart", lsdBT}, {"Tstop", lsdDT}, {"Fric", fric}, {"C5", radC5},{"Gap_scale", gapScale},
                                                                  {"C6", radC6}, {"Stfac", stfac}, {"Gap_max_s", slaveGapMax}, {"Gap_max_m", masterGapMax} });
            for (pair<sdiString, double> tempPair : mapAttrNameVsAttrVal)
            {
                radInterEdit.SetValue(sdiIdentifier(tempPair.first), sdiValue(tempPair.second));
            }
            radInterEdit.SetValue(sdiIdentifier("Ifric"), sdiValue(2));
        }


        sdiValue tempValue1;
        double lsdSofscl = 0.0;
        selContact->GetValue(sdiIdentifier("SOFSCL"), tempValue1);
        tempValue1.GetValue(lsdSofscl);
        if(interType != "TYPE7")
        {
            if (lsdSofscl == 0.0)
            {
                radInterEdit.SetValue(sdiIdentifier("STFAC_MDT"), sdiValue(0.05));
            }
            else
            {
                p_ConvertUtils.SetExpressionValue(contactRead, radInterEdit,"0.5*SOFSCL","STFAC_MDT");
            }
        }
        if (keyWord.find("CONTACT_AIRBAG_SINGLE_SURFACE") != keyWord.npos)  // TYPE19
        {
            p_ConvertUtils.CopyValue(contactRead, radInterEdit, "DTSTIF", "DTSTIF");
        }
        else if(interType != "TYPE7" && interType != "TYPE11" && interType != "TYPE19")
        {
            p_ConvertUtils.CopyValue(contactRead, radInterEdit, "CONT_DTSIF_field", "DTSTIF");
        }

        if (keyWord.find("CONTACT_TIEBREAK_NODES")       != keyWord.npos )
        {
        }
        else if (keyWord.find("AUTOMATIC_SURFACE_TO_SURFACE")   != keyWord.npos ||
                 keyWord.find("ERODING_SURFACE_TO_SURFACE")     != keyWord.npos ||
                 keyWord.find("AUTOMATIC_SINGLE_SURFACE")       != keyWord.npos ||
                 keyWord.find("AUTOMATIC_NODES_TO_SURFACE")       != keyWord.npos ||
                 keyWord.find("AUTOMATIC_BEAMS_TO_SURFACE")       != keyWord.npos)
        {
            radInterEdit.SetValue(sdiIdentifier("Istf"), sdiValue(2));
            int lsdDEPTH = GetValue<int>(*selContact, "LSDYNA_DEPTH");
            if (lsdSOFT == 0)
            {
                radInterEdit.SetValue(sdiIdentifier("Istf"), sdiValue(2));
                radInterEdit.SetValue(sdiIdentifier("IPSTIF"), sdiValue(1));
            }
            else if (lsdSOFT == 1)
            {
                radInterEdit.SetValue(sdiIdentifier("Istf"), sdiValue(4));
            }
            else if (lsdSOFT == 2)
            {
                radInterEdit.SetValue(sdiIdentifier("Istf"), sdiValue(2));
                radInterEdit.SetValue(sdiIdentifier("IPSTIF"), sdiValue(1));
                if (lsdDEPTH == 5 || lsdDEPTH == 15 || lsdDEPTH == 25 || lsdDEPTH == 35 ||
                    lsdDEPTH == 45 || lsdDEPTH == 55)
                  radInterEdit.SetValue(sdiIdentifier("Iedge"), sdiValue(22));
            }
        }
        else if (keyWord.find("AUTOMATIC_GENERAL")            != keyWord.npos ||
                 keyWord.find("ERODING_SINGLE_SURFACE")       != keyWord.npos ||
                 keyWord.find("ERODING_NODES_TO_SURFACE")     != keyWord.npos ||
                 keyWord.find("NODES_TO_SURFACE")             != keyWord.npos ||
                 keyWord.find("TIEBREAK_NODES")               != keyWord.npos )
        {
            if (lsdSOFT >= 1)
            {
                radInterEdit.SetValue(sdiIdentifier("Istf"), sdiValue(4));
            }
            else
            {
                radInterEdit.SetValue(sdiIdentifier("Istf"), sdiValue(2));
            }
        }
        else if (keyWord.find("CONTACT_AIRBAG_SINGLE_SURFACE") != keyWord.npos)
        {
            if(interType == "TYPE25")
            {
                radInterEdit.SetValue(sdiIdentifier("Istf"), sdiValue(2));
                if (lsdSOFT == 0)
                {
                    radInterEdit.SetValue(sdiIdentifier("Istf"), sdiValue(2));
                }
                else if (lsdSOFT == 1)
                {
                    radInterEdit.SetValue(sdiIdentifier("Istf"), sdiValue(4));
                }
                else if (lsdSOFT == 2)
                {
                    radInterEdit.SetValue(sdiIdentifier("Istf"), sdiValue(2));
                    radInterEdit.SetValue(sdiIdentifier("IPSTIF"), sdiValue(1));
                }
            }
            else if(interType == "TYPE19")
            {
                gap = lsdSST/2.0*lsdSFST;
                radInterEdit.SetValue(sdiIdentifier("Gapmin"), sdiValue(gap));
                radInterEdit.SetValue(sdiIdentifier("Istf"), sdiValue(4));
                radInterEdit.SetValue(sdiIdentifier("Idel"), sdiValue(2));
                radInterEdit.SetValue(sdiIdentifier("Ibag"), sdiValue(1));
                radInterEdit.SetValue(sdiIdentifier("Edge_scale_gap"), sdiValue(0.9));
            }
        }
        else if (interType == "TYPE25")
        {
            radInterEdit.SetValue(sdiIdentifier("Istf"), sdiValue(4));
        }
        else if (interType == "TYPE10")
        {
            radInterEdit.SetValue(sdiIdentifier("ITIED"), sdiValue(0));
            radInterEdit.SetValue(sdiIdentifier("VIS_S"), sdiValue(0.0));
            radInterEdit.SetValue(sdiIdentifier("BUMULT"), sdiValue(0.0));
        }
    }
}

void sdiD2R::ConvertContact::ConvertContactInterior()
{
    EntityType radPartType = p_radiossModel->GetEntityType("/PART");
    EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");

    SelectionRead selContInterior(p_lsdynaModel, "*CONTACT_INTERIOR");
    //sdiConvert::ContainStrVsInt mapDefaultIntVals( { { {"Istf",2}, {"Igap",3}, {"Idel",2}, {"Inacti",5}, {"Irem_gap",1}, {"Iform",2} } });
    while (selContInterior.Next())
    {
        int slaveSetListMax = 0;
        sdiValue tempValue(slaveSetListMax);
        selContInterior->GetValue(sdiIdentifier("secondarysetlistmax"), tempValue);
        tempValue.GetValue(slaveSetListMax);
        sdiString contactName = selContInterior->GetName();
        unsigned int contactID = selContInterior->GetId();
        sdiConvert::SDIHandlReadList sourceHandleList = { {selContInterior->GetHandle()} };

        for (int i = 0; i < slaveSetListMax; ++i)
        {
            HandleRead setHRead;
            selContInterior->GetEntityHandle(sdiIdentifier("PSID", 0, i), setHRead);
            EntityRead setRead(p_lsdynaModel, setHRead);
            sdiString keyWord = setRead.GetKeyword();
            sdiUIntList idList;
            if (keyWord.find("ADD") != keyWord.npos)
            {
                int partSetIdsMax;
                sdiValue tempVal(partSetIdsMax);
                setRead.GetValue(sdiIdentifier("idsmax"), tempVal);
                tempVal.GetValue(partSetIdsMax);

                sdiIntList parSetIdList;
                tempVal = sdiValue(parSetIdList);
                setRead.GetValue(sdiIdentifier("ids"), tempVal);
                tempVal.GetValue(parSetIdList);
                for (int j = 0; j < partSetIdsMax; ++j)
                {
                    HandleRead partSetHRead;
                    if (p_lsdynaModel->FindById("*SET_PART", parSetIdList[j], partSetHRead))
                    {
                        sdiValueEntityList entityList;
                        tempVal = sdiValue(entityList);
                        partSetHRead.GetValue(p_lsdynaModel, sdiIdentifier("ids"), tempVal);
                        tempVal.GetValue(entityList);
                        for (const auto entity : entityList)
                            idList.push_back(entity.GetId());
                    }
                }
            }
            else
            {
                sdiValueEntityList entityList;
                sdiValue tempVal(entityList);
                setRead.GetValue(sdiIdentifier("ids"), tempVal);
                tempVal.GetValue(entityList);
                entityList.GetIdList(idList);
            }
            for (unsigned int partId : idList)
            {
                /*HandleEdit radSetHEdit;
                p_radiossModel->CreateEntity(radSetHEdit, "/SET/GENERAL", "SET_GEN_CONTACT_INTERIOR_Part_" + to_string(partId));
                EntityEdit radSetEdit(p_radiossModel, radSetHEdit);
                radSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                radSetEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("PART")));
                radSetEdit.SetValue(sdiIdentifier("opt_A", 0, 0), sdiValue(1));
                radSetEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
                radSetEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(radPartType, sdiUIntList(1, partId))));

                HandleEdit radInterHEdit;
                if (p_radiossModel->IsIdAvailable(destEntityType, contactID))
                    p_radiossModel->CreateEntity(radInterHEdit, "/INTER/TYPE7", contactName + "_Part_" + to_string(partId), contactID);
                else
                    p_radiossModel->CreateEntity(radInterHEdit, "/INTER/TYPE7", contactName + "_Part_" + to_string(partId));
                EntityEdit radInterEdit(p_radiossModel, radInterHEdit);
                radInterEdit.SetEntityHandle(sdiIdentifier("grnod_id"), radSetHEdit);
                radInterEdit.SetEntityHandle(sdiIdentifier("surf_id"), radSetHEdit);
                radInterEdit.SetValue(sdiIdentifier("Fpenmax"), sdiValue(0.8));
                for (auto tempPair : mapDefaultIntVals)
                    radInterEdit.SetValue(sdiIdentifier(tempPair.first), sdiValue(tempPair.second));*/

                // set Icontrol parameter to 1 in /PROP
                HandleEdit partradHRead;
                p_radiossModel->FindById(p_radiossModel->GetEntityType("/PART"), partId, partradHRead);
                HandleEdit propradRead;
                partradHRead.GetEntityHandle(p_radiossModel,sdiIdentifier("prop_ID"), propradRead);
                if(propradRead.IsValid())
                {
                    propradRead.SetValue(p_radiossModel,sdiIdentifier("Icontrol"), sdiValue(1));
                }

                /*sdiConvert::Convert::PushToConversionLog(std::make_pair(radInterHEdit, sourceHandleList));
                sdiConvert::Convert::PushToConversionLog(std::make_pair(radSetHEdit, sourceHandleList));*/
            }

        }
    }
}

void sdiD2R::ConvertContact::ConvertContactForceTranducerPenalty()
{
    EntityType radPartType = p_radiossModel->GetEntityType("/PART");
    EntityType radBoxType = p_radiossModel->GetEntityType("/BOX");
    EntityType radSetType = p_radiossModel->GetEntityType("/SET");
    EntityType radInterType = p_radiossModel->GetEntityType("/INTER");

    SelectionRead selContForceTranducePenalty(p_lsdynaModel, "*CONTACT_FORCE_TRANSDUCER_PENALTY");
    while (selContForceTranducePenalty.Next())
    {
        sdiValueEntity ssidEntity;
        sdiValueEntity sboxEntity;
        sdiValueEntity msidEntity;
        sdiValueEntity mboxEntity;
        vector<reference_wrapper<sdiValueEntity>> contactSetEntities({ ssidEntity, sboxEntity, msidEntity, mboxEntity });
        sdiStringList contactSetAttrNames({ "SSID", "SBOXID", "MSID", "MBOXID" });
        p_ConvertUtils.GetAttribValues(*selContForceTranducePenalty, contactSetAttrNames, contactSetEntities);
        unsigned int ssid = ssidEntity.GetId();
        unsigned int sbox = sboxEntity.GetId();
        unsigned int msid = msidEntity.GetId();
        unsigned int mbox = mboxEntity.GetId();

        int lsdSSTYP;
        sdiValue tempValue(lsdSSTYP);
        selContForceTranducePenalty->GetValue(sdiIdentifier("SSTYP"), tempValue);
        tempValue.GetValue(lsdSSTYP);

        int lsdMSTYP;
        sdiValue tempValue1(lsdMSTYP);
        selContForceTranducePenalty->GetValue(sdiIdentifier("MSTYP"), tempValue1);
        tempValue1.GetValue(lsdMSTYP);

        sdiString contactName = selContForceTranducePenalty->GetName();
        unsigned int contactID = selContForceTranducePenalty->GetId();
        sdiConvert::SDIHandlReadList sourceHandleList = { {selContForceTranducePenalty->GetHandle()} };


        HandleEdit radInterHEdit;
        if (p_radiossModel->IsIdAvailable(destEntityType, contactID))
            p_radiossModel->CreateEntity(radInterHEdit, "/INTER/SUB", contactName , contactID);
        else
            p_radiossModel->CreateEntity(radInterHEdit, "/INTER/SUB", contactName );

        EntityEdit radInterEdit(p_radiossModel, radInterHEdit);

        HandleEdit radSetHEdit;

        if (ssid != 0)
        {
            switch (lsdSSTYP)
            {
            case 0:
                radInterEdit.SetValue(sdiIdentifier("Main_ID2"), sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(ssid, "*SET_SEGMENT"))));
                break;
            case 1:
                radInterEdit.SetValue(sdiIdentifier("Main_ID2"), sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(ssid, "*SET_SHELL"))));
                break;
            case 2:
                radInterEdit.SetValue(sdiIdentifier("Main_ID2"), sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(ssid, "*SET_PART"))));
                break;
            case 3:
            {
                p_radiossModel->CreateEntity(radSetHEdit, "/SET/GENERAL", "Component");
                EntityEdit radSetEdit(p_radiossModel, radSetHEdit);
                radSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                radSetEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
                radSetEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(radPartType, sdiUIntList(1, ssid))));
                radSetEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("PART")));
                radInterEdit.SetEntityHandle(sdiIdentifier("Main_ID2"), radSetHEdit);
                break;
            }
            case 5:
            {
                p_radiossModel->CreateEntity(radSetHEdit, "/SET/GENERAL", "All");
                EntityEdit radSetEdit(p_radiossModel, radSetHEdit);
                radSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                radSetEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("ALL")));
                radInterEdit.SetEntityHandle(sdiIdentifier("Main_ID2"), radSetHEdit);
                break;
            }
            case 6:
            {
                p_radiossModel->CreateEntity(radSetHEdit, "/SET/GENERAL", "Delete Part");
                EntityEdit radSetEdit(p_radiossModel, radSetHEdit);
                radSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(2));
                radSetEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("ALL")));
                radSetEdit.SetValue(sdiIdentifier("opt_D", 0, 1), sdiValue(1));
                radSetEdit.SetValue(sdiIdentifier("KEY_type", 0, 1), sdiValue(sdiString("SET")));
                radSetEdit.SetValue(sdiIdentifier("idsmax", 0, 1), sdiValue(1));
                radSetEdit.SetValue(sdiIdentifier("ids", 0, 1), sdiValue(sdiValueEntityList(radSetType, 
                    sdiUIntList(1, DynaToRad::GetRadiossSetIdFromLsdSet(ssid, "*SET_PART")))));
                radInterEdit.SetEntityHandle(sdiIdentifier("Main_ID2"), radSetHEdit);
                break;
            }
            default:
                break;
            }
        }

        HandleEdit radSetHEdit2;

        if (ssid != 0 && sbox != 0)
        {
            p_radiossModel->CreateEntity(radSetHEdit2, "/SET/GENERAL", "Intersection");
            EntityEdit radSetEdit2(p_radiossModel, radSetHEdit2);
            radSetEdit2.SetValue(sdiIdentifier("clausesmax"), sdiValue(2));
            radSetEdit2.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("SET")));
            radSetEdit2.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));

            switch (lsdSSTYP)
            {
            case 0:
                radSetEdit2.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(radSetType, 
                    sdiUIntList(1, DynaToRad::GetRadiossSetIdFromLsdSet(ssid, "*SET_SEGMENT")))));
                break;
            case 1:
                radSetEdit2.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(radSetType, 
                    sdiUIntList(1, DynaToRad::GetRadiossSetIdFromLsdSet(ssid, "*SET_SHELL")))));
            case 2:
                radSetEdit2.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(radSetType, 
                    sdiUIntList(1, DynaToRad::GetRadiossSetIdFromLsdSet(ssid, "*SET_PART")))));
                break;
            case 3:
            {
                EntityEdit radSetEdit(p_radiossModel, radSetHEdit);
                radSetEdit2.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntity(radSetType,radSetEdit.GetId())));
                break;
            }
            case 5:
            {
                EntityEdit radSetEdit(p_radiossModel, radSetHEdit);
                radSetEdit2.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntity(radSetType,radSetEdit.GetId())));
                break;
            }
            case 6:
            {
                EntityEdit radSetEdit(p_radiossModel, radSetHEdit);
                radSetEdit2.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntity(radSetType,radSetEdit.GetId())));
                break;
            }
            default:
                break;
            }

            radSetEdit2.SetValue(sdiIdentifier("opt_I", 0, 1), sdiValue(1));
            radSetEdit2.SetValue(sdiIdentifier("KEY_type", 0, 1), sdiValue(sdiString("BOX")));
            radSetEdit2.SetValue(sdiIdentifier("idsmax", 0, 1), sdiValue(1));
            radSetEdit2.SetValue(sdiIdentifier("ids", 0, 1), sdiValue(sdiValueEntityList(radBoxType, sdiUIntList(1, sbox))));
            radInterEdit.SetEntityHandle(sdiIdentifier("Main_ID2"), radSetHEdit2);
        }

        HandleEdit radSetHEdit3;

        if (msid != 0)
        {
            switch (lsdMSTYP)
            {
            case 0:
                radInterEdit.SetValue(sdiIdentifier("Main_ID1"), sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(msid, "*SET_SEGMENT"))));
                break;
            case 1:
                radInterEdit.SetValue(sdiIdentifier("Main_ID1"), sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(msid, "*SET_SHELL"))));
                break;
            case 2:
                radInterEdit.SetValue(sdiIdentifier("Main_ID1"), sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(msid, "*SET_PART"))));
                break;
            case 3:
            {
                p_radiossModel->CreateEntity(radSetHEdit3, "/SET/GENERAL", "Component");
                EntityEdit radSetEdit(p_radiossModel, radSetHEdit3);
                radSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                radSetEdit.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));
                radSetEdit.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(radPartType, sdiUIntList(1, msid))));
                radSetEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("PART")));
                radInterEdit.SetEntityHandle(sdiIdentifier("Main_ID1"), radSetHEdit3);
                break;
            }
            case 5:
            {
                p_radiossModel->CreateEntity(radSetHEdit3, "/SET/GENERAL", "All");
                EntityEdit radSetEdit(p_radiossModel, radSetHEdit3);
                radSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(1));
                radSetEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("ALL")));
                radInterEdit.SetEntityHandle(sdiIdentifier("Main_ID1"), radSetHEdit3);
                break;
            }
            case 6:
            {
                p_radiossModel->CreateEntity(radSetHEdit3, "/SET/GENERAL", "Delete Part");
                EntityEdit radSetEdit(p_radiossModel, radSetHEdit3);
                radSetEdit.SetValue(sdiIdentifier("clausesmax"), sdiValue(2));
                radSetEdit.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("ALL")));
                radSetEdit.SetValue(sdiIdentifier("opt_D", 0, 1), sdiValue(1));
                radSetEdit.SetValue(sdiIdentifier("KEY_type", 0, 1), sdiValue(sdiString("SET")));
                radSetEdit.SetValue(sdiIdentifier("idsmax", 0, 1), sdiValue(1));
                radSetEdit.SetValue(sdiIdentifier("ids", 0, 1), sdiValue(sdiValueEntityList(radSetType, 
                    sdiUIntList(1, DynaToRad::GetRadiossSetIdFromLsdSet(msid, "*SET_PART")))));
                radInterEdit.SetEntityHandle(sdiIdentifier("Main_ID1"), radSetHEdit3);
                break;
            }
            default:
                break;
            }
        }

        HandleEdit radSetHEdit4;

        if (msid  != 0 && mbox != 0)
        {
            p_radiossModel->CreateEntity(radSetHEdit4, "/SET/GENERAL", "Intersection");
            EntityEdit radSetEdit2(p_radiossModel, radSetHEdit4);
            radSetEdit2.SetValue(sdiIdentifier("clausesmax"), sdiValue(2));
            radSetEdit2.SetValue(sdiIdentifier("KEY_type", 0, 0), sdiValue(sdiString("SET")));
            radSetEdit2.SetValue(sdiIdentifier("idsmax", 0, 0), sdiValue(1));

            switch (lsdMSTYP)
            {
            case 0:
                radSetEdit2.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(radSetType, 
                    sdiUIntList(1, DynaToRad::GetRadiossSetIdFromLsdSet(msid, "*SET_SEGMENT")))));
                break;
            case 1:
                radSetEdit2.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(radSetType, 
                    sdiUIntList(1, DynaToRad::GetRadiossSetIdFromLsdSet(msid, "*SET_SHELL")))));
            case 2:
                radSetEdit2.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntityList(radSetType, 
                    sdiUIntList(1, DynaToRad::GetRadiossSetIdFromLsdSet(msid, "*SET_PART")))));
                break;
            case 3:
            {
                EntityEdit radSetEdit(p_radiossModel, radSetHEdit3);
                radSetEdit2.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntity(radSetType,radSetEdit.GetId())));
                break;
            }
            case 5:
            {
                EntityEdit radSetEdit(p_radiossModel, radSetHEdit3);
                radSetEdit2.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntity(radSetType,radSetEdit.GetId())));
                break;
            }
            case 6:
            {
                EntityEdit radSetEdit(p_radiossModel, radSetHEdit3);
                radSetEdit2.SetValue(sdiIdentifier("ids", 0, 0), sdiValue(sdiValueEntity(radSetType,radSetEdit.GetId())));
                break;
            }
            default:
                break;
            }

            radSetEdit2.SetValue(sdiIdentifier("opt_I", 0, 1), sdiValue(1));
            radSetEdit2.SetValue(sdiIdentifier("KEY_type", 0, 1), sdiValue(sdiString("BOX")));
            radSetEdit2.SetValue(sdiIdentifier("idsmax", 0, 1), sdiValue(1));
            radSetEdit2.SetValue(sdiIdentifier("ids", 0, 1), sdiValue(sdiValueEntityList(radBoxType, sdiUIntList(1, mbox))));
            radInterEdit.SetEntityHandle(sdiIdentifier("Main_ID1"), radSetHEdit4);

        }

        sdiConvert::Convert::PushToConversionLog(std::make_pair(radInterHEdit, sourceHandleList));
        EntityEdit radSetEdit2(p_radiossModel, radSetHEdit2);
        EntityEdit radSetEdit3(p_radiossModel, radSetHEdit3);
        EntityEdit radSetEdit4(p_radiossModel, radSetHEdit4);
        if (radSetEdit2.GetId() != 0) sdiConvert::Convert::PushToConversionLog(std::make_pair(radSetHEdit2, sourceHandleList));
        if (radSetEdit3.GetId() != 0) sdiConvert::Convert::PushToConversionLog(std::make_pair(radSetHEdit3, sourceHandleList));
        if (radSetEdit4.GetId() != 0) sdiConvert::Convert::PushToConversionLog(std::make_pair(radSetHEdit4, sourceHandleList));

    }
}

void sdiD2R::ConvertContact::p_CreateThInter()
{
    SelectionRead selContact(p_lsdynaModel, "*CONTACT");
    while (selContact.Next())
    {
        sdiUIntList allInterList;
        SelectionRead selRadInter(p_radiossModel, "/INTER");

        allInterList.reserve(allInterList.size());
        while (selRadInter.Next())
            allInterList.push_back(selRadInter->GetId());

        if (!allInterList.empty())
        {
            HandleEdit radThInterHEdit;
            p_radiossModel->CreateEntity(radThInterHEdit, "/TH/INTER", "TH-INTER");
            EntityEdit radThInterEdit(p_radiossModel, radThInterHEdit);

            radThInterEdit.SetValue(sdiIdentifier("idsmax"), sdiValue((int)allInterList.size()));
            radThInterEdit.SetValue(sdiIdentifier("Number_Of_Variables"), sdiValue(1));
            radThInterEdit.SetValue(sdiIdentifier("Obj"), sdiValue(sdiValueEntityList(destEntityType, allInterList)));
            radThInterEdit.SetValue(sdiIdentifier("var"), sdiValue(sdiStringList({ "DEF       " "CE_ELAST  " 
                                                                                   "CE_FRIC   " "CE_DAMP   "})));

            sdiConvert::SDIHandlReadList sourceHandles = { {selContact->GetHandle()} };
            sdiConvert::Convert::PushToConversionLog(std::make_pair(radThInterHEdit, sourceHandles));
        }

        break;
    }
}
