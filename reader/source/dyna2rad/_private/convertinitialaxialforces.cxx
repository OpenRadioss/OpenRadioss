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

#include <dyna2rad/convertinitialaxialforces.h>
#include <dyna2rad/dyna2rad.h>
#include <typedef.h>
#include <dyna2rad/convertelements.h>

using namespace sdi;
using namespace std;

void sdiD2R::ConvertInitialAxialForce::ConvertInitialAxialForces()
{
    ConvertEntities();
}

void sdiD2R::ConvertInitialAxialForce::ConvertEntities()
{
    ConvertInitialAxialForceBeam();
}

void sdiD2R::ConvertInitialAxialForce::ConvertInitialAxialForceBeam()
{
    SelectionRead selIniAxialForce(p_lsdynaModel, "*INITIAL_AXIAL_FORCE_BEAM");

    EntityType radSpringType = p_radiossModel->GetEntityType("/SPRING");
    EntityType radFunctType = p_radiossModel->GetEntityType("/FUNCT");
    EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");


    while (selIniAxialForce.Next())
    {
        sdiValueEntity lsdBSID;
        sdiValueEntity lsdLCID;

        vector<reference_wrapper<sdiValueEntity>> lsdEntityAttribVals({ lsdBSID, lsdLCID });
        vector <sdiString> lsdEntityAttribNames({ "BSID", "LCID"});
        p_ConvertUtils.GetAttribValues(*selIniAxialForce, lsdEntityAttribNames, lsdEntityAttribVals);

        unsigned int bsid = lsdBSID.GetId();
        

        double lsdSCALE = GetValue<double>(*selIniAxialForce, "SCALE");

        HandleEdit preloadHEdit;
        p_radiossModel->CreateEntity(preloadHEdit, "/PRELOAD/AXIAL", selIniAxialForce->GetName(), selIniAxialForce->GetId());
        EntityEdit preloadEdit(p_radiossModel, preloadHEdit);
        preloadEdit.SetValue(sdiIdentifier("displayname"), sdiValue(selIniAxialForce->GetName()));

        preloadEdit.SetValue(sdiIdentifier("set_id"),sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(bsid,"*SET_BEAM"))));    
        //if (lcid)
        
        preloadEdit.SetValue(sdiIdentifier("Preload"), sdiValue(lsdSCALE));

        sdiValue tempValue;
        HandleRead lcidHandle;
        selIniAxialForce->GetEntityHandle(sdiIdentifier("LCID"), lcidHandle);
        if (lcidHandle.IsValid())
        {
             EntityRead lcidRead(p_lsdynaModel, lcidHandle);
            int nPnts = 0;
            sdiDoubleList crvPoints;
            crvPoints.reserve(2 * nPnts + 2);
            crvPoints.clear();

            double lsdSFA = 1.0;
            double lsdSFO = 1.0;
            double lsdOFFA = 0.0;
            double lsdOFFO = 0.0;

            tempValue = sdiValue(crvPoints);
            lcidRead.GetValue(sdiIdentifier("points"), tempValue);
            tempValue.GetValue(crvPoints);

            tempValue = sdiValue(nPnts);
            lcidRead.GetValue(sdiIdentifier("numberofpoints"), tempValue);
            tempValue.GetValue(nPnts);


            vector< reference_wrapper<double>> attribVals({ lsdSFA, lsdSFO, lsdOFFA, lsdOFFO });
            vector<sdiString> lsdQueryAttribs = { "SFA", "SFO", "OFFA", "OFFO" };
            p_ConvertUtils.GetAttribValues(lcidRead, lsdQueryAttribs, attribVals);
            lsdSFA = (lsdSFA == 0.0) ? 1.0 : lsdSFA;
            lsdSFO = (lsdSFO == 0.0) ? 1.0 : lsdSFO;
            //-----------------------
            // cut monotonicaly function when it descends
            sdiDoubleList monocrvPoints;
            monocrvPoints.reserve(2 * nPnts + 2);
            monocrvPoints.clear();

            monocrvPoints.clear();
            for (size_t j = 0; j < crvPoints.size(); j += 2)
            {
                monocrvPoints.push_back(crvPoints[j]);
                monocrvPoints.push_back(crvPoints[j+1]);
            }
            crvPoints.clear();
            double minordonate = monocrvPoints[1];
            crvPoints.push_back(monocrvPoints[0]);
            crvPoints.push_back(monocrvPoints[1]);
            for (size_t j = 2; j < monocrvPoints.size(); j += 2)
            {
                if (monocrvPoints[j+1] >= minordonate)
                {
                    minordonate = monocrvPoints[j+1];
                    crvPoints.push_back(monocrvPoints[j]);
                    crvPoints.push_back(monocrvPoints[j+1]);
                }
                else
                {
                    break;
                }
            }
            //-----------------------
            HandleEdit functHEdit;
            p_ConvertUtils.CreateCurve("Recalculated_" + to_string(lcidRead.GetId()) + "_INITIAL_AXIAL_FORCE_BEAM_" + 
                                                         to_string(selIniAxialForce->GetId()),
                                    (int)crvPoints.size() / 2, crvPoints, functHEdit, lsdSFA, lsdSFO, lsdOFFA);
            if (functHEdit.IsValid())
            {
                
                preloadEdit.SetEntityHandle(sdiIdentifier("curveid"), functHEdit); 
                sdiConvert::SDIHandlReadList sourcemats = { {selIniAxialForce->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(functHEdit, sourcemats));
            }
        }
        //--------------------      
        sdiConvert::SDIHandlReadList sourceInitialAxialBeam = { {selIniAxialForce->GetHandle()} };
        sdiConvert::Convert::PushToConversionLog(std::make_pair(preloadHEdit, sourceInitialAxialBeam));
        //--------------------
    } // while
}

