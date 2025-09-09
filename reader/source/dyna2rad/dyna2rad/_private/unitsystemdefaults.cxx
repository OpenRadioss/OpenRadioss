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

#include <dyna2rad/unitsystemdefaults.h>

using namespace sdiD2R;
using namespace std;
using namespace sdi;
using namespace sdiConvert;

Units::Units(sdiString currentUnits) :
    p_CurrentUnits(currentUnits),
    p_UnitSystemList({ "m_s_kg", "mm_ms_kg" , "mm_ms_g" , "mm_s_Mg"})
{
}


void Units::PopulateDefaultValues()
{
    p_CommonIntList["PROP_SOLID_ISOLID"] = 0;
    p_CommonIntList["PROP_SOLID_Ismstr"] = 2;
    p_CommonIntList["PROP_SOLID_ITHICK"] = 1;
    p_CommonIntList["PROP_SOLID_IPLAS"] =  1;

    p_CommonDoubleList["TYPE7_Fpenmax"] = 0.0;
    /*
        kg, m, s: mass=2e-03kg, inertia=2e-05kg*m*
        kg, mm, ms: mass=2e-03kg, inertia=2e-02kg*mm*
        g, mm, ms: mass=2g, inertia=20g*mm*
        Mg, mm, s: mass=2e-06Mg, inertia=2e-05Mg*mm*
    */

    vector<double> beamMassList = { {2.00E-3}, {2.00E-03}, {2.00}, {2.00E-06}};
    vector<double> beamInertiaList = { {2.00E-5}, {2.00E-02}, {2.00E01}, {2.00E-05}};
    vector<double> stminList = { {1000000.0}, {1000.0}, {1.0}, {1000.0}};

    vector <vector<double>> listStrainRatesList = 
    {
        {{0.0, 1.0E-4, 0.001, 0.01, 0.1,   1.0,    10.0,    100.0,    1000.0,   10000.0}},
        {{0.0, 0.1,    1.0,   10.0, 100.0, 1000.0, 10000.0, 100000.0, 1.0E+06,  1.0E+07}},
        {{0.0, 0.1,    1.0,   10.0, 100.0, 1000.0, 10000.0, 100000.0, 1.0E+06,  1.0E+07}},
        {{0.0, 1.0E-4, 0.001, 0.01, 0.1,   1.0,    10.0,    100.0,    1000.0,   10000.0}},
    };
    vector <vector<double>> listMatUnknownVals =
    {
        {{7.89E-03, 2.10E+11, 0.3}},
        {{7.89E-06, 2.10E+02, 0.3}},
        {{7.89E-03, 2.10E+11, 0.3}},
        {{7.89E-09, 2.10E+05, 0.3}}
    };

    /*E, SIGY, RHO, area*/
    vector<vector<double>> matLaw2DefaultVals =
    {
        {{2.10E-07, 1.00E-12, 1.00E+03, 2.00E06}},
        {{2.10E+2, 1.00E-3, 1.00E-06, 2.0}},
        {{2.10E-1, 1.00E-6, 1.00E-03, 2.0}},
        {{2.10E-1, 1.00E-6, 1.00E-09, 2.0}}
    };

    /*Universal gas constant*/
    vector<double> UniversalGasConstantDefaultVals =
    {
        {8.314, 8.314, 8314000000, 8314}
    };

    /* PROP/TYPE25  mass, inertia, K1, k2, k3, k4*/
    vector<vector<double>> ListPropSprAxiDefaultVals =
    {
        {0.001, 0.01E-3, 100E6, 100E6, 1000E6, 1000E6},
        {0.001, 0.01, 100.0, 100.0, 1000.0, 1000.0},
        {1.0, 10.0, 100.0E3, 100.0E3, 1000.0E3, 1000.0E3},
        {0.001E-3, 0.01E-3, 100.0E3, 100.0E3, 1000.0E3, 1000.0E3}
    };

    for (int i = 0; i < p_UnitSystemList.size(); ++i)
    {
        sdiString unitSyst = p_UnitSystemList[i];
        p_DefaultsDoubleValuesMap[unitSyst]["PROP_SPR_MASS"] = std::vector<double>(1, beamMassList[i]);
        p_DefaultsDoubleValuesMap[unitSyst]["PROP_SPR_BEAM_INERTIA"] = std::vector<double>(1, beamInertiaList[i]);
        p_DefaultsDoubleValuesMap[unitSyst]["INTER_Stmin"] = std::vector<double>(1, stminList[i]);
        p_DefaultsDoubleValuesMap[unitSyst]["MAT_STRAIN_RATES"] = listStrainRatesList[i];
        p_DefaultsDoubleValuesMap[unitSyst]["MAT_UNKNOWN_VALS"] = listMatUnknownVals[i];
        p_DefaultsDoubleValuesMap[unitSyst]["MATL2_Default_VALS"] = matLaw2DefaultVals[i];
        p_DefaultsDoubleValuesMap[unitSyst]["UNIVERSAL_GAS_CONSTANT"] = std::vector<double>(1, UniversalGasConstantDefaultVals[i]);
        p_DefaultsDoubleValuesMap[unitSyst]["PROP_SPR_AXI_VALS"] = ListPropSprAxiDefaultVals[i];
    }
}

int Units::GetDefaultSoliDPropIsolid()
{
    return p_CommonIntList["PROP_SOLID_ISOLID"];
}

int  Units::GetDefaultSoliDPropIsmstr()
{
    return p_CommonIntList["PROP_SOLID_Ismstr"];
}

int  Units::GetDefaultSoliDPropITHICK()
{
    return p_CommonIntList["PROP_SOLID_ITHICK"];
}

int  Units::GetDefaultSoliDPropIPLAS()
{
    return p_CommonIntList["PROP_SOLID_IPLAS"];
}

double Units::GetDefaultBeamMass()
{
    if (find(p_UnitSystemList.begin(), p_UnitSystemList.end(), p_CurrentUnits) != p_UnitSystemList.end())
        return p_DefaultsDoubleValuesMap[p_CurrentUnits]["PROP_SPR_MASS"][0];
    else
        return p_DefaultsDoubleValuesMap["m_s_kg"]["PROP_SPR_MASS"][0];
}

double Units::GetDefaultBeamInertia()
{
    if (find(p_UnitSystemList.begin(), p_UnitSystemList.end(), p_CurrentUnits) != p_UnitSystemList.end())
        return p_DefaultsDoubleValuesMap[p_CurrentUnits]["PROP_SPR_BEAM_INERTIA"][0];
    else
        return p_DefaultsDoubleValuesMap["m_s_kg"]["PROP_SPR_BEAM_INERTIA"][0];;
}

double Units::GetDefaultInterStmin()
{
    if (find(p_UnitSystemList.begin(), p_UnitSystemList.end(), p_CurrentUnits) != p_UnitSystemList.end())
        return p_DefaultsDoubleValuesMap[p_CurrentUnits]["INTER_Stmin"][0];
    else
        return p_DefaultsDoubleValuesMap["m_s_kg"]["INTER_Stmin"][0];;
}

void Units::GetDefaultStrainRateList(vector<double>& strainRateList)
{
    if (find(p_UnitSystemList.begin(), p_UnitSystemList.end(), p_CurrentUnits) != p_UnitSystemList.end())
        strainRateList = p_DefaultsDoubleValuesMap[p_CurrentUnits]["MAT_STRAIN_RATES"];
    else 
        strainRateList = p_DefaultsDoubleValuesMap["m_s_kg"]["MAT_STRAIN_RATES"];
}

void Units::GetDefaultUnKnownMatParams(vector<double>& defaultVals)
{
    if (find(p_UnitSystemList.begin(), p_UnitSystemList.end(), p_CurrentUnits) != p_UnitSystemList.end())
        defaultVals = p_DefaultsDoubleValuesMap[p_CurrentUnits]["MAT_UNKNOWN_VALS"];
    else
        defaultVals = p_DefaultsDoubleValuesMap["m_s_kg"]["MAT_UNKNOWN_VALS"];
}

void Units::GetDefaultMatlaw2Vals(vector<double>& matLaw2Vals)
{
    if (find(p_UnitSystemList.begin(), p_UnitSystemList.end(), p_CurrentUnits) != p_UnitSystemList.end())
        matLaw2Vals = p_DefaultsDoubleValuesMap[p_CurrentUnits]["MATL2_Default_VALS"];
    else
        matLaw2Vals = p_DefaultsDoubleValuesMap["m_s_kg"]["MATL2_Default_VALS"];
}

double Units::GetUniversalGasConstant()
{
    if (find(p_UnitSystemList.begin(), p_UnitSystemList.end(), p_CurrentUnits) != p_UnitSystemList.end())
        return p_DefaultsDoubleValuesMap[p_CurrentUnits]["UNIVERSAL_GAS_CONSTANT"][0];
    else
        return p_DefaultsDoubleValuesMap["m_s_kg"]["UNIVERSAL_GAS_CONSTANT"][0];
}

void Units::GetDefaultProp25Vals(vector<double>& prop25defVals)
{
    if (find(p_UnitSystemList.begin(), p_UnitSystemList.end(), p_CurrentUnits) != p_UnitSystemList.end())
        prop25defVals = p_DefaultsDoubleValuesMap[p_CurrentUnits]["PROP_SPR_AXI_VALS"];
    else
        prop25defVals = p_DefaultsDoubleValuesMap["m_s_kg"]["PROP_SPR_AXI_VALS"];
}
