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

#include "GlobalModelSDI.h"

#include <stdio.h>
#include <string.h>
#include <dll_settings.h>

using namespace std;

extern "C" 
{

CDECL void cpp_option_count_(char *ENTITY_TYPE, int *S_ENTITY_TYPE, int *HM_OPTION_NUMBER)
{
// Char fortran -> c++
    char *cname;
    int cname_len;
	unsigned int UHM_OPTION_NUMBER;
    int i;
    cname_len = *S_ENTITY_TYPE + 1;
    cname=(char*) malloc(sizeof(char)*cname_len);
    for(i=0;i<*S_ENTITY_TYPE;i++)  cname[i] = ENTITY_TYPE[i];
    cname[*S_ENTITY_TYPE]='\0';
//

    if (strncmp(cname,"MATERIAL",8) == 0 ) GlobalModelSDISelectionStart("/MAT");
    if (strncmp(cname,"PROPERTY",8) == 0 ) GlobalModelSDISelectionStart("/PROP");
    if (strncmp(cname,"PART",4) == 0 ) GlobalModelSDISelectionStart("/PART");
    if (strncmp(cname,"TRANSFORM",9) == 0 ) GlobalModelSDISelectionStart("/TRANSFORM");
    if (strncmp(cname,"SUBSET",6) == 0 ) GlobalModelSDISelectionStart("/SUBSET");
    if (strncmp(cname,"FAILURE",7) == 0 ) GlobalModelSDISelectionStart("/FAIL");

    if (strncmp(cname,"MATERIAL",8) != 0 && strncmp(cname,"PROPERTY",8) != 0  && 
        strncmp(cname,"PART",4) != 0     && strncmp(cname,"TRANSFORM",9) != 0 && 
        strncmp(cname,"SUBSET",6) != 0   && strncmp(cname,"FAILURE",7) != 0    ) GlobalModelSDISelectionStart(cname);

    GlobalModelSDISelectionCount(&UHM_OPTION_NUMBER);

//
/*
    if (strncmp(cname,"MATERIAL",8) == 0 ) GlobalModelSelectionStart(ENTITY_TYPE_MATERIAL);
    if (strncmp(cname,"PROPERTY",8) == 0 ) GlobalModelSelectionStart(ENTITY_TYPE_PROPERTY);
    if (strncmp(cname,"PART",4) == 0 ) GlobalModelSelectionStart(ENTITY_TYPE_COMPONENT);
    if (strncmp(cname,"TRANSFORM",9) == 0 ) GlobalModelSelectionStart(ENTITY_TYPE_TRANSFORMATION);
    if (strncmp(cname,"SUBSET",6) == 0 ) GlobalModelSelectionStart(ENTITY_TYPE_ASSEMBLY);

    //if (strncmp(cname,"NEW",3) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_GENERAL);
    if (strncmp(cname,"LINE",4) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_LINE);
    if (strncmp(cname,"SURF",4) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_SURFACE);
    if (strncmp(cname,"LOAD",4) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_LOAD);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_SYSTEM);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_LOADCOL);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_SYSTEMCOL);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_SET);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_PROPERTY);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_GROUP);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_PLOT);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_CURVE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_BLOCK);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_MATERIAL);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_ASSEMBLY);
    if (strncmp(cname,"TITLE",5) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_TITLE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_VECTORCOL);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_VECTOR);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_EQUATION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_CONTROLCARD);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_OUTPUTBLOCK);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_LOADSTEP);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_POINT);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_FACE);
    if (strncmp(cname,"SENSOR",6) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_SENSOR);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_DESIGNVARIABLE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_BEAMSECTCOL);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_BEAMSECT);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_TABLE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_OPTIFUNCTION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_OPTIRESPONSE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_DVPREL);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_OPTICONSTRAINT);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_DVLINK);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_OBJECTIVE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_CONTROLVOLUME);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_MULTIBODY);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_ELLIPSOID);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_OPTICONTROL);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_OPTIDSCREEN);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_TAG);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_MBJOINT);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_MBPLANE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_OBJECTIVEREFERENCE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_CONTACTSURF);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_CONNECTOR);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_SHAPE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_HANDLE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_DOMAIN);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_SYMMETRY);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_SOLID);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_MORPHCONSTRAINT);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_MORPHVOLUME);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_DVDISCRETE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_BAG);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_PLY);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_LAMINATE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_MODULE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_REGION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_PARAMETER);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_FEATURE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_MASS);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_MESHCONTROL);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_CONTROLLER);
    if (strncmp(cname,"INTER",5) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_CONTACT);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_JOINT);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_LOADING);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_CONSTRAINT);
    if (strncmp(cname,"RWALL",5) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_RIGIDWALL);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_PRIMITIVE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_ENCRYPTION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_ITEM);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_CONFIGURATION);
    if (strncmp(cname,"FAILURE",7) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_FAILURE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_STATEEQUATION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_CROSSSECTION);
    if (strncmp(cname,"DAMP",4) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_DAMPING);
//    if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_HOURGLASS);
    if (strncmp(cname,"GAUGE",5) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_GAUGE);
//    if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_FIELD);
    if (strncmp(cname,"RBODY",5) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_RIGIDBODY);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_ADDEDMASS);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_SLIPRING);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_RETRACTOR);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_PRETENSIONER);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_FORMINGTOOL);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_FORMINGBLANK);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_ANALYSIS);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_BEAM);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_BUSHING);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_COUPLER);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_SPRINGDAMPER);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_MOTION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_JOB);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_SYMBOL);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_SYMBOLICLINK);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_BODY);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_DUMMY);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_MECHANISM);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_POSITION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_MARKER);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_SEATBELT);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_PANEL);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_ENGPROPERTY);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_COLLECTIONENTITY);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_CONSTRAINEDEXTRANODES);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_MODULEPROTOTYPE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_MODULEOCCURRENCE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_CONNECTORPROTOTYPE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_CONNECTOROCCURRENCE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_REPRESENTATION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_COLLISION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_FREQUENCYSET);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_OUTPUTREQUEST);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_TIREPATCHPOINT);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_SOLVEROPTION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_CONSTRAINEDRIGIDBODY);
    if (strncmp(cname,"ACCEL",5) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_ACCELEROMETER);
    if (strncmp(cname,"BCS",3) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_BOUNDARYCONDITION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_CONTACTBEHAVIOR);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_LAMINATEBEHAVIOR);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_LOADINGBCSET);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_DIRECTMATRIXINPUT);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_ELEMENTCLUSTER);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_MATERIALGROUP);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_PROPERTYGROUP);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_LOADCOLLECTORSET);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_RESULT);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_CONTACTGROUP);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_ALEFSIPROJECTION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_ALEREFERENCESYSTEMCURVE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_ALEREFERENCESYSTEMGROUP);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_ALEREFERENCESYSTEMNODE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_ALEREFERENCESYSTEMSWITCH);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_ALESMOOTHING);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_ALETANKTEST);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_INTERFACECOMPONENT);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_INTERFACELINKING);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_INCLUDE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_INCLUDESET);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_INCLUDECONFIGURATION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_PARTSET);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_CONSTRAINEDLAGRANGE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_SEATBELTCONTROLPOINT);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_SOLVERSUBMODEL);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_MODELCHECKCHECK);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_MODELCHECKCORRECTION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_MODELCHECKRESULT);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_DISTRIBUTION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_DESIGNPOINT);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_DESIGNPOINTSET);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_DESIGNPOINTMETHOD);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_STRUCTURALPROPERTY);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_FREEBODYSECTION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_ATTACHMENT);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_INITIALGEOMETRIES);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_IMPACTPOINT);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_CONNECTORCONTROL);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_CONNECTORCONTROLSCHEME);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_CONNECTORCONTROLDEFAULT);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_EQUIVALENCE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_PLYGROUP);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_CONNECTORSET);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_CONNECTORGROUP);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_WELDLINE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_LOADSET);
    if (strncmp(cname,"AIRBAG",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_AIRBAG);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_AIRBAGFOLD);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_AIRBAGFOLDINGSEQUENCE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_AIRBAGCHAMBER);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_AIRBAGINJECTOR);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_AIRBAGSTRAP);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_AIRBAGVENTHOLE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_AIRBAGGASSPECIE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_AIRBAGINTERACTION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_AIRBAGINITIALCONDITION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_HIERARCHY);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_INSTANCE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_PROTOTYPE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_REPRESENTATIONDEFINITION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_REPRESENTATIONREALIZATION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_SUBSYSTEM);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_SUBSYSTEMCONFIGURATION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_SUBSYSTEMSET);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_MEASURE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_SMARTRULE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_SOLVERMASS);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_VEHICLE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_BARRIER);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_FESOLVERJOINT);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_NOTE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_REPORT);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_STUDY);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_RESPONSE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_MEMBER);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_MEMBERJOINT);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_MEMBERSECTION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_MEMBERSEGMENT);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_GEOMETRYCOLLECTION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_TERMINATION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_SOLUTIONPARAMETER);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_LOADCASE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_FREEBODYGROUP);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_EXPLORATION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_SYMMETRYPIVOT);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_SHAPE3D);
    if (strncmp(cname,"FRICTION",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_FRICTION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_ATTACHMENTCONTROL);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_ATTACHMENTCONTROLDEFAULT);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_MPSOLUTION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_SOLUTION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_MPSOLVER);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_GENERICLOAD);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_GENERICPARAMETER);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_FLUIDVOLUME);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_OPERATORMETHOD);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_OPERATORCONSTRAINT);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_GENERICMATERIAL);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_PHYSICALPART);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_GENERICOUTPUT);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_GEOMETRICREPRESENTATION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_SOLVER);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_DESIGNCONTROL);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_MEMBERPANEL);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_PHYSICALPROP);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_FUNCTIONALREPRESENTATION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_SOLVERREPRESENTATION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_REPRESENTATIONFACTORY);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_GENERICSHAPE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_GENERICRESULT);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_GENERICSELECTIONSET);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_RESPONSEREQUEST);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_INTERNALLIST);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_ACTION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_COMMENT);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_LIST);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_PLYSHAPE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_SEQUENCE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_POLYCAGE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_SKETCH);
    if (strncmp(cname,"UNIT",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_UNIT);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_THICKNESS);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_OFFSET);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_ORIENTATION);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_NONSTRUCTURALMASS);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_INITIALSTRESS);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_INITIALSTRAIN);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_MAX);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_INCLUDEFILE);
    //if (strncmp(cname,"ADEFINIR",8) == 0 )GlobalModelSelectionStart(ENTITY_TYPE_ALLOC_MAX)
    GlobalModelSelectionCount(&UHM_OPTION_NUMBER);
*/
	*HM_OPTION_NUMBER=UHM_OPTION_NUMBER;
        free(cname);
//    printf("nb option : %d \n",*HM_OPTION_NUMBER);
}

CDECL void CPP_OPTION_COUNT(char *ENTITY_TYPE, int *S_ENTITY_TYPE, int *HM_OPTION_NUMBER)
{cpp_option_count_ (ENTITY_TYPE,S_ENTITY_TYPE, HM_OPTION_NUMBER);}

CDECL void cpp_option_count__(char *ENTITY_TYPE, int *S_ENTITY_TYPE, int *HM_OPTION_NUMBER)
{cpp_option_count_ (ENTITY_TYPE,S_ENTITY_TYPE, HM_OPTION_NUMBER);}

CDECL void cpp_option_count(char *ENTITY_TYPE, int *S_ENTITY_TYPE, int *HM_OPTION_NUMBER)
{cpp_option_count_ (ENTITY_TYPE,S_ENTITY_TYPE, HM_OPTION_NUMBER);}


}
