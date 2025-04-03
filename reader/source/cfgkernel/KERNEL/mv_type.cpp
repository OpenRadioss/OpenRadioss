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

#include <UTILS/win32_utils.h>



#include <UTILS/mv_string.h>
#include <UTILS/mv_stl_various.h>
#include <KERNEL_BASE/utils.h>







#include "mv_type.h"

typedef map<string,object_type_e> MvStrTypeMap_t;


class MvTypeMapBase_t {
public:
  MvTypeMapBase_t() : myNbTypes(0), myTypeArray(NULL) {}
  ~MvTypeMapBase_t();
public:
  object_type_e  getType(const string &keyword) const;
  const string  &getType(object_type_e type)    const;
protected:
  void InitTypeArray();
protected:
  MvStrTypeMap_t  myTypeMap;
  int             myNbTypes;
  string         *myTypeArray;
};

class MvTypeMap_t : public MvTypeMapBase_t {
public:
  MvTypeMap_t();
  virtual ~MvTypeMap_t() {}
private:
  void InitTypeMap();
};

MvTypeMap_t::MvTypeMap_t() : MvTypeMapBase_t() {
  InitTypeMap();
  InitTypeArray();
}

MvTypeMapBase_t::~MvTypeMapBase_t() {
  if(myTypeArray!=NULL) delete [] myTypeArray;
}

void MvTypeMap_t::InitTypeMap() {
  myTypeMap["UNKNOWN"]                           = HCDI_OBJ_TYPE_NULL;
  myTypeMap["NODE"]                              = HCDI_OBJ_TYPE_NODES;
  myTypeMap["ELEMENT"]                           = HCDI_OBJ_TYPE_ELEMS;
  myTypeMap["ELEMS"]                             = HCDI_OBJ_TYPE_ELEMS;
  myTypeMap["COMPONENT"]                         = HCDI_OBJ_TYPE_COMPS;
  myTypeMap["LINE"]                              = HCDI_OBJ_TYPE_LINES;
  myTypeMap["SURFACE"]                           = HCDI_OBJ_TYPE_SURFS;
  myTypeMap["LOAD"]                              = HCDI_OBJ_TYPE_LOADS;
  myTypeMap["SYSTEM"]                            = HCDI_OBJ_TYPE_SYSTS;               
  myTypeMap["LOADCOL"]                           = HCDI_OBJ_TYPE_LOADCOLS;
  myTypeMap["SYSTCOL"]                           = HCDI_OBJ_TYPE_SYSTCOLS;
  myTypeMap["SETS"]                              = HCDI_OBJ_TYPE_SETS;
  myTypeMap["PROPERTY"]                          = HCDI_OBJ_TYPE_PROPS;
  myTypeMap["PROP"]                              = HCDI_OBJ_TYPE_PROPS;
  
  myTypeMap["GROUP"]                             = HCDI_OBJ_TYPE_GROUPS;
  myTypeMap["PLOT"]                              = HCDI_OBJ_TYPE_PLOTS;
  myTypeMap["CURVE"]                             = HCDI_OBJ_TYPE_CURVES;
  myTypeMap["FUNCT"]                             = HCDI_OBJ_TYPE_CURVES;
  
  myTypeMap["BLOCK"]                             = HCDI_OBJ_TYPE_BLOCKS;
  myTypeMap["MATERIAL"]                          = HCDI_OBJ_TYPE_MATS;      
  myTypeMap["MAT"]                               = HCDI_OBJ_TYPE_MATS;
  myTypeMap["ASSEMBLY"]                          = HCDI_OBJ_TYPE_ASSEMS;            
  myTypeMap["TITLE"]                             = HCDI_OBJ_TYPE_TITLES;            
  myTypeMap["VECTORCOL"]                         = HCDI_OBJ_TYPE_VECTORCOLS;            
  myTypeMap["VECTOR"]                            = HCDI_OBJ_TYPE_VECTORS;
  myTypeMap["EQUATION"]                          = HCDI_OBJ_TYPE_EQUATIONS;

  myTypeMap["CARD"]                              = HCDI_OBJ_TYPE_CARDS;
  myTypeMap["OUTPUTBLOCK"]                       = HCDI_OBJ_TYPE_OUTPUTBLOCKS;       
  myTypeMap["LOADSTEP"]                          = HCDI_OBJ_TYPE_LOADSTEPS;

  myTypeMap["POINT"]                             = HCDI_OBJ_TYPE_POINTS;
  myTypeMap["FACE"]                              = HCDI_OBJ_TYPE_SPLINES;
  myTypeMap["POINT"]                             = HCDI_OBJ_TYPE_TOPOPOINTS;
  myTypeMap["LINE"]                              = HCDI_OBJ_TYPE_TOPOLINES;
  myTypeMap["SENSOR"]                            = HCDI_OBJ_TYPE_SENSORS;
  myTypeMap["DESIGNVAR"]                         = HCDI_OBJ_TYPE_DESIGNVARS;
  myTypeMap["BEAMSECTCOL"]                       = HCDI_OBJ_TYPE_BEAMSECTCOLS;
  myTypeMap["BEAMSECTION"]                       = HCDI_OBJ_TYPE_BEAMSECTS;
  
  
  myTypeMap["TABLE"]                             = HCDI_OBJ_TYPE_OPTITABLEENTRS;

  myTypeMap["DEQUATION"]                         = HCDI_OBJ_TYPE_OPTIFUNCTIONS;


  myTypeMap["OPTIRESPONSE"]                      = HCDI_OBJ_TYPE_OPTIRESPONSES;
  myTypeMap["DVPREL"]                            = HCDI_OBJ_TYPE_DVPRELS;
  myTypeMap["OPTICONSTRAINT"]                    = HCDI_OBJ_TYPE_OPTICONSTRAINTS;
  myTypeMap["DESVARLINK"]                        = HCDI_OBJ_TYPE_DESVARLINKS;
  myTypeMap["OBJECTIVE"]                         = HCDI_OBJ_TYPE_OBJECTIVES;

  
  myTypeMap["CONTROLVOL"]                        = HCDI_OBJ_TYPE_CONTROLVOLS;
  myTypeMap["OPTICONTROL"]                       = HCDI_OBJ_TYPE_OPTICONTROLS;
  myTypeMap["OPTIDSCREEN"]                       = HCDI_OBJ_TYPE_OPTIDSCREENS;
  myTypeMap["TAG"]                               = HCDI_OBJ_TYPE_TAG;
  myTypeMap["DOBJREF"]                           = HCDI_OBJ_TYPE_DOBJREFS;
  myTypeMap["CONTACTSURF"]                       = HCDI_OBJ_TYPE_CONTACTSURFS;
  myTypeMap["CONNECTOR"]                         = HCDI_OBJ_TYPE_CONNECTORS;   
  myTypeMap["SHAPE"]                             = HCDI_OBJ_TYPE_SHAPES;    
  myTypeMap["HANDLE"]                            = HCDI_OBJ_TYPE_HANDLES; 
  myTypeMap["DOMAIN"]                            = HCDI_OBJ_TYPE_DOMAINS;
  myTypeMap["SYMMETRY"]                          = HCDI_OBJ_TYPE_SYMMETRYS;            
  myTypeMap["SOLID"]                             = HCDI_OBJ_TYPE_SOLIDS;
  myTypeMap["MORPHCONSTRAINT"]                   = HCDI_OBJ_TYPE_MORPHCONSTRAINTS;
  myTypeMap["MORPHVOLUME"]                       = HCDI_OBJ_TYPE_HYPERCUBES;
  myTypeMap["DDVAL"]                             = HCDI_OBJ_TYPE_DDVALS;
  myTypeMap["BAG"]                               = HCDI_OBJ_TYPE_BAGS;
  myTypeMap["PLY"]                               = HCDI_OBJ_TYPE_PLYS;
  myTypeMap["LAMINATE"]                          = HCDI_OBJ_TYPE_LAMINATES;

  

  myTypeMap["MODULE"]                            = HCDI_OBJ_TYPE_MODULES;
  myTypeMap["REGION"]                            = HCDI_OBJ_TYPE_REGIONS;
  myTypeMap["PARAMETER"]                         = HCDI_OBJ_TYPE_PARAMETERS;
  myTypeMap["FEATURE"]                           = HCDI_OBJ_TYPE_HM_FEATURES;
  myTypeMap["MASS"]                              = HCDI_OBJ_TYPE_MASSES;
  myTypeMap["MESHCONTROL"]                       = HCDI_OBJ_TYPE_MESHCONTROLS;             
  myTypeMap["CONTROLLER"]                        = HCDI_OBJ_TYPE_CONTROLLERS;           
  myTypeMap["CONTACT"]                           = HCDI_OBJ_TYPE_CONTACTS;        
  myTypeMap["JOINT"]                             = HCDI_OBJ_TYPE_JOINTS;
  myTypeMap["LOADING"]                           = HCDI_OBJ_TYPE_LOADINGS;
  myTypeMap["MECHANISMCONSTRAINT"]               = HCDI_OBJ_TYPE_MECHANISMCONSTRAINTS;
  myTypeMap["RIGIDWALL"]                         = HCDI_OBJ_TYPE_RIGIDWALLS;      
  myTypeMap["BOX"]                               = HCDI_OBJ_TYPE_PRIMITIVES;      
  myTypeMap["ENCRYPTION"]                        = HCDI_OBJ_TYPE_ENCRYPTIONS;     
  myTypeMap["ITEM"]                              = HCDI_OBJ_TYPE_ITEMS;           
  myTypeMap["CONFIGURATION"]                     = HCDI_OBJ_TYPE_CONFIGURATIONS;  
  myTypeMap["FAILURE"]                           = HCDI_OBJ_TYPE_FAILURES;    
  myTypeMap["FAIL"]                              = HCDI_OBJ_TYPE_FAILURES;
  myTypeMap["STATEEQUATION"]                     = HCDI_OBJ_TYPE_STATEEQUATIONS;  
  myTypeMap["CROSSSECTION"]                      = HCDI_OBJ_TYPE_CROSSSECTIONS;   
  myTypeMap["DAMPING"]                           = HCDI_OBJ_TYPE_DAMPINGS;        
  myTypeMap["HOURGLASS"]                         = HCDI_OBJ_TYPE_HOURGLASS;       
  myTypeMap["GAUGE"]                             = HCDI_OBJ_TYPE_GAUGES;          

  myTypeMap["FIELD"]                             = HCDI_OBJ_TYPE_FIELDS;          
  myTypeMap["RIGIDBODY"]                         = HCDI_OBJ_TYPE_RIGIDBODIES;     
  myTypeMap["ADDEDMASS"]                         = HCDI_OBJ_TYPE_ADDEDMASS;       
  myTypeMap["SLIPRING"]                          = HCDI_OBJ_TYPE_SLIPRINGS;       
  myTypeMap["RETRACTOR"]                         = HCDI_OBJ_TYPE_RETRACTORS;      
  myTypeMap["PRETENSIONER"]                      = HCDI_OBJ_TYPE_PRETENSIONERS;
  myTypeMap["FORMINGTOOL"]                       = HCDI_OBJ_TYPE_FORMINGTOOLS;
  myTypeMap["FORMINGBLANK"]                      = HCDI_OBJ_TYPE_FORMINGBLANKS;
  myTypeMap["ANALYSIS"]                          = HCDI_OBJ_TYPE_ANALYSIS;
  myTypeMap["BEAM"]                              = HCDI_OBJ_TYPE_BEAMS;           
  myTypeMap["BUSHING"]                           = HCDI_OBJ_TYPE_BUSHINGS;        
  myTypeMap["COUPLER"]                           = HCDI_OBJ_TYPE_COUPLERS;        
  myTypeMap["SPRINGDAMPER"]                      = HCDI_OBJ_TYPE_SPRINGDAMPERS;   
  myTypeMap["MOTION"]                            = HCDI_OBJ_TYPE_MOTIONS;         
  myTypeMap["JOB"]                               = HCDI_OBJ_TYPE_JOBS;            
  myTypeMap["SYMBOL"]                            = HCDI_OBJ_TYPE_SYMBOLS;         
  myTypeMap["SYMBOLICLINK"]                      = HCDI_OBJ_TYPE_SYMBOLICLINKS;   
  myTypeMap["BODY"]                              = HCDI_OBJ_TYPE_BODIES;          
  myTypeMap["DUMMY"]                             = HCDI_OBJ_TYPE_DUMMIES;         

  myTypeMap["MECHANISM"]                         = HCDI_OBJ_TYPE_MECHANISMS;      
  myTypeMap["TRANSFORMATION"]                    = HCDI_OBJ_TYPE_TRANSFORMATIONS; 

  myTypeMap["POSITION"]                          = HCDI_OBJ_TYPE_POSITIONS;       
  myTypeMap["MARKER"]                            = HCDI_OBJ_TYPE_MARKERS;         
  myTypeMap["SEATBELT"]                          = HCDI_OBJ_TYPE_SEATBELTS;       
  myTypeMap["PANEL"]                             = HCDI_OBJ_TYPE_PANELS;          
  myTypeMap["ENGPROPERTY"]                       = HCDI_OBJ_TYPE_ENGPROPERTIES;        
  myTypeMap["COLLECTION"]                        = HCDI_OBJ_TYPE_COLLECTIONS;     
  
  myTypeMap["CONSTRAINEDEXTRANODE"]              = HCDI_OBJ_TYPE_CONSTRAINEDEXTRANODES;    
  myTypeMap["MODULEPROTOTYPE"]                   = HCDI_OBJ_TYPE_MODULEPROTOTYPES;         
  myTypeMap["MODULEOCCURRENCE"]                  = HCDI_OBJ_TYPE_MODULEOCCURRENCES;        
  myTypeMap["RESULTSUBCASE"]                = HCDI_OBJ_TYPE_RESULTSUBCASES;
  myTypeMap["RESULTSIMULATION"]               = HCDI_OBJ_TYPE_RESULTSIMULATIONS;     
  myTypeMap["REPRESENTATION"]                    = HCDI_OBJ_TYPE_REPRESENTATIONS;          
  myTypeMap["COLLISION"]                         = HCDI_OBJ_TYPE_COLLISIONS;               
  myTypeMap["FREQUENCYSET"]                      = HCDI_OBJ_TYPE_FREQUENCYSETS;           
  myTypeMap["OUTPUTREQUEST"]                     = HCDI_OBJ_TYPE_OUTPUTREQUESTS;          
  myTypeMap["TIREPATCHPOINT"]                    = HCDI_OBJ_TYPE_TIREPATCHPOINTS;         
  myTypeMap["SOLVEROPTION"]                      = HCDI_OBJ_TYPE_SOLVEROPTIONS;           
  myTypeMap["CONSTRAINEDRIGIDBODY"]              = HCDI_OBJ_TYPE_CONSTRAINEDRIGIDBODIES;   
  myTypeMap["ACCELEROMETER"]                     = HCDI_OBJ_TYPE_ACCELEROMETERS;          
  myTypeMap["BOUNDARYCONDITION"]                 = HCDI_OBJ_TYPE_BOUNDARYCONDITIONS;
  myTypeMap["CONTACTBEHAVIOR"]                   = HCDI_OBJ_TYPE_CONTACTBEHAVIORS;
  myTypeMap["LAMINATEBEHAVIOR"]                  = HCDI_OBJ_TYPE_LAMINATEBEHAVIORS;
  myTypeMap["LOADINGBCSET"]                      = HCDI_OBJ_TYPE_LOADINGBCSETS;
  myTypeMap["DIRECTMATRIXINPUT"]                 = HCDI_OBJ_TYPE_DIRECTMATRIXINPUTS;

  myTypeMap["ELEMENTCLUSTER"]                    = HCDI_OBJ_TYPE_ELEMENTCLUSTERS;      

  myTypeMap["MATERIALGROUP"]                     = HCDI_OBJ_TYPE_MATERIALGROUPS;
  myTypeMap["PROPERTYGROUP"]                     = HCDI_OBJ_TYPE_PROPERTYGROUPS;       
  myTypeMap["LOADCOLLECTORSET"]                  = HCDI_OBJ_TYPE_LOADCOLLECTORSETS;
  myTypeMap["RESULT"]                            = HCDI_OBJ_TYPE_RESULTS;
  myTypeMap["CONTACTGROUP" ]                     = HCDI_OBJ_TYPE_CONTACTGROUPS;
  
  myTypeMap["ALEFSIPROJECTION" ]                 = HCDI_OBJ_TYPE_ALEFSIPROJECTIONS;  
  myTypeMap["_ALEREFERENCESYSTEMCURVE" ]         = HCDI_OBJ_TYPE_ALEREFERENCESYSTEMCURVES;  
  myTypeMap["ALEREFERENCESYSTEMGROUP"]           = HCDI_OBJ_TYPE_ALEREFERENCESYSTEMGROUPS;
  myTypeMap["ALEREFERENCESYSTEMNODE" ]           = HCDI_OBJ_TYPE_ALEREFERENCESYSTEMNODES;       
  myTypeMap["ALEREFERENCESYSTEMSWITCH" ]         = HCDI_OBJ_TYPE_ALEREFERENCESYSTEMSWITCHES;       
  myTypeMap["ALESMOOTHING" ]                     = HCDI_OBJ_TYPE_ALESMOOTHINGS;             
  myTypeMap["ALETANKTEST"]                       = HCDI_OBJ_TYPE_ALETANKTESTS;       
  myTypeMap["INTERFACECOMPONENT"]                = HCDI_OBJ_TYPE_INTERFACECOMPONENTS;         
  myTypeMap["INTERFACELINKING"]                  = HCDI_OBJ_TYPE_INTERFACELINKINGS; 
  myTypeMap["INCLUDE"]                           = HCDI_OBJ_TYPE_INCLUDES;      
  myTypeMap["LEGEND"]                            = HCDI_OBJ_TYPE_LEGENDS;        
  
  myTypeMap["CHARTS"]							 = HCDI_OBJ_TYPE_CHARTS;    
  myTypeMap["PARTSET"]                           = HCDI_OBJ_TYPE_PARTSETS;            
  myTypeMap["CONSTRAINEDLAGRANGE"]               = HCDI_OBJ_TYPE_CONSTRAINEDLAGRANGES;          
  myTypeMap["SEATBELTCONTROLPOINT"]              = HCDI_OBJ_TYPE_SEATBELTCONTROLPOINTS;    
  myTypeMap["SOLVERSUBMODEL"]                    = HCDI_OBJ_TYPE_SOLVERSUBMODELS;         

  myTypeMap["MODELCHECKCHECK"]                   = HCDI_OBJ_TYPE_MODELCHECKCHECKS;       
  myTypeMap["MODELCHECKCORRECTION"]              = HCDI_OBJ_TYPE_MODELCHECKCORRECTIONS;  
  myTypeMap["MODELCHECKRESULT"]                  = HCDI_OBJ_TYPE_MODELCHECKRESULTS;      
  myTypeMap["DISTRIBUTION"]                      = HCDI_OBJ_TYPE_DISTRIBUTIONS;          
  myTypeMap["DESIGNPOINT"]                       = HCDI_OBJ_TYPE_DESIGNPOINTS;           
  myTypeMap["DESIGNPOINTSET"]                    = HCDI_OBJ_TYPE_DESIGNPOINTSETS;        
  myTypeMap["DESIGNPOINTMETHOD"]                 = HCDI_OBJ_TYPE_DESIGNPOINTMETHODS;     
  myTypeMap["STRUCTURALPROPERTY"]                = HCDI_OBJ_TYPE_STRUCTURALPROPERTIES;
  myTypeMap["FREEBODYSECTION"]                   = HCDI_OBJ_TYPE_FREEBODYSECTIONS;       
  myTypeMap["ATTACHMENT"]                        = HCDI_OBJ_TYPE_ATTACHMENTS;            
  myTypeMap["INITIALGEOMETRY"]                   = HCDI_OBJ_TYPE_INITIALGEOMETRIES;      
  myTypeMap["INTEGRATIONRULE"]                   = HCDI_OBJ_TYPE_INTEGRATIONRULES;           
  myTypeMap["INTEGRATIONRULES"]                  = HCDI_OBJ_TYPE_INTEGRATIONRULES;
  
  myTypeMap["CONNECTORCONTROL"]                  = HCDI_OBJ_TYPE_CONNECTORCONTROLS;
  myTypeMap["CONNECTORCONTROLSCHEME"]            = HCDI_OBJ_TYPE_CONNECTORCONTROLSCHEMES;
  myTypeMap["CONNECTORCONTROLDEFAULT"]           = HCDI_OBJ_TYPE_CONNECTORCONTROLDEFAULTS;
  myTypeMap["EQUIVALENCE"]                       = HCDI_OBJ_TYPE_EQUIVALENCES;
  myTypeMap["PLYGROUP"]                          = HCDI_OBJ_TYPE_PLYGROUPS;
  
  myTypeMap["CONNECTORSET"]                      = HCDI_OBJ_TYPE_CONNECTORSETS;        
  myTypeMap["CONNECTORGROUP"]                    = HCDI_OBJ_TYPE_CONNECTORGROUPS;        
  myTypeMap["WELDLINE"]                          = HCDI_OBJ_TYPE_WELDLINES;
  myTypeMap["LOADSET"]                           = HCDI_OBJ_TYPE_LOADSETS;
  myTypeMap["AIRBAG"]                            = HCDI_OBJ_TYPE_AIRBAGS;
  myTypeMap["AIRBAGFOLD"]                        = HCDI_OBJ_TYPE_AIRBAGFOLDS; 
  myTypeMap["AIRBAGFOLDINGSEQUENCE"]             = HCDI_OBJ_TYPE_AIRBAGFOLDINGSEQUENCES;
  myTypeMap["AIRBAGCHAMBER"]                     = HCDI_OBJ_TYPE_AIRBAGCHAMBERS;

  myTypeMap["AIRBAGINJECTOR"]                    = HCDI_OBJ_TYPE_AIRBAGINJECTORS; 
  myTypeMap["AIRBAGSTRAP"]                       = HCDI_OBJ_TYPE_AIRBAGSTRAPS;
  myTypeMap["AIRBAGVENTHOLE"]                    = HCDI_OBJ_TYPE_AIRBAGVENTHOLES; 

  myTypeMap["AIRBAGGASSPECIE"]                   = HCDI_OBJ_TYPE_AIRBAGGASSPECIES;   
  myTypeMap["AIRBAGINTERACTION"]                 = HCDI_OBJ_TYPE_AIRBAGINTERACTIONS;   

  myTypeMap["AIRBAGINITIALCONDITION"]            = HCDI_OBJ_TYPE_AIRBAGINITIALCONDITIONS;
  myTypeMap["HIERARCHY"]                         = HCDI_OBJ_TYPE_HIERARCHIES;
  myTypeMap["INSTANCE"]                          = HCDI_OBJ_TYPE_INSTANCES;
  myTypeMap["PROTOTYPE"]                         = HCDI_OBJ_TYPE_PROTOTYPES;
  myTypeMap["REPRESENTATIONDEFINITION"]          = HCDI_OBJ_TYPE_REPRESENTATIONDEFINITIONS;
  myTypeMap["REPRESENTATIONREALIZATION"]         = HCDI_OBJ_TYPE_REPRESENTATIONREALIZATIONS;
  myTypeMap["SUBSYSTEM"]                         = HCDI_OBJ_TYPE_SUBSYSTEMS; 
  myTypeMap["SUBSYSTEMCONFIGURATION"]            = HCDI_OBJ_TYPE_SUBSYSTEMCONFIGURATIONS;
  myTypeMap["SUBSYSTEMSET"]                      = HCDI_OBJ_TYPE_SUBSYSTEMSETS;  
  myTypeMap["MEASURE"]                           = HCDI_OBJ_TYPE_MEASURES;   
  myTypeMap["SMARTRULE"]                         = HCDI_OBJ_TYPE_SMARTRULES;  
  myTypeMap["SOLVERMASS"]                        = HCDI_OBJ_TYPE_SOLVERMASSES;   
  myTypeMap["VEHICLE"]                           = HCDI_OBJ_TYPE_VEHICLES;   

  myTypeMap["SOUNDPACKAGEPART"]                           = HCDI_OBJ_TYPE_SOUNDPACKAGEPARTS;
  myTypeMap["RESOURCE"]                     = HCDI_OBJ_TYPE_RESOURCES;
  myTypeMap["NOTE"]                              = HCDI_OBJ_TYPE_NOTES;

  myTypeMap["REPORT"]                            = HCDI_OBJ_TYPE_REPORTS;       
  myTypeMap["STUDY"]                             = HCDI_OBJ_TYPE_STUDIES;
  myTypeMap["RESPONSE"]                          = HCDI_OBJ_TYPE_RESPONSES;

  myTypeMap["MEMBER"]                            = HCDI_OBJ_TYPE_MEMBERS;   
   
  myTypeMap["MEMBERJOINT"]                       = HCDI_OBJ_TYPE_MEMBERJOINTS;
  myTypeMap["MEMBERSECTION"]                     = HCDI_OBJ_TYPE_MEMBERSECTIONS;
  myTypeMap["MEMBERSEGMENT"]                     = HCDI_OBJ_TYPE_MEMBERSEGMENTS;
  myTypeMap["GEOMETRYCOLLECTION"]                = HCDI_OBJ_TYPE_GEOMETRYCOLLECTIONS;
  myTypeMap["TERMINATION"]                       = HCDI_OBJ_TYPE_TERMINATIONS;
  myTypeMap["SOLUTIONPARAMETER"]                 = HCDI_OBJ_TYPE_SOLUTIONPARAMETERS;
  myTypeMap["LOADCASE"]                          = HCDI_OBJ_TYPE_LOADCASES;
  myTypeMap["FREEBODYGROUP"]                     = HCDI_OBJ_TYPE_FREEBODYGROUPS;
  myTypeMap["EXPLORATION"]                       = HCDI_OBJ_TYPE_EXPLORATIONS;
  myTypeMap["SYMMETRYPIVOT"]                     = HCDI_OBJ_TYPE_SYMMETRYPIVOTS;
  myTypeMap["SHAPE3D"]                           = HCDI_OBJ_TYPE_SHAPE3DS;
  myTypeMap["FRICTION"]                          = HCDI_OBJ_TYPE_FRICTIONS;
  myTypeMap["ATTACHMENTCONTROLS"]                = HCDI_OBJ_TYPE_ATTACHMENTCONTROLS;
  myTypeMap["ATTACHMENTCONTROLDEFAULTS"]         = HCDI_OBJ_TYPE_ATTACHMENTCONTROLDEFAULTS;
  myTypeMap["PROCESS"]                           = HCDI_OBJ_TYPE_PROCESSES;
  myTypeMap["SOLUTION"]                          = HCDI_OBJ_TYPE_SOLUTIONS;
  myTypeMap["MPSOLVER"]                          = HCDI_OBJ_TYPE_MPSOLVERS;
  myTypeMap["GENERICLOAD"]                       = HCDI_OBJ_TYPE_GENERICLOADS;
  myTypeMap["GENERICPARAMETER"]                  = HCDI_OBJ_TYPE_GENERICPARAMETERS;
  myTypeMap["FLUIDVOLUME"]                       = HCDI_OBJ_TYPE_FLUIDVOLUMES;
  myTypeMap["TASK"]                              = HCDI_OBJ_TYPE_TASKS;
  myTypeMap["OPERATORCONSTRAINT"]                = HCDI_OBJ_TYPE_OPERATORCONSTRAINTS;
  myTypeMap["GENERICMATERIAL"]                   = HCDI_OBJ_TYPE_GENERICMATERIALS;
  myTypeMap["PHYSICALPART"]                      = HCDI_OBJ_TYPE_PHYSICALPARTS;
  myTypeMap["AUTOMATION"]                        = HCDI_OBJ_TYPE_AUTOMATIONS;
  myTypeMap["GEOMETRICREPRESENTATION"]           = HCDI_OBJ_TYPE_GEOMETRICREPRESENTATIONS;
  myTypeMap["SOLVER"]                            = HCDI_OBJ_TYPE_SOLVERS;
  myTypeMap["DESIGNCONTROL"]                     = HCDI_OBJ_TYPE_DESIGNCONTROLS;
  myTypeMap["MEMBERPANEL"]                       = HCDI_OBJ_TYPE_MEMBERPANELS;
  myTypeMap["PHYSICALPROP"]                      = HCDI_OBJ_TYPE_PHYSICALPROPS;
  myTypeMap["FUNCTIONALREPRESENTATION"]          = HCDI_OBJ_TYPE_FUNCTIONALREPRESENTATIONS;
  myTypeMap["SOLVERREPRESENTATION"]              = HCDI_OBJ_TYPE_SOLVERREPRESENTATIONS;
  myTypeMap["REPRESENTATIONFACTORY"]             = HCDI_OBJ_TYPE_REPRESENTATIONFACTORIES;
  myTypeMap["GENERICSHAPE"]                      = HCDI_OBJ_TYPE_GENERICSHAPES;
  myTypeMap["GENERICRESULT"]                     = HCDI_OBJ_TYPE_GENERICRESULTS;
  myTypeMap["GENERICSELECTIONSET"]               = HCDI_OBJ_TYPE_GENERICSELECTIONSETS;
  myTypeMap["RESPONSEREQUEST"]                   = HCDI_OBJ_TYPE_RESPONSEREQUESTS;
  myTypeMap["SECTION"]                           = HCDI_OBJ_TYPE_SECTIONS;
  myTypeMap["ACTION"]                            = HCDI_OBJ_TYPE_ACTIONS;
  myTypeMap["COMMENT"]                           = HCDI_OBJ_TYPE_COMMENTS;
  myTypeMap["LIST"]                              = HCDI_OBJ_TYPE_LISTS;
  myTypeMap["PLYSHAPE"]                          = HCDI_OBJ_TYPE_PLYSHAPES;
  myTypeMap["SEQUENCE"]                          = HCDI_OBJ_TYPE_SEQUENCES;
  myTypeMap["POLYCAGE"]                          = HCDI_OBJ_TYPE_POLYCAGES;
  myTypeMap["SKETCH"]                            = HCDI_OBJ_TYPE_SKETCHES;
  myTypeMap["UNIT"]                              = HCDI_OBJ_TYPE_UNITS;
  myTypeMap["THICKNESS"]                         = HCDI_OBJ_TYPE_THICKNESSES;
  myTypeMap["OFFSET"]                            = HCDI_OBJ_TYPE_OFFSETS;
  myTypeMap["ORIENTATION"]                       = HCDI_OBJ_TYPE_ORIENTATIONS;
  myTypeMap["NONSTRUCTURALMASS"]                 = HCDI_OBJ_TYPE_NONSTRUCTURALMASSES;
  myTypeMap["INITIALSTATE"]                      = HCDI_OBJ_TYPE_INITIALSTATES;
  myTypeMap["SUBOBJECT"]                     = HCDI_OBJ_TYPE_SUBOBJECTS;
  myTypeMap["PLOTCONTROLLER"]                    = HCDI_OBJ_TYPE_PLOTCONTROLLERS;
  myTypeMap["GUIMODELVIEW"]                      = HCDI_OBJ_TYPE_GUIMODELVIEWS;
  myTypeMap["DATASOURCE"]                        = HCDI_OBJ_TYPE_DATASOURCES;
  myTypeMap["EXTERNALAPPLICATION"]               = HCDI_OBJ_TYPE_EXTERNALAPPLICATIONS;
  myTypeMap["CONNECTORCONTROLLERGROUP"]          = HCDI_OBJ_TYPE_CONNECTORCONTROLLERGROUPS;
  myTypeMap["SEAMSUBSYSTEM"]                     = HCDI_OBJ_TYPE_SEAMSUBSYSTEMS;
  myTypeMap["SEAMJUNCTION"]                      = HCDI_OBJ_TYPE_SEAMJUNCTIONS;
  myTypeMap["SEAMEXCITATION"]                    = HCDI_OBJ_TYPE_SEAMEXCITATIONS;
  myTypeMap["SOLVERSETTING"]                     = HCDI_OBJ_TYPE_SOLVERSETTINGS;
  myTypeMap["PARTOCCURRENCE"]                = HCDI_OBJ_TYPE_PARTOCCURRENCES;
  myTypeMap["ANALYSISPARAMETER"]                 = HCDI_OBJ_TYPE_ANALYSISPARAMETERS;
  myTypeMap["FREQUENCY"]                         = HCDI_OBJ_TYPE_FREQUENCIES;
  myTypeMap["FREQUENCIES"]                       = HCDI_OBJ_TYPE_FREQUENCIES;
  myTypeMap["PERTURBATION"]                      = HCDI_OBJ_TYPE_PERTURBATIONS;
  myTypeMap["WORKPLANE"]                         = HCDI_OBJ_TYPE_WORKPLANES;
  myTypeMap["SKELETON"]                          = HCDI_OBJ_TYPE_SKELETONS;
  myTypeMap["TIMESTEPCONTROL"]                   = HCDI_OBJ_TYPE_TIMESTEPCONTROLS;
  myTypeMap["CLEARANCE"]                         = HCDI_OBJ_TYPE_CLEARANCES;
  myTypeMap["PHYSICALQUANTITY"]                  = HCDI_OBJ_TYPE_PHYSICALQUANTITIES;
  myTypeMap["BEHAVIOR"]                          = HCDI_OBJ_TYPE_BEHAVIORS;
  myTypeMap["MERGE"]                             = HCDI_OBJ_TYPE_MERGES;
  myTypeMap["SEASUBSYSTEM"]                      = HCDI_OBJ_TYPE_SEASUBSYSTEMS;
  myTypeMap["CONSTRAINT"]                        = HCDI_OBJ_TYPE_CONSTRAINTS;
  myTypeMap["AXISYMMETRY"]                       = HCDI_OBJ_TYPE_AXISYMMETRIES;
  myTypeMap["ELEMENTBEHAVIOR"]                             = HCDI_OBJ_TYPE_ELEMENTBEHAVIORS;
  myTypeMap["ENGINEFILE"]                        = HCDI_OBJ_TYPE_ENGINEFILES;
  myTypeMap["ATTACHMENTCONTROLLOCAL"]            = HCDI_OBJ_TYPE_ATTACHMENTCONTROLLOCALS;
  myTypeMap["DEVENTITY"]                         = HCDI_OBJ_TYPE_DEVENTITIES;
  myTypeMap["DEVCONTROL"]                        = HCDI_OBJ_TYPE_DEVCONTROLS;
  myTypeMap["SCRATCH"]                           = HCDI_OBJ_TYPE_SCRATCHES;
  myTypeMap["PRESSURECONTROL"]                   = HCDI_OBJ_TYPE_PRESSURECONTROLS;
  myTypeMap["ENGFORCE"]                          = HCDI_OBJ_TYPE_ENGFORCES;
  myTypeMap["FORCECONTROL"]                      = HCDI_OBJ_TYPE_FORCECONTROLS;
  myTypeMap["ENGVELOCITY"]                       = HCDI_OBJ_TYPE_ENGVELOCITIES;
  myTypeMap["VELOCITYCONTROL"]                   = HCDI_OBJ_TYPE_VELOCITYCONTROLS;
  myTypeMap["ENGACCELERATION"]                   = HCDI_OBJ_TYPE_ENGACCELERATIONS;
  myTypeMap["ACCELERATIONCONTROL"]               = HCDI_OBJ_TYPE_ACCELERATIONCONTROLS;
  myTypeMap["ENGTEMPERATURE"]                    = HCDI_OBJ_TYPE_ENGTEMPERATURES;
  myTypeMap["TEMPERATURECONTROL"]                = HCDI_OBJ_TYPE_TEMPERATURECONTROLS;
  myTypeMap["ENGCONSTRAINT"]                     = HCDI_OBJ_TYPE_ENGCONSTRAINTS;
  myTypeMap["CONSTRAINTCONTROL"]                 = HCDI_OBJ_TYPE_CONSTRAINTCONTROLS;
  myTypeMap["ENGBOUNDARYCONDITION"]              = HCDI_OBJ_TYPE_ENGBOUNDARYCONDITIONS;
  myTypeMap["BOUNDARYCONDITIONCONTROL"]          = HCDI_OBJ_TYPE_BOUNDARYCONDITIONCONTROLS;
  myTypeMap["ENGFLUX"]                           = HCDI_OBJ_TYPE_ENGFLUXES;
  myTypeMap["FLUXCONTROL"]                       = HCDI_OBJ_TYPE_FLUXCONTROLS;
  myTypeMap["ENGRADIATION"]                      = HCDI_OBJ_TYPE_ENGRADIATIONS;
  myTypeMap["RADIATIONCONTROL"]                  = HCDI_OBJ_TYPE_RADIATIONCONTROLS;
  myTypeMap["ENGCONVECTION"]                     = HCDI_OBJ_TYPE_ENGCONVECTIONS;
  myTypeMap["CONVECTIONCONTROL"]                 = HCDI_OBJ_TYPE_CONVECTIONCONTROLS;
  myTypeMap["ENGCONTACT"]                        = HCDI_OBJ_TYPE_ENGCONTACTS;
  myTypeMap["CONTACTCONTROL"]                    = HCDI_OBJ_TYPE_CONTACTCONTROLS;
  myTypeMap["ENGANALYSIS"]                       = HCDI_OBJ_TYPE_ENGANALYSES;
  myTypeMap["ANALYSISCONTROL"]                   = HCDI_OBJ_TYPE_ANALYSISCONTROLS;
  myTypeMap["ENGLOADSTEP"]                       = HCDI_OBJ_TYPE_ENGLOADSTEPS;
  myTypeMap["LOADSTEPCONTROL"]                   = HCDI_OBJ_TYPE_LOADSTEPCONTROLS;
  myTypeMap["ENGLOADCASE"]                       = HCDI_OBJ_TYPE_ENGLOADCASES;
  myTypeMap["LOADCASECONTROL"]                   = HCDI_OBJ_TYPE_LOADCASECONTROLS;
  myTypeMap["ENGSUBCASE"]                        = HCDI_OBJ_TYPE_ENGSUBCASES;
  myTypeMap["SUBCASECONTROL"]                    = HCDI_OBJ_TYPE_SUBCASECONTROLS;
  myTypeMap["INTERNALSUPPORT"]                   = HCDI_OBJ_TYPE_INTERNALSUPPORTS;
  myTypeMap["NOISECONTROLTREATMENT"]             = HCDI_OBJ_TYPE_NOISECONTROLTREATMENTS;
  myTypeMap["CROSSROAD"]                         = HCDI_OBJ_TYPE_CROSSROADS;
  myTypeMap["PROCESSVARIABLE"]                   = HCDI_OBJ_TYPE_PROCESSVARIABLES;
  myTypeMap["DELTACONTROL"]                      = HCDI_OBJ_TYPE_DELTACONTROLS;
  myTypeMap["COORDINATESYSTEMCONTROL"]             = HCDI_OBJ_TYPE_COORDINATESYSTEMCONTROL;        // COORDINATESYSTEMCONTROLS
  myTypeMap["CURVECONTROL"]                        = HCDI_OBJ_TYPE_CURVECONTROL;        // CURVECONTROLS
  myTypeMap["ENGBEARINGPRESSURE"]                   = HCDI_OBJ_TYPE_ENGBEARINGPRESSURE;        // ENGBEARINGPRESSURES
  myTypeMap["ENGCENTRIFUGALFORC"]                   = HCDI_OBJ_TYPE_ENGCENTRIFUGALFORCE;        // ENGCENTRIFUGALFORCES
  myTypeMap["ENGCONCENTRATEDFORCE"]                 = HCDI_OBJ_TYPE_ENGCONCENTRATEDFORCE;        // ENGCONCENTRATEDFORCES
  myTypeMap["ENGCOORDINATESYSTEM"]                  = HCDI_OBJ_TYPE_ENGCOORDINATESYSTEM;        // ENGCOORDINATESYSTEMS
  myTypeMap["ENGCURVE"]                             = HCDI_OBJ_TYPE_ENGCURVE;        // ENGCURVES
  myTypeMap["ENGDAMPINGCONSTRAINT"]                 = HCDI_OBJ_TYPE_ENGDAMPINGCONSTRAINT;        // ENGDAMPINGCONSTRAINTS
  myTypeMap["ENGDAMPINGPRESSURE"]                   = HCDI_OBJ_TYPE_ENGDAMPINGPRESSURE;        // ENGDAMPINGPRESSURES
  myTypeMap["ENGENFORCEDDISPLACEMENT"]              = HCDI_OBJ_TYPE_ENGENFORCEDDISPLACEMENT;        // ENGENFORCEDDISPLACEMENTS
  myTypeMap["ENGEXCITATION"]                        = HCDI_OBJ_TYPE_ENGEXCITATION;        // ENGEXCITATIONS
  myTypeMap["ENGFIXEDCONSTRAINT"]                   = HCDI_OBJ_TYPE_ENGFIXEDCONSTRAINT;        // ENGFIXEDCONSTRAINTS
  myTypeMap["ENGFLUIDPRESSURE"]                     = HCDI_OBJ_TYPE_ENGFLUIDPRESSURE;        // ENGFLUIDPRESSURES
  myTypeMap["ENGHYDROSTATICPRESSURE"]               = HCDI_OBJ_TYPE_ENGHYDROSTATICPRESSURE;        // ENGHYDROSTATICPRESSURES
  myTypeMap["ENGINITIALPRESSURE"]                   = HCDI_OBJ_TYPE_ENGINITIALPRESSURE;        // ENGINITIALPRESSURES
  myTypeMap["ENGINITIALTEMPERATURE"]                = HCDI_OBJ_TYPE_ENGINITIALTEMPERATURE;        // ENGINITIALTEMPERATURES
  myTypeMap["ENGINITIALVELOCITY"]                   = HCDI_OBJ_TYPE_ENGINITIALVELOCITY;        // ENGINITIALVELOCITIES
  myTypeMap["ENGMANUFACTURINGPRESSURE"]             = HCDI_OBJ_TYPE_ENGMANUFACTURINGPRESSURE;        // ENGMANUFACTURINGPRESSURES
  myTypeMap["ENGMOMENT"]                            = HCDI_OBJ_TYPE_ENGMOMENT;        // ENGMOMENTS
  myTypeMap["ENGMOTION"]                            = HCDI_OBJ_TYPE_ENGMOTION;       // ENGMOTIONS
  myTypeMap["ENGOUTPUTREQUEST"]                     = HCDI_OBJ_TYPE_ENGOUTPUTREQUEST;        // ENGOUTPUTREQUESTS
  myTypeMap["ENGPARAMETER"]                         = HCDI_OBJ_TYPE_ENGPARAMETER;        // ENGPARAMETERS
  myTypeMap["ENGRANDOMRESPONSE"]                    = HCDI_OBJ_TYPE_ENGRANDOMRESPONSE;        // ENGRANDOMRESPONSES
  myTypeMap["ENGSTRUCTURALPRESSURE"]                = HCDI_OBJ_TYPE_ENGSTRUCTURALPRESSURE;        // ENGSTRUCTURALPRESSURES
  myTypeMap["ENGTABLE"]                             = HCDI_OBJ_TYPE_ENGTABLE;        // ENGTABLES
  myTypeMap["ENGTRACTIONPRESSURE"]                  = HCDI_OBJ_TYPE_ENGTRACTIONPRESSURE;        // ENGTRACTIONPRESSURES
  myTypeMap["ENGVELOCITYCONSTRAINT"]                = HCDI_OBJ_TYPE_ENGVELOCITYCONSTRAINT;        // ENGVELOCITYCONSTRAINTS
  myTypeMap["ENTITYDEATH"]                          = HCDI_OBJ_TYPE_ENTITYDEATH;        // ENTITYDEATHS
  myTypeMap["LOCK"]                                 = HCDI_OBJ_TYPE_LOCK;        
  myTypeMap["MATERIALBEHAVIOR"]                     = HCDI_OBJ_TYPE_MATERIALBEHAVIOR;        // MATERIALBEHAVIORS
  myTypeMap["NCTLAYER"]                             = HCDI_OBJ_TYPE_NCTLAYER;        // NCTLAYERS
  myTypeMap["OUTPUTREQUESTCONTROL"]                 = HCDI_OBJ_TYPE_OUTPUTREQUESTCONTROL;        // OUTPUTREQUESTCONTROLS
  myTypeMap["PARAMETERCONTROL"]                     = HCDI_OBJ_TYPE_PARAMETERCONTROL;        // PARAMETERCONTROLS
  myTypeMap["PARTCONTROL"]                          = HCDI_OBJ_TYPE_PARTCONTROL;        // PARTCONTROLS
  myTypeMap["PATTERN"]                              = HCDI_OBJ_TYPE_PATTERN;        // PATTERNS
  myTypeMap["PATTERNCONTROL"]                       = HCDI_OBJ_TYPE_PATTERNCONTROL;        // PATTERNCONTROLS
  myTypeMap["PROPERTYCONTROL"]                      = HCDI_OBJ_TYPE_PROPERTYCONTROL;        // PROPERTYCONTROLS
  myTypeMap["QUERYDEFINITION"]                      = HCDI_OBJ_TYPE_QUERYDEFINITION;        // QUERYDEFINITIONS
  myTypeMap["SCENARIO"]                             = HCDI_OBJ_TYPE_SCENARIO;        // SCENARIOS
  myTypeMap["SUBSYSTEMCONTROL"]                     = HCDI_OBJ_TYPE_SUBSYSTEMCONTROL;        // SUBSYSTEMCONTROLS
  myTypeMap["TABLECONTROL"]                         = HCDI_OBJ_TYPE_TABLECONTROL;        // TABLECONTROLS
  myTypeMap["PARTINSTANCE"]                         = HCDI_OBJ_TYPE_PARTINSTANCE;        // PARTINSTANCES
  myTypeMap["PARTREPRESENTATIONDEFINITION"]         = HCDI_OBJ_TYPE_PARTREPRESENTATIONDEFINITION;        // PARTREPRESENTATIONDEFINITIONS
  myTypeMap["PARTREPRESENTATION"]                   = HCDI_OBJ_TYPE_PARTREPRESENTATION;       // PARTREPRESENTATIONS
  myTypeMap["TOPOLOGYSHELL"]                        = HCDI_OBJ_TYPE_TOPOLOGYSHELL;        // TOPOLOGYSHELLS
  myTypeMap["TOPOLOGYSOLID"]                        = HCDI_OBJ_TYPE_TOPOLOGYSOLID;        // TOPOLOGYSOLIDS
  myTypeMap["WORKFLOW"]                             = HCDI_OBJ_TYPE_WORKFLOW;       // WORKFLOWS
  myTypeMap["ENGMASS"]                              = HCDI_OBJ_TYPE_ENGMASS;        // ENGMASSES
  myTypeMap["MASSCONTROL"]                          = HCDI_OBJ_TYPE_MASSCONTROL;
  myTypeMap["GEOMETRY"]                             = HCDI_OBJ_TYPE_GEOMETRY;
  myTypeMap["MODELS"]                               = HCDI_OBJ_TYPE_MODELS;
  myTypeMap["DICTS"]                                = HCDI_OBJ_TYPE_DICTS,
  myTypeMap["PLOTLEGENDS"]                          = HCDI_OBJ_TYPE_PLOTLEGENDS,
  myTypeMap["DESCRIPTORS"]                          = HCDI_OBJ_TYPE_DESCRIPTORS,
  myTypeMap["PLOTTITLES"]                           = HCDI_OBJ_TYPE_PLOTTITLES,
  myTypeMap["MINMAXTITLES"]                         = HCDI_OBJ_TYPE_MINMAXTITLES,
  myTypeMap["PLOTINFOTITLE"]                        = HCDI_OBJ_TYPE_PLOTINFOTITLE,
  myTypeMap["SIMPLEPLOTS"]                          = HCDI_OBJ_TYPE_SIMPLEPLOTS,
  myTypeMap["COMPLEXPLOTS"]                         = HCDI_OBJ_TYPE_COMPLEXPLOTS,
  myTypeMap["ALTGEOMENGINE"]                        = HCDI_OBJ_TYPE_ALTGEOMENGINE,
  myTypeMap["INCLUDEFILE"]                          = HCDI_OBJ_TYPE_INCLUDEFILES,
  myTypeMap["METADATA"]                             = HCDI_OBJ_TYPE_METADATA,
  myTypeMap["ENTITY_MAX"]                           = HCDI_OBJ_TYPE_MAX;
  myTypeMap["SUBOBJECT"]                            = HCDI_OBJ_TYPE_SUBOBJECT;
  myTypeMap["MULTIOBJECT"]                          = HCDI_OBJ_TYPE_MULTIOBJECT;
  myTypeMap["HC_MAX"]                               = HCDI_OBJ_TYPE_HC_MAX;
}

object_type_e MvTypeMapBase_t::getType(const string &keyword) const {
  MvStrTypeMap_t::const_iterator it=myTypeMap.find(keyword);
  if(it!=myTypeMap.end()) return (*it).second;
  return HCDI_OBJ_TYPE_NULL;
}

const string &MvTypeMapBase_t::getType(object_type_e type) const {
  static const string a_MV_EMPTY_str = "EMPTY";
  static const string a_MODEL_str    = "MODEL";
  static const string a_ROOT_str     = "ROOT";
  static const string a_SHELL_str    = "2DELEM";
  /*
  if(type==MV_EMPTY) return a_MV_EMPTY_str;
  if(type==MODEL)    return a_MODEL_str;
  if(type==ROOT)     return a_ROOT_str;
  if(type==SHELL)    return a_SHELL_str;
  */
  if(type<HCDI_OBJ_TYPE_NULL || ((int)type)>=myNbTypes) type= HCDI_OBJ_TYPE_NULL;
  return myTypeArray[type];
}

void MvTypeMapBase_t::InitTypeArray() {
  // Counting
  myNbTypes=0;
  MvStrTypeMap_t::const_iterator it;
  for(it=myTypeMap.begin();it!=myTypeMap.end();++it) {
    int type=(int)((*it).second);
    if(type>myNbTypes) myNbTypes=type;
  }
  // Allocating
  myTypeArray=new string[++myNbTypes];
  // Filling
  for(int i=0;i<myNbTypes;i++) myTypeArray[i]="UNKNOWN";
  for(it=myTypeMap.begin();it!=myTypeMap.end();++it) {
    int type=(int)((*it).second);
    if(type>0) myTypeArray[type]=(*it).first;
  }
}


static const MvTypeMap_t *get_type_map(int option=0) {
    //static const MvTypeMap_t MvTypeMap_t;
    static MvTypeMap_t *MV_TYPE_MAP=NULL;/*multimodel.. not required*/
    switch(option)
    {
    case 0:
        if(MV_TYPE_MAP==NULL) MV_TYPE_MAP=new MvTypeMap_t();
        break;
    case -1:  // Destruction
        if(NULL!=MV_TYPE_MAP)
        {
            delete MV_TYPE_MAP;
            MV_TYPE_MAP = NULL;
        }
        break;
    default:
        break;
    }
    return MV_TYPE_MAP;
}
void MV_delete_type_map()
{
   get_type_map(-1);
}
object_type_e MV_get_type(const string &keyword) {

    return get_type_map()->getType(keyword);
}


const string &MV_get_type(object_type_e obj_type) {
   /* 
    ApplicationMode_e mode = GetApplicationMode();
    if(obj_type == PAM2G_PLY && (mode == CODE_IS_RADIOSS || 
        mode == CODE_IS_RADIOSS_EXPLI || mode == CODE_IS_RADIOSS_IMPLI))
    {
        return get_type_map()->getType(PLY_ENTITY);
    }
    */
  return get_type_map()->getType(obj_type);
}

// Solver type names

class MvTypeMapSolver_t : public MvTypeMapBase_t {
public:
  MvTypeMapSolver_t();
  ~MvTypeMapSolver_t() {}
private:
  void InitTypeMap();
};

MvTypeMapSolver_t::MvTypeMapSolver_t() : MvTypeMapBase_t() {
  InitTypeMap();
  InitTypeArray();
}

void MvTypeMapSolver_t::InitTypeMap() {
    // NB: Not sure whether this is enough or better loop from somewhere negative to MCDS_UNKNOBJ?
    for(int i = HCDI_OBJ_TYPE_NULL + 1; i < HCDI_OBJ_TYPE_HC_MAX; i++)
    {
        object_type_e obj_type = (object_type_e) i;
        string s_type_str  = MV_get_solver_type(obj_type);
        char *type_str = (char *)(s_type_str.c_str());

        myTypeMap[type_str] = obj_type;
        my_free(type_str);        
    }
}

static const MvTypeMapSolver_t &get_type_map_solver() {
  static const MvTypeMapSolver_t MV_TYPE_MAP_SOLVER;
  return MV_TYPE_MAP_SOLVER;
}

object_type_e  MV_get_solver_type(const string &keyword)
{
  return get_type_map_solver().getType(keyword);
}

const string  &MV_get_solver_type(object_type_e obj_type)
{
  return get_type_map_solver().getType(obj_type);
}

extern "C"
const char *MV_get_solver_type_cstring(obj_type_e obj_type)
{
     return get_type_map_solver().getType(obj_type).c_str();
}
