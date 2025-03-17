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

#include <dyna2rad/convertdefinetransform.h>
#include <dyna2rad/dyna2rad.h>

using namespace std;
using namespace sdi;

void sdiD2R::ConvertDefineTransform::ConvertEntities()
{
}


void sdiD2R::ConvertDefineTransform::ConvertSelectedDefineTransform(int defineTransformId, int setNodeId, int includeTransformId)
{
    ConvertSelectedEntity(defineTransformId, setNodeId, includeTransformId);
}

void sdiD2R::ConvertDefineTransform::ConvertSelectedEntity(int defineTransformId, int setNodeId, int includeTransformId)
{
     HandleRead defineTransformHRead;
     HandleRead TransformHRead;
     p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*DEFINE_TRANSFORMATION"), defineTransformId, defineTransformHRead);
     if(!defineTransformHRead.IsValid())return;

     EntityRead defineTransformEntityRead(p_lsdynaModel, defineTransformHRead);

     EntityType radTransformType = p_radiossModel->GetEntityType("/TRANSFORM");
     EntityType radSetType = p_radiossModel->GetEntityType("/SET/GENERAL");
     EntityType radSubmodelType = p_radiossModel->GetEntityType("//SUBMODEL");
     EntityType radNodeType = p_radiossModel->GetEntityType("/NODE");

     sdiString defineTransformName = defineTransformEntityRead.GetName();

     sdiValue tempVal;

     int transformations_count = 0;
     int nbPoints = 0;
     int cptPoints = 0;
     bool isOk = false;

     isOk = defineTransformEntityRead.GetValue(sdiIdentifier("transformations_count"), tempVal);
     if (isOk) tempVal.GetValue(transformations_count);


    SelectionRead selTransform(p_lsdynaModel, "*DEFINE_TRANSFORMATION");

    map<int, int> mapPointIdx;

     for (int i = 0; i < transformations_count; i++)
     {
         sdiString OPTION;
         isOk = defineTransformEntityRead.GetValue(sdiIdentifier("OPTION",0 ,i), tempVal);
         if (isOk) tempVal.GetValue(OPTION);
         if (OPTION == "POINT") nbPoints++;
     }


     std::vector<double> xPoint(nbPoints), yPoint(nbPoints), zPoint(nbPoints);
     std::vector<double> pointId(nbPoints);

     for (int i = 0; i < transformations_count; i++)
     {   
         p_radiossModel->FindById(p_radiossModel->GetEntityType("/TRANSFORM"), defineTransformId, TransformHRead);
         if(TransformHRead.IsValid()) 
            defineTransformId = 0;

         HandleEdit transformHEdit;

         sdiString OPTION;
         isOk = defineTransformEntityRead.GetValue(sdiIdentifier("OPTION",0 ,i), tempVal);
         if (isOk) tempVal.GetValue(OPTION);

         int a1_int = 0;
         int a2_int = 0;
         int a3_int = 0;
         int a4_int = 0;
         int a5_int = 0;
         int a6_int = 0;
         bool isNodes = false;
         double a1 = 0;
         double a2 = 0;
         double a3 = 0;
         double a4 = 0;
         double a5 = 0;
         double a6 = 0;
         double a7 = 0;

         
         if (OPTION == "POINT")
         {
             isOk = defineTransformEntityRead.GetValue(sdiIdentifier("localid",0 ,i), tempVal);
             if (isOk) tempVal.GetValue(a1_int);
             isOk = defineTransformEntityRead.GetValue(sdiIdentifier("position_x",0 ,i), tempVal);
             if (isOk) tempVal.GetValue(a1);
             isOk = defineTransformEntityRead.GetValue(sdiIdentifier("position_y",0 ,i), tempVal);
             if (isOk) tempVal.GetValue(a2);
             isOk = defineTransformEntityRead.GetValue(sdiIdentifier("position_z",0 ,i), tempVal);
             if (isOk) tempVal.GetValue(a3);

             mapPointIdx[a1_int] = cptPoints;
             xPoint[cptPoints] = a1;
             yPoint[cptPoints] = a2;
             zPoint[cptPoints] = a3;
             cptPoints++;
         }
         else if (OPTION == "TRANSL")
         {
             tempVal=sdiValue(a1);
             isOk = defineTransformEntityRead.GetValue(sdiIdentifier("translation_x",0 ,i), tempVal);
             if (isOk) tempVal.GetValue(a1);
             tempVal=sdiValue(a2);
             isOk = defineTransformEntityRead.GetValue(sdiIdentifier("translation_y",0 ,i), tempVal);
             if (isOk) tempVal.GetValue(a2);
             tempVal=sdiValue(a3);
             isOk = defineTransformEntityRead.GetValue(sdiIdentifier("translation_z",0 ,i), tempVal);
             if (isOk) tempVal.GetValue(a3);

             p_radiossModel->CreateEntity(transformHEdit, "/TRANSFORM/TRA", defineTransformName, defineTransformId);
             EntityEdit transformEdit(p_radiossModel, transformHEdit);
             if( setNodeId != 0 ) transformEdit.SetValue(sdiIdentifier("GR_NODE"),sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(setNodeId, "*SET_NODE"))));
             if( includeTransformId != 0 ) transformEdit.SetValue(sdiIdentifier("SUBMODEL"), sdiValue(sdiValueEntity(sdiValueEntityType(radSubmodelType), includeTransformId)));

             transformEdit.SetValue(sdiIdentifier("translation_x"), sdiValue(a1));
             transformEdit.SetValue(sdiIdentifier("translation_y"), sdiValue(a2));
             transformEdit.SetValue(sdiIdentifier("translation_z"), sdiValue(a3));
         }
         else if (OPTION == "TRANSL2ND")
         {
             tempVal=sdiValue(a3);
             isOk = defineTransformEntityRead.GetValue(sdiIdentifier("translation_magnitude",0 ,i), tempVal);
             if (isOk) tempVal.GetValue(a3);

             HandleRead handleN1;
             HandleRead handleN2;

             defineTransformEntityRead.GetEntityHandle(sdiIdentifier("position_node1",0 ,i), handleN1);
             defineTransformEntityRead.GetEntityHandle(sdiIdentifier("position_node2",0 ,i), handleN2);

             NodeRead nodeReadN1(p_lsdynaModel, handleN1);
             sdiTriple posN1 = nodeReadN1.GetPosition();
             NodeRead nodeReadN2(p_lsdynaModel, handleN2);
             sdiTriple posN2 = nodeReadN2.GetPosition();

             p_radiossModel->CreateEntity(transformHEdit, "/TRANSFORM/TRA", defineTransformName, defineTransformId);
             EntityEdit transformEdit(p_radiossModel, transformHEdit);
             if( setNodeId != 0 ) transformEdit.SetValue(sdiIdentifier("GR_NODE"),sdiValue(sdiValueEntity(sdiValueEntityType(radSetType),setNodeId)));
             if( includeTransformId != 0 ) transformEdit.SetValue(sdiIdentifier("SUBMODEL"), sdiValue(sdiValueEntity(sdiValueEntityType(radSubmodelType), includeTransformId)));
             transformEdit.SetValue(sdiIdentifier("node1"), sdiValue(sdiValueEntity(radNodeType, nodeReadN1.GetId())));

             HandleNodeEdit nodeHEdit;
             sdiTriple Rnode2(0.0, 0.0, 0.0);

             sdiTriple Direction(posN2-posN1);
             Direction.Normalize();

             Rnode2[0] = posN1[0] + a3 * Direction[0];
             Rnode2[1] = posN1[1] + a3 * Direction[1];
             Rnode2[2] = posN1[2] + a3 * Direction[2];
             p_radiossModel->CreateNode(nodeHEdit, "/NODE", Rnode2);
             if (nodeHEdit.IsValid())
                 transformEdit.SetValue(sdiIdentifier("node2"), sdiValue(sdiValueEntity(radNodeType, nodeHEdit.GetId(p_radiossModel))));
         }
         else if (OPTION == "ROTATE3NA")
         {
             HandleRead handleN1;
             HandleRead handleN2;
             HandleRead handleN3;

             defineTransformEntityRead.GetEntityHandle(sdiIdentifier("position_node1",0 ,i), handleN1);
             defineTransformEntityRead.GetEntityHandle(sdiIdentifier("position_node2",0 ,i), handleN2);
             defineTransformEntityRead.GetEntityHandle(sdiIdentifier("position_node3",0 ,i), handleN3);
             isOk = defineTransformEntityRead.GetValue(sdiIdentifier("rotation_angle",0 ,i), tempVal);
             if (isOk) tempVal.GetValue(a1);

             NodeRead nodeReadN1(p_lsdynaModel, handleN1);
             NodeRead nodeReadN2(p_lsdynaModel, handleN2);
             NodeRead nodeReadN3(p_lsdynaModel, handleN3);

             p_radiossModel->CreateEntity(transformHEdit, "/TRANSFORM/ROT", defineTransformName, defineTransformId);
             EntityEdit transformEdit(p_radiossModel, transformHEdit);

             if( setNodeId != 0 ) transformEdit.SetValue(sdiIdentifier("GR_NODE"),sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(setNodeId, "*SET_NODE"))));
             if( includeTransformId != 0 ) transformEdit.SetValue(sdiIdentifier("SUBMODEL"), sdiValue(sdiValueEntity(sdiValueEntityType(radSubmodelType), includeTransformId)));

             transformEdit.SetValue(sdiIdentifier("node1"), sdiValue(sdiValueEntity(radNodeType,nodeReadN1.GetId())));
             transformEdit.SetValue(sdiIdentifier("node2"), sdiValue(sdiValueEntity(radNodeType,nodeReadN2.GetId())));
             transformEdit.SetValue(sdiIdentifier("node3"), sdiValue(sdiValueEntity(radNodeType,nodeReadN3.GetId())));
             transformEdit.SetValue(sdiIdentifier("rotation_angle"), sdiValue(a1));
         }
         else if (OPTION == "ROTATE")
         {

             isOk = defineTransformEntityRead.GetValue(sdiIdentifier("rotation_point_x",0 ,i), tempVal);
             if (isOk) tempVal.GetValue(a4);
             isOk = defineTransformEntityRead.GetValue(sdiIdentifier("rotation_point_y",0 ,i), tempVal);
             if (isOk) tempVal.GetValue(a5);
             isOk = defineTransformEntityRead.GetValue(sdiIdentifier("rotation_point_z",0 ,i), tempVal);
             if (isOk) tempVal.GetValue(a6);
             isOk = defineTransformEntityRead.GetValue(sdiIdentifier("rotation_angle",0 ,i), tempVal);
             if (isOk) tempVal.GetValue(a7);

             sdiValueEntity pointReadP1;
             isOk = defineTransformEntityRead.GetValue(sdiIdentifier("point1",0 ,i), tempVal);
             if (isOk) tempVal.GetValue(pointReadP1);
             int pt1 = pointReadP1.GetId();

             sdiValueEntity pointReadP2;
             isOk = defineTransformEntityRead.GetValue(sdiIdentifier("point2",0 ,i), tempVal);
             if (isOk) tempVal.GetValue(pointReadP2);
             tempVal.GetValue(pointReadP2);
             int pt2 = pointReadP2.GetId();

             if (pt1 != 0 || pt2 != 0) isNodes = true; 

             if ( isNodes && nbPoints > 0)
             {
                 isOk = defineTransformEntityRead.GetValue(sdiIdentifier("rotation_angle",0 ,i), tempVal);
                 if (isOk) tempVal.GetValue(a3);

                 p_radiossModel->CreateEntity(transformHEdit, "/TRANSFORM/ROT", defineTransformName, defineTransformId);
                 EntityEdit transformEdit(p_radiossModel, transformHEdit);
                 if( setNodeId != 0 ) transformEdit.SetValue(sdiIdentifier("GR_NODE"),sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(setNodeId, "*SET_NODE"))));
                 if( includeTransformId != 0 ) transformEdit.SetValue(sdiIdentifier("SUBMODEL"), sdiValue(sdiValueEntity(sdiValueEntityType(radSubmodelType), includeTransformId)));

                 transformEdit.SetValue(sdiIdentifier("rotation_point1_x"), sdiValue(xPoint[mapPointIdx[pt1]]));
                 transformEdit.SetValue(sdiIdentifier("rotation_point1_y"), sdiValue(yPoint[mapPointIdx[pt1]]));
                 transformEdit.SetValue(sdiIdentifier("rotation_point1_z"), sdiValue(zPoint[mapPointIdx[pt1]]));
                 transformEdit.SetValue(sdiIdentifier("rotation_point2_x"), sdiValue(xPoint[mapPointIdx[pt2]]));
                 transformEdit.SetValue(sdiIdentifier("rotation_point2_y"), sdiValue(yPoint[mapPointIdx[pt2]]));
                 transformEdit.SetValue(sdiIdentifier("rotation_point2_z"), sdiValue(zPoint[mapPointIdx[pt2]]));
                 transformEdit.SetValue(sdiIdentifier("rotation_angle"), sdiValue(a7));
             }
             else
             {
                 isOk = defineTransformEntityRead.GetValue(sdiIdentifier("rotation_direction_x",0 ,i), tempVal);
                 if (isOk) tempVal.GetValue(a1);
                 isOk = defineTransformEntityRead.GetValue(sdiIdentifier("rotation_direction_y",0 ,i), tempVal);
                 if (isOk) tempVal.GetValue(a2);
                 isOk = defineTransformEntityRead.GetValue(sdiIdentifier("rotation_direction_z",0 ,i), tempVal);
                 if (isOk) tempVal.GetValue(a3);

                 p_radiossModel->CreateEntity(transformHEdit, "/TRANSFORM/ROT", defineTransformName, defineTransformId);
                 EntityEdit transformEdit(p_radiossModel, transformHEdit);
                 if( setNodeId != 0 ) transformEdit.SetValue(sdiIdentifier("GR_NODE"),sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(setNodeId, "*SET_NODE"))));
                 if( includeTransformId != 0 ) transformEdit.SetValue(sdiIdentifier("SUBMODEL"), sdiValue(sdiValueEntity(sdiValueEntityType(radSubmodelType), includeTransformId)));

                 transformEdit.SetValue(sdiIdentifier("rotation_point1_x"), sdiValue(a4));
                 transformEdit.SetValue(sdiIdentifier("rotation_point1_y"), sdiValue(a5));
                 transformEdit.SetValue(sdiIdentifier("rotation_point1_z"), sdiValue(a6));
                 transformEdit.SetValue(sdiIdentifier("rotation_point2_x"), sdiValue(a4+a1));
                 transformEdit.SetValue(sdiIdentifier("rotation_point2_y"), sdiValue(a5+a2));
                 transformEdit.SetValue(sdiIdentifier("rotation_point2_z"), sdiValue(a6+a3));
                 transformEdit.SetValue(sdiIdentifier("rotation_angle"), sdiValue(a7));
             }
         }
         else if (OPTION == "SCALE")
         {
             isOk = defineTransformEntityRead.GetValue(sdiIdentifier("scalefactor_x",0 ,i), tempVal);
             if (isOk) tempVal.GetValue(a1);
             isOk = defineTransformEntityRead.GetValue(sdiIdentifier("scalefactor_y",0 ,i), tempVal);
             if (isOk) tempVal.GetValue(a2);
             isOk = defineTransformEntityRead.GetValue(sdiIdentifier("scalefactor_z",0 ,i), tempVal);
             if (isOk) tempVal.GetValue(a3);

             p_radiossModel->CreateEntity(transformHEdit, "/TRANSFORM/SCA", defineTransformName, defineTransformId);
             EntityEdit transformEdit(p_radiossModel, transformHEdit);
             if( setNodeId != 0 ) transformEdit.SetValue(sdiIdentifier("GR_NODE"),sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(setNodeId, "*SET_NODE"))));
             if( includeTransformId != 0 ) transformEdit.SetValue(sdiIdentifier("SUBMODEL"), sdiValue(sdiValueEntity(sdiValueEntityType(radSubmodelType), includeTransformId)));

             transformEdit.SetValue(sdiIdentifier("scalefactor_x"), sdiValue(a1));
             transformEdit.SetValue(sdiIdentifier("scalefactor_y"), sdiValue(a2));
             transformEdit.SetValue(sdiIdentifier("scalefactor_z"), sdiValue(a3));

             HandleNodeEdit nodeHEdit;
             sdiTriple centroid(0.0, 0.0, 0.0);
             p_radiossModel->CreateNode(nodeHEdit, "/NODE", centroid);
             if (nodeHEdit.IsValid())
                 transformEdit.SetValue(sdiIdentifier("node1"), sdiValue(sdiValueEntity(1, nodeHEdit.GetId(p_radiossModel))));

         }
         else if (OPTION == "MIRROR" )
         {
             isOk = defineTransformEntityRead.GetValue(sdiIdentifier("reflect_point1_x",0 ,i), tempVal);
             if (isOk) tempVal.GetValue(a1);
             isOk = defineTransformEntityRead.GetValue(sdiIdentifier("reflect_point1_y",0 ,i), tempVal);
             if (isOk) tempVal.GetValue(a2);
             isOk = defineTransformEntityRead.GetValue(sdiIdentifier("reflect_point1_z",0 ,i), tempVal);
             if (isOk) tempVal.GetValue(a3);
             isOk = defineTransformEntityRead.GetValue(sdiIdentifier("reflect_point2_x",0 ,i), tempVal);
             if (isOk) tempVal.GetValue(a4);
             isOk = defineTransformEntityRead.GetValue(sdiIdentifier("reflect_point2_y",0 ,i), tempVal);
             if (isOk) tempVal.GetValue(a5);
             isOk = defineTransformEntityRead.GetValue(sdiIdentifier("reflect_point2_z",0 ,i), tempVal);
             if (isOk) tempVal.GetValue(a6);

             p_radiossModel->CreateEntity(transformHEdit, "/TRANSFORM/SYM", defineTransformName, defineTransformId);
             EntityEdit transformEdit(p_radiossModel, transformHEdit);
             if( setNodeId != 0 ) transformEdit.SetValue(sdiIdentifier("GR_NODE"),sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(setNodeId, "*SET_NODE"))));
             if( includeTransformId != 0 ) transformEdit.SetValue(sdiIdentifier("SUBMODEL"), sdiValue(sdiValueEntity(sdiValueEntityType(radSubmodelType), includeTransformId)));

             transformEdit.SetValue(sdiIdentifier("reflect_point1_x"), sdiValue(a1));
             transformEdit.SetValue(sdiIdentifier("reflect_point1_y"), sdiValue(a2));
             transformEdit.SetValue(sdiIdentifier("reflect_point1_z"), sdiValue(a3));
             transformEdit.SetValue(sdiIdentifier("reflect_point2_x"), sdiValue(a4));
             transformEdit.SetValue(sdiIdentifier("reflect_point2_y"), sdiValue(a5));
             transformEdit.SetValue(sdiIdentifier("reflect_point2_z"), sdiValue(a6));
         }
         else if (OPTION == "POS6P" )
         {
            defineTransformEntityRead.GetValue(sdiIdentifier("point1",0 ,i), tempVal);
            sdiValueEntity pointReadP1;
            tempVal.GetValue(pointReadP1);
            int pt1 = pointReadP1.GetId();

            defineTransformEntityRead.GetValue(sdiIdentifier("point2",0 ,i), tempVal);
            sdiValueEntity  pointReadP2;
            tempVal.GetValue(pointReadP2);
            int pt2 = pointReadP2.GetId();

            defineTransformEntityRead.GetValue(sdiIdentifier("point3",0 ,i), tempVal);
            sdiValueEntity pointReadP3;
            tempVal.GetValue(pointReadP3);
            int pt3 = pointReadP3.GetId();

            defineTransformEntityRead.GetValue(sdiIdentifier("point4",0 ,i), tempVal);
            sdiValueEntity pointReadP4;
            tempVal.GetValue(pointReadP4);
            int pt4 = pointReadP4.GetId();

            defineTransformEntityRead.GetValue(sdiIdentifier("point5",0 ,i), tempVal);
            sdiValueEntity pointReadP5;
            tempVal.GetValue(pointReadP5);
            int pt5 = pointReadP5.GetId();

            defineTransformEntityRead.GetValue(sdiIdentifier("point6",0 ,i), tempVal);
            sdiValueEntity pointReadP6;
            tempVal.GetValue(pointReadP6);
            int pt6 = pointReadP6.GetId();

             p_radiossModel->CreateEntity(transformHEdit, "/TRANSFORM/POSITION", defineTransformName, defineTransformId);
             EntityEdit transformEdit(p_radiossModel, transformHEdit);
             if( setNodeId != 0 ) transformEdit.SetValue(sdiIdentifier("GR_NODE"),sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(setNodeId, "*SET_NODE"))));
             if( includeTransformId != 0 ) transformEdit.SetValue(sdiIdentifier("SUBMODEL"), sdiValue(sdiValueEntity(sdiValueEntityType(radSubmodelType), includeTransformId)));

             transformEdit.SetValue(sdiIdentifier("X_Point_1"), sdiValue(xPoint[mapPointIdx[pt1]]));
             transformEdit.SetValue(sdiIdentifier("Y_Point_1"), sdiValue(yPoint[mapPointIdx[pt1]]));
             transformEdit.SetValue(sdiIdentifier("Z_Point_1"), sdiValue(zPoint[mapPointIdx[pt1]]));
             transformEdit.SetValue(sdiIdentifier("X_Point_2"), sdiValue(xPoint[mapPointIdx[pt2]]));
             transformEdit.SetValue(sdiIdentifier("Y_Point_2"), sdiValue(yPoint[mapPointIdx[pt2]]));
             transformEdit.SetValue(sdiIdentifier("Z_Point_2"), sdiValue(zPoint[mapPointIdx[pt2]]));
             transformEdit.SetValue(sdiIdentifier("X_Point_3"), sdiValue(xPoint[mapPointIdx[pt3]]));
             transformEdit.SetValue(sdiIdentifier("Y_Point_3"), sdiValue(yPoint[mapPointIdx[pt3]]));
             transformEdit.SetValue(sdiIdentifier("Z_Point_3"), sdiValue(zPoint[mapPointIdx[pt3]]));
             transformEdit.SetValue(sdiIdentifier("X_Point_4"), sdiValue(xPoint[mapPointIdx[pt4]]));
             transformEdit.SetValue(sdiIdentifier("Y_Point_4"), sdiValue(yPoint[mapPointIdx[pt4]]));
             transformEdit.SetValue(sdiIdentifier("Z_Point_4"), sdiValue(zPoint[mapPointIdx[pt4]]));
             transformEdit.SetValue(sdiIdentifier("X_Point_5"), sdiValue(xPoint[mapPointIdx[pt5]]));
             transformEdit.SetValue(sdiIdentifier("Y_Point_5"), sdiValue(yPoint[mapPointIdx[pt5]]));
             transformEdit.SetValue(sdiIdentifier("Z_Point_5"), sdiValue(zPoint[mapPointIdx[pt5]]));
             transformEdit.SetValue(sdiIdentifier("X_Point_6"), sdiValue(xPoint[mapPointIdx[pt6]]));
             transformEdit.SetValue(sdiIdentifier("Y_Point_6"), sdiValue(yPoint[mapPointIdx[pt6]]));
             transformEdit.SetValue(sdiIdentifier("Z_Point_6"), sdiValue(zPoint[mapPointIdx[pt6]]));
         }
         else if (OPTION == "POS6N" )
         {
             HandleRead handleN1;
             HandleRead handleN2;
             HandleRead handleN3;
             HandleRead handleN4;
             HandleRead handleN5;
             HandleRead handleN6;

             defineTransformEntityRead.GetEntityHandle(sdiIdentifier("position_node1",0 ,i), handleN1);
             defineTransformEntityRead.GetEntityHandle(sdiIdentifier("position_node2",0 ,i), handleN2);
             defineTransformEntityRead.GetEntityHandle(sdiIdentifier("position_node3",0 ,i), handleN3);
             defineTransformEntityRead.GetEntityHandle(sdiIdentifier("position_node4",0 ,i), handleN4);
             defineTransformEntityRead.GetEntityHandle(sdiIdentifier("position_node5",0 ,i), handleN5);
             defineTransformEntityRead.GetEntityHandle(sdiIdentifier("position_node6",0 ,i), handleN6);

             NodeRead nodeReadN1(p_lsdynaModel, handleN1);
             NodeRead nodeReadN2(p_lsdynaModel, handleN2);
             NodeRead nodeReadN3(p_lsdynaModel, handleN3);
             NodeRead nodeReadN4(p_lsdynaModel, handleN4);
             NodeRead nodeReadN5(p_lsdynaModel, handleN5);
             NodeRead nodeReadN6(p_lsdynaModel, handleN6);

             p_radiossModel->CreateEntity(transformHEdit, "/TRANSFORM/POSITION", defineTransformName, defineTransformId);
             EntityEdit transformEdit(p_radiossModel, transformHEdit);

             if( setNodeId != 0 ) transformEdit.SetValue(sdiIdentifier("GR_NODE"),sdiValue(sdiValueEntity(radSetType, DynaToRad::GetRadiossSetIdFromLsdSet(setNodeId, "*SET_NODE"))));
             if( includeTransformId != 0 ) transformEdit.SetValue(sdiIdentifier("SUBMODEL"), sdiValue(sdiValueEntity(sdiValueEntityType(radSubmodelType), includeTransformId)));

             transformEdit.SetValue(sdiIdentifier("node1"), sdiValue(sdiValueEntity(radNodeType,nodeReadN1.GetId())));
             transformEdit.SetValue(sdiIdentifier("node2"), sdiValue(sdiValueEntity(radNodeType,nodeReadN2.GetId())));
             transformEdit.SetValue(sdiIdentifier("node3"), sdiValue(sdiValueEntity(radNodeType,nodeReadN3.GetId())));
             transformEdit.SetValue(sdiIdentifier("node4"), sdiValue(sdiValueEntity(radNodeType,nodeReadN4.GetId())));
             transformEdit.SetValue(sdiIdentifier("node5"), sdiValue(sdiValueEntity(radNodeType,nodeReadN5.GetId())));
             transformEdit.SetValue(sdiIdentifier("node6"), sdiValue(sdiValueEntity(radNodeType,nodeReadN6.GetId())));
         }

         if (transformHEdit.IsValid())
         {
             sdiConvert::SDIHandlReadList sourceDefineTransform = { {defineTransformHRead} };
             sdiConvert::Convert::PushToConversionLog(std::make_pair(transformHEdit, sourceDefineTransform));
         }
     }
}
