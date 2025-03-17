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
#include <dyna2rad/convertfrictions.h>


using namespace std;
using namespace sdi;


void sdiD2R::ConvertFriction::ConvertAllFrictions()
{

    ConvertEntities();

}


void sdiD2R::ConvertFriction::ConvertEntities()
{
    ConvertFrictions();

    // + other frictions

}


void sdiD2R::ConvertFriction::ConvertFrictions()
{

    SelectionRead selFriction(p_lsdynaModel, "*DEFINE_FRICTION");

    while (selFriction.Next())
    {
      HandleEdit frictionHEdit;
      p_radiossModel->CreateEntity(frictionHEdit, "/FRICTION",selFriction->GetName(),selFriction->GetId());
      if (frictionHEdit.IsValid())
      {
        EntityEdit frictionEdit(p_radiossModel, frictionHEdit);
        EntityType radPartType = p_radiossModel->GetEntityType("/PART");
        EntityType radSetType = p_radiossModel->GetEntityType("/SET");

        frictionEdit.SetValue(sdiIdentifier("I_fric"),sdiValue(2));
        frictionEdit.SetValue(sdiIdentifier("I_form"),sdiValue(2));

        sdiValue tempValue;

        // constant (fixed part of card)
        double lsdFSD;
        tempValue = sdiValue(lsdFSD);
        selFriction->GetValue(sdiIdentifier("FS_D"), tempValue);
        tempValue.GetValue(lsdFSD);

        double lsdFDD;
        tempValue = sdiValue(lsdFDD);
        selFriction->GetValue(sdiIdentifier("FD_D"), tempValue);
        tempValue.GetValue(lsdFDD);

        double lsdDCD;
        tempValue = sdiValue(lsdDCD);
        selFriction->GetValue(sdiIdentifier("DC_D"), tempValue);
        tempValue.GetValue(lsdDCD);

        double lsdVCD;
        tempValue = sdiValue(lsdVCD);
        selFriction->GetValue(sdiIdentifier("VC_D"), tempValue);
        tempValue.GetValue(lsdVCD);

        double radC5=0.0, radC6=0.0;
        radC5 = lsdFSD-lsdFDD;
        if (lsdDCD != 0.0) radC6 = -lsdDCD;

        frictionEdit.SetValue(sdiIdentifier("FRIC"),sdiValue(lsdFDD));
        frictionEdit.SetValue(sdiIdentifier("C5"),sdiValue(radC5));
        frictionEdit.SetValue(sdiIdentifier("C6"),sdiValue(radC6));
        frictionEdit.SetValue(sdiIdentifier("VIS_f"),sdiValue(lsdVCD));

        int N;
        tempValue = sdiValue(N);
        selFriction->GetValue(sdiIdentifier("N"), tempValue);
        tempValue.GetValue(N);
        frictionEdit.SetValue(sdiIdentifier("N"),sdiValue(N));

        // N - entries

        for (int i = 0; i < N; i++)
        {
          double lsdFSij;
          tempValue = sdiValue(lsdFSij);
          selFriction->GetValue(sdiIdentifier("FS_ij",0,i), tempValue);
          tempValue.GetValue(lsdFSij);

          double lsdFDij;
          tempValue = sdiValue(lsdFDij);
          selFriction->GetValue(sdiIdentifier("FD_ij",0,i), tempValue);
          tempValue.GetValue(lsdFDij);

          double lsdDCij;
          tempValue = sdiValue(lsdDCij);
          selFriction->GetValue(sdiIdentifier("DC_ij",0,i), tempValue);
          tempValue.GetValue(lsdDCij);

          double lsdVCij;
          tempValue = sdiValue(lsdVCij);
          selFriction->GetValue(sdiIdentifier("VC_ij",0,i), tempValue);
          tempValue.GetValue(lsdVCij);

          double radC5ij=0.0, radC6ij=0.0;
          radC5ij = lsdFSij-lsdFDij;
          if (lsdDCij != 0.0) radC6ij = -lsdDCij;


          frictionEdit.SetValue(sdiIdentifier("fric_part",0,i),sdiValue(lsdFDij));
          frictionEdit.SetValue(sdiIdentifier("c5_part",0,i),sdiValue(radC5ij));
          frictionEdit.SetValue(sdiIdentifier("c6_part",0,i),sdiValue(radC6ij));
          frictionEdit.SetValue(sdiIdentifier("vis_f_part",0,i),sdiValue(lsdVCij));

          int lsdptypei;
          tempValue = sdiValue(lsdptypei);
          selFriction->GetValue(sdiIdentifier("ptype_i",0,i), tempValue);
          tempValue.GetValue(lsdptypei);

          int lsdptypej;
          tempValue = sdiValue(lsdptypej);
          selFriction->GetValue(sdiIdentifier("ptype_j",0,i), tempValue);
          tempValue.GetValue(lsdptypej);

          // SET_ID or Part_ID

          int PIDiId = 0;
          sdiValueEntity PIDiEntity;
          if(lsdptypei == 1)
          {
            selFriction->GetValue(sdiIdentifier("grpart_ID1",0,i), tempValue);
            tempValue.GetValue(PIDiEntity);
            PIDiId = PIDiEntity.GetId();
            frictionEdit.SetValue(sdiIdentifier("grpart_ID1",0,i), sdiValue(sdiValueEntity(radSetType, PIDiId)));
          }
          else
          {
            selFriction->GetValue(sdiIdentifier("part_ID1",0,i), tempValue);
            tempValue.GetValue(PIDiEntity);
            PIDiId = PIDiEntity.GetId();
            frictionEdit.SetValue(sdiIdentifier("part_ID1",0,i), sdiValue(sdiValueEntity(radPartType, PIDiId)));
          }

          int PIDjId = 0;
          sdiValueEntity PIDjEntity;
          if(lsdptypej == 1)
          {
            selFriction->GetValue(sdiIdentifier("grpart_ID2",0,i), tempValue);
            tempValue.GetValue(PIDjEntity);
            PIDjId = PIDjEntity.GetId();
            frictionEdit.SetValue(sdiIdentifier("grpart_ID2",0,i), sdiValue(sdiValueEntity(radSetType, PIDjId)));
          }
          else
          {
            selFriction->GetValue(sdiIdentifier("part_ID2",0,i), tempValue);
            tempValue.GetValue(PIDjEntity);
            PIDjId = PIDjEntity.GetId();
            frictionEdit.SetValue(sdiIdentifier("part_ID2",0,i), sdiValue(sdiValueEntity(radPartType, PIDjId)));
          }
        } // for (int i = 0; i < N; i++)

      sdiConvert::SDIHandlReadList sourcehandleList = { {selFriction->GetHandle()} };
      sdiConvert::Convert::PushToConversionLog(std::make_pair(frictionHEdit, sourcehandleList));
      } // if (frictionHEdit.IsValid())
   } // while
}
