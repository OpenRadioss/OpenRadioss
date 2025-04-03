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

#include <dyna2rad/convertinitialstresses.h>
#include <dyna2rad/dyna2rad.h>
#include <typedef.h>
#include <dyna2rad/convertelements.h>

using namespace sdi;
using namespace std;

void sdiD2R::ConvertInitialStress::ConvertAllInitialStresses()
{
    ConvertEntities();
}

void sdiD2R::ConvertInitialStress::ConvertEntities()
{
    ConvertInitialStressesShell();

    ConvertInitialStressesSolid();
}

void sdiD2R::ConvertInitialStress::ConvertInitialStressesShell()
{
    SelectionRead selIniStressShell(p_lsdynaModel, "*INITIAL_STRESS_SHELL");

    EntityType radShellType = p_radiossModel->GetEntityType("/SHELL");

    EntityType radSh3nType = p_radiossModel->GetEntityType("/SH3N");


    while (selIniStressShell.Next())
    {
        // nb of shell + sh3n (inside *INITIAL_STRESS_SHELL)
        int nbSh3n =0;
        int nbShell =0;

        sdiValue tempValue;
        int nrows = 0;
        tempValue =sdiValue(nrows);
        selIniStressShell->GetValue(sdiIdentifier("nrows"), tempValue);
        tempValue.GetValue(nrows);

        for (int i = 0; i < nrows; ++i)
        {
          sdiValueEntity eidId;
          tempValue = sdiValue(eidId);
          selIniStressShell->GetValue(sdiIdentifier("eid",0,i), tempValue);
          tempValue.GetValue(eidId);
          unsigned int eid = eidId.GetId();

          HandleRead elementsh4HRead;
          p_radiossModel->FindById(p_radiossModel->GetEntityType("/SHELL"), eid, elementsh4HRead);
          if ( elementsh4HRead.IsValid()) nbShell++;
          HandleRead elementsh3HRead;
          p_radiossModel->FindById(p_radiossModel->GetEntityType("/SH3N"), eid, elementsh3HRead);
          if ( elementsh3HRead.IsValid()) nbSh3n++;
        }

        HandleEdit inistressHEdit;
        HandleEdit inistress3nHEdit;
        if (nbShell > 0) p_radiossModel->CreateEntity(inistressHEdit, "/INISHE/STRS_F/GLOB");
        if (nbSh3n > 0)  p_radiossModel->CreateEntity(inistress3nHEdit, "/INISH3/STRS_F/GLOB");
        //--------------------------------------
        // ***********  SHELL 4N  **************
        //--------------------------------------

        if(inistressHEdit.IsValid())
        {
          // all staff shell 4N ...
          EntityEdit inistressEdit(p_radiossModel, inistressHEdit);
          inistressEdit.SetValue(sdiIdentifier("inishe_strs_f_glob_count"), sdiValue(nbShell));
          //
          int cnt = 0;
          int cntDyna = 0;
          for (int i = 0; i < nrows; ++i)
          {
            sdiValueEntity eidId;
            tempValue = sdiValue(eidId);
            selIniStressShell->GetValue(sdiIdentifier("eid",0,i), tempValue);
            tempValue.GetValue(eidId);
            unsigned int eid = eidId.GetId();
            HandleRead elementHRead;
            p_radiossModel->FindById(p_radiossModel->GetEntityType("/SHELL"), eid, elementHRead);
            if ( elementHRead.IsValid())
            {
              inistressEdit.SetValue(sdiIdentifier("shell_ID",0,cnt),sdiValue(sdiValueEntity(radShellType, eid)));
              // get prop_id (p_radiossModel) only for shell 4N  in order to get Ishell formulation
              int Ishellform = 0;
              HandleRead partHRead;
              elementHRead.GetEntityHandle(p_radiossModel, sdiIdentifier("part_ID"), partHRead);

              if (partHRead.IsValid())
              {
                HandleRead propHRead;
                partHRead.GetEntityHandle(p_radiossModel, sdiIdentifier("prop_ID"), propHRead);

                EntityRead propEntityRead(p_radiossModel, propHRead);
                tempValue = sdiValue(Ishellform);
                propEntityRead.GetValue(sdiIdentifier("Ishell"), tempValue);
                tempValue.GetValue(Ishellform);
              }
              //
              // Element values in inicard
              // ----------------------------
              int lsdNPLANE_ini = 0;
              int lsdNPLANE = 0;
              tempValue = sdiValue(lsdNPLANE);
              selIniStressShell->GetValue(sdiIdentifier("ngaussianpoints",0,cntDyna), tempValue);
              tempValue.GetValue(lsdNPLANE);

              lsdNPLANE_ini = lsdNPLANE;
              if (Ishellform == 24) lsdNPLANE = 4;

              inistressEdit.SetValue(sdiIdentifier("npg",0,cnt), sdiValue(lsdNPLANE));

              int lsdNTHICK = 0;
              tempValue = sdiValue(lsdNTHICK);
              selIniStressShell->GetValue(sdiIdentifier("nintegrationpoints",0,cntDyna), tempValue);
              tempValue.GetValue(lsdNTHICK);

              inistressEdit.SetValue(sdiIdentifier("nb_integr",0,cnt), sdiValue(lsdNTHICK));

              int totintpoints = 0;
              tempValue = sdiValue(totintpoints);
              selIniStressShell->GetValue(sdiIdentifier("tot_nintegrationpoints",0,cntDyna), tempValue);
              tempValue.GetValue(totintpoints);

              inistressEdit.SetValue(sdiIdentifier("nb_integr2",0,cnt), sdiValue(lsdNTHICK*lsdNPLANE));

              inistressEdit.SetValue(sdiIdentifier("Thick",0,cnt), sdiValue(0.0));
              inistressEdit.SetValue(sdiIdentifier("Em",0,cnt), sdiValue(0.0));
              inistressEdit.SetValue(sdiIdentifier("Eb",0,cnt), sdiValue(0.0));
              inistressEdit.SetValue(sdiIdentifier("H1",0,cnt), sdiValue(0.0));
              inistressEdit.SetValue(sdiIdentifier("H2",0,cnt), sdiValue(0.0));
              inistressEdit.SetValue(sdiIdentifier("H3",0,cnt), sdiValue(0.0));

              // Loop over integration points
              // ----------------------------

              int countinteg = 0;
              int count_gauss = 0;

              for (int j = 0; j < totintpoints; ++j)
              {
                double sigxx = 0.0;
                tempValue = sdiValue(sigxx);
                selIniStressShell->GetValue(sdiIdentifier("xx",0,cntDyna,j), tempValue);
                tempValue.GetValue(sigxx);

                double sigyy = 0.0;
                tempValue = sdiValue(sigyy);
                selIniStressShell->GetValue(sdiIdentifier("yy",0,cntDyna,j), tempValue);
                tempValue.GetValue(sigyy);

                double sigzz = 0.0;
                tempValue = sdiValue(sigzz);
                selIniStressShell->GetValue(sdiIdentifier("zz",0,cntDyna,j), tempValue);
                tempValue.GetValue(sigzz);

                double sigxy = 0.0;
                tempValue = sdiValue(sigxy);
                selIniStressShell->GetValue(sdiIdentifier("xy",0,cntDyna,j), tempValue);
                tempValue.GetValue(sigxy);

                double sigyz = 0.0;
                tempValue = sdiValue(sigyz);
                selIniStressShell->GetValue(sdiIdentifier("yz",0,cntDyna,j), tempValue);
                tempValue.GetValue(sigyz);

                double sigzx = 0.0;
                tempValue = sdiValue(sigzx);
                selIniStressShell->GetValue(sdiIdentifier("zx",0,cntDyna,j), tempValue);
                tempValue.GetValue(sigzx);

                double eps = 0.0;
                tempValue = sdiValue(eps);
                selIniStressShell->GetValue(sdiIdentifier("eps",0,cntDyna,j), tempValue);
                tempValue.GetValue(eps);

                double t_pos = 0.0;
                tempValue = sdiValue(t_pos);
                selIniStressShell->GetValue(sdiIdentifier("t",0,cntDyna,j), tempValue);
                tempValue.GetValue(t_pos);

                // if ISHELL == 24 (radioss converted) -> write data for NPG=4 even if NPG=1 (Dyna)

                if (Ishellform == 24 && lsdNPLANE_ini == 1)
                {
                inistressEdit.SetValue(sdiIdentifier("sigma_X",0,cnt,j+countinteg), sdiValue(sigxx));
                inistressEdit.SetValue(sdiIdentifier("sigma_X",0,cnt,j+countinteg+1), sdiValue(sigxx));
                inistressEdit.SetValue(sdiIdentifier("sigma_X",0,cnt,j+countinteg+2), sdiValue(sigxx));
                inistressEdit.SetValue(sdiIdentifier("sigma_X",0,cnt,j+countinteg+3), sdiValue(sigxx));
                //
                inistressEdit.SetValue(sdiIdentifier("sigma_Y",0,cnt,j+countinteg), sdiValue(sigyy));
                inistressEdit.SetValue(sdiIdentifier("sigma_Y",0,cnt,j+countinteg+1), sdiValue(sigyy));
                inistressEdit.SetValue(sdiIdentifier("sigma_Y",0,cnt,j+countinteg+2), sdiValue(sigyy));
                inistressEdit.SetValue(sdiIdentifier("sigma_Y",0,cnt,j+countinteg+3), sdiValue(sigyy));
                //
                inistressEdit.SetValue(sdiIdentifier("sigma_Z",0,cnt,j+countinteg), sdiValue(sigzz));
                inistressEdit.SetValue(sdiIdentifier("sigma_Z",0,cnt,j+countinteg+1), sdiValue(sigzz));
                inistressEdit.SetValue(sdiIdentifier("sigma_Z",0,cnt,j+countinteg+2), sdiValue(sigzz));
                inistressEdit.SetValue(sdiIdentifier("sigma_Z",0,cnt,j+countinteg+3), sdiValue(sigzz));
                //
                inistressEdit.SetValue(sdiIdentifier("sigma_XY",0,cnt,j+countinteg), sdiValue(sigxy));
                inistressEdit.SetValue(sdiIdentifier("sigma_XY",0,cnt,j+countinteg+1), sdiValue(sigxy));
                inistressEdit.SetValue(sdiIdentifier("sigma_XY",0,cnt,j+countinteg+2), sdiValue(sigxy));
                inistressEdit.SetValue(sdiIdentifier("sigma_XY",0,cnt,j+countinteg+3), sdiValue(sigxy));
                //
                inistressEdit.SetValue(sdiIdentifier("sigma_YZ",0,cnt,j+countinteg), sdiValue(sigyz));
                inistressEdit.SetValue(sdiIdentifier("sigma_YZ",0,cnt,j+countinteg+1), sdiValue(sigyz));
                inistressEdit.SetValue(sdiIdentifier("sigma_YZ",0,cnt,j+countinteg+2), sdiValue(sigyz));
                inistressEdit.SetValue(sdiIdentifier("sigma_YZ",0,cnt,j+countinteg+3), sdiValue(sigyz));
                //
                inistressEdit.SetValue(sdiIdentifier("sigma_ZX",0,cnt,j+countinteg), sdiValue(sigzx));
                inistressEdit.SetValue(sdiIdentifier("sigma_ZX",0,cnt,j+countinteg+1), sdiValue(sigzx));
                inistressEdit.SetValue(sdiIdentifier("sigma_ZX",0,cnt,j+countinteg+2), sdiValue(sigzx));
                inistressEdit.SetValue(sdiIdentifier("sigma_ZX",0,cnt,j+countinteg+3), sdiValue(sigzx));
                //
                inistressEdit.SetValue(sdiIdentifier("eps_p",0,cnt,j+countinteg), sdiValue(eps));
                inistressEdit.SetValue(sdiIdentifier("eps_p",0,cnt,j+countinteg+1), sdiValue(eps));
                inistressEdit.SetValue(sdiIdentifier("eps_p",0,cnt,j+countinteg+2), sdiValue(eps));
                inistressEdit.SetValue(sdiIdentifier("eps_p",0,cnt,j+countinteg+3), sdiValue(eps));
                //
                inistressEdit.SetValue(sdiIdentifier("pos_nip",0,cnt,j+countinteg), sdiValue(t_pos));
                inistressEdit.SetValue(sdiIdentifier("pos_nip",0,cnt,j+countinteg+1), sdiValue(t_pos));
                inistressEdit.SetValue(sdiIdentifier("pos_nip",0,cnt,j+countinteg+2), sdiValue(t_pos));
                inistressEdit.SetValue(sdiIdentifier("pos_nip",0,cnt,j+countinteg+3), sdiValue(t_pos));
                //
                countinteg = countinteg + 3;
                }
                else
                {
                inistressEdit.SetValue(sdiIdentifier("sigma_X",0,cnt,j), sdiValue(sigxx));
                inistressEdit.SetValue(sdiIdentifier("sigma_Y",0,cnt,j), sdiValue(sigyy));
                inistressEdit.SetValue(sdiIdentifier("sigma_Z",0,cnt,j), sdiValue(sigzz));
                inistressEdit.SetValue(sdiIdentifier("sigma_XY",0,cnt,j), sdiValue(sigxy));
                inistressEdit.SetValue(sdiIdentifier("sigma_YZ",0,cnt,j), sdiValue(sigyz));
                inistressEdit.SetValue(sdiIdentifier("sigma_ZX",0,cnt,j), sdiValue(sigzx));
                inistressEdit.SetValue(sdiIdentifier("eps_p",0,cnt,j), sdiValue(eps));
                inistressEdit.SetValue(sdiIdentifier("pos_nip",0,cnt,j), sdiValue(t_pos));
                }
              } // for (int j = 0; j < totintpoints; ++j)
              //--------------------
              sdiConvert::SDIHandlReadList sourceInitialStressesShell = { {selIniStressShell->GetHandle()} };
              sdiConvert::Convert::PushToConversionLog(std::make_pair(inistressHEdit, sourceInitialStressesShell));
              //--------------------
              cnt = cnt + 1;
            } // if ( elementHRead.IsValid())
            cntDyna = cntDyna + 1;
          } // end for
        } // if(inistressHEdit.IsValid())


        //--------------------------------------
        // ***********   SH3N 3N  **************
        //--------------------------------------

        if(inistress3nHEdit.IsValid())
        {
          // all staff sh3N  ...
          EntityEdit inistress3nEdit(p_radiossModel, inistress3nHEdit);
          inistress3nEdit.SetValue(sdiIdentifier("inish3_strs_f_glob_count"), sdiValue(nbSh3n));
          int cnt = 0;
          int cntDyna = 0;
          for (int i = 0; i < nrows; ++i)
          {
            sdiValueEntity eidId;
            tempValue = sdiValue(eidId);
            selIniStressShell->GetValue(sdiIdentifier("eid",0,i), tempValue);
            tempValue.GetValue(eidId);
            unsigned int eid = eidId.GetId();
            HandleRead elementHRead;
            p_radiossModel->FindById(p_radiossModel->GetEntityType("/SH3N"), eid, elementHRead);
            if ( elementHRead.IsValid())
            {
              inistress3nEdit.SetValue(sdiIdentifier("shell_ID",0,cnt),sdiValue(sdiValueEntity(radSh3nType, eid)));

              // Element values in inicard
              // ----------------------------
              int lsdNPLANE_ini = 0;
              int lsdNPLANE = 0;
              tempValue = sdiValue(lsdNPLANE);
              selIniStressShell->GetValue(sdiIdentifier("ngaussianpoints",0,cntDyna), tempValue);
              tempValue.GetValue(lsdNPLANE);

              lsdNPLANE_ini = lsdNPLANE;
              if (lsdNPLANE == 4) lsdNPLANE = 1;

              inistress3nEdit.SetValue(sdiIdentifier("npg",0,cnt), sdiValue(lsdNPLANE));

              int lsdNTHICK = 0;
              tempValue = sdiValue(lsdNTHICK);
              selIniStressShell->GetValue(sdiIdentifier("nintegrationpoints",0,cntDyna), tempValue);
              tempValue.GetValue(lsdNTHICK);

              inistress3nEdit.SetValue(sdiIdentifier("nb_integr",0,cnt), sdiValue(lsdNTHICK));

              int totintpoints = 0;
              tempValue = sdiValue(totintpoints);
              selIniStressShell->GetValue(sdiIdentifier("tot_nintegrationpoints",0,cntDyna), tempValue);
              tempValue.GetValue(totintpoints);

              inistress3nEdit.SetValue(sdiIdentifier("nb_integr2",0,cnt), sdiValue(lsdNTHICK*lsdNPLANE));

              inistress3nEdit.SetValue(sdiIdentifier("Thick",0,cnt), sdiValue(0.0));
              inistress3nEdit.SetValue(sdiIdentifier("Em",0,cnt), sdiValue(0.0));
              inistress3nEdit.SetValue(sdiIdentifier("Eb",0,cnt), sdiValue(0.0));
              inistress3nEdit.SetValue(sdiIdentifier("H1",0,cnt), sdiValue(0.0));
              inistress3nEdit.SetValue(sdiIdentifier("H2",0,cnt), sdiValue(0.0));
              inistress3nEdit.SetValue(sdiIdentifier("H3",0,cnt), sdiValue(0.0));

              // Loop over integration points
              // ----------------------------

              int countinteg = 3;
              int count_gauss = 0;
              double mean_sigxx = 0.0;
              double mean_sigyy = 0.0;
              double mean_sigzz = 0.0;
              double mean_sigxy = 0.0;
              double mean_sigyz = 0.0;
              double mean_sigzx = 0.0;
              double mean_eps = 0.0;


              for (int j = 0; j < totintpoints; ++j)
              {
                double sigxx = 0.0;
                tempValue = sdiValue(sigxx);
                selIniStressShell->GetValue(sdiIdentifier("xx",0,cntDyna,j), tempValue);
                tempValue.GetValue(sigxx);

                double sigyy = 0.0;
                tempValue = sdiValue(sigyy);
                selIniStressShell->GetValue(sdiIdentifier("yy",0,cntDyna,j), tempValue);
                tempValue.GetValue(sigyy);

                double sigzz = 0.0;
                tempValue = sdiValue(sigzz);
                selIniStressShell->GetValue(sdiIdentifier("zz",0,cntDyna,j), tempValue);
                tempValue.GetValue(sigzz);

                double sigxy = 0.0;
                tempValue = sdiValue(sigxy);
                selIniStressShell->GetValue(sdiIdentifier("xy",0,cntDyna,j), tempValue);
                tempValue.GetValue(sigxy);

                double sigyz = 0.0;
                tempValue = sdiValue(sigyz);
                selIniStressShell->GetValue(sdiIdentifier("yz",0,cntDyna,j), tempValue);
                tempValue.GetValue(sigyz);

                double sigzx = 0.0;
                tempValue = sdiValue(sigzx);
                selIniStressShell->GetValue(sdiIdentifier("zx",0,cntDyna,j), tempValue);
                tempValue.GetValue(sigzx);

                double eps = 0.0;
                tempValue = sdiValue(eps);
                selIniStressShell->GetValue(sdiIdentifier("eps",0,cntDyna,j), tempValue);
                tempValue.GetValue(eps);

                double t_pos = 0.0;
                tempValue = sdiValue(t_pos);
                selIniStressShell->GetValue(sdiIdentifier("t",0,cntDyna,j), tempValue);
                tempValue.GetValue(t_pos);

                if (lsdNPLANE_ini == 4)
                {
                  // compute the mean value over initial Gauss points 
                  // and write this mean value in one Gauss points for Radioss
                  mean_sigxx = mean_sigxx + sigxx/4.0;
                  mean_sigyy = mean_sigyy + sigyy/4.0;
                  mean_sigzz = mean_sigzz + sigzz/4.0;
                  mean_sigxy = mean_sigxy + sigxy/4.0;
                  mean_sigyz = mean_sigyz + sigyz/4.0;
                  mean_sigzx = mean_sigzx + sigzx/4.0;
                  mean_eps = mean_eps + eps/4.0;
                  if (j == countinteg)
                  {
                    sigxx = mean_sigxx;
                    inistress3nEdit.SetValue(sdiIdentifier("sigma_X",0,cnt,count_gauss), sdiValue(sigxx));
                    sigyy = mean_sigyy;
                    inistress3nEdit.SetValue(sdiIdentifier("sigma_Y",0,cnt,count_gauss), sdiValue(sigyy));
                    sigzz = mean_sigzz;
                    inistress3nEdit.SetValue(sdiIdentifier("sigma_Z",0,cnt,count_gauss), sdiValue(sigzz));
                    sigxy = mean_sigxy;
                    inistress3nEdit.SetValue(sdiIdentifier("sigma_XY",0,cnt,count_gauss), sdiValue(sigxy));
                    sigyz = mean_sigyz;
                    inistress3nEdit.SetValue(sdiIdentifier("sigma_YZ",0,cnt,count_gauss), sdiValue(sigyz));
                    sigzx = mean_sigzx;
                    inistress3nEdit.SetValue(sdiIdentifier("sigma_ZX",0,cnt,count_gauss), sdiValue(sigzx));
                    eps = mean_eps;
                    inistress3nEdit.SetValue(sdiIdentifier("eps_p",0,cnt,count_gauss), sdiValue(eps));
                    count_gauss = count_gauss + 1;
                    countinteg = countinteg + 4;
                  }
                }
                else
                {
                inistress3nEdit.SetValue(sdiIdentifier("sigma_X",0,cnt,j), sdiValue(sigxx));
                inistress3nEdit.SetValue(sdiIdentifier("sigma_Y",0,cnt,j), sdiValue(sigyy));
                inistress3nEdit.SetValue(sdiIdentifier("sigma_Z",0,cnt,j), sdiValue(sigzz));
                inistress3nEdit.SetValue(sdiIdentifier("sigma_XY",0,cnt,j), sdiValue(sigxy));
                inistress3nEdit.SetValue(sdiIdentifier("sigma_YZ",0,cnt,j), sdiValue(sigyz));
                inistress3nEdit.SetValue(sdiIdentifier("sigma_ZX",0,cnt,j), sdiValue(sigzx));
                inistress3nEdit.SetValue(sdiIdentifier("eps_p",0,cnt,j), sdiValue(eps));
                inistress3nEdit.SetValue(sdiIdentifier("pos_nip",0,cnt,j), sdiValue(t_pos));
                }
              } // for (int j = 0; j < totintpoints; ++j)
              //--------------------
              sdiConvert::SDIHandlReadList sourceInitialStressesShell = { {selIniStressShell->GetHandle()} };
              sdiConvert::Convert::PushToConversionLog(std::make_pair(inistress3nHEdit, sourceInitialStressesShell));
              //--------------------
              cnt = cnt + 1;
            } // if ( elementHRead.IsValid())
            cntDyna = cntDyna + 1;
          } // end for
        } // if(inistress3nHEdit.IsValid())
    //--------------------------------------------
    } // while
}

void sdiD2R::ConvertInitialStress::ConvertInitialStressesSolid()
{
    SelectionRead selIniStressSolid(p_lsdynaModel, "*INITIAL_STRESS_SOLID");

    EntityType radSolidType = p_radiossModel->GetEntityType("/BRICK");

    while (selIniStressSolid.Next())
    {
        // nb of solids (inside *INITIAL_STRESS_SOLID)
        int nbSolid =0;

        sdiValue tempValue;
        int nrows = 0;
        tempValue =sdiValue(nrows);
        selIniStressSolid->GetValue(sdiIdentifier("nrows"), tempValue);
        tempValue.GetValue(nrows);

        for (int i = 0; i < nrows; ++i)
        {
          sdiValueEntity eidId;
          tempValue = sdiValue(eidId);
          selIniStressSolid->GetValue(sdiIdentifier("eid",0,i), tempValue);
          tempValue.GetValue(eidId);
          unsigned int eid = eidId.GetId();

          HandleRead elementHRead;
          p_radiossModel->FindById(radSolidType, eid, elementHRead);
          if ( elementHRead.IsValid() ) nbSolid++;
        }

        HandleEdit inistressHEdit;
        if (nbSolid > 0) p_radiossModel->CreateEntity(inistressHEdit, "/INIBRI/STRS_FGLO");

        //--------------------------------------
        // ***********  SOLID  *****************
        //--------------------------------------

        if(inistressHEdit.IsValid())
        {
          // solid elements
          EntityEdit inistressEdit(p_radiossModel, inistressHEdit);
          inistressEdit.SetValue(sdiIdentifier("inibri_strs_fglo_count"), sdiValue(nbSolid));

          int cnt = 0;
          int cntDyna = 0;
          for (int i = 0; i < nrows; ++i)
          {
            sdiValueEntity eidId;
            tempValue = sdiValue(eidId);
            selIniStressSolid->GetValue(sdiIdentifier("eid",0,i), tempValue);
            tempValue.GetValue(eidId);
            unsigned int eid = eidId.GetId();

            HandleRead elementHRead;
            p_radiossModel->FindById(radSolidType, eid, elementHRead);
            if ( elementHRead.IsValid())
            {
              inistressEdit.SetValue(sdiIdentifier("brick_ID",0,cnt),sdiValue(sdiValueEntity(radSolidType, eid)));
              // get prop_id (p_radiossModel) Isolid formulation
              int Isolid = 0;
              HandleRead partHRead;
              elementHRead.GetEntityHandle(p_radiossModel, sdiIdentifier("part_ID"), partHRead);

              if (partHRead.IsValid())
              {
                HandleRead propHRead;
                partHRead.GetEntityHandle(p_radiossModel, sdiIdentifier("prop_ID"), propHRead);

                EntityRead propEntityRead(p_radiossModel, propHRead);
                tempValue = sdiValue(Isolid);
                propEntityRead.GetValue(sdiIdentifier("Isolid"), tempValue);
                tempValue.GetValue(Isolid);
              }

              // ----------------------------
              // Element values in inicard
              // ----------------------------

              int totintpoints = 0;
              tempValue = sdiValue(totintpoints);
              selIniStressSolid->GetValue(sdiIdentifier("NINT",0,cntDyna), tempValue);
              tempValue.GetValue(totintpoints);

              inistressEdit.SetValue(sdiIdentifier("Isolid",0,cnt), sdiValue(Isolid));
              inistressEdit.SetValue(sdiIdentifier("Isolnod",0,cnt), sdiValue(8));

              // ----------------------------
              // Loop over integration points
              // ----------------------------

              if ( Isolid == 1 || Isolid == 2 || Isolid == 5 || Isolid == 24 )
              {
                // reduced integration (one integration point)
                if(totintpoints == 1)
                {
                    inistressEdit.SetValue(sdiIdentifier("nptr",0,cnt), sdiValue(1));
                    inistressEdit.SetValue(sdiIdentifier("npts",0,cnt), sdiValue(1));
                    inistressEdit.SetValue(sdiIdentifier("nptt",0,cnt), sdiValue(1));
                    inistressEdit.SetValue(sdiIdentifier("Nb_integr",0,cnt), sdiValue(1));

                    double lsdSIGXX = 0;
                    tempValue = sdiValue(lsdSIGXX);
                    selIniStressSolid->GetValue(sdiIdentifier("SIGXX",0,cntDyna), tempValue);
                    tempValue.GetValue(lsdSIGXX);

                    double lsdSIGYY = 0;
                    tempValue = sdiValue(lsdSIGYY);
                    selIniStressSolid->GetValue(sdiIdentifier("SIGYY",0,cntDyna), tempValue);
                    tempValue.GetValue(lsdSIGYY);

                    double lsdSIGZZ = 0;
                    tempValue = sdiValue(lsdSIGZZ);
                    selIniStressSolid->GetValue(sdiIdentifier("SIGZZ",0,cntDyna), tempValue);
                    tempValue.GetValue(lsdSIGZZ);

                    double lsdSIGXY = 0;
                    tempValue = sdiValue(lsdSIGXY);
                    selIniStressSolid->GetValue(sdiIdentifier("SIGXY",0,cntDyna), tempValue);
                    tempValue.GetValue(lsdSIGXY);

                    double lsdSIGYZ = 0;
                    tempValue = sdiValue(lsdSIGYZ);
                    selIniStressSolid->GetValue(sdiIdentifier("SIGYZ",0,cntDyna), tempValue);
                    tempValue.GetValue(lsdSIGYZ);

                    double lsdSIGZX = 0;
                    tempValue = sdiValue(lsdSIGZX);
                    selIniStressSolid->GetValue(sdiIdentifier("SIGZX",0,cntDyna), tempValue);
                    tempValue.GetValue(lsdSIGZX);

                    double lsdEPS = 0;
                    tempValue = sdiValue(lsdEPS);
                    selIniStressSolid->GetValue(sdiIdentifier("EPS",0,cntDyna), tempValue);
                    tempValue.GetValue(lsdEPS);

                    inistressEdit.SetValue(sdiIdentifier("E_int",0,cnt,0), sdiValue(0.));
                    inistressEdit.SetValue(sdiIdentifier("RHO",0,cnt,0), sdiValue(0.));
                    inistressEdit.SetValue(sdiIdentifier("SIGMA1",0,cnt,0), sdiValue(lsdSIGXX));
                    inistressEdit.SetValue(sdiIdentifier("SIGMA2",0,cnt,0), sdiValue(lsdSIGYY));
                    inistressEdit.SetValue(sdiIdentifier("SIGMA3",0,cnt,0), sdiValue(lsdSIGZZ));
                    inistressEdit.SetValue(sdiIdentifier("SIGMA12",0,cnt,0), sdiValue(lsdSIGXY));
                    inistressEdit.SetValue(sdiIdentifier("SIGMA23",0,cnt,0), sdiValue(lsdSIGYZ));
                    inistressEdit.SetValue(sdiIdentifier("SIGMA31",0,cnt,0), sdiValue(lsdSIGZX));
                    inistressEdit.SetValue(sdiIdentifier("EPSILON_p",0,cnt,0), sdiValue(lsdEPS));
                }
                else if(totintpoints > 1)
                {
                  // mean integration over points (one integration point kept)
                  double lsdmeanSIGXX = 0;
                  double lsdmeanSIGYY = 0;
                  double lsdmeanSIGZZ = 0;
                  double lsdmeanSIGXY = 0;
                  double lsdmeanSIGYZ = 0;
                  double lsdmeanSIGZX = 0;
                  double lsdmeanEPS = 0;

                  for (int j = 0; j < totintpoints; ++j)
                  {
                      double lsdSIGXX = 0;
                      tempValue = sdiValue(lsdSIGXX);
                      selIniStressSolid->GetValue(sdiIdentifier("SIGXX",0,cntDyna,j), tempValue);
                      tempValue.GetValue(lsdSIGXX);

                      double lsdSIGYY = 0;
                      tempValue = sdiValue(lsdSIGYY);
                      selIniStressSolid->GetValue(sdiIdentifier("SIGYY",0,cntDyna,j), tempValue);
                      tempValue.GetValue(lsdSIGYY);

                      double lsdSIGZZ = 0;
                      tempValue = sdiValue(lsdSIGZZ);
                      selIniStressSolid->GetValue(sdiIdentifier("SIGZZ",0,cntDyna,j), tempValue);
                      tempValue.GetValue(lsdSIGZZ);

                      double lsdSIGXY = 0;
                      tempValue = sdiValue(lsdSIGXY);
                      selIniStressSolid->GetValue(sdiIdentifier("SIGXY",0,cntDyna,j), tempValue);
                      tempValue.GetValue(lsdSIGXY);

                      double lsdSIGYZ = 0;
                      tempValue = sdiValue(lsdSIGYZ);
                      selIniStressSolid->GetValue(sdiIdentifier("SIGYZ",0,cntDyna,j), tempValue);
                      tempValue.GetValue(lsdSIGYZ);

                      double lsdSIGZX = 0;
                      tempValue = sdiValue(lsdSIGZX);
                      selIniStressSolid->GetValue(sdiIdentifier("SIGZX",0,cntDyna,j), tempValue);
                      tempValue.GetValue(lsdSIGZX);

                      double lsdEPS = 0;
                      tempValue = sdiValue(lsdEPS);
                      selIniStressSolid->GetValue(sdiIdentifier("EPS",0,cntDyna,j), tempValue);
                      tempValue.GetValue(lsdEPS);

                      lsdmeanSIGXX = lsdmeanSIGXX + lsdSIGXX/totintpoints;
                      lsdmeanSIGYY = lsdmeanSIGYY + lsdSIGYY/totintpoints;
                      lsdmeanSIGZZ = lsdmeanSIGZZ + lsdSIGZZ/totintpoints;
                      lsdmeanSIGXY = lsdmeanSIGXY + lsdSIGXY/totintpoints;
                      lsdmeanSIGYZ = lsdmeanSIGYZ + lsdSIGYZ/totintpoints;
                      lsdmeanSIGZX = lsdmeanSIGZX + lsdSIGZX/totintpoints;
                      lsdmeanEPS = lsdmeanEPS + lsdEPS/totintpoints;
                  }
                  inistressEdit.SetValue(sdiIdentifier("Nb_integr",0,cnt), sdiValue(1));
                  inistressEdit.SetValue(sdiIdentifier("E_int",0,cnt,0), sdiValue(0.));
                  inistressEdit.SetValue(sdiIdentifier("RHO",0,cnt,0), sdiValue(0.));
                  inistressEdit.SetValue(sdiIdentifier("SIGMA1",0,cnt,0), sdiValue(lsdmeanSIGXX));
                  inistressEdit.SetValue(sdiIdentifier("SIGMA2",0,cnt,0), sdiValue(lsdmeanSIGYY));
                  inistressEdit.SetValue(sdiIdentifier("SIGMA3",0,cnt,0), sdiValue(lsdmeanSIGZZ));
                  inistressEdit.SetValue(sdiIdentifier("SIGMA12",0,cnt,0), sdiValue(lsdmeanSIGXY));
                  inistressEdit.SetValue(sdiIdentifier("SIGMA23",0,cnt,0), sdiValue(lsdmeanSIGYZ));
                  inistressEdit.SetValue(sdiIdentifier("SIGMA31",0,cnt,0), sdiValue(lsdmeanSIGZX));
                  inistressEdit.SetValue(sdiIdentifier("EPSILON_p",0,cnt,0), sdiValue(lsdmeanEPS));
                }
              }
              else if( Isolid == 17 || Isolid == 18 )
              {
                // fully integrated NINT = 8 (2*2*2)

                inistressEdit.SetValue(sdiIdentifier("nptr",0,cnt), sdiValue(2));
                inistressEdit.SetValue(sdiIdentifier("npts",0,cnt), sdiValue(2));
                inistressEdit.SetValue(sdiIdentifier("nptt",0,cnt), sdiValue(2));
                inistressEdit.SetValue(sdiIdentifier("Nb_integr",0,cnt), sdiValue(totintpoints));

                for (int j = 0; j < totintpoints; ++j)
                {
                  double lsdSIGXX = 0;
                  tempValue = sdiValue(lsdSIGXX);
                  selIniStressSolid->GetValue(sdiIdentifier("SIGXX",0,cntDyna,j), tempValue);
                  tempValue.GetValue(lsdSIGXX);

                  double lsdSIGYY = 0;
                  tempValue = sdiValue(lsdSIGYY);
                  selIniStressSolid->GetValue(sdiIdentifier("SIGYY",0,cntDyna,j), tempValue);
                  tempValue.GetValue(lsdSIGYY);

                  double lsdSIGZZ = 0;
                  tempValue = sdiValue(lsdSIGZZ);
                  selIniStressSolid->GetValue(sdiIdentifier("SIGZZ",0,cntDyna,j), tempValue);
                  tempValue.GetValue(lsdSIGZZ);

                  double lsdSIGXY = 0;
                  tempValue = sdiValue(lsdSIGXY);
                  selIniStressSolid->GetValue(sdiIdentifier("SIGXY",0,cntDyna,j), tempValue);
                  tempValue.GetValue(lsdSIGXY);

                  double lsdSIGYZ = 0;
                  tempValue = sdiValue(lsdSIGYZ);
                  selIniStressSolid->GetValue(sdiIdentifier("SIGYZ",0,cntDyna,j), tempValue);
                  tempValue.GetValue(lsdSIGYZ);

                  double lsdSIGZX = 0;
                  tempValue = sdiValue(lsdSIGZX);
                  selIniStressSolid->GetValue(sdiIdentifier("SIGZX",0,cntDyna,j), tempValue);
                  tempValue.GetValue(lsdSIGZX);

                  double lsdEPS = 0;
                  tempValue = sdiValue(lsdEPS);
                  selIniStressSolid->GetValue(sdiIdentifier("EPS",0,cntDyna,j), tempValue);
                  tempValue.GetValue(lsdEPS);

                  inistressEdit.SetValue(sdiIdentifier("E_int",0,cnt,j), sdiValue(0.));
                  inistressEdit.SetValue(sdiIdentifier("RHO",0,cnt,j), sdiValue(0.));
                  inistressEdit.SetValue(sdiIdentifier("SIGMA1",0,cnt,j), sdiValue(lsdSIGXX));
                  inistressEdit.SetValue(sdiIdentifier("SIGMA2",0,cnt,j), sdiValue(lsdSIGYY));
                  inistressEdit.SetValue(sdiIdentifier("SIGMA3",0,cnt,j), sdiValue(lsdSIGZZ));
                  inistressEdit.SetValue(sdiIdentifier("SIGMA12",0,cnt,j), sdiValue(lsdSIGXY));
                  inistressEdit.SetValue(sdiIdentifier("SIGMA23",0,cnt,j), sdiValue(lsdSIGYZ));
                  inistressEdit.SetValue(sdiIdentifier("SIGMA31",0,cnt,j), sdiValue(lsdSIGZX));
                  inistressEdit.SetValue(sdiIdentifier("EPSILON_p",0,cnt,j), sdiValue(lsdEPS));
                }
              }
              //--------------------
              sdiConvert::SDIHandlReadList sourceInitialStressesSolid = { {selIniStressSolid->GetHandle()} };
              sdiConvert::Convert::PushToConversionLog(std::make_pair(inistressHEdit, sourceInitialStressesSolid));
              //--------------------
              cnt = cnt + 1;
            } // if ( elementHRead.IsValid())
            cntDyna = cntDyna + 1;
          } // for (int i = 0; i < nrows; ++i)
        } // if(inistressHEdit.IsValid())
    //--------------------------------------------
    } // while
}