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

#include <dyna2rad/convertinitialstrains.h>
#include <dyna2rad/dyna2rad.h>
#include <typedef.h>
#include <dyna2rad/convertelements.h>

using namespace sdi;
using namespace std;

void sdiD2R::ConvertInitialStrain::ConvertAllInitialStrains()
{
    ConvertEntities();
}

void sdiD2R::ConvertInitialStrain::ConvertEntities()
{
    ConvertInitialStrainsShell();
}

void sdiD2R::ConvertInitialStrain::ConvertInitialStrainsShell()
{
    SelectionRead selIniStrainShell(p_lsdynaModel, "*INITIAL_STRAIN_SHELL");

    EntityType radShellType = p_radiossModel->GetEntityType("/SHELL");

    EntityType radSh3nType = p_radiossModel->GetEntityType("/SH3N");

    while (selIniStrainShell.Next())
    {
        // nb of shell + sh3n (inside *INITIAL_STRAIN_SHELL)
        int nbSh3n =0;
        int nbShell =0;

        sdiValue tempValue;

        int nrows = GetValue<int>(*selIniStrainShell, "nrows");

        sdiValueEntity eidId;

        for (int i = 0; i < nrows; ++i)
        {
          tempValue = sdiValue(eidId);
          selIniStrainShell->GetValue(sdiIdentifier("eid",0,i), tempValue);
          tempValue.GetValue(eidId);
          unsigned int eid = eidId.GetId();

          HandleRead elementsh4HRead;
          p_radiossModel->FindById(p_radiossModel->GetEntityType("/SHELL"), eid, elementsh4HRead);
          if ( elementsh4HRead.IsValid()) nbShell++;
          HandleRead elementsh3HRead;
          p_radiossModel->FindById(p_radiossModel->GetEntityType("/SH3N"), eid, elementsh3HRead);
          if ( elementsh3HRead.IsValid()) nbSh3n++;

        }

        HandleEdit inistrainHEdit;
        HandleEdit inistrain3nHEdit;
        if (nbShell > 0) p_radiossModel->CreateEntity(inistrainHEdit, "/INISHE/STRA_F/GLOB");
        if (nbSh3n > 0)  p_radiossModel->CreateEntity(inistrain3nHEdit, "/INISH3/STRA_F/GLOB");

        //--------------------------------------
        // ***********  SHELL 4N  **************
        //--------------------------------------

        if(inistrainHEdit.IsValid())
        {
          //  shell 4N ...
          EntityEdit inistrainEdit(p_radiossModel, inistrainHEdit);
          inistrainEdit.SetValue(sdiIdentifier("inishe_stra_f_glob_count"), sdiValue(nbShell));
          //
          int cnt = 0;
          int cntDyna = 0;
          for (int i = 0; i < nrows; ++i)
          {
            sdiValueEntity eidId;
            tempValue = sdiValue(eidId);
            selIniStrainShell->GetValue(sdiIdentifier("eid",0,i), tempValue);
            tempValue.GetValue(eidId);
            unsigned int eid = eidId.GetId();

            HandleRead elementHRead;
            p_radiossModel->FindById(p_radiossModel->GetEntityType("/SHELL"), eid, elementHRead);
            if ( elementHRead.IsValid())
            {
              inistrainEdit.SetValue(sdiIdentifier("shell_ID",0,cnt),sdiValue(sdiValueEntity(radShellType, eid)));
              // get prop_id (p_radiossModel) only for shell 4N  in order to get Ishell formulation
              int Ishellform = 0;  // shell formulation in radioss property
              // radioss part read
              HandleRead partHRead;
              elementHRead.GetEntityHandle(p_radiossModel, sdiIdentifier("part_ID"), partHRead);

              // dyna part read
              HandleRead elementdynaHRead;
              p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*ELEMENT_SHELL"), eid, elementdynaHRead);
              HandleRead partdynaHRead;
              elementdynaHRead.GetEntityHandle(p_lsdynaModel,sdiIdentifier("PID"), partdynaHRead);
              
              EntityRead partdynaRead(p_lsdynaModel, partdynaHRead);
              sdiString partCard = partdynaRead.GetKeyword();

              int lsdNIP = 0;
              sdiDoubleList lsdThickLayerList;
              lsdThickLayerList.reserve(lsdNIP);

              if( partCard.find("COMPOSITE") != string::npos)
              {
                  lsdNIP = GetValue<int>(partdynaRead, "Number_of_Plies");

                  tempValue = sdiValue(lsdThickLayerList);
                  partdynaHRead.GetValue(p_lsdynaModel,sdiIdentifier("THICK"), tempValue);
                  tempValue.GetValue(lsdThickLayerList);
              }
              else
              {
                  HandleRead propdynaHRead;
                  partdynaHRead.GetEntityHandle(p_lsdynaModel, sdiIdentifier("SECID"), propdynaHRead);
                  EntityRead propdynaRead(p_lsdynaModel, propdynaHRead);

                  lsdNIP = GetValue<int>(propdynaRead, "NIP");
                  double lsdThick = GetValue<double>(propdynaRead, "T1");
                  for (int j = 0; j < lsdNIP; ++j)
                  {
                      lsdThickLayerList.push_back(lsdThick/(double)lsdNIP);
                  }
              }


              int radIPOS = 0;
              if (partHRead.IsValid())
              {
                HandleRead propHRead;
                partHRead.GetEntityHandle(p_radiossModel, sdiIdentifier("prop_ID"), propHRead);

                EntityRead propEntityRead(p_radiossModel, propHRead);

                tempValue = sdiValue(Ishellform);
                propEntityRead.GetValue(sdiIdentifier("Ishell"), tempValue);
                tempValue.GetValue(Ishellform);

                radIPOS = GetValue<int>(propEntityRead, "Ipos");

                sdiString propCard = propEntityRead.GetKeyword();

              // ----------------------------
              // Element initial values in inicard
              // ----------------------------

                int lsdNPLANE = 0;
                tempValue = sdiValue(lsdNPLANE);
                selIniStrainShell->GetValue(sdiIdentifier("ngaussianpoints",0,cntDyna), tempValue);
                tempValue.GetValue(lsdNPLANE);

                int lsdNTHICK = 0;
                tempValue = sdiValue(lsdNTHICK);
                selIniStrainShell->GetValue(sdiIdentifier("nintegrationpoints",0,cntDyna), tempValue);
                tempValue.GetValue(lsdNTHICK);

                int totintpoints = 0;
                tempValue = sdiValue(totintpoints);
                selIniStrainShell->GetValue(sdiIdentifier("tot_nintegrationpoints",0,cntDyna), tempValue);
                tempValue.GetValue(totintpoints);

                // position of NIP
                sdiDoubleList ThickPosLayerList;
                ThickPosLayerList.reserve(lsdNIP);

                p_ConvertUtils.ShellThickPosLayerList(propCard, lsdNIP, radIPOS, ThickPosLayerList, lsdThickLayerList);

                //---
                if(lsdNPLANE > 0 && lsdNTHICK > 0)
                {
                    inistrainEdit.SetValue(sdiIdentifier("npg",0,cnt), sdiValue(lsdNPLANE));
                    inistrainEdit.SetValue(sdiIdentifier("nb_integr",0,cnt), sdiValue(lsdNTHICK));
                    inistrainEdit.SetValue(sdiIdentifier("Thick",0,cnt), sdiValue(0.0));
                    inistrainEdit.SetValue(sdiIdentifier("nb_integr2",0,cnt), sdiValue(totintpoints));

                    // Loop over integration points
                    // ----------------------------
                    for (int j = 0; j < totintpoints; ++j)
                    {
                        double epsxx = 0.0;
                        tempValue = sdiValue(epsxx);
                        selIniStrainShell->GetValue(sdiIdentifier("xx",0,cntDyna,j), tempValue);
                        tempValue.GetValue(epsxx);

                        double epsyy = 0.0;
                        tempValue = sdiValue(epsyy);
                        selIniStrainShell->GetValue(sdiIdentifier("yy",0,cntDyna,j), tempValue);
                        tempValue.GetValue(epsyy);

                        double epszz = 0.0;
                        tempValue = sdiValue(epszz);
                        selIniStrainShell->GetValue(sdiIdentifier("zz",0,cntDyna,j), tempValue);
                        tempValue.GetValue(epszz);

                        double epsxy = 0.0;
                        tempValue = sdiValue(epsxy);
                        selIniStrainShell->GetValue(sdiIdentifier("xy",0,cntDyna,j), tempValue);
                        tempValue.GetValue(epsxy);

                        double epsyz = 0.0;
                        tempValue = sdiValue(epsyz);
                        selIniStrainShell->GetValue(sdiIdentifier("yz",0,cntDyna,j), tempValue);
                        tempValue.GetValue(epsyz);

                        double epszx = 0.0;
                        tempValue = sdiValue(epszx);
                        selIniStrainShell->GetValue(sdiIdentifier("zx",0,cntDyna,j), tempValue);
                        tempValue.GetValue(epszx);

                        double t_pos = 0.0;
                        tempValue = sdiValue(t_pos);
                        selIniStrainShell->GetValue(sdiIdentifier("t",0,cntDyna,j), tempValue);
                        tempValue.GetValue(t_pos);

                        inistrainEdit.SetValue(sdiIdentifier("eps_XX",0,cnt,j), sdiValue(epsxx));
                        inistrainEdit.SetValue(sdiIdentifier("eps_YY",0,cnt,j), sdiValue(epsyy));
                        inistrainEdit.SetValue(sdiIdentifier("eps_ZZ",0,cnt,j), sdiValue(epszz));
                        inistrainEdit.SetValue(sdiIdentifier("eps_XY",0,cnt,j), sdiValue(epsxy));
                        inistrainEdit.SetValue(sdiIdentifier("eps_YZ",0,cnt,j), sdiValue(epsyz));
                        inistrainEdit.SetValue(sdiIdentifier("eps_ZX",0,cnt,j), sdiValue(epszx));
                        inistrainEdit.SetValue(sdiIdentifier("T",0,cnt,j), sdiValue(t_pos));
                    } // for (int j = 0; j < totintpoints; ++j)
                    cnt = cnt + 1;
                }
                else
                {
                    //---
                    // Strain tensor values should be interpolated between lower (first dyna entry) 
                    // and upper (last dyna entry) linearly.
                    // All in-plane strain tensor entries are the same
                    //---
                    // if option = _SET, or lsdNPLANE =0, or lsdNTHICK=0
                    // get "npg" and "nb_integr" from property
                    // Ishell=24 then npg=1, ishell=12 npg=4, nb_integr=NP (number of integration points)

                    if (Ishellform == 12)
                    {
                        lsdNPLANE = 4;
                        inistrainEdit.SetValue(sdiIdentifier("npg",0,cnt), sdiValue(lsdNPLANE));
                    }
                    else if(Ishellform == 24)
                    {
                        //lsdNPLANE = 4; // lsdNPLANE = 1; only for sh3n and Ishellform /= 12
                        lsdNPLANE = 1;
                        inistrainEdit.SetValue(sdiIdentifier("npg",0,cnt), sdiValue(lsdNPLANE));
                    }
                    else
                    {
                        lsdNPLANE = 1;
                        inistrainEdit.SetValue(sdiIdentifier("npg",0,cnt), sdiValue(lsdNPLANE));
                    }
                    lsdNTHICK = lsdNIP;
                    inistrainEdit.SetValue(sdiIdentifier("npg",0,cnt), sdiValue(lsdNPLANE));
                    inistrainEdit.SetValue(sdiIdentifier("nb_integr",0,cnt), sdiValue(lsdNTHICK));
                    inistrainEdit.SetValue(sdiIdentifier("Thick",0,cnt), sdiValue(0.0));
                    inistrainEdit.SetValue(sdiIdentifier("nb_integr2",0,cnt), sdiValue(lsdNTHICK));

                    // Loop over integration points
                    // ----------------------------
                    // totintpoints =2, two line input (lower,upper shell surface) (if lsdNPLANE=0 and lsdNTHICK=0)
                    
                    sdiDoubleList strainXXList;
                    strainXXList.reserve(totintpoints);
                    sdiDoubleList strainYYList;
                    strainYYList.reserve(totintpoints);
                    sdiDoubleList strainZZList;
                    strainZZList.reserve(totintpoints);
                    sdiDoubleList strainXYList;
                    strainXYList.reserve(totintpoints);
                    sdiDoubleList strainYZList;
                    strainYZList.reserve(totintpoints);
                    sdiDoubleList strainZXList;
                    strainZXList.reserve(totintpoints);

                    sdiDoubleList strainInterpXXList;
                    strainInterpXXList.reserve(lsdNIP);
                    sdiDoubleList strainInterpYYList;
                    strainInterpYYList.reserve(lsdNIP);
                    sdiDoubleList strainInterpZZList;
                    strainInterpZZList.reserve(lsdNIP);
                    sdiDoubleList strainInterpXYList;
                    strainInterpXYList.reserve(lsdNIP);
                    sdiDoubleList strainInterpYZList;
                    strainInterpYZList.reserve(lsdNIP);
                    sdiDoubleList strainInterpZXList;
                    strainInterpZXList.reserve(lsdNIP);

                    for (int j = 0; j < totintpoints; ++j)
                    {
                        double epsxx = 0.0;
                        tempValue = sdiValue(epsxx);
                        selIniStrainShell->GetValue(sdiIdentifier("xx",0,cntDyna,j), tempValue);
                        tempValue.GetValue(epsxx);
                        strainXXList.push_back(epsxx);

                        double epsyy = 0.0;
                        tempValue = sdiValue(epsyy);
                        selIniStrainShell->GetValue(sdiIdentifier("yy",0,cntDyna,j), tempValue);
                        tempValue.GetValue(epsyy);
                        strainYYList.push_back(epsyy);

                        double epszz = 0.0;
                        tempValue = sdiValue(epszz);
                        selIniStrainShell->GetValue(sdiIdentifier("zz",0,cntDyna,j), tempValue);
                        tempValue.GetValue(epszz);
                        strainZZList.push_back(epszz);

                        double epsxy = 0.0;
                        tempValue = sdiValue(epsxy);
                        selIniStrainShell->GetValue(sdiIdentifier("xy",0,cntDyna,j), tempValue);
                        tempValue.GetValue(epsxy);
                        strainXYList.push_back(epsxy);

                        double epsyz = 0.0;
                        tempValue = sdiValue(epsyz);
                        selIniStrainShell->GetValue(sdiIdentifier("yz",0,cntDyna,j), tempValue);
                        tempValue.GetValue(epsyz);
                        strainYZList.push_back(epsyz);

                        double epszx = 0.0;
                        tempValue = sdiValue(epszx);
                        selIniStrainShell->GetValue(sdiIdentifier("zx",0,cntDyna,j), tempValue);
                        tempValue.GetValue(epszx);
                        strainZXList.push_back(epszx);

                        double t_pos = 0.0;
                        tempValue = sdiValue(t_pos);
                        selIniStrainShell->GetValue(sdiIdentifier("t",0,cntDyna,j), tempValue);
                        tempValue.GetValue(t_pos);
                    } // for (int j = 0; j < totintpoints; ++j)

                    // interpolate strain between lower (first dyna entry) and upper (last dyna entry) linearly.
                    double ratio;
                    double strainintp;
                    for (int j = 0; j < lsdNIP; ++j)
                    {
                      ratio = (ThickPosLayerList[j] - ThickPosLayerList[0])/(ThickPosLayerList[lsdNIP-1] - ThickPosLayerList[0]);

                      strainintp = strainXXList[0] + ratio*(strainXXList[1]-strainXXList[0]);
                      strainInterpXXList.push_back(strainintp);

                      strainintp = strainYYList[0] + ratio*(strainYYList[1]-strainYYList[0]);
                      strainInterpYYList.push_back(strainintp);

                      strainintp = strainZZList[0] + ratio*(strainZZList[1]-strainZZList[0]);
                      strainInterpZZList.push_back(strainintp);

                      strainintp = strainXYList[0] + ratio*(strainXYList[1]-strainXYList[0]);
                      strainInterpXYList.push_back(strainintp);

                      strainintp = strainYZList[0] + ratio*(strainYZList[1]-strainYZList[0]);
                      strainInterpYZList.push_back(strainintp);

                      strainintp = strainZXList[0] + ratio*(strainZXList[1]-strainZXList[0]);
                      strainInterpZXList.push_back(strainintp);
                    }

                    for (int j = 0; j < lsdNIP; ++j)
                    {
                        inistrainEdit.SetValue(sdiIdentifier("eps_XX",0,cnt,j), sdiValue(strainInterpXXList[j]));
                        inistrainEdit.SetValue(sdiIdentifier("eps_YY",0,cnt,j), sdiValue(strainInterpYYList[j]));
                        inistrainEdit.SetValue(sdiIdentifier("eps_ZZ",0,cnt,j), sdiValue(strainInterpZZList[j]));
                        inistrainEdit.SetValue(sdiIdentifier("eps_XY",0,cnt,j), sdiValue(strainInterpXYList[j]));
                        inistrainEdit.SetValue(sdiIdentifier("eps_YZ",0,cnt,j), sdiValue(strainInterpYZList[j]));
                        inistrainEdit.SetValue(sdiIdentifier("eps_ZX",0,cnt,j), sdiValue(strainInterpZXList[j]));
                        inistrainEdit.SetValue(sdiIdentifier("T",0,cnt,j), sdiValue(ThickPosLayerList[j]));
                    }
                    cnt = cnt + 1;
                } // if(lsdNPLANE > 0 && lsdNTHICK > 0)
                //--------------------
                sdiConvert::SDIHandlReadList sourceInitialStressesShell = { {selIniStrainShell->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(inistrainHEdit, sourceInitialStressesShell));
                //--------------------
              } // if (partHRead.IsValid())
            } // if ( elementHRead.IsValid())
            cntDyna = cntDyna + 1;
          } // end for
        } // if(inistrainHEdit.IsValid())
        

        //--------------------------------------
        // ***********   SH3N 3N  **************
        //--------------------------------------

        if(inistrain3nHEdit.IsValid())
        {
          //  shell 3N ...
          EntityEdit inistrain3nEdit(p_radiossModel, inistrain3nHEdit);
          inistrain3nEdit.SetValue(sdiIdentifier("inish3_stra_f_glob_count"), sdiValue(nbSh3n));
          //
          int cnt = 0;
          int cntDyna = 0;
          for (int i = 0; i < nrows; ++i)
          {
            sdiValueEntity eidId;
            tempValue = sdiValue(eidId);
            selIniStrainShell->GetValue(sdiIdentifier("eid",0,i), tempValue);
            tempValue.GetValue(eidId);
            unsigned int eid = eidId.GetId();

            HandleRead elementHRead;
            p_radiossModel->FindById(p_radiossModel->GetEntityType("/SH3N"), eid, elementHRead);
            if ( elementHRead.IsValid())
            {
              inistrain3nEdit.SetValue(sdiIdentifier("shell_ID",0,cnt),sdiValue(sdiValueEntity(radSh3nType, eid)));
              // get prop_id (p_radiossModel) in order to get Ishell formulation
              int Ishellform = 0;  // shell formulation in radioss property
              // radioss part read
              HandleRead partHRead;
              elementHRead.GetEntityHandle(p_radiossModel, sdiIdentifier("part_ID"), partHRead);

              // dyna part read
              HandleRead elementdynaHRead;
              p_lsdynaModel->FindById(p_lsdynaModel->GetEntityType("*ELEMENT_SHELL"), eid, elementdynaHRead);
              HandleRead partdynaHRead;
              elementdynaHRead.GetEntityHandle(p_lsdynaModel,sdiIdentifier("PID"), partdynaHRead);
              
              EntityRead partdynaRead(p_lsdynaModel, partdynaHRead);
              sdiString partCard = partdynaRead.GetKeyword();

              int lsdNIP = 0;
              sdiDoubleList lsdThickLayerList;
              lsdThickLayerList.reserve(lsdNIP);

              if( partCard.find("COMPOSITE") != string::npos)
              {
                  lsdNIP = GetValue<int>(partdynaRead, "Number_of_Plies");

                  tempValue = sdiValue(lsdThickLayerList);
                  partdynaHRead.GetValue(p_lsdynaModel,sdiIdentifier("THICK"), tempValue);
                  tempValue.GetValue(lsdThickLayerList);
              }
              else
              {
                  HandleRead propdynaHRead;
                  partdynaHRead.GetEntityHandle(p_lsdynaModel, sdiIdentifier("SECID"), propdynaHRead);
                  EntityRead propdynaRead(p_lsdynaModel, propdynaHRead);

                  lsdNIP = GetValue<int>(propdynaRead, "NIP");
                  double lsdThick = GetValue<double>(propdynaRead, "T1");
                  for (int j = 0; j < lsdNIP; ++j)
                  {
                      lsdThickLayerList.push_back(lsdThick/(double)lsdNIP);
                  }
              }


              int radIPOS = 0;
              if (partHRead.IsValid())
              {
                HandleRead propHRead;
                partHRead.GetEntityHandle(p_radiossModel, sdiIdentifier("prop_ID"), propHRead);

                EntityRead propEntityRead(p_radiossModel, propHRead);

                tempValue = sdiValue(Ishellform);
                propEntityRead.GetValue(sdiIdentifier("Ishell"), tempValue);
                tempValue.GetValue(Ishellform);

                radIPOS = GetValue<int>(propEntityRead, "Ipos");

                sdiString propCard = propEntityRead.GetKeyword();

                // ----------------------------
                // Element initial values in inicard
                // ----------------------------

                int lsdNPLANE_ini = 0;
                int lsdNPLANE = 0;
                tempValue = sdiValue(lsdNPLANE);
                selIniStrainShell->GetValue(sdiIdentifier("ngaussianpoints",0,cntDyna), tempValue);
                tempValue.GetValue(lsdNPLANE);

                lsdNPLANE_ini = lsdNPLANE;
                if (lsdNPLANE == 4) lsdNPLANE = 1;

                int lsdNTHICK = 0;
                tempValue = sdiValue(lsdNTHICK);
                selIniStrainShell->GetValue(sdiIdentifier("nintegrationpoints",0,cntDyna), tempValue);
                tempValue.GetValue(lsdNTHICK);

                int totintpoints = 0;
                tempValue = sdiValue(totintpoints);
                selIniStrainShell->GetValue(sdiIdentifier("tot_nintegrationpoints",0,cntDyna), tempValue);
                tempValue.GetValue(totintpoints);

                // position of NIP
                sdiDoubleList ThickPosLayerList;
                ThickPosLayerList.reserve(lsdNIP);

                p_ConvertUtils.ShellThickPosLayerList(propCard, lsdNIP, radIPOS, ThickPosLayerList, lsdThickLayerList);

                //---
                if(lsdNPLANE > 0 && lsdNTHICK > 0)
                {
                    inistrain3nEdit.SetValue(sdiIdentifier("npg",0,cnt), sdiValue(lsdNPLANE));
                    inistrain3nEdit.SetValue(sdiIdentifier("nb_integr",0,cnt), sdiValue(lsdNTHICK));
                    inistrain3nEdit.SetValue(sdiIdentifier("Thick",0,cnt), sdiValue(0.0));
                    inistrain3nEdit.SetValue(sdiIdentifier("nb_integr2",0,cnt), sdiValue(totintpoints));

                    int countinteg = 3;
                    int count_gauss = 0;
                    double mean_epsxx = 0.0;
                    double mean_epsyy = 0.0;
                    double mean_epszz = 0.0;
                    double mean_epsxy = 0.0;
                    double mean_epsyz = 0.0;
                    double mean_epszx = 0.0;

                    // Loop over integration points
                    // ----------------------------
                    for (int j = 0; j < totintpoints; ++j)
                    {
                        double epsxx = 0.0;
                        tempValue = sdiValue(epsxx);
                        selIniStrainShell->GetValue(sdiIdentifier("xx",0,cntDyna,j), tempValue);
                        tempValue.GetValue(epsxx);

                        double epsyy = 0.0;
                        tempValue = sdiValue(epsyy);
                        selIniStrainShell->GetValue(sdiIdentifier("yy",0,cntDyna,j), tempValue);
                        tempValue.GetValue(epsyy);

                        double epszz = 0.0;
                        tempValue = sdiValue(epszz);
                        selIniStrainShell->GetValue(sdiIdentifier("zz",0,cntDyna,j), tempValue);
                        tempValue.GetValue(epszz);

                        double epsxy = 0.0;
                        tempValue = sdiValue(epsxy);
                        selIniStrainShell->GetValue(sdiIdentifier("xy",0,cntDyna,j), tempValue);
                        tempValue.GetValue(epsxy);

                        double epsyz = 0.0;
                        tempValue = sdiValue(epsyz);
                        selIniStrainShell->GetValue(sdiIdentifier("yz",0,cntDyna,j), tempValue);
                        tempValue.GetValue(epsyz);

                        double epszx = 0.0;
                        tempValue = sdiValue(epszx);
                        selIniStrainShell->GetValue(sdiIdentifier("zx",0,cntDyna,j), tempValue);
                        tempValue.GetValue(epszx);

                        double t_pos = 0.0;
                        tempValue = sdiValue(t_pos);
                        selIniStrainShell->GetValue(sdiIdentifier("t",0,cntDyna,j), tempValue);
                        tempValue.GetValue(t_pos);

                        if (lsdNPLANE_ini == 4)
                        {
                            // compute the mean value over initial Gauss points 
                            // and write this mean value in one Gauss points for Radioss
                            mean_epsxx = mean_epsxx + epsxx/4.0;
                            mean_epsyy = mean_epsyy + epsyy/4.0;
                            mean_epszz = mean_epszz + epszz/4.0;
                            mean_epsxy = mean_epsxy + epsxy/4.0;
                            mean_epsyz = mean_epsyz + epsyz/4.0;
                            mean_epszx = mean_epszx + epszx/4.0;

                            if (j == countinteg)
                            {
                                inistrain3nEdit.SetValue(sdiIdentifier("eps_XX",0,cnt,count_gauss), sdiValue(mean_epsxx));
                                inistrain3nEdit.SetValue(sdiIdentifier("eps_YY",0,cnt,count_gauss), sdiValue(mean_epsyy));
                                inistrain3nEdit.SetValue(sdiIdentifier("eps_ZZ",0,cnt,count_gauss), sdiValue(mean_epszz));
                                inistrain3nEdit.SetValue(sdiIdentifier("eps_XY",0,cnt,count_gauss), sdiValue(mean_epsxy));
                                inistrain3nEdit.SetValue(sdiIdentifier("eps_YZ",0,cnt,count_gauss), sdiValue(mean_epsyz));
                                inistrain3nEdit.SetValue(sdiIdentifier("eps_ZX",0,cnt,count_gauss), sdiValue(mean_epszx));
                                inistrain3nEdit.SetValue(sdiIdentifier("T",0,cnt,count_gauss), sdiValue(t_pos));
                                count_gauss = count_gauss + 1;
                                countinteg = countinteg + 4;
                            }
                        }
                        else
                        {
                            inistrain3nEdit.SetValue(sdiIdentifier("eps_XX",0,cnt,j), sdiValue(epsxx));
                            inistrain3nEdit.SetValue(sdiIdentifier("eps_YY",0,cnt,j), sdiValue(epsyy));
                            inistrain3nEdit.SetValue(sdiIdentifier("eps_ZZ",0,cnt,j), sdiValue(epszz));
                            inistrain3nEdit.SetValue(sdiIdentifier("eps_XY",0,cnt,j), sdiValue(epsxy));
                            inistrain3nEdit.SetValue(sdiIdentifier("eps_YZ",0,cnt,j), sdiValue(epsyz));
                            inistrain3nEdit.SetValue(sdiIdentifier("eps_ZX",0,cnt,j), sdiValue(epszx));
                            inistrain3nEdit.SetValue(sdiIdentifier("T",0,cnt,j), sdiValue(t_pos));
                        }
                    } // for (int j = 0; j < totintpoints; ++j)
                    cnt = cnt + 1;
                }
                else
                {
                    //---
                    // Strain tensor values should be interpolated between lower (first dyna entry) 
                    // and upper (last dyna entry) linearly.
                    // All in-plane strain tensor entries are the same
                    //---
                    // if option = _SET, or lsdNPLANE =0, or lsdNTHICK=0
                    // get "npg" and "nb_integr" from property
                    // Ishell=24 then npg=1, ishell=12 npg=4, nb_integr=NP (number of integration points)

                    if (Ishellform == 12)
                    {
                        lsdNPLANE = 3;
                        inistrain3nEdit.SetValue(sdiIdentifier("npg",0,cnt), sdiValue(lsdNPLANE));
                    }
                    else
                    {
                        lsdNPLANE = 1;
                        inistrain3nEdit.SetValue(sdiIdentifier("npg",0,cnt), sdiValue(lsdNPLANE));
                    }
                    lsdNTHICK = lsdNIP;
                    inistrain3nEdit.SetValue(sdiIdentifier("npg",0,cnt), sdiValue(lsdNPLANE));
                    inistrain3nEdit.SetValue(sdiIdentifier("nb_integr",0,cnt), sdiValue(lsdNTHICK));
                    inistrain3nEdit.SetValue(sdiIdentifier("Thick",0,cnt), sdiValue(0.0));
                    inistrain3nEdit.SetValue(sdiIdentifier("nb_integr2",0,cnt), sdiValue(lsdNTHICK));

                    // Loop over integration points
                    // ----------------------------
                    // totintpoints =2, two line input (lower,upper shell surface) (if lsdNPLANE=0 and lsdNTHICK=0)
                    
                    sdiDoubleList strainXXList;
                    strainXXList.reserve(totintpoints);
                    sdiDoubleList strainYYList;
                    strainYYList.reserve(totintpoints);
                    sdiDoubleList strainZZList;
                    strainZZList.reserve(totintpoints);
                    sdiDoubleList strainXYList;
                    strainXYList.reserve(totintpoints);
                    sdiDoubleList strainYZList;
                    strainYZList.reserve(totintpoints);
                    sdiDoubleList strainZXList;
                    strainZXList.reserve(totintpoints);

                    sdiDoubleList strainInterpXXList;
                    strainInterpXXList.reserve(lsdNIP);
                    sdiDoubleList strainInterpYYList;
                    strainInterpYYList.reserve(lsdNIP);
                    sdiDoubleList strainInterpZZList;
                    strainInterpZZList.reserve(lsdNIP);
                    sdiDoubleList strainInterpXYList;
                    strainInterpXYList.reserve(lsdNIP);
                    sdiDoubleList strainInterpYZList;
                    strainInterpYZList.reserve(lsdNIP);
                    sdiDoubleList strainInterpZXList;
                    strainInterpZXList.reserve(lsdNIP);

                    for (int j = 0; j < totintpoints; ++j)
                    {
                        double epsxx = 0.0;
                        tempValue = sdiValue(epsxx);
                        selIniStrainShell->GetValue(sdiIdentifier("xx",0,cntDyna,j), tempValue);
                        tempValue.GetValue(epsxx);
                        strainXXList.push_back(epsxx);

                        double epsyy = 0.0;
                        tempValue = sdiValue(epsyy);
                        selIniStrainShell->GetValue(sdiIdentifier("yy",0,cntDyna,j), tempValue);
                        tempValue.GetValue(epsyy);
                        strainYYList.push_back(epsyy);

                        double epszz = 0.0;
                        tempValue = sdiValue(epszz);
                        selIniStrainShell->GetValue(sdiIdentifier("zz",0,cntDyna,j), tempValue);
                        tempValue.GetValue(epszz);
                        strainZZList.push_back(epszz);

                        double epsxy = 0.0;
                        tempValue = sdiValue(epsxy);
                        selIniStrainShell->GetValue(sdiIdentifier("xy",0,cntDyna,j), tempValue);
                        tempValue.GetValue(epsxy);
                        strainXYList.push_back(epsxy);

                        double epsyz = 0.0;
                        tempValue = sdiValue(epsyz);
                        selIniStrainShell->GetValue(sdiIdentifier("yz",0,cntDyna,j), tempValue);
                        tempValue.GetValue(epsyz);
                        strainYZList.push_back(epsyz);

                        double epszx = 0.0;
                        tempValue = sdiValue(epszx);
                        selIniStrainShell->GetValue(sdiIdentifier("zx",0,cntDyna,j), tempValue);
                        tempValue.GetValue(epszx);
                        strainZXList.push_back(epszx);

                        double t_pos = 0.0;
                        tempValue = sdiValue(t_pos);
                        selIniStrainShell->GetValue(sdiIdentifier("t",0,cntDyna,j), tempValue);
                        tempValue.GetValue(t_pos);
                    } // for (int j = 0; j < totintpoints; ++j)

                    // interpolate strain between lower (first dyna entry) and upper (last dyna entry) linearly.
                    double ratio;
                    double strainintp;
                    for (int j = 0; j < lsdNIP; ++j)
                    {
                      ratio = (ThickPosLayerList[j] - ThickPosLayerList[0])/(ThickPosLayerList[lsdNIP-1] - ThickPosLayerList[0]);

                      strainintp = strainXXList[0] + ratio*(strainXXList[1]-strainXXList[0]);
                      strainInterpXXList.push_back(strainintp);

                      strainintp = strainYYList[0] + ratio*(strainYYList[1]-strainYYList[0]);
                      strainInterpYYList.push_back(strainintp);

                      strainintp = strainZZList[0] + ratio*(strainZZList[1]-strainZZList[0]);
                      strainInterpZZList.push_back(strainintp);

                      strainintp = strainXYList[0] + ratio*(strainXYList[1]-strainXYList[0]);
                      strainInterpXYList.push_back(strainintp);

                      strainintp = strainYZList[0] + ratio*(strainYZList[1]-strainYZList[0]);
                      strainInterpYZList.push_back(strainintp);

                      strainintp = strainZXList[0] + ratio*(strainZXList[1]-strainZXList[0]);
                      strainInterpZXList.push_back(strainintp);
                    }

                    for (int j = 0; j < lsdNIP; ++j)
                    {
                        inistrain3nEdit.SetValue(sdiIdentifier("eps_XX",0,cnt,j), sdiValue(strainInterpXXList[j]));
                        inistrain3nEdit.SetValue(sdiIdentifier("eps_YY",0,cnt,j), sdiValue(strainInterpYYList[j]));
                        inistrain3nEdit.SetValue(sdiIdentifier("eps_ZZ",0,cnt,j), sdiValue(strainInterpZZList[j]));
                        inistrain3nEdit.SetValue(sdiIdentifier("eps_XY",0,cnt,j), sdiValue(strainInterpXYList[j]));
                        inistrain3nEdit.SetValue(sdiIdentifier("eps_YZ",0,cnt,j), sdiValue(strainInterpYZList[j]));
                        inistrain3nEdit.SetValue(sdiIdentifier("eps_ZX",0,cnt,j), sdiValue(strainInterpZXList[j]));
                        inistrain3nEdit.SetValue(sdiIdentifier("T",0,cnt,j), sdiValue(ThickPosLayerList[j]));
                    }


                    cnt = cnt + 1;
                } // if(lsdNPLANE > 0 && lsdNTHICK > 0)
                //--------------------
                sdiConvert::SDIHandlReadList sourceInitialStressesShell = { {selIniStrainShell->GetHandle()} };
                sdiConvert::Convert::PushToConversionLog(std::make_pair(inistrain3nHEdit, sourceInitialStressesShell));
                //--------------------
              } // if (partHRead.IsValid())
            } // if ( elementHRead.IsValid())
            cntDyna = cntDyna + 1;
          } // end for
        } // if(inistrain3nHEdit.IsValid())
    //--------------------------------------------
    } // while
}

