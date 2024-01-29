Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
Copyright>    
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>    
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>    
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>    
Copyright>    
Copyright>        Commercial Alternative: Altair Radioss Software 
Copyright>    
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss 
Copyright>        software under a commercial license.  Contact Altair to discuss further if the 
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.    
C.../SCR16/
      COMMON /SCR16R/ DTOUTP0,TOUTP0
      my_real
     .    DTOUTP0,TOUTP0
C
      INTEGER MX_OUTP,MX_OUTP2
      PARAMETER (MX_OUTP = 140,MX_OUTP2 = 50000)
      COMMON /SCR16/NV_OUTP,OUTP_V(MX_OUTP),NN_OUTP,OUTP_N(MX_OUTP),
     .        NSS_OUTP,OUTP_SS(MX_OUTP),NCS_OUTP,OUTP_CS(MX_OUTP),
     .        NTS_OUTP,OUTP_TS(MX_OUTP),NPS_OUTP,OUTP_PS(MX_OUTP),
     .        NRS_OUTP,OUTP_RS(MX_OUTP),
     .        NST_OUTP,OUTP_ST(MX_OUTP),NCT_OUTP,OUTP_CT(MX_OUTP),
     .        NTT_OUTP,OUTP_TT(MX_OUTP),NPT_OUTP,OUTP_PT(MX_OUTP),
     .        NRT_OUTP,OUTP_RT(MX_OUTP),IOUTP_FMT,OUTYY_FMT,
     .        NSPS_OUTP,OUTP_SPS(MX_OUTP),NSPT_OUTP,OUTP_SPT(MX_OUTP),
     .        IROOTYY,IDROT,MOUTPT,MOUTP(MX_OUTP2),
     .        IROOTYY_R,S0FILE
      INTEGER NV_OUTP,OUTP_V,NN_OUTP,OUTP_N,
     .        NSS_OUTP,OUTP_SS,NCS_OUTP,OUTP_CS,
     .        NTS_OUTP,OUTP_TS,NPS_OUTP,OUTP_PS,
     .        NRS_OUTP,OUTP_RS,
     .        NST_OUTP,OUTP_ST,NCT_OUTP,OUTP_CT,
     .        NTT_OUTP,OUTP_TT,NPT_OUTP,OUTP_PT,
     .        NRT_OUTP,OUTP_RT,IOUTP_FMT,OUTYY_FMT,
     .        NSPS_OUTP,OUTP_SPS,NSPT_OUTP,OUTP_SPT, 
     .        IROOTYY,IDROT,MOUTPT,MOUTP,IROOTYY_R
      CHARACTER S0FILE*500
 
C.../SCR16_STATR/
      COMMON /SCR16_STATR/ DTSTAT0,TSTAT0,DTABF0(10),DTABFWR0(10)
      my_real
     .    DTSTAT0,TSTAT0,DTABF0,DTABFWR0
      INTEGER MX_STAT,MX_STAT2,MX_DYNAIN
      PARAMETER (MX_STAT = 20,MX_STAT2 = 50000,MX_DYNAIN = 20)
C.../SCR16_STATI/
      COMMON /SCR16_STATI/MX_STAT0,NSTATPRT,NC_STAT,STAT_C(MX_STAT),
     .              STAT_NUMELC,STAT_NUMELTG,
     .              STAT_NUMELC_G,STAT_NUMELTG_G,
     .              STAT_S(MX_STAT),STAT_NUMELS,STAT_NUMELT,
     .              STAT_NUMELS_G,STAT_NUMELT_G,
     .              STAT_N(MX_STAT),MSTATT,MSTAT,
     .              STAT_NUMELR,STAT_NUMELR_G,STAT_R(MX_STAT),
     .              STAT_NUMELP,STAT_NUMELP_G,STAT_P(MX_STAT),
     .              STAT_T(MX_STAT),NSTATALL
      INTEGER MX_STAT0,NSTATPRT,NC_STAT,STAT_C,
     .        STAT_NUMELC,STAT_NUMELTG,
     .        STAT_NUMELC_G,STAT_NUMELTG_G,
     .        STAT_S,STAT_NUMELS,STAT_NUMELT,
     .        STAT_NUMELS_G,STAT_NUMELT_G,
     .        STAT_N,MSTATT,MSTAT(MX_STAT2),
     .        STAT_NUMELR,STAT_NUMELR_G,STAT_R,
     .        STAT_NUMELP,STAT_NUMELP_G,STAT_P,STAT_T,NSTATALL
