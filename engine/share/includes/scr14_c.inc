Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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

c----------------------------------------------------------
      INTEGER MX_ANI
      PARAMETER (MX_ANI = 80000)      
c----------------------------------------------------------
      COMMON /SCR14/NV_ANI,ANIM_V(MX_ANI),NT_ANI,ANIM_T(MX_ANI),
     .              NE_ANI,ANIM_E(MX_ANI),NN_ANI,ANIM_N(MX_ANI),
     .              NCT_ANI,ANIM_CT(MX_ANI),NCE_ANI,ANIM_CE(MX_ANI),
     .              NST_ANI,ANIM_ST(MX_ANI),NSE_ANI,ANIM_SE(MX_ANI),
     .              NFT_ANI,ANIM_FT(MX_ANI),NFE_ANI,ANIM_FE(MX_ANI),
     .              ANIM_M,ANIM_K,ANIM_U,ANIM_MAT,IEPSDOT,FMT_ANI,
     .              ANIM_VERS,IZIP,IANISENS,IFVANI,IFAILA,IAD_GPS,
     .              IZIPSTRS,ANIM_PLY,NLSENS,LANISENS(MX_ANI),ANIM_CRK,
     .              TITLETAB,NTITLETAB,NLTITLE,IANISENS_OLD,LANISENS_OLD(MX_ANI),
     .              ISTRESALL,ISTRAIALL,IEPSDOALL,IEPSPALL,IEPSPNLALL,IORTHDALL,
     .              IEPSPFULL,ISTRESFULL,IPLYALL,ISTRESALL_PLY,ISTRESALL_PLY_IPT,
     .              IPHIALL_PLY,IEPSPALL_PLY,ISTRAINALL_PLY,IDAMAFULL,ISTRAINFULL,
     .              IEPSDOFULL,IPHIALL,IWPLAFULL,IWPLAALL,IDAMAALL,IEPSDNLALL,
     .              INXTFALL,SIGH1ALL,SIGH2ALL,ITSAIWUALL,ITSAIWUFULL
      INTEGER NV_ANI,ANIM_V,NT_ANI,ANIM_T,NE_ANI,ANIM_E,
     .        NST_ANI,ANIM_ST,NSE_ANI,ANIM_SE,
     .        NCT_ANI,ANIM_CT,NCE_ANI,ANIM_CE,
     .        NFT_ANI,ANIM_FT,NFE_ANI,ANIM_FE,
     .        NN_ANI,ANIM_N,ANIM_M,ANIM_K,ANIM_U,ANIM_MAT,
     .        IEPSDOT,FMT_ANI,ANIM_VERS,IZIP,IANISENS,IFVANI,IFAILA,
     .        IAD_GPS,IZIPSTRS,ANIM_PLY,NLSENS,LANISENS,ANIM_CRK,
     .        NTITLETAB(MX_ANI),NLTITLE,IANISENS_OLD,LANISENS_OLD,
     .        ISTRESALL,ISTRAIALL,IEPSDOALL,IEPSPALL,IEPSPNLALL,IORTHDALL,
     .        IEPSPFULL,ISTRESFULL,IPLYALL,ISTRESALL_PLY,ISTRESALL_PLY_IPT,
     .        IPHIALL_PLY,IEPSPALL_PLY,ISTRAINALL_PLY,IDAMAFULL,ISTRAINFULL,
     .        IEPSDOFULL,IPHIALL,IWPLAFULL,IWPLAALL,IDAMAALL,IEPSDNLALL,
     .        INXTFALL,SIGH1ALL,SIGH2ALL,ITSAIWUALL,ITSAIWUFULL
      CHARACTER*80  TITLETAB(MX_ANI)
