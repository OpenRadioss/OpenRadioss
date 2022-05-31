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
Chd|====================================================================
Chd|  MMAIN                         source/materials/mat_share/mmain.F
Chd|-- called by -----------
Chd|        S4REFSTA3                     source/elements/solid/solide4/s4refsta3.F
Chd|        SREFSTA3                      source/elements/solid/solide/srefsta3.F
Chd|-- calls ---------------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE MMAIN(PM      ,ELBUF_STR,IX   ,NIX    ,X       ,
     2                  GEO    ,IPARG  ,NEL    ,SKEW   ,BUFMAT  ,
     3                  IPART  ,IPARTEL ,
     4                  MAT    ,IPM    ,NGL    ,PID    ,NPF     ,
     5                  TF     ,MFXX   ,MFXY   ,MFXZ   ,MFYX    ,
     6                  MFYY   ,MFYZ   ,MFZX   ,MFZY   ,MFZZ    ,
     7                  RX     ,RY     ,RZ     ,SX     ,SY      ,
     8                  SZ     ,GAMA   ,VOLN   ,DVOL   ,S1      ,
     C                  S2     ,S3     ,S4     ,S5     ,S6      ,
     9                  DXX    ,DYY    ,DZZ    ,D4     ,D5      ,
     A                  D6     ,WXX    ,WYY    ,WZZ    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "scr01_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
#include      "warn_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIX,NEL
      INTEGER IX(NIX,*), IPARG(*),
     .        IPART(LIPART1,*),IPARTEL(*),MAT(*),IPM(NPROPMI,*),
     .        NGL(*), PID(*), NPF(*)
      my_real
     .   X(*), GEO(*), PM(NPROPM,*),
     .   SKEW(LSKEW,*),BUFMAT(*),
     .   MFXX(*)  ,MFXY(*)  ,MFXZ(*)  ,MFYX(*)  ,MFYY(*)  ,
     .   MFYZ(*)  ,MFZX(*)  ,MFZY(*)  ,MFZZ(*) ,
     .   RX(*)    ,RY(*)    ,RZ(*)    ,
     .   SX(*)    ,SY(*)    ,SZ(*)    ,GAMA(NEL,6),
     .   VOLN(*)  ,DVOL(*),
     .   TF(*)    ,DXX(*)   ,DYY(*)   ,DZZ(*),
     .   D4(*)    ,D5(*)    ,D6(*),
     .   WXX(*)   ,WYY(*)   ,WZZ(*),
     .   S1(*)    ,S2(*)    ,S3(*) ,
     .   S4(*)    ,S5(*)    ,S6(*)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NBE,NUVAR, NUVARR, NB11A, I, IADBUF
      my_real
     .   TX(MVSIZ), TY(MVSIZ), TZ(MVSIZ),
     .   VIS(MVSIZ), CXX(MVSIZ), OFF(MVSIZ), EPSD(MVSIZ)
C-----
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(BUF_MAT_) ,POINTER :: MBUF  
C=======================================================================
      IF (MTN >= 28 .AND. MTN /= 49) THEN
        GBUF  => ELBUF_STR%GBUF
        LBUF  => ELBUF_STR%BUFLY(1)%LBUF(1,1,1)
        MBUF  => ELBUF_STR%BUFLY(1)%MAT(1,1,1)
        NUVAR =  ELBUF_STR%BUFLY(1)%NVAR_MAT
        OFF   =  ONE
        EPSD  =  ZERO
C
        CALL MULAW(LFT      ,LLT      ,NFT      ,MTN      ,JCVT    ,
     2             PM       ,OFF      ,GBUF%SIG ,GBUF%EINT,GBUF%RHO,
     3             GBUF%VOL ,LBUF%STRA,GAMA     ,
     4             MBUF%VAR ,BUFMAT   ,TF       ,NPF      ,
     5             MAT      ,NGL      ,NUVAR    ,
     6                       GEO      ,PID      ,EPSD     ,
     8                                                     WXX    ,
     9             WYY      ,WZZ      ,JSPH     ,          CXX    ,
     A                       VOLN     ,                    VIS    ,
     B             DXX      ,DYY      ,DZZ      ,D4       ,D5     ,
     C             D6       ,
     D             DVOL     ,S1       ,S2       ,S3       ,S4     ,
     E             S5       ,S6       ,RX       ,RY       ,RZ     ,
     F             SX       ,SY       ,SZ       ,TX       ,TY     ,
     G             TZ       ,                              ISMSTR ,
     H             MFXX     ,MFXY     ,MFXZ     ,MFYX     ,MFYY   ,
     I             MFYZ     ,MFZX     ,MFZY     ,MFZZ     ,IPM    ,
     J             ISORTH   ,NEL      )
      ENDIF
C------------
      RETURN
      END
