Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    mmain          ../starter/source/materials/mat_share/mmain.F
!||--- called by ------------------------------------------------------
!||    s4refsta3      ../starter/source/elements/solid/solide4/s4refsta3.F
!||    srefsta3       ../starter/source/elements/solid/solide/srefsta3.F
!||--- calls      -----------------------------------------------------
!||    mulaw          ../starter/source/materials/mat_share/mulaw.F
!||--- uses       -----------------------------------------------------
!||====================================================================
      SUBROUTINE MMAIN(PM      ,ELBUF_STR,IX   ,NIX    ,X       ,
     2                  GEO    ,IPARG  ,NEL    ,SKEW   ,BUFMAT  ,
     3                  IPART  ,IPARTEL,NUMMAT ,MATPARAM,
     4                  IMAT   ,IPM    ,NGL    ,PID    ,NPF     ,
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
      USE MAT_ELEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "vect01_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NUMMAT
      INTEGER ,INTENT(IN) :: IMAT
      INTEGER ,INTENT(IN) :: NIX,NEL
      INTEGER IX(NIX,*), IPARG(*),
     .        IPART(LIPART1,*),IPARTEL(*),IPM(NPROPMI,*),
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
      TYPE(MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(INOUT) :: MATPARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NUVAR,NVARTMP,NUVARR
      my_real
     .   TX(MVSIZ), TY(MVSIZ), TZ(MVSIZ),
     .   VIS(MVSIZ), CXX(MVSIZ), OFF(MVSIZ), EPSD(MVSIZ)
C-----
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(BUF_MAT_) ,POINTER :: MBUF  
C=======================================================================
      IF (MTN >= 28 .AND. MTN /= 49.OR.MTN ==1) THEN
        GBUF  => ELBUF_STR%GBUF
        LBUF  => ELBUF_STR%BUFLY(1)%LBUF(1,1,1)
        MBUF  => ELBUF_STR%BUFLY(1)%MAT(1,1,1)
        NUVAR =  ELBUF_STR%BUFLY(1)%NVAR_MAT
        NVARTMP =  ELBUF_STR%BUFLY(1)%NVARTMP
        OFF   =  ONE
        EPSD  =  ZERO
C
        CALL MULAW(LFT      ,LLT      ,NFT      ,MTN      ,JCVT    ,
     2             PM       ,OFF      ,GBUF%SIG ,GBUF%EINT,GBUF%RHO,
     3             GBUF%VOL ,LBUF%STRA,GAMA     ,
     4             MBUF%VAR ,BUFMAT   ,TF       ,NPF      ,
     5             IMAT     ,NGL      ,NUVAR    ,NVARTMP, MBUF%VARTMP,
     6             GEO      ,PID      ,EPSD     ,
     8             WXX      ,WYY      ,WZZ      ,JSPH     ,CXX    ,
     A             VOLN     ,VIS      ,DXX      ,
     B             DYY      ,DZZ      ,D4       ,D5       ,D6     ,
     C             DVOL     ,S1       ,S2       ,S3       ,S4     ,
     D             S5       ,S6       ,RX       ,RY       ,RZ     ,
     E             SX       ,SY       ,SZ       ,TX       ,TY     ,
     F             TZ       ,ISMSTR   ,
     G             MFXX     ,MFXY     ,MFXZ     ,MFYX     ,MFYY   ,
     H             MFYZ     ,MFZX     ,MFZY     ,MFZZ     ,IPM    ,
     I             ISORTH   ,NEL      ,MATPARAM)
     J             
      ENDIF
C------------
      RETURN
      END
