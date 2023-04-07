Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  I5KE3                         source/interfaces/inter3d/i5ke3.F
Chd|-- called by -----------
Chd|        IMP_INT_K                     source/implicit/imp_int_k.F   
Chd|-- calls ---------------
Chd|        ASSEM_INT                     source/implicit/assem_int.F   
Chd|        I3CST3                        source/interfaces/inter3d/i3cst3.F
Chd|        I3DIS3                        source/interfaces/inter3d/i3dis3.F
Chd|        I3GAP3                        source/interfaces/inter3d/i3gap3.F
Chd|        I5CORK3                       source/interfaces/inter3d/i5cork3.F
Chd|        I5FRIK3                       source/interfaces/inter3d/i5keg3.F
Chd|        I5KEG3                        source/interfaces/inter3d/i5keg3.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I5KE3(  A     ,V      ,MS   ,
     1                   IPARI ,INTBUF_TAB,X    ,
     2                   NUM_IMP,CAND_N,CAND_E     ,
     3                   IDDL  ,K_DIAG ,K_LT ,IADK ,JDIK  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
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
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "impl2_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(*)
      INTEGER NUM_IMP,CAND_N(*),CAND_E(*),IDDL(*),
     .         IADK(*)     ,JDIK(*)
C     REAL
      my_real 
     .  A(3,*), MS(*), V(3,*),X(*),K_DIAG(*),K_LT(*) 

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IGAP, INACTI, IFQ, MFROT, IGSTI
      INTEGER JX1(MVSIZ), JX2(MVSIZ), JX3(MVSIZ), JX4(MVSIZ),
     .        NSVG(MVSIZ), I3N ,IGIMP
      INTEGER, DIMENSION(MVSIZ) :: IX1,IX2,IX3,IX4
C     REAL
      my_real
     .   STARTT, FRIC, GAP, STOPT,STIGLO,GAPMIN,
     .   KMIN, KMAX, GAPMAX,GAP_IMP
C-----------------------------------------------
C     REAL
      my_real
     .    NTJ(3,MVSIZ),KI11(9,MVSIZ),KJ11(9,MVSIZ),
     .    KK11(9,MVSIZ),KL11(9,MVSIZ),KI12(9,MVSIZ),FREQ,
     .    KJ12(9,MVSIZ),KK12(9,MVSIZ),KL12(9,MVSIZ),OFF(MVSIZ),
     .    DIST(MVSIZ)
      my_real, DIMENSION(MVSIZ) :: X1,X2,X3,X4,XI
      my_real, DIMENSION(MVSIZ) :: Y1,Y2,Y3,Y4,YI
      my_real, DIMENSION(MVSIZ) :: Z1,Z2,Z3,Z4,ZI
      my_real, DIMENSION(MVSIZ) :: XFACE,N1,N2,N3
      my_real, DIMENSION(MVSIZ) :: SSC,TTC,AREA,THK,ALP
      my_real, DIMENSION(MVSIZ) :: X0,Y0,Z0,ANS
      my_real, DIMENSION(MVSIZ) :: XX1,XX2,XX3,XX4
      my_real, DIMENSION(MVSIZ) :: YY1,YY2,YY3,YY4
      my_real, DIMENSION(MVSIZ) :: ZZ1,ZZ2,ZZ3,ZZ4
      my_real, DIMENSION(MVSIZ) :: XI1,XI2,XI3,XI4
      my_real, DIMENSION(MVSIZ) :: YI1,YI2,YI3,YI4
      my_real, DIMENSION(MVSIZ) :: ZI1,ZI2,ZI3,ZI4
      my_real, DIMENSION(MVSIZ) :: XN1,XN2,XN3,XN4
      my_real, DIMENSION(MVSIZ) :: YN1,YN2,YN3,YN4
      my_real, DIMENSION(MVSIZ) :: ZN1,ZN2,ZN3,ZN4
      my_real, DIMENSION(MVSIZ) :: XP,YP,ZP
      my_real, DIMENSION(MVSIZ) :: H1,H2,H3,H4,STIF
      INTEGER :: NSN,NTY
      INTEGER :: LFT,LLT,NFT
C=======================================================================
      NSN   = IPARI(5)
      NTY   = IPARI(7)
      MFROT = IPARI(30)
      IFQ   = IPARI(31)
      FREQ  = ONE
C
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT>TT) RETURN
      IF(TT>STOPT)  RETURN
C
      GAP  =INTBUF_TAB%VARIABLES(2)
C
      IF(NTY==3)THEN
      ELSEIF(NTY==4)THEN
      ELSEIF(NTY==5)THEN
        FRIC = INTBUF_TAB%VARIABLES(1)
        IF (IFQ /= 0) FREQ = INTBUF_TAB%XFILTR(1)
c
        DO NFT = 0 , NUM_IMP - 1 , NVSIZ
          LFT=1
          LLT = MIN( NVSIZ, NUM_IMP - NFT )
          CALL I5CORK3(
     1   X,                INTBUF_TAB%IRECTM,INTBUF_TAB%MSR,   INTBUF_TAB%NSV,
     2   INTBUF_TAB%IRTLM, CAND_N(NFT+1),    CAND_E(NFT+1),    NSVG,
     3   JX1,              JX2,              JX3,              JX4,
     4   X1,               X2,               X3,               X4,
     5   Y1,               Y2,               Y3,               Y4,
     6   Z1,               Z2,               Z3,               Z4,
     7   XFACE,            XI,               YI,               ZI,
     8   IX1,              IX2,              IX3,              IX4,
     9   LFT,              LLT,              NFT)
          CALL I3CST3(
     1   X1,      X2,      X3,      X4,
     2   XI,      Y1,      Y2,      Y3,
     3   Y4,      YI,      Z1,      Z2,
     4   Z3,      Z4,      ZI,      XFACE,
     5   N1,      N2,      N3,      SSC,
     6   TTC,     X0,      Y0,      Z0,
     7   XX1,     XX2,     XX3,     XX4,
     8   YY1,     YY2,     YY3,     YY4,
     9   ZZ1,     ZZ2,     ZZ3,     ZZ4,
     A   XI1,     XI2,     XI3,     XI4,
     B   YI1,     YI2,     YI3,     YI4,
     C   ZI1,     ZI2,     ZI3,     ZI4,
     D   XN1,     XN2,     XN3,     XN4,
     E   YN1,     YN2,     YN3,     YN4,
     F   ZN1,     ZN2,     ZN3,     ZN4,
     G   AREA,    LFT,     LLT)
          CALL I3GAP3(
     1   GAP,     AREA,    THK,     ALP,
     2   LFT,     LLT)
          CALL I3DIS3(
     1   IGIMP,   NTY,     DIST,    X1,
     2   X2,      X3,      X4,      XI,
     3   Y1,      Y2,      Y3,      Y4,
     4   YI,      Z1,      Z2,      Z3,
     5   Z4,      ZI,      XFACE,   N1,
     6   N2,      N3,      SSC,     TTC,
     7   ALP,     ANS,     XP,      YP,
     8   ZP,      H1,      H2,      H3,
     9   H4,      LFT,     LLT)
          CALL I5FRIK3(LFT   ,LLT   ,CAND_N(NFT+1),CAND_E(NFT+1),IPARI ,
     2                 X     ,INTBUF_TAB%IRECTM,INTBUF_TAB%MSR,INTBUF_TAB%NSV,
     3          INTBUF_TAB%IRTLM,INTBUF_TAB%CSTS,INTBUF_TAB%IRTLOM,INTBUF_TAB%FRICOS,
     4          FRIC ,FREQ   ,INTBUF_TAB%FTSAV,INTBUF_TAB%STFM,NTJ    ,XP    ,YP     ,
     5          ZP    ,N1    ,N2    ,N3    ,ANS    ,
     6          STIF  )!,FXI   ,FYI   ,FZI   ,FNI)
          CALL I5KEG3(LFT    ,LLT   ,FRIC   ,SK_INT,
     3                NTJ    ,KI11  ,KI12   ,KJ11   ,KJ12  ,
     4                KK11   ,KK12   ,KL11  ,KL12   ,OFF   ,
     5                N1     ,N2     ,N3    ,STIF   ,H1    ,
     6                H2     ,H3     ,H4)
          CALL ASSEM_INT(3       ,NSVG     ,JX1    ,JX2    ,JX3    ,
     1         JX4      ,LLT       ,IDDL      ,K_DIAG    ,K_LT     ,
     2         IADK     ,JDIK      ,KI11      ,KI12      ,KJ11     ,
     3         KJ12     ,KK11      ,KK12      ,KL11      ,KL12     ,
     4         OFF      )
        END DO 
      ELSEIF(NTY==6)THEN
C
      ELSEIF(NTY==7.OR.NTY==22)THEN
C
      ENDIF
C
      RETURN
      END
