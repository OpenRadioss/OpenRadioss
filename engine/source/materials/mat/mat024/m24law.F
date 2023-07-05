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
Chd|  M24LAW                        source/materials/mat/mat024/m24law.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|        CONC24                        source/materials/mat/mat024/conc24.F
Chd|        M24ANIM                       source/materials/mat/mat024/m24anim.F
Chd|        M24DMAX                       source/materials/mat/mat024/m24dmax.F
Chd|        MDTSPH                        source/materials/mat_share/mdtsph.F
Chd|        MNSVIS                        source/materials/mat_share/mnsvis.F
Chd|        MQVISCB                       source/materials/mat_share/mqviscb.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE M24LAW(
     1   LBUF,    PM,      OFF,     SIG,
     2   EINT,    RHO,     QOLD,    VOL,
     3   STIFN,   DT2T,    NELTST,  ITYPTST,
     4   OFFSET,  NEL,     OFFG,    GEO,
     5   PID,     MAT,     NGL,     SSP,
     6   AIRE,    VOLN,    VD2,     DELTAX,
     7   VIS,     D1,      D2,      D3,
     8   PNEW,    PSH,     Q,       SSP_EQ,
     9   DVOL,    D4,      D5,      D6,
     A   MUMAX,   GAMA,    MSSA,    DMELS,
     B   D_MAX,   SOLD1,   SOLD2,   SOLD3,
     C   SOLD4,   SOLD5,   SOLD6,   RX,
     D   RY,      RZ,      SX,      SY,
     E   SZ,      CONDE,   VOL_AVG, DTEL,
     F   G_DT,    IPM,     RHOREF,  RHOSP,
     G   EPSD,    ITY,     JTUR,    JTHE,
     H   JHBE,    JCVT,    JSPH,    ISMSTR,
     I   JSMS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
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
#include      "nsvis_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER, INTENT(IN) :: JSMS
      INTEGER, INTENT(IN) :: ITY
      INTEGER, INTENT(IN) :: JTUR
      INTEGER, INTENT(IN) :: JTHE
      INTEGER, INTENT(IN) :: JHBE
      INTEGER, INTENT(IN) :: JCVT
      INTEGER, INTENT(IN) :: JSPH
      INTEGER MAT(NEL),NGL(NEL),PID(NEL),G_DT, IPM(NPROPMI,*)
      INTEGER NELTST,ITYPTST,OFFSET,NEL
      my_real DT2T
      my_real
     .   PM(NPROPM,*), OFF(NEL), SIG(NEL,6), EINT(NEL), RHO(NEL), QOLD(NEL),
     .   VOL(NEL), STIFN(NEL),OFFG(NEL),GEO(NPROPG,*), GAMA(MVSIZ,6),
     .   VOLN(NEL), VD2(NEL), DELTAX(NEL), SSP(NEL), AIRE(NEL), VIS(NEL), 
     .   PSH(NEL), PNEW(MVSIZ),Q(NEL) ,SSP_EQ(NEL), DVOL(NEL),MUMAX(NEL), 
     .   D1(NEL), D2(NEL), D3(NEL), D4(NEL), D5(NEL), D6(NEL),
     .   MSSA(NEL), DMELS(NEL),D_MAX(NEL),EPSD(NEL),
     .   RX(NEL),RY(NEL),RZ(NEL),SX(NEL),SY(NEL),SZ(NEL),
     .   SOLD1(NEL),SOLD2(NEL),SOLD3(NEL),SOLD4(NEL), 
     .   SOLD5(NEL), SOLD6(NEL), CONDE(NEL), VOL_AVG(NEL),DTEL(NEL),
     .   RHOREF(*)  ,RHOSP(*)  
      TYPE(L_BUFEL_)  :: LBUF     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IMAT,IBID,ISVIS
      my_real E1, E2, E3, E4, E5, E6,BID1,BID2,BID3,FACQ0
      my_real, DIMENSION(NEL) :: R11,R12,R13,R21,R22,R23,R31,R32,R33
C=======================================================================
      ISVIS = 0
      FACQ0 = ONE
      IMAT  = MAT(1)
c
c     strain rate calculation (for output only)
c      
       DO I=1,NEL                                                                                                    
         EPSD(I) = MAX( ABS(D1(I)), ABS(D2(I)), ABS(D3(I)), 
     .   HALF*ABS(D4(I)),HALF*ABS(D5(I)),HALF*ABS(D6(I)))   
       ENDDO                                                        
c
      CALL CONC24(
     1   PM(1,IMAT),OFF,       SIG,       EINT,
     2   LBUF%SIGA, LBUF%EPSA, GAMA,      LBUF%DAM,
     3   LBUF%ANG,  LBUF%SF,   LBUF%VK,   LBUF%STRA,
     4   LBUF%CRAK, LBUF%DSUM, LBUF%ROB,  LBUF%SIGC,
     5   LBUF%RK,   LBUF%PLA,  NGL,       D1,
     6   D2,        D3,        D4,        D5,
     7   D6,        RX,        RY,        RZ,
     8   SX,        SY,        SZ,        LBUF%SEQ,
     9   RHO,       LBUF%EPE,  NEL,       R11,
     A   R12,       R13,       R21,       R22,
     B   R23,       R31,       R32,       R33,
     C   JCVT,      JSPH)
C----------------------------------------
C     CRACKS DANS REPERE GLOBAL POUR ANIM
C----------------------------------------
      IF (PM(56,MAT(1)) == ONE) 
     .  CALL M24ANIM(LBUF%DAM,LBUF%ANG,LBUF%DGLO,NEL,
     .               R11,R12,R13,R21,R22,
     .               R23,R31,R32,R33)
c
      IF (JHBE == 24) CALL M24DMAX(LBUF%DAM,D_MAX,NEL)
C------------     
      DO I=1,NEL
        SSP(I)=SQRT(PM(24,IMAT)/PM(1,IMAT))
        VIS(I)=ZERO
      ENDDO
C
      IF(JSPH==0)THEN
       CALL MQVISCB(
     1   PM,      OFF,     RHO,     BID1,
     2   BID2,    SSP,     BID3,    STIFN,
     3   DT2T,    NELTST,  ITYPTST, AIRE,
     4   OFFG,    GEO,     PID,     VOLN,
     5   VD2,     DELTAX,  VIS,     D1,
     6   D2,      D3,      PNEW,    PSH,
     7   MAT,     NGL,     Q,       SSP_EQ,
     8   VOL,     MSSA,    DMELS,   IBID,
     9   FACQ0,   CONDE,   DTEL,    G_DT,
     A   IPM,     RHOREF,  RHOSP,   NEL,
     B   ITY,     ISMSTR,  JTUR,    JTHE,
     C   JSMS)
C
       CALL MNSVIS(
     1   PM,      OFF,     RHO,     GEO,
     2   PID,     SSP,     AIRE,    VOLN,
     3   D1,      D2,      D3,      D4,
     4   D5,      D6,      MAT,     ISVIS,
     5   RHOREF,  NEL)
C . . . . . . . . . . . . . . . . . . . . . . . . . .
C L'ENERGIE INTERNE COMPREND L ENERGIE DE FISSURATION
C . . . . . . . . . . . . . . . . . . . . . . . . . .
       DO 500 I=1,NEL
       E1=D1(I)*(SOLD1(I)+SIG(I,1)+SVIS(I,1))
       E2=D2(I)*(SOLD2(I)+SIG(I,2)+SVIS(I,2))
       E3=D3(I)*(SOLD3(I)+SIG(I,3)+SVIS(I,3))
       E4=D4(I)*(SOLD4(I)+SIG(I,4)+SVIS(I,4))
       E5=D5(I)*(SOLD5(I)+SIG(I,5)+SVIS(I,5))
       E6=D6(I)*(SOLD6(I)+SIG(I,6)+SVIS(I,6))
       EINT(I)=EINT(I)
     .       -(Q(I)+QOLD(I))*DVOL(I)*HALF
     .       +(E1+E2+E3+E4+E5+E6)*VOL_AVG(I)*DT1*HALF
C
       EINT(I)=EINT(I)/VOL(I)
       QOLD(I)=Q(I)
  500  CONTINUE
      ELSE
       CALL MDTSPH(
     1   PM,      OFF,     RHO,     BID1,
     2   BID2,    BID3,    STIFN,   DT2T,
     3   NELTST,  ITYPTST, OFFG,    GEO,
     4   PID,     MUMAX,   SSP,     VOLN,
     5   VD2,     DELTAX,  VIS,     D1,
     6   D2,      D3,      PNEW,    PSH,
     7   MAT,     NGL,     Q,       SSP_EQ,
     8   G_DT,    DTEL,    NEL,     ITY,
     9   JTUR,    JTHE)
     
       CALL MNSVIS(
     1   PM,      OFF,     RHO,     GEO,
     2   PID,     SSP,     AIRE,    VOLN,
     3   D1,      D2,      D3,      D4,
     4   D5,      D6,      MAT,     ISVIS,
     5   RHOREF,  NEL)
C . . . . . . . . . . . . . . . . . . . . . . . . . .
C L'ENERGIE INTERNE COMPREND L ENERGIE DE FISSURATION
C . . . . . . . . . . . . . . . . . . . . . . . . . .
       DO 510 I=1,NEL
       E1=D1(I)*(SOLD1(I)+SIG(I,1)+SVIS(I,1))
       E2=D2(I)*(SOLD2(I)+SIG(I,2)+SVIS(I,2))
       E3=D3(I)*(SOLD3(I)+SIG(I,3)+SVIS(I,3))
       E4=D4(I)*(SOLD4(I)+SIG(I,4)+SVIS(I,4))
       E5=D5(I)*(SOLD5(I)+SIG(I,5)+SVIS(I,5))
       E6=D6(I)*(SOLD6(I)+SIG(I,6)+SVIS(I,6))
       EINT(I)=EINT(I)+(E1+E2+E3+E4+E5+E6)*VOL_AVG(I)*DT1*HALF
       EINT(I)=EINT(I)/VOL(I)
  510  CONTINUE
      ENDIF
c-----------
      RETURN
      END
