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
Chd|  I25STO                        source/interfaces/inter3d1/i25sto.F
Chd|-- called by -----------
Chd|        I25TRIVOX1                    source/interfaces/inter3d1/i25trivox1.F
Chd|-- calls ---------------
Chd|        I25COR3T                      source/interfaces/inter3d1/i25cor3t.F
Chd|        I25PEN3A                      source/interfaces/inter3d1/i25pen3a.F
Chd|        I25S1S2                       source/interfaces/inter3d1/i25sto.F
Chd|        TRI7BOX                       share/modules1/tri7box.F      
Chd|====================================================================
      SUBROUTINE I25STO(
     1      J_STOK,IRECT  ,X     ,NSV   ,II_STOK,
     2      CAND_N,CAND_E ,MULNSN,NOINT ,MARGE  ,
     3      I_MEM ,PROV_N ,PROV_E,ESHIFT,NSN    ,
     4      NRTM  ,GAP_S  ,GAP_M ,NBINFLG,MBINFLG,
     5      ILEV ,MSEGTYP,ITAB ,IGAP,GAP_S_L,
     6      GAP_M_L,EDGE_L2,ICODE,ISKEW,DRAD,
     7      DGAPLOAD)
C============================================================================
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_MEM, NSN, NRTM, NBINFLG(*),MBINFLG(*),ILEV, IGAP
      INTEGER J_STOK,MULNSN,NOINT,ESHIFT
      INTEGER IRECT(4,*),NSV(*),CAND_N(*),CAND_E(*)
      INTEGER PROV_N(MVSIZ),PROV_E(MVSIZ),II_STOK,MSEGTYP(*),ICODE(*),ISKEW(*),
     .        ITAB(*)
C     REAL
      my_real
     .        X(3,*), GAP_S(*), GAP_M(*),
     .        MARGE,
     .        GAP_S_L(*), GAP_M_L(*), EDGE_L2(*)
      my_real , INTENT(IN) :: DGAPLOAD ,DRAD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K_STOK,I_STOK,N,NE,J,ITYPE,ISH
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        ETYP(MVSIZ), IBC(MVSIZ)
C     REAL
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .   PENE(MVSIZ), GAPV(MVSIZ)
      DATA ITYPE/25/
C-----------------------------------------------
      CALL I25COR3T( J_STOK  ,X    ,IRECT   ,NSV   ,PROV_E  ,
     1               PROV_N  ,X1    ,X2      ,
     2               X3      ,X4   ,Y1      ,Y2    ,Y3      ,
     3               Y4      ,Z1   ,Z2      ,Z3    ,Z4      ,
     4               XI      ,YI   ,ZI      ,STIF  ,IX1     ,
     5               IX2     ,IX3  ,IX4     ,NSN   ,NRTM    ,
     6               MARGE   ,GAP_S,GAP_M   ,GAPV  ,ITYPE   ,
     7               IGAP ,GAP_S_L ,GAP_M_L ,EDGE_L2,MSEGTYP,
     8               ETYP ,ICODE   ,ISKEW    ,IBC   ,DRAD   ,
     9               DGAPLOAD )
C-----------------------------------------------
      CALL I25PEN3A( J_STOK ,X1    ,X2     ,X3   ,X4    ,
     .                Y1    ,Y2    ,Y3     ,Y4   ,
     .                Z1    ,Z2    ,Z3    ,Z4     ,XI   ,
     .                YI    ,ZI    ,PENE  ,IX1    ,IX2  ,
     .                IX3   ,IX4   ,GAPV  ,NRTM   ,ETYP ,
     .                IBC   )
C-----------------------------------------------
      IF (ILEV==2)
     .  CALL I25S1S2(J_STOK,PROV_N,PROV_E,NBINFLG,MBINFLG,PENE)
C-----------------------------------------------
      K_STOK = 0
      DO I=1,J_STOK
          IF(PENE(I)/=ZERO) THEN
            K_STOK = K_STOK + 1
          END IF
      ENDDO
      IF(K_STOK==0)RETURN
C
#include "lockon.inc"
      I_STOK = II_STOK
      IF(I_STOK+K_STOK>MULNSN) THEN
            I_MEM = 2
#include "lockoff.inc"
            RETURN
      ENDIF
      II_STOK   = I_STOK + K_STOK
#include "lockoff.inc"
      DO I=1,J_STOK
        IF(PENE(I)/=ZERO)THEN
          I_STOK = I_STOK + 1
          CAND_N(I_STOK) = PROV_N(I)
          CAND_E(I_STOK) = PROV_E(I)+ESHIFT
        ENDIF
      ENDDO
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  I25S1S2                       source/interfaces/inter3d1/i25sto.F
Chd|-- called by -----------
Chd|        I25STO                        source/interfaces/inter3d1/i25sto.F
Chd|        I25TRI                        source/interfaces/inter3d1/i25tri.F
Chd|-- calls ---------------
Chd|        BITGET                        source/interfaces/inter3d1/bitget.F
Chd|====================================================================
      SUBROUTINE I25S1S2(J_STOK,PROV_N,PROV_E,NBINFLG,MBINFLG,PENE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER J_STOK,PROV_N(*),PROV_E(*),NBINFLG(*),MBINFLG(*)
C     REAL
      my_real
     .   PENE(MVSIZ)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,NE,IMS1,IMS2,ISS1,ISS2
C     REAL
C-----------------------------------------------
C
!      EQUIVALENCE (PENE,BV(1,22))
      INTEGER BITGET
      EXTERNAL BITGET
C=======================================================================
         DO I=1,J_STOK
           N  = PROV_N(I)
           NE = PROV_E(I)
           IMS1 = BITGET(MBINFLG(NE),0)
           IMS2 = BITGET(MBINFLG(NE),1)
           ISS1 = BITGET(NBINFLG(N),0)
           ISS2 = BITGET(NBINFLG(N),1)
           IF(.NOT.((IMS1 == 1 .and. ISS2==1).or.
     .              (IMS2 == 1 .and. ISS1==1)))THEN
             PENE(I)=ZERO
           ENDIF
         ENDDO
C
      RETURN
      END
