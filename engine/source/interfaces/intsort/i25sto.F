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
Chd|  I25STO                        source/interfaces/intsort/i25sto.F
Chd|-- called by -----------
Chd|        I25TRIVOX                     source/interfaces/intsort/i25trivox.F
Chd|-- calls ---------------
Chd|        I25COR3T                      source/interfaces/intsort/i25cor3t.F
Chd|        I25PEN3                       source/interfaces/intsort/i25pen3.F
Chd|        I25S1S2                       source/interfaces/intsort/i25sto.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25STO(
     1      J_STOK,IRECT  ,X     ,NSV   ,II_STOK,
     2      CAND_N,CAND_E ,MULNSN,NOINT ,MARGE  ,
     3      I_MEM ,PROV_N ,PROV_E,ESHIFT,V      ,
     4      NSN   ,NRTM   ,GAP_S  ,GAP_M ,CURV_MAX,NIN  ,
     5      PENE_OLD,NBINFLG,MBINFLG,ILEV ,MSEGTYP,
     6      ITAB ,IGAP,GAP_S_L,GAP_M_L,ICODT,ISKEW,
     7      DRAD ,DGAPLOAD)
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
#include      "task_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_MEM, NSN, NRTM, NIN,NBINFLG(*),MBINFLG(*),ILEV, IGAP
      INTEGER J_STOK,MULNSN,NOINT,ESHIFT
      INTEGER IRECT(4,*),NSV(*),CAND_N(*),CAND_E(*)
      INTEGER PROV_N(MVSIZ),PROV_E(MVSIZ),II_STOK,MSEGTYP(*),
     .        ITAB(*),ICODT(*),ISKEW(*)
C     REAL
      my_real , INTENT(IN) :: DGAPLOAD ,DRAD
      my_real
     .        X(3,*), V(3,*), GAP_S(*), GAP_M(*),
     .        MARGE, CURV_MAX(*),PENE_OLD(5,NSN),
     .        GAP_S_L(*), GAP_M_L(*)
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
#include "sync_dir.inc"
C-----------------------------------------------
      CALL I25COR3T( J_STOK  ,X    ,IRECT   ,NSV   ,PROV_E  ,
     1               PROV_N  ,X1    ,X2      ,
     2               X3      ,X4   ,Y1      ,Y2    ,Y3      ,
     3               Y4      ,Z1   ,Z2      ,Z3    ,Z4      ,
     4               XI      ,YI   ,ZI      ,STIF  ,IX1     ,
     5               IX2     ,IX3  ,IX4     ,NSN   ,GAP_S   ,
     6               GAP_M   ,GAPV ,CURV_MAX,ITYPE ,NIN     ,
     7               V       ,IGAP ,GAP_S_L ,GAP_M_L,MSEGTYP,
     8               ETYP    ,ICODT   ,ISKEW,IBC    ,DRAD   ,
     9               DGAPLOAD)
C-----------------------------------------------
      CALL I25PEN3( J_STOK ,MARGE ,X1    ,X2     ,X3   ,
     .               X4    ,Y1    ,Y2    ,Y3     ,Y4   ,
     .               Z1    ,Z2    ,Z3    ,Z4     ,XI   ,
     .               YI    ,ZI    ,PENE  ,IX1    ,IX2  ,
     .               IX3   ,IX4   ,GAPV  ,NRTM   ,ETYP ,
     .               IBC   )
C-----------------------------------------------
      IF (ILEV==2) 
     .  CALL I25S1S2(J_STOK,NSN,ESHIFT,PROV_N,PROV_E,
     .               NBINFLG,MBINFLG,PENE)
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
Chd|  I25S1S2                       source/interfaces/intsort/i25sto.F
Chd|-- called by -----------
Chd|        I25STO                        source/interfaces/intsort/i25sto.F
Chd|-- calls ---------------
Chd|        BITGET                        source/interfaces/intsort/i20sto.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25S1S2(LLT  ,NSN  ,ESHIFT,PROV_N,PROV_E,
     .                   NBINFLG,MBINFLG,PENE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LLT,NSN,ESHIFT,PROV_N(*),PROV_E(*),NBINFLG(*),MBINFLG(*)
C     REAL
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,NE,IMS1,IMS2,ISS1,ISS2
C     REAL
C-----------------------------------------------
      my_real
     .   PENE(*)
      INTEGER BITGET
      EXTERNAL BITGET
C=======================================================================
         DO I=1,LLT
           N  = PROV_N(I)
           NE = PROV_E(I)+ESHIFT
           IMS1 = BITGET(MBINFLG(NE),0)
           IMS2 = BITGET(MBINFLG(NE),1)
           IF(N <= NSN) THEN
             ISS1 = BITGET(NBINFLG(N),0)
             ISS2 = BITGET(NBINFLG(N),1)
           ELSE
             ISS1 = BITGET(IREM(I24IREMP+6,N-NSN),0) 
             ISS2 = BITGET(IREM(I24IREMP+6,N-NSN),1) 
           ENDIF
           IF(.NOT.((IMS1 == 1 .and. ISS2==1).or.
     .              (IMS2 == 1 .and. ISS1==1)))THEN
             PENE(I)=ZERO
           ENDIF
         ENDDO
C
      RETURN
      END
