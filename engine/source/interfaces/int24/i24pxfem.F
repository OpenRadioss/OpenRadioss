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
Chd|  I24PXFEM                      source/interfaces/int24/i24pxfem.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        I24GAP                        source/interfaces/int24/i24pxfem.F
Chd|        SPMD_EXCH_I24_GAP             source/mpi/interfaces/spmd_exch_i24_gap.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I24PXFEM(
     1   IPARI    ,INTBUF_TAB,WAGAP ,IAD_ELEM,FR_ELEM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      USE INTBUFDEF_MOD   
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com09_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "scr03_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), IAD_ELEM(2,*),FR_ELEM(*)
C     REAL
      my_real
     .   WAGAP(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER SIZE,LENR ,ITY,NG,NRTM,INTPLY
      my_real
     .  GAPMAX,DGAPMAX
C=======================================================================
                                                   
C------------------------------------------------------------
      IF (NSPMD > 1 ) THEN
C---                                                          
        SIZE = 2        
        LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
        CALL SPMD_EXCH_I24_GAP( WAGAP,IAD_ELEM,FR_ELEM,SIZE  ,LENR )  
      ENDIF                                                   
C------------------------------------------------------------
      DO NG=1,NINTER
            GAPMAX = ZERO
            DGAPMAX = ZERO
            ITY = IPARI(7,NG)
            INTPLY = IPARI(66,NG)
            IF(ITY == 24 .AND. INTPLY > 0) THEN
              NRTM =IPARI(4,NG)
              CALL I24GAP(
     1       NRTM  ,INTBUF_TAB(NG)%IRECTM,WAGAP, INTBUF_TAB(NG)%GAP_NM,
     .       INTBUF_TAB(NG)%GAP_M,
     2       INTBUF_TAB(NG)%MSEGTYP24,GAPMAX,INTBUF_TAB(NG)%NVOISIN,
     .       INTBUF_TAB(NG)%DGAP_NM,
     3       INTBUF_TAB(NG)%DGAP_M,DGAPMAX,INTBUF_TAB(NG)%STFM)
#include "lockon.inc"
              INTBUF_TAB(NG)%VARIABLES(16) = 
     .               MAX(GAPMAX,INTBUF_TAB(NG)%VARIABLES(16))
              INTBUF_TAB(NG)%DELTA_PMAX_DGAP(1) = DGAPMAX
#include "lockoff.inc"   
            END IF
      END DO
C      
      RETURN
      END
C
Chd|====================================================================
Chd|  I24GAP                        source/interfaces/int24/i24pxfem.F
Chd|-- called by -----------
Chd|        I24PXFEM                      source/interfaces/int24/i24pxfem.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I24GAP(
     1  NRTM,IRECT,WAGAP,GAP_MN,GAP_M,MSEGTYP, GAPMAX,NVOISIN,DGAP_MN,
     .  DGAP_M ,DGAPMAX,STFM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTM, IRECT(4,*),MSEGTYP(*), NVOISIN(8,*)
      my_real
     .        XMAX, YMAX, ZMAX, XMIN, YMIN, ZMIN, C_MAX,
     .        GAP_MN(12,*),WAGAP(2,*),GAP_M(*),GAPMAX,DGAP_MN(4,*),
     .        DGAP_M(*),DGAPMAX,STFM(*)
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  II,M1,M2,M3,M4,NE,L
      my_real
     .    GAPN_OLD(4),GAP_MMAX
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C    
C
      DO NE=1,NRTM
        IF(STFM(NE) == ZERO) CYCLE
        GAP_MMAX = ZERO
        M1 = IRECT(1,NE)
        M2 = IRECT(2,NE)
        M3 = IRECT(3,NE)
        M4 = IRECT(4,NE)
C        
        GAPN_OLD(1) = GAP_MN(1,NE)
        GAPN_OLD(2) = GAP_MN(2,NE)  
        GAPN_OLD(3) = GAP_MN(3,NE)  
        GAPN_OLD(4) = GAP_MN(4,NE)   
C
        IF(MSEGTYP(NE) > 0 ) THEN 
C              
            GAP_MN(1,NE) =MAX(GAP_MN(1,NE), WAGAP(1,M1))
            GAP_MMAX = MAX(GAP_MMAX,GAP_MN(1,NE))
            GAP_MN(2,NE) = MAX(GAP_MN(2,NE), WAGAP(1,M2)) 
            GAP_MMAX = MAX(GAP_MMAX,GAP_MN(2,NE))
            GAP_MN(3,NE) = MAX(GAP_MN(3,NE), WAGAP(1,M3))
            GAP_MMAX = MAX(GAP_MMAX,GAP_MN(3,NE))
            GAP_MN(4,NE) = MAX(GAP_MN(4,NE), WAGAP(1,M4))
            GAP_MMAX = MAX(GAP_MMAX,GAP_MN(4,NE))
! voisin
            L = IABS(NVOISIN(1,NE)) 
           IF(L > 0) GAP_MN(5,NE) =MAX(GAP_MN(5,NE), WAGAP(1,L)) 
           L = IABS(NVOISIN(2,NE)) 
           IF(L > 0) GAP_MN(6,NE) =MAX(GAP_MN(6,NE), WAGAP(1,L)) 
           L = IABS(NVOISIN(3,NE)) 
           IF(L > 0) GAP_MN(7,NE) =MAX(GAP_MN(7,NE), WAGAP(1,L))
           L = IABS(NVOISIN(4,NE)) 
           IF(L > 0) GAP_MN(8,NE) =MAX(GAP_MN(8,NE), WAGAP(1,L))
           L = IABS(NVOISIN(5,NE)) 
           IF(L > 0) GAP_MN(9,NE) =MAX(GAP_MN(9,NE), WAGAP(1,L))
            L = IABS(NVOISIN(6,NE)) 
           IF(L > 0) GAP_MN(10,NE) =MAX(GAP_MN(10,NE), WAGAP(1,L))
           L = IABS(NVOISIN(7,NE))
           IF(L > 0) GAP_MN(11,NE) =MAX(GAP_MN(11,NE), WAGAP(1,L))
           L = IABS(NVOISIN(8,NE)) 
           IF(L > 0) GAP_MN(12,NE) =MAX(GAP_MN(12,NE), WAGAP(1,L))
        ELSEIF(MSEGTYP(NE) < 0) THEN
           GAP_MN(1,NE) =MAX(GAP_MN(1,NE), WAGAP(2,M1))
           GAP_MMAX = MAX(GAP_MMAX,GAP_MN(1,NE))
           GAP_MN(2,NE) = MAX(GAP_MN(2,NE), WAGAP(2,M2))
           GAP_MMAX = MAX(GAP_MMAX,GAP_MN(2,NE))
           GAP_MN(3,NE) = MAX(GAP_MN(3,NE), WAGAP(2,M3))
           GAP_MMAX = MAX(GAP_MMAX,GAP_MN(3,NE))
           GAP_MN(4,NE) = MAX(GAP_MN(4,NE), WAGAP(2,M4))
           GAP_MMAX = MAX(GAP_MMAX,GAP_MN(4,NE))
C            
           L = IABS(NVOISIN(1,NE)) 
           IF(L > 0) GAP_MN(5,NE) =MAX(GAP_MN(5,NE), WAGAP(2,L))
           L = IABS(NVOISIN(2,NE)) 
           IF(L > 0) GAP_MN(6,NE) =MAX(GAP_MN(6,NE), WAGAP(2,L))
           L = IABS(NVOISIN(3,NE)) 
           IF(L > 0) GAP_MN(7,NE) =MAX(GAP_MN(7,NE), WAGAP(2,L))
           L = IABS(NVOISIN(4,NE)) 
           IF(L > 0) GAP_MN(8,NE) =MAX(GAP_MN(8,NE), WAGAP(2,L))
           L = IABS(NVOISIN(5,NE)) 
           IF(L > 0) GAP_MN(9,NE) =MAX(GAP_MN(9,NE), WAGAP(2,L))
            L = IABS(NVOISIN(6,NE)) 
           IF(L > 0) GAP_MN(10,NE) =MAX(GAP_MN(10,NE), WAGAP(2,L))
            L = IABS(NVOISIN(7,NE)) 
           IF(L > 0) GAP_MN(11,NE) =MAX(GAP_MN(11,NE), WAGAP(2,L))
            L = IABS(NVOISIN(8,NE)) 
           IF(L > 0) GAP_MN(12,NE) =MAX(GAP_MN(12,NE), WAGAP(2,L))
          ENDIF          
           GAP_M(NE) = GAP_MMAX 
           GAPMAX = MAX(GAPMAX,GAP_MMAX)
           DGAP_MN(1,NE)=  GAP_MN(1,NE) - GAPN_OLD(1)
           DGAP_MN(2,NE)=  GAP_MN(2,NE) - GAPN_OLD(2)
           DGAP_MN(3,NE)=  GAP_MN(3,NE) - GAPN_OLD(3)
           DGAP_MN(4,NE)=  GAP_MN(4,NE) - GAPN_OLD(4)
           DGAP_M(NE) = MAX (DGAP_MN(1,NE), DGAP_MN(2,NE),
     .                       DGAP_MN(3,NE), DGAP_MN(4,NE))
#include "lockon.inc"       
           DGAPMAX = MAX(DGAPMAX, DGAP_M(NE))  
#include "lockoff.inc"                   
       ENDDO
C
      RETURN
      END
      
