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
Chd|  I25GAP3                       source/interfaces/int25/i25gap3.F
Chd|-- called by -----------
Chd|        I25MAIN_GAP                   source/interfaces/int25/i25main_gap.F
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|====================================================================
      SUBROUTINE I25GAP3(
     1 ITASK,
     2 NRTM  ,IRECT   ,GAP_NM   ,GAP_M   ,
     3 NMN   ,MSR     ,GAPN_M   ,
     4 GAPMAX_M ,GAPSCALE ,MSEGTYP , THKNOD, 
     5 GAPMSAV, MAXDGAP_G)        
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "thknod_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTM,IRECT(4,*),    
     .   MSR(*),NMN,MSEGTYP(*),ITASK
C     REAL
      my_real
     .   GAP_NM(4,*),GAP_M(*), GAPMSAV(*), THKNOD(NUMNOD),
     .   GAPN_M(*),GAPMAX_M, GAPSCALE, MAXDGAP_G
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,IW,I1,I2,I3,MG,M,IP,IGTYP,
     .        NMNF,NMNL,NRTMF,NRTML,IERROR
      my_real,
     .       DIMENSION(:), ALLOCATABLE :: WA
      my_real MAXDGAP_L
      SAVE WA
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      ALLOCATE(WA(NUMNOD),STAT=IERROR)
      NMNF = 1 + ITASK*NMN / NTHREAD
      NMNL = (ITASK+1)*NMN / NTHREAD
      NRTMF = 1 + ITASK * NRTM / NTHREAD
      NRTML = (ITASK+1) * NRTM / NTHREAD
      MAXDGAP_L = -EP30
C------------------------------------
C GAP_S = Secnds nodes gap [NSN]
C GAPN_M = gap for each node on main [NMN]
C GAP_NM = gap of each node on each main segment [4*NRTM]
C GAP_M = for each main segment, max of gap of its connected nodes [NRTM]

#include "vectorize.inc"
      DO I=NMNF,NMNL
        M = MSR(I)
        WA(M)=HALF*GAPSCALE*THKNOD(M)
      END DO
C
      CALL MY_BARRIER
C
!$OMP SINGLE
#include "vectorize.inc"
      DO I=1,NRTM
        IF (MSEGTYP(I)==0) THEN
          DO J=1,4
            M=IRECT(J,I)
            WA(M) = ZERO
          END DO
        END IF 
      END DO
!$OMP END SINGLE 
C
#include "vectorize.inc"
      DO I=NMNF,NMNL
        M = MSR(I)
        WA(M) = MIN(WA(M),GAPMAX_M)
        GAPN_M(I) = WA(M)
      END DO
C
      CALL MY_BARRIER
C
#include "vectorize.inc" 
      DO I=NRTMF,NRTML 
        GAP_M(I) = ZERO
        DO J=1,4
          M=IRECT(J,I)
          GAP_NM(J,I)=WA(M)
          GAP_M(I) = MAX(GAP_M(I),WA(M))
        END DO
      END DO
C
#include "vectorize.inc"
C calculate the maximum change in the gap, to be used in sorting criteria
      DO I=NRTMF,NRTML
        MAXDGAP_L = MAX(MAXDGAP_L,GAP_M(I)-GAPMSAV(I))
      END DO
C
      CALL MY_BARRIER
C
#include "lockon.inc"
C obtain the max dgap for all
      MAXDGAP_G = MAX(MAXDGAP_L,MAXDGAP_G)
#include "lockoff.inc"      

      RETURN
      END
C
