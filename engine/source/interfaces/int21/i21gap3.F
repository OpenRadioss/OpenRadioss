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
Chd|  I21GAP3                       source/interfaces/int21/i21gap3.F
Chd|-- called by -----------
Chd|        I21MAIN_GAP                   source/interfaces/int21/i21main_gap.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I21GAP3(
     1 ITASK ,IRECTS ,NRTS ,NSN   ,NSV   ,
     2 GAP   ,IGAP  ,GAP_S ,GAPMIN,CRITER,
     3 GAPMAX,TZINF ,DIST  ,THKNOD,GAPSCALE,
     4 DEPTH ,MARGE ,DRAD  ,IDEL  ,THKNOD0,
     5 GAP_S0,DGAPLOAD)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITASK, NRTS, NSN, IGAP, IDEL
      INTEGER IRECTS(4,*), NSV(*)
C     REAL
      my_real
     .   GAP, GAPMIN, CRITER, GAPMAX, TZINF, DIST, GAPSCALE,
     .   DEPTH, MARGE
      my_real , INTENT(IN) :: DGAPLOAD ,DRAD
      my_real
     .   GAP_S(*), THKNOD(*), GAP_S0(*), THKNOD0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NSNF, NSNL, I, J, NELS, NELC, NELTG, NEL
C     REAL
      my_real
     .   GAPM, CRITERL, GAPSUPL
C------------------------------------
C     GAP VARIABLE NOEUDS SECONDS
C------------------------------------
      NSNF = 1 + ITASK*NSN / NTHREAD
      NSNL = (ITASK+1)*NSN / NTHREAD
      
      IF(IGAP>=2)THEN
       DO I=NSNF,NSNL
        GAP_S(I) = MAX(HALF*GAPSCALE*(THKNOD(NSV(I))-THKNOD0(I))
     .                                            +GAP_S0(I),ZERO)
       ENDDO
      ENDIF
C---------------------------------------------
C
C Calcul du gap reel a utiliser lors du critere de retri
C
      IF (IGAP==0) THEN
        CRITER =MAX(GAP,EM01*MAX(DEPTH,DRAD,GAP+DGAPLOAD))
        DIST   =TZINF-MAX(GAP+DGAPLOAD,DEPTH,DRAD)
      ELSE
        CRITERL=EP30
        GAPSUPL=ZERO
        DO I = NSNF,NSNL
          IF(IDEL==0.OR.GAP_S(I)/=ZERO)THEN
C gap_s/=0 <=> node is still connected through non deleted shells
            CRITERL = MIN(CRITERL,GAP_S(I))
            GAPSUPL = MAX(GAPSUPL,GAP_S(I))
          END IF
        ENDDO
        CRITERL=MAX(MAX(GAPMIN,CRITERL),EM01*MAX(DEPTH,DRAD,GAPSUPL+DGAPLOAD))
        GAPSUPL=MIN(GAPMAX,GAPSUPL)
#include "lockon.inc"
        CRITER=MIN(CRITER,CRITERL)
        DIST  =MIN(DIST,TZINF-MAX(GAPSUPL+DGAPLOAD,DEPTH,DRAD))
        IF( TZINF-GAPSUPL <= ZERO)THEN
C assure le retri et remet la marge initiale (on a forcement depth  <= gapsupl)
          TZINF= GAPSUPL+MARGE
          DIST =-ONE
        END IF
#include "lockoff.inc"
      ENDIF      
C------------------------------------
      RETURN
      END
