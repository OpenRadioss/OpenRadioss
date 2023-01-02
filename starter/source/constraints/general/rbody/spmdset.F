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
Chd|  SPMDSET                       source/constraints/general/rbody/spmdset.F
Chd|-- called by -----------
Chd|        HM_READ_RBODY                 source/constraints/general/rbody/hm_read_rbody.F
Chd|        TRIRBMERGE                    source/constraints/general/merge/hm_read_merge.F
Chd|-- calls ---------------
Chd|        IFRONTPLUS                    source/spmd/node/frontplus.F  
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE SPMDSET(N,NPBY,NNPBY,LPBY,NSL,K)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N,NPBY(NNPBY,*),LPBY(*), NNPBY, NSL, K
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER P,J,FMAIN(NSPMD),S_FMAIN
C-----------------------------------------------------------------
      DO P = 1, NSPMD
        FMAIN(P) = 0
      ENDDO
      S_FMAIN = 0
      DO P = 1, NSPMD
        DO J = 1, NSL
          IF (NLOCAL(LPBY(J+K),P)/=0)THEN
            FMAIN(P) = 1
            S_FMAIN = S_FMAIN + 1
            GO TO 85
          ENDIF
        ENDDO
85      CONTINUE
      END DO
C rigid body // noeud main sur les procs ayant au moins 1 secnd
      DO P = 1, NSPMD
        IF (FMAIN(P) == 1) THEN
          CALL IFRONTPLUS(NPBY(1,N),P)
        ENDIF
      ENDDO
C rigid body // les secnds nodes ne sont pas connectes, on affecte arbitrairement le main sur le proc0
      IF (S_FMAIN==0) CALL IFRONTPLUS(NPBY(1,N),1)
      IF(NPBY(8,N)/=0)THEN
        CALL IFRONTPLUS(NPBY(1,N),1)
      ENDIF
c-----------
      RETURN
      END
