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
Chd|  W_ITHFLUX                     source/restart/ddsplit/w_ithflux.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|====================================================================
      SUBROUTINE W_ITHFLUX(IBFFLUX,CEP,PROC,NODLOCAL,NFXFLUX_L,LEN_IA,CEL,IXS)

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "thermal_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEN_IA, PROC, NFXFLUX_L, IBFFLUX(NITFLUX,*),CEP(*)
      INTEGER NODLOCAL(*), CEL(*), IXS(NIXS,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IBTMP(NITFLUX,NFXFLUX_L), N1, N2, N3, N4, OFF, NB_L
      INTEGER N, NEL, NUSR
C
      OFF = NUMELC+NUMELTG+NUMELS+NUMELQ+NUMELT+NUMELP+NUMELR
     +     +NUMELX+NCONLD+NUMCONV+NUMRADIA
      NB_L = 0
C
      DO I = 1, NFXFLUX
        IF(CEP(I+OFF)==PROC) THEN
           NB_L = NB_L + 1
           IF(IBFFLUX(10,I) == 0) THEN ! SURFACE FLUX
             N1 = IBFFLUX(1,I)
             N2 = IBFFLUX(2,I)
             N3 = IBFFLUX(3,I)
             N4 = IBFFLUX(4,I)
             IBTMP(1,NB_L) = NODLOCAL(N1)
             IBTMP(2,NB_L) = NODLOCAL(N2)
             IBTMP(3,NB_L) = NODLOCAL(N3)
             IBTMP(4,NB_L) = 0
             IF(N4 /= 0) IBTMP(4,NB_L) = NODLOCAL(N4)
             DO J = 5, NITFLUX
               IBTMP(J,NB_L) = IBFFLUX(J,I)
             ENDDO
           ELSEIF(IBFFLUX(10,I) == 1) THEN ! VOLUMIC FLUX
             DO J = 1,NITFLUX
               IBTMP(J,NB_L) = IBFFLUX(J,I)
             ENDDO
             IBTMP(8,NB_L) = CEL(IBTMP(8,NB_L)) ! id local to proc
           ENDIF
        ENDIF
      ENDDO
C
      CALL WRITE_I_C(IBTMP,NITFLUX*NFXFLUX_L)
      LEN_IA = LEN_IA + NITFLUX*NFXFLUX_L
C
      RETURN
      END




Chd|====================================================================
Chd|  APPLYSORT2FLUX                source/restart/ddsplit/w_ithflux.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE APPLYSORT2FLUX(IBFFLUX,SIZ1,SIZ2,PERMUTATIONS)
C Apply the new numbering (given in permutations) to the elements IBFFLUX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------

C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER SIZ1,SIZ2
      INTEGER PERMUTATIONS(*)
      INTEGER IBFFLUX(SIZ1,SIZ2)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J
C=======================================================================
      DO I = 1,SIZ2
        IF(IBFFLUX(8,I)<= 0) CYCLE
        IBFFLUX(8,I) = PERMUTATIONS(IBFFLUX(8,I))
      END DO
      RETURN
      END
