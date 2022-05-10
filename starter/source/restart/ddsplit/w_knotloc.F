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
C
Chd|====================================================================
Chd|  W_KNOTLOC                     source/restart/ddsplit/w_knotloc.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        W_KNOTLOCPC                   source/restart/ddsplit/w_knotlocpc.F
Chd|====================================================================
      SUBROUTINE W_KNOTLOC(KXIG3D,NUMELIG3D_L,ISUMNIG3D_L,IXIG3D,CEP,
     +                  PROC,NODLOCAL,LEN_IA,KNOTLOCPC,KNOTLOCEL,NUMNOD_L)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr23_c.inc"
#include      "rnur_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMELIG3D_L, PROC, ISUMNIG3D_L,LEN_IA,
     .        KXIG3D(NIXIG3D,*), CEP(*), NODLOCAL(*), IXIG3D(*),NUMNOD_L
      my_real
     .        KNOTLOCPC(*),KNOTLOCEL(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NN, IAD, IAD_L, J, NB_L, NNOD, IE_L, OFF,
     .        KXIG3D_L(NIXIG3D,NUMELIG3D_L), IXIG3D_L(ISUMNIG3D_L),DECALGEO,MAXGEO
      my_real
     .        KNOTLOCPC_L(SKNOTLOCPC),KNOTLOCEL_L(SKNOTLOCEL)
C
      OFF = NUMELC+NUMELTG+NUMELS+NUMELQ+NUMELT+NUMELP+NUMELR+NUMELUR+
     .      NUMCONV+NUMRADIA+SLLOADP/4
      IE_L = 0
      MAXGEO=1
C
      DO I = 1, NUMELIG3D
c        IF(CEP(OFF+I).EQ.PROC) THEN
          MAXGEO= MAX(MAXGEO,KXIG3D(2,I))
C
          NNOD  = KXIG3D(3,I)
          IAD   = KXIG3D(4,I)
C
          IE_L = IE_L + 1
          DO J = 1, NIXIG3D
            KXIG3D_L(J,IE_L) = KXIG3D(J,I)
          ENDDO
c          DO J = 1, 6 !2*3
c            KNOTLOCEL_L((IE_L-1)*6+J) = KNOTLOCEL(J,KXIG3D(J,I))
c          ENDDO
c        ENDIF
      ENDDO

      DO J = 1, SKNOTLOCEL
        KNOTLOCEL_L(J) = KNOTLOCEL(J)
      ENDDO
C
      KNOTLOCPC_L = 0  ! il faut que els autre cases knotlocpc vides pour les neodus soient mises a 0

      CALL W_KNOTLOCPC(KNOTLOCPC,KNOTLOCPC_L,MAXGEO,NUMNOD_L,NODLOCAL)
C
      CALL WRITE_DB(KNOTLOCPC_L,SKNOTLOCPC)
      CALL WRITE_DB(KNOTLOCEL_L,SKNOTLOCEL)
C
      RETURN
      END
