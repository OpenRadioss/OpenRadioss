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
Chd|  S10CNDV                       source/elements/solid/solide10/s10cndv.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10CNDV(ICNDS10 ,VND   ,V     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ICNDS10(3,*)
        my_real
     .     V(3,*),VND(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,ND,N1,N2
        my_real
     .     VX,VY,VZ
C======================================================================|
        DO I=1,NS10E
          ND  = ICNDS10(1,I)
          N1  = ICNDS10(2,I)
          N2  = ICNDS10(3,I)
          VX = HALF*(V(1,N1) + V(1,N2))
          VY = HALF*(V(2,N1) + V(2,N2))
          VZ = HALF*(V(3,N1) + V(3,N2))
          IF (ND<0) THEN
            ND = -ND
            V(1,ND) =  VX
            V(2,ND) =  VY
            V(3,ND) =  VZ
          ELSE
            V(1,ND) = V(1,ND)-VND(1,I) + VX
            V(2,ND) = V(2,ND)-VND(2,I) + VY
            V(3,ND) = V(3,ND)-VND(3,I) + VZ
          END IF
          VND(1,I) = VX
          VND(2,I) = VY
          VND(3,I) = VZ
        END DO
C----6---------------------------------------------------------------7---------8
        RETURN
 1005   FORMAT(3X,1PE10.3)
      END
Chd|====================================================================
Chd|  S10MVND                       source/elements/solid/solide10/s10cndv.F
Chd|-- called by -----------
Chd|        SORTIE_MAIN                   source/output/sortie_main.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10MVND(ICNDS10 ,VND   ,V     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ICNDS10(3,*)
        my_real
     .     V(3,*),VND(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,ND,N1,N2
        my_real
     .     VX,VY,VZ
C======================================================================|
C---relative velocity for KE compute----
        DO I=1,NS10E
          ND  = ICNDS10(1,I)
          IF (ND>0) THEN
            V(1,ND) = V(1,ND)-VND(1,I)
            V(2,ND) = V(2,ND)-VND(2,I)
            V(3,ND) = V(3,ND)-VND(3,I)
          ELSEIF (ND<0) THEN
            ND = -ND
            V(1,ND) = ZERO
            V(2,ND) = ZERO
            V(3,ND) = ZERO
          END IF
        END DO
C----6---------------------------------------------------------------7---------8
        RETURN
      END
Chd|====================================================================
Chd|  S10PRTV                       source/elements/solid/solide10/s10cndv.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10PRTV(ICNDS10 ,V     ,itab)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ICNDS10(3,*),itab(*)
        my_real
     .     V(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,ND,N1,N2
C======================================================================|
        DO I=1,NS10E
          ND  = iabs(ICNDS10(1,I))
          N1  = ICNDS10(2,I)
          N2  = ICNDS10(3,I)
          write(iout,*)'ND,N1,N2,ND_id,I=',itab(ND),itab(N1),itab(N2),ICNDS10(1,I),I
          write(iout,*)V(1,ND),V(1,N1) , V(1,N2)
          write(iout,*)V(2,ND),V(2,N1) , V(2,N2)
          write(iout,*)V(3,ND),V(3,N1) , V(3,N2)
        END DO
C----6---------------------------------------------------------------7---------8
        RETURN
      END
Chd|====================================================================
Chd|  S10CNDI2A                     source/elements/solid/solide10/s10cndv.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10CNDI2A(ICNDS10 ,ITAGND ,A      )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ICNDS10(3,*),ITAGND(*)
        my_real
     .     A(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,ND,N1,N2
        my_real
     .     AX,AY,AZ
C======================================================================|
C----for Nd as main of int2 only; Ad-> acce absolu
        DO I=1,NS10E
          ND  = ICNDS10(1,I)
          IF (IABS(ITAGND(IABS(ND)))>NS10E) THEN
            N1  = ICNDS10(2,I)
            N2  = ICNDS10(3,I)
            AX = HALF*(A(1,N1) + A(1,N2))
            AY = HALF*(A(2,N1) + A(2,N2))
            AZ = HALF*(A(3,N1) + A(3,N2))
            IF (ND<0) THEN
              ND = -ND
              A(1,ND) =  AX
              A(2,ND) =  AY
              A(3,ND) =  AZ
            ELSE
              A(1,ND) = A(1,ND) + AX
              A(2,ND) = A(2,ND) + AY
              A(3,ND) = A(3,ND) + AZ
            END IF
          END IF !(IABS(ITAGND(IABS(ND)))>NS10E) THEN
        END DO
C----6---------------------------------------------------------------7---------8
        RETURN
      END
Chd|====================================================================
Chd|  S10CNDI2A1                    source/elements/solid/solide10/s10cndv.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10CNDI2A1(ICNDS10 ,ITAGND ,A      )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ICNDS10(3,*),ITAGND(*)
C     REAL
        my_real
     .     A(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,ND,N1,N2
C     REAL
        my_real
     .     AX,AY,AZ
C======================================================================|
C----for Nd as main of int2 only; Ad-> acce relative
        DO I=1,NS10E
          ND  = ICNDS10(1,I)
          IF (ND>0.AND.IABS(ITAGND(IABS(ND)))>NS10E) THEN
            N1  = ICNDS10(2,I)
            N2  = ICNDS10(3,I)
            AX = HALF*(A(1,N1) + A(1,N2))
            AY = HALF*(A(2,N1) + A(2,N2))
            AZ = HALF*(A(3,N1) + A(3,N2))
            A(1,ND) = A(1,ND) - AX
            A(2,ND) = A(2,ND) - AY
            A(3,ND) = A(3,ND) - AZ
          END IF !(IABS(ITAGND(IABS(ND)))>NS10E) THEN
        END DO
C----6---------------------------------------------------------------7---------8
        RETURN
      END
Chd|====================================================================
Chd|  S10GETVDM                     source/elements/solid/solide10/s10cndv.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10GETVDM(ICNDS10 ,V,VND,VMD)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ICNDS10(3,*)
C     REAL
        my_real
     .     V(3,*),VND(3,*),VMD(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,ND,N1,N2
C     REAL
        my_real
     .     VX,VY,VZ
C======================================================================|
        VMD(1:3,1:NUMNOD) = V(1:3,1:NUMNOD)
        DO I=1,NS10E
          ND  = IABS(ICNDS10(1,I))
          VMD(1:3,ND) = V(1:3,ND)-VND(1:3,I)
        END DO
C----6---------------------------------------------------------------7---------8
        RETURN
      END SUBROUTINE S10GETVDM
