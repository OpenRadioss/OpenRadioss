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
Chd|  SHSUB2                        source/elements/shell/subcycling/shsub2.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SHSUB2(
     2  ITASK ,NODFT ,NODLT ,
     4  A     ,ASAVE ,AR    ,ARSAVE,
     5  FSAV  ,FSAVSAV )
C----6---------------------------------------------------------------7---------8
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
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITASK,NODFT,NODLT
      my_real
     .   A(3,*), ASAVE(3,*), AR(3,*), ARSAVE(3,*),
     .   FSAV(NTHVKI,*),FSAVSAV(NTHVKI,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,I
C-----------------------------------------------
C     on stocke dans ASAVE meme en Parith/ON
      IF(IPARIT==0)THEN
        DO N=NODFT,NODLT
          A(1,N) =A(1,N)+ASAVE(1,N)
          A(2,N) =A(2,N)+ASAVE(2,N)
          A(3,N) =A(3,N)+ASAVE(3,N)
          AR(1,N)=AR(1,N)+ARSAVE(1,N)
          AR(2,N)=AR(2,N)+ARSAVE(2,N)
          AR(3,N)=AR(3,N)+ARSAVE(3,N)
        ENDDO
      ELSE
        DO N=NODFT,NODLT
          A(1,N) =A(1,N)+ASAVE(1,N)
          A(2,N) =A(2,N)+ASAVE(2,N)
          A(3,N) =A(3,N)+ASAVE(3,N)
          AR(1,N)=AR(1,N)+ARSAVE(1,N)
          AR(2,N)=AR(2,N)+ARSAVE(2,N)
          AR(3,N)=AR(3,N)+ARSAVE(3,N)
        ENDDO
      ENDIF
C
      DO I=ITASK+1,NSECT,NTHREAD
        FSAV(1,I)=FSAV(1,I)+DT12*FSAVSAV(1,I)
        FSAV(2,I)=FSAV(2,I)+DT12*FSAVSAV(2,I)
        FSAV(3,I)=FSAV(3,I)+DT12*FSAVSAV(3,I)
        FSAV(4,I)=FSAV(4,I)+DT12*FSAVSAV(4,I)
        FSAV(5,I)=FSAV(5,I)+DT12*FSAVSAV(5,I)
        FSAV(6,I)=FSAV(6,I)+DT12*FSAVSAV(6,I)
        FSAV(7,I)=FSAV(7,I)+DT12*FSAVSAV(7,I)
        FSAV(8,I)=FSAV(8,I)+DT12*FSAVSAV(8,I)
        FSAV(9,I)=FSAV(9,I)+DT12*FSAVSAV(9,I)
      END DO
      RETURN
      END
