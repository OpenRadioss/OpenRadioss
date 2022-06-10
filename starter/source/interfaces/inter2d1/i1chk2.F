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
Chd|  I1CHK2                        source/interfaces/inter2d1/i1chk2.F
Chd|-- called by -----------
Chd|        ININT2                        source/interfaces/inter2d1/inint2.F
Chd|-- calls ---------------
Chd|        INORI2                        source/interfaces/inter2d1/inori2.F
Chd|        INRCH2                        source/interfaces/inter2d1/inrch2.F
Chd|        LOCAL_INDEX                   source/interfaces/interf1/local_index.F
Chd|====================================================================
      SUBROUTINE I1CHK2(X,IRECT,IXQ,NRT,NINT,NSN,NSV,NOINT,ID,TITR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include     "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRT, NINT, NSN, NOINT
      INTEGER IRECT(4,*), IXQ(*), NSV(*)
C     REAL
      my_real
     .   X(3,*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, NEL, IG, IL
      my_real
     . YM1, YM2, ZM1, ZM2, YE(4) ,ZE(4)
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C
      DO I=1,NRT
        II=I
        CALL INRCH2(X, IRECT, IXQ, II, NEL,
     .              NINT, NOINT, YM1, YM2, ZM1,
     .              ZM2 ,YE   ,ZE, ID, TITR)
        CALL INORI2(X,IRECT,II,NEL,NINT,
     .              NOINT, YM1, YM2, ZM1, ZM2 ,
     .              YE   , ZE )
      ENDDO
C-----------------------------------------------------
C     MISE DANS IRECT DU NUMERO LOCAL DU NOEUD
C-----------------------------------------------------
      DO I=1,NRT
        IG=IRECT(1,I)
        CALL LOCAL_INDEX(IL,IG,NSV,NSN)
        IRECT(1,I)=IL
        IG=IRECT(2,I)
        CALL LOCAL_INDEX(IL,IG,NSV,NSN)
        IRECT(2,I)=IL
      ENDDO
C
      RETURN
      END
