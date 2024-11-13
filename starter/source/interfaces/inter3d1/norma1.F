Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      !||====================================================================
      !||    norma1      ../starter/source/interfaces/inter3d1/norma1.F
      !||--- called by ------------------------------------------------------
      !||    findele     ../starter/source/boundary_conditions/ebcs/findele.F
      !||    i11sti3     ../starter/source/interfaces/inter3d1/i11sti3.F
      !||    i12sol3     ../starter/source/interfaces/inter3d1/insol3.F
      !||    i20nelts    ../starter/source/interfaces/inter3d1/inelt.F
      !||    i2surfs     ../starter/source/interfaces/inter3d1/i2surfs.F
      !||    ineltigeo   ../starter/source/interfaces/inter3d1/inelt.F
      !||    inelts      ../starter/source/interfaces/inter3d1/inelt.F
      !||    inelts_np   ../starter/source/interfaces/inter3d1/i24sti3.F
      !||    insol3      ../starter/source/interfaces/inter3d1/insol3.F
      !||====================================================================
      SUBROUTINE NORMA1(N1,N2,N3,AREA,XX1,XX2,XX3)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   N1, N2, N3, AREA
      my_real, INTENT(IN) :: XX1(4),XX2(4),XX3(4)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   XX13, YY13, ZZ13, XX24, YY24, ZZ24
C
      XX13 =XX1(3)-XX1(1)
      YY13 =XX2(3)-XX2(1)
      ZZ13 =XX3(3)-XX3(1)
      XX24 =XX1(4)-XX1(2)
      YY24 =XX2(4)-XX2(2)
      ZZ24 =XX3(4)-XX3(2)
C
      N1=YY13*ZZ24-ZZ13*YY24
      N2=ZZ13*XX24-XX13*ZZ24
      N3=XX13*YY24-YY13*XX24
      AREA= MAX(EM20,SQRT(N1*N1+N2*N2+N3*N3))
      N1=N1/AREA
      N2=N2/AREA
      N3=N3/AREA
      AREA=HALF*AREA
      RETURN
      END



      !||====================================================================
      !||    norma1d   ../starter/source/interfaces/inter3d1/norma1.F
      !||--- called by ------------------------------------------------------
      !||    insol3d   ../starter/source/interfaces/inter3d1/insol3.F
      !||====================================================================
      SUBROUTINE NORMA1D(N1,N2,N3,AREA,XX1,XX2,XX3)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   N1, N2, N3, AREA
      my_real, INTENT(IN) :: XX1(4),XX2(4),XX3(4)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J
      my_real
     .   XX13, YY13, ZZ13, XX24, YY24, ZZ24
C
      XX13 =XX1(3)-XX1(1)
      YY13 =XX2(3)-XX2(1)
      ZZ13 =XX3(3)-XX3(1)
      XX24 =XX1(4)-XX1(2)
      YY24 =XX2(4)-XX2(2)
      ZZ24 =XX3(4)-XX3(2)
C
      N1=YY13*ZZ24-ZZ13*YY24
      N2=ZZ13*XX24-XX13*ZZ24
      N3=XX13*YY24-YY13*XX24
      AREA= MAX(EM20,SQRT(N1*N1+N2*N2+N3*N3))
      N1=N1/AREA
      N2=N2/AREA
      N3=N3/AREA
C
      AREA=HALF*AREA
      RETURN
      END



      !||====================================================================
      !||    norma4n                  ../starter/source/interfaces/inter3d1/norma1.F
      !||--- called by ------------------------------------------------------
      !||    i25neigh_removeallbut1   ../starter/source/interfaces/inter3d1/i25neigh.F
      !||    removeic                 ../starter/source/interfaces/inter3d1/i24tools.F
      !||    removeic1                ../starter/source/interfaces/inter3d1/i24tools.F
      !||    seg_opp                  ../starter/source/interfaces/inter3d1/i24tools.F
      !||    select_s2s               ../starter/source/interfaces/inter3d1/select_s2s.F90
      !||    sh_offset_jonct_chk      ../starter/source/elements/shell/shell_offset/sh_offset_jonkt_chk.F90
      !||    voisin2                  ../starter/source/interfaces/inter3d1/i24tools.F
      !||====================================================================
      SUBROUTINE NORMA4N(N1,N2,N3,AREA,IRECT,X)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4)
      my_real
     .   N1, N2, N3, AREA, X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,NN
C     REAL
      my_real
     .   X1(4),X2(4),X3(4),XX13, YY13, ZZ13, XX24, YY24, ZZ24
C
       DO J=1,4
        NN=IRECT(J)
        X1(J)=X(1,NN)
        X2(J)=X(2,NN)
        X3(J)=X(3,NN)
       END DO 
C       
      XX13 =X1(3)-X1(1)
      YY13 =X2(3)-X2(1)
      ZZ13 =X3(3)-X3(1)
      XX24 =X1(4)-X1(2)
      YY24 =X2(4)-X2(2)
      ZZ24 =X3(4)-X3(2)
C
      N1=YY13*ZZ24-ZZ13*YY24
      N2=ZZ13*XX24-XX13*ZZ24
      N3=XX13*YY24-YY13*XX24
      AREA= MAX(EM20,SQRT(N1*N1+N2*N2+N3*N3))
      N1=N1/AREA
      N2=N2/AREA
      N3=N3/AREA
C
      AREA=HALF*AREA
C      
      RETURN
      END
      !||====================================================================
      !||    norma4nx   ../starter/source/interfaces/inter3d1/norma1.F
      !||--- called by ------------------------------------------------------
      !||    voisin2    ../starter/source/interfaces/inter3d1/i24tools.F
      !||====================================================================
      SUBROUTINE NORMA4NX(N1,N2,N3,AREA,IRECT,X ,I1,I2,SHF)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4),I1,I2
      my_real
     .   N1, N2, N3, AREA, X(3,*),SHF(3)
C-----------------------------------------------
c FUNCTION: calculate normale with perturbation
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,NN
C     REAL
      my_real
     .   X1(4),X2(4),X3(4),XX13, YY13, ZZ13, XX24, YY24, ZZ24
C
       DO J=1,4
        NN=IRECT(J)
        IF (NN == I1 .OR. NN == I2) THEN
         X1(J)=X(1,NN)
         X2(J)=X(2,NN)
         X3(J)=X(3,NN)
        ELSE
         X1(J)=X(1,NN)+SHF(1)
         X2(J)=X(2,NN)+SHF(2)
         X3(J)=X(3,NN)+SHF(3)
        END IF        
       END DO 
C       
      XX13 =X1(3)-X1(1)
      YY13 =X2(3)-X2(1)
      ZZ13 =X3(3)-X3(1)
      XX24 =X1(4)-X1(2)
      YY24 =X2(4)-X2(2)
      ZZ24 =X3(4)-X3(2)
C
      N1=YY13*ZZ24-ZZ13*YY24
      N2=ZZ13*XX24-XX13*ZZ24
      N3=XX13*YY24-YY13*XX24
      AREA= MAX(EM20,SQRT(N1*N1+N2*N2+N3*N3))
      N1=N1/AREA
      N2=N2/AREA
      N3=N3/AREA
C
      AREA=HALF*AREA
C      
      RETURN
      END
