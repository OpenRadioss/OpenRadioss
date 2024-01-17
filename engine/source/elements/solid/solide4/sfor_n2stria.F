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
Chd|====================================================================
Chd|  SFOR_N2STRIA                  source/elements/solid/solide4/sfor_n2stria.F
Chd|-- called by -----------
Chd|        S4FOR_DISTOR                  source/elements/solid/solide4/s4for_distor.F
Chd|-- calls ---------------
Chd|        SFOR_N2S3                     source/elements/solid/solide4/sfor_n2s3.F
Chd|====================================================================
      SUBROUTINE SFOR_N2STRIA(XC,      YC,     ZC, 
     .                        X1,      X2,     X3,
     .                        Y1,      Y2,     Y3,
     .                        Z1,      Z2,     Z3,
     .                     FOR_T1, FOR_T2, FOR_T3,
     .                     FORC_N,  STIF0,   STIF, 
     .                     FQMAX , PENMIN, PENREF,
     .                         LL,  IFCTL,   NEL )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT (IN)  :: NEL
      INTEGER, INTENT (OUT) :: IFCTL
      my_real,                   INTENT (IN) :: FQMAX
      my_real, DIMENSION(MVSIZ), INTENT (IN) :: STIF0,LL,
     .                        X1,      X2,     X3,
     .                        Y1,      Y2,     Y3,
     .                        Z1,      Z2,     Z3,
     .                        XC,      YC,     ZC,
     .                     PENMIN, PENREF
      my_real, DIMENSION(MVSIZ), INTENT (INOUT) :: STIF
      my_real, DIMENSION(MVSIZ,3), INTENT (INOUT) :: FORC_N,
     .                           FOR_T1, FOR_T2, FOR_T3
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IFCTL1,IFC2(MVSIZ)
C                                                                     12
      my_real
     .   FX,FY,FZ,FKT(MVSIZ)
C----------------------------
        IFC2(1:NEL) = 1
        FKT(1:NEL) = ONE
        DO I=1,NEL
          IF (STIF0(I)>ZERO) CYCLE
          IFC2(I) = 0
        ENDDO
         CALL SFOR_N2S3(XC ,     YC ,    ZC , FORC_N,     
     .                   X1,      Y1,     Z1, FOR_T1, 
     .                   X2,      Y2,     Z2, FOR_T2,
     .                   X3,      Y3,     Z3, FOR_T3,
     .                STIF0,    IFC2,  IFCTL, PENMIN,
     .               PENREF,     FKT,    LL ,  FQMAX,
     .                  NEL)
        DO I=1,NEL
          IF (STIF0(I)==ZERO) CYCLE
          STIF(I) =MAX(STIF(I),FKT(I)*STIF0(I))
        ENDDO
C
      RETURN
      END
