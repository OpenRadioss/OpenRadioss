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
Chd|  Q4FINT2                       source/elements/solid_2d/quad4/q4fint2.F
Chd|-- called by -----------
Chd|        Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE Q4FINT2(
     1   SIG,     AY,      FAY,     FAZ,
     2   PY1,     PY2,     PY3,     PY4,
     3   PZ1,     PZ2,     PZ3,     PZ4,
     4   BYZ1,    BYZ2,    BYZ3,    BYZ4,
     5   BZY1,    BZY2,    BZY3,    BZY4,
     6   FY1,     FZ1,     FY2,     FZ2,
     7   FY3,     FZ3,     FY4,     FZ4,
     8   R22,     R23,     R32,     R33,
     9   AIR,     VOL,     QVIS,    ICP,
     A   NEL,     JHBE,    JCVT)
C-----------------------------------------------
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
#include      "com01_c.inc"
#include      "nsvis_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: JHBE
      INTEGER, INTENT(IN) :: JCVT
      INTEGER ICP,NEL
C     REAL
      my_real
     .    SIG(NEL,6),AY(*),FAY(*),FAZ(*),
     .    PY1(*),PY2(*),PY3(*),PY4(*),
     .    PZ1(*),PZ2(*),PZ3(*),PZ4(*),
     .    BYZ1(*),BYZ2(*),BYZ3(*),BYZ4(*),
     .    BZY1(*),BZY2(*),BZY3(*),BZY4(*),
     .    FY1(*),FZ1(*),FY2(*),FZ2(*),
     .    FY3(*),FZ3(*),FY4(*),FZ4(*),
     .    R22(*),R23(*),R32(*),R33(*),
     .    AIR(*),VOL(*),QVIS(*)
C-----------------------------------------------
C FUNCTION: 
C ARGUMENTS:  (I: input, O: output, IO: input & output, W: workspace)
C TYPE NAME                FUNCTION
C  I   SIG(NEL,6)           - STRESS COMPONENTS
C  I   AY(*)              - Ni/r AT CENTER FOR AXISYMMETRIC CASE
C  I   FAY(*),FAZ(*)      - COMPONENT OF INTERNAL FORCE FOR AXISYMMETRIC CASE
C  I   PY1(*)~PZ4(*)      - SHAPE DERIVATIVES (dNi/dY, dNi/dZ)
C  O   FY1(*)~FZ4(*)      - INTERNAL FORCE IN LOCAL STORAGE
C  I   R22(*)~R33(*)      - TRANSFORMATION MATRIX
C  I   AIR(*)             - W*|J|
C  I   VOL(*)             - W*|J| FOR PLAIN CASE; r'*W*|J| FOR AXISYMMETRIC CASE
C  I   QVIS(*)            - VISCOUS PRESSURE
C  I   ICP                - FLAG FOR CONSTANT PRESURE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .    S1(MVSIZ),S2(MVSIZ),S3(MVSIZ),S4(MVSIZ),
     .    FAC(MVSIZ),P(MVSIZ),T1,T2,T3,T4
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF(N2D==1 .AND. JHBE==17) THEN
        DO I=1,NEL
          FAC(I) = AIR(I)
        ENDDO
      ELSE
        DO I=1,NEL
          FAC(I) = VOL(I)
        ENDDO
      ENDIF
C
C     ADD VISCOUS STRESSES "SVIS" & "QVIS" INTO THE TOTAL ONES
C     REMOVE PRESSURE FROM STRESSES FOR THE CONSTANT PRESSURE CASE
      IF(ICP==1) THEN
        DO I=1,NEL
          P(I)=THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3)+
     .                SVIS(I,1)+SVIS(I,2)+SVIS(I,3))
          S1(I)=(SIG(I,1)+SVIS(I,1)-P(I))*FAC(I)
          S2(I)=(SIG(I,2)+SVIS(I,2)-P(I))*FAC(I)
        ENDDO
      ELSE
        DO I=1,NEL
          S1(I)=(SIG(I,1)+SVIS(I,1)-QVIS(I))*FAC(I)
          S2(I)=(SIG(I,2)+SVIS(I,2)-QVIS(I))*FAC(I)
        ENDDO
      ENDIF
C
      DO I=1,NEL
        FY1(I) = FY1(I) - S1(I)*PY1(I)- S2(I)*BZY1(I)
        FY2(I) = FY2(I) - S1(I)*PY2(I)- S2(I)*BZY2(I)
        FY3(I) = FY3(I) - S1(I)*PY3(I)- S2(I)*BZY3(I)
        FY4(I) = FY4(I) - S1(I)*PY4(I)- S2(I)*BZY4(I)
        FZ1(I) = FZ1(I) - S2(I)*PZ1(I)- S1(I)*BYZ1(I)
        FZ2(I) = FZ2(I) - S2(I)*PZ2(I)- S1(I)*BYZ2(I)
        FZ3(I) = FZ3(I) - S2(I)*PZ3(I)- S1(I)*BYZ3(I)
        FZ4(I) = FZ4(I) - S2(I)*PZ4(I)- S1(I)*BYZ4(I)
      ENDDO
C
      IF(N2D==1.AND.JHBE==17) THEN
         IF(ICP==1) THEN
          DO I=1,NEL
            S3(I)=(SIG(I,3)+SVIS(I,3)-P(I))*FAC(I)
            S4(I)=(SIG(I,4)+SVIS(I,4))*FAC(I)
          ENDDO
           ELSE
          DO I=1,NEL
            S3(I)=(SIG(I,3)+SVIS(I,3)-QVIS(I))*FAC(I)
            S4(I)=(SIG(I,4)+SVIS(I,4))*FAC(I)
          ENDDO
           END IF
         IF(JCVT/=0) THEN
            DO I=1,NEL
              T1=S1(I)*R22(I)+S4(I)*R23(I)
              T2=S4(I)*R32(I)+S2(I)*R33(I)
              T3=S1(I)*R32(I)+S4(I)*R33(I)
              T4=S4(I)*R22(I)+S2(I)*R23(I)
              S1(I)=R22(I)*T1+R23(I)*T4
              S2(I)=R32(I)*T3+R33(I)*T2
              S4(I)=R22(I)*T3+R23(I)*T2
            ENDDO
          ENDIF
          DO I=1,NEL
            FAY(I) = FAY(I) + (S1(I)-S3(I))*AY(I)
            FAZ(I) = FAZ(I) + S4(I)*AY(I)
          ENDDO
      ENDIF
C
      RETURN
      END
            
