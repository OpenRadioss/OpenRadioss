Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!  ------------------------------------
      SUBROUTINE INIT_DATA(NBR_GROUP,PM,EINT,OFF,SIGY,DT1C,VOL0,GS,
     .         DPLA,PLA,FOR,SIGO,SIGN,DEP,A)

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com01_c.inc"
  
!  ------------------------------------
        INTEGER, INTENT(IN) :: NBR_GROUP
        my_real, INTENT(INOUT) :: PM(PMSIZE,*),EINT(*),
     .             OFF(*),SIGY(*),DT1C(*),VOL0(*),GS(*),
     .             DPLA(*),PLA(*),FOR(*),A(5,*)
        my_real, DIMENSION(*), INTENT(INOUT) :: SIGO,SIGN,DEP

!  ------------------------------------
        INTEGER :: I,NG
!  ------------------------------------
        DO NG=1,NBR_GROUP
                DO I=1,25
                        PM(I,NG) = COS(2.d+00*3.14d+00*I/19) * EXP(1.d+00*NG/NBR_GROUP)
                ENDDO
                PM(9,NG) = 1.0498
        ENDDO
        
        DO I=1,NUMNOD
                EINT(I) = TAN(2.d+00*3.14d+00*I/NUMNOD)
                EINT(NUMNOD+I) = SIN(2.d+00*3.14d+00*I/NUMNOD)
                OFF(I) = UN
                SIGY(I)= COSH(2.d+00*3.14d+00*I/NUMNOD)
                DT1C(I) = i*10.d-05/NUMNOD
                VOL0(I) = i*10.d-02/NUMNOD
                GS(I) = 1.2345d+00
                DPLA(I) = 0.d+00
                PLA(I) = 0.d+00
        ENDDO
        SIGO(1:5*NUMNOD) = 0.0d+00
        SIGN(1:5*NUMNOD) = 0.0d+00
        DEP(1:5*NUMNOD) = 0.001d+00
        FOR(1:5*NUMNOD) = 0.123456d+00
        A(1:5,1:NUMNOD) = 0.d+00

        RETURN
  
!  ------------------------------------
       END SUBROUTINE INIT_DATA
!  ------------------------------------
