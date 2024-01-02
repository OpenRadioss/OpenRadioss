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
Chd|  DISP_VEL_SAVED_CLOAD          source/assembly/disp_vel_saved_cload.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DISP_VEL_SAVED_CLOAD(V      ,D      ,VR    ,DR    ,IB         ,
     2                                DPL0CLD,VEL0CLD,NIBCLD,NCONLD,NCLOAD,
     3                                IRODDL ,NUMNOD )
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
!      USE PINCHTYPE_MOD
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NIBCLD,NCONLD,NCLOAD,IRODDL,NUMNOD
      INTEGER, INTENT(IN) :: IB(NIBCLD,NCONLD)
C     REAL
      my_real, INTENT(IN) :: 
     .   D(3,NUMNOD) ,V(3,NUMNOD) ,DR(3,NUMNOD)  ,VR(3,NUMNOD)
      my_real, INTENT(INOUT) :: 
     .   DPL0CLD(6,NCLOAD),VEL0CLD(6,NCLOAD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  NL,N1,N2,ISK,IDIR,IFUN
C-----------------------------------------------
C     IBCL(1:NIBCLD,1:NUMCLD)              IPRES = IBCL(1:NIBCLD,NUMCCLD+1,NUMCLD+NUMPRES)         
C     1: Node Number                       1st node number of the segment
C     2: NS = 10*Noskew+Idir               2nd node number of the segment
!! Save displacement and velocity in the previous cycle
      DO NL=1,NCLOAD
        N1 = IB(1,NL)
        N2 = IB(2,NL)
        IFUN = IB(9,NL)
        ISK = IB(2,NL)/10
        IDIR  = IB(2,NL)-10*ISK
        IF (IFUN > 1) THEN
          IF (IDIR<=3) THEN
            DPL0CLD(IDIR,NL) = D(IDIR,N1)
            VEL0CLD(IDIR,NL) = V(IDIR,N1)
          ELSEIF (IDIR<=6 .AND. IRODDL/=0) THEN
            DPL0CLD(IDIR,NL) = DR(IDIR-3,N1)
            VEL0CLD(IDIR,NL) = VR(IDIR-3,N1)
          ENDIF
        ENDIF
      ENDDO
C
      RETURN
      END