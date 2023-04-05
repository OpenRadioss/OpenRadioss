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
Chd|  EDE112                        source/ale/euler2d/ede112.F   
Chd|-- called by -----------
Chd|        QFORC2                        source/elements/solid_2d/quad/qforc2.F
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE EDE112(PM  ,V   ,VEUL ,X   ,IXQ ,ALE_CONNECT ,
     .                  WYZ ,DYZ ,DZY  ,EYY ,EZZ ,ETT         ,EYZ ,EYT ,EZT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
! SPMD CASE : SIXQ >= NIXQ*NUMELQ    (SIXQ = NIXQ*NUMELQ_L+NIXQ*NQVOIS_L)
! IXQ(1:NIXQ, 1:NUMELQ) local elems
!    (1:NIXQ, NUMELQ+1:) additional elems (also on adjacent domains but connected to the boundary of the current domain)
      INTEGER,INTENT(IN) :: IXQ(NIXQ,SIXQ/NIXQ)
! SPMD CASE : SX >= 3*NUMNOD    (SX = 3*(NUMNOD_L+NRCVVOIS_L))
! X(1:3,1:NUMNOD) : local nodes
!  (1:3, NUMNOD+1:) additional nodes (also on adjacent domains but connected to the boundary of the current domain)
      my_real,INTENT(IN) :: X(3,SX/3)
!
      my_real,INTENT(IN) :: V(3,NUMNOD), VEUL(10,*), PM(NPROPM,NUMMAT), ETT(*), EYZ(*), EYT(*), EZT(*)
      my_real,INTENT(INOUT) :: EYY(*),EZZ(*),DYZ(*), DZY(*),WYZ(*)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II,IADJ,ML,NC1,NC2,NC3,NC4,J,IAD2
      my_real VY13, VY24, VZ13,VZ24,PY1,PY2,PZ1,PZ2,AIRE
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------
      DO I=LFT,LLT
       II=I+NFT
       IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
       DO J=1,4
         IADJ=ALE_CONNECT%ee_connect%connected(IAD2 + J - 1)
         IF(IADJ /= 0) THEN
           ML=NINT(PM(19,IXQ(1,IADJ)))
           IF(ML /= 11) THEN
             NC1=IXQ(2,IADJ)
             NC2=IXQ(3,IADJ)
             NC3=IXQ(4,IADJ)
             NC4=IXQ(5,IADJ)
             PY1=HALF*(X(3,NC2)-X(3,NC4))
             PY2=HALF*(X(3,NC3)-X(3,NC1))
             PZ1=HALF*(X(2,NC4)-X(2,NC2))
             PZ2=HALF*(X(2,NC1)-X(2,NC3))    
             AIRE=TWO*(PY1*PZ2-PZ1*PY2)
             VY13=V(2,NC1)-V(2,NC3)
             VY24=V(2,NC2)-V(2,NC4)
             VZ13=V(3,NC1)-V(3,NC3)
             VZ24=V(3,NC2)-V(3,NC4)
             EYY(I)=(PY1*VY13+PY2*VY24)/AIRE
             EZZ(I)=(PZ1*VZ13+PZ2*VZ24)/AIRE
             DZY(I)=(PY1*VZ13+PY2*VZ24)/AIRE
             DYZ(I)=(PZ1*VY13+PZ2*VY24)/AIRE
             EZZ(I)=ZERO
             WYZ(I)=HALF*DT1*(DZY(I)-DYZ(I))
           ENDIF
         ENDIF
       ENDDO!next J
      ENDDO!next I
C-----------------------------------------------
      RETURN
      END
