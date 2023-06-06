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
Chd|  AGAUG3Q                       source/ale/agaug3q.F           
Chd|-- called by -----------
Chd|        AGAUGE                        source/ale/agauge.F           
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE AGAUG3Q(LGAUGE,GAUGE,PHI,IXQ  ,X    ,ALE_CONNECT )
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: IXQ(NIXQ,NUMELQ)
      INTEGER, INTENT(INOUT) :: LGAUGE(3,NBGAUGE)
      my_real, INTENT(IN) :: PHI(SPHI),X(3,NUMNOD)
      my_real, INTENT(INOUT) :: GAUGE(LLGAUGE,NBGAUGE)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,N,N1,N2,N3,N4,IG,IAD2
      my_real ALPHA,XG,YG,ZG,
     . det_12_1P,det_23_2P,det_34_3P,det_41_4P,
     . Y1,Z1,Y2,Z2,Y3,Z3,Y4,Z4
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------     
c--------------------------------------------------------------
c  Searching for the new element associated to the gauge
c--------------------------------------------------------------
      DO I=1,LLT
        II=I+NFT  
        IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
        DO J=1,4
          N= ALE_CONNECT%ee_connect%connected(IAD2 + J - 1)
          IF(N<=0)CYCLE
          IG=NINT(PHI(N))
          IF(IG==0)CYCLE

C            
            XG     = GAUGE(2,IG)
            YG     = GAUGE(3,IG)
            ZG     = GAUGE(4,IG)
C                    
!           check if gauge is changing of element
            Y1 = X(2,IXQ(2,II))
            Z1 = X(3,IXQ(2,II))
            Y2 = X(2,IXQ(3,II))
            Z2 = X(3,IXQ(3,II))
            Y3 = X(2,IXQ(4,II))
            Z3 = X(3,IXQ(4,II))
            Y4 = X(2,IXQ(5,II))
            Z4 = X(3,IXQ(5,II))

            det_12_1P = (Y2-Y1)*(ZG-Z1)-(Z2-Z1)*(YG-Y1)
            det_23_2P = (Y3-Y2)*(ZG-Z2)-(Z3-Z2)*(YG-Y2)
            det_34_3P = (Y4-Y3)*(ZG-Z3)-(Z4-Z3)*(YG-Y3)
            det_41_4P = (Y1-Y4)*(ZG-Z4)-(Z1-Z4)*(YG-Y4)

            ALPHA = ZERO
            IF ((det_12_1P >=0 .AND. det_23_2P >=0 .AND.
     .           det_34_3P >=0 .AND. det_41_4P >=0) .OR.
     .          (det_12_1P <=0 .AND. det_23_2P <=0 .AND.
     .           det_34_3P <=0 .AND. det_41_4P <=0)) ALPHA = ONE

#include "lockon.inc"            
            IF(ALPHA == ONE .AND. ALPHA >= GAUGE(5,IG))THEN
c the gauge is changing of element
c element associated to the gauge
              LGAUGE(1,IG)=-II
              GAUGE(5,IG)=ALPHA 
            ENDIF
#include "lockoff.inc"
          ENDDO
      ENDDO


      RETURN
      END