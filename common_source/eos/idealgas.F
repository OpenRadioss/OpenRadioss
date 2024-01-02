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
Chd|  IDEALGAS                      eos/idealgas.F                
Chd|-- called by -----------
Chd|        EOSMAIN                       common_source/eos/eosmain.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IDEALGAS(IFLAG ,NEL  ,PM   ,OFF   ,EINT  ,MU   ,MU2 , 
     2                     ESPE ,DVOL ,DF   ,VNEW  ,MAT   ,PSH  ,
     3                     PNEW ,DPDM ,DPDE ,THETA ,ECOLD)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine contains numerical solving
C of IDEAL GAS EOS
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "scr06_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(NEL), IFLAG, NEL
      my_real PM(NPROPM,NUMMAT), 
     .     OFF(NEL)  ,EINT(NEL) ,MU(NEL)   , 
     .     MU2(NEL)  ,ESPE(NEL) ,DVOL(NEL) ,DF(NEL)  , 
     .     VNEW(NEL) ,PNEW(NEL) ,DPDM(NEL),
     .     DPDE(NEL) ,THETA(NEL),ECOLD(NEL)
      my_real, INTENT(INOUT) :: PSH(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX
      my_real :: P0,GAMMA,T0,E0,SPH,AA, BB,TFEXTT, DVV, PP, Cv, RHO, RHO0
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF(IFLAG == 0) THEN
         MX         = MAT(1)
         E0         = PM(23,MX)         
         GAMMA      = PM(32,MX)
         P0         = PM(31,MX)
         PSH(1:NEL) = PM(88,MX)
         T0         = PM(35,MX)
         SPH        = PM(69,MX)
         DO I=1,NEL
           PP      = (GAMMA-ONE)*(ONE+MU(I))*ESPE(I)
           DPDM(I) = (GAMMA-ONE)*(ESPE(I)+PP*DF(I))     !total derivative
           DPDE(I) = (GAMMA-ONE)*(ONE+MU(I))             !partial derivative
         ENDDO
         DO I=1,NEL
           ECOLD(I)=-THREE100*SPH
           IF(MU(I)>ZERO) ECOLD(I)=ECOLD(I)*(ONE+(GAMMA-ONE)*MU(I)*(ONE-MU(I)))
         ENDDO
C-----------------------------------------------
      ELSEIF(IFLAG == 1) THEN
         TFEXTT     =ZERO
         MX         = MAT(1)
         E0         = PM(23,MX)
         GAMMA      = PM(32,MX)
         P0         = PM(31,MX)
         PSH(1:NEL) = PM(88,MX)
         T0         = PM(35,MX)
         SPH        = PM(69,MX)
         ! p1 = p(mu,E+dE) = (g-1)*(1+mu)*(E-dvv*(p0+p1))    where p1,p0 are total pressure (no pshift), dvv = DVOL/2/V0                                               
         DO I=1,NEL
           BB          = (GAMMA-ONE)*(ONE+MU(I))
           DVV         = HALF*DVOL(I)*DF(I) / MAX(EM15,VNEW(I))
           PNEW(I)     = BB*ESPE(I) / (ONE+BB*DVV)         
           PNEW(I)     = MAX(PNEW(I),ZERO)*OFF(I)
           EINT(I)     = EINT(I) - HALF*DVOL(I)*PNEW(I)
           TFEXTT      = TFEXTT-DVOL(I)*PSH(I)
           PNEW(I)     = PNEW(I)-PSH(I)
         ENDDO       
#include "atomic.inc"
       TFEXT = TFEXT + TFEXTT
#include "atomend.inc"
         MX     = MAT(1)
         RHO0   = PM(1 ,MX)
         SPH    = PM(69,MX)
         Cv     = PM(36,MX)
         DO I=1,NEL
           RHO      = RHO0*(ONE+MU(I))
           IF(OFF(I)<1.) CYCLE
           THETA(I) = EINT(I)/Cv/RHO/VNEW(I)
           THETA(I) = MAX(ZERO,THETA(I))
         ENDDO
C-----------------------------------------------
       ELSEIF (IFLAG == 2) THEN
          MX           = MAT(1)
          E0           = PM(23,MX)         
          GAMMA        = PM(32,MX)
          P0           = PM(31,MX)
          PSH(1:NEL) = PM(88,MX)
          T0           = PM(35,MX)
          SPH          = PM(69,MX)
          DO I=1, NEL
             IF (VNEW(I) > ZERO) THEN
                PP      = (GAMMA-ONE)*(ONE+MU(I))*ESPE(I) - PSH(I)
                DPDM(I) = (GAMMA-ONE)*(ESPE(I)+(PP+PSH(I))*DF(I)) !total derivative
                DPDE(I) = (GAMMA-ONE)*(ONE+MU(I)) !partial derivative
                PNEW(I) = PP
             ENDIF
          ENDDO
      ENDIF
C-----------------------------------------------
      RETURN
      END
