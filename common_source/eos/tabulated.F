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
Chd|====================================================================
Chd|  IDEALGAS                      eos/idealgas.F                
Chd|-- called by -----------
Chd|        EOSMAIN                       common_source/eos/eosmain.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TABULATED(IFLAG,NEL  ,PM   ,OFF  ,EINT ,MU  ,MU2 , 
     2                     ESPE ,DVOL ,DF   ,VNEW ,MAT  ,PSH ,
     3                     PNEW ,DPDM ,DPDE ,THETA,ECOLD, 
     4                     NPF  ,TF)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine contains numerical solving  of TABULATED Equation Of State (/EOS/TABULATED)
C   P(mu,E) = A(mu) + B(mu)*E
C     where A and B are two user functions (/FUNCT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: MAT(NEL), IFLAG, NEL,NPF(SNPC)
      my_real, INTENT(INOUT) :: PM(NPROPM,NUMMAT), 
     .   OFF(NEL)  ,EINT(NEL) ,MU(NEL)   , 
     .   MU2(NEL)  ,ESPE(NEL) ,DVOL(NEL) ,DF(NEL)  , 
     .   VNEW(NEL) ,PNEW(NEL) ,DPDM(NEL),
     .   DPDE(NEL) ,THETA(NEL),ECOLD(NEL),
     .   PSH(NEL), TF(STF)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "scr06_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX
      my_real :: P0,GAMMA,T0,E0,SPH,AA, BB,TFEXTT, DVV, PP, Cv, RHO, RHO0
      my_real :: XSCALE_A,XSCALE_B,FSCALE_A,FSCALE_B
      INTEGER :: A_fun_id, B_fun_id
      my_real :: RES_A(MVSIZ),RES_B(MVSIZ),DERI_A(MVSIZ),DERI_B(MVSIZ),PC
      my_real,EXTERNAL :: FINTER
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF(IFLAG == 0) THEN
         MX = MAT(1)
         E0 = PM(23,MX)         
         PSH(LFT:LLT) = PM(88,MX)
         SPH = PM(69,MX)
         XSCALE_A = PM(33,MX)
         XSCALE_B = PM(34,MX)
         A_fun_id = PM(35,MX)
         B_fun_id = PM(36,MX)
         PC  = PM( 37,MX)          
         FSCALE_A = PM(160,MX)
         FSCALE_B = PM(161,MX)
         
         ! both A_fun_id & B_fun_id cannot be 0. This is ensured by the reader during the Starter process
         IF(A_fun_id == 0)THEN
           DO I=LFT,LLT
             RES_A(I) = ZERO
             DERI_A(I) = ZERO
             RES_B(I) = FSCALE_B*FINTER(B_fun_id,MU(I),NPF,TF,DERI_B(I))
           ENDDO
         ELSEIF(B_fun_id == 0)THEN
           DO I=LFT,LLT
             RES_A(I) = FSCALE_A*FINTER(A_fun_id,MU(I),NPF,TF,DERI_A(I))
             RES_B(I) = ZERO
             DERI_B(I) = ZERO             
           ENDDO         
         ELSE
           DO I=LFT,LLT         
             RES_A(I) = FSCALE_A*FINTER(A_fun_id,MU(I),NPF,TF,DERI_A(I))
             RES_B(I) = FSCALE_B*FINTER(B_fun_id,MU(I),NPF,TF,DERI_B(I))
           ENDDO
         ENDIF
         
         DO I=LFT,LLT
           PP      = RES_A(I) + RES_B(I) * ESPE(I) - PSH(I) ! A(MU(I))+B(MU(I))*ESPE(I)
           DPDM(I) = DERI_A(I)+DERI_B(I)*ESPE(I) + RES_B(I)*(PP+PSH(I))/( (ONE+MU(I))*(ONE+MU(I)) ) ! A'(MU0) + B'(MU0)*E0+B(MU0)/(ONE+MU0)/(ONE+MU0)*P0     !total derivative
           DPDE(I) = RES_B(I) ! B(MU(I))                                               !partial derivative
         ENDDO
         
         DO I=LFT,LLT
           ECOLD(I)=-THREE100*SPH
           IF(MU(I).GT.ZERO) ECOLD(I)=ECOLD(I)*ONE
         ENDDO
C-----------------------------------------------
      ELSEIF(IFLAG == 1) THEN
         TFEXTT       =ZERO
         MX = MAT(1)
         E0 = PM(23,MX)         
         PSH(LFT:LLT) = PM(88,MX)
         SPH = PM(69,MX)
         XSCALE_A = PM(33,MX)
         XSCALE_B = PM(34,MX)
         A_fun_id = PM(35,MX)
         B_fun_id = PM(36,MX) 
         PC  = PM( 37,MX)         
         FSCALE_A = PM(160,MX)
         FSCALE_B = PM(161,MX)

         ! both A_fun_id & B_fun_id cannot be 0. This is ensured by the reader during the Starter process
         IF(A_fun_id == 0)THEN
           DO I=LFT,LLT
             RES_A(I) = ZERO
             RES_B(I) = FSCALE_B*FINTER(B_fun_id,MU(I),NPF,TF,DERI_B(I))
           ENDDO
         ELSEIF(B_fun_id == 0)THEN
           DO I=LFT,LLT
             RES_A(I) = FSCALE_A*FINTER(A_fun_id,MU(I),NPF,TF,DERI_A(I))
             RES_B(I) = ZERO
           ENDDO         
         ELSE
           DO I=LFT,LLT         
             RES_A(I) = FSCALE_A*FINTER(A_fun_id,MU(I),NPF,TF,DERI_A(I))
             RES_B(I) = FSCALE_B*FINTER(B_fun_id,MU(I),NPF,TF,DERI_B(I))
           ENDDO
         ENDIF
         
         DO I=LFT,LLT
           AA      = RES_A(I)
           BB      = RES_B(I)
           DVV     = HALF*DVOL(I)*DF(I) / MAX(EM15,VNEW(I))
           PP      = AA + BB * ESPE(I)
           PNEW(I) = (AA+BB*(ESPE(I)-PSH(I)*DVV))/(ONE+BB*DVV)
           PNEW(I) = MAX(PNEW(I),PC )*OFF(I)
           EINT(I) = EINT(I) - HALF*DVOL(I)*(PNEW(I)+PSH(I) )
           TFEXTT  = TFEXTT-DVOL(I)*PSH(I)
         ENDDO       
#include "atomic.inc"
       TFEXT = TFEXT + TFEXTT
#include "atomend.inc"
         MX     = MAT(1)
         RHO0   = PM(1 ,MX)
         SPH    = PM(69,MX)
         Cv     = PM(36,MX)
         IF(SPH == ZERO)SPH=ONE
         DO I=LFT,LLT
           RHO      = RHO0*(ONE+MU(I))
           IF(OFF(I) >= ONE)THEN
             THETA(I) = (DF(I)*EINT(I)/VNEW(I)-ECOLD(I))/SPH
             THETA(I) = MAX(ZERO,THETA(I))
           ENDIF
         ENDDO
C-----------------------------------------------
       ELSEIF (IFLAG == 2) THEN
         MX = MAT(1)
         E0 = PM(23,MX)         
         PSH(LFT:LLT) = PM(88,MX)
         SPH = PM(69,MX)
         XSCALE_A = PM(33,MX)
         XSCALE_B = PM(34,MX)
         A_fun_id = PM(35,MX)
         B_fun_id = PM(36,MX) 
         PC  = PM( 37,MX)         
         FSCALE_A = PM(160,MX)
         FSCALE_B = PM(161,MX)

         ! both A_fun_id & B_fun_id cannot be 0. This is ensured by the reader during the Starter process
         IF(A_fun_id == 0)THEN
           DO I=LFT,LLT
             RES_A(I) = ZERO
             RES_B(I) = FSCALE_B*FINTER(B_fun_id,MU(I),NPF,TF,DERI_B(I))
           ENDDO
         ELSEIF(B_fun_id == 0)THEN
           DO I=LFT,LLT
             RES_A(I) = FSCALE_A*FINTER(A_fun_id,MU(I),NPF,TF,DERI_A(I))
             RES_B(I) = ZERO
           ENDDO         
         ELSE
           DO I=LFT,LLT         
             RES_A(I) = FSCALE_A*FINTER(A_fun_id,MU(I),NPF,TF,DERI_A(I))
             RES_B(I) = FSCALE_B*FINTER(B_fun_id,MU(I),NPF,TF,DERI_B(I))
           ENDDO
         ENDIF
                  
         DO I=1, NEL                                                               
            IF (VNEW(I) .GT. ZERO) THEN    
               PP      = RES_A(I) + RES_B(I)*ESPE(I) - PSH(I)                  
               DPDM(I) = DERI_A(I)+DERI_B(I)*ESPE(I) + RES_B(I)*(PP+PSH(I))/( (ONE+MU(I))*(ONE+MU(I)) ) ! A'(MU0) + B'(MU0)*E0+B(MU0)/(ONE+MU0)/(ONE+MU0)*P0     !total derivative
               DPDE(I) = RES_B(I) ! B(MU(I))                                               !partial derivative
               PNEW(I) = PP                                                        
            ENDIF                                                                  
         ENDDO                                                                     
      ENDIF
C-----------------------------------------------
      RETURN
      END
