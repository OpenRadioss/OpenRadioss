Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    visc_prony       ../engine/source/materials/visc/visc_prony.F
!||--- called by ------------------------------------------------------
!||    viscmain         ../engine/source/materials/visc/viscmain.F
!||--- uses       -----------------------------------------------------
!||    visc_param_mod   ../common_source/modules/mat_elem/visc_param_mod.F90
!||====================================================================
      SUBROUTINE VISC_PRONY(VISC  ,NPRONY  ,NEL     ,NVARVIS ,UVARVIS ,
     .           EPSPXX  ,EPSPYY  ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX  ,
     .           SV1     ,SV2     ,SV3     ,SV4     ,SV5     ,SV6     ,
     .           TIMESTEP,RHO     ,VISCMAX ,SOUNDSP ,NVAR_DAMP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE VISC_PARAM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,NVARVIS,NPRONY
      INTEGER ,INTENT(IN) :: NVAR_DAMP
      my_real
     .   TIMESTEP
      my_real
     .   RHO(NEL),
     .   EPSPXX(NEL),EPSPYY(NEL),EPSPZZ(NEL),
     .   EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   SV1(NEL),SV2(NEL),SV3(NEL),SV4(NEL),SV5(NEL),SV6(NEL)
      TYPE(VISC_PARAM_) ,INTENT(IN) :: VISC
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    VISCMAX(NEL),SOUNDSP(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real ,INTENT(INOUT) :: UVARVIS(NEL,NVARVIS)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II,IMOD
      my_real 
     .  KV0,G,DAV
      my_real, DIMENSION(NEL) :: P,EPXX,EPYY,EPZZ,TRACE
      my_real 
     .  AA(NPRONY),BB(NPRONY),GV(NPRONY),BETA(NPRONY),H0(6),H(6),
     .  AAK(NPRONY),BBK(NPRONY),BETAK(NPRONY),KV(NPRONY),HP0,
     .  RBULK,HP
C=======================================================================
      G  = ZERO
      RBULK = ZERO
      IMOD  = VISC%IPARAM(2)
      KV0   = VISC%UPARAM(1)
      VISCMAX(1:NEL) = ZERO                                            

      DO J=1,NPRONY                                  
        GV(J)    = VISC%UPARAM(1 + J)            
        BETA(J)  = VISC%UPARAM(1 + J + NPRONY  )
        KV(J)    = VISC%UPARAM(1 + J + NPRONY*2)            
        BETAK(J) = VISC%UPARAM(1 + J + NPRONY*3)
        G = G + GV(J)   
        RBULK = RBULK + KV(J)
        AA(J) = EXP(-BETA(J)*TIMESTEP)
        BB(J) = TWO*TIMESTEP*GV(J)*EXP(-HALF*BETA(J)*TIMESTEP) 
C        
        AAK(J) = EXP(-BETAK(J)*TIMESTEP)
        BBK(J) = TIMESTEP*KV(J)*EXP(-HALF*BETAK(J)*TIMESTEP)
      ENDDO 
C    
      IF(IMOD > 0) THEN        
        DO I=1,NEL                                                     
c       spheric part 
          TRACE(I) = -(EPSPXX(I) + EPSPYY(I) + EPSPZZ(I))
          DAV = THIRD*TRACE(I)
          P(I)   = ZERO 
                                             
c         deviatoric part                                           
          EPXX(I) = EPSPXX(I) + DAV                                  
          EPYY(I) = EPSPYY(I) + DAV                                   
          EPZZ(I) = EPSPZZ(I) + DAV
C           
          SV1(I) = ZERO
          SV2(I) = ZERO
          SV3(I) = ZERO
          SV4(I) = ZERO
          SV5(I) = ZERO
          SV6(I) = ZERO
        ENDDO
C            
        DO J= 1,NPRONY  
          II = (J-1)*(NVARVIS-NVAR_DAMP)/NPRONY
          DO I=1,NEL
C                                                                    
            H0(1) = UVARVIS(I,II + 1)                         
            H0(2) = UVARVIS(I,II + 2)                         
            H0(3) = UVARVIS(I,II + 3)                         
            H0(4) = UVARVIS(I,II + 4)                         
            H0(5) = UVARVIS(I,II + 5)                         
            H0(6) = UVARVIS(I,II + 6)                          
            HP0    = UVARVIS(I,II + 7)            
C
            H(1) = AA(J)*H0(1) + BB(J)*EPXX(I)                     
            H(2) = AA(J)*H0(2) + BB(J)*EPYY(I)                     
            H(3) = AA(J)*H0(3) + BB(J)*EPZZ(I)                     
            H(4) = AA(J)*H0(4) + HALF*BB(J)*EPSPXY(I)         
            H(5) = AA(J)*H0(5) + HALF*BB(J)*EPSPYZ(I)         
            H(6) = AA(J)*H0(6) + HALF*BB(J)*EPSPZX(I)                    
            HP = AAK(J)*HP0 + BBK(J)*TRACE(I)
C
            UVARVIS(I,II + 1) = H(1)                        
            UVARVIS(I,II + 2) = H(2)                        
            UVARVIS(I,II + 3) = H(3)                        
            UVARVIS(I,II + 4) = H(4)                        
            UVARVIS(I,II + 5) = H(5)                        
            UVARVIS(I,II + 6) = H(6)                          
            UVARVIS(I,II + 7) = HP                      
c
            SV1(I) = SV1(I) + H(1)                                    
            SV2(I) = SV2(I) + H(2)                                    
            SV3(I) = SV3(I) + H(3)                                    
            SV4(I) = SV4(I) + H(4)                                    
            SV5(I) = SV5(I) + H(5)                                    
            SV6(I) = SV6(I) + H(6)
            P(I)  = P(I)  + HP                                       
          ENDDO
        ENDDO                                                     
c
        DO I=1,NEL
          SV1(I) = SV1(I) - P(I)                                       
          SV2(I) = SV2(I) - P(I)                                       
          SV3(I) = SV3(I) - P(I)                        
C                                                                       
          SOUNDSP(I) = SQRT(SOUNDSP(I)**2 + (FOUR_OVER_3*G + RBULK)/RHO(I)) 
        ENDDO  !   I=1,NEL
      ELSE
        DO I=1,NEL                                                     
c       spheric part 
          TRACE(I) = EPSPXX(I) + EPSPYY(I) + EPSPZZ(I) 
          DAV = THIRD*TRACE(I)  
          P(I)  = -KV0*TRACE(I)                  
c         deviatoric part                                           
          EPXX(I) = EPSPXX(I) - DAV                                    
          EPYY(I) = EPSPYY(I) - DAV                                    
          EPZZ(I) = EPSPZZ(I) - DAV  
C           
          SV1(I) = ZERO
          SV2(I) = ZERO
          SV3(I) = ZERO
          SV4(I) = ZERO
          SV5(I) = ZERO
          SV6(I) = ZERO
        ENDDO
C            
        DO J= 1,NPRONY  
          II = (J-1)*(NVARVIS-NVAR_DAMP)/NPRONY
C               
          DO I=1,NEL                                                      
            H0(1) = UVARVIS(I,II + 1)                         
            H0(2) = UVARVIS(I,II + 2)                         
            H0(3) = UVARVIS(I,II + 3)                         
            H0(4) = UVARVIS(I,II + 4)                         
            H0(5) = UVARVIS(I,II + 5)                         
            H0(6) = UVARVIS(I,II + 6)                          
            HP0    = UVARVIS(I,II + 7)                         
C
            H(1) = AA(J)*H0(1) + BB(J)*EPXX(I)                     
            H(2) = AA(J)*H0(2) + BB(J)*EPYY(I)                     
            H(3) = AA(J)*H0(3) + BB(J)*EPZZ(I)                     
            H(4) = AA(J)*H0(4) + HALF*BB(J)*EPSPXY(I)         
            H(5) = AA(J)*H0(5) + HALF*BB(J)*EPSPYZ(I)         
            H(6) = AA(J)*H0(6) + HALF*BB(J)*EPSPZX(I)
C
            UVARVIS(I,II + 1) = H(1)                        
            UVARVIS(I,II + 2) = H(2)                        
            UVARVIS(I,II + 3) = H(3)                        
            UVARVIS(I,II + 4) = H(4)                        
            UVARVIS(I,II + 5) = H(5)                        
            UVARVIS(I,II + 6) = H(6)                   
c
            SV1(I) = SV1(I) + H(1)                                    
            SV2(I) = SV2(I) + H(2)                                    
            SV3(I) = SV3(I) + H(3)                                    
            SV4(I) = SV4(I) + H(4)                                    
            SV5(I) = SV5(I) + H(5)                                    
            SV6(I) = SV6(I) + H(6)                            
          ENDDO                                                     
        ENDDO
c
        DO I=1,NEL 
          SV1(I) = SV1(I) - P(I)                                       
          SV2(I) = SV2(I) - P(I)                                       
          SV3(I) = SV3(I) - P(I)                                       
C                                                                       
          SOUNDSP(I) = SQRT(SOUNDSP(I)**2 + (FOUR_OVER_3*G + RBULK)/RHO(I)) 
        ENDDO  !   I=1,NEL 
      ENDIF                                                         
c------------
      RETURN
      END
