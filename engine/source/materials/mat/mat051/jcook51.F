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
Chd|  JCOOK51                       source/materials/mat/mat051/jcook51.F
Chd|-- called by -----------
Chd|        SIGEPS51                      source/materials/mat/mat051/sigeps51.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE JCOOK51 (NEL   ,SIGD     ,PLAS   ,TEMP   ,VOL  ,
     .                    DEPS  ,EPD      ,UPARAM ,VOLUME ,EINT ,
     .                    DE    ,TIMESTEP ,OFF    ,IX     ,NFT  ,
     .                    VFRAC )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER NEL, IX(NIXS,*),NFT
      my_real 
     .   SIGD(6,NEL),DEPS(6,NEL),PLAS(NEL),TEMP(NEL),EPD(NEL),
     .   VOL(NEL),UPARAM(*) ,VOLUME(NEL),EINT(NEL) , TIMESTEP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
       my_real 
     .   P,S1,S2,S3,G2,G3,VM,DPLA,R,DY,CMX,TSTAR,
     .   G,Y0,B,N,CC,EPDR,CM,TMELT,THETL,PLAMX,SIGMX,
     .   VM2X,T0,EPS0,BEPS0PNM1,CTE,FACT,EINC,BETA
      my_real 
     .   Y(NEL),H(NEL),VM2(NEL),DE(NEL),SIGDO(6,NEL),OFF(NEL),VFRAC(NEL),VOL_AVG
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      G         = UPARAM(1)
      Y0        = UPARAM(2)
      B         = UPARAM(3)
      N         = UPARAM(4)
      CC        = UPARAM(5)
      EPDR      = UPARAM(6)
      CM        = UPARAM(7)
      TMELT     = UPARAM(8)
      THETL     = UPARAM(9)
      PLAMX     = UPARAM(10)
      SIGMX     = UPARAM(11)
      T0        = UPARAM(13)  
      G2        = TWO*G
      G3        = THREE*G
      VM2X      = ZERO
      EPS0      = EM06
      BEPS0PNM1 = B * EPS0**N / EPS0
      BETA      = EM06  !HALF

      if(timestep==zero)return

      DO I = 1,NEL
        PLAS(I) = MAX(PLAS(I),ZERO)
        IF(PLAS(I) >= PLAMX) THEN
          Y(I)    = ZERO
          H(I)    = ZERO
        ELSEIF(TEMP(I) >= TMELT) THEN
          !PLAS(I) = ZERO
          Y(I)    = ZERO
          H(I)    = ZERO
        ELSEIF(VFRAC(I) < BETA) THEN
          !PLAS(I) = ZERO
          Y(I)    = ZERO
          H(I)    = ZERO
        ELSE
          CTE = ONE
          !temperature
          IF(CM /= ZERO.and.TEMP(I) > T0) THEN
            TSTAR = (TEMP(I)-T0)/(TMELT-T0)
            CMX   = CM
            IF(TEMP(I) > THETL) CMX = ONE
            CTE   = ONE - TSTAR**CMX
          ENDIF
          !vitesse de deformation
          IF(CC /= ZERO.and.EPD(I) > EPDR) THEN
            CTE   = CTE * (ONE + CC * LOG(EPD(I)/EPDR))
          ENDIF
          IF(PLAS(I) <= EM20)THEN
            !H(I)  = CTE * BEPS0PNM1
            !Y(I)  = CTE * Y0  + H(I) * PLAS(I)

            DY    = zero
            Y(I)  = CTE * (Y0 + DY)
            H(I)  = zero            
          ELSE
            DY    = B * PLAS(I)**N
            Y(I)  = CTE * (Y0 + DY)
            H(I)  = CTE * N * DY / PLAS(I)
          ENDIF
          IF(Y(I) > SIGMX)THEN
            Y(I)  = SIGMX
            H(I)  = ZERO
          ENDIF
        ENDIF
      ENDDO

      DO I = 1,NEL
        IF(VFRAC(I) > ZERO ) THEN
          !--------------
          ! elastique
          !--------------
          FACT         = ONE
          SIGDO(1:6,I) = SIGD(1:6,I)
          SIGD(1,I)    = SIGD(1,I) + G2 * (DEPS(1,I)-DE(I))*FACT
          SIGD(2,I)    = SIGD(2,I) + G2 * (DEPS(2,I)-DE(I))*FACT
          SIGD(3,I)    = SIGD(3,I) + G2 * (DEPS(3,I)-DE(I))*FACT
          SIGD(4,I)    = SIGD(4,I) + G  * DEPS(4,I)*FACT
          SIGD(5,I)    = SIGD(5,I) + G  * DEPS(5,I)*FACT
          SIGD(6,I)    = SIGD(6,I) + G  * DEPS(6,I)*FACT
          !--------------
          ! von mises
          !--------------
          !s_vm = sqrt(3.J2),  J2 = 1/2 (s1**2 + s2**2 + s3**2) + (s4**2 + s5**2 + s6**2)   where s is deviator
          VM2(I) = THREE_HALF * ( SIGD(1,I)*SIGD(1,I) + SIGD(2,I)*SIGD(2,I) + SIGD(3,I)*SIGD(3,I) )
     .            +THREE * ( SIGD(4,I)*SIGD(4,I) + SIGD(5,I)*SIGD(5,I) + SIGD(6,I)*SIGD(6,I) )
          VM2X   = MAX(VM2X,VM2(I))
        ENDIF
      ENDDO
      DO I = 1,NEL
           IF(VFRAC(I)>BETA.AND.TEMP(I)<TMELT) THEN
             !--------------
             ! critere
             !--------------
             IF(VM2(I) > Y(I)*Y(I) .AND. Y(I) /= ZERO)THEN      
                VM      = SQRT(VM2(I))                          
                DPLA    = (VM-Y(I)) / (G3+H(I))                 
                PLAS(I) = PLAS(I) + DPLA                        
                R       = (Y(I) + H(I) * DPLA) / MAX(VM,EM15)   
                !--------------
                ! projection
                !--------------
                SIGD(1,I) = SIGD(1,I) * R  * OFF(I)
                SIGD(2,I) = SIGD(2,I) * R  * OFF(I)
                SIGD(3,I) = SIGD(3,I) * R  * OFF(I)
                SIGD(4,I) = SIGD(4,I) * R  * OFF(I)
                SIGD(5,I) = SIGD(5,I) * R  * OFF(I)
                SIGD(6,I) = SIGD(6,I) * R  * OFF(I)
              ENDIF
              IF(VFRAC(I) < BETA) PLAS(I) = ZERO                                          
              !--------------                                                                       
              ! energie                                                                             
              !--------------                                                                       
              FACT    = ONE! VFRAC(I) 
              VOL_AVG = HALF*(VFRAC(I)*VOLUME(I)+VOL(I))    
              
              EINC    = HALF    *HALF*VOL_AVG*                                                          
     .                       ( (SIGDO(1,I)+SIGD(1,I)) * DEPS(1,I)                                   
     .                       + (SIGDO(2,I)+SIGD(2,I)) * DEPS(2,I)                                   
     .                       + (SIGDO(3,I)+SIGD(3,I)) * DEPS(3,I)                                   
     .                       + (SIGDO(4,I)+SIGD(4,I)) * DEPS(4,I)                                   
     .                       + (SIGDO(5,I)+SIGD(5,I)) * DEPS(5,I)                                   
     .                       + (SIGDO(6,I)+SIGD(6,I)) * DEPS(6,I))  
              EINT(I) = EINT(I) + EINC                                                              
           ELSE
             SIGD(1,I) = ZERO        
             SIGD(2,I) = ZERO        
             SIGD(3,I) = ZERO        
             SIGD(4,I) = ZERO        
             SIGD(5,I) = ZERO        
             SIGD(6,I) = ZERO        
           ENDIF 
        ENDDO

      RETURN
      END


