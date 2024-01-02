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
Chd|  PHASEKINETIC2                 source/materials/mat/mat080/phasekinetic2.F
Chd|-- called by -----------
Chd|        SIGEPS80                      source/materials/mat/mat080/sigeps80.F
Chd|        SIGEPS80C                     source/materials/mat/mat080/sigeps80c.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PHASEKINETIC2(NEL0,TIME,TEMPEL,TEMPO,TEMPMIN,AE1,AE3,BS,MS,FCFER,
     .  FCPER,FCBAI,FGRAIN,FRAC1,FRAC2,FRAC3,FRAC4,FRAC5,X2,X3,X4,X5,
     .     GFAC_F,PHI_F,PSI_F,CR_F,CF,GFAC_P,PHI_P,PSI_P,CR_P,CP,
     .     GFAC_B,PHI_B,PSI_B,CR_B,CB,PHI_M,PSI_M,N_M,FGFER,FGPER,FGBAI,
     .  QR2,QR3,QR4,KPER,KBAIN,ALPHA,XEQ2,XEQ4,XGAMA,TOTFRAC,TIMESTEP,NICOOL,INDEX)
     
#include      "implicit_f.inc"
#include      "scr_thermal_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
        INTEGER NEL0,NICOOL,INDEX(NICOOL)
        my_real
     .        TEMPEL(NEL0),TEMPO(NEL0),TEMPMIN(NEL0),TIMESTEP
        my_real
     .        TIME,ALPHA,TREF,AE1,AE3,BS,MS,GSIZE,NU,FCFER,FCPER,FCBAI,
     .        FGRAIN,QR2,QR3,QR4,KPER,KBAIN,XEQ2,XEQ4,YX,
     .        GFAC_F,PHI_F,PSI_F,CR_F,CF,GFAC_P,PHI_P,PSI_P,CR_P,CP,
     .        GFAC_B,PHI_B,PSI_B,CR_B,CB,PHI_M,PSI_M,N_M,FGFER,FGPER,FGBAI
C-----------------------------------------------
        my_real
     .        FRAC1(NEL0),FRAC2(NEL0),FRAC3(NEL0),FRAC4(NEL0),FRAC5(NEL0)
        my_real
     .        X2(NEL0),X3(NEL0),X4(NEL0),X5(NEL0),TOTFRAC(NEL0),XGAMA(NEL0)
        my_real
     .        FTEMP,UX,VX,UDOT,VDOT,F,CONST,FDOT,X2OLD(NEL0),
     .        X3OLD(NEL0),X4OLD(NEL0),X5OLD(NEL0),GX,GDOT,DTI,TERM1,TERM2 ,TERM3
        INTEGER I,K,J 
C-----------------------------------------------
      ! SUM OF FRAC1+FRAC2+FRAC3+FRAC4+FRAC5=1--------
       DTI= ONE/MAX(TIMESTEP*THEACCFACT,EM10)
      DO J=1,NICOOL ! NEL0

       I=INDEX(J)
       ! CHECK TEMPERATURE FOR CORRESPONDING PHASE CHANGE
       IF(TOTFRAC(I)<ONE.AND.TEMPEL(I)<TEMPMIN(I))THEN
       IF (TEMPEL(I)<AE3)THEN 
        IF(TEMPEL(I)>AE1)THEN
          IF(X2(I)<0.999)THEN
           ! FERRITE FORMATION X2
           !X2OLD(I)= FRAC2(I)/XEQ2
           X2OLD(I)=X2(I) 
           IF(X2(I)==ZERO)X2(I)=EM10
           FTEMP = EXP(-QR2/TEMPEL(I))*ABS(TEMPEL(I)-AE3)**3
           CONST = FTEMP * FGFER * CF
           DO K=1,4
              UX = X2(I)     **(PHI_F*(ONE-X2(I)))
              VX = (ONE-X2(I))**(PSI_F*X2(I)) 
              GX = EXP(CR_F *X2(I)*X2(I))
              IF (GX<ZERO) GX=ONE
              F =(X2(I)-X2OLD(I))*DTI - CONST * UX*VX /GX
              UDOT = PHI_F* UX * ((ONE-X2(I))/MAX(EM10,X2(I))-LOG(MAX(EM10,X2(I))))                        
              VDOT = PSI_F* VX * (LOG(ONE-X2(I))-X2(I)/(ONE-X2(I)))
              GDOT = TWO * CR_F * X2(I) * GX
              FDOT = DTI-CONST*((UDOT*VX+VDOT*UX)*GX-UX*VX*GDOT)/GX/GX
              X2(I)= MAX(EM20,X2(I)-F/FDOT)
           ENDDO
           FRAC2(I)=X2(I)*XEQ2
           FRAC1(I)=ONE-FRAC2(I)-FRAC3(I)-FRAC4(I)-FRAC5(I)
          ENDIF
c---------------------------------          
        ELSEIF(TEMPEL(I)>BS)THEN 
          
         IF(X3(I)<0.999)THEN
            ! PEARLITE X3                                       
            X3OLD(I)= X3(I)  !FRAC3(I)/(ONE-XEQ2) !!
            IF(X3(I)==ZERO)X3(I)=EM10
            FTEMP=6.17*EXP(-QR3/TEMPEL(I))*ABS(TEMPEL(I)-AE1)**3
            CONST=FTEMP* FGPER *CP !!! adjusted
            DO K=1,4
              UX = X3(I)     **(PHI_P*(ONE-X3(I)))
              VX = (ONE-X3(I))**(PSI_P*X3(I)) 
              GX = EXP(CR_P *X3(I)*X3(I))
              IF (GX<ZERO) GX=ONE
              F =(X3(I)-X3OLD(I))*DTI - CONST * UX*VX /GX
              UDOT = PHI_P* UX * ((ONE-X3(I))/MAX(EM10,X3(I))-LOG(MAX(EM10,X3(I))))                        
              VDOT = PSI_P* VX * (LOG(ONE-X3(I))-X3(I)/(ONE-X3(I)))
              GDOT = TWO * CR_P * X3(I) * GX
              FDOT = DTI-CONST*((UDOT*VX+VDOT*UX)*GX-UX*VX*GDOT)/GX/GX
              X3(I)= MAX(EM20,X3(I)-F/FDOT)
            ENDDO       
c            FRAC3(I)=X3(I)*(ONE-X2(I))
            FRAC3(I)=X3(I)*(ONE-XEQ2)
            FRAC1(I)=ONE-FRAC2(I)-FRAC3(I)-FRAC4(I)-FRAC5(I)
            ! CHECK IF FERRITE FORMATION
          ENDIF
c---------------------------------                    
        ELSEIF(TEMPEL(I)>MS)THEN
          IF(X4(I)<0.999)THEN
           ! BAINITE  FORMATION 
            X4OLD(I) =  X4(I)  
            IF(X4(I) == ZERO) X4(I) = X3(I)  
            ! INITIALISE AU TAUX DE PERLITE
            FTEMP=EXP(-QR4/TEMPEL(I)) *(TEMPEL(I)-BS)**2
            CONST=FTEMP* FGBAI *CB
            DO K=1,4
              UX = X4(I)     **(PHI_B*(ONE-X4(I)))
              VX = (ONE-X4(I))**(PSI_B*X4(I)) 
              GX = EXP(CR_B *X4(I)*X4(I))
              IF (GX<ZERO) GX=ONE
              F =(X4(I)-X4OLD(I))*DTI - CONST * UX*VX /GX
              UDOT = PHI_B* UX * ( (ONE-X4(I))/MAX(EM10,X4(I))-LOG(MAX(EM10,X4(I))) )                        
              VDOT = PSI_B* VX * (LOG(ONE-X4(I))-X4(I)/(ONE-X4(I)))
              GDOT = TWO * CR_B * X4(I) * GX
              FDOT = DTI-CONST*((UDOT*VX+VDOT*UX)*GX-UX*VX*GDOT)/GX/GX
              X4(I)= MAX(EM20,X4(I)-F/FDOT)
           ENDDO
            FRAC4(I)=X4(I)*(ONE-X2(I))
            FRAC1(I)=ONE-FRAC2(I)-FRAC3(I)-FRAC4(I)-FRAC5(I)
          ENDIF
            
        ELSE           
          ! MARTENSITE  FORMATION X5
          IF (FRAC5(I)==ZERO)XGAMA(I)= FRAC1(I)
          X5OLD(I) =  X5(I)  
          TERM1 = ALPHA*(MS-TEMPEL(I))**N_M
          TERM2 = MAX(EM20,X5(I))     ** PHI_M
          IF(PSI_M >=ZERO) THEN
             TERM3 = (ONE - X5(I)) ** PSI_M
          ELSE
             TERM3 = (ONE - X5(I)) ** (PSI_M*TWO -XGAMA(I) )
          ENDIF
          X5(I) = X5OLD(I) + (TERM1 * TERM2 *TERM3)*(TEMPO(I)-TEMPEL(I))
          FRAC5(I)=X5(I)*XGAMA(I)
          FRAC1(I)=ONE-FRAC2(I)-FRAC3(I)-FRAC4(I)-FRAC5(I)
         ENDIF


       ENDIF   
       ENDIF   
      ENDDO    
     
      RETURN
      END
