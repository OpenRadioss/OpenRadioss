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
!||    m2lawpi        ../engine/source/materials/mat/mat002/m2lawpi.F
!||--- called by ------------------------------------------------------
!||    main_beam18    ../engine/source/elements/beam/main_beam18.F
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod   ../common_source/modules/mat_elem/elbufdef_mod.F90
!||====================================================================
      SUBROUTINE M2LAWPI(ELBUF_STR,
     1                  JFT    ,JLT    ,NPT    ,PM     ,GEO,
     2                  EINT   ,OFF    ,IMAT,
     3                  PID    ,EPSP   ,EXX    ,EXY    ,EXZ,
     4                  KXX    ,KYY    ,KZZ    ,AL     ,NEL,
     5                  IPM    ,ASRATE ,TIMESTEP,NUMMAT,SIGY)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: IMAT,NUMMAT
      INTEGER ,DIMENSION(NPROPMI,NUMMAT) ,INTENT(IN) :: IPM
      INTEGER JFT,JLT,NPT,NEL
      INTEGER PID(NEL)
      my_real , INTENT(IN) :: TIMESTEP
      my_real , INTENT(IN) :: ASRATE
C     REAL
      my_real
     .   EXX(NEL), EXY(NEL), EXZ(NEL), 
     .   KXX(NEL), KYY(NEL), KZZ(NEL),
     .   PM(NPROPM,*), GEO(NPROPG,*), EINT(NEL,2),
     .   OFF(*),EPSP(*), AL(*) 
      my_real , INTENT(INOUT)::SIGY(NEL,NPT)
C
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,IPT,ICC,IRTY,IPY,IPZ,IPA,VP,IR,IS,ILAYER
      INTEGER :: II(3)
      my_real :: EPMX(NEL),CA(NEL), CB(NEL), CN(NEL), YMAX(NEL),
     .   T(NEL),Z3(NEL),CC(NEL),EPDR(NEL),
     .   YLD(NEL),ETSE(NEL),Q(NEL),E(NEL),G(NEL),
     .   YPT(NEL),ZPT(NEL),APT(NEL),VOL(NEL),DPLA(NEL),
     .   SIGNXX(NEL),SIGNXY(NEL),SIGNXZ(NEL),LOGEP(NEL),
     .   DEPSXX(NEL),DEPSXY(NEL),DEPSXZ(NEL),PLAP(NEL)
      my_real :: TREF,TMELT,CP,
     .   EPIF,SVM1,GS,MT,TSTAR,UMR,R,SHFACT,PLAP1,Z4
C
      TYPE(L_BUFEL_) ,POINTER :: LBUF  
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
C=======================================================================
      IPY  = 200        
      IPZ  = 300        
      IPA  = 400        
      EPIF = ZERO
      SHFACT = FIVE_OVER_6
!
      Z4 = ZERO
      DO I=1,3
        II(I) = NEL*(I-1)
      ENDDO
!
C---
      VP   = IPM(255,IMAT) 
      IRTY = NINT(PM(50,IMAT))
      IF (IRTY == 0) THEN
        TREF  = PM(79,IMAT)                    
        TMELT = PM(80,IMAT)                    
        CP    = ONE / MAX(EM20,PM(69,IMAT))                  
      ELSE
        Z4    = PM(52,IMAT)                    
        CP    = PM(53,IMAT)                    
        TREF  = PM(54,IMAT)                    
        TMELT = 0
      END IF             
!
      ICC  = NINT(PM(49,IMAT))              
      DO I=JFT,JLT                           
        E(I)    = PM(20,IMAT)                    
        G(I)    = PM(22,IMAT)                    
        CA(I)   = PM(38,IMAT)                    
        CB(I)   = PM(39,IMAT)                    
        CN(I)   = PM(40,IMAT)                    
        EPMX(I) = PM(41,IMAT)                    
        YMAX(I) = PM(42,IMAT)                    
        CC(I)   = PM(43,IMAT)    
        IF(VP == 1)THEN
          EPDR(I) = MAX(EM20,PM(44,IMAT))
        ELSE               
          EPDR(I) = MAX(EM20,PM(44,IMAT)*DT1)  
        ENDIF
        EPIF    = MAX(EPIF,CC(I))              
        Z3(I)   = PM(51,IMAT)                    
        EPSP(I) = MAX(EPSP(I),EPDR(I))
        VOL(I)  = AL(I)*GEO(1,PID(I))            
      ENDDO                                    
C
      DO I=JFT,JLT
        T(I) = TREF + CP*(EINT(I,1)+EINT(I,2))/VOL(I)
      ENDDO   
C-------------------------------------
C     DEBUT DE BOUCLE SUR POINTS INTEGRATION
C--------------------------------------
C        
      DO IPT= 1,NPT
 
        ILAYER=1
        IR = 1
        IS = 1
        LBUF => ELBUF_STR%BUFLY(ILAYER)%LBUF(IR,IS,IPT)
        BUFLY => ELBUF_STR%BUFLY(ILAYER)
C---    Coordonnees du point d'integration
        DO I=JFT,JLT                                 
          YPT(I) = GEO(IPY+IPT,PID(I))              
          ZPT(I) = GEO(IPZ+IPT,PID(I))           
          APT(I) = GEO(IPA+IPT,PID(I))           
        ENDDO                                        
        DO I=JFT,JLT
          SIGNXX(I) = LBUF%SIG(II(1)+I)
          SIGNXY(I) = LBUF%SIG(II(2)+I)
          SIGNXZ(I) = LBUF%SIG(II(3)+I)
       ENDDO        
C---    Deformations Incrementales
        DO I= JFT,JLT
          DEPSXX(I) = EXX(I) - YPT(I)*KZZ(I) + ZPT(I)*KYY(I)
          DEPSXY(I) = EXY(I) + ZPT(I)*KXX(I) 
          DEPSXZ(I) = EXZ(I) - YPT(I)*KXX(I)
          DEPSXY(I) = DEPSXY(I) / SHFACT
          DEPSXZ(I) = DEPSXZ(I) / SHFACT 
        ENDDO
C
c---    Total strain
C 
        IF (BUFLY%L_STRA > 0) THEN
          DO I= JFT,JLT
            LBUF%STRA(II(1)+I) = LBUF%STRA(II(1)+I) + DEPSXX(I)
            LBUF%STRA(II(2)+I) = LBUF%STRA(II(2)+I) + DEPSXY(I)
            LBUF%STRA(II(3)+I) = LBUF%STRA(II(3)+I) + DEPSXZ(I)
          ENDDO
        ENDIF        
C
C---    Contraintes elastiques
C
        DO I = JFT,JLT
          GS = SHFACT*G(I)                         
          SIGNXX(I) = SIGNXX(I) + E(I)*DEPSXX(I)
          SIGNXY(I) = SIGNXY(I) + GS*DEPSXY(I)
          SIGNXZ(I) = SIGNXZ(I) + GS*DEPSXZ(I)
          ETSE(I)   = ONE                        
        ENDDO                                    
        DO I=JFT,JLT
          ETSE(I) = ONE
          CA(I)   = PM(38,IMAT)                  
          CB(I)   = PM(39,IMAT)                  
          YMAX(I) = PM(42,IMAT)                  
        ENDDO
c---    strain rate dependency
        IF(VP == 1)THEN
          DO I= JFT,JLT
            PLAP(I)  = BUFLY%MAT(IR,IS,IPT)%VAR(I)
            PLAP(I)  = MAX(PLAP(I),EPDR(I) )
            LOGEP(I) = LOG(PLAP(I)/EPDR(I))
          ENDDO      
        ELSE
          DO I= JFT,JLT
            LOGEP(I) = LOG(EPSP(I)/EPDR(I))
          ENDDO      
        ENDIF
C--     Yield 
        IF (EPIF /= ZERO) THEN
          IF (IRTY == 0)THEN
            DO I=JFT,JLT
              MT      = MAX(EM20,Z3(I))
              TSTAR   = MAX(ZERO,(T(I)-TREF)/(TMELT-TREF))
              IF (TSTAR == ZERO) THEN
                Q(I) = (ONE + CC(I) * LOGEP(I))
              ELSE
                Q(I) = (ONE + CC(I) * LOGEP(I))*(ONE-EXP(MT*LOG(TSTAR)))
              ENDIF
              Q(I)  = MAX(Q(I),EM20)
              CA(I) = CA(I) * Q(I)
              CB(I) = CB(I) * Q(I)
              IF (ICC== 1) YMAX(I) = YMAX(I) * Q(I)
            ENDDO
          ELSEIF (IRTY == 1) THEN
            DO I=JFT,JLT
              Q(I) = LOGEP(I)
              Q(I) = CC(I)*EXP((-Z3(I)+Z4 * Q(I))*T(I))
              IF (ICC == 1) YMAX(I)= YMAX(I) + Q(I)
              CA(I) = CA(I) + Q(I)
            ENDDO
          ENDIF
        ENDIF
C---
        DO I=JFT,JLT
          IF(LBUF%PLA(I) == ZERO) THEN
            YLD(I)= CA(I)                                
          ELSE                                           
            YLD(I)= CA(I) + CB(I)*EXP(CN(I)*LOG(LBUF%PLA(I)))
          ENDIF                                          
          YLD(I)  = MIN(YLD(I),YMAX(I))      
          SIGY(I,IPT) = YLD(I)          
        ENDDO
C-------------------
C       PROJECTION   -   radial return
C-------------------
        DO I=JFT,JLT
          SVM1 = SIGNXX(I)**2 + THREE*(SIGNXY(I)**2 + SIGNXZ(I)**2)
          IF (SVM1 > YLD(I)**2) THEN
            SVM1 = SQRT(SVM1)                                        
            R    = MIN( ONE, YLD(I)/MAX(EM20,SVM1) )                  
            SIGNXX(I) = SIGNXX(I)*R                                  
            SIGNXY(I) = SIGNXY(I)*R                                  
            SIGNXZ(I) = SIGNXZ(I)*R                                  
            UMR = ONE - R               
            DPLA(I) =    SVM1*UMR/(E(I))                           
            LBUF%PLA(I) = LBUF%PLA(I) + OFF(I)* DPLA(I) 
c            IF (R < ONE) ETSE(I)= H(I)/(H(I)+E(I)) 
          ENDIF                                   
        ENDDO 
        IF (VP == 1) THEN 
          DO I=JFT,JLT
            PLAP1   = DPLA(I) / MAX(EM20,TIMESTEP)
            BUFLY%MAT(IR,IS,IPT)%VAR(I) = ASRATE * PLAP1 + (ONE - ASRATE) * PLAP(I)
          ENDDO
        ENDIF
    
        
C  failure criteria 
C--------------------------------
C     TEST DE RUPTURE DUCTILE
C--------------------------------
C      
        DO  I=JFT,JLT
          IF (LBUF%PLA(I) >= EPMX(I) .AND. OFF(I) == ONE) THEN
            OFF(I)=FOUR_OVER_5
          ENDIF
        ENDDO                                          
C-----------
C
        DO I=JFT,JLT                                                    
          LBUF%SIG(II(1)+I) = SIGNXX(I)
          LBUF%SIG(II(2)+I) = SIGNXY(I)
          LBUF%SIG(II(3)+I) = SIGNXZ(I)
        ENDDO                                                       
C-------------------------------------
C       FIN DE BOUCLE SUR POINT INTEGRATION
C-------------------------------------
       ENDDO   
C-----------
      RETURN
      END

