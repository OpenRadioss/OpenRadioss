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
      !||====================================================================
      !||    m2lawpi        ../engine/source/materials/mat/mat002/m2lawpi.F
      !||--- called by ------------------------------------------------------
      !||    main_beam18    ../engine/source/elements/beam/main_beam18.F
      !||--- uses       -----------------------------------------------------
      !||    elbufdef_mod   ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||====================================================================
      SUBROUTINE M2LAWPI(ELBUF_STR,MAT_PARAM   ,
     1                  NEL    ,NPT    ,GEO,
     2                  EINT   ,OFF    ,ASRATE ,TIMESTEP,
     3                  PID    ,EPSP   ,EXX    ,EXY    ,EXZ,
     4                  KXX    ,KYY    ,KZZ    ,AL     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE MATPARAM_DEF_MOD
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
      INTEGER ,INTENT(IN)  :: NPT,NEL
      INTEGER ,INTENT(IN)  :: PID(NEL)
      my_real , INTENT(IN) :: TIMESTEP
      my_real , INTENT(IN) :: ASRATE
      my_real :: EXX(NEL),EXY(NEL),EXZ(NEL),KXX(NEL),KYY(NEL),KZZ(NEL),
     .   GEO(NPROPG,*), EINT(NEL,2),OFF(NEL),EPSP(NEL),AL(NEL)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE (MATPARAM_STRUCT_) ,INTENT(IN) :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,IPT,ICC,IFORM,IPY,IPZ,IPA,VP,IR,IS,ILAYER
      INTEGER :: II(3)
      my_real :: EPMX(NEL),CA(NEL), CB(NEL), CN(NEL), YMAX(NEL),
     .   TEMP(NEL),EPDR(NEL),YLD(NEL),ETSE(NEL),Q(NEL),
     .   YPT(NEL),ZPT(NEL),APT(NEL),VOL(NEL),DPLA(NEL),
     .   SIGNXX(NEL),SIGNXY(NEL),SIGNXZ(NEL),LOGEP(NEL),
     .   DEPSXX(NEL),DEPSXY(NEL),DEPSXZ(NEL),PLAP(NEL)
      my_real :: YOUNG,SHEAR,TREF,TMELT,RHOCPI,M_EXP,CC
      my_real :: SVM1,GS,MT,TSTAR,UMR,R,SHFACT,PLAP1,Z3,Z4
C
      TYPE(L_BUFEL_) ,POINTER :: LBUF  
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
C=======================================================================
      IPY  = 200        
      IPZ  = 300        
      IPA  = 400        
      SHFACT = FIVE_OVER_6
!
      Z4 = ZERO
      DO I=1,3
        II(I) = NEL*(I-1)
      ENDDO

      SHEAR  = MAT_PARAM%SHEAR 
      YOUNG  = MAT_PARAM%YOUNG 

      IFORM  = MAT_PARAM%IPARAM(1)
      ICC    = MAT_PARAM%IPARAM(2)
      VP     = MAT_PARAM%IPARAM(3)
!
      IF (IFORM == 0) THEN
        RHOCPI = MAT_PARAM%UPARAM(10)
        IF (RHOCPI > ZERO) RHOCPI = ONE / RHOCPI                
        TREF  = MAT_PARAM%UPARAM(11)             
        TMELT = MAT_PARAM%UPARAM(12)
        M_EXP = MAT_PARAM%UPARAM(13)
        MT    = MAX(EM20,M_EXP)
      ELSE
        Z3 = MAT_PARAM%UPARAM(10)
        Z4 = MAT_PARAM%UPARAM(11)
        RHOCPI    = MAT_PARAM%UPARAM(12)
        TREF  = MAT_PARAM%UPARAM(13)             
        TMELT = 0
      END IF             
      CC  = MAT_PARAM%UPARAM(6)
!
      DO I = 1,NEL                           
        CA(I)  = MAT_PARAM%UPARAM(1)
        CB(I)  = MAT_PARAM%UPARAM(2)
        CN(I)  = MAT_PARAM%UPARAM(3)
        EPMX(I)= MAT_PARAM%UPARAM(4)
        YMAX(I)= MAT_PARAM%UPARAM(5)
        IF (VP == 1) THEN
          EPDR(I) = MAX(EM20,MAT_PARAM%UPARAM(7))
        ELSE               
          EPDR(I) = MAX(EM20,MAT_PARAM%UPARAM(7)*DT1)  
        ENDIF
        EPSP(I) = MAX(EPSP(I),EPDR(I))
        VOL(I)  = AL(I)*GEO(1,PID(I))            
      ENDDO                                    
C
      DO I = 1,NEL
        TEMP(I) = TREF + RHOCPI*(EINT(I,1)+EINT(I,2))/VOL(I)
      ENDDO   
      ETSE(:) = ONE
C-------------------------------------
C     DEBUT DE BOUCLE SUR POINTS INTEGRATION
C--------------------------------------
      DO IPT= 1,NPT
 
        ILAYER=1
        IR = 1
        IS = 1
        LBUF => ELBUF_STR%BUFLY(ILAYER)%LBUF(IR,IS,IPT)
        BUFLY => ELBUF_STR%BUFLY(ILAYER)
C---    Coordonnees du point d'integration
        DO I = 1,NEL                                 
          YPT(I) = GEO(IPY+IPT,PID(I))              
          ZPT(I) = GEO(IPZ+IPT,PID(I))           
          APT(I) = GEO(IPA+IPT,PID(I))           
        ENDDO                                        
        DO I = 1,NEL
          SIGNXX(I) = LBUF%SIG(II(1)+I)
          SIGNXY(I) = LBUF%SIG(II(2)+I)
          SIGNXZ(I) = LBUF%SIG(II(3)+I)
       ENDDO        
C---    Deformations Incrementales
        DO I= 1,NEL
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
          DO I= 1,NEL
            LBUF%STRA(II(1)+I) = LBUF%STRA(II(1)+I) + DEPSXX(I)
            LBUF%STRA(II(2)+I) = LBUF%STRA(II(2)+I) + DEPSXY(I)
            LBUF%STRA(II(3)+I) = LBUF%STRA(II(3)+I) + DEPSXZ(I)
          ENDDO
        ENDIF        
C
C---    Contraintes elastiques
C
        DO I = 1,NEL
          GS = SHFACT*SHEAR                         
          SIGNXX(I) = SIGNXX(I) + YOUNG*DEPSXX(I)
          SIGNXY(I) = SIGNXY(I) + GS*DEPSXY(I)
          SIGNXZ(I) = SIGNXZ(I) + GS*DEPSXZ(I)
        ENDDO                                    
c---    strain rate dependency
        IF (VP == 1)THEN
          DO I= 1,NEL
            PLAP(I)  = BUFLY%MAT(IR,IS,IPT)%VAR(I)
            PLAP(I)  = MAX(PLAP(I),EPDR(I) )
            LOGEP(I) = LOG(PLAP(I)/EPDR(I))
          ENDDO      
        ELSE
          DO I= 1,NEL
            LOGEP(I) = LOG(EPSP(I)/EPDR(I))
          ENDDO      
        ENDIF
C--     Yield 
        IF (CC > ZERO) THEN
          IF (IFORM == 0)THEN
            DO I = 1,NEL
              TSTAR = MAX(ZERO,(TEMP(I)-TREF)/(TMELT-TREF))
              IF (TSTAR == ZERO) THEN
                Q(I) = (ONE + CC * LOGEP(I))
              ELSE
                Q(I) = (ONE + CC * LOGEP(I))*(ONE-EXP(MT*LOG(TSTAR)))
              ENDIF
              Q(I)  = MAX(Q(I),EM20)
              CA(I) = CA(I) * Q(I)
              CB(I) = CB(I) * Q(I)
              IF (ICC== 1) YMAX(I) = YMAX(I) * Q(I)
            ENDDO
          ELSEIF (IFORM == 1) THEN
            DO I = 1,NEL
              Q(I) = LOGEP(I)
              Q(I) = CC*EXP((-Z3+Z4 * Q(I))*TEMP(I))
              IF (ICC == 1) YMAX(I)= YMAX(I) + Q(I)
              CA(I) = CA(I) + Q(I)
            ENDDO
          ENDIF
        ENDIF
C---
        DO I = 1,NEL
          IF(LBUF%PLA(I) == ZERO) THEN
            YLD(I)= CA(I)                                
          ELSE                                           
            YLD(I)= CA(I) + CB(I)*EXP(CN(I)*LOG(LBUF%PLA(I)))
          ENDIF                                          
          YLD(I)  = MIN(YLD(I),YMAX(I))                  
        ENDDO
C-------------------
C       PROJECTION   -   radial return
C-------------------
        DO I = 1,NEL
          SVM1 = SIGNXX(I)**2 + THREE*(SIGNXY(I)**2 + SIGNXZ(I)**2)
          IF (SVM1 > YLD(I)**2) THEN
            SVM1 = SQRT(SVM1)                                        
            R    = MIN( ONE, YLD(I)/MAX(EM20,SVM1) )                  
            SIGNXX(I) = SIGNXX(I)*R                                  
            SIGNXY(I) = SIGNXY(I)*R                                  
            SIGNXZ(I) = SIGNXZ(I)*R                                  
            UMR = ONE - R               
            DPLA(I) =    SVM1*UMR/(YOUNG)                           
            LBUF%PLA(I) = LBUF%PLA(I) + OFF(I)* DPLA(I) 
c            IF (R < ONE) ETSE(I)= H(I)/(H(I)+YOUNG) 
          ENDIF                                   
        ENDDO 
        IF (VP == 1) THEN 
          DO I = 1,NEL
            PLAP1   = DPLA(I) / MAX(EM20,TIMESTEP)
            BUFLY%MAT(IR,IS,IPT)%VAR(I) = ASRATE * PLAP1 + (ONE - ASRATE) * PLAP(I)
          ENDDO
        ENDIF
    
        
C  failure criteria 
C--------------------------------
C     TEST DE RUPTURE DUCTILE
C--------------------------------
C      
        DO I = 1,NEL
          IF (LBUF%PLA(I) >= EPMX(I) .AND. OFF(I) == ONE) THEN
            OFF(I)=FOUR_OVER_5
          ENDIF
        ENDDO                                          
C-----------
C
        DO I = 1,NEL                                                    
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

