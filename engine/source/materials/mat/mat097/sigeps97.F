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
Chd|  SIGEPS97                      source/materials/mat/mat097/sigeps97.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE SIGEPS97 (
     1     NEL    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC    ,TBURN  ,
     2     NPF    ,TF     ,TIME    ,TIMESTEP,UPARAM   ,BFRAC  ,
     3     RHO0   ,RHO    ,VOLUME  ,EINT    ,SIGY     ,DELTAX ,
     4     EPSPXX ,EPSPYY ,EPSPZZ  ,EPSPXY  ,EPSPYZ   ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSZZ  ,DEPSXY  ,DEPSYZ   ,DEPSZX ,
     6     EPSXX  ,EPSYY  ,EPSZZ   ,EPSXY   ,EPSYZ    ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOZZ  ,SIGOXY  ,SIGOYZ   ,SIGOZX ,
     8     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ   ,SIGNZX ,
     9     SIGVXX ,SIGVYY ,SIGVZZ  ,SIGVXY  ,SIGVYZ   ,SIGVZX ,
     A     SOUNDSP,VISCMAX,UVAR    ,OFF     ,NFT      ,V      ,
     B     W      ,X      ,IX      ,N48     ,NIX      ,JTHE   ,
     C     GEO    ,PID    ,ILAY    ,NG      ,ELBUF_TAB,PM     , 
     D     IPARG  ,BUFVOIS ,IPM     ,BUFMAT   ,STIFN  ,
     E     VD2    ,VDX    ,VDY     ,VDZ     ,MAT      ,VOLN   ,
     F     QNEW   ,DDVOL  ,QOLD)  
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD   
      USE I22BUFBRIC_MOD
      USE I22TRI_MOD       
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com06_c.inc"
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "mvsiz_p.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL, NUPARAM, NUVAR,NFT,N48,NIX,JTHE,
     .        IX(NIX,*), PID(*), ILAY, NG,PM(NPROPM,*),IPARG(*),MAT(*),
     .        IPM(NPROPMI,*)
      my_real 
     .   TIME         ,TIMESTEP   ,UPARAM(NUPARAM),
     .   SIGY(NEL)    ,VK(NEL)    ,
     .   RHO(NEL)     ,RHO0(NEL)  ,VOLUME(NEL),
     .   EINT(NEL)    ,BUFVOIS(*) ,QNEW(NEL)    ,
     .   DDVOL(NEL)   ,QOLD(NEL)  ,
     .   EPSPXX(NEL)  ,EPSPYY(NEL),EPSPZZ(NEL),
     .   EPSPXY(NEL)  ,EPSPYZ(NEL),EPSPZX(NEL),
     .   DEPSXX(NEL)  ,DEPSYY(NEL),DEPSZZ(NEL),
     .   DEPSXY(NEL)  ,DEPSYZ(NEL),DEPSZX(NEL),
     .   EPSXX(NEL)   ,EPSYY(NEL) ,EPSZZ(NEL) ,
     .   EPSXY(NEL)   ,EPSYZ(NEL) ,EPSZX(NEL) ,
     .   SIGOXX(NEL)  ,SIGOYY(NEL),SIGOZZ(NEL),
     .   SIGOXY(NEL)  ,SIGOYZ(NEL),SIGOZX(NEL),
     .   V(3,*)       ,W(3,*)     ,X(3,*)     ,
     .   GEO(NPROPG,*),BUFMAT(*)  ,
     .   STIFN(MVSIZ) ,VD2(*)     ,VDX(*)     ,
     .   VDY(*)       ,VDZ(*)     ,SSP        ,
     .   VOLN(*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SIGVXX(NEL),SIGVYY(NEL),SIGVZZ(NEL),
     .    SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .    SOUNDSP(NEL),VISCMAX(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real UVAR(NEL,NUVAR), OFF(NEL), TBURN(NEL), BFRAC(NEL), DELTAX(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real FINTER ,TF(*)
      EXTERNAL FINTER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
c      my_real
c     .     B1, B2, R1, R2, W1, D, PCJ, E0, C1, VCJ,
c     .     EADD, TBEGIN, TEND, TFEXTT,BHE,
c     .     PSH,REACTION_RATE,REACTION_RATE2,A_MIL,M_MIL,N_MIL,ALPHA_UNIT,
c     .     DPDMU,
c     .     R1V, R2V, ER1M, ER2M, MUP1, MU, AA,BB, P0, P, VOLD, POLD,
c     .     WDR1V,WDR2V,DR1V,DR2V,ER1V,ER2V, PNEW,VOLO,FACM,ESPE,DVOL,
c     .     W1DF,EINC,QA,QB,QAL,QBL,DD     
c      INTEGER :: IBID, IBFRAC, QOPT, I      
c      my_real :: XL, MASS, DF, TB
      
      my_real
     .     W1, D, PCJ, E0, P0, VCJ,C,PSH,
     .     A(5),R(5),AL(5),BL(5),RL(5),
     .     DPDMU,
     .     PNEW,FACM,ESPE,DVOL,
     .     QA,QB,QAL,QBL,DD,TFEXTT,BHE, 
     .     LAMBDA1,LAMBDA2,LAMBDA3,LAMBDA4,LAMBDA5,
     .     LAMBDA,
     .     DLDV1,DLDV2,DLDV3,DLDV4,DLDV5,
     .     DLDV,
     .     ERLV1,ERLV2,ERLV3,ERLV4,ERLV5,
     .     P1,P2,P3,P4,P5,
     .     RHOC2,
     .     RV1,RV2,RV3,RV4,RV5,
     .     RHOC2_1,RHOC2_2,RHOC2_3,RHOC2_4,RHOC2_5,
     .     Dv, vv, vv_,
     .     EINC,
     .     WW, DENOM, POLD, P, V0
     
      INTEGER :: IBID, IBFRAC, I      
      my_real :: XL, MASS, DF, TB
      
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
      TYPE(L_BUFEL_)  ,POINTER :: LBUF      
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY  
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      TFEXTT =  ZERO
      GBUF   => ELBUF_TAB(NG)%GBUF           
      LBUF   => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(1,1,1)
      BUFLY  => ELBUF_TAB(NG)%BUFLY(ILAY)      
      

C-----------------------------------------------
C   S t a n d a r d   J W L   E O S 
C-----------------------------------------------  
    !IN CASE OF SWITCHING LAW5 LATER FROM LECMAT/MMAIN TO LECMUSER/MULAW
      
c      E0                 = UPARAM(1)   
c      P0                 = UPARAM(2)          
c      B1                 = UPARAM(4)   
c      B2                 = UPARAM(5)   
c      R1                 = UPARAM(6)   
c      R2                 = UPARAM(7)   
c      W1                 = UPARAM(8)   
c      D                  = UPARAM(9)   
c      PCJ                = UPARAM(10)  
c      BHE                = UPARAM(11)
c      IBFRAC             = UPARAM(12)  
c      QOPT               = UPARAM(13)         
c      EADD               = UPARAM(14)  
c      TBEGIN             = UPARAM(15)  
c      TEND               = UPARAM(16)  
c      REACTION_RATE      = UPARAM(17)  
c      A_MIL              = UPARAM(18)  
c      M_MIL              = UPARAM(19)  
c      N_MIL              = UPARAM(20)  
c      REACTION_RATE2     = UPARAM(21)  
c      ALPHA_UNIT         = UPARAM(22)  
c      PSH                = UPARAM(23) 
c                 
c      IF(DT1==ZERO)THEN
c        DO I=1,NEL
c          EINT(I) = E0*VOLN(I)
c        ENDDO
c        UVAR(1:NEL,4)    = VOLN(1:NEL)   !VOLD
c        UVAR(1:NEL,5)    = P0            !POLD
c      ENDIF
c      DO I=1,NEL     
c        !--------------------------------!
c        ! Calculation of BFRAC in [0,1]  !
c        !--------------------------------!
c        XL = DELTAX(I)  !VOLN(I)**THIRD
c        IF(BFRAC(I) < ONE) THEN
c         TB = - TBURN(I)
c         BFRAC(I) = ZERO
c         IF(IBFRAC/=1 .AND. TIME > TB) BFRAC(I) = D*(TIME-TB)*TWO_THIRD/XL 
c         IF(IBFRAC/=2) BFRAC(I)  = MAX( BFRAC(I) , BHE * (ONE - RHO0(I)/RHO(I)) )
c         IF(BFRAC(I) < EM04) THEN
c           BFRAC(I) = ZERO
c         ELSEIF(BFRAC(I) > ONE) THEN
c           BFRAC(I) = ONE
c         ENDIF
c        ENDIF
c        !--------------------------------!
c        ! EOS SOLVING                    !
c        !--------------------------------!
c        DF            = RHO0(I)/RHO(I)
c        R1V           = B1*W1/(R1*DF)
c        R2V           = B2*W1/(R2*DF)
c        WDR1V         = B1-R1V
c        WDR2V         = B2-R2V
c        DR1V          = W1*EINT(I)/MAX(EM20,VOLN(I))    !w*Eint/V = w*E/v  where v=V/V0
c        ER1V          = EXP(-R1*DF)
c        ER2V          = EXP(-R2*DF)
c        P             = P0 - PSH + WDR1V*ER1V+WDR2V*ER2V+DR1V
c        P             = MAX(ZERO - PSH, P)
c        SSP           = B1*ER1V*( (-W1/DF/R1) + R1*DF - W1)
c     .                + B2*ER2V*( (-W1/DF/R2) + R2*DF - W1)
c     .                + DR1V  +   (P + PSH)*W1
c        SSP           = SSP * DF 
c        SSP           = SQRT(ABS(SSP)/RHO0(I))
c        SSP           = MAX(SSP,D*(ONE-BFRAC(I)))
c        QA            = GEO(14,PID(I))
c        QB            = GEO(15,PID(I))
c        DD            = -EPSPXX(I)-EPSPYY(I)-EPSPZZ(I)
c        QAL           = QA*XL
c        QAL           = QAL*QAL
c        QBL           = QB*XL
c        VISCMAX(I)    = RHO(I)*(QAL*MAX(ZERO,DD) + QBL*SSP)
c        QNEW(I)       = VISCMAX(I)*MAX(ZERO,DD)
c        viscmax(I) = zero
c        !QNEW(I) = zero !QOLD(I)
c        VOLD          = UVAR(I,4)
c        POLD          = -UVAR(I,5)
c        DVOL          = HALF*(VOLN(I)-VOLD)        
c        EINC          = DVOL*(POLD-PSH-QOLD(I)-QNEW(I))
c        !EINT(I)       = EINT(I)+EINC      
c        QOLD(I)       = QNEW(I)
c        VOLO          = VOLN(I)/DF
c        ESPE          = (EINT(I)+EINC)/MAX(EM20,VOLO)
c        W1DF          = BFRAC(I)*W1/DF
c        FACM          = BFRAC(I)*(WDR1V*ER1V+WDR2V*ER2V) 
c        PNEW          = P0 - PSH + (FACM+ESPE*W1DF)/(ONE +W1DF*DVOL/MAX(EM20,VOLO))
c        PNEW          = MAX(ZERO - PSH, PNEW)*OFF(I)
c        EINC          = EINC-(PNEW + PSH)*DVOL
c       ! EINT(I)       = (EINT(I)-(PNEW + PSH)*DVOL)!/MAX(EM20,VOLN(I))
c        UVAR(I,4)     = VOLN(I)  
c        UVAR(I,5)     = PNEW  
c        !--------------------------------!
c        ! Returning values               !
c        !--------------------------------!     
c        SIGNXX(I)  = -PNEW
c        SIGNYY(I)  = -PNEW
c        SIGNZZ(I)  = -PNEW
c        SIGNXY(I)  = ZERO
c        SIGNYZ(I)  = ZERO
c        SIGNZX(I)  = ZERO
c        SIGVXX(I)  = ZERO
c        SIGVYY(I)  = ZERO
c        SIGVZZ(I)  = ZERO
c        SIGVXY(I)  = ZERO
c        SIGVYZ(I)  = ZERO
c        SIGVZX(I)  = ZERO
c       
c        SOUNDSP(I) = SSP
c      ENDDO! next I   


C-----------------------------------------------
C   J W L - B A K E R   E O S 
C-----------------------------------------------

      P0     = UPARAM(01)    
      PSH    = UPARAM(02)    
      IBFRAC = UPARAM(03)    
      D      = UPARAM(04)    
      PCJ    = UPARAM(05)    
      E0     = UPARAM(06)    
      WW     = UPARAM(07)    
      C      = UPARAM(08)    
      A(1:5) = UPARAM(09:13) 
      R(1:5) = UPARAM(14:18) 
      AL(1:5)= UPARAM(19:23) 
      BL(1:5)= UPARAM(24:28) 
      RL(1:5)= UPARAM(29:33) 
      BHE    = UPARAM(34)    
      VCJ    = UPARAM(35)

      
      IF(DT1==ZERO)THEN
        DO I=1,NEL
          EINT(I) = E0*VOLN(I)
        ENDDO
        UVAR(1:NEL,4)    = VOLN(1:NEL)   !VOLD
        UVAR(1:NEL,5)    = P0            !POLD
      ENDIF

      DO I=1,NEL     
        !--------------------------------!
        ! Calculation of BFRAC in [0,1]  !
        !--------------------------------!
        XL = DELTAX(I)  !VOLN(I)**THIRD
        IF(BFRAC(I) < ONE) THEN
         TB = - TBURN(I)
         BFRAC(I) = ZERO
         IF(IBFRAC/=1 .AND. TIME > TB) BFRAC(I) = D*(TIME-TB)*TWO_THIRD/XL 
         IF(IBFRAC/=2) BFRAC(I)  = MAX( BFRAC(I) , BHE * (ONE - RHO0(I)/RHO(I)) )
         IF(BFRAC(I) < EM04) THEN
           BFRAC(I) = ZERO
         ELSEIF(BFRAC(I) > ONE) THEN
           BFRAC(I) = ONE
         ENDIF
        ENDIF
        !--------------------------------!
        ! EOS SOLVING                    !
        !--------------------------------!
        DF            = RHO0(I)/RHO(I)
        V0            = RHO(I)*VOLN(I) / RHO0(I)
        ESPE          = EINT(I)/MAX(EM20,V0)
        
        ! REMINDER
        !UVAR(I,4)     = VOLN(I)  
        !UVAR(I,5)     = PNEW  

                
        vv            = VOLN(I)   / V0   ! current relative volume
        vv_           = UVAR(I,4) / V0   ! previous relative volume
        Dv            = vv-vv_

        ERLV1         = EXP(-RL(1)*vv)
        ERLV2         = EXP(-RL(2)*vv)
        ERLV3         = EXP(-RL(3)*vv)
        ERLV4         = EXP(-RL(4)*vv)
        ERLV5         = EXP(-RL(5)*vv)

        LAMBDA1       = (AL(1)*vv+BL(1))*ERLV1
        LAMBDA2       = (AL(2)*vv+BL(2))*ERLV2
        LAMBDA3       = (AL(3)*vv+BL(3))*ERLV3
        LAMBDA4       = (AL(4)*vv+BL(4))*ERLV4
        LAMBDA5       = (AL(5)*vv+BL(5))*ERLV5
        
        LAMBDA        = LAMBDA1 + LAMBDA2 + LAMBDA3 + LAMBDA4 + LAMBDA5 + WW
        
        DLDV1         = AL(1)*ERLV1-(AL(1)*vv+BL(1))*RL(1)*ERLV1
        DLDV2         = AL(2)*ERLV2-(AL(2)*vv+BL(2))*RL(2)*ERLV2
        DLDV3         = AL(3)*ERLV3-(AL(3)*vv+BL(3))*RL(3)*ERLV3
        DLDV4         = AL(4)*ERLV4-(AL(4)*vv+BL(4))*RL(4)*ERLV4
        DLDV5         = AL(5)*ERLV5-(AL(5)*vv+BL(5))*RL(5)*ERLV5
        
        DLDV          = DLDV1 + DLDV2 + DLDV3 + DLDV4 + DLDV5
        
        RV1           = R(1)*vv
        RV2           = R(2)*vv
        RV3           = R(3)*vv
        RV4           = R(4)*vv
        RV5           = R(5)*vv

        P1            = A(1)*(ONE-LAMBDA/RV1)*EXP(-RV1)
        P2            = A(2)*(ONE-LAMBDA/RV2)*EXP(-RV2)
        P3            = A(3)*(ONE-LAMBDA/RV3)*EXP(-RV3)
        P4            = A(4)*(ONE-LAMBDA/RV4)*EXP(-RV4)
        P5            = A(5)*(ONE-LAMBDA/RV5)*EXP(-RV5)
        
        P             = P1+P2+P3+P4+P5 + LAMBDA*ESPE/vv + C*(ONE-LAMBDA/WW)*EXP((-WW-ONE)*LOG(vv))
         
        RHOC2_1       = A(1)*( (vv*DLDV-LAMBDA)/R(1) + R(1)*vv*vv - LAMBDA*vv )*EXP(-RV1)
        RHOC2_2       = A(2)*( (vv*DLDV-LAMBDA)/R(2) + R(2)*vv*vv - LAMBDA*vv )*EXP(-RV2)
        RHOC2_3       = A(3)*( (vv*DLDV-LAMBDA)/R(3) + R(3)*vv*vv - LAMBDA*vv )*EXP(-RV3)
        RHOC2_4       = A(4)*( (vv*DLDV-LAMBDA)/R(4) + R(4)*vv*vv - LAMBDA*vv )*EXP(-RV4)
        RHOC2_5       = A(5)*( (vv*DLDV-LAMBDA)/R(5) + R(5)*vv*vv - LAMBDA*vv )*EXP(-RV5)
        
        !EINC          = -HALF*(UVAR(I,5)+P+PSH+PSH)*Dv
        
        RHOC2         = RHOC2_1 + RHOC2_2 + RHOC2_3 + RHOC2_4 + RHOC2_5 
        RHOC2         = RHOC2 + C*((WW+ONE)*(ONE-LAMBDA/WW)+vv*DLDV/WW)*EXP(-WW*LOG(vv)) 
        !RHOC2         = RHOC2 + (ESPE+EINC)*LAMBDA + LAMBDA*vv*P - (ESPE+EINC)*vv*DLDV
        RHOC2         = RHOC2 + (ESPE)*LAMBDA + LAMBDA*vv*(P+PSH) - (ESPE)*vv*DLDV         

        SSP           = SQRT(MAX(RHOC2/RHO0(I),EM20)) 
        QA            = GEO(14,PID(I))
        QB            = GEO(15,PID(I))
        DD            = -EPSPXX(I)-EPSPYY(I)-EPSPZZ(I)
        QAL           = QA*XL
        QAL           = QAL*QAL
        QBL           = QB*XL
        VISCMAX(I)    = RHO(I)*(QAL*MAX(ZERO,DD) + QBL*SSP)
        QNEW(I)       = VISCMAX(I)*MAX(ZERO,DD)
        !viscmax(I)    = zero
        
        DENOM         = (ONE+HALF*Dv*LAMBDA/vv) 
        DENOM         = ONE/DENOM
        
        !CURRENT PRESSURE
        PNEW          = P - LAMBDA/vv*HALF*(UVAR(I,5)+PSH)*Dv
        PNEW          = DENOM * PNEW
        PNEW          = (ONE-BFRAC(I))*P0 + BFRAC(I)*PNEW      
        PNEW          = MAX(-PSH, PNEW-PSH)*OFF(I)
        
        !CURRENT ENERGY
        !POLD          = UVAR(I,5)
        DVOL          = HALF*(VOLN(I)-UVAR(I,4))        
        !EINC          = DVOL*(-PNEW-PSH-POLD-PSH-QOLD(I)-QNEW(I))
        EINT(I)        = EINT(I) - (PSH+PSH)*DVOL
        
        !BACKUP FOR NEXT CYCLE
        QOLD(I)       = QNEW(I)
        UVAR(I,4)     = VOLN(I)  
        UVAR(I,5)     = PNEW  
        
        !--------------------------------!
        ! Returning values               !
        !--------------------------------!     

        SIGNXX(I)  = -PNEW
        SIGNYY(I)  = -PNEW
        SIGNZZ(I)  = -PNEW
        SIGNXY(I)  = ZERO
        SIGNYZ(I)  = ZERO
        SIGNZX(I)  = ZERO

        SIGVXX(I)  = ZERO
        SIGVYY(I)  = ZERO
        SIGVZZ(I)  = ZERO
        SIGVXY(I)  = ZERO
        SIGVYZ(I)  = ZERO
        SIGVZX(I)  = ZERO
       
        SOUNDSP(I) = SSP

      ENDDO! next I   


C-----------------------------------------------
      RETURN
C-----------------------------------------------
      END SUBROUTINE SIGEPS97
C-----------------------------------------------
