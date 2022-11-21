Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
Copyright>    
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it ONEder the terms of the GNU Affero General Public License as published by
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
Chd|  SIGEPS120_CONNECT_VM          source/materials/mat/mat120/sigeps120_connect_vm.F
Chd|-- called by -----------
Chd|        SIGEPS120_CONNECT_MAIN        source/materials/mat/mat120/sigeps120_connect_main.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SIGEPS120_CONNECT_VM(
     1     NEL     ,NGL     ,TIME    ,TIMESTEP,UPARAM  ,OFF     ,
     2     EPSD    ,STIFM   ,THICK   ,
     3     AREA    ,DEPSZZ  ,DEPSYZ  ,DEPSZX  ,NUPARAM ,
     4     SIGOZZ  ,SIGOYZ  ,SIGOZX  ,SIGNZZ  ,SIGNYZ  ,SIGNZX  ,
     5     PLA     ,JSMS    ,DMELS   ,     UVAR    ,NUVAR   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "comlock.inc"
#include      "sms_c.inc"
C----------------------------------------------------------------
C  D u m m y   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER :: NEL,NUPARAM,NUVAR,JSMS
      my_real :: TIME,TIMESTEP
      INTEGER ,DIMENSION(NEL)    ,INTENT(IN) :: NGL
      my_real ,DIMENSION(NUPARAM),INTENT(IN) :: UPARAM
      my_real ,DIMENSION(NEL)    ,INTENT(IN) :: THICK,
     .         DEPSZZ,DEPSYZ,DEPSZX,
     .         SIGOZZ,SIGOYZ,SIGOZX,AREA
c
      my_real ,DIMENSION(NEL) ,INTENT(OUT) :: EPSD
      my_real ,DIMENSION(NEL) ,INTENT(OUT) :: 
     .                   SIGNZZ,SIGNYZ,SIGNZX
      my_real ,DIMENSION(NEL) ,INTENT(INOUT) :: OFF,PLA,STIFM,DMELS
      my_real ,DIMENSION(NEL,NUVAR) ,INTENT(INOUT) :: UVAR
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER  :: I,II,ITER,NITER,NINDX,NINDF,ITRX,IDAM
      INTEGER  ,DIMENSION(NEL) :: INDX,INDF
c     material parameters
      my_real :: YOUNG,NU,G,G2,BULK,YLD0,QQ,BETA,HH,C11,C12,
     .   A1F,A2F,AS,D1C,D2C,D1F,D2F,D_TRX,D_JC,DM,EXPN
c     Johnson & Cook rate-dependency
      my_real CC,EPSDREF,EPSDMAX,FRATE,FCUT,ALPHA,ALPHAI,EPSDOT
c     Local variables
      my_real FACR,TRIAX,RVOCE,FVM,I1P,IP,JP,TP,FACT,FACD,DFAC,DTINV,
     .   DLAM,DDEP,DFDLAM,DYLD_DPLA,DPLA_DLAM,DPHI_DLAM,DTB,
     .   GAMC,GAMF,NP_XX,NP_YY,NP_ZZ,NP_XY,NP_YZ,NP_ZX,
     .   NF_XX,NF_YY,NF_ZZ,NF_XY,NF_YZ,NF_ZX,DPXX,DPYY,DPZZ,DPXY,DPYZ,DPZX
      my_real ,DIMENSION(NEL) :: I1,J2,YLD,PHI,DAM,FDAM,JCRATE,STF,
     .   SXX,SYY,SZZ,SXY,SYZ,SZX,DPLA,JCR_LOG,GAMMA,DGAMM,PLA_NL,DPDT_NL
C---------------------------------------------

      YOUNG = UPARAM(1)
      NU    = UPARAM(2)
      G     = UPARAM(3)
      BULK  = UPARAM(4)
c     Parameters of yield function/hardening law and flow potential
      YLD0  = UPARAM(5)
      QQ    = UPARAM(6)
      BETA  = UPARAM(7)
      HH    = UPARAM(8)
      A1F   = UPARAM(9) 
      A2F   = UPARAM(10)
c      A1H   = UPARAM(11)
c      A2H   = UPARAM(12)
      AS    = UPARAM(13)
c     Johnson-Cook failure parameters
      D1C   = UPARAM(14)
      D2C   = UPARAM(15)
      D1F   = UPARAM(16)
      D2F   = UPARAM(17)
      D_TRX = UPARAM(18)
      D_JC  = UPARAM(19)
      EXPN  = UPARAM(20) 
c     Johnson-Cook strain rate-dependency and distortional hardening
      CC      = UPARAM(21) 
      EPSDREF = UPARAM(22) 
      EPSDMAX = UPARAM(23) 
      FCUT    = UPARAM(24)
c
      ITRX    = NINT(UPARAM(26)) 
      IDAM    = NINT(UPARAM(27))
  
c
      !YOUNG  = YOUNG/THICK
      !G      = G/THICK
      !BULK   = BULK/THICK
      G2     = G * TWO
      ALPHA  = 0.005
      ALPHAI = ONE - ALPHA
      DTINV   = ONE / (MAX(TIMESTEP, EM20))
C---------------------------------------------
      STF(1:NEL)     = YOUNG *  AREA(1:NEL)                                            
      STIFM(1:NEL)   = STIFM(1:NEL)  + STF(1:NEL)*OFF(1:NEL)                                              
C---------------------------------------------
      DAM (1:NEL) = UVAR(1:NEL,2)
      FDAM(1:NEL) = ONE - DAM(1:NEL)
      DO I = 1,NEL
        IF (OFF(I) < 0.001) OFF(I) = ZERO
        IF (OFF(I) < ONE)    OFF(I) = OFF(I)*FOUR/FIVE
        IF (OFF(I) == ONE) THEN
          SIGNZZ(I) = SIGOZZ(I)/FDAM(I) + (DEPSZZ(I) /THICK(I) )*YOUNG
          SIGNYZ(I) = SIGOYZ(I)/FDAM(I) + (DEPSYZ(I) /THICK(I) )*G
          SIGNZX(I) = SIGOZX(I)/FDAM(I) + (DEPSZX(I) /THICK(I) )*G
          I1(I)     = SIGNZZ(I)
c
          SXX(I) = - I1(I) * THIRD
          SYY(I) = - I1(I) * THIRD
          SZZ(I) = SIGNZZ(I) - I1(I) * THIRD
          SYZ(I) = SIGNYZ(I)
          SZX(I) = SIGNZX(I)
          J2(I)   = (SXX(I)**2 + SYY(I)**2 + SZZ(I)**2)*HALF
     .            +  SYZ(I)**2 + SZX(I)**2
        END IF
      ENDDO

c     Johnson & Cook rate dependency  (total strain rate)
      JCRATE(1:NEL)  = ONE
      DO I=1,NEL   
        IF (OFF(I) == ONE) THEN
          EPSDOT  = SQRT(DEPSYZ(I)**2 + DEPSZX(I)**2 + DEPSZZ(I)**2)
          EPSDOT  = EPSDOT * DTINV / THICK(I)
          EPSD(I) = MIN(EPSDOT ,EPSDMAX)
          EPSD(I) = ALPHA *EPSD(I) + ALPHAI * UVAR(I,3)
          UVAR(I,3) = EPSD(I)
          IF (EPSD(I) > EPSDREF) THEN
                JCR_LOG(I) = LOG(EPSD(I) / EPSDREF)
                JCRATE(I)  = ONE + CC * JCR_LOG(I)
          END IF
        END IF
      ENDDO
c------------------------------------------------------------------
c     Current yield value, yield criterion and plastic flow function
c------------------------------------------------------------------
      DO I = 1,NEL
        RVOCE  = QQ*(ONE - EXP(-BETA*PLA(I))) + HH*PLA(I)
        YLD(I) = (YLD0 + RVOCE) * JCRATE(I)
        FVM    = MAX(ZERO, I1(I) + HALF*SQR3*A1F*YLD0 / A2F )
        PHI(I) = J2(I) + THIRD*A2F * FVM**2 
     .         - (YLD(I)**2 + FOURTH*(A1F*YLD0)**2/A2F)
      ENDDO

c------------------------------------------------------------------
c     Plasticity
c------------------------------------------------------------------
      NINDX = 0
      NINDF = 0
      DO I = 1,NEL
        IF (PHI(I) > ZERO .and. OFF(I) == ONE) THEN
          NINDX = NINDX + 1
          INDX(NINDX) = I
        END IF
      ENDDO
c
c
      DO II = 1,NINDX
        I = INDX(II)
        DPLA(I) = ZERO
        I1P   = MAX(ZERO, I1(I))
        FVM   = MAX(ZERO, I1(I) + HALF*SQR3*A1F*YLD0 / A2F)
        NITER = 4
        DO ITER = 1,NITER
          ! normal to non associated plastic flow surface = d_psi/d_sig
          JP    = THIRD * AS * I1P
          TP    = TWO  * THIRD * AS * I1P 
c
          NP_XX = SXX(I)  
          NP_YY = SYY(I) 
          NP_ZZ = SZZ(I) + TP
          NP_YZ = SYZ(I)
          NP_ZX = SZX(I)
c
          ! normal to plastic yield surface = d_sig_eff/d_sig
          IP = TWO_THIRD * A2F * FVM
          NF_XX = SXX(I)  
          NF_YY = SYY(I)  
          NF_ZZ = SZZ(I) + IP
          NF_YZ = SYZ(I)
          NF_ZX = SZX(I)
c         
          ! derivative of yld criterion : dphi/dlam = d_phi/d_sig * dsig/dlam
          DFDLAM = - NF_ZZ * (YOUNG * NP_ZZ )
     .             -(NF_YZ*NP_YZ + NF_ZX*NP_ZX)*TWO*G2
c
          DYLD_DPLA = (HH + BETA*QQ*EXP(-BETA*PLA(I))) * JCRATE(I)
          DPLA_DLAM = TWO*SQRT((J2(I) + TP*AS*I1(I))) 

          DPLA_DLAM = DPLA_DLAM / FDAM(I)
          DPHI_DLAM = DFDLAM - TWO*YLD(I)*DYLD_DPLA*DPLA_DLAM
c----------------------------------------------------------------
          DLAM = -PHI(I) / DPHI_DLAM
c----------------------------------------------------------------
          ! Plastic strains tensor update                        
          DPZZ = DLAM*NP_ZZ                                                  
          DPYZ = DLAM*NP_YZ                          
          DPZX = DLAM*NP_ZX                 
          ! Elasto-plastic stresses update   
          SIGNZZ(I) = SIGNZZ(I) - YOUNG *DPZZ 
          SIGNYZ(I) = SIGNYZ(I) - G2    *DPYZ  
          SIGNZX(I) = SIGNZX(I) - G2    *DPZX
c
          ! Plastic strain increments update
          DDEP   = DLAM * DPLA_DLAM
          DPLA(I)= MAX(ZERO, DPLA(I) + DDEP)
          PLA(I) = PLA(I) + DDEP
c----------------------        
c         criterion update  
c----------------------        
          I1(I)  = SIGNZZ(I)
          I1P    = MAX(ZERO, I1(I))
          SXX(I) =  - I1(I)*THIRD
          SYY(I) =  - I1(I)*THIRD
          SZZ(I) = SIGNZZ(I) - I1(I)*THIRD
          SYZ(I) = SIGNYZ(I)
          SZX(I) = SIGNZX(I)
          J2(I)  =(SXX(I)**2 + SYY(I)**2 + SZZ(I)**2 ) * HALF
     .           + SYZ(I)**2 + SZX(I)**2
          RVOCE  = QQ*(ONE - EXP(-BETA*PLA(I))) + HH*PLA(I)
          YLD(I) = (YLD0 + RVOCE) * JCRATE(I)
          FVM    = MAX(ZERO, I1(I) + HALF*SQR3*A1F*YLD0 / A2F)
          PHI(I) = J2(I) + THIRD*A2F * FVM**2 
     .           - (YLD(I)**2 + FOURTH*(A1F*YLD0)**2/A2F)
        END DO  ! Newton iterations
c
        UVAR(I,4) = YLD(I)**2 + FOURTH*(A1F*YLD0)**2/A2F
        UVAR(I,5) = J2(I) + THIRD*A2F * FVM**2 
      ENDDO   ! II = 1,NINDX    
c---------------------------------------------------------------------
c     Update damage
c---------------------------------------------------------------------
      IF (IDAM > 0) THEN
        IF (IDAM == 1) THEN
            DGAMM(1:NEL) = DPLA(1:NEL)
            GAMMA(1:NEL) = PLA(1:NEL)
        ELSE
            DGAMM(1:NEL)  = DPLA(1:NEL) * FDAM(1:NEL)
            UVAR(1:NEL,1) = UVAR(1:NEL,1) + DGAMM(1:NEL)
            GAMMA (1:NEL) = UVAR(1:NEL,1)
        END IF
c
        DO II = 1,NINDX
          I = INDX(II)
          FACR = ONE + D_JC * JCR_LOG(I)  ! total strain rate factor on damage
          IF (ITRX == 1) THEN
            TRIAX = THIRD * I1(I) / SQRT(THREE*J2(I))
            FACT  = EXP(-D_TRX*TRIAX)
          ELSE IF (I1(I) <= ZERO) THEN  ! ITRX = 2 => no triaxiality influence in compression
            FACT = ONE
          END IF
          GAMC = (D1C + D2C * FACT) * FACR
          GAMF = (D1F + D2F * FACT) * FACR
          IF (GAMMA(I) > GAMC) THEN
            IF (DAM(I) == ZERO) THEN
              NINDF = NINDF + 1
              INDF(NINDF) = I
            END IF
            IF (EXPN == ONE) THEN
              DAM(I) = DAM(I) + DGAMM(I) / (GAMF - GAMC)
            ELSE
              DFAC   = (GAMMA(I) - GAMC) / (GAMF - GAMC)
              DAM(I) = DAM(I) + EXPN * DFAC**(EXPN-ONE) * DGAMM(I) / (GAMF - GAMC)
            END IF
            IF (DAM(I) >= ONE) THEN
              DAM(I) = ONE
              OFF(I) = FOUR/FIVE
            END IF
            UVAR(I,2) = DAM(I)
          END IF ! GAMMA > GAMC
        ENDDO
c       Update damaged stress
        DO I = 1, NEL
          FACD = ONE - DAM(I)
          SIGNZZ(I) = SIGNZZ(I) * FACD
          SIGNYZ(I) = SIGNYZ(I) * FACD
          SIGNZX(I) = SIGNZX(I) * FACD        
        END DO
      END IF
c-------------------------
      IF (NINDF > 0) THEN
        DO II=1,NINDF
#include "lockon.inc"
          WRITE(IOUT, 1000) NGL(INDF(II))
          WRITE(ISTDO,1100) NGL(INDF(II)),TIME
#include "lockoff.inc"
        ENDDO
      END IF
c-----------------------------------------------------------------
 1000 FORMAT(1X,'START DAMAGE IN SOLID ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'START DAMAGE IN SOLID ELEMENT NUMBER ',I10,1X,' AT TIME :',G11.4) 
c-----------------------------------------------------------------
c-----------------------------------------------------      
c omega = sqrt(2k/2*dmels), dt=2/omega, 2*dmels=dt**2 * 2k / 4
      IF (IDTMINS==2 .AND. JSMS/=0 ) THEN
        DTB = (DTMINS/DTFACS)**2
        DO I=1,NEL                                                 
          DMELS(I)=MAX(DMELS(I),HALF*DTB*STF(I)*OFF(I))
        ENDDO                                                        
      ENDIF
c-----------------------------------------------------------------
      RETURN
      END
