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
!||    sigeps36g               ../engine/source/materials/mat/mat036/sigeps36g.F
!||--- called by ------------------------------------------------------
!||    mulawglc                ../engine/source/materials/mat_share/mulawglc.F
!||--- calls      -----------------------------------------------------
!||    table_mat_vinterp       ../engine/source/materials/tools/table_mat_vinterp.F
!||--- uses       -----------------------------------------------------
!||    matparam_def_mod        ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    table_mat_vinterp_mod   ../engine/source/materials/tools/table_mat_vinterp.F
!||====================================================================
      SUBROUTINE SIGEPS36G(MAT_PARAM,
     1     NEL    ,RHO0   ,THK0    ,OFF     ,ETSE   ,
     4     EPSPXX ,EPSPYY ,EPSPXY  ,
     5     DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     5     DEPBXX ,DEPBYY ,DEPBXY  ,
     6     EPSXX  ,EPSYY  ,EPSXY   ,
     7     SIGOXX ,SIGOYY ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     7     MOMOXX ,MOMOYY ,MOMOXY  ,
     8     SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     8     MOMNXX ,MOMNYY ,MOMNXY  ,
     A     SOUNDSP,VISCMAX,THK     ,PLA     ,
     C     GS     ,YLD    ,EPSP    ,ISRATE  ,IPLA   ,
     D     SHF    ,NVARTMP,VARTMP)
! ------------------------------------------------------------------------------
!       modules
! ------------------------------------------------------------------------------
          use matparam_def_mod
          use table_mat_vinterp_mod
C-----------------------------------------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   I N P U T   A R G U M E N T S
C-----------------------------------------------
      INTEGER NEL,  IPLA, ISRATE,NVARTMP
      my_real RHO0(NEL),THK0(NEL),PLA(NEL),
     .   EPSPXX(NEL),EPSPYY(NEL),EPSPXY(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),DEPSXY(NEL),
     .   DEPBXX(NEL),DEPBYY(NEL),DEPBXY(NEL),
     .   DEPSYZ(NEL),DEPSZX(NEL),
     .   EPSXX(NEL) ,EPSYY(NEL) ,EPSXY(NEL) ,
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOXY(NEL),
     .   MOMOXX(NEL),MOMOYY(NEL),MOMOXY(NEL),
     .   SIGOYZ(NEL),SIGOZX(NEL),
     .   GS(*),EPSP(NEL),SHF(NEL)
      type(matparam_struct_) ,intent(in) :: mat_param
C-----------------------------------------------
C   O U T P U T   A R G U M E N T S
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),
     .    MOMNXX(NEL),MOMNYY(NEL),MOMNXY(NEL),
     .    SIGNYZ(NEL),SIGNZX(NEL),
     .    SOUNDSP(NEL),VISCMAX(NEL),ETSE(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A R G U M E N T S 
C-----------------------------------------------
      my_real
     .    OFF(NEL),THK(NEL),YLD(NEL)
      INTEGER, INTENT(INOUT) :: VARTMP(NEL,NVARTMP)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,II,J,N,NDIM,NINDX,NMAX,FUNC_E
      INTEGER INDX(MVSIZ)
      my_real
     .        E(MVSIZ),YOUNG,SHEAR,NU,A,B,DEZZ,S1(MVSIZ),S2(MVSIZ),
     .        DPLA,EPST(MVSIZ),A1(MVSIZ),A2(MVSIZ),G(MVSIZ),G3(MVSIZ),
     .        A11,A21,G31,NNU11,NU11,NU21,NU31,DPLA_I,DR,
     .        F,DF,YLD_I,TOL,epsmax,EPSR1,EPSR2,EPSF ,TOLD,EINF,CE,
     .        CF,CP1,CQ1,CP2,CQ2,SM1(MVSIZ),SM2(MVSIZ),SM3,
     .        DFNP,DFNQ,DFMP,DFMQ,DFNMP,DFNMQ,XP,XQ,XPG,XQG,
     .        DA,DB,A_I,B_I,FNM,
     .        SVM(MVSIZ),DPLA_J(MVSIZ),H(MVSIZ),FAIL(MVSIZ),
     .        GM(MVSIZ),CM(MVSIZ),QTIER(MVSIZ),
     .        CNM(MVSIZ),AM(MVSIZ),BM(MVSIZ),ANM(MVSIZ),BNM(MVSIZ),
     .        NUM1(MVSIZ),NUM2(MVSIZ),AN(MVSIZ),BN(MVSIZ),
     .        NVM(MVSIZ),MVM(MVSIZ),NMVM(MVSIZ),QN,SN1,SN2,S,
     .        QNM1,QNM2,FNP,FNQ,FMP,FMQ,FNMP,FNMQ,S3,AA,BB,M1,M2,
     .        LFN(MVSIZ),QFN(MVSIZ),QFNM(MVSIZ),RR(MVSIZ),C1(MVSIZ),
     .        D1,D2,DWT,DWE,DWP,AAA,BBB,CCC,FS,MS,
     .        AM1(MVSIZ),AM2(MVSIZ),GAMA(MVSIZ),GAMA2(MVSIZ),Y1(MVSIZ)
      my_real ,dimension(nel)   :: escale,dydx
      my_real ,dimension(nel,1) :: xvec1
      my_real ,dimension(nel,2) :: xvec2
C===============================================================================
      DATA NMAX/2/
        TOL = EM4     
      IF(IRESP == 1)THEN
        TOLD=EM8
      ELSE
        TOLD=EM20
      END IF
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
      YOUNG  = mat_param%young
      SHEAR  = mat_param%shear
      nu     = mat_param%nu
      a11    = mat_param%uparam(1)
      a21    = mat_param%uparam(2)
      epsmax = mat_param%uparam(3)
      epsr1  = mat_param%uparam(4)
      epsr2  = mat_param%uparam(5)
      epsf   = mat_param%uparam(6)   
      ce     = mat_param%uparam(8) 
      einf   = mat_param%uparam(9)  

      G31    = THREE*SHEAR
      NNU11  = NU / (ONE - NU)

      ndim   = mat_param%table(1)%ndim
      func_e = mat_param%table(3)%notable

      C1(1:NEL) = THK0(1:NEL)*ONE_OVER_12
c--------------------
      nindx=0
      do i=1,nel         
        if (pla(i) > zero .and. off(i) == one) then
          nindx = nindx+1
          indx(nindx) = i
        endif
      enddo
!
      if (func_e > 0) then           ! young modulus evolution function                                
        xvec1(1:nel,1) = pla(1:nel)                                                           
        call table_mat_vinterp(mat_param%table(3),nel,nel,vartmp(1,4),xvec1,escale,DYDX)     
        e(1:nel) = escale(1:nel) * young                                                     
        g(1:nel) = half*e(1:nel) / (one+nu)                                                      
      elseif (ce /= zero) then       ! variable young modulus defined analytically                                                 
        do ii=1,nindx
          i = indx(ii)
          e(i) = young - (young-einf)*(one-exp(-ce*pla(i)))
          g(i) = half*e(i) / (one+nu)                                                      
        end do
      else                           ! constant Young modulus                                                                              
        e(1:nel) = young
        g(1:nel) = shear
      end if                                                                                  
      GS(1:NEL)  = G(1:NEL)*SHF(1:NEL)                             
      G3(1:NEL)  = THREE*G(1:NEL) 
      A1(1:NEL)  = E(1:NEL)/(ONE - NU*NU)                        
      A2(1:NEL)  = NU*A1(1:NEL)                                    
      AM1(1:NEL) = A1(1:NEL)*C1(1:NEL)  
      AM2(1:NEL) = A2(1:NEL)*C1(1:NEL)  
      GM(1:NEL)  = G(1:NEL) *C1(1:NEL)   
C-----------------------------------------------
C
      DO I=1,NEL
       SIGNXX(I)=SIGOXX(I)+A1(I)*DEPSXX(I)+A2(I)*DEPSYY(I)
       SIGNYY(I)=SIGOYY(I)+A2(I)*DEPSXX(I)+A1(I)*DEPSYY(I)
       SIGNXY(I)=SIGOXY(I)+G(I) *DEPSXY(I)
       MOMNXX(I)=MOMOXX(I)+AM1(I)*DEPBXX(I)+AM2(I)*DEPBYY(I)
       MOMNYY(I)=MOMOYY(I)+AM2(I)*DEPBXX(I)+AM1(I)*DEPBYY(I)
       MOMNXY(I)=MOMOXY(I)+GM(I) *DEPBXY(I)
       SIGNYZ(I)=SIGOYZ(I)+GS(I) *DEPSYZ(I)
       SIGNZX(I)=SIGOZX(I)+GS(I) *DEPSZX(I)
C
       SOUNDSP(I) = SQRT(A1(I)/RHO0(I))
       VISCMAX(I) = ZERO
       ETSE(I) = ONE
C-------------------
C     STRAIN RATE
C-------------------
       IF (ISRATE == 0) THEN
         EPSP(I) = HALF*( ABS(EPSPXX(I)+EPSPYY(I))
     .           + SQRT( (EPSPXX(I)-EPSPYY(I))*(EPSPXX(I)-EPSPYY(I))
     .           + EPSPXY(I)*EPSPXY(I) ) )
       END IF
C-------------------
C     STRAIN 
C-------------------
       EPST(I) = HALF*( EPSXX(I)+EPSYY(I)
     .         + SQRT( (EPSXX(I)-EPSYY(I))*(EPSXX(I)-EPSYY(I))
     .         + EPSXY(I)*EPSXY(I) ) )
       FAIL(I) = MAX(ZERO,MIN(ONE,(EPSR2-EPST(I)) / (EPSR2-EPSR1)))
      ENDDO
C-------------------
C     Yield Stress
C-------------------
      if (ndim == 1) then    ! only static curve => no strain rate interpolation
        xvec1(1:nel,1) = pla(1:nel)
        call table_mat_vinterp(mat_param%table(1),nel,nel,vartmp,xvec1,y1,dydx)
      else if (ndim == 2) then
        xvec2(1:nel,1) = pla (1:nel)
        xvec2(1:nel,2) = epst(1:nel)
        call table_mat_vinterp(mat_param%table(1),nel,nel,vartmp,xvec2,y1,dydx)
      end if
      yld(1:nel) = y1(1:nel)   * fail(1:nel)
      h(1:nel)   = dydx(1:nel) * fail(1:nel)
C---------------------------

      IF (IPLA==0) THEN ! RADIAL RETURN
        DO I=1,NEL
         MS=MOMNXX(I)+MOMNYY(I)
         FS=SIGNXX(I)+SIGNYY(I)
         SVM(I) = SIXTEEN*(MS*MS +THREE*(MOMNXY(I)*MOMNXY(I) 
     .                                      - MOMNXX(I)*MOMNYY(I)))
     .            + FS*FS+ THREE*(SIGNXY(I)*SIGNXY(I)-SIGNXX(I)*SIGNYY(I))
         SVM(I) = SQRT(MAX(SVM(I),EM20))
         RR(I) = MIN(ONE,YLD(I)/SVM(I))
         IF(RR(I)<ONE) ETSE(I)= H(I)/(H(I)+E(I))
        ENDDO
        DO I=1,NEL
         SIGNXX(I) = SIGNXX(I)*RR(I)
         SIGNYY(I) = SIGNYY(I)*RR(I)
         SIGNXY(I) = SIGNXY(I)*RR(I)
         MOMNXX(I) = MOMNXX(I)*RR(I)
         MOMNYY(I) = MOMNYY(I)*RR(I)
         MOMNXY(I) = MOMNXY(I)*RR(I)
         D1 = SIGNXX(I)-SIGOXX(I)
         D2 = SIGNYY(I)-SIGOYY(I)
         DWE =((SIGNXX(I)+SIGOXX(I))*(D1-NU*D2)+
     .         (SIGNYY(I)+SIGOYY(I))*(-NU*D1+D2))/E(I)+
     .        (SIGNXY(I)+SIGOXY(I))*(SIGNXY(I)-SIGOXY(I))/G(I)
         D1 = MOMNXX(I)-MOMOXX(I)
         D2 = MOMNYY(I)-MOMOYY(I)
         DWE =DWE+ TWELVE*(
     .            ((MOMNXX(I)+MOMOXX(I))*(D1-NU*D2)
     .           +(MOMNYY(I)+MOMOYY(I))*(-NU*D1+D2))/E(I)
     .           +(MOMNXY(I)+MOMOXY(I))*(MOMNXY(I)-MOMOXY(I))/G(I) )
         DWT =   (SIGNXX(I)+SIGOXX(I))*DEPSXX(I)+
     .           (SIGNYY(I)+SIGOYY(I))*DEPSYY(I)+
     .           (SIGNXY(I)+SIGOXY(I))*DEPSXY(I)
         DWT = DWT+THK0(I)*((MOMNXX(I)+MOMOXX(I))*DEPBXX(I)+
     .                      (MOMNYY(I)+MOMOYY(I))*DEPBYY(I)+
     .                      (MOMNXY(I)+MOMOXY(I))*DEPBXY(I))
         DWP =DWT-DWE
         DPLA = OFF(I)* MAX(ZERO,HALF*DWP/YLD(I))
         PLA(I)=PLA(I) + DPLA
         AAA  = ABS(DWE)
         BBB  = MAX(ZERO,DWP)
         CCC  = MAX(EM20,AAA+BBB)
         DEZZ = - (DEPSXX(I)+DEPSYY(I)) * (NNU11*AAA + BBB) / CCC
         THK(I) = THK(I) * (ONE + DEZZ*OFF(I))
        ENDDO
!
      ELSE   ! IPLA==1  => Iterative projection
C-------------------------
        DO I=1,NEL
C-------------------------------------------------------------------------
C         gama (the inverse of gama in the formula)
C-------------------------------------------------------------------------
          CF = PLA(I)*E(I)/YLD(I)
          CCC=EXP(-TWOP6667*CF)
          GAMA(I) = TWO/(THREE-CCC)
          GAMA2(I) = GAMA(I)*GAMA(I)
          CM(I) = THIRTY6*GAMA2(I)
          CNM(I) = THREEP4641*GAMA(I)  
          QTIER(I) = THREE*GAMA2(I)
          H(I) = MAX(ZERO,H(I))
          S1(I) = (SIGNXX(I)+SIGNYY(I))*HALF
          S2(I) = (SIGNXX(I)-SIGNYY(I))*HALF
          S3 = SIGNXY(I)
          SM1(I) = (MOMNXX(I)+MOMNYY(I))*HALF
          SM2(I) = (MOMNXX(I)-MOMNYY(I))*HALF
          SM3 = MOMNXY(I)
          AN(I) = S1(I)*S1(I)
          BN(I) = THREE*(S2(I)*S2(I)+S3*S3)
          NVM(I) = AN(I)+BN(I)  
          AM(I) = SM1(I)*SM1(I)*CM(I)
          BM(I) = THREE*(SM2(I)*SM2(I)+SM3*SM3)*CM(I)
          MVM(I) = AM(I)+BM(I)
          ANM(I) = S1(I)*SM1(I)*CNM(I)
          BNM(I) = THREE*(S2(I)*SM2(I)+S3*SM3)*CNM(I)
          NMVM(I) = ANM(I)+BNM(I)
          SVM(I) = SQRT(NVM(I)+MVM(I)+ABS(NMVM(I)))
          DEZZ = -(DEPSXX(I)+DEPSYY(I))*NNU11
          THK(I) = THK(I) +THK(I)* DEZZ*OFF(I)
        ENDDO
C-------------------------
C       GATHER PLASTIC FLOW
C-------------------------
        NINDX=0
        DO I=1,NEL
          IF(SVM(I)>YLD(I).AND.OFF(I)==ONE) THEN
            NINDX=NINDX+1
            INDX(NINDX)=I
          ENDIF
        ENDDO
        IF (NINDX==0) RETURN
C---------------------------
C       DEP EN CONTRAINTE PLANE
C---------------------------
        NU11 = HALF*(ONE + NU)
        NU21 = THREE_HALF*(ONE-NU)
        NU31 = ONE-NNU11
        DO  J=1,NINDX
          I=INDX(J)
          NUM1(I) = ONE+QTIER(I)
          NUM2(I) = FIVEP5*GAMA2(I)
          LFN(I)=NUM2(I)
          QFN(I)=SIXTEENP5*GAMA2(I)*GAMA2(I)
          QFNM(I)=-NUM2(I)
          DPLA_J(I)=(SVM(I)-YLD(I))/(G3(I)*QTIER(I)+H(I))
          ETSE(I)= H(I)/(H(I)+E(I))
        ENDDO
C-------------------------------
C       takes coupling into account
C-------------------------------
        DO N=1,NMAX
#include "vectorize.inc"
          DO J=1,NINDX
           I=INDX(J)
           DPLA_I=DPLA_J(I)
           YLD_I =YLD(I)+H(I)*DPLA_I
           DR =A1(I)*DPLA_I/YLD_I
           XP  =DR*NU11
           XQ  =DR*NU21
           DA=NUM1(I)+NUM2(I)*XP
           DB=NUM1(I)+NUM2(I)*XQ
           A=ONE+(DA+NUM1(I))*XP*HALF
           B=ONE+(DB+NUM1(I))*XQ*HALF
           A_I=ONE/A
           B_I=ONE/B
           AA=A_I*A_I
           BB=B_I*B_I
           DFNP=LFN(I)+QFN(I)*XP
           DFNQ=LFN(I)+QFN(I)*XQ
           DFMP=ONEP8333*(XP+ONE)
           DFMQ=ONEP8333*(XQ+ONE)
           DFNMP=QFNM(I)*XP
           DFNMQ=QFNM(I)*XQ
           XP = HALF*XP
           XQ = HALF*XQ
           FNP=ONE+(DFNP+LFN(I))*XP
           FNQ=ONE+(DFNQ+LFN(I))*XQ
           FMP=ONE+(DFMP+ONEP8333)*XP
           FMQ=ONE+(DFMQ+ONEP8333)*XQ
           FNMP=ONE+DFNMP*XP
           FNMQ=ONE+DFNMQ*XQ
           FNM=AA*FNMP*ANM(I)+BB*FNMQ*BNM(I)
           IF (FNM<ZERO) THEN
            S=-ONE
           ELSE
            S=ONE
           ENDIF 
C
           CP1 =(FNP*AN(I)+S*FNMP*ANM(I)+FMP*AM(I))*AA
           CQ1 =(FNQ*BN(I)+S*FNMQ*BNM(I)+FMQ*BM(I))*BB
           CP2 =(DFNP*AN(I)+S*DFNMP*ANM(I)+DFMP*AM(I))*AA
           CQ2 =(DFNQ*BN(I)+S*DFNMQ*BNM(I)+DFMQ*BM(I))*BB
           XPG =TWO*NU11*DA*A_I
           XQG =TWO*NU21*DB*B_I
           F    =CP1 +CQ1-YLD_I*YLD_I
           DF    =(CP2*NU11+CQ2*NU21-CP1*XPG-CQ1*XQG)*
     .            (A1(I)-DR*H(I))/YLD_I-TWO*H(I)*YLD_I
C
           DPLA_J(I)=MAX(ZERO,DPLA_I-F/DF)
          ENDDO
        ENDDO
C------------------------------------------
C       CONTRAINTES PLASTIQUEMENT ADMISSIBLES
C------------------------------------------
#include "vectorize.inc"
        DO J=1,NINDX
          I=INDX(J)
          PLA(I) = PLA(I) + DPLA_J(I)
          DPLA_I=DPLA_J(I)
          YLD_I =YLD(I)+H(I)*DPLA_I
          DR =A1(I)*DPLA_I/YLD_I
          XP  =DR*NU11
          XQ  =DR*NU21
          XPG  =XP*XP
          XQG  =XQ*XQ
          A=ONE + NUM1(I)*XP+NUM2(I)*XPG
          B=ONE+NUM1(I)*XQ+NUM2(I)*XQG
          A_I=ONE/A
          B_I=ONE/B
          AA=A_I*A_I
          BB=B_I*B_I
          FNMP=ONE+QFNM(I)*XPG
          FNMQ=ONE+QFNM(I)*XQG
          FNM=AA*FNMP*ANM(I)+BB*FNMQ*BNM(I)
          IF (FNM<ZERO) THEN
           S=-ONEP732*GAMA(I)
          ELSE
           S=ONEP732*GAMA(I)
          ENDIF 
          QN=ONE+QTIER(I)*XQ
          QNM1=XQ*S
          QNM2=QNM1*ONE_OVER_12
          SN1=(S1(I)*(ONE+QTIER(I)*XP)-SM1(I)*S*XP)*A_I
          SN2=(S2(I)*QN-SM2(I)*QNM1)*B_I
          S3=(SIGNXY(I)*QN-MOMNXY(I)*QNM1)*B_I
          M1=(SM1(I)*(ONE+XP)-S1(I)*S*XP*ONE_OVER_12)*A_I
          M2=(SM2(I)*(1.+XQ)-S2(I)*QNM2)*B_I
          MOMNXY(I)=(MOMNXY(I)*(1.+XQ)-SIGNXY(I)*QNM2)*B_I
          SIGNXX(I)=SN1+SN2
          SIGNYY(I)=SN1-SN2
          SIGNXY(I)=S3
          MOMNXX(I)=M1+M2
          MOMNYY(I)=M1-M2
          DEZZ = - NU31*DR*SN1/E(I)
          THK(I) = THK(I) + DEZZ*THK(I)*OFF(I)
        ENDDO
C
      ENDIF ! IPLA
C---------------------------------------------------------------------
      DO I=1,NEL
        IF((PLA(I) > epsmax .OR. EPST(I) > EPSF) .AND. OFF(I)==ONE) THEN
           OFF(I)=FOUR_OVER_5
        ENDIF
      ENDDO
C-----------
      RETURN
      END
