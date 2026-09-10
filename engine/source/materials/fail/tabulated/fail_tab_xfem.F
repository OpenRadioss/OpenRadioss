Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    fail_tab_xfem         ../engine/source/materials/fail/tabulated/fail_tab_xfem.F
!||--- called by ------------------------------------------------------
!||    mulawc                ../engine/source/materials/mat_share/mulawc.F90
!||    usermat_shell         ../engine/source/materials/mat_share/usermat_shell.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    message_mod           ../engine/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE FAIL_TAB_XFEM(FAIL ,
     1           NEL      ,NUVAR    ,NVARTMP  ,UVAR     ,VARTMP   ,
     2           TIME     ,TIMESTEP ,NGL      ,IPT      ,NPTOT    ,
     4           SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     5           DPLA     ,EPSP     ,TSTAR    ,TENS     ,DMG_FLAG ,
     6           NOFF     ,ALDT     ,OFF      ,OFFL     ,ELCRKINI ,
     7           IXFEM    ,IXEL     ,ILAY     ,DFMAX    ,TDEL     )
C-----------------------------------------------
C    tabulated failure model
C-----------------------------------------------
      use fail_param_mod
      use table_mat_vinterp_mod
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include "units_c.inc"
#include "comlock.inc"
#include "com_xfem1.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C NPF     |  *      | I | R | FUNCTION ARRAY   
C NPT     |  1      | I | R | NUMBER OF LAYERS OR INTEGRATION POINTS   
C IPT     |  1      | I | R | LAYER OR INTEGRATION POINT NUMBER   
C ISHELL  |  *      | I | R | GEOMETRICAL FLAGS   
C TF      |  *      | F | R | FUNCTION ARRAY 
C NGL     | NEL     | I | R | ELEMENT NUMBER
C SHF     | NEL     | F | R | SHEAR FACTOR
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SOUNDSP | NEL     | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C VISCMAX | NEL     | F | W | MAXIMUM DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C PLA     | NEL     | F |R/W| PLASTIC STRAIN
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C ILAY                        CURRENT LAYER
C IPT                         CURRENT INTEGRATION POINT IN ALL LAYERS
C NPTOT                       NUMBER OF INTEGRATION POINTS IN ALL LAYERS (TOTAL)
C NOFF                        NUMBER OF FAILED INTEGRATION POINTS (TOTAL)
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NPARAM,NUVAR,NVARTMP,IPT,IXFEM,IXEL,ILAY,
     .   NPTOT,DMG_FLAG
      INTEGER NGL(NEL),NOFF(NEL),
     .   ELCRKINI(NXLAYMAX,NEL)
      my_real TIME,TIMESTEP(NEL),DPLA(NEL),EPSP(NEL),
     .   TSTAR(NEL),ALDT(NEL)
      type (fail_param_) ,intent(in) :: fail    !< failure model data structure
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER, DIMENSION(NEL,NVARTMP), INTENT(INOUT) :: VARTMP
      my_real 
     .  UVAR(NEL,NUVAR),OFF(NEL),OFFL(NEL),
     .  SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .  TENS(NEL,5),DFMAX(NEL),TDEL(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,IADR,NINDX,ISHELL,I_MOD,I_DAM
      INTEGER :: IFUN_EPSF,IFUN_INST,IFUN_EL,IFUN_TEMP,IFUN_DMG,IFUN_FAD
      INTEGER, DIMENSION(NEL) :: INDX,RFLAG
C
      my_real, DIMENSION(NEL,3) :: XVEC
      my_real, DIMENSION(NEL)   :: EPSF,EPSF_N,size_fac,dydx,lambda,fade_expo,dp
      my_real :: P,SIGM,SIGM_PS,SVM,DADV,P_THINN,ECRIT,DCRIT,EL_REF,DD,DN
      CHARACTER (LEN=3) :: XCHAR
C-----------------------------------------------
C Storage of initial thickness in UVAR(x,2) at time = 0.0
C 1 = DAMAGE
C 2 = initial shell thickness
C 3 = DCrit_NS --> Instability starts
C 4 = percent from Instability to failure
C 5 = initial characteristic el. length
C=======================================================================
C     INITIALIZATIONS
C-----------------------------------------------
      DMG_FLAG = 1
      IADR = (IPT-1)*NEL
      INDX = 0
      SIGM_PS = ONE/SQRT(THREE)

      IF (UVAR(1,5) == ZERO) THEN
       DO I=1,NEL
         UVAR(I,5) = ALDT(I) 
       ENDDO
      ENDIF
C-----------------------------------------------
      ISHELL    = INT(FAIL%UPARAM(2))  !ISHEL
      DCRIT     = FAIL%UPARAM(4)
      DD        = FAIL%UPARAM(5)
      DN        = FAIL%UPARAM(6)
      EL_REF    = FAIL%UPARAM(9)
      DADV      = FAIL%UPARAM(11)
      P_THINN   = FAIL%UPARAM(16)
      ECRIT     = FAIL%UPARAM(17)
      FADE_EXPO(1:nel) = FAIL%UPARAM(18)
      I_MOD     = INT(FAIL%UPARAM(19))
c-----------------------------
      IF (IXFEM == 1 .and. ISHELL == 1) ISHELL=2
c-----------------------------
      I_DAM = 0      
      IF (ECRIT /= ZERO .OR. FADE_EXPO(1) /= ZERO) I_DAM = 1
C---------
      NINDX = 0  
      RFLAG = 0
C---------
      DO I=1,NEL
        TENS(I,1) = SIGNXX(I)
        TENS(I,2) = SIGNYY(I)
        TENS(I,3) = SIGNXY(I)
        TENS(I,4) = SIGNYZ(I)
        TENS(I,5) = SIGNZX(I)
      END DO
c
      IF (IXEL > 0) THEN  ! testing phantom elements
        IF (IXEL == 1) THEN
          XCHAR = '1st'
        ELSEIF (IXEL == 2) THEN
          XCHAR = '2nd'
        ELSEIF (IXEL == 3) THEN
          XCHAR = '3rd'
        ENDIF
      ELSE
        XCHAR = ' '
      ENDIF
C-------------------------------------------------------------------
c     ifun_epsf = 1    ! failure strain table
      ifun_inst = 2    ! instability strain table     
      ifun_el   = 3    ! element size function
      ifun_temp = 4    ! temperature scale function
      ifun_dmg  = 5    ! damage function
      ifun_fad  = 6    ! fading exponent function
c-------------------------------------------------------------------
c---  failure strain interpolation
      DO I=1,NEL
        P    = THIRD*(SIGNXX(I) + SIGNYY(I))     
        SVM  = SQRT(SIGNXX(I)*SIGNXX(I) + SIGNYY(I)*SIGNYY(I)          
     .       - SIGNXX(I)*SIGNYY(I) + THREE*SIGNXY(I)*SIGNXY(I))        
        SIGM = P / MAX(EM20,SVM)
        XVEC(I,1) = SIGM
        XVEC(I,2) = EPSP(I)
        XVEC(I,3) = ZERO
      ENDDO
!
      call table_mat_vinterp(fail%table4d(ifun_epsf),nel,nindx,vartmp,xvec,epsf,dydx)
!
      ! element size scale factor
      if (fail%table4d(ifun_el)%notable > 0) then
        lambda(1:nel) = uvar(1:nel,5) / el_ref
        call table_mat_vinterp(fail%table4d(ifun_el),nel,nel,vartmp(1:nel,4:4),lambda,size_fac,dydx)
        epsf(1:nel) = epsf(1:nel) * size_fac(1:nel)
      end if
      ! temperature scale factor
      if (fail%table4d(ifun_temp)%notable > 0) then
        lambda(1:nel) = uvar(1:nel,5) / el_ref
        call table_mat_vinterp(fail%table4d(ifun_temp),nel,nel,vartmp(1:nel,5:5),tstar,size_fac,dydx)
        epsf(1:nel) = epsf(1:nel) * size_fac(1:nel)
      end if 
c        
C----   Instability function

        IF (fail%table4d(ifun_inst)%notable > 0) THEN
          call table_mat_vinterp(fail%table4d(ifun_inst),nel,nel,vartmp(1:nel,6:7),xvec,epsf_n,dydx)
        ELSEIF (ECRIT > 0.0) THEN
           EPSF_N(I) = ECRIT
        ELSE 
           EPSF_N(I) = ZERO
        ENDIF       
C----   Fading exponent
        IF (fail%table4d(ifun_fad)%notable > 0) THEN
          lambda(1:nel) = uvar(1:nel,5) / el_ref
          call table_mat_vinterp(fail%table4d(ifun_fad),nel,nel,vartmp(1:nel,8:8),lambda,fade_expo,dydx)
        ENDIF
C-------------------------------------------------------------------
          IF (ISHELL == 1) THEN    ! shell deleted when rupture in one integration point
            IF (IXFEM == 1 .OR. IXFEM == 2) THEN
              if (fail%table4d(ifun_dmg)%notable > 0) then
                call table_mat_vinterp(fail%table4d(ifun_dmg),nel,nel,vartmp(1:nel,9:9),uvar(1:nel,1),dp,dydx)
              else
                if(uvar(1,1) == zero) then
                  dp(1:nel) = one
                else
                  dp(1:nel) = dn * uvar(1:nel,1)**(one-one/dn)
                end if
              end if
              !
              DO I=1,NEL
                IF (ISHELL == 1 .AND. OFF(I)==ONE) THEN
                  IF (EPSF(I) > ZERO) UVAR(I,1) = UVAR(I,1)+DP(I)*DPLA(I)/EPSF(I)
                  IF (IXEL == 0) THEN
                    IF (ELCRKINI(ILAY,I)==0) THEN
                      IF (UVAR(I,1) >= DCRIT) THEN
                        ELCRKINI(ILAY,I) = -1
                        OFF(I) = FOUR_OVER_5                                           
                        NINDX=NINDX+1
                        INDX(NINDX)=I
                        RFLAG(I) = 1
                        TDEL(I)= TIME
                      ENDIF
                    ELSEIF (ELCRKINI(ILAY,I) == 2) THEN
                      IF (UVAR(I,1) >= DADV) THEN
                        ELCRKINI(ILAY,I) = 1
                        OFF(I) = FOUR_OVER_5                                           
                        NINDX=NINDX+1
                        INDX(NINDX)=I
                        RFLAG(I) = -1
                        TDEL(I)= TIME
                      ENDIF
                    ENDIF
                  ELSEIF (UVAR(I,1 )>= DCRIT) THEN  ! IXEL > 0
                    OFF(I) = FOUR_OVER_5                                           
                    NINDX=NINDX+1
                    INDX(NINDX)=I
                    RFLAG(I) = 2
                  ENDIF ! IXEL
                ENDIF
              ENDDO ! DO J=1,IR 
            ENDIF  ! IF (IXFEM == 1 .OR. IXFEM == 2)
C
            IF (NINDX > 0) THEN
              DO J=1,NINDX
                I=INDX(J)
#include        "lockon.inc"
c               initialization
                IF (RFLAG(I)>0.AND.RFLAG(I)<2)
     .                            WRITE(IOUT, 3800) NGL(I)
                IF (RFLAG(I)>0.AND.RFLAG(I)<2)
     .                            WRITE(ISTDO,3900) NGL(I),TIME
c               advancement
                IF (RFLAG(I) < 0) WRITE(IOUT, 4000) NGL(I)
                IF (RFLAG(I) < 0) WRITE(ISTDO,4100) NGL(I),TIME
c               delete
                IF (RFLAG(I) > 1) WRITE(IOUT, 4200)XCHAR,NGL(I)
                IF (RFLAG(I) > 1) WRITE(ISTDO,4300)XCHAR,NGL(I),TIME
#include        "lockoff.inc"
              ENDDO
            ENDIF
          ENDIF  ! IF(ISHELL == 1)
C-------------------------------
          IF (ISHELL > 1) THEN
!
            IF (IXFEM == 1) THEN
              if (fail%table4d(ifun_dmg)%notable > 0) then
                call table_mat_vinterp(fail%table4d(ifun_dmg),nel,nel,vartmp(1:nel,9:9),uvar(1:nel,1),dp,dydx)
              else ! no damage function, use power law
                do i=1,nel
                  if (off(i) == one) then
                    if (uvar(i,1) < dcrit) then
                      dp(i) = dn * uvar(i,1)**(one-one/dn)
                    end if
                  end if
                end do
              end if
!
              do i=1,nel
                if (off(i) == one)then
                  if (uvar(i,1) < dcrit) then
                    if (epsf(i) > zero) uvar(i,1) = uvar(i,1)+dp(i)*dpla(i)/epsf(i)
                    if (ixel == 0) then
                      if (elcrkini(ilay,i) == 0 .and. uvar(i,1) >= dcrit) then
                        if (ishell == 2) then
                          signxx(i) = zero
                          signyy(i) = zero
                          signxy(i) = zero
                          signyz(i) = zero
                          signzx(i) = zero
                        endif
                        nindx=nindx+1
                        indx(nindx)=i
                        elcrkini(ilay,i) = -1
                        noff(i) = noff(i) + 1
                        if (noff(i) == nptot) then
                          off(i) = four_over_5                                           
                          tdel(i)= time
                        endif 
                        rflag(i) = 1
                      elseif (elcrkini(ilay,i) == 2 .and. uvar(i,1) >= dadv) then
                        if (ishell == 2) then
                          signxx(i) = zero
                          signyy(i) = zero
                          signxy(i) = zero
                          signyz(i) = zero
                          signzx(i) = zero
                        endif
                        nindx=nindx+1
                        indx(nindx)=i
                        elcrkini(ilay,i) = 1
                        noff(i) = noff(i) + 1
                        if(dadv < dcrit) uvar(i,1) = dcrit
                        if (noff(i) == nptot) then
                          off(i) = four_over_5                                           
                          tdel(i)= time
                        endif
                        rflag(i) = -1
                      endif
                    elseif (uvar(i,1) >= dcrit) then  ! ixel > 0
                      if (ishell == 2) then
                        signxx(i) = zero
                        signyy(i) = zero
                        signxy(i) = zero
                        signyz(i) = zero
                        signzx(i) = zero
                      endif
                      nindx=nindx+1
                      indx(nindx)=i
                      noff(i) = noff(i) + 1
                      rflag(i) = 3
                      if (noff(i) == nptot) then
                        off(i) = four_over_5                                           
                        rflag(i) = 4 !!! obsolete
                      endif
                    endif ! ixel
                  elseif (ishell == 2) then
                    signxx(i) = zero
                    signyy(i) = zero
                    signxy(i) = zero
                    signyz(i) = zero
                    signzx(i) = zero
                  endif
                endif ! off 
              enddo
!
            ELSEIF (IXFEM == 2) THEN
              if (fail%table4d(ifun_dmg)%notable > 0) then
                call table_mat_vinterp(fail%table4d(ifun_dmg),nel,nel,vartmp(1:nel,9:9),uvar(1:nel,1),dp,dydx)
              else ! no damage function, use power law
                do i=1,nel
                  if (off(i) == one) then
                    if (uvar(i,1) < dcrit) then
                      dp(i) = dn * uvar(i,1)**(one-one/dn)
                    end if
                  end if
                end do
              end if
!
              DO I=1,NEL
                IF (OFF(I)==ONE .AND. (ISHELL==2 .OR. ISHELL==3))THEN
                  IF (UVAR(I,1) < DCRIT) THEN
                    IF (EPSF(I) > ZERO) UVAR(I,1) = UVAR(I,1)+DP(I)*DPLA(I)/EPSF(I)
                    IF (IXEL == 0) THEN
                      IF (ELCRKINI(ILAY,I) == 0 .AND. 
     .                    UVAR(I,1) >= DCRIT) THEN
                        IF (ISHELL == 2) THEN
                          SIGNXX(I) = ZERO
                          SIGNYY(I) = ZERO
                          SIGNXY(I) = ZERO
                          SIGNYZ(I) = ZERO
                          SIGNZX(I) = ZERO
                        ENDIF
                        NINDX=NINDX+1
                        INDX(NINDX)=I
                        NOFF(I) = NOFF(I) + 1
                        IF (NOFF(I) == NPTOT) THEN
                          OFF(I) = FOUR_OVER_5                                           
                          ELCRKINI(ILAY,I) = -1
                          RFLAG(I) = 1
                          TDEL(I)= TIME
                        ENDIF
                      ELSEIF (ELCRKINI(ILAY,I) == 2 .AND.
     .                        UVAR(I,1) >= DADV) THEN
                        IF (ISHELL == 2) THEN
                          SIGNXX(I) = ZERO
                          SIGNYY(I) = ZERO
                          SIGNXY(I) = ZERO
                          SIGNYZ(I) = ZERO
                          SIGNZX(I) = ZERO
                        ENDIF
                        NINDX=NINDX+1
                        INDX(NINDX)=I
                        NOFF(I) = NOFF(I) + 1
                        IF(DADV < DCRIT) UVAR(I,1) = DCRIT
                        IF (NOFF(I) == NPTOT) THEN
                          OFF(I) = FOUR_OVER_5                                           
                          ELCRKINI(ILAY,I) = 1
                          RFLAG(I) = -1
                          TDEL(I)= TIME
                        ENDIF
                      ENDIF
                    ELSEIF (UVAR(I,1) >= DCRIT) THEN  ! IXEL > 0
                      IF (ISHELL == 2) THEN 
                        SIGNXX(I) = ZERO
                        SIGNYY(I) = ZERO
                        SIGNXY(I) = ZERO
                        SIGNYZ(I) = ZERO
                        SIGNZX(I) = ZERO
                      ENDIF
                      NINDX=NINDX+1
                      INDX(NINDX)=I
                      NOFF(I) = NOFF(I) + 1
                      IF (NOFF(I) == NPTOT) THEN
                        OFF(I) = FOUR_OVER_5                                           
                        RFLAG(I) = 4
                      ENDIF
                    ENDIF ! IXEL
                  ELSEIF (ISHELL == 2) THEN
                    SIGNXX(I) = ZERO
                    SIGNYY(I) = ZERO
                    SIGNXY(I) = ZERO
                    SIGNYZ(I) = ZERO
                    SIGNZX(I) = ZERO
                  ENDIF
                ENDIF ! OFF 
              ENDDO
            ENDIF  ! IF (IXFEM == 1)
c---------------------------------------------------------------------------------
            IF (NINDX > 0) THEN
              DO J=1,NINDX
                I = INDX(J)
#include "lockon.inc"
               IF(IXFEM ==1)THEN
c               initialization
                IF (RFLAG(I)>0.AND.RFLAG(I)<3)WRITE(IOUT,4600)NGL(I),IPT
                IF (RFLAG(I)>0.AND.RFLAG(I)<3)WRITE(ISTDO,4700)
     .                                             NGL(I),IPT,TIME
c               advancement
                IF (RFLAG(I) < 0) WRITE(IOUT, 4800) NGL(I),IPT
                IF (RFLAG(I) < 0) WRITE(ISTDO,4900) NGL(I),IPT,TIME
c               delete
                IF (RFLAG(I) > 2) WRITE(IOUT, 4400)XCHAR,NGL(I),IPT
                IF (RFLAG(I) > 2) WRITE(ISTDO,4500)XCHAR,NGL(I),IPT,TIME
C
                IF (RFLAG(I) /= 0 .AND. IXEL == 0)
     .                            WRITE(IOUT, 2000) NGL(I),IPT
                IF (RFLAG(I) /= 0.AND. IXEL == 0)
     .                            WRITE(ISTDO,2100) NGL(I),IPT,TIME
               ELSEIF(IXFEM ==2)THEN
c               initialization
                IF (RFLAG(I)>0.AND.RFLAG(I)<3)WRITE(IOUT,3800)NGL(I)
                IF (RFLAG(I)>0.AND.RFLAG(I)<3)WRITE(ISTDO,3900)
     .                                             NGL(I),TIME
c               advancement
                IF (RFLAG(I) < 0) WRITE(IOUT, 4000) NGL(I)
                IF (RFLAG(I) < 0) WRITE(ISTDO,4100) NGL(I),TIME
c               delete
                IF (RFLAG(I) > 2) WRITE(IOUT, 4200)XCHAR,NGL(I)
                IF (RFLAG(I) > 2) WRITE(ISTDO,4300)XCHAR,NGL(I),TIME
               ENDIF
#include "lockoff.inc"
              ENDDO
            ENDIF
          ENDIF  ! IF (ISHELL > 1)
c
C-------------Maximum Damage storing for output : 0 < DFMAX < 1--------------    
      DO I=1,NEL
        DFMAX(I)= MIN(ONE,MAX(DFMAX(I),UVAR(I,1)/DCRIT))
      ENDDO
C------------------
 2000 FORMAT(1X,'FAILURE OF SHELL ELEMENT (TAB)',I10,1X,
     .'LAYER',I10)
 2100 FORMAT(1X,'FAILURE OF SHELL ELEMENT (TAB)',I10,1X,
     .'LAYER',I10,':',/,'AT TIME :',1PE12.4)
C---
 3800 FORMAT(1X,'CRACK INITIALIZATION IN SHELL ELEMENT (TAB)',I10)
 3900 FORMAT(1X,'CRACK INITIALIZATION IN SHELL ELEMENT (TAB)',I10,
     .       1X,':',/,' AT TIME :',1PE12.4)
 4000 FORMAT(1X,'CRACK ADVANCEMENT IN SHELL ELEMENT (TAB)   ',I10)
 4100 FORMAT(1X,'CRACK ADVANCEMENT IN SHELL ELEMENT (TAB)   ',I10,
     .       1X,':',/,' AT TIME :',1PE12.4)
 4200 FORMAT(1X,'DELETE OF ',A5,' CRACKED PHANTOM ELEMENT'/
     .       1X,'OF THE ORIGINAL SHELL ELEMENT (TAB)        ',
     .       I10)
 4300 FORMAT(1X,'DELETE OF ',A5,' CRACKED PHANTOM ELEMENT'/
     .       1X,'OF THE ORIGINAL SHELL ELEMENT (TAB)        ',
     .       I10,':',/1X,'AT TIME :',1PE20.13)
 4400 FORMAT(1X,'DELETE OF ',A5,' CRACKED PHANTOM ELEMENT'/
     .       1X,'OF THE ORIGINAL SHELL ELEMENT (TAB)        ',
     .       I10,' LAYER',I10)
 4500 FORMAT(1X,'DELETE OF ',A5,' CRACKED PHANTOM ELEMENT'/
     .       1X,'OF THE ORIGINAL SHELL ELEMENT (TAB)        ',
     .       I10,' LAYER',I10,':',/1X,'AT TIME :',1PE20.13)
 4600 FORMAT(1X,'CRACK INITIALIZATION IN SHELL ELEMENT (TAB)',I10,
     .       1X,'LAYER',I10)
 4700 FORMAT(1X,'CRACK INITIALIZATION IN SHELL ELEMENT (TAB)',I10,
     .       1X,'LAYER',I10,':',/,' AT TIME :',1PE12.4)
 4800 FORMAT(1X,'CRACK ADVANCEMENT IN SHELL ELEMENT (TAB)   ',I10,
     .       1X,'LAYER',I10)
 4900 FORMAT(1X,'CRACK ADVANCEMENT IN SHELL ELEMENT (TAB)   ',I10,
     .       1X,'LAYER',I10,':',/,' AT TIME :',1PE12.4)
 
c-----------
      RETURN
      END
