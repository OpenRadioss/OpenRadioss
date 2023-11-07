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
Chd|  FAIL_TAB_OLD_XFEM             source/materials/fail/tabulated/fail_tab_old_xfem.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FAIL_TAB_OLD_XFEM(
     1           NEL      ,NPARAM   ,NUVAR    ,NPF      ,TF       ,
     2           TIME     ,TIMESTEP ,UPARAM   ,NGL      ,IPT      ,
     3           NPTOT    ,NFUNC    ,IFUNC    ,DMG_FLAG ,
     4           SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     5           DPLA     ,EPSP     ,TSTAR    ,TENS     ,UVAR     ,
     6           NOFF     ,ALDT     ,OFF      ,OFFL     ,ELCRKINI ,
     7           IXFEM    ,IXEL     ,ILAY     ,DFMAX    ,TDEL     )
C-----------------------------------------------
C    tabulated failure model (old, obsolete version) for XFEM
C-----------------------------------------------
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
C ILAY                        CURRENT LAYER
C IPT                         CURRENT INTEGRATION POINT IN ALL LAYERS
C NPTOT                       NUMBER OF INTEGRATION POINTS IN ALL LAYERS (TOTAL)
C NOFF                        NUMBER OF FAILED INTEGRATION POINTS (TOTAL)
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NPARAM,NUVAR,IPT,NFUNC,IXFEM,IXEL,ILAY,NPTOT
      INTEGER NGL(NEL),NOFF(NEL),IFUNC(NFUNC),
     .   ELCRKINI(NXLAYMAX,NEL)
      my_real TIME,TIMESTEP(NEL),UPARAM(*),DPLA(NEL),EPSP(NEL),
     .   TSTAR(NEL),ALDT(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real, DIMENSION(2) :: XX0
      my_real 
     .  UVAR(NEL,NUVAR),OFF(NEL),OFFL(NEL),
     .  SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .  TENS(NEL,5),DFMAX(NEL),TDEL(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*)
      my_real FINTER ,TF(*)
      EXTERNAL FINTER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  :: I,J,J1,J2,K,L,NINDX,ISHELL,LAYXFEM,NRATE,DMG_FLAG,
     .   IFUN_EL,IFUN_TEMP,NF_LOC
      INTEGER, DIMENSION(NEL)   :: INDX,RFLAG
      INTEGER ,DIMENSION(NFUNC) :: IFUN_STR
C
      my_real  :: DP,P,SIGM,SIGM_PS,SVM,EF1,EF2,DF,FAC,DEPSF,LAMBDA,
     .  CC,BB,CR,ORM,SS1,SS2,YY,YY_N,DADV,
     .  DCRIT,EL_REF,SC_EL,SC_TEMP,DD,DN
      my_real, DIMENSION(NEL)   :: EPSF,DMG_SCALE
      my_real, DIMENSION(NFUNC) :: YFAC,RATE
      CHARACTER (LEN=3) :: XCHAR
C-----------------------------------------------
C UVAR(1) = DAMAGE
C UVAR(2) = initial characteristic el. length
C=======================================================================
C     INITIALIZATIONS
C-----------------------------------------------
      DMG_FLAG = 1
      INDX = 0
      SIGM_PS = ONE/SQRT(THREE)

      IF (UVAR(1,2) == ZERO) THEN
        UVAR(1:NEL,2) = ALDT(1:NEL) 
      ENDIF
C-----------------------------------------------
      ISHELL    = INT(UPARAM(2))  !ISHEL
      DCRIT     = UPARAM(4)
      DD        = UPARAM(5)
      DN        = UPARAM(6)
      SC_TEMP   = UPARAM(7)
      SC_EL     = UPARAM(8)
      EL_REF    = UPARAM(9)
      DADV      = UPARAM(11)
      NRATE     = NFUNC - 2
      YFAC(1:NRATE) = UPARAM(11+1      :11+NRATE)
      RATE(1:NRATE) = UPARAM(11+1+NRATE:11+NRATE*2)
c-----------------------------
      LAYXFEM = IXFEM
      IF (LAYXFEM == 1 .and. ISHELL == 1) ISHELL=2
c-----------------------------
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
c     Failure strain value - function interpolation
C-------------------------------------------------------------------
      IFUN_STR(1:NRATE) = IFUNC(1:NRATE) 
c---- Scale functions       
      IFUN_EL   = IFUNC(NRATE+1)
      IFUN_TEMP = IFUNC(NRATE+2)
c-------------------------------------------------------------------
c     Failure strain value - function interpolation
c-------------------------------------------------------------------
      DO I=1,NEL                      
        J1 = 1                                                             
        DO K=2, NRATE-1                                                     
          IF (EPSP(I) > RATE(J1)) J1 = K                                   
        ENDDO                                                              
        P    = THIRD*(SIGNXX(I) + SIGNYY(I))     
        SVM  = SQRT(SIGNXX(I)*SIGNXX(I) + SIGNYY(I)*SIGNYY(I)          
     .       - SIGNXX(I)*SIGNYY(I) + THREE*SIGNXY(I)*SIGNXY(I))        
        SIGM = P / MAX(EM20,SVM)
c----
        IF (NRATE > 1) THEN                                                 
          J2  = J1+1                                                        
          EF1 = YFAC(J1)*FINTER(IFUN_STR(J1),SIGM,NPF,TF,DF)            
          EF2 = YFAC(J2)*FINTER(IFUN_STR(J2),SIGM,NPF,TF,DF)            
          FAC = (EPSP(I) - RATE(J1))/(RATE(J2) - RATE(J1))              
          FAC = MAX(FAC, ONE) ! no extrapolation        
          EPSF(I) = MAX(EF1 + FAC*(EF2 - EF1), EM20)                       
        ELSE                                                               
          EPSF(I) = YFAC(J1)*FINTER(IFUN_STR(J1),SIGM,NPF,TF,DF)        
        ENDIF  
      ENDDO
c
      DO I=1,NEL
c----   element length scale function
        IF (IFUN_EL > 0) THEN
           LAMBDA = UVAR(I,2) / EL_REF
           FAC = SC_EL*FINTER(IFUN_EL,LAMBDA,NPF,TF,DF) 
           EPSF(I) = EPSF(I)* FAC 
        ENDIF                                                              
c----   temperature scale function
        IF (IFUN_TEMP > 0) THEN
           FAC = SC_TEMP*FINTER(IFUN_TEMP,TSTAR(I),NPF,TF,DF) 
           EPSF(I) = EPSF(I)* FAC 
        ENDIF    
      ENDDO
C-------------------------------------------------------------------
      IF (ISHELL == 1) THEN    ! shell deleted when rupture in one integration point
        IF (IXFEM == 1 .OR. IXFEM == 2) THEN 
          DO I=1,NEL
            IF (ISHELL == 1 .AND. OFF(I)==ONE) THEN
              IF(UVAR(I,1) == ZERO) THEN 
                DP = ONE
              ELSE
C               DP = DN*DD**(ONE-ONE/DN) ! old and wrong
                DP = DN*UVAR(I,1)**(ONE-ONE/DN)
              ENDIF
              IF (EPSF(I) > ZERO) UVAR(I,1)=
     .                            UVAR(I,1)+DP*DPLA(I)/EPSF(I)
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
#include    "lockon.inc"
c           initialization
            IF (RFLAG(I)>0.AND.RFLAG(I)<2)
     .                        WRITE(IOUT, 3800) NGL(I)
            IF (RFLAG(I)>0.AND.RFLAG(I)<2)
     .                        WRITE(ISTDO,3900) NGL(I),TIME
c           advancement
            IF (RFLAG(I) < 0) WRITE(IOUT, 4000) NGL(I)
            IF (RFLAG(I) < 0) WRITE(ISTDO,4100) NGL(I),TIME
c           delete
            IF (RFLAG(I) > 1) WRITE(IOUT, 4200)XCHAR,NGL(I)
            IF (RFLAG(I) > 1) WRITE(ISTDO,4300)XCHAR,NGL(I),TIME
#include    "lockoff.inc"
          ENDDO
        ENDIF
      ENDIF  ! IF(ISHELL == 1)
C-------------------------------
      IF (ISHELL > 1) THEN  ! shell deleted when rupture in all integration points
        IF (IXFEM == 1) THEN
          DO I=1,NEL
            IF (OFF(I) == ONE)THEN
              IF (UVAR(I,1) < DCRIT) THEN
                IF(UVAR(I,1) == ZERO) THEN 
                  DP = ONE
                ELSE
C                 DP = DN*DD**(ONE-ONE/DN) ! old and wrong
                  DP = DN*UVAR(I,1)**(ONE-ONE/DN)
                ENDIF
                IF (EPSF(I) > ZERO) UVAR(I,1)=
     .                              UVAR(I,1)+DP*DPLA(I)/EPSF(I)
                IF (IXEL == 0) THEN
                  IF (ELCRKINI(ILAY,I) == 0 .AND. 
     .                UVAR(I,1) >= DCRIT) THEN
                    IF (ISHELL == 2) THEN
                      SIGNXX(I) = ZERO
                      SIGNYY(I) = ZERO
                      SIGNXY(I) = ZERO
                      SIGNYZ(I) = ZERO
                      SIGNZX(I) = ZERO
                    ENDIF
                    NINDX=NINDX+1
                    INDX(NINDX)=I
                    ELCRKINI(ILAY,I) = -1
                    NOFF(I) = NOFF(I) + 1
                    IF (NOFF(I) == NPTOT) THEN
                      OFF(I) = FOUR_OVER_5                                           
                      TDEL(I)= TIME
                    ENDIF 
                    RFLAG(I) = 1
                  ELSEIF (ELCRKINI(ILAY,I) == 2 .AND.
     .                    UVAR(I,1) >= DADV) THEN
                    IF (ISHELL == 2) THEN
                      SIGNXX(I) = ZERO
                      SIGNYY(I) = ZERO
                      SIGNXY(I) = ZERO
                      SIGNYZ(I) = ZERO
                      SIGNZX(I) = ZERO
                    ENDIF
                    NINDX=NINDX+1
                    INDX(NINDX)=I
                    ELCRKINI(ILAY,I) = 1
                    NOFF(I) = NOFF(I) + 1
                    IF(DADV < DCRIT) UVAR(I,1) = DCRIT
                    IF (NOFF(I) == NPTOT) THEN
                      OFF(I) = FOUR_OVER_5                                           
                      TDEL(I)= TIME
                    ENDIF
                    RFLAG(I) = -1
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
                  RFLAG(I) = 3
                  IF (NOFF(I) == NPTOT) THEN
                    OFF(I) = FOUR_OVER_5                                           
                    RFLAG(I) = 4 !!! obsolete
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
        ELSEIF (IXFEM == 2) THEN
          DO I=1,NEL
            IF (OFF(I)==ONE .AND. (ISHELL==2 .OR. ISHELL==3))THEN
              IF (UVAR(I,1) < DCRIT) THEN
                IF(UVAR(I,1) == ZERO) THEN 
                  DP = ONE
                ELSE
C                 DP = DN*DD**(ONE-ONE/DN) ! old and wrong
                  DP = DN*UVAR(I,1)**(ONE-ONE/DN)
                ENDIF

                IF (EPSF(I) > ZERO) UVAR(I,1)=
     .                              UVAR(I,1)+DP*DPLA(I)/EPSF(I)
                IF (IXEL == 0) THEN
                  IF (ELCRKINI(ILAY,I) == 0 .AND. 
     .                UVAR(I,1) >= DCRIT) THEN
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
     .                    UVAR(I,1) >= DADV) THEN
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
c-----------------------------------------------------------------------
        IF (NINDX > 0) THEN
          DO J=1,NINDX
            I = INDX(J)
#include "lockon.inc"
            IF(IXFEM ==1)THEN
c             initialization
              IF (RFLAG(I)>0.AND.RFLAG(I)<3)WRITE(IOUT,4600)NGL(I),IPT
              IF (RFLAG(I)>0.AND.RFLAG(I)<3)WRITE(ISTDO,4700)
     .                                           NGL(I),IPT,TIME
c             advancement
              IF (RFLAG(I) < 0) WRITE(IOUT, 4800) NGL(I),IPT
              IF (RFLAG(I) < 0) WRITE(ISTDO,4900) NGL(I),IPT,TIME
c             delete
              IF (RFLAG(I) > 2) WRITE(IOUT, 4400)XCHAR,NGL(I),IPT
              IF (RFLAG(I) > 2) WRITE(ISTDO,4500)XCHAR,NGL(I),IPT,TIME
C
              IF (RFLAG(I) /= 0 .AND. IXEL == 0)
     .                          WRITE(IOUT, 2000) NGL(I),IPT
              IF (RFLAG(I) /= 0.AND. IXEL == 0)
     .                          WRITE(ISTDO,2100) NGL(I),IPT,TIME
            ELSEIF(IXFEM ==2)THEN
c             initialization
              IF (RFLAG(I)>0.AND.RFLAG(I)<3)WRITE(IOUT,3800)NGL(I)
              IF (RFLAG(I)>0.AND.RFLAG(I)<3)WRITE(ISTDO,3900)
     .                                           NGL(I),TIME
c             advancement
              IF (RFLAG(I) < 0) WRITE(IOUT, 4000) NGL(I)
              IF (RFLAG(I) < 0) WRITE(ISTDO,4100) NGL(I),TIME
c             delete
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
 2200 FORMAT(1X,'STRESS TENSOR SET TO ZERO IN THE LAYER')
 2400 FORMAT(1X,1PG20.13,' % OF THICKNESS OF SHELL BROKEN ')
 2500 FORMAT(1X,'  LOWER SKIN -> UPPER SKIN ')
 2600 FORMAT(1X,'  UPPER SKIN -> LOWER SKIN ')
 3700 FORMAT(1X,'STRESS TENSOR SET TO ZERO, LAYER',I10)
C---
 2410 FORMAT(1X,1PG20.13,' % OF THICKNESS OF SHELL ',I10,' BROKEN ')
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
 5010 FORMAT(1X,'SHELL ELEMENT FAILURE DUE TO THINNING (TAB)',I10)
 5020 FORMAT(1X,'SHELL ELEMENT FAILURE DUE TO THINNING (TAB)',I10,
     .       1X,':',/1X,'AT TIME :',1PE12.4)
 
c-----------
      RETURN
      END
