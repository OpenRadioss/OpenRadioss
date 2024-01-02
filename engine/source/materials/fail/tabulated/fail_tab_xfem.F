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
Chd|  FAIL_TAB_XFEM                 source/materials/fail/tabulated/fail_tab_xfem.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|        TABLE_INTERP                  source/tools/curve/table_tools.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        INTERFACE_TABLE_MOD           share/modules/table_mod.F     
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE FAIL_TAB_XFEM(
     1           NEL      ,NPARAM   ,NUVAR    ,NPF      ,TF       ,
     2           TIME     ,TIMESTEP ,UPARAM   ,NGL      ,IPT      ,
     3           NPTOT    ,IP       ,NFUNC    ,IFUNC    ,TABLE    ,
     4           SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     5           DPLA     ,EPSP     ,TSTAR    ,TENS     ,UVAR     ,
     6           NOFF     ,ALDT     ,OFF      ,OFFL     ,ELCRKINI ,
     7           IXFEM    ,IXEL     ,ILAY     ,DFMAX    ,TDEL     ,
     8           DMG_FLAG )
C-----------------------------------------------
C    tabulated failure model
C-----------------------------------------------
      USE TABLE_MOD
      USE INTERFACE_TABLE_MOD
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
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
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
      INTEGER NEL,NPARAM,NUVAR,IP,IPT,NFUNC,IXFEM,IXEL,ILAY,
     .   NPTOT,DMG_FLAG
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
      TYPE(TTABLE) TABLE(*)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*)
      my_real FINTER ,TF(*)
      EXTERNAL FINTER
C        Y = FINTER(IFUNC(J),X,NPF,TF,DYDX)
C        Y       : y = f(x)
C        X       : x
C        DYDX    : f'(x) = dy/dx
C        IFUNC(J): FUNCTION INDEX
C              J : FIRST(J=1), SECOND(J=2) .. FUNCTION USED FOR THIS LAW
C        NPF,TF  : FUNCTION PARAMETER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  ::
     .  I,J,K,L,IADR,NINDX,ISHELL,LAYXFEM,I_MOD,I_DAM,
     .  ITABLE,IFUN_EL,IFUN_TEMP,INSTABIL,ID_DD,NF_LOC
      INTEGER, DIMENSION(NEL) :: INDX,NRATE,RFLAG,DMG_FLAG_INT
C
      my_real, DIMENSION(NEL) :: EPSF,EPSF_N,DMG_SCALE
      my_real  ::
     .  DP,P,SIGM,SIGM_PS,SVM,EF1,EF2,DF,FAC,DEPSF,LAMBDA,
     .  RATE1,RATE2,YFAC1,YFAC2,CC,BB,CR,ORM,SS1,SS2,YY,YY_N,DADV,
     .  X1SCALE,X2SCALE,X3SCALE,X4SCALE,P_THINN,ECRIT,FADE_EXPO,
     .  DCRIT,EL_REF,SC_EL,SC_TEMP,DD,DN
      CHARACTER (LEN=3) :: XCHAR
C-----------------------------------------------
C Storage of initial thickness in UVAR(x,2) at time = 0.0
C 1 = DAMAGE
C 2 = initial shell thickness
C 3 = DCrit_NS --> Instability starts
C 4 = percent from instability to failure
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
      ISHELL    = INT(UPARAM(2))  !ISHEL
      DCRIT     = UPARAM(4)
      DD        = UPARAM(5)
      DN        = UPARAM(6)
      SC_TEMP   = UPARAM(7)
      SC_EL     = UPARAM(8)
      EL_REF    = UPARAM(9)
      LAYXFEM   = UPARAM(10)
      DADV      = UPARAM(11)
      X1SCALE   = UPARAM(12)
      X2SCALE   = UPARAM(13)
      X3SCALE   = UPARAM(14)
      X4SCALE   = UPARAM(15)
      P_THINN   = UPARAM(16)
      ECRIT     = UPARAM(17)
      FADE_EXPO = UPARAM(18)
      I_MOD     = INT(UPARAM(19))
c-----------------------------
      IF (LAYXFEM == 0) RETURN
c-----------------------------
      LAYXFEM = IXFEM
      IF (LAYXFEM == 1 .and. ISHELL == 1) ISHELL=2
c-----------------------------
      I_DAM = 0      
      IF (ECRIT /= ZERO .OR. FADE_EXPO /= ZERO) I_DAM = 1
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
c     Failure strain value - function interpolation
C-------------------------------------------------------------------
      ITABLE = IFUNC(1)
c---- Scale functions       
      IFUN_EL   = IFUNC(2)
      IFUN_TEMP = IFUNC(3)
c---- Instability function
      INSTABIL  = IFUNC(5)
c
c---  failure strain interpolation
      DO I=1,NEL
        P    = THIRD*(SIGNXX(I) + SIGNYY(I))     
        SVM  = SQRT(SIGNXX(I)*SIGNXX(I) + SIGNYY(I)*SIGNYY(I)          
     .       - SIGNXX(I)*SIGNYY(I) + THREE*SIGNXY(I)*SIGNXY(I))        
        SIGM = P / MAX(EM20,SVM)
C----
C         XX0(1)=SIGM *X1SCALE
         XX0(1)=SIGM
         XX0(2)=EPSP(I) *X2SCALE
         CALL TABLE_INTERP (TABLE(ITABLE),XX0,YY)            
         EPSF(I) = YY * X1SCALE
      ENDDO
c
      DO I=1,NEL
c----   element length scale function
        IF (IFUN_EL > 0) THEN
           LAMBDA = UVAR(I,5) / EL_REF
           FAC = SC_EL*FINTER(IFUN_EL,LAMBDA,NPF,TF,DF) 
           EPSF(I) = EPSF(I)* FAC 
        ENDIF                                                              
c----   temperature scale function
        IF (IFUN_TEMP > 0) THEN
           FAC = SC_TEMP*FINTER(IFUN_TEMP,TSTAR(I),NPF,TF,DF) 
           EPSF(I) = EPSF(I)* FAC 
        ENDIF    
        
C----   Instability function

        IF (INSTABIL > 0) THEN
           XX0(2)=EPSP(I) *X4SCALE
           CALL TABLE_INTERP (TABLE(INSTABIL),XX0,YY_N)            
           EPSF_N(I) = YY_N * X3SCALE
        ELSEIF (ECRIT > 0.0) THEN
           EPSF_N(I) = ECRIT
        ELSE 
           EPSF_N(I) = ZERO
        ENDIF
        
C----   Fading exponent
        IF (FADE_EXPO < ZERO) THEN
           LAMBDA = UVAR(I,5) / EL_REF
           FADE_EXPO = FINTER(IFUNC(6),LAMBDA,NPF,TF,DF) 
        ENDIF
C----         
      ENDDO
C-------------------------------------------------------------------
          IF (ISHELL == 1) THEN    ! shell deleted when rupture in one integration point
            IF (IXFEM == 1 .OR. IXFEM == 2) THEN 
              DO I=1,NEL
                IF (ISHELL == 1 .AND. OFF(I)==ONE) THEN
                  ID_DD = IFUNC(NFUNC)
                  IF(ID_DD /= 0 )THEN
                    DP = FINTER(ID_DD,UVAR(I,1),NPF,TF,DF)
                  ELSE
                    IF(UVAR(I,1) == ZERO) THEN 
                      DP = ONE
                      ELSE
C                     DP = DN*DD**(ONE-ONE/DN) ! old and wrong
                      DP = DN*UVAR(I,1)**(ONE-ONE/DN)
                    ENDIF
                  ENDIF
                  IF (EPSF(I) > ZERO) UVAR(I,1)=
     .                                UVAR(I,1)+DP*DPLA(I)/EPSF(I)
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
            IF (IXFEM == 1) THEN
              DO I=1,NEL
                IF (OFF(I) == ONE)THEN
                  IF (UVAR(I,1) < DCRIT) THEN
                    ID_DD = IFUNC(NFUNC)
                    IF(ID_DD /= 0 )THEN
                      DP = FINTER(ID_DD,UVAR(I,1),NPF,TF,DF)
                    ELSE
                     IF(UVAR(I,1) == ZERO) THEN 
                       DP = ONE
                       ELSE
C                      DP = DN*DD**(ONE-ONE/DN) ! old and wrong
                       DP = DN*UVAR(I,1)**(ONE-ONE/DN)
                     ENDIF
                    ENDIF
                    IF (EPSF(I) > ZERO) UVAR(I,1)=
     .                                  UVAR(I,1)+DP*DPLA(I)/EPSF(I)
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
                        ELCRKINI(ILAY,I) = -1
                        NOFF(I) = NOFF(I) + 1
                        IF (NOFF(I) == NPTOT) THEN
                          OFF(I) = FOUR_OVER_5                                           
                          TDEL(I)= TIME
                        ENDIF 
                        RFLAG(I) = 1
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
                     ID_DD = IFUNC(NFUNC)
                    IF(ID_DD /= 0 )THEN
                      DP = FINTER(ID_DD,UVAR(I,1),NPF,TF,DF)
                    ELSE
                     IF(UVAR(I,1) == ZERO) THEN 
                       DP = ONE
                       ELSE
C                      DP = DN*DD**(ONE-ONE/DN) ! old and wrong
                       DP = DN*UVAR(I,1)**(ONE-ONE/DN)
                     ENDIF
                    ENDIF

                    IF (EPSF(I) > ZERO) UVAR(I,1)=
     .                                  UVAR(I,1)+DP*DPLA(I)/EPSF(I)
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
c
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
