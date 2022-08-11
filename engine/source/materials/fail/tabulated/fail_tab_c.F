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
Chd|  FAIL_TAB_C                    source/materials/fail/tabulated/fail_tab_c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|        TABLE_INTERP                  source/tools/curve/table_tools.F
Chd|        TABLE_VINTERP                 source/tools/curve/table_tools.F
Chd|        VINTER                        source/tools/curve/vinter.F   
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        INTERFACE_TABLE_MOD           share/modules/table_mod.F     
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE FAIL_TAB_C(
     1           NEL      ,NUPARAM  ,NUVAR    ,UPARAM   ,UVAR     ,
     2           NFUNC    ,IFUNC    ,TABLE    ,NPF      ,TF       ,
     3           TIME     ,NGL      ,IPG      ,ILAY     ,IPT      ,
     4           SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     5           DPLA     ,EPSP     ,THK      ,ALDT     ,TEMP     ,
     6           PTHKF    ,DMG_FLAG ,DMG_SCALE,OFF      ,FOFF     ,
     7           DFMAX    ,TDEL     ,IGTYP    ,INLOC    )
C-----------------------------------------------
C    tabulated failure model
C-----------------------------------------------
      USE TABLE_MOD
      USE INTERFACE_TABLE_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include "units_c.inc"
#include "comlock.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C DPLA    | NEL     | F | R | PLASTIC STRAIN
C EPSP    | NEL     | F | R | STRAIN RATE
C THK     | NEL     | F | R | ELEMENT THICKNESS
C TEMP    | NEL     | F | R | ELEMENT TEMPERATURE
C---------+---------+---+---+--------------------------------------------
C OFF     | NEL     | F | R | DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C FOFF    | NEL     | I |R/W| DELETED INTEGRATION POINT FLAG (=1 ON, =0 OFF)
C PTHKF   |  1      | F | W | PERCENTAGE OF LAYER THICKNESS TO FAIL
C DFMAX   | NEL     | F |R/W| MAX DAMAGE FACTOR 
C TDEL    | NEL     | F | W | FAILURE TIME
C DMG_FLAG|  1      | I | W | STRESS REDUCTION FLAG DUE TO DAMAGE
C DMG_SCALE| NEL    | F | W | STRESS REDUCTION FACTOR
C---------+---------+---+---+--------------------------------------------
C NGL                         ELEMENT ID
C IPG                         CURRENT GAUSS POINT (in plane)
C ILAY                        CURRENT LAYER
C IPT                         CURRENT INTEGRATION POINT IN THE LAYER
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,NUPARAM,NUVAR,IPG,ILAY,IPT,IGTYP,INLOC
      INTEGER ,DIMENSION(NEL) ,INTENT(IN) :: NGL
      my_real ,INTENT(IN) :: TIME
      my_real ,DIMENSION(NEL) ,INTENT(IN)    :: OFF,THK,ALDT,DPLA,EPSP,
     .   TEMP,SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX
      my_real,DIMENSION(NUPARAM) ,INTENT(IN) :: UPARAM
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER ,INTENT(OUT) ::DMG_FLAG
      INTEGER ,DIMENSION(NEL) ,INTENT(INOUT) :: FOFF
      my_real ,INTENT(OUT) :: PTHKF
      my_real ,DIMENSION(NEL) ,INTENT(INOUT) :: DFMAX
      my_real ,DIMENSION(NEL) ,INTENT(OUT)   :: TDEL,DMG_SCALE
      my_real ,DIMENSION(NEL,NUVAR) ,INTENT(INOUT) :: UVAR
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real FINTER ,TF(*)
      EXTERNAL FINTER
      TYPE(TTABLE) TABLE(*)
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
      INTEGER  :: I,J,K,L,IP_THICK,NINDX,NINDXF,NINSTAB,IFAIL_SH,NDIM,
     .  ITABLE,IFUN_EL,IFUN_TEMP,INSTABIL,ID_DD,NF_LOC,INST_FLAG,SIZE_FLAG
      INTEGER, DIMENSION(NEL) :: INDX,INDXF,INDSTAB,IPOSV,IADP,ILENP
      INTEGER IPOST1(NEL,1),IPOST2(NEL,2),IPOST3(NEL,3)
C
      my_real :: SHRF,BIAXF
      my_real, DIMENSION(NEL) :: EPSF,EPSF_N,SIGM,YYV,DXDYV,LAMBDAV
      my_real, DIMENSION(2)   :: XX2
      my_real, DIMENSION(3)   :: XX3
      my_real, DIMENSION(NEL,1) :: XXV1
      my_real, DIMENSION(NEL,2) :: XXV2
      my_real, DIMENSION(NEL,3) :: XXV3
      my_real  :: P,SVM,DF,FAC,LAMBDA,
     .  Y1SCALE,X1SCALE,Y2SCALE,X2SCALE,P_THINN,ECRIT,FADE_EXPO,
     .  DCRIT,EL_REF,SC_EL,SC_TEMP,P_THICK,DP,DD,DN,YY,YY_N
C-----------------------------------------------
C Storage of initial thickness in UVAR(x,2) at time = 0.0
C 1 = DAMAGE
C 2 = initial shell thickness
C 3 = DCrit_NS --> Instability starts
C 4 = percent from instability to failure
C 5 = initial characteristic el. length
C 6 = IPOS1 for TABLE_VINTERP
C 7 = IPOS2 for TABLE_VINTERP
C 8 = IPOS1 for VINTER
C=======================================================================
      IFAIL_SH  = INT(UPARAM(2))
      PTHKF     = UPARAM(3)
      P_THICK   = UPARAM(3)
      DCRIT     = UPARAM(4)
      DD        = UPARAM(5)
      DN        = UPARAM(6)
      SC_TEMP   = UPARAM(7)
      SC_EL     = UPARAM(8)
      EL_REF    = UPARAM(9)
      Y1SCALE   = UPARAM(12)
      X1SCALE   = UPARAM(13)
      Y2SCALE   = UPARAM(14)
      X2SCALE   = UPARAM(15)
      P_THINN   = UPARAM(16)
      ECRIT     = UPARAM(17)
      FADE_EXPO = UPARAM(18)
      DMG_FLAG  = INT(UPARAM(19))
      INST_FLAG = INT(UPARAM(20))
      SHRF      = UPARAM(21)
      BIAXF     = UPARAM(22)
      IF (SHRF == -ONE .and. BIAXF == ONE) THEN
        SIZE_FLAG = 0
      ELSE
        SIZE_FLAG = 1
      END IF
C-------------------------------------------------------------------
c     Failure strain value - function interpolation
C-------------------------------------------------------------------
      ITABLE    = IFUNC(1)
c---- Scale functions       
      IFUN_EL   = IFUNC(2)
      IFUN_TEMP = IFUNC(3)
c---- Instability function
      INSTABIL  = IFUNC(5)
c---------------------------
      IF (P_THICK > ZERO .and. IFAIL_SH > 1 .and. IGTYP <= 10) THEN
        PTHKF = P_THICK
      ELSEIF (IFAIL_SH == 1) THEN
        PTHKF = EM06
      ELSEIF (IFAIL_SH == 2) THEN
        PTHKF = ONE - EM06
      ENDIF
C---------
      NINDXF  = 0  
      NINDX   = 0  
      NINSTAB = 0
      DO I=1,NEL
        IF (INLOC > 0) UVAR(I,5) = ALDT(I)
        IF (OFF(I) == ONE .and. FOFF(I) == 1) THEN
          NINDX = NINDX+1                                                 
          INDX(NINDX) = I
        ENDIF
      ENDDO
C-------------------------------------------------------------------
c     Failure strain value - function interpolation
C-------------------------------------------------------------------
c
c---  failure strain interpolation
      NDIM = TABLE(ITABLE)%NDIM
      DO J=1,NINDX                           
        I = INDX(J)  
        P    = THIRD*(SIGNXX(I) + SIGNYY(I))     
        SVM  = SQRT(SIGNXX(I)*SIGNXX(I) + SIGNYY(I)*SIGNYY(I)          
     .       - SIGNXX(I)*SIGNYY(I) + THREE*SIGNXY(I)*SIGNXY(I))        
        SIGM(I) = P / MAX(EM20,SVM)
      ENDDO
C----
      IF (NDIM == 3) THEN
        DO J=1,NINDX                           
          I = INDX(J)  
          XXV3(J,1) = SIGM(I)
          XXV3(J,2) = EPSP(I)*X1SCALE
          XXV3(J,3) = ZERO
          IPOST3(J,1)= UVAR(I,6)
          IPOST3(J,2)= UVAR(I,7)
          IPOST3(J,3)= 0
        ENDDO
      ELSE IF (NDIM == 2) THEN
        DO J=1,NINDX                           
          I = INDX(J)  
          XXV2(J,1) = SIGM(I)
          XXV2(J,2) = EPSP(I)*X1SCALE
          IPOST2(J,1)= UVAR(I,6)
          IPOST2(J,2)= UVAR(I,7)
        ENDDO
      ELSE IF (NDIM == 1) THEN
        DO J=1,NINDX                           
          I = INDX(J)  
          XXV1(J,1)   = SIGM(I)
          IPOST1(J,1) = NINT(UVAR(I,6))
        ENDDO
      END IF
c
c     check elements with triaxiality between SHRF and BIAXF) in case of SIZE_FLAG=1
      IF (SIZE_FLAG == 1) THEN      
        DO J=1,NINDX                         
          I = INDX(J)
          IF (SIGM(I) > SHRF .and. SIGM(I) < BIAXF) THEN
            NINSTAB = NINSTAB + 1
            INDSTAB(NINSTAB) = I
          END IF
        ENDDO
      END IF                                                                                
c
      IF (NDIM == 3) THEN
c
        CALL TABLE_VINTERP (TABLE(ITABLE),NINDX,IPOST3,XXV3,YYV,DXDYV)
c
        DO J=1,NINDX                           
          I = INDX(J)  
          EPSF(I) = YYV(J) * Y1SCALE     
          UVAR(I,6)=IPOST3(J,1)
          UVAR(I,7)=IPOST3(J,2)
        ENDDO
c
      ELSE IF (NDIM == 2) THEN
c
        CALL TABLE_VINTERP (TABLE(ITABLE),NINDX,IPOST2,XXV2,YYV,DXDYV)
c
        DO J=1,NINDX                           
          I = INDX(J)  
          EPSF(I) = YYV(J) * Y1SCALE     
          UVAR(I,6)=IPOST2(J,1)
          UVAR(I,7)=IPOST2(J,2)
        ENDDO
c
      ELSE IF (NDIM == 1) THEN
c
        CALL TABLE_VINTERP (TABLE(ITABLE),NINDX,IPOST1,XXV1,YYV,DXDYV)
c
        DO J=1,NINDX                           
          I = INDX(J)  
          EPSF(I)   = YYV(J) * Y1SCALE     
          UVAR(I,6) = IPOST1(J,1)
        ENDDO

      END IF
c
      IF (IFUN_EL > 0 .AND. INST_FLAG /= 2) THEN
        IF (SIZE_FLAG == 0) THEN          
#include "vectorize.inc"
          DO J=1,NINDX                           
            I = INDX(J)  
c----       element length scale function
            LAMBDAV(J) = UVAR(I,5) / EL_REF
            IPOSV(J) = NINT(UVAR(I,8))
            IADP (J) = NPF(IFUN_EL) / 2 + 1
            ILENP(J) = NPF(IFUN_EL + 1) / 2 -IADP (J) - IPOSV(J)
          ENDDO
c
          CALL VINTER(TF,IADP,IPOSV,ILENP,NINDX,LAMBDAV,DXDYV,YYV)
c
#include   "vectorize.inc"
          DO J=1,NINDX                           
            I = INDX(J)  
            FAC = SC_EL*YYV(J) 
            EPSF(I)   = EPSF(I)* FAC 
            UVAR(I,8) = IPOSV(J)
          ENDDO
c
        ELSE  ! SIZE_FLAG = 1
#include "vectorize.inc"
          DO J=1,NINSTAB                           
            I = INDSTAB(J)  
c----       element length scale function
            LAMBDAV(J) = UVAR(I,5) / EL_REF
            IPOSV(J) = NINT(UVAR(I,8))
            IADP (J) = NPF(IFUN_EL) / 2 + 1
            ILENP(J) = NPF(IFUN_EL + 1) / 2 -IADP (J) - IPOSV(J)
          ENDDO
c
          CALL VINTER(TF,IADP,IPOSV,ILENP,NINSTAB,LAMBDAV,DXDYV,YYV)
c
#include   "vectorize.inc"
          DO J=1,NINSTAB                           
            I = INDSTAB(J)  
            FAC = SC_EL*YYV(J) 
            EPSF(I)   = EPSF(I)* FAC 
            UVAR(I,8) = IPOSV(J)
          ENDDO
        END IF          
      ENDIF  

c----   Instability function
c
      IF (INSTABIL > 0) THEN
c
#include "vectorize.inc"
        DO J=1,NINDX                           
          I = INDX(J)  
          XX2(1) = SIGM(I)
          XX2(2) = EPSP(I) *X2SCALE
          CALL TABLE_INTERP (TABLE(INSTABIL),XX2,YY_N)            
          EPSF_N(I) = YY_N * Y2SCALE
        ENDDO
c       apply element scale factor
        IF (IFUN_EL > 0 .AND. INST_FLAG /= 1) THEN
          IF (SIZE_FLAG == 0) THEN          
#include "vectorize.inc"
            DO J=1,NINDX                           
              I = INDX(J)  
              LAMBDA = UVAR(I,5) / EL_REF
              FAC = SC_EL*FINTER(IFUN_EL,LAMBDA,NPF,TF,DF) 
              EPSF_N(I) = EPSF_N(I)* FAC   
            ENDDO
          ELSE
#include "vectorize.inc"
            DO J=1,NINSTAB                           
              I = INDSTAB(J)  
              LAMBDA = UVAR(I,5) / EL_REF
              FAC = SC_EL*FINTER(IFUN_EL,LAMBDA,NPF,TF,DF) 
              EPSF_N(I) = EPSF_N(I)* FAC   
            ENDDO
          END IF
        ENDIF            
c
      ELSEIF (ECRIT > ZERO) THEN
c
#include "vectorize.inc"
        DO J=1,NINDX                           
          I = INDX(J)  
          EPSF_N(I) = ECRIT
        ENDDO
      ELSE 
#include "vectorize.inc"
        DO J=1,NINDX                           
          I = INDX(J)  
          EPSF_N(I) = ZERO
        ENDDO
      ENDIF
c----   temperature scale function
      IF (IFUN_TEMP > 0) THEN
        DO J=1,NINDX                           
          I = INDX(J)  
          FAC = SC_TEMP*FINTER(IFUN_TEMP,TEMP(I),NPF,TF,DF) 
          EPSF(I) = EPSF(I)* FAC 
        ENDDO
      ENDIF    

c----   Fading exponent
      IF (FADE_EXPO < ZERO) THEN
        DO J=1,NINDX                           
          I = INDX(J)  
          LAMBDA = UVAR(I,5) / EL_REF
          FADE_EXPO = FINTER(IFUNC(6),LAMBDA,NPF,TF,DF) 
        ENDDO
      ENDIF
c-----------------------------------------------------------------------------
      DO J=1,NINDX                           
        I = INDX(J)  
          IF (UVAR(I,1) < DCRIT) THEN
            ID_DD = IFUNC(4)
            IF (ID_DD /= 0 ) THEN
              DP = FINTER(ID_DD,UVAR(I,1),NPF,TF,DF)
            ELSE
              IF (UVAR(I,1) == ZERO) THEN 
                DP = ONE
              ELSE
                DP = DN*UVAR(I,1)**(ONE-ONE/DN)
              ENDIF
            ENDIF
            IF (EPSF(I) > ZERO) UVAR(I,1) = UVAR(I,1)+DP*DPLA(I)/EPSF(I)
            IF ((P_THINN*UVAR(I,2)) > THK(I)) THEN ! Failure due to thinning
              UVAR(I,1) = DCRIT     
              FOFF(I)   = 0
              TDEL(I)   = TIME
              PTHKF     = EM06
            ENDIF
c-----
c           Damage accumulation
            IF (DMG_FLAG == 1 .AND. UVAR(I,1) <= DCRIT) THEN
              IF (EPSF_N(I) > ZERO .AND. SIGM(I) >= ZERO ) THEN 
                UVAR(I,3) = UVAR(I,3) + DP*DPLA(I)/EPSF_N(I)
              ENDIF                
c
              IF (UVAR(I,3) >= ONE) THEN                ! softening starts
                IF (FADE_EXPO /= ZERO) THEN 
                  UVAR(I,4) = UVAR(I,4) + DP*DPLA(I)/(EPSF(I)-EPSF_N(I))
                  IF (UVAR(I,4) > DD) THEN 
                    DMG_SCALE(I) = ONE - ((UVAR(I,4)-DD)/(ONE-DD))**FADE_EXPO
                    DMG_SCALE(I) = MAX(DMG_SCALE(I),ZERO)
                  ENDIF
                ENDIF
              ENDIF   
c            ELSEIF (DMG_FLAG == 1) THEN 
c              DMG_SCALE(I) = ZERO
            ENDIF ! end damage accumulation and softening
c-----            
            IF (UVAR(I,1) >= DCRIT) THEN
              NINDXF = NINDXF+1                                                 
              INDXF(NINDXF) = I                                                                                     
              TDEL(I)= TIME  
              IF (IFAIL_SH == 3) THEN
                FOFF(I) = -1
              ELSE
                FOFF(I) = 0
              ENDIF
            ENDIF                                                 
          ENDIF  
      ENDDO ! IEL
c
c--------------------------------------------      
c     Maximum Damage storing for output : 0 < DFMAX < 1
c
      DO J=1,NINDX                           
        I = INDX(J)  
        DFMAX(I)= MIN(ONE,MAX(DFMAX(I),UVAR(I,1)/DCRIT))
      ENDDO
c--------------------------------------------      
c     print
c--------------------------------------------      
      IF (NINDXF > 0) THEN  
        DO J=1,NINDXF                           
          I = INDXF(J)  
#include  "lockon.inc"
          WRITE(IOUT, 2000) NGL(I),IPG,ILAY,IPT
          WRITE(ISTDO,2100) NGL(I),IPG,ILAY,IPT,TIME
#include  "lockoff.inc" 
        ENDDO
      ENDIF  
c------------------------
 2000 FORMAT(1X,'FAILURE (FTAB) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3)
 2100 FORMAT(1X,'FAILURE (FTAB) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3,1X,'AT TIME :',1PE12.4)
c------------------------
      RETURN
      END
