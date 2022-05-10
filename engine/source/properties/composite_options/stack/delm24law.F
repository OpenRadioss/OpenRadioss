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
C total strain delamination failure model ------
Chd|====================================================================
Chd|  DELM24LAW                     source/properties/composite_options/stack/delm24law.F
Chd|-- called by -----------
Chd|        DELAMINATION                  source/properties/composite_options/stack/delamination.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DELM24LAW(
     1     NEL    ,NUPARAM ,NUVAR  ,MFUNC    ,KFUNC   ,
     2     NPF    ,TF      ,TIME   ,TIMESTEP ,UPARAM  ,
     3     NGL    ,IPLY    ,IPM    ,MAT      ,IP      ,
     4     OFF    ,SIGNYZ  ,SIGNXZ ,SIGNZZ   ,EPSYZ   ,
     5     EPSXZ  ,EPSZZ   ,UVAR   , OFFI    ,REDUC   ,
     6     COUNT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C MFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW not used
C KFUNC   | NFUNC   | I | R | FUNCTION INDEX not used
C NPF     |  *      | I | R | FUNCTION ARRAY   
C TF      |  *      | F | R | FUNCTION ARRAY 
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C EPSPXX  | NEL     | F | R | STRAIN RATE XX
C EPSPYY  | NEL     | F | R | STRAIN RATE YY
C ...     |         |   |   |
C DEPSXX  | NEL     | F | R | STRAIN INCREMENT XX
C DEPSYY  | NEL     | F | R | STRAIN INCREMENT YY
C ...     |         |   |   |
C EPSXX   | NEL     | F | R | STRAIN XX
C EPSYY   | NEL     | F | R | STRAIN YY
C ...     |         |   |   |
C SIGOXX  | NEL     | F | R | OLD ELASTO PLASTIC STRESS XX 
C SIGOYY  | NEL     | F | R | OLD ELASTO PLASTIC STRESS YY
C ...     |         |   |   |    
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SIGVXX  | NEL     | F | W | VISCOUS STRESS XX
C SIGVYY  | NEL     | F | W | VISCOUS STRESS YY
C ...     |         |   |   |
C SOUNDSP | NEL     | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C VISCMAX | NEL     | F | W | MAXIMUN DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
#include "mvsiz_p.inc"
#include "scr17_c.inc"
#include "units_c.inc"
#include  "comlock.inc"
#include  "param_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER NEL, NUPARAM, NUVAR,NGL(*),IPM(NPROPMI,*),
     .        MAT(*),IP, IPLY
      my_real 
     .   TIME,TIMESTEP(*),UPARAM(*),SIGNZZ(*),
     .   SIGNYZ(*),SIGNXZ(*),EPSYZ(*),EPSXZ(*),EPSZZ(*),
     .   OFFI(*),REDUC(*),COUNT(*)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
cc      my_real
 
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real UVAR(NEL,NUVAR), OFF(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), MFUNC, KFUNC(MFUNC)
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
      INTEGER 
     .        I,J,IDEL,IDEL_L,IFLAG(MVSIZ),INDX(MVSIZ),IADBUF,NINDX,
     .        NINDEX,INDEX(MVSIZ),IFAIL,JST(MVSIZ),IR,JJ,IMATLY,IMOD,
     .        MODE(MVSIZ)
      my_real 
     .   DAM1,DAM2,DAM3,DAM
      my_real
     .  EPST3(MVSIZ),EPSF3(MVSIZ),EPST13(MVSIZ),EPSF13(MVSIZ),
     .  EPST23(MVSIZ),EPSF23(MVSIZ)
C--------------------------------------------------------------
C
      IR = 0
      DO I=1,NEL
C
        IF(OFF(I)==ZERO) CYCLE
C
        IMATLY=MAT(I)  
        IFAIL = IPM(111 +IP,IMATLY)
        IF(IFAIL == 24)THEN  
         IADBUF     =  IPM(114 +IP, IMATLY)
cc         EPST1(I) = UPARAM(IADBUF)
cc         EPSF1(I) = UPARAM(IADBUF+1)
cc         EPST2(I) = UPARAM(IADBUF+2)
cc         EPSF2(I) = UPARAM(IADBUF+3)
         EPST3(I) = UPARAM(IADBUF+4)
         EPSF3(I) = UPARAM(IADBUF+5)
cc         EPST12(I) = UPARAM(IADBUF+6)
cc         EPSF12(I) = UPARAM(IADBUF+7)
         EPST23(I) = UPARAM(IADBUF+8)
         EPSF23(I) = UPARAM(IADBUF+9)
         EPST13(I) = UPARAM(IADBUF+10)
         EPSF13(I) = UPARAM(IADBUF+11)
         REDUC(I) = UPARAM(IADBUF+15)
         IR = IR + 1
         JST(IR) = I 
        ENDIF 
C
         INDX(I) = 0
         INDEX(I) = 0
      ENDDO 
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
      IF(TIME == ZERO)THEN
       DO JJ=1,IR
        I = JST(JJ)
        DO J=1,NUVAR  
          UVAR(I,J)= ZERO
        ENDDO
       ENDDO
      ENDIF   
C-------------------------------
C      
C     OFF = 0.
C-------------------------------
C           
        NINDX=0 
        NINDEX = 0
        DO J =1,IR
         I=JST(J)
C         
         DAM1 = ZERO
         DAM2 = ZERO
         DAM3 = ZERO 
         IMOD = 0 
         MODE(I)  = 0      
         IF(OFF(I) == UN )THEN
C------------------------------- 
C direction 33                
           IF(UVAR(I,1) < UN )THEN 
C direction 23
               IF(EPSYZ(I) >= EPST23(I) ) THEN
                   DAM2 = (EPSYZ(I) - EPST23(I))/(EPSF23(I) - EPST23(I))
                   DAM2 = MIN(UN, DAM2) 
                   IF(DAM2 >= UN) IMOD = 3
               ENDIF         
C direction 13
               IF(EPSXZ(I) >= EPST13(I) ) THEN
                   DAM3 = (EPSXZ(I) - EPST13(I))/(EPSF13(I) - EPST13(I))
                   DAM3 = MIN(UN, DAM3)
                  IF(DAM3 >= UN) IMOD = 2
               ENDIF     
                                 
               IF(EPSZZ(I) >= EPST3(I) ) THEN
                   DAM1= (EPSZZ(I) - EPST3(I))/(EPSF3(I) - EPST3(I))
                   DAM1 = MIN(UN, DAM1)  
                   IF(DAM1 >= UN) IMOD = 1
               ENDIF
C
               DAM = MAX (UVAR(I,1),DAM1, DAM2, DAM3) 
               UVAR(I,1) = DAM
               SIGNXZ(I) = SIGNXZ(I)*MAX(UN  - DAM,REDUC(I))
               SIGNYZ(I) = SIGNYZ(I)*MAX(UN  - DAM,REDUC(I))
               SIGNZZ(I) = SIGNZZ(I)*MAX(UN  - DAM,REDUC(I))
C                        
               IF(DAM == UN) THEN
                  NINDX=NINDX+1
                  INDX(NINDX)=I
!!               OFFI(I) = REDUC(I)
                  MODE(I) = IMOD
                  COUNT(I) = COUNT(I) + UN
                  IF(INT(COUNT(I)) == 4)THEN
                    WRITE(IOUT, 1300) NGL(I),IPLY,MODE(I),TIME
                    WRITE(ISTDO,1300) NGL(I),IPLY,MODE(I), TIME
                  ENDIF
               ENDIF
          ELSE            
!!              SIGNXZ(I) = ZERO
!!              SIGNYZ(I) = ZERO
!!              SIGNZZ(I) = ZERO
!!              OFFI(I) = MIN(OFFI(I), ZERO)
              OFFI(I) = REDUC(I)
          ENDIF
         ENDIF   
        ENDDO  

        IF(NINDX > 0)THEN
          DO J=1,NINDX
           I = INDX(J)
#include "lockon.inc"
          WRITE(IOUT, 1200) NGL(I),IPLY,MODE(I),TIME
           WRITE(ISTDO,1200) NGL(I),IPLY,MODE(I), TIME
#include "lockoff.inc"
          END DO
         ENDIF            
C--------------------------------------------

 1200 FORMAT(1X,'DELAMINATION OF SHELL #',I10,1X,
     . 'INTERPLY', I10, 1X,'MODE #',I10,1X,
     . 'AT TIME # ',1PE20.13)
 1300 FORMAT(1X,'FULL DELAMINATION OF SHELL #',I10,1X,
     . 'INTERPLY', I10, 1X,'MODE #',I10,1X,
     . 'AT TIME # ',1PE20.13)    
      RETURN
      END
