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
Chd|  VISC_PRONY                    source/materials/visc/visc_prony.F
Chd|-- called by -----------
Chd|        VISCMAIN                      source/materials/visc/viscmain.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE VISC_PRONY_LSTRAIN(
     .           NEL     ,NUVAR   ,NPRONY  ,IADBUF  ,NUPARAM, UPARAM  ,
     .           UVAR    , TIMESTEP  ,
     .           MFXX    ,MFXY    ,MFXZ    ,MFYX    ,MFYY    ,MFYZ    ,
     .           MFZX    ,MFZY    ,MFZZ    ,
     .           S1      ,S2      ,S3      ,S4      ,S5      ,S6      )
C----------------------------------------------
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
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C RHO     | NEL     | F | R | INITIAL DENSITY
C EPSPXX  | NEL     | F | R | STRAIN RATE XX
C EPSPYY  | NEL     | F | R | STRAIN RATE YY
C ...     |         |   |   |
C ...     |         |   |   |
C VISC    | NEL*6   | F | W | VISCOUS  STRESS 
C ...     |         |   |   |
C VISCMAX | NEL     | F | W | MAXIMUN DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NUVAR,NPRONY,IADBUF,NUPARAM
      my_real
     .       TIMESTEP
      my_real, INTENT(IN) :: UPARAM(NUPARAM)
      my_real, DIMENSION(NEL), INTENT(IN) :: MFXX,MFXY,MFXZ,MFYX,MFYY,MFYZ ,
     .                                         MFZX,MFZY,MFZZ
      my_real, DIMENSION(NEL), INTENT(INOUT) ::   S1  ,S2  ,S3  ,S4  ,S5  ,S6  
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
!!      my_real , INTENT(INOUT) ::   SOUNDSP(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real
     .    UVAR(NEL,NUVAR)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,II,IFLAG,IMOD,NUVARI,N, FLAG_VISC
      my_real ::  
     . DTINV,FAC,INVDT,RHELP,GSTARD,GAMADSTAR,TTT,
     , MFXX1,MFYY1,MFZZ1
      my_real, DIMENSION(NPRONY) :: EPSILD,ALPHAD,BETAD,GAMAD,EPSIK,
     .                              ALPHAK,BETAK,GAMAK, TAUD,GAMAD
     .                              GAMAK,TAUK
     
      my_real, DIMENSION(NEL) :: DETF,INVDETF,JDEFG, P
      my_real :: DEFGI(NEL,9),DEFG(NEL,9),STRS0(NEL,6),STRS1(NEL,6),
     .          QINT(NEL,6,NPRONY),QINTLD(NEL,6,NPRONY), TEMP(NEL,9),
     .          QBAR(NEL,6),TEMP6(NEL,6),SHYP(NEL,6)
C=======================================================================
      FLAG_VISC = INT(UPARAM(IADBUF + 2))
      GAMADSTAR  = ZERO
      DTINV = TIMESTEP /MAX(EM20,TIMESTEP**2) 
      DO J=1,NPRONY                                  
          GAMAD(J)   = UPARAM(IADBUF + 2 + J)            
          TAUD(J)    = UPARAM(IADBUF + 2 + NPRONY + J)  
      ENDDO 
      GSTARD = ZERO
      DO I = 1, NPRONY
        TTT = TAUD(I)*DTINV
        EPSILD(I) = EXP(-TIMESTEP/TAUD(I))
        ALPHAD(I) = ONE - TTT*(ONE - EPSILD(I))
        BETAD(I)  = ONE - ALPHAD(I) - EPSILD(I)
        ALPHAD(I) = GAMAD(I)*ALPHAD(I)
        BETAD(I)  = GAMAD(I)*BETAD(I)
        GSTARD    = GSTARD + ALPHAD(I)
      ENDDO
      GSTARD = ONE  - GSTARD
C Computing the inverse of F
      DO I=1,NEL
        ! J=DETF
        MFXX1 = MFXX(I) + ONE
        MFYY1 = MFYY(I) + ONE
        MFZZ1 = MFZZ(I) + ONE 
        !!
        SHYP(I,1) = S1(I)
        SHYP(I,2) = S2(I)
        SHYP(I,3) = S3(I)
        SHYP(I,4) = S4(I)
        SHYP(I,5) = S5(I)
        SHYP(I,6) = S6(I)
        
        DEFG(I,1) = MFXX1
        DEFG(I,2) = MFYX(I)
        DEFG(I,3) = MFZX(I)
        DEFG(I,4) = MFXY(I)
        DEFG(I,5) = MFYY1 
        DEFG(I,6) = MFZY(I)
        DEFG(I,7) = MFXZ(I)
        DEFG(I,8) = MFYZ(I)
        DEFG(I,9) = MFZZ1 
        !!        
        DETF(I) = MFXX1*MFYY1*MFZZ1 + MFXY(I)*MFYZ(I)*MFZX(I) +
     .            MFYX(I)*MFZY(I)*MFXZ(I) - MFYY1*MFXZ(I)*MFZX(I) -
     .            MFXY(I)*MFYX(I)*MFZZ1 - MFYZ(I)*MFZY(I)*MFXX1   
         !! F^-1
        INVDETF(I) = ONE/DETF(I)
 
!!        rhelp = INVDETF(I)
        DEFGI(I,1) = (DEFG(I,5)*DEFG(I,9)-DEFG(I,6)*DEFG(I,8))*INVDETF(I)
        DEFGI(I,2) =-(DEFG(I,2)*DEFG(I,9)-DEFG(I,3)*DEFG(I,8))*INVDETF(I)
        DEFGI(I,3) = (DEFG(I,2)*DEFG(I,6)-DEFG(I,3)*DEFG(I,5))*INVDETF(I)
        DEFGI(I,4) =-(DEFG(I,4)*DEFG(I,9)-DEFG(I,6)*DEFG(I,7))*INVDETF(I)
        DEFGI(I,5) = (DEFG(I,1)*DEFG(I,9)-DEFG(I,3)*DEFG(I,7))*INVDETF(I)
        DEFGI(I,6) =-(DEFG(I,1)*DEFG(I,6)-DEFG(I,3)*DEFG(I,4))*INVDETF(I)
        DEFGI(I,7) = (DEFG(I,4)*DEFG(I,8)-DEFG(I,5)*DEFG(I,7))*INVDETF(I)
        DEFGI(I,8) =-(DEFG(I,1)*DEFG(I,8)-DEFG(I,2)*DEFG(I,7))*INVDETF(I)
        DEFGI(I,9) = (DEFG(I,1)*DEFG(I,5)-DEFG(I,2)*DEFG(I,4))*INVDETF(I)
        !
        JDEFG(I) = DETF(I)
      ENDDO 
      IF(FLAG_VISC == 2 ) THEN
        DO I=1,NEL
           P(I) = THIRD*(S1(I) + S2(I) +S3(I))
           SHYP(I,1) = S1(I) - P(I)
           SHYP(I,2) = S2(I) - P(I)
           SHYP(I,3) = S3(I) - P(I)
        ENDDO 
      ENDIF
      
       DO I = 1,NEL
            STRS1(I,1) = SHYP(I,1)
            STRS1(I,2) = SHYP(I,2)
            STRS1(I,3) = SHYP(I,3)
            STRS1(I,4) = SHYP(I,4)
            STRS1(I,5) = SHYP(I,5)
            STRS1(I,6) = SHYP(I,6)
            !!
            STRS0(I,1) = UVAR(I,1)
            STRS0(I,2) = UVAR(I,2)
            STRS0(I,3) = UVAR(I,3)
            STRS0(I,4) = UVAR(I,4)
            STRS0(I,5) = UVAR(I,5)
            STRS0(I,6) = UVAR(I,6) 
      ENDDO
      !!  From cauchy to PK2
       DO N=1,NPRONY
         II = 6*N  !  6 + 6*(N - 1)  
         DO I=1,NEL                                                  
             QINT(I,1,N) = UVAR(I,II + 1)                         
             QINT(I,2,N) = UVAR(I,II + 2)                         
             QINT(I,3,N) = UVAR(I,II + 3)                         
             QINT(I,4,N) = UVAR(I,II + 4)                         
             QINT(I,5,N) = UVAR(I,II + 5)                         
             QINT(I,6,N) = UVAR(I,II + 6)
             !!
             QINTLD(I,1,N) = EPSILD(N)*QINT(I,1,N) + BETAD(N)*STRS0(I,1)
             QINTLD(I,2,N) = EPSILD(N)*QINT(I,2,N) + BETAD(N)*STRS0(I,2)
             QINTLD(I,3,N) = EPSILD(N)*QINT(I,3,N) + BETAD(N)*STRS0(I,3)
             QINTLD(I,4,N) = EPSILD(N)*QINT(I,4,N) + BETAD(N)*STRS0(I,4)
             QINTLD(I,5,N) = EPSILD(N)*QINT(I,5,N) + BETAD(N)*STRS0(I,5)
             QINTLD(I,6,N) = EPSILD(N)*QINT(I,6,N) + BETAD(N)*STRS0(I,6)
          ENDDO
       ENDDO             
      DO I=1,NEL
            ! f-1*sig
            TEMP(I,1) = DEFGI(I,1)*STRS1(I,1) + DEFGI(I,4)*STRS1(I,4) +
     .                  DEFGI(I,7)*STRS1(I,6)
            TEMP(I,2) = DEFGI(I,2)*STRS1(I,1) + DEFGI(I,5)*STRS1(I,4) +
     .                  DEFGI(I,8)*STRS1(I,6)
            TEMP(I,3) = DEFGI(I,3)*STRS1(I,1) + DEFGI(I,6)*STRS1(I,4) +
     .                  DEFGI(I,9)*STRS1(I,6)
            TEMP(I,4) = DEFGI(I,1)*STRS1(I,4) + DEFGI(I,4)*STRS1(I,2) +
     .                  DEFGI(I,7)*STRS1(I,5)
            TEMP(I,5) = DEFGI(I,2)*STRS1(I,4) + DEFGI(I,5)*STRS1(I,2) +
     .                  DEFGI(I,8)*STRS1(I,5)
            TEMP(I,6) = DEFGI(I,3)*STRS1(I,4) + DEFGI(I,6)*STRS1(I,2) +
     .                  DEFGI(I,9)*STRS1(I,5)
            TEMP(I,7) = DEFGI(I,1)*STRS1(I,6) + DEFGI(I,4)*STRS1(I,5) +
     .                  DEFGI(I,7)*STRS1(I,3)
            TEMP(I,8) = DEFGI(I,2)*STRS1(I,6) + DEFGI(I,5)*STRS1(I,5) +
     .                  DEFGI(I,8)*STRS1(I,3)
            TEMP(I,9) = DEFGI(I,3)*STRS1(I,6) + DEFGI(I,6)*STRS1(I,5) +
     .                  DEFGI(I,9)*STRS1(I,3)
c
            STRS0(I,1) = JDEFG(I)*(TEMP(I,1)*DEFGI(I,1) + TEMP(I,4)*DEFGI(I,4) 
     .                                                  + TEMP(I,7)*DEFGI(I,7))
            STRS0(I,2) = JDEFG(I)*(TEMP(I,2)*DEFGI(I,2) + TEMP(I,5)*DEFGI(I,5) 
     .                                                  + TEMP(I,8)*DEFGI(I,8))
            STRS0(I,3) = JDEFG(I)*(TEMP(I,3)*DEFGI(I,3) + TEMP(I,6)*DEFGI(I,6) 
     .                                                  + TEMP(I,9)*DEFGI(I,9))
            STRS0(I,4) = JDEFG(I)*(TEMP(I,2)*DEFGI(I,1) + TEMP(I,5)*DEFGI(I,4) 
     .                                                  + TEMP(I,8)*DEFGI(I,7))
            STRS0(I,5) = JDEFG(I)*(TEMP(I,3)*DEFGI(I,2) + TEMP(I,6)*DEFGI(I,5) 
     .                                                  + TEMP(I,9)*DEFGI(I,8))
            STRS0(I,6) = JDEFG(I)*(TEMP(I,3)*DEFGI(I,1) + TEMP(I,6)*DEFGI(I,4) 
     .                                                  + TEMP(I,9)*DEFGI(I,7))
            UVAR(I,1) = STRS0(I,1)
            UVAR(I,2) = STRS0(I,2)
            UVAR(I,3) = STRS0(I,3)
            UVAR(I,4) = STRS0(I,4)
            UVAR(I,5) = STRS0(I,5)
            UVAR(I,6) = STRS0(I,6)
       ENDDO
       DO N = 1, NPRONY
          II = 6*N  ! 6 + 6*(N-1)   
          DO I = 1,NEL
            QINT(I,1,N) = QINTLD(I,1,N) + ALPHAD(N)*STRS0(I,1)
            QINT(I,2,N) = QINTLD(I,2,N) + ALPHAD(N)*STRS0(I,2)
            QINT(I,3,N) = QINTLD(I,3,N) + ALPHAD(N)*STRS0(I,3)
            QINT(I,4,N) = QINTLD(I,4,N) + ALPHAD(N)*STRS0(I,4)
            QINT(I,5,N) = QINTLD(I,5,N) + ALPHAD(N)*STRS0(I,5)
            QINT(I,6,N) = QINTLD(I,6,N) + ALPHAD(N)*STRS0(I,6)
            !!             
              UVAR(I,II + 1) = QINT(I,1,N)
              UVAR(I,II + 2) = QINT(I,2,N)
              UVAR(I,II + 3) = QINT(I,3,N)
              UVAR(I,II + 4) = QINT(I,4,N)
              UVAR(I,II + 5) = QINT(I,5,N)
              UVAR(I,II + 6) = QINT(I,6,N)
          ENDDO
        ENDDO
      !  total viscous stress 
          DO I = 1,NEL
            QBAR(I,1) = ZERO
            QBAR(I,2) = ZERO
            QBAR(I,3) = ZERO
            QBAR(I,4) = ZERO
            QBAR(I,5) = ZERO
            QBAR(I,6) = ZERO
          ENDDO 
c
          DO N = 1,NPRONY
            DO I = 1,NEL
              TEMP(I,1) = DEFG(I,1)*QINTLD(I,1,N) +
     .                    DEFG(I,4)*QINTLD(I,4,N) +
     .                    DEFG(I,7)*QINTLD(I,6,N)
              TEMP(I,2) = DEFG(I,2)*QINTLD(I,1,N) +
     .                    DEFG(I,5)*QINTLD(I,4,N) +
     .                    DEFG(I,8)*QINTLD(I,6,N)
              TEMP(I,3) = DEFG(I,3)*QINTLD(I,1,N) +
     .                    DEFG(I,6)*QINTLD(I,4,N) +
     .                    DEFG(I,9)*QINTLD(I,6,N)
              TEMP(I,4) = DEFG(I,1)*QINTLD(I,4,N) +
     .                    DEFG(I,4)*QINTLD(I,2,N) +
     .                    DEFG(I,7)*QINTLD(I,5,N)
              TEMP(I,5) = DEFG(I,2)*QINTLD(I,4,N) +
     .                    DEFG(I,5)*QINTLD(I,2,N) +
     .                    DEFG(I,8)*QINTLD(I,5,N)
              TEMP(I,6) = DEFG(I,3)*QINTLD(I,4,N) +
     .                    DEFG(I,6)*QINTLD(I,2,N) +
     .                    DEFG(I,9)*QINTLD(I,5,N)
              TEMP(I,7) = DEFG(I,1)*QINTLD(I,6,N) +
     .                    DEFG(I,4)*QINTLD(I,5,N) +
     .                    DEFG(I,7)*QINTLD(I,3,N)
              TEMP(I,8) = DEFG(I,2)*QINTLD(I,6,N) +
     .                    DEFG(I,5)*QINTLD(I,5,N) +
     .                    DEFG(I,8)*QINTLD(I,3,N)
              TEMP(I,9) = DEFG(I,3)*QINTLD(I,6,N) +
     .                    DEFG(I,6)*QINTLD(I,5,N) +
     .                    DEFG(I,9)*QINTLD(I,3,N)
c           
              TEMP6(I,1) = TEMP(I,1)*DEFG(I,1) +
     .                     TEMP(I,4)*DEFG(I,4) +
     .                     TEMP(I,7)*DEFG(I,7)
              TEMP6(I,2) = TEMP(I,2)*DEFG(I,2) +
     .                     TEMP(I,5)*DEFG(I,5) +
     .                     TEMP(I,8)*DEFG(I,8)
              TEMP6(I,3) = TEMP(I,3)*DEFG(I,3) +
     .                     TEMP(I,6)*DEFG(I,6) +
     .                     TEMP(I,9)*DEFG(I,9)
              TEMP6(I,4) = TEMP(I,2)*DEFG(I,1) +
     .                     TEMP(I,5)*DEFG(I,4) +
     .                     TEMP(I,8)*DEFG(I,7)
              TEMP6(I,5) = TEMP(I,3)*DEFG(I,2) +
     .                     TEMP(I,6)*DEFG(I,5) +
     .                     TEMP(I,9)*DEFG(I,8)
              TEMP6(I,6) = TEMP(I,3)*DEFG(I,1) +
     .                     TEMP(I,6)*DEFG(I,4) +
     .                     TEMP(I,9)*DEFG(I,7)
c
             QBAR(I,1) = QBAR(I,1) + TEMP6(I,1)
             QBAR(I,2) = QBAR(I,2) + TEMP6(I,2)
             QBAR(I,3) = QBAR(I,3) + TEMP6(I,3)
             QBAR(I,4) = QBAR(I,4) + TEMP6(I,4)
             QBAR(I,5) = QBAR(I,5) + TEMP6(I,5)
             QBAR(I,6) = QBAR(I,6) + TEMP6(I,6)
c
            ENDDO
          ENDDO
c
c         updated cauchy stress
c
          DO I = 1,NEL   
            STRS1(I,1) = GSTARD*STRS1(I,1) - QBAR(I,1)/JDEFG(I)
            STRS1(I,2) = GSTARD*STRS1(I,2) - QBAR(I,2)/JDEFG(I)
            STRS1(I,3) = GSTARD*STRS1(I,3) - QBAR(I,3)/JDEFG(I)
            STRS1(I,4) = GSTARD*STRS1(I,4) - QBAR(I,4)/JDEFG(I)
            STRS1(I,5) = GSTARD*STRS1(I,5) - QBAR(I,5)/JDEFG(I)
            STRS1(I,6) = GSTARD*STRS1(I,6) - QBAR(I,6)/JDEFG(I)
             !!
             S1(I) = STRS1(I,1)
             S2(I) = STRS1(I,2)
             S3(I) = STRS1(I,3)
             S4(I) = STRS1(I,4)
             S5(I) = STRS1(I,5)
             S6(I) = STRS1(I,6)
          ENDDO    
          IF(FLAG_VISC == 2) THEN
             DO I=1,NEL
               S1(I) = S1(I) + P(I)
               S2(I) = S2(I) + P(I)
               S3(I) = S3(I) + P(I)
             ENDDO 
          ENDIF                                                 
c------------
      RETURN
      END
