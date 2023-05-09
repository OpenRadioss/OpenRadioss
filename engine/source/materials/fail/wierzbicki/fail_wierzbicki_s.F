Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  FAIL_WIERZBICKI_S             source/materials/fail/wierzbicki/fail_wierzbicki_s.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        MMAIN8                        source/materials/mat_share/mmain8.F
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FAIL_WIERZBICKI_S(
     1     NEL    ,NUPARAM,NUVAR   ,MFUNC   ,KFUNC   ,
     2     NPF    ,TF     ,TIME   ,TIMESTEP ,UPARAM  ,
     3     NGL    ,IPM    ,MAT,
     4     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     5     DPLA   ,PLA    ,UVAR    ,OFF     ,DFMAX   ,
     6     TDELE  )
C-----------------------------------------------
C    Xue-Wierbzcki
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF FAILURE ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C MFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW not used
C KFUNC   | NFUNC   | I | R | FUNCTION INDEX not used
C NPF     |  *      | I | R | FUNCTION ARRAY   
C TF      |  *      | F | R | FUNCTION ARRAY 
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER FAILURE PARAMETER ARRAY
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
#include "mvsiz_p.inc"
#include "scr17_c.inc"
#include "units_c.inc"
#include  "comlock.inc"
#include  "param_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER NEL, NUPARAM, NUVAR,NGL(NEL),IPM(NPROPMI,*),
     .        MAT(NEL)
      my_real TIME,TIMESTEP,UPARAM(*),
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .   SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .   DPLA(NEL),PLA(NEL) 
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
cc      my_real
 
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real UVAR(NEL,NUVAR), OFF(NEL),DFMAX(NEL),TDELE(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), MFUNC, KFUNC(MFUNC)
      my_real TF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IDEL,IDEV,IFLAG,INDX(MVSIZ),IADBUF,NINDX,
     .        NINDEX,INDEX(MVSIZ),IFAIL,JJ,IMOY
      my_real 
     .        C1,C2,C3,C4,CM,
     .        CN,P,EPSF,SVM,J3,ETA,XI,SXX,SYY,SZZ,DELTA,
     .        XI_OLD, ETA_OLD,PLA_OLD,EPSF_OLD
C--------------------------------------------------------------
      C1 = UPARAM(1)
      C2 = UPARAM(2)
      C3 = UPARAM(3)
      C4 = UPARAM(4)
      CM = UPARAM(5)
      CN = UPARAM(6)
      IFLAG = INT(UPARAM(8))
      IMOY  = INT( UPARAM(9))
Cc
      IDEL=0
      IDEV=0
      IF(IFLAG==1)THEN
         IDEL=1
      ELSEIF(IFLAG==2)THEN
         IDEV =1
      END IF
C...
      IF(IDEL==1)THEN
        DO I=1,NEL
          IF(OFF(I)<0.1) OFF(I)=0.0
          IF(OFF(I)<1.0) OFF(I)=OFF(I)*0.8
        END DO
      END IF
C      
      IF(IDEL==1)THEN
       NINDX=0  
       DO I=1,NEL
        IF(IFLAG==1.AND.OFF(I)==1.AND.DPLA(I)/=ZERO)THEN
         P = THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
         SXX = SIGNXX(I) - P
         SYY = SIGNYY(I) - P
         SZZ = SIGNZZ(I) - P
C  Vmises stress
         SVM =HALF*(SXX**2 + SYY**2 + SZZ**2)
     .          +SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2
         SVM=SQRT(THREE*SVM)
C third invariant
         J3 = SXX*SYY*SZZ + TWO*SIGNXY(I)*SIGNZX(I)*SIGNYZ(I) 
     .                    -  SYY*SIGNZX(I)**2  - SXX*SIGNYZ(I)**2
     .                    -  SZZ*SIGNXY(I)**2
          ETA = P / MAX(EM20,SVM)
          XI  = HALF*(TWENTY + SEVEN)*J3/MAX(EM20,SVM**3)
c
          IF(IMOY==1)THEN 
           UVAR(I,1) = UVAR(I,1) + ETA*DPLA(I)
           UVAR(I,2) = UVAR(I,2) +  XI*DPLA(I)
           ETA = UVAR(I,1)/MAX(EM20,PLA(I))
           XI  = UVAR(I,2)/MAX(EM20,PLA(I))
          ENDIF                              
c  
          EPSF = (C1*EXP(-C2*ETA))**CN
     .        - (C3*EXP(-C4*ETA))**CN
           EPSF = EPSF * MAX(ZERO,(ONE - XI**CM))**(ONE/CM)
          EPSF = ((C1*EXP(-C2*ETA))**CN - EPSF)**(ONE/CN)
         IF(EPSF>ZERO)
     .     DFMAX(I) = DFMAX(I) + DPLA(I)*(ONE/EPSF)
         IF(DFMAX(I)>=ONE.AND.OFF(I)==ONE) THEN
          OFF(I)=FOUR_OVER_5
          NINDX=NINDX+1
          INDX(NINDX)=I
          IDEL7NOK = 1 
          TDELE(I) = TIME  
         ENDIF
        ENDIF 
       ENDDO
       IF(NINDX>0.AND.IMCONV==1)THEN
        DO J=1,NINDX
#include "lockon.inc"
         WRITE(IOUT, 1000) NGL(INDX(J))
         WRITE(ISTDO,1100) NGL(INDX(J)),TIME
#include "lockoff.inc"
        END DO
       END IF         
      ENDIF
Cc deviatoric will vanish     
      IF(IDEV==1)THEN
       NINDX=0 
       NINDEX = 0 
       DO I=1,NEL
        IF(IFLAG==2.AND.OFF(I)==ONE)THEN 
         IF(DFMAX(I)<ONE)THEN
         P = THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
         SXX = SIGNXX(I) - P
         SYY = SIGNYY(I) - P
         SZZ = SIGNZZ(I) - P
C  Vmises stress
         SVM =HALF*(SXX**2 + SYY**2 + SZZ**2)
     .          +SIGNXY(I)**2 + SIGNZX(I)**2 + SIGNYZ(I)**2
         SVM=SQRT(THREE*SVM)
C third invariant
         J3 = SXX*SYY*SZZ + TWO*SIGNXY(I)*SIGNZX(I)*SIGNYZ(I) 
     .                    -  SYY*SIGNZX(I)**2  - SXX*SIGNYZ(I)**2
     .                    -  SZZ*SIGNXY(I)**2
         ETA = P / MAX(EM20,SVM)
         XI  = HALF*(TWENTY + SEVEN)*J3/MAX(EM20,SVM**3)
C average valeur
          IF(IMOY==1)THEN
           UVAR(I,1) = UVAR(I,1) + ETA*DPLA(I)
           UVAR(I,2) = UVAR(I,2) +  XI*DPLA(I)
           ETA = UVAR(I,1)/MAX(EM20,PLA(I))
           XI  = UVAR(I,2)/MAX(EM20,PLA(I))
          ENDIF
CC                    
         EPSF = (C1*EXP(-C2*ETA))**CN
     .        - (C3*EXP(-C4*ETA))**CN
         EPSF = EPSF * MAX(ZERO,(ONE - XI**CM))**(ONE/CM)
         EPSF = ((C1*EXP(-C2*ETA))**CN - EPSF)**(ONE/CN)
          IF(EPSF>ZERO) DFMAX(I) = DFMAX(I) + DPLA(I)/EPSF
          IF(DFMAX(I)>=ONE.AND.OFF(I)==ONE) THEN
           NINDX=NINDX+1
           INDX(NINDX)=I
           SIGNXX(I) =   P
           SIGNYY(I) =   P
           SIGNZZ(I) =   P
           SIGNXY(I) = ZERO
           SIGNYZ(I) = ZERO
           SIGNZX(I) = ZERO                 
          ENDIF
c UVAR> 1          
         ELSE 
          P = THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))
          SIGNXX(I) =   P
          SIGNYY(I) =   P
          SIGNZZ(I) =   P
          SIGNXY(I) = ZERO
          SIGNYZ(I) = ZERO
          SIGNZX(I) = ZERO
         ENDIF
        ENDIF  
       ENDDO
       IF(NINDX>0.AND.IMCONV==1)THEN
        DO J=1,NINDX
         I = INDX(J)
#include "lockon.inc"
         WRITE(IOUT, 2000) NGL(I)
         WRITE(ISTDO,2100) NGL(I),TIME
#include "lockoff.inc"
        END DO
       END IF           
      ENDIF      
C-------------Maximum Damage storing for output : 0 < DFMAX < 1--------------
       DO I=1,NEL
          DFMAX(I)= MIN(ONE,DFMAX(I))
       ENDDO
C-----------------------------------------------
 1000 FORMAT(1X,'DELETE SOLID ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'DELETE SOLID ELEMENT NUMBER ',I10,
     .          ' AT TIME :',1PE20.13)
C     
 2000 FORMAT(1X,' DEVIATORIC STRESS WILL BE VANISHED',I10)
 2100 FORMAT(1X,' DEVIATORIC STRESS WILL BE VANISHED',I10,
     .          ' AT TIME :',1PE20.13)
      RETURN
      END
