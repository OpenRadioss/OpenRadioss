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
Chd|  FAIL_JOHNSON_XFEM             source/materials/fail/johnson_cook/fail_johnson_xfem.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE FAIL_JOHNSON_XFEM(
     1           NEL      ,NUPARAM  ,UPARAM   ,NUVAR    ,UVAR     ,
     2           TIME     ,TENS     ,DPLA     ,EPSP     ,TSTAR    ,
     3           NGL      ,IPT      ,NPTOT    ,NPTT     ,NPTTF    ,
     4           SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     5           OFF      ,OFFL     ,NOFF     ,DFMAX    ,TDEL     ,
     6           ELCRKINI ,IXFEM    ,IXEL     ,ILAY     ,IPTT     )
C-----------------------------------------------
c    Johnson cook failure model
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include  "units_c.inc"
#include  "comlock.inc"
#include  "com_xfem1.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C NPTOT   |  1      | I | R | NUMBER OF LAYERS OR INTEGRATION POINTS   
C IPT     |  1      | I | R | LAYER OR INTEGRATION POINT NUMBER   
C IFLAG   |  *      | I | R | GEOMETRICAL FLAGS   
C NGL     | NEL     | I | R | ELEMEMT NUMBER
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C---------+---------+---+---+--------------------------------------------
C PLA     | NEL     | F |R/W| PLASTIC STRAIN
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C ILAY                        CURRENT LAYER
C IPT                         CURRENT INTEGRATION POINT IN ALL LAYERS
C IPTT                        CURRENT INTEGRATION POINT IN THE LAYER (FOR OUTPUT ONLY)
C NPTT                        NUMBER OF INTEGRATION POINTS IN THE LAYER THICKNESS
C NPTTF                       NUMBER OF FAILED INTEGRATION POINTS IN THE LAYER
C NPTOT                       NUMBER OF INTEGRATION POINTS IN ALL LAYERS (TOTAL)
C NOFF                        NUMBER OF FAILED INTEGRATION POINTS (TOTAL)
C---------+---------+---+---+--------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL, NUPARAM, NUVAR,IXFEM,IXEL,ILAY,NPTT, 
     .   IPT,IPTT,NPTOT
      INTEGER NGL(NEL),ELCRKINI(NXLAYMAX,*),NOFF(NEL)
      my_real TIME,UPARAM(*),DPLA(NEL),NPTTF(NEL),
     .   EPSP(NEL),TSTAR(NEL),TENS(NEL,5)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real UVAR(NEL,NUVAR),OFF(NEL),OFFL(NEL),DFMAX(NEL),TDEL(NEL),
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,JJ,ISHELL,NINDX,NINDXP,IADR,LAYXFEM
      INTEGER INDX(NEL),INDXP(NEL),RFLAG(NEL),RFLAGP(NEL)
      my_real D1,D2,D3,D4,D5,EPSP0,P,EPSF,SVM,THK_TMP,
     .    DADV,CC,BB,CR,ORM,SS1,SS2
      CHARACTER (LEN=3) :: XCHAR
c=======================================================================
      IADR = (IPT-1)*NEL
      XCHAR = ' '
c
      D1      = UPARAM(1)
      D2      = UPARAM(2)
      D3      = UPARAM(3)
      D4      = UPARAM(4)
      D5      = UPARAM(5)
      EPSP0   = UPARAM(6)
      DADV    = UPARAM(10)
      ISHELL  = INT(UPARAM(7))
      LAYXFEM = INT(UPARAM(11))
c-----------------------------
      IF (LAYXFEM == 0) RETURN
c-----------------------------
      LAYXFEM = IXFEM
      IF (LAYXFEM == 1 .and. ISHELL == 1) ISHELL=2
c-----------------------------
      NINDX  = 0  
      NINDXP = 0  
      DO I=1,NEL
        RFLAG(I) = 0
        RFLAGP(I)= 0
        INDXP(I) = 0
        INDX(I)  = 0
      ENDDO 
C---------
      DO I=1,NEL
        TENS(I,1) = SIGNXX(I)
        TENS(I,2) = SIGNYY(I)
        TENS(I,3) = SIGNXY(I)
        TENS(I,4) = SIGNYZ(I)
        TENS(I,5) = SIGNZX(I)
      END DO
C
      IF (IXEL > 0) THEN  ! testing phantom elements
        IF (IXEL == 1) THEN
          XCHAR = '1st'
        ELSEIF (IXEL == 2) THEN
          XCHAR = '2nd'
        ELSEIF (IXEL == 3) THEN
          XCHAR = '3rd'
        ENDIF
      ENDIF
C-------------------------------------------------------------------
      SELECT CASE (LAYXFEM)                           
c---------------
        CASE (1)    ! multilayer Xfem     (with Ishell = 2)                               
C---------------
          DO I=1,NEL                                                            
            IF (OFF(I) == ONE) THEN                                              
              IF (DFMAX(I) < ONE) THEN                                           
                P   = THIRD*(SIGNXX(I) + SIGNYY(I))                               
                SVM = SQRT(SIGNXX(I)*SIGNXX(I)                                  
     .                   + SIGNYY(I)*SIGNYY(I)                                  
     .                   - SIGNXX(I)*SIGNYY(I)                                  
     .                   + THREE*SIGNXY(I)*SIGNXY(I))                           
                EPSF = D3*P/MAX(EM20,SVM)                                       
                EPSF = (D1 + D2*EXP(EPSF))*(ONE                                       
     .                     + D4*LOG(MAX(ONE,EPSP(I)/EPSP0)))                   
     .                     * (ONE + D5*TSTAR(I))                                
                IF (EPSF > ZERO) DFMAX(I) = DFMAX(I) + DPLA(I)/EPSF              
c                                                                               
                IF (IXEL == 0) THEN                                             
                  IF (ELCRKINI(ILAY,I)==0 .AND. DFMAX(I) >= ONE) THEN
                    NINDXP = NINDXP+1
                    INDXP(NINDXP) = I
                    OFFL(I)   = ZERO                                            
                    NOFF(I)   = NOFF(I) + 1                                        
                    NPTTF(I)  = NPTTF(I) + ONE
                    RFLAGP(I) = 1
                    IF (INT(NPTTF(I)) == NPTT) THEN
                      NINDX = NINDX+1
                      INDX(NINDX) = I
                      ELCRKINI(ILAY,I) = -1 ! one layer failed (by initiation)
                      RFLAG(I) = 1                                               
                    ENDIF
                    IF (NOFF(I) == NPTOT) THEN  ! total element rupture 
                      OFF(I) = FOUR_OVER_5                                           
                      TDEL(I)= TIME                                             
                    ENDIF                                                       
                  ELSEIF (ELCRKINI(ILAY,I) == 2 .AND. DFMAX(I) >= DADV) THEN
                    NINDXP   = NINDXP + 1
                    INDXP(NINDXP) = I
                    OFFL(I)  = ZERO                                            
                    NOFF(I)  = NOFF(I) + 1                                        
                    NPTTF(I) = NPTTF(I)+ ONE
                    RFLAGP(I) = 1
                    IF (INT(NPTTF(I)) == NPTT) THEN
                      NINDX = NINDX+1
                      INDX(NINDX) = I
                      ELCRKINI(ILAY,I) = 1 !  one layer failed (by advancing)
                      RFLAG(I) = -1
                    ENDIF
                    IF (NOFF(I) == NPTOT) THEN
                      OFF(I) = FOUR_OVER_5                                           
                      TDEL(I)= TIME
                    ENDIF
                  ENDIF                                                       
                ELSEIF (DFMAX(I) >= ONE) THEN  ! phantom element failure (IXEL > 0)                        
                  NINDXP = NINDXP + 1
                  INDXP(NINDXP) = I
                  OFFL(I)  = ZERO                                            
                  NPTTF(I) = NPTTF(I) + ONE
                  RFLAGP(I) = 1
                  IF (INT(NPTTF(I)) == NPTT) THEN
                    NINDX = NINDX+1
                    INDX(NINDX) = I
                    OFF(I) = FOUR_OVER_5                                           
                    TDEL(I)  = TIME
                    RFLAG(I) = 3
                  ENDIF
                ENDIF  ! IXEL                                                   
              ELSE     ! DFMAX(I) >= ONE                                              
                SIGNXX(I) = ZERO                                                
                SIGNYY(I) = ZERO                                                
                SIGNXY(I) = ZERO                                                
                SIGNYZ(I) = ZERO                                                
                SIGNZX(I) = ZERO                                                
              ENDIF                                                             
            ENDIF  ! IF (OFF(I) == ONE)                                          
          ENDDO    ! DO I=1,NEL                                                 
c
          IF (NINDXP > 0) THEN                                             
            DO J=1,NINDXP                                                  
              I = INDXP(J)                                                 
              SIGNXX(I) = ZERO                                                
              SIGNYY(I) = ZERO                                                
              SIGNXY(I) = ZERO                                                
              SIGNYZ(I) = ZERO                                                
              SIGNZX(I) = ZERO                                                
#include      "lockon.inc"                                                
              IF (RFLAGP(I) == 1) WRITE(IOUT, 3800)NGL(I),ILAY,IPTT        
              IF (RFLAGP(I) == 1) WRITE(ISTDO,3900)NGL(I),ILAY,IPTT,TIME        
#include     "lockoff.inc"                                                
            ENDDO                                                          
          ENDIF  !  NINDXP > 0                                              
          IF (NINDX > 0) THEN      
            DO J=1,NINDX           
              I = INDX(J)          
#include      "lockon.inc"
c             initialization std element                                                
              IF (RFLAG(I) == 1) WRITE(IOUT ,3000) NGL(I),ILAY    
              IF (RFLAG(I) == 1) WRITE(ISTDO,3100) NGL(I),ILAY,TIME            
c             advancement std element                                                   
              IF (RFLAG(I) == -1) WRITE(IOUT ,3200) NGL(I),ILAY                
              IF (RFLAG(I) == -1) WRITE(ISTDO,3300) NGL(I),ILAY,TIME           
c             delete phantom element                                                       
              IF (RFLAG(I) == 3) WRITE(IOUT, 3400)XCHAR,NGL(I),ILAY           
              IF (RFLAG(I) == 3) WRITE(ISTDO,3500)XCHAR,NGL(I),ILAY,TIME      
#include      "lockoff.inc"
            ENDDO               
          ENDIF  !  NINDX > 0   
c
C---------------
        CASE (2)    ! monolayer Xfem                                    
C---------------
          IF (ISHELL == 1) THEN
c
            DO I=1,NEL                                               
              IF (OFF(I) == ONE) THEN                                 
                P   = THIRD*(SIGNXX(I) + SIGNYY(I))                    
                SVM = SQRT(SIGNXX(I)*SIGNXX(I)                       
     .                   + SIGNYY(I)*SIGNYY(I)                       
     .                   - SIGNXX(I)*SIGNYY(I)                       
     .                   + THREE*SIGNXY(I)*SIGNXY(I))                
                EPSF = D3*P/MAX(EM20,SVM)                            
                EPSF = (D1 + D2*EXP(EPSF))*(ONE                       
     .                     + D4*LOG(MAX(ONE,EPSP(I)/EPSP0)))       
     .                     * (ONE + D5*TSTAR(I))                    
                IF (EPSF > ZERO) DFMAX(I) = DFMAX(I) + DPLA(I)/EPSF  
c
                IF (IXEL == 0) THEN                                  
                  IF (ELCRKINI(ILAY,I)==0 .AND. DFMAX(I)>=ONE) THEN   
                    ELCRKINI(ILAY,I) = -1   ! Initialisation         
                    NINDX = NINDX + 1                                    
                    INDX(NINDX) = I                                    
                    RFLAG(I)  = 1                                     
                    TDEL(I)= TIME                                    
                    OFF(I) = FOUR_OVER_5                                           
                  ELSEIF (ELCRKINI(ILAY,I) == 2 .AND.                
     .                    DFMAX(I) >= DADV) THEN                     
                    ELCRKINI(ILAY,I) = 1    ! Advancement            
                    NINDX = NINDX + 1                                    
                    INDX(NINDX) = I                                    
                    RFLAG(I)  =-1                                    
                    TDEL(I)= TIME                                    
                    OFF(I) = FOUR_OVER_5                                           
                  ENDIF                                              
                ELSEIF (DFMAX(I) >= ONE) THEN  ! IXEL > 0             
                  NINDX = NINDX+1                                      
                  INDX(NINDX) = I                                      
                  RFLAG(I) = 3                                       
                  OFF(I) = FOUR_OVER_5                                           
                ENDIF ! IXEL                                         
              ENDIF                                                  
            ENDDO     ! I=1,NEL                                      

            IF (NINDX > 0) THEN
              DO J=1,NINDX
                I = INDX(J)
#include "lockon.inc"
                WRITE(IOUT, 4800)NGL(I),IPTT        
                WRITE(ISTDO,4900)NGL(I),IPTT,TIME        
c               initialization std element
                IF (RFLAG(I) == 1) WRITE(IOUT, 4000) NGL(I)
                IF (RFLAG(I) == 1) WRITE(ISTDO,4100) NGL(I),TIME
c               advancement std element
                IF (RFLAG(I) ==-1) WRITE(IOUT, 4200) NGL(I)
                IF (RFLAG(I) ==-1) WRITE(ISTDO,4300) NGL(I),TIME
c               delete phantom
                IF (RFLAG(I) == 3) WRITE(IOUT, 4400) XCHAR,NGL(I)
                IF (RFLAG(I) == 3) WRITE(ISTDO,4500) XCHAR,NGL(I),TIME
#include "lockoff.inc"
              ENDDO
            ENDIF
c
          ELSEIF (ISHELL == 2) THEN  ! monolayer Xfem
c
            DO I=1,NEL                                                            
              IF (OFF(I) == ONE) THEN                            
                IF (DFMAX(I) < ONE) THEN                                           
                  P = THIRD*(SIGNXX(I) + SIGNYY(I))                               
                  SVM = SQRT(SIGNXX(I)*SIGNXX(I)                                  
     .                     + SIGNYY(I)*SIGNYY(I)                                  
     .                     - SIGNXX(I)*SIGNYY(I)                                  
     .                     + THREE*SIGNXY(I)*SIGNXY(I))                           
                  EPSF = D3*P/MAX(EM20,SVM)                                       
                  EPSF = (D1 + D2*EXP(EPSF))*(ONE                                       
     .                       + D4*LOG(MAX(ONE,EPSP(I)/EPSP0)))                   
     .                       * (ONE + D5*TSTAR(I))                                
                  IF (EPSF > ZERO) DFMAX(I) = DFMAX(I) + DPLA(I)/EPSF
c                                                                                 
                  IF (IXEL == 0) THEN                                             
                    IF (ELCRKINI(ILAY,I)==0 .and. DFMAX(I) >= ONE) THEN              
                      NINDXP = NINDXP + 1                                                 
                      INDXP(NINDXP) = I                                                 
                      NOFF(I) = NOFF(I) + 1                                       
                      OFFL(I) = ZERO                                     
                      RFLAGP(I) = 1
                      IF (NOFF(I) == NPTOT) THEN  ! all integration points failed                                
                        NINDX = NINDX + 1                                               
                        INDX(NINDX) = I                                               
                        ELCRKINI(ILAY,I) = -1     ! initialize crack  
                        RFLAG(I) = 1                                              
                        TDEL(I)= TIME                                             
                        OFF(I) = FOUR_OVER_5                                           
                      ENDIF                                                       
                    ELSEIF (ELCRKINI(ILAY,I) == 2 .AND. DFMAX(I) >= DADV) THEN                                
                      NINDXP = NINDXP + 1                                                 
                      INDXP(NINDXP) = I                                                 
                      NOFF(I) = NOFF(I) + 1                                       
                      OFFL(I) = ZERO
                      DFMAX(I) = ONE                               
                      RFLAGP(I) = 1
                      IF (NOFF(I) == NPTOT) THEN  ! all integration points failed                                  
                        NINDX = NINDX + 1                                               
                        INDX(NINDX) = I                                               
                        ELCRKINI(ILAY,I) = 1      ! advance crack  
                        RFLAG(I) = -1                                             
                        TDEL(I)= TIME                                             
                        OFF(I) = FOUR_OVER_5                                           
                      ENDIF                                                       
                    ENDIF                                                         
                  ELSEIF (DFMAX(I) >= ONE) THEN  ! phantom element failure (IXEL > 0)                   
                    NINDXP = NINDXP + 1                                                 
                    INDXP(NINDXP) = I                                                 
                    NOFF(I) = NOFF(I) + 1                                         
                    RFLAGP(I) = 1
                    IF (NOFF(I) == NPTOT) THEN                                    
                      NINDX  = NINDX + 1                                                 
                      INDX(NINDX)   = I                                                 
                      OFF(I) = FOUR_OVER_5                                           
                      RFLAG(I) = 3                                                
                    ENDIF                                                         
                  ENDIF  ! IXEL                                                   
                ELSE     ! DFMAX >= 1                                                 
                  SIGNXX(I) = ZERO                                                
                  SIGNYY(I) = ZERO                                                
                  SIGNXY(I) = ZERO                                                
                  SIGNYZ(I) = ZERO                                                
                  SIGNZX(I) = ZERO                                                
                ENDIF                                                             
              ENDIF  ! IF (OFF(I) == ONE)                           
            ENDDO    ! DO I=1,NEL                                                 
C
            IF (NINDXP > 0) THEN                                             
              DO J=1,NINDXP                                                  
                I = INDXP(J)                                                 
                SIGNXX(I) = ZERO                                                
                SIGNYY(I) = ZERO                                                
                SIGNXY(I) = ZERO                                                
                SIGNYZ(I) = ZERO                                                
                SIGNZX(I) = ZERO                                                
#include        "lockon.inc"                                                
                IF (RFLAGP(I) == 1) WRITE(IOUT, 4800)NGL(I),IPTT        
                IF (RFLAGP(I) == 1) WRITE(ISTDO,4900)NGL(I),IPTT,TIME        
#include       "lockoff.inc"                                                
              ENDDO                                                          
            ENDIF  !  NINDXP > 0                                              
            IF (NINDX > 0) THEN
              DO J=1,NINDX
                I = INDX(J)
#include "lockon.inc"
c               initialization std element
                IF (RFLAG(I) == 1)  WRITE(IOUT ,4000) NGL(I)
                IF (RFLAG(I) == 1)  WRITE(ISTDO,4100) NGL(I),TIME
c               advancement std element
                IF (RFLAG(I) == -1) WRITE(IOUT, 4200) NGL(I)
                IF (RFLAG(I) == -1) WRITE(ISTDO,4300) NGL(I),TIME
c               delete phantom
                IF (RFLAG(I) == 3)  WRITE(IOUT, 4400) XCHAR,NGL(I)
                IF (RFLAG(I) == 3)  WRITE(ISTDO,4500) XCHAR,NGL(I),TIME
#include "lockoff.inc"
              ENDDO
            ENDIF  ! NINDX > 0

          ENDIF    ! ISHELL
c-----------------
      END SELECT ! LAYXFEM
c
C---------Damage for output  0 < DFMAX < 1 --------------------
      DO I=1,NEL
        DFMAX(I) = MIN(ONE,DFMAX(I))
      ENDDO
C------------------
 2000 FORMAT(1X,'FOR SHELL ELEMENT (JOHNS)',I10,1X,'LAYER',I3,':',/
     .       1X, 'STRESS TENSOR SET TO ZERO')
 2100 FORMAT(1X,'FOR SHELL ELEMENT (JOHNS)',I10,1X,'LAYER',I3,':',/,
     .       1X,'STRESS TENSOR SET TO ZERO',1X,'AT TIME :',1PE12.4)
 2400 FORMAT(1X,1PG20.13,' % OF THICKNESS OF SHELL BROKEN ')
 2410 FORMAT(1X,1PG20.13,' % OF THICKNESS OF SHELL ',I10,' BROKEN ')
 2500 FORMAT(1X,'  LOWER SKIN -> UPPER SKIN ')
 2600 FORMAT(1X,'  UPPER SKIN -> LOWER SKIN ')
c---  multilayer
 3000 FORMAT(1X,'CRACK INITIALIZATION IN SHELL ELEMENT',I10,1X,'LAYER',I3)
 3100 FORMAT(1X,'CRACK INITIALIZATION IN SHELL ELEMENT',I10,1X,'LAYER',I3,/
     .       1X,'AT TIME :',1PE12.4)
 3200 FORMAT(1X,'CRACK ADVANCEMENT IN SHELL ELEMENT',I10,' LAYER',I3)
 3300 FORMAT(1X,'CRACK ADVANCEMENT IN SHELL ELEMENT',I10,' LAYER',I3/
     .       1X,'AT TIME :',1PE12.4)
 3400 FORMAT(1X,'DELETE ',A4,' PHANTOM ELEMENT, SHELL ID=',I10,' LAYER',I3)
 3500 FORMAT(1X,'DELETE ',A4,' PHANTOM ELEMENT, SHELL ID=',I10,' LAYER',I3,/
     .       1X,'AT TIME :',1PE12.4)
 3800 FORMAT(1X,'JC FAILURE IN SHELL',I10,1X,'LAYER',I3,1X,'INT POINT',I2)
 3900 FORMAT(1X,'JC FAILURE IN SHELL',I10,1X,'LAYER',I3,1X,'INT POINT',I2,/
     .       1X,'AT TIME :',1PE12.4)
c---  monolayer
 4000 FORMAT(1X,'CRACK INITIALIZATION IN SHELL ELEMENT',I10)
 4100 FORMAT(1X,'CRACK INITIALIZATION IN SHELL ELEMENT',I10,1X,'AT TIME :',1PE12.4)
 4200 FORMAT(1X,'CRACK ADVANCEMENT IN SHELL ELEMENT',I10)
 4300 FORMAT(1X,'CRACK ADVANCEMENT IN SHELL ELEMENT',I10,1X,'AT TIME :',1PE12.4)
 4400 FORMAT(1X,'DELETE ',A4,' PHANTOM ELEMENT, SHELL ID=',I10)
 4500 FORMAT(1X,'DELETE ',A4,' PHANTOM ELEMENT, SHELL ID=',I10,1X,'AT TIME :',1PE12.4)
 4800 FORMAT(1X,'JC FAILURE IN SHELL',I10,1X,'INT POINT',I2)
 4900 FORMAT(1X,'JC FAILURE IN SHELL',I10,1X,'INT POINT',I2,1X,'AT TIME :',1PE12.4)
c------------------
      RETURN
      END
