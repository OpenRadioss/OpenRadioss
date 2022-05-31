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
Chd|  FAIL_FLD_XFEM                 source/materials/fail/fld/fail_fld_xfem.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        FINTERFLD                     source/materials/fail/fld/fail_fld_c.F
Chd|====================================================================
      SUBROUTINE FAIL_FLD_XFEM(
     1     NEL      ,NUPARAM  ,NUVAR    ,NFUNC    ,IFUNC    ,
     2     NPF      ,TF       ,TIME     ,UPARAM   ,
     3     NGL      ,IPT      ,NPTOT    ,SSP      ,TENS     ,
     4     SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     5     EPSXX    ,EPSYY    ,EPSXY    ,EPSYZ    ,EPSZX    ,  
     6     UVAR     ,NOFF     ,OFF      ,IP       ,
     7     ELCRKINI ,IXFEM    ,IXEL     ,ILAY     ,IPTT     ,  
     8     OFFL     ,NPTT     ,NPTTF    ,DFMAX    ,TDEL     ,   
     9     DAM      ,FLD_IDX  )
C-----------------------------------------------
c    FLD failure model for XFEM
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
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C---------+---------+---+---+--------------------------------------------
C NFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW not used
C IFUNC   | NFUNC   | I | R | FUNCTION INDEX
C NPF     |  *      | I | R | FUNCTION ARRAY   
C TF      |  *      | F | R | FUNCTION ARRAY 
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C---------+---------+---+---+--------------------------------------------
C EPSXX   | NEL     | F | R | STRAIN XX
C EPSYY   | NEL     | F | R | STRAIN YY
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C ILAY                        CURRENT LAYER
C IPTT                        CURRENT INTEGRATION POINT IN THE LAYER (FOR OUTPUT ONLY)
C---------+---------+---+---+--------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NUPARAM,NUVAR,NFUNC,IP,IPT,IPTT,NPTOT,IXFEM,IXEL,ILAY,NPTT
      INTEGER ELCRKINI(NXLAYMAX,NEL),NOFF(NEL),IFUNC(NFUNC),NGL(NEL),FLD_IDX(NEL)
      my_real 
     .   TIME,UPARAM(NUPARAM),SIGNXX(NEL),SIGNYY(NEL),
     .   SIGNZZ(NEL),SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),SSP(NEL),
     .   EPSXX(NEL),EPSYY(NEL),EPSXY(NEL),EPSYZ(NEL),EPSZX(NEL),
     .   TENS(NEL,5),DFMAX(NEL),TDEL(NEL),NPTTF(NEL),DAM(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real UVAR(NEL,NUVAR),OFF(NEL),OFFL(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*)
      my_real FINTER , FINTERFLD ,TF(*)
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
      INTEGER I,II,J,K,L,IENG,IAD,NINDX,NINDXP,ISHELL,LAYXFEM,LENF
      INTEGER INDX(NEL),INDXP(NEL),RFLAG(NEL),RFLAGP(NEL)  
      my_real 
     .   RANI,R1,R2,DADV,SIGMAX,S1,S2,SS,Q,DYDX,E12
      my_real, ALLOCATABLE, DIMENSION(:) :: XF  
      my_real 
     .   TBA(NEL),TBK(NEL),SIGR(NEL),EMAJ(NEL),EMIN(NEL),
     .   EM(NEL),EM_DCRIT(NEL),EM_DADV(NEL)
      CHARACTER (LEN=3) :: XCHAR
c=======================================================================
      IAD = (IPT-1)*NEL
      XCHAR  = ' '
      DO I=1,NEL
        RFLAG(I) = 0
        RFLAGP(I)= 0
        INDXP(I) = 0
        INDX(I)  = 0
      ENDDO 
      NINDX  = 0  
      NINDXP = 0  
c---
      ISHELL  = INT(UPARAM(1))
      LAYXFEM = INT(UPARAM(4))   ! Ixfem flag of layer material
      DADV    = UPARAM(5)
      IENG    = INT(UPARAM(6))
      RANI    = UPARAM(7)
c
      IF (LAYXFEM == 0) RETURN
      LAYXFEM = IXFEM
      IF (LAYXFEM == 1 .and. ISHELL == 1) ISHELL=2
c
      DO I=1,NEL
        EM_DCRIT(I)= ZERO
        EM_DADV(I) = ZERO
      END DO
C
c --- Minor and major (true) strain deformation.
C
      DO I = 1,NEL
        E12= HALF*EPSXY(I)
        S1 = HALF*(EPSXX(I) + EPSYY(I))
        S2 = HALF*(EPSXX(I) - EPSYY(I))
        Q  = SQRT(S2**2 + E12**2)
        EMAJ(I) = S1 + Q
        EMIN(I) = S1 - Q
        IF (EMIN(I) >= EMAJ(I)) THEN
          SS      = EMIN(I)
          EMIN(I) = EMAJ(I)
          EMAJ(I) = SS
        ENDIF
      ENDDO
c-----
c     failure major strain from input curve and damage ratio  : 0 < DFMAX < 1 
c-----
      IF (IENG == 1) THEN   ! transform input fld curve to true strain
        II   = NPF(IFUNC(1))
        LENF = NPF(IFUNC(1)+ 1) - NPF(IFUNC(1))
        ALLOCATE(XF(LENF))
        DO I = 1,LENF
          XF(I) = LOG(TF(II + I-1) + ONE)
        ENDDO
c
        DO I = 1,NEL
          EM(I)    = FINTERFLD(EMIN(I),IFUNC(1),LENF,XF)
          DAM(I)   = EMAJ(I) / EM(I)
          DFMAX(I) = MIN(ONE, DAM(I))
        ENDDO
      ELSE
        DO I = 1,NEL
          EM(I)    = FINTER(IFUNC(1),EMIN(I),NPF,TF,DYDX)
          DAM(I)   = EMAJ(I) / EM(I)
          DFMAX(I) = MIN(ONE, DAM(I))
        ENDDO
      ENDIF 
c--------------------------------------------------------------------
c     FLD zone index calculation for ANIM output
c
      R1 = TWO*EM02
      R2 = RANI/(RANI+ONE)
      DO I = 1,NEL
        IF (EMAJ(I) >= EM(I)) THEN
          FLD_IDX(I) = 6      ! zone 6 = failure
        ELSEIF (EMAJ(I) >= EM(I) - EM01) THEN
          FLD_IDX(I) = 5      ! zone 5 = margin to fail
        ELSEIF (EMAJ(I)**2 + EMIN(I)**2 < R1**2) THEN
          FLD_IDX(I) = 1      ! zone 1 = radius 0.02
        ELSEIF (EMAJ(I) >= ABS(EMIN(I))) THEN
          FLD_IDX(I) = 4      ! zone 4 = safe (45 deg line)
        ELSEIF (EMAJ(I) >= R2*ABS(EMIN(I))) THEN
          FLD_IDX(I) = 3      ! zone 3  = angle atan(r/(1+r))  - compresion
        ELSE
          FLD_IDX(I) = 2      ! zone 2  - high wrinkle tendency
        ENDIF
      ENDDO
c--------------------------------------------------------------------
      DO I = 1,NEL
        TENS(I,1) = SIGNXX(I)
        TENS(I,2) = SIGNYY(I)
        TENS(I,3) = SIGNXY(I)
        TENS(I,4) = SIGNYZ(I)
        TENS(I,5) = SIGNZX(I)
        EM_DCRIT(I)= EM(I)
        IF (NFUNC > 1 .and. IFUNC(2) > 0) THEN
          EM_DADV(I) = DADV*FINTER(IFUNC(2),EMIN(I),NPF,TF,DYDX)
        ELSE
          EM_DADV(I) = DADV*EM_DCRIT(I)
        ENDIF
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
      ELSE
        XCHAR = 'standard'
      ENDIF
c--------------------------
      SELECT CASE (LAYXFEM)                           
c---------------
        CASE (1)    ! multilayer XFEM                                    
c---------------
            DO I = 1,NEL                                                          
              IF (OFF(I) == ONE) THEN                                              
                IF (UVAR(I,1) == ZERO) THEN                                       
                  IF (IXEL == 0) THEN                                             
                    IF (ELCRKINI(ILAY,I)==0 .and. EMAJ(I)>=EM_DCRIT(I)) THEN                                
                      NINDXP = NINDXP+1
                      INDXP(NINDXP) = I
                      UVAR(I,1) = ONE                                              
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
                      IF (NOFF(I) == NPTOT) THEN   
                        OFF(I) = FOUR_OVER_5                                           
                        TDEL(I)= TIME                                             
                      ENDIF                                                       
                    ELSEIF (ELCRKINI(ILAY,I)==2 .and. EMAJ(I)>=EM_DADV(I)) THEN                             
                      NINDXP   = NINDXP + 1
                      INDXP(NINDXP) = I
                      UVAR(I,1) = ONE                                              
                      OFFL(I)   = ZERO                                            
                      NOFF(I)   = NOFF(I) + 1                                        
                      NPTTF(I)  = NPTTF(I)+ ONE
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
                  ELSEIF (EMAJ(I) >= EM_DCRIT(I)) THEN  ! IXEL > 0                  
                    NINDXP    = NINDXP + 1
                    INDXP(NINDXP) = I
                    UVAR(I,1) = ONE                                                
                    OFFL(I)   = ZERO                                            
                    NPTTF(I)  = NPTTF(I) + ONE
                    RFLAGP(I) = 1
                    IF (INT(NPTTF(I)) == NPTT) THEN
                      NINDX = NINDX+1
                      INDX(NINDX) = I
                      OFF(I) = FOUR_OVER_5                                           
                      TDEL(I)  = TIME
                      RFLAG(I) = 3
                    ENDIF
                  ENDIF ! IXEL == 0                                                   
                ELSEIF (UVAR(I,1) == ONE) THEN                                     
                  SIGNXX(I) = ZERO                                                
                  SIGNYY(I) = ZERO                                                
                  SIGNXY(I) = ZERO                                                
                  SIGNYZ(I) = ZERO                                                
                  SIGNZX(I) = ZERO                                                
                ENDIF ! UVAR                                                      
              ENDIF   ! IF (OFF(I) == ONE)                                                                  
            ENDDO     ! DO I=1,NEL                                                                        
            IF (NINDXP > 0) THEN    
              DO J=1,NINDXP        
                I = INDXP(J)        
                SIGNXX(I) = ZERO                                              
                SIGNYY(I) = ZERO                                              
                SIGNXY(I) = ZERO                                              
                SIGNYZ(I) = ZERO                                              
                SIGNZX(I) = ZERO                                              
#include "lockon.inc"
                IF (RFLAGP(I) == 1) WRITE(IOUT, 3800)NGL(I),ILAY,IPTT
                IF (RFLAGP(I) == 1) WRITE(ISTDO,3900)NGL(I),ILAY,IPTT,TIME      
#include "lockoff.inc"
              ENDDO               
            ENDIF  !  NINDXP > 0   
            IF (NINDX > 0) THEN    
              DO J=1,NINDX         
                I = INDX(J)        
#include "lockon.inc"
c               initialization std element                                                
                IF (RFLAG(I) == 1) WRITE(IOUT ,3000)  NGL(I),ILAY    
                IF (RFLAG(I) == 1) WRITE(ISTDO,3100)  NGL(I),ILAY,TIME            
c               advancement std element                                                   
                IF (RFLAG(I) == -1) WRITE(IOUT ,3200) NGL(I),ILAY                
                IF (RFLAG(I) == -1) WRITE(ISTDO,3300) NGL(I),ILAY,TIME           
c               delete phantom element                                                       
                IF (RFLAG(I) == 3) WRITE(IOUT, 3400)  XCHAR,NGL(I),ILAY           
                IF (RFLAG(I) == 3) WRITE(ISTDO,3500)  XCHAR,NGL(I),ILAY,TIME      
#include "lockoff.inc"
              ENDDO               
            ENDIF  !  NINDX > 0   
c---------------
        CASE (2)    ! monolayer XFEM                                    
c---------------
            IF (ISHELL == 1) THEN   
c
              IF (LAYXFEM > 0) THEN
                DO I = 1,NEL
                  IF (OFF(I) == ONE) THEN
                    IF (IXEL == 0) THEN    ! not cut yet
                      IF (ELCRKINI(ILAY,I) == 0 .and. 
     .                    EMAJ(I) >= EM_DCRIT(I)) THEN
                        ELCRKINI(ILAY,I) = -1 ! ready to start       
                        OFF(I) = FOUR_OVER_5                                           
                        NINDX=NINDX+1
                        INDX(NINDX)=I
                        RFLAG(I) = 1
                        TDEL(I)= TIME
                      ELSEIF (ELCRKINI(ILAY,I) == 2 .and. 
     .                        EMAJ(I) >= EM_DADV(I)) THEN  
                        ELCRKINI(ILAY,I) = 1  ! ready to advance
                        OFF(I) = FOUR_OVER_5                                           
                        NINDX=NINDX+1
                        INDX(NINDX)=I
                        RFLAG(I) = -1
                        TDEL(I)= TIME
                      ENDIF
                    ELSE IF (EMAJ(I) >= EM_DCRIT(I)) THEN! IXEL > 0 
                      OFF(I) = FOUR_OVER_5                                           
                      NINDX=NINDX+1
                      INDX(NINDX)=I
                      RFLAG(I) = 3
                    ENDIF  ! IXEL
                  ENDIF
                ENDDO
              ENDIF  ! LAYXFEM
C
              IF (NINDX > 0) THEN
                DO J=1,NINDX
                  I=INDX(J)
#include "lockon.inc"
                  WRITE(IOUT, 4800)NGL(I),IPTT        
                  WRITE(ISTDO,4900)NGL(I),IPTT,TIME        
c                 initialization std element
                  IF (RFLAG(I) == 1)  WRITE(IOUT ,4000) NGL(I)
                  IF (RFLAG(I) == 1)  WRITE(ISTDO,4100) NGL(I),TIME
c                 advancement std element
                  IF (RFLAG(I) == -1) WRITE(IOUT, 4200) NGL(I)
                  IF (RFLAG(I) == -1) WRITE(ISTDO,4300) NGL(I),TIME
c                 delete phantom
                  IF (RFLAG(I) == 3)  WRITE(IOUT, 4400) XCHAR,NGL(I)
                  IF (RFLAG(I) == 3)  WRITE(ISTDO,4500) XCHAR,NGL(I),TIME
#include "lockoff.inc"
                ENDDO
              ENDIF
c
            ELSEIF (ISHELL == 2)THEN
c
              DO I = 1,NEL
                IF (OFF(I) == ONE) THEN
                  IF (UVAR(I,1) == ZERO) THEN
                    IF (IXEL == 0) THEN    ! testing standard element
                      IF (ELCRKINI(ILAY,I) == 0 .and. EMAJ(I) >= EM_DCRIT(I)) THEN
                        UVAR(I,1) = ONE
                        NINDX=NINDX+1
                        INDX(NINDX)=I
                        NINDXP = NINDXP + 1                                                 
                        INDXP(NINDXP) = I                                                 
                        RFLAGP(I) = 1
                        NOFF(I) = NOFF(I) + 1
                        IF (NOFF(I) == NPTOT) THEN
                          ELCRKINI(ILAY,I) = -1 ! one layer failed (by initiation)
                          OFF(I) = FOUR_OVER_5                                           
                          RFLAG(I) = 1
                          TDEL(I)= TIME 
                        ENDIF
                      ELSEIF (ELCRKINI(ILAY,I) == 2 .and. EMAJ(I) >= EM_DADV(I)) THEN
                        UVAR(I,1) = ONE
                        NINDX=NINDX+1
                        INDX(NINDX)=I
                        NINDXP = NINDXP + 1                                                 
                        INDXP(NINDXP) = I                                                 
                        RFLAGP(I) = 1
                        NOFF(I) = NOFF(I) + 1
                        IF (NOFF(I) == NPTOT) THEN
                          OFF(I) = FOUR_OVER_5                                           
                          ELCRKINI(ILAY,I) = 1 !  one layer failed (by advancing)
                          RFLAG(I) = -1
                          TDEL(I)= TIME
                        ENDIF
                      ENDIF
                    ELSEIF (EMAJ(I) >= EM_DCRIT(I)) THEN  ! IXEL > 0
                      UVAR(I,1) = ONE
                      NINDX = NINDX+1
                      INDX(NINDX) = I
                      NINDXP = NINDXP + 1                                                 
                      INDXP(NINDXP) = I                                                 
                      RFLAGP(I) = 1
                      NOFF(I) = NOFF(I) + 1
                      IF (NOFF(I) == NPTOT) THEN
                        OFF(I) = FOUR_OVER_5                                           
                        RFLAG(I) = 3
                      ENDIF
                    ENDIF ! IXEL 
                  ELSEIF (UVAR(I,1) == ONE) THEN
                    SIGNXX(I) = ZERO
                    SIGNYY(I) = ZERO
                    SIGNXY(I) = ZERO
                    SIGNYZ(I) = ZERO
                    SIGNZX(I) = ZERO
                  ENDIF ! UVAR
                ENDIF
              ENDDO
c
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
            ENDIF  !  NINDX > 0
          ENDIF    ! ISHELL
c-----------------
      END SELECT ! LAYXFEM
c
      IF (IENG == 1) DEALLOCATE(XF)
c-----------------------------------------------
 2000 FORMAT(1X,'FOR SHELL ELEMENT (FLD)',I10,1X,'LAYER',I3,':',/
     .       1X, 'STRESS TENSOR SET TO ZERO')
 2100 FORMAT(1X,'FOR SHELL ELEMENT (FLD)',I10,1X,'LAYER',I3,':',/,
     .       1X, 'STRESS TENSOR SET TO ZERO',1X,'AT TIME :',1PE20.13)
 2400 FORMAT(1X,1PG20.13,' % OF THICKNESS OF SHELL BROKEN ')
 2410 FORMAT(1X,1PG20.13,' % OF THICKNESS OF SHELL ',I10,' BROKEN ')
 2500 FORMAT(1X,'  LOWER SKIN -> UPPER SKIN ')
 2600 FORMAT(1X,'  UPPER SKIN -> LOWER SKIN ')
c---  multilayer xfem
 3000 FORMAT(1X,'CRACK INITIALIZATION IN SHELL ELEMENT',I10,1X,'LAYER',I3)
 3100 FORMAT(1X,'CRACK INITIALIZATION IN SHELL ELEMENT',I10,1X,'LAYER',I3,/
     .       1X,'AT TIME :',1PE12.4)
 3200 FORMAT(1X,'CRACK ADVANCEMENT IN SHELL ELEMENT',I10,' LAYER',I3)
 3300 FORMAT(1X,'CRACK ADVANCEMENT IN SHELL ELEMENT',I10,' LAYER',I3/
     .       1X,'AT TIME :',1PE12.4)
 3400 FORMAT(1X,'DELETE ',A4,' PHANTOM ELEMENT, SHELL ID=',I10,' LAYER',I3)
 3500 FORMAT(1X,'DELETE ',A4,' PHANTOM ELEMENT, SHELL ID=',I10,' LAYER',I3,/
     .       1X,'AT TIME :',1PE12.4)
 3800 FORMAT(1X,'FLD FAILURE IN SHELL',I10,1X,'LAYER',I3,1X,'INT POINT',I2)
 3900 FORMAT(1X,'FLD FAILURE IN SHELL',I10,1X,'LAYER',I3,1X,'INT POINT',I2,/
     .       1X,'AT TIME :',1PE12.4)
c---  monolayer xfem
 4000 FORMAT(1X,'CRACK INITIALIZATION IN SHELL ELEMENT',I10)
 4100 FORMAT(1X,'CRACK INITIALIZATION IN SHELL ELEMENT',I10,/
     .       1X,'AT TIME :',1PE12.4)
 4200 FORMAT(1X,'CRACK ADVANCEMENT IN SHELL ELEMENT',I10)
 4300 FORMAT(1X,'CRACK ADVANCEMENT IN SHELL ELEMENT',I10,/
     .       1X,'AT TIME :',1PE12.4)
 4400 FORMAT(1X,'DELETE ',A4,' PHANTOM ELEMENT, SHELL ID=',I10)
 4500 FORMAT(1X,'DELETE ',A4,' PHANTOM ELEMENT, SHELL ID=',I10,/
     .       1X,'AT TIME :',1PE12.4)
 4800 FORMAT(1X,'FLD FAILURE IN SHELL',I10,1X,'INT POINT',I2)
 4900 FORMAT(1X,'FLD FAILURE IN SHELL',I10,1X,'INT POINT',I2,1X,'AT TIME :',1PE12.4)
c-----------------------------------------------
      RETURN
      END
