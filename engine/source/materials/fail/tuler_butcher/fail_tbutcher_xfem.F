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
Chd|  FAIL_TBUTCHER_XFEM            source/materials/fail/tuler_butcher/fail_tbutcher_xfem.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FAIL_TBUTCHER_XFEM(
     1     NEL      ,NUPARAM  ,UPARAM   ,NUVAR    ,UVAR     ,
     2     TIME     ,TIMESTEP ,TENS     ,DFMAX    ,TDEL     ,
     3     SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     4     NGL      ,IPT      ,NPTOT    ,
     5     NOFF     ,OFF      ,OFFL     ,ELCRKINI ,IXFEM    ,
     6     IXEL     ,ILAY     ,IPTT     ,NPTT     ,NPTTF    )
C---------+---------+---+---+--------------------------------------------
c    Tuler Butcher Failure model
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include  "units_c.inc"
#include  "comlock.inc"
#include  "com_xfem1.inc"
C-----------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | R | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | R | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C NGL                         ELEMENT ID
C IPG                         CURRENT GAUSS POINT (in plane)
C ILAY                        CURRENT LAYER
C IPT                         CURRENT INTEGRATION POINT IN ALL LAYERS
C IPTT                        CURRENT INTEGRATION POINT IN THE LAYER (FOR OUTPUT ONLY)
C NPTT                        NUMBER OF INTEGRATION POINTS IN THE LAYER THICKNESS
C NPTTF                       NUMBER OF FAILED INTEGRATION POINTS IN THE LAYER
C NPTOT                       NUMBER OF INTEGRATION POINTS IN ALL LAYERS (TOTAL)
C NOFF                        NUMBER OF FAILED INTEGRATION POINTS (TOTAL)
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NUPARAM,IXFEM,IXEL,ILAY,IPTT,NPTT,NUVAR,IPT,NPTOT
      INTEGER NGL(NEL),ELCRKINI(NXLAYMAX,*),NOFF(NEL)
      my_real 
     .   TIME,TIMESTEP(NEL),UPARAM(NUPARAM),NPTTF(NEL),
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),SIGNXY(NEL),SIGNYZ(NEL),
     .   SIGNZX(NEL),TENS(NEL,5),DFMAX(NEL),TDEL(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real UVAR(NEL,NUVAR), OFF(NEL),OFFL(NEL)
      my_real, DIMENSION(:) ,POINTER  :: SIGSAV
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ISHELL,NINDX,NINDXP,LAYXFEM,IBRIT
      INTEGER INDX(NEL),INDXP(NEL),RFLAG(NEL),RFLAGP(NEL) 
      my_real 
     .   TBA,TBK,KK,SIGMAX,DADV,SIGR_INI,SIGR_ADV,
     .   SIG1,SIG2,S1,S2,BRIT_B,BRIT_C,DAMD(NEL)
      CHARACTER (LEN=3) :: XCHAR
C=======================================================================
      TBA      = UPARAM(1)                           
      TBK      = UPARAM(2)                           
      SIGR_INI = UPARAM(3)                        
      ISHELL   = INT(UPARAM(4))                     
      IBRIT    = INT(UPARAM(6))                   
      LAYXFEM  = INT(UPARAM(7))                  
      BRIT_B   = UPARAM(8)                        
      BRIT_C   = UPARAM(9)                        
      DADV     = UPARAM(10)
c-----------------------------
      IF (LAYXFEM == 0) RETURN
c-----------------------------
      LAYXFEM = IXFEM
      IF (LAYXFEM == 1 .and. ISHELL == 1) ISHELL = 2
c-----------------------------
      SIGR_ADV = SIGR_INI*DADV                        
C
      DO I=1,NEL
        DAMD(I)  = ZERO
        RFLAG(I) = 0
        RFLAGP(I)= 0
        INDXP(I) = 0
        INDX(I)  = 0
      ENDDO
      NINDX  = 0  
      NINDXP = 0  
C-----------------------------------------------
      DO I=1,NEL
        TENS(I,1) = SIGNXX(I)
        TENS(I,2) = SIGNYY(I)
        TENS(I,3) = SIGNXY(I)
        TENS(I,4) = SIGNYZ(I)
        TENS(I,5) = SIGNZX(I)
      END DO
C
      IF (IXEL > 0) THEN   ! testing phantom elements
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
c--------------------------
      SELECT CASE (LAYXFEM)                           
c---------------
        CASE (1)    ! multilayer XFEM                                    
c---------------
          DO I=1,NEL                                                                         
            IF (OFF(I) == UN) THEN                                                             
              IF (UVAR(I,1) < TBK) THEN                                                      
                S1 = UNDEMI*(SIGNXX(I) + SIGNYY(I))                                          
                S2 = UNDEMI*(SIGNXX(I) - SIGNYY(I))                                          
                SIG1 = S1  + SQRT(S2**2 + SIGNXY(I)**2)                                      
                SIG2 = S1  - SQRT(S2**2 + SIGNXY(I)**2)                                      
                SIGMAX = MAX(SIG1,SIG2)                                                      
c                  if(ngl(i)==7847)print*,'ilay,ippt,elcrkini=',ilay,ippt,elcrkini(ILAY,I)
                IF (IXEL == 0) THEN
                  IF (ELCRKINI(ILAY,I)==0 .and. SIGMAX > SIGR_INI) THEN            
                    UVAR(I,1) = UVAR(I,1) + TIMESTEP(I)*(SIGMAX - SIGR_INI)**TBA
c                  if(ngl(i)==7847)then
c                    print*,'ilay,iptt=',ilay,iptt
c                    print*,'SIGMAX,uvar=',SIGMAX,UVAR(I,1)
c                  endif
                    IF (UVAR(I,1) >= TBK) THEN   
                      NINDXP = NINDXP + 1             
                      INDXP(NINDXP) = I
                      OFFL(I)   = ZERO                                            
                      NOFF(I)   = NOFF(I)  + 1                                        
                      NPTTF(I)  = NPTTF(I) + UN       
                      RFLAGP(I) = 1                   
                      IF (INT(NPTTF(I)) == NPTT) THEN
                        NINDX = NINDX + 1
                        INDX(NINDX) = I
                        ELCRKINI(ILAY,I) = -1 ! one layer failed (by initiation)
                        RFLAG(I) = 1                                             
                      ENDIF
                      IF (NOFF(I) == NPTOT) THEN   
                        OFF(I) = QCINQ                                           
                        TDEL(I)= TIME                                            
                      ENDIF                                                      
                    ENDIF                                                      
                  ELSEIF (ELCRKINI(ILAY,I) == 2 .and. SIGMAX > SIGR_ADV) THEN
                    UVAR(I,1) = UVAR(I,1) + TIMESTEP(I)*(SIGMAX - SIGR_ADV)**TBA
c                    print*,'ilay,iptt=',ilay,iptt
c                    print*,'SIGMAX,uvar=',SIGMAX,UVAR(I,1)
                    IF (UVAR(I,1) >= TBK) THEN   
                      NINDXP   = NINDXP + 1
                      INDXP(NINDXP) = I
                      OFFL(I)   = ZERO                                            
                      NOFF(I)   = NOFF(I) + 1                                        
                      NPTTF(I)  = NPTTF(I)+ UN
                      RFLAGP(I) = 1
                      IF (INT(NPTTF(I)) == NPTT) THEN
                        NINDX = NINDX+1
                        INDX(NINDX) = I
                        ELCRKINI(ILAY,I) = 1 !  one layer failed (by advancing)
                        RFLAG(I) = -1
                      ENDIF
                      IF (NOFF(I) == NPTOT) THEN
                        OFF(I) = QCINQ                                           
                        TDEL(I)= TIME
                      ENDIF
                    ENDIF                                                         
                  ENDIF                                                         
                ELSEIF (SIGMAX > SIGR_INI) THEN   ! IXEL > 0 
                  UVAR(I,1) = UVAR(I,1) + TIMESTEP(I)*(SIGMAX - SIGR_INI)**TBA
                  IF (UVAR(I,1) >= TBK)  THEN                                        
                    NINDXP    = NINDXP + 1
                    INDXP(NINDXP) = I
                    OFFL(I)   = ZERO                                         
                    NPTTF(I)  = NPTTF(I) + UN
                    RFLAGP(I) = 1
                    IF (INT(NPTTF(I)) == NPTT) THEN
                      NINDX = NINDX+1
                      INDX(NINDX) = I
                      OFF(I) = QCINQ                                           
                      TDEL(I)  = TIME
                      RFLAG(I) = 3
                    ENDIF
                  ENDIF
                ENDIF ! IXEL                                                                      
              ELSE    ! UVAR(I,1) > TBK                                               
                SIGNXX(I) = ZERO                                                           
                SIGNYY(I) = ZERO                                                           
                SIGNXY(I) = ZERO                                                           
                SIGNYZ(I) = ZERO                                                           
                SIGNZX(I) = ZERO                                                           
              ENDIF  !                                         
            ENDIF    !  OFF(I) == UN                                          
          ENDDO                                                                              
c-----------------------------------------------------------------------
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
c
          IF (NINDX > 0) THEN                                                     
            DO J=1,NINDX                                                          
#include     "lockon.inc"                                                         
              I = INDX(J)                                                         
c             initialization std element                                                  
              IF (RFLAG(I) == 1) WRITE(IOUT ,3000) NGL(I),ILAY                    
              IF (RFLAG(I) == 1) WRITE(ISTDO,3100) NGL(I),ILAY,TIME               
c             advancement std element                                                     
              IF (RFLAG(I) == -1) WRITE(IOUT ,3200) NGL(I),ILAY                   
              IF (RFLAG(I) == -1) WRITE(ISTDO,3300) NGL(I),ILAY,TIME              
c             delete phantom element                                                         
              IF (RFLAG(I) == 3) WRITE(IOUT, 3400)XCHAR,NGL(I),ILAY               
              IF (RFLAG(I) == 3) WRITE(ISTDO,3500)XCHAR,NGL(I),ILAY,TIME          
#include    "lockoff.inc"                                                         
            ENDDO                                                                 
          ENDIF  !  NINDX > 0                                                     
c
c---------------
        CASE (2)    ! monolayer XFEM                                    
c---------------
          IF (ISHELL == 1) THEN
c---
            DO I=1,NEL                                                          
              IF (OFF(I) == UN) THEN                                              
                S1 = UNDEMI*(SIGNXX(I) + SIGNYY(I))                             
                S2 = UNDEMI*(SIGNXX(I) - SIGNYY(I))                             
                SIG1 = S1 + SQRT(S2**2 + SIGNXY(I)**2)                          
                SIG2 = S1 - SQRT(S2**2 + SIGNXY(I)**2)                          
                SIGMAX = MAX(SIG1,SIG2)                                         
C
                IF (IBRIT == 1) THEN  ! ductile rupture                           
                  IF (IXEL == 0) THEN                                           
                    IF (ELCRKINI(ILAY,I) == 0 .and. SIGMAX > SIGR_INI) THEN
                      UVAR(I,1) = UVAR(I,1) + TIMESTEP(I)*(SIGMAX - SIGR_INI)**TBA                
                      IF (UVAR(I,1) >= TBK) THEN                             
                        ELCRKINI(ILAY,I) = -1  ! ready for initiation           
                        NINDXP = NINDXP + 1             
                        INDXP(NINDXP) = I               
                        NINDX = NINDX + 1                                          
                        INDX(NINDX) = I                                           
                        RFLAGP(I) = 1
                        RFLAG(I) = 1                                            
                        TDEL(I)= TIME                                           
                        OFF(I) = QCINQ                                           
                      ENDIF                                         
                    ELSEIF (ELCRKINI(ILAY,I) == 2 .and. SIGMAX > SIGR_ADV) THEN                         
                      UVAR(I,1) = UVAR(I,1) + TIMESTEP(I)*(SIGMAX - SIGR_ADV)**TBA                
                      IF (UVAR(I,1) >= TBK) THEN                             
                        ELCRKINI(ILAY,I) = 1     ! ready to advance           
                        NINDXP = NINDXP + 1             
                        INDXP(NINDXP) = I               
                        NINDX = NINDX + 1                                         
                        INDX(NINDX) = I                                         
                        RFLAGP(I) = 1
                        RFLAG(I) = -1                                         
                        TDEL(I)= TIME                                         
                        OFF(I) = QCINQ                                           
                      ENDIF                                                       
                    ENDIF                                                       
                  ELSEIF (SIGMAX > SIGR_INI) THEN  ! IXEL > 0  
                    UVAR(I,1) = UVAR(I,1) + TIMESTEP(I)*(SIGMAX - SIGR_INI)**TBA                
                    IF (UVAR(I,1) >= TBK) THEN                  
                      OFF(I) = QCINQ                                           
                      NINDXP = NINDXP + 1           
                      INDXP(NINDXP) = I             
                      NINDX = NINDX + 1                                             
                      INDX(NINDX) = I                                             
                      RFLAGP(I) = 1
                      RFLAG(I)  = 3                                              
                    ENDIF                                                  
                  ENDIF  ! IXEL                                                 
c
                ELSEIF (IBRIT == 2) THEN  ! brittle rupture                       
                  KK = UN/MAX(EM10,TBK)                                           
                  IF (SIGMAX > UVAR(I,1)) THEN                                    
                    DAMD(I) = KK*(SIGMAX-UVAR(I,1))**BRIT_B                       
                  ENDIF                                                         
                  NOFF(I) = NOFF(I) + DAMD(I)*TIMESTEP(I)                           
                  IF (NOFF(I)<UN) UVAR(I,1) = SIGR_INI*(UN-NOFF(I))**BRIT_C        
                  IF (IXEL == 0) THEN                                           
                    IF (ELCRKINI(ILAY,I) == 0) THEN ! ready for initiation      
                      IF (NOFF(I) >= UN) THEN                                   
                        ELCRKINI(ILAY,I) = -1                                   
                        NINDXP = NINDXP + 1             
                        INDXP(NINDXP) = I               
                        NINDX=NINDX+1                                           
                        INDX(NINDX)=I                                           
                        RFLAGP(I) = 1
                        RFLAG(I) = 1                                            
                        TDEL(I)= TIME                                           
                        OFF(I) = QCINQ                                           
                      END IF                                                    
                    ELSEIF (ELCRKINI(ILAY,I) == 2) THEN  ! ready for advancing  
                      IF (NOFF(I) >= UN) THEN                                   
                        ELCRKINI(ILAY,I) = 1                                    
                        NINDXP = NINDXP + 1             
                        INDXP(NINDXP) = I               
                        NINDX=NINDX+1                                           
                        INDX(NINDX)=I                                           
                        RFLAGP(I) = 1
                        RFLAG(I) = -1                                           
                        TDEL(I)= TIME                                           
                        OFF(I) = QCINQ                                           
                      ENDIF                                                     
                    ENDIF                                                       
                  ELSEIF (NOFF(I) >= UN) THEN  ! IXEL > 0                       
                    NINDXP = NINDXP + 1             
                    INDXP(NINDXP) = I               
                    NINDX=NINDX+1                                               
                    INDX(NINDX)=I                                               
                    RFLAGP(I) = 1
                    RFLAG(I)  = 3                                                
                    OFF(I) = QCINQ                                           
                  END IF  ! IXEL                                                
c
                ENDIF  !  IF(IBRIT==1                                           
              ENDIF   !  IF(ISHELL==1                                           
            ENDDO                                                               
c
          ELSEIF (ISHELL == 2)THEN
c
            DO I=1,NEL
              IF (OFF(I) == UN) THEN
                IF (UVAR(I,1) < TBK) THEN
                  S1 = UNDEMI*(SIGNXX(I) + SIGNYY(I))
                  S2 = UNDEMI*(SIGNXX(I) - SIGNYY(I))
                  SIG1 = S1  + SQRT(S2**2 + SIGNXY(I)**2)
                  SIG2 = S1  - SQRT(S2**2 + SIGNXY(I)**2)
                  SIGMAX = MAX(SIG1,SIG2)
c
                  IF (IXEL == 0) THEN
                    IF (ELCRKINI(ILAY,I)==0 .and. SIGMAX > SIGR_INI) THEN           
                      UVAR(I,1) = UVAR(I,1) + TIMESTEP(I)*(SIGMAX - SIGR_INI)**TBA
                      IF (UVAR(I,1) >= TBK) THEN ! ready for initiation
                        NOFF(I) = NOFF(I) + 1                                                      
                        NINDXP = NINDXP + 1             
                        INDXP(NINDXP) = I               
                        NINDX = NINDX+1                                                           
                        INDX(NINDX) = I                                                           
                        RFLAGP(I) = 1
                        IF (NOFF(I) == NPTOT) THEN
                          ELCRKINI(ILAY,I) = -1 ! one layer failed (by initiation)                
                          RFLAG(I) = 1
                          TDEL(I)= TIME
                          OFF(I) = QCINQ                                           
                        ENDIF                                     
                      ENDIF                                     
                    ELSEIF (ELCRKINI(ILAY,I) == 2 .and. SIGMAX > SIGR_ADV) THEN
                      UVAR(I,1) = UVAR(I,1) + TIMESTEP(I)*(SIGMAX - SIGR_ADV)**TBA
                      IF (UVAR(I,1) >= TBK) THEN ! ready for advancing                                             
                         NOFF(I) = NOFF(I) + 1                                                     
                         NINDXP = NINDXP + 1             
                         INDXP(NINDXP) = I               
                         NINDX = NINDX+1                                                            
                         INDX(NINDX) = I                                                            
                         RFLAGP(I)  = 1
                         IF (NOFF(I) == NPTOT) THEN
                           ELCRKINI(ILAY,I) = 1 !  one layer failed (by advancing) 
                           RFLAG(I) = -1
                           TDEL(I)= TIME
                           OFF(I) = QCINQ                                           
                         ENDIF                    
                      ENDIF
                    ENDIF
                  ELSEIF  (SIGMAX > SIGR_INI) THEN  ! IXEL > 0  
                    UVAR(I,1) = UVAR(I,1) + TIMESTEP(I)*(SIGMAX - SIGR_INI)**TBA
                    IF (UVAR(I,1) >= TBK)  THEN   ! IXEL > 0                                       
                      NINDXP = NINDXP + 1             
                      INDXP(NINDXP) = I               
                      NINDX = NINDX+1
                      INDX(NINDX) = I
                      NOFF(I) = NOFF(I) + 1                                                        
                      IF (NOFF(I) == NPTOT) THEN
                        OFF(I) = QCINQ                                           
                        RFLAG(I) = 3
                      ENDIF
                    ENDIF
                  ENDIF ! IXEL                                                                    
                ELSE    ! UVAR(I,1) >= TBK
                  SIGNXX(I) = ZERO
                  SIGNYY(I) = ZERO
                  SIGNXY(I) = ZERO
                  SIGNYZ(I) = ZERO
                  SIGNZX(I) = ZERO
                ENDIF  !  IF(UVAR(I,1) < TBK)
              ENDIF  ! IF(ISHELL==2.AND.OFF(I)==UN)
            ENDDO
c----
            IF (NINDXP > 0) THEN    
              DO J=1,NINDXP        
                I = INDXP(J)        
                SIGNXX(I) = ZERO                                              
                SIGNYY(I) = ZERO                                              
                SIGNXY(I) = ZERO                                              
                SIGNYZ(I) = ZERO                                              
                SIGNZX(I) = ZERO                                              
#include "lockon.inc"
                IF (RFLAGP(I) == 1) WRITE(IOUT, 4800)NGL(I),IPTT        
                IF (RFLAGP(I) == 1) WRITE(ISTDO,4900)NGL(I),IPTT,TIME        
#include "lockoff.inc"
              ENDDO               
            ENDIF  !  NINDXP > 0   
c
            IF (NINDX > 0) THEN
              DO J=1,NINDX
                I = INDX(J)
#include        "lockon.inc"
c               initialization std element
                IF (RFLAG(I) == 1) WRITE(IOUT, 4000) NGL(I)
                IF (RFLAG(I) == 1) WRITE(ISTDO,4100) NGL(I),TIME
c               advancement std element
                IF (RFLAG(I) == -1) WRITE(IOUT, 4200) NGL(I)
                IF (RFLAG(I) == -1) WRITE(ISTDO,4300) NGL(I),TIME
c               delete phantom
                IF (RFLAG(I) == 3) WRITE(IOUT, 4400)XCHAR,NGL(I)
                IF (RFLAG(I) == 3) WRITE(ISTDO,4500)XCHAR,NGL(I),TIME
#include        "lockoff.inc"
              ENDDO
            ENDIF
c----
          ENDIF  !  ISHELL
c
c-----------------
      END SELECT ! LAYXFEM
c-----------------
c
c---  Maximum Damage storing for output : 0 < DFMAX < 1
      DO I=1,NEL
        DFMAX(I) = MIN(UN, MAX(DFMAX(I),UVAR(I,1)/TBK))
      ENDDO
C-----------------------------------------------
 2000 FORMAT(1X,'FOR SHELL ELEMENT',I10,1X,'LAYER',I3,':',/
     .       1X, 'STRESS TENSOR SET TO ZERO (TBUTCHER)')
 2100 FORMAT(1X,'FOR SHELL ELEMENT',I10,1X,'LAYER',I3,':',/,
     .       1X, 'STRESS TENSOR SET TO ZERO (TBUTCHER)',1X,'AT TIME :',1PE20.13)
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
 3800 FORMAT(1X,'TBUTCHER FAILURE IN SHELL',I10,1X,'LAYER',I3,1X,'INT POINT',I2)
 3900 FORMAT(1X,'TBUTCHER FAILURE IN SHELL',I10,1X,'LAYER',I3,1X,'INT POINT',I2,/
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
 4800 FORMAT(1X,'TBUTCHER FAILURE IN SHELL',I10,1X,'INT POINT',I2)
 4900 FORMAT(1X,'TBUTCHER FAILURE IN SHELL',I10,1X,'INT POINT',I2,1X,'AT TIME :',1PE12.4)
c-----------
      RETURN
      END
