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
Chd|  RUPTINT2                      source/interfaces/interf/ruptint2.F
Chd|-- called by -----------
Chd|        I2RUPT                        source/interfaces/interf/int2rupt.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        UINTBUF_MOD                   source/user_interface/uintbuf_mod.F
Chd|====================================================================
      SUBROUTINE RUPTINT2(
     1             NSL     ,ISL     ,NUVAR   ,UVAR    ,USERBUF ,
     2             PROP    ,IFUNS   ,IFUNN   ,IFUNT   ,IMOD    ,
     3             IFILTR  ,IDBG    ,NPF     ,TF      ,NOINT   ,
     4             ITAB    ,PDAMA2  ,ISYM    ,H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UINTBUF_MOD
      USE H3D_MOD
C----------+---------+---+---+--------------------------------------------
C VAR      | SIZE    |TYP| RW| DEFINITION
C----------+---------+---+---+--------------------------------------------
C NSL      |  1      | I | R | NUMBER OF SECONDARY NODES
C NUVAR    |  1      | I | R | NUMBER OF USER VARIABLES
C PROP     |  6      | F | R | PROPERTY BUFFER
C UVAR     |NUVAR    | F |R/W| USER SECONDARY NODE VARIABLES
C USERBUF  |         | F |R/W| SECONDARY NODE DATA STRUCTURE
C=======================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "scr14_c.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      INTEGER NPF(*),ITAB(*)
      INTEGER NSL,ISL,NUVAR,IMOD,IFUNN,IFUNT,IFUNS,IDBG,IFILTR,
     .        NOINT,ISYM
      my_real
     .        PROP(*),UVAR(*), TF(*),PDAMA2(2,*)
      type(UINTBUF) :: USERBUF
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  IERROR,IRUPT,IRUPT0,ISLAV
      my_real
     .   DTIM,SIGN,SIGT,SIGN1,SIGT1,AREA,DFSIGN,DFSIGT,DSIG,
     .   SIGNMAX,SIGTMAX,DNMAX,DTMAX,FACN,FACT,
     .   SCAL_F,SCAL_D,SCAL_SR,SSR,ALPHA,DERI,DIS_N,DIS_T,DIS_NA,SIGNA
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      my_real  FINTER
C=======================================================================
C     MODULE COMPONENTS
C-----------------------------------------------
      ISLAV =  USERBUF%ISLAVE     
      AREA  =  USERBUF%AREA        
      DIS_N =  USERBUF%DXN       
      DIS_T =  USERBUF%DXT       
      SIGN  =  USERBUF%SIGN       
      SIGT  =  USERBUF%SIGT       
      DTIM  =  USERBUF%DT
      IRUPT =  NINT(USERBUF%RUPT)
      IRUPT0= IRUPT
      DIS_NA = ABS(DIS_N)
      SIGNA  = ABS(SIGN)
C-----------------------------------------------
      SSR  = UN
      FACN = UN    
      FACT = UN    
C-----
      SCAL_F  = PROP(1)        
      SCAL_D  = PROP(2)   
      SCAL_SR = PROP(3)    
      ALPHA   = PROP(4)
      DNMAX   = PROP(5)
      DTMAX   = PROP(6)
C-------
c      IF (IRUPT == 0) THEN            
c        UVAR(1) = SIGN
c        UVAR(2) = SIGT
c      ENDIF                             
c
C---  Force filtering
c
      SIGN1  = UVAR(1)                           
      SIGT1  = UVAR(2)                           
      IF (IFILTR == 1) THEN
        SIGN = ALPHA*SIGN + (UN-ALPHA)*SIGN1
        SIGT = ALPHA*SIGT + (UN-ALPHA)*SIGT1
      ENDIF
C        
      DFSIGN = (SIGN - SIGN1) / DTIM                           
      DFSIGT = (SIGT - SIGT1) / DTIM                           
      DSIG   = SQRT(DFSIGN**2 + DFSIGT**2)   
      IF (IFUNS > 0) SSR = FINTER(IFUNS,DSIG/SCAL_SR,NPF,TF,DERI)
c
C---  Rupture Criteria 
c
      SIGNMAX = SSR*SCAL_F * FINTER(IFUNN,DIS_NA/SCAL_D,NPF,TF,DERI) 
      SIGTMAX = SSR*SCAL_F * FINTER(IFUNT,DIS_T /SCAL_D,NPF,TF,DERI)
C       
      IF (SIGT > ZERO) FACT = MIN(UN, ABS(SIGTMAX / SIGT))
C
      IF (ISYM == 0 .OR. IRUPT /= 0) THEN
        IF (ABS(SIGN) > ZERO) FACN = MIN(UN, ABS(SIGNMAX / SIGN))
        IF (IMOD == 2) THEN
          IF (DIS_NA > DNMAX .OR. DIS_T > DTMAX) THEN
            IRUPT = 1
            FACN = ZERO
            FACT = ZERO
          ENDIF
          IF (ANIM_N(15)==1 .OR. H3D_DATA%N_SCAL_DAMA2 == 1) 
     .            PDAMA2(1,ISLAV)=MIN(CENT*DIS_NA/DNMAX,CENT)
          IF (ANIM_N(16)==1 .OR. H3D_DATA%N_SCAL_DAMA2 == 1) 
     .            PDAMA2(2,ISLAV)=MIN(CENT*DIS_T/DTMAX,CENT)
        ELSEIF (IMOD == 1) THEN
          DIS_NA = DIS_NA / DNMAX
          DIS_T  = DIS_T  / DTMAX
          IF (SQRT(DIS_N*DIS_N + DIS_T*DIS_T) > UN) THEN
            IRUPT = 1
            FACN = ZERO
            FACT = ZERO
          ENDIF
          IF (ANIM_N(15)==1 .OR. H3D_DATA%N_SCAL_DAMA2 == 1) 
     .            PDAMA2(1,ISLAV)=MIN(CENT*DIS_NA,CENT)
          IF (ANIM_N(16)==1 .OR. H3D_DATA%N_SCAL_DAMA2 == 1) 
     .            PDAMA2(2,ISLAV)=MIN(CENT*DIS_T,CENT)
        ENDIF
C
      ELSE    ! ISYM == 1 .AND. IRUPT == 0)
        IF (SIGN > ZERO) FACN = MIN(UN, ABS(SIGNMAX / SIGN))
        IF (IMOD == 2) THEN
          IF (DIS_N > ZERO .AND. DIS_NA > DNMAX .OR. DIS_T > DTMAX) THEN
            IRUPT = 1
            FACN = ZERO
            FACT = ZERO
          ENDIF
          IF (ANIM_N(15)==1 .OR. H3D_DATA%N_SCAL_DAMA2 == 1) 
     .            PDAMA2(1,ISLAV)=MIN(CENT*DIS_NA/DNMAX,CENT)
          IF (ANIM_N(16)==1 .OR. H3D_DATA%N_SCAL_DAMA2 == 1) 
     .            PDAMA2(2,ISLAV)=MIN(CENT*DIS_T/DTMAX,CENT)
        ENDIF
      ENDIF  ! ISYM
C-----
      IF (IRUPT == 1) THEN
        FACN = ZERO
        FACT = ZERO
      ELSEIF (FACN < UN .OR. FACT < UN) THEN
        IRUPT = -1
      ENDIF
C-----
      if (IDBG > 0) then                                       
        IF (IRUPT /= 0) THEN                                         
          IF (IRUPT == 1) THEN
            WRITE(IOUT,*)'RUPTURE TOTALE' 
          ELSEIF (IRUPT == -1) THEN
            WRITE(IOUT,*)'RUPTURE PARTIELLE' 
          ENDIF
          WRITE(IOUT,*)'Time =',TT, ' SECONDARY =',ISL,ITAB(ISLAV)
          if (IDBG == 2) WRITE(IOUT,*)'AREA  =',AREA                     
          WRITE(IOUT,*)'Dist N  =',DIS_N,  ' Dist T  =',DIS_T                             
          WRITE(IOUT,*)'DNmax  =',DNMAX,  ' DTmax  =',Dtmax                           
          WRITE(IOUT,*)'Sig N   =',sign,   ' sig T   =',sigt        
          WRITE(IOUT,*)'SIGNMAX =',SIGNMAX,' SIGTMAX =',SIGTMAX                               
          WRITE(IOUT,*)'Facn,t  =',FACN,FACT                            
          WRITE(IOUT,*)'------------------------------------- '     
        ENDIF                                                         
      endif                                                       
C
      IF (IRUPT0 == 0 .AND. IRUPT /= 0) THEN
        WRITE(IOUT,'(A,I9,A,E16.9,A,I9)')
     .            'INTERFACE TYPE 2 N ',NOINT, '  TIME= ',TT,                   
     .            '   START RUPTURE SECONDARY NODE ',ITAB(ISLAV)
        WRITE(*,'(A,I9,A,E16.9,A,I9)')
     .            'INTERFACE TYPE 2 N ',NOINT, '  TIME= ',TT,                   
     .            '   START RUPTURE SECONDARY NODE ',ITAB(ISLAV)
      ENDIF
      IF (IRUPT0 /= 1 .AND. IRUPT == 1) THEN                            
        WRITE(IOUT,'(A,I9,A,E16.9,A,I9)')
     .            'INTERFACE TYPE 2 N ',NOINT, '  TIME= ',TT,                   
     .            '   TOTAL RUPTURE SECONDARY NODE ',ITAB(ISLAV)
      ENDIF                                           
C-----------
      UVAR(1) = SIGN   
      UVAR(2) = SIGT  
      USERBUF%FACN = FACN
      USERBUF%FACT = FACT
      USERBUF%RUPT = IRUPT      
C-------------------------------
      RETURN
      END SUBROUTINE RUPTINT2
        
Chd|====================================================================
Chd|  BUTT                          source/interfaces/interf/ruptint2.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BUTT(DT,ALPHA,SIG2,SIG1,SIG,FSIG2,FSIG1,FSIG)
C-----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION SIG2,SIG1,SIG,FSIG2,FSIG1,FSIG,ALPHA
      DOUBLE PRECISION DT,PI,SQ2,WD,WA,WA2,A0,A1,A2,B1,B2,C1
C-----------------------------------------------------------------------
      DOUBLE PRECISION ZERO,UN,DEUX
      INTEGER I
C-----------------------------------------------------------------------
      ZERO = 0.0
      UN   = 1.0
      DEUX = 2.0
      DT   = DT / DEUX                           
      PI   = ATAN2(ZERO,-UN)
      SQ2  = SQRT(DEUX)
      WD = SQ2*PI*ALPHA*5.0/3.0  
      WA = SIN(WD*DT) / COS(WD*DT)                     
      WA2 = WA*WA                                      
      C1 = UN + SQ2*WA + WA2                           
      A0 = WA2 / C1                                    
      A1 = DEUX * A0                                   
      A2 = A0                                          
      B1 = -DEUX*(WA2 - UN) / C1                       
      B2 = (-UN + SQ2*WA - WA2) / C1                   
      FSIG = A0*SIG + A1*SIG1 + A2*SIG2 + B1*FSIG1 + B2*FSIG2
C---
      RETURN
      END SUBROUTINE BUTT
