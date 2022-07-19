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
Chd|  LAW119_UPD                    source/materials/mat/mat119/law119_upd.F
Chd|-- called by -----------
Chd|        UPDMAT                        source/materials/updmat.F     
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        TABLE_INTERS                  source/tools/curve/func_inters.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE LAW119_UPD(NUPARAM ,NUMTABL ,ITABLE ,TABLE  ,TABLE_ID ,
     .                      UPARAM  ,PM      ,TITR   ,MAT_ID )          
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE MESSAGE_MOD   
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: NUPARAM,NUMTABL,MAT_ID
      INTEGER ,DIMENSION(NUMTABL) :: ITABLE,TABLE_ID
      my_real ,DIMENSION(NPROPM)  :: PM
      my_real ,DIMENSION(NUPARAM), INTENT(INOUT) :: UPARAM
      TYPE(TTABLE) ,DIMENSION(NTABLE) ::  TABLE
      CHARACTER*nchartitle  :: TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,FUNC,FUND,NDIM,NPOINT
      my_real :: E11,N12,N21,NU,C1,SSP,KMAX,KINI,DX,DY,SLOPE,RHO,
     .           FSCALE1,FSCALE2,FSCALET,DET,A11,A12,A22,XINT,YINT
c=======================================================================
      FUNC = ITABLE(1)
      FUND = ITABLE(2)
      N12  = UPARAM(3)
      FSCALE1 = UPARAM(11)                        
      FSCALE2 = UPARAM(12)
      FSCALET = UPARAM(13)
c
      IF (FUNC > 0) THEN
        NDIM = TABLE(FUNC)%NDIM
        NPOINT = SIZE(TABLE(FUNC)%X(1)%VALUES)
        DX = TABLE(FUNC)%X(1)%VALUES(2) - TABLE(FUNC)%X(1)%VALUES(1)
        DY = TABLE(FUNC)%Y%VALUES(2) - TABLE(FUNC)%Y%VALUES(1)
        KINI = FSCALE1 * DY / DX
        KMAX = KINI        
        DO I = 3,NPOINT
          J  = I-1
          DX = TABLE(FUNC)%X(1)%VALUES(I) - TABLE(FUNC)%X(1)%VALUES(J)
          DY = TABLE(FUNC)%Y%VALUES(I) - TABLE(FUNC)%Y%VALUES(J)
          SLOPE = FSCALE1 * DY / DX
          KMAX  = MAX(KMAX, SLOPE)
        END DO
c
        IF (FUND > 0) THEN
          NDIM = TABLE(FUND)%NDIM
          NPOINT = SIZE(TABLE(FUND)%X(1)%VALUES)
          DX = TABLE(FUND)%X(1)%VALUES(2) - TABLE(FUND)%X(1)%VALUES(1)
          DY = TABLE(FUND)%Y%VALUES(2) - TABLE(FUND)%Y%VALUES(1)
          KINI = FSCALE2 * DY / DX
          KMAX =  MAX(KMAX, KINI)  
          DO I = 3,NPOINT
            J  = I-1
            DX = TABLE(FUND)%X(1)%VALUES(I) - TABLE(FUND)%X(1)%VALUES(J)
            DY = TABLE(FUND)%Y%VALUES(I) - TABLE(FUND)%Y%VALUES(J)
            SLOPE = FSCALE2 * DY / DX
            KMAX  = MAX(KMAX, SLOPE)
          END DO
          UPARAM(1) = KMAX             ! E11 
          UPARAM(2) = KMAX * FSCALET   ! E22 
c
c         check intersection with loading curve
c
          CALL TABLE_INTERS(TABLE,FUNC,FUND,FSCALE1,FSCALE2,XINT,YINT)
c
          IF (XINT == ZERO .or. YINT == ZERO) THEN
            CALL ANCMSG(MSGID=1716 ,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_2,
     .                  I1 = MAT_ID,
     .                  I2 = TABLE_ID(1),                                               
     .                  I3 = TABLE_ID(2),                                              
     .                  C1 = TITR  )
          ENDIF 
          UPARAM(18) = XINT   
          UPARAM(19) = YINT
c
        END IF
c
        RHO = PM(89)
        C1  = KMAX / (ONE - FSCALET*N12**2)
        SSP = SQRT(C1/RHO)
        N21 = N12 * FSCALET
        NU  = SQRT(N12*N21)
        DET = ONE / (ONE - N12*N21)
        A11 = KMAX * DET
        A22 = A11 * FSCALET
        A12 = A11 * N21
        UPARAM(1) = KMAX                 ! E11 
        UPARAM(2) = UPARAM(1) * FSCALET  ! E22 
        UPARAM(4) = N21
        UPARAM(5) = NU
        UPARAM(7) = A11 
        UPARAM(8) = A22 
        UPARAM(9) = A12   
        UPARAM(17)= SSP        
        PM(20) = KMAX
        PM(21) = NU
        PM(24) = KMAX/(ONE - NU**2)
        PM(32) = C1     
      END IF      
c-----------
      RETURN
      END
