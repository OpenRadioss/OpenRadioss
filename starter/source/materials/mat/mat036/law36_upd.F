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
Chd|  LAW36_UPD                     source/materials/mat/mat036/law36_upd.F
Chd|-- called by -----------
Chd|        UPDMAT                        source/materials/updmat.F     
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FUNC_INTERS                   source/tools/curve/func_inters.F
Chd|        VW_SMOOTH                     source/tools/curve/vw_smooth.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE LAW36_UPD(IOUT   ,TITR   ,MAT_ID ,NUPARAM,UPARAM ,
     .                     NFUNC  ,IFUNC  ,FUNC_ID,NPC    ,PLD    ,
     .                     MTAG   )
C----------------------------------------------- 
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE TABLE_MOD
      USE ELBUFTAG_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT_ID,IOUT,NUPARAM,NFUNC
      INTEGER, DIMENSION(NFUNC) :: IFUNC, FUNC_ID
      INTEGER NPC(*)
      my_real UPARAM(NUPARAM),PLD(*)
      CHARACTER*nchartitle  :: TITR
      TYPE(MLAW_TAG_),INTENT(INOUT)   :: MTAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,IFE,IE,IX1,IX2,IY1,IY2,NRATE,IYLD,IFAIL,FUNC1,FUNC2
      my_real :: EPSMAX,EPSLAST,X1,X2,Y1,Y2,FAC1,FAC2,XINT,YINT,Y0



      INTEGER :: K,NPT,NPTMAX,ZEROSTART,MONOTONIC
      my_real ,DIMENSION(:) ,ALLOCATABLE :: XX,YY,FX,FY
C==================================================================== 
c     Check if scale factor function of Young modulus is decreasing with plastic strain   
c    
      IFE = IFUNC(NFUNC)                   
      IF (IFE > 0) THEN 
        IE  = NPC(IFE)                                                       
        IY2 = NPC(IFE+1)                                                     
        DO I = IE+1,IY2-3,2                                                  
          IF (PLD(I) < PLD(I+2)) THEN                                       
             CALL ANCMSG(MSGID=975, MSGTYPE=MSGERROR, ANMODE=ANINFO,        
     .            I1 = FUNC_ID(NFUNC),                                               
     .            C1 = TITR   )                                               
             EXIT                                                           
          ENDIF                                                             
        ENDDO                                                                
      ENDIF                                      
c    
c     Check if static yield function decreases to zero (last point or negative slope)
c     In this case we introduce failure at the plastic strain corresponding to sig_yld=0
c  
      NRATE  = NINT(UPARAM(1))
      EPSMAX = UPARAM(2*NRATE + 7)
      IFAIL  = NINT(UPARAM(2*NRATE + 27))
      IYLD = IFUNC(1)
      IX1  = NPC(IYLD+1) - 4                                     
      IY1  = NPC(IYLD+1) - 3                                     
      IY2  = NPC(IYLD+1) - 1                                     
      IX2  = NPC(IYLD+1) - 2
      X1 = PLD(IX1)                                    
      X2 = PLD(IX2)                                    
      Y1 = PLD(IY1)                                    
      Y2 = PLD(IY2)                                    
      IF (IX2 > ZERO .and. Y2 == ZERO) THEN  ! last value of yield curve is 0
        EPSLAST = X2
        EPSMAX  = UPARAM(7+2*NRATE)
        IF (EPSLAST < EPSMAX) UPARAM(2*NRATE + 7 ) = EPSLAST
        IF (IFAIL == 0)       UPARAM(2*NRATE + 27) = 1    ! IFAIL
        UPARAM(2*NRATE + 28) = 1    ! YLDCHECK
        MTAG%G_DMG = 1
        MTAG%L_DMG = 1        
      ELSE IF (Y1 > Y2) THEN                 ! yield function slope is negative 
        EPSLAST = (X2*Y1 - X1*Y2) / (Y1 - Y2)
        IF (EPSLAST < EPSMAX) UPARAM(2*NRATE + 7 ) = EPSLAST
        IF (IFAIL == 0)       UPARAM(2*NRATE + 27) = 1    ! IFAIL
        UPARAM(2*NRATE + 28) = 1    ! YLDCHECK
        MTAG%G_DMG = 1
        MTAG%L_DMG = 1        
      ENDIF                                      
c-----------------------------------------------------------------------    
c     Check if yield curves for different strain rates do not intersect
c-----------------------------------------------------------------------
      DO I = 1,NRATE
        DO J = I+1,NRATE
          FUNC1 = IFUNC(I)
          FUNC2 = IFUNC(J)
          FAC1  = UPARAM(NRATE + 6 + I)
          FAC2  = UPARAM(NRATE + 6 + J)
          IF (FUNC1 > 0 .and. FUNC2 > 0 .and. FUNC1 /= FUNC2) THEN
            CALL FUNC_INTERS(TITR,MAT_ID,FUNC1 ,FUNC2 ,FAC1 ,FAC2 ,
     .                 NPC ,PLD   ,XINT ,YINT )
c
            IF (XINT > ZERO .and. YINT > ZERO) THEN
              CALL ANCMSG(MSGID=2064, MSGTYPE=MSGWARNING, ANMODE=ANINFO,        
     .                    I1 = MAT_ID,                                               
     .                    I2 = FUNC_ID(FUNC1),                                               
     .                    I3 = FUNC_ID(FUNC2),                                               
     .                    C1 = TITR   )
            END IF
          END IF
        END DO                                                      
      END DO
      
c===========================================================================
c        TEST VW algorithm
c===========================================================================
        
      ZEROSTART = 1
      MONOTONIC = 1
      FUNC1 = IFUNC(1)
      NPT = (NPC(FUNC1+1)-NPC(FUNC1))*HALF  ! nb of points of the curve
      ALLOCATE (XX(NPT))
      ALLOCATE (YY(NPT))
      ALLOCATE (FX(NPT))
      ALLOCATE (FY(NPT))
      XX(:) = ZERO
      YY(:) = ZERO
      FX(:) = ZERO
      FY(:) = ZERO
      
      print*,'NPT = ',NPT
      NPTMAX = 100
      IF (MONOTONIC == 1) THEN 
        Y0 = PLD(NPC(FUNC1)+1)
        DO I = 2,NPT+1
          K = I-1
          J = 2*(I-2)
          XX(K) = PLD(NPC(FUNC1)+J)
          YY(K) = MAX(PLD(NPC(FUNC1)+J+1) , Y0)
          Y0    = YY(K)
        ENDDO
      ELSE
        DO I = 2,NPT+1
          K = I-1
          J = 2*(I-2)
          XX(K) = PLD(NPC(FUNC1)+J)
          YY(K) = PLD(NPC(FUNC1)+J+1)
        ENDDO
      END IF
      IF (ZEROSTART == 1) THEN  ! first function point must be (0,0)
        XX(1) = ZERO
        YY(1) = ZERO
      END IF
      DO I=1,NPT      
        write(*,'(2E12.3)')  XX(I),YY(I) 
      END DO    
        print*,' '

        CALL VW_SMOOTH(NPT,NPTMAX ,XX,YY, FX,FY)
       
c      
        print*,' '
        print*,' '
      DO I=1,NPTMAX    
        write(*,'(2E12.3)')  FX(I),FY(I) 
      END DO    
     
      
      
      
      
c--------------------------------------------------------
      RETURN
      END
