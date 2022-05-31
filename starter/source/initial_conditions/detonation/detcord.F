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
Chd|  DETCORD                       source/initial_conditions/detonation/detcord.F
Chd|-- called by -----------
Chd|        M5IN2                         source/initial_conditions/detonation/m5in2.F
Chd|        M5IN2T                        source/initial_conditions/detonation/m5in2t.F
Chd|        M5IN3                         source/initial_conditions/detonation/m5in3.F
Chd|-- calls ---------------
Chd|        CR_SPLINE_KNOTS               source/initial_conditions/detonation/detcord.F
Chd|        CR_SPLINE_LENGTH              source/initial_conditions/detonation/detcord.F
Chd|        CR_SPLINE_POINT_PROJ          source/initial_conditions/detonation/detcord.F
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|====================================================================
      SUBROUTINE DETCORD(DETONATORS,N2,NP2,X,MTL,XC,YC,ZC,VDET,VDET2,ALT,BT,TB,JJ,IOPT,IX,NIX)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
      !IOPT=0  : def=3
      !IOPT=1  : piecewise linear  - multiple segments (experimental / osbslete)
      !IOPT=2  : instantaneous - multiple segments (experimental / obsolete)
      !IOPT=3  : Centripetal-Catmull-Rom SPLINE interpolation + projection along neutral fiber
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DETONATORS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "vect01_c.inc"
#include      "units_c.inc"
#include      "scr11_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: MTL, N2, NP2, JJ(MVSIZ),iopt,NIX,IX(NIX,*)
      my_real :: X(3,*),XC(MVSIZ),YC(MVSIZ),ZC(MVSIZ),BT(MVSIZ),VDET,VDET2,ALT,TB(*)
      TYPE(DETONATOR_STRUCT_)::DETONATORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: J, I1,I2, II,K, iTdet,  i, NPTS, NSEG,IDIST(MVSIZ),NDETCORD,NPE,NPE2
      my_real :: DDMX, DTOS, XLP2, YLP2, XLP1     , ZLP2 ,YLP1,
     .           ZLP1, XL0 , YL0 , ZL0 , XL1      , YL1  ,ZL1 , XL2 ,YL2 ,ZL2,
     .           PS1 , PS2 , DL1 , DL2 , DL(MVSIZ), S1   ,S2  , S3,  TdetC, TP1, TP2, TH, TH1,TH2,
     .           TC1,TC2,TC0,TC,p1p2,dh,ALPHA,KNOTS(4),ZP(3),
     .           ZH1(3),DIST1,T1,ZH2(3),DIST2,T2,DIST(MVSIZ),XX,YY,ZZ,D,LOCAL_PT(4,3),C(3),T,DD,
     .           LEN,LEN1,LEN2

      type SPLINE_PATH
         INTEGER :: NUM_POINTS
         my_real,ALLOCATABLE,DIMENSION(:,:) :: control_point
         my_real,ALLOCATABLE,DIMENSION(:)   :: length
         my_real,ALLOCATABLE,DIMENSION(:)   :: cumulative_length
         my_real,ALLOCATABLE,DIMENSION(:,:) :: knots
      end type SPLINE_PATH

      type(SPLINE_PATH),TARGET :: USER_SPLINE_PATH

      my_real, POINTER, DIMENSION(:,:) :: ptr

C-----------------------------------------------
C   P r e - C o n d i t i o n
C-----------------------------------------------
      NDETCORD = DETONATORS%NDETCORD
      IF(NDETCORD.EQ.0)RETURN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      NPE = DETONATORS%NPE
      NPE2 = NP2      
      DTOS = EP30

      IF(IOPT==2)THEN
      !INSTANTANEOUS IGNITION OF THE WHOLE CORD
        DO J=1,NPE2-1 
          !first node
          I       = N2+J-1
          II      = DETONATORS%ICORD(I)
          XLP1    = X(1,II)
          YLP1    = X(2,II)
          ZLP1    = X(3,II)
          !second node
          I       = N2+J
          II      = DETONATORS%ICORD(I)
          XLP2    = X(1,II)       
          YLP2    = X(2,II)
          ZLP2    = X(3,II)
          !loop on elems to compute burning time from line [P1,P2]       
          DO I=LFT,LLT                                                   
            XL0   = (XLP1-XLP2)                                          
            YL0   = (YLP1-YLP2)                                          
            ZL0   = (ZLP1-ZLP2)                                          
            XL1   = (XC(I)-XLP1)                                         
            YL1   = (YC(I)-YLP1)                                         
            ZL1   = (ZC(I)-ZLP1)                                         
            XL2   = (XC(I)-XLP2)                                         
            YL2   = (YC(I)-YLP2)                                         
            ZL2   = (ZC(I)-ZLP2)                                         
            PS1   = XL1*XL0+YL1*YL0+ZL1*ZL0                               
            PS2   = XL2*XL0+YL2*YL0+ZL2*ZL0                               
            IF(PS1*PS2.GT.ZERO)THEN   
              !projection point not on the segment [P1,P2]                                  
              DL1 = SQRT(XL1**2+YL1**2+ZL1**2)                            
              DL2 = SQRT(XL2**2+YL2**2+ZL2**2)                            
              DL(I)=MIN(DL1,DL2)                                         
            ELSE              
              !projection along the segment [P1,P2]                                        
              S1  = YL1*ZL0 - ZL1*YL0                                     
              S2  = ZL1*XL0 - XL1*ZL0                                     
              S3  = XL1*YL0 - YL1*XL0                                     
              DL(I)=SQRT((S1**2+S2**2+S3**2)/(XL0**2+YL0**2+ZL0**2))     
            ENDIF                                                        
            BT(I) =ALT+DL(I)/VDET                                        
            IF(BT(I).LT.ABS(TB(I))) TB(I)=-BT(I)                       
          END DO !next I                                              
        END DO !J=1,NPE      
      
      ELSEIF(IOPT==1)THEN
      ! DETONATING CORD HAS ITS OWN DETONATION VELOCITY ALONG THE PATH  
        DO J=1,NPE2-1 
          !first node
          I1          = N2+J-1
          II          = DETONATORS%ICORD(I1)
          XLP1        = X(1,II)
          YLP1        = X(2,II)
          ZLP1        = X(3,II)
          TP1         = DETONATORS%DCORD(I1)
          !second node
          I2          = N2+J
          II          = DETONATORS%ICORD(I2)
          XLP2        = X(1,II)       
          YLP2        = X(2,II)
          ZLP2        = X(3,II)
          TP2         = DETONATORS%DCORD(I2)
          !loop on elems to compute burning time from line [P1,P2]       
          DO I=LFT,LLT     
            TdetC     = ZERO                                            
            XL0       = (XLP1-XLP2)                                      
            YL0       = (YLP1-YLP2)                                      
            ZL0       = (ZLP1-ZLP2)                                      
            XL1       = (XC(I)-XLP1)                                     
            YL1       = (YC(I)-YLP1)                                     
            ZL1       = (ZC(I)-ZLP1)                                     
            XL2       = (XC(I)-XLP2)                                     
            YL2       = (YC(I)-YLP2)                                     
            ZL2       = (ZC(I)-ZLP2) 
            PS1       = XL1*XL0+YL1*YL0+ZL1*ZL0                               
            PS2       = XL2*XL0+YL2*YL0+ZL2*ZL0                                                 
            DL1       = SQRT(XL1**2+YL1**2+ZL1**2)                            
            DL2       = SQRT(XL2**2+YL2**2+ZL2**2)  
            TC1       = TP1 + DL1  /VDET                                                    
            TC2       = TP2 + DL2  /VDET   
            TC        = MIN(TC1,TC2) 
            IF(PS1*PS2.LE.ZERO)THEN            
              S1        = YL1*ZL0 - ZL1*YL0                                                    
              S2        = ZL1*XL0 - XL1*ZL0                                                    
              S3        = XL1*YL0 - YL1*XL0                                                    
              !
              P1P2      = (XL0**2+YL0**2+ZL0**2)                                               
              DL2       = (S1**2+S2**2+S3**2)/P1P2                                             
              DL(I)     = SQRT(DL2)                                                            
              P1P2      = SQRT(P1P2)                                                           
              DH        = SQRT((XL1**2+YL1**2+ZL1**2)-DL2)  !P1H 
              !
              TH1       = TP1 + DH/VDET2                                                       
              TH2       = TP2 + (P1P2-DH)/VDET2                                                
              TH        = MIN(TH1,TH2)                                                         
              TC0       = TH  + DL(I)/VDET        
              TC        = MIN(TC,TC0) 
            ENDIF                                                         
            BT(I)     = TC                                                                   
            IF(BT(I).LT.ABS(TB(I))) TB(I)=-BT(I) 
          END DO !next I                                              
        END DO !J=1,NPE

      ELSEIF(IOPT==3)THEN
 
        !CATMULL-ROM PARAMETER
        ! ALPHA = 0.0 : uniform
        ! ALPHA = 0.5 : centripetal
        ! ALPHA = 1.0 : chordal
        ALPHA = HALF

        IF(VDET2.EQ.ZERO)VDET2=VDET

        !initialize burning times by following the neutral fiber.        
        NPTS = NPE2                                                      
        NSEG = NPE2-1                                                     
        ALLOCATE(USER_SPLINE_PATH%control_point(0:NPTS+1,3))              
        ALLOCATE(USER_SPLINE_PATH%length(NPTS-1))                         
        ALLOCATE(USER_SPLINE_PATH%cumulative_length(NPTS-1)) 
        ALLOCATE(USER_SPLINE_PATH%knots(NPTS-1,4) )                  
        !PT(0    ,1:3)= will be defined by energy minimum                
        !PT(NPTS+1,1:3)= will be defined by energy minimum                

        !===CONTROL POINTS===!                                           
        DO J=1,NPE2                                                      
            I1          = N2+J-1                                         
            II          = DETONATORS%ICORD(I1)                                      
            XLP1        = X(1,II)                                        
            YLP1        = X(2,II)                                        
            ZLP1        = X(3,II)                                        
          USER_SPLINE_PATH%control_point(J,1:3)=(/XLP1, YLP1, ZLP1/)     
        ENDDO                                                            

        !===END POINTS===!                                               
        ptr=>USER_SPLINE_PATH%control_point(0:NPTS+1,1:3)                 
        !ADDING FIRST END POINTS - BENDING ENERGY MINIMUM                
        ptr(1+0,1)=0.5*(5.*ptr(1+1,1)-4.*ptr(1+2,1)+ptr(1+3,1))                  
        ptr(1+0,2)=0.5*(5.*ptr(1+1,2)-4.*ptr(1+2,2)+ptr(1+3,2))                  
        ptr(1+0,3)=0.5*(5.*ptr(1+1,3)-4.*ptr(1+2,3)+ptr(1+3,3))                  
        !ADDING LAST END POINTS - BENDING ENERGY MINIMUM                 
        ptr(1+NPTS+1,1)=0.5*(1.*ptr(1+NPTS-2,1)-4.*ptr(1+NPTS-1,1)+5.*ptr(1+NPTS,1)) 
        ptr(1+NPTS+1,2)=0.5*(1.*ptr(1+NPTS-2,2)-4.*ptr(1+NPTS-1,2)+5.*ptr(1+NPTS,2)) 
        ptr(1+NPTS+1,3)=0.5*(1.*ptr(1+NPTS-2,3)-4.*ptr(1+NPTS-1,3)+5.*ptr(1+NPTS,3)) 

        !DEFINE KNOTS                                                    
        DO J=1,NSEG                                                      
          CALL CR_SPLINE_KNOTS(PTR(1+J,1:3), KNOTS, ALPHA)
          USER_SPLINE_PATH%knots(J,1:4)=KNOTS(1:4)     
        ENDDO    

       !COMPUTE DISTANCE FROM CENTROID Z TO CONTROL POINT ptr(1+:)
       !  idist(J) is the shortest point in PT from Z(J)
       DO J=LFT,LLT                      
         DIST(J)  = 1E30                  
         IDIST(J) = 0                     
         DO I=1,NPTS                       
           XX = XC(J)-PTR(1+I,1)            
           YY = YC(J)-PTR(1+I,2)            
           ZZ = ZC(J)-PTR(1+I,3)            
           DD = SQRT(XX*XX+YY*YY+ZZ*ZZ)          
           IF(DD < DIST(J))THEN            
             DIST(J)  = DD                 
             IDIST(J) = I                 
           ENDIF                          
         ENDDO                            
       ENDDO                                                       

       !CALCUL DES LONGUEUR DE SPLINE  
       DO I=1,NSEG                                      
         LOCAL_PT(1,1:3)=PTR(1+I-1,1:3)                    
         LOCAL_PT(2,1:3)=PTR(1+I+0,1:3)                    
         LOCAL_PT(3,1:3)=PTR(1+I+1,1:3)                    
         LOCAL_PT(4,1:3)=PTR(1+I+2,1:3)                    
         T=1.0                                          
         CALL CR_SPLINE_LENGTH(LOCAL_PT,ALPHA,T,LEN)    
         USER_SPLINE_PATH%length(I) = LEN 
         IF(I>1)THEN
           USER_SPLINE_PATH%cumulative_length(I) = USER_SPLINE_PATH%cumulative_length(I-1)  + LEN 
         ELSE
           USER_SPLINE_PATH%cumulative_length(1) =  LEN
         ENDIF            
         !print *, "spline-i=", I                        
         !print *, "len =", len                                              
       ENDDO                                            

       !PROJECTION DES POINTS TESTS
       DO J=LFT,LLT
         !point le plus proche : IDIST(J)
         I=IDIST(J)
         I=MIN(I,NSEG)
         K=I
         T=0.5
         LOCAL_PT(1,1:3)=PTR(1+I-1,1:3)
         LOCAL_PT(2,1:3)=PTR(1+I+0,1:3)
         LOCAL_PT(3,1:3)=PTR(1+I+1,1:3)
         LOCAL_PT(4,1:3)=PTR(1+I+2,1:3)
         ZP(1:3) =(/XC(J),YC(J),ZC(J)/)
         CALL CR_SPLINE_POINT_PROJ(LOCAL_PT,ZP(1:3),ALPHA,ZH1,DIST1,T1)
         CALL CR_SPLINE_LENGTH(LOCAL_PT,ALPHA,T1,LEN1)
         C(1:3)=ZH1(1:3)
         T=T1 ! position [0,1]
         LEN=LEN1
         !two adjacent spline on given node IDIST(J), unless for first spline I==1
         IF(I >= 2)THEN
           I=I-1
           LOCAL_PT(1,1:3)=PTR(1+I-1,1:3)
           LOCAL_PT(2,1:3)=PTR(1+I+0,1:3)
           LOCAL_PT(3,1:3)=PTR(1+I+1,1:3)
           LOCAL_PT(4,1:3)=PTR(1+I+2,1:3)
           CALL CR_SPLINE_POINT_PROJ(LOCAL_PT,ZP(1:3),ALPHA,ZH2,DIST2,T2)
           CALL CR_SPLINE_LENGTH(LOCAL_PT,ALPHA,T2,LEN2)
           IF(DIST2<DIST1)THEN
             C(1:3)=ZH2(1:3)
             T=T2
             K=I
             LEN = LEN2
           ENDIF
         ENDIF
         !print *, "point=", ZP(1:3)
         !print *, "    ->", C(1:3)
         !!CALL CR_SPLINE_LENGTH(LOCAL_PT,ALPHA,T,LEN)
         IF(K>1)LEN=LEN+USER_SPLINE_PATH%cumulative_length(K-1)
         BT(J)     = DETONATORS%DCORD(N2) + LEN/VDET2                                                                  
         IF(BT(J).LT.ABS(TB(J))) TB(J)=-BT(J) 
       ENDDO

      ENDIF

      DO I=LFT,LLT
        JJ(I) = JJ(I)-1 
      ENDDO
      
      DTO=DTOS 

C-----------------------------------------------
      RETURN
      END SUBROUTINE DETCORD






 
Chd|====================================================================
Chd|  CR_SPLINE_KNOTS               source/initial_conditions/detonation/detcord.F
Chd|-- called by -----------
Chd|        CR_SPLINE_LENGTH              source/initial_conditions/detonation/detcord.F
Chd|        CR_SPLINE_POINT_PROJ          source/initial_conditions/detonation/detcord.F
Chd|        DETCORD                       source/initial_conditions/detonation/detcord.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE CR_SPLINE_KNOTS( PTS , KNOTS, ALPHA)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
       ! computes knots for solving 4-point spline path. 
       ! CATMULL-ROM PARAMETER
       !  ALPHA = 0.0 : uniform
       !  ALPHA = 0.5 : centripetal
       !  ALPHA = 1.0 : chordal   
       !      INPUT  - PTS(0:3,:) are 4 control points
       !      INPUT  - ALPHA CCR SPLINE PARAMETER
       !      OUTPU  - KNOTS() : t0,t1,t2,t3
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
         my_real, INTENT(IN)    :: PTS(0:3,3),  ALPHA
         my_real, INTENT(INOUT) :: KNOTS(0:3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
         my_real :: X,Y,Z
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
          KNOTS(0)=0.0
          X=PTS(1-1,1)-PTS(1,1)
          Y=PTS(1-1,2)-PTS(1,2)
          Z=PTS(1-1,3)-PTS(1,3)  
          KNOTS(1)=KNOTS(0)+ EXP(ALPHA*LOG(SQRT(X*X+Y*Y+Z*Z)))
          X=PTS(1,1)-PTS(1+1,1)
          Y=PTS(1,2)-PTS(1+1,2)
          Z=PTS(1,3)-PTS(1+1,3)  
          KNOTS(2)=KNOTS(1)+ EXP(ALPHA*LOG(SQRT(X*X+Y*Y+Z*Z)))
          X=PTS(1+1,1)-PTS(1+2,1)
          Y=PTS(1+1,2)-PTS(1+2,2)
          Z=PTS(1+1,3)-PTS(1+2,3)  
          KNOTS(3)=KNOTS(2)+ EXP(ALPHA*LOG(SQRT(X*X+Y*Y+Z*Z)))
       END SUBROUTINE CR_SPLINE_KNOTS     





       !FOR A GIVEN POSITION T in [0,1] which is position on [P1,P2], GET VALUE ON SPLINE P0-P1-P2-P3 
Chd|====================================================================
Chd|  CR_SPLINE_INTERPOL            source/initial_conditions/detonation/detcord.F
Chd|-- called by -----------
Chd|        CR_SPLINE_LENGTH              source/initial_conditions/detonation/detcord.F
Chd|        CR_SPLINE_POINT_PROJ          source/initial_conditions/detonation/detcord.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE CR_SPLINE_INTERPOL( PTS , KNOTS, T, C, C_D, C_DD)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
         ! INPUT  - PTS(0:3,:) are 4 control points
         ! INPUT  - T is in [0,1]  : T=0 => VAL = P(2,:)
         ! OUTPUT - C is interpolated value.
         ! OUTPUT - C_D is 1st derivative
         ! OUTPUT - C_DD is 2nd derivatice
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
         my_real, INTENT(IN)    :: PTS(0:3,3),KNOTS(0:3),T
         my_real, INTENT(INOUT) :: C(3),C_D(3),C_DD(3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
         my_real :: A1(3),A2(3),A3(3)
         my_real :: B1(3),B2(3)
         my_real :: A1P(3),A2P(3),A3P(3), B1P(3),B2P(3),B1PP(3),B2PP(3)
         my_real :: TT  !in [T1,T2]=[KNOT(1),KNOT(2)]
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
         TT=KNOTS(1)+T*(KNOTS(2)-KNOTS(1))
         A1(1:3)   = ((KNOTS(1)-TT)*PTS(0,1:3)+(TT-KNOTS(0))*PTS(1,1:3)) /(KNOTS(1)-KNOTS(0))
         A2(1:3)   = ((KNOTS(2)-TT)*PTS(1,1:3)+(TT-KNOTS(1))*PTS(2,1:3)) /(KNOTS(2)-KNOTS(1))
         A3(1:3)   = ((KNOTS(3)-TT)*PTS(2,1:3)+(TT-KNOTS(2))*PTS(3,1:3)) /(KNOTS(3)-KNOTS(2))
         B1(1:3)   = ((KNOTS(2)-TT)*A1(1:3) + (TT-KNOTS(0))*A2(1:3)) / (KNOTS(2)-KNOTS(0))
         B2(1:3)   = ((KNOTS(3)-TT)*A2(1:3) + (TT-KNOTS(1))*A3(1:3)) / (KNOTS(3)-KNOTS(1))
         C(1:3)    = ((KNOTS(2)-TT)*B1(1:3) + (TT-KNOTS(1))*B2(1:3)) / (KNOTS(2)-KNOTS(1)) 
         A1P(1:3)  = (PTS(1,1:3)-PTS(0,1:3))/(KNOTS(1)-KNOTS(0))
         A2P(1:3)  = (PTS(2,1:3)-PTS(1,1:3))/(KNOTS(2)-KNOTS(1))
         A3P(1:3)  = (PTS(3,1:3)-PTS(2,1:3))/(KNOTS(3)-KNOTS(2))
         B1P(1:3)  =  (A2(1:3)-A1(1:3))/(KNOTS(2)-KNOTS(0))  
     .             + (KNOTS(2)-TT)/(KNOTS(2)-KNOTS(0))*A1P(1:3)  
     .             + (TT-KNOTS(0))/(KNOTS(2)-KNOTS(0))*A2P(1:3)  
         B2P(1:3)  =  (A3(1:3)-A2(1:3))/(KNOTS(3)-KNOTS(1)) 
     .             + (KNOTS(3)-TT)/(KNOTS(3)-KNOTS(1))*A2P(1:3) 
     .             + (TT-KNOTS(1))/(KNOTS(3)-KNOTS(1))*A3P(1:3) 
         C_D(1:3)  =  (B2(1:3)-B1(1:3))/(KNOTS(2)-KNOTS(1)) 
     .             + (KNOTS(2)-TT)/(KNOTS(2)-KNOTS(1))*B1P(1:3) 
     .             + (TT-KNOTS(1))/(KNOTS(2)-KNOTS(1))*B2P(1:3)
         B1PP(1:3) =  (2.*A2P(1:3)-2.*A1P(1:3))/(KNOTS(2)-KNOTS(0))  
         B2PP(1:3) =  (2.*A3P(1:3)-2.*A2P(1:3))/(KNOTS(3)-KNOTS(1))   
         C_DD(1:3) = (2.*B2P(1:3)-2.*B1P(1:3)+(KNOTS(2)-TT)*B1PP(1:3)+(TT-KNOTS(1))*B2PP(1:3))/(KNOTS(2)-KNOTS(1))
         RETURN
       END SUBROUTINE CR_SPLINE_INTERPOL     




Chd|====================================================================
Chd|  CR_SPLINE_POINT_PROJ          source/initial_conditions/detonation/detcord.F
Chd|-- called by -----------
Chd|        DETCORD                       source/initial_conditions/detonation/detcord.F
Chd|-- calls ---------------
Chd|        CR_SPLINE_INTERPOL            source/initial_conditions/detonation/detcord.F
Chd|        CR_SPLINE_KNOTS               source/initial_conditions/detonation/detcord.F
Chd|====================================================================
       SUBROUTINE CR_SPLINE_POINT_PROJ(LOCAL_PT,Z,ALPHA,Zh,h,T)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
       !COMPUTE PROJECTION ZH ON A GIVEN POINT Z ALONG THE SPLINE INTERPOLATION
       !  INPUT  - LOCAL_PT : 4 control points P0,P1,P2,P3
       !  INPUT  - Z : TEST POINt to project
       !  INPUT  - ALPHA : CCR SPLINE PARAMETER
       !  OUTPUT - Zh : projected point
       !  OUTPUT - H : distance from spline
       !  OUTPUT - T : position [0,1] on SPLINE [T1,T2]
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        my_real,INTENT(IN)    :: LOCAL_PT(4,3),Z(3),ALPHA 
        my_real,INTENT(INOUT) :: Zh(3),h
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        my_real               :: KNOTS(4),C(3),C_D(3),C_DD(3),T
        my_real               :: F,F_D
        INTEGER            :: ITER
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
        T = 0.5
        CALL CR_SPLINE_KNOTS(LOCAL_PT, KNOTS, ALPHA)                                        
        DO ITER=1,20
          CALL CR_SPLINE_INTERPOL(LOCAL_PT,KNOTS,T,C,C_D,C_DD)
          F   = DOT_PRODUCT(C(1:3)-Z(1:3), C_D(1:3))
          F_D = DOT_PRODUCT(C_D(1:3), C_D(1:3)) + DOT_PRODUCT(C(1:3)-Z(1:3), C_DD(1:3))
          IF(F_D.NE.0.)THEN
            T=T-F/F_D
          ELSE
            IF(F.EQ.0.)EXIT
            print *, "/DFS/DETCORD : error, convergence issue"
            stop 220022
          ENDIF
          IF(T<0.)T=1E-10
          IF(T>1.)T=1.-1E-10
        ENDDO
        CALL CR_SPLINE_INTERPOL(LOCAL_PT,KNOTS,T,C,C_D,C_DD) 
        Zh(1:3) = C(1:3)
        h=SQRT(DOT_PRODUCT(C(1:3)-Z(1:3),C(1:3)-Z(1:3)))
       END SUBROUTINE CR_SPLINE_POINT_PROJ






Chd|====================================================================
Chd|  CR_SPLINE_LENGTH              source/initial_conditions/detonation/detcord.F
Chd|-- called by -----------
Chd|        DETCORD                       source/initial_conditions/detonation/detcord.F
Chd|-- calls ---------------
Chd|        CR_SPLINE_INTERPOL            source/initial_conditions/detonation/detcord.F
Chd|        CR_SPLINE_KNOTS               source/initial_conditions/detonation/detcord.F
Chd|====================================================================
       SUBROUTINE CR_SPLINE_LENGTH(LOCAL_PT,ALPHA,T,LEN)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
       !compute SPLINE length with third order method (SIMPSON)
       !  INPUT  - LOCAL_PT : 4 control points P0,P1,P2,P3
       !  INPUT  - ALPHA : CCR SPLINE PARAMETER
       !  INPUT  - T : position [0,1] on SPLINE [T1,T2].  1.0 means full spline length
       !  OUTPUT - LEN : length of the curve parametrised with t in [0,T]  T<=1.0
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        my_real,INTENT(IN)    :: LOCAL_PT(4,3),ALPHA ,T
        my_real,INTENT(INOUT) :: LEN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        my_real               :: KNOTS(4),C(3),C_D(3),C_DD(3),T_,DT, COLD(3)
        my_real               :: F,F_D,VAL(6),K(2)
        INTEGER            :: ITER,NITER
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
        CALL CR_SPLINE_KNOTS(LOCAL_PT, KNOTS, ALPHA)
        LEN = 0.
        NITER=20
         DO ITER=1,NITER-1
          !first point
          T_=T*(1.*ITER-1.)/(NITER-1.)
          CALL CR_SPLINE_INTERPOL(LOCAL_PT,KNOTS,T_,C,C_D,C_DD)
          VAL(1)=SQRT(DOT_PRODUCT(C_D(1:3),C_D(1:3)))
          K(1)=KNOTS(2)+T_*(KNOTS(3)-KNOTS(2))
          !middle point
          T_=(1.*ITER-1.)/(1.*NITER-1.) + 0.5*1./(NITER-1.)
          T_=T_*T
          CALL CR_SPLINE_INTERPOL(LOCAL_PT,KNOTS,T_,C,C_D,C_DD)
          VAL(2)=SQRT(DOT_PRODUCT(C_D(1:3),C_D(1:3)))
          !last point
          T_=T*(1.*ITER)/(NITER-1.)
          CALL CR_SPLINE_INTERPOL(LOCAL_PT,KNOTS,T_,C,C_D,C_DD)
          VAL(3)=SQRT(DOT_PRODUCT(C_D(1:3),C_D(1:3)))
          K(2)=KNOTS(2)+T_*(KNOTS(3)-KNOTS(2))
          !length
          LEN = LEN+(K(2)-K(1))/6. * (VAL(1)+4.*VAL(2)+VAL(3))
        ENDDO
       END SUBROUTINE CR_SPLINE_LENGTH

