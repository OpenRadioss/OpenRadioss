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
Chd|  SCOOR3                        source/elements/solid/solide/scoor3.F
Chd|-- called by -----------
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- calls ---------------
Chd|        SORTHDIR3                     source/elements/solid/solide/sorthdir3.F
Chd|        SREPISO3                      source/elements/solid/solide/srepiso3.F
Chd|        SREPLOC3                      source/elements/solid/solide/sreploc3.F
Chd|====================================================================
      SUBROUTINE SCOOR3(X,IXS,V,W,GAMA0,GAMA,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .   VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .   VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .   VDX1, VDX2, VDX3, VDX4, VDX5, VDX6, VDX7, VDX8,
     .   VDY1, VDY2, VDY3, VDY4, VDY5, VDY6, VDY7, VDY8,
     .   VDZ1, VDZ2, VDZ3, VDZ4, VDZ5, VDZ6, VDZ7, VDZ8,
     .   VDX,VDY,VDZ,VD2,VIS,OFFG,OFF,SAV,RHO,
     .   RHOO,NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8,NGL,MXT,NGEO,
     .   VR,VXR,VYR,VZR,FR_WAVE,FR_WAV,
     .   XD1, XD2  , XD3, XD4, XD5, XD6, XD7, XD8,
     .   YD1, YD2  , YD3, YD4, YD5, YD6, YD7, YD8,
     .   ZD1, ZD2  , ZD3, ZD4, ZD5, ZD6, ZD7, ZD8,
     .   XDP, IPARG, NG , NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "scr05_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IPARG(NPARG,*),NG,NEL
      my_real
     .  X(3,*),V(3,*),W(3,*), VIS(*),VR(3,*),VXR(*),VYR(*),VZR(*),                           
     .  X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*), 
     .  Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*), 
     .  Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*), 
     .  VX1(*), VX2(*), VX3(*), VX4(*), VX5(*), VX6(*), VX7(*), VX8(*),
     .  VY1(*), VY2(*), VY3(*), VY4(*), VY5(*), VY6(*), VY7(*), VY8(*),
     .  VZ1(*), VZ2(*), VZ3(*), VZ4(*), VZ5(*), VZ6(*), VZ7(*), VZ8(*),
     .  VDX1(*),VDX2(*),VDX3(*),VDX4(*),VDX5(*),VDX6(*),VDX7(*),VDX8(*),
     .  VDY1(*),VDY2(*),VDY3(*),VDY4(*),VDY5(*),VDY6(*),VDY7(*),VDY8(*),
     .  VDZ1(*),VDZ2(*),VDZ3(*),VDZ4(*),VDZ5(*),VDZ6(*),VDZ7(*),VDZ8(*),
     .  VDX(*), VDY(*), VDZ(*),VD2(*),OFFG(*),OFF(*),RHO(*),
     .  RHOO(*),GAMA0(NEL,6),GAMA(MVSIZ,6),FR_WAVE(*),FR_WAV(*)
               
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*),
     .        NC5(*), NC6(*), NC7(*), NC8(*), MXT(*), NGL(*),NGEO(*)
     
      DOUBLE PRECISION 
     .  XDP(3,*),SAV(NEL,21),      
     .  XD1(*), XD2(*), XD3(*), XD4(*), XD5(*), XD6(*), XD7(*), XD8(*), 
     .  YD1(*), YD2(*), YD3(*), YD4(*), YD5(*), YD6(*), YD7(*), YD8(*), 
     .  ZD1(*), ZD2(*), ZD3(*), ZD4(*), ZD5(*), ZD6(*), ZD7(*), ZD8(*)     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,MXT_1
      my_real, DIMENSION(MVSIZ) ::
     .   RX, RY, RZ, SX, SY, SZ, TX, TY, TZ,
     .   E1X, E1Y, E1Z, E2X, E2Y, E2Z, E3X, E3Y, E3Z
      my_real
     .   OFF_L
C=======================================================================
      MXT_1 = IXS(1,LFT)
      DO I=LFT,LLT
        VIS(I)=ZERO
        NGEO(I)=IXS(10,I)
        NGL(I)=IXS(11,I)
        MXT(I)=MXT_1
        NC1(I)=IXS(2,I)
        NC2(I)=IXS(3,I)
        NC3(I)=IXS(4,I)
        NC4(I)=IXS(5,I)
        NC5(I)=IXS(6,I)
        NC6(I)=IXS(7,I)
        NC7(I)=IXS(8,I)
        NC8(I)=IXS(9,I)
        RHOO(I)=RHO(I)
      ENDDO

      OFF_L  = ZERO
      !----------------------------
      !     NODAL COORDINATES     |
      !----------------------------
      IF(IRESP==1)THEN 
        DO I=LFT,LLT
          XD1(I)=XDP(1,NC1(I))  
          YD1(I)=XDP(2,NC1(I))  
          ZD1(I)=XDP(3,NC1(I))  
          XD2(I)=XDP(1,NC2(I))  
          YD2(I)=XDP(2,NC2(I))  
          ZD2(I)=XDP(3,NC2(I))  
          XD3(I)=XDP(1,NC3(I))  
          YD3(I)=XDP(2,NC3(I))  
          ZD3(I)=XDP(3,NC3(I))  
          XD4(I)=XDP(1,NC4(I))  
          YD4(I)=XDP(2,NC4(I))  
          ZD4(I)=XDP(3,NC4(I))  
          XD5(I)=XDP(1,NC5(I))  
          YD5(I)=XDP(2,NC5(I))  
          ZD5(I)=XDP(3,NC5(I))  
          XD6(I)=XDP(1,NC6(I))  
          YD6(I)=XDP(2,NC6(I))  
          ZD6(I)=XDP(3,NC6(I))  
          XD7(I)=XDP(1,NC7(I))  
          YD7(I)=XDP(2,NC7(I))  
          ZD7(I)=XDP(3,NC7(I))  
          XD8(I)=XDP(1,NC8(I))  
          YD8(I)=XDP(2,NC8(I))  
          ZD8(I)=XDP(3,NC8(I))  	  	  	  
        ENDDO
      ELSE	
        DO I=LFT,LLT
          XD1(I)=X(1,NC1(I))  
          YD1(I)=X(2,NC1(I))  
          ZD1(I)=X(3,NC1(I))  
          XD2(I)=X(1,NC2(I))  
          YD2(I)=X(2,NC2(I))  
          ZD2(I)=X(3,NC2(I))  
          XD3(I)=X(1,NC3(I))  
          YD3(I)=X(2,NC3(I))  
          ZD3(I)=X(3,NC3(I))  
          XD4(I)=X(1,NC4(I))  
          YD4(I)=X(2,NC4(I))  
          ZD4(I)=X(3,NC4(I))  
          XD5(I)=X(1,NC5(I))  
          YD5(I)=X(2,NC5(I))  
          ZD5(I)=X(3,NC5(I))  
          XD6(I)=X(1,NC6(I))  
          YD6(I)=X(2,NC6(I))  
          ZD6(I)=X(3,NC6(I))  
          XD7(I)=X(1,NC7(I))  
          YD7(I)=X(2,NC7(I))  
          ZD7(I)=X(3,NC7(I))  
          XD8(I)=X(1,NC8(I))  
          YD8(I)=X(2,NC8(I))  
          ZD8(I)=X(3,NC8(I))  
        ENDDO      
      ENDIF                   

      CALL SREPISO3(
     .       XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8,
     .       YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8,
     .       ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8,
     .       RX, RY, RZ, SX, SY, SZ, TX, TY, TZ )
      CALL SREPLOC3(
     .       RX , RY , RZ , SX , SY , SZ , TX , TY , TZ  ,
     .       E1X, E2X, E3X, E1Y, E2Y, E3Y, E1Z, E2Z, E3Z )
      IF (ISORTH == 0) THEN            
        DO I=LFT,LLT                                            
          GAMA(I,1) = ONE                               
          GAMA(I,2) = ZERO                                
          GAMA(I,3) = ZERO             
          GAMA(I,4) = ZERO                                
          GAMA(I,5) = ONE                                
          GAMA(I,6) = ZERO             
        ENDDO                          
      ELSE                             
        CALL SORTHDIR3(
     .       RX , RY , RZ , SX , SY , SZ , TX , TY , TZ  ,
     .       E1X, E2X, E3X, E1Y, E2Y, E3Y, E1Z, E2Z, E3Z ,
     .       GAMA0,GAMA )
      ENDIF                             
      IF(ISMSTR<=4.AND.JLAG>0.OR.(ISMSTR==12.AND.IDTMIN(1)==3)) THEN
C
       DO I=LFT,LLT
         IF(ABS(OFFG(I))>ONE)THEN
          XD1(I)=SAV(I,1)
          YD1(I)=SAV(I,2)
          ZD1(I)=SAV(I,3)
          XD2(I)=SAV(I,4)
          YD2(I)=SAV(I,5)
          ZD2(I)=SAV(I,6)
          XD3(I)=SAV(I,7)
          YD3(I)=SAV(I,8)
          ZD3(I)=SAV(I,9)
          XD4(I)=SAV(I,10)
          YD4(I)=SAV(I,11)
          ZD4(I)=SAV(I,12)
          XD5(I)=SAV(I,13)
          YD5(I)=SAV(I,14)
          ZD5(I)=SAV(I,15)
          XD6(I)=SAV(I,16)
          YD6(I)=SAV(I,17)
          ZD6(I)=SAV(I,18)
          XD7(I)=SAV(I,19)
          YD7(I)=SAV(I,20)
          ZD7(I)=SAV(I,21)
          XD8(I)=ZERO
          YD8(I)=ZERO
          ZD8(I)=ZERO
          OFF(I) = ABS(OFFG(I))-ONE
          OFF_L  = MIN(OFF_L,OFFG(I))
         ELSE
          OFF(I) = ABS(OFFG(I))
          OFF_L  = MIN(OFF_L,OFFG(I))
         ENDIF
       ENDDO

      ELSE
        DO I=LFT,LLT
         OFF(I) = ABS(OFFG(I))
         OFF_L  = MIN(OFF_L,OFFG(I))
        ENDDO
      ENDIF

      !copy and cast XD (DP) to X (SP) to assure coherence between XD and X      
      DO I=LFT,LLT
        X1(I)= XD1(I) 
        Y1(I)= YD1(I) 
        Z1(I)= ZD1(I)
        X2(I)= XD2(I)  
        Y2(I)= YD2(I)  
        Z2(I)= ZD2(I) 
        X3(I)= XD3(I) 
        Y3(I)= YD3(I) 
        Z3(I)= ZD3(I) 
        X4(I)= XD4(I) 
        Y4(I)= YD4(I) 
        Z4(I)= ZD4(I) 
        X5(I)= XD5(I) 
        Y5(I)= YD5(I)
        Z5(I)= ZD5(I) 
        X6(I)= XD6(I) 
        Y6(I)= YD6(I) 
        Z6(I)= ZD6(I) 
        X7(I)= XD7(I) 
        Y7(I)= YD7(I) 
        Z7(I)= ZD7(I) 
        X8(I)= XD8(I) 
        Y8(I)= YD8(I) 
        Z8(I)= ZD8(I) 
      ENDDO    
            
      DO I=LFT,LLT
       VX1(I)=V(1,NC1(I))
       VY1(I)=V(2,NC1(I))
       VZ1(I)=V(3,NC1(I))
       VX2(I)=V(1,NC2(I))
       VY2(I)=V(2,NC2(I))
       VZ2(I)=V(3,NC2(I))
       VX3(I)=V(1,NC3(I))
       VY3(I)=V(2,NC3(I))
       VZ3(I)=V(3,NC3(I))
       VX4(I)=V(1,NC4(I))
       VY4(I)=V(2,NC4(I))
       VZ4(I)=V(3,NC4(I))
       VX5(I)=V(1,NC5(I))
       VY5(I)=V(2,NC5(I))
       VZ5(I)=V(3,NC5(I))
       VX6(I)=V(1,NC6(I))
       VY6(I)=V(2,NC6(I))
       VZ6(I)=V(3,NC6(I))
       VX7(I)=V(1,NC7(I))
       VY7(I)=V(2,NC7(I))
       VZ7(I)=V(3,NC7(I))
       VX8(I)=V(1,NC8(I))
       VY8(I)=V(2,NC8(I))
       VZ8(I)=V(3,NC8(I))
      ENDDO
      
      IF(ISROT/=0)THEN
       DO I=LFT,LLT
        VXR(I)=(VR(1,NC1(I))+VR(1,NC2(I))+VR(1,NC3(I))+VR(1,NC4(I)) + VR(1,NC5(I))+VR(1,NC6(I))+VR(1,NC7(I))+VR(1,NC8(I)))/EIGHT
        VYR(I)=(VR(2,NC1(I))+VR(2,NC2(I))+VR(2,NC3(I))+VR(2,NC4(I)) + VR(2,NC5(I))+VR(2,NC6(I))+VR(2,NC7(I))+VR(2,NC8(I)))/EIGHT
        VZR(I)=(VR(3,NC1(I))+VR(3,NC2(I))+VR(3,NC3(I))+VR(3,NC4(I)) + VR(3,NC5(I))+VR(3,NC6(I))+VR(3,NC7(I))+VR(3,NC8(I)))/EIGHT
        FR_WAV(I)=MAX(
     +  FR_WAVE(NC1(I)),FR_WAVE(NC2(I)),FR_WAVE(NC3(I)),FR_WAVE(NC4(I)),
     +  FR_WAVE(NC5(I)),FR_WAVE(NC6(I)),FR_WAVE(NC7(I)),FR_WAVE(NC8(I)))
       ENDDO
      ENDIF
      
      IF(OFF_L<ZERO)THEN
        DO I=LFT,LLT
          IF(OFFG(I)<ZERO)THEN
            VX1(I)=ZERO
            VY1(I)=ZERO
            VZ1(I)=ZERO
            VX2(I)=ZERO
            VY2(I)=ZERO
            VZ2(I)=ZERO
            VX3(I)=ZERO
            VY3(I)=ZERO
            VZ3(I)=ZERO
            VX4(I)=ZERO
            VY4(I)=ZERO
            VZ4(I)=ZERO
            VX5(I)=ZERO
            VY5(I)=ZERO
            VZ5(I)=ZERO
            VX6(I)=ZERO
            VY6(I)=ZERO
            VZ6(I)=ZERO
            VX7(I)=ZERO
            VY7(I)=ZERO
            VZ7(I)=ZERO
            VX8(I)=ZERO
            VY8(I)=ZERO
            VZ8(I)=ZERO
          ENDIF
        ENDDO
      ENDIF

      IF (JLAG/=0)THEN
       DO I=LFT,LLT
        VD2(I)=ZERO
	VDX1(I)=ZERO
	VDY1(I)=ZERO
	VDZ1(I)=ZERO
	VDX2(I)=ZERO
	VDY2(I)=ZERO
	VDZ2(I)=ZERO
	VDX3(I)=ZERO
	VDY3(I)=ZERO
	VDZ3(I)=ZERO
	VDX4(I)=ZERO
	VDY4(I)=ZERO
	VDZ4(I)=ZERO
	VDX5(I)=ZERO
	VDY5(I)=ZERO
	VDZ5(I)=ZERO
	VDX6(I)=ZERO
	VDY6(I)=ZERO
	VDZ6(I)=ZERO
	VDX7(I)=ZERO
	VDY7(I)=ZERO
	VDZ7(I)=ZERO
	VDX8(I)=ZERO
	VDY8(I)=ZERO
	VDZ8(I)=ZERO
       ENDDO
       RETURN
      ELSEIF(JALE/=0)THEN
       IF(IPARG(64,NG)==1)THEN
         DO I=LFT,LLT
         VD2(I) =ZERO
	 VDX1(I)=ZERO
	 VDY1(I)=ZERO
	 VDZ1(I)=ZERO
	 VDX2(I)=ZERO
	 VDY2(I)=ZERO
	 VDZ2(I)=ZERO
	 VDX3(I)=ZERO
	 VDY3(I)=ZERO
	 VDZ3(I)=ZERO
	 VDX4(I)=ZERO
	 VDY4(I)=ZERO
	 VDZ4(I)=ZERO
	 VDX5(I)=ZERO
	 VDY5(I)=ZERO
	 VDZ5(I)=ZERO
	 VDX6(I)=ZERO
	 VDY6(I)=ZERO
	 VDZ6(I)=ZERO
	 VDX7(I)=ZERO
	 VDY7(I)=ZERO
	 VDZ7(I)=ZERO
	 VDX8(I)=ZERO
	 VDY8(I)=ZERO
	 VDZ8(I)=ZERO
         ENDDO
         RETURN
       ENDIF
       DO I=LFT,LLT
        VDX1(I)=VX1(I)-W(1,NC1(I))
        VDY1(I)=VY1(I)-W(2,NC1(I))
        VDZ1(I)=VZ1(I)-W(3,NC1(I))
        VDX2(I)=VX2(I)-W(1,NC2(I))
        VDY2(I)=VY2(I)-W(2,NC2(I))
        VDZ2(I)=VZ2(I)-W(3,NC2(I))
        VDX3(I)=VX3(I)-W(1,NC3(I))
        VDY3(I)=VY3(I)-W(2,NC3(I))
        VDZ3(I)=VZ3(I)-W(3,NC3(I))
        VDX4(I)=VX4(I)-W(1,NC4(I))
        VDY4(I)=VY4(I)-W(2,NC4(I))
        VDZ4(I)=VZ4(I)-W(3,NC4(I))
        VDX5(I)=VX5(I)-W(1,NC5(I))
        VDY5(I)=VY5(I)-W(2,NC5(I))
        VDZ5(I)=VZ5(I)-W(3,NC5(I))
        VDX6(I)=VX6(I)-W(1,NC6(I))
        VDY6(I)=VY6(I)-W(2,NC6(I))
        VDZ6(I)=VZ6(I)-W(3,NC6(I))
        VDX7(I)=VX7(I)-W(1,NC7(I))
        VDY7(I)=VY7(I)-W(2,NC7(I))
        VDZ7(I)=VZ7(I)-W(3,NC7(I))
        VDX8(I)=VX8(I)-W(1,NC8(I))
        VDY8(I)=VY8(I)-W(2,NC8(I))
        VDZ8(I)=VZ8(I)-W(3,NC8(I))
       ENDDO
      ELSEIF(JEUL/=0)THEN
       IF(IPARG(64,NG)==1)THEN
         DO I=LFT,LLT
         VD2(I) =ZERO
         VDX1(I)=ZERO
	 VDY1(I)=ZERO
	 VDZ1(I)=ZERO
	 VDX2(I)=ZERO
	 VDY2(I)=ZERO
	 VDZ2(I)=ZERO
	 VDX3(I)=ZERO
	 VDY3(I)=ZERO
	 VDZ3(I)=ZERO
	 VDX4(I)=ZERO
	 VDY4(I)=ZERO
	 VDZ4(I)=ZERO
	 VDX5(I)=ZERO
	 VDY5(I)=ZERO
	 VDZ5(I)=ZERO
	 VDX6(I)=ZERO
	 VDY6(I)=ZERO
	 VDZ6(I)=ZERO
	 VDX7(I)=ZERO
	 VDY7(I)=ZERO
	 VDZ7(I)=ZERO
	 VDX8(I)=ZERO
	 VDY8(I)=ZERO
	 VDZ8(I)=ZERO
         ENDDO
         RETURN
       ENDIF      
       DO I=LFT,LLT
        VDX1(I)=VX1(I)
        VDY1(I)=VY1(I)
        VDZ1(I)=VZ1(I)
        VDX2(I)=VX2(I)
        VDY2(I)=VY2(I)
        VDZ2(I)=VZ2(I)
        VDX3(I)=VX3(I)
        VDY3(I)=VY3(I)
        VDZ3(I)=VZ3(I)
        VDX4(I)=VX4(I)
        VDY4(I)=VY4(I)
        VDZ4(I)=VZ4(I)
        VDX5(I)=VX5(I)
        VDY5(I)=VY5(I)
        VDZ5(I)=VZ5(I)
        VDX6(I)=VX6(I)
        VDY6(I)=VY6(I)
        VDZ6(I)=VZ6(I)
        VDX7(I)=VX7(I)
        VDY7(I)=VY7(I)
        VDZ7(I)=VZ7(I)
        VDX8(I)=VX8(I)
        VDY8(I)=VY8(I)
        VDZ8(I)=VZ8(I)
       ENDDO
      ENDIF

      DO I=LFT,LLT
       VDX(I)=ONE_OVER_8*(VDX1(I)+VDX2(I)+VDX3(I)+VDX4(I)+VDX5(I)+VDX6(I)+VDX7(I)+VDX8(I))
       VDY(I)=ONE_OVER_8*(VDY1(I)+VDY2(I)+VDY3(I)+VDY4(I)+VDY5(I)+VDY6(I)+VDY7(I)+VDY8(I))
       VDZ(I)=ONE_OVER_8*(VDZ1(I)+VDZ2(I)+VDZ3(I)+VDZ4(I)+VDZ5(I)+VDZ6(I)+VDZ7(I)+VDZ8(I))
       VD2(I)=(VDX(I)**2+VDY(I)**2+VDZ(I)**2)  
      ENDDO
C-----------     
      RETURN
      END
