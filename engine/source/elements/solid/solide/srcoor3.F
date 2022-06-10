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
Chd|  SRCOOR3                       source/elements/solid/solide/srcoor3.F
Chd|-- called by -----------
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SCFORC3                       source/elements/thickshell/solidec/scforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|        SCORTHO3                      source/elements/thickshell/solidec/scortho3.F
Chd|        SORTHDIR3                     source/elements/solid/solide/sorthdir3.F
Chd|        SORTHO3                       source/elements/solid/solide/sortho3.F
Chd|        SREPISO3                      source/elements/solid/solide/srepiso3.F
Chd|        SRROTA3                       source/elements/solid/solide/srrota3.F
Chd|====================================================================
      SUBROUTINE SRCOOR3(X   ,IXS   ,V     ,W     ,GAMA0 ,GAMA  ,
     .   X1    ,X2    ,X3    ,X4    ,X5    ,X6    ,X7    ,X8    ,
     .   Y1    ,Y2    ,Y3    ,Y4    ,Y5    ,Y6    ,Y7    ,Y8    ,
     .   Z1    ,Z2    ,Z3    ,Z4    ,Z5    ,Z6    ,Z7    ,Z8    ,
     .   VX1   ,VX2   ,VX3   ,VX4   ,VX5   ,VX6   ,VX7   ,VX8   , 
     .   VY1   ,VY2   ,VY3   ,VY4   ,VY5   ,VY6   ,VY7   ,VY8   , 
     .   VZ1   ,VZ2   ,VZ3   ,VZ4   ,VZ5   ,VZ6   ,VZ7   ,VZ8   , 
     .   VD2   ,VIS   ,OFFG  ,OFF   ,SAV   ,RHO   ,RHOO  ,R11   ,
     .   R12   ,R13   ,R21   ,R22   ,R23   ,R31   ,R32   ,R33   ,
     .   NC1   ,NC2   ,NC3   ,NC4   ,NC5   ,NC6   ,NC7   ,NC8   ,
     .   NGL   ,MXT   ,NGEO  ,IOUTPRT,VGAX ,VGAY  ,VGAZ  ,VGA2  ,
     .   XD1   ,XD2   ,XD3   ,XD4   ,XD5   ,XD6   ,XD7   ,XD8   ,
     .   YD1   ,YD2   ,YD3   ,YD4   ,YD5   ,YD6   ,YD7   ,YD8   ,
     .   ZD1   ,ZD2   ,ZD3   ,ZD4   ,ZD5   ,ZD6   ,ZD7   ,ZD8   ,         
     .   XDP   ,X0    ,Y0    ,Z0    ,NEL   ,XGAX  ,XGAY  ,XGAZ  ,
     .   XGXA2,XGYA2  ,XGZA2 ,XGXYA ,XGYZA ,XGZXA ,IPARG ,GAMA_R) 
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
      INTEGER NEL
C     REAL
      my_real
     .   X(3,*),V(3,*),W(3,*), VIS(*),
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*),
     .   VX1(*), VX2(*), VX3(*), VX4(*), VX5(*), VX6(*), VX7(*), VX8(*),
     .   VY1(*), VY2(*), VY3(*), VY4(*), VY5(*), VY6(*), VY7(*), VY8(*),
     .   VZ1(*), VZ2(*), VZ3(*), VZ4(*), VZ5(*), VZ6(*), VZ7(*), VZ8(*),
     .   VD2(*), OFFG(*), OFF(*),  RHO(*), RHOO(*),
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .   R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),
     .   GAMA0(NEL,6),GAMA(MVSIZ,6) ,VGAX(*), VGAY(*), VGAZ(*), VGA2(*),
     .   XGAX(*), XGAY(*), XGAZ(*),
     .   XGXA2(MVSIZ),XGYA2(MVSIZ),XGZA2(MVSIZ),
     .   XGXYA(MVSIZ),XGYZA(MVSIZ),XGZXA(MVSIZ),GAMA_R(NEL,6)
      INTEGER IXS(NIXS,*), MXT(*), NGL(*),NGEO(*),IPARG(*),
     .        NC1(*),NC2(*),NC3(*),NC4(*),NC5(*),NC6(*),NC7(*),NC8(*)
      INTEGER IOUTPRT
      
      DOUBLE PRECISION 
     .   XDP(3,*),X0(MVSIZ,8),Y0(MVSIZ,8),Z0(MVSIZ,8),SAV(NEL,21),
     .   XD1(*), XD2(*), XD3(*), XD4(*), XD5(*), XD6(*), XD7(*), XD8(*), 
     .   YD1(*), YD2(*), YD3(*), YD4(*), YD5(*), YD6(*), YD7(*), YD8(*), 
     .   ZD1(*), ZD2(*), ZD3(*), ZD4(*), ZD5(*), ZD6(*), ZD7(*), ZD8(*)      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      
      DOUBLE PRECISION
     .   XDL, YDL, ZDL
    
C     REAL
      my_real
     .  DT05     
      my_real
     .   G11,G12,G13,
     .   G21,G22,G23,
     .   G31,G32,G33,
     .   T11,T12,T13,
     .   T21,T22,T23,
     .   T31,T32,T33,
     .   RX(MVSIZ) , RY(MVSIZ) , RZ(MVSIZ) ,
     .   SX(MVSIZ) , SY(MVSIZ) , SZ(MVSIZ) ,
     .   TX(MVSIZ) , TY(MVSIZ) , TZ(MVSIZ) 
      my_real
     .   XL(MVSIZ),YL(MVSIZ),ZL(MVSIZ)
      my_real
     .   OFF_L
C=======================================================================
      OFF_L  = ZERO
C
      DO I=LFT,LLT
      VIS(I)=ZERO
      NGEO(I)=IXS(10,I)
      NGL(I)=IXS(11,I)
      MXT(I)=IXS(1,I)
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
C----------------------------
C     NODAL COORDINATES     |
C----------------------------
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
C-----------
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
C-----------
C     Prepare les sorties par part.
C-----------
      IF (IOUTPRT /= 0) THEN
       DO I=LFT,LLT
        VGAX(I)=VX1(I)+VX2(I)+VX3(I)+VX4(I)+VX5(I)+VX6(I)+VX7(I)+VX8(I)
        VGAY(I)=VY1(I)+VY2(I)+VY3(I)+VY4(I)+VY5(I)+VY6(I)+VY7(I)+VY8(I)
        VGAZ(I)=VZ1(I)+VZ2(I)+VZ3(I)+VZ4(I)+VZ5(I)+VZ6(I)+VZ7(I)+VZ8(I)
        VGA2(I)=VX1(I)*VX1(I)+VX2(I)*VX2(I)+VX3(I)*VX3(I)+VX4(I)*VX4(I)
     1         +VX5(I)*VX5(I)+VX6(I)*VX6(I)+VX7(I)*VX7(I)+VX8(I)*VX8(I)
     2         +VY1(I)*VY1(I)+VY2(I)*VY2(I)+VY3(I)*VY3(I)+VY4(I)*VY4(I)
     3         +VY5(I)*VY5(I)+VY6(I)*VY6(I)+VY7(I)*VY7(I)+VY8(I)*VY8(I)
     4         +VZ1(I)*VZ1(I)+VZ2(I)*VZ2(I)+VZ3(I)*VZ3(I)+VZ4(I)*VZ4(I)
     5         +VZ5(I)*VZ5(I)+VZ6(I)*VZ6(I)+VZ7(I)*VZ7(I)+VZ8(I)*VZ8(I)
       ENDDO
       IF(IPARG(80).EQ.1) THEN
         DO I=LFT,LLT
          XGAX(I)=XD1(I)+XD2(I)+XD3(I)+XD4(I)+XD5(I)+XD6(I)+XD7(I)+XD8(I)
          XGAY(I)=YD1(I)+YD2(I)+YD3(I)+YD4(I)+YD5(I)+YD6(I)+YD7(I)+YD8(I)
          XGAZ(I)=ZD1(I)+ZD2(I)+ZD3(I)+ZD4(I)+ZD5(I)+ZD6(I)+ZD7(I)+ZD8(I)
          XGXA2(I)=XD1(I)**2+XD2(I)**2+XD3(I)**2+XD4(I)**2
     1            +XD5(I)**2+XD6(I)**2+XD7(I)**2+XD8(I)**2
          XGYA2(I)=YD1(I)**2+YD2(I)**2+YD3(I)**2+YD4(I)**2
     1            +YD5(I)**2+YD6(I)**2+YD7(I)**2+YD8(I)**2
          XGZA2(I)=ZD1(I)**2+ZD2(I)**2+ZD3(I)**2+ZD4(I)**2
     1            +ZD5(I)**2+ZD6(I)**2+ZD7(I)**2+ZD8(I)**2
          XGXYA(I)=XD1(I)*YD1(I)+XD2(I)*YD2(I)+XD3(I)*YD3(I)+XD4(I)*YD4(I)
     1            +XD5(I)*YD5(I)+XD6(I)*YD6(I)+XD7(I)*YD7(I)+XD8(I)*YD8(I)
          XGYZA(I)=YD1(I)*ZD1(I)+YD2(I)*ZD2(I)+YD3(I)*ZD3(I)+YD4(I)*ZD4(I)
     1            +YD5(I)*ZD5(I)+YD6(I)*ZD6(I)+YD7(I)*ZD7(I)+YD8(I)*ZD8(I)
          XGZXA(I)=ZD1(I)*XD1(I)+ZD2(I)*XD2(I)+ZD3(I)*XD3(I)+ZD4(I)*XD4(I)
     1            +ZD5(I)*XD5(I)+ZD6(I)*XD6(I)+ZD7(I)*XD7(I)+ZD8(I)*XD8(I)
         ENDDO
       ENDIF
      ENDIF
C-----------
C     REPERE CONVECTE (ITERATIONS).
C-----------
      CALL SREPISO3(
     .       XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8,
     .       YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8,
     .       ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8,
     .       RX, RY, RZ, SX, SY, SZ, TX, TY, TZ )
C---
      IF (JHBE == 14 .OR. JHBE == 24) THEN
        CALL SORTHO3(
     .       RX , RY , RZ , SX , SY , SZ , TX , TY , TZ ,
     .       R12, R13, R11, R22, R23, R21, R32, R33, R31)
      ELSEIF (JHBE == 15 ) THEN
        CALL SCORTHO3(
     .       RX , RY , RZ , SX , SY , SZ , TX , TY , TZ ,
     .       R11, R12, R13, R21, R22, R23, R31, R32, R33)
      ELSE
        CALL SORTHO3(
     .       RX , RY , RZ , SX , SY , SZ , TX , TY , TZ ,
     .       R11, R12, R13, R21, R22, R23, R31, R32, R33)
      ENDIF
C------stocker [R] in %GAMA_R--------------     
       GAMA_R(LFT:LLT,1) = R11(LFT:LLT) ! Dir1_x                            
       GAMA_R(LFT:LLT,2) = R21(LFT:LLT) ! Dir1_y                            
       GAMA_R(LFT:LLT,3) = R31(LFT:LLT) ! Dir1_z                            
       GAMA_R(LFT:LLT,4) = R12(LFT:LLT) ! Dir2_x                            
       GAMA_R(LFT:LLT,5) = R22(LFT:LLT) ! Dir2_y                           
       GAMA_R(LFT:LLT,6) = R32(LFT:LLT) ! Dir2_z                            
C-------sauf thick shells --------------     
      IF (IGTYP /= 21 .AND. IGTYP /= 22) THEN
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
     .        RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   , 
     .        R11  ,R12  ,R13  ,R21  ,R22  ,R23  ,R31  ,R32  ,R33  , 
     .        GAMA0,GAMA )                                           
        ENDIF
      ENDIF
C-----------
C     PASSAGE AU REPERE CONVECTE.
C-----------
Clm   X=RX' <=> X'=t(R)X chgt de base.
      IF((ISMSTR<=4.OR.(ISMSTR==12.AND.IDTMIN(1)==3)).AND.JLAG>0) THEN
        DO I=LFT,LLT
          IF(ABS(OFFG(I)) > ONE)THEN
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
            XDL=R11(I)*XD1(I)+R21(I)*YD1(I)+R31(I)*ZD1(I)
            YDL=R12(I)*XD1(I)+R22(I)*YD1(I)+R32(I)*ZD1(I)
            ZDL=R13(I)*XD1(I)+R23(I)*YD1(I)+R33(I)*ZD1(I)
            XD1(I)=XDL
            YD1(I)=YDL
            ZD1(I)=ZDL
            XDL=R11(I)*XD2(I)+R21(I)*YD2(I)+R31(I)*ZD2(I)
            YDL=R12(I)*XD2(I)+R22(I)*YD2(I)+R32(I)*ZD2(I)
            ZDL=R13(I)*XD2(I)+R23(I)*YD2(I)+R33(I)*ZD2(I)
            XD2(I)=XDL
            YD2(I)=YDL
            ZD2(I)=ZDL
            XDL=R11(I)*XD3(I)+R21(I)*YD3(I)+R31(I)*ZD3(I)
            YDL=R12(I)*XD3(I)+R22(I)*YD3(I)+R32(I)*ZD3(I)
            ZDL=R13(I)*XD3(I)+R23(I)*YD3(I)+R33(I)*ZD3(I)
            XD3(I)=XDL
            YD3(I)=YDL
            ZD3(I)=ZDL
            XDL=R11(I)*XD4(I)+R21(I)*YD4(I)+R31(I)*ZD4(I)
            YDL=R12(I)*XD4(I)+R22(I)*YD4(I)+R32(I)*ZD4(I)
            ZDL=R13(I)*XD4(I)+R23(I)*YD4(I)+R33(I)*ZD4(I)
            XD4(I)=XDL
            YD4(I)=YDL
            ZD4(I)=ZDL
            XDL=R11(I)*XD5(I)+R21(I)*YD5(I)+R31(I)*ZD5(I)
            YDL=R12(I)*XD5(I)+R22(I)*YD5(I)+R32(I)*ZD5(I)
            ZDL=R13(I)*XD5(I)+R23(I)*YD5(I)+R33(I)*ZD5(I)
            XD5(I)=XDL
            YD5(I)=YDL
            ZD5(I)=ZDL
            XDL=R11(I)*XD6(I)+R21(I)*YD6(I)+R31(I)*ZD6(I)
            YDL=R12(I)*XD6(I)+R22(I)*YD6(I)+R32(I)*ZD6(I)
            ZDL=R13(I)*XD6(I)+R23(I)*YD6(I)+R33(I)*ZD6(I)
            XD6(I)=XDL
            YD6(I)=YDL
            ZD6(I)=ZDL
            XDL=R11(I)*XD7(I)+R21(I)*YD7(I)+R31(I)*ZD7(I)
            YDL=R12(I)*XD7(I)+R22(I)*YD7(I)+R32(I)*ZD7(I)
            ZDL=R13(I)*XD7(I)+R23(I)*YD7(I)+R33(I)*ZD7(I)
            XD7(I)=XDL
            YD7(I)=YDL
            ZD7(I)=ZDL
            XDL=R11(I)*XD8(I)+R21(I)*YD8(I)+R31(I)*ZD8(I)
            YDL=R12(I)*XD8(I)+R22(I)*YD8(I)+R32(I)*ZD8(I)
            ZDL=R13(I)*XD8(I)+R23(I)*YD8(I)+R33(I)*ZD8(I)
            XD8(I)=XDL
            YD8(I)=YDL
            ZD8(I)=ZDL		    	    
            OFF(I) = ABS(OFFG(I))
            OFF_L  = MIN(OFF_L,OFFG(I))
          ENDIF
        ENDDO
        IF((ISMSTR==12.AND.IDTMIN(1)==3).AND.JLAG>0) THEN
         DO I=LFT,LLT	  
          IF(ABS(OFFG(I)) > ONE)THEN
          XDL=R11(I)*XD1(I)+R21(I)*YD1(I)+R31(I)*ZD1(I)
          YDL=R12(I)*XD1(I)+R22(I)*YD1(I)+R32(I)*ZD1(I)
          ZDL=R13(I)*XD1(I)+R23(I)*YD1(I)+R33(I)*ZD1(I)
          XD1(I)=XDL
          YD1(I)=YDL
          ZD1(I)=ZDL
          XDL=R11(I)*XD2(I)+R21(I)*YD2(I)+R31(I)*ZD2(I)
          YDL=R12(I)*XD2(I)+R22(I)*YD2(I)+R32(I)*ZD2(I)
          ZDL=R13(I)*XD2(I)+R23(I)*YD2(I)+R33(I)*ZD2(I)
          XD2(I)=XDL
          YD2(I)=YDL
          ZD2(I)=ZDL
          XDL=R11(I)*XD3(I)+R21(I)*YD3(I)+R31(I)*ZD3(I)
          YDL=R12(I)*XD3(I)+R22(I)*YD3(I)+R32(I)*ZD3(I)
          ZDL=R13(I)*XD3(I)+R23(I)*YD3(I)+R33(I)*ZD3(I)
          XD3(I)=XDL
          YD3(I)=YDL
          ZD3(I)=ZDL
          XDL=R11(I)*XD4(I)+R21(I)*YD4(I)+R31(I)*ZD4(I)
          YDL=R12(I)*XD4(I)+R22(I)*YD4(I)+R32(I)*ZD4(I)
          ZDL=R13(I)*XD4(I)+R23(I)*YD4(I)+R33(I)*ZD4(I)
          XD4(I)=XDL
          YD4(I)=YDL
          ZD4(I)=ZDL
          XDL=R11(I)*XD5(I)+R21(I)*YD5(I)+R31(I)*ZD5(I)
          YDL=R12(I)*XD5(I)+R22(I)*YD5(I)+R32(I)*ZD5(I)
          ZDL=R13(I)*XD5(I)+R23(I)*YD5(I)+R33(I)*ZD5(I)
          XD5(I)=XDL
          YD5(I)=YDL
          ZD5(I)=ZDL
          XDL=R11(I)*XD6(I)+R21(I)*YD6(I)+R31(I)*ZD6(I)
          YDL=R12(I)*XD6(I)+R22(I)*YD6(I)+R32(I)*ZD6(I)
          ZDL=R13(I)*XD6(I)+R23(I)*YD6(I)+R33(I)*ZD6(I)
          XD6(I)=XDL
          YD6(I)=YDL
          ZD6(I)=ZDL
          XDL=R11(I)*XD7(I)+R21(I)*YD7(I)+R31(I)*ZD7(I)
          YDL=R12(I)*XD7(I)+R22(I)*YD7(I)+R32(I)*ZD7(I)
          ZDL=R13(I)*XD7(I)+R23(I)*YD7(I)+R33(I)*ZD7(I)
          XD7(I)=XDL
          YD7(I)=YDL
          ZD7(I)=ZDL
          XDL=R11(I)*XD8(I)+R21(I)*YD8(I)+R31(I)*ZD8(I)
          YDL=R12(I)*XD8(I)+R22(I)*YD8(I)+R32(I)*ZD8(I)
          ZDL=R13(I)*XD8(I)+R23(I)*YD8(I)+R33(I)*ZD8(I)
          XD8(I)=XDL
          YD8(I)=YDL
          ZD8(I)=ZDL		   	 
          END IF
	 ENDDO
        END IF
C
      ELSE    ! Ismstr = 4
        DO I=LFT,LLT	  
          XDL=R11(I)*XD1(I)+R21(I)*YD1(I)+R31(I)*ZD1(I)
          YDL=R12(I)*XD1(I)+R22(I)*YD1(I)+R32(I)*ZD1(I)
          ZDL=R13(I)*XD1(I)+R23(I)*YD1(I)+R33(I)*ZD1(I)
          XD1(I)=XDL
          YD1(I)=YDL
          ZD1(I)=ZDL
          XDL=R11(I)*XD2(I)+R21(I)*YD2(I)+R31(I)*ZD2(I)
          YDL=R12(I)*XD2(I)+R22(I)*YD2(I)+R32(I)*ZD2(I)
          ZDL=R13(I)*XD2(I)+R23(I)*YD2(I)+R33(I)*ZD2(I)
          XD2(I)=XDL
          YD2(I)=YDL
          ZD2(I)=ZDL
          XDL=R11(I)*XD3(I)+R21(I)*YD3(I)+R31(I)*ZD3(I)
          YDL=R12(I)*XD3(I)+R22(I)*YD3(I)+R32(I)*ZD3(I)
          ZDL=R13(I)*XD3(I)+R23(I)*YD3(I)+R33(I)*ZD3(I)
          XD3(I)=XDL
          YD3(I)=YDL
          ZD3(I)=ZDL
          XDL=R11(I)*XD4(I)+R21(I)*YD4(I)+R31(I)*ZD4(I)
          YDL=R12(I)*XD4(I)+R22(I)*YD4(I)+R32(I)*ZD4(I)
          ZDL=R13(I)*XD4(I)+R23(I)*YD4(I)+R33(I)*ZD4(I)
          XD4(I)=XDL
          YD4(I)=YDL
          ZD4(I)=ZDL
          XDL=R11(I)*XD5(I)+R21(I)*YD5(I)+R31(I)*ZD5(I)
          YDL=R12(I)*XD5(I)+R22(I)*YD5(I)+R32(I)*ZD5(I)
          ZDL=R13(I)*XD5(I)+R23(I)*YD5(I)+R33(I)*ZD5(I)
          XD5(I)=XDL
          YD5(I)=YDL
          ZD5(I)=ZDL
          XDL=R11(I)*XD6(I)+R21(I)*YD6(I)+R31(I)*ZD6(I)
          YDL=R12(I)*XD6(I)+R22(I)*YD6(I)+R32(I)*ZD6(I)
          ZDL=R13(I)*XD6(I)+R23(I)*YD6(I)+R33(I)*ZD6(I)
          XD6(I)=XDL
          YD6(I)=YDL
          ZD6(I)=ZDL
          XDL=R11(I)*XD7(I)+R21(I)*YD7(I)+R31(I)*ZD7(I)
          YDL=R12(I)*XD7(I)+R22(I)*YD7(I)+R32(I)*ZD7(I)
          ZDL=R13(I)*XD7(I)+R23(I)*YD7(I)+R33(I)*ZD7(I)
          XD7(I)=XDL
          YD7(I)=YDL
          ZD7(I)=ZDL
          XDL=R11(I)*XD8(I)+R21(I)*YD8(I)+R31(I)*ZD8(I)
          YDL=R12(I)*XD8(I)+R22(I)*YD8(I)+R32(I)*ZD8(I)
          ZDL=R13(I)*XD8(I)+R23(I)*YD8(I)+R33(I)*ZD8(I)
          XD8(I)=XDL
          YD8(I)=YDL
          ZD8(I)=ZDL		   	    	   
          OFF(I) = ABS(OFFG(I))
          OFF_L  = MIN(OFF_L,OFFG(I))
        ENDDO
C
      ENDIF  ! ISMSTR    
C-----------
C     PASSAGE AU REPERE ORTHOTROPE.
C-----------
      IF (ISORTH/=0 .AND. JHBE/=24 .AND. JHBE/=222 .AND. JHBE/=14
     .              .AND. JHBE/=15) THEN
C------save for SAV update     
       IF(ISMSTR <= 3) THEN
        DO I=LFT,LLT
         X0(I,1) = XD1(I)
         Y0(I,1) = YD1(I)
         Z0(I,1) = ZD1(I)
         X0(I,2) = XD2(I)
         Y0(I,2) = YD2(I)
         Z0(I,2) = ZD2(I)
         X0(I,3) = XD3(I)
         Y0(I,3) = YD3(I)
         Z0(I,3) = ZD3(I)
         X0(I,4) = XD4(I)
         Y0(I,4) = YD4(I)
         Z0(I,4) = ZD4(I)
         X0(I,5) = XD5(I)
         Y0(I,5) = YD5(I)
         Z0(I,5) = ZD5(I)
         X0(I,6) = XD6(I)
         Y0(I,6) = YD6(I)
         Z0(I,6) = ZD6(I)
         X0(I,7) = XD7(I)
         Y0(I,7) = YD7(I)
         Z0(I,7) = ZD7(I)
         X0(I,8) = XD8(I)
         Y0(I,8) = YD8(I)
         Z0(I,8) = ZD8(I)
        END DO
       END IF
C	
        DO I=LFT,LLT
C         Extration de G tq Xortho=Transpose(G) Xcvt
C                                 =Transpose(G) Transpose(R) Xglobal.
          G11=GAMA(I,1)
          G21=GAMA(I,2)
          G31=GAMA(I,3)
          G12=GAMA(I,4)
          G22=GAMA(I,5)
          G32=GAMA(I,6)
          G13=G21*G32-G31*G22
          G23=G31*G12-G11*G32
          G33=G11*G22-G21*G12
C
          XDL=G11*XD1(I)+G21*YD1(I)+G31*ZD1(I)
          YDL=G12*XD1(I)+G22*YD1(I)+G32*ZD1(I)
          ZDL=G13*XD1(I)+G23*YD1(I)+G33*ZD1(I)
          XD1(I)=XDL
          YD1(I)=YDL
          ZD1(I)=ZDL
          XDL=G11*XD2(I)+G21*YD2(I)+G31*ZD2(I)
          YDL=G12*XD2(I)+G22*YD2(I)+G32*ZD2(I)
          ZDL=G13*XD2(I)+G23*YD2(I)+G33*ZD2(I)
          XD2(I)=XDL
          YD2(I)=YDL
          ZD2(I)=ZDL
          XDL=G11*XD3(I)+G21*Yd3(I)+G31*ZD3(I)
          YDL=G12*XD3(I)+G22*YD3(I)+G32*ZD3(I)
          ZDL=G13*XD3(I)+G23*YD3(I)+G33*ZD3(I)
          XD3(I)=XDL
          YD3(I)=YDL
          ZD3(I)=ZDL
          XDL=G11*XD4(I)+G21*YD4(I)+G31*ZD4(I)
          YDL=G12*XD4(I)+G22*YD4(I)+G32*ZD4(I)
          ZDL=G13*XD4(I)+G23*YD4(I)+G33*ZD4(I)
          XD4(I)=XDL
          YD4(I)=YDL
          ZD4(I)=ZDL
          XDL=G11*XD5(I)+G21*YD5(I)+G31*ZD5(I)
          YDL=G12*XD5(I)+G22*YD5(I)+G32*ZD5(I)
          ZDL=G13*XD5(I)+G23*YD5(I)+G33*ZD5(I)
          XD5(I)=XDL
          YD5(I)=YDL
          ZD5(I)=ZDL
          XDL=G11*XD6(I)+G21*YD6(I)+G31*ZD6(I)
          YDL=G12*XD6(I)+G22*YD6(I)+G32*ZD6(I)
          ZDL=G13*XD6(I)+G23*YD6(I)+G33*ZD6(I)
          XD6(I)=XDL
          YD6(I)=YDL
          ZD6(I)=ZDL
          XDL=G11*XD7(I)+G21*YD7(I)+G31*ZD7(I)
          YDL=G12*XD7(I)+G22*YD7(I)+G32*ZD7(I)
          ZDL=G13*XD7(I)+G23*YD7(I)+G33*ZD7(I)
          XD7(I)=XDL
          YD7(I)=YDL
          ZD7(I)=ZDL
          XDL=G11*XD8(I)+G21*YD8(I)+G31*ZD8(I)
          YDL=G12*XD8(I)+G22*YD8(I)+G32*ZD8(I)
          ZDL=G13*XD8(I)+G23*YD8(I)+G33*ZD8(I)
          XD8(I)=XDL
          YD8(I)=YDL
          ZD8(I)=ZDL
C         MATRICE DE PASSAGE GLOBAL -> ORTHOTROPE.
          T11=R11(I)*G11+R12(I)*G21+R13(I)*G31
          T12=R11(I)*G12+R12(I)*G22+R13(I)*G32
          T13=R11(I)*G13+R12(I)*G23+R13(I)*G33
          T21=R21(I)*G11+R22(I)*G21+R23(I)*G31
          T22=R21(I)*G12+R22(I)*G22+R23(I)*G32
          T23=R21(I)*G13+R22(I)*G23+R23(I)*G33
          T31=R31(I)*G11+R32(I)*G21+R33(I)*G31
          T32=R31(I)*G12+R32(I)*G22+R33(I)*G32
          T33=R31(I)*G13+R32(I)*G23+R33(I)*G33
          R11(I)=T11
          R12(I)=T12
          R13(I)=T13
          R21(I)=T21
          R22(I)=T22
          R23(I)=T23
          R31(I)=T31
          R32(I)=T32
          R33(I)=T33
        ENDDO
      ENDIF   

c copy and cast XD (DP) to C(SP) to assure coherence between XD et X      
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
C-----------
C     PASSAGE DES VITESSES AU REPERE CONVECTE (OU ORTHOTROPE).
C-----------
      CALL SRROTA3(
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33,
     .   VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .   VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8)


      
      
      IF(OFF_L < ZERO)THEN
        DO I=LFT,LLT
          IF(OFFG(I) < ZERO)THEN
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
C-----------
      DO I=LFT,LLT
        VD2(I)=ZERO
      ENDDO
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  S8SAV3                        source/elements/solid/solide/srcoor3.F
Chd|-- called by -----------
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SCFORC3                       source/elements/thickshell/solidec/scforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8SAV3(OFFG   ,SAV   ,
     .   XD1   ,XD2   ,XD3   ,XD4   ,XD5   ,XD6   ,XD7   ,XD8   ,
     .   YD1   ,YD2   ,YD3   ,YD4   ,YD5   ,YD6   ,YD7   ,YD8   ,
     .   ZD1   ,ZD2   ,ZD3   ,ZD4   ,ZD5   ,ZD6   ,ZD7   ,ZD8   ,
     .   NEL   ) 
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
      my_real
     .   OFFG(*)
      
      DOUBLE PRECISION 
     .   XD1(*), XD2(*), XD3(*), XD4(*), XD5(*), XD6(*), XD7(*), XD8(*), 
     .   YD1(*), YD2(*), YD3(*), YD4(*), YD5(*), YD6(*), YD7(*), YD8(*), 
     .   ZD1(*), ZD2(*), ZD3(*), ZD4(*), ZD5(*), ZD6(*), ZD7(*), ZD8(*),      
     .   SAV(NEL,21)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I      
C=======================================================================
        DO I=LFT,LLT
          IF(ABS(OFFG(I)) <= ONE )THEN
           SAV(I,1)=XD1(I)-XD8(I)
           SAV(I,2)=YD1(I)-YD8(I)
           SAV(I,3)=ZD1(I)-ZD8(I)
           SAV(I,4)=XD2(I)-XD8(I)
           SAV(I,5)=YD2(I)-YD8(I)
           SAV(I,6)=ZD2(I)-ZD8(I)
           SAV(I,7)=XD3(I)-XD8(I)
           SAV(I,8)=YD3(I)-YD8(I)
           SAV(I,9)=ZD3(I)-ZD8(I)
           SAV(I,10)=XD4(I)-XD8(I)
           SAV(I,11)=YD4(I)-YD8(I)
           SAV(I,12)=ZD4(I)-ZD8(I)
           SAV(I,13)=XD5(I)-XD8(I)
           SAV(I,14)=YD5(I)-YD8(I)
           SAV(I,15)=ZD5(I)-ZD8(I)
           SAV(I,16)=XD6(I)-XD8(I)
           SAV(I,17)=YD6(I)-YD8(I)
           SAV(I,18)=ZD6(I)-ZD8(I)
           SAV(I,19)=XD7(I)-XD8(I)
           SAV(I,20)=YD7(I)-YD8(I)
           SAV(I,21)=ZD7(I)-ZD8(I)
          ENDIF
        ENDDO
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  S8SAVSR3                      source/elements/solid/solide/srcoor3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8SAVSR3(OFFG  ,SAV   ,XD   ,YD   ,ZD, NEL) 
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
      my_real
     .   OFFG(*)
      
      DOUBLE PRECISION 
     .   XD(MVSIZ,8),YD(MVSIZ,8),ZD(MVSIZ,8), SAV(NEL,21)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I      
C=======================================================================
        DO I=LFT,LLT
          IF(ABS(OFFG(I)) <= ONE )THEN
           SAV(I,1)=XD(I,1)-XD(I,8)
           SAV(I,2)=YD(I,1)-YD(I,8)
           SAV(I,3)=ZD(I,1)-ZD(I,8)
           SAV(I,4)=XD(I,2)-XD(I,8)
           SAV(I,5)=YD(I,2)-YD(I,8)
           SAV(I,6)=ZD(I,2)-ZD(I,8)
           SAV(I,7)=XD(I,3)-XD(I,8)
           SAV(I,8)=YD(I,3)-YD(I,8)
           SAV(I,9)=ZD(I,3)-ZD(I,8)
           SAV(I,10)=XD(I,4)-XD(I,8)
           SAV(I,11)=YD(I,4)-YD(I,8)
           SAV(I,12)=ZD(I,4)-ZD(I,8)
           SAV(I,13)=XD(I,5)-XD(I,8)
           SAV(I,14)=YD(I,5)-YD(I,8)
           SAV(I,15)=ZD(I,5)-ZD(I,8)
           SAV(I,16)=XD(I,6)-XD(I,8)
           SAV(I,17)=YD(I,6)-YD(I,8)
           SAV(I,18)=ZD(I,6)-ZD(I,8)
           SAV(I,19)=XD(I,7)-XD(I,8)
           SAV(I,20)=YD(I,7)-YD(I,8)
           SAV(I,21)=ZD(I,7)-ZD(I,8)
          ENDIF
        ENDDO
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  S8SAV12                       source/elements/solid/solide/srcoor3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8SAV12(OFFG   ,OFFG0  ,SAV   ,X  ,XDP  ,
     .     NC1   ,NC2   ,NC3   ,NC4   ,NC5   ,NC6   ,NC7   ,NC8   ,NEL)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
      INTEGER 
     .        NC1(*),NC2(*),NC3(*),NC4(*),NC5(*),NC6(*),NC7(*),NC8(*)
      my_real
     .   OFFG(*),OFFG0(*),X(3,*)
      DOUBLE PRECISION 
     .   XDP(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,IH    
      DOUBLE PRECISION 
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ), SAV(NEL,21)
C=======================================================================
C-----Always in global system and fixed then (not like Ismstr=11
C-----for JCVT>0 Fixed SAV is rotated w/ local system not like Ismstr=1
        IH=0
        DO I=LFT,LLT
         IF(OFFG(I)/=OFFG0(I).AND.ABS(OFFG(I)) > ONE ) IH=1
	END DO
	IF (IH==0) RETURN
C	
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
C   -----attention, format is different to Ismstr=10,11
        DO I=LFT,LLT
          IF(OFFG(I)/=OFFG0(I).AND.ABS(OFFG(I)) > ONE )THEN
           SAV(I,1)=XD1(I)-XD8(I)
           SAV(I,2)=YD1(I)-YD8(I)
           SAV(I,3)=ZD1(I)-ZD8(I)
           SAV(I,4)=XD2(I)-XD8(I)
           SAV(I,5)=YD2(I)-YD8(I)
           SAV(I,6)=ZD2(I)-ZD8(I)
           SAV(I,7)=XD3(I)-XD8(I)
           SAV(I,8)=YD3(I)-YD8(I)
           SAV(I,9)=ZD3(I)-ZD8(I)
           SAV(I,10)=XD4(I)-XD8(I)
           SAV(I,11)=YD4(I)-YD8(I)
           SAV(I,12)=ZD4(I)-ZD8(I)
           SAV(I,13)=XD5(I)-XD8(I)
           SAV(I,14)=YD5(I)-YD8(I)
           SAV(I,15)=ZD5(I)-ZD8(I)
           SAV(I,16)=XD6(I)-XD8(I)
           SAV(I,17)=YD6(I)-YD8(I)
           SAV(I,18)=ZD6(I)-ZD8(I)
           SAV(I,19)=XD7(I)-XD8(I)
           SAV(I,20)=YD7(I)-YD8(I)
           SAV(I,21)=ZD7(I)-ZD8(I)
          ENDIF
        ENDDO
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  SRCOOR12                      source/elements/solid/solide/srcoor3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- calls ---------------
Chd|        SORTHO12                      source/elements/solid/solide/sortho3.F
Chd|        SREPISO12                     source/elements/solid/solide/srepiso3.F
Chd|====================================================================
      SUBROUTINE SRCOOR12(OFF,X     ,XDP   ,
     .   NC1   ,NC2   ,NC3   ,NC4   ,NC5   ,NC6   ,NC7   ,NC8   ,
     .   R11   ,R12   ,R13   ,R21   ,R22   ,R23   ,R31   ,R32   ,R33  ) 
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
C     REAL
      my_real
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .   R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),
     .   X(3,*),OFF(*)
      INTEGER 
     .        NC1(*),NC2(*),NC3(*),NC4(*),NC5(*),NC6(*),NC7(*),NC8(*)
      
      DOUBLE PRECISION 
     .   XDP(3,*)      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      
C     REAL
      my_real
     .   RX(MVSIZ) , RY(MVSIZ) , RZ(MVSIZ) ,
     .   SX(MVSIZ) , SY(MVSIZ) , SZ(MVSIZ) ,
     .   TX(MVSIZ) , TY(MVSIZ) , TZ(MVSIZ) 
      DOUBLE PRECISION 
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ)
C=======================================================================
C
C----------------------------
C     NODAL COORDINATES     |
C----------------------------
      IF(IRESP==1)THEN 
        DO I=LFT,LLT
         IF (OFF(I) <= ONE ) CYCLE 
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
         IF (OFF(I) <= ONE ) CYCLE 
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
C-----------
C     REPERE CONVECTE (ITERATIONS).
C-----------
      CALL SREPISO12(OFF,
     .       XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8,
     .       YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8,
     .       ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8,
     .       RX, RY, RZ, SX, SY, SZ, TX, TY, TZ )
C---
      IF (JHBE == 14 .OR. JHBE == 24) THEN
        CALL SORTHO12(OFF,
     .       RX , RY , RZ , SX , SY , SZ , TX , TY , TZ ,
     .       R12, R13, R11, R22, R23, R21, R32, R33, R31)
      ELSE
        CALL SORTHO12(OFF,
     .       RX , RY , RZ , SX , SY , SZ , TX , TY , TZ ,
     .       R11, R12, R13, R21, R22, R23, R31, R32, R33)
      ENDIF
C-----------
      RETURN
      END
