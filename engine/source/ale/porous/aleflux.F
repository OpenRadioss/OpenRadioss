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
Chd|  ALEFLUX                       source/ale/aleflux.F          
Chd|-- called by -----------
Chd|        ALEFLOW                       source/ale/aleflow.F          
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE ALEFLUX(PM ,IXS,NC1 ,NC2 ,NC3 ,NC4 ,NC5 ,
     .               NC6 , NC7,NC8 ,X1  ,X2  ,X3  ,X4 ,
     .               X5  ,X6 ,X7  ,X8  ,Y1  ,Y2  ,Y3  ,
     .               Y4  , Y5 ,Y6  ,Y7  ,Y8  ,Z1  ,Z2 ,
     .               Z3  ,Z4 ,Z5  ,Z6  ,Z7 ,Z8   ,V   ,
     .               VDX1,VDX2,VDX3,VDX4,VDX5,VDX6,VDX7,VDX8,
     .               VDY1,VDY2,VDY3,VDY4,VDY5,VDY6,VDY7,VDY8,
     .               VDZ1,VDZ2,VDZ3,VDZ4,VDZ5,VDZ6,VDZ7,VDZ8,
     .               W  , FLUX ,FLU1 ,ALE_CONNECT,ALPHA  ,NGL  )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is related to porous material law 77
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "scr01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "scr06_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  IXS(NIXS,NUMELS),NC1(*), NC2(*), NC3(*), NC4(*), NC5(*), NC6(*),NC7(*),NC8(*),NGL(*)
      my_real PM(NPROPM,NUMMAT), V(3,NUMNOD), W(3,NUMNOD), FLUX(MVSIZ,6), FLU1(*),
     .        X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .        Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),
     .        Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*),
     .        ALPHA(MVSIZ,6),
     .        VDX1(*),VDX2(*),VDX3(*),VDX4(*),VDX5(*),VDX6(*),VDX7(*),VDX8(*),
     .        VDY1(*),VDY2(*),VDY3(*),VDY4(*),VDY5(*),VDY6(*),VDY7(*),VDY8(*),
     .        VDZ1(*),VDZ2(*),VDZ3(*),VDZ4(*),VDZ5(*),VDZ6(*),VDZ7(*),VDZ8(*)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER MAT(MVSIZ),  I,II,ICLOS,IAD2
      my_real N1X(MVSIZ), N2X(MVSIZ), N3X(MVSIZ), N4X(MVSIZ), N5X(MVSIZ), 
     .        N6X(MVSIZ), N1Y(MVSIZ), N2Y(MVSIZ), N3Y(MVSIZ), N4Y(MVSIZ), 
     .        N5Y(MVSIZ), N6Y(MVSIZ), N1Z(MVSIZ), N2Z(MVSIZ), N3Z(MVSIZ), 
     .        N4Z(MVSIZ), N5Z(MVSIZ), N6Z(MVSIZ), 
     .        FLUX1(MVSIZ),FLUX2(MVSIZ), FLUX3(MVSIZ), FLUX4(MVSIZ), 
     .        FLUX5(MVSIZ),FLUX6(MVSIZ), VX1(MVSIZ),
     .        VX2(MVSIZ), VX3(MVSIZ), VX4(MVSIZ), VX5(MVSIZ), VX6(MVSIZ),
     .        VY1(MVSIZ), VY2(MVSIZ), VY3(MVSIZ), VY4(MVSIZ), VY5(MVSIZ), 
     .        VY6(MVSIZ), VZ1(MVSIZ), VZ2(MVSIZ), VZ3(MVSIZ), VZ4(MVSIZ), 
     .        VZ5(MVSIZ), VZ6(MVSIZ),
     .        UPWL(MVSIZ,6),XC(MVSIZ),YC(MVSIZ),
     .        ZC(MVSIZ) ,XF1(MVSIZ),YF1(MVSIZ),ZF1(MVSIZ),XF2(MVSIZ),
     .        YF2(MVSIZ),ZF2(MVSIZ), XF3(MVSIZ),YF3(MVSIZ),ZF3(MVSIZ),
     .        XF4(MVSIZ),YF4(MVSIZ),ZF4(MVSIZ),XF5(MVSIZ),YF5(MVSIZ),
     .        ZF5(MVSIZ), XF6(MVSIZ),YF6(MVSIZ),ZF6(MVSIZ),TEST
C
      INTEGER MA
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------   

C-----------------------------------------------
C     RELATIVE VELOCITY OVER THE FACE
C-----------------------------------------------
      DO  I=LFT,LLT
       II=I+NFT
       MAT(I)=IXS(1,II)
       VX1(I)=ONE_OVER_8*(VDX1(I)+VDX2(I)+VDX3(I)+VDX4(I))
       VX2(I)=ONE_OVER_8*(VDX3(I)+VDX4(I)+VDX8(I)+VDX7(I))
       VX3(I)=ONE_OVER_8*(VDX5(I)+VDX6(I)+VDX7(I)+VDX8(I))
       VX4(I)=ONE_OVER_8*(VDX1(I)+VDX2(I)+VDX6(I)+VDX5(I))
       VX5(I)=ONE_OVER_8*(VDX2(I)+VDX3(I)+VDX7(I)+VDX6(I))
       VX6(I)=ONE_OVER_8*(VDX1(I)+VDX4(I)+VDX8(I)+VDX5(I))
C
       VY1(I)=ONE_OVER_8*(VDY1(I)+VDY2(I)+VDY3(I)+VDY4(I))
       VY2(I)=ONE_OVER_8*(VDY3(I)+VDY4(I)+VDY8(I)+VDY7(I))
       VY3(I)=ONE_OVER_8*(VDY5(I)+VDY6(I)+VDY7(I)+VDY8(I))
       VY4(I)=ONE_OVER_8*(VDY1(I)+VDY2(I)+VDY6(I)+VDY5(I))
       VY5(I)=ONE_OVER_8*(VDY2(I)+VDY3(I)+VDY7(I)+VDY6(I))
       VY6(I)=ONE_OVER_8*(VDY1(I)+VDY4(I)+VDY8(I)+VDY5(I))
C
       VZ1(I)=ONE_OVER_8*(VDZ1(I)+VDZ2(I)+VDZ3(I)+VDZ4(I))
       VZ2(I)=ONE_OVER_8*(VDZ3(I)+VDZ4(I)+VDZ8(I)+VDZ7(I))
       VZ3(I)=ONE_OVER_8*(VDZ5(I)+VDZ6(I)+VDZ7(I)+VDZ8(I))
       VZ4(I)=ONE_OVER_8*(VDZ1(I)+VDZ2(I)+VDZ6(I)+VDZ5(I))
       VZ5(I)=ONE_OVER_8*(VDZ2(I)+VDZ3(I)+VDZ7(I)+VDZ6(I))
       VZ6(I)=ONE_OVER_8*(VDZ1(I)+VDZ4(I)+VDZ8(I)+VDZ5(I))
      ENDDO
C------------------------------------------
C     NORMAL VECTORS
C------------------------------------------
      DO  I=LFT,LLT
       N1X(I)=(Y3(I)-Y1(I))*(Z2(I)-Z4(I)) - (Z3(I)-Z1(I))*(Y2(I)-Y4(I))
       N1Y(I)=(Z3(I)-Z1(I))*(X2(I)-X4(I)) - (X3(I)-X1(I))*(Z2(I)-Z4(I))
       N1Z(I)=(X3(I)-X1(I))*(Y2(I)-Y4(I)) - (Y3(I)-Y1(I))*(X2(I)-X4(I))
C
       N2X(I)=(Y7(I)-Y4(I))*(Z3(I)-Z8(I)) - (Z7(I)-Z4(I))*(Y3(I)-Y8(I))
       N2Y(I)=(Z7(I)-Z4(I))*(X3(I)-X8(I)) - (X7(I)-X4(I))*(Z3(I)-Z8(I))
       N2Z(I)=(X7(I)-X4(I))*(Y3(I)-Y8(I)) - (Y7(I)-Y4(I))*(X3(I)-X8(I))
C
       N3X(I)=(Y6(I)-Y8(I))*(Z7(I)-Z5(I)) - (Z6(I)-Z8(I))*(Y7(I)-Y5(I))
       N3Y(I)=(Z6(I)-Z8(I))*(X7(I)-X5(I)) - (X6(I)-X8(I))*(Z7(I)-Z5(I))
       N3Z(I)=(X6(I)-X8(I))*(Y7(I)-Y5(I)) - (Y6(I)-Y8(I))*(X7(I)-X5(I))
C
       N4X(I)=(Y2(I)-Y5(I))*(Z6(I)-Z1(I)) - (Z2(I)-Z5(I))*(Y6(I)-Y1(I))
       N4Y(I)=(Z2(I)-Z5(I))*(X6(I)-X1(I)) - (X2(I)-X5(I))*(Z6(I)-Z1(I))
       N4Z(I)=(X2(I)-X5(I))*(Y6(I)-Y1(I)) - (Y2(I)-Y5(I))*(X6(I)-X1(I))
C
       N5X(I)=(Y7(I)-Y2(I))*(Z6(I)-Z3(I)) - (Z7(I)-Z2(I))*(Y6(I)-Y3(I))
       N5Y(I)=(Z7(I)-Z2(I))*(X6(I)-X3(I)) - (X7(I)-X2(I))*(Z6(I)-Z3(I))
       N5Z(I)=(X7(I)-X2(I))*(Y6(I)-Y3(I)) - (Y7(I)-Y2(I))*(X6(I)-X3(I))
C
       N6X(I)=(Y8(I)-Y1(I))*(Z4(I)-Z5(I)) - (Z8(I)-Z1(I))*(Y4(I)-Y5(I))
       N6Y(I)=(Z8(I)-Z1(I))*(X4(I)-X5(I)) - (X8(I)-X1(I))*(Z4(I)-Z5(I))
       N6Z(I)=(X8(I)-X1(I))*(Y4(I)-Y5(I)) - (Y8(I)-Y1(I))*(X4(I)-X5(I))
      ENDDO
C
C CHECK AREA WITH ELEMENT CLOSURE
C is not available for this new formulation
      IF(ICLOSE == 1) THEN
        DO I=LFT,LLT
C
          XC(I)=ONE_OVER_8*(X1(I)+X2(I)+X3(I)+X4(I)+X5(I)+X6(I)+X7(I)+X8(I))
          YC(I)=ONE_OVER_8*(Y1(I)+Y2(I)+Y3(I)+Y4(I)+Y5(I)+Y6(I)+Y7(I)+Y8(I))
          ZC(I)=ONE_OVER_8*(Z1(I)+Z2(I)+Z3(I)+Z4(I)+Z5(I)+Z6(I)+Z7(I)+Z8(I))
          XF1(I)=FOURTH*(X1(I)+X2(I)+X3(I)+X4(I))
          XF2(I)=FOURTH*(X3(I)+X4(I)+X8(I)+X7(I))
          XF3(I)=FOURTH*(X5(I)+X6(I)+X7(I)+X8(I))
          XF4(I)=FOURTH*(X1(I)+X2(I)+X6(I)+X5(I))
          XF5(I)=FOURTH*(X2(I)+X3(I)+X7(I)+X6(I))
          XF6(I)=FOURTH*(X1(I)+X4(I)+X8(I)+X5(I))
C
          YF1(I)=FOURTH*(Y1(I)+Y2(I)+Y3(I)+Y4(I))
          YF2(I)=FOURTH*(Y3(I)+Y4(I)+Y8(I)+Y7(I))
          YF3(I)=FOURTH*(Y5(I)+Y6(I)+Y7(I)+Y8(I))
          YF4(I)=FOURTH*(Y1(I)+Y2(I)+Y6(I)+Y5(I))
          YF5(I)=FOURTH*(Y2(I)+Y3(I)+Y7(I)+Y6(I))
          YF6(I)=FOURTH*(Y1(I)+Y4(I)+Y8(I)+Y5(I))
C
          ZF1(I)=FOURTH*(Z1(I)+Z2(I)+Z3(I)+Z4(I))
          ZF2(I)=FOURTH*(Z3(I)+Z4(I)+Z8(I)+Z7(I))
          ZF3(I)=FOURTH*(Z5(I)+Z6(I)+Z7(I)+Z8(I))
          ZF4(I)=FOURTH*(Z1(I)+Z2(I)+Z6(I)+Z5(I))
          ZF5(I)=FOURTH*(Z2(I)+Z3(I)+Z7(I)+Z6(I))
          ZF6(I)=FOURTH*(Z1(I)+Z4(I)+Z8(I)+Z5(I))          
        ENDDO
        DO I=LFT,LLT
          TEST=(XF1(I)-XC(I))*N1X(I)+
     .         (YF1(I)-YC(I))*N1Y(I)+
     .         (ZF1(I)-ZC(I))*N1Z(I)
          IF(TEST <= 0)THEN
              N1X(I)=ZERO
              N1Y(I)=ZERO
              N1Z(I)=ZERO     
          ENDIF
        ENDDO
        DO I=LFT,LLT
          TEST=(XF2(I)-XC(I))*N2X(I)+
     .         (YF2(I)-YC(I))*N2Y(I)+
     .         (ZF2(I)-ZC(I))*N2Z(I)
          IF(TEST <= 0)THEN
              N2X(I)=ZERO
              N2Y(I)=ZERO
              N2Z(I)=ZERO     
          ENDIF
        ENDDO
        DO I=LFT,LLT
          TEST=(XF3(I)-XC(I))*N3X(I)+
     .         (YF3(I)-YC(I))*N3Y(I)+
     .         (ZF3(I)-ZC(I))*N3Z(I)
          IF(TEST <= 0)THEN
              N3X(I)=ZERO
              N3Y(I)=ZERO
              N3Z(I)=ZERO              
          ENDIF
        ENDDO
        DO I=LFT,LLT
          TEST=(XF4(I)-XC(I))*N4X(I)+
     .         (YF4(I)-YC(I))*N4Y(I)+
     .         (ZF4(I)-ZC(I))*N4Z(I)
            IF(TEST <= ZERO)THEN
              N4X(I)=ZERO
              N4Y(I)=ZERO
              N4Z(I)=ZERO              
          ENDIF
        ENDDO
        DO I=LFT,LLT
          TEST=(XF5(I)-XC(I))*N5X(I)+
     .         (YF5(I)-YC(I))*N5Y(I)+
     .         (ZF5(I)-ZC(I))*N5Z(I)
             IF(TEST <= ZERO)THEN
              N5X(I)=ZERO
              N5Y(I)=ZERO
              N5Z(I)=ZERO              
          ENDIF
        ENDDO
        DO I=LFT,LLT
          TEST=(XF6(I)-XC(I))*N6X(I)+
     .         (YF6(I)-YC(I))*N6Y(I)+
     .         (ZF6(I)-ZC(I))*N6Z(I)
          IF(TEST <= ZERO)THEN
              N6X(I)=ZERO
              N6Y(I)=ZERO
              N6Z(I)=ZERO      
          ENDIF
        ENDDO
      ENDIF
C--------------------
C     CALCUL DES FLUX
C--------------------
      DO  I=LFT,LLT
        FLUX1(I)=(VX1(I)*N1X(I)+VY1(I)*N1Y(I)+VZ1(I)*N1Z(I))
        FLUX2(I)=(VX2(I)*N2X(I)+VY2(I)*N2Y(I)+VZ2(I)*N2Z(I))
        FLUX3(I)=(VX3(I)*N3X(I)+VY3(I)*N3Y(I)+VZ3(I)*N3Z(I))
        FLUX4(I)=(VX4(I)*N4X(I)+VY4(I)*N4Y(I)+VZ4(I)*N4Z(I))
        FLUX5(I)=(VX5(I)*N5X(I)+VY5(I)*N5Y(I)+VZ5(I)*N5Z(I))
        FLUX6(I)=(VX6(I)*N6X(I)+VY6(I)*N6Y(I)+VZ6(I)*N6Z(I))        
C      
        FLUX1(I)=ALPHA(I,1)*FLUX1(I)
        FLUX2(I)=ALPHA(I,2)*FLUX2(I)
        FLUX3(I)=ALPHA(I,3)*FLUX3(I)
        FLUX4(I)=ALPHA(I,4)*FLUX4(I)
        FLUX5(I)=ALPHA(I,5)*FLUX5(I)
        FLUX6(I)=ALPHA(I,6)*FLUX6(I) 
      ENDDO
C-----------------------------------------------
C     UPWIND
C-----------------------------------------------
      MA = MAT(LFT)
      DO I=LFT,LLT
           UPWL(I,1)=PM(16,MA)
           UPWL(I,2)=PM(16,MA)
           UPWL(I,3)=PM(16,MA)
           UPWL(I,4)=PM(16,MA)
           UPWL(I,5)=PM(16,MA)
           UPWL(I,6)=PM(16,MA)
      ENDDO

      ICLOS = NINT(PM(198, MAT(1)))
      IF(ICLOS == 1) THEN 
        DO  I=LFT,LLT
           IAD2 = ALE_CONNECT%ee_connect%iad_connect(I + NFT)
           II=ALE_CONNECT%ee_connect%connected(IAD2 + 1 - 1)
           IF(II == 0) FLUX1(I)= ZERO
c             
           II=ALE_CONNECT%ee_connect%connected(IAD2 + 2 - 1)
           IF(II == 0) FLUX2(I)= ZERO
C
           II=ALE_CONNECT%ee_connect%connected(IAD2 + 3 - 1)
           IF(II == 0)FLUX3(I)= ZERO
C
           II=ALE_CONNECT%ee_connect%connected(IAD2 + 4 - 1)
           IF(II == 0)FLUX4(I)= ZERO
c             
           II=ALE_CONNECT%ee_connect%connected(IAD2 + 5 - 1)
           IF(II == 0) FLUX5(I)=ZERO
C           
           II=ALE_CONNECT%ee_connect%connected(IAD2 + 6 - 1)
           IF(II == 0) FLUX6(I)= ZERO
C                      
        ENDDO
       ENDIF
C ---
       DO  I=LFT,LLT
C                            
          FLUX(I,1)=FLUX1(I)-UPWL(I,1)*ABS(FLUX1(I))
          FLUX(I,2)=FLUX2(I)-UPWL(I,2)*ABS(FLUX2(I))
          FLUX(I,3)=FLUX3(I)-UPWL(I,3)*ABS(FLUX3(I))
          FLUX(I,4)=FLUX4(I)-UPWL(I,4)*ABS(FLUX4(I))
          FLUX(I,5)=FLUX5(I)-UPWL(I,5)*ABS(FLUX5(I))
          FLUX(I,6)=FLUX6(I)-UPWL(I,6)*ABS(FLUX6(I))
C
          FLU1(I)  =FLUX1(I)+UPWL(I,1)*ABS(FLUX1(I))
     .            +FLUX2(I)+UPWL(I,2)*ABS(FLUX2(I))
     .            +FLUX3(I)+UPWL(I,3)*ABS(FLUX3(I))
     .            +FLUX4(I)+UPWL(I,4)*ABS(FLUX4(I))
     .            +FLUX5(I)+UPWL(I,5)*ABS(FLUX5(I))
     .            +FLUX6(I)+UPWL(I,6)*ABS(FLUX6(I))
       
       ENDDO
C
      RETURN
      END
