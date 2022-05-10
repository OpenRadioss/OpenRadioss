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
Chd|  SRCOOR3_IMP                   source/elements/solid/solide8s/srcoor3_imp.F
Chd|-- called by -----------
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|-- calls ---------------
Chd|        CRFRAME_IMP                   source/elements/solid/solide8s/srcoor3_imp.F
Chd|        CRTRANS_IMP                   source/elements/solid/solide8s/srcoor3_imp.F
Chd|        GETULOC                       source/elements/solid/solide8s/srcoor3_imp.F
Chd|        S8SAV3_IMP                    source/elements/solid/solide8s/srcoor3_imp.F
Chd|        S8XREF_IMP                    source/elements/solid/solide8s/srcoor3_imp.F
Chd|====================================================================
      SUBROUTINE SRCOOR3_IMP(X   ,IXS   ,V     ,W     ,GAMA0 ,GAMA  ,
     .   X1    ,X2    ,X3    ,X4    ,X5    ,X6    ,X7    ,X8    ,
     .   Y1    ,Y2    ,Y3    ,Y4    ,Y5    ,Y6    ,Y7    ,Y8    ,
     .   Z1    ,Z2    ,Z3    ,Z4    ,Z5    ,Z6    ,Z7    ,Z8    ,
     .   VX1   ,VX2   ,VX3   ,VX4   ,VX5   ,VX6   ,VX7   ,VX8   , 
     .   VY1   ,VY2   ,VY3   ,VY4   ,VY5   ,VY6   ,VY7   ,VY8   , 
     .   VZ1   ,VZ2   ,VZ3   ,VZ4   ,VZ5   ,VZ6   ,VZ7   ,VZ8   , 
     .   VD2   ,VIS   ,OFFG  ,OFF   ,SAV   ,RHO   ,RHOO  ,R     , 
     .   NC1   ,NC2   ,NC3   ,NC4   ,NC5   ,NC6   ,NC7   ,NC8   ,
     .   NGL   ,MXT   ,NGEO  ,IOUTPRT,VGAX ,VGAY  ,VGAZ  ,VGA2  ,
     .   XD1   ,XD2   ,XD3   ,XD4   ,XD5   ,XD6   ,XD7   ,XD8   ,
     .   YD1   ,YD2   ,YD3   ,YD4   ,YD5   ,YD6   ,YD7   ,YD8   ,
     .   ZD1   ,ZD2   ,ZD3   ,ZD4   ,ZD5   ,ZD6   ,ZD7   ,ZD8   ,         
     .   XDP   ,X0    ,Y0    ,Z0    ,NEL   ,TRM   ,XREF  ,
     .   ULX1  ,ULX2  ,ULX3  ,ULX4  ,ULX5  ,ULX6  ,ULX7  ,ULX8  ,
     .   ULY1  ,ULY2  ,ULY3  ,ULY4  ,ULY5  ,ULY6  ,ULY7  ,ULY8  ,
     .   ULZ1  ,ULZ2  ,ULZ3  ,ULZ4  ,ULZ5  ,ULZ6  ,ULZ7  ,ULZ8  ,
     .   XGAX  ,XGAY  ,XGAZ  ,XGXA2 ,XGYA2 ,XGZA2 ,XGXYA ,XGYZA ,
     .   XGZXA ,IPARG)  
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
#include      "impl1_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL ou REAL*8
      my_real
     .   X(3,*),V(3,*),W(3,*), VIS(*),
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*),
     .   VX1(*), VX2(*), VX3(*), VX4(*), VX5(*), VX6(*), VX7(*), VX8(*),
     .   VY1(*), VY2(*), VY3(*), VY4(*), VY5(*), VY6(*), VY7(*), VY8(*),
     .   VZ1(*), VZ2(*), VZ3(*), VZ4(*), VZ5(*), VZ6(*), VZ7(*), VZ8(*),
     .   VD2(*), OFFG(*), OFF(*),  RHO(*), RHOO(*),
     .   R(3,3,MVSIZ),
!
     .   RD11(MVSIZ),RD12(MVSIZ),RD13(MVSIZ),
     .   RD21(MVSIZ),RD22(MVSIZ),RD23(MVSIZ),
     .   RD31(MVSIZ),RD32(MVSIZ),RD33(MVSIZ),
     .   RR11,RR12,RR13,
     .   RR21,RR22,RR23,
     .   RR31,RR32,RR33,

     .   GAMA0(NEL,6),GAMA(MVSIZ,6) ,VGAX(*), VGAY(*), VGAZ(*), VGA2(*),
     .   XGAX(*), XGAY(*), XGAZ(*),
     .   XGXA2(MVSIZ),XGYA2(MVSIZ),XGZA2(MVSIZ),
     .   XGXYA(MVSIZ),XGYZA(MVSIZ),XGZXA(MVSIZ)
      INTEGER IXS(NIXS,*), MXT(*), NGL(*),NGEO(*),IPARG(*),
     .        NC1(*),NC2(*),NC3(*),NC4(*),NC5(*),NC6(*),NC7(*),NC8(*)
      INTEGER IOUTPRT
      
      DOUBLE PRECISION 
     .   XDP(3,*),SAV(NEL,21),X0(MVSIZ,8),Y0(MVSIZ,8),Z0(MVSIZ,8),
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ), 
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ), 
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ), 
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ), 
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ), 
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ),
     .   ULX1(MVSIZ), ULX2(MVSIZ), ULX3(MVSIZ), ULX4(MVSIZ),
     .   ULX5(MVSIZ), ULX6(MVSIZ), ULX7(MVSIZ), ULX8(MVSIZ),
     .   ULY1(MVSIZ), ULY2(MVSIZ), ULY3(MVSIZ), ULY4(MVSIZ),
     .   ULY5(MVSIZ), ULY6(MVSIZ), ULY7(MVSIZ), ULY8(MVSIZ),
     .   ULZ1(MVSIZ), ULZ2(MVSIZ), ULZ3(MVSIZ), ULZ4(MVSIZ),
     .   ULZ5(MVSIZ), ULZ6(MVSIZ), ULZ7(MVSIZ), ULZ8(MVSIZ),
     .   TRM(NEL,24,24), UL, VL, WL, UG, VG, WG, XREF(NEL,21)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,j
      
      DOUBLE PRECISION
     .   XDL, YDL, ZDL, INVJ(MVSIZ,9),
     .   V1(MVSIZ,9), V2(MVSIZ,9), V3(MVSIZ,9), V4(MVSIZ,9),
     .   V5(MVSIZ,9), V6(MVSIZ,9), V7(MVSIZ,9), V8(MVSIZ,9)
    
C     REAL ou REAL*8
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
C
C      
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
          XGAX(I)=X1(I)+X2(I)+X3(I)+X4(I)+X5(I)+X6(I)+X7(I)+X8(I)
          XGAY(I)=Y1(I)+Y2(I)+Y3(I)+Y4(I)+Y5(I)+Y6(I)+Y7(I)+Y8(I)
          XGAZ(I)=Z1(I)+Z2(I)+Z3(I)+Z4(I)+Z5(I)+Z6(I)+Z7(I)+Z8(I)
          XGXA2(I)=X1(I)**2+X2(I)**2+X3(I)**2+X4(I)**2
     1            +X5(I)**2+X6(I)**2+X7(I)**2+X8(I)**2
          XGYA2(I)=Y1(I)**2+Y2(I)**2+Y3(I)**2+Y4(I)**2
     1            +Y5(I)**2+Y6(I)**2+Y7(I)**2+Y8(I)**2
          XGZA2(I)=Z1(I)**2+Z2(I)**2+Z3(I)**2+Z4(I)**2
     1            +Z5(I)**2+Z6(I)**2+Z7(I)**2+Z8(I)**2
          XGXYA(I)=X1(I)*Y1(I)+X2(I)*Y2(I)+X3(I)*Y3(I)+X4(I)*Y4(I)
     1            +X5(I)*Y5(I)+X6(I)*Y6(I)+X7(I)*Y7(I)+X8(I)*Y8(I)
          XGYZA(I)=Y1(I)*Z1(I)+Y2(I)*Z2(I)+Y3(I)*Z3(I)+Y4(I)*Z4(I)
     1            +Y5(I)*Z5(I)+Y6(I)*Z6(I)+Y7(I)*Z7(I)+Y8(I)*Z8(I)
          XGZXA(I)=Z1(I)*X1(I)+Z2(I)*X2(I)+Z3(I)*X3(I)+Z4(I)*X4(I)
     1            +Z5(I)*X5(I)+Z6(I)*X6(I)+Z7(I)*X7(I)*Z8(I)*X8(I)
         ENDDO
       ENDIF
      ENDIF
C
       IF(INCONV == 1) THEN              ! Debut de pas
c       IF(IMCONV == 1) THEN              ! Debut de pas
         IF(ISMSTR == 1) THEN
          DO I=LFT,LLT
           IF (OFFG(I) >= 1 ) OFFG(I) = 1     ! A verifier avec qqun: Je ne suis pas sur de l'endroit ou offg doit etre maj
          ENDDO
          IF (DT1 == ZERO) THEN
           CALL S8SAV3_IMP(OFFG,SAV,
     .             XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8,
     .             YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8,
     .             ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8,
     .             NEL)
          ENDIF
         ELSE
          CALL S8SAV3_IMP(OFFG,SAV,
     .             XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8,
     .             YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8,
     .             ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8,
     .             NEL)
         ENDIF  !(ISMSTR == 1)
       ENDIF   !(INCONV == 1)
! fin ajout qz
C-----------
C     REPERE CONVECTE 
C-----------
      IF((ISMSTR<=4.OR.(ISMSTR==12.AND.IDTMIN(1)==3)).AND.JLAG>0) THEN
        DO I=LFT,LLT
          IF(ABS(OFFG(I)) > UN)THEN
            OFF(I) = ABS(OFFG(I))-UN
            OFF_L  = MIN(OFF_L,OFFG(I))
          ELSE
            OFF(I) = ABS(OFFG(I))
            OFF_L  = MIN(OFF_L,OFFG(I))
          ENDIF
        ENDDO
        IF((ISMSTR==12.AND.IDTMIN(1)==3).AND.JLAG>0) THEN
         DO I=LFT,LLT	  
          IF(ABS(OFFG(I)) > UN)THEN
          END IF
	 ENDDO
        END IF
C
      ELSE    ! Ismstr = 4
        DO I=LFT,LLT	  
          OFF(I) = ABS(OFFG(I))
          OFF_L  = MIN(OFF_L,OFFG(I))
        ENDDO
C
      ENDIF  ! ISMSTR    

      CALL CRFRAME_IMP(SAV, INVJ, 
     .             XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8,
     .             YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8,
     .             ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8,
!     .             R11, R21, R31, R12, R22, R32, R13, R23, R33,
     .             R,NEL)
     
!      !! POUR COMPARAISON AVEC LA FORMULATION CLASSIQUE RADIOSS
!            CALL SREPISO3(
!     .       XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8,
!     .       YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8,
!     .       ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8,
!     .       RX, RY, RZ, SX, SY, SZ, TX, TY, TZ )
!        CALL SORTHO3(
!     .       RX , RY , RZ , SX , SY , SZ , TX , TY , TZ ,
!     .       RD12, RD13, RD11, RD22, RD23, RD21, RD32, RD33, RD31)          !isolid14
!!     .       RD11, RD12, RD13, RD21, RD22, RD23, RD31, RD32, RD33)         !isolid17
!
!      !!DO I=LFT,LLT 
!        I = LLT
!        RR11= R(1,1,I)*RD11(I)+R(2,1,I)*RD21(I)+R(3,1,I)*RD31(I)
!        RR12= R(1,2,I)*RD11(I)+R(2,2,I)*RD21(I)+R(3,2,I)*RD31(I)
!        RR13= R(1,3,I)*RD11(I)+R(2,3,I)*RD21(I)+R(3,3,I)*RD31(I)
!        RR21= R(1,1,I)*RD12(I)+R(2,1,I)*RD22(I)+R(3,1,I)*RD32(I)
!        RR22= R(1,2,I)*RD12(I)+R(2,2,I)*RD22(I)+R(3,2,I)*RD32(I)
!        RR23= R(1,3,I)*RD12(I)+R(2,3,I)*RD22(I)+R(3,3,I)*RD32(I)
!        RR31= R(1,1,I)*RD13(I)+R(2,1,I)*RD23(I)+R(3,1,I)*RD33(I)
!        RR32= R(1,2,I)*RD13(I)+R(2,2,I)*RD23(I)+R(3,2,I)*RD33(I)
!        RR33= R(1,3,I)*RD13(I)+R(2,3,I)*RD23(I)+R(3,3,I)*RD33(I)
!        print*,'REPERE Pour Fint: ',I
!        write(*,'(3(3(1X,1PE10.3),6X))')R(1,1,I),R(1,2,I),R(1,3,I),RD11(I),RD12(I),RD13(I),RR11,RR12,RR13
!        write(*,'(3(3(1X,1PE10.3),6X))')R(2,1,I),R(2,2,I),R(2,3,I),RD21(I),RD22(I),RD23(I),RR21,RR22,RR23
!        write(*,'(3(3(1X,1PE10.3),6X))')R(3,1,I),R(3,2,I),R(3,3,I),RD31(I),RD32(I),RD33(I),RR31,RR32,RR33
        
      CALL CRTRANS_IMP(SAV,INVJ, 
     .             XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8,
     .             YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8,
     .             ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8,
     .             V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,
     .              R,TRM, NEL)

      IF(ISMSTR == 1)THEN
        IF(TT == ZERO) THEN
         CALL S8XREF_IMP(OFFG,XREF,
     .             XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8,
     .             YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8,
     .             ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8,
     .             R,NEL)
        ENDIF
        CALL GETULOC(XREF,                                       !Pour ISMSTR = 1
     .             XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8,
     .             YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8,
     .             ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8,
     .             ULX1,ULX2,ULX3,ULX4,ULX5,ULX6,ULX7,ULX8,
     .             ULY1,ULY2,ULY3,ULY4,ULY5,ULY6,ULY7,ULY8,
     .             ULZ1,ULZ2,ULZ3,ULZ4,ULZ5,ULZ6,ULZ7,ULZ8,
!     .             R11, R21, R31, R12, R22, R32, R13, R23, R33,
     .             R, NEL)
      ELSE
!!sb pour verification
!      CALL GETVLOC(TRM,
!     .             VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
!     .             VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
!     .             VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
!     .             ULX1,ULX2,ULX3,ULX4,ULX5,ULX6,ULX7,ULX8,
!     .             ULY1,ULY2,ULY3,ULY4,ULY5,ULY6,ULY7,ULY8,
!     .             ULZ1,ULZ2,ULZ3,ULZ4,ULZ5,ULZ6,ULZ7,ULZ8,
!     .             R11, R21, R31, R12, R22, R32, R13, R23, R33,
!     .             R, NEL)
        CALL GETULOC(SAV,                                       !Pour ISMSTR = 2 ou 4
     .             XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8,
     .             YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8,
     .             ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8,
     .             ULX1,ULX2,ULX3,ULX4,ULX5,ULX6,ULX7,ULX8,
     .             ULY1,ULY2,ULY3,ULY4,ULY5,ULY6,ULY7,ULY8,
     .             ULZ1,ULZ2,ULZ3,ULZ4,ULZ5,ULZ6,ULZ7,ULZ8,
     .             R, NEL)
      ENDIF
!     
!      CALL SREPISO3(
!     .       XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8,
!     .       YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8,
!     .       ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8,
!     .       RX, RY, RZ, SX, SY, SZ, TX, TY, TZ )
!C---
!      IF (JHBE == 14 .OR. JHBE == 24) THEN
!        CALL SORTHO3(
!     .       RX , RY , RZ , SX , SY , SZ , TX , TY , TZ ,
!     .       R12, R13, R11, R22, R23, R21, R32, R33, R31)
!      ELSEIF (JHBE == 15 ) THEN
!        CALL SCORTHO3(
!     .       RX , RY , RZ , SX , SY , SZ , TX , TY , TZ ,
!     .       R11, R12, R13, R21, R22, R23, R31, R32, R33)
!      ELSE
!        CALL SORTHO3(
!     .       RX , RY , RZ , SX , SY , SZ , TX , TY , TZ ,
!     .       R11, R12, R13, R21, R22, R23, R31, R32, R33)
!      ENDIF
!C-------sauf thick shells --------------     
!      IF (IGTYP /= 21 .AND. IGTYP /= 22) THEN
!        IF (ISORTH == 0) THEN
!          DO I=LFT,LLT                                          
!            GAMA(I,1) = UN                             
!            GAMA(I,2) = ZERO                              
!            GAMA(I,3) = ZERO
!            GAMA(I,4) = ZERO                              
!            GAMA(I,5) = UN                              
!            GAMA(I,6) = ZERO
!          ENDDO      
!        ELSE    
!          CALL SORTHDIR3(                                            
!     .        RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   , 
!     .        R11  ,R12  ,R13  ,R21  ,R22  ,R23  ,R31  ,R32  ,R33  , 
!     .        GAMA0,GAMA )                                           
!        ENDIF
!      ENDIF
c Recuperer les coordonnees courantes     
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
!C-----------
!C     PASSAGE AU REPERE CONVECTE.
!C-----------
!sb   ne s'applique que dans le cas des petites deformations (ismstr = 1)
!     il faut voir ce qu'il faut faire dans les autres cas
      IF(ISMSTR == 1)THEN
       DO I = LFT,LLT
        XD1(I) = ZERO
        YD1(I) = ZERO
        ZD1(I) = ZERO
        XD2(I) = SAV(I,1)
        YD2(I) = SAV(I,2)
        ZD2(I) = SAV(I,3)
        XD3(I) = SAV(I,4)
        YD3(I) = SAV(I,5)
        ZD3(I) = SAV(I,6)
        XD4(I) = SAV(I,7)
        YD4(I) = SAV(I,8)
        ZD4(I) = SAV(I,9)
        XD5(I) = SAV(I,10)
        YD5(I) = SAV(I,11)
        ZD5(I) = SAV(I,12)
        XD6(I) = SAV(I,13)
        YD6(I) = SAV(I,14)
        ZD6(I) = SAV(I,15)
        XD7(I) = SAV(I,16)
        YD7(I) = SAV(I,17)
        ZD7(I) = SAV(I,18)
        XD8(I) = SAV(I,19)
        YD8(I) = SAV(I,20)
        ZD8(I) = SAV(I,21)
       ENDDO
      ELSE IF(ISMSTR == 2 .OR. ISMSTR == 4) THEN
       DO I=LFT,LLT
        UG = XD2(I)-XD1(I)
        VG = YD2(I)-YD1(I)
        WG = ZD2(I)-ZD1(I)
        UL = R(1,1,I)*UG+R(2,1,I)*VG+R(3,1,I)*WG
        VL = R(1,2,I)*UG+R(2,2,I)*VG+R(3,2,I)*WG
        WL = R(1,3,I)*UG+R(2,3,I)*VG+R(3,3,I)*WG
        XD2(I) = UL
        YD2(I) = VL
        ZD2(I) = WL
        
        UG = XD3(I)-XD1(I)
        VG = YD3(I)-YD1(I)
        WG = ZD3(I)-ZD1(I)
        UL = R(1,1,I)*UG+R(2,1,I)*VG+R(3,1,I)*WG
        VL = R(1,2,I)*UG+R(2,2,I)*VG+R(3,2,I)*WG
        WL = R(1,3,I)*UG+R(2,3,I)*VG+R(3,3,I)*WG
        XD3(I) = UL
        YD3(I) = VL
        ZD3(I) = WL
      
        UG = XD4(I)-XD1(I)
        VG = YD4(I)-YD1(I)
        WG = ZD4(I)-ZD1(I)
        UL = R(1,1,I)*UG+R(2,1,I)*VG+R(3,1,I)*WG
        VL = R(1,2,I)*UG+R(2,2,I)*VG+R(3,2,I)*WG
        WL = R(1,3,I)*UG+R(2,3,I)*VG+R(3,3,I)*WG
        XD4(I) = UL
        YD4(I) = VL
        ZD4(I) = WL
      
        UG = XD5(I)-XD1(I)
        VG = YD5(I)-YD1(I)
        WG = ZD5(I)-ZD1(I)
        UL = R(1,1,I)*UG+R(2,1,I)*VG+R(3,1,I)*WG
        VL = R(1,2,I)*UG+R(2,2,I)*VG+R(3,2,I)*WG
        WL = R(1,3,I)*UG+R(2,3,I)*VG+R(3,3,I)*WG
        XD5(I) = UL
        YD5(I) = VL
        ZD5(I) = WL
      
        UG = XD6(I)-XD1(I)
        VG = YD6(I)-YD1(I)
        WG = ZD6(I)-ZD1(I)
        UL = R(1,1,I)*UG+R(2,1,I)*VG+R(3,1,I)*WG
        VL = R(1,2,I)*UG+R(2,2,I)*VG+R(3,2,I)*WG
        WL = R(1,3,I)*UG+R(2,3,I)*VG+R(3,3,I)*WG
        XD6(I) = UL
        YD6(I) = VL
        ZD6(I) = WL
      
        UG = XD7(I)-XD1(I)
        VG = YD7(I)-YD1(I)
        WG = ZD7(I)-ZD1(I)
        UL = R(1,1,I)*UG+R(2,1,I)*VG+R(3,1,I)*WG
        VL = R(1,2,I)*UG+R(2,2,I)*VG+R(3,2,I)*WG
        WL = R(1,3,I)*UG+R(2,3,I)*VG+R(3,3,I)*WG
        XD7(I) = UL
        YD7(I) = VL
        ZD7(I) = WL
      
        UG = XD8(I)-XD1(I)
        VG = YD8(I)-YD1(I)
        WG = ZD8(I)-ZD1(I)
        UL = R(1,1,I)*UG+R(2,1,I)*VG+R(3,1,I)*WG
        VL = R(1,2,I)*UG+R(2,2,I)*VG+R(3,2,I)*WG
        WL = R(1,3,I)*UG+R(2,3,I)*VG+R(3,3,I)*WG
        XD8(I) = UL
        YD8(I) = VL
        ZD8(I) = WL

        XD1(I)=ZERO
        YD1(I)=ZERO
        ZD1(I)=ZERO
       ENDDO
      ENDIF 
C-----------
C     PASSAGE DES VITESSES AU REPERE CONVECTE (OU ORTHOTROPE).
C-----------
!      CALL SRROTA3(
!     .   R11, R12, R13, R21, R22, R23, R31, R32, R33,
!     .   VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
!     .   VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
!     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8)


      
      
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
Chd|  S8SAV3_IMP                    source/elements/solid/solide8s/srcoor3_imp.F
Chd|-- called by -----------
Chd|        SRCOOR3_IMP                   source/elements/solid/solide8s/srcoor3_imp.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8SAV3_IMP(OFFG   ,SAV   ,
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
C     REAL ou REAL*8
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
        !print*,'OFFG(10) ', offg(10)
        DO I=LFT,LLT
          IF(ABS(OFFG(I)) <= UN )THEN
           SAV(I,1)=XD2(I)-XD1(I)
           SAV(I,2)=YD2(I)-YD1(I)
           SAV(I,3)=ZD2(I)-ZD1(I)
           SAV(I,4)=XD3(I)-XD1(I)
           SAV(I,5)=YD3(I)-YD1(I)
           SAV(I,6)=ZD3(I)-ZD1(I)
           SAV(I,7)=XD4(I)-XD1(I)
           SAV(I,8)=YD4(I)-YD1(I)
           SAV(I,9)=ZD4(I)-ZD1(I)
           SAV(I,10)=XD5(I)-XD1(I)
           SAV(I,11)=YD5(I)-YD1(I)
           SAV(I,12)=ZD5(I)-ZD1(I)
           SAV(I,13)=XD6(I)-XD1(I)
           SAV(I,14)=YD6(I)-YD1(I)
           SAV(I,15)=ZD6(I)-ZD1(I)
           SAV(I,16)=XD7(I)-XD1(I)
           SAV(I,17)=YD7(I)-YD1(I)
           SAV(I,18)=ZD7(I)-ZD1(I)
           SAV(I,19)=XD8(I)-XD1(I)
           SAV(I,20)=YD8(I)-YD1(I)
           SAV(I,21)=ZD8(I)-ZD1(I)
          ENDIF
        ENDDO
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  CRFRAME_IMP                   source/elements/solid/solide8s/srcoor3_imp.F
Chd|-- called by -----------
Chd|        SRCOOR3_IMP                   source/elements/solid/solide8s/srcoor3_imp.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CRFRAME_IMP(SAV, INVJ,
     .             XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8,
     .             YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8,
     .             ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8,
     .             R,NEL)
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
C     REAL ou REAL*8
!      my_real
!     .   OFFG(*)
      
      DOUBLE PRECISION 
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ),
     .   SAV(NEL,21),INVJ(MVSIZ,3,3), R(3,3,MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J      
C=======================================================================
      DOUBLE PRECISION DX_DR,DX_DS,DX_DT,DY_DR,DY_DS,DY_DT,DZ_DR,DZ_DS,DZ_DT
      DOUBLE PRECISION FMAT(3,3),UM(3,3),IC,I2C,I3C,IU,I2U,I3U
      DOUBLE PRECISION C11,C21,C31,C12,C22,C32,C13,C23,C33,DETJ0,DETM1
      DOUBLE PRECISION CC11,CC21,CC31,CC12,CC22,CC32,CC13,CC23,CC33
      DOUBLE PRECISION A,B,ZZ,A1,A2,A3,A4,X0(8),Y0(8),Z0(8)
      DOUBLE PRECISION MILLE24
      MILLE24 = 1024.0
      
      DO I=LFT,LLT
        X0(1) = ZERO
        Y0(1) = ZERO
        Z0(1) = ZERO
        X0(2) = SAV(I,1)
        Y0(2) = SAV(I,2)
        Z0(2) = SAV(I,3)
        X0(3) = SAV(I,4)
        Y0(3) = SAV(I,5)
        Z0(3) = SAV(I,6)
        X0(4) = SAV(I,7)
        Y0(4) = SAV(I,8)
        Z0(4) = SAV(I,9)
        X0(5) = SAV(I,10)
        Y0(5) = SAV(I,11)
        Z0(5) = SAV(I,12)
        X0(6) = SAV(I,13)
        Y0(6) = SAV(I,14)
        Z0(6) = SAV(I,15)
        X0(7) = SAV(I,16)
        Y0(7) = SAV(I,17)
        Z0(7) = SAV(I,18)
        X0(8) = SAV(I,19)
        Y0(8) = SAV(I,20)
        Z0(8) = SAV(I,21)

        DX_DR = (X0(3)+X0(4)+X0(7)+X0(8))-(X0(1)+X0(2)+X0(5)+X0(6))
        DY_DR = (Y0(3)+Y0(4)+Y0(7)+Y0(8))-(Y0(1)+Y0(2)+Y0(5)+Y0(6))
        DZ_DR = (Z0(3)+Z0(4)+Z0(7)+Z0(8))-(Z0(1)+Z0(2)+Z0(5)+Z0(6))
        !DX_DR = DX_DR/HUIT
        !DY_DR = DY_DR/HUIT
        !DZ_DR = DZ_DR/HUIT
        DX_DS = (X0(5)+X0(6)+X0(7)+X0(8))-(X0(1)+X0(2)+X0(3)+X0(4))
        DY_DS = (Y0(5)+Y0(6)+Y0(7)+Y0(8))-(Y0(1)+Y0(2)+Y0(3)+Y0(4))
        DZ_DS = (Z0(5)+Z0(6)+Z0(7)+Z0(8))-(Z0(1)+Z0(2)+Z0(3)+Z0(4))
        !DX_DS = DX_DS/HUIT
        !DY_DS = DY_DS/HUIT
        !DZ_DS = DZ_DS/HUIT
        DX_DT = (X0(2)+X0(3)+X0(6)+X0(7))-(X0(1)+X0(4)+X0(5)+X0(8))
        DY_DT = (Y0(2)+Y0(3)+Y0(6)+Y0(7))-(Y0(1)+Y0(4)+Y0(5)+Y0(8))
        DZ_DT = (Z0(2)+Z0(3)+Z0(6)+Z0(7))-(Z0(1)+Z0(4)+Z0(5)+Z0(8))
        !DX_DT = DX_DT/HUIT
        !DY_DT = DY_DT/HUIT
        !DZ_DT = DZ_DT/HUIT
        DETJ0 =(DX_DR*(DY_DS*DZ_DT-DZ_DS*DY_DT)
     .         -DX_DS*(DY_DR*DZ_DT-DY_DT*DZ_DR)
     .         +DX_DT*(DY_DR*DZ_DS-DY_DS*DZ_DR))
        DETM1 = UN/DETJ0
        DETM1 = HUIT*DETM1
        INVJ(I,1,1) = (DY_DS*DZ_DT-DZ_DS*DY_DT)*DETM1
        INVJ(I,2,1) = (DZ_DR*DY_DT-DY_DR*DZ_DT)*DETM1
        INVJ(I,3,1) = (DY_DR*DZ_DS-DY_DS*DZ_DR)*DETM1
        INVJ(I,1,2) = (DX_DT*DZ_DS-DX_DS*DZ_DT)*DETM1
        INVJ(I,2,2) = (DX_DR*DZ_DT-DX_DT*DZ_DR)*DETM1
        INVJ(I,3,2) = (DX_DS*DZ_DR-DX_DR*DZ_DS)*DETM1
        INVJ(I,1,3) = (DX_DS*DY_DT-DX_DT*DY_DS)*DETM1
        INVJ(I,2,3) = (DX_DT*DY_DR-DX_DR*DY_DT)*DETM1
        INVJ(I,3,3) = (DX_DR*DY_DS-DX_DS*DY_DR)*DETM1
      
        DX_DR = (XD3(I)+XD4(I)+XD7(I)+XD8(I))-(XD1(I)+XD2(I)+XD5(I)+XD6(I))
        DY_DR = (YD3(I)+YD4(I)+YD7(I)+YD8(I))-(YD1(I)+YD2(I)+YD5(I)+YD6(I))
        DZ_DR = (ZD3(I)+ZD4(I)+ZD7(I)+ZD8(I))-(ZD1(I)+ZD2(I)+ZD5(I)+ZD6(I))
        DX_DR = DX_DR/HUIT
        DY_DR = DY_DR/HUIT
        DZ_DR = DZ_DR/HUIT
        DX_DS = (XD5(I)+XD6(I)+XD7(I)+XD8(I))-(XD1(I)+XD2(I)+XD3(I)+XD4(I))
        DY_DS = (YD5(I)+YD6(I)+YD7(I)+YD8(I))-(YD1(I)+YD2(I)+YD3(I)+YD4(I))
        DZ_DS = (ZD5(I)+ZD6(I)+ZD7(I)+ZD8(I))-(ZD1(I)+ZD2(I)+ZD3(I)+ZD4(I))
        DX_DS = DX_DS/HUIT
        DY_DS = DY_DS/HUIT
        DZ_DS = DZ_DS/HUIT
        DX_DT = (XD2(I)+XD3(I)+XD6(I)+XD7(I))-(XD1(I)+XD4(I)+XD5(I)+XD8(I))
        DY_DT = (YD2(I)+YD3(I)+YD6(I)+YD7(I))-(YD1(I)+YD4(I)+YD5(I)+YD8(I))
        DZ_DT = (ZD2(I)+ZD3(I)+ZD6(I)+ZD7(I))-(ZD1(I)+ZD4(I)+ZD5(I)+ZD8(I))
        DX_DT = DX_DT/HUIT
        DY_DT = DY_DT/HUIT
        DZ_DT = DZ_DT/HUIT
        
        FMAT(1,1) = (DX_DR*INVJ(I,1,1)+DX_DS*INVJ(I,2,1)+DX_DT*INVJ(I,3,1))
        FMAT(2,1) = (DY_DR*INVJ(I,1,1)+DY_DS*INVJ(I,2,1)+DY_DT*INVJ(I,3,1))
        FMAT(3,1) = (DZ_DR*INVJ(I,1,1)+DZ_DS*INVJ(I,2,1)+DZ_DT*INVJ(I,3,1))
        FMAT(1,2) = (DX_DR*INVJ(I,1,2)+DX_DS*INVJ(I,2,2)+DX_DT*INVJ(I,3,2))
        FMAT(2,2) = (DY_DR*INVJ(I,1,2)+DY_DS*INVJ(I,2,2)+DY_DT*INVJ(I,3,2))
        FMAT(3,2) = (DZ_DR*INVJ(I,1,2)+DZ_DS*INVJ(I,2,2)+DZ_DT*INVJ(I,3,2))
        FMAT(1,3) = (DX_DR*INVJ(I,1,3)+DX_DS*INVJ(I,2,3)+DX_DT*INVJ(I,3,3))
        FMAT(2,3) = (DY_DR*INVJ(I,1,3)+DY_DS*INVJ(I,2,3)+DY_DT*INVJ(I,3,3))
        FMAT(3,3) = (DZ_DR*INVJ(I,1,3)+DZ_DS*INVJ(I,2,3)+DZ_DT*INVJ(I,3,3))
        
        C11 = FMAT(1,1)*FMAT(1,1)+FMAT(2,1)*FMAT(2,1)+FMAT(3,1)*FMAT(3,1)
        C21 = FMAT(1,2)*FMAT(1,1)+FMAT(2,2)*FMAT(2,1)+FMAT(3,2)*FMAT(3,1)
        C31 = FMAT(1,3)*FMAT(1,1)+FMAT(2,3)*FMAT(2,1)+FMAT(3,3)*FMAT(3,1)
        C12 = FMAT(1,1)*FMAT(1,2)+FMAT(2,1)*FMAT(2,2)+FMAT(3,1)*FMAT(3,2)
        C22 = FMAT(1,2)*FMAT(1,2)+FMAT(2,2)*FMAT(2,2)+FMAT(3,2)*FMAT(3,2)
        C32 = FMAT(1,3)*FMAT(1,2)+FMAT(2,3)*FMAT(2,2)+FMAT(3,3)*FMAT(3,2)
        C13 = FMAT(1,1)*FMAT(1,3)+FMAT(2,1)*FMAT(2,3)+FMAT(3,1)*FMAT(3,3)
        C23 = FMAT(1,2)*FMAT(1,3)+FMAT(2,2)*FMAT(2,3)+FMAT(3,2)*FMAT(3,3)
        C33 = FMAT(1,3)*FMAT(1,3)+FMAT(2,3)*FMAT(2,3)+FMAT(3,3)*FMAT(3,3)
        
        CC11 = C11*C11+C12*C21+C13*C31
        CC21 = C21*C11+C22*C21+C23*C31
        CC31 = C31*C11+C32*C21+C33*C31
        CC12 = C11*C12+C12*C22+C13*C32
        CC22 = C21*C12+C22*C22+C23*C32
        CC32 = C31*C12+C32*C22+C33*C32
        CC13 = C11*C13+C12*C23+C13*C33
        CC23 = C21*C13+C22*C23+C23*C33
        CC33 = C31*C13+C32*C23+C33*C33
        
        IC = C11+C22+C33
        I2C = C11*C22+C22*C33+C11*C33-C21*C12-C13*C31-C23*C32
        I3C = C11*C22*C33+C12*C23*C31+C13*C21*C32
     .      -(C13*C22*C31+C12*C21*C33+C11*C23*C32)
     	
        A = (DEUX*IC*IC*IC-NEUF*IC*I2C+VINGT7*I3C)*TRENTE2/VINGT7
	  B = (QUATRE*(I2C*I2C*I2C+IC*IC*IC*I3C)-IC*IC*I2C*I2C-DIXHUIT*IC*I2C*I3C
     .      +VINGT7*I3C*I3C)*MILLE24/VINGT7
        B = MAX(B,ZERO)
        A1 = A+SQRT(B)
        A2 = A-SQRT(B)
        !ZZ = -DTIERS*IC+CBRT(A1)+CBRT(A2)
        ZZ = -DTIERS*IC+SIGN(ABS(A1)**THIRD,A1)+SIGN(ABS(A2)**THIRD,A2)
        A = DEUX*IC+ZZ
        IF (A == ZERO) THEN
          IU = SQRT(IC+DEUX*SQRT(I2C))
        ELSE
          A = SQRT(A)
          IU = UNDEMI*(A+SQRT(DEUX*IC-ZZ+SEIZE*SQRT(I3C)/A))
        ENDIF
        I3U = SQRT(I3C)
        I2U = UNDEMI*(IU*IU-IC)
        A = IU*I2U-I3U
        B = I3U+IU*IC
        A1 = IU*A
        A2 = A*B
        A3 = I2U*I3U*B+IU*IU*(I2U*I2C+I3C)
        A4 = UN/(I3U*I3U*B+IU*IU*(IU*I3C+I3U*I2C))
        UM(1,1) = A4*(A1*CC11-A2*C11+A3)
        UM(2,1) = A4*(A1*CC21-A2*C21)
        UM(3,1) = A4*(A1*CC31-A2*C31)
        UM(1,2) = A4*(A1*CC12-A2*C12)
        UM(2,2) = A4*(A1*CC22-A2*C22+A3)
        UM(3,2) = A4*(A1*CC32-A2*C32)
        UM(1,3) = A4*(A1*CC13-A2*C13)
        UM(2,3) = A4*(A1*CC23-A2*C23)
        UM(3,3) = A4*(A1*CC33-A2*C33+A3)
        
        R(1,1,I) = FMAT(1,1)*UM(1,1)+FMAT(1,2)*UM(2,1)+FMAT(1,3)*UM(3,1)
        R(2,1,I) = FMAT(2,1)*UM(1,1)+FMAT(2,2)*UM(2,1)+FMAT(2,3)*UM(3,1)
        R(3,1,I) = FMAT(3,1)*UM(1,1)+FMAT(3,2)*UM(2,1)+FMAT(3,3)*UM(3,1)
        R(1,2,I) = FMAT(1,1)*UM(1,2)+FMAT(1,2)*UM(2,2)+FMAT(1,3)*UM(3,2)
        R(2,2,I) = FMAT(2,1)*UM(1,2)+FMAT(2,2)*UM(2,2)+FMAT(2,3)*UM(3,2)
        R(3,2,I) = FMAT(3,1)*UM(1,2)+FMAT(3,2)*UM(2,2)+FMAT(3,3)*UM(3,2)
        R(1,3,I) = FMAT(1,1)*UM(1,3)+FMAT(1,2)*UM(2,3)+FMAT(1,3)*UM(3,3)
        R(2,3,I) = FMAT(2,1)*UM(1,3)+FMAT(2,2)*UM(2,3)+FMAT(2,3)*UM(3,3)
        R(3,3,I) = FMAT(3,1)*UM(1,3)+FMAT(3,2)*UM(2,3)+FMAT(3,3)*UM(3,3)
      ENDDO
      RETURN
      END
      
Chd|====================================================================
Chd|  GETULOC                       source/elements/solid/solide8s/srcoor3_imp.F
Chd|-- called by -----------
Chd|        SRCOOR3_IMP                   source/elements/solid/solide8s/srcoor3_imp.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GETULOC(SAV,
     .             XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8,
     .             YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8,
     .             ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8,
     .             ULX1,ULX2,ULX3,ULX4,ULX5,ULX6,ULX7,ULX8,
     .             ULY1,ULY2,ULY3,ULY4,ULY5,ULY6,ULY7,ULY8,
     .             ULZ1,ULZ2,ULZ3,ULZ4,ULZ5,ULZ6,ULZ7,ULZ8,
     .             R, NEL)
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
C     REAL ou REAL*8
!      my_real
!     .   OFFG(*)
      
      DOUBLE PRECISION 
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ),
     .   SAV(NEL,21), R(3,3,MVSIZ),
     .   ULX1(MVSIZ), ULX2(MVSIZ), ULX3(MVSIZ), ULX4(MVSIZ),
     .   ULX5(MVSIZ), ULX6(MVSIZ), ULX7(MVSIZ), ULX8(MVSIZ),
     .   ULY1(MVSIZ), ULY2(MVSIZ), ULY3(MVSIZ), ULY4(MVSIZ),
     .   ULY5(MVSIZ), ULY6(MVSIZ), ULY7(MVSIZ), ULY8(MVSIZ),
     .   ULZ1(MVSIZ), ULZ2(MVSIZ), ULZ3(MVSIZ), ULZ4(MVSIZ),
     .   ULZ5(MVSIZ), ULZ6(MVSIZ), ULZ7(MVSIZ), ULZ8(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I    
C=======================================================================
      DOUBLE PRECISION U,V,W,UL,VL,WL 

      DO I=LFT,LLT
        ULX1(I) = ZERO    ! sb : On pourrait eviter de stocker ces 3 tables (toujours zero)
        ULY1(I) = ZERO
        ULZ1(I) = ZERO

        U = (XD2(I)-XD1(I))
        V = (YD2(I)-YD1(I))
        W = (ZD2(I)-ZD1(I))
        UL = R(1,1,I)*U+R(2,1,I)*V+R(3,1,I)*W
        VL = R(1,2,I)*U+R(2,2,I)*V+R(3,2,I)*W
        WL = R(1,3,I)*U+R(2,3,I)*V+R(3,3,I)*W
        ULX2(I) = UL-SAV(I,1)
        ULY2(I) = VL-SAV(I,2)
        ULZ2(I) = WL-SAV(I,3)

        U = (XD3(I)-XD1(I))
        V = (YD3(I)-YD1(I))
        W = (ZD3(I)-ZD1(I))
        UL = R(1,1,I)*U+R(2,1,I)*V+R(3,1,I)*W
        VL = R(1,2,I)*U+R(2,2,I)*V+R(3,2,I)*W
        WL = R(1,3,I)*U+R(2,3,I)*V+R(3,3,I)*W
        ULX3(I) = UL-SAV(I,4)
        ULY3(I) = VL-SAV(I,5)
        ULZ3(I) = WL-SAV(I,6)
      
        U = (XD4(I)-XD1(I))
        V = (YD4(I)-YD1(I))
        W = (ZD4(I)-ZD1(I))
        UL = R(1,1,I)*U+R(2,1,I)*V+R(3,1,I)*W
        VL = R(1,2,I)*U+R(2,2,I)*V+R(3,2,I)*W
        WL = R(1,3,I)*U+R(2,3,I)*V+R(3,3,I)*W
        ULX4(I) = UL-SAV(I,7)
        ULY4(I) = VL-SAV(I,8)
        ULZ4(I) = WL-SAV(I,9)
      
        U = (XD5(I)-XD1(I))
        V = (YD5(I)-YD1(I))
        W = (ZD5(I)-ZD1(I))
        UL = R(1,1,I)*U+R(2,1,I)*V+R(3,1,I)*W
        VL = R(1,2,I)*U+R(2,2,I)*V+R(3,2,I)*W
        WL = R(1,3,I)*U+R(2,3,I)*V+R(3,3,I)*W
        ULX5(I) = UL-SAV(I,10)
        ULY5(I) = VL-SAV(I,11)
        ULZ5(I) = WL-SAV(I,12)
      
        U = (XD6(I)-XD1(I))
        V = (YD6(I)-YD1(I))
        W = (ZD6(I)-ZD1(I))
        UL = R(1,1,I)*U+R(2,1,I)*V+R(3,1,I)*W
        VL = R(1,2,I)*U+R(2,2,I)*V+R(3,2,I)*W
        WL = R(1,3,I)*U+R(2,3,I)*V+R(3,3,I)*W
        ULX6(I) = UL-SAV(I,13)
        ULY6(I) = VL-SAV(I,14)
        ULZ6(I) = WL-SAV(I,15)
      
        U = (XD7(I)-XD1(I))
        V = (YD7(I)-YD1(I))
        W = (ZD7(I)-ZD1(I))
        UL = R(1,1,I)*U+R(2,1,I)*V+R(3,1,I)*W
        VL = R(1,2,I)*U+R(2,2,I)*V+R(3,2,I)*W
        WL = R(1,3,I)*U+R(2,3,I)*V+R(3,3,I)*W
        ULX7(I) = UL-SAV(I,16)
        ULY7(I) = VL-SAV(I,17)
        ULZ7(I) = WL-SAV(I,18)
      
        U = (XD8(I)-XD1(I))
        V = (YD8(I)-YD1(I))
        W = (ZD8(I)-ZD1(I))
        UL = R(1,1,I)*U+R(2,1,I)*V+R(3,1,I)*W
        VL = R(1,2,I)*U+R(2,2,I)*V+R(3,2,I)*W
        WL = R(1,3,I)*U+R(2,3,I)*V+R(3,3,I)*W
        ULX8(I) = UL-SAV(I,19)
        ULY8(I) = VL-SAV(I,20)
        ULZ8(I) = WL-SAV(I,21)
        
      ENDDO
      RETURN
      END
      
Chd|====================================================================
Chd|  GETVLOC                       source/elements/solid/solide8s/srcoor3_imp.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GETVLOC(TRM,
     .             VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .             VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .             VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .             ULX1,ULX2,ULX3,ULX4,ULX5,ULX6,ULX7,ULX8,
     .             ULY1,ULY2,ULY3,ULY4,ULY5,ULY6,ULY7,ULY8,
     .             ULZ1,ULZ2,ULZ3,ULZ4,ULZ5,ULZ6,ULZ7,ULZ8,
     .             R, NEL)
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
C     REAL ou REAL*8
!      my_real
!     .   OFFG(*)
      
      DOUBLE PRECISION 
     .   TRM(NEL,24,24), R(3,3,MVSIZ),    
     .   ULX1(MVSIZ), ULX2(MVSIZ), ULX3(MVSIZ), ULX4(MVSIZ),
     .   ULX5(MVSIZ), ULX6(MVSIZ), ULX7(MVSIZ), ULX8(MVSIZ),
     .   ULY1(MVSIZ), ULY2(MVSIZ), ULY3(MVSIZ), ULY4(MVSIZ),
     .   ULY5(MVSIZ), ULY6(MVSIZ), ULY7(MVSIZ), ULY8(MVSIZ),
     .   ULZ1(MVSIZ), ULZ2(MVSIZ), ULZ3(MVSIZ), ULZ4(MVSIZ),
     .   ULZ5(MVSIZ), ULZ6(MVSIZ), ULZ7(MVSIZ), ULZ8(MVSIZ)
      my_real
     .   VX1(*), VX2(*), VX3(*), VX4(*), VX5(*), VX6(*), VX7(*), VX8(*),
     .   VY1(*), VY2(*), VY3(*), VY4(*), VY5(*), VY6(*), VY7(*), VY8(*),
     .   VZ1(*), VZ2(*), VZ3(*), VZ4(*), VZ5(*), VZ6(*), VZ7(*), VZ8(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I ,J,K    
C=======================================================================
      DOUBLE PRECISION U,V,W!,UL,VL,WL 
      DOUBLE PRECISION UG(24),UL(24) 
c
      DO I=LFT,LLT
        UG(1) = ZERO 
        UG(2) = ZERO 
        UG(3) = ZERO 
        UG(4) = VX2(I)-VX1(I)
        UG(5) = VY2(I)-VY1(I)
        UG(6) = VZ2(I)-VZ1(I)
        UG(7) = VX3(I)-VX1(I)
        UG(8) = VY3(I)-VY1(I)
        UG(9) = VZ3(I)-VZ1(I)
        UG(10) = VX4(I)-VX1(I)
        UG(11) = VY4(I)-VY1(I)
        UG(12) = VZ4(I)-VZ1(I)
        UG(13) = VX5(I)-VX1(I)
        UG(14) = VY5(I)-VY1(I)
        UG(15) = VZ5(I)-VZ1(I)
        UG(16) = VX6(I)-VX1(I)
        UG(17) = VY6(I)-VY1(I)
        UG(18) = VZ6(I)-VZ1(I)
        UG(19) = VX7(I)-VX1(I)
        UG(20) = VY7(I)-VY1(I)
        UG(21) = VZ7(I)-VZ1(I)
        UG(22) = VX8(I)-VX1(I)
        UG(23) = VY8(I)-VY1(I)
        UG(24) = VZ8(I)-VZ1(I)
!        if (i==10) write(*,'("Vitesse G ",/,3(1x,1pe10.3))')   ug(1:24)
          
        DO J=1,24
          UL(J) = ZERO
          DO K=1,24
            UL(J) = UL(J)+TRM(I,J,K)*UG(K)
          ENDDO
        ENDDO
        
!        if (i==10) write(*,'("vLOC ",/,3(1x,1pe10.3))')   ul(1:24)
        !
        !U = R11(I)*VX1(I)+R21(I)*VY1(I)+R31(I)*VZ1(I)
        !V = R12(I)*VX1(I)+R22(I)*VY1(I)+R32(I)*VZ1(I)
        !W = R13(I)*VX1(I)+R23(I)*VY1(I)+R33(I)*VZ1(I)
        
        ULX1(I) = UL(1)!-u
        ULY1(I) = UL(2)!-v
        ULZ1(I) = UL(3)!-w
        ULX2(I) = UL(4)!-u
        ULY2(I) = UL(5)!-v
        ULZ2(I) = UL(6)!-w
        ULX3(I) = UL(7)!-u
        ULY3(I) = UL(8)!-v
        ULZ3(I) = UL(9)!-w
        ULX4(I) = UL(10)!-u
        ULY4(I) = UL(11)!-v
        ULZ4(I) = UL(12)!-w
        ULX5(I) = UL(13)!-u
        ULY5(I) = UL(14)!-v
        ULZ5(I) = UL(15)!-w
        ULX6(I) = UL(16)!-u
        ULY6(I) = UL(17)!-v
        ULZ6(I) = UL(18)!-w
        ULX7(I) = UL(19)!-u
        ULY7(I) = UL(20)!-v
        ULZ7(I) = UL(21)!-w
        ULX8(I) = UL(22)!-u
        ULY8(I) = UL(23)!-v
        ULZ8(I) = UL(24)!-w
        !if (i==10) then
        !  write(*,'("vLx vly vlz ",/,3(1x,1pe10.3))')   ULX1(I),ULY1(I),ULZ1(I)
        !  write(*,'(3(1x,1pe10.3))')   ULX2(I),ULY2(I),ULZ2(I)
        !  write(*,'(3(1x,1pe10.3))')   ULX3(I),ULY3(I),ULZ3(I)
        !  write(*,'(3(1x,1pe10.3))')   ULX4(I),ULY4(I),ULZ4(I)
        !  write(*,'(3(1x,1pe10.3))')   ULX5(I),ULY5(I),ULZ5(I)
        !  write(*,'(3(1x,1pe10.3))')   ULX6(I),ULY6(I),ULZ6(I)
        !  write(*,'(3(1x,1pe10.3))')   ULX7(I),ULY7(I),ULZ7(I)
        !  write(*,'(3(1x,1pe10.3))')   ULX8(I),ULY8(I),ULZ8(I)
        !endif
      ENDDO
!      print*,"vitesses :"
!      DO I=LFT,LLT
!        ULX1(I) = ZERO    ! sb : On pourrait eviter de stocker ces 3 tables (toujours zero)
!        ULY1(I) = ZERO
!        ULZ1(I) = ZERO
!
!        U = VX2(I)-VX1(I)
!        V = VY2(I)-VY1(I)
!        W = VZ2(I)-VZ1(I)
!        UL = R11(I)*U+R21(I)*V+R31(I)*W
!        VL = R12(I)*U+R22(I)*V+R32(I)*W
!        WL = R13(I)*U+R23(I)*V+R33(I)*W
!        ULX2(I) = UL
!        ULY2(I) = VL
!        ULZ2(I) = WL
!        if (i==10)       write(*,'(3(1X,1PE10.3))')U,V,W
!
!        U = VX3(I)-VX1(I)
!        V = VY3(I)-VY1(I)
!        W = VZ3(I)-VZ1(I)
!        UL = R11(I)*U+R21(I)*V+R31(I)*W
!        VL = R12(I)*U+R22(I)*V+R32(I)*W
!        WL = R13(I)*U+R23(I)*V+R33(I)*W
!        ULX3(I) = UL
!        ULY3(I) = VL
!        ULZ3(I) = WL
!        if (i==10)       write(*,'(3(1X,1PE10.3))')U,V,W
!      
!        U = VX4(I)-VX1(I)
!        V = VY4(I)-VY1(I)
!        W = VZ4(I)-VZ1(I)
!        UL = R11(I)*U+R21(I)*V+R31(I)*W
!        VL = R12(I)*U+R22(I)*V+R32(I)*W
!        WL = R13(I)*U+R23(I)*V+R33(I)*W
!        ULX4(I) = UL
!        ULY4(I) = VL
!        ULZ4(I) = WL
!        if (i==10)       write(*,'(3(1X,1PE10.3))')U,V,W
!      
!        U = VX5(I)-VX1(I)
!        V = VY5(I)-VY1(I)
!        W = VZ5(I)-VZ1(I)
!        UL = R11(I)*U+R21(I)*V+R31(I)*W
!        VL = R12(I)*U+R22(I)*V+R32(I)*W
!        WL = R13(I)*U+R23(I)*V+R33(I)*W
!        ULX5(I) = UL
!        ULY5(I) = VL
!        ULZ5(I) = WL
!        if (i==10)       write(*,'(3(1X,1PE10.3))')U,V,W
!      
!        U = VX6(I)-VX1(I)
!        V = VY6(I)-VY1(I)
!        W = VZ6(I)-VZ1(I)
!        UL = R11(I)*U+R21(I)*V+R31(I)*W
!        VL = R12(I)*U+R22(I)*V+R32(I)*W
!        WL = R13(I)*U+R23(I)*V+R33(I)*W
!        ULX6(I) = UL
!        ULY6(I) = VL
!        ULZ6(I) = WL
!        if (i==10)       write(*,'(3(1X,1PE10.3))')U,V,W
!      
!        U = VX7(I)-VX1(I)
!        V = VY7(I)-VY1(I)
!        W = VZ7(I)-VZ1(I)
!        UL = R11(I)*U+R21(I)*V+R31(I)*W
!        VL = R12(I)*U+R22(I)*V+R32(I)*W
!        WL = R13(I)*U+R23(I)*V+R33(I)*W
!        ULX7(I) = UL
!        ULY7(I) = VL
!        ULZ7(I) = WL
!        if (i==10)       write(*,'(3(1X,1PE10.3))')U,V,W
!      
!        U = VX8(I)-VX1(I)
!        V = VY8(I)-VY1(I)
!        W = VZ8(I)-VZ1(I)
!        UL = R11(I)*U+R21(I)*V+R31(I)*W
!        VL = R12(I)*U+R22(I)*V+R32(I)*W
!        WL = R13(I)*U+R23(I)*V+R33(I)*W
!        ULX8(I) = UL
!        ULY8(I) = VL
!        ULZ8(I) = WL
!        if (i==10)       write(*,'(3(1X,1PE10.3))')U,V,W
!        if (i==10) then
!          write(*,'("vLx vly vlz ",/,3(1x,1pe10.3))')   ULX1(I),ULY1(I),ULZ1(I)
!          write(*,'(3(1x,1pe10.3))')   ULX2(I),ULY2(I),ULZ2(I)
!          write(*,'(3(1x,1pe10.3))')   ULX3(I),ULY3(I),ULZ3(I)
!          write(*,'(3(1x,1pe10.3))')   ULX4(I),ULY4(I),ULZ4(I)
!          write(*,'(3(1x,1pe10.3))')   ULX5(I),ULY5(I),ULZ5(I)
!          write(*,'(3(1x,1pe10.3))')   ULX6(I),ULY6(I),ULZ6(I)
!          write(*,'(3(1x,1pe10.3))')   ULX7(I),ULY7(I),ULZ7(I)
!          write(*,'(3(1x,1pe10.3))')   ULX8(I),ULY8(I),ULZ8(I)
!        endif
!       
!      ENDDO
      RETURN
      END
      
Chd|====================================================================
Chd|  CRTRANS_IMP                   source/elements/solid/solide8s/srcoor3_imp.F
Chd|-- called by -----------
Chd|        S8SCOORK_IMP                  source/elements/solid/solide8s/srcoork_imp.F
Chd|        SRCOOR3_IMP                   source/elements/solid/solide8s/srcoor3_imp.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CRTRANS_IMP(SAV, INVJ,
     .             XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8,
     .             YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8,
     .             ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8,
     .             V1,  V2,  V3,  V4,  V5,  V6,  V7,  V8,
     .             R  , TRM, NEL)
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
C     REAL ou REAL*8
!      my_real
!     .   OFFG(*)
      
      DOUBLE PRECISION 
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ),
     .   SAV(NEL,21),INVJ(MVSIZ,3,3),
     .   V1(MVSIZ,3,3), V2(MVSIZ,3,3), V3(MVSIZ,3,3), V4(MVSIZ,3,3),
     .   V5(MVSIZ,3,3), V6(MVSIZ,3,3), V7(MVSIZ,3,3), V8(MVSIZ,3,3),
     .   TRM(NEL,24,24),R(3,3,MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,JJ      
C=======================================================================
      DOUBLE PRECISION DX_DR,DX_DS,DX_DT,DY_DR,DY_DS,DY_DT,DZ_DR,DZ_DS,DZ_DT
      DOUBLE PRECISION FMAT(3,3),UM(3,3),IC,I2C,I3C,IU,I2U,I3U
      DOUBLE PRECISION 
     .   SA11,SA21,SA31,SA12,SA22,SA32,SA13,SA23,SA33,
     .   SB11,SB21,SB31,SB12,SB22,SB32,SB13,SB23,SB33,
     .   SC11,SC21,SC31,SC12,SC22,SC32,SC13,SC23,SC33,
     .   SD11,SD21,SD31,SD12,SD22,SD32,SD13,SD23,SD33,
     .   SE11,SE21,SE31,SE12,SE22,SE32,SE13,SE23,SE33,
     .   SF11,SF21,SF31,SF12,SF22,SF32,SF13,SF23,SF33,
     .   SG11,SG21,SG31,SG12,SG22,SG32,SG13,SG23,SG33,
     .   S11,S21,S31,S12,S22,S32,S13,S23,S33
      DOUBLE PRECISION 
     .   B11,B21,B31,B12,B22,B32,B13,B23,B33,BB,
     .   BI11,BI21,BI31,BI12,BI22,BI32,BI13,BI23,BI33
      DOUBLE PRECISION AA 
      DOUBLE PRECISION 
     .   DN1_DX(MVSIZ), DN1_DY(MVSIZ), DN1_DZ(MVSIZ),
     .   DN2_DX(MVSIZ), DN2_DY(MVSIZ), DN2_DZ(MVSIZ),
     .   DN3_DX(MVSIZ), DN3_DY(MVSIZ), DN3_DZ(MVSIZ),
     .   DN4_DX(MVSIZ), DN4_DY(MVSIZ), DN4_DZ(MVSIZ),
     .   DN5_DX(MVSIZ), DN5_DY(MVSIZ), DN5_DZ(MVSIZ),
     .   DN6_DX(MVSIZ), DN6_DY(MVSIZ), DN6_DZ(MVSIZ),
     .   DN7_DX(MVSIZ), DN7_DY(MVSIZ), DN7_DZ(MVSIZ),
     .   DN8_DX(MVSIZ), DN8_DY(MVSIZ), DN8_DZ(MVSIZ),
     .   XI1(MVSIZ), XI2(MVSIZ), XI3(MVSIZ), XI4(MVSIZ),
     .   XI5(MVSIZ), XI6(MVSIZ), XI7(MVSIZ), XI8(MVSIZ),
     .   YI1(MVSIZ), YI2(MVSIZ), YI3(MVSIZ), YI4(MVSIZ),
     .   YI5(MVSIZ), YI6(MVSIZ), YI7(MVSIZ), YI8(MVSIZ),
     .   ZI1(MVSIZ), ZI2(MVSIZ), ZI3(MVSIZ), ZI4(MVSIZ),
     .   ZI5(MVSIZ), ZI6(MVSIZ), ZI7(MVSIZ), ZI8(MVSIZ)
      
      AA = UN/HUIT
      
      DO I=LFT,LLT
        XI1(I) = ZERO
        YI1(I) = ZERO
        ZI1(I) = ZERO
        XI2(I) = XD2(I)-XD1(I)
        YI2(I) = YD2(I)-YD1(I)
        ZI2(I) = ZD2(I)-ZD1(I)
        XI3(I) = XD3(I)-XD1(I)
        YI3(I) = YD3(I)-YD1(I)
        ZI3(I) = ZD3(I)-ZD1(I)
        XI4(I) = XD4(I)-XD1(I)
        YI4(I) = YD4(I)-YD1(I)
        ZI4(I) = ZD4(I)-ZD1(I)
        XI5(I) = XD5(I)-XD1(I)
        YI5(I) = YD5(I)-YD1(I)
        ZI5(I) = ZD5(I)-ZD1(I)
        XI6(I) = XD6(I)-XD1(I)
        YI6(I) = YD6(I)-YD1(I)
        ZI6(I) = ZD6(I)-ZD1(I)
        XI7(I) = XD7(I)-XD1(I)
        YI7(I) = YD7(I)-YD1(I)
        ZI7(I) = ZD7(I)-ZD1(I)
        XI8(I) = XD8(I)-XD1(I)
        YI8(I) = YD8(I)-YD1(I)
        ZI8(I) = ZD8(I)-ZD1(I)
      ENDDO
      
      DO I=LFT,LLT
        DN1_DX(I) = (-INVJ(I,1,1)-INVJ(I,2,1)-INVJ(I,3,1))*AA
        DN2_DX(I) = (-INVJ(I,1,1)-INVJ(I,2,1)+INVJ(I,3,1))*AA
        DN3_DX(I) = ( INVJ(I,1,1)-INVJ(I,2,1)+INVJ(I,3,1))*AA
        DN4_DX(I) = ( INVJ(I,1,1)-INVJ(I,2,1)-INVJ(I,3,1))*AA
        DN5_DX(I) = (-INVJ(I,1,1)+INVJ(I,2,1)-INVJ(I,3,1))*AA
        DN6_DX(I) = (-INVJ(I,1,1)+INVJ(I,2,1)+INVJ(I,3,1))*AA
        DN7_DX(I) = ( INVJ(I,1,1)+INVJ(I,2,1)+INVJ(I,3,1))*AA
        DN8_DX(I) = ( INVJ(I,1,1)+INVJ(I,2,1)-INVJ(I,3,1))*AA
        
        DN1_DY(I) = (-INVJ(I,1,2)-INVJ(I,2,2)-INVJ(I,3,2))*AA
        DN2_DY(I) = (-INVJ(I,1,2)-INVJ(I,2,2)+INVJ(I,3,2))*AA
        DN3_DY(I) = ( INVJ(I,1,2)-INVJ(I,2,2)+INVJ(I,3,2))*AA
        DN4_DY(I) = ( INVJ(I,1,2)-INVJ(I,2,2)-INVJ(I,3,2))*AA
        DN5_DY(I) = (-INVJ(I,1,2)+INVJ(I,2,2)-INVJ(I,3,2))*AA
        DN6_DY(I) = (-INVJ(I,1,2)+INVJ(I,2,2)+INVJ(I,3,2))*AA
        DN7_DY(I) = ( INVJ(I,1,2)+INVJ(I,2,2)+INVJ(I,3,2))*AA
        DN8_DY(I) = ( INVJ(I,1,2)+INVJ(I,2,2)-INVJ(I,3,2))*AA
      
        DN1_DZ(I) = (-INVJ(I,1,3)-INVJ(I,2,3)-INVJ(I,3,3))*AA
        DN2_DZ(I) = (-INVJ(I,1,3)-INVJ(I,2,3)+INVJ(I,3,3))*AA
        DN3_DZ(I) = ( INVJ(I,1,3)-INVJ(I,2,3)+INVJ(I,3,3))*AA
        DN4_DZ(I) = ( INVJ(I,1,3)-INVJ(I,2,3)-INVJ(I,3,3))*AA
        DN5_DZ(I) = (-INVJ(I,1,3)+INVJ(I,2,3)-INVJ(I,3,3))*AA
        DN6_DZ(I) = (-INVJ(I,1,3)+INVJ(I,2,3)+INVJ(I,3,3))*AA
        DN7_DZ(I) = ( INVJ(I,1,3)+INVJ(I,2,3)+INVJ(I,3,3))*AA
        DN8_DZ(I) = ( INVJ(I,1,3)+INVJ(I,2,3)-INVJ(I,3,3))*AA
        
        SA11 = R(2,1,I)*ZI2(I)-R(3,1,I)*YI2(I)     ! Noeud 2
        SA12 =-R(1,1,I)*ZI2(I)+R(3,1,I)*XI2(I)
        SA13 = R(1,1,I)*YI2(I)-R(2,1,I)*XI2(I)
        SA21 = R(2,2,I)*ZI2(I)-R(3,2,I)*YI2(I)
        SA22 =-R(1,2,I)*ZI2(I)+R(3,2,I)*XI2(I)
        SA23 = R(1,2,I)*YI2(I)-R(2,2,I)*XI2(I)
        SA31 = R(2,3,I)*ZI2(I)-R(3,3,I)*YI2(I)
        SA32 =-R(1,3,I)*ZI2(I)+R(3,3,I)*XI2(I)
        SA33 = R(1,3,I)*YI2(I)-R(2,3,I)*XI2(I)
        SB11 = R(2,1,I)*ZI3(I)-R(3,1,I)*YI3(I)     ! Noeud 3
        SB12 =-R(1,1,I)*ZI3(I)+R(3,1,I)*XI3(I)
        SB13 = R(1,1,I)*YI3(I)-R(2,1,I)*XI3(I)
        SB21 = R(2,2,I)*ZI3(I)-R(3,2,I)*YI3(I)
        SB22 =-R(1,2,I)*ZI3(I)+R(3,2,I)*XI3(I)
        SB23 = R(1,2,I)*YI3(I)-R(2,2,I)*XI3(I)
        SB31 = R(2,3,I)*ZI3(I)-R(3,3,I)*YI3(I)
        SB32 =-R(1,3,I)*ZI3(I)+R(3,3,I)*XI3(I)
        SB33 = R(1,3,I)*YI3(I)-R(2,3,I)*XI3(I)
        SC11 = R(2,1,I)*ZI4(I)-R(3,1,I)*YI4(I)     ! Noeud 4
        SC12 =-R(1,1,I)*ZI4(I)+R(3,1,I)*XI4(I)
        SC13 = R(1,1,I)*YI4(I)-R(2,1,I)*XI4(I)
        SC21 = R(2,2,I)*ZI4(I)-R(3,2,I)*YI4(I)
        SC22 =-R(1,2,I)*ZI4(I)+R(3,2,I)*XI4(I)
        SC23 = R(1,2,I)*YI4(I)-R(2,2,I)*XI4(I)
        SC31 = R(2,3,I)*ZI4(I)-R(3,3,I)*YI4(I)
        SC32 =-R(1,3,I)*ZI4(I)+R(3,3,I)*XI4(I)
        SC33 = R(1,3,I)*YI4(I)-R(2,3,I)*XI4(I)
        SD11 = R(2,1,I)*ZI5(I)-R(3,1,I)*YI5(I)     ! Noeud 5
        SD12 =-R(1,1,I)*ZI5(I)+R(3,1,I)*XI5(I)
        SD13 = R(1,1,I)*YI5(I)-R(2,1,I)*XI5(I)
        SD21 = R(2,2,I)*ZI5(I)-R(3,2,I)*YI5(I)
        SD22 =-R(1,2,I)*ZI5(I)+R(3,2,I)*XI5(I)
        SD23 = R(1,2,I)*YI5(I)-R(2,2,I)*XI5(I)
        SD31 = R(2,3,I)*ZI5(I)-R(3,3,I)*YI5(I)
        SD32 =-R(1,3,I)*ZI5(I)+R(3,3,I)*XI5(I)
        SD33 = R(1,3,I)*YI5(I)-R(2,3,I)*XI5(I)
        SE11 = R(2,1,I)*ZI6(I)-R(3,1,I)*YI6(I)     ! Noeud 6
        SE12 =-R(1,1,I)*ZI6(I)+R(3,1,I)*XI6(I)
        SE13 = R(1,1,I)*YI6(I)-R(2,1,I)*XI6(I)
        SE21 = R(2,2,I)*ZI6(I)-R(3,2,I)*YI6(I)
        SE22 =-R(1,2,I)*ZI6(I)+R(3,2,I)*XI6(I)
        SE23 = R(1,2,I)*YI6(I)-R(2,2,I)*XI6(I)
        SE31 = R(2,3,I)*ZI6(I)-R(3,3,I)*YI6(I)
        SE32 =-R(1,3,I)*ZI6(I)+R(3,3,I)*XI6(I)
        SE33 = R(1,3,I)*YI6(I)-R(2,3,I)*XI6(I)
        SF11 = R(2,1,I)*ZI7(I)-R(3,1,I)*YI7(I)     ! Noeud 7
        SF12 =-R(1,1,I)*ZI7(I)+R(3,1,I)*XI7(I)
        SF13 = R(1,1,I)*YI7(I)-R(2,1,I)*XI7(I)
        SF21 = R(2,2,I)*ZI7(I)-R(3,2,I)*YI7(I)
        SF22 =-R(1,2,I)*ZI7(I)+R(3,2,I)*XI7(I)
        SF23 = R(1,2,I)*YI7(I)-R(2,2,I)*XI7(I)
        SF31 = R(2,3,I)*ZI7(I)-R(3,3,I)*YI7(I)
        SF32 =-R(1,3,I)*ZI7(I)+R(3,3,I)*XI7(I)
        SF33 = R(1,3,I)*YI7(I)-R(2,3,I)*XI7(I)
        SG11 = R(2,1,I)*ZI8(I)-R(3,1,I)*YI8(I)     ! Noeud 8
        SG12 =-R(1,1,I)*ZI8(I)+R(3,1,I)*XI8(I)
        SG13 = R(1,1,I)*YI8(I)-R(2,1,I)*XI8(I)
        SG21 = R(2,2,I)*ZI8(I)-R(3,2,I)*YI8(I)
        SG22 =-R(1,2,I)*ZI8(I)+R(3,2,I)*XI8(I)
        SG23 = R(1,2,I)*YI8(I)-R(2,2,I)*XI8(I)
        SG31 = R(2,3,I)*ZI8(I)-R(3,3,I)*YI8(I)
        SG32 =-R(1,3,I)*ZI8(I)+R(3,3,I)*XI8(I)
        SG33 = R(1,3,I)*YI8(I)-R(2,3,I)*XI8(I)
        
        B11 = -DN2_DZ(I)*SA21+DN2_DY(I)*SA31
     .        -DN3_DZ(I)*SB21+DN3_DY(I)*SB31
     .        -DN4_DZ(I)*SC21+DN4_DY(I)*SC31
     .        -DN5_DZ(I)*SD21+DN5_DY(I)*SD31
     .        -DN6_DZ(I)*SE21+DN6_DY(I)*SE31
     .        -DN7_DZ(I)*SF21+DN7_DY(I)*SF31
     .        -DN8_DZ(I)*SG21+DN8_DY(I)*SG31
        B12 = -DN2_DZ(I)*SA22+DN2_DY(I)*SA32
     .        -DN3_DZ(I)*SB22+DN3_DY(I)*SB32
     .        -DN4_DZ(I)*SC22+DN4_DY(I)*SC32
     .        -DN5_DZ(I)*SD22+DN5_DY(I)*SD32
     .        -DN6_DZ(I)*SE22+DN6_DY(I)*SE32
     .        -DN7_DZ(I)*SF22+DN7_DY(I)*SF32
     .        -DN8_DZ(I)*SG22+DN8_DY(I)*SG32
        B13 = -DN2_DZ(I)*SA23+DN2_DY(I)*SA33
     .        -DN3_DZ(I)*SB23+DN3_DY(I)*SB33
     .        -DN4_DZ(I)*SC23+DN4_DY(I)*SC33
     .        -DN5_DZ(I)*SD23+DN5_DY(I)*SD33
     .        -DN6_DZ(I)*SE23+DN6_DY(I)*SE33
     .        -DN7_DZ(I)*SF23+DN7_DY(I)*SF33
     .        -DN8_DZ(I)*SG23+DN8_DY(I)*SG33
        B21 =  DN2_DZ(I)*SA11-DN2_DX(I)*SA31
     .        +DN3_DZ(I)*SB11-DN3_DX(I)*SB31
     .        +DN4_DZ(I)*SC11-DN4_DX(I)*SC31
     .        +DN5_DZ(I)*SD11-DN5_DX(I)*SD31
     .        +DN6_DZ(I)*SE11-DN6_DX(I)*SE31
     .        +DN7_DZ(I)*SF11-DN7_DX(I)*SF31
     .        +DN8_DZ(I)*SG11-DN8_DX(I)*SG31
        B22 =  DN2_DZ(I)*SA12-DN2_DX(I)*SA32
     .        +DN3_DZ(I)*SB12-DN3_DX(I)*SB32
     .        +DN4_DZ(I)*SC12-DN4_DX(I)*SC32
     .        +DN5_DZ(I)*SD12-DN5_DX(I)*SD32
     .        +DN6_DZ(I)*SE12-DN6_DX(I)*SE32
     .        +DN7_DZ(I)*SF12-DN7_DX(I)*SF32
     .        +DN8_DZ(I)*SG12-DN8_DX(I)*SG32
        B23 =  DN2_DZ(I)*SA13-DN2_DX(I)*SA33
     .        +DN3_DZ(I)*SB13-DN3_DX(I)*SB33
     .        +DN4_DZ(I)*SC13-DN4_DX(I)*SC33
     .        +DN5_DZ(I)*SD13-DN5_DX(I)*SD33
     .        +DN6_DZ(I)*SE13-DN6_DX(I)*SE33
     .        +DN7_DZ(I)*SF13-DN7_DX(I)*SF33
     .        +DN8_DZ(I)*SG13-DN8_DX(I)*SG33
        B31 = -DN2_DY(I)*SA11+DN2_DX(I)*SA21
     .        -DN3_DY(I)*SB11+DN3_DX(I)*SB21
     .        -DN4_DY(I)*SC11+DN4_DX(I)*SC21
     .        -DN5_DY(I)*SD11+DN5_DX(I)*SD21
     .        -DN6_DY(I)*SE11+DN6_DX(I)*SE21
     .        -DN7_DY(I)*SF11+DN7_DX(I)*SF21
     .        -DN8_DY(I)*SG11+DN8_DX(I)*SG21
        B32 = -DN2_DY(I)*SA12+DN2_DX(I)*SA22
     .        -DN3_DY(I)*SB12+DN3_DX(I)*SB22
     .        -DN4_DY(I)*SC12+DN4_DX(I)*SC22
     .        -DN5_DY(I)*SD12+DN5_DX(I)*SD22
     .        -DN6_DY(I)*SE12+DN6_DX(I)*SE22
     .        -DN7_DY(I)*SF12+DN7_DX(I)*SF22
     .        -DN8_DY(I)*SG12+DN8_DX(I)*SG22
        B33 = -DN2_DY(I)*SA13+DN2_DX(I)*SA23
     .        -DN3_DY(I)*SB13+DN3_DX(I)*SB23
     .        -DN4_DY(I)*SC13+DN4_DX(I)*SC23
     .        -DN5_DY(I)*SD13+DN5_DX(I)*SD23
     .        -DN6_DY(I)*SE13+DN6_DX(I)*SE23
     .        -DN7_DY(I)*SF13+DN7_DX(I)*SF23
     .        -DN8_DY(I)*SG13+DN8_DX(I)*SG23
        BB = B11*(B22*B33-B23*B32)               !Determinant
     .      -B12*(B21*B33-B31*B23)
     .      +B13*(B21*B32-B31*B22)
        IF (BB /= ZERO) BB = UN/BB
        BI11 = (B22*B33-B32*B23)*BB              !Inverse
        BI21 = (B31*B23-B21*B33)*BB
        BI31 = (B21*B32-B31*B22)*BB
        BI12 = (B13*B32-B12*B33)*BB
        BI22 = (B11*B33-B13*B31)*BB
        BI32 = (B12*B31-B11*B32)*BB
        BI13 = (B12*B23-B13*B22)*BB
        BI23 = (B21*B13-B11*B23)*BB
        BI33 = (B11*B22-B12*B21)*BB
        
        S11 = -DN1_DZ(I)*R(1,2,I)+DN1_DY(I)*R(1,3,I)
        S21 =  DN1_DZ(I)*R(1,1,I)-DN1_DX(I)*R(1,3,I)
        S31 = -DN1_DY(I)*R(1,1,I)+DN1_DX(I)*R(1,2,I)
        S12 = -DN1_DZ(I)*R(2,2,I)+DN1_DY(I)*R(2,3,I)
        S22 =  DN1_DZ(I)*R(2,1,I)-DN1_DX(I)*R(2,3,I)
        S32 = -DN1_DY(I)*R(2,1,I)+DN1_DX(I)*R(2,2,I)
        S13 = -DN1_DZ(I)*R(3,2,I)+DN1_DY(I)*R(3,3,I)
        S23 =  DN1_DZ(I)*R(3,1,I)-DN1_DX(I)*R(3,3,I)
        S33 = -DN1_DY(I)*R(3,1,I)+DN1_DX(I)*R(3,2,I)
        
        V1(I,1,1) = -(BI11*S11+BI12*S21+BI13*S31)
        V1(I,2,1) = -(BI21*S11+BI22*S21+BI23*S31)
        V1(I,3,1) = -(BI31*S11+BI32*S21+BI33*S31)
        V1(I,1,2) = -(BI11*S12+BI12*S22+BI13*S32)
        V1(I,2,2) = -(BI21*S12+BI22*S22+BI23*S32)
        V1(I,3,2) = -(BI31*S12+BI32*S22+BI33*S32)
        V1(I,1,3) = -(BI11*S13+BI12*S23+BI13*S33)
        V1(I,2,3) = -(BI21*S13+BI22*S23+BI23*S33)
        V1(I,3,3) = -(BI31*S13+BI32*S23+BI33*S33)

        S11 = -DN2_DZ(I)*R(1,2,I)+DN2_DY(I)*R(1,3,I)
        S21 =  DN2_DZ(I)*R(1,1,I)-DN2_DX(I)*R(1,3,I)
        S31 = -DN2_DY(I)*R(1,1,I)+DN2_DX(I)*R(1,2,I)
        S12 = -DN2_DZ(I)*R(2,2,I)+DN2_DY(I)*R(2,3,I)
        S22 =  DN2_DZ(I)*R(2,1,I)-DN2_DX(I)*R(2,3,I)
        S32 = -DN2_DY(I)*R(2,1,I)+DN2_DX(I)*R(2,2,I)
        S13 = -DN2_DZ(I)*R(3,2,I)+DN2_DY(I)*R(3,3,I)
        S23 =  DN2_DZ(I)*R(3,1,I)-DN2_DX(I)*R(3,3,I)
        S33 = -DN2_DY(I)*R(3,1,I)+DN2_DX(I)*R(3,2,I)

        V2(I,1,1) = -(BI11*S11+BI12*S21+BI13*S31)
        V2(I,2,1) = -(BI21*S11+BI22*S21+BI23*S31)
        V2(I,3,1) = -(BI31*S11+BI32*S21+BI33*S31)
        V2(I,1,2) = -(BI11*S12+BI12*S22+BI13*S32)
        V2(I,2,2) = -(BI21*S12+BI22*S22+BI23*S32)
        V2(I,3,2) = -(BI31*S12+BI32*S22+BI33*S32)
        V2(I,1,3) = -(BI11*S13+BI12*S23+BI13*S33)
        V2(I,2,3) = -(BI21*S13+BI22*S23+BI23*S33)
        V2(I,3,3) = -(BI31*S13+BI32*S23+BI33*S33)
        
        S11 = -DN3_DZ(I)*R(1,2,I)+DN3_DY(I)*R(1,3,I)
        S21 =  DN3_DZ(I)*R(1,1,I)-DN3_DX(I)*R(1,3,I)
        S31 = -DN3_DY(I)*R(1,1,I)+DN3_DX(I)*R(1,2,I)
        S12 = -DN3_DZ(I)*R(2,2,I)+DN3_DY(I)*R(2,3,I)
        S22 =  DN3_DZ(I)*R(2,1,I)-DN3_DX(I)*R(2,3,I)                      
        S32 = -DN3_DY(I)*R(2,1,I)+DN3_DX(I)*R(2,2,I)
        S13 = -DN3_DZ(I)*R(3,2,I)+DN3_DY(I)*R(3,3,I)
        S23 =  DN3_DZ(I)*R(3,1,I)-DN3_DX(I)*R(3,3,I)
        S33 = -DN3_DY(I)*R(3,1,I)+DN3_DX(I)*R(3,2,I)

        V3(I,1,1) = -(BI11*S11+BI12*S21+BI13*S31)
        V3(I,2,1) = -(BI21*S11+BI22*S21+BI23*S31)
        V3(I,3,1) = -(BI31*S11+BI32*S21+BI33*S31)
        V3(I,1,2) = -(BI11*S12+BI12*S22+BI13*S32)
        V3(I,2,2) = -(BI21*S12+BI22*S22+BI23*S32)
        V3(I,3,2) = -(BI31*S12+BI32*S22+BI33*S32)
        V3(I,1,3) = -(BI11*S13+BI12*S23+BI13*S33)
        V3(I,2,3) = -(BI21*S13+BI22*S23+BI23*S33)
        V3(I,3,3) = -(BI31*S13+BI32*S23+BI33*S33)
        
        S11 = -DN4_DZ(I)*R(1,2,I)+DN4_DY(I)*R(1,3,I)
        S21 =  DN4_DZ(I)*R(1,1,I)-DN4_DX(I)*R(1,3,I)
        S31 = -DN4_DY(I)*R(1,1,I)+DN4_DX(I)*R(1,2,I)
        S12 = -DN4_DZ(I)*R(2,2,I)+DN4_DY(I)*R(2,3,I)
        S22 =  DN4_DZ(I)*R(2,1,I)-DN4_DX(I)*R(2,3,I)
        S32 = -DN4_DY(I)*R(2,1,I)+DN4_DX(I)*R(2,2,I)
        S13 = -DN4_DZ(I)*R(3,2,I)+DN4_DY(I)*R(3,3,I)
        S23 =  DN4_DZ(I)*R(3,1,I)-DN4_DX(I)*R(3,3,I)
        S33 = -DN4_DY(I)*R(3,1,I)+DN4_DX(I)*R(3,2,I)

        V4(I,1,1) = -(BI11*S11+BI12*S21+BI13*S31)
        V4(I,2,1) = -(BI21*S11+BI22*S21+BI23*S31)
        V4(I,3,1) = -(BI31*S11+BI32*S21+BI33*S31)
        V4(I,1,2) = -(BI11*S12+BI12*S22+BI13*S32)
        V4(I,2,2) = -(BI21*S12+BI22*S22+BI23*S32)
        V4(I,3,2) = -(BI31*S12+BI32*S22+BI33*S32)
        V4(I,1,3) = -(BI11*S13+BI12*S23+BI13*S33)
        V4(I,2,3) = -(BI21*S13+BI22*S23+BI23*S33)
        V4(I,3,3) = -(BI31*S13+BI32*S23+BI33*S33)
        
        S11 = -DN5_DZ(I)*R(1,2,I)+DN5_DY(I)*R(1,3,I)
        S21 =  DN5_DZ(I)*R(1,1,I)-DN5_DX(I)*R(1,3,I)
        S31 = -DN5_DY(I)*R(1,1,I)+DN5_DX(I)*R(1,2,I)
        S12 = -DN5_DZ(I)*R(2,2,I)+DN5_DY(I)*R(2,3,I)
        S22 =  DN5_DZ(I)*R(2,1,I)-DN5_DX(I)*R(2,3,I)
        S32 = -DN5_DY(I)*R(2,1,I)+DN5_DX(I)*R(2,2,I)
        S13 = -DN5_DZ(I)*R(3,2,I)+DN5_DY(I)*R(3,3,I)
        S23 =  DN5_DZ(I)*R(3,1,I)-DN5_DX(I)*R(3,3,I)
        S33 = -DN5_DY(I)*R(3,1,I)+DN5_DX(I)*R(3,2,I)

        V5(I,1,1) = -(BI11*S11+BI12*S21+BI13*S31)
        V5(I,2,1) = -(BI21*S11+BI22*S21+BI23*S31)
        V5(I,3,1) = -(BI31*S11+BI32*S21+BI33*S31)
        V5(I,1,2) = -(BI11*S12+BI12*S22+BI13*S32)
        V5(I,2,2) = -(BI21*S12+BI22*S22+BI23*S32)
        V5(I,3,2) = -(BI31*S12+BI32*S22+BI33*S32)
        V5(I,1,3) = -(BI11*S13+BI12*S23+BI13*S33)
        V5(I,2,3) = -(BI21*S13+BI22*S23+BI23*S33)
        V5(I,3,3) = -(BI31*S13+BI32*S23+BI33*S33)
        
        S11 = -DN6_DZ(I)*R(1,2,I)+DN6_DY(I)*R(1,3,I)
        S21 =  DN6_DZ(I)*R(1,1,I)-DN6_DX(I)*R(1,3,I)
        S31 = -DN6_DY(I)*R(1,1,I)+DN6_DX(I)*R(1,2,I)
        S12 = -DN6_DZ(I)*R(2,2,I)+DN6_DY(I)*R(2,3,I)
        S22 =  DN6_DZ(I)*R(2,1,I)-DN6_DX(I)*R(2,3,I)
        S32 = -DN6_DY(I)*R(2,1,I)+DN6_DX(I)*R(2,2,I)
        S13 = -DN6_DZ(I)*R(3,2,I)+DN6_DY(I)*R(3,3,I)
        S23 =  DN6_DZ(I)*R(3,1,I)-DN6_DX(I)*R(3,3,I)
        S33 = -DN6_DY(I)*R(3,1,I)+DN6_DX(I)*R(3,2,I)

        V6(I,1,1) = -(BI11*S11+BI12*S21+BI13*S31)
        V6(I,2,1) = -(BI21*S11+BI22*S21+BI23*S31)
        V6(I,3,1) = -(BI31*S11+BI32*S21+BI33*S31)
        V6(I,1,2) = -(BI11*S12+BI12*S22+BI13*S32)
        V6(I,2,2) = -(BI21*S12+BI22*S22+BI23*S32)
        V6(I,3,2) = -(BI31*S12+BI32*S22+BI33*S32)
        V6(I,1,3) = -(BI11*S13+BI12*S23+BI13*S33)
        V6(I,2,3) = -(BI21*S13+BI22*S23+BI23*S33)
        V6(I,3,3) = -(BI31*S13+BI32*S23+BI33*S33)

        S11 = -DN7_DZ(I)*R(1,2,I)+DN7_DY(I)*R(1,3,I)
        S21 =  DN7_DZ(I)*R(1,1,I)-DN7_DX(I)*R(1,3,I)
        S31 = -DN7_DY(I)*R(1,1,I)+DN7_DX(I)*R(1,2,I)
        S12 = -DN7_DZ(I)*R(2,2,I)+DN7_DY(I)*R(2,3,I)
        S22 =  DN7_DZ(I)*R(2,1,I)-DN7_DX(I)*R(2,3,I)
        S32 = -DN7_DY(I)*R(2,1,I)+DN7_DX(I)*R(2,2,I)
        S13 = -DN7_DZ(I)*R(3,2,I)+DN7_DY(I)*R(3,3,I)
        S23 =  DN7_DZ(I)*R(3,1,I)-DN7_DX(I)*R(3,3,I)
        S33 = -DN7_DY(I)*R(3,1,I)+DN7_DX(I)*R(3,2,I)

        V7(I,1,1) = -(BI11*S11+BI12*S21+BI13*S31)
        V7(I,2,1) = -(BI21*S11+BI22*S21+BI23*S31)
        V7(I,3,1) = -(BI31*S11+BI32*S21+BI33*S31)
        V7(I,1,2) = -(BI11*S12+BI12*S22+BI13*S32)
        V7(I,2,2) = -(BI21*S12+BI22*S22+BI23*S32)
        V7(I,3,2) = -(BI31*S12+BI32*S22+BI33*S32)
        V7(I,1,3) = -(BI11*S13+BI12*S23+BI13*S33)
        V7(I,2,3) = -(BI21*S13+BI22*S23+BI23*S33)
        V7(I,3,3) = -(BI31*S13+BI32*S23+BI33*S33)

        S11 = -DN8_DZ(I)*R(1,2,I)+DN8_DY(I)*R(1,3,I)
        S21 =  DN8_DZ(I)*R(1,1,I)-DN8_DX(I)*R(1,3,I)
        S31 = -DN8_DY(I)*R(1,1,I)+DN8_DX(I)*R(1,2,I)
        S12 = -DN8_DZ(I)*R(2,2,I)+DN8_DY(I)*R(2,3,I)
        S22 =  DN8_DZ(I)*R(2,1,I)-DN8_DX(I)*R(2,3,I)
        S32 = -DN8_DY(I)*R(2,1,I)+DN8_DX(I)*R(2,2,I)
        S13 = -DN8_DZ(I)*R(3,2,I)+DN8_DY(I)*R(3,3,I)
        S23 =  DN8_DZ(I)*R(3,1,I)-DN8_DX(I)*R(3,3,I)
        S33 = -DN8_DY(I)*R(3,1,I)+DN8_DX(I)*R(3,2,I)

        V8(I,1,1) = -(BI11*S11+BI12*S21+BI13*S31)
        V8(I,2,1) = -(BI21*S11+BI22*S21+BI23*S31)
        V8(I,3,1) = -(BI31*S11+BI32*S21+BI33*S31)
        V8(I,1,2) = -(BI11*S12+BI12*S22+BI13*S32)
        V8(I,2,2) = -(BI21*S12+BI22*S22+BI23*S32)
        V8(I,3,2) = -(BI31*S12+BI32*S22+BI33*S32)
        V8(I,1,3) = -(BI11*S13+BI12*S23+BI13*S33)
        V8(I,2,3) = -(BI21*S13+BI22*S23+BI23*S33)
        V8(I,3,3) = -(BI31*S13+BI32*S23+BI33*S33)
        
        TRM(I,1,1) = ZERO
        TRM(I,2,1) = ZERO
        TRM(I,3,1) = ZERO
        TRM(I,1,2) = ZERO
        TRM(I,2,2) = ZERO
        TRM(I,3,2) = ZERO
        TRM(I,1,3) = ZERO
        TRM(I,2,3) = ZERO
        TRM(I,3,3) = ZERO

        TRM(I,4,1) = SA11*V1(I,1,1)+SA12*V1(I,2,1)+SA13*V1(I,3,1)
        TRM(I,5,1) = SA21*V1(I,1,1)+SA22*V1(I,2,1)+SA23*V1(I,3,1)
        TRM(I,6,1) = SA31*V1(I,1,1)+SA32*V1(I,2,1)+SA33*V1(I,3,1)
        TRM(I,4,2) = SA11*V1(I,1,2)+SA12*V1(I,2,2)+SA13*V1(I,3,2)
        TRM(I,5,2) = SA21*V1(I,1,2)+SA22*V1(I,2,2)+SA23*V1(I,3,2)
        TRM(I,6,2) = SA31*V1(I,1,2)+SA32*V1(I,2,2)+SA33*V1(I,3,2)
        TRM(I,4,3) = SA11*V1(I,1,3)+SA12*V1(I,2,3)+SA13*V1(I,3,3)
        TRM(I,5,3) = SA21*V1(I,1,3)+SA22*V1(I,2,3)+SA23*V1(I,3,3)
        TRM(I,6,3) = SA31*V1(I,1,3)+SA32*V1(I,2,3)+SA33*V1(I,3,3)
        
        TRM(I,7,1) = SB11*V1(I,1,1)+SB12*V1(I,2,1)+SB13*V1(I,3,1)
        TRM(I,8,1) = SB21*V1(I,1,1)+SB22*V1(I,2,1)+SB23*V1(I,3,1)
        TRM(I,9,1) = SB31*V1(I,1,1)+SB32*V1(I,2,1)+SB33*V1(I,3,1)
        TRM(I,7,2) = SB11*V1(I,1,2)+SB12*V1(I,2,2)+SB13*V1(I,3,2)
        TRM(I,8,2) = SB21*V1(I,1,2)+SB22*V1(I,2,2)+SB23*V1(I,3,2)
        TRM(I,9,2) = SB31*V1(I,1,2)+SB32*V1(I,2,2)+SB33*V1(I,3,2)
        TRM(I,7,3) = SB11*V1(I,1,3)+SB12*V1(I,2,3)+SB13*V1(I,3,3)
        TRM(I,8,3) = SB21*V1(I,1,3)+SB22*V1(I,2,3)+SB23*V1(I,3,3)
        TRM(I,9,3) = SB31*V1(I,1,3)+SB32*V1(I,2,3)+SB33*V1(I,3,3)
        
        TRM(I,10,1) = SC11*V1(I,1,1)+SC12*V1(I,2,1)+SC13*V1(I,3,1)
        TRM(I,11,1) = SC21*V1(I,1,1)+SC22*V1(I,2,1)+SC23*V1(I,3,1)
        TRM(I,12,1) = SC31*V1(I,1,1)+SC32*V1(I,2,1)+SC33*V1(I,3,1)
        TRM(I,10,2) = SC11*V1(I,1,2)+SC12*V1(I,2,2)+SC13*V1(I,3,2)
        TRM(I,11,2) = SC21*V1(I,1,2)+SC22*V1(I,2,2)+SC23*V1(I,3,2)
        TRM(I,12,2) = SC31*V1(I,1,2)+SC32*V1(I,2,2)+SC33*V1(I,3,2)
        TRM(I,10,3) = SC11*V1(I,1,3)+SC12*V1(I,2,3)+SC13*V1(I,3,3)
        TRM(I,11,3) = SC21*V1(I,1,3)+SC22*V1(I,2,3)+SC23*V1(I,3,3)
        TRM(I,12,3) = SC31*V1(I,1,3)+SC32*V1(I,2,3)+SC33*V1(I,3,3)
        
        TRM(I,13,1) = SD11*V1(I,1,1)+SD12*V1(I,2,1)+SD13*V1(I,3,1)
        TRM(I,14,1) = SD21*V1(I,1,1)+SD22*V1(I,2,1)+SD23*V1(I,3,1)
        TRM(I,15,1) = SD31*V1(I,1,1)+SD32*V1(I,2,1)+SD33*V1(I,3,1)
        TRM(I,13,2) = SD11*V1(I,1,2)+SD12*V1(I,2,2)+SD13*V1(I,3,2)
        TRM(I,14,2) = SD21*V1(I,1,2)+SD22*V1(I,2,2)+SD23*V1(I,3,2)
        TRM(I,15,2) = SD31*V1(I,1,2)+SD32*V1(I,2,2)+SD33*V1(I,3,2)
        TRM(I,13,3) = SD11*V1(I,1,3)+SD12*V1(I,2,3)+SD13*V1(I,3,3)
        TRM(I,14,3) = SD21*V1(I,1,3)+SD22*V1(I,2,3)+SD23*V1(I,3,3)
        TRM(I,15,3) = SD31*V1(I,1,3)+SD32*V1(I,2,3)+SD33*V1(I,3,3)
        
        TRM(I,16,1) = SE11*V1(I,1,1)+SE12*V1(I,2,1)+SE13*V1(I,3,1)
        TRM(I,17,1) = SE21*V1(I,1,1)+SE22*V1(I,2,1)+SE23*V1(I,3,1)
        TRM(I,18,1) = SE31*V1(I,1,1)+SE32*V1(I,2,1)+SE33*V1(I,3,1)
        TRM(I,16,2) = SE11*V1(I,1,2)+SE12*V1(I,2,2)+SE13*V1(I,3,2)
        TRM(I,17,2) = SE21*V1(I,1,2)+SE22*V1(I,2,2)+SE23*V1(I,3,2)
        TRM(I,18,2) = SE31*V1(I,1,2)+SE32*V1(I,2,2)+SE33*V1(I,3,2)
        TRM(I,16,3) = SE11*V1(I,1,3)+SE12*V1(I,2,3)+SE13*V1(I,3,3)
        TRM(I,17,3) = SE21*V1(I,1,3)+SE22*V1(I,2,3)+SE23*V1(I,3,3)
        TRM(I,18,3) = SE31*V1(I,1,3)+SE32*V1(I,2,3)+SE33*V1(I,3,3)
        
        TRM(I,19,1) = SF11*V1(I,1,1)+SF12*V1(I,2,1)+SF13*V1(I,3,1)
        TRM(I,20,1) = SF21*V1(I,1,1)+SF22*V1(I,2,1)+SF23*V1(I,3,1)
        TRM(I,21,1) = SF31*V1(I,1,1)+SF32*V1(I,2,1)+SF33*V1(I,3,1)
        TRM(I,19,2) = SF11*V1(I,1,2)+SF12*V1(I,2,2)+SF13*V1(I,3,2)
        TRM(I,20,2) = SF21*V1(I,1,2)+SF22*V1(I,2,2)+SF23*V1(I,3,2)
        TRM(I,21,2) = SF31*V1(I,1,2)+SF32*V1(I,2,2)+SF33*V1(I,3,2)
        TRM(I,19,3) = SF11*V1(I,1,3)+SF12*V1(I,2,3)+SF13*V1(I,3,3)
        TRM(I,20,3) = SF21*V1(I,1,3)+SF22*V1(I,2,3)+SF23*V1(I,3,3)
        TRM(I,21,3) = SF31*V1(I,1,3)+SF32*V1(I,2,3)+SF33*V1(I,3,3)
        
        TRM(I,22,1) = SG11*V1(I,1,1)+SG12*V1(I,2,1)+SG13*V1(I,3,1)
        TRM(I,23,1) = SG21*V1(I,1,1)+SG22*V1(I,2,1)+SG23*V1(I,3,1)
        TRM(I,24,1) = SG31*V1(I,1,1)+SG32*V1(I,2,1)+SG33*V1(I,3,1)
        TRM(I,22,2) = SG11*V1(I,1,2)+SG12*V1(I,2,2)+SG13*V1(I,3,2)
        TRM(I,23,2) = SG21*V1(I,1,2)+SG22*V1(I,2,2)+SG23*V1(I,3,2)
        TRM(I,24,2) = SG31*V1(I,1,2)+SG32*V1(I,2,2)+SG33*V1(I,3,2)
        TRM(I,22,3) = SG11*V1(I,1,3)+SG12*V1(I,2,3)+SG13*V1(I,3,3)
        TRM(I,23,3) = SG21*V1(I,1,3)+SG22*V1(I,2,3)+SG23*V1(I,3,3)
        TRM(I,24,3) = SG31*V1(I,1,3)+SG32*V1(I,2,3)+SG33*V1(I,3,3)
        
        TRM(I,1,4) = ZERO
        TRM(I,2,4) = ZERO
        TRM(I,3,4) = ZERO
        TRM(I,1,5) = ZERO
        TRM(I,2,5) = ZERO
        TRM(I,3,5) = ZERO
        TRM(I,1,6) = ZERO
        TRM(I,2,6) = ZERO
        TRM(I,3,6) = ZERO

        TRM(I,4,4) = SA11*V2(I,1,1)+SA12*V2(I,2,1)+SA13*V2(I,3,1)
        TRM(I,5,4) = SA21*V2(I,1,1)+SA22*V2(I,2,1)+SA23*V2(I,3,1)
        TRM(I,6,4) = SA31*V2(I,1,1)+SA32*V2(I,2,1)+SA33*V2(I,3,1)
        TRM(I,4,5) = SA11*V2(I,1,2)+SA12*V2(I,2,2)+SA13*V2(I,3,2)
        TRM(I,5,5) = SA21*V2(I,1,2)+SA22*V2(I,2,2)+SA23*V2(I,3,2)
        TRM(I,6,5) = SA31*V2(I,1,2)+SA32*V2(I,2,2)+SA33*V2(I,3,2)
        TRM(I,4,6) = SA11*V2(I,1,3)+SA12*V2(I,2,3)+SA13*V2(I,3,3)
        TRM(I,5,6) = SA21*V2(I,1,3)+SA22*V2(I,2,3)+SA23*V2(I,3,3)
        TRM(I,6,6) = SA31*V2(I,1,3)+SA32*V2(I,2,3)+SA33*V2(I,3,3)
        
        TRM(I,7,4) = SB11*V2(I,1,1)+SB12*V2(I,2,1)+SB13*V2(I,3,1)
        TRM(I,8,4) = SB21*V2(I,1,1)+SB22*V2(I,2,1)+SB23*V2(I,3,1)
        TRM(I,9,4) = SB31*V2(I,1,1)+SB32*V2(I,2,1)+SB33*V2(I,3,1)
        TRM(I,7,5) = SB11*V2(I,1,2)+SB12*V2(I,2,2)+SB13*V2(I,3,2)
        TRM(I,8,5) = SB21*V2(I,1,2)+SB22*V2(I,2,2)+SB23*V2(I,3,2)
        TRM(I,9,5) = SB31*V2(I,1,2)+SB32*V2(I,2,2)+SB33*V2(I,3,2)
        TRM(I,7,6) = SB11*V2(I,1,3)+SB12*V2(I,2,3)+SB13*V2(I,3,3)
        TRM(I,8,6) = SB21*V2(I,1,3)+SB22*V2(I,2,3)+SB23*V2(I,3,3)
        TRM(I,9,6) = SB31*V2(I,1,3)+SB32*V2(I,2,3)+SB33*V2(I,3,3)
        
        TRM(I,10,4) = SC11*V2(I,1,1)+SC12*V2(I,2,1)+SC13*V2(I,3,1)
        TRM(I,11,4) = SC21*V2(I,1,1)+SC22*V2(I,2,1)+SC23*V2(I,3,1)
        TRM(I,12,4) = SC31*V2(I,1,1)+SC32*V2(I,2,1)+SC33*V2(I,3,1)
        TRM(I,10,5) = SC11*V2(I,1,2)+SC12*V2(I,2,2)+SC13*V2(I,3,2)
        TRM(I,11,5) = SC21*V2(I,1,2)+SC22*V2(I,2,2)+SC23*V2(I,3,2)
        TRM(I,12,5) = SC31*V2(I,1,2)+SC32*V2(I,2,2)+SC33*V2(I,3,2)
        TRM(I,10,6) = SC11*V2(I,1,3)+SC12*V2(I,2,3)+SC13*V2(I,3,3)
        TRM(I,11,6) = SC21*V2(I,1,3)+SC22*V2(I,2,3)+SC23*V2(I,3,3)
        TRM(I,12,6) = SC31*V2(I,1,3)+SC32*V2(I,2,3)+SC33*V2(I,3,3)
        
        TRM(I,13,4) = SD11*V2(I,1,1)+SD12*V2(I,2,1)+SD13*V2(I,3,1)
        TRM(I,14,4) = SD21*V2(I,1,1)+SD22*V2(I,2,1)+SD23*V2(I,3,1)
        TRM(I,15,4) = SD31*V2(I,1,1)+SD32*V2(I,2,1)+SD33*V2(I,3,1)
        TRM(I,13,5) = SD11*V2(I,1,2)+SD12*V2(I,2,2)+SD13*V2(I,3,2)
        TRM(I,14,5) = SD21*V2(I,1,2)+SD22*V2(I,2,2)+SD23*V2(I,3,2)
        TRM(I,15,5) = SD31*V2(I,1,2)+SD32*V2(I,2,2)+SD33*V2(I,3,2)
        TRM(I,13,6) = SD11*V2(I,1,3)+SD12*V2(I,2,3)+SD13*V2(I,3,3)
        TRM(I,14,6) = SD21*V2(I,1,3)+SD22*V2(I,2,3)+SD23*V2(I,3,3)
        TRM(I,15,6) = SD31*V2(I,1,3)+SD32*V2(I,2,3)+SD33*V2(I,3,3)
        
        TRM(I,16,4) = SE11*V2(I,1,1)+SE12*V2(I,2,1)+SE13*V2(I,3,1)
        TRM(I,17,4) = SE21*V2(I,1,1)+SE22*V2(I,2,1)+SE23*V2(I,3,1)
        TRM(I,18,4) = SE31*V2(I,1,1)+SE32*V2(I,2,1)+SE33*V2(I,3,1)
        TRM(I,16,5) = SE11*V2(I,1,2)+SE12*V2(I,2,2)+SE13*V2(I,3,2)
        TRM(I,17,5) = SE21*V2(I,1,2)+SE22*V2(I,2,2)+SE23*V2(I,3,2)
        TRM(I,18,5) = SE31*V2(I,1,2)+SE32*V2(I,2,2)+SE33*V2(I,3,2)
        TRM(I,16,6) = SE11*V2(I,1,3)+SE12*V2(I,2,3)+SE13*V2(I,3,3)
        TRM(I,17,6) = SE21*V2(I,1,3)+SE22*V2(I,2,3)+SE23*V2(I,3,3)
        TRM(I,18,6) = SE31*V2(I,1,3)+SE32*V2(I,2,3)+SE33*V2(I,3,3)
        
        TRM(I,19,4) = SF11*V2(I,1,1)+SF12*V2(I,2,1)+SF13*V2(I,3,1)
        TRM(I,20,4) = SF21*V2(I,1,1)+SF22*V2(I,2,1)+SF23*V2(I,3,1)
        TRM(I,21,4) = SF31*V2(I,1,1)+SF32*V2(I,2,1)+SF33*V2(I,3,1)
        TRM(I,19,5) = SF11*V2(I,1,2)+SF12*V2(I,2,2)+SF13*V2(I,3,2)
        TRM(I,20,5) = SF21*V2(I,1,2)+SF22*V2(I,2,2)+SF23*V2(I,3,2)
        TRM(I,21,5) = SF31*V2(I,1,2)+SF32*V2(I,2,2)+SF33*V2(I,3,2)
        TRM(I,19,6) = SF11*V2(I,1,3)+SF12*V2(I,2,3)+SF13*V2(I,3,3)
        TRM(I,20,6) = SF21*V2(I,1,3)+SF22*V2(I,2,3)+SF23*V2(I,3,3)
        TRM(I,21,6) = SF31*V2(I,1,3)+SF32*V2(I,2,3)+SF33*V2(I,3,3)
        
        TRM(I,22,4) = SG11*V2(I,1,1)+SG12*V2(I,2,1)+SG13*V2(I,3,1)
        TRM(I,23,4) = SG21*V2(I,1,1)+SG22*V2(I,2,1)+SG23*V2(I,3,1)
        TRM(I,24,4) = SG31*V2(I,1,1)+SG32*V2(I,2,1)+SG33*V2(I,3,1)
        TRM(I,22,5) = SG11*V2(I,1,2)+SG12*V2(I,2,2)+SG13*V2(I,3,2)
        TRM(I,23,5) = SG21*V2(I,1,2)+SG22*V2(I,2,2)+SG23*V2(I,3,2)
        TRM(I,24,5) = SG31*V2(I,1,2)+SG32*V2(I,2,2)+SG33*V2(I,3,2)
        TRM(I,22,6) = SG11*V2(I,1,3)+SG12*V2(I,2,3)+SG13*V2(I,3,3)
        TRM(I,23,6) = SG21*V2(I,1,3)+SG22*V2(I,2,3)+SG23*V2(I,3,3)
        TRM(I,24,6) = SG31*V2(I,1,3)+SG32*V2(I,2,3)+SG33*V2(I,3,3)
        
        TRM(I,1,7) = ZERO
        TRM(I,2,7) = ZERO
        TRM(I,3,7) = ZERO
        TRM(I,1,8) = ZERO
        TRM(I,2,8) = ZERO
        TRM(I,3,8) = ZERO
        TRM(I,1,9) = ZERO
        TRM(I,2,9) = ZERO
        TRM(I,3,9) = ZERO

        TRM(I,4,7) = SA11*V3(I,1,1)+SA12*V3(I,2,1)+SA13*V3(I,3,1)
        TRM(I,5,7) = SA21*V3(I,1,1)+SA22*V3(I,2,1)+SA23*V3(I,3,1)
        TRM(I,6,7) = SA31*V3(I,1,1)+SA32*V3(I,2,1)+SA33*V3(I,3,1)
        TRM(I,4,8) = SA11*V3(I,1,2)+SA12*V3(I,2,2)+SA13*V3(I,3,2)
        TRM(I,5,8) = SA21*V3(I,1,2)+SA22*V3(I,2,2)+SA23*V3(I,3,2)
        TRM(I,6,8) = SA31*V3(I,1,2)+SA32*V3(I,2,2)+SA33*V3(I,3,2)
        TRM(I,4,9) = SA11*V3(I,1,3)+SA12*V3(I,2,3)+SA13*V3(I,3,3)
        TRM(I,5,9) = SA21*V3(I,1,3)+SA22*V3(I,2,3)+SA23*V3(I,3,3)
        TRM(I,6,9) = SA31*V3(I,1,3)+SA32*V3(I,2,3)+SA33*V3(I,3,3)
        
        TRM(I,7,7) = SB11*V3(I,1,1)+SB12*V3(I,2,1)+SB13*V3(I,3,1)
        TRM(I,8,7) = SB21*V3(I,1,1)+SB22*V3(I,2,1)+SB23*V3(I,3,1)
        TRM(I,9,7) = SB31*V3(I,1,1)+SB32*V3(I,2,1)+SB33*V3(I,3,1)
        TRM(I,7,8) = SB11*V3(I,1,2)+SB12*V3(I,2,2)+SB13*V3(I,3,2)
        TRM(I,8,8) = SB21*V3(I,1,2)+SB22*V3(I,2,2)+SB23*V3(I,3,2)
        TRM(I,9,8) = SB31*V3(I,1,2)+SB32*V3(I,2,2)+SB33*V3(I,3,2)
        TRM(I,7,9) = SB11*V3(I,1,3)+SB12*V3(I,2,3)+SB13*V3(I,3,3)
        TRM(I,8,9) = SB21*V3(I,1,3)+SB22*V3(I,2,3)+SB23*V3(I,3,3)
        TRM(I,9,9) = SB31*V3(I,1,3)+SB32*V3(I,2,3)+SB33*V3(I,3,3)
        
        TRM(I,10,7) = SC11*V3(I,1,1)+SC12*V3(I,2,1)+SC13*V3(I,3,1)
        TRM(I,11,7) = SC21*V3(I,1,1)+SC22*V3(I,2,1)+SC23*V3(I,3,1)
        TRM(I,12,7) = SC31*V3(I,1,1)+SC32*V3(I,2,1)+SC33*V3(I,3,1)
        TRM(I,10,8) = SC11*V3(I,1,2)+SC12*V3(I,2,2)+SC13*V3(I,3,2)
        TRM(I,11,8) = SC21*V3(I,1,2)+SC22*V3(I,2,2)+SC23*V3(I,3,2)
        TRM(I,12,8) = SC31*V3(I,1,2)+SC32*V3(I,2,2)+SC33*V3(I,3,2)
        TRM(I,10,9) = SC11*V3(I,1,3)+SC12*V3(I,2,3)+SC13*V3(I,3,3)
        TRM(I,11,9) = SC21*V3(I,1,3)+SC22*V3(I,2,3)+SC23*V3(I,3,3)
        TRM(I,12,9) = SC31*V3(I,1,3)+SC32*V3(I,2,3)+SC33*V3(I,3,3)
        
        TRM(I,13,7) = SD11*V3(I,1,1)+SD12*V3(I,2,1)+SD13*V3(I,3,1)
        TRM(I,14,7) = SD21*V3(I,1,1)+SD22*V3(I,2,1)+SD23*V3(I,3,1)
        TRM(I,15,7) = SD31*V3(I,1,1)+SD32*V3(I,2,1)+SD33*V3(I,3,1)
        TRM(I,13,8) = SD11*V3(I,1,2)+SD12*V3(I,2,2)+SD13*V3(I,3,2)
        TRM(I,14,8) = SD21*V3(I,1,2)+SD22*V3(I,2,2)+SD23*V3(I,3,2)
        TRM(I,15,8) = SD31*V3(I,1,2)+SD32*V3(I,2,2)+SD33*V3(I,3,2)
        TRM(I,13,9) = SD11*V3(I,1,3)+SD12*V3(I,2,3)+SD13*V3(I,3,3)
        TRM(I,14,9) = SD21*V3(I,1,3)+SD22*V3(I,2,3)+SD23*V3(I,3,3)
        TRM(I,15,9) = SD31*V3(I,1,3)+SD32*V3(I,2,3)+SD33*V3(I,3,3)
        
        TRM(I,16,7) = SE11*V3(I,1,1)+SE12*V3(I,2,1)+SE13*V3(I,3,1)
        TRM(I,17,7) = SE21*V3(I,1,1)+SE22*V3(I,2,1)+SE23*V3(I,3,1)
        TRM(I,18,7) = SE31*V3(I,1,1)+SE32*V3(I,2,1)+SE33*V3(I,3,1)
        TRM(I,16,8) = SE11*V3(I,1,2)+SE12*V3(I,2,2)+SE13*V3(I,3,2)
        TRM(I,17,8) = SE21*V3(I,1,2)+SE22*V3(I,2,2)+SE23*V3(I,3,2)
        TRM(I,18,8) = SE31*V3(I,1,2)+SE32*V3(I,2,2)+SE33*V3(I,3,2)
        TRM(I,16,9) = SE11*V3(I,1,3)+SE12*V3(I,2,3)+SE13*V3(I,3,3)
        TRM(I,17,9) = SE21*V3(I,1,3)+SE22*V3(I,2,3)+SE23*V3(I,3,3)
        TRM(I,18,9) = SE31*V3(I,1,3)+SE32*V3(I,2,3)+SE33*V3(I,3,3)
        
        TRM(I,19,7) = SF11*V3(I,1,1)+SF12*V3(I,2,1)+SF13*V3(I,3,1)
        TRM(I,20,7) = SF21*V3(I,1,1)+SF22*V3(I,2,1)+SF23*V3(I,3,1)
        TRM(I,21,7) = SF31*V3(I,1,1)+SF32*V3(I,2,1)+SF33*V3(I,3,1)
        TRM(I,19,8) = SF11*V3(I,1,2)+SF12*V3(I,2,2)+SF13*V3(I,3,2)
        TRM(I,20,8) = SF21*V3(I,1,2)+SF22*V3(I,2,2)+SF23*V3(I,3,2)
        TRM(I,21,8) = SF31*V3(I,1,2)+SF32*V3(I,2,2)+SF33*V3(I,3,2)
        TRM(I,19,9) = SF11*V3(I,1,3)+SF12*V3(I,2,3)+SF13*V3(I,3,3)
        TRM(I,20,9) = SF21*V3(I,1,3)+SF22*V3(I,2,3)+SF23*V3(I,3,3)
        TRM(I,21,9) = SF31*V3(I,1,3)+SF32*V3(I,2,3)+SF33*V3(I,3,3)
        
        TRM(I,22,7) = SG11*V3(I,1,1)+SG12*V3(I,2,1)+SG13*V3(I,3,1)
        TRM(I,23,7) = SG21*V3(I,1,1)+SG22*V3(I,2,1)+SG23*V3(I,3,1)
        TRM(I,24,7) = SG31*V3(I,1,1)+SG32*V3(I,2,1)+SG33*V3(I,3,1)
        TRM(I,22,8) = SG11*V3(I,1,2)+SG12*V3(I,2,2)+SG13*V3(I,3,2)
        TRM(I,23,8) = SG21*V3(I,1,2)+SG22*V3(I,2,2)+SG23*V3(I,3,2)
        TRM(I,24,8) = SG31*V3(I,1,2)+SG32*V3(I,2,2)+SG33*V3(I,3,2)
        TRM(I,22,9) = SG11*V3(I,1,3)+SG12*V3(I,2,3)+SG13*V3(I,3,3)
        TRM(I,23,9) = SG21*V3(I,1,3)+SG22*V3(I,2,3)+SG23*V3(I,3,3)
        TRM(I,24,9) = SG31*V3(I,1,3)+SG32*V3(I,2,3)+SG33*V3(I,3,3)
        
        TRM(I,1,10) = ZERO
        TRM(I,2,10) = ZERO
        TRM(I,3,10) = ZERO
        TRM(I,1,11) = ZERO
        TRM(I,2,11) = ZERO
        TRM(I,3,11) = ZERO
        TRM(I,1,12) = ZERO
        TRM(I,2,12) = ZERO
        TRM(I,3,12) = ZERO

        TRM(I,4,10) = SA11*V4(I,1,1)+SA12*V4(I,2,1)+SA13*V4(I,3,1)
        TRM(I,5,10) = SA21*V4(I,1,1)+SA22*V4(I,2,1)+SA23*V4(I,3,1)
        TRM(I,6,10) = SA31*V4(I,1,1)+SA32*V4(I,2,1)+SA33*V4(I,3,1)
        TRM(I,4,11) = SA11*V4(I,1,2)+SA12*V4(I,2,2)+SA13*V4(I,3,2)
        TRM(I,5,11) = SA21*V4(I,1,2)+SA22*V4(I,2,2)+SA23*V4(I,3,2)
        TRM(I,6,11) = SA31*V4(I,1,2)+SA32*V4(I,2,2)+SA33*V4(I,3,2)
        TRM(I,4,12) = SA11*V4(I,1,3)+SA12*V4(I,2,3)+SA13*V4(I,3,3)
        TRM(I,5,12) = SA21*V4(I,1,3)+SA22*V4(I,2,3)+SA23*V4(I,3,3)
        TRM(I,6,12) = SA31*V4(I,1,3)+SA32*V4(I,2,3)+SA33*V4(I,3,3)
        
        TRM(I,7,10) = SB11*V4(I,1,1)+SB12*V4(I,2,1)+SB13*V4(I,3,1)
        TRM(I,8,10) = SB21*V4(I,1,1)+SB22*V4(I,2,1)+SB23*V4(I,3,1)
        TRM(I,9,10) = SB31*V4(I,1,1)+SB32*V4(I,2,1)+SB33*V4(I,3,1)
        TRM(I,7,11) = SB11*V4(I,1,2)+SB12*V4(I,2,2)+SB13*V4(I,3,2)
        TRM(I,8,11) = SB21*V4(I,1,2)+SB22*V4(I,2,2)+SB23*V4(I,3,2)
        TRM(I,9,11) = SB31*V4(I,1,2)+SB32*V4(I,2,2)+SB33*V4(I,3,2)
        TRM(I,7,12) = SB11*V4(I,1,3)+SB12*V4(I,2,3)+SB13*V4(I,3,3)
        TRM(I,8,12) = SB21*V4(I,1,3)+SB22*V4(I,2,3)+SB23*V4(I,3,3)
        TRM(I,9,12) = SB31*V4(I,1,3)+SB32*V4(I,2,3)+SB33*V4(I,3,3)
        
        TRM(I,10,10) = SC11*V4(I,1,1)+SC12*V4(I,2,1)+SC13*V4(I,3,1)
        TRM(I,11,10) = SC21*V4(I,1,1)+SC22*V4(I,2,1)+SC23*V4(I,3,1)
        TRM(I,12,10) = SC31*V4(I,1,1)+SC32*V4(I,2,1)+SC33*V4(I,3,1)
        TRM(I,10,11) = SC11*V4(I,1,2)+SC12*V4(I,2,2)+SC13*V4(I,3,2)
        TRM(I,11,11) = SC21*V4(I,1,2)+SC22*V4(I,2,2)+SC23*V4(I,3,2)
        TRM(I,12,11) = SC31*V4(I,1,2)+SC32*V4(I,2,2)+SC33*V4(I,3,2)
        TRM(I,10,12) = SC11*V4(I,1,3)+SC12*V4(I,2,3)+SC13*V4(I,3,3)
        TRM(I,11,12) = SC21*V4(I,1,3)+SC22*V4(I,2,3)+SC23*V4(I,3,3)
        TRM(I,12,12) = SC31*V4(I,1,3)+SC32*V4(I,2,3)+SC33*V4(I,3,3)
        
        TRM(I,13,10) = SD11*V4(I,1,1)+SD12*V4(I,2,1)+SD13*V4(I,3,1)
        TRM(I,14,10) = SD21*V4(I,1,1)+SD22*V4(I,2,1)+SD23*V4(I,3,1)
        TRM(I,15,10) = SD31*V4(I,1,1)+SD32*V4(I,2,1)+SD33*V4(I,3,1)
        TRM(I,13,11) = SD11*V4(I,1,2)+SD12*V4(I,2,2)+SD13*V4(I,3,2)
        TRM(I,14,11) = SD21*V4(I,1,2)+SD22*V4(I,2,2)+SD23*V4(I,3,2)
        TRM(I,15,11) = SD31*V4(I,1,2)+SD32*V4(I,2,2)+SD33*V4(I,3,2)
        TRM(I,13,12) = SD11*V4(I,1,3)+SD12*V4(I,2,3)+SD13*V4(I,3,3)
        TRM(I,14,12) = SD21*V4(I,1,3)+SD22*V4(I,2,3)+SD23*V4(I,3,3)
        TRM(I,15,12) = SD31*V4(I,1,3)+SD32*V4(I,2,3)+SD33*V4(I,3,3)
        
        TRM(I,16,10) = SE11*V4(I,1,1)+SE12*V4(I,2,1)+SE13*V4(I,3,1)
        TRM(I,17,10) = SE21*V4(I,1,1)+SE22*V4(I,2,1)+SE23*V4(I,3,1)
        TRM(I,18,10) = SE31*V4(I,1,1)+SE32*V4(I,2,1)+SE33*V4(I,3,1)
        TRM(I,16,11) = SE11*V4(I,1,2)+SE12*V4(I,2,2)+SE13*V4(I,3,2)
        TRM(I,17,11) = SE21*V4(I,1,2)+SE22*V4(I,2,2)+SE23*V4(I,3,2)
        TRM(I,18,11) = SE31*V4(I,1,2)+SE32*V4(I,2,2)+SE33*V4(I,3,2)
        TRM(I,16,12) = SE11*V4(I,1,3)+SE12*V4(I,2,3)+SE13*V4(I,3,3)
        TRM(I,17,12) = SE21*V4(I,1,3)+SE22*V4(I,2,3)+SE23*V4(I,3,3)
        TRM(I,18,12) = SE31*V4(I,1,3)+SE32*V4(I,2,3)+SE33*V4(I,3,3)
        
        TRM(I,19,10) = SF11*V4(I,1,1)+SF12*V4(I,2,1)+SF13*V4(I,3,1)
        TRM(I,20,10) = SF21*V4(I,1,1)+SF22*V4(I,2,1)+SF23*V4(I,3,1)
        TRM(I,21,10) = SF31*V4(I,1,1)+SF32*V4(I,2,1)+SF33*V4(I,3,1)
        TRM(I,19,11) = SF11*V4(I,1,2)+SF12*V4(I,2,2)+SF13*V4(I,3,2)
        TRM(I,20,11) = SF21*V4(I,1,2)+SF22*V4(I,2,2)+SF23*V4(I,3,2)
        TRM(I,21,11) = SF31*V4(I,1,2)+SF32*V4(I,2,2)+SF33*V4(I,3,2)
        TRM(I,19,12) = SF11*V4(I,1,3)+SF12*V4(I,2,3)+SF13*V4(I,3,3)
        TRM(I,20,12) = SF21*V4(I,1,3)+SF22*V4(I,2,3)+SF23*V4(I,3,3)
        TRM(I,21,12) = SF31*V4(I,1,3)+SF32*V4(I,2,3)+SF33*V4(I,3,3)
        
        TRM(I,22,10) = SG11*V4(I,1,1)+SG12*V4(I,2,1)+SG13*V4(I,3,1)
        TRM(I,23,10) = SG21*V4(I,1,1)+SG22*V4(I,2,1)+SG23*V4(I,3,1)
        TRM(I,24,10) = SG31*V4(I,1,1)+SG32*V4(I,2,1)+SG33*V4(I,3,1)
        TRM(I,22,11) = SG11*V4(I,1,2)+SG12*V4(I,2,2)+SG13*V4(I,3,2)
        TRM(I,23,11) = SG21*V4(I,1,2)+SG22*V4(I,2,2)+SG23*V4(I,3,2)
        TRM(I,24,11) = SG31*V4(I,1,2)+SG32*V4(I,2,2)+SG33*V4(I,3,2)
        TRM(I,22,12) = SG11*V4(I,1,3)+SG12*V4(I,2,3)+SG13*V4(I,3,3)
        TRM(I,23,12) = SG21*V4(I,1,3)+SG22*V4(I,2,3)+SG23*V4(I,3,3)
        TRM(I,24,12) = SG31*V4(I,1,3)+SG32*V4(I,2,3)+SG33*V4(I,3,3)
        
        TRM(I,1,13) = ZERO
        TRM(I,2,13) = ZERO
        TRM(I,3,13) = ZERO
        TRM(I,1,14) = ZERO
        TRM(I,2,14) = ZERO
        TRM(I,3,14) = ZERO
        TRM(I,1,15) = ZERO
        TRM(I,2,15) = ZERO
        TRM(I,3,15) = ZERO

        TRM(I,4,13) = SA11*V5(I,1,1)+SA12*V5(I,2,1)+SA13*V5(I,3,1)
        TRM(I,5,13) = SA21*V5(I,1,1)+SA22*V5(I,2,1)+SA23*V5(I,3,1)
        TRM(I,6,13) = SA31*V5(I,1,1)+SA32*V5(I,2,1)+SA33*V5(I,3,1)
        TRM(I,4,14) = SA11*V5(I,1,2)+SA12*V5(I,2,2)+SA13*V5(I,3,2)
        TRM(I,5,14) = SA21*V5(I,1,2)+SA22*V5(I,2,2)+SA23*V5(I,3,2)
        TRM(I,6,14) = SA31*V5(I,1,2)+SA32*V5(I,2,2)+SA33*V5(I,3,2)
        TRM(I,4,15) = SA11*V5(I,1,3)+SA12*V5(I,2,3)+SA13*V5(I,3,3)
        TRM(I,5,15) = SA21*V5(I,1,3)+SA22*V5(I,2,3)+SA23*V5(I,3,3)
        TRM(I,6,15) = SA31*V5(I,1,3)+SA32*V5(I,2,3)+SA33*V5(I,3,3)
        
        TRM(I,7,13) = SB11*V5(I,1,1)+SB12*V5(I,2,1)+SB13*V5(I,3,1)
        TRM(I,8,13) = SB21*V5(I,1,1)+SB22*V5(I,2,1)+SB23*V5(I,3,1)
        TRM(I,9,13) = SB31*V5(I,1,1)+SB32*V5(I,2,1)+SB33*V5(I,3,1)
        TRM(I,7,14) = SB11*V5(I,1,2)+SB12*V5(I,2,2)+SB13*V5(I,3,2)
        TRM(I,8,14) = SB21*V5(I,1,2)+SB22*V5(I,2,2)+SB23*V5(I,3,2)
        TRM(I,9,14) = SB31*V5(I,1,2)+SB32*V5(I,2,2)+SB33*V5(I,3,2)
        TRM(I,7,15) = SB11*V5(I,1,3)+SB12*V5(I,2,3)+SB13*V5(I,3,3)
        TRM(I,8,15) = SB21*V5(I,1,3)+SB22*V5(I,2,3)+SB23*V5(I,3,3)
        TRM(I,9,15) = SB31*V5(I,1,3)+SB32*V5(I,2,3)+SB33*V5(I,3,3)
        
        TRM(I,10,13) = SC11*V5(I,1,1)+SC12*V5(I,2,1)+SC13*V5(I,3,1)
        TRM(I,11,13) = SC21*V5(I,1,1)+SC22*V5(I,2,1)+SC23*V5(I,3,1)
        TRM(I,12,13) = SC31*V5(I,1,1)+SC32*V5(I,2,1)+SC33*V5(I,3,1)
        TRM(I,10,14) = SC11*V5(I,1,2)+SC12*V5(I,2,2)+SC13*V5(I,3,2)
        TRM(I,11,14) = SC21*V5(I,1,2)+SC22*V5(I,2,2)+SC23*V5(I,3,2)
        TRM(I,12,14) = SC31*V5(I,1,2)+SC32*V5(I,2,2)+SC33*V5(I,3,2)
        TRM(I,10,15) = SC11*V5(I,1,3)+SC12*V5(I,2,3)+SC13*V5(I,3,3)
        TRM(I,11,15) = SC21*V5(I,1,3)+SC22*V5(I,2,3)+SC23*V5(I,3,3)
        TRM(I,12,15) = SC31*V5(I,1,3)+SC32*V5(I,2,3)+SC33*V5(I,3,3)
        
        TRM(I,13,13) = SD11*V5(I,1,1)+SD12*V5(I,2,1)+SD13*V5(I,3,1)
        TRM(I,14,13) = SD21*V5(I,1,1)+SD22*V5(I,2,1)+SD23*V5(I,3,1)
        TRM(I,15,13) = SD31*V5(I,1,1)+SD32*V5(I,2,1)+SD33*V5(I,3,1)
        TRM(I,13,14) = SD11*V5(I,1,2)+SD12*V5(I,2,2)+SD13*V5(I,3,2)
        TRM(I,14,14) = SD21*V5(I,1,2)+SD22*V5(I,2,2)+SD23*V5(I,3,2)
        TRM(I,15,14) = SD31*V5(I,1,2)+SD32*V5(I,2,2)+SD33*V5(I,3,2)
        TRM(I,13,15) = SD11*V5(I,1,3)+SD12*V5(I,2,3)+SD13*V5(I,3,3)
        TRM(I,14,15) = SD21*V5(I,1,3)+SD22*V5(I,2,3)+SD23*V5(I,3,3)
        TRM(I,15,15) = SD31*V5(I,1,3)+SD32*V5(I,2,3)+SD33*V5(I,3,3)
        
        TRM(I,16,13) = SE11*V5(I,1,1)+SE12*V5(I,2,1)+SE13*V5(I,3,1)
        TRM(I,17,13) = SE21*V5(I,1,1)+SE22*V5(I,2,1)+SE23*V5(I,3,1)
        TRM(I,18,13) = SE31*V5(I,1,1)+SE32*V5(I,2,1)+SE33*V5(I,3,1)
        TRM(I,16,14) = SE11*V5(I,1,2)+SE12*V5(I,2,2)+SE13*V5(I,3,2)
        TRM(I,17,14) = SE21*V5(I,1,2)+SE22*V5(I,2,2)+SE23*V5(I,3,2)
        TRM(I,18,14) = SE31*V5(I,1,2)+SE32*V5(I,2,2)+SE33*V5(I,3,2)
        TRM(I,16,15) = SE11*V5(I,1,3)+SE12*V5(I,2,3)+SE13*V5(I,3,3)
        TRM(I,17,15) = SE21*V5(I,1,3)+SE22*V5(I,2,3)+SE23*V5(I,3,3)
        TRM(I,18,15) = SE31*V5(I,1,3)+SE32*V5(I,2,3)+SE33*V5(I,3,3)
        
        TRM(I,19,13) = SF11*V5(I,1,1)+SF12*V5(I,2,1)+SF13*V5(I,3,1)
        TRM(I,20,13) = SF21*V5(I,1,1)+SF22*V5(I,2,1)+SF23*V5(I,3,1)
        TRM(I,21,13) = SF31*V5(I,1,1)+SF32*V5(I,2,1)+SF33*V5(I,3,1)
        TRM(I,19,14) = SF11*V5(I,1,2)+SF12*V5(I,2,2)+SF13*V5(I,3,2)
        TRM(I,20,14) = SF21*V5(I,1,2)+SF22*V5(I,2,2)+SF23*V5(I,3,2)
        TRM(I,21,14) = SF31*V5(I,1,2)+SF32*V5(I,2,2)+SF33*V5(I,3,2)
        TRM(I,19,15) = SF11*V5(I,1,3)+SF12*V5(I,2,3)+SF13*V5(I,3,3)
        TRM(I,20,15) = SF21*V5(I,1,3)+SF22*V5(I,2,3)+SF23*V5(I,3,3)
        TRM(I,21,15) = SF31*V5(I,1,3)+SF32*V5(I,2,3)+SF33*V5(I,3,3)
        
        TRM(I,22,13) = SG11*V5(I,1,1)+SG12*V5(I,2,1)+SG13*V5(I,3,1)
        TRM(I,23,13) = SG21*V5(I,1,1)+SG22*V5(I,2,1)+SG23*V5(I,3,1)
        TRM(I,24,13) = SG31*V5(I,1,1)+SG32*V5(I,2,1)+SG33*V5(I,3,1)
        TRM(I,22,14) = SG11*V5(I,1,2)+SG12*V5(I,2,2)+SG13*V5(I,3,2)
        TRM(I,23,14) = SG21*V5(I,1,2)+SG22*V5(I,2,2)+SG23*V5(I,3,2)
        TRM(I,24,14) = SG31*V5(I,1,2)+SG32*V5(I,2,2)+SG33*V5(I,3,2)
        TRM(I,22,15) = SG11*V5(I,1,3)+SG12*V5(I,2,3)+SG13*V5(I,3,3)
        TRM(I,23,15) = SG21*V5(I,1,3)+SG22*V5(I,2,3)+SG23*V5(I,3,3)
        TRM(I,24,15) = SG31*V5(I,1,3)+SG32*V5(I,2,3)+SG33*V5(I,3,3)
        
        TRM(I,1,16) = ZERO
        TRM(I,2,16) = ZERO
        TRM(I,3,16) = ZERO
        TRM(I,1,17) = ZERO
        TRM(I,2,17) = ZERO
        TRM(I,3,17) = ZERO
        TRM(I,1,18) = ZERO
        TRM(I,2,18) = ZERO
        TRM(I,3,18) = ZERO

        TRM(I,4,16) = SA11*V6(I,1,1)+SA12*V6(I,2,1)+SA13*V6(I,3,1)
        TRM(I,5,16) = SA21*V6(I,1,1)+SA22*V6(I,2,1)+SA23*V6(I,3,1)
        TRM(I,6,16) = SA31*V6(I,1,1)+SA32*V6(I,2,1)+SA33*V6(I,3,1)
        TRM(I,4,17) = SA11*V6(I,1,2)+SA12*V6(I,2,2)+SA13*V6(I,3,2)
        TRM(I,5,17) = SA21*V6(I,1,2)+SA22*V6(I,2,2)+SA23*V6(I,3,2)
        TRM(I,6,17) = SA31*V6(I,1,2)+SA32*V6(I,2,2)+SA33*V6(I,3,2)
        TRM(I,4,18) = SA11*V6(I,1,3)+SA12*V6(I,2,3)+SA13*V6(I,3,3)
        TRM(I,5,18) = SA21*V6(I,1,3)+SA22*V6(I,2,3)+SA23*V6(I,3,3)
        TRM(I,6,18) = SA31*V6(I,1,3)+SA32*V6(I,2,3)+SA33*V6(I,3,3)
        
        TRM(I,7,16) = SB11*V6(I,1,1)+SB12*V6(I,2,1)+SB13*V6(I,3,1)
        TRM(I,8,16) = SB21*V6(I,1,1)+SB22*V6(I,2,1)+SB23*V6(I,3,1)
        TRM(I,9,16) = SB31*V6(I,1,1)+SB32*V6(I,2,1)+SB33*V6(I,3,1)
        TRM(I,7,17) = SB11*V6(I,1,2)+SB12*V6(I,2,2)+SB13*V6(I,3,2)
        TRM(I,8,17) = SB21*V6(I,1,2)+SB22*V6(I,2,2)+SB23*V6(I,3,2)
        TRM(I,9,17) = SB31*V6(I,1,2)+SB32*V6(I,2,2)+SB33*V6(I,3,2)
        TRM(I,7,18) = SB11*V6(I,1,3)+SB12*V6(I,2,3)+SB13*V6(I,3,3)
        TRM(I,8,18) = SB21*V6(I,1,3)+SB22*V6(I,2,3)+SB23*V6(I,3,3)
        TRM(I,9,18) = SB31*V6(I,1,3)+SB32*V6(I,2,3)+SB33*V6(I,3,3)
        
        TRM(I,10,16) = SC11*V6(I,1,1)+SC12*V6(I,2,1)+SC13*V6(I,3,1)
        TRM(I,11,16) = SC21*V6(I,1,1)+SC22*V6(I,2,1)+SC23*V6(I,3,1)
        TRM(I,12,16) = SC31*V6(I,1,1)+SC32*V6(I,2,1)+SC33*V6(I,3,1)
        TRM(I,10,17) = SC11*V6(I,1,2)+SC12*V6(I,2,2)+SC13*V6(I,3,2)
        TRM(I,11,17) = SC21*V6(I,1,2)+SC22*V6(I,2,2)+SC23*V6(I,3,2)
        TRM(I,12,17) = SC31*V6(I,1,2)+SC32*V6(I,2,2)+SC33*V6(I,3,2)
        TRM(I,10,18) = SC11*V6(I,1,3)+SC12*V6(I,2,3)+SC13*V6(I,3,3)
        TRM(I,11,18) = SC21*V6(I,1,3)+SC22*V6(I,2,3)+SC23*V6(I,3,3)
        TRM(I,12,18) = SC31*V6(I,1,3)+SC32*V6(I,2,3)+SC33*V6(I,3,3)
        
        TRM(I,13,16) = SD11*V6(I,1,1)+SD12*V6(I,2,1)+SD13*V6(I,3,1)
        TRM(I,14,16) = SD21*V6(I,1,1)+SD22*V6(I,2,1)+SD23*V6(I,3,1)
        TRM(I,15,16) = SD31*V6(I,1,1)+SD32*V6(I,2,1)+SD33*V6(I,3,1)
        TRM(I,13,17) = SD11*V6(I,1,2)+SD12*V6(I,2,2)+SD13*V6(I,3,2)
        TRM(I,14,17) = SD21*V6(I,1,2)+SD22*V6(I,2,2)+SD23*V6(I,3,2)
        TRM(I,15,17) = SD31*V6(I,1,2)+SD32*V6(I,2,2)+SD33*V6(I,3,2)
        TRM(I,13,18) = SD11*V6(I,1,3)+SD12*V6(I,2,3)+SD13*V6(I,3,3)
        TRM(I,14,18) = SD21*V6(I,1,3)+SD22*V6(I,2,3)+SD23*V6(I,3,3)
        TRM(I,15,18) = SD31*V6(I,1,3)+SD32*V6(I,2,3)+SD33*V6(I,3,3)
        
        TRM(I,16,16) = SE11*V6(I,1,1)+SE12*V6(I,2,1)+SE13*V6(I,3,1)
        TRM(I,17,16) = SE21*V6(I,1,1)+SE22*V6(I,2,1)+SE23*V6(I,3,1)
        TRM(I,18,16) = SE31*V6(I,1,1)+SE32*V6(I,2,1)+SE33*V6(I,3,1)
        TRM(I,16,17) = SE11*V6(I,1,2)+SE12*V6(I,2,2)+SE13*V6(I,3,2)
        TRM(I,17,17) = SE21*V6(I,1,2)+SE22*V6(I,2,2)+SE23*V6(I,3,2)
        TRM(I,18,17) = SE31*V6(I,1,2)+SE32*V6(I,2,2)+SE33*V6(I,3,2)
        TRM(I,16,18) = SE11*V6(I,1,3)+SE12*V6(I,2,3)+SE13*V6(I,3,3)
        TRM(I,17,18) = SE21*V6(I,1,3)+SE22*V6(I,2,3)+SE23*V6(I,3,3)
        TRM(I,18,18) = SE31*V6(I,1,3)+SE32*V6(I,2,3)+SE33*V6(I,3,3)
        
        TRM(I,19,16) = SF11*V6(I,1,1)+SF12*V6(I,2,1)+SF13*V6(I,3,1)
        TRM(I,20,16) = SF21*V6(I,1,1)+SF22*V6(I,2,1)+SF23*V6(I,3,1)
        TRM(I,21,16) = SF31*V6(I,1,1)+SF32*V6(I,2,1)+SF33*V6(I,3,1)
        TRM(I,19,17) = SF11*V6(I,1,2)+SF12*V6(I,2,2)+SF13*V6(I,3,2)
        TRM(I,20,17) = SF21*V6(I,1,2)+SF22*V6(I,2,2)+SF23*V6(I,3,2)
        TRM(I,21,17) = SF31*V6(I,1,2)+SF32*V6(I,2,2)+SF33*V6(I,3,2)
        TRM(I,19,18) = SF11*V6(I,1,3)+SF12*V6(I,2,3)+SF13*V6(I,3,3)
        TRM(I,20,18) = SF21*V6(I,1,3)+SF22*V6(I,2,3)+SF23*V6(I,3,3)
        TRM(I,21,18) = SF31*V6(I,1,3)+SF32*V6(I,2,3)+SF33*V6(I,3,3)
        
        TRM(I,22,16) = SG11*V6(I,1,1)+SG12*V6(I,2,1)+SG13*V6(I,3,1)
        TRM(I,23,16) = SG21*V6(I,1,1)+SG22*V6(I,2,1)+SG23*V6(I,3,1)
        TRM(I,24,16) = SG31*V6(I,1,1)+SG32*V6(I,2,1)+SG33*V6(I,3,1)
        TRM(I,22,17) = SG11*V6(I,1,2)+SG12*V6(I,2,2)+SG13*V6(I,3,2)
        TRM(I,23,17) = SG21*V6(I,1,2)+SG22*V6(I,2,2)+SG23*V6(I,3,2)
        TRM(I,24,17) = SG31*V6(I,1,2)+SG32*V6(I,2,2)+SG33*V6(I,3,2)
        TRM(I,22,18) = SG11*V6(I,1,3)+SG12*V6(I,2,3)+SG13*V6(I,3,3)
        TRM(I,23,18) = SG21*V6(I,1,3)+SG22*V6(I,2,3)+SG23*V6(I,3,3)
        TRM(I,24,18) = SG31*V6(I,1,3)+SG32*V6(I,2,3)+SG33*V6(I,3,3)
        
        TRM(I,1,19) = ZERO
        TRM(I,2,19) = ZERO
        TRM(I,3,19) = ZERO
        TRM(I,1,20) = ZERO
        TRM(I,2,20) = ZERO
        TRM(I,3,20) = ZERO
        TRM(I,1,21) = ZERO
        TRM(I,2,21) = ZERO
        TRM(I,3,21) = ZERO

        TRM(I,4,19) = SA11*V7(I,1,1)+SA12*V7(I,2,1)+SA13*V7(I,3,1)
        TRM(I,5,19) = SA21*V7(I,1,1)+SA22*V7(I,2,1)+SA23*V7(I,3,1)
        TRM(I,6,19) = SA31*V7(I,1,1)+SA32*V7(I,2,1)+SA33*V7(I,3,1)
        TRM(I,4,20) = SA11*V7(I,1,2)+SA12*V7(I,2,2)+SA13*V7(I,3,2)
        TRM(I,5,20) = SA21*V7(I,1,2)+SA22*V7(I,2,2)+SA23*V7(I,3,2)
        TRM(I,6,20) = SA31*V7(I,1,2)+SA32*V7(I,2,2)+SA33*V7(I,3,2)
        TRM(I,4,21) = SA11*V7(I,1,3)+SA12*V7(I,2,3)+SA13*V7(I,3,3)
        TRM(I,5,21) = SA21*V7(I,1,3)+SA22*V7(I,2,3)+SA23*V7(I,3,3)
        TRM(I,6,21) = SA31*V7(I,1,3)+SA32*V7(I,2,3)+SA33*V7(I,3,3)
        
        TRM(I,7,19) = SB11*V7(I,1,1)+SB12*V7(I,2,1)+SB13*V7(I,3,1)
        TRM(I,8,19) = SB21*V7(I,1,1)+SB22*V7(I,2,1)+SB23*V7(I,3,1)
        TRM(I,9,19) = SB31*V7(I,1,1)+SB32*V7(I,2,1)+SB33*V7(I,3,1)
        TRM(I,7,20) = SB11*V7(I,1,2)+SB12*V7(I,2,2)+SB13*V7(I,3,2)
        TRM(I,8,20) = SB21*V7(I,1,2)+SB22*V7(I,2,2)+SB23*V7(I,3,2)
        TRM(I,9,20) = SB31*V7(I,1,2)+SB32*V7(I,2,2)+SB33*V7(I,3,2)
        TRM(I,7,21) = SB11*V7(I,1,3)+SB12*V7(I,2,3)+SB13*V7(I,3,3)
        TRM(I,8,21) = SB21*V7(I,1,3)+SB22*V7(I,2,3)+SB23*V7(I,3,3)
        TRM(I,9,21) = SB31*V7(I,1,3)+SB32*V7(I,2,3)+SB33*V7(I,3,3)
        
        TRM(I,10,19) = SC11*V7(I,1,1)+SC12*V7(I,2,1)+SC13*V7(I,3,1)
        TRM(I,11,19) = SC21*V7(I,1,1)+SC22*V7(I,2,1)+SC23*V7(I,3,1)
        TRM(I,12,19) = SC31*V7(I,1,1)+SC32*V7(I,2,1)+SC33*V7(I,3,1)
        TRM(I,10,20) = SC11*V7(I,1,2)+SC12*V7(I,2,2)+SC13*V7(I,3,2)
        TRM(I,11,20) = SC21*V7(I,1,2)+SC22*V7(I,2,2)+SC23*V7(I,3,2)
        TRM(I,12,20) = SC31*V7(I,1,2)+SC32*V7(I,2,2)+SC33*V7(I,3,2)
        TRM(I,10,21) = SC11*V7(I,1,3)+SC12*V7(I,2,3)+SC13*V7(I,3,3)
        TRM(I,11,21) = SC21*V7(I,1,3)+SC22*V7(I,2,3)+SC23*V7(I,3,3)
        TRM(I,12,21) = SC31*V7(I,1,3)+SC32*V7(I,2,3)+SC33*V7(I,3,3)
        
        TRM(I,13,19) = SD11*V7(I,1,1)+SD12*V7(I,2,1)+SD13*V7(I,3,1)
        TRM(I,14,19) = SD21*V7(I,1,1)+SD22*V7(I,2,1)+SD23*V7(I,3,1)
        TRM(I,15,19) = SD31*V7(I,1,1)+SD32*V7(I,2,1)+SD33*V7(I,3,1)
        TRM(I,13,20) = SD11*V7(I,1,2)+SD12*V7(I,2,2)+SD13*V7(I,3,2)
        TRM(I,14,20) = SD21*V7(I,1,2)+SD22*V7(I,2,2)+SD23*V7(I,3,2)
        TRM(I,15,20) = SD31*V7(I,1,2)+SD32*V7(I,2,2)+SD33*V7(I,3,2)
        TRM(I,13,21) = SD11*V7(I,1,3)+SD12*V7(I,2,3)+SD13*V7(I,3,3)
        TRM(I,14,21) = SD21*V7(I,1,3)+SD22*V7(I,2,3)+SD23*V7(I,3,3)
        TRM(I,15,21) = SD31*V7(I,1,3)+SD32*V7(I,2,3)+SD33*V7(I,3,3)
        
        TRM(I,16,19) = SE11*V7(I,1,1)+SE12*V7(I,2,1)+SE13*V7(I,3,1)
        TRM(I,17,19) = SE21*V7(I,1,1)+SE22*V7(I,2,1)+SE23*V7(I,3,1)
        TRM(I,18,19) = SE31*V7(I,1,1)+SE32*V7(I,2,1)+SE33*V7(I,3,1)
        TRM(I,16,20) = SE11*V7(I,1,2)+SE12*V7(I,2,2)+SE13*V7(I,3,2)
        TRM(I,17,20) = SE21*V7(I,1,2)+SE22*V7(I,2,2)+SE23*V7(I,3,2)
        TRM(I,18,20) = SE31*V7(I,1,2)+SE32*V7(I,2,2)+SE33*V7(I,3,2)
        TRM(I,16,21) = SE11*V7(I,1,3)+SE12*V7(I,2,3)+SE13*V7(I,3,3)
        TRM(I,17,21) = SE21*V7(I,1,3)+SE22*V7(I,2,3)+SE23*V7(I,3,3)
        TRM(I,18,21) = SE31*V7(I,1,3)+SE32*V7(I,2,3)+SE33*V7(I,3,3)
        
        TRM(I,19,19) = SF11*V7(I,1,1)+SF12*V7(I,2,1)+SF13*V7(I,3,1)
        TRM(I,20,19) = SF21*V7(I,1,1)+SF22*V7(I,2,1)+SF23*V7(I,3,1)
        TRM(I,21,19) = SF31*V7(I,1,1)+SF32*V7(I,2,1)+SF33*V7(I,3,1)
        TRM(I,19,20) = SF11*V7(I,1,2)+SF12*V7(I,2,2)+SF13*V7(I,3,2)
        TRM(I,20,20) = SF21*V7(I,1,2)+SF22*V7(I,2,2)+SF23*V7(I,3,2)
        TRM(I,21,20) = SF31*V7(I,1,2)+SF32*V7(I,2,2)+SF33*V7(I,3,2)
        TRM(I,19,21) = SF11*V7(I,1,3)+SF12*V7(I,2,3)+SF13*V7(I,3,3)
        TRM(I,20,21) = SF21*V7(I,1,3)+SF22*V7(I,2,3)+SF23*V7(I,3,3)
        TRM(I,21,21) = SF31*V7(I,1,3)+SF32*V7(I,2,3)+SF33*V7(I,3,3)
        
        TRM(I,22,19) = SG11*V7(I,1,1)+SG12*V7(I,2,1)+SG13*V7(I,3,1)
        TRM(I,23,19) = SG21*V7(I,1,1)+SG22*V7(I,2,1)+SG23*V7(I,3,1)
        TRM(I,24,19) = SG31*V7(I,1,1)+SG32*V7(I,2,1)+SG33*V7(I,3,1)
        TRM(I,22,20) = SG11*V7(I,1,2)+SG12*V7(I,2,2)+SG13*V7(I,3,2)
        TRM(I,23,20) = SG21*V7(I,1,2)+SG22*V7(I,2,2)+SG23*V7(I,3,2)
        TRM(I,24,20) = SG31*V7(I,1,2)+SG32*V7(I,2,2)+SG33*V7(I,3,2)
        TRM(I,22,21) = SG11*V7(I,1,3)+SG12*V7(I,2,3)+SG13*V7(I,3,3)
        TRM(I,23,21) = SG21*V7(I,1,3)+SG22*V7(I,2,3)+SG23*V7(I,3,3)
        TRM(I,24,21) = SG31*V7(I,1,3)+SG32*V7(I,2,3)+SG33*V7(I,3,3)
        
        TRM(I,1,22) = ZERO
        TRM(I,2,22) = ZERO
        TRM(I,3,22) = ZERO
        TRM(I,1,23) = ZERO
        TRM(I,2,23) = ZERO
        TRM(I,3,23) = ZERO
        TRM(I,1,24) = ZERO
        TRM(I,2,24) = ZERO
        TRM(I,3,24) = ZERO

        TRM(I,4,22) = SA11*V8(I,1,1)+SA12*V8(I,2,1)+SA13*V8(I,3,1)
        TRM(I,5,22) = SA21*V8(I,1,1)+SA22*V8(I,2,1)+SA23*V8(I,3,1)
        TRM(I,6,22) = SA31*V8(I,1,1)+SA32*V8(I,2,1)+SA33*V8(I,3,1)
        TRM(I,4,23) = SA11*V8(I,1,2)+SA12*V8(I,2,2)+SA13*V8(I,3,2)
        TRM(I,5,23) = SA21*V8(I,1,2)+SA22*V8(I,2,2)+SA23*V8(I,3,2)
        TRM(I,6,23) = SA31*V8(I,1,2)+SA32*V8(I,2,2)+SA33*V8(I,3,2)
        TRM(I,4,24) = SA11*V8(I,1,3)+SA12*V8(I,2,3)+SA13*V8(I,3,3)
        TRM(I,5,24) = SA21*V8(I,1,3)+SA22*V8(I,2,3)+SA23*V8(I,3,3)
        TRM(I,6,24) = SA31*V8(I,1,3)+SA32*V8(I,2,3)+SA33*V8(I,3,3)
        
        TRM(I,7,22) = SB11*V8(I,1,1)+SB12*V8(I,2,1)+SB13*V8(I,3,1)
        TRM(I,8,22) = SB21*V8(I,1,1)+SB22*V8(I,2,1)+SB23*V8(I,3,1)
        TRM(I,9,22) = SB31*V8(I,1,1)+SB32*V8(I,2,1)+SB33*V8(I,3,1)
        TRM(I,7,23) = SB11*V8(I,1,2)+SB12*V8(I,2,2)+SB13*V8(I,3,2)
        TRM(I,8,23) = SB21*V8(I,1,2)+SB22*V8(I,2,2)+SB23*V8(I,3,2)
        TRM(I,9,23) = SB31*V8(I,1,2)+SB32*V8(I,2,2)+SB33*V8(I,3,2)
        TRM(I,7,24) = SB11*V8(I,1,3)+SB12*V8(I,2,3)+SB13*V8(I,3,3)
        TRM(I,8,24) = SB21*V8(I,1,3)+SB22*V8(I,2,3)+SB23*V8(I,3,3)
        TRM(I,9,24) = SB31*V8(I,1,3)+SB32*V8(I,2,3)+SB33*V8(I,3,3)
        
        TRM(I,10,22) = SC11*V8(I,1,1)+SC12*V8(I,2,1)+SC13*V8(I,3,1)
        TRM(I,11,22) = SC21*V8(I,1,1)+SC22*V8(I,2,1)+SC23*V8(I,3,1)
        TRM(I,12,22) = SC31*V8(I,1,1)+SC32*V8(I,2,1)+SC33*V8(I,3,1)
        TRM(I,10,23) = SC11*V8(I,1,2)+SC12*V8(I,2,2)+SC13*V8(I,3,2)
        TRM(I,11,23) = SC21*V8(I,1,2)+SC22*V8(I,2,2)+SC23*V8(I,3,2)
        TRM(I,12,23) = SC31*V8(I,1,2)+SC32*V8(I,2,2)+SC33*V8(I,3,2)
        TRM(I,10,24) = SC11*V8(I,1,3)+SC12*V8(I,2,3)+SC13*V8(I,3,3)
        TRM(I,11,24) = SC21*V8(I,1,3)+SC22*V8(I,2,3)+SC23*V8(I,3,3)
        TRM(I,12,24) = SC31*V8(I,1,3)+SC32*V8(I,2,3)+SC33*V8(I,3,3)
        
        TRM(I,13,22) = SD11*V8(I,1,1)+SD12*V8(I,2,1)+SD13*V8(I,3,1)
        TRM(I,14,22) = SD21*V8(I,1,1)+SD22*V8(I,2,1)+SD23*V8(I,3,1)
        TRM(I,15,22) = SD31*V8(I,1,1)+SD32*V8(I,2,1)+SD33*V8(I,3,1)
        TRM(I,13,23) = SD11*V8(I,1,2)+SD12*V8(I,2,2)+SD13*V8(I,3,2)
        TRM(I,14,23) = SD21*V8(I,1,2)+SD22*V8(I,2,2)+SD23*V8(I,3,2)
        TRM(I,15,23) = SD31*V8(I,1,2)+SD32*V8(I,2,2)+SD33*V8(I,3,2)
        TRM(I,13,24) = SD11*V8(I,1,3)+SD12*V8(I,2,3)+SD13*V8(I,3,3)
        TRM(I,14,24) = SD21*V8(I,1,3)+SD22*V8(I,2,3)+SD23*V8(I,3,3)
        TRM(I,15,24) = SD31*V8(I,1,3)+SD32*V8(I,2,3)+SD33*V8(I,3,3)
        
        TRM(I,16,22) = SE11*V8(I,1,1)+SE12*V8(I,2,1)+SE13*V8(I,3,1)
        TRM(I,17,22) = SE21*V8(I,1,1)+SE22*V8(I,2,1)+SE23*V8(I,3,1)
        TRM(I,18,22) = SE31*V8(I,1,1)+SE32*V8(I,2,1)+SE33*V8(I,3,1)
        TRM(I,16,23) = SE11*V8(I,1,2)+SE12*V8(I,2,2)+SE13*V8(I,3,2)
        TRM(I,17,23) = SE21*V8(I,1,2)+SE22*V8(I,2,2)+SE23*V8(I,3,2)
        TRM(I,18,23) = SE31*V8(I,1,2)+SE32*V8(I,2,2)+SE33*V8(I,3,2)
        TRM(I,16,24) = SE11*V8(I,1,3)+SE12*V8(I,2,3)+SE13*V8(I,3,3)
        TRM(I,17,24) = SE21*V8(I,1,3)+SE22*V8(I,2,3)+SE23*V8(I,3,3)
        TRM(I,18,24) = SE31*V8(I,1,3)+SE32*V8(I,2,3)+SE33*V8(I,3,3)
        
        TRM(I,19,22) = SF11*V8(I,1,1)+SF12*V8(I,2,1)+SF13*V8(I,3,1)
        TRM(I,20,22) = SF21*V8(I,1,1)+SF22*V8(I,2,1)+SF23*V8(I,3,1)
        TRM(I,21,22) = SF31*V8(I,1,1)+SF32*V8(I,2,1)+SF33*V8(I,3,1)
        TRM(I,19,23) = SF11*V8(I,1,2)+SF12*V8(I,2,2)+SF13*V8(I,3,2)
        TRM(I,20,23) = SF21*V8(I,1,2)+SF22*V8(I,2,2)+SF23*V8(I,3,2)
        TRM(I,21,23) = SF31*V8(I,1,2)+SF32*V8(I,2,2)+SF33*V8(I,3,2)
        TRM(I,19,24) = SF11*V8(I,1,3)+SF12*V8(I,2,3)+SF13*V8(I,3,3)
        TRM(I,20,24) = SF21*V8(I,1,3)+SF22*V8(I,2,3)+SF23*V8(I,3,3)
        TRM(I,21,24) = SF31*V8(I,1,3)+SF32*V8(I,2,3)+SF33*V8(I,3,3)
        
        TRM(I,22,22) = SG11*V8(I,1,1)+SG12*V8(I,2,1)+SG13*V8(I,3,1)
        TRM(I,23,22) = SG21*V8(I,1,1)+SG22*V8(I,2,1)+SG23*V8(I,3,1)
        TRM(I,24,22) = SG31*V8(I,1,1)+SG32*V8(I,2,1)+SG33*V8(I,3,1)
        TRM(I,22,23) = SG11*V8(I,1,2)+SG12*V8(I,2,2)+SG13*V8(I,3,2)
        TRM(I,23,23) = SG21*V8(I,1,2)+SG22*V8(I,2,2)+SG23*V8(I,3,2)
        TRM(I,24,23) = SG31*V8(I,1,2)+SG32*V8(I,2,2)+SG33*V8(I,3,2)
        TRM(I,22,24) = SG11*V8(I,1,3)+SG12*V8(I,2,3)+SG13*V8(I,3,3)
        TRM(I,23,24) = SG21*V8(I,1,3)+SG22*V8(I,2,3)+SG23*V8(I,3,3)
        TRM(I,24,24) = SG31*V8(I,1,3)+SG32*V8(I,2,3)+SG33*V8(I,3,3)
      enddo
      
       DO J=1,8
        JJ = 3*(J-1)
        DO I=LFT,LLT
          TRM(I,JJ+1,JJ+1) = R(1,1,I)+TRM(I,JJ+1,JJ+1)
          TRM(I,JJ+2,JJ+1) = R(1,2,I)+TRM(I,JJ+2,JJ+1)
          TRM(I,JJ+3,JJ+1) = R(1,3,I)+TRM(I,JJ+3,JJ+1)
          TRM(I,JJ+1,JJ+2) = R(2,1,I)+TRM(I,JJ+1,JJ+2)
          TRM(I,JJ+2,JJ+2) = R(2,2,I)+TRM(I,JJ+2,JJ+2)
          TRM(I,JJ+3,JJ+2) = R(2,3,I)+TRM(I,JJ+3,JJ+2)
          TRM(I,JJ+1,JJ+3) = R(3,1,I)+TRM(I,JJ+1,JJ+3)
          TRM(I,JJ+2,JJ+3) = R(3,2,I)+TRM(I,JJ+2,JJ+3)
          TRM(I,JJ+3,JJ+3) = R(3,3,I)+TRM(I,JJ+3,JJ+3)
        ENDDO
      ENDDO
      
      RETURN
      END
        
Chd|====================================================================
Chd|  S8XREF_IMP                    source/elements/solid/solide8s/srcoor3_imp.F
Chd|-- called by -----------
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|        SRCOOR3_IMP                   source/elements/solid/solide8s/srcoor3_imp.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8XREF_IMP(OFFG   ,XREF   ,
     .             XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8,
     .             YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8,
     .             ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8,
     .             R,NEL   ) 
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
C     REAL ou REAL*8
      my_real
     .   OFFG(*)
      
      DOUBLE PRECISION 
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ), 
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ), 
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ), 
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ), 
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ), 
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ),
     .   R(3,3,MVSIZ), XREF(NEL,21)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      DOUBLE PRECISION
     .    U,V,W,UL,VL,WL
C=======================================================================
        DO I=LFT,LLT
          !IF(ABS(OFFG(I)) <= UN )THEN           !sb voir comment faire avec OFFG
            U = XD2(I)-XD1(I)
            V = YD2(I)-YD1(I)
            W = ZD2(I)-ZD1(I)
            UL = R(1,1,I)*U+R(2,1,I)*V+R(3,1,I)*W
            VL = R(1,2,I)*U+R(2,2,I)*V+R(3,2,I)*W
            WL = R(1,3,I)*U+R(2,3,I)*V+R(3,3,I)*W
            XREF(I,1) = UL
            XREF(I,2) = VL
            XREF(I,3) = WL

            U = XD3(I)-XD1(I)
            V = YD3(I)-YD1(I)
            W = ZD3(I)-ZD1(I)
            UL = R(1,1,I)*U+R(2,1,I)*V+R(3,1,I)*W
            VL = R(1,2,I)*U+R(2,2,I)*V+R(3,2,I)*W
            WL = R(1,3,I)*U+R(2,3,I)*V+R(3,3,I)*W
            XREF(I,4) = UL
            XREF(I,5) = VL
            XREF(I,6) = WL
      
            U = XD4(I)-XD1(I)
            V = YD4(I)-YD1(I)
            W = ZD4(I)-ZD1(I)
            UL = R(1,1,I)*U+R(2,1,I)*V+R(3,1,I)*W
            VL = R(1,2,I)*U+R(2,2,I)*V+R(3,2,I)*W
            WL = R(1,3,I)*U+R(2,3,I)*V+R(3,3,I)*W
            XREF(I,7) = UL
            XREF(I,8) = VL
            XREF(I,9) = WL
      
            U = XD5(I)-XD1(I)
            V = YD5(I)-YD1(I)
            W = ZD5(I)-ZD1(I)
            UL = R(1,1,I)*U+R(2,1,I)*V+R(3,1,I)*W
            VL = R(1,2,I)*U+R(2,2,I)*V+R(3,2,I)*W
            WL = R(1,3,I)*U+R(2,3,I)*V+R(3,3,I)*W
            XREF(I,10) = UL
            XREF(I,11) = VL
            XREF(I,12) = WL
      
            U = XD6(I)-XD1(I)
            V = YD6(I)-YD1(I)
            W = ZD6(I)-ZD1(I)
            UL = R(1,1,I)*U+R(2,1,I)*V+R(3,1,I)*W
            VL = R(1,2,I)*U+R(2,2,I)*V+R(3,2,I)*W
            WL = R(1,3,I)*U+R(2,3,I)*V+R(3,3,I)*W
            XREF(I,13) = UL
            XREF(I,14) = VL
            XREF(I,15) = WL
      
            U = XD7(I)-XD1(I)
            V = YD7(I)-YD1(I)
            W = ZD7(I)-ZD1(I)
            UL = R(1,1,I)*U+R(2,1,I)*V+R(3,1,I)*W
            VL = R(1,2,I)*U+R(2,2,I)*V+R(3,2,I)*W
            WL = R(1,3,I)*U+R(2,3,I)*V+R(3,3,I)*W
            XREF(I,16) = UL
            XREF(I,17) = VL
            XREF(I,18) = WL
      
            U = XD8(I)-XD1(I)
            V = YD8(I)-YD1(I)
            W = ZD8(I)-ZD1(I)
            UL = R(1,1,I)*U+R(2,1,I)*V+R(3,1,I)*W
            VL = R(1,2,I)*U+R(2,2,I)*V+R(3,2,I)*W
            WL = R(1,3,I)*U+R(2,3,I)*V+R(3,3,I)*W
            XREF(I,19) = UL
            XREF(I,20) = VL
            XREF(I,21) = WL
        
        ENDDO
C-----------
      RETURN
      END
