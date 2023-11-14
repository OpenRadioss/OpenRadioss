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
Chd|  SRCOOR3_IMP                   source/elements/solid/solide8s/srcoor3_imp.F
Chd|-- called by -----------
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|-- calls ---------------
Chd|        CRFRAME_IMP                   source/elements/solid/solide8s/crframe_imp.F
Chd|        CRTRANS_IMP                   source/elements/solid/solide8s/crtrans_imp.F
Chd|        GETULOC                       source/elements/solid/solide8s/getuloc.F
Chd|        S8SAV3_IMP                    source/elements/solid/solide8s/s8sav3_imp.F
Chd|        S8XREF_IMP                    source/elements/solid/solide8s/s8xref_imp.F
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
#include      "impl1_c.inc"
#include      "com08_c.inc"
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
       IF(IPARG(80)==1) THEN
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
           CALL S8SAV3_IMP(
     1   OFFG,    SAV,     XD1,     XD2,
     2   XD3,     XD4,     XD5,     XD6,
     3   XD7,     XD8,     YD1,     YD2,
     4   YD3,     YD4,     YD5,     YD6,
     5   YD7,     YD8,     ZD1,     ZD2,
     6   ZD3,     ZD4,     ZD5,     ZD6,
     7   ZD7,     ZD8,     NEL)
          ENDIF
         ELSE
          CALL S8SAV3_IMP(
     1   OFFG,    SAV,     XD1,     XD2,
     2   XD3,     XD4,     XD5,     XD6,
     3   XD7,     XD8,     YD1,     YD2,
     4   YD3,     YD4,     YD5,     YD6,
     5   YD7,     YD8,     ZD1,     ZD2,
     6   ZD3,     ZD4,     ZD5,     ZD6,
     7   ZD7,     ZD8,     NEL)
         ENDIF  !(ISMSTR == 1)
       ENDIF   !(INCONV == 1)
! fin ajout qz
C-----------
C     REPERE CONVECTE 
C-----------
      IF((ISMSTR<=4.OR.(ISMSTR==12.AND.IDTMIN(1)==3)).AND.JLAG>0) THEN
        DO I=LFT,LLT
          IF(ABS(OFFG(I)) > ONE)THEN
            OFF(I) = ABS(OFFG(I))-ONE
            OFF_L  = MIN(OFF_L,OFFG(I))
          ELSE
            OFF(I) = ABS(OFFG(I))
            OFF_L  = MIN(OFF_L,OFFG(I))
          ENDIF
        ENDDO
        IF((ISMSTR==12.AND.IDTMIN(1)==3).AND.JLAG>0) THEN
         DO I=LFT,LLT    
          IF(ABS(OFFG(I)) > ONE)THEN
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

      CALL CRFRAME_IMP(
     1   SAV,     INVJ,    XD1,     XD2,
     2   XD3,     XD4,     XD5,     XD6,
     3   XD7,     XD8,     YD1,     YD2,
     4   YD3,     YD4,     YD5,     YD6,
     5   YD7,     YD8,     ZD1,     ZD2,
     6   ZD3,     ZD4,     ZD5,     ZD6,
     7   ZD7,     ZD8,     R,       NEL)
     
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
        
      CALL CRTRANS_IMP(
     1   SAV,     INVJ,    XD1,     XD2,
     2   XD3,     XD4,     XD5,     XD6,
     3   XD7,     XD8,     YD1,     YD2,
     4   YD3,     YD4,     YD5,     YD6,
     5   YD7,     YD8,     ZD1,     ZD2,
     6   ZD3,     ZD4,     ZD5,     ZD6,
     7   ZD7,     ZD8,     V1,      V2,
     8   V3,      V4,      V5,      V6,
     9   V7,      V8,      R,       TRM,
     A   NEL)

      IF(ISMSTR == 1)THEN
        IF(TT == ZERO) THEN
         CALL S8XREF_IMP(
     1   OFFG,    XREF,    XD1,     XD2,
     2   XD3,     XD4,     XD5,     XD6,
     3   XD7,     XD8,     YD1,     YD2,
     4   YD3,     YD4,     YD5,     YD6,
     5   YD7,     YD8,     ZD1,     ZD2,
     6   ZD3,     ZD4,     ZD5,     ZD6,
     7   ZD7,     ZD8,     R,       NEL)
        ENDIF
        CALL GETULOC(
     1   XREF,         XD1,          XD2,
     2   XD3,          XD4,          XD5,          XD6,
     3   XD7,          XD8,          YD1,          YD2,
     4   YD3,          YD4,          YD5,          YD6,
     5   YD7,          YD8,          ZD1,          ZD2,
     6   ZD3,          ZD4,          ZD5,          ZD6,
     7   ZD7,          ZD8,          ULX1,         ULX2,
     8   ULX3,         ULX4,         ULX5,         ULX6,
     9   ULX7,         ULX8,         ULY1,         ULY2,
     A   ULY3,         ULY4,         ULY5,         ULY6,
     B   ULY7,         ULY8,         ULZ1,         ULZ2,
     C   ULZ3,         ULZ4,         ULZ5,         ULZ6,
     D   ULZ7,         ULZ8,         R,            NEL)
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
        CALL GETULOC(
     1   SAV,             XD1,             XD2,
     2   XD3,             XD4,             XD5,             XD6,
     3   XD7,             XD8,             YD1,             YD2,
     4   YD3,             YD4,             YD5,             YD6,
     5   YD7,             YD8,             ZD1,             ZD2,
     6   ZD3,             ZD4,             ZD5,             ZD6,
     7   ZD7,             ZD8,             ULX1,            ULX2,
     8   ULX3,            ULX4,            ULX5,            ULX6,
     9   ULX7,            ULX8,            ULY1,            ULY2,
     A   ULY3,            ULY4,            ULY5,            ULY6,
     B   ULY7,            ULY8,            ULZ1,            ULZ2,
     C   ULZ3,            ULZ4,            ULZ5,            ULZ6,
     D   ULZ7,            ULZ8,            R,               NEL)
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
!            GAMA(I,1) = ONE                             
!            GAMA(I,2) = ZERO                              
!            GAMA(I,3) = ZERO
!            GAMA(I,4) = ZERO                              
!            GAMA(I,5) = ONE                              
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
