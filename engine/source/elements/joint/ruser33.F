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
Chd|  RUSER33                       source/elements/joint/ruser33.F
Chd|-- called by -----------
Chd|        RGJOINT                       source/elements/joint/rgjoint.F
Chd|-- calls ---------------
Chd|        DEF_FDOF                      source/elements/joint/ruser33.F
Chd|        SENS_BLOCK                    source/elements/joint/ruser33.F
Chd|        STDPL                         source/elements/joint/ruser33.F
Chd|        XDDL33                        source/elements/joint/ruser33.F
Chd|        XDDL33I                       source/elements/joint/ruser33.F
Chd|        GET_U_GEO                     source/user_interface/upidmid.F
Chd|        GET_U_PNU                     source/user_interface/upidmid.F
Chd|====================================================================
      SUBROUTINE RUSER33(NEL   ,IOUT   ,IPROP  ,NUVAR  ,UVAR  ,
     .                   FX    ,FY     ,FZ     ,XMOM   ,YMOM  ,
     .                   ZMOM  ,XKM    ,XKR    ,XCM    ,XCR   ,
     .                   XL    ,MASS   ,INER   ,OFF    ,EINT  ,
     .                   ROT1  ,ROT2   ,DX     ,DY     ,DZ    ,
     .                   RX    ,RY     ,RZ     ,IGTYP  ,ISENS ,
     .                   X0_ERR)
C-------------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      INTEGER NEL,IOUT,IPROP,NUVAR,IGTYP,ISENS
      my_real DT,
     .   UVAR(NUVAR,*), FX(*), FY(*),  FZ(*), EINT(*),
     .   XMOM(*), YMOM(*), ZMOM(*), XKM(*), XKR(*),
     .   XCM(*) ,XCR(*), MASS(*) ,INER(*),
     .   OFF(*), ROT1(3,MVSIZ), ROT2(3,MVSIZ),
     .   DX(*), DY(*), DZ(*), RX(*), RY(*), RZ(*), X0_ERR(3,*)
      DOUBLE PRECISION XL(MVSIZ,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, K, JTYP, ISK1, ISK2,
     .        IFUN_XX,IFUN_YY,IFUN_ZZ,IFUN_RX,IFUN_RY,IFUN_RZ,
     .        IFUN_CXX,IFUN_CYY,IFUN_CZZ,IFUN_CRX,IFUN_CRY,IFUN_CRZ,
     .        IFUN_FMX,IFUN_FMY,IFUN_FMZ,IFUN_FMRX,IFUN_FMRY,IFUN_FMRZ,     
     .        KFUNC,KMAT,KPROP,GET_U_PNU,GET_U_PID,GET_U_MID,GET_U_MNU,
     .        FDOF(6),ICOMBT,ICOMBR     
      my_real 
     .        LX2,LY2,LZ2,CXX,CYY,CZZ,CRX,CRY,CRZ,
     .        CR1,CR2,CR3,CR4,CR5,CR6,CX, MS,IN,FAC_CTX,FAC_CRX,
     .        MY1(MVSIZ),MY2(MVSIZ),MZ1(MVSIZ),MZ2(MVSIZ),MX1(MVSIZ),
     .        MX2(MVSIZ),FOLD(MVSIZ,6),DXOLD(MVSIZ,6),L2(MVSIZ),
     .        KNX(MVSIZ),KNY(MVSIZ),KNZ(MVSIZ),
     .        KRX(MVSIZ),KRY(MVSIZ),KRZ(MVSIZ),
     .        VX(MVSIZ),VY(MVSIZ),VZ(MVSIZ),
     .        VX2(MVSIZ),VY2(MVSIZ),VZ2(MVSIZ),
     .        TH1(MVSIZ),TH2(MVSIZ),TH3(MVSIZ),
     .        VRX(MVSIZ),VRY(MVSIZ),VRZ(MVSIZ),
     .        GET_U_MAT, GET_U_GEO, GET_U_FUNC,
     .        SMA(6),SMI(6),KNN(MVSIZ),KRR(MVSIZ),CR,
     .        DXS(MVSIZ),DYS(MVSIZ),DZS(MVSIZ),DRXS(MVSIZ),
     .        DRYS(MVSIZ),DRZS(MVSIZ),KF(6),DXSK(6),FM(6),
     .        FCOMB(6),DEQ(MVSIZ),REQ(MVSIZ),XCENT(6),SMEQT,SMEQR,
     .        FAC_LOC_L,FAC_LOC_T,FAC_X,FAC_R
      DOUBLE PRECISION X0DP(3)              
C-----------------------------------------------
      EXTERNAL GET_U_MAT,GET_U_GEO, GET_U_FUNC, 
     .         GET_U_PNU,GET_U_PID,GET_U_MID,GET_U_MNU
C-----------------------------------------------
      PARAMETER (KFUNC=29)
      PARAMETER (KMAT=31)
      PARAMETER (KPROP=33)
C=======================================================================
      DT = DT1
      IF (DT.EQ.ZERO) DT = EP30
      JTYP = NINT(GET_U_GEO(1,IPROP))
C
      CR   = GET_U_GEO(12,IPROP)  
      CR1  = GET_U_GEO(15,IPROP)
      CR2  = GET_U_GEO(16,IPROP)
      CR3  = GET_U_GEO(17,IPROP)
      CR4  = GET_U_GEO(18,IPROP)
      CR5  = GET_U_GEO(19,IPROP)
      CR6  = GET_U_GEO(20,IPROP)
      CXX = GET_U_GEO(21,IPROP)
      CYY = GET_U_GEO(22,IPROP)
      CZZ = GET_U_GEO(23,IPROP)
      CRX = GET_U_GEO(24,IPROP)
      CRY = GET_U_GEO(25,IPROP)
      CRZ = GET_U_GEO(26,IPROP)
      FAC_LOC_L = GET_U_GEO(27,IPROP)
      FAC_LOC_T = GET_U_GEO(28,IPROP)
C
      FAC_X = ONE / FAC_LOC_L
      FAC_R = ONE
      FAC_CTX = FAC_LOC_T / FAC_LOC_L
      FAC_CRX = FAC_LOC_T
C
      IF (IGTYP.EQ.45) THEN
        IFUN_FMX = GET_U_PNU(13,IPROP,KFUNC) 
        IFUN_FMY = GET_U_PNU(14,IPROP,KFUNC) 
        IFUN_FMZ = GET_U_PNU(15,IPROP,KFUNC) 
        IFUN_FMRX = GET_U_PNU(16,IPROP,KFUNC) 
        IFUN_FMRY = GET_U_PNU(17,IPROP,KFUNC) 
        IFUN_FMRZ = GET_U_PNU(18,IPROP,KFUNC)      
        CALL DEF_FDOF(JTYP,FDOF) 
        K = 28
C
        XCENT(1:6) = ZERO
        DO I=1,6
	  KF(I) = GET_U_GEO(40+I,IPROP)
	  FM(I) = GET_U_GEO(46+I,IPROP)	  
          SMI(I) = GET_U_GEO(K+1,IPROP)      
          SMA(I) = GET_U_GEO(K+2,IPROP)
          FCOMB(I) = GET_U_GEO(54+I,IPROP)
          XCENT(I) = HALF*(SMA(I)+SMI(I))
	  K = K+2
        END DO
C
        ICOMBT = NINT(FCOMB(1)+FCOMB(2)+FCOMB(3))
        ICOMBR = NINT(FCOMB(4)+FCOMB(5)+FCOMB(6))
      ELSE
C--  old kjoint type33 -> combined stopping displ/angles not allowed
        ICOMBT = 0
        ICOMBR = 0
      ENDIF	      
C
      IFUN_XX = GET_U_PNU(1,IPROP,KFUNC) 
      IFUN_YY = GET_U_PNU(2,IPROP,KFUNC) 
      IFUN_ZZ = GET_U_PNU(3,IPROP,KFUNC) 
      IFUN_RX = GET_U_PNU(4,IPROP,KFUNC) 
      IFUN_RY = GET_U_PNU(5,IPROP,KFUNC) 
      IFUN_RZ = GET_U_PNU(6,IPROP,KFUNC)
C
      IFUN_CXX = GET_U_PNU(7,IPROP,KFUNC) 
      IFUN_CYY = GET_U_PNU(8,IPROP,KFUNC) 
      IFUN_CZZ = GET_U_PNU(9,IPROP,KFUNC) 
      IFUN_CRX = GET_U_PNU(10,IPROP,KFUNC) 
      IFUN_CRY = GET_U_PNU(11,IPROP,KFUNC) 
      IFUN_CRZ = GET_U_PNU(12,IPROP,KFUNC)
C
      DO I=1,NEL
        KNX(I) = UVAR(19,I)
        KNY(I) = UVAR(20,I)
        KNZ(I) = UVAR(21,I)
        KRX(I) = UVAR(31,I)
        KRY(I) = UVAR(32,I)
        KRZ(I) = UVAR(33,I)
	KNN(I) = UVAR(17,I)
	KRR(I) = UVAR(18,I)
        IF ((IGTYP.EQ.45).AND.(ISENS.EQ.1)) THEN
	  DO K=1,6
            DXSK(K) = UVAR(4+K-1,I)	    
	    IF (ABS(SMA(K)).GT.EM20) DXSK(K) = MIN(DXSK(K),SMA(K))
	    IF (ABS(SMI(K)).GT.EM20) DXSK(K) = MAX(DXSK(K),SMI(K))	    
	  END DO
	  DXS(I) = DXSK(1)	  
	  DYS(I) = DXSK(2)
	  DZS(I) = DXSK(3)
	  DRXS(I) = DXSK(4)
	  DRYS(I) = DXSK(5)
	  DRZS(I) = DXSK(6)		  	  	  	  
	ENDIF 		
        DXOLD(I,1) = DX(I)
        DXOLD(I,2) = DY(I)
        DXOLD(I,3) = DZ(I)
        DXOLD(I,4) = RX(I)
        DXOLD(I,5) = RY(I)
        DXOLD(I,6) = RZ(I)
        FOLD(I,1) = FX(I)
        FOLD(I,2) = FY(I)
        FOLD(I,3) = FZ(I)
        FOLD(I,4) = XMOM(I)
        FOLD(I,5) = YMOM(I)
        FOLD(I,6) = ZMOM(I)
        IF (IRESP == 1) THEN
C- simple precision - extended sple precsion only for translational dof
          IF (TT == ZERO) THEN
            UVAR(1,I)=XL(I,1)
            UVAR(2,I)=XL(I,2)
            UVAR(3,I)=XL(I,3)
            X0_ERR(1,I)= XL(I,1)-UVAR(1,I)
            X0_ERR(2,I)= XL(I,2)-UVAR(2,I)
            X0_ERR(3,I)= XL(I,3)-UVAR(3,I)
          ENDIF
          X0DP(1) = UVAR(1,I)
          X0DP(2) = UVAR(2,I)
          X0DP(3) = UVAR(3,I)
          X0DP(1) = X0DP(1) + X0_ERR(1,I)
          X0DP(2) = X0DP(2) + X0_ERR(2,I)
          X0DP(3) = X0DP(3) + X0_ERR(3,I)         
        ELSE
C-- double precision
          X0DP(1) = UVAR(1,I)
          X0DP(2) = UVAR(2,I)
          X0DP(3) = UVAR(3,I)
        ENDIF
C
        DX(I) = XL(I,1)-X0DP(1)
        DY(I) = XL(I,2)-X0DP(2)
        DZ(I) = XL(I,3)-X0DP(3)
        IF (ICOMBT > 1) THEN
          DEQ(I) = SQRT(FCOMB(1)*(DX(I)-XCENT(1))*(DX(I)-XCENT(1))
     .               +FCOMB(2)*(DY(I)-XCENT(2))*(DY(I)-XCENT(2))
     .               +FCOMB(3)*(DZ(I)-XCENT(3))*(DZ(I)-XCENT(3)))
        ELSE
          DEQ(I) = ZERO
        ENDIF
C
        RX(I) = ROT2(1,I)-ROT1(1,I)
        RY(I) = ROT2(2,I)-ROT1(2,I)
        RZ(I) = ROT2(3,I)-ROT1(3,I)
        IF (ICOMBR > 1) THEN
          REQ(I) = SQRT(FCOMB(4)*(RX(I)-XCENT(4))*(RX(I)-XCENT(4))
     .               +FCOMB(5)*(RY(I)-XCENT(5))*(RY(I)-XCENT(5))
     .               +FCOMB(6)*(RZ(I)-XCENT(6))*(RZ(I)-XCENT(6)))
        ELSE
          REQ(I) = ZERO
        ENDIF
C
        VX(I)  = (DX(I) - DXOLD(I,1)) / DT
        VY(I)  = (DY(I) - DXOLD(I,2)) / DT
        VZ(I)  = (DZ(I) - DXOLD(I,3)) / DT
        VRX(I) = (RX(I) - DXOLD(I,4)) / DT
        VRY(I) = (RY(I) - DXOLD(I,5)) / DT
        VRZ(I) = (RZ(I) - DXOLD(I,6)) / DT
        XKM(I) = ZERO
        XKR(I) = ZERO
        XCM(I) = ZERO
        XCR(I) = ZERO
      ENDDO
C--------      ELASTIC
       CALL XDDL33(NEL, DX,   FX, KNX, IFUN_XX, XKM,FAC_X)
       CALL XDDL33(NEL, DY,   FY, KNY, IFUN_YY, XKM,FAC_X)
       CALL XDDL33(NEL, DZ,   FZ, KNZ, IFUN_ZZ, XKM,FAC_X)
       CALL XDDL33(NEL, RX, XMOM, KRX, IFUN_RX, XKR,FAC_R)
       CALL XDDL33(NEL, RY, YMOM, KRY, IFUN_RY, XKR,FAC_R)
       CALL XDDL33(NEL, RZ, ZMOM, KRZ, IFUN_RZ, XKR,FAC_R)

C--------      STOP DISPLACEMENT / ANGLE + FRICTION
       IF (IGTYP.EQ.45) THEN       
         CALL STDPL(NEL,DX,VX,FX,KNX,KNN,KF(1),CR,CR1,SMI(1),SMA(1),
     .              XKM,FM(1),UVAR(10,1),IFUN_FMX,ICOMBT,DEQ,FCOMB(1),XCENT(1),FAC_X)
         CALL STDPL(NEL,DY,VY,FY,KNY,KNN,KF(2),CR,CR2,SMI(2),SMA(2),
     .              XKM,FM(2),UVAR(11,1),IFUN_FMY,ICOMBT,DEQ,FCOMB(2),XCENT(2),FAC_X)
         CALL STDPL(NEL,DZ,VZ,FZ,KNZ,KNN,KF(3),CR,CR3,SMI(3),SMA(3),
     .              XKM,FM(3),UVAR(12,1),IFUN_FMZ,ICOMBT,DEQ,FCOMB(3),XCENT(3),FAC_X)

         CALL STDPL(NEL,RX,VRX,XMOM,KRX,KRR,KF(4),CR,CR4,SMI(4),SMA(4),
     .              XKR,FM(4),UVAR(13,1),IFUN_FMRX,ICOMBR,REQ,FCOMB(4),XCENT(4),FAC_R)
         CALL STDPL(NEL,RY,VRY,YMOM,KRY,KRR,KF(5),CR,CR5,SMI(5),SMA(5),
     .              XKR,FM(5),UVAR(14,1),IFUN_FMRY,ICOMBR,REQ,FCOMB(5),XCENT(5),FAC_R)
         CALL STDPL(NEL,RZ,VRZ,ZMOM,KRZ,KRR,KF(6),CR,CR6,SMI(6),SMA(6),
     .              XKR,FM(6),UVAR(15,1),IFUN_FMRZ,ICOMBR,REQ,FCOMB(6),XCENT(6),FAC_R)
       ENDIF
C--------      JOINT BLOCKED BY SENSOR ACTIVATION
       IF ((IGTYP.EQ.45).AND.(ISENS.EQ.1)) THEN       
         CALL SENS_BLOCK(NEL,DX,FX,KNX,KNN,CR,CR1,DXS,FDOF(1),XKM)
         CALL SENS_BLOCK(NEL,DY,FY,KNY,KNN,CR,CR2,DYS,FDOF(2),XKM)
         CALL SENS_BLOCK(NEL,DZ,FZ,KNZ,KNN,CR,CR3,DZS,FDOF(3),XKM)
         CALL SENS_BLOCK(NEL,RX,XMOM,KRX,KRR,CR,CR4,DRXS,FDOF(4),XKR)
         CALL SENS_BLOCK(NEL,RY,YMOM,KRY,KRR,CR,CR5,DRYS,FDOF(5),XKR)
         CALL SENS_BLOCK(NEL,RZ,ZMOM,KRZ,KRR,CR,CR6,DRZS,FDOF(6),XKR)
       ENDIF       	                              
C--------      CRITICAL DAMPING
       DO I=1,NEL
         MS = ZERO
         IN = ZERO
         IF((UVAR(34,I)+UVAR(35,I)).NE.ZERO)
     .              MS = (UVAR(34,I)*UVAR(35,I))/(UVAR(34,I)+UVAR(35,I))
         IF((UVAR(36,I)+UVAR(37,I)).NE.ZERO)
     .              IN = (UVAR(36,I)*UVAR(37,I))/(UVAR(36,I)+UVAR(37,I))
         CX = MAX(CR1,CR2,CR3,CR4,CR5,CR6)
         XCM(I) = CX*SQRT(XKM(I)*MS)
         XCR(I) = CX*SQRT(XKR(I)*IN)
C
         FX(I)= FX(I) + CR1*SQRT(KNX(I)*MS)*VX(I)
         FY(I)= FY(I) + CR2*SQRT(KNY(I)*MS)*VY(I)
         FZ(I)= FZ(I) + CR3*SQRT(KNZ(I)*MS)*VZ(I)
         XMOM(I)= XMOM(I) + CR4*SQRT(KRX(I)*IN)*VRX(I)
         YMOM(I)= YMOM(I) + CR5*SQRT(KRY(I)*IN)*VRY(I)
         ZMOM(I)= ZMOM(I) + CR6*SQRT(KRZ(I)*IN)*VRZ(I)
       ENDDO
C--------      USER DAMPING
       CALL XDDL33I(NEL, VX,   FX, CXX, IFUN_CXX, XCM, FAC_CTX)
       CALL XDDL33I(NEL, VY,   FY, CYY, IFUN_CYY, XCM, FAC_CTX)
       CALL XDDL33I(NEL, VZ,   FZ, CZZ, IFUN_CZZ, XCM, FAC_CTX)
       CALL XDDL33I(NEL, VRX, XMOM, CRX, IFUN_CRX, XCR, FAC_CRX)
       CALL XDDL33I(NEL, VRY, YMOM, CRY, IFUN_CRY, XCR, FAC_CRX)
       CALL XDDL33I(NEL, VRZ, ZMOM, CRZ, IFUN_CRZ, XCR, FAC_CRX)
C
C----   Internal Energy 
      DO I=1,NEL
        EINT(I) = EINT(I) + HALF*(
     .         (DX(I)-DXOLD(I,1)) * (FX(I)+FOLD(I,1))
     .       + (DY(I)-DXOLD(I,2)) * (FY(I)+FOLD(I,2))
     .       + (DZ(I)-DXOLD(I,3)) * (FZ(I)+FOLD(I,3))
     .       + (RX(I)-DXOLD(I,4)) * (XMOM(I)+FOLD(I,4))
     .       + (RY(I)-DXOLD(I,5)) * (YMOM(I)+FOLD(I,5))
     .       + (RZ(I)-DXOLD(I,6)) * (ZMOM(I)+FOLD(I,6)))
        MASS(I) = ZERO
        INER(I) = ZERO
      ENDDO
C-------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  XDDL33                        source/elements/joint/ruser33.F
Chd|-- called by -----------
Chd|        RUSER33                       source/elements/joint/ruser33.F
Chd|-- calls ---------------
Chd|        GET_U_FUNC                    source/user_interface/ufunc.F 
Chd|====================================================================
      SUBROUTINE XDDL33(NEL, DX, FX, KX, IFUN, KMX, FAC_X)
C-------------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      INTEGER I,NEL,IFUN
      my_real DERI, KX(*), DX(*), FX(*), KMX(*), 
     .         GET_U_FUNC,FAC_X       
      EXTERNAL GET_U_FUNC
C----------------------------------------------------------
      my_real FX0
C-------------------------------------------------------------------------
      DO I=1,NEL
        IF(IFUN.EQ.0) THEN
          FX(I) = KX(I)*DX(I)
          KMX(I)= MAX(KMX(I),KX(I))
        ELSE
          FX0= KX(I)*GET_U_FUNC(IFUN,ZERO,DERI)
          FX(I) = KX(I)*GET_U_FUNC(IFUN,DX(I)*FAC_X,DERI)
          KMX(I) = MAX(KMX(I),(KX(I)*FAC_X)*DERI,(FX(I)-FX0)/MAX(EM15,DX(I)))
C---- KX is no longer a scale factor on force but the effective stiffness
          KX(I) = (KX(I)*FAC_X)*DERI
        ENDIF
      ENDDO
C-------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  XDDL33I                       source/elements/joint/ruser33.F
Chd|-- called by -----------
Chd|        RUSER33                       source/elements/joint/ruser33.F
Chd|-- calls ---------------
Chd|        GET_U_FUNC                    source/user_interface/ufunc.F 
Chd|====================================================================
      SUBROUTINE XDDL33I(NEL, DX, FX, KX, IFUN, KMX, FAC_X)
C-------------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      INTEGER I,NEL,IFUN
      my_real DERI, FAC_X, KX, DX(*), FX(*), KMX(*), 
     .        GET_U_FUNC 
      EXTERNAL GET_U_FUNC 
C-------------------------------------------------------------------------
      IF (KX.NE.ZERO) THEN
        DO I=1,NEL
          IF(IFUN.EQ.0) THEN
            FX(I) = FX(I) + KX*DX(I)
            KMX(I)= MAX(KMX(I),KX)
          ELSE
            FX(I)  = FX(I) + KX*GET_U_FUNC(IFUN,DX(I)*FAC_X,DERI)
            KMX(I) = MAX(KMX(I),(KX*FAC_X)*DERI)
          ENDIF
        ENDDO
      ENDIF
C-------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  STDPL                         source/elements/joint/ruser33.F
Chd|-- called by -----------
Chd|        RUSER33                       source/elements/joint/ruser33.F
Chd|-- calls ---------------
Chd|        GET_U_FUNC                    source/user_interface/ufunc.F 
Chd|====================================================================
      SUBROUTINE STDPL(NEL,DX,VX,FX,KX,KNN,KF,CR,CRX,DXMI,DXMA,KMX,
     .                 FM,FRP,IFUN,ICOMB,DEQ,FCOMB,XCENT,FAC_X)       
C-------------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com08_c.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      INTEGER I,NEL,IFUN,FLAG_FRIC,ICOMB
      my_real DERI, KX(*), DX(*), VX(*), FX(*), KMX(*),CRX, 
     .         CR,DXMI,DXMA,DELTAX,KNN(*),KF,KLOC,FM,FR,BETA,
     .         DFR,FT,FRP(*),GET_U_FUNC,KTAN,FFM,KTMAX,DEQ(*),FCOMB,RADIUS,XCENT,FAC_X
      EXTERNAL GET_U_FUNC                
C-------------------------------------------------------------------------
C
      IF ((KF.GT.EM20).AND.((FM.GT.EM20).OR.(IFUN.GT.0))) THEN      
C------Friction-----------------------------------------------------------     
        DO I=1,NEL
	  DFR = KF*VX(I)*DT12
	  FR = FRP(I) + DFR 
          FT = MAX(FR*FR,EM30)
          DELTAX = ZERO
C
          IF (IFUN.GT.0) THEN
	    FFM = FM*GET_U_FUNC(IFUN,DX(I)*FAC_X,DERI)
	    KTMAX = MAX(KF,ABS((FM*FAC_X)*DERI))
	  ELSE
	    FFM = FM
	    KTMAX = KF  
	  ENDIF	  
C	  
          BETA = MIN(ONE,SQRT((FFM**2)/FT))
C
          IF (ICOMB <= 1) THEN
C-    stopping displacment/ angle
	    IF ((DX(I).GT.0).AND.(DXMA.GT.0)) DELTAX = MAX(ZERO,DX(I)-DXMA)
            IF ((DX(I).LT.0).AND.(DXMI.LT.0)) DELTAX = MIN(ZERO,DX(I)-DXMI)
	    IF (ABS(DELTAX).GT.EM20) BETA = ONE
          ELSE
C-    combined stopping displacment/ angle
            RADIUS = HALF*(DXMA-DXMI)
            IF ((DEQ(I).GT.0).AND.(RADIUS.GT.0)) DELTAX = MAX(ZERO,DEQ(I)-RADIUS)
            FR = FRP(I)
	    IF (ABS(DELTAX).GT.EM20) THEN
              DFR = DFR*FCOMB*ABS(DX(I)-XCENT)/DEQ(I)
              BETA = ONE
              FR = FRP(I) + DFR
            ENDIF
          ENDIF
C	  
	  FR = FR * BETA
	  KTAN = ABS((FR-FRP(I))/(MAX(EM15,VX(I)*DT12)))	  
	  KTAN = MIN(KTAN,KTMAX)
	  FRP(I) = FR	  
C	    
          FX(I) = FX(I) + FR
	  KX(I) = KX(I) + KTAN
	  KLOC = KX(I) + MAX(KTAN,KF)
	  CRX = CR	  
          KMX(I)= MAX(KMX(I),KLOC)
        ENDDO      

      ELSEIF (((ABS(DXMI)+DXMA).GT.0).AND.(ICOMB <= 1)) THEN      
C------Stopping Angles / displacements only--------------------------------      
        DO I=1,NEL
          DELTAX = ZERO
	  KLOC = KNN(I)
	  IF (KF.GT.EM20) KLOC = KF
          IF ((DX(I).GT.0).AND.(DXMA.GT.0)) DELTAX = MAX(ZERO,DX(I)-DXMA)
          IF ((DX(I).LT.0).AND.(DXMI.LT.0)) DELTAX = MIN(ZERO,DX(I)-DXMI)
	  IF (ABS(DELTAX).GT.EM20) THEN
             FX(I) = FX(I) + KLOC*DELTAX
	     KX(I) = KX(I) + KLOC
	     CRX = CR	  
             KMX(I)= MAX(KMX(I),KX(I))
	  ELSEIF (ABS((DX(I)-DXMA)/DXMA).LT.0.001) THEN
	     KX(I) = KX(I) + KLOC
	     CRX = CR	  
             KMX(I)= MAX(KMX(I),KX(I))	        
	  ENDIF   
        ENDDO
C
      ELSEIF (((ABS(DXMI)+DXMA).GT.0).AND.(ICOMB > 1)) THEN      
C------Combined stopping Angles / displacements only--------------------------------      
        DO I=1,NEL
          DELTAX = ZERO
	  KLOC = KNN(I)
          RADIUS = HALF*(DXMA-DXMI)
	  IF (KF.GT.EM20) KLOC = KF
          IF ((DEQ(I).GT.0).AND.(RADIUS.GT.0)) DELTAX = MAX(ZERO,DEQ(I)-RADIUS)
	  IF (ABS(DELTAX).GT.EM20) THEN
             FX(I) = FX(I) + KLOC*DELTAX*(FCOMB*(DX(I)-XCENT)/DEQ(I))
	     KX(I) = KX(I) + KLOC
	     CRX = CR	  
             KMX(I)= MAX(KMX(I),KX(I))
	  ELSEIF (ABS((DEQ(I)-RADIUS)/RADIUS).LT.0.001) THEN
	     KX(I) = KX(I) + KLOC
	     CRX = CR	  
             KMX(I)= MAX(KMX(I),KX(I))	        
	  ENDIF   
        ENDDO
C	      
      ENDIF
C
                  
C-------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  SENS_BLOCK                    source/elements/joint/ruser33.F
Chd|-- called by -----------
Chd|        RUSER33                       source/elements/joint/ruser33.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SENS_BLOCK(NEL,DX,FX,KX,KNN,CR,CRX,DXS,FLAG,KMX)       
C-------------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      INTEGER I,NEL,FLAG
      my_real DERI, KX(*), DX(*), FX(*), KMX(*), 
     .         CRX,CR,DELTAX,KNN(*),DXS(*)       
C-------------------------------------------------------------------------
      IF (FLAG.EQ.0) GOTO 350
C      
      DO I=1,NEL
         DELTAX = DX(I)-DXS(I)
         FX(I) = FX(I) + KNN(I)*DELTAX
	 KX(I) = KX(I) + KNN(I)
	 CRX = CR	  
         KMX(I)= MAX(KMX(I),KX(I))  
      ENDDO
C      
350   CONTINUE      
C-------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  DEF_FDOF                      source/elements/joint/ruser33.F
Chd|-- called by -----------
Chd|        RUSER33                       source/elements/joint/ruser33.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DEF_FDOF(JTYP,FDOF)       
C-------------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      INTEGER JTYP,FDOF(6)     
C-------------------------------------------------------------------------

      FDOF = 0

      IF (JTYP.EQ.1) THEN
        FDOF(4) = 1
        FDOF(5) = 1
        FDOF(6) = 1		
      ELSEIF (JTYP.EQ.2) THEN
        FDOF(4) = 1		
      ELSEIF (JTYP.EQ.3) THEN
        FDOF(1) = 1
        FDOF(4) = 1
      ELSEIF (JTYP.EQ.4) THEN
        FDOF(2) = 1
        FDOF(3) = 1
        FDOF(4) = 1   		
      ELSEIF (JTYP.EQ.6) THEN
        FDOF(1) = 1
      ELSEIF (JTYP.EQ.7) THEN
        FDOF(2) = 1
        FDOF(3) = 1
      ELSEIF (JTYP.EQ.9) THEN
        FDOF(1) = 1
        FDOF(2) = 1
        FDOF(3) = 1
        FDOF(4) = 1
        FDOF(5) = 1
        FDOF(6) = 1
      ENDIF
     
C-------------------------------
      RETURN
      END
