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
Chd|  RSKEW33                       source/elements/joint/rskew33.F
Chd|-- called by -----------
Chd|        RGJOINT                       source/elements/joint/rgjoint.F
Chd|-- calls ---------------
Chd|        INV3                          source/elements/joint/rskew33.F
Chd|        PROD_AB                       source/elements/joint/rskew33.F
Chd|        QROT33                        source/elements/joint/rskew33.F
Chd|        GET_U_GEO                     source/user_interface/upidmid.F
Chd|        GET_U_SKEW                    source/user_interface/uaccess.F
Chd|====================================================================
      SUBROUTINE RSKEW33(JFT    ,JLT     ,IXR   ,IOUT  ,IPROP,
     .                   NUVAR  ,UVAR    ,RBY   ,X     ,XL   ,
     .                   ROT1   ,ROT2    ,DX    ,DY    ,DZ   ,
     .                   RX     ,RY      ,RZ    ,VR    ,IGTYP,
     .                   SENSOR_TAB,ISENS ,NC1   ,NC2  ,
     .                   XDP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      USE SENSOR_MOD
C-----------------------------------------------
C IXR      | 5*NEL   | I | R | SPRING CONNECTIVITY
C                            | IXR(1,I) IPROP
C                            | IXR(2,I) NODE 1 ID
C                            | IXR(3,I) NODE 2 ID
C                            | IXR(4,I) OPTIONNAL NODE 3 ID
C                            | IXR(5,I) Material ID
C                            | IXR(6,I) SPRING ID
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "com08_c.inc"
#include      "com04_c.inc"
#include      "com01_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT, IOUT, NUVAR, IPROP, IXR(NIXR,*),IGTYP,
     .        ISENS,NC1(*),NC2(*)
C     REAL
      my_real UVAR(NUVAR,*),X(3,*),
     .        ROT1(3,MVSIZ),ROT2(3,MVSIZ),RBY(*),
     .        DX(*), DY(*), DZ(*), RX(*), RY(*), RZ(*),VR(3,*)
      DOUBLE PRECISION XDP(3,*),XL(MVSIZ,3)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, JTYP, IERR, NRB, SKFLG,
     .        IDSK1,IDSK2,ISK1,ISK2,IP1,IP2, 
     .        N1,N2,N3,USENS,INSENS,
     .        ISENS_OLD,ISENS_ACT     
      my_real CO,SI,KSI,NX,NY,NZ,TH,
     .        U(LSKEW),V(LSKEW),U0(LSKEW),V0(LSKEW),EX(LSKEW),
     .        R1(3),R2(3),RM(3),RL1(3),RL2(3),T(3),
     .        X1(LSKEW),X2(LSKEW),Q(LSKEW),A0(LSKEW),B0(LSKEW),
     .        A(LSKEW),B(LSKEW),EXI(LSKEW),
     .        GET_U_GEO,NR,DT(3),DEX(LSKEW),EXPREC(LSKEW)
      DOUBLE PRECISION X21(MVSIZ),Y21(MVSIZ),Z21(MVSIZ)
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  GET_U_SKEW
      EXTERNAL GET_U_SKEW
C=======================================================================
      DO I=JFT,JLT
        NC1(I)= IXR(2,I)
        NC2(I)= IXR(3,I)
      ENDDO
C
      IERR = 0
      ISENS = 0
      JTYP  = NINT(GET_U_GEO(1,IPROP))
      IF (IGTYP==33) THEN
C----------------   skew initialisation for kjoints      
        IDSK1 = NINT(GET_U_GEO(2,IPROP))
        IDSK2 = NINT(GET_U_GEO(3,IPROP))
        SKFLG = NINT(GET_U_GEO(14,IPROP))
        ISK1 = GET_U_SKEW(IDSK1,N1,N2,N2,U)
        ISK2 = GET_U_SKEW(IDSK2,N1,N2,N3,V)
      ELSE
        SKFLG = 0       	
C----------------   sensor check for kjoints2
	      USENS = NINT(GET_U_GEO(2,IPROP))
	      ISENS_ACT = 0
	      IF (USENS>0) THEN
	         ISENS_OLD = NINT(UVAR(16,JFT))      
          DO K=1,NSENSOR
            IF(USENS==SENSOR_TAB(K)%SENS_ID) INSENS=K
          ENDDO
C      
          IF (TT>SENSOR_TAB(INSENS)%TSTART) THEN
            ISENS = 1
	           UVAR(16,JFT) = ISENS
          ENDIF
	         IF (ISENS/=ISENS_OLD) ISENS_ACT = 1
        ENDIF
      ENDIF	
C            	
C ----------------
C     
      DO I=JFT,JLT
C
C----------------   initizialisation at time 0
C
	IF ((NCYCLE==0).AND.(TT==0)) THEN
          IF (IGTYP==33) THEN
            DO J=1,9
              A0(J)= UVAR(3+J,I)
            END DO
          ELSE
            RX(I)= UVAR(7,I)
	    RY(I)= UVAR(8,I)
            RZ(I)= UVAR(9,I)
          ENDIF         	
        ENDIF
C
C----------------   stockage of displacements if sensor is activated 
C
        IF ((IGTYP==45).AND.(ISENS_ACT==1)) THEN	
          UVAR(4,I) = DX(I)
          UVAR(5,I) = DY(I) 
          UVAR(6,I) = DZ(I)
          UVAR(7,I) = RX(I)
          UVAR(8,I) = RY(I) 
          UVAR(9,I) = RZ(I)
        ENDIF	  	 		 		  	
C
C----------------   local frame 
C
        IF (JTYP==5) THEN
C---------------------------------------------		
C----   universal joint - orthogonalized  ----
C---------------------------------------------	
          IF (IGTYP==45) THEN  
C-------------------- nod 1		 	     
             DT(1)= VR(1,NC1(I))*DT1
             DT(2)= VR(2,NC1(I))*DT1
             DT(3)= VR(3,NC1(I))*DT1  	  
	     U(1)=UVAR(10,I) - UVAR(11,I)*DT(3)+UVAR(12,I)*DT(2)
             U(2)=UVAR(11,I) - UVAR(12,I)*DT(1)+UVAR(10,I)*DT(3)
             U(3)=UVAR(12,I) - UVAR(10,I)*DT(2)+UVAR(11,I)*DT(1)
	     NR =SQRT(U(1)*U(1)+U(2)*U(2)+U(3)*U(3))
	     IF (NR>0) THEN
	       U(1)=U(1)/NR
	       U(2)=U(2)/NR
	       U(3)=U(3)/NR
	     ENDIF
C-----
	     UVAR(10,I) = U(1)
	     UVAR(11,I) = U(2)	
	     UVAR(12,I) = U(3)
	     		     	     	     
C-------------------- nod 2		  	     
             DT(1)= VR(1,NC2(I))*DT1
             DT(2)= VR(2,NC2(I))*DT1
             DT(3)= VR(3,NC2(I))*DT1	  	  	     
	     V(1)=UVAR(13,I) - UVAR(14,I)*DT(3)+UVAR(15,I)*DT(2)
             V(2)=UVAR(14,I) - UVAR(15,I)*DT(1)+UVAR(13,I)*DT(3)
             V(3)=UVAR(15,I) - UVAR(13,I)*DT(2)+UVAR(14,I)*DT(1)
	     NR =SQRT(V(1)*V(1)+V(2)*V(2)+V(3)*V(3))
	     IF (NR>0) THEN
	       V(1)=V(1)/NR
	       V(2)=V(2)/NR
	       V(3)=V(3)/NR
	     ENDIF
C-----
	     UVAR(13,I) = V(1)
	     UVAR(14,I) = V(2)	
	     UVAR(15,I) = V(3)	     	     	     
	  ENDIF
	  
          EX(1) = U(2)*V(3) - U(3)*V(2)
          EX(2) = U(3)*V(1) - U(1)*V(3)
          EX(3) = U(1)*V(2) - U(2)*V(1)
          NX = SQRT(EX(1)*EX(1)+EX(2)*EX(2)+EX(3)*EX(3))
          EX(1) = EX(1) / NX
          EX(2) = EX(2) / NX
          EX(3) = EX(3) / NX
          EX(4) = U(1) 
          EX(5) = U(2)
          EX(6) = U(3) 
          EX(7) = V(1)
          EX(8) = V(2)
          EX(9) = V(3)
	  
	  CALL INV3(EX,EXI)	  
	  	    	  	  	  	  
C----- Calcul du vecteur rotation R12
          X21(I) = (VR(1,NC2(I))-VR(1,NC1(I)))*DT1
          Y21(I) = (VR(2,NC2(I))-VR(2,NC1(I)))*DT1
          Z21(I) = (VR(3,NC2(I))-VR(3,NC1(I)))*DT1
C
          RM(1) = RX(I)+EXI(1)*X21(I)+EXI(4)*Y21(I)+EXI(7)*Z21(I)
	  RM(2) = RY(I)+EXI(2)*X21(I)+EXI(5)*Y21(I)+EXI(8)*Z21(I)
	  RM(3) = RZ(I)+EXI(3)*X21(I)+EXI(6)*Y21(I)+EXI(9)*Z21(I)	  	  
C-----		      
          R2(1) =  0.5*RM(1)
          R2(2) =  0.5*RM(2)
          R2(3) =  0.5*RM(3)
          R1(1) = -0.5*RM(1)
          R1(2) = -0.5*RM(2)
          R1(3) = -0.5*RM(3)
C-----		  
          X21(I) = X(1,NC2(I))-X(1,NC1(I))
          Y21(I) = X(2,NC2(I))-X(2,NC1(I))
          Z21(I) = X(3,NC2(I))-X(3,NC1(I))
          XL(I,1)=EXI(1)*X21(I)+EXI(4)*Y21(I)+EXI(7)*Z21(I)
          XL(I,2)=EXI(2)*X21(I)+EXI(5)*Y21(I)+EXI(8)*Z21(I)
          XL(I,3)=EXI(3)*X21(I)+EXI(6)*Y21(I)+EXI(9)*Z21(I)
	  	  
C---------------------------	  
        ELSE
C---------------------------
          IF (SKFLG==1) THEN
C---------------------------------------------	  
C----   first skew is used as mean skew  -----
C---------------------------------------------
	      
            DO J=1,9
	       EX(J) = U(J)
	    ENDDO
   
          ELSE	  
C-------------------------------------	  
C----   mean skew is calculated  -----
C-------------------------------------
	    
C----- Initialisation de EXPREC
            IF ((NCYCLE==0).AND.(TT==0).AND.(IGTYP==33)) THEN
               CALL PROD_AB(U,A0,A)	      
               DO J=1,9
	          EXPREC(J) = A(J)
	       END DO
	    ELSE
               DO J=1,9
	          EXPREC(J) = UVAR(21+J,I)
	       END DO	       
	    ENDIF
	      
C----- Calcul de DEX
            X21(I) = HALF*(VR(1,NC2(I))+VR(1,NC1(I)))*DT1
            Y21(I) = HALF*(VR(2,NC2(I))+VR(2,NC1(I)))*DT1
            Z21(I) = HALF*(VR(3,NC2(I))+VR(3,NC1(I)))*DT1
            DT(1)=EXPREC(1)*X21(I)+EXPREC(2)*Y21(I)+EXPREC(3)*Z21(I)
            DT(2)=EXPREC(4)*X21(I)+EXPREC(5)*Y21(I)+EXPREC(6)*Z21(I)
            DT(3)=EXPREC(7)*X21(I)+EXPREC(8)*Y21(I)+EXPREC(9)*Z21(I)
C----- 	  	  
	    NR =SQRT(DT(1)*DT(1)+DT(2)*DT(2)+DT(3)*DT(3))
	    IF (NR>0) THEN
	       DT(1)=DT(1)/NR
	       DT(2)=DT(2)/NR
	       DT(3)=DT(3)/NR
	    ENDIF
	    CO = COS(NR)
	    SI = SIN(NR)
            CALL QROT33(DEX, DT, CO, SI)
	  
C----- Calcul du nouveau EX	  
            CALL PROD_AB(EXPREC,DEX,EX)

C-------------------------------	  
          ENDIF
C-------------------------------
C----- Calcul du vecteur rotation R12
          X21(I) = (VR(1,NC2(I))-VR(1,NC1(I)))*DT1
          Y21(I) = (VR(2,NC2(I))-VR(2,NC1(I)))*DT1
          Z21(I) = (VR(3,NC2(I))-VR(3,NC1(I)))*DT1
C
          RM(1) = RX(I)+EX(1)*X21(I)+EX(2)*Y21(I)+EX(3)*Z21(I)
	  RM(2) = RY(I)+EX(4)*X21(I)+EX(5)*Y21(I)+EX(6)*Z21(I)
	  RM(3) = RZ(I)+EX(7)*X21(I)+EX(8)*Y21(I)+EX(9)*Z21(I)  	  
C-----		      
          R2(1) =  0.5*RM(1)
          R2(2) =  0.5*RM(2)
          R2(3) =  0.5*RM(3)
          R1(1) = -0.5*RM(1)
          R1(2) = -0.5*RM(2)
          R1(3) = -0.5*RM(3)
C-----	
          IF (IRESP == 1) THEN
C- simple precision - extended sple precsion only for translational dof 
            X21(I) = XDP(1,NC2(I))-XDP(1,NC1(I))
            Y21(I) = XDP(2,NC2(I))-XDP(2,NC1(I))
            Z21(I) = XDP(3,NC2(I))-XDP(3,NC1(I))
          ELSE
C- double precision
            X21(I) = X(1,NC2(I))-X(1,NC1(I))
            Y21(I) = X(2,NC2(I))-X(2,NC1(I))
            Z21(I) = X(3,NC2(I))-X(3,NC1(I))  
          ENDIF
C
          XL(I,1)=EX(1)*X21(I)+EX(2)*Y21(I)+EX(3)*Z21(I)
          XL(I,2)=EX(4)*X21(I)+EX(5)*Y21(I)+EX(6)*Z21(I)
          XL(I,3)=EX(7)*X21(I)+EX(8)*Y21(I)+EX(9)*Z21(I)

C-------------------------------
        ENDIF
C-------------------------------
C
        ROT1(1,I) = R1(1)
        ROT1(2,I) = R1(2)
        ROT1(3,I) = R1(3)
        ROT2(1,I) = R2(1)
        ROT2(2,I) = R2(2)
        ROT2(3,I) = R2(3)
C
        DO J=1,9
          UVAR(21+J,I) = EX(J)
        END DO
      END DO ! DO I=JFT,JLT

C-----------
      RETURN
      END
Chd|====================================================================
Chd|  INV3                          source/elements/joint/rskew33.F
Chd|-- called by -----------
Chd|        I2LOCEQ                       ../common_source/interf/i2loceq.F
Chd|        I2LOCEQ_27                    ../common_source/interf/i2loceq.F
Chd|        I2CURVF                       source/interfaces/interf/i2curvf.F
Chd|        I2CURVFP                      source/interfaces/interf/i2curvfp.F
Chd|        I2CURVV                       source/interfaces/interf/i2curvv.F
Chd|        RSKEW33                       source/elements/joint/rskew33.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INV3(A,B)
C----------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "param_c.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      my_real A(LSKEW),B(LSKEW)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real DET
C=======================================================================

      DET = A(1)*A(5)*A(9)+A(4)*A(8)*A(3)+A(7)*A(2)*A(6)
     .    - A(4)*A(2)*A(9)-A(1)*A(8)*A(6)-A(7)*A(5)*A(3)

      B(1) =  (A(5)*A(9)-A(6)*A(8)) / DET
      B(4) = -(A(4)*A(9)-A(6)*A(7)) / DET
      B(7) =  (A(4)*A(8)-A(5)*A(7)) / DET

      B(2) = -(A(2)*A(9)-A(3)*A(8)) / DET
      B(5) =  (A(1)*A(9)-A(3)*A(7)) / DET
      B(8) = -(A(1)*A(8)-A(2)*A(7)) / DET

      B(3) =  (A(2)*A(6)-A(3)*A(5)) / DET
      B(6) = -(A(1)*A(6)-A(3)*A(4)) / DET
      B(9) =  (A(1)*A(5)-A(2)*A(4)) / DET
C-----------------------------------------------
      RETURN
      END

Chd|====================================================================
Chd|  ROTQ33                        source/elements/joint/rskew33.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ROTQ33(SKEW, T, ROT, C, S)
C-------------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "param_c.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      my_real ROT(3), T(3), SKEW(LSKEW), S, C
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real E11,E22,E33,E12,E21,E13,E31,E23,E32,NR,KSI,
     .        PI1    
C=======================================================================
CCsm45a2       PI= 2.*ATAN2(ONE,ZERO)
       PI1 = 2.*ATAN2(ONE,ZERO)
C---   first skew rotation
       E11 = SKEW(1)   
       E12 = SKEW(4)   
       E13 = SKEW(7)   
       E21 = SKEW(2)   
       E22 = SKEW(5)   
       E23 = SKEW(8)   
       E31 = SKEW(3)   
       E32 = SKEW(6)   
       E33 = SKEW(9)  
       C = HALF * (E11+E22+E33 - ONE)
       C = MIN(C,ONE)
       C = MAX(C,-ONE)
       KSI = ACOS(C)
       S   = SIN(KSI)
       IF(S/=ZERO) S = HALF / S
       T(1) = (E32 - E23) * S
       T(2) = (E13 - E31) * S
       T(3) = (E21 - E12) * S
       NR = SQRT(T(1)*T(1)+T(2)*T(2)+T(3)*T(3))
       IF (NR/=ZERO) NR = ONE/NR
       T(1) = T(1)*NR
       T(2) = T(2)*NR 
       T(3) = T(3)*NR 
       ROT(1) = T(1)*KSI
       ROT(2) = T(2)*KSI
       ROT(3) = T(3)*KSI
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  ROT12                         source/elements/joint/rskew33.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ROT12(SKEW, RVEC, C, S)
C-------------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "param_c.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      my_real SKEW(LSKEW), RVEC(3), S, C
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real E11,E22,E33,E12,E21,E13,E31,E23,E32,NR,KSI,
     .        PI1      
C=======================================================================
C
       PI1 = 2.*ATAN2(ONE,ZERO)
C---   first skew rotation
       E11 = SKEW(1)   
       E12 = SKEW(4)   
       E13 = SKEW(7)   
       E21 = SKEW(2)   
       E22 = SKEW(5)   
       E23 = SKEW(8)   
       E31 = SKEW(3)   
       E32 = SKEW(6)   
       E33 = SKEW(9)  
       C = HALF * (E11+E22+E33 - ONE)
       C = MIN(C,ONE)
       C = MAX(C,-ONE)
       KSI = ACOS(C)
       S   = SIN(KSI)
       IF(S/=ZERO) S  = HALF / S
       RVEC(1) = (E32 - E23) * S
       RVEC(2) = (E13 - E31) * S
       RVEC(3) = (E21 - E12) * S
       NR = SQRT(RVEC(1)*RVEC(1)+RVEC(2)*RVEC(2)+RVEC(3)*RVEC(3))
       IF (NR/=ZERO) NR = ONE/NR
       RVEC(1) = RVEC(1)*NR
       RVEC(2) = RVEC(2)*NR 
       RVEC(3) = RVEC(3)*NR 
C
       C = HALF*(C+ ONE)
       S = SQRT(ONE-C)
       C = SQRT(C)
C---------------------------
      RETURN
      END
Chd|====================================================================
Chd|  QROT33                        source/elements/joint/rskew33.F
Chd|-- called by -----------
Chd|        RSKEW33                       source/elements/joint/rskew33.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE QROT33(SKEW, T, C, S)
C-------------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "param_c.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      my_real T(3), SKEW(LSKEW)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real E11,E22,E33,E12,E21,E13,E31,E23,E32,
     .        U1,U2,U3, U1S, U2S, U3S, S,C,CI
C=======================================================================
      CI = ONE - C
      U1 = T(1)
      U2 = T(2)
      U3 = T(3)
      U1S =  U1*S
      U2S =  U2*S
      U3S =  U3*S
C
      E11 = U1 * U1 *CI + C
      E22 = U2 * U2 *CI + C
      E33 = U3 * U3 *CI + C

      E12 = U1 * U2 * CI   
      E21 = E12 + U3S
      E12 = E12 - U3S
      E13 = U1 * U3 * CI
      E31 = E13 - U2S
      E13 = E13 + U2S
      E23 = U2 * U3 * CI
      E32 = E23 + U1S
      E23 = E23 - U1S
C
      SKEW(1) =  E11  
      SKEW(4) =  E12    
      SKEW(7) =  E13    
      SKEW(2) =  E21    
      SKEW(5) =  E22   
      SKEW(8) =  E23  
      SKEW(3) =  E31  
      SKEW(6) =  E32  
      SKEW(9) =  E33  
C-----------------------------------------------
      RETURN
      END
C==================================================== 
Chd|====================================================================
Chd|  PROD_ABT                      source/elements/joint/rskew33.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PROD_ABT(A,B,X)
#include      "implicit_f.inc"
#include      "param_c.inc"
      INTEGER I,J
      my_real A(LSKEW),B(LSKEW),X(LSKEW)
C
      X(1)=A(1)*B(1)+A(4)*B(4)+A(7)*B(7) 
      X(2)=A(2)*B(1)+A(5)*B(4)+A(8)*B(7) 
      X(3)=A(3)*B(1)+A(6)*B(4)+A(9)*B(7) 
      X(4)=A(1)*B(2)+A(4)*B(5)+A(7)*B(8) 
      X(5)=A(2)*B(2)+A(5)*B(5)+A(8)*B(8) 
      X(6)=A(3)*B(2)+A(6)*B(5)+A(9)*B(8) 
      X(7)=A(1)*B(3)+A(4)*B(6)+A(7)*B(9) 
      X(8)=A(2)*B(3)+A(5)*B(6)+A(8)*B(9) 
      X(9)=A(3)*B(3)+A(6)*B(6)+A(9)*B(9) 
      RETURN
      END
C==================================================== 
Chd|====================================================================
Chd|  PROD_ATB                      source/elements/joint/rskew33.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PROD_ATB(A,B,X)
#include      "implicit_f.inc"
#include      "param_c.inc"
      my_real A(LSKEW),B(LSKEW),X(LSKEW)
C
      X(1)=A(1)*B(1)+A(2)*B(2)+A(3)*B(3) 
      X(2)=A(4)*B(1)+A(5)*B(2)+A(6)*B(3) 
      X(3)=A(7)*B(1)+A(8)*B(2)+A(9)*B(3) 
      X(4)=A(1)*B(4)+A(2)*B(5)+A(3)*B(6) 
      X(5)=A(4)*B(4)+A(5)*B(5)+A(6)*B(6) 
      X(6)=A(7)*B(4)+A(8)*B(5)+A(9)*B(6) 
      X(7)=A(1)*B(7)+A(2)*B(8)+A(3)*B(9) 
      X(8)=A(4)*B(7)+A(5)*B(8)+A(6)*B(9) 
      X(9)=A(7)*B(7)+A(8)*B(8)+A(9)*B(9) 
      RETURN
      END
C==================================================== 
Chd|====================================================================
Chd|  PROD_AB                       source/elements/joint/rskew33.F
Chd|-- called by -----------
Chd|        RSKEW33                       source/elements/joint/rskew33.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PROD_AB(A,B,X)
#include      "implicit_f.inc"
#include      "param_c.inc"
      my_real A(LSKEW),B(LSKEW),X(LSKEW)
C
      X(1)=A(1)*B(1)+A(4)*B(2)+A(7)*B(3) 
      X(2)=A(2)*B(1)+A(5)*B(2)+A(8)*B(3) 
      X(3)=A(3)*B(1)+A(6)*B(2)+A(9)*B(3) 
      X(4)=A(1)*B(4)+A(4)*B(5)+A(7)*B(6) 
      X(5)=A(2)*B(4)+A(5)*B(5)+A(8)*B(6) 
      X(6)=A(3)*B(4)+A(6)*B(5)+A(9)*B(6) 
      X(7)=A(1)*B(7)+A(4)*B(8)+A(7)*B(9) 
      X(8)=A(2)*B(7)+A(5)*B(8)+A(8)*B(9) 
      X(9)=A(3)*B(7)+A(6)*B(8)+A(9)*B(9) 
      RETURN
      END
C==================================================== 
