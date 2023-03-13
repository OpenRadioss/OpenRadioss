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
Chd|  I2MOM27_CIN                   source/interfaces/interf/i2mom27_cin.F
Chd|-- called by -----------
Chd|        I2FOR27                       source/interfaces/interf/i2for27.F
Chd|-- calls ---------------
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE I2MOM27_CIN(NSN ,NMN     ,AR      ,IRECT    ,CRST    ,
     2               MSR     ,NSV     ,IRTL    ,IN       ,MS      ,
     3               A       ,X       ,WEIGHT  ,STIFR    ,STIFN   ,
     4               IDEL2   ,SMASS   ,SINER   ,NMAS     ,ADI     ,
     5               INDXC   ,MINER   ,H3D_DATA,MSEGTYP2 ,CSTS_BIS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NMN, IDEL2,
     .   IRECT(4,*), MSR(*), NSV(*), IRTL(*), WEIGHT(*),INDXC(NSN),MSEGTYP2(*)
C     REAL
      my_real
     .   A(3,*), AR(3,*),CRST(2,*), MS(*),
     .   X(3,*),IN(*),STIFR(*),STIFN(*), SMASS(*), SINER(*),
     .   NMAS(*),ADI(*),MINER(*),CSTS_BIS(2,*)
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, I3, J3, I2, J2, I1, J1, II, L, JJ, W,NIR
C     REAL
      my_real
     .   H(4), XMSJ, SS, ST, XMSI, FXI, FYI, FZI, MXI, MYI, MZI,INS,
     .   X0,X1,X2,X3,X4,Y0,Y1,Y2,Y3,Y4,Z0,Z1,Z2,Z3,Z4,AA,
     .   XC0,YC0,ZC0,SP,SM,TP,TM,XC,YC,ZC,
     .   STF,AI,H2(4)
C=======================================================================
C     MINER(II) initialise a MS(J) dans resol_init
      IF(ANIM_N(12)+OUTP_N(3)+H3D_DATA%N_SCAL_DINER >0) THEN
        DO II=1,NMN
          J=MSR(II)
          ADI(J) = ADI(J)*NMAS(II)
        ENDDO
      ENDIF
      IF (IMACH/=3) THEN
#include "vectorize.inc"
        DO II=1,NMN
          J=MSR(II)
          IN(J)=MAX(EM20,IN(J))
        ENDDO
      ENDIF
C
      IF(IMPL_S>0) THEN
      DO II=1,NSN
       K = INDXC(II)
       IF (K == 0) CYCLE
       I = NSV(K)
       IF(I>0)THEN
        L=IRTL(II)
C
        IF (IRECT(3,L) == IRECT(4,L)) THEN
C--      Shape functions of triangles
          NIR = 3
          H(1) = CRST(1,II)
          H(2) = CRST(2,II)
          H(3) = ONE-CRST(1,II)-CRST(2,II)
          H(4) = ZERO
          H2(1) = CSTS_BIS(1,II)
          H2(2) = CSTS_BIS(2,II)
          H2(3) = ONE-CSTS_BIS(1,II)-CSTS_BIS(2,II)
          H2(4) = ZERO
        ELSE
C--       Shape functions of quadrangles
          NIR = 4
          SS=CRST(1,II)                                    
          ST=CRST(2,II)
          SP=ONE + SS                                       
          SM=ONE - SS                                       
          TP=FOURTH*(ONE + ST)                               
          TM=FOURTH*(ONE - ST)                               
          H(1)=TM*SM                                       
          H(2)=TM*SP                                       
          H(3)=TP*SP                                       
          H(4)=TP*SM

C         Additional form functions for distribution of mass / inertia - to avoid negative masses for projection outside of the element
          SS=CSTS_BIS(1,II)                                    
          ST=CSTS_BIS(2,II)
          SP=ONE + SS                                       
          SM=ONE - SS                                       
          TP=FOURTH*(ONE + ST)                               
          TM=FOURTH*(ONE - ST)                               
          H2(1)=TM*SM                                       
          H2(2)=TM*SP                                       
          H2(3)=TP*SP                                       
          H2(4)=TP*SM  
        ENDIF
C
        XC=ZERO
        YC=ZERO
        ZC=ZERO
        DO JJ=1,NIR
          J=IRECT(JJ,L)
          XC=XC+X(1,J)*H(JJ)
          YC=YC+X(2,J)*H(JJ)
          ZC=ZC+X(3,J)*H(JJ)
        ENDDO
C
        X0 = X(1,I)
        Y0 = X(2,I)
        Z0 = X(3,I)
C
        XC0=X0-XC
        YC0=Y0-YC
        ZC0=Z0-ZC
C
        AA = XC0*XC0 + YC0*YC0 + ZC0*ZC0
        INS = IN(I) + AA * MS(I)
        STF = STIFR(I) + AA * STIFN(I)
C
        FXI=A(1,I)
        FYI=A(2,I)
        FZI=A(3,I)
C
        MXI = AR(1,I) + YC0 * FZI - ZC0 * FYI
        MYI = AR(2,I) + ZC0 * FXI - XC0 * FZI
        MZI = AR(3,I) + XC0 * FYI - YC0 * FXI
C
        W = WEIGHT(I)
        AI=AA * MS(I) * W
        IF (ANIM_N(12)+OUTP_N(3)+H3D_DATA%N_SCAL_DINER >0) THEN
          DO JJ=1,NIR
            J=IRECT(JJ,L)
            ADI(J)=ADI(J)+AI*H(JJ)
          END DO
        END IF
        DO JJ=1,NIR
         J=IRECT(JJ,L)
         IF (MSEGTYP2(L)==1) THEN
          AR(1,J)=AR(1,J)+MXI*H(JJ)*W
          AR(2,J)=AR(2,J)+MYI*H(JJ)*W
          AR(3,J)=AR(3,J)+MZI*H(JJ)*W
          IN(J)=IN(J)+INS*H2(JJ)*W
          STIFR(J)=STIFR(J)+ABS(STF*H(JJ)*W)
         END IF
        ENDDO
        STIFR(I)=EM20
        IF(IDEL2/=0.AND.IN(I)/=0.)SINER(II)=IN(I)
        IN(I)=ZERO
        STIFN(I)=EM20
        IF(IDEL2/=0.AND.MS(I)/=0.)SMASS(II)=MS(I)
        MS(I)=ZERO
        A(1,I)=ZERO
        A(2,I)=ZERO
        A(3,I)=ZERO
       ENDIF
C
      ENDDO
c
      ELSE
c
      DO II=1,NSN
       K = INDXC(II)
       IF (K == 0) CYCLE
       I = NSV(K)
       IF(I>0)THEN
        L=IRTL(II)
C
        SS=CRST(1,II)                                    
        ST=CRST(2,II)
        SP=ONE + SS                                       
        SM=ONE - SS                                       
        TP=FOURTH*(ONE + ST)                               
        TM=FOURTH*(ONE - ST)
C                               
        IF (IRECT(3,L) == IRECT(4,L)) THEN
C--      Shape functions of triangles
          NIR = 3
          H(1) = CRST(1,II)
          H(2) = CRST(2,II)
          H(3) = ONE-CRST(1,II)-CRST(2,II)
          H(4) = ZERO
          H2(1) = CSTS_BIS(1,II)
          H2(2) = CSTS_BIS(2,II)
          H2(3) = ONE-CSTS_BIS(1,II)-CSTS_BIS(2,II)
          H2(4) = ZERO
        ELSE
C--       Shape functions of quadrangles
          NIR = 4
          SS=CRST(1,II)                                    
          ST=CRST(2,II)
          SP=ONE + SS                                       
          SM=ONE - SS                                       
          TP=FOURTH*(ONE + ST)                               
          TM=FOURTH*(ONE - ST)                               
          H(1)=TM*SM                                       
          H(2)=TM*SP                                       
          H(3)=TP*SP                                       
          H(4)=TP*SM

C         Additional form functions for distribution of mass / inertia - to avoid negative masses for projection outside of the element
          SS=CSTS_BIS(1,II)                                    
          ST=CSTS_BIS(2,II)
          SP=ONE + SS                                       
          SM=ONE - SS                                       
          TP=FOURTH*(ONE + ST)                               
          TM=FOURTH*(ONE - ST)                               
          H2(1)=TM*SM                                       
          H2(2)=TM*SP                                       
          H2(3)=TP*SP                                       
          H2(4)=TP*SM  
        ENDIF      
C
        X0 = X(1,I)
        Y0 = X(2,I)
        Z0 = X(3,I)
C
        X1 = X(1,IRECT(1,L))
        Y1 = X(2,IRECT(1,L))
        Z1 = X(3,IRECT(1,L))
        X2 = X(1,IRECT(2,L))
        Y2 = X(2,IRECT(2,L))
        Z2 = X(3,IRECT(2,L))
        X3 = X(1,IRECT(3,L))
        Y3 = X(2,IRECT(3,L))
        Z3 = X(3,IRECT(3,L))
        X4 = X(1,IRECT(4,L))
        Y4 = X(2,IRECT(4,L))
        Z4 = X(3,IRECT(4,L))
C
        XC = X1 * H(1) + X2 * H(2) + X3 * H(3) + X4 * H(4)  
        YC = Y1 * H(1) + Y2 * H(2) + Y3 * H(3) + Y4 * H(4)  
        ZC = Z1 * H(1) + Z2 * H(2) + Z3 * H(3) + Z4 * H(4) 
C
        XC0=X0-XC
        YC0=Y0-YC
        ZC0=Z0-ZC
C
        AA = XC0*XC0 + YC0*YC0 + ZC0*ZC0
        INS = IN(I) + AA * MS(I)
        STF = STIFR(I) + AA * STIFN(I)
C
        FXI=A(1,I)
        FYI=A(2,I)
        FZI=A(3,I)
C
        MXI = AR(1,I) + YC0 * FZI - ZC0 * FYI
        MYI = AR(2,I) + ZC0 * FXI - XC0 * FZI
        MZI = AR(3,I) + XC0 * FYI - YC0 * FXI
C
        W = WEIGHT(I)
        AI=AA * MS(I) * W
        IF (ANIM_N(12)+OUTP_N(3)+H3D_DATA%N_SCAL_DINER >0) THEN
          DO JJ=1,4
            J=IRECT(JJ,L)
            ADI(J)=ADI(J)+AI*H(JJ)
          END DO
        END IF
        DO JJ=1,4
         J=IRECT(JJ,L)
         IF (MSEGTYP2(L)==1) THEN
           AR(1,J)=AR(1,J)+MXI*H(JJ)*W
           AR(2,J)=AR(2,J)+MYI*H(JJ)*W
           AR(3,J)=AR(3,J)+MZI*H(JJ)*W
           IN(J)=IN(J)+INS*H2(JJ)*W
           STIFR(J)=STIFR(J)+ABS(STF*H(JJ)*W)
         END IF
        ENDDO
        STIFR(I)=EM20
        IF(IDEL2/=0.AND.IN(I)/=0.)SINER(II)=IN(I)
        IN(I)=ZERO
        STIFN(I)=EM20
        IF(IDEL2/=0.AND.MS(I)/=0.)SMASS(II)=MS(I)
        MS(I)=ZERO
        A(1,I)=ZERO
        A(2,I)=ZERO
        A(3,I)=ZERO
       ENDIF
C
      ENDDO
      ENDIF
C
C
      IF(ANIM_N(12)+OUTP_N(3)+H3D_DATA%N_SCAL_DINER >0) THEN
#include "vectorize.inc"
        DO II=1,NMN
          J=MSR(II)
          ADI(J) = ADI(J)/MAX(EM20,NMAS(II))
        ENDDO
      ENDIF
C
      RETURN
      END
