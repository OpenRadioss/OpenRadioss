Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  BCSCYC                        source/constraints/general/bcs/bcscyc.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ACC_CYCON                     source/constraints/general/bcs/bcscyc.F
Chd|        CHKV0_CY                      source/constraints/general/bcs/bcscyc.F
Chd|====================================================================
      SUBROUTINE BCSCYC(IBCSCYC,LBCSCYC,SKEW,X,V,A,ITAB)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBCSCYC(4,*),LBCSCYC(2,*),ITAB(*)
      my_real
     .     X(3,*),V(3,*),A(3,*),SKEW(LSKEW,*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J ,ISK,IAD,NN,N1,N2,ID
      my_real
     .   AX, AY, AZ
C-----------------------------------------------
C---Check Ncycle=0 V(,N1)=V(,N2) in c.s.
      IF (NCYCLE==0) THEN
        DO I=1,NBCSCYC
         IAD = IBCSCYC(1,I)+1
         ISK = IBCSCYC(2,I)
         NN  = IBCSCYC(3,I)
         ID  = IBCSCYC(4,I)
         CALL CHKV0_CY(NN,LBCSCYC(1,IAD),ISK,SKEW,X  ,V,ID,ITAB)
        END DO 
      END IF 
C----case AMS done before in sms_pcg      
      IF(IDTMINS == 2.OR.IDTMINS_INT /= 0) RETURN
      DO I=1,NBCSCYC
        IAD = IBCSCYC(1,I)+1
        ISK = IBCSCYC(2,I)
        NN  = IBCSCYC(3,I)
        ID  = IBCSCYC(4,I)
        CALL ACC_CYCON(NN,LBCSCYC(1,IAD),ISK,SKEW,X  ,A)
      END DO 
C
      RETURN
      END
Chd|====================================================================
Chd|  ACC_CYCON                     source/constraints/general/bcs/bcscyc.F
Chd|-- called by -----------
Chd|        BCSCYC                        source/constraints/general/bcs/bcscyc.F
Chd|        SMS_BCSCYC                    source/ams/sms_bcscyc.F       
Chd|-- calls ---------------
Chd|        V_C2CYLIN                     source/constraints/general/bcs/bcscyc.F
Chd|        V_CYC2C                       source/constraints/general/bcs/bcscyc.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ACC_CYCON(NBCY_N,IXCYCL,ISK,SKEW,X  ,A)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NBCY_N,IXCYCL(2,*),ISK
      my_real
     .     X(3,*),A(3,*),SKEW(LSKEW,*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J ,N1(NBCY_N),N2(NBCY_N)
C
      my_real
     .   CCOS1(NBCY_N),CSIN1(NBCY_N),CCOS2(NBCY_N),CSIN2(NBCY_N),
     .   A_C1(3,NBCY_N),A_C2(3,NBCY_N),A_C(3,NBCY_N)
C========================================================================|
C---- for each section nodes :
C-------A (SKEW) -> A'(x',y',z')->A"(r,dthe,z')
C--- mean(A")-> A'(x',y',z')->A (SKEW^t)
      DO I=1,NBCY_N
        N1(I) = IXCYCL(1,I)
        N2(I) = IXCYCL(2,I)
      ENDDO
      CALL V_C2CYLIN(NBCY_N,N1,X,A,SKEW(1,ISK),SKEW(10,ISK),CCOS1,CSIN1,A_C1)
      CALL V_C2CYLIN(NBCY_N,N2,X,A,SKEW(1,ISK),SKEW(10,ISK),CCOS2,CSIN2,A_C2)
      A_C(1:3,1:NBCY_N) = HALF*(A_C1(1:3,1:NBCY_N)+A_C2(1:3,1:NBCY_N))
      CALL V_CYC2C(NBCY_N,SKEW(1,ISK),CCOS1,CSIN1,A_C,A_C1)
      CALL V_CYC2C(NBCY_N,SKEW(1,ISK),CCOS2,CSIN2,A_C,A_C2)
      DO I=1,NBCY_N
        A(1:3,N1(I)) = A_C1(1:3,I)
        A(1:3,N2(I)) = A_C2(1:3,I)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  V_C2CYLIN                     source/constraints/general/bcs/bcscyc.F
Chd|-- called by -----------
Chd|        ACC_CYCON                     source/constraints/general/bcs/bcscyc.F
Chd|        CHKV0_CY                      source/constraints/general/bcs/bcscyc.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE V_C2CYLIN(NBCY,IX,X,V,SKEW,XYZ0,CCOS,CSIN,V_C)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NBCY,IX(*)
      my_real
     .     X(3,*),SKEW(9),XYZ0(3),V(3,*),
     .     V_C(3,NBCY),CCOS(NBCY),CSIN(NBCY)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J 
C
      my_real
     .     XX,YY,ZZ,XL,YL,ZL,R2,CR(NBCY),CZ(NBCY)
C========================================================================|
C----- compute v to v_cylin
      DO I=1,NBCY
        XX = X(1,IX(I))-XYZ0(1)
        YY = X(2,IX(I))-XYZ0(2)
        ZZ = X(3,IX(I))-XYZ0(3)
        XL = XX*SKEW(1)+YY*SKEW(2)+ZZ*SKEW(3)
        YL = XX*SKEW(4)+YY*SKEW(5)+ZZ*SKEW(6)
        ZL = XX*SKEW(7)+YY*SKEW(8)+ZZ*SKEW(9)
        R2 = XL*XL+YL*YL
        CR(I) = SQRT(R2)
        CCOS(I) = XL/CR(I)
        CSIN(I) = YL/CR(I)
        CZ(I) = ZL
      ENDDO
      DO I=1,NBCY
        XX = V(1,IX(I))
        YY = V(2,IX(I))
        ZZ = V(3,IX(I))
        XL = XX*SKEW(1)+YY*SKEW(2)+ZZ*SKEW(3)
        YL = XX*SKEW(4)+YY*SKEW(5)+ZZ*SKEW(6)
        ZL = XX*SKEW(7)+YY*SKEW(8)+ZZ*SKEW(9)
        V_C(1,I) = XL*CCOS(I)+YL*CSIN(I)
        V_C(2,I) = YL*CCOS(I)-XL*CSIN(I)
        V_C(3,I) = ZL
      ENDDO
C
      RETURN
      END SUBROUTINE V_C2CYLIN
Chd|====================================================================
Chd|  V_CYC2C                       source/constraints/general/bcs/bcscyc.F
Chd|-- called by -----------
Chd|        ACC_CYCON                     source/constraints/general/bcs/bcscyc.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE V_CYC2C(NBCY,SKEW,CCOS,CSIN,V_C,VC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NBCY
      my_real
     .     SKEW(9),V_C(3,NBCY),CCOS(NBCY),CSIN(NBCY),VC(3,NBCY)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J 
C
      my_real
     .     XX,YY,ZZ,XL,YL,ZL
C========================================================================|
C----- compute v_cylin to v 
      DO I=1,NBCY
        XX = V_C(1,I)*CCOS(I)-V_C(2,I)*CSIN(I)
        YY = V_C(1,I)*CSIN(I)+V_C(2,I)*CCOS(I)
        ZZ = V_C(3,I)
        XL = XX*SKEW(1)+YY*SKEW(4)+ZZ*SKEW(7)
        YL = XX*SKEW(2)+YY*SKEW(5)+ZZ*SKEW(8)
        ZL = XX*SKEW(3)+YY*SKEW(6)+ZZ*SKEW(9)
        VC(1,I) = XL
        VC(2,I) = YL
        VC(3,I) = ZL
      ENDDO
C
      RETURN
      END SUBROUTINE V_CYC2C
Chd|====================================================================
Chd|  CHKV0_CY                      source/constraints/general/bcs/bcscyc.F
Chd|-- called by -----------
Chd|        BCSCYC                        source/constraints/general/bcs/bcscyc.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        V_C2CYLIN                     source/constraints/general/bcs/bcscyc.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE CHKV0_CY(NBCY_N,IXCYCL,ISK,SKEW,X  ,V ,ID ,ITAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NBCY_N,IXCYCL(2,*),ISK,ID,ITAB(*)
      my_real
     .     X(3,*),V(3,*),SKEW(LSKEW,*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J ,N1(NBCY_N),N2(NBCY_N) ,II1,II2
C
      my_real
     .    CCOS(NBCY_N),CSIN(NBCY_N),V_C1(3,NBCY_N),V_C2(3,NBCY_N),
     .    DV,DVMAX,DVMEAN
C========================================================================|
C-----for each cut-section nodes, compute cylindrical co-ordinates and vel
      DO I=1,NBCY_N
        N1(I) = IXCYCL(1,I)
        N2(I) = IXCYCL(2,I)
      ENDDO
      CALL V_C2CYLIN(NBCY_N,N1,X,V,SKEW(1,ISK),SKEW(10,ISK),CCOS,CSIN,V_C1)
      CALL V_C2CYLIN(NBCY_N,N2,X,V,SKEW(1,ISK),SKEW(10,ISK),CCOS,CSIN,V_C2)
C--- check  
      DVMAX =ZERO  
      J =1      
      DVMEAN =ZERO
      DO I=1,NBCY_N
        DV = ABS(V_C1(2,I)-V_C2(2,I))
        IF (DV >DVMAX) THEN
         DVMAX = DV
         J = I
        END IF
        DVMEAN =DVMEAN + ABS(V_C1(2,I))+ABS(V_C2(2,I))  
      ENDDO
      IF (NBCY_N>0) DVMEAN =HALF*DVMEAN/NBCY_N  
      IF (DVMAX>ZEP05*DVMEAN.AND.DVMEAN>EM06) THEN
        II1 = ITAB(N1(J))
        II2 = ITAB(N2(J))
         CALL ANCMSG(MSGID=285,ANMODE=ANINFO,I1=ID,I2=II1,I3=II2)
      END IF
C
      RETURN
      END
