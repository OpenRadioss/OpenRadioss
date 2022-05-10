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
Chd|  RBE2T1                        source/constraints/general/rbe2/rbe2f.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        PRERBE2                       source/constraints/general/rbe2/rbe2f.F
Chd|        RBE2F                         source/constraints/general/rbe2/rbe2f.F
Chd|        RBE2FL                        source/constraints/general/rbe2/rbe2f.F
Chd|        RBE2_S                        source/constraints/general/rbe2/rbe2f.F
Chd|        SPMD_EXCH_RBE2_PON            source/mpi/kinematic_conditions/spmd_exch_rbe2_pon.F
Chd|        SPMD_MAX_I                    source/mpi/implicit/imp_spmd.F
Chd|====================================================================
      SUBROUTINE RBE2T1(IRBE2 ,LRBE2 ,X      ,A      ,AR     ,
     1                  MS    ,IN    ,SKEW   ,WEIGHT ,IAD_RBE2,
     2                  FR_RBE2M,NMRBE2,STIFN ,STIFR  ,R2SIZE)
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE2(NRBE2L,*),LRBE2(*),WEIGHT(*),IAD_RBE2(*),
     .        FR_RBE2M(*) ,NMRBE2,R2SIZE
C     REAL ou REAL*8
      my_real
     .   STIFN(*) ,STIFR(*),X(3,*), A(3,*), AR(3,*),
     .   MS(*), IN(*), SKEW(LSKEW,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, JT(3,NRBE2),JR(3,NRBE2),IERR,IAD,
     .        NS,NML,ICOM,ISK,M,K,ID,NSN,MID,IROT,NHI,IRAD
C     REAL ou REAL*8
      DOUBLE PRECISION
     .   FRBE2M6(3,6,NMRBE2),MRBE2M6(3,6,NMRBE2),
     .   STRBE2M6(6,NMRBE2),SRRBE2M6(6,NMRBE2)
C======================================================================|
      CALL PRERBE2(IRBE2 ,JT  ,JR   )
      ICOM = IAD_RBE2(NSPMD+1)-IAD_RBE2(1)
      IF (NSPMD.GT.1)CALL SPMD_MAX_I(ICOM)
      DO NHI=0,NHRBE2
       DO N=1,NMRBE2
        DO J=1,3
        DO K=1,6
         FRBE2M6(J,K,N) = ZERO
         MRBE2M6(J,K,N) = ZERO
        END DO
        END DO
        DO K=1,6
         STRBE2M6(K,N) = ZERO
         SRRBE2M6(K,N) = ZERO
        END DO
       END DO
c       CALL RBE2_POFF(IRBE2 ,A     ,AR    ,MS    ,IN    ,
c     1                STIFN ,STIFR ,WEIGHT,JR    ,NHI   )
       DO N=1,NRBE2
        IF (IRBE2(9,N).NE.NHI) CYCLE
	IAD = IRBE2(1,N)
        NSN = IRBE2(5,N)
        M   = IRBE2(3,N)
        ISK = IRBE2(7,N)
	MID = IABS(IRBE2(6,N))
	IRAD = IRBE2(11,N)
c	print *,'iad,m,mid,ih=',iad,m,IRBE2(6,N),IRBE2(9,N)
	IF (ISK.GT.1) THEN
          CALL RBE2FL(NSN   ,LRBE2(IAD+1),X     ,A     ,AR    ,
     1                MS    ,IN    ,WEIGHT,JT(1,N),JR(1,N),
     2               FRBE2M6(1,1,MID),MRBE2M6(1,1,MID),STIFN ,STIFR,
     3               STRBE2M6(1,MID),SRRBE2M6(1,MID),M  ,SKEW(1,ISK),
     4               IRAD   )
        ELSE
          CALL RBE2F(NSN   ,LRBE2(IAD+1),X      ,A     ,AR    ,
     1               MS    ,IN    ,WEIGHT,JT(1,N),JR(1,N),
     2               FRBE2M6(1,1,MID),MRBE2M6(1,1,MID),STIFN ,STIFR,
     3               STRBE2M6(1,MID),SRRBE2M6(1,MID),M  ,IRAD   )
	END IF
       END DO
C-----------------
        IF (ICOM.GT.0) THEN
            CALL SPMD_EXCH_RBE2_PON(
     .       FRBE2M6 ,MRBE2M6 ,STRBE2M6 ,SRRBE2M6 ,IAD_RBE2,
     .       FR_RBE2M,IAD_RBE2(NSPMD+1),R2SIZE)
        ENDIF
C
C Routine assemblage parith/ON
C
        CALL RBE2_S(IRBE2 ,A      ,AR     ,MS    ,IN    ,
     1              STIFN ,STIFR  ,WEIGHT ,FRBE2M6,MRBE2M6,
     2              STRBE2M6,SRRBE2M6,JR  ,NMRBE2 ,NHI  )
C
      END DO ! NHI=1,NHRBE2
C---
      RETURN
      END
Chd|====================================================================
Chd|  RBE2F                         source/constraints/general/rbe2/rbe2f.F
Chd|-- called by -----------
Chd|        RBE2T1                        source/constraints/general/rbe2/rbe2f.F
Chd|-- calls ---------------
Chd|        FOAT_TO_6_FLOAT               source/system/parit.F         
Chd|====================================================================
      SUBROUTINE RBE2F(NSL   ,ISL   ,X     ,A     ,AR    ,
     1                 MS    ,IN    ,WEIGHT,JT    ,JR    ,
     2                 F6    ,M6    ,STIFN ,STIFR ,STIF6 ,
     3                 STIR6 ,M     ,IRAD  )
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSL,ISL(*),WEIGHT(*),JT(3),JR(3),M,IRAD
C     REAL ou REAL*8
      my_real
     .   X(3,*), A(3,*), AR(3,*), MS(*), IN(*) ,STIFN(*) ,STIFR(*)
      DOUBLE PRECISION
     .   F6(3,6), M6(3,6),STIF6(6), STIR6(6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, NS ,JTW(3),JRW(3),K,IJT,IJR
C     REAL ou REAL*8
      my_real
     .     RX, RY, RZ,AS(3,NSL),STIS(NSL),DD,FX,FY,FZ
      DOUBLE PRECISION
     .   AS6(6,3,NSL),STIS6(6,NSL)
C======================================================================|
      IF ((JT(1)+JT(2)+JT(3)).GT.0) THEN
       IJT=1
      ELSE
       IJT=0
      ENDIF
      IF ((JR(1)+JR(2)+JR(3)).GT.0) THEN
       IJR=1
      ELSE
       IJR=0
      ENDIF
      DO I=1,NSL
        NS  = ISL(I)
        DO J=1,3
         JTW(J) = JT(J)*WEIGHT(NS)
         AS(J,I) = A(J,NS)*JTW(J)
        ENDDO
        STIS(I) = STIFN(NS)*IJT*WEIGHT(NS)
      ENDDO
      CALL FOAT_TO_6_FLOAT(1  ,NSL*3  ,AS ,AS6 )
      CALL FOAT_TO_6_FLOAT(1  ,NSL    ,STIS ,STIS6 )
c---    summ slave forces pon
      DO I=1,NSL
        DO K=1,6
         F6(1,K) = F6(1,K) + AS6(K,1,I)
         F6(2,K) = F6(2,K) + AS6(K,2,I)
         F6(3,K) = F6(3,K) + AS6(K,3,I)
         STIF6(K) = STIF6(K) + STIS6(K,I)
        ENDDO
      ENDDO
C-----------Nastran's formulation----
      IF (IRAD.EQ.0) THEN
       DO I=1,NSL
        NS  = ISL(I)
        DO J=1,3
         JRW(J) = JR(J)*WEIGHT(NS)
         JTW(J) = JT(J)*WEIGHT(NS)
        ENDDO
          RX = X(1,NS) - X(1,M)
          RY = X(2,NS) - X(2,M)
          RZ = X(3,NS) - X(3,M)
          FX = A(1,NS) *JTW(1)
          FY = A(2,NS) *JTW(2)
          FZ = A(3,NS) *JTW(3)
          AS(1,I) = AR(1,NS)*JRW(1)+ RY*FZ-RZ*FY
          AS(2,I) = AR(2,NS)*JRW(2)+ RZ*FX-RX*FZ
          AS(3,I) = AR(3,NS)*JRW(3)+ RX*FY-RY*FX
          DD = RX*RX+RY*RY+RZ*RZ
          STIS(I) = (STIFR(NS)*IJR+STIFN(NS)*DD*IJT)*WEIGHT(NS)
       ENDDO
       CALL FOAT_TO_6_FLOAT(1  ,NSL*3  ,AS ,AS6 )
       CALL FOAT_TO_6_FLOAT(1  ,NSL    ,STIS ,STIS6 )
c---    summ slave moments pon
       DO I=1,NSL
        DO K=1,6
         M6(1,K) = M6(1,K)+AS6(K,1,I)
         M6(2,K) = M6(2,K)+AS6(K,2,I)
         M6(3,K) = M6(3,K)+AS6(K,3,I)
         STIR6(K) = STIR6(K) + STIS6(K,I)
        ENDDO
       ENDDO
      ELSEIF ((JR(1)+JR(2)+JR(3)).GT.0) THEN
       DO I=1,NSL
        NS  = ISL(I)
        DO J=1,3
         JRW(J) = JR(J)*WEIGHT(NS)
        ENDDO
          RX = X(1,NS) - X(1,M)
          RY = X(2,NS) - X(2,M)
          RZ = X(3,NS) - X(3,M)
          AS(1,I) = (AR(1,NS)+(RY*A(3,NS)-RZ*A(2,NS)))*JRW(1)
          AS(2,I) = (AR(2,NS)+(RZ*A(1,NS)-RX*A(3,NS)))*JRW(2)
          AS(3,I) = (AR(3,NS)+(RX*A(2,NS)-RY*A(1,NS)))*JRW(3)
          DD = RX*RX+RY*RY+RZ*RZ
          STIS(I) = (STIFR(NS)*IJR+STIFN(NS)*DD*IJT)*WEIGHT(NS)
       ENDDO
       CALL FOAT_TO_6_FLOAT(1  ,NSL*3  ,AS ,AS6 )
       CALL FOAT_TO_6_FLOAT(1  ,NSL    ,STIS ,STIS6 )
c---    summ slave moments pon
       DO I=1,NSL
        DO K=1,6
         M6(1,K) = M6(1,K)+AS6(K,1,I)
         M6(2,K) = M6(2,K)+AS6(K,2,I)
         M6(3,K) = M6(3,K)+AS6(K,3,I)
         STIR6(K) = STIR6(K) + STIS6(K,I)
        ENDDO
       ENDDO
      END IF
C---  reset of slave nodes forces is necessary w/AMS
      IF(IJT/=0)THEN
       DO I=1,NSL
        NS  = ISL(I)
        DO J=1,3
         IF(JT(J)/=0)A(J,NS)=ZERO
        ENDDO
C---  partial depending dof will add more mass w/ /DT/NODA w.r.t. RBODY
        IF ((JT(1)+JT(2)+JT(3))==3)STIFN(NS)=EM20
       ENDDO
      END IF
      IF(IJR/=0)THEN
       DO I=1,NSL
        NS  = ISL(I)
        DO J=1,3
         IF(JR(J)/=0)AR(J,NS)=ZERO
        ENDDO
        IF ((JR(1)+JR(2)+JR(3))==3) STIFR(NS)=EM20
       ENDDO
      END IF
C---
      RETURN
      END
Chd|====================================================================
Chd|  RBE2FL                        source/constraints/general/rbe2/rbe2f.F
Chd|-- called by -----------
Chd|        RBE2T1                        source/constraints/general/rbe2/rbe2f.F
Chd|-- calls ---------------
Chd|        CDI_BCN                       source/constraints/general/rbe2/rbe2_imp0.F
Chd|        CDI_BCN1                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|        FOAT_TO_6_FLOAT               source/system/parit.F         
Chd|        RBE2FLSN                      source/constraints/general/rbe2/rbe2f.F
Chd|====================================================================
      SUBROUTINE RBE2FL(NSL   ,ISL   ,X     ,A     ,AR    ,
     1                  MS    ,IN    ,WEIGHT,JT    ,JR    ,
     2                  F6    ,M6    ,STIFN ,STIFR ,STIF6 ,
     3                  STIR6 ,M     ,SKEW  ,IRAD  )
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSL,ISL(*),WEIGHT(*),JT(3),JR(3),M,IRAD
C     REAL ou REAL*8
      my_real
     .   X(3,*), A(3,*), AR(3,*), MS(*),IN(*),SKEW(*),STIFN(*),STIFR(*)
      DOUBLE PRECISION
     .   F6(3,6), M6(3,6),STIF6(6), STIR6(6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NS ,K,IC,JT1(3),JR1(3),IJT,IJR,JJ
C     REAL ou REAL*8
      my_real
     .     RX, RY, RZ,AS(3,NSL),AAR(3),LARM(3),LAS(3,NSL),
     .     STIS(NSL),DD,CDT(9),CDR(9),CDTR(9),AA
      DOUBLE PRECISION
     .   AS6(6,3,NSL),STIS6(6,NSL)
C======================================================================|
       IC = JT(1)*100+JT(2)*10+JT(3)
       CALL CDI_BCN(IC   ,SKEW  ,JT   ,CDT  ,JT1  )
      IF ((JT(1)+JT(2)+JT(3)).GT.0) THEN
       IJT=1
      ELSE
       IJT=0
      ENDIF
      IF ((JR(1)+JR(2)+JR(3)).GT.0) THEN
       IJR=1
      ELSE
       IJR=0
      ENDIF
      DO I=1,NSL
        NS  = ISL(I)
        RX = A(1,NS)*WEIGHT(NS)
        RY = A(2,NS)*WEIGHT(NS)
        RZ = A(3,NS)*WEIGHT(NS)
        AS(1,I) = CDT(1)*RX+CDT(2)*RY+CDT(3)*RZ
        AS(2,I) = CDT(4)*RX+CDT(5)*RY+CDT(6)*RZ
        AS(3,I) = CDT(7)*RX+CDT(8)*RY+CDT(9)*RZ
        LAS(1,I) = RX
        LAS(2,I) = RY
        LAS(3,I) = RZ
        STIS(I) = STIFN(NS)*IJT*WEIGHT(NS)
      ENDDO
      CALL FOAT_TO_6_FLOAT(1  ,NSL*3  ,AS ,AS6 )
      CALL FOAT_TO_6_FLOAT(1  ,NSL    ,STIS ,STIS6 )
c---    summ slave forces pon
      DO I=1,NSL
        DO K=1,6
         F6(1,K) = F6(1,K) + AS6(K,1,I)
         F6(2,K) = F6(2,K) + AS6(K,2,I)
         F6(3,K) = F6(3,K) + AS6(K,3,I)
         STIF6(K) = STIF6(K) + STIS6(K,I)
        ENDDO
      ENDDO
C---    NS components
      IF (IC.GT.0.AND.IC.LT.111) THEN
       CALL RBE2FLSN(NSL   ,ISL  ,A     ,WEIGHT ,IC     ,
     1              SKEW  )
      END IF
C---
      IF (IRAD.EQ.0.OR.(JR(1)+JR(2)+JR(3)).GT.0) THEN
       IC = JR(1)*100+JR(2)*10+JR(3)
       CALL CDI_BCN(IC   ,SKEW  ,JR   ,CDR  ,JR1 )
       DO I=1,NSL
        NS  = ISL(I)
        RX = X(1,NS) - X(1,M)
        RY = X(2,NS) - X(2,M)
        RZ = X(3,NS) - X(3,M)
        CALL CDI_BCN1(RX,RY,RZ,JT,JR,SKEW,CDTR,IRAD)
        DD = RX*RX+RY*RY+RZ*RZ
C
        AAR(1) = CDTR(1)*LAS(1,I)+CDTR(2)*LAS(2,I)+CDTR(3)*LAS(3,I)
        AAR(2) = CDTR(4)*LAS(1,I)+CDTR(5)*LAS(2,I)+CDTR(6)*LAS(3,I)
        AAR(3) = CDTR(7)*LAS(1,I)+CDTR(8)*LAS(2,I)+CDTR(9)*LAS(3,I)
        RX = AR(1,NS)*WEIGHT(NS)
        RY = AR(2,NS)*WEIGHT(NS)
        RZ = AR(3,NS)*WEIGHT(NS)
        AS(1,I)= AAR(1)+CDR(1)*RX+CDR(2)*RY+CDR(3)*RZ
        AS(2,I)= AAR(2)+CDR(4)*RX+CDR(5)*RY+CDR(6)*RZ
        AS(3,I)= AAR(3)+CDR(7)*RX+CDR(8)*RY+CDR(9)*RZ
        STIS(I) = (STIFR(NS)*IJR+STIFN(NS)*DD)*WEIGHT(NS)
       ENDDO
       CALL FOAT_TO_6_FLOAT(1  ,NSL*3  ,AS ,AS6 )
       CALL FOAT_TO_6_FLOAT(1  ,NSL    ,STIS ,STIS6 )
c---    summ slave moments pon
       DO I=1,NSL
        DO K=1,6
         M6(1,K) = M6(1,K)+AS6(K,1,I)
         M6(2,K) = M6(2,K)+AS6(K,2,I)
         M6(3,K) = M6(3,K)+AS6(K,3,I)
         STIR6(K) = STIR6(K) + STIS6(K,I)
        ENDDO
       ENDDO
       IF (IC.GT.0.AND.IC.LT.111) THEN
        CALL RBE2FLSN(NSL   ,ISL  ,AR    ,WEIGHT ,IC     ,
     1                SKEW  )
       END IF
      END IF
C---  reset of slave nodes forces is necessary w/AMS
      IF(IJT/=0)THEN
       DO I=1,NSL
        NS  = ISL(I)
        DO J=1,3
         IF(JT(J)/=0)THEN
           JJ=3*(J-1)
           AA=A(1,NS)*CDT(JJ+1)+A(2,NS)*CDT(JJ+2)+A(3,NS)*CDT(JJ+3)
           A(1,NS)=A(1,NS)-AA*CDT(JJ+1)
           A(2,NS)=A(2,NS)-AA*CDT(JJ+2)
           A(3,NS)=A(3,NS)-AA*CDT(JJ+3)
         END IF
        ENDDO
        IF ((JT(1)+JT(2)+JT(3))==3)STIFN(NS)=EM20
       ENDDO
      END IF
      IF(IJR/=0)THEN
       DO I=1,NSL
        NS  = ISL(I)
        DO J=1,3
         IF(JR(J)/=0)THEN
           JJ=3*(J-1)
           AA=AR(1,NS)*CDR(JJ+1)+AR(2,NS)*CDR(JJ+2)+AR(3,NS)*CDR(JJ+3)
           AR(1,NS)=AR(1,NS)-AA*CDR(JJ+1)
           AR(2,NS)=AR(2,NS)-AA*CDR(JJ+2)
           AR(3,NS)=AR(3,NS)-AA*CDR(JJ+3)
         END IF      
        ENDDO
        IF ((JR(1)+JR(2)+JR(3))==3) STIFR(NS)=EM20
       ENDDO
      END IF
C---
      RETURN
      END
Chd|====================================================================
Chd|  RBE2_POFF                     source/constraints/general/rbe2/rbe2f.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RBE2_POFF(IRBE2 ,A     ,AR    ,MS    ,IN    ,
     1                     STIFN ,STIFR ,WEIGHT,JR    ,IH    )
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE2(NRBE2L,*),WEIGHT(*),JR(3,*),IH
C     REAL ou REAL*8
      my_real
     .   A(3,*), AR(3,*), MS(*), IN(*) ,STIFN(*) ,STIFR(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, K, N, NS ,NML, IAD,JJ,IROT,M
C     REAL ou REAL*8
C======================================================================|
#include    "vectorize.inc"
      DO N=1,NRBE2
        IF (IRBE2(9,N).NE.IH) CYCLE
        M  = IRBE2(3,N)
	A(1,M) = A(1,M)*WEIGHT(M)
	A(2,M) = A(2,M)*WEIGHT(M)
	A(3,M) = A(3,M)*WEIGHT(M)
	STIFN(M) = STIFN(M)*WEIGHT(M)
	IROT = JR(1,N)+JR(2,N)+JR(3,N)
        IF (IROT.GT.0) THEN
	 AR(1,M) = AR(1,M)*WEIGHT(M)
	 AR(2,M) = AR(2,M)*WEIGHT(M)
	 AR(3,M) = AR(3,M)*WEIGHT(M)
	 STIFR(M) = STIFR(M)*WEIGHT(M)
        ENDIF
      ENDDO
C---
      RETURN
      END
Chd|====================================================================
Chd|  RBE2_S                        source/constraints/general/rbe2/rbe2f.F
Chd|-- called by -----------
Chd|        RBE2T1                        source/constraints/general/rbe2/rbe2f.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RBE2_S(IRBE2 ,A     ,AR    ,MS    ,IN    ,
     1                  STIFN ,STIFR ,WEIGHT,F6    ,M6    ,
     2                  ST6   ,SR6   ,JR    ,NMRBE2,IH    )
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE2(NRBE2L,*),WEIGHT(*),NMRBE2,JR(3,*),IH
C     REAL ou REAL*8
      my_real
     .   A(3,*), AR(3,*), MS(*), IN(*) ,STIFN(*) ,STIFR(*)
      DOUBLE PRECISION
     .   F6(3,6,*), M6(3,6,*) ,ST6(6,*)   ,SR6(6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, K, N, NS ,NML, IAD,JJ,M,MID,IROT,IRAD
C     REAL ou REAL*8
C======================================================================|
#include    "vectorize.inc"
      DO N=1,NRBE2
        IF (IH.NE.IRBE2(9,N)) CYCLE
        M  = IRBE2(3,N)
        MID = IRBE2(6,N)
        IRAD = IRBE2(11,N)
        IF (MID.LT.0) CYCLE
	IROT = JR(1,N)+JR(2,N)+JR(3,N)
         DO K=1,6
	  A(1,M) = A(1,M)+ F6(1,K,MID)
	  A(2,M) = A(2,M)+ F6(2,K,MID)
	  A(3,M) = A(3,M)+ F6(3,K,MID)
	  STIFN(M) = STIFN(M)+ST6(K,MID)
         ENDDO
         IF (IROT.GT.0.OR.IRAD.EQ.0) THEN
          DO K=1,6
	   AR(1,M) = AR(1,M)+ M6(1,K,MID)
	   AR(2,M) = AR(2,M)+ M6(2,K,MID)
	   AR(3,M) = AR(3,M)+ M6(3,K,MID)
	   STIFR(M) = STIFR(M)+SR6(K,MID)
          ENDDO
         ENDIF
      ENDDO
C---
      RETURN
      END
Chd|====================================================================
Chd|  PRERBE2                       source/constraints/general/rbe2/rbe2f.F
Chd|-- called by -----------
Chd|        RBE2COR                       source/constraints/general/rbody/rgbcor.F
Chd|        RBE2T1                        source/constraints/general/rbe2/rbe2f.F
Chd|        RBE2V                         source/constraints/general/rbe2/rbe2v.F
Chd|        RBE2_IMP0                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE2_IMPD                     source/constraints/general/rbe2/rbe2v.F
Chd|        RBE2_IMPI                     source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE2_IMPR1                    source/constraints/general/rbe2/rbe2_imp0.F
Chd|        SMS_DIAG_RBE2                 source/ams/sms_rbe2.F         
Chd|        SMS_RBE2_NODXI                source/ams/sms_rbe2.F         
Chd|        SMS_RBE_ACCL                  source/ams/sms_rbe2.F         
Chd|        SMS_RBE_CNDS                  source/ams/sms_rbe2.F         
Chd|        SMS_RBE_CORR                  source/ams/sms_rbe2.F         
Chd|        SMS_RBE_PREC                  source/ams/sms_rbe2.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PRERBE2(IRBE2 ,JT  ,JR   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE2(NRBE2L,*),JT(3,*)  ,JR(3,*)
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N,NML,IC,ICT,ICR,IROT
C======================================================================|
      DO N=1,NRBE2
	IC=IRBE2(4,N)
        ICT=IC/512
        ICR=(IC-512*(ICT))/64
	IF (IRODDL.EQ.0) ICR =0
	DO J =1,3
           JT(J,N)=0
           JR(J,N)=0
	ENDDO
        SELECT CASE (ICT)
          CASE(1)
           JT(3,N)=1
          CASE(2)
           JT(2,N)=1
          CASE(3)
           JT(2,N)=1
           JT(3,N)=1
          CASE(4)
           JT(1,N)=1
          CASE(5)
           JT(1,N)=1
           JT(3,N)=1
          CASE(6)
           JT(1,N)=1
           JT(2,N)=1
          CASE(7)
           JT(1,N)=1
           JT(2,N)=1
           JT(3,N)=1
       END SELECT
       SELECT CASE (ICR)
          CASE(1)
           JR(3,N)=1
          CASE(2)
           JR(2,N)=1
          CASE(3)
           JR(2,N)=1
           JR(3,N)=1
          CASE(4)
           JR(1,N)=1
          CASE(5)
           JR(1,N)=1
           JR(3,N)=1
          CASE(6)
           JR(1,N)=1
           JR(2,N)=1
          CASE(7)
           JR(1,N)=1
           JR(2,N)=1
           JR(3,N)=1
       END SELECT
      ENDDO
C---
      RETURN
      END
Chd|====================================================================
Chd|  RBE2_INIT                     source/constraints/general/rbe2/rbe2f.F
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RBE2_INIT(IRBE2 ,LRBE2,NMRBE2,FR_RBE2,FR_RBE2M,NFR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRBE2(NRBE2L,*),LRBE2(*),NMRBE2,FR_RBE2(*),FR_RBE2M(*),NFR
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, M,N,ITAG(NUMNOD),IAD,IH(NRBE2),NSL,NS,NIH
C======================================================================|
      NMRBE2 = 0
      IF (NRBE2.EQ.0) RETURN
      DO N=1,NUMNOD
	ITAG(N)=0
      ENDDO
C-----s'il y a hierarchy----
C      DO N=1,NRBE2
C	M=IRBE2(3,N)
C	ITAG(M)=N
C	IH(N)=0
C      ENDDO
C      DO N=1,NRBE2
C	IAD=IRBE2(1,N)
C	M=IRBE2(3,N)
C	NSL =IRBE2(5,N)
C	DO J=1,NSL
C	 NS= LRBE2(IAD+J)
C	 IF (ITAG(NS).GT.0) IH(ITAG(NS)) =M
C	ENDDO
C      ENDDO
C      DO N=1,NRBE2
C	M=IRBE2(3,N)
C	ITAG(M)=0
C      ENDDO
C
       DO N=1,NRBE2
	M=IRBE2(3,N)
	IF (ITAG(M).EQ.0) THEN
	 NMRBE2 =NMRBE2 +1
	 ITAG(M)= NMRBE2
	 IRBE2(6,N) = ITAG(M)
	 IH(NMRBE2) = IRBE2(9,N)
	ELSE
	 NIH = IH(ITAG(M))
	 IRBE2(6,N) = ITAG(M)
C---------to avoid the double sum on A,AR for master nodes in the same IH
	 IF (IRBE2(9,N).EQ.NIH) THEN
           IRBE2(6,N) = -ITAG(M)
C---------case the same master in the same IH, but also before
         ELSE
           IH(ITAG(M)) = IRBE2(9,N)
         END IF
	ENDIF
       ENDDO
C---
      DO N=1,NFR
	M=FR_RBE2(N)
        FR_RBE2M(N)=ITAG(M)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  RBE2FRF                       source/constraints/general/rbe2/rbe2f.F
Chd|-- called by -----------
Chd|        IMP3_A2B                      source/airbag/monv_imp0.F     
Chd|        UPD_FR                        source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        CDI_BCN                       source/constraints/general/rbe2/rbe2_imp0.F
Chd|        CDI_BCN1                      source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE2FLSNFR                    source/constraints/general/rbe2/rbe2f.F
Chd|====================================================================
      SUBROUTINE RBE2FRF(NS    ,M     ,A     ,AR    ,JT    ,
     1                   JR    ,X     ,ISK   ,SKEW0 ,IRAD  )
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NS   , M,JT(*),JR(*),ISK,IRAD
C     REAL ou REAL*8
      my_real
     .   A(3,*), AR(3,*), SKEW0(*),X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N,K,JT1(3),JR1(3),IC
C     REAL ou REAL*8
      my_real
     .       RX,RY,RZ,FX,FY,FZ, SKEW(LSKEW),CDT(9),CDR(9),CDTR(9),AAR(3)
C======================================================================|
	IF (ISK.GT.1) THEN
	 DO K=1,9
	  SKEW(K)=SKEW0(K)
	 ENDDO
	ELSE
	 DO K=1,LSKEW
	  SKEW(K)=ZERO
	 ENDDO
	  SKEW(1)=UN
	  SKEW(5)=UN
	  SKEW(9)=UN
        ENDIF
       IC = JT(1)*100+JT(2)*10+JT(3)
       CALL CDI_BCN(IC   ,SKEW  ,JT   ,CDT  ,JT1  )
        A(1,M) = A(1,M)+CDT(1)*A(1,NS)+CDT(2)*A(2,NS)+CDT(3)*A(3,NS)
        A(2,M) = A(2,M)+CDT(4)*A(1,NS)+CDT(5)*A(2,NS)+CDT(6)*A(3,NS)
        A(3,M) = A(3,M)+CDT(7)*A(1,NS)+CDT(8)*A(2,NS)+CDT(9)*A(3,NS)
C---    NS components
      IF (IC.GT.0.AND.IC.LT.111) THEN
       CALL RBE2FLSNFR(NS   ,A     ,IC     ,SKEW  )
      END IF
C---
      IF (IRAD.EQ.0.OR.(JR(1)+JR(2)+JR(3)).GT.0) THEN
       IC = JR(1)*100+JR(2)*10+JR(3)
       CALL CDI_BCN(IC   ,SKEW  ,JR   ,CDR  ,JR1 )
        RX = X(1,NS) - X(1,M)
        RY = X(2,NS) - X(2,M)
        RZ = X(3,NS) - X(3,M)
        CALL CDI_BCN1(RX,RY,RZ,JT,JR,SKEW,CDTR,IRAD)
C
        AAR(1) = CDTR(1)*A(1,NS)+CDTR(2)*A(2,NS)+CDTR(3)*A(3,NS)
        AAR(2) = CDTR(4)*A(1,NS)+CDTR(5)*A(2,NS)+CDTR(6)*A(3,NS)
        AAR(3) = CDTR(7)*A(1,NS)+CDTR(8)*A(2,NS)+CDTR(9)*A(3,NS)
        AR(1,M)= AR(1,M)+
     .           AAR(1)+CDR(1)*AR(1,NS)+CDR(2)*AR(2,NS)+CDR(3)*AR(3,NS)
        AR(2,M)= AR(2,M)+
     .           AAR(2)+CDR(4)*AR(1,NS)+CDR(5)*AR(2,NS)+CDR(6)*AR(3,NS)
        AR(3,M)= AR(3,M)+
     .           AAR(3)+CDR(7)*AR(1,NS)+CDR(8)*AR(2,NS)+CDR(9)*AR(3,NS)
       IF (IC.GT.0.AND.IC.LT.111) THEN
        CALL RBE2FLSNFR(NS   ,AR    ,IC     ,SKEW  )
       END IF
      END IF
C---
      RETURN
      END
Chd|====================================================================
Chd|  RBE2FLSN                      source/constraints/general/rbe2/rbe2f.F
Chd|-- called by -----------
Chd|        RBE2FL                        source/constraints/general/rbe2/rbe2f.F
Chd|-- calls ---------------
Chd|        DIR_RBE2                      source/constraints/general/rbe2/rbe2v.F
Chd|        L_DIR                         source/constraints/general/bcs/bc_imp0.F
Chd|====================================================================
      SUBROUTINE RBE2FLSN(NSL   ,ISL  ,A     ,WEIGHT,ICT    ,
     2                    SKEW  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSL   ,ISL(*)   ,ICT, WEIGHT(*)
      my_real
     .   SKEW(*),A(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,J1,L,NS
      my_real
     .   EJ(3),EJ1(3),S,EA,EB
C----------------100-------------------------
        SELECT CASE (ICT)
         CASE(100)
          EJ(1)=SKEW(1)
          EJ(2)=SKEW(2)
          EJ(3)=SKEW(3)
          CALL L_DIR(EJ,J)
	  J1=0
          CALL DIR_RBE2(J    ,J1    ,K     )
C----------------010-------------------------
         CASE(10)
          EJ(1)=SKEW(4)
          EJ(2)=SKEW(5)
          EJ(3)=SKEW(6)
          CALL L_DIR(EJ,J)
	  J1=0
          CALL DIR_RBE2(J    ,J1    ,K     )
C----------------001-------------------------
         CASE(1)
          EJ(1)=SKEW(7)
          EJ(2)=SKEW(8)
          EJ(3)=SKEW(9)
          CALL L_DIR(EJ,J)
	  J1=0
          CALL DIR_RBE2(J    ,J1    ,K     )
C----------------011-------------------------
         CASE(11)
          EJ(1)=SKEW(7)
          EJ(2)=SKEW(8)
          EJ(3)=SKEW(9)
          CALL L_DIR(EJ,J)
          EJ1(1)=SKEW(4)
          EJ1(2)=SKEW(5)
          EJ1(3)=SKEW(6)
          CALL L_DIR(EJ1,J1)
          IF (J1.EQ.J) THEN
           EJ1(J)=ZERO
           CALL L_DIR(EJ1,J1)
           EJ1(1)=SKEW(4)/SKEW(3+J1)
           EJ1(2)=SKEW(5)/SKEW(3+J1)
           EJ1(3)=SKEW(6)/SKEW(3+J1)
          ENDIF
          CALL DIR_RBE2(J    ,J1    ,K     )
	  S=UN/(UN-EJ(J1)*EJ1(J))
	  EA=S*(EJ(J1)*EJ1(K)-EJ(K))
	  EB=S*(EJ1(J)*EJ(K)-EJ1(K))
C----------------101-------------------------
         CASE(101)
          EJ(1)=SKEW(7)
          EJ(2)=SKEW(8)
          EJ(3)=SKEW(9)
          CALL L_DIR(EJ,J)
          EJ1(1)=SKEW(1)
          EJ1(2)=SKEW(2)
          EJ1(3)=SKEW(3)
          CALL L_DIR(EJ1,J1)
          IF (J1.EQ.J) THEN
           EJ1(J)=ZERO
           CALL L_DIR(EJ1,J1)
           EJ1(1)=SKEW(1)/SKEW(J1)
           EJ1(2)=SKEW(2)/SKEW(J1)
           EJ1(3)=SKEW(3)/SKEW(J1)
          ENDIF
          CALL DIR_RBE2(J    ,J1    ,K     )
	  S=UN/(UN-EJ(J1)*EJ1(J))
	  EA=S*(EJ(J1)*EJ1(K)-EJ(K))
	  EB=S*(EJ1(J)*EJ(K)-EJ1(K))
C----------------110-------------------------
         CASE(110)
          EJ(1)=SKEW(4)
          EJ(2)=SKEW(5)
          EJ(3)=SKEW(6)
          CALL L_DIR(EJ,J)
          EJ1(1)=SKEW(1)
          EJ1(2)=SKEW(2)
          EJ1(3)=SKEW(3)
          CALL L_DIR(EJ1,J1)
          IF (J1.EQ.J) THEN
           EJ1(J)=ZERO
           CALL L_DIR(EJ1,J1)
           EJ1(1)=SKEW(1)/SKEW(J1)
           EJ1(2)=SKEW(2)/SKEW(J1)
           EJ1(3)=SKEW(3)/SKEW(J1)
          ENDIF
          CALL DIR_RBE2(J    ,J1    ,K     )
	  S=UN/(UN-EJ(J1)*EJ1(J))
	  EA=S*(EJ(J1)*EJ1(K)-EJ(K))
	  EB=S*(EJ1(J)*EJ(K)-EJ1(K))
       END SELECT
C
       DO I=1,NSL
        NS = ISL(I)
	IF (WEIGHT(NS).EQ.0) CYCLE
C-------------------100---------------------
         IF (ICT .EQ. 100 ) THEN
          A(J1,NS) = A(J1,NS)-EJ(J1)*A(J,NS)
          A(K,NS) = A(K,NS)-EJ(K)*A(J,NS)
C-------------------010---------------------
         ELSEIF (ICT .EQ. 10) THEN
          A(J1,NS) = A(J1,NS)-EJ(J1)*A(J,NS)
          A(K,NS) = A(K,NS)-EJ(K)*A(J,NS)
C-------------------001---------------------
         ELSEIF (ICT .EQ. 1) THEN
          A(J1,NS) = A(J1,NS)-EJ(J1)*A(J,NS)
          A(K,NS) = A(K,NS)-EJ(K)*A(J,NS)
C-------------------011---------------------
         ELSEIF (ICT .EQ. 11) THEN
          A(K,NS)=A(K,NS)+EA*A(J,NS)+EB*A(J1,NS)
C-------------------101---------------------
         ELSEIF (ICT .EQ. 101) THEN
          A(K,NS)=A(K,NS)+EA*A(J,NS)+EB*A(J1,NS)
C-------------------110---------------------
         ELSEIF (ICT .EQ. 110 ) THEN
          A(K,NS)=A(K,NS)+EA*A(J,NS)+EB*A(J1,NS)
         ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  RBE2FLSNFR                    source/constraints/general/rbe2/rbe2f.F
Chd|-- called by -----------
Chd|        RBE2FRF                       source/constraints/general/rbe2/rbe2f.F
Chd|-- calls ---------------
Chd|        DIR_RBE2                      source/constraints/general/rbe2/rbe2v.F
Chd|        L_DIR                         source/constraints/general/bcs/bc_imp0.F
Chd|====================================================================
      SUBROUTINE RBE2FLSNFR(NS   ,A     ,ICT    ,SKEW  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NS   ,ICT
      my_real
     .   SKEW(*),A(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,J1,L
      my_real
     .   EJ(3),EJ1(3),S,EA,EB
C----------------100-------------------------
        SELECT CASE (ICT)
         CASE(100)
          EJ(1)=SKEW(1)
          EJ(2)=SKEW(2)
          EJ(3)=SKEW(3)
          CALL L_DIR(EJ,J)
	  J1=0
          CALL DIR_RBE2(J    ,J1    ,K     )
          A(J1,NS) = A(J1,NS)-EJ(J1)*A(J,NS)
          A(K,NS) = A(K,NS)-EJ(K)*A(J,NS)
C----------------010-------------------------
         CASE(10)
          EJ(1)=SKEW(4)
          EJ(2)=SKEW(5)
          EJ(3)=SKEW(6)
          CALL L_DIR(EJ,J)
	  J1=0
          CALL DIR_RBE2(J    ,J1    ,K     )
          A(J1,NS) = A(J1,NS)-EJ(J1)*A(J,NS)
          A(K,NS) = A(K,NS)-EJ(K)*A(J,NS)
C----------------001-------------------------
         CASE(1)
          EJ(1)=SKEW(7)
          EJ(2)=SKEW(8)
          EJ(3)=SKEW(9)
          CALL L_DIR(EJ,J)
	  J1=0
          CALL DIR_RBE2(J    ,J1    ,K     )
          A(J1,NS) = A(J1,NS)-EJ(J1)*A(J,NS)
          A(K,NS) = A(K,NS)-EJ(K)*A(J,NS)
C----------------011-------------------------
         CASE(11)
          EJ(1)=SKEW(7)
          EJ(2)=SKEW(8)
          EJ(3)=SKEW(9)
          CALL L_DIR(EJ,J)
          EJ1(1)=SKEW(4)
          EJ1(2)=SKEW(5)
          EJ1(3)=SKEW(6)
          CALL L_DIR(EJ1,J1)
          IF (J1.EQ.J) THEN
           EJ1(J)=ZERO
           CALL L_DIR(EJ1,J1)
           EJ1(1)=SKEW(4)/SKEW(3+J1)
           EJ1(2)=SKEW(5)/SKEW(3+J1)
           EJ1(3)=SKEW(6)/SKEW(3+J1)
          ENDIF
          CALL DIR_RBE2(J    ,J1    ,K     )
	  S=UN/(UN-EJ(J1)*EJ1(J))
	  EA=S*(EJ(J1)*EJ1(K)-EJ(K))
	  EB=S*(EJ1(J)*EJ(K)-EJ1(K))
          A(K,NS)=A(K,NS)+EA*A(J,NS)+EB*A(J1,NS)
C----------------101-------------------------
         CASE(101)
          EJ(1)=SKEW(7)
          EJ(2)=SKEW(8)
          EJ(3)=SKEW(9)
          CALL L_DIR(EJ,J)
          EJ1(1)=SKEW(1)
          EJ1(2)=SKEW(2)
          EJ1(3)=SKEW(3)
          CALL L_DIR(EJ1,J1)
          IF (J1.EQ.J) THEN
           EJ1(J)=ZERO
           CALL L_DIR(EJ1,J1)
           EJ1(1)=SKEW(1)/SKEW(J1)
           EJ1(2)=SKEW(2)/SKEW(J1)
           EJ1(3)=SKEW(3)/SKEW(J1)
          ENDIF
          CALL DIR_RBE2(J    ,J1    ,K     )
	  S=UN/(UN-EJ(J1)*EJ1(J))
	  EA=S*(EJ(J1)*EJ1(K)-EJ(K))
	  EB=S*(EJ1(J)*EJ(K)-EJ1(K))
          A(K,NS)=A(K,NS)+EA*A(J,NS)+EB*A(J1,NS)
C----------------110-------------------------
         CASE(110)
          EJ(1)=SKEW(4)
          EJ(2)=SKEW(5)
          EJ(3)=SKEW(6)
          CALL L_DIR(EJ,J)
          EJ1(1)=SKEW(1)
          EJ1(2)=SKEW(2)
          EJ1(3)=SKEW(3)
          CALL L_DIR(EJ1,J1)
          IF (J1.EQ.J) THEN
           EJ1(J)=ZERO
           CALL L_DIR(EJ1,J1)
           EJ1(1)=SKEW(1)/SKEW(J1)
           EJ1(2)=SKEW(2)/SKEW(J1)
           EJ1(3)=SKEW(3)/SKEW(J1)
          ENDIF
          CALL DIR_RBE2(J    ,J1    ,K     )
	  S=UN/(UN-EJ(J1)*EJ1(J))
	  EA=S*(EJ(J1)*EJ1(K)-EJ(K))
	  EB=S*(EJ1(J)*EJ(K)-EJ1(K))
          A(K,NS)=A(K,NS)+EA*A(J,NS)+EB*A(J1,NS)
       END SELECT
C
      RETURN
      END
Chd|====================================================================
Chd|  PRERBE2FR                     source/constraints/general/rbe2/rbe2f.F
Chd|-- called by -----------
Chd|        DIAG_INT                      source/mpi/implicit/imp_fri.F 
Chd|        FR_U2DD                       source/mpi/implicit/imp_fri.F 
Chd|        IMP3_A2B                      source/airbag/monv_imp0.F     
Chd|        IMP3_U2X                      source/airbag/monv_imp0.F     
Chd|        UPDK_MV                       source/airbag/monv_imp0.F     
Chd|        UPD_FR                        source/mpi/implicit/imp_fri.F 
Chd|        UPD_KML                       source/mpi/implicit/imp_fri.F 
Chd|        UPD_KSL                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PRERBE2FR(IC    ,JT  ,JR   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IC,JT(3)  ,JR(3)
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NML,ICT,ICR
C======================================================================|
C	IC=IRBE2(4,N)
        ICT=IC/512
        ICR=(IC-512*(ICT))/64
	DO J =1,3
           JT(J)=0
           JR(J)=0
	ENDDO
        SELECT CASE (ICT)
          CASE(1)
           JT(3)=1
          CASE(2)
           JT(2)=1
          CASE(3)
           JT(2)=1
           JT(3)=1
          CASE(4)
           JT(1)=1
          CASE(5)
           JT(1)=1
           JT(3)=1
          CASE(6)
           JT(1)=1
           JT(2)=1
          CASE(7)
           JT(1)=1
           JT(2)=1
           JT(3)=1
       END SELECT
       SELECT CASE (ICR)
          CASE(1)
           JR(3)=1
          CASE(2)
           JR(2)=1
          CASE(3)
           JR(2)=1
           JR(3)=1
          CASE(4)
           JR(1)=1
          CASE(5)
           JR(1)=1
           JR(3)=1
          CASE(6)
           JR(1)=1
           JR(2)=1
          CASE(7)
           JR(1)=1
           JR(2)=1
           JR(3)=1
       END SELECT
C---
      RETURN
      END
