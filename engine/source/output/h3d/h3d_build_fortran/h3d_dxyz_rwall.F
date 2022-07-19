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
Chd|  H3D_DXYZ_RWALL                source/output/h3d/h3d_build_fortran/h3d_dxyz_rwall.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        SPMD_H3D_GETMSR               source/output/h3d/spmd/spmd_h3d_getmsr.F
Chd|====================================================================
      SUBROUTINE H3D_DXYZ_RWALL (NSTRF,RWBUF,NPRW  ,X     ,XMIN ,
     2                     YMIN ,ZMIN  ,XMAX ,YMAX  , ZMAX,
     3                      FR_SEC,FR_WALL,WEIGHT,ITAB,
     4                      XWL ,YWL , ZWL, 
     5  RWALL_V1, RWALL_V2, RWALL_V3, RWALL_V4, RWALL_V5, RWALL_V6 , RWALL_V7, 
     6  RWALL_V8, RWALL_V9, RWALL_V10)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSTRF(*),NPRW(*),ITAB(*)
      my_real
     .  RWBUF(NRWLP,*),X(3,*),XMIN ,YMIN ,ZMIN  ,XMAX ,YMAX, ZMAX
      my_real
     .  XWL(*), YWL(*), ZWL(*), RWALL_V1(*), RWALL_V2(*), RWALL_V3(*),
     .  RWALL_V4(*), RWALL_V5(*), RWALL_V6(*), RWALL_V7(*),
     .  RWALL_V8(*), RWALL_V9(*), RWALL_V10(*)
      INTEGER
     .   FR_SEC(NSPMD+1,*),FR_WALL(NSPMD+2,*),WEIGHT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J, I, K, K0, K1, N, NSEG, N1, N2, N3, N4,MSR, ITYP
      my_real
     .   XX1, YY1, ZZ1, XX2, YY2, ZZ2, XX3, YY3, ZZ3,
     .   XX4, YY4, ZZ4, D13, XXC, YYC, ZZC, AL4,
     .   PMAIN,LOC_PROC, V1, V2, V3, VV1, VV2,
     .   VV3, R, XN,YN,ZN,D,DX,DY,DZ, VV, XL

      my_real
     .   XSEC(3,3,NSECT)
      REAL R4,SBUF(3*NSECT)
CC-----------------------------------------------
      LOC_PROC=ISPMD+1
C
C
      K=1
      DO N=1,NRWALL
        N2=N +NRWALL
        N3=N2+NRWALL
        N4=N3+NRWALL
        MSR = NPRW(N3)
        IF (NSPMD == 1) THEN
          IF(MSR.EQ.0)THEN
            XWL(N)=RWBUF(4,N)
            YWL(N)=RWBUF(5,N)
            ZWL(N)=RWBUF(6,N)
          ELSE
C verifier que ce noeud est sur proc0 !
            XWL(N)=X(1,MSR)
            YWL(N)=X(2,MSR)
            ZWL(N)=X(3,MSR)
          ENDIF
        ELSE
          CALL SPMD_H3D_GETMSR(FR_WALL(1,N),X,MSR,XWL(N),YWL(N),ZWL(N),RWBUF(1,N))
        END IF
      ENDDO
C
      K=1
      DO N=1,NRWALL
        N2=N +NRWALL
        N3=N2+NRWALL
        N4=N3+NRWALL
        ITYP= NPRW(N4)

        IF(IABS(ITYP).EQ.1)THEN
C
          XN =RWBUF(1,N)
          YN =RWBUF(2,N)
          ZN =RWBUF(3,N)
          IF (ISPMD.EQ.0) THEN
            DX = XMAX - XMIN
            DY = YMAX - YMIN
            DZ = ZMAX - ZMIN
C
            R = ZEP707*MAX(DX,DY,DZ)
            IF (XN == ZERO .AND. YN == ZERO .AND. ZN .NE. ZERO ) THEN
              V1 = ZEP707
              V2 = ZEP707
              V3 = ZERO
            ELSE
              V1 = ZERO
              V2 = ZEP707
              V3 = ZEP707
            ENDIF
            VV1 = V2 * ZN - V3 * YN
            VV2 = V3 * XN - V1 * ZN
            VV3 = V1 * YN - V2 * XN
            VV = SQRT(VV1*VV1 + VV2*VV2 + VV3*VV3)
            IF(VV.LE.HALF)THEN
              IF (XN == ZERO .AND. YN == ZERO .AND. ZN .NE. ZERO ) THEN
                V1 = -ZEP707
                V2 = ZEP707
                V3 = ZERO        
              ELSE 
                V1 = ZERO
                V2 = -ZEP707
                V3 = ZEP707 
              ENDIF      
              VV1 = V2 * ZN - V3 * YN
              VV2 = V3 * XN - V1 * ZN
              VV3 = V1 * YN - V2 * XN
              VV = SQRT(VV1*VV1 + VV2*VV2 + VV3*VV3)
            ENDIF
            VV1 = R*VV1/VV
            VV2 = R*VV2/VV
            VV3 = R*VV3/VV
            V1 = VV2 * ZN - VV3 * YN
            V2 = VV3 * XN - VV1 * ZN
            V3 = VV1 * YN - VV2 * XN
c
            RWALL_V1(N) = V1
            RWALL_V2(N) = V2
            RWALL_V3(N) = V3
            RWALL_V4(N) = VV1
            RWALL_V5(N) = VV2
            RWALL_V6(N) = VV3
            RWALL_V7(N) = ZERO
            RWALL_V8(N) = ZERO
            RWALL_V9(N) = ZERO
            RWALL_V10(N) = ZERO
          ENDIF
c
        ELSEIF(ITYP.EQ.2)THEN
          XN = RWBUF(1,N)
          YN = RWBUF(2,N)
          ZN = RWBUF(3,N)

          DX = XMAX - XMIN
          DY = YMAX - YMIN
          DZ = ZMAX - ZMIN

  	  R =  HALF*RWBUF(7,N)
  	  XL = HALF*MAX(DX,DY,DZ)
  	  V1 = ZERO
  	  V2 = ZEP707
  	  V3 = ZEP707
  	  VV1 = V2 * ZN - V3 * YN
  	  VV2 = V3 * XN - V1 * ZN
  	  VV3 = V1 * YN - V2 * XN
  	  VV = SQRT(VV1*VV1 + VV2*VV2 + VV3*VV3)
  	  IF(VV.LE.HALF)THEN
  	    V1 = ZERO
  	    V2 = -ZEP707
  	    V3 = ZEP707
  	    VV1 = V2 * ZN - V3 * YN
  	    VV2 = V3 * XN - V1 * ZN
  	    VV3 = V1 * YN - V2 * XN
  	    VV = SQRT(VV1*VV1 + VV2*VV2 + VV3*VV3)
  	  ENDIF
  	  VV1 = R*VV1/VV
  	  VV2 = R*VV2/VV
  	  VV3 = R*VV3/VV
  	  V1 = VV2 * ZN - VV3 * YN
  	  V2 = VV3 * XN - VV1 * ZN
  	  V3 = VV1 * YN - VV2 * XN

          RWALL_V1(N) = V1
          RWALL_V2(N) = V2
          RWALL_V3(N) = V3
          RWALL_V4(N) = VV1
          RWALL_V5(N) = VV2
          RWALL_V6(N) = VV3
          RWALL_V7(N) = XL
          RWALL_V8(N) = XN
          RWALL_V9(N) = YN
          RWALL_V10(N) = ZN

        ELSEIF(ITYP.EQ.3)THEN
          XN = RWBUF(1,N)
          YN = RWBUF(2,N)
          ZN = RWBUF(3,N)

          RWALL_V1(N) = HALF*RWBUF(7,N)
          RWALL_V2(N) = ZERO
          RWALL_V3(N) = ZERO
          RWALL_V4(N) = ZERO
          RWALL_V5(N) = ZERO
          RWALL_V6(N) = ZERO
          RWALL_V7(N) = ZERO
          RWALL_V8(N) = ZERO
          RWALL_V9(N) = ZERO
          RWALL_V10(N) = ZERO
c
        ELSEIF(ITYP.EQ.4)THEN
          XN =RWBUF(1,N)
          YN =RWBUF(2,N)
          ZN =RWBUF(3,N)
          IF (ISPMD.EQ.0) THEN
C
            RWALL_V1(N)=RWBUF(7,N)
            RWALL_V2(N)=RWBUF(8,N)
            RWALL_V3(N)=RWBUF(9,N)
            RWALL_V4(N)=RWBUF(10,N)
            RWALL_V5(N)=RWBUF(11,N)
            RWALL_V6(N)=RWBUF(12,N)
            RWALL_V7(N) = ZERO
            RWALL_V8(N) = ZERO
            RWALL_V9(N) = ZERO
            RWALL_V10(N) = ZERO
          ENDIF

        ENDIF
        K=K+NPRW(N)
        IF(NPRW(N4).EQ.-1)K=K+NINT(RWBUF(8,N))
      ENDDO
C
      RETURN
      END
