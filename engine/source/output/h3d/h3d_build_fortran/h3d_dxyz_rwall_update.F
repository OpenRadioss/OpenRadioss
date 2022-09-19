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
Chd|  H3D_DXYZ_RWALL_UPDATE         source/output/h3d/h3d_build_fortran/h3d_dxyz_rwall_update.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        SPMD_H3D_GETMSR_UPDATE        source/output/h3d/spmd/spmd_h3d_getmsr_update.F
Chd|====================================================================
      SUBROUTINE H3D_DXYZ_RWALL_UPDATE (NSTRF,RWBUF,NPRW  ,DISP     ,XMIN ,
     2                     YMIN ,ZMIN  ,XMAX ,YMAX  , ZMAX,
     3                      FR_SEC,FR_WALL,WEIGHT,ITAB,
     4                      XWL ,YWL , ZWL,
     .  RWALL_V1, RWALL_V2, RWALL_V3, RWALL_V4, RWALL_V5, RWALL_V6 )
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
     .  RWBUF(NRWLP,*),DISP(3,*),XMIN ,YMIN ,ZMIN  ,XMAX ,YMAX, ZMAX
      my_real
     .  XWL(*), YWL(*), ZWL(*), RWALL_V1(*), RWALL_V2(*), RWALL_V3(*),
     .  RWALL_V4(*), RWALL_V5(*), RWALL_V6(*)
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
     .   VV3, R, XN,YN,ZN,D,DX,DY,DZ, VV

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
          IF(MSR==0)THEN
            XWL(N)= ZERO
            YWL(N)= ZERO
            ZWL(N)= ZERO
          ELSE
C verifier que ce noeud est sur proc0 !
            XWL(N)=DISP(1,MSR)
            YWL(N)=DISP(2,MSR)
            ZWL(N)=DISP(3,MSR)
          ENDIF
        ELSE
          CALL SPMD_H3D_GETMSR_UPDATE(FR_WALL(1,N),DISP,MSR,XWL(N),YWL(N),ZWL(N),RWBUF(1,N))
        END IF
        IF (ISPMD==0) THEN
          ITYP= NPRW(N4)
          IF(ITYP==4)THEN
           XWL(N) = XWL(N)
           YWL(N) = YWL(N)
           ZWL(N) = ZWL(N)
          ENDIF
          K=K+NPRW(N)
          IF(NPRW(N4)==-1)K=K+NINT(RWBUF(8,N))
        END IF
      ENDDO
C
      K=1
      DO N=1,NRWALL
        N2=N +NRWALL
        N3=N2+NRWALL
        N4=N3+NRWALL
        ITYP= NPRW(N4)

        IF(IABS(ITYP)==1)THEN
C
          IF (ISPMD==0) THEN
c
            RWALL_V1(N) = ZERO
            RWALL_V2(N) = ZERO
            RWALL_V3(N) = ZERO
            RWALL_V4(N) = ZERO
            RWALL_V5(N) = ZERO
            RWALL_V6(N) = ZERO
          ENDIF
c
        ELSEIF(ITYP==2)THEN
C
          IF (ISPMD==0) THEN
c
            RWALL_V1(N) = ZERO
            RWALL_V2(N) = ZERO
            RWALL_V3(N) = ZERO
            RWALL_V4(N) = ZERO
            RWALL_V5(N) = ZERO
            RWALL_V6(N) = ZERO
          ENDIF
        ELSEIF(ITYP==3)THEN
c
          IF (ISPMD==0) THEN
            RWALL_V1(N) = ZERO
            RWALL_V2(N) = ZERO
            RWALL_V3(N) = ZERO
            RWALL_V4(N) = ZERO
            RWALL_V5(N) = ZERO
            RWALL_V6(N) = ZERO
          ENDIF
        ELSEIF(ITYP==4)THEN
          IF (ISPMD==0) THEN
C
            RWALL_V1(N) = ZERO
            RWALL_V2(N) = ZERO
            RWALL_V3(N) = ZERO
            RWALL_V4(N) = ZERO
            RWALL_V5(N) = ZERO
            RWALL_V6(N) = ZERO
          ENDIF

        ENDIF
        K=K+NPRW(N)
        IF(NPRW(N4)==-1)K=K+NINT(RWBUF(8,N))
      ENDDO
C
      RETURN
      END
