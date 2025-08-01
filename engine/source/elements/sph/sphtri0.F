Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    sphtri0         ../engine/source/elements/sph/sphtri0.F
!||--- called by ------------------------------------------------------
!||    sphprep         ../engine/source/elements/sph/sphprep.F
!||--- calls      -----------------------------------------------------
!||    my_barrier      ../engine/source/system/machine.F
!||    spmd_all_dmax   ../engine/source/mpi/elements/spmd_sph.F
!||    spmd_barrier    ../engine/source/mpi/spmd_mod.F90
!||    spmd_sphvox     ../engine/source/mpi/sph/spmd_sphvox.F
!||    spmd_sphvox0    ../engine/source/mpi/elements/spmd_sph.F
!||    startime        ../engine/source/system/timer_mod.F90
!||    stoptime        ../engine/source/system/timer_mod.F90
!||--- uses       -----------------------------------------------------
!||    spmd_mod        ../engine/source/mpi/spmd_mod.F90
!||    timer_mod       ../engine/source/system/timer_mod.F90
!||    tri7box         ../engine/share/modules/tri7box.F
!||====================================================================
      SUBROUTINE SPHTRI0(TIMERS, X   , SPBUF, KXSP, WSP2SORT,BMINMA , DMAX,
     2                 NSP2SORTF,NSP2SORTL,NMN,ITASK, DBUC)  
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE SPMD_MOD, ONLY : SPMD_BARRIER
      USE TIMER_MOD
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "sphcom.inc"
#include      "task_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(TIMER_), INTENT(INOUT) :: TIMERS
      INTEGER KXSP(NISP,*), WSP2SORT(*),NSP2SORTF,NSP2SORTL,ITASK,NMN
      my_real X(3,*),SPBUF(NSPBUF,*), BMINMA(12),  DMAX, DBUC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, J, NS,LOC_PROC
      my_real XMAX,YMAX,ZMAX,XMIN,YMIN,ZMIN,DBUCL,MX,MY,MZ,DX,DY,DZ,SX,SY,SZ,SX2,SY2,SZ2
     
C-----------------------------------------------
      XMIN=EP30
      XMAX=-EP30
      YMIN=EP30
      YMAX=-EP30
      ZMIN=EP30
      ZMAX=-EP30     
      SX=ZERO
      SY=ZERO
      SZ=ZERO
      SX2=ZERO
      SY2=ZERO
      SZ2=ZERO
C        
C
C        Bucket sort. DBUC + MIN / MAX

      DBUCL=ZERO
      DO NS=NSP2SORTF,NSP2SORTL
        N=WSP2SORT(NS)
        DBUCL=MAX(DBUCL,SPBUF(1,N))
C
        J=KXSP(3,N)
        XMIN= MIN(XMIN,X(1,J))
        YMIN= MIN(YMIN,X(2,J))
        ZMIN= MIN(ZMIN,X(3,J))
        XMAX= MAX(XMAX,X(1,J))
        YMAX= MAX(YMAX,X(2,J))
        ZMAX= MAX(ZMAX,X(3,J))
        SX=SX+X(1,J)
        SY=SY+X(2,J)
        SZ=SZ+X(3,J)
        SX2=SX2+X(1,J)**2
        SY2=SY2+X(2,J)**2
        SZ2=SZ2+X(3,J)**2
      END DO

      IF(ITASK == 0)THEN
         BMINMA(7:12)=0
         BMINMA(1)=-EP30
         BMINMA(2)=-EP30
         BMINMA(3)=-EP30
         BMINMA(4)=EP30
         BMINMA(5)=EP30
         BMINMA(6)=EP30
         DBUC = ZERO
      ENDIF

      CALL MY_BARRIER

#include "lockon.inc"
      BMINMA(1) = MAX(BMINMA(1),XMAX) 
      BMINMA(2) = MAX(BMINMA(2),YMAX) 
      BMINMA(3) = MAX(BMINMA(3),ZMAX) 
      BMINMA(4) = MIN(BMINMA(4),XMIN) 
      BMINMA(5) = MIN(BMINMA(5),YMIN) 
      BMINMA(6) = MIN(BMINMA(6),ZMIN)
      DBUC = MAX(DBUC,DBUCL) 
      DMAX = DBUC
      BMINMA(7) = BMINMA(7)+SX
      BMINMA(8) = BMINMA(8)+SY
      BMINMA(9) = BMINMA(9)+SZ
      BMINMA(10)= BMINMA(10)+SX2
      BMINMA(11)= BMINMA(11)+SY2
      BMINMA(12)= BMINMA(12)+SZ2
#include "lockoff.inc"
C
      CALL MY_BARRIER

!$OMP SINGLE
        DBUC=DBUC*SQRT(ONE +SPATRUE)*ONEP0001
          BMINMA(1) = BMINMA(1)+DBUC
        BMINMA(2) = BMINMA(2)+DBUC
        BMINMA(3) = BMINMA(3)+DBUC
          BMINMA(4) = BMINMA(4)-DBUC
          BMINMA(5) = BMINMA(5)-DBUC
          BMINMA(6) = BMINMA(6)-DBUC
C   Computation of standard deviation of X main
C   use the formula dev = sum(xi  )-n.m  
C   mean value m by direction
        MX=BMINMA(7)/MAX(NMN,1)
        MY=BMINMA(8)/MAX(NMN,1)
        MZ=BMINMA(9)/MAX(NMN,1)
c          print*,noint,'m=',mx,my,mz,NMN,NRTM
C   standard deviation by direction
        DX=SQRT(BMINMA(10)/MAX(NMN,1)-MX**2)
        DY=SQRT(BMINMA(11)/MAX(NMN,1)-MY**2)
        DZ=SQRT(BMINMA(12)/MAX(NMN,1)-MZ**2)
c          print*,noint,'var=',dx,dy,dz
C   Computation of new boundary of the domain mean values +/- 2 sigma
C   => 95% of the population for normal distribution
        BMINMA(7) =MIN(MX+2*DX,BMINMA(1))
        BMINMA(8) =MIN(MY+2*DY,BMINMA(2))
        BMINMA(9) =MIN(MZ+2*DZ,BMINMA(3))
        BMINMA(10)=MAX(MX-2*DX,BMINMA(4))
        BMINMA(11)=MAX(MY-2*DY,BMINMA(5))
        BMINMA(12)=MAX(MZ-2*DZ,BMINMA(6))
   
C   Test cas particulier 2D
        IF(BMINMA(10)==BMINMA(7))THEN
          BMINMA(10)=BMINMA(4)
          BMINMA(7)=BMINMA(1)
        END IF
        IF(BMINMA(11)==BMINMA(8))THEN
          BMINMA(11)=BMINMA(5)
          BMINMA(8)=BMINMA(2)
        END IF
        IF(BMINMA(12)==BMINMA(9))THEN
          BMINMA(12)=BMINMA(6)
          BMINMA(9)=BMINMA(3)
        END IF
!$OMP END SINGLE
                

      IF(NSPMD>1) THEN
C
C     Boite   MIN / MAX en SPMD
C
        IF(ITASK==0) THEN
          LOC_PROC = ISPMD+1
          CRVOXEL(0:LRVOXEL,0:LRVOXEL,LOC_PROC)=0
        ENDIF

        CALL MY_BARRIER

        CALL SPMD_SPHVOX0(KXSP ,SPBUF,WSP2SORT,BMINMA,X,
     2                    NSP2SORTF,NSP2SORTL)
 
        CALL MY_BARRIER

        IF(ITASK==0)THEN
C
         IF(IMONM == 2)THEN
           CALL STARTIME(TIMERS,95)
           CALL SPMD_BARRIER()
           CALL STOPTIME(TIMERS,95)
         END IF
         CALL STARTIME(TIMERS,91)
C Recuperation des cellules SPH remotes NSPHR stockees dans XSPHR
C 
         CALL SPMD_SPHVOX(KXSP ,SPBUF,WSP2SORT,BMINMA,X)
         CALL SPMD_ALL_DMAX(DMAX,1)
C
         CALL STOPTIME(TIMERS,91)
        ENDIF

      END IF 
  
      CALL MY_BARRIER
C
      RETURN
      END
