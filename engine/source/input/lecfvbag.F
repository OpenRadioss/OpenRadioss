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
Chd|  LECFVBAG                      source/input/lecfvbag.F       
Chd|-- called by -----------
Chd|        LECTUR                        source/input/lectur.F         
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        SPMD_FVB_GATH                 source/mpi/airbags/spmd_fvb_gath.F
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|====================================================================
      SUBROUTINE LECFVBAG(NFVMESH, MONVOL, VOLMON, X)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE FVBAG_MOD
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
#include      "units_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NFVMESH, MONVOL(*)
      my_real
     .        VOLMON(*), X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ID, NBX, NBY, K1, K2, KIBJET, KIBHOL, KIBALE, KK1, N,
     .        ITYP, NN, KI1, J, JJ, NSTEP, IV(10), IFVI, NBZ, IFV
      my_real
     .        VX3, VY3, VZ3, VX1, VY1, VZ1, DX0, DY0, DZ0, LX, LY, NORM,
     .        VVX3, VVY3, VVZ3, NORM2, SS, VVX1, VVY1, VVZ1, VVX2,
     .        VVY2, VVZ2, LXMAX, LYMAX, XX, YY, ZZ, XL, YL, X0, Y0, Z0,
     .        LZ, LZMAX, ZL, RBID
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: XXX
C
      DO I=1,NFVMESH
         READ(IIN,'(I10)') IFVI
         IF (IFVI.EQ.1) THEN
            READ(IIN,'(2I10)') ID, NSTEP
            IF (NSTEP.EQ.0) NSTEP=20
            READ(IIN,'(3F20.0)') VX3, VY3, VZ3
            READ(IIN,'(3F20.0)') VX1, VY1, VZ1
            READ(IIN,'(3F20.0)') DX0, DY0, DZ0
            READ(IIN,'(3F20.0)') LX, LY, LZ
            READ(IIN,'(3I10)') NBX, NBY, NBZ
C
            K1=1
            K2=1+NIMV*NVOLU
            KIBJET=K2+LICBAG
            KIBHOL=KIBJET+LIBAGJET
            KIBALE=KIBHOL+LIBAGHOL
            KK1=1
            DO N=1,NVOLU
               IF (MONVOL(K1).EQ.ID) THEN
                  ITYP=MONVOL(K1-1+2)
                  IF (ITYP.NE.6) THEN
                     WRITE(IOUT,'(A33,I8,A38)') 
     . ' /FVMBAG/REMESH : MON. VOLUME ID ',ID,' IS NOT OF TYPE FVMBAG'//
     . ' - IT IS IGNORED'
                     GOTO 100
                  ENDIF
C
                  NORM=SQRT(VX3**2+VY3**2+VZ3**2)
                  IF (NORM.EQ.ZERO) THEN
                     VX3=VOLMON(KK1-1+35)
                     VY3=VOLMON(KK1-1+36)
                     VZ3=VOLMON(KK1-1+37)
                     NORM=SQRT(VX3**2+VY3**2+VZ3**2)
                  ENDIF
                  VVX3=VX3/NORM
                  VVY3=VY3/NORM
                  VVZ3=VZ3/NORM
                  NORM2=VX1**2+VY1**2+VZ1**2
                  IF (NORM2.EQ.ZERO) THEN
                     VX1=VOLMON(KK1-1+38)
                     VY1=VOLMON(KK1-1+39)
                     VZ1=VOLMON(KK1-1+40)
                  ENDIF
                  X0=VOLMON(KK1-1+41)+DX0
                  Y0=VOLMON(KK1-1+42)+DY0
                  Z0=VOLMON(KK1-1+43)+DZ0
                  IF (LX.EQ.ZERO) LX=VOLMON(KK1-1+44)
                  IF (LY.EQ.ZERO) LY=VOLMON(KK1-1+45)
                  IF (LZ.EQ.ZERO) LZ=VOLMON(KK1-1+53)
                  IF (NBX.EQ.0) NBX=MONVOL(K1-1+54)
                  IF (NBY.EQ.0) NBY=MONVOL(K1-1+55)
                  IF (NBZ.EQ.0) NBZ=MONVOL(K1-1+65)
C Verifications
                  SS=VX1*VVX3+VY1*VVY3+VZ1*VVZ3
                  VVX1=VX1-SS*VVX3
                  VVY1=VY1-SS*VVY3
                  VVZ1=VZ1-SS*VVZ3
                  NORM=SQRT(VVX1**2+VVY1**2+VVZ1**2)
                  IF (NORM.EQ.ZERO) THEN
                     WRITE(ISTDO,'(A)') 
     .                       ' ** ERROR IN FVMBAG MESHING DATA '
                     WRITE(IOUT,'(A)') 
     .                       ' ** ERROR IN FVMBAG MESHING DATA '
                     WRITE(IOUT,'(A14,I8)') '    MONVOL ID ',ID
                     WRITE(IOUT,'(A)') 
     .                    '    NEW CUT DIRECTIONS ARE COLINEAR'
                     CALL ARRET(2)
                  ENDIF
                  VVX1=VVX1/NORM
                  VVY1=VVY1/NORM
                  VVZ1=VVZ1/NORM
                  VVX2=VVY3*VVZ1-VVZ3*VVY1
                  VVY2=VVZ3*VVX1-VVX3*VVZ1
                  VVZ2=VVX3*VVY1-VVY3*VVX1
C
                  LXMAX=ZERO
                  LYMAX=ZERO
                  LZMAX=ZERO
                  NN=MONVOL(K1-1+32)
                  KI1=KIBALE+MONVOL(K1-1+31)
C
                  IF (IMACH.NE.3) THEN
                     DO J=1,NN
                        JJ=MONVOL(KI1-1+J)
                        XX=X(1,JJ)
                        YY=X(2,JJ)
                        ZZ=X(3,JJ)
                        XL=(XX-X0)*VVX1+(YY-Y0)*VVY1+(ZZ-Z0)*VVZ1
                        YL=(XX-X0)*VVX2+(YY-Y0)*VVY2+(ZZ-Z0)*VVZ2
                        ZL=(XX-X0)*VVX3+(YY-Y0)*VVY3+(ZZ-Z0)*VVZ3
                        LXMAX=MAX(LXMAX,ABS(XL))
                        LYMAX=MAX(LYMAX,ABS(YL))
                        LZMAX=MAX(LZMAX,ABS(ZL))
                     ENDDO
C
                  ELSE
                     IFV=MONVOL(K1-1+45)
                     ALLOCATE(XXX(3,NN))
                     CALL SPMD_FVB_GATH(IFV, X, XXX, RBID, RBID,
     .                                  1  )
                     IF (ISPMD.EQ.FVSPMD(IFV)%PMAIN-1) THEN
                        DO J=1,NN
                           XX=XXX(1,J)
                           YY=XXX(2,J)
                           ZZ=XXX(3,J)
                           XL=(XX-X0)*VVX1+(YY-Y0)*VVY1+(ZZ-Z0)*VVZ1
                           YL=(XX-X0)*VVX2+(YY-Y0)*VVY2+(ZZ-Z0)*VVZ2
                           ZL=(XX-X0)*VVX3+(YY-Y0)*VVY3+(ZZ-Z0)*VVZ3
                           LXMAX=MAX(LXMAX,ABS(XL))
                           LYMAX=MAX(LYMAX,ABS(YL))
                           LZMAX=MAX(LZMAX,ABS(ZL))
                        ENDDO
                     ENDIF
                     DEALLOCATE(XXX)
                  ENDIF
C
                  IF (LXMAX.GT.LX) THEN
                     WRITE(IOUT,'(A14,I8,A9)') 
     .                        ' ** MONVOL ID ',ID,' (FVMBAG)'
                     WRITE(IOUT,'(A)') '    IN LOCAL FRAME DIRECTION 1'
                     WRITE(IOUT,'(A18,G11.4,A33,G11.4,G11.4)') 
     .    '    GIVEN LENGTH ',LX,
     .    ' IS SMALLER THAN BOUNDING LENGTH ',LXMAX
                     LX=LXMAX*ONEP01
                     WRITE(IOUT,'(A20,G11.4)') '    IT IS RESET TO ',LX
                  ENDIF
                  IF (LYMAX.GT.LY) THEN
                     WRITE(IOUT,'(A14,I8,A9)') 
     .                        ' ** MONVOL ID ',ID,' (FVMBAG)'
                     WRITE(IOUT,'(A)') '    IN LOCAL FRAME DIRECTION 2'
                     WRITE(IOUT,'(A18,G11.4,A33,G11.4,G11.4)') 
     .    '    GIVEN LENGTH ',LY,
     .    ' IS SMALLER THAN BOUNDING LENGTH ',LYMAX
                     LY=LYMAX*ONEP01
                     WRITE(IOUT,'(A20,G11.4)') '    IT IS RESET TO ',LY
                  ENDIF
C On autorise le decoupage horizontal d'une bande du maillage uniquement
C                  IF (LZMAX.GT.LZ) THEN
C                     WRITE(IOUT,'(A14,I8,A9)') 
C     .                        ' ** MONVOL ID ',ID,' (ALEBAG)'
C                     WRITE(IOUT,'(A)') '    IN LOCAL FRAME DIRECTION 3'
C                     WRITE(IOUT,'(A18,G11.4,A33,G11.4,G11.4)') 
C     .    '    GIVEN LENGTH ',LZ,
C     .    ' IS SMALLER THAN BOUNDING LENGTH ',LZMAX
C                     LZ=LZMAX*ONEP01
C                     WRITE(IOUT,'(A20,G11.4)') '    IT IS RESET TO ',LZ
C                  ENDIF
C
                  VOLMON(KK1-1+35)=VX3
                  VOLMON(KK1-1+36)=VY3
                  VOLMON(KK1-1+37)=VZ3
                  VOLMON(KK1-1+38)=VX1
                  VOLMON(KK1-1+39)=VY1
                  VOLMON(KK1-1+40)=VZ1
                  VOLMON(KK1-1+41)=X0
                  VOLMON(KK1-1+42)=Y0
                  VOLMON(KK1-1+43)=Z0
                  VOLMON(KK1-1+44)=LX
                  VOLMON(KK1-1+45)=LY
                  VOLMON(KK1-1+53)=LZ
                  MONVOL(K1-1+54)=NBX
                  MONVOL(K1-1+55)=NBY
                  MONVOL(K1-1+65)=NBZ
C
                  MONVOL(K1-1+56)=1
                  MONVOL(K1-1+58)=NSTEP
               ENDIF
               K1=K1+NIMV
               KK1=KK1+NRVOLU
            ENDDO
         ENDIF
  100 ENDDO
C
      RETURN
      END
      
