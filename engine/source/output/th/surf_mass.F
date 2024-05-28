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
Chd|  SURF_MASS_MONV                source/output/th/surf_mass.F  
Chd|-- called by -----------
Chd|        HIST2                         source/output/th/hist2.F      
Chd|-- calls ---------------
Chd|        SURF_MASS_COM                 source/output/th/surf_mass.F  
Chd|        SURF_MASS_FVM                 source/output/th/surf_mass.F  
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        TH_SURF_MOD                   ../common_source/modules/interfaces/th_surf_mod.F
Chd|====================================================================
      SUBROUTINE SURF_MASS_MONV(FSAVSURF, IGRSURF, MONVOL, VOLMON, FR_MV)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE TH_SURF_MOD , only : TH_SURF_NUM_CHANNEL
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
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MONVOL(*), FR_MV(NSPMD+2,NVOLU)
      my_real FSAVSURF(TH_SURF_NUM_CHANNEL,NSURF), VOLMON(*)
      TYPE (SURF_), DIMENSION(NSURF) :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, NN, IAD, KAD, ITYP, NCA, IDSURF, PMAIN
      INTEGER NTG, NTGI, NNT, KIBALE, KRBALE
      INTEGER K1, K2, KK2, KI4, KR5, KR6, KR7, NNS, NNI, NTGT
      INTEGER :: KR1, KR2, KR3, KR4
C-----------------------------------------------------------------------
         K1 = 1
         K2 = 1+NIMV*NVOLU
         KK2 = 1+NRVOLU*NVOLU
         KIBALE=1+NIMV*NVOLU  +LICBAG+LIBAGJET+LIBAGHOL
         KRBALE=1+NRVOLU*NVOLU+LRCBAG+LRBAGJET+LRBAGHOL
         DO N=1,NVOLU
           NCA  = MONVOL(K1-1+3)
           PMAIN = FR_MV(NSPMD+2,N)
           IF(ISPMD+1 == PMAIN) THEN
             ITYP = MONVOL(K1-1+2)
             IF(ITYP == 6.OR.ITYP == 8) THEN
               NTGI = MONVOL(K1-1+69)
               IF(NTGI == 0) CYCLE
               NTG = MONVOL(K1-1+33)
               NTGT = NTG + NTGI
               KAD = KIBALE+MONVOL(K1-1+18)
               NNS = MONVOL(K1-1+32)
               NNI = MONVOL(K1-1+68) 
               NNT = NNS + NNI
               KR1=KRBALE+MONVOL(K1-1+34)
               KR2=KR1+NNT
               KR3=KR2+NNT
               KR4=KR3+NNT
               KR5=KR4+3*NNT
               KR6=KR5+NTGT
               KR7 = KR6+NTG+NTGI
               DO I=1,NSURF
                  IF(IGRSURF(I)%TH_SURF == 1) THEN
                     IF(MONVOL(KAD) /= 0) CALL SURF_MASS_FVM(NTGI, MONVOL(KAD),
     .                  VOLMON(KR6), FSAVSURF(2,I), VOLMON(KR7), FSAVSURF(3,I),
     .                  VOLMON(KR5))
                  ENDIF
                  KAD = KAD+NTGI+1
               ENDDO
             ELSEIF(ITYP == 9 .AND. NCA > 0) THEN
               DO I=1,NSURF
                  IF(IGRSURF(I)%TH_SURF /= 1) CYCLE
                  IDSURF=I
                  CALL SURF_MASS_COM(MONVOL(K2), VOLMON(KK2), NCA, IDSURF,
     .                               FSAVSURF(2,I),FSAVSURF(3,I))
               ENDDO
             ENDIF       
           ENDIF
           K1 = K1+NIMV
           K2 = K2+NICBAG*NCA
           KK2 = KK2+NRCBAG*NCA
         ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  SURF_MASS_FVM                 source/output/th/surf_mass.F  
Chd|-- called by -----------
Chd|        SURF_MASS_MONV                source/output/th/surf_mass.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SURF_MASS_FVM(NTGI, ITAG, ELFMASS, MASS, ELFVEL, VEL, ELSINI)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NTGI, ITAG(*)
      my_real
     .   ELFMASS(*), MASS, ELFVEL(*), VEL, ELSINI(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real SURF
C-----------------------------------------------
      MASS = ZERO
      SURF = ZERO
      VEL  = ZERO
      DO I=1,NTGI
        IF(ITAG(I+1) == 0) CYCLE
        MASS=MASS+ELFMASS(I)
        SURF=SURF+ELSINI(I)
        VEL =VEL +ELFVEL(I)*ELSINI(I)
      ENDDO
      MASS=ABS(MASS)
      VEL =ABS(VEL)/SURF
C
      RETURN
      END
Chd|====================================================================
Chd|  SURF_MASS_COM                 source/output/th/surf_mass.F  
Chd|-- called by -----------
Chd|        SURF_MASS_MONV                source/output/th/surf_mass.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SURF_MASS_COM(ICBAG, RCBAG, NCA, IDSURF, MASS, VEL)
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
      INTEGER ICBAG(NICBAG,*), NCA, IDSURF
C     REAL
      my_real
     .   RCBAG(NRCBAG,*), MASS, VEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IDCOM
C-----------------------------------------------
      DO I=1,NCA
        IDCOM = ICBAG(2,I)
        IF(IDCOM /= IDSURF) CYCLE
        MASS = MASS+RCBAG(8,I)
        VEL  = VEL +RCBAG(9,I)
      ENDDO
C
      RETURN
      END
