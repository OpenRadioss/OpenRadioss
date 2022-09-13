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
Chd|  MULTI_GLOBALIZE               source/multifluid/multi_globalize.F
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|-- calls ---------------
Chd|        MULTI_BILAN                   source/multifluid/multi_bilan.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE MULTI_GLOBALIZE(ELBUF_TAB, IPARG, ITASK, MULTI_FVM, 
     .     PARTSAV, IPARTS, GRESAV, IGRTH, GRTH)
C-----------------------------------------------
C     D e s c r i p t i o n
C-----------------------------------------------
C     Submaterial values contained in Layer buffer
C     a globalized into the global buffer
C     It concerns mass density and internal energy
C     Ponderation is made with volumic fracion of
C     each submaterial
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD      
      USE ELBUFDEF_MOD
      USE MULTI_FVM_MOD
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      INTEGER, INTENT(IN) :: IPARG(NPARG, *)
      INTEGER, INTENT(IN) :: ITASK ! SMP TASK
      my_real, INTENT(INOUT) :: PARTSAV(*), GRESAV(*)
      INTEGER, INTENT(IN) :: IPARTS(*), IGRTH(*), GRTH(*)
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      TYPE(G_BUFEL_), POINTER :: GBUF
      TYPE(L_BUFEL_), POINTER :: LBUF
      INTEGER :: NG, II, I
      INTEGER :: NEL, NFT, MTN
      INTEGER :: NBMAT, IMAT
      my_real :: VOL_FRAC,RHO_FRAC
C-----------------------------------------------
C     B e g i n n i n g   o f   s u b r o u t i n e
C-----------------------------------------------
      DO NG = ITASK + 1, NGROUP, NTHREAD
         MTN = IPARG(1, NG)
         IF (MTN == 151) THEN
            NEL = IPARG(2, NG)
            NFT = IPARG(3, NG)
C     Multifluid law number, get the number of materials
            NBMAT = IPARG(20, NG)
C     Global buffer of the current group
            GBUF => ELBUF_TAB(NG)%GBUF
C     Save velocities in GBUF
            DO II = 1, NEL
               I = II + NFT
               GBUF%MOM(II + 0 * NEL) = MULTI_FVM%VEL(1, I)
               GBUF%MOM(II + 1 * NEL) = MULTI_FVM%VEL(2, I)
               GBUF%MOM(II + 2 * NEL) = MULTI_FVM%VEL(3, I)
               MULTI_FVM%RHO(II + NFT) =  GBUF%RHO(II)
               MULTI_FVM%EINT(II + NFT) = GBUF%EINT(II)
             ENDDO
            IF (NBMAT > 1) THEN
               DO II = 1, NEL
                  I = II + NFT
                  GBUF%RHO(II) = ZERO
                  GBUF%EINT(II) = ZERO
                  GBUF%TEMP(II) = ZERO
                  GBUF%SIG(II + 0 * NEL) = ZERO
                  GBUF%SIG(II + 1 * NEL) = ZERO
                  GBUF%SIG(II + 2 * NEL) = ZERO
                  GBUF%SIG(II + 3 * NEL) = ZERO
                  GBUF%SIG(II + 4 * NEL) = ZERO
                  GBUF%SIG(II + 5 * NEL) = ZERO
                  
               ENDDO
C     Loop over the fluid layers
               DO IMAT = 1, NBMAT
                  LBUF => ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1, 1, 1)
                  DO II = 1, NEL
                     I = II + NFT
                     VOL_FRAC = LBUF%VOL(II) / GBUF%VOL(II)
                     GBUF%RHO(II) = GBUF%RHO(II) + VOL_FRAC * LBUF%RHO(II)
                     GBUF%EINT(II) = GBUF%EINT(II) + VOL_FRAC * LBUF%EINT(II)
                     GBUF%SIG(II + 0 * NEL) = GBUF%SIG(II + 0 * NEL) + VOL_FRAC * LBUF%SIG(II + 0 * NEL)
                     GBUF%SIG(II + 1 * NEL) = GBUF%SIG(II + 1 * NEL) + VOL_FRAC * LBUF%SIG(II + 1 * NEL)
                     GBUF%SIG(II + 2 * NEL) = GBUF%SIG(II + 2 * NEL) + VOL_FRAC * LBUF%SIG(II + 2 * NEL)
                     GBUF%SIG(II + 3 * NEL) = GBUF%SIG(II + 3 * NEL) + VOL_FRAC * LBUF%SIG(II + 3 * NEL)
                     GBUF%SIG(II + 4 * NEL) = GBUF%SIG(II + 4 * NEL) + VOL_FRAC * LBUF%SIG(II + 4 * NEL)
                     GBUF%SIG(II + 5 * NEL) = GBUF%SIG(II + 5 * NEL) + VOL_FRAC * LBUF%SIG(II + 5 * NEL)
                     MULTI_FVM%PHASE_ALPHA(IMAT, I) = VOL_FRAC
                     MULTI_FVM%PHASE_RHO(IMAT, I) = LBUF%RHO(II)
                     MULTI_FVM%PHASE_EINT(IMAT, I) = LBUF%EINT(II)
                     MULTI_FVM%PHASE_PRES(IMAT, I) = -THIRD * (
     .                    LBUF%SIG(II + 0 * NEL) + 
     .                    LBUF%SIG(II + 1 * NEL) + 
     .                    LBUF%SIG(II + 2 * NEL))
                  ENDDO
               ENDDO            ! IMAT = 1, NBMAT 
C     Global temperature
               DO IMAT = 1, NBMAT
                  LBUF => ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1, 1, 1)
                  DO II = 1, NEL
                     I = II + NFT
                     VOL_FRAC = LBUF%VOL(II) / GBUF%VOL(II)
                     RHO_FRAC = LBUF%RHO(II) / GBUF%RHO(II)
                     GBUF%TEMP(II) = GBUF%TEMP(II) + RHO_FRAC*VOL_FRAC * LBUF%TEMP(II)
                  ENDDO
               ENDDO            ! IMAT = 1, NBMAT 
C               
               DO II = 1, NEL
                  I = II + NFT
                  MULTI_FVM%RHO(I) = GBUF%RHO(II)
                  MULTI_FVM%EINT(I) = GBUF%EINT(II)
                  MULTI_FVM%PRES(I) = -THIRD * (GBUF%SIG(II + 0 * NEL) + GBUF%SIG(II + 1 * NEL) + GBUF%SIG(II + 2 * NEL))
               ENDDO
            ENDIF               ! NBMAT > 1
            CALL MULTI_BILAN(PARTSAV, GBUF%VOL, MULTI_FVM, 
     .           NEL, NFT, IPARTS(1 + NFT), GRESAV, IGRTH, GRTH)
         ENDIF
      ENDDO                     ! NG = ITASK + 1, NGROUP, NTHREAD
C-----------------------------------------------
C     E n d   o f   s u b r o u t i n e
C-----------------------------------------------
      END SUBROUTINE MULTI_GLOBALIZE
