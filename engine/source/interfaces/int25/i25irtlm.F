Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    i25irtlm          ../engine/source/interfaces/int25/i25irtlm.F
!||--- called by ------------------------------------------------------
!||    i25main_opt_tri   ../engine/source/interfaces/intsort/i25main_opt_tri.F
!||--- calls      -----------------------------------------------------
!||    my_barrier        ../engine/source/system/machine.F
!||--- uses       -----------------------------------------------------
!||    intbufdef_mod     ../common_source/modules/interfaces/intbufdef_mod.F90
!||    tri7box           ../engine/share/modules/tri7box.F
!||====================================================================
      SUBROUTINE I25IRTLM(IPARI,INTBUF_TAB,ITAB,NIN,itask)
C=======================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      integer, intent(in) :: itask !< omp task id
      INTEGER NIN
      INTEGER IPARI(NPARI,NINTER), ITAB(*)
C     REAL
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER L,N,NSN,NSNR,IVIS2,i
      integer, save :: I_STOK_RTLM
      integer, dimension(:), allocatable :: local_cand_opt_n
      integer, dimension(:), allocatable :: local_cand_opt_e
      integer, dimension(:), allocatable, save :: local_i_stok,offset
C     REAL
C-----------------------------------------------
C     REAL
C-----------------------------------------------
      NSN   =IPARI(5,NIN)
      NSNR  =IPARI(24,NIN)
      IVIS2 =IPARI(14,NIN)
!$omp single
      allocate(local_i_stok(nthread)) 
      allocate(offset(nthread+1))
      i_stok_rtlm = 0
!$omp end single

      allocate(local_cand_opt_n(nsn+nsnr))
      allocate(local_cand_opt_e(nsn+nsnr))
      local_i_stok(itask+1) = 0
      offset(itask+1) = 0
      IF(IVIS2/=-1) THEN
        ! ----------------------
!$omp do schedule(guided) private(N,L)
        DO N=1,NSN
          IF(INTBUF_TAB%IRTLM(4*(N-1)+1) > 0)THEN 
            IF(INTBUF_TAB%STFNS(N)==ZERO)THEN
              ! Shooting Nodes
              INTBUF_TAB%IRTLM(4*N-3:4*N)=0
            ELSEIF(INTBUF_TAB%IRTLM(4*(N-1)+4) == ISPMD+1)THEN
              L = INTBUF_TAB%IRTLM(4*(N-1)+3)
              IF(INTBUF_TAB%STFM(L)==ZERO)THEN
                ! Reset IRTLM when a Second node is in contact with a Main surface which was deleted.
                INTBUF_TAB%IRTLM(4*(N-1)+1)=0
                INTBUF_TAB%IRTLM(4*(N-1)+2)=0
                ! The segment where the node was impacted has been deleted 
                ! at the previous cycle => the node can not impact during THIS cycle, but the next cycle ONLY
                INTBUF_TAB%IRTLM(4*(N-1)+3)  = -1
                INTBUF_TAB%IRTLM(4*(N-1)+4)  =  0
                INTBUF_TAB%TIME_S(2*(N-1)+1) =  EP20
                INTBUF_TAB%TIME_S(2*(N-1)+2) =  EP20

                INTBUF_TAB%SECND_FR(6*(N-1)+1:6*N) = ZERO
                INTBUF_TAB%STIF_OLD(2*(N-1)+1:2*N)= ZERO
                INTBUF_TAB%PENE_OLD(5*(N-1)+1:5*N)= ZERO
              ELSE
                ! n impacts on my spmd domain, on segment n irtlm(3,n)
                local_i_stok(itask+1) = local_i_stok(itask+1) + 1
                local_cand_opt_n(local_i_stok(itask+1)) = n
                local_cand_opt_e(local_i_stok(itask+1)) = l

                ! copy old friction forces
                INTBUF_TAB%SECND_FR(6*(N-1)+4:6*N)=INTBUF_TAB%SECND_FR(6*(N-1)+1:6*(N-1)+3)
                ! set new friction forces at 0
                INTBUF_TAB%SECND_FR(6*(N-1)+1:6*(N-1)+3)=ZERO
                INTBUF_TAB%PENE_OLD(5*(N-1)+2) = INTBUF_TAB%PENE_OLD(5*(N-1)+1)
                INTBUF_TAB%PENE_OLD(5*(N-1)+1) = ZERO
                INTBUF_TAB%STIF_OLD(2*(N-1)+2) = INTBUF_TAB%STIF_OLD(2*(N-1)+1) 
                INTBUF_TAB%STIF_OLD(2*(N-1)+1) = ZERO
                ! prepares to leave the contact
                INTBUF_TAB%TIME_S(2*(N-1)+1) = EP20
                INTBUF_TAB%TIME_S(2*(N-1)+2) = EP20
              END IF
            ELSE ! IF(INTBUF_TAB%IRTLM(4*(N-1)+4) == ISPMD+1)THEN
              ! only the processor with the previous impact keeps the corresponding information
              INTBUF_TAB%SECND_FR(6*(N-1)+1:6*N) =ZERO
              INTBUF_TAB%STIF_OLD(2*(N-1)+1:2*N)=ZERO
              INTBUF_TAB%PENE_OLD(5*(N-1)+1)  =ZERO
              INTBUF_TAB%PENE_OLD(5*(N-1)+2)  =ZERO
              INTBUF_TAB%PENE_OLD(5*(N-1)+3)  =ZERO
              INTBUF_TAB%PENE_OLD(5*(N-1)+4)  =ZERO
              INTBUF_TAB%PENE_OLD(5*(N-1)+5)  =ZERO
              ! prepares to leave the contact
              INTBUF_TAB%TIME_S(2*(N-1)+1) = EP20
              INTBUF_TAB%TIME_S(2*(N-1)+2) = EP20
            END IF
          ELSE ! IF(INTBUF_TAB%IRTLM(4*(N-1)+1) > 0)THEN 
            ! reset all for future impact
            INTBUF_TAB%PENE_OLD(5*(N-1)+3)  =ZERO
            INTBUF_TAB%PENE_OLD(5*(N-1)+4)  =ZERO
            INTBUF_TAB%TIME_S(2*(N-1)+1) = -EP20
            INTBUF_TAB%TIME_S(2*(N-1)+2) =  EP20
          END IF
        END DO
!$omp end do
        ! ----------------------

        ! ----------------------
!$omp do schedule(guided) private(N,L)
        DO N=1,NSNR
          IF(IRTLM_FI(NIN)%P(1,N) > 0)THEN
            IF(STIFI(NIN)%P(N)==ZERO)THEN  
              ! Shooting Nodes
              IRTLM_FI(NIN)%P(1:4,N)=0
            ELSEIF(IRTLM_FI(NIN)%P(4,N) == ISPMD+1)THEN
              L = IRTLM_FI(NIN)%P(3,N)
              IF(INTBUF_TAB%STFM(L)==ZERO)THEN
                ! Reset IRTLM when a Second node is in contact with a Main surface which was deleted.
                IRTLM_FI(NIN)%P(1,N)=0
                IRTLM_FI(NIN)%P(2,N)=0
                ! The segment where the node was impacted has been deleted 
                ! at the previous cycle => the node can not impact during THIS cycle, but the next cycle ONLY
                IRTLM_FI(NIN)%P(3,N)  = -1
                IRTLM_FI(NIN)%P(4,N)  =  0
                TIME_SFI(NIN)%P(2*(N-1)+1) =  EP20
                TIME_SFI(NIN)%P(2*(N-1)+2) =  EP20

                SECND_FRFI(NIN)%P (1:6,N)=ZERO
                PENE_OLDFI(NIN)%P(1:5,N)=ZERO
                STIF_OLDFI(NIN)%P(1:2,N)=ZERO
              ELSE

                local_i_stok(itask+1) = local_i_stok(itask+1) + 1
                local_cand_opt_n(local_i_stok(itask+1)) = nsn + n
                local_cand_opt_e(local_i_stok(itask+1)) = l

                ! copy old friction forces
                SECND_FRFI(NIN)%P(4:6,N)=SECND_FRFI(NIN)%P(1:3,N)
                ! set new friction forces at 0
                SECND_FRFI(NIN)%P(1:3,N)=ZERO
                PENE_OLDFI(NIN)%P(2,N) = PENE_OLDFI(NIN)%P(1,N)
                PENE_OLDFI(NIN)%P(1,N) = ZERO
                STIF_OLDFI(NIN)%P(2,N) = STIF_OLDFI(NIN)%P(1,N)
                STIF_OLDFI(NIN)%P(1,N) = ZERO
                ! prepares to leave the contact
                TIME_SFI(NIN)%P(2*(N-1)+1) = EP20
                TIME_SFI(NIN)%P(2*(N-1)+2) = EP20
              END IF
            ELSE ! IF(IRTLM_FI(NIN)%P(4,N) == ISPMD+1)THEN
              ! only the processor with the previous impact keeps the corresponding information
              SECND_FRFI(NIN)%P(1:6,N) =ZERO
              STIF_OLDFI(NIN)%P(1:2,N)=ZERO
              PENE_OLDFI(NIN)%P(1,N)  =ZERO
              PENE_OLDFI(NIN)%P(2,N)  =ZERO
              PENE_OLDFI(NIN)%P(3,N)  =ZERO
              PENE_OLDFI(NIN)%P(4,N)  =ZERO
              PENE_OLDFI(NIN)%P(5,N)  =ZERO
              ! prepares to leave the contact
              TIME_SFI(NIN)%P(2*(N-1)+1) = EP20
              TIME_SFI(NIN)%P(2*(N-1)+2) = EP20
            END IF
          ELSE ! IF(IRTLM_FI(NIN)%P(1,N) > 0)THEN
            ! reset all for future impact
            PENE_OLDFI(NIN)%P(3,N)  =ZERO
            PENE_OLDFI(NIN)%P(4,N)  =ZERO
            TIME_SFI(NIN)%P(2*(N-1)+1) = -EP20
            TIME_SFI(NIN)%P(2*(N-1)+2) =  EP20
          END IF
        END DO
!$omp end do
        ! ----------------------
      ELSE ! IVIS2 == -1 : Interface adhesion case
        ! ----------------------
!$omp do schedule(guided) private(N,L)
        DO N=1,NSN
          IF(INTBUF_TAB%IRTLM(4*(N-1)+1) > 0)THEN 
            IF(INTBUF_TAB%STFNS(N)==ZERO)THEN
              ! Shooting Nodes
              INTBUF_TAB%IRTLM(4*N-3:4*N)=0
            ELSEIF(INTBUF_TAB%IRTLM(4*(N-1)+4) == ISPMD+1)THEN
              L = INTBUF_TAB%IRTLM(4*(N-1)+3)
              IF(INTBUF_TAB%STFM(L)==ZERO)THEN
                ! Reset IRTLM when a Second node is in contact with a Main surface which was deleted.
                INTBUF_TAB%IRTLM(4*(N-1)+1)=0
                INTBUF_TAB%IRTLM(4*(N-1)+2)=0
                ! The segment where the node was impacted has been deleted 
                ! at the previous cycle => the node can not impact during THIS cycle, but the next cycle ONLY
                INTBUF_TAB%IRTLM(4*(N-1)+3)  = -1
                INTBUF_TAB%IRTLM(4*(N-1)+4)  =  0
                INTBUF_TAB%TIME_S(2*(N-1)+1) =  EP20
                INTBUF_TAB%TIME_S(2*(N-1)+2) =  EP20

                INTBUF_TAB%SECND_FR(6*(N-1)+1:6*N) = ZERO
                INTBUF_TAB%STIF_OLD(2*(N-1)+1:2*N)= ZERO
                INTBUF_TAB%PENE_OLD(5*(N-1)+1:5*N)= ZERO
                INTBUF_TAB%IF_ADH(N) = 0    
              ELSE
                ! n impacts on my spmd domain, on segment n irtlm(3,n)
                local_i_stok(itask+1) = local_i_stok(itask+1) + 1
                local_cand_opt_n(local_i_stok(itask+1)) = n
                local_cand_opt_e(local_i_stok(itask+1)) = l

                ! copy old friction forces
                INTBUF_TAB%SECND_FR(6*(N-1)+4:6*N)=INTBUF_TAB%SECND_FR(6*(N-1)+1:6*(N-1)+3)
                ! set new friction forces at 0
                INTBUF_TAB%SECND_FR(6*(N-1)+1:6*(N-1)+3)=ZERO
                INTBUF_TAB%PENE_OLD(5*(N-1)+2) = INTBUF_TAB%PENE_OLD(5*(N-1)+1)
                INTBUF_TAB%PENE_OLD(5*(N-1)+1) = ZERO
                INTBUF_TAB%STIF_OLD(2*(N-1)+2) = INTBUF_TAB%STIF_OLD(2*(N-1)+1) 
                INTBUF_TAB%STIF_OLD(2*(N-1)+1) = ZERO
                ! prepares to leave the contact
                INTBUF_TAB%TIME_S(2*(N-1)+1) = EP20
                INTBUF_TAB%TIME_S(2*(N-1)+2) = EP20
              END IF
            ELSE ! IF(INTBUF_TAB%IRTLM(4*(N-1)+4) == ISPMD+1)THEN
              ! only the processor with the previous impact keeps the corresponding information
              INTBUF_TAB%SECND_FR(6*(N-1)+1:6*N) =ZERO
              INTBUF_TAB%STIF_OLD(2*(N-1)+1:2*N)=ZERO
              INTBUF_TAB%PENE_OLD(5*(N-1)+1)  =ZERO
              INTBUF_TAB%PENE_OLD(5*(N-1)+2)  =ZERO
              INTBUF_TAB%PENE_OLD(5*(N-1)+3)  =ZERO
              INTBUF_TAB%PENE_OLD(5*(N-1)+4)  =ZERO
              INTBUF_TAB%PENE_OLD(5*(N-1)+5)  =ZERO
              INTBUF_TAB%IF_ADH(N) = 0
              ! prepares to leave the contact
              INTBUF_TAB%TIME_S(2*(N-1)+1) = EP20
              INTBUF_TAB%TIME_S(2*(N-1)+2) = EP20
            END IF
          ELSE ! IF(INTBUF_TAB%IRTLM(4*(N-1)+1) > 0)THEN 
            ! reset all for future impact
            INTBUF_TAB%TIME_S(2*(N-1)+1) = -EP20
            INTBUF_TAB%TIME_S(2*(N-1)+2) =  EP20
          END IF
        END DO
!$omp end do
        ! ----------------------

        ! ----------------------
!$omp do schedule(guided) private(N,L)
        DO N=1,NSNR
          IF(IRTLM_FI(NIN)%P(1,N) > 0)THEN
            IF(STIFI(NIN)%P(N)==ZERO)THEN
              ! Shooting Nodes
              IRTLM_FI(NIN)%P(1:4,N)=0
            ELSEIF(IRTLM_FI(NIN)%P(4,N) == ISPMD+1)THEN
              L = IRTLM_FI(NIN)%P(3,N)
              IF(INTBUF_TAB%STFM(L)==ZERO)THEN
                ! Reset IRTLM when a Second node is in contact with a Main surface which was deleted.
                IRTLM_FI(NIN)%P(1,N)=0
                IRTLM_FI(NIN)%P(2,N)=0
                ! The segment where the node was impacted has been deleted 
                ! at the previous cycle => the node can not impact during THIS cycle, but the next cycle ONLY
                IRTLM_FI(NIN)%P(3,N)  = -1
                IRTLM_FI(NIN)%P(4,N)  =  0
                TIME_SFI(NIN)%P(2*(N-1)+1) =  EP20
                TIME_SFI(NIN)%P(2*(N-1)+2) =  EP20

                SECND_FRFI(NIN)%P (1:6,N)=ZERO
                PENE_OLDFI(NIN)%P(1:5,N)=ZERO
                STIF_OLDFI(NIN)%P(1:2,N)=ZERO
                IF_ADHFI(NIN)%P(N) = 0
              ELSE
                local_i_stok(itask+1) = local_i_stok(itask+1) + 1
                local_cand_opt_n(local_i_stok(itask+1)) = nsn+n 
                local_cand_opt_e(local_i_stok(itask+1)) = l

                ! copy old friction forces
                SECND_FRFI(NIN)%P(4:6,N)=SECND_FRFI(NIN)%P(1:3,N)
                ! set new friction forces at 0
                SECND_FRFI(NIN)%P(1:3,N)=ZERO
                PENE_OLDFI(NIN)%P(2,N) = PENE_OLDFI(NIN)%P(1,N)
                PENE_OLDFI(NIN)%P(1,N) = ZERO
                STIF_OLDFI(NIN)%P(2,N) = STIF_OLDFI(NIN)%P(1,N)
                STIF_OLDFI(NIN)%P(1,N) = ZERO
                ! prepares to leave the contact
                TIME_SFI(NIN)%P(2*(N-1)+1) = EP20
                TIME_SFI(NIN)%P(2*(N-1)+2) = EP20
              END IF
            ELSE ! IF(IRTLM_FI(NIN)%P(4,N) == ISPMD+1)THEN
              ! only the processor with the previous impact keeps the corresponding information
              SECND_FRFI(NIN)%P(1:6,N) =ZERO
              STIF_OLDFI(NIN)%P(1:2,N)=ZERO
              PENE_OLDFI(NIN)%P(1,N)  =ZERO
              PENE_OLDFI(NIN)%P(2,N)  =ZERO
              PENE_OLDFI(NIN)%P(3,N)  =ZERO
              PENE_OLDFI(NIN)%P(4,N)  =ZERO
              PENE_OLDFI(NIN)%P(5,N)  =ZERO
              IF_ADHFI(NIN)%P(N) = 0
              ! prepares to leave the contact
              TIME_SFI(NIN)%P(2*(N-1)+1) = EP20
              TIME_SFI(NIN)%P(2*(N-1)+2) = EP20
            END IF
          ELSE ! IF(IRTLM_FI(NIN)%P(1,N) > 0)THEN
            ! reset all for future impact
            TIME_SFI(NIN)%P(2*(N-1)+1) = -EP20
            TIME_SFI(NIN)%P(2*(N-1)+2) =  EP20
          END IF
        END DO
!$omp end do
        ! ----------------------
      ENDIF ! IVIS2 if
        
      call my_barrier()

!$omp single
      do i=1,nthread
        i_stok_rtlm = local_i_stok(i) + i_stok_rtlm
      enddo
      offset(1) = 0
      do i=2,nthread+1
        offset(i) = offset(i-1) + local_i_stok(i-1)
      enddo
!$omp end single

      do i=1,local_i_stok(itask+1)
        INTBUF_TAB%CAND_OPT_N(offset(itask+1)+i) = local_cand_opt_n(i)
        INTBUF_TAB%CAND_OPT_E(offset(itask+1)+i) = local_cand_opt_e(i)
      enddo

      call my_barrier()

!$omp single
      INTBUF_TAB%I_STOK(3) = I_STOK_RTLM
      INTBUF_TAB%I_STOK(2) = I_STOK_RTLM
      deallocate(local_i_stok) 
      deallocate(offset)
!$omp end single

      deallocate(local_cand_opt_n)
      deallocate(local_cand_opt_e)

      call my_barrier()
      RETURN
      END SUBROUTINE I25IRTLM