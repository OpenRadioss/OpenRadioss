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
!||    anioffc_crk       ../engine/source/output/anim/generate/anioffc_crk.F
!||--- called by ------------------------------------------------------
!||    genani            ../engine/source/output/anim/generate/genani.F
!||--- calls      -----------------------------------------------------
!||    spmd_iget_partn   ../engine/source/mpi/anim/spmd_iget_partn.F
!||    write_c_c         ../common_source/tools/input_output/write_routtines.c
!||--- uses       -----------------------------------------------------
!||    crackxfem_mod     ../engine/share/modules/crackxfem_mod.F
!||    elbufdef_mod      ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    my_alloc_mod      ../common_source/tools/memory/my_alloc.F90
!||====================================================================
      SUBROUTINE ANIOFFC_CRK( 
     .                  XFEM_TAB ,IPARG  ,IPART,IPARTC,IPARTTG ,
     .                  IOFF     ,EL2FA  ,NBF  ,NBF_L ,IAD_CRKG,
     .                  IEL_CRK  ,INDX_CRK)

C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE CRACKXFEM_MOD
      USE ELBUFDEF_MOD
      use my_alloc_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "scr17_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),EL2FA(*),NBF,IOFF(*),IEL_CRK(*),INDX_CRK(*),
     .        NBF_L,NBPART, IAD_CRKG(NSPMD,*),
     .        IPART(LIPART1,*),IPARTC(*),IPARTTG(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP,NXEL), TARGET :: XFEM_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NG,NEL,NFT,ITY,LFT,LLT,N,ILEV,MLW,N_FA,IXFEM,NI,NLAY,
     .        ILAY,IXEL,CRKS,NEL_CRK,RBUF,ELCRK
      INTEGER IE(NCRKPART),NELCRK(NCRKPART)
      INTEGER,DIMENSION(:),ALLOCATABLE::IOFFBUF

      my_real  OFFG
      my_real, DIMENSION(:) ,POINTER  ::  XOFF
C=======================================================================
      CALL MY_ALLOC(IOFFBUF,NBF_L)
      NEL_CRK = 0
C
      DO CRKS = 1,NCRKPART
        ILEV = INDX_CRK(CRKS)
        NELCRK(CRKS) = NEL_CRK
        NEL_CRK = NEL_CRK + CRKSHELL(ILEV)%CRKNUMSHELL
        IE(ILEV) = 0
      ENDDO
C
      DO NG=1,NGROUP
        MLW   =IPARG(1,NG)
        NEL   =IPARG(2,NG)
        ITY   =IPARG(5,NG)
        NFT   =IPARG(3,NG)
        IXFEM =IPARG(54,NG)
        LFT=1
        LLT=NEL
        IF (IXFEM == 0) CYCLE
C
        IF (ITY == 3) THEN
          NI = NFT
        ELSE
          NI = NFT + NUMELC
        ENDIF
C-----------------------------------------
C          LOOP OVER PHANTOM ELEMENTS
C-----------------------------------------
        DO IXEL=1,NXEL
          NLAY = XFEM_TAB(NG,IXEL)%NLAY
          DO ILAY=1,NLAY
C---
            ILEV = NXEL*(ILAY-1) + IXEL
            N_FA = NELCRK(ILEV)
C---
            IF (NLAY > 1) THEN
              XOFF => XFEM_TAB(NG,IXEL)%BUFLY(ILAY)%LBUF(1,1,1)%OFF
            ELSEIF (NLAY == 1) THEN
              XOFF => XFEM_TAB(NG,IXEL)%GBUF%OFF
            ENDIF
c---
            IF (ITY == 3) THEN
c--  -        SH - 4N
              IF (MLW == 0 .OR. MLW == 13) THEN
                DO I=LFT,LLT
                  N = I + NI
                  IF (IEL_CRK(N) > 0) THEN
                    IE(ILEV) = IE(ILEV) + 1
                    IOFF(EL2FA(N_FA+IE(ILEV))) = 1
                  ENDIF
                ENDDO
              ELSE
                DO I=LFT,LLT
                  N = I + NI
                  IF (IEL_CRK(N) > 0) THEN
                    OFFG = XOFF(I)
                    IE(ILEV) = IE(ILEV) + 1
                    IOFF(EL2FA(N_FA+IE(ILEV))) = NINT(MIN(OFFG,ONE))
                  ENDIF
                ENDDO
              ENDIF  !  IF (MLW == 0 .OR. MLW == 13)
c---
            ELSEIF (ITY == 7) THEN
c---          SH - 3N
              IF (MLW == 0 .OR. MLW == 13) THEN
                DO I=LFT,LLT
                  N = I + NI
                  IF (IEL_CRK(N) > 0) THEN
                    IE(ILEV) = IE(ILEV) + 1
                    IOFF(EL2FA(N_FA+IE(ILEV))) = 1
                  ENDIF
                ENDDO
              ELSE
                DO I=LFT,LLT
                  N = I + NI
                  IF (IEL_CRK(N) > 0) THEN
                    OFFG = XOFF(I)
                    IE(ILEV) = IE(ILEV) + 1
                    IOFF(EL2FA(N_FA+IE(ILEV)))=NINT(MIN(OFFG,ONE))
                  ENDIF
                ENDDO
              ENDIF  !  IF (MLW == 0 .OR. MLW == 13)
            ENDIF  !  IF (ITY == 3)
          ENDDO  !  DO ILAY=1,NLAY
        ENDDO  !  DO IXEL=1,NXEL
      ENDDO  !  DO NG=1,NGROUP
C-----------------------------------------------
      IF (NSPMD==1) THEN
        CALL WRITE_C_C(IOFF,NBF)
      ELSE
        DO I = 1,NBF_L
          IOFFBUF(I) = IOFF(I)
        ENDDO
C
        IF (ISPMD == 0) THEN
          RBUF = NBF
          CALL SPMD_IGET_PARTN(1,NBF_L,IOFFBUF,NCRKPART,
     .                         IAD_CRKG,RBUF,2)
        ELSE
          RBUF = 1
          CALL SPMD_IGET_PARTN(1,NBF_L,IOFFBUF,NCRKPART,
     .                         IAD_CRKG,RBUF,2)
        END IF
      ENDIF
C-----------
      DEALLOCATE(IOFFBUF)
      RETURN
      END
