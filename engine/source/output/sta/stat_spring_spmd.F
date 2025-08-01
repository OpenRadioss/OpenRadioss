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
!||    stat_spring_spmd      ../engine/source/output/sta/stat_spring_spmd.F
!||--- called by ------------------------------------------------------
!||    genstat               ../engine/source/output/sta/genstat.F
!||--- calls      -----------------------------------------------------
!||    my_orders             ../common_source/tools/sort/my_orders.c
!||    spmd_iget_partn_sta   ../engine/source/mpi/output/spmd_stat.F
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod          ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    my_alloc_mod          ../common_source/tools/memory/my_alloc.F90
!||====================================================================
      SUBROUTINE STAT_SPRING_SPMD(ITAB   ,IPART     ,IXR   ,IPARTR,IPART_STATE,
     .                            NODTAG ,STAT_INDXR,LENGR ,IPARG ,ELBUF_TAB  ,
     .                            IDEL      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD 
      USE MY_ALLOC_MOD
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
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "spmd_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*),IPART(LIPART1,*),IXR(NIXR,*),IPARTR(*),
     .        IPART_STATE(*),NODTAG(*), STAT_INDXR(*),IPARG(NPARG,*)
      INTEGER LENGR,IDEL
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,JJ,IPRT,BUF,IPRT0,K,II
      INTEGER NG,NEL,NFT,LFT,LLT,ITY,IOFF
      INTEGER WORK(70000)
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
      INTEGER,DIMENSION(:),ALLOCATABLE :: IADD
      INTEGER,DIMENSION(:,:),ALLOCATABLE :: IADG
      INTEGER,DIMENSION(:),ALLOCATABLE :: NP
      INTEGER,DIMENSION(:),ALLOCATABLE :: NPGLOB
      INTEGER,DIMENSION(:,:),ALLOCATABLE :: CLEF     
C-----------------------------------------------
C     SPRING
C-----------------------------------------------
      CALL MY_ALLOC(IADG,NSPMD,NPART)
      CALL MY_ALLOC(IADD,NPART+1)
      CALL MY_ALLOC(NP,5*NUMELR)
      CALL MY_ALLOC(NPGLOB,7*LENGR)
      CALL MY_ALLOC(CLEF,2,NUMELRG)
C-----------------------------------------------
      IADD = 0
      NPGLOB(1:7*LENGR) = 0
C
      JJ = 0
      II = 0
      DO NG=1,NGROUP
        ITY = IPARG(5,NG)
        IF (ITY == 6) THEN
          NEL = IPARG(2,NG)
          NFT = IPARG(3,NG)
          GBUF => ELBUF_TAB(NG)%GBUF   
          LFT=1
          LLT=NEL
c
          DO I=LFT,LLT
            N = I + NFT
            IPRT=IPARTR(N)
            IF (IPART_STATE(IPRT) == 0) CYCLE
c
            NP(JJ+1) = IXR(NIXR,N)
            NP(JJ+2) = ITAB(IXR(2,N))
            NP(JJ+3) = ITAB(IXR(3,N))
            NP(JJ+4) = IPRT
            NP(JJ+5) = IABS(NINT(GBUF%OFF(I)))
c
            II = II + 1
c
            JJ = JJ + 5
c
            STAT_NUMELR = STAT_NUMELR + 1
c
            NODTAG(IXR(2,N))=1
            NODTAG(IXR(3,N))=1
          ENDDO ! DO I=LFT,LLT
        ENDIF ! IF (ITY == 6)
      ENDDO ! DO NG=1,NGROUP
C----
      STAT_NUMELR_G = 0
      CALL SPMD_IGET_PARTN_STA(5,STAT_NUMELR,STAT_NUMELR_G,LENGR,NP,
     .             IADG,NPGLOB,STAT_INDXR)
C----
      IF (ISPMD == 0) THEN
        DO N=1,STAT_NUMELR_G
          STAT_INDXR(N)=N
          CLEF(1,N)=NPGLOB(5*(N-1)+5)
          CLEF(2,N)=NPGLOB(5*(N-1)+1)
        ENDDO
        CALL MY_ORDERS(0,WORK,CLEF,STAT_INDXR,STAT_NUMELR_G,2)
C----
        IPRT0=0
        DO N=1,STAT_NUMELR_G
          K=STAT_INDXR(N)
          JJ=5*(K-1)
          IPRT=NPGLOB(JJ+4)
          IOFF=NPGLOB(JJ+5)
          IF (IDEL==0 .OR. (IDEL==1 .AND. IOFF >= 1)) THEN
            IF (IPRT /= IPRT0) THEN
              WRITE(IUGEO,'(A,I10)')'/SPRING/',IPART(4,IPRT)
              WRITE(IUGEO,'(A)')
     .      '#SPRING_ID      NOD1      NOD2'
              IPRT0=IPRT
            ENDIF
            WRITE(IUGEO,'(3I10)') NPGLOB(JJ+1),NPGLOB(JJ+2),NPGLOB(JJ+3)
          ENDIF !IF (IDEL)
        ENDDO ! DO N=1,STAT_NUMELR_G
      ENDIF ! IF (ISPMD == 0)
C-----------------------------------------------
      DEALLOCATE(IADD)
      DEALLOCATE(IADG)
      DEALLOCATE(NP)
      DEALLOCATE(NPGLOB)
      DEALLOCATE(CLEF)
      RETURN
      END
