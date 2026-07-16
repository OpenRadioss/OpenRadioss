Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    stat_quad_spmd   ../engine/source/output/sta/stat_quad_spmd.F
!||--- called by ------------------------------------------------------
!||    genstat          ../engine/source/output/sta/genstat.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod     ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    element_mod      ../common_source/modules/elements/element_mod.F90
!||    my_alloc_mod     ../common_source/tools/memory/my_alloc.F90
!||    my_dealloc_mod   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
      SUBROUTINE STAT_QUAD_SPMD(ITAB,IPART,IGEO,IXQ,IPARTQ,
     .                     IPART_STATE,NODTAG,STAT_INDXQ,
     .                     IPARG, LENGQ, ELBUF_TAB )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE MY_ALLOC_MOD
      use element_mod , only : nixq
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
      use my_dealloc_mod, only : my_dealloc
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*), IPART(LIPART1,*),
     .        IGEO(NPROPGI,*), IXQ(NIXQ,*),
     .        IPARTQ(*), IPART_STATE(*),
     .        NODTAG(*), STAT_INDXQ(*),
     . IPARG(NPARG,*),LENGQ
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, JJ, IPRT
      INTEGER NG, NEL, NFT, LFT, LLT, ITY, ISOLNOD
      INTEGER,DIMENSION(:),ALLOCATABLE::IADD
      INTEGER,DIMENSION(:,:),ALLOCATABLE::IADG
      INTEGER,DIMENSION(:),ALLOCATABLE::NP
      INTEGER,DIMENSION(:),ALLOCATABLE::NPGLOB
      INTEGER,DIMENSION(:,:),ALLOCATABLE:: CLEF
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C-----------------------------------------------
C     8 NODES BRICK
C-----------------------------------------------
      call my_alloc(IADG, NSPMD, NPART, "IADG")
      call my_alloc(IADD, NPART+1, "IADD")
      call my_alloc(NP, 24*NUMELQG, "NP")
      call my_alloc(NPGLOB, 24*NUMELQG, "NPGLOB")
      call my_alloc(CLEF, 2, NUMELQG, "CLEF")
C-----------------------------------------------
      JJ = 0
      DO NG=1,NGROUP
        ITY   =IPARG(5,NG)
        ISOLNOD = IPARG(28,NG)
        NEL   =IPARG(2,NG)
        NFT   =IPARG(3,NG)
        GBUF => ELBUF_TAB(NG)%GBUF   
        LFT=1
        LLT=NEL
        IF(ITY == 2) THEN
         DO I=LFT,LLT
          N  = I + NFT

          IPRT=IPARTQ(N)
          IF(IPART_STATE(IPRT)==0)CYCLE


            NODTAG(IXQ(2,N))=1
            NODTAG(IXQ(3,N))=1
            NODTAG(IXQ(4,N))=1
            NODTAG(IXQ(5,N))=1

       
         END DO
       END IF
      END DO

C-----------------------------------------------
      call my_dealloc(IADG)
      call my_dealloc(IADD)
      call my_dealloc(NP)
      call my_dealloc(NPGLOB)
      call my_dealloc(CLEF)
C-----------------------------------------------
      RETURN
      END
