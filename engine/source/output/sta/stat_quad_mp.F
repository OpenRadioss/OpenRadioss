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
!||    stat_quad_mp   ../engine/source/output/sta/stat_quad_mp.F
!||--- called by ------------------------------------------------------
!||    genstat        ../engine/source/output/sta/genstat.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod   ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    my_alloc_mod   ../common_source/tools/memory/my_alloc.F90
!||====================================================================
      SUBROUTINE STAT_QUAD_MP(ITAB,IPART,IGEO,IXQ,IPARTQ,
     .                        IPART_STATE,NODTAG,STAT_INDXQ,
     .                        IPARG ,ELBUF_TAB)
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
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*), IPART(LIPART1,*),IPARTQ(*),
     .        IGEO(NPROPGI,*), IXQ(NIXQ,*), IPART_STATE(*),
     .        NODTAG(*), STAT_INDXQ(*),
     .        IPARG(NPARG,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, JJ, IPRT0, IPRT, K, STAT_NUMELQ_1,N10,N20,N16
      INTEGER NG, NEL, NFT, LFT, LLT, ITY, ISOLNOD, IOFF
      INTEGER,DIMENSION(:),ALLOCATABLE::NP
      INTEGER,DIMENSION(:,:),ALLOCATABLE:: CLEF
      INTEGER WORK(70000)
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C-----------------------------------------------
C     QUAD
C-----------------------------------------------
      CALL MY_ALLOC(NP,4*NUMELQ)
      CALL MY_ALLOC(CLEF,2,NUMELQ)
C-----------------------------------------------
      JJ = 0
      IF(NUMELQ/=0)THEN

       DO NG=1,NGROUP
        ITY   =IPARG(5,NG)
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

      ENDIF
C-----------------------------------------------
      DEALLOCATE(NP)
      DEALLOCATE(CLEF)
C-----------------------------------------------
      RETURN
      END
