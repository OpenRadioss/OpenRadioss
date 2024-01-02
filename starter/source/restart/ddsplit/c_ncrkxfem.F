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
Chd|  C_NCRKXFEM                    source/restart/ddsplit/c_ncrkxfem.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        XFEM2DEF_MOD                  ../common_source/modules/xfem2def_mod.F
Chd|====================================================================
      SUBROUTINE C_NCRKXFEM(NODGLOB,INOD_CRKXFEM,INOD_L,
     .                      NUMNOD_L,NUMNODCRKXFE_L,INDEX,PROC,
     .                      IXC,IXTG,CEP_XFE,NODLOCAL,NODLEVXF_L,
     .                      NODLEVXF,NODGLOBXFE,NOD_XFE_L,CRKSHELL)
C-----------------------------------------------
      USE XFEM2DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com_xfem1.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NODGLOB(*),INOD_CRKXFEM(*),INOD_L(*),
     .         NUMNOD_L,NUMNODCRKXFE_L,INDEX(*),PROC,
     .         IXC(NIXC,*),IXTG(NIXTG,*),CEP_XFE(*),
     .         NODLOCAL(*),NODLEVXF_L(*),NODLEVXF(*),
     .         NODGLOBXFE(*),NOD_XFE_L
      TYPE (XFEM_SHELL_) , DIMENSION(NLEVMAX)       :: CRKSHELL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NL_L,II,JJ,J,K,NOD,ELTYP,ELEM,
     .        INOD_CRK,NOD_XFE_G,NELCRK
      INTEGER, DIMENSION(:), ALLOCATABLE :: NODTAG
C=======================================================================
!     1d array
      ALLOCATE( NODTAG(0:NUMNOD_L+1) )
! ----------------------------------
c     Tableaux noeuds phantomes chaque ply
C-----------------------------------------------
      NELCRK = 0
      DO K=1,NLEVMAX
        DO I=1,CRKSHELL(K)%CRKNUMSHELL
          ELEM  = CRKSHELL(K)%PHANTOML(I)
          ELTYP = CRKSHELL(K)%ELTYPE(I)
          IF (CEP_XFE(I) == PROC) THEN
            IF(ELTYP == 4) THEN
               DO J=1,ELTYP
                NOD = IXC(J+1,ELEM)
                IF (NOD > 0) THEN
                  IF (INOD_CRKXFEM(NOD) > 0) THEN        ! N noeud local xfem
                    NOD_XFE_G = CRKSHELL(K)%XNODEG(J,I)  ! ID glob node phantome sur ply
                    NOD_XFE_L = NOD_XFE_L + 1            ! ID local node phantome sur ply
                    NODGLOBXFE(NOD_XFE_L) = NOD_XFE_G    ! Id local -> Id global (phant)
                  ENDIF
                ENDIF
              ENDDO
            ELSEIF (ELTYP == 3) THEN
              DO J=1,ELTYP
               NOD = IXTG(J+1,ELEM)
               IF (NOD > 0) THEN
                 IF(INOD_CRKXFEM(NOD) > 0)THEN
                   NOD_XFE_G = CRKSHELL(K)%XNODEG(J,I)
                   NOD_XFE_L = NOD_XFE_L + 1
                   NODGLOBXFE(NOD_XFE_L) = NOD_XFE_G
                  ENDIF
                ENDIF
              ENDDO
C
c             add one more node (as sh4) for animation files  (3N -> 4N)
C
              NOD_XFE_G = CRKSHELL(K)%XNODEG(4,I)
              NOD_XFE_L = NOD_XFE_L + 1
              NODGLOBXFE(NOD_XFE_L) = NOD_XFE_G
            END IF
          ENDIF
        ENDDO
        NELCRK = NELCRK + CRKSHELL(K)%CRKNUMSHELL     ! Nb elements total sur nlevmax
      ENDDO
      NUMNODXFE = NOD_XFE_L                           ! Nb noeuds total sur nlevmax
C
      NODTAG(1:NUMNOD_L) = 0
      K = 1 ! the same as K=1,NLEVMAX
      DO I=1,CRKSHELL(K)%CRKNUMSHELL
        ELTYP = CRKSHELL(K)%ELTYPE(I)
        ELEM  = CRKSHELL(K)%PHANTOML(I)
        IF (CEP_XFE(I) == PROC) THEN
          IF (ELTYP == 4) THEN
            DO J=1,ELTYP
              NOD = IXC(J+1,ELEM)
              NODTAG(NODLOCAL(NOD))=NOD
            ENDDO
          ELSEIF (ELTYP == 3) THEN
            DO J=1,ELTYP
              NOD = IXTG(J+1,ELEM)
              NODTAG(NODLOCAL(NOD))=NOD
            ENDDO
          END IF
        END IF
      END DO
C---
      NL_L = 0
      DO I=1,NUMNOD_L
        NOD = NODTAG(I)
        IF (NOD > 0) THEN
          IF (INOD_CRKXFEM(NOD) > 0) THEN
            NL_L = NL_L + 1
            INOD_L(I) = NL_L
            INDEX(NL_L) = INOD_CRKXFEM(NOD)
            INOD_CRK = INOD_CRKXFEM(NOD)
            NODLEVXF_L(NL_L) = NODLEVXF(INOD_CRK) ! nb des copies d'un nooeuds std xfem
          ENDIF
        ENDIF
      ENDDO
C---
      NUMNODCRKXFE_L = NL_L
C---
! ----------------------------------
!     1d array
      DEALLOCATE( NODTAG )
! ----------------------------------
      RETURN
      END
