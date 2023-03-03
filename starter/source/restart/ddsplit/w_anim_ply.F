Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
C
Chd|====================================================================
Chd|  W_ANIM_PLY                    source/restart/ddsplit/w_anim_ply.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        GET_PLY_NOD                   source/spmd/spmd_anim_ply_init.F
Chd|        PLYXFEM_MOD                   share/modules1/plyxfem_mod.F  
Chd|====================================================================
      SUBROUTINE W_ANIM_PLY(IXC,NUMELC_L,NODLOCAL,
     *                      NUMNOD_L,CEL,CEP,PROC)
      USE PLYXFEM_MOD
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
      INTEGER IXC(NIXC,*),NUMELC_L, NODLOCAL(*),
     *        NUMNOD_L,CEL(*),CEP(*),PROC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,P,ELEM,ND,NDSZ_L,ELSZ_L,ELPL,NCOUNT,EMPL
      INTEGER, DIMENSION(:), ALLOCATABLE :: ELNUM,ELEMIPT,ELEMXFEMID,
     *                                      NODNUM,NODEXTN,
     .                                      NODTAG,ELEMTAG,PLYSIZNG
      INTEGER GET_PLY_NOD
      EXTERNAL GET_PLY_NOD
C-----------------------------------------------
!     1d array
      ALLOCATE( NODTAG(NUMNOD_L),ELEMTAG(NUMELC_L) )
      ALLOCATE( PLYSIZNG(NPLYMAX) )
! ----------------------------------

      CALL WRITE_I_C(NPLYPART,1)
      CALL WRITE_I_C(INDX_PLY,NPLYPART)

      DO K=1,NPLYMAX
        NODTAG=0
        ELEMTAG=0

        PLYSIZNG(K)=PLYNOD(K)%PLYNUMNODS
C Premiere partie, sur 1 pli on tag les elements et
C les noeuds du proc
        DO I=1,PLYSHELL(K)%PLYNUMSHELL
           ELEM = PLYSHELL(K)%SHID(I)
           IF (CEP(ELEM)==PROC)THEN
             ELEMTAG(CEL(ELEM))=I
             ND = IXC(2,ELEM)
             NODTAG(NODLOCAL(ND))=ND
             ND = IXC(3,ELEM)
             NODTAG(NODLOCAL(ND))=ND
             ND = IXC(4,ELEM)
             NODTAG(NODLOCAL(ND))=ND
             ND = IXC(5,ELEM)
             NODTAG(NODLOCAL(ND))=ND
           ENDIF
        ENDDO
C On les compte pour preparer les tableaux a envoyer a
C L engine
        ELSZ_L=0
        NDSZ_L =0
        DO I=1,NUMELC_L
          IF (ELEMTAG(I)  > 0) ELSZ_L=ELSZ_L+1
        ENDDO
        DO I=1,NUMNOD_L
          IF (NODTAG(I) > 0) NDSZ_L=NDSZ_L+1
        ENDDO

C on prepare les tableaux a envoyer
        ELPL = 0
        ALLOCATE ( ELNUM(ELSZ_L),ELEMIPT(ELSZ_L),ELEMXFEMID(ELSZ_L) )

C Tableaux locaux elements
        DO I=1,NUMELC_L
          IF (ELEMTAG(I)  > 0) THEN
             ELPL=ELPL+1
             ND = ELEMTAG(I)
C
             ELNUM(ELPL)      = I
             ELEMIPT(ELPL)    = PLYSHELL(K)%SHELLIPT(ND)
             ELEMXFEMID(ELPL) = PLYSHELL(K)%SHELLID(ND)
          ENDIF
        ENDDO
C Tableaux locaux noeuds xfem
        NCOUNT = 0
        ALLOCATE ( NODNUM(NDSZ_L), NODEXTN(NDSZ_L) )
        DO I=1,NUMNOD_L
          IF (NODTAG(I) > 0) THEN
            NCOUNT = NCOUNT + 1
            ND=NODTAG(I)
C
            EMPL = GET_PLY_NOD(K,ND)
            NODNUM(NCOUNT) = I
            NODEXTN(NCOUNT) = PLYNOD(K)%PLYNODID(EMPL)
          ENDIF
        ENDDO
C Ecriture par ply des tableaux
C Coques ply
        CALL WRITE_I_C(ELSZ_L    , 1     )
        CALL WRITE_I_C(ELNUM     , ELSZ_L)
        CALL WRITE_I_C(ELEMIPT   , ELSZ_L)
        CALL WRITE_I_C(ELEMXFEMID, ELSZ_L)
C noeuds ply
        CALL WRITE_I_C(NDSZ_L    , 1     )
        CALL WRITE_I_C(NODNUM    , NDSZ_L)
        CALL WRITE_I_C(NODEXTN   , NDSZ_L)

C
        DEALLOCATE ( NODEXTN,NODNUM,ELNUM,ELEMIPT,ELEMXFEMID)
      ENDDO

        CALL WRITE_I_C(PLYSIZNG,NPLYMAX)
C id ply pid     
        CALL WRITE_I_C(IDPID_PLY,NPLYPART)    
! ----------------------------------    
!     1d array
      DEALLOCATE( NODTAG,ELEMTAG )
      DEALLOCATE( PLYSIZNG )
! ----------------------------------   
      RETURN
      END
