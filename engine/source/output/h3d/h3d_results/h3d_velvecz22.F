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
Chd|====================================================================
Chd|  H3D_VELVECZ22                 source/output/h3d/h3d_results/h3d_velvecz22.F
Chd|-- called by -----------
Chd|        H3D_NODAL_VECTOR              source/output/h3d/h3d_results/h3d_nodal_vector.F
Chd|-- calls ---------------
Chd|        H3D_WRITE_VECTOR              source/output/h3d/h3d_results/h3d_write_vector.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F
Chd|====================================================================
      SUBROUTINE H3D_VELVECZ22(ELBUF_TAB,IPARG,IPARI,IGRNOD,X,IXS,IXQ,ITAB,IFLG,
     .                         IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutines writes velocities & internal forces at face
C centers for coupling interface 22. Free nodes are used as
C marker to plot centroid vectors (see input card for grnod_id)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INITBUF_MOD
        USE ELBUFDEF_MOD
        USE I22BUFBRIC_MOD
        USE I22EDGE_MOD
        USE I22TRI_MOD
        USE GROUPDEF_MOD
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
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN)    :: IPARG(NPARG,*), IPARI(NPARI,*),IXS(NIXS,*),IXQ(NIXQ,*)
        INTEGER, INTENT(IN)    :: ITAB(NUMNOD),IFLG
        INTEGER, INTENT(IN)    :: IOK_PART(*),IS_WRITTEN_NODE(*)
        my_real, INTENT(INOUT) :: X(3,NUMNOD)
        my_real, INTENT(INOUT) :: NODAL_VECTOR(3,*)
        REAL R4
        TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
        TYPE (GROUP_)  , DIMENSION(NGRNOD) :: IGRNOD
C-----------------------------------------------
C   L o c a l   A r g u m e n t s
C-----------------------------------------------
        INTEGER :: ICELLM,NCELL,NBF,NBL,ICELL,NIN,NODE_ID,IB,NG,I
        INTEGER :: IAD0, NNODES, II, NFACE, J, K, IE,IGR
        my_real :: rho_cell, RHO(4)
        REAL,DIMENSION(:,:),ALLOCATABLE :: BUFFER
        LOGICAL :: lStillNode
        my_real VALUE(3)
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------
        NIN = 1
        IF(INT22==0)        RETURN
        IF(IPARI(82,NIN)==0)RETURN
C-----------------------------------------------
C   S o u r c e   C o d e
C-----------------------------------------------
        !---------------------------------------------------------!
        NBF         = 1
        NBL         = NB
        ALLOCATE(BUFFER(3,NUMNOD))
        BUFFER(:,:) = ZERO
        !---------------------------------------------------------!

        lStillNode = .TRUE.
        IGR         = IPARI(82,NIN)
        NNODES      = IGRNOD(IGR)%NENTITY
        IF(NNODES==0)RETURN
        II          = 1  ! start with the first node of the group
        DO IB=NBF,NBL
          IE                    = BRICK_LIST(NIN,IB)%ID
          ICELL                 =  0
          NCELL                 = BRICK_LIST(NIN,IB)%NBCUT
          DO WHILE (ICELL<=NCELL) ! loop on polyhedron {1:NCELL} U {9}
            ICELL               = ICELL +1
            IF (ICELL>NCELL .AND. NCELL/=0)ICELL=9
            IF(.NOT.lStillNode) CYCLE
            ! nFACE               = BRICK_LIST(NIN,IB)%NFACE_Cell(ICELL)
            DO J=1, 6
              IF(II>NNODES)THEN
                lStillNode  = .FALSE.
                print *, "** Warning inter22 : no more node in group to mark cell center"
                EXIT
              ENDIF
              NODE_ID           = IGRNOD(IGR)%ENTITY(II)
              IF(IFLG==1)THEN
                !velocity at faces
                BUFFER(1,NODE_ID) = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Vel(1)
                BUFFER(2,NODE_ID) = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Vel(2)
                BUFFER(3,NODE_ID) = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Vel(3)
              ELSEIF(IFLG==2)THEN
                !internal forces at face int(P.dS)
                BUFFER(1,NODE_ID) = -BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%F_FACE(1)
                BUFFER(2,NODE_ID) = -BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%F_FACE(2)
                BUFFER(3,NODE_ID) = -BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%F_FACE(3)
              ELSE
                BUFFER(1,NODE_ID) = ZERO
                BUFFER(2,NODE_ID) = ZERO
                BUFFER(3,NODE_ID) = ZERO
              ENDIF
              II                = II + 1 !next node
            ENDDO
          ENDDO !next ICELL
        ENDDO!next IB

        DO II=1,NNODES
          NODE_ID = IGRNOD(IGR)%ENTITY(II)
          X(1:3,NODE_ID) = ZERO
        ENDDO

        DO I=1,NUMNOD
          VALUE(1)=BUFFER(1,I)
          VALUE(2)=BUFFER(2,I)
          VALUE(3)=BUFFER(3,I)
          CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,
     .    VALUE)
        ENDDO

        DEALLOCATE(BUFFER)

C-----------------------------------------------
        RETURN
      END
