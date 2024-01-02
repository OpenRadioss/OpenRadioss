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
Chd|  FVBRIC01                      source/airbag/fvbric01.F      
Chd|-- called by -----------
Chd|        INIT_MONVOL                   source/airbag/init_monvol.F   
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE FVBRIC01(IBUF, ELEM, NTGI, NB_NODE, NBRIC, IXS, 
     .     TBRIC, TFAC)
C-----------------------------------------------  
C     D e s c r i p t i o n
C-----------------------------------------------     
C     Find brick faces connected to internal airbag surface.
C     Faces are flagged -2 in th TFAC array.
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"     
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C     NIXS
C     NUMELS
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NBRIC, NTGI, NB_NODE, TBRIC(2, NBRIC), IXS(NIXS, *)
      INTEGER, INTENT(IN) :: IBUF(*), ELEM(3, NTGI)
      INTEGER, INTENT(INOUT) :: TFAC(12, NBRIC)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: II, KK, KKK, KKKK, JJ, CONNECT_MAX, NODEID, COUNT, ELID, ELID1, NODE_LIST(8), SURFELE
      INTEGER :: NSEG, IFACE, ITYPE, NNODE, NSURFNODE, SUM, NNF
      INTEGER, DIMENSION(:), ALLOCATABLE :: IFLAG
      INTEGER, DIMENSION(:, :), ALLOCATABLE :: N_E_CONNECT, N_E_CONNECT_LOCID
      INTEGER, TARGET :: REDIRT(4), REDIRP(6), REDIRB(8), REDIRPY(5), 
     .     NOD8(6), NOD3(4), NOD6(5), NOD5(5),
     .     FAC8(4,6), FAC4(3,4), FAC6(4,5), FAC5(4,5), NFACE(4)
      INTEGER, DIMENSION(:), POINTER :: REDIR, NOD
      INTEGER, DIMENSION(:,:), POINTER :: FAC
      DATA FAC4 /1,5,3,
     .           3,5,6,
     .           6,5,1,
     .           1,3,6/
      DATA FAC8 /1,4,3,2,
     .           5,6,7,8,
     .           1,2,6,5,
     .           2,3,7,6,
     .           3,4,8,7,
     .           4,1,5,8/
      DATA FAC6 /1,3,2,0,
     .           5,6,7,0,
     .           1,2,6,5,
     .           2,3,7,6,
     .           3,4,8,7/
      DATA NOD6 /3,3,4,4,4/
      DATA NOD8 /4,4,4,4,4,4/
      DATA NOD3 /3,3,3,3/
      DATA FAC5 /1,2,5,0,
     .           2,3,5,0,
     .           3,4,5,0,
     .           4,1,5,0,
     .           1,4,3,2/
      DATA NOD5 /3,3,3,3,4/
      DATA NFACE/6,4,5,5/
      
      LOGICAL :: FACE_OK
C-----------------------------------------------
C     S o u r c e  l i n e s
C-----------------------------------------------
C     TETRA indirection
      REDIRT(1)=1
      REDIRT(2)=3
      REDIRT(3)=5
      REDIRT(4)=6
C     BRICK indirection
      DO KK = 1, 8
         REDIRB(KK) = KK
      ENDDO
C     PENTA indirection
      REDIRP(1)=1
      REDIRP(2)=2
      REDIRP(3)=3
      REDIRP(4)=5
      REDIRP(5)=6
      REDIRP(6)=7
C     PYRAMIDE indirection
      DO KK = 1, 5
         REDIRPY(KK) = KK
      ENDDO
     

C     Start 

      NSEG = NTGI

C     Count maximum connectivity
      ALLOCATE(IFLAG(NB_NODE))
      IFLAG(1:NB_NODE) = 0
      DO II = 1, NBRIC
         ELID = TBRIC(1, II)
         ITYPE = TBRIC(2, II)
         SELECT CASE(ITYPE)
         CASE(1) 
C     Brick
            NNODE = 8
            REDIR => REDIRB(1:8)
         CASE(2)
C     Tetra
            NNODE = 4
            REDIR => REDIRT(1:4)
         CASE(3)
C     Penta
            NNODE = 6
            REDIR => REDIRP(1:6)
         CASE(4)
C     Pyramide
            NNODE = 5
            REDIR => REDIRPY(1:5)
         CASE DEFAULT
            CALL ARRET(2)
         END SELECT
         DO KK = 1, NNODE
            NODEID = IXS(1 + REDIR(KK), ELID)
            IFLAG(NODEID) = IFLAG(NODEID) + 1
         ENDDO
      ENDDO

      CONNECT_MAX = MAXVAL(IFLAG(1:NB_NODE))

C     Node element connectivity
      ALLOCATE(N_E_CONNECT(NB_NODE, CONNECT_MAX + 1))
      ALLOCATE(N_E_CONNECT_LOCID(NB_NODE, CONNECT_MAX + 1))
      N_E_CONNECT(1:NB_NODE, 1:CONNECT_MAX + 1) = 0
      N_E_CONNECT_LOCID(1:NB_NODE, 1:CONNECT_MAX + 1) = 0
      DO II = 1, NBRIC
         ELID = TBRIC(1, II)
         ITYPE = TBRIC(2, II)
         SELECT CASE(ITYPE)
         CASE(1) 
C     Brick
            NNODE = 8
            REDIR => REDIRB(1:8)
         CASE(2)
C     Tetra
            NNODE = 4
            REDIR => REDIRT(1:4)
         CASE(3)
C     Penta
            NNODE = 6
            REDIR => REDIRP(1:6)
         CASE(4)
C     Pyramide
            NNODE = 5
            REDIR => REDIRPY(1:5)
         CASE DEFAULT
            CALL ARRET(2)
         END SELECT
         DO KK = 1, NNODE
            NODEID = IXS(1 + REDIR(KK), ELID)
            COUNT = N_E_CONNECT(NODEID, 1)
            COUNT = COUNT + 1
            N_E_CONNECT(NODEID, 1) = COUNT
            N_E_CONNECT(NODEID, COUNT + 1) = ELID
            N_E_CONNECT_LOCID(NODEID, COUNT + 1) = II
         ENDDO
      ENDDO

C     Reset IFLAG to 0
      IFLAG(1:NB_NODE) = 0
      COUNT = 0
C     Find bricks connected to surface element
      DO II = 1, NSEG
         NSURFNODE = 3
C     Flag surface element nodes
         DO KK = 1, NSURFNODE
            KKK = IBUF(ELEM(KK, II))
            IFLAG(KKK) = 1
         ENDDO
         DO KK = 1, NSURFNODE
            KKK = IBUF(ELEM(KK, II))
            DO JJ = 1, N_E_CONNECT(KKK, 1)
               ELID = N_E_CONNECT(KKK, 1 + JJ)
               ITYPE = TBRIC(2, II)
               SELECT CASE(ITYPE)
               CASE(1) 
C     Brick
                  NNODE = 8
                  REDIR => REDIRB(1:8)
                  FAC => FAC8(1:4, 1:6)
                  NOD => NOD8(1:6)
               CASE(2)
C     Tetra
                  NNODE = 4
                  REDIR => REDIRT(1:4)
                  FAC => FAC4(1:3, 1:4)
                  NOD => NOD3(1:4)
               CASE(3)
C     Penta
                  NNODE = 6
                  REDIR => REDIRP(1:6)
                  FAC => FAC6(1:4, 1:5)
                  NOD => NOD6(1:5)
               CASE(4)
C     Pyramide
                  NNODE = 5
                  REDIR => REDIRPY(1:5)
                  FAC => FAC5(1:4, 1:5)
                  NOD => NOD5(1:5)
               CASE DEFAULT
                  CALL ARRET(2)
               END SELECT
               SUM = 0
               DO KKKK = 1, NNODE
                  NODEID = IXS(1 + REDIR(KKKK), ELID)
                  SUM = SUM + IFLAG(NODEID)
               ENDDO
               IF (SUM == NSURFNODE) THEN
C     Find the corresponding face
                  FACE_OK = .FALSE.
                  DO IFACE = 1, NFACE(ITYPE)
                     SUM = 0
                     DO KKKK = 1, NOD(IFACE)
                        SUM = SUM + IFLAG(IXS(1 + FAC(KKKK, IFACE), ELID))
                     ENDDO
                     IF (SUM == NSURFNODE) THEN
                        FACE_OK = .TRUE.
                     ENDIF
                     IF (FACE_OK) THEN
                        EXIT
                     ENDIF
                  ENDDO
                  IF (.NOT. FACE_OK) THEN
                     CALL ARRET(2)
                  ELSE
                     ELID1 = N_E_CONNECT_LOCID(KKK, 1 + JJ)
                     TFAC(2 * IFACE - 1, ELID1) = -2
                     COUNT = COUNT + 1
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
C     Unflag surface element nodes
         DO KK = 1, NSURFNODE
            KKK = IBUF(ELEM(KK, II))
            IFLAG(KKK) = 0               
         ENDDO                     
      ENDDO

C     Memory deallocation
      DEALLOCATE(IFLAG)
      DEALLOCATE(N_E_CONNECT)
      DEALLOCATE(N_E_CONNECT_LOCID)
      END SUBROUTINE
