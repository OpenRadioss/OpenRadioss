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
      !||    sort_surf      ../starter/source/groups/sort_surf.F
      !||--- called by ------------------------------------------------------
      !||    lectur         ../starter/source/starter/lectur.F
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      SUBROUTINE SORT_SURF(IGRSURF,IXS,IXC,IXTG,IXQ,IXP,IXR,IXT,KXX,NIXX)
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
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NIXX !< array size
      INTEGER,INTENT(IN) :: IXS(NIXS,NUMELS)    !< elem buffer for /BRIC entities
      INTEGER,INTENT(IN) :: IXC(NIXC,NUMELC)    !< elem buffer for /SHELL entities
      INTEGER,INTENT(IN) :: IXTG(NIXTG,NUMELTG) !< elem buffer for /SH3N (3d) or /TRIA (2d) entities
      INTEGER,INTENT(IN) :: IXQ(NIXQ,NUMELQ)    !< elem buffer for /QUAD entities
      INTEGER,INTENT(IN) :: IXP(NIXP,NUMELP)    !< elem buffer for /BEAM entities
      INTEGER,INTENT(IN) :: IXR(NIXR,NUMELR)    !< elem buffer for /SPRING entities
      INTEGER,INTENT(IN) :: IXT(NIXT,NUMELT)    !< elem buffer for /TRUSS entities
      INTEGER,INTENT(IN) :: KXX(NIXX,NUMELX)    !< elem buffer for /XELEM entities
      TYPE(SURF_),INTENT(INOUT), DIMENSION(NSURF) :: IGRSURF !< data structure for surfaces
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IAD_L,NN,ELEM,ELEM_G,N1,N2,N3,N4,ITYP,K,IADG2,IADIBUF,IT,IAD
      INTEGER, DIMENSION(:), ALLOCATABLE :: IBUFSSG_TRI,IWORK,INDEX
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ITRI
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      ALLOCATE (IWORK(80000))
        
      DO K=1,NSURF
        NN  = IGRSURF(K)%NSEG

        ALLOCATE (IBUFSSG_TRI(NISX*NN))
        ALLOCATE (ITRI(2,NN))
        ALLOCATE (INDEX(3*NN))

        DO I=1,NN
          IBUFSSG_TRI(6*(I-1)+1) = IGRSURF(K)%NODES(I,1)
          IBUFSSG_TRI(6*(I-1)+2) = IGRSURF(K)%NODES(I,2)
          IBUFSSG_TRI(6*(I-1)+3) = IGRSURF(K)%NODES(I,3)
          IBUFSSG_TRI(6*(I-1)+4) = IGRSURF(K)%NODES(I,4)
          IBUFSSG_TRI(6*(I-1)+5) = IGRSURF(K)%ELTYP(I)
          IBUFSSG_TRI(6*(I-1)+6) = IGRSURF(K)%ELEM(I)

          ELEM = IGRSURF(K)%ELEM(I)
          ITYP = IGRSURF(K)%ELTYP(I)
          ! ITYP = 0  - surf of segments
          ! ITYP = 1  - surf of solids
          ! ITYP = 2  - surf of quads
          ! ITYP = 3  - surf of SH4N
          ! ITYP = 4  - line of trusses
          ! ITYP = 5  - line of beams
          ! ITYP = 6  - line of springs
          ! ITYP = 7  - surf of SH3N
          ! ITYP = 8  - line of XELEM (nstrand element)
          ! ITYP = 101 - ISOGEOMETRIC

          ITRI(1,I) = ITYP
          ITRI(2,I) = 0

          SELECT CASE (ITYP)
            CASE ( 0 ) 
               ITRI(2,I) = 0
            CASE ( 1 )
               ITRI(2,I) = IXS(NIXS,ELEM)
            CASE ( 2 )
               ITRI(2,I) = IXQ(NIXQ,ELEM)
            CASE ( 3 )
               ITRI(2,I) = IXC(NIXC,ELEM)
            CASE ( 4 )
               ITRI(2,I) = IXT(NIXT,ELEM)
            CASE ( 5 )
               ITRI(2,I) = IXP(NIXP,ELEM)
            CASE ( 6 )
               ITRI(2,I) = IXR(NIXR,ELEM)
            CASE ( 7 )
               ITRI(2,I) = IXTG(NIXTG,ELEM)
            CASE ( 8 )
               ITRI(2,I) = KXX(NIXX,ELEM)
          END SELECT

          INDEX(I) = I 

        ENDDO ! next NN

         CALL MY_ORDERS(0,IWORK,ITRI,INDEX,NN,2)

         DO I=1,NN
           IT = INDEX(I)
           IGRSURF(K)%NODES(I,1) = IBUFSSG_TRI(6*(IT-1)+1)
           IGRSURF(K)%NODES(I,2) = IBUFSSG_TRI(6*(IT-1)+2)
           IGRSURF(K)%NODES(I,3) = IBUFSSG_TRI(6*(IT-1)+3)
           IGRSURF(K)%NODES(I,4) = IBUFSSG_TRI(6*(IT-1)+4)
           IGRSURF(K)%ELTYP(I)  = IBUFSSG_TRI(6*(IT-1)+5)
           IGRSURF(K)%ELEM(I)  = IBUFSSG_TRI(6*(IT-1)+6)
         ENDDO

         DEALLOCATE(IBUFSSG_TRI)
         DEALLOCATE(ITRI)
         DEALLOCATE(INDEX)

      ENDDO ! DO K=1,NSURF
!---
      RETURN
      END
