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
Chd|  SORT_SURF                     source/groups/sort_surf.F     
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE SORT_SURF(IGRSURF,IXS,IXC,IXTG,IXQ)
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
      INTEGER IXS(NIXS,*),IXC(NIXC,*),IXTG(NIXTG,*),IXQ(NIXQ,*)
!
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IAD_L,NN,ELEM,ELEM_G,N1,N2,N3,N4,ITYP,K,
     *        IADG2,IADIBUF,IT,IAD
      INTEGER, DIMENSION(:), ALLOCATABLE :: IBUFSSG_TRI,IWORK,INDEX
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ITRI

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

          ITYP = IGRSURF(K)%ELTYP(I)
          ELEM = IGRSURF(K)%ELEM(I)

          ITRI(1,I) = ITYP
          SELECT CASE (ITYP)
            CASE ( 0 ) 
               ITRI(2,I) = 0
            CASE ( 1 )
               ITRI(2,I) = IXS(NIXS,ELEM)
            CASE ( 2 )
               ITRI(2,I) = IXQ(NIXQ,ELEM)
            CASE ( 3 )
               ITRI(2,I) = IXC(NIXC,ELEM)
            CASE ( 7 )
               ITRI(2,I) = IXTG(NIXTG,ELEM)
          END SELECT

          INDEX(I) = I 

         ENDDO 

         CALL MY_ORDERS(0,IWORK,ITRI,INDEX,NN,2)

         DO I=1,NN
          IT = INDEX(I)
          IGRSURF(K)%NODES(I,1) = IBUFSSG_TRI(6*(IT-1)+1)
          IGRSURF(K)%NODES(I,2) = IBUFSSG_TRI(6*(IT-1)+2)
          IGRSURF(K)%NODES(I,3) = IBUFSSG_TRI(6*(IT-1)+3)
          IGRSURF(K)%NODES(I,4) = IBUFSSG_TRI(6*(IT-1)+4)
          IGRSURF(K)%ELTYP(I)	= IBUFSSG_TRI(6*(IT-1)+5)
          IGRSURF(K)%ELEM(I)	= IBUFSSG_TRI(6*(IT-1)+6)
         ENDDO

         DEALLOCATE(IBUFSSG_TRI)
         DEALLOCATE(ITRI)
         DEALLOCATE(INDEX)

      ENDDO ! DO K=1,NSURF
!---
      RETURN
      END
