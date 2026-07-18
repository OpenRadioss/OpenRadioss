!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
!Chd|====================================================================
!Chd|  FINDHEX8FROMSURF               source/elements/solid/solid_q1np/findhex8fromsurf.F90
!Chd|====================================================================
!=======================================================================
!   Map surface segments to HEX8 elements by matching any face
!
!   This routine finds the HEX8 element that corresponds to a given
!   surface segment by matching the segment 4 nodes to any of the
!   six faces of HEX8 elements. Returns element index and the four
!   node IDs of the opposite face (bulk side), ordered so each returned
!   bulk node corresponds to the same corner position as the input
!   NODES_SURF array.
!
!   Algorithm Overview:
!     - Iterate through all HEX8 elements; for each, check all six faces.
!     - For each face (4 nodes), match the input surface segment’s four nodes
!       by comparing node IDs.
!     - When all four match a face, return element index and fill NODES_BULK
!       with the 4 nodes of the opposite face. If no match, IEL_HEX8 = 0.
!   HEX8 face convention (IXS(2:9)=nodes 1-8): Face 1: 1,2,3,4; Face 2: 5,6,7,8;
!   Face 3: 1,2,6,5; Face 4: 2,3,7,6; Face 5: 3,4,8,7; Face 6: 4,1,5,8.
!
!=======================================================================
!||====================================================================
!||    findhex8fromsurface_mod   ../starter/source/elements/solid/solid_q1np/q1np_findhex8fromsurface.F90
!||--- called by ------------------------------------------------------
!||    genq1np_mod               ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||====================================================================
      module findhex8fromsurface_mod
        implicit none
      contains
!
!=======================================================================
!||====================================================================
!||    findhex8fromsurf                    ../starter/source/elements/solid/solid_q1np/q1np_findhex8fromsurface.F90
!||--- called by ------------------------------------------------------
!||    genq1np                             ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||    q1np_select_global_cp_orientation   ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||--- calls      -----------------------------------------------------
!||    iface                               ../starter/source/ale/ale3d/iface.F
!||====================================================================
        subroutine findhex8fromsurf(nodes_surf, ixs, iel_hex8, &
     &      nodes_bulk, nixs, numels)
!-----------------------------------------------------------------------
!     NODES_SURF  - ordered surface segment node IDs (4 corner nodes)
!     IXS         - HEX8 element connectivity array
!     IEL_HEX8    - HEX8 element index (output, 0 if not found)
!     NODES_BULK  - output: 4 node IDs of the opposite face, ordered
!                   to match NODES_SURF corner positions
!     NIXS        - leading dimension of IXS
!     NUMELS      - number of HEX8 elements to scan
!-----------------------------------------------------------------------
          implicit none
!-----------------------------------------------------------------------
!     Arguments
!-----------------------------------------------------------------------
          integer, intent(in) :: nodes_surf(4)
          integer, intent(in) :: nixs, numels
          integer, intent(in) :: ixs(nixs, numels)
          integer, intent(inout) :: iel_hex8
          integer, intent(inout) :: nodes_bulk(4)
!-----------------------------------------------------------------------
!     Local variables
!-----------------------------------------------------------------------
          integer :: iel, i, j, iface, ifopp
          integer :: nodes_hex8(4)   ! Current face node IDs from HEX8
          integer :: nodes_opp(4)    ! Opposite face node IDs from HEX8
          integer :: match_count     ! Number of matching nodes found
          logical :: found_pos
!     HEX8 faces: IXS(2:9)=nodes 1-8. Face 1: 1,2,3,4; Face 2: 5,6,7,8;
!     Face 3: 1,2,6,5; Face 4: 2,3,7,6; Face 5: 3,4,8,7; Face 6: 4,1,5,8
!     IXS row index for each face's 4 nodes
          integer, parameter :: face_ixs(4,6) = reshape( (/ &
     &      2,3,4,5, 6,7,8,9, 2,3,7,6, 3,4,8,7, 4,5,9,8, 5,2,6,9 /), (/4,6/) )
!     Opposite face index for each face
          integer, parameter :: opposite(6) = (/ 2,1,5,6,3,4 /)
!     Same-corner pairing between each matched face and its opposite face.
!     Faces 3<->5 and 4<->6 need a flip because the stored local face order
!     follows outward normals, not through-thickness corner correspondence.
          integer, parameter :: opp_pair(4,6) = reshape( (/ &
     &      1,2,3,4, 1,2,3,4, 2,1,4,3, 2,1,4,3, 2,1,4,3, 2,1,4,3 /), (/4,6/) )
!=======================================================================
!   1. Initialize: no element found yet
!=======================================================================
          iel_hex8 = 0
!=======================================================================
!   2. Loop through all HEX8 elements and all six faces
!=======================================================================
          do iel = 1, numels
            face_loop: do iface = 1, 6
!           Extract the 4 nodes of this face
              do i = 1, 4
                nodes_hex8(i) = ixs(face_ixs(i,iface), iel)
              end do
!           Match surface segment nodes to this face (ID):
!           every surface node must be found among this face's nodes
              match_count = 0
              do i = 1, 4
                found_pos = .false.
                do j = 1, 4
                  if (nodes_surf(i) == nodes_hex8(j)) then
                    found_pos = .true.
                    exit
                  end if
                end do
!               No match for this surface node: skip to next face
                if (.not. found_pos) cycle face_loop
                match_count = match_count + 1
              end do
              if (match_count >= 4) then
                iel_hex8 = iel
                ifopp = opposite(iface)
                do i = 1, 4
                  nodes_opp(i) = ixs(face_ixs(i,ifopp), iel)
                end do
                do i = 1, 4
                  found_pos = .false.
                  do j = 1, 4
                    if (nodes_surf(i) == nodes_hex8(j)) then
                      nodes_bulk(i) = nodes_opp(opp_pair(j,iface))
                      found_pos = .true.
                      exit
                    end if
                  end do
                  if (.not. found_pos) then
                    iel_hex8 = 0
                    return
                  end if
                end do
                return
              end if
            end do face_loop
          end do
!=======================================================================
!   No matching element found
!=======================================================================
          iel_hex8 = 0
          return
        end subroutine findhex8fromsurf
!
      end module findhex8fromsurface_mod

