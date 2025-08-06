!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    create_h3d_output_per_part_mod   ../engine/source/output/h3d/h3d_build_fortran/create_h3d_output_per_part.F90
!||--- called by ------------------------------------------------------
!||    create_h3d_1d_scalar             ../engine/source/output/h3d/h3d_build_fortran/create_h3d_1d_scalar.F
!||    create_h3d_1d_tensor             ../engine/source/output/h3d/h3d_build_fortran/create_h3d_1d_tensor.F
!||    create_h3d_1d_torsor             ../engine/source/output/h3d/h3d_build_fortran/create_h3d_1d_torsor.F
!||    create_h3d_1d_vector             ../engine/source/output/h3d/h3d_build_fortran/create_h3d_1d_vector.F
!||    create_h3d_nodal_scalar          ../engine/source/output/h3d/h3d_build_fortran/create_h3d_nodal_scalar.F
!||    create_h3d_nodal_tensor          ../engine/source/output/h3d/h3d_build_fortran/create_h3d_nodal_tensor.F
!||    create_h3d_nodal_vector          ../engine/source/output/h3d/h3d_build_fortran/create_h3d_nodal_vector.F
!||    create_h3d_quad_scalar           ../engine/source/output/h3d/h3d_build_fortran/create_h3d_quad_scalar.F
!||    create_h3d_quad_tensor           ../engine/source/output/h3d/h3d_build_fortran/create_h3d_quad_tensor.F
!||    create_h3d_quad_vector           ../engine/source/output/h3d/h3d_build_fortran/create_h3d_quad_vector.F
!||    create_h3d_shell_scalar          ../engine/source/output/h3d/h3d_build_fortran/create_h3d_shell_scalar.F
!||    create_h3d_shell_tensor          ../engine/source/output/h3d/h3d_build_fortran/create_h3d_shell_tensor.F
!||    create_h3d_shell_vector          ../engine/source/output/h3d/h3d_build_fortran/create_h3d_shell_vector.F
!||    create_h3d_skin_scalar           ../engine/source/output/h3d/h3d_build_fortran/create_h3d_skin_scalar.F
!||    create_h3d_skin_tensor           ../engine/source/output/h3d/h3d_build_fortran/create_h3d_skin_tensor.F
!||    create_h3d_skin_vector           ../engine/source/output/h3d/h3d_build_fortran/create_h3d_skin_vector.F
!||    create_h3d_solid_scalar          ../engine/source/output/h3d/h3d_build_fortran/create_h3d_solid_scalar.F
!||    create_h3d_solid_tensor          ../engine/source/output/h3d/h3d_build_fortran/create_h3d_solid_tensor.F
!||    create_h3d_solid_vector          ../engine/source/output/h3d/h3d_build_fortran/create_h3d_solid_vector.F
!||    create_h3d_sph_scalar            ../engine/source/output/h3d/h3d_build_fortran/create_h3d_sph_scalar.F
!||    create_h3d_sph_tensor            ../engine/source/output/h3d/h3d_build_fortran/create_h3d_sph_tensor.F
!||====================================================================
      module create_h3d_output_per_part_mod
      contains
! ======================================================================================================================
! \brief creation of h3d output per part or set of parts
! \details creation of h3d output per part or set of parts
! ======================================================================================================================
!||====================================================================
!||    create_h3d_output_per_part   ../engine/source/output/h3d/h3d_build_fortran/create_h3d_output_per_part.F90
!||--- called by ------------------------------------------------------
!||    create_h3d_1d_scalar         ../engine/source/output/h3d/h3d_build_fortran/create_h3d_1d_scalar.F
!||    create_h3d_1d_tensor         ../engine/source/output/h3d/h3d_build_fortran/create_h3d_1d_tensor.F
!||    create_h3d_1d_torsor         ../engine/source/output/h3d/h3d_build_fortran/create_h3d_1d_torsor.F
!||    create_h3d_1d_vector         ../engine/source/output/h3d/h3d_build_fortran/create_h3d_1d_vector.F
!||    create_h3d_nodal_scalar      ../engine/source/output/h3d/h3d_build_fortran/create_h3d_nodal_scalar.F
!||    create_h3d_nodal_tensor      ../engine/source/output/h3d/h3d_build_fortran/create_h3d_nodal_tensor.F
!||    create_h3d_nodal_vector      ../engine/source/output/h3d/h3d_build_fortran/create_h3d_nodal_vector.F
!||    create_h3d_quad_scalar       ../engine/source/output/h3d/h3d_build_fortran/create_h3d_quad_scalar.F
!||    create_h3d_quad_tensor       ../engine/source/output/h3d/h3d_build_fortran/create_h3d_quad_tensor.F
!||    create_h3d_quad_vector       ../engine/source/output/h3d/h3d_build_fortran/create_h3d_quad_vector.F
!||    create_h3d_shell_scalar      ../engine/source/output/h3d/h3d_build_fortran/create_h3d_shell_scalar.F
!||    create_h3d_shell_tensor      ../engine/source/output/h3d/h3d_build_fortran/create_h3d_shell_tensor.F
!||    create_h3d_shell_vector      ../engine/source/output/h3d/h3d_build_fortran/create_h3d_shell_vector.F
!||    create_h3d_skin_scalar       ../engine/source/output/h3d/h3d_build_fortran/create_h3d_skin_scalar.F
!||    create_h3d_skin_tensor       ../engine/source/output/h3d/h3d_build_fortran/create_h3d_skin_tensor.F
!||    create_h3d_skin_vector       ../engine/source/output/h3d/h3d_build_fortran/create_h3d_skin_vector.F
!||    create_h3d_solid_scalar      ../engine/source/output/h3d/h3d_build_fortran/create_h3d_solid_scalar.F
!||    create_h3d_solid_tensor      ../engine/source/output/h3d/h3d_build_fortran/create_h3d_solid_tensor.F
!||    create_h3d_solid_vector      ../engine/source/output/h3d/h3d_build_fortran/create_h3d_solid_vector.F
!||    create_h3d_sph_scalar        ../engine/source/output/h3d/h3d_build_fortran/create_h3d_sph_scalar.F
!||    create_h3d_sph_tensor        ../engine/source/output/h3d/h3d_build_fortran/create_h3d_sph_tensor.F
!||--- uses       -----------------------------------------------------
!||    groupdef_mod                 ../common_source/modules/groupdef_mod.F
!||    h3d_mod                      ../engine/share/modules/h3d_mod.F
!||    names_and_titles_mod         ../common_source/modules/names_and_titles_mod.F
!||====================================================================
      subroutine create_h3d_output_per_part(                                      &         
                 n_h3d_part,h3d_data,id_input,lipart1,npart,ipart,ngrpart,igrpart)
 !-----------------------------------------------
 !   M o d u l e s
 !-----------------------------------------------
        use h3d_mod
        use names_and_titles_mod, only : ncharline100
        use groupdef_mod
 !-----------------------------------------------
 !   I m p l i c i t   T y p e s
 !-----------------------------------------------
         implicit none 
 !-----------------------------------------------
 !   D u m m y   A r g u m e n t s
 !-----------------------------------------------
      integer, intent(in)                          :: n_h3d_part           !< number of parts in the h3d output
      type(h3d_database), intent(inout)            :: h3d_data             !< H3D output structure
      integer, intent(in)                          :: id_input             !< id of the input
      integer, intent(in)                          :: lipart1              !< length of part array
      integer, intent(in)                          :: npart                !< number of parts
      integer, intent(in)                          :: ipart(lipart1,npart) !< part array
      integer, intent(in)                          :: ngrpart              !< number of part groups
      type(group_), intent(in)                     :: igrpart(ngrpart)     !< part group structure
 !-----------------------------------------------
 !   L o c a l   V a r i a b l e s
 !-----------------------------------------------
      integer :: i,j,k,l,m,index
 !=======================================================================
 !------------------------------------------
      
      do j=1,n_h3d_part
        if(h3d_data%input_list(id_input)%part_list(j) > 0) then
          ! find the part with this id
          do i=1,npart
            if(h3d_data%input_list(id_input)%part_list(j) == ipart(4,i)) then
                h3d_data%output_list(h3d_data%n_outp_h3d)%part(i) = 1
            endif
          enddo
        else
          ! negative value refers to a part group
          l = -h3d_data%input_list(id_input)%part_list(j)  ! get positive index of part group
            index = 0
            do i=1,ngrpart
              if(igrpart(i)%id == l) then
                index = i
                exit
              endif
            enddo
            if(index > 0) then
              do i=1,igrpart(index)%nentity
                k = igrpart(index)%entity(i)  ! get the part id from the group
                ! find the part with this id
                do m=1,npart
                  if (ipart(4,m) == k) then
                    h3d_data%output_list(h3d_data%n_outp_h3d)%part(m) = 1
                    exit
                  endif
                enddo
              enddo
            endif
        endif
      enddo

! ----------------------------------------------------------------------------------------------------------------------
        end subroutine create_h3d_output_per_part
! ----------------------------------------------------------------------------------------------------------------------
      end module create_h3d_output_per_part_mod
