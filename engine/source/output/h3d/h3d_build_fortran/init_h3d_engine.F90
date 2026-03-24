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
!||====================================================================
!||    init_h3d_engine_mod   ../engine/source/output/h3d/h3d_build_fortran/init_h3d_engine.F90
!||--- called by ------------------------------------------------------
!||    radioss2              ../engine/source/engine/radioss2.F
!||====================================================================
      module init_h3d_engine_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Initialize H3D output data structures and allocations
!! \details This subroutine handles all H3D-related initialization including:
!!          - Calling PRELECH3D and LECH3D for H3D data reading
!!          - Output information printing
!!          - Sensor validation
!!          - Interface output allocation (skid lines, frictional energy)
!||====================================================================
!||    init_h3d_engine    ../engine/source/output/h3d/h3d_build_fortran/init_h3d_engine.F90
!||--- called by ------------------------------------------------------
!||    radioss2           ../engine/source/engine/radioss2.F
!||--- calls      -----------------------------------------------------
!||    ancmsg             ../engine/source/output/message/message.F
!||    arret              ../engine/source/system/arret.F
!||    lech3d             ../engine/source/output/h3d/h3d_build_fortran/lech3d.F
!||    prelech3d          ../engine/source/output/h3d/h3d_build_fortran/prelech3d.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod       ../common_source/modules/constant_mod.F
!||    elbufdef_mod       ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    groupdef_mod       ../common_source/modules/groupdef_mod.F
!||    h3d_mod            ../engine/share/modules/h3d_mod.F
!||    loads_mod          ../common_source/modules/loads/loads_mod.F90
!||    matparam_def_mod   ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    message_mod        ../engine/share/message_module/message_mod.F
!||    multi_fvm_mod      ../common_source/modules/ale/multi_fvm_mod.F90
!||    my_alloc_mod       ../common_source/tools/memory/my_alloc.F90
!||    output_mod         ../common_source/modules/output/output_mod.F90
!||    pblast_mod         ../common_source/modules/loads/pblast_mod.F90
!||    prelech3d_mod      ../engine/source/output/h3d/h3d_build_fortran/prelech3d.F90
!||    sensor_mod         ../common_source/modules/sensor_mod.F90
!||    stack_mod          ../engine/share/modules/stack_mod.F
!||====================================================================
        subroutine init_h3d_engine(output, geo, igeo, ipm, ipart, h3d_data, multi_fvm, ipari, iparg, &
                                    tag_skins6, mds_label, mds_output_table, mds_nmat, max_depvar, &
                                    mds_ndepsvar, elbuf_str, stack, ibcl, iloadp, lloadp, loads, &
                                    mat_param, pblast, igrpart, npc, pld, snpc, stf, sensors, &
                                    sensor_tab, pskids, nintskidold, ninterskid, nintstamp, numnod, &
                                    numnodg, ninter, numsphg, &
                                    npropg, npropgi, npropmi, lipart1, npari, nparg, &
                                    ngroup, numels, sibcl, nloadp, sizloadp, slloadp, ngrpart, nsensor, &
                                    ispmd, iout, mcheck, &
                                    numelcg, numeltgg, numelsg, numeltrg, numelpg, numelrg, &
                                    numelqg, numsking, numgeo, nummat, numply)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use H3D_MOD
          use OUTPUT_MOD
          use MULTI_FVM_MOD
          use ELBUFDEF_MOD
          use STACK_MOD
          use LOADS_MOD
          use MATPARAM_DEF_MOD
          use PBLAST_MOD
          use GROUPDEF_MOD
          use SENSOR_MOD
          use PRELECH3D_MOD
          use MY_ALLOC_MOD
          use MESSAGE_MOD
          use CONSTANT_MOD, only : ZERO, EP30
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      ! Output structure
      type(OUTPUT_), intent(inout) :: output                                !< Output data structure
      
      ! Geometry and property dimensions
      integer, intent(in) :: npropg                                         !< Number of geometry properties
      integer, intent(in) :: npropgi                                        !< Number of integer geometry properties
      integer, intent(in) :: npropmi                                        !< Number of integer material properties
      integer, intent(in) :: lipart1                                        !< Size of ipart array
      integer, intent(in) :: npari                                          !< Number of integer interface properties
      integer, intent(in) :: nparg                                          !< Number of real interface properties
      
      ! Material and output dimensions
      integer, intent(in) :: mds_nmat                                       !< Number of materials for MDS output
      integer, intent(in) :: max_depvar                                     !< Maximum number of dependent variables
      integer, intent(in) :: snpc                                           !< Size of npc array
      integer, intent(in) :: stf                                            !< Size of pld array
      integer, intent(in) :: numsphg                                        !< Global number of SPH particles
      
      ! Array dimension parameters
      integer, intent(in) :: ngroup                                         !< Number of groups
      integer, intent(in) :: numels                                         !< Number of skin elements
      integer, intent(in) :: sibcl                                          !< Size of boundary condition array
      integer, intent(in) :: nloadp                                         !< Number of load cases
      integer, intent(in) :: sizloadp                                       !< Size of load pointer array
      integer, intent(in) :: slloadp                                        !< Size of load list pointer array
      integer, intent(in) :: ngrpart                                        !< Number of interface groups
      integer, intent(in) :: nsensor                                        !< Number of sensors
      
      ! Interface and stamp dimensions
      integer, intent(in) :: nintskidold                                    !< Old number of skid interfaces
      integer, intent(in) :: nintstamp                                      !< Number of stamp interfaces
      integer, intent(in) :: numnod                                         !< Number of nodes
      integer, intent(in) :: numnodg                                        !< Global number of nodes
      integer, intent(in) :: ninter                                         !< Number of interfaces
      
      ! MPI and output control
      integer, intent(in) :: ispmd                                          !< MPI rank
      integer, intent(in) :: iout                                           !< Output file unit
      integer, intent(in) :: mcheck                                         !< Check mode flag
      
      ! Element counts
      integer, intent(in) :: numelcg                                        !< Global number of shell elements
      integer, intent(in) :: numeltgg                                       !< Global number of thick shell elements
      integer, intent(in) :: numelsg                                        !< Global number of solid elements
      integer, intent(in) :: numeltrg                                       !< Global number of truss elements
      integer, intent(in) :: numelpg                                        !< Global number of beam elements
      integer, intent(in) :: numelrg                                        !< Global number of spring elements
      integer, intent(in) :: numelqg                                        !< Global number of quad elements
      integer, intent(in) :: numsking                                       !< Global number of skin elements
      integer, intent(in) :: numgeo                                         !< Number of geometries
      integer, intent(in) :: nummat                                         !< Number of materials
      integer, intent(in) :: numply                                         !< Number of plies
      
      ! Modified interface counter
      integer, intent(inout) :: ninterskid                                  !< Number of skid interfaces
      
      ! Geometry and property arrays
      real(kind=WP), dimension(npropg,numgeo), intent(in) :: geo            !< Geometry properties
      integer, dimension(npropgi, numgeo), intent(in) :: igeo               !< Integer geometry properties
      integer, dimension(npropmi, nummat), intent(in) :: ipm                !< Integer material properties
      integer, dimension(lipart1, numgeo), intent(in) :: ipart              !< Part information
      integer, dimension(npari, ninter), intent(in) :: ipari                !< Integer interface properties
      integer, dimension(nparg, ngroup), intent(in) :: iparg                !< Real interface properties
      
      ! Node and element arrays
      integer, dimension(numels), intent(in) :: tag_skins6                     !< Skin element tags
      integer, dimension(max_depvar, mds_nmat), intent(in) :: mds_output_table !< MDS output table
      integer, dimension(mds_nmat), intent(in) :: mds_ndepsvar                 !< Number of dependent variables per material
      integer, dimension(sibcl), intent(in) :: ibcl                            !< boundary condition flags
      integer, dimension(sizloadp*nloadp), intent(in) :: iloadp                !< load pointers
      integer, dimension(slloadp), intent(in) :: lloadp                        !< Load list pointers
      integer, dimension(snpc), intent(in) :: npc                              !< Node pair contact array
      
      ! Labels and data arrays
      character(len=64), dimension(1024, mds_nmat), intent(in) :: mds_label !< MDS variable labels
      real(kind=WP), dimension(stf), intent(in) :: pld                       !< Load data array
      real(kind=WP), dimension(:, :), allocatable, intent(inout) :: pskids   !< Skid line data
      
      ! Derived type structures
      type(H3D_DATABASE), intent(inout) :: h3d_data                         !< H3D database structure
      type(MULTI_FVM_STRUCT), intent(in) :: multi_fvm                       !< Multi-FVM structure
      type(ELBUF_STRUCT_), dimension(ngroup), intent(in) :: elbuf_str       !< Element buffer structure
      type(STACK_PLY), intent(in) :: stack                                  !< Ply stack structure
      type(LOADS_), intent(in) :: loads                                     !< Loads structure
      type(MATPARAM_STRUCT_), dimension(nummat), intent(inout) :: mat_param !< Material parameter structure
      type(PBLAST_), intent(in) :: pblast                                   !< Blast load structure
      type(GROUP_), dimension(ngrpart), intent(in) :: igrpart                !< Interface group structure
      type(SENSORS_), intent(in) :: sensors                                 !< Sensors structure
      type(SENSOR_STR_), dimension(nsensor), intent(in) :: sensor_tab        !< Sensor table
      
! ----------------------------------------------------------------------------------------------------------------------
!                                                   LOCAL VARIABLES
! ----------------------------------------------------------------------------------------------------------------------
      integer :: i, k, n, iok, ni, ns, nn, stext1, ninefricg                ! Loop counters and temporary variables
      character(len=10) :: char1, char2                                     ! Character strings for output formatting
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

          ! Call PRELECH3D to prepare H3D data
          call prelech3d(numgeo, npropgi, npropmi, nummat, numply, &
                         igeo, ipm, h3d_data, multi_fvm, mds_output_table, &
                         mds_nmat, max_depvar, mds_ndepsvar, mat_param, numsphg)

          ! Call LECH3D to read H3D options
          call lech3d(output, geo, igeo, ipm, ipart, h3d_data, multi_fvm, ipari, iparg, tag_skins6, &
                      mds_label, mds_output_table, mds_nmat, max_depvar, mds_ndepsvar, &
                      elbuf_str, stack, ibcl, iloadp, lloadp, loads, mat_param, pblast, &
                      igrpart, npc, pld, snpc, stf)

          ! Print H3D output information
          if (h3d_data%n_outp_h3d /= 0 .and. ispmd == 0) then
            write(iout, 5000) h3d_data%th3d, h3d_data%dth3d
5000        format(/ &
            '     H3D OUTPUT TIME  . . . . . . . . . . . . . . .=', 1pg12.5/ &
            '     H3D TIME INTERVAL. . . . . . . . . . . . . . .=', 1pg12.5/)
            
            write(iout, *)'       |'
            do i = 1, h3d_data%n_outp_h3d
              if (h3d_data%output_list(i)%etype == 1 .or. &
                  h3d_data%output_list(i)%etype == 2 .and. numelcg + numeltgg > 0 .or. &
                  h3d_data%output_list(i)%etype == 3 .and. numelsg > 0 .or. &
                  h3d_data%output_list(i)%etype == 4 .and. numeltrg + numelpg + numelrg > 0 .or. &
                  h3d_data%output_list(i)%etype == 5 .and. numsphg > 0 .or. &
                  h3d_data%output_list(i)%etype == 6 .and. numelqg > 0 .or. &
                  h3d_data%output_list(i)%etype == 7 .and. numsking > 0) then
                
                char1 = ' '
                if (h3d_data%output_list(i)%etype == 1) then
                  char1 = 'NODAL'
                elseif (h3d_data%output_list(i)%etype == 2) then
                  char1 = 'SHELL'
                elseif (h3d_data%output_list(i)%etype == 3) then
                  char1 = 'SOLID'
                elseif (h3d_data%output_list(i)%etype == 4) then
                  char1 = 'ONED'
                elseif (h3d_data%output_list(i)%etype == 5) then
                  char1 = 'SPH'
                elseif (h3d_data%output_list(i)%etype == 6) then
                  char1 = 'QUAD'
                elseif (h3d_data%output_list(i)%etype == 7) then
                  char1 = 'SKIN'
                endif

                char2 = ' '
                if (h3d_data%output_list(i)%outp_type == 1) then
                  char2 = 'SCALAR'
                elseif (h3d_data%output_list(i)%outp_type == 2) then
                  char2 = 'VECTOR'
                elseif (h3d_data%output_list(i)%outp_type == 3) then
                  char2 = 'TENSOR'
                elseif (h3d_data%output_list(i)%outp_type == 4) then
                  char2 = 'TORSOR'
                endif

                stext1 = h3d_data%output_list(i)%s_string1
                write(iout, *) '       |----'//trim(char1)//' '//trim(char2)//' : '
                write(iout, *) '       |    '//h3d_data%output_list(i)%string1(1:stext1)

                if (h3d_data%output_list(i)%iuvar > 0) &
                  write(iout, *) '       |        UVAR=', h3d_data%output_list(i)%iuvar

                if (h3d_data%output_list(i)%ply > 0) &
                  write(iout, *) '       |        PLY=', h3d_data%output_list(i)%ply

                if (h3d_data%output_list(i)%layer > 0) &
                  write(iout, *) '       |        LAYER=', h3d_data%output_list(i)%layer

                if (h3d_data%output_list(i)%ipt > 0) &
                  write(iout, *) '       |        IPT=', h3d_data%output_list(i)%ipt

                if (h3d_data%output_list(i)%ir > 0) &
                  write(iout, *) '       |        IR=', h3d_data%output_list(i)%ir

                if (h3d_data%output_list(i)%is > 0) &
                  write(iout, *) '       |        IS=', h3d_data%output_list(i)%is

                if (h3d_data%output_list(i)%it > 0) &
                  write(iout, *) '       |        IT=', h3d_data%output_list(i)%it

                write(iout, *)'       |'
              endif
            enddo
            write(iout, *)' '
            write(iout, *)' '
          endif

          ! H3D/SENSOR validation
          if (ispmd == 0 .and. mcheck == 0) then
            do k = 1, h3d_data%n_sens_h3d
              iok = 0
              if (h3d_data%lsens_h3d(k) /= 0) then
                do i = 1, sensors%nsensor
                  if (h3d_data%lsens_h3d(k) == sensor_tab(i)%sens_id) then
                    h3d_data%lsens_h3d(k) = i
                    iok = 1
                    exit
                  endif
                enddo
              endif
              if (iok == 0) then
                call ancmsg(msgid=283, anmode=aninfo, i1=h3d_data%lsens_h3d(k))
                call arret(2)
              endif
            enddo
          endif

          ! Output by interface: 1 - Skid lines
          if (h3d_data%n_scal_skid > 0 .and. nintskidold == 0) then
            ninterskid = h3d_data%n_scal_skid
            if (nintstamp /= 0) then
              allocate(pskids(ninterskid, numnodg))
              pskids(1:ninterskid, 1:numnodg) = ZERO
            else
              allocate(pskids(ninterskid, numnod))
              pskids(1:ninterskid, 1:numnod) = ZERO
            endif
          elseif (h3d_data%n_scal_skid == 0 .and. nintskidold == 0) then
            allocate(pskids(0, 0))
          endif

          ! Output by interface: 2 - Frictional energy allocations
          if (h3d_data%n_scal_cse_fric > 0 .and. output%data%s_efric == 0) then
            allocate(output%data%efricg(numnod))
            output%data%efricg(1:numnod) = ZERO
            if (nintstamp /= 0) then
              allocate(output%data%efricg_stamp(numnodg))
              output%data%efricg_stamp(1:numnodg) = ZERO
            else
              allocate(output%data%efricg_stamp(0))
            endif
          elseif (output%data%s_efric == 0) then
            allocate(output%data%efricg(0))
            allocate(output%data%efricg_stamp(0))
          endif

          if (h3d_data%n_scal_cse_fricint > 0 .and. output%data%s_efricint == 0) then
            ninefricg = h3d_data%n_scal_cse_fricint
            output%data%ninefric_stamp = 0
            output%data%ninefric = 0
            
            if (nintstamp /= 0) then
              do n = 1, ninter
                ni = h3d_data%n_cse_fric_inter(n)
                if (ni /= 0 .and. ipari(7, n) == 21) output%data%ninefric_stamp = output%data%ninefric_stamp + 1
              enddo
            endif
            
            if (output%data%ninefric_stamp == ninefricg) then
              call my_alloc(output%data%efric_stamp, ninefricg, numnodg)
              output%data%efric_stamp(1:ninefricg, 1:numnodg) = ZERO
              call my_alloc(output%data%efric, 0, 0)
            elseif (output%data%ninefric_stamp == 0) then
              output%data%ninefric = ninefricg
              call my_alloc(output%data%efric, output%data%ninefric, numnod)
              output%data%efric(1:ninefricg, 1:numnod) = ZERO
              call my_alloc(output%data%efric_stamp, 0, 0)
            else
              output%data%ninefric = ninefricg - output%data%ninefric_stamp
              call my_alloc(output%data%efric_stamp, output%data%ninefric_stamp, numnodg)
              output%data%efric_stamp(1:output%data%ninefric_stamp, 1:numnodg) = ZERO
              call my_alloc(output%data%efric, output%data%ninefric, numnod)
              output%data%efric(1:output%data%ninefric, 1:numnod) = ZERO
              ns = 0
              nn = 0
              do n = 1, ninter
                ni = h3d_data%n_cse_fric_inter(n)
                if (ni /= 0 .and. ipari(7, n) == 21) then
                  ns = ns + 1
                  h3d_data%n_cse_fric_inter(n) = output%data%ninefric + ns
                elseif (ni /= 0) then
                  nn = nn + 1
                  h3d_data%n_cse_fric_inter(n) = nn
                endif
              enddo
            endif
          elseif (output%data%s_efricint == 0) then
            call my_alloc(output%data%efric, 0, 0)
            call my_alloc(output%data%efric_stamp, 0, 0)
          endif
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine init_h3d_engine
      end module init_h3d_engine_mod
