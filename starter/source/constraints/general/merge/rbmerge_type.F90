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
      !||    rbmerge_mod     ../starter/source/constraints/general/merge/rbmerge_type.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_merge   ../starter/source/constraints/general/merge/hm_read_merge.F
      !||    rb_explore      ../starter/source/constraints/general/merge/hm_read_merge.F
      !||    rbleveldown     ../starter/source/constraints/general/merge/hm_read_merge.F
      !||    rbtag2down      ../starter/source/constraints/general/merge/hm_read_merge.F
      !||    retrirby        ../starter/source/constraints/general/merge/hm_read_merge.F
      !||    trirbmerge      ../starter/source/constraints/general/merge/hm_read_merge.F
      !||====================================================================
      module rbmerge_mod
        !=======================================================================   
        !                           rigid body merge
        !c=======================================================================   
        !-----------------------------------------------------------------------
              type rbmerge_
                integer   :: id                                     !< - rigid body identifier
                integer   :: nbsecondary                            !< number of secondary rigid body
                integer, dimension(:) , allocatable :: idsecondary  !< secondary rigid bodys attached to the main
                integer   :: imain                                  !< 0 if this rigid body doesn't have a main
        !                                                           !< the rigid body's main 
                integer   :: level                                  !< rigid body level
                                                                    !<    = 0  ! if the rigid body is only a main
                                                                    !<    = -1 ! if the rigid body has a main
                                                                    !<    = -2 ! if the main of the rigid body has a main
                                                                    !<    = -3 ! and again
                integer   :: flag_main                              !< flag relating the rigid option merge to the main
                                                                    !<    = -1 ! if the rigid body is only a main
                                                                    !<    =  0 ! default value
                                                                    !<    =  1 ! option 1
                                                                    !<    =  2 ! option 2
                                                                    !<    =  3 ! option 3
                integer   :: nnode                                  !< number of secondary node
                integer, dimension(:) , allocatable :: node         !< secondary node attached to the main
                integer, dimension(:) , allocatable :: flag_node    !< flag relating the rigid option merge to the main
                                                                    !<    =  0 ! default value
                                                                    !<    =  1 ! option 1
                                                                    !<    =  2 ! option 2
                                                                    !<    =  3 ! option 3
        !--------------
              end type rbmerge_
              end module rbmerge_mod
