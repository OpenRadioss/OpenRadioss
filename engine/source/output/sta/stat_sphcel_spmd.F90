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
      !||    stat_sphcel_spmd_mod   ../engine/source/output/sta/stat_sphcel_spmd.F90
      !||--- called by ------------------------------------------------------
      !||    genstat                ../engine/source/output/sta/genstat.F
      !||====================================================================
      module stat_sphcel_spmd_mod
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \sphcel element state file write for mpi
      !||====================================================================
      !||    stat_sphcel_spmd      ../engine/source/output/sta/stat_sphcel_spmd.F90
      !||--- called by ------------------------------------------------------
      !||    genstat               ../engine/source/output/sta/genstat.F
      !||--- calls      -----------------------------------------------------
      !||    my_orders             ../common_source/tools/sort/my_orders.c
      !||    spmd_iget_partn_sta   ../engine/source/mpi/output/spmd_stat.F
      !||--- uses       -----------------------------------------------------
      !||    elbufdef_mod          ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||====================================================================
        subroutine stat_sphcel_spmd(numnod          ,numsph      ,numsphg      ,nisp          ,npart           ,  &
                                    ngroup          ,nparg       ,lipart1      ,stat_numelsph ,stat_numelsph_g ,  &
                                    lengsph         ,nspmd       ,itab         ,ipart         ,kxsp            ,  &
                                    ipartsph        ,ipart_state ,nodtag       ,stat_indxsph  ,iparg           ,  &
                                    elbuf_tab       ,idel        )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use ELBUFDEF_MOD, only: elbuf_struct_
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
#include "task_c.inc"
#include "units_c.inc"
#include "mvsiz_p.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,                                   intent(in) :: numnod                      !< number of nodes
          integer,                                   intent(in) :: numsph                      !< number of SPHCELS
          integer,                                   intent(in) :: numsphg                     !< global number of SPHCELS
          integer,                                   intent(in) :: nisp                        !< size for sphcel array definition
          integer,                                   intent(in) :: npart                       !< number of parts
          integer,                                   intent(in) :: ngroup                      !< group number
          integer,                                   intent(in) :: nparg                       !< size for array definition group array
          integer,                                   intent(in) :: lipart1                     !< size for array definition part array
          integer,                                   intent(inout) :: stat_numelsph            !< number of sphcell written
          integer,                                   intent(inout) :: stat_numelsph_g          !< global number of sphcell written
          integer,                                   intent(in) :: lengsph                     !< length of work array
          integer,                                   intent(inout) :: nspmd                    !< number of mpi domains
          integer,                                   intent(in) :: itab(numnod)                !< array for user nodes Ids
          integer,                                   intent(in) :: ipart(lipart1,npart)        !< array for parts
          integer,                                   intent(in) :: kxsp(nisp,numsph)           !< array for sphcel
          integer,                                   intent(in) :: ipartsph(numsph)            !< array for sphcel -> part internal Id
          integer,                                   intent(in) :: ipart_state(numsph)         !< array for written parts in sta files
          integer,                                   intent(inout) :: nodtag(numnod)           !< array for written nodes in sta files
          integer,                                   intent(inout) :: stat_indxsph(numsph)     !< array for index of sphcels in sta file
          integer,                                   intent(in) :: iparg(nparg,ngroup)         !< array for groups
          type(elbuf_struct_),                       intent(in) :: elbuf_tab(ngroup)           !< element buffer structure
          integer,                                   intent(in) :: idel                        !< is not activated sphcel to be written in sta file
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,l,k,m 
          integer :: pos 
          integer n,jj,iprt0,iprt,ii
          integer ng,nel,nft,lft,llt,ity,ioff
          integer np(4*numsph),work(70000),clef(2,numsphg),npglob(4*lengsph)
          integer iadg(nspmd,npart)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          jj = 0
          ii = 0
          if (numsph /= 0) then
            do ng=1,ngroup
              ity = iparg(5,ng)
              if (ity == 51) then
                nel = iparg(2,ng)
                nft = iparg(3,ng) 
                lft=1
                llt=nel
                do i=lft,llt
                  n  = i + nft
                  iprt=ipartsph(n)
                  if (ipart_state(iprt) /= 0) then
                    np(jj+1) = kxsp(nisp,n)
                    np(jj+2) = itab(kxsp(3,n))
                    np(jj+3) = iprt
                    np(jj+4) = max(0,nint(elbuf_tab(ng)%gbuf%off(i)))
                    ii = ii + 1
                    jj = jj + 4
                    stat_numelsph =stat_numelsph+1
                    clef(1,ii)=iprt
                    clef(2,ii)=kxsp(nisp,n)
                    nodtag(kxsp(3,n))=1
                  endif ! if (ipart_state(iprt) /= 0)
                enddo ! do i=lft,llt
              endif ! if (ity == 51)
            enddo ! do ng=1,ngroup
          endif ! if (numelsph /= 0)

          stat_numelsph_g=0
          call spmd_iget_partn_sta(4,stat_numelsph,stat_numelsph_g,lengsph,np, &
               iadg,npglob,stat_indxsph)
          
          if (ispmd==0) then
            do n=1,stat_numelsph_g
              stat_indxsph(n)=n
              clef(1,n)=npglob(4*(n-1)+3)
              clef(2,n)=npglob(4*(n-1)+1)
            end do
            call my_orders(0,work,clef,stat_indxsph,stat_numelsph_g,2)

          
            iprt0=0
            do n=1,stat_numelsph_g
              k=stat_indxsph(n)
              jj=4*(k-1)
              iprt=npglob(jj+3)
              ioff=npglob(jj+4)
              if (idel==0 .or. (idel==1 .and. ioff >= 1)) then
                if (iprt /= iprt0) then
                 write(iugeo,'(a,i10)')'/SPHCEL/',ipart(4,iprt)
                  write(iugeo,'(a)')'#sphcel_id'
                  iprt0=iprt
                endif
                write(iugeo,'(i10)') npglob(jj+2)
              endif !if (idel)
            enddo ! do n=1,stat_numelsph_g
          endif !if (ispmd)
          
          return
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine stat_sphcel_spmd
      end module stat_sphcel_spmd_mod
      
      