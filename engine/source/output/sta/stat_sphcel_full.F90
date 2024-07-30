!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      !||    stat_sphcel_full_mod   ../engine/source/output/sta/stat_sphcel_full.F90
      !||--- called by ------------------------------------------------------
      !||    genstat                ../engine/source/output/sta/genstat.F
      !||====================================================================
      module stat_sphcel_full_mod
        contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \sphcel element write full in state file ( stress , energy , rho, slen, epsp , aux )
      !||====================================================================
      !||    stat_sphcel_full    ../engine/source/output/sta/stat_sphcel_full.F90
      !||--- called by ------------------------------------------------------
      !||    genstat             ../engine/source/output/sta/genstat.F
      !||--- calls      -----------------------------------------------------
      !||    spmd_rgather9_dp    ../engine/source/mpi/interfaces/spmd_outp.F
      !||    spmd_stat_pgather   ../engine/source/mpi/output/spmd_stat.F
      !||--- uses       -----------------------------------------------------
      !||    elbufdef_mod        ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||====================================================================
        subroutine stat_sphcel_full(numsph     ,nisp       ,ngroup         ,nparg         ,sizloc          ,  &
                                    npart      ,sizp0      ,nspmd          ,stat_numelsph ,stat_numelsph_g ,  &
                                    nspbuf     ,numnod     ,npropmi        ,nummat        ,lipart1         ,  &
                                    kxsp       ,ipartsph   ,ipart_state    ,stat_indxsph  ,iparg           ,  &
                                    elbuf_tab  ,wa         ,wap0           ,spbuf         ,itab            ,  &
                                    ipm        ,idel       ,ipart          )
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
          integer,                                   intent(in) :: numsph                      !< number of SPHCELS
          integer,                                   intent(in) :: nisp                        !< size for sphcel array definition
          integer,                                   intent(in) :: ngroup                      !< group number
          integer,                                   intent(in) :: nparg                       !< size for array definition group array
          integer,                                   intent(in) :: sizloc                      !< size for array definition work array
          integer,                                   intent(in) :: sizp0                       !< size for array definition work array
          integer,                                   intent(inout) :: stat_numelsph            !< number of sphcell written
          integer,                                   intent(inout) :: stat_numelsph_g          !< global number of sphcell written
          integer,                                   intent(in) :: nspmd                       !<  number of mpi domains
          integer,                                   intent(in) :: npart                       !< numper of parts
          integer,                                   intent(in) :: nspbuf                      !< size for array definition sphcel buffer
          integer,                                   intent(in) :: numnod                      !<  number of nodes
          integer,                                   intent(in) :: npropmi                     !< size for array definition properties array
          integer,                                   intent(in) :: nummat                      !< number of material laws
          integer,                                   intent(in) :: lipart1                     !< size for array definition part array
          integer,                                   intent(in) :: kxsp(nisp,numsph)           !< array for sphcel
          integer,                                   intent(in) :: ipartsph(numsph)            !< array for sphcel -> part internal Id
          integer,                                   intent(in) :: ipart_state(npart)          !< array for written parts in sta files
          integer,                                   intent(inout) :: stat_indxsph(numsph)     !< array for index of sphcels in sta file
          integer,                                   intent(in) :: iparg(nparg,ngroup)         !< array for groups
          type(elbuf_struct_),                       intent(in) :: elbuf_tab(ngroup)           !<element buffer structure
          double precision,                          intent(inout) :: wa(sizloc)               !< work array
          double precision,                          intent(inout) :: wap0(sizp0)              !< work array
          my_real,                                   intent(in) :: spbuf(nspbuf,numsph)        !< sphcel buffer
          integer,                                   intent(in) :: itab(numnod)                !< array for user nodes Ids
          integer,                                   intent(in) :: ipm(npropmi,nummat)         !< array for properties
          integer,                                   intent(in) :: idel                        !< is not activated sphcel to be written in sta file
          integer,                                   intent(in) :: ipart(lipart1,npart)        !< array for parts
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer i,j,k,n,jj,len,ioff,ie,ng,nel,nft,lft,llt,ity,id,iprt0,iprt,igtyp,iprop,g_pla,mlw,ivar,my_nuvar,mt
          integer ii(6)
          integer ptwa(stat_numelsph),ptwa_p0(0:max(1,stat_numelsph_g))
          character*100 delimit,line
          data delimit(1:60) &
           /'#---1----|----2----|----3----|----4----|----5----|----6----|'/
          data delimit(61:100) &
           /'----7----|----8----|----9----|----10---|'/
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          jj = 0
          !
          if (stat_numelsph /= 0) then
            !
            ie=0
            do ng=1,ngroup
              ity = iparg(5,ng)
              if (ity == 51) then
                nel  = iparg(2,ng)
                do i=1,6
                  ii(i) = nel*(i-1)
                enddo
                nft  = iparg(3,ng)
                iprop = kxsp(4,nft+1)
                mlw   = iparg(1,ng)
                lft=1
                llt=nel
                !
                do i=lft,llt
                  n = i + nft
                  iprt=ipartsph(n)
                  mt  =ipart(1,iprt)
                  if (ipart_state(iprt) /= 0) then
                    jj = jj + 1
                    wa(jj) = max(0,nint(elbuf_tab(ng)%gbuf%off(i)))
                    jj = jj + 1
                    wa(jj) = iprt
                    jj = jj + 1
                    !wa(jj) = kxsp(nisp,n)
                    wa(jj) = itab(kxsp(3,n))
                    jj = jj + 1
                    wa(jj) = elbuf_tab(ng)%gbuf%eint(ii(1)+i)
                    jj = jj + 1
                    wa(jj) = elbuf_tab(ng)%gbuf%rho(ii(1)+i)
                    jj = jj + 1
                    wa(jj) = spbuf(1,n)
                    jj = jj + 1
                    wa(jj) = elbuf_tab(ng)%gbuf%sig(ii(1)+i)
                    jj = jj + 1
                    wa(jj) = elbuf_tab(ng)%gbuf%sig(ii(2)+i)
                    jj = jj + 1
                    wa(jj) = elbuf_tab(ng)%gbuf%sig(ii(3)+i)
                    jj = jj + 1
                    if(elbuf_tab(ng)%gbuf%g_pla == 1) then 
                      wa(jj) = elbuf_tab(ng)%gbuf%pla(i)
                    else
                      wa(jj) = 0.
                    endif
                    if (mlw >= 28) then
                      my_nuvar = ipm(8,mt)
                      jj = jj + 1
                      wa(jj) = my_nuvar
                      do ivar=1,my_nuvar
                        jj = jj + 1
                        wa(jj) = elbuf_tab(ng)%bufly(1)%mat(1,1,1)%var((ivar-1)*nel + i)
                      enddo
                    else
                      my_nuvar = 0
                      jj = jj + 1
                      wa(jj) = my_nuvar
                    endif
                    !---
                    ie=ie+1
                    !--- 
                    ptwa(ie)=jj
                  endif ! if (ipart_state(iprt) /= 0)
                enddo  !  do i=lft,llt
                ! end loop over sph elements
              endif ! ity == 51
            enddo ! ng = 1, ngroup
          endif ! if (stat_numelsph == 0) then
!-----------------------------------------------------------------------
!     sphcel - write
!-----------------------------------------------------------------------
            if (nspmd == 1) then
              ptwa_p0(0)=0
              do n=1,stat_numelsph
                ptwa_p0(n)=ptwa(n)
              enddo
              len=jj
              do j=1,len
                wap0(j)=wa(j)
              enddo
            else
              call spmd_stat_pgather(ptwa,stat_numelsph,ptwa_p0,stat_numelsph_g)
              len = 0
              call spmd_rgather9_dp(wa,jj,wap0,sizp0,len)
            endif
!-------------------------------------
            if (ispmd == 0 .and. len > 0) then
              iprt0 = 0
              do n=1,stat_numelsph_g
                k=stat_indxsph(n)
                j=ptwa_p0(k-1)
                !
                ioff=nint(wap0(j + 1))
                if (idel==0 .or. (idel==1 .and. ioff >= 1)) then
                  iprt  = nint(wap0(j + 2)) 
                  id    = nint(wap0(j + 3))
                  my_nuvar = nint(wap0(j + 11))
!--------------------------------------
                    if (iprt /= iprt0) then
                      write(iugeo,'(a)') delimit
                      write(iugeo,'(a)')'/INISPHCEL/FULL'
                      write(iugeo,'(a)')&
                                 '#----------------------------------------------------------'
                      write(iugeo,'(a)')'#SPHCEL_ID   NUVAR'
                      write(iugeo,'(a)')'#format:(1p3e20.13) #(eint,rho,slen)'
                      write(iugeo,'(a)')'#format:(1p3e20.13) #(sig1,sig2,sig3)'
                      write(iugeo,'(a)')'#format:(1pe20.13) #(epsp)'
                      write(iugeo,'(a)')'#format:(1p3e20.13) #(uvar(i) , i = 1 ,nuvar)'
                      write(iugeo,'(a)')&
                                 '#----------------------------------------------------------'
                      !
                      iprt0=iprt
                    endif ! if (iprt /= iprt0)
                    !
                    write(iugeo,'(2i10)') id,my_nuvar
                    write(iugeo,'(1p3e20.13)')wap0(j+4),wap0(j+5),wap0(j+6)
                    write(iugeo,'(1p3e20.13)')wap0(j+7),wap0(j+8),wap0(j+9)
                    write(iugeo,'(1p3e20.13)')wap0(j+10)
                    if(my_nuvar > 0) write(iugeo,'(1p3e20.13)')(wap0(j + 11 + k),k=1,my_nuvar)
!--------------------------------------
                endif  !  if (ioff >= 1)
              enddo  !  do n=1,stat_numelsph_g
            endif  !  if (ispmd == 0.and.len > 0)

          return
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine stat_sphcel_full
      end module stat_sphcel_full_mod
      
            