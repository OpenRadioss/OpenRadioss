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
      !||    sph_crit_voxel_mod   ../engine/source/elements/sph/sph_crit_voxel.F90
      !||--- called by ------------------------------------------------------
      !||    sphprep              ../engine/source/elements/sph/sphprep.F
      !||====================================================================
      module sph_crit_voxel_mod
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!!\brief This subroutine computes damping forces for /DAMP/FREQ_RANGE for solid elements
!=======================================================================================================================
!
      !||====================================================================
      !||    sph_crit_voxel   ../engine/source/elements/sph/sph_crit_voxel.F90
      !||--- called by ------------------------------------------------------
      !||    sphprep          ../engine/source/elements/sph/sphprep.F
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||    constant_mod     ../common_source/modules/constant_mod.F
      !||    sph_work_mod     ../common_source/modules/mat_elem/sph_work.F90
      !||    spmd_mod         ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine sph_crit_voxel(xmax     ,ymax     ,zmax      ,xmin      ,ymin    ,         &
                                  zmin     ,diam_max ,voxsiz    ,majord_vox,nbk     ,         &
                                  nsp2sortf,nsp2sortl,itask     ,nbgauge   ,nthread ,         &
                                  lgauge   ,numels   ,spbuf     ,wsp2sort  ,kxsp    ,         &
                                  nisp     ,nspbuf   ,numsph    ,x         ,numnod  ,         &
                                  llgauge  ,gauge    ,nspmd     ,voxel) 
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod , only: one,zero,ep20,em20,half
          use sph_work_mod
          use spmd_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,                                   intent(in)    :: nbk                      !< number of voxel per drection  
          integer,                                   intent(in)    :: nsp2sortf                !< number of particles to sort
          integer,                                   intent(in)    :: nsp2sortl                !< number of particles to sort
          integer,                                   intent(in)    :: itask                    !< task number
          integer,                                   intent(in)    :: nbgauge                  !< number of gauge
          integer,                                   intent(in)    :: nthread                  !< number of threads
          integer, dimension(3,nbgauge),             intent(in)    :: lgauge                   !< gauge array
          integer,                                   intent(in)    :: numels                   !< number of solid elements
          integer,                                   intent(in)    :: numnod                   !< number of nodes          
          integer,                                   intent(in)    :: nisp                     !< size of kxsp
          integer, dimension(numsph),                intent(in)    :: wsp2sort                 !< number of particles to sort
          integer,                                   intent(in)    :: nspbuf                   !< size of kxsp
          integer,                                   intent(in)    :: numsph                   !< number of sph particles
          integer,                                   intent(in)    :: llgauge                  !< size of gauge array        
          integer,                                   intent(in)    :: nspmd                    !< number of cpus      
          integer,                                   intent(in)    :: voxsiz                   !< sorting criteria                
          integer, dimension(nisp,numsph),           intent(in)    :: kxsp                     !< sorted particles
          my_real,                                   intent(inout) :: xmax                     !< maximum coordinates
          my_real,                                   intent(inout) :: ymax                     !< maximum coordinates
          my_real,                                   intent(inout) :: zmax                     !< maximum coordinates
          my_real,                                   intent(inout) :: xmin                     !< minimum coordinates
          my_real,                                   intent(inout) :: ymin                     !< minimum coordinates
          my_real,                                   intent(inout) :: zmin                     !< minimum coordinates
          my_real,                                   intent(in)    :: diam_max                 !< maximum diameter    
          my_real,                                   intent(inout) :: majord_vox               !< majorant of interparticle distance
          my_real, dimension(nspbuf,numsph),         intent(in)    :: spbuf                    !< sorted particles
          my_real, dimension(3,numnod),              intent(in)    :: x                        !< node corrdinates
          my_real, dimension(llgauge,nbgauge),       intent(in)    :: gauge                    !< gauge coordinates
          type(sph_work_voxel_),                     intent(inout) :: voxel                    !< work array for voxels (shared in openmp)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: inod
          integer :: bnbnod(nbk, nbk, nbk),bnbnod2(nbk, nbk, nbk)
          integer :: ibx, iby, ibz, n, ns, ippx, ippz, ippy
          my_real :: dx,dy,dz,majord_vox_g
          my_real :: xminl, yminl, zminl, xmaxl, ymaxl, zmaxl
          my_real :: bdxmin(nbk, nbk, nbk), bdxmax(nbk, nbk, nbk)
          my_real :: bdymin(nbk, nbk, nbk), bdymax(nbk, nbk, nbk)
          my_real :: bdzmin(nbk, nbk, nbk), bdzmax(nbk, nbk, nbk)
          my_real :: llx, lly, llz, zob(6)
          my_real :: send_buf(voxsiz), recv_buf(voxsiz)
          save    :: majord_vox_g
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------             
!
          llx = max(em20, (xmax - xmin) / nbk)
          lly = max(em20, (ymax - ymin) / nbk)
          llz = max(em20, (zmax - zmin) / nbk)       
!
          llx = max(1.8 * diam_max, llx)
          lly = max(1.8 * diam_max, lly)
          llz = max(1.8 * diam_max, llz)    
!
          xminl = ((xmin + xmax) * half) - half * llx * nbk
          yminl = ((ymin + ymax) * half) - half * lly * nbk
          zminl = ((zmin + zmax) * half) - half * llz * nbk
          xmaxl = ((xmin + xmax) * half) + half * llx * nbk
          ymaxl = ((ymin + ymax) * half) + half * lly * nbk
          zmaxl = ((zmin + zmax) * half) + half * llz * nbk
!
          bnbnod = 0
          bdxmin = ep20
          bdymin = ep20
          bdzmin = ep20
          bdxmax = -ep20
          bdymax = -ep20
          bdzmax = -ep20
!$omp single
          voxel%nnod = 0
          voxel%dxmin = ep20
          voxel%dymin = ep20
          voxel%dzmin = ep20
          voxel%dxmax = -ep20
          voxel%dymax = -ep20
          voxel%dzmax = -ep20           
!$omp end single          
!
          do ns=nsp2sortf,nsp2sortl
            n=wsp2sort(ns)
            inod=kxsp(3,n)
            dx  =x(1,inod)-spbuf(5,n)
            dy  =x(2,inod)-spbuf(6,n)
            dz  =x(3,inod)-spbuf(7,n)
            ibx = min(1+floor((x(1,inod)-xminl)/llx),nbk)
            iby = min(1+floor((x(2,inod)-yminl)/lly),nbk)
            ibz = min(1+floor((x(3,inod)-zminl)/llz),nbk)
            ibx = max(1,ibx)
            iby = max(1,iby)
            ibz = max(1,ibz) 
            bnbnod(ibx,iby,ibz) = bnbnod(ibx,iby,ibz)+1
            bdxmin(ibx,iby,ibz) = min(bdxmin(ibx,iby,ibz),dx)
            bdymin(ibx,iby,ibz) = min(bdymin(ibx,iby,ibz),dy)
            bdzmin(ibx,iby,ibz) = min(bdzmin(ibx,iby,ibz),dz)
            bdxmax(ibx,iby,ibz) = max(bdxmax(ibx,iby,ibz),dx)
            bdymax(ibx,iby,ibz) = max(bdymax(ibx,iby,ibz),dy)
            bdzmax(ibx,iby,ibz) = max(bdzmax(ibx,iby,ibz),dz)  
          enddo
!
          do n=itask+1,nbgauge,nthread
            if(lgauge(1,n) <= -(numels+1))then
              dx  =gauge(2,n)-gauge(6,n)
              dy  =gauge(3,n)-gauge(7,n)
              dz  =gauge(4,n)-gauge(8,n)
              ibx = min(1+floor((gauge(2,n)-xminl)/llx),nbk)
              iby = min(1+floor((gauge(3,n)-yminl)/lly),nbk)
              ibz = min(1+floor((gauge(4,n)-zminl)/llz),nbk)
              ibx = max(1,ibx)
              iby = max(1,iby)
              ibz = max(1,ibz) 
              bnbnod(ibx,iby,ibz) = bnbnod(ibx,iby,ibz)+1
              bdxmin(ibx,iby,ibz) = min(bdxmin(ibx,iby,ibz),dx)
              bdymin(ibx,iby,ibz) = min(bdymin(ibx,iby,ibz),dy)
              bdzmin(ibx,iby,ibz) = min(bdzmin(ibx,iby,ibz),dz)
              bdxmax(ibx,iby,ibz) = max(bdxmax(ibx,iby,ibz),dx)
              bdymax(ibx,iby,ibz) = max(bdymax(ibx,iby,ibz),dy)
              bdzmax(ibx,iby,ibz) = max(bdzmax(ibx,iby,ibz),dz)
            end if
          end do
!          
!         Sync of voxels with other threads        
!$omp critical
          do ibx=1,nbk
            do iby=1,nbk
              do ibz=1,nbk
                voxel%nnod(ibx,iby,ibz) = max(voxel%nnod(ibx,iby,ibz),bnbnod(ibx,iby,ibz))
                voxel%dxmin(ibx,iby,ibz) = min(voxel%dxmin(ibx,iby,ibz),bdxmin(ibx,iby,ibz))
                voxel%dymin(ibx,iby,ibz) = min(voxel%dymin(ibx,iby,ibz),bdymin(ibx,iby,ibz))
                voxel%dzmin(ibx,iby,ibz) = min(voxel%dzmin(ibx,iby,ibz),bdzmin(ibx,iby,ibz))
                voxel%dxmax(ibx,iby,ibz) = max(voxel%dxmax(ibx,iby,ibz),bdxmax(ibx,iby,ibz))
                voxel%dymax(ibx,iby,ibz) = max(voxel%dymax(ibx,iby,ibz),bdymax(ibx,iby,ibz))
                voxel%dzmax(ibx,iby,ibz) = max(voxel%dzmax(ibx,iby,ibz),bdzmax(ibx,iby,ibz))    
              end do
            end do
          end do
!$omp end critical    
!
!$omp barrier                    
!
!         Sync of voxels with other CPUs      
!         bnbnod is number of particles in the voxel - maximum over CPUs is enough - not need to make a specific com wiht sum  
          if (itask == 0) then          
!
            bnbnod(1:nbk,1:nbk,1:nbk) = voxel%nnod(1:nbk,1:nbk,1:nbk)
            bdxmin(1:nbk,1:nbk,1:nbk) = voxel%dxmin(1:nbk,1:nbk,1:nbk)
            bdymin(1:nbk,1:nbk,1:nbk) = voxel%dymin(1:nbk,1:nbk,1:nbk)
            bdzmin(1:nbk,1:nbk,1:nbk) = voxel%dzmin(1:nbk,1:nbk,1:nbk)
            bdxmax(1:nbk,1:nbk,1:nbk) = voxel%dxmax(1:nbk,1:nbk,1:nbk)
            bdymax(1:nbk,1:nbk,1:nbk) = voxel%dymax(1:nbk,1:nbk,1:nbk)
            bdzmax(1:nbk,1:nbk,1:nbk) = voxel%dzmax(1:nbk,1:nbk,1:nbk)
!            
            if (nspmd > 1) then
              do ibx=1,nbk
                do iby=1,nbk
                  do ibz=1,nbk
                    send_buf(7*(nbk*nbk*(ibx-1)+nbk*(iby-1)+ibz-1)+1) =  bnbnod(ibx,iby,ibz)
                    send_buf(7*(nbk*nbk*(ibx-1)+nbk*(iby-1)+ibz-1)+2) = -bdxmin(ibx,iby,ibz)
                    send_buf(7*(nbk*nbk*(ibx-1)+nbk*(iby-1)+ibz-1)+3) = -bdymin(ibx,iby,ibz)
                    send_buf(7*(nbk*nbk*(ibx-1)+nbk*(iby-1)+ibz-1)+4) = -bdzmin(ibx,iby,ibz)
                    send_buf(7*(nbk*nbk*(ibx-1)+nbk*(iby-1)+ibz-1)+5) =  bdxmax(ibx,iby,ibz)
                    send_buf(7*(nbk*nbk*(ibx-1)+nbk*(iby-1)+ibz-1)+6) =  bdymax(ibx,iby,ibz)
                    send_buf(7*(nbk*nbk*(ibx-1)+nbk*(iby-1)+ibz-1)+7) =  bdzmax(ibx,iby,ibz)
                  end do
                end do
              end do
              call spmd_allreduce(send_buf,recv_buf,voxsiz,SPMD_MAX)
              do ibx=1,nbk
                do iby=1,nbk
                  do ibz=1,nbk
                    bnbnod(ibx,iby,ibz) =  nint(recv_buf(7*(nbk*nbk*(ibx-1)+nbk*(iby-1)+ibz-1)+1))
                    bdxmin(ibx,iby,ibz) = -recv_buf(7*(nbk*nbk*(ibx-1)+nbk*(iby-1)+ibz-1)+2)
                    bdymin(ibx,iby,ibz) = -recv_buf(7*(nbk*nbk*(ibx-1)+nbk*(iby-1)+ibz-1)+3)
                    bdzmin(ibx,iby,ibz) = -recv_buf(7*(nbk*nbk*(ibx-1)+nbk*(iby-1)+ibz-1)+4)
                    bdxmax(ibx,iby,ibz) =  recv_buf(7*(nbk*nbk*(ibx-1)+nbk*(iby-1)+ibz-1)+5)
                    bdymax(ibx,iby,ibz) =  recv_buf(7*(nbk*nbk*(ibx-1)+nbk*(iby-1)+ibz-1)+6)
                    bdzmax(ibx,iby,ibz) =  recv_buf(7*(nbk*nbk*(ibx-1)+nbk*(iby-1)+ibz-1)+7)
                  end do
                end do
              end do
            endif            
!     
!           Maximum over neighbouring voxels         
            voxel%dxmin = ep20
            voxel%dymin = ep20
            voxel%dzmin = ep20
            voxel%dxmax = -ep20
            voxel%dymax = -ep20
            voxel%dzmax = -ep20   

            do ibx=1,nbk
              do iby=1,nbk
                do ibz=1,nbk
                  if (bnbnod(ibx,iby,ibz) == 0) cycle
                  do ippx=max(1,ibx-1),min(nbk,ibx+1)
                    do ippy=max(1,iby-1),min(nbk,iby+1)
                      do ippz=max(1,ibz-1),min(nbk,ibz+1)
                        voxel%dxmin(ibx,iby,ibz) = min(voxel%dxmin(ibx,iby,ibz),bdxmin(ippx,ippy,ippz))
                        voxel%dymin(ibx,iby,ibz) = min(voxel%dymin(ibx,iby,ibz),bdymin(ippx,ippy,ippz))
                        voxel%dzmin(ibx,iby,ibz) = min(voxel%dzmin(ibx,iby,ibz),bdzmin(ippx,ippy,ippz))
                        voxel%dxmax(ibx,iby,ibz) = max(voxel%dxmax(ibx,iby,ibz),bdxmax(ippx,ippy,ippz))
                        voxel%dymax(ibx,iby,ibz) = max(voxel%dymax(ibx,iby,ibz),bdymax(ippx,ippy,ippz))
                        voxel%dzmax(ibx,iby,ibz) = max(voxel%dzmax(ibx,iby,ibz),bdzmax(ippx,ippy,ippz))
                      enddo
                    enddo
                  enddo      
                end do
              end do
            end do
!  
            majord_vox_g = zero
            do ibx=1,nbk
              do iby=1,nbk
                do ibz=1,nbk
                  if (bnbnod(ibx,iby,ibz) == 0) cycle
                  dx = voxel%dxmax(ibx,iby,ibz)-voxel%dxmin(ibx,iby,ibz)
                  dy = voxel%dymax(ibx,iby,ibz)-voxel%dymin(ibx,iby,ibz)
                  dz = voxel%dzmax(ibx,iby,ibz)-voxel%dzmin(ibx,iby,ibz)
                  majord_vox_g =max(majord_vox_g,sqrt(dx*dx+dy*dy+dz*dz)*half)
                end do
              end do
            end do      
!            
          endif    
!
!$omp barrier           
!
          majord_vox = majord_vox_g                                                                                                                                
!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine sph_crit_voxel
      end module sph_crit_voxel_mod
