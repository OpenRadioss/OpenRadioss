Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    s4lagsfem      ../engine/source/elements/solid/solide4_sfem/s4lagsfem.F
!||--- called by ------------------------------------------------------
!||    resol          ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    initbuf        ../engine/share/resol/initbuf.F
!||    s10volnod3     ../engine/source/elements/solid/solide4_sfem/s10volnod3.F
!||    s10volnodt3    ../engine/source/elements/solid/solide4_sfem/s10volnodt3.F
!||    s4volnod3      ../engine/source/elements/solid/solide4_sfem/s4volnod3.F
!||    s4volnod_sm    ../engine/source/elements/solid/solide4_sfem/s4volnod_sm.F
!||    spmd_waitall   ../engine/source/mpi/spmd_wait.F90
!||    spmd_waitany   ../engine/source/mpi/spmd_wait.F90
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod   ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    element_mod    ../common_source/modules/elements/element_mod.F90
!||    initbuf_mod    ../engine/share/resol/initbuf.F
!||    sfem_mod       ../common_source/modules/elements/sfem_mod.F90
!||    spmd_mod       ../engine/source/mpi/spmd_mod.F90
!||====================================================================
      SUBROUTINE S4LAGSFEM(IPARG,IXS,X,V,ELBUF_TAB,SFEM_NODVAR,S_SFEM_NODVAR,
     .                     IXS10,XDP,SXDP,
     .                     NUMNOD, NSPMD, NUMELS, NUMELS8, NUMELS10, NPARG, NGROUP, IRESP,
     .                     mode,ispmd,sfem)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD         
      use element_mod , only : nixs
      use sfem_mod , only : sfem_
      use spmd_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include "spmd.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: S_SFEM_NODVAR, SXDP !< size for array declaration
      INTEGER,INTENT(IN) :: NUMELS, NUMELS8, NUMELS10 !< number of elements
      INTEGER,INTENT(IN) :: NPARG, NGROUP !< sizes for data structure IPARG
      INTEGER,INTENT(IN) :: NUMNOD !< number of nodes (/NODE)
      INTEGER,INTENT(IN) :: IRESP !< flag for simple precision
      INTEGER,INTENT(IN) :: NSPMD !< number of SPMD domain
      integer, intent(in) :: mode !< 0 : computation and isend/irecv, 1 : wait and finalization
      integer, intent(in) :: ispmd !< mpi processor id
      INTEGER IXS(NIXS,NUMELS),IPARG(NPARG,NGROUP),IXS10(6,NUMELS10)
      my_real, INTENT(IN) :: X(3*NUMNOD),V(3*NUMNOD)
      my_real,intent(inout) :: SFEM_NODVAR(S_SFEM_NODVAR)
      DOUBLE PRECISION , DIMENSION(SXDP), INTENT(IN) :: XDP     
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      type(sfem_), intent(inout) :: sfem
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      logical :: condition
      INTEGER NG, I, J, I1, I2, I3, I4, K, LENR,NEL,NNOD,ICPRE,IBID,IP
      INTEGER NC1(MVSIZ),NC2(MVSIZ),NC3(MVSIZ),NC4(MVSIZ),NC(MVSIZ,10)
      integer :: fsky_nb
      integer :: my_address,my_index
      integer :: node_id,proc_id
      my_real VOL0(MVSIZ),VOLG(MVSIZ)


      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF  
      integer, parameter :: tag = 10003  
      integer :: status_mpi(mpi_status_size)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

C---------------------------------------------------
C     COMPUTE NODAL VOLUME FOR ALL TETRAHEDRON
C---------------------------------------------------

      if(mode==0) then
!$omp parallel default(private)
!$omp+ shared(iparg,ixs,ixs10,x,v,elbuf_tab,sfem,ngroup,numels8,iresp)
                  
!$omp do schedule(dynamic,1)
        do ng=1,ngroup
          nnod = iparg(28,ng) ! get the number of node per element for the current group
          icpre = iparg(10,ng) ! get the value of ICPRE for the current group
          if (nnod /= 4 .and. nnod /= 10) cycle ! only tetrahedral elements are supported
          call initbuf(iparg,ng, 
     .                  mtn,llt,nft,iad,ity, 
     .                  npt,jale,ismstr,jeul,jtur, 
     .                  jthe,jlag,jmult,jhbe,jivf, 
     .                  nvaux,jpor,jcvt,jclose,jplasol, 
     .                  irep,iint,igtyp,israt,isrot, 
     .                  icsen,isorth,isorthg,ifailure,jsms)
          condition = (nnod==4.and.isrot==3).or.(icpre/=0.and.(nnod==10.or.(nnod==4.and.isrot==1)))
          if(.not.condition) cycle ! for 4-node elements, only ISROT=3 is supported, and for both 4-node and 10-node elements, only ICPRE=1 is supported
          if(jeul==1) cycle
          if(jale==1) cycle
          lft=1
          nel = llt
          do i=1,nel
            j=i+nft
            nc1(i)=ixs(2,j)
            nc2(i)=ixs(4,j)
            nc3(i)=ixs(7,j)
            nc4(i)=ixs(6,j)
          enddo
          gbuf => elbuf_tab(ng)%gbuf
          if(nnod==4.and.isrot==3) then
            call s4volnod3( 
     .            x,       nc1,       nc2, 
     .            nc3,      nc4,     gbuf%off,  xdp, 
     .            nel,      ismstr,sfem,nft)


            if(iresp==1)then
              lbuf => elbuf_tab(ng)%bufly(1)%lbuf(1,1,1)
              do i=1,llt
                if(gbuf%off(i) == zero) then
                  vol0(i)=zero
                else
                  vol0(i)=lbuf%vol0dp(i)
                endif
              enddo          
            else
              do i=1,llt
                if(gbuf%off(i) == zero) then
                  vol0(i)=zero
                else
                  vol0(i)=gbuf%vol(i)
                endif
              enddo
            end if !(iresp==1)then

            call s4volnod_sm(
     .              v,nc1,nc2,
     .              nc3,nc4,vol0,gbuf%amu,
     .              gbuf%off,gbuf%smstr,nel,ismstr,sfem,nft)

          else
            nc(1:nel,1) = nc1(1:nel)
            nc(1:nel,2) = nc2(1:nel)
            nc(1:nel,3) = nc3(1:nel)
            nc(1:nel,4) = nc4(1:nel)
            if(isrot /= 1)then
              do i=1,nel
                j=i+nft-numels8
                nc(i,5:10) = ixs10(1:6,j)
              enddo
            else
              nc(1:nel,5:10) = 0
            endif
            if (ismstr==10) then
              npt = 4
              call s10volnodt3(elbuf_tab(ng),x,nc,
     .                       gbuf%off,gbuf%smstr,xdp,nel,
     .                       npt,sfem,nft)

              do ip=1,npt
                lbuf => elbuf_tab(ng)%bufly(1)%lbuf(ip,1,1)
                do i=1,nel
                  if(gbuf%off(i) == zero) then
                    vol0(i)=zero
                  else
                    vol0(i)=lbuf%vol(i)
                  endif
                  my_address = sfem%tetra4_iad(ip,i+nft)
                  sfem%tetra_fsky(my_address,4+ip) = vol0(i)
                enddo
              enddo          
            else
              call s10volnod3(
     .                      x,      nc,     gbuf%off,
     .                      volg,   xdp,     nel,          npt,
     .                      ismstr,sfem,nft)

              do i=1,nel
                if(gbuf%off(i) == zero) then
                  vol0(i)=zero
                else
                  vol0(i)=gbuf%vol(i)
                endif
              enddo
              do i=1,nel
                do j=1,4
                  my_address = sfem%tetra4_iad(j,i+nft)
                  sfem%tetra_fsky(my_address,2) = vol0(i)
                enddo
              enddo                    
            endif            
          endif
        enddo
!$omp end do
!$omp end parallel

        if(.not.allocated(sfem%r_request)) allocate(sfem%r_request(nspmd))
        if(.not.allocated(sfem%s_request)) allocate(sfem%s_request(nspmd))
        if(.not.allocated(sfem%r_index)) allocate(sfem%r_index(nspmd))
        if(.not.allocated(sfem%s_index)) allocate(sfem%s_index(nspmd))        
        sfem%r_request_nb = 0
        sfem%s_request_nb = 0
        do i=1,nspmd
          if(i-1/=ispmd.and.sfem%spmd(i)%r_size>0) then
            allocate(sfem%spmd(i)%r_buffer(sfem%spmd(i)%r_size,sfem%tetra_fsky_dim2))
            sfem%r_request_nb = sfem%r_request_nb + 1
            sfem%r_index(sfem%r_request_nb) = i
            call spmd_irecv(sfem%spmd(i)%r_buffer(:,1),sfem%spmd(i)%r_size*sfem%tetra_fsky_dim2,i-1, 
     .                      tag,sfem%r_request(sfem%r_request_nb),SPMD_COMM_WORLD)
          endif

          if(i-1/=ispmd.and.sfem%spmd(i)%s_size>0) then
            allocate(sfem%spmd(i)%s_buffer(sfem%spmd(i)%s_size,sfem%tetra_fsky_dim2)) 
            do j=1,sfem%tetra_fsky_dim2
              do k=1,sfem%spmd(i)%s_size ! loop over the S nodes
                my_address = sfem%spmd(i)%send_iad(k) ! get the adress of the Fsky value to be sent
                sfem%spmd(i)%s_buffer(k,j) = sfem%tetra_fsky(my_address,j) ! put the Fsky value into the send buffer
              enddo
            enddo
            sfem%s_request_nb = sfem%s_request_nb + 1
            sfem%s_index(sfem%s_request_nb) = i
            call spmd_isend(sfem%spmd(i)%s_buffer(:,1),sfem%spmd(i)%s_size*sfem%tetra_fsky_dim2,i-1, 
     .                      tag,sfem%s_request(sfem%s_request_nb),SPMD_COMM_WORLD)
          endif
        enddo

      else

        do i=1,sfem%r_request_nb
          call spmd_waitany(sfem%r_request_nb,sfem%r_request,my_index,status_mpi)
          proc_id  = sfem%r_index(my_index)
          do j=1,sfem%tetra_fsky_dim2
            do k=1,sfem%spmd(proc_id)%r_size ! loop over the S nodes
              my_address = sfem%spmd(proc_id)%rcv_iad(k) ! get the adress of the Fsky value to be received
              sfem%tetra_fsky(my_address,j) = sfem%spmd(proc_id)%r_buffer(k,j) ! save the received Fsky value into the right place
            enddo
          enddo
          deallocate(sfem%spmd(proc_id)%r_buffer)
        enddo

!$omp parallel 
!$omp+ private(node_id,fsky_nb,my_address,i,j,k)
!$omp do schedule(guided)
        do i=1,2*numnod      
          sfem_nodvar(i) = zero
        enddo
!$omp end do

!$omp do schedule(guided)        
        do i=1,sfem%node_nb ! loop over the nodes with SFEM option
          node_id = sfem%node_list(i) ! get the node id
          fsky_nb = sfem%node_iad(i+1) - sfem%node_iad(i) ! get the number of Fsky values 
          my_address = sfem%node_iad(i) ! get the address of the first Fsky value
          do k=1,sfem%sub_tetra_fsky_dim2(1)
            do j=1,fsky_nb
              sfem_nodvar(node_id) = sfem_nodvar(node_id) + sfem%tetra_fsky(my_address+j-1,k) ! sum up the Fsky values to get the nodal volume        
            enddo
          enddo
          do k=1,sfem%sub_tetra_fsky_dim2(2)
            do j=1,fsky_nb
              sfem_nodvar(node_id+numnod) = sfem_nodvar(node_id+numnod) + 
     .                                      sfem%tetra_fsky(my_address+j-1,sfem%sub_tetra_fsky_dim2(1)+k)
            enddo
          enddo
          if(sfem_nodvar(node_id+numnod)/=zero) sfem_nodvar(node_id) =  sfem_nodvar(node_id) / sfem_nodvar(node_id+numnod)
        enddo
!$omp end do        

!$omp do schedule(guided) collapse(2)
        do j=1,sfem%tetra_fsky_dim2
          do i=1,sfem%tetra_fsky_dim1
            sfem%tetra_fsky(i,j) = zero ! reset the Fsky array for the next step
          enddo
        enddo
!$omp end do
!$omp end parallel    

        call spmd_waitall(sfem%s_request_nb,sfem%s_request)

        do i=1,nspmd
          if(allocated(sfem%spmd(i)%s_buffer)) deallocate(sfem%spmd(i)%s_buffer)
        enddo
        if(allocated(sfem%r_request)) deallocate(sfem%r_request)
        if(allocated(sfem%s_request)) deallocate(sfem%s_request)
        if(allocated(sfem%r_index)) deallocate(sfem%r_index)
        if(allocated(sfem%s_index)) deallocate(sfem%s_index)
      endif

C-----------------------------------------------
      RETURN
      END
