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
!||    s4alesfem      ../engine/source/elements/solid/solide4_sfem/s4alesfem.F
!||--- called by ------------------------------------------------------
!||    alemain        ../engine/source/ale/alemain.F
!||--- calls      -----------------------------------------------------
!||    initbuf        ../engine/share/resol/initbuf.F
!||    s4volnod3      ../engine/source/elements/solid/solide4_sfem/s4volnod3.F
!||    spmd_waitall   ../engine/source/mpi/spmd_wait.F90
!||    spmd_waitany   ../engine/source/mpi/spmd_wait.F90
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod   ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    element_mod    ../common_source/modules/elements/element_mod.F90
!||    initbuf_mod    ../engine/share/resol/initbuf.F
!||    sfem_mod       ../common_source/modules/elements/sfem_mod.F90
!||    spmd_mod       ../engine/source/mpi/spmd_mod.F90
!||====================================================================
      SUBROUTINE S4ALESFEM(IPARG,IXS,X,ELBUF_TAB,SFEM_NODVAR, S_SFEM_NODVAR,PM,
     .                     ispmd,sfem)
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
#include      "param_c.inc"
#include "spmd.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: S_SFEM_NODVAR
      INTEGER IXS(NIXS,*),IPARG(NPARG,*)
      integer, intent(in) :: ispmd !< mpi processor id      
      my_real X(*),SFEM_NODVAR(S_SFEM_NODVAR),PM(NPROPM,*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      type(sfem_), intent(inout) :: sfem      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      logical :: condition
      INTEGER NG, I, J, I1, I2, I3, I4, K, LENR,NEL
      INTEGER NC1(MVSIZ),NC2(MVSIZ),NC3(MVSIZ),NC4(MVSIZ)
      INTEGER MAT(MVSIZ)
      integer :: fsky_nb,nnod
      integer :: my_address,my_index
      integer :: node_id,proc_id      
      my_real MASS(MVSIZ)

C
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      integer, parameter :: tag = 10004
      integer :: status_mpi(mpi_status_size)      
C----------------------------------------------------
C     COMPUTE NODAL VOLUME & MASS FOR ALL TETRAHEDRON
C----------------------------------------------------

        do ng=1,ngroup
          nnod = iparg(28,ng) ! get the number of node per element for the current group
          if (nnod /= 4) cycle ! only tetrahedral elements are supported
          call initbuf(iparg,ng, 
     .                  mtn,llt,nft,iad,ity, 
     .                  npt,jale,ismstr,jeul,jtur, 
     .                  jthe,jlag,jmult,jhbe,jivf, 
     .                  nvaux,jpor,jcvt,jclose,jplasol, 
     .                  irep,iint,igtyp,israt,isrot, 
     .                  icsen,isorth,isorthg,ifailure,jsms)
          condition = (nnod==4.and.isrot==3)
          if(.not.condition) cycle ! for 4-node elements, only ISROT=3 is supported
          if(jeul==1) cycle
          if(jale==1) cycle
          lft=1
          nel = llt

          do i=1,nel
            j=i+nft
            mat(i)=ixs(1,j)
            nc1(i)=ixs(2,j)
            nc2(i)=ixs(4,j)
            nc3(i)=ixs(7,j)
            nc4(i)=ixs(6,j)
          enddo
          gbuf => elbuf_tab(ng)%gbuf
          if(nnod==4.and.isrot==3) then
            call s4volnod3( 
     .            x,       nc1,       nc2, 
     .            nc3,      nc4,     gbuf%off,  gbuf%smstr, 
     .            nel,      ismstr,sfem,nft)
            do i=1,nel
              mass(i) = gbuf%rho(i)/pm(1,mat(i))
            enddo
            do i=1,nel
              do j=1,4
                my_address = sfem%tetra4_iad(j,i+nft)
                sfem%tetra_fsky(my_address,2) = mass(i)
              enddo
            enddo           
          endif
        enddo

        if(.not.allocated(sfem%r_request)) allocate(sfem%r_request(nspmd))
        if(.not.allocated(sfem%s_request)) allocate(sfem%s_request(nspmd))
        if(.not.allocated(sfem%r_index)) allocate(sfem%r_index(nspmd))
        if(.not.allocated(sfem%s_index)) allocate(sfem%s_index(nspmd))        
        sfem%r_request_nb = 0
        sfem%s_request_nb = 0
        do i=1,nspmd
          if(i-1/=ispmd.and.sfem%spmd(i)%r_size>0) then
            allocate(sfem%spmd(i)%r_buffer(sfem%spmd(i)%r_size,sfem%tetra_fsky_dim2))
            sfem%spmd(i)%r_buffer(1:sfem%spmd(i)%r_size,1:sfem%tetra_fsky_dim2) = -123456789. ! for debug
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
        
        sfem_nodvar(1:2*numnod) = zero
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
        enddo
#include "vectorize.inc"
        do i=1,sfem%node_nb ! loop over the nodes with SFEM option
          node_id = sfem%node_list(i) ! get the node id
          if(sfem_nodvar(node_id+numnod)/=zero) sfem_nodvar(node_id) =  sfem_nodvar(node_id) / sfem_nodvar(node_id+numnod)
        enddo      

        sfem%tetra_fsky(1:sfem%tetra_fsky_dim1,1:sfem%tetra_fsky_dim2) = zero ! reset the Fsky array for the next step

        call spmd_waitall(sfem%s_request_nb,sfem%s_request)

        do i=1,nspmd
          if(allocated(sfem%spmd(i)%s_buffer)) deallocate(sfem%spmd(i)%s_buffer)
        enddo
        if(allocated(sfem%r_request)) deallocate(sfem%r_request)
        if(allocated(sfem%s_request)) deallocate(sfem%s_request)
        if(allocated(sfem%r_index)) deallocate(sfem%r_index)
        if(allocated(sfem%s_index)) deallocate(sfem%s_index)      

      RETURN
      END
