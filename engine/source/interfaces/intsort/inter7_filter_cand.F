Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  I7STO                         source/interfaces/intsort/i7sto.F
Chd|-- called by -----------
Chd|        I7TRI                         source/interfaces/intsort/i7tri.F
Chd|        I7TRIVOX                      source/interfaces/intsort/i7trivox.F
Chd|-- calls ---------------
Chd|        I7COR3T                       source/interfaces/int07/i7cor3t.F
Chd|        I7PEN3                        source/interfaces/intsort/i7pen3.F
Chd|====================================================================
      MODULE INTER7_FILTER_CAND_MOD
      contains
!! \brief broad phase filtering of candidate pairs
!! \details input: nodes and segment sharing the same voxel, output: filtered candidate pairs
        SUBROUTINE INTER7_FILTER_CAND(
     1        j_stok,irect  ,x     ,nsv   ,ii_stok,
     2        cand_n,cand_e ,mulnsn,margin  ,
     3        i_mem ,prov_n ,prov_e,eshift,inacti ,
     4        ifq   ,cand_a ,cand_p,ifpen ,nsn    ,
     5        oldnum,nsnrold,igap  ,gap   ,gap_s  ,
     6        gap_m ,gapmin ,gapmax,curv_max,
     7        gap_s_l,gap_m_l,drad,itied    ,
     8        cand_f ,dgapload,
     .        nsnr,
     .        xrem ,s_xrem)


          USE COLLISION_MOD , ONLY : GROUP_SIZE
          USE INTER7_GATHER_CAND_MOD , ONLY: INTER7_GATHER_CAND
          USE INTER7_PENETRATION_MOD , ONLY: INTER7_PENETRATION
          USE CONSTANT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
          implicit none
#include   "my_real.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
          integer, intent(inout) :: i_mem !< memory error flag
          integer, intent(in) :: nsn  !< number secondary nodes
          integer, intent(in) :: nsnrold !< number of remote nodes in the previous collision detection
          integer, intent(in) :: igap !< gap formulation
          integer, intent(in) :: itied !< tied contact formulation
          integer, intent(in) :: j_stok !< number of candidate pairs to be filtered
          integer, intent(in) :: mulnsn !< allocated size of cand_n, cand_e, cand_p, cand_f
          integer, intent(in) :: inacti !< initial penetration formulation
          integer, intent(in) :: ifq !< friction formulation ?
          integer, intent(in) :: eshift !< shift for segment numbering
          integer, intent(in) :: irect(4,*) !< 4 nodes of the segment
          integer, intent(in) :: nsv(*) !< secondary node ids
          integer, intent(inout) :: cand_n(*) !< output: node number of the candidate pair
          integer, intent(inout) :: cand_e(*) !< output: segment number of the candidate pair
          integer, intent(inout) :: cand_a(*)
          integer, intent(inout) :: prov_n(j_stok) !< input node number of the candidate pair, before filtering
          integer, intent(inout) :: prov_e(j_stok) !< input segment number of the candidate pair, before filtering
          integer, intent(inout) :: ifpen(*) !
          integer, intent(in) :: oldnum(*)
          integer, intent(inout) :: ii_stok !< current total number of candidate pairs
          my_real, intent(in) :: drad
          my_real, intent(in) :: dgapload
          my_real, intent(in) :: x(3,*) !< coordinates of all the nodes
          my_real, intent(inout) :: cand_p(*)
          my_real, intent(in) :: gap_s(*)
          my_real, intent(in) :: gap_m(*)
          my_real, intent(in) :: margin
          my_real, intent(in) :: gap
          my_real, intent(in) :: gapmin
          my_real, intent(in) :: gapmax
          my_real, intent(in) :: curv_max(*)
          my_real, intent(in) :: gap_s_l(*)
          my_real, intent(in) :: gap_m_l(*)
          my_real, intent(inout) :: cand_f(8,*)
          integer, intent(in) :: nsnr !< number of remote nodes
          integer, intent(in) :: s_xrem !< size of xrem
          my_real, intent(in) :: xrem(s_xrem, nsnr)
c-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
          integer :: i,k_stok,i_stok,n,ne,j
          integer :: inacti_l, itied_l, ifq_l
          integer :: j_start, j_end
          integer, parameter :: itype = 7
          my_real :: x1(GROUP_SIZE) !< x coordinate of the first node of the quadrangle or triangle
          my_real :: x2(GROUP_SIZE)
          my_real :: x3(GROUP_SIZE)
          my_real :: x4(GROUP_SIZE)
          my_real :: y1(GROUP_SIZE)
          my_real :: y2(GROUP_SIZE)
          my_real :: y3(GROUP_SIZE)
          my_real :: y4(GROUP_SIZE)
          my_real :: z1(GROUP_SIZE)
          my_real :: z2(GROUP_SIZE)
          my_real :: z3(GROUP_SIZE)
          my_real :: z4(GROUP_SIZE)
          my_real :: xi(GROUP_SIZE) !< x coordinate of the second nodes
          my_real :: yi(GROUP_SIZE)
          my_real :: zi(GROUP_SIZE)
          my_real :: pene(GROUP_SIZE)
          my_real :: gapv(GROUP_SIZE)
          integer :: ix1(GROUP_SIZE)
          integer :: ix2(GROUP_SIZE)
          integer :: ix3(GROUP_SIZE)
          integer :: ix4(GROUP_SIZE)
          logical :: exit_flag


C-----------------------------------------------

          call inter7_gather_cand(j_stok  ,x    ,irect ,nsv   ,prov_e  ,
     1                  prov_n  ,igap ,gap   ,x1    ,x2      ,
     2                  x3      ,x4   ,y1    ,y2    ,y3      ,
     3                  y4      ,z1   ,z2    ,z3    ,z4      ,
     4                  xi      ,yi   ,zi    ,
     5                  nsn     ,gap_s   , ix1, ix2, ix3, ix4,
     6                  gap_m   ,gapv ,gapmax, gapmin, curv_max,
     7                  itype   ,gap_s_l,gap_m_l,
     8                  drad    ,dgapload, nsnr,
     .                  s_xrem, xrem)
          call inter7_penetration(j_stok ,margin ,x1    ,x2     ,x3   ,
     .                 x4    ,y1    ,y2    ,y3     ,y4   ,
     .                 z1    ,z2    ,z3    ,z4     ,xi   ,
     .                 ix3,   ix4,
     .                 yi    ,zi    ,pene  ,gapv )
c-----------------------------------------------
c removal of old candidates already stored (initial penetration)
c-----------------------------------------------
          if(inacti==5.or.inacti==6.or.inacti==7.or.ifq>0.or.itied/=0)then
            do i=1,j_stok
              if(pene(i)/=zero)then
                n  = prov_n(i)
                ne = prov_e(i)+eshift
                if(n>nsn) then
C numbering of previous collisions for remote nodes (spmd)
                  n = oldnum(n-nsn)+nsn
                  if(n==nsn) n = nsn+nsnrold+1
                end if
                j_start = cand_a(n)
                j_end = cand_a(n+1)-1
                do j = j_start, j_end
                  if(cand_e(j)==ne)then
                    pene(i)=zero
                    exit
                  endif
                enddo
              endif
            enddo
          endif
c-----------------------------------------------
          k_stok = 0
          do i=1,j_stok
            if(pene(i)/=zero) k_stok = k_stok + 1
          enddo
          if(k_stok==0)return
          exit_flag = .false.
!$omp critical
          i_stok = ii_stok
          if (i_stok + k_stok > mulnsn) then
              i_mem = 2
              exit_flag = .true.
          else
              ii_stok = i_stok + k_stok
          endif
!$omp end critical
          
          if (exit_flag) then
              return
          endif

          inacti_l = inacti
          itied_l = itied
          ifq_l = ifq
          do i=1,j_stok
            if(pene(i)/=zero)then
              i_stok = i_stok + 1
              cand_n(i_stok) = prov_n(i)
              cand_e(i_stok) = prov_e(i)+eshift
              if(ifq_l > 0)ifpen(i_stok)  = 0
              if(inacti_l==5 .or. inacti_l==6 .or. inacti_l==7) cand_p(i_stok) = zero
              if(itied_l /= 0) cand_f(1:8,i_stok)=zero
            endif
          enddo
          return
        end
      END MODULE
