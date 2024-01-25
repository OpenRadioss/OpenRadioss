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
Chd|  INTER7_PENETRATION_MOD        source/interfaces/intsort/inter7_penetration.F
Chd|-- called by -----------
Chd|        INTER7_FILTER_CAND            source/interfaces/intsort/inter7_filter_cand.F
Chd|-- calls ---------------
Chd|        COLLISION_MOD                 source/interfaces/intsort/collision_mod.F
Chd|====================================================================
      MODULE INTER7_PENETRATION_MOD
      contains
!! \brief computes the penetration between a chuck of secondary nodes and a main surface/segment
!! \details the candidate for penetration are couple of secondary nodes and the main surface/segment
!! the couple is the result of the broad phase collision detection, keeping all possible penetration
!! for the given timestep, and also the possible penetration for the few next timesteps, until
!! a secondary node has moved more than the margin (relatively to the main surface/segment)
        subroutine inter7_penetration(jlt   ,margin ,x1    ,x2     ,x3   ,
     .                    x4    ,y1    ,y2    ,y3     ,y4   ,
     .                    z1    ,z2    ,z3    ,z4     ,xi   ,
     .                    ix3,  ix4, 
     .                    yi    ,zi    ,pene  ,gapv)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
          USE CONSTANT_MOD
          USE COLLISION_MOD , ONLY : GROUP_SIZE
          implicit none
#include "my_real.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
          integer, intent(in) :: jlt
          my_real, intent(in) :: gapv(GROUP_SIZE), margin
          my_real, intent(in) ::
     .         x1(GROUP_SIZE), x2(GROUP_SIZE), x3(GROUP_SIZE), x4(GROUP_SIZE), !< x coordinates of the 4 nodes of the surface
     .         y1(GROUP_SIZE), y2(GROUP_SIZE), y3(GROUP_SIZE), y4(GROUP_SIZE), !< y coordinates of the 4 nodes of the surface
     .         z1(GROUP_SIZE), z2(GROUP_SIZE), z3(GROUP_SIZE), z4(GROUP_SIZE), !< z coordinates of the 4 nodes of the surface
     .         xi(GROUP_SIZE), yi(GROUP_SIZE), zi(GROUP_SIZE)
          my_real, intent(inout) ::  pene(GROUP_SIZE) !< secondary nodes and penetration
          integer, intent(in) :: ix3(GROUP_SIZE), ix4(GROUP_SIZE) !< index of the 3rd and 4th nodes of the surface
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
          integer i, i3n
          my_real
     .     X0(GROUP_SIZE), Y0(GROUP_SIZE), Z0(GROUP_SIZE), GAP2(GROUP_SIZE),
     .     NX1(GROUP_SIZE), NX2(GROUP_SIZE), NX3(GROUP_SIZE), NX4(GROUP_SIZE),
     .     NY1(GROUP_SIZE), NY2(GROUP_SIZE), NY3(GROUP_SIZE), NY4(GROUP_SIZE),
     .     NZ1(GROUP_SIZE), NZ2(GROUP_SIZE), NZ3(GROUP_SIZE), NZ4(GROUP_SIZE),
     .     LB1(GROUP_SIZE), LB2(GROUP_SIZE), LB3(GROUP_SIZE), LB4(GROUP_SIZE),
     .     LC1(GROUP_SIZE), LC2(GROUP_SIZE), LC3(GROUP_SIZE), LC4(GROUP_SIZE),
     .     AL1(GROUP_SIZE), AL2(GROUP_SIZE), AL3(GROUP_SIZE), AL4(GROUP_SIZE),
     .     P1(GROUP_SIZE),  P2(GROUP_SIZE),  P3(GROUP_SIZE),  P4(GROUP_SIZE),
     .     X01(GROUP_SIZE),  X02(GROUP_SIZE),  X03(GROUP_SIZE), X04(GROUP_SIZE),
     .     Y01(GROUP_SIZE),  Y02(GROUP_SIZE),  Y03(GROUP_SIZE), Y04(GROUP_SIZE),
     .     Z01(GROUP_SIZE),  Z02(GROUP_SIZE),  Z03(GROUP_SIZE), Z04(GROUP_SIZE),
     .     XI1(GROUP_SIZE),  XI2(GROUP_SIZE),  XI3(GROUP_SIZE), XI4(GROUP_SIZE),
     .     YI1(GROUP_SIZE),  YI2(GROUP_SIZE),  YI3(GROUP_SIZE), YI4(GROUP_SIZE),
     .     ZI1(GROUP_SIZE),  ZI2(GROUP_SIZE),  ZI3(GROUP_SIZE), ZI4(GROUP_SIZE),
     .     HLB1(GROUP_SIZE), HLC1(GROUP_SIZE), HLB2(GROUP_SIZE),HLC2(GROUP_SIZE),
     .     HLB3(GROUP_SIZE), HLC3(GROUP_SIZE), HLB4(GROUP_SIZE),HLC4(GROUP_SIZE)
          my_real
     .         s2,d1,d2,d3,d4,
     .         x12,x23,x34,x41,xi0,sx1,sx2,sx3,sx4,sx0,
     .         y12,y23,y34,y41,yi0,sy1,sy2,sy3,sy4,sy0,
     .         z12,z23,z34,z41,zi0,sz1,sz2,sz3,sz4,sz0,
     .         la, hla, aaa, zoneinf
c-----------------------------------------------
! the aim is to compute the penetration between a chuck of secondary nodes and a main surface
! the candidate for penetration are couple of secondary nodes and the main surface
! we keep the nodes that are in the zone of influence of the main surface
! we will perform a new collision detection when a node has moved more than the zone of influence (relatively to the main surface)
          do i = 1, jlt
            zoneinf = gapv(i)+margin !< zone of influence: gap of the element + margin
            gap2(i)= zoneinf*zoneinf
          end do
c
c  0 quad, 1 tri ,2 mixte
      i3n=0
      do i=1,jlt
       if(ix3(i)==ix4(i))i3n=i3n+1
      enddo
      if(i3n==jlt)then
        i3n=1
      elseif(i3n/=0)then
        i3n=2
      endif
c--------------------------------------------------------
c   quadrangle
c--------------------------------------------------------
          if(i3n==0) then
       do i=1,jlt
         x0(i) = fourth*(x1(i)+x2(i)+x3(i)+x4(i))
         y0(i) = fourth*(y1(i)+y2(i)+y3(i)+y4(i))
         z0(i) = fourth*(z1(i)+z2(i)+z3(i)+z4(i)) 
       enddo
c--------------------------------------------------------
c  triangle
c--------------------------------------------------------
      elseif(i3n==2) then
       do i=1,jlt
        if(ix3(i)/=ix4(i))then
         x0(i) = fourth*(x1(i)+x2(i)+x3(i)+x4(i))
         y0(i) = fourth*(y1(i)+y2(i)+y3(i)+y4(i))
         z0(i) = fourth*(z1(i)+z2(i)+z3(i)+z4(i)) 
          else
         x0(i) = x3(i)
         y0(i) = y3(i)
         z0(i) = z3(i)
        endif
       enddo
          endif
c--------------------------------------------------------
c  triangle
c--------------------------------------------------------
          if(i3n==1) then
       do i=1,jlt
c
       x01(i) = x1(i) - x3(i)
       y01(i) = y1(i) - y3(i)
       z01(i) = z1(i) - z3(i)
c
       x02(i) = x2(i) - x3(i)
       y02(i) = y2(i) - y3(i)
       z02(i) = z2(i) - z3(i)
c
       xi0 = x3(i) - xi(i)
       yi0 = y3(i) - yi(i)
       zi0 = z3(i) - zi(i)
c
       xi1(i) = x1(i) - xi(i)
       yi1(i) = y1(i) - yi(i)
       zi1(i) = z1(i) - zi(i)
c
       xi2(i) = x2(i) - xi(i)
       yi2(i) = y2(i) - yi(i)
       zi2(i) = z2(i) - zi(i)
c
              sx1 = yi0*zi1(i) - zi0*yi1(i)
              sy1 = zi0*xi1(i) - xi0*zi1(i)
              sz1 = xi0*yi1(i) - yi0*xi1(i)
c
              sx2 = yi0*zi2(i) - zi0*yi2(i)
              sy2 = zi0*xi2(i) - xi0*zi2(i)
              sz2 = xi0*yi2(i) - yi0*xi2(i)
c
       sx0 = y01(i)*z02(i) - z01(i)*y02(i)
       sy0 = z01(i)*x02(i) - x01(i)*z02(i)
       sz0 = x01(i)*y02(i) - y01(i)*x02(i)
              s2 = 1./max(em30,sx0*sx0 + sy0*sy0 + sz0*sz0)
c
              lb1(i) = -(sx0*sx2 + sy0*sy2 + sz0*sz2) * s2
              lc1(i) =  (sx0*sx1 + sy0*sy1 + sz0*sz1) * s2
c
        aaa    = one/max(em30,x01(i)*x01(i)+y01(i)*y01(i)+z01(i)*z01(i))
              hlc1(i)= lc1(i)*abs(lc1(i))*aaa
        al1(i) = -(xi0*x01(i)+yi0*y01(i)+zi0*z01(i))*aaa
              al1(i) = max(zero,min(one,al1(i)))
        aaa    = one/max(em30,x02(i)*x02(i)+y02(i)*y02(i)+z02(i)*z02(i))
              hlb1(i)= lb1(i)*abs(lb1(i))*aaa
        al2(i) = -(xi0*x02(i)+yi0*y02(i)+zi0*z02(i))*aaa
              al2(i) = max(zero,min(one,al2(i)))
            enddo
c
            do i=1,jlt
        x12 = x2(i) - x1(i)
        y12 = y2(i) - y1(i)
        z12 = z2(i) - z1(i)
              la = one - lb1(i) - lc1(i)
              aaa = one / max(em20,x12*x12+y12*y12+z12*z12)
              hla= la*abs(la) * aaa
              if(la<zero.and.
     +           hla<=hlb1(i).and.hla<=hlc1(i))then
                lb1(i) = (xi2(i)*x12+yi2(i)*y12+zi2(i)*z12)*aaa
                lb1(i) = max(zero,min(one,lb1(i)))
                lc1(i) = one - lb1(i)
              elseif(lb1(i)<zero.and.
     +              hlb1(i)<=hlc1(i).and.hlb1(i)<=hla)then
                lb1(i) = zero
                lc1(i) = al2(i)
              elseif(lc1(i)<zero.and.
     +              hlc1(i)<=hla.and.hlc1(i)<=hlb1(i))then
                lc1(i) = zero
                lb1(i) = al1(i)
              endif
            enddo
c
            do i=1,jlt
c
       nx1(i) = xi(i)-(x3(i) + lb1(i)*x01(i) + lc1(i)*x02(i))
       ny1(i) = yi(i)-(y3(i) + lb1(i)*y01(i) + lc1(i)*y02(i))
       nz1(i) = zi(i)-(z3(i) + lb1(i)*z01(i) + lc1(i)*z02(i))
       p1(i) = nx1(i)*nx1(i) + ny1(i)*ny1(i) +nz1(i)*nz1(i)
c !!!!!!!!!!!!!!!!!!!!!!!
c  pene = gap^2 - dist^2 utilise pour tester si non nul
c!!!!!!!!!!!!!!!!!!!!!!!!
       pene(i) = max(zero, gap2(i) - p1(i))
c
            enddo
c--------------------------------------------------------
c  mixed group of quadrangle and triangles
c--------------------------------------------------------
      else
c
       do i=1,jlt
c
        x01(i) = x1(i) - x0(i)
        y01(i) = y1(i) - y0(i)
        z01(i) = z1(i) - z0(i)
c
        x02(i) = x2(i) - x0(i)
        y02(i) = y2(i) - y0(i)
        z02(i) = z2(i) - z0(i)
c
        x03(i) = x3(i) - x0(i)
        y03(i) = y3(i) - y0(i)
        z03(i) = z3(i) - z0(i)
c
        x04(i) = x4(i) - x0(i)
        y04(i) = y4(i) - y0(i)
        z04(i) = z4(i) - z0(i)
c
        xi0 = x0(i) - xi(i)
        yi0 = y0(i) - yi(i)
        zi0 = z0(i) - zi(i)
c
        xi1(i) = x1(i) - xi(i)
        yi1(i) = y1(i) - yi(i)
        zi1(i) = z1(i) - zi(i)
c
        xi2(i) = x2(i) - xi(i)
        yi2(i) = y2(i) - yi(i)
        zi2(i) = z2(i) - zi(i)
c
        xi3(i) = x3(i) - xi(i)
        yi3(i) = y3(i) - yi(i)
        zi3(i) = z3(i) - zi(i)
c
        xi4(i) = x4(i) - xi(i)
        yi4(i) = y4(i) - yi(i)
        zi4(i) = z4(i) - zi(i)
c
              sx1 = yi0*zi1(i) - zi0*yi1(i)
              sy1 = zi0*xi1(i) - xi0*zi1(i)
              sz1 = xi0*yi1(i) - yi0*xi1(i)
c
              sx2 = yi0*zi2(i) - zi0*yi2(i)
              sy2 = zi0*xi2(i) - xi0*zi2(i)
              sz2 = xi0*yi2(i) - yi0*xi2(i)
c
        sx0 = y01(i)*z02(i) - z01(i)*y02(i)
        sy0 = z01(i)*x02(i) - x01(i)*z02(i)
        sz0 = x01(i)*y02(i) - y01(i)*x02(i)
              s2 = one/max(em30,sx0*sx0 + sy0*sy0 + sz0*sz0)
c
              lb1(i) = -(sx0*sx2 + sy0*sy2 + sz0*sz2) * s2
              lc1(i) =  (sx0*sx1 + sy0*sy1 + sz0*sz1) * s2
c
              sx3 = yi0*zi3(i) - zi0*yi3(i)
              sy3 = zi0*xi3(i) - xi0*zi3(i)
              sz3 = xi0*yi3(i) - yi0*xi3(i)
c
        sx0 = y02(i)*z03(i) - z02(i)*y03(i)
        sy0 = z02(i)*x03(i) - x02(i)*z03(i)
        sz0 = x02(i)*y03(i) - y02(i)*x03(i)
              s2 = one/max(em30,sx0*sx0 + sy0*sy0 + sz0*sz0)
c
              lb2(i) = -(sx0*sx3 + sy0*sy3 + sz0*sz3) * s2
              lc2(i) =  (sx0*sx2 + sy0*sy2 + sz0*sz2) * s2
c
              sx4 = yi0*zi4(i) - zi0*yi4(i)
              sy4 = zi0*xi4(i) - xi0*zi4(i)
              sz4 = xi0*yi4(i) - yi0*xi4(i)
c
        sx0 = y03(i)*z04(i) - z03(i)*y04(i)
        sy0 = z03(i)*x04(i) - x03(i)*z04(i)
        sz0 = x03(i)*y04(i) - y03(i)*x04(i)
              s2 = one/max(em30,sx0*sx0 + sy0*sy0 + sz0*sz0)
c
              lb3(i) = -(sx0*sx4 + sy0*sy4 + sz0*sz4) * s2
              lc3(i) =  (sx0*sx3 + sy0*sy3 + sz0*sz3) * s2
c
        sx0 = y04(i)*z01(i) - z04(i)*y01(i)
        sy0 = z04(i)*x01(i) - x04(i)*z01(i)
        sz0 = x04(i)*y01(i) - y04(i)*x01(i)
              s2 = one/max(em30,sx0*sx0 + sy0*sy0 + sz0*sz0)
c
              lb4(i) = -(sx0*sx1 + sy0*sy1 + sz0*sz1) * s2
              lc4(i) =  (sx0*sx4 + sy0*sy4 + sz0*sz4) * s2
c
        aaa    = one/max(em30,x01(i)*x01(i)+y01(i)*y01(i)+z01(i)*z01(i))
              hlc1(i)= lc1(i)*abs(lc1(i))*aaa
              hlb4(i)= lb4(i)*abs(lb4(i))*aaa
        al1(i) = -(xi0*x01(i)+yi0*y01(i)+zi0*z01(i))*aaa
              al1(i) = max(zero,min(one,al1(i)))
        aaa    = one/max(em30,x02(i)*x02(i)+y02(i)*y02(i)+z02(i)*z02(i))
              hlc2(i)= lc2(i)*abs(lc2(i))*aaa
              hlb1(i)= lb1(i)*abs(lb1(i))*aaa
        al2(i) = -(xi0*x02(i)+yi0*y02(i)+zi0*z02(i))*aaa
              al2(i) = max(zero,min(one,al2(i)))
        aaa    = one/max(em30,x03(i)*x03(i)+y03(i)*y03(i)+z03(i)*z03(i))
              hlc3(i)= lc3(i)*abs(lc3(i))*aaa
              hlb2(i)= lb2(i)*abs(lb2(i))*aaa
        al3(i) = -(xi0*x03(i)+yi0*y03(i)+zi0*z03(i))*aaa
              al3(i) = max(zero,min(one,al3(i)))
        aaa    = one/max(em30,x04(i)*x04(i)+y04(i)*y04(i)+z04(i)*z04(i))
              hlc4(i)= lc4(i)*abs(lc4(i))*aaa
              hlb3(i)= lb3(i)*abs(lb3(i))*aaa
        al4(i) = -(xi0*x04(i)+yi0*y04(i)+zi0*z04(i))*aaa
              al4(i) = max(zero,min(one,al4(i)))
c
            enddo
c
            do i=1,jlt
        x12 = x2(i) - x1(i)
        y12 = y2(i) - y1(i)
        z12 = z2(i) - z1(i)
              la = one - lb1(i) - lc1(i)
              aaa = one / max(em20,x12*x12+y12*y12+z12*z12)
              hla= la*abs(la) * aaa
              if(la<zero.and.
     +           hla<=hlb1(i).and.hla<=hlc1(i))then
                lb1(i) = (xi2(i)*x12+yi2(i)*y12+zi2(i)*z12) * aaa
                lb1(i) = max(zero,min(one,lb1(i)))
                lc1(i) = one - lb1(i)
              elseif(lb1(i)<zero.and.
     +               hlb1(i)<=hlc1(i).and.hlb1(i)<=hla)then
                lb1(i) = zero
                lc1(i) = al2(i)
              elseif(lc1(i)<zero.and.
     +               hlc1(i)<=hla.and.hlc1(i)<=hlb1(i))then
                lc1(i) = zero
                lb1(i) = al1(i)
              endif
            enddo
c
            do i=1,jlt
          x23 = x3(i) - x2(i)
          y23 = y3(i) - y2(i)
          z23 = z3(i) - z2(i)
              la = one - lb2(i) - lc2(i)
              aaa = one / max(em20,x23*x23+y23*y23+z23*z23)
              hla= la*abs(la) * aaa
              if(la<zero.and.
     +           hla<=hlb2(i).and.hla<=hlc2(i))then
                lb2(i) = (xi3(i)*x23+yi3(i)*y23+zi3(i)*z23)*aaa
                lb2(i) = max(zero,min(one,lb2(i)))
                lc2(i) = one - lb2(i)
              elseif(lb2(i)<zero.and.
     +               hlb2(i)<=hlc2(i).and.hlb2(i)<=hla)then
                lb2(i) = zero
                lc2(i) = al3(i)
              elseif(lc2(i)<zero.and.
     +               hlc2(i)<=hla.and.hlc2(i)<=hlb2(i))then
                lc2(i) = zero
                lb2(i) = al2(i)
              endif
            enddo
c
            do i=1,jlt
          x34 = x4(i) - x3(i)
          y34 = y4(i) - y3(i)
          z34 = z4(i) - z3(i)
              la = one - lb3(i) - lc3(i)
              aaa = one / max(em20,x34*x34+y34*y34+z34*z34)
              hla= la*abs(la) * aaa
              if(la<zero.and.
     +           hla<=hlb3(i).and.hla<=hlc3(i))then
                lb3(i) = (xi4(i)*x34+yi4(i)*y34+zi4(i)*z34)*aaa
                lb3(i) = max(zero,min(one,lb3(i)))
                lc3(i) = one - lb3(i)
              elseif(lb3(i)<zero.and.
     +               hlb3(i)<=hlc3(i).and.hlb3(i)<=hla)then
                lb3(i) = zero
                lc3(i) = al4(i)
              elseif(lc3(i)<zero.and.
     +               hlc3(i)<=hla.and.hlc3(i)<=hlb3(i))then
                lc3(i) = zero
                lb3(i) = al3(i)
              endif
            enddo
c
            do i=1,jlt
          x41 = x1(i) - x4(i)
          y41 = y1(i) - y4(i)
          z41 = z1(i) - z4(i)
              la = one - lb4(i) - lc4(i)
              aaa = one / max(em20,x41*x41+y41*y41+z41*z41)
              hla= la*abs(la) * aaa
              if(la<zero.and.
     +           hla<=hlb4(i).and.hla<=hlc4(i))then
                lb4(i) = (xi1(i)*x41+yi1(i)*y41+zi1(i)*z41)*aaa
                lb4(i) = max(zero,min(one,lb4(i)))
                lc4(i) = one - lb4(i)
              elseif(lb4(i)<zero.and.
     +               hlb4(i)<=hlc4(i).and.hlb4(i)<=hla)then
                lb4(i) = zero
                lc4(i) = al1(i)
              elseif(lc4(i)<zero.and.
     +               hlc4(i)<=hla.and.hlc4(i)<=hlb4(i))then
                lc4(i) = zero
                lb4(i) = al4(i)
              endif
            enddo

            do i=1,jlt
c
        nx1(i) = xi(i)-(x0(i) + lb1(i)*x01(i) + lc1(i)*x02(i))
        ny1(i) = yi(i)-(y0(i) + lb1(i)*y01(i) + lc1(i)*y02(i))
        nz1(i) = zi(i)-(z0(i) + lb1(i)*z01(i) + lc1(i)*z02(i))
        p1(i) = nx1(i)*nx1(i) + ny1(i)*ny1(i) +nz1(i)*nz1(i)
        d1 = max(zero, gap2(i) - p1(i))
c
        nx2(i) = xi(i)-(x0(i) + lb2(i)*x02(i) + lc2(i)*x03(i))
        ny2(i) = yi(i)-(y0(i) + lb2(i)*y02(i) + lc2(i)*y03(i))
        nz2(i) = zi(i)-(z0(i) + lb2(i)*z02(i) + lc2(i)*z03(i))
        p2(i) = nx2(i)*nx2(i) + ny2(i)*ny2(i) +nz2(i)*nz2(i)
        d2 = max(zero, gap2(i) - p2(i))
c
        nx3(i) = xi(i)-(x0(i) + lb3(i)*x03(i) + lc3(i)*x04(i))
        ny3(i) = yi(i)-(y0(i) + lb3(i)*y03(i) + lc3(i)*y04(i))
        nz3(i) = zi(i)-(z0(i) + lb3(i)*z03(i) + lc3(i)*z04(i))
        p3(i) = nx3(i)*nx3(i) + ny3(i)*ny3(i) +nz3(i)*nz3(i)
        d3 = max(zero, gap2(i) - p3(i))
c
        nx4(i) = xi(i)-(x0(i) + lb4(i)*x04(i) + lc4(i)*x01(i))
        ny4(i) = yi(i)-(y0(i) + lb4(i)*y04(i) + lc4(i)*y01(i))
        nz4(i) = zi(i)-(z0(i) + lb4(i)*z04(i) + lc4(i)*z01(i))
        p4(i) = nx4(i)*nx4(i) + ny4(i)*ny4(i) +nz4(i)*nz4(i)
        d4 = max(zero, gap2(i) - p4(i))
c !!!!!!!!!!!!!!!!!!!!!!!
c  pene = gap^2 - dist^2 utilise pour tester si non nul
c!!!!!!!!!!!!!!!!!!!!!!!!
              pene(i) = max(d1,d2,d3,d4)
            enddo
          endif
          return
        end
      end module
