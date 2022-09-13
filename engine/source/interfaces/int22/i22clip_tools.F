Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
        function CrossProd2D( v1, v2)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
          my_real         , intent(inout) :: v1(2),v2(2)
          my_real                      :: CrossProd2D
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------  
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------         
          CrossProd2D = v1(1)*v2(2) - v1(2)*v2(1)
          return
        end function CrossProd2D
C----------------------------------------------- 



        subroutine PolygonalClipping( basePolygon, clipPolygon, resultPolygon )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"        
          ! based on Sutherland Hodgman  algorithm
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
          type polygon
            my_real          :: node(16,2)
            integer :: NumNodes          
          end type polygon
          type(polygon)    :: basePolygon, clipPolygon, resultPolygon
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
          type(polygon)    :: tmpPolygon
          my_real          :: edge_P1(2),edge_P2(2)    ! vertices of edge to clipPolygon tmpPolygon
          integer :: i  
          tmpPolygon%NumNodes = clipPolygon%NumNodes
          IF(tmpPolygon%NumNodes > 0)THEN
            tmpPolygon%node(1:tmpPolygon%NumNodes,:) = clipPolygon%node(1:tmpPolygon%NumNodes,:)
          END IF
          
          do i=1,basePolygon%NumNodes-1 ! for each edge i of the polygon basePolygon
            edge_P1(:) = basePolygon%node(i,:)   !  node 1 of edge i
            edge_P2(:) = basePolygon%node(i+1,:) !  node 2 of edge i
            ! clipPolygon the work polygon by edge i
            call ClipEdge( tmpPolygon, edge_P1, edge_P2, resultPolygon)
            ! tmpPolygon <= resultPolygon
            tmpPolygon%NumNodes = resultPolygon%NumNodes
            tmpPolygon%node(1:tmpPolygon%NumNodes,:) = resultPolygon%node(1:tmpPolygon%NumNodes,:)
          end do 
        end subroutine PolygonalClipping
C----------------------------------------------- 



        subroutine ClipEdge( polyg, p1, p2, resultPolyg )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
          type polygon
            my_real          :: node(16,2)
            integer :: NumNodes          
          end type polygon
          type(polygon) :: polyg, resultPolyg
          my_real         , intent(inout) :: p1(2), p2(2) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------         
          my_real                      :: x1(2), x2(2), intersecPoint(2)
          integer  ::  i, k
          INTERFACE
            function intersectP(a,b,c,d,iflg)
              my_real         , intent(inout) :: a(2),b(2),c(2),d(2)
              integer, intent(in)             :: iflg
              my_real          intersectP(2)
            end function
            function IS_ON_1ST_HALF_PLANE(a,b,c)
              my_real         , intent(inout) :: a(2),b(2),c(2)
              logical                         :: IS_ON_1ST_HALF_PLANE
            end function
          END INTERFACE
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------           
          k = 0
          do i=1,polyg%NumNodes-1 ! for each edge i of poly
            x1(:) = polyg%node(i,:)   ! node 1 of edge i
            x2(:) = polyg%node(i+1,:) ! node 2 of edge i

            if ( IS_ON_1ST_HALF_PLANE(x1, p1, p2) ) then 
              if ( IS_ON_1ST_HALF_PLANE(x2, p1, p2) ) then
                ! add the node 2 to the output polygon
                k = k+1
                resultPolyg%node(k,:) = x2(:)

              else ! node i+1 is outside
                intersecPoint = intersectP(x1, x2, p1,p2  ,1)
                k = k+1
                resultPolyg%node(k,:) = intersecPoint(:)
              end if
            else ! node i is outside
              if ( IS_ON_1ST_HALF_PLANE(x2, p1, p2) ) then
                intersecPoint = intersectP(x1, x2, p1,p2  ,1)
                k = k+1
                resultPolyg%node(k,:) = intersecPoint(:)

                k = k+1
                resultPolyg%node(k,:) = x2(:)
              end if
            end if
          end do
          if (k > 0) then
            ! if the last vertice is not equal to the first one
            if ( (resultPolyg%node(1,1) /= resultPolyg%node(k,1)) .or.  (resultPolyg%node(1,2) /= resultPolyg%node(k,2))) then
              k=k+1
              resultPolyg%node(k,:) = resultPolyg%node(1,:)
            end if
          end if
          ! set the size of the resultPolyggon
          resultPolyg%NumNodes = k
        end subroutine ClipEdge
C-----------------------------------------------         



        function intersectP( Seg_P1, Seg_P2, Line_P1, Line_P2, IFLG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc" 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------     
          my_real         ,intent(inout) :: Seg_P1(2), Seg_P2(2)   
          my_real         ,intent(inout) :: Line_P1(2), Line_P2(2)  
          INTEGER         ,intent(in)    :: IFLG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------         
          my_real                     :: intersectP(2), v1(2), v2(2), Seg_P1_Line_P1(2) 
          my_real          :: alpha, tmp,tmp2, L
          my_real          :: CrossProd2D, TOL
          external CrossProd2D
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------   
          TOL = EM06
        
          intersectP(1:2) = EP30
          v1(:) = Seg_P2(:) - Seg_P1(:) 
          v2(:) = Line_P2(:) - Line_P1(:)
          L=max(sum(v1(:)*v1(:)),sum(v2(:)*v2(:)))
          tmp =  CrossProd2D(v1,v2)
          if (abs(tmp) <= TOL*L) then  
c          print *, "colinear"
!          if (tmp == 0.0D00) then
            !colinear vectors
            Seg_P1_Line_P1(:) = Line_P1(:) - Seg_P1(:)
            ! if the the segment [Seg_P1Seg_P2] is included in the line (Line_P1Line_P2)
            tmp =  CrossProd2D(Seg_P1_Line_P1,v1)
!            if ( tmp == 0.0d00) then
            if(abs(tmp) <= TOL*L)then
              ! the intersection is the last point of the segment
              intersectP(:) = Seg_P2(:)
            end if
          else 
c          print *, "not colinear"          
            ! non colinear vectors
            Seg_P1_Line_P1(:) = Line_P1(:) - Seg_P1(:) 
            ! parametric coordinates
            tmp2 = CrossProd2D(Seg_P1_Line_P1,v2)
            alpha = tmp2
            tmp2 = CrossProd2D(v1,v2)
            alpha = alpha/tmp2
            ! if a is not in [0;1]
c            print *, "alpha=", alpha
            if ( (alpha >= -EM03) .and. (alpha <= ONE+EM03)) then
              alpha=max(ZERO,min(alpha,ONE))
              intersectP(:) = Seg_P1(:) + alpha*v1(:)
            end if
          end if
          
          !post-condition
c22          IF(intersectP(1)==EP30 .AND. IFLG==1)THEN
c22            print *, "**warning : inter22 polygonal clipping."
c22            write (*,FMT='(A,3F45.35)')        " *createnode ",Seg_P1(1:2)  ,ZERO                                             
c22            write (*,FMT='(A,3F45.35)')        " *createnode ",Seg_P2(1:2)  ,ZERO                                             
c22            write (*,FMT='(A,3F45.35)')        " *createnode ",Line_P1(1:2) ,ZERO                                             
c22            write (*,FMT='(A,3F45.35)')        " *createnode ",Line_P2(1:2) ,ZERO  
c22          ENDIF
          
        end function intersectP
C----------------------------------------------- 



        function IS_ON_1ST_HALF_PLANE( point, p1, p2)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"    
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------        
          my_real         , intent(inout) :: point(2), p1(2), p2(2)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------         
          my_real                         :: v1(2), v2 (2)         
          logical :: IS_ON_1ST_HALF_PLANE
          my_real          :: crossP, L
          my_real          :: CrossProd2D
          EXTERNAL         :: CrossProd2D  
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------                   
          v1(:) = p2(:) -  p1(:)
          v2(:) = point(:)  -  p1(:)
          L=max(sum(v1(:)*v1(:)),sum(v2(:)*v2(:)))  
          crossP = CrossProd2D(v1,v2)
c          print *, "crossP",crossP, EM06*L,L
          !DONT BE TOO MUCH TOLERANT OTHERWISE ALGO WILL ADD AN EMPTY INTERSECTION POINT WHICH WOULD RECQUIRE ADDITIONAL CODE TO BE SKIPPED.
          !EM06 : issue
          !EM10 : OK
          if (  crossP >= -EM10*L) then
            IS_ON_1ST_HALF_PLANE = .true.
c            print *, "IS_ON_1ST_HALF_PLANE"
          else 
            IS_ON_1ST_HALF_PLANE = .false.
c            print *, "NOT_ON_1ST_HALF_PLANE"
          end if
        end function IS_ON_1ST_HALF_PLANE
C----------------------------------------------- 


        subroutine SetClockWisePolyg( Polyg, Area )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
          type polygon
            my_real          :: node(16,2)
            integer :: NumNodes          
          end type polygon
          type(polygon)   ,intent(inout)  :: Polyg
          my_real         , intent(inout) :: Area 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------         
          my_real                      :: x(16),y(16),total
          integer  ::  i,n
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------           

          total = ZERO
          N     = Polyg%NumNodes
          
          DO I=1,N
            x(I) = Polyg%node(I,1)            
            y(I) = Polyg%node(I,2)                        
          ENDDO
          
          DO I=1,N-1
            total = total + (x(I+1)-x(I))*(y(I+1)+y(I))
          ENDDO
          
          Area = HALF * ABS(total)
          
          !setclockwise
          IF (total < ZERO) THEN
            DO I=1,N
              Polyg%node(I,1) = x(N-I+1)           
              Polyg%node(I,2) = y(N-I+1)                       
            ENDDO 
          ENDIF   
          
      END SUBROUTINE
C----------------------------------------------- 



        subroutine SetCounterClockWisePolyg( Polyg, Area )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
          type polygon
            my_real          :: node(16,2)
            integer :: NumNodes          
          end type polygon
          type(polygon)   ,intent(inout)  :: Polyg
          my_real         , intent(inout) :: Area 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------         
          my_real                      :: x(16),y(16),total
          integer  ::  i,n
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------           

          total = ZERO
          N     = Polyg%NumNodes
          
          DO I=1,N
            x(I) = Polyg%node(I,1)            
            y(I) = Polyg%node(I,2)                        
          ENDDO
          
          DO I=1,N-1
            total = total + (x(I+1)-x(I))*(y(I+1)+y(I))
          ENDDO
          
          Area = HALF * ABS(total)
          
          !setclockwise
          IF (total > ZERO) THEN
            DO I=1,N
              Polyg%node(I,1) = x(N-I+1)           
              Polyg%node(I,2) = y(N-I+1)                       
            ENDDO 
          ENDIF   
          
      END SUBROUTINE
C----------------------------------------------- 
