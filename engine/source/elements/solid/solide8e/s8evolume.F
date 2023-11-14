Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  S8EVOLUME                     source/elements/solid/solide8e/s8evolume.F
Chd|-- called by -----------
Chd|        ELOFF                         source/elements/eloff.F       
Chd|        FIXFLUX                       source/constraints/thermic/fixflux.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EVOLUME(X, VOLG, VOLP, NELA, NPTR, NPTS, NPTT,
     .                     NC1, NC2, NC3, NC4, NC5, NC6, NC7, NC8 )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NELA, NPTR, NPTS, NPTT
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*), NC5(*), NC6(*), NC7(*), NC8(*)
      my_real X(3,*), VOLG(*), VOLP(MVSIZ,*)                      
C
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
      INTEGER IR,IS,IT,IP
C
      my_real
     .  X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ), X5(MVSIZ), X6(MVSIZ), X7(MVSIZ), X8(MVSIZ), 
     .  Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ), Y5(MVSIZ), Y6(MVSIZ), Y7(MVSIZ), Y8(MVSIZ), 
     .  Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ), Z5(MVSIZ), Z6(MVSIZ), Z7(MVSIZ), Z8(MVSIZ)
      my_real
     .   AJ1(MVSIZ),AJ2(MVSIZ),AJ3(MVSIZ),
     .   AJ4(MVSIZ),AJ5(MVSIZ),AJ6(MVSIZ),
     .   AJ7(MVSIZ),AJ8(MVSIZ),AJ9(MVSIZ)
            my_real
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ)
      my_real
     .   X17(MVSIZ) , X28(MVSIZ) , X35(MVSIZ) , X46(MVSIZ),
     .   Y17(MVSIZ) , Y28(MVSIZ) , Y35(MVSIZ) , Y46(MVSIZ),
     .   Z17(MVSIZ) , Z28(MVSIZ) , Z35(MVSIZ) , Z46(MVSIZ),
     .   A17(MVSIZ) , A28(MVSIZ) ,
     .   B17(MVSIZ) , B28(MVSIZ) ,
     .   C17(MVSIZ) , C28(MVSIZ)
      my_real
     .   HX(MVSIZ,4),  HY(MVSIZ,4), HZ(MVSIZ,4),
     .   HX1PG(MVSIZ), HX2PG(MVSIZ), HX3PG(MVSIZ),HX4PG2(MVSIZ),
     .   HY1PG(MVSIZ), HY2PG(MVSIZ), HY3PG(MVSIZ),HY4PG2(MVSIZ),
     .   HZ1PG(MVSIZ), HZ2PG(MVSIZ), HZ3PG(MVSIZ),HZ4PG2(MVSIZ),
     .   AJP1(MVSIZ,8),AJP2(MVSIZ,8),AJP3(MVSIZ,8),
     .   AJP4(MVSIZ,8),AJP5(MVSIZ,8),AJP6(MVSIZ,8),
     .   AJP7(MVSIZ,8),AJP8(MVSIZ,8),AJP9(MVSIZ,8)
            my_real
     .   PG2, PG, WI
      my_real
     .  W_GAUSS(9,9)
      DATA W_GAUSS / 
     1 2.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 1.               ,1.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 0.555555555555556,0.888888888888889,0.555555555555556,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 0.347854845137454,0.652145154862546,0.652145154862546,
     4 0.347854845137454,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 0.236926885056189,0.478628670499366,0.568888888888889,
     5 0.478628670499366,0.236926885056189,0.               ,
     5 0.               ,0.               ,0.               ,
     6 0.171324492379170,0.360761573048139,0.467913934572691,
     6 0.467913934572691,0.360761573048139,0.171324492379170,
     6 0.               ,0.               ,0.               ,
     7 0.129484966168870,0.279705391489277,0.381830050505119,
     7 0.417959183673469,0.381830050505119,0.279705391489277,
     7 0.129484966168870,0.               ,0.               ,
     8 0.101228536290376,0.222381034453374,0.313706645877887,
     8 0.362683783378362,0.362683783378362,0.313706645877887,
     8 0.222381034453374,0.101228536290376,0.               ,
     9 0.081274388361574,0.180648160694857,0.260610696402935,
     9 0.312347077040003,0.330239355001260,0.312347077040003,
     9 0.260610696402935,0.180648160694857,0.081274388361574/

       PARAMETER (PG=.577350269189626)

C---+----1
      DO I=1,NELA
        X1(I)=X(1,NC1(I))  
        Y1(I)=X(2,NC1(I))  
        Z1(I)=X(3,NC1(I))  
        X2(I)=X(1,NC2(I))  
        Y2(I)=X(2,NC2(I))  
        Z2(I)=X(3,NC2(I))  
        X3(I)=X(1,NC3(I))  
        Y3(I)=X(2,NC3(I))  
        Z3(I)=X(3,NC3(I))  
        X4(I)=X(1,NC4(I))  
        Y4(I)=X(2,NC4(I))  
        Z4(I)=X(3,NC4(I))  
        X5(I)=X(1,NC5(I))  
        Y5(I)=X(2,NC5(I))  
        Z5(I)=X(3,NC5(I))  
        X6(I)=X(1,NC6(I))  
        Y6(I)=X(2,NC6(I))  
        Z6(I)=X(3,NC6(I))  
        X7(I)=X(1,NC7(I))  
        Y7(I)=X(2,NC7(I))  
        Z7(I)=X(3,NC7(I))  
        X8(I)=X(1,NC8(I))  
        Y8(I)=X(2,NC8(I))  
        Z8(I)=X(3,NC8(I))  
      ENDDO      
C---+----1
      DO I=1,NELA
         X17(I)=X7(I)-X1(I)
         X28(I)=X8(I)-X2(I)
         X35(I)=X5(I)-X3(I)
         X46(I)=X6(I)-X4(I)
C
         Y17(I)=Y7(I)-Y1(I)
         Y28(I)=Y8(I)-Y2(I)
         Y35(I)=Y5(I)-Y3(I)
         Y46(I)=Y6(I)-Y4(I)
C
         Z17(I)=Z7(I)-Z1(I)
         Z28(I)=Z8(I)-Z2(I)
         Z35(I)=Z5(I)-Z3(I)
         Z46(I)=Z6(I)-Z4(I)
C
         AJ4(I)=X17(I)+X28(I)-X35(I)-X46(I)
         AJ5(I)=Y17(I)+Y28(I)-Y35(I)-Y46(I)
         AJ6(I)=Z17(I)+Z28(I)-Z35(I)-Z46(I)

         A17(I)=X17(I)+X46(I)
         A28(I)=X28(I)+X35(I)
         B17(I)=Y17(I)+Y46(I)
         B28(I)=Y28(I)+Y35(I)
         C17(I)=Z17(I)+Z46(I)
         C28(I)=Z28(I)+Z35(I)

         AJ7(I)=A17(I)+A28(I)
         AJ8(I)=B17(I)+B28(I)
         AJ9(I)=C17(I)+C28(I)
         AJ1(I)=A17(I)-A28(I)
         AJ2(I)=B17(I)-B28(I)
         AJ3(I)=C17(I)-C28(I)

         JAC_59_68(I)=AJ5(I)*AJ9(I)-AJ6(I)*AJ8(I)
         JAC_67_49(I)=AJ6(I)*AJ7(I)-AJ4(I)*AJ9(I)
         JAC_48_57(I)=AJ4(I)*AJ8(I)-AJ5(I)*AJ7(I)
C
         VOLG(I)=ONE_OVER_64*(AJ1(I)*JAC_59_68(I)+AJ2(I)*JAC_67_49(I)+AJ3(I)*JAC_48_57(I))
      ENDDO

C mode 1
C 1 1 -1 -1 -1 -1 1 1
      DO I=1,NELA
         HX(I,1)=(X1(I)+X2(I)-X3(I)-X4(I)-X5(I)-X6(I)+X7(I)+X8(I))
         HY(I,1)=(Y1(I)+Y2(I)-Y3(I)-Y4(I)-Y5(I)-Y6(I)+Y7(I)+Y8(I))
         HZ(I,1)=(Z1(I)+Z2(I)-Z3(I)-Z4(I)-Z5(I)-Z6(I)+Z7(I)+Z8(I))
      ENDDO
C mode 2
C 1 -1 -1 1 -1 1 1 -1
      DO I=1,NELA
         HX(I,2)=(X1(I)-X2(I)-X3(I)+X4(I)-X5(I)+X6(I)+X7(I)-X8(I))
         HY(I,2)=(Y1(I)-Y2(I)-Y3(I)+Y4(I)-Y5(I)+Y6(I)+Y7(I)-Y8(I))
         HZ(I,2)=(Z1(I)-Z2(I)-Z3(I)+Z4(I)-Z5(I)+Z6(I)+Z7(I)-Z8(I))
      ENDDO
C mode 3
C 1 -1 1 -1 1 -1 1 -1
      DO I=1,NELA
         HX(I,3)=(X1(I)-X2(I)+X3(I)-X4(I)+X5(I)-X6(I)+X7(I)-X8(I))
         HY(I,3)=(Y1(I)-Y2(I)+Y3(I)-Y4(I)+Y5(I)-Y6(I)+Y7(I)-Y8(I))
         HZ(I,3)=(Z1(I)-Z2(I)+Z3(I)-Z4(I)+Z5(I)-Z6(I)+Z7(I)-Z8(I))
      ENDDO
C mode 4
C -1 1 -1 1 1 -1 1 -1
      DO I=1,NELA
         HX(I,4)=(-X1(I)+X2(I)-X3(I)+X4(I)+X5(I)-X6(I)+X7(I)-X8(I))
         HY(I,4)=(-Y1(I)+Y2(I)-Y3(I)+Y4(I)+Y5(I)-Y6(I)+Y7(I)-Y8(I))
         HZ(I,4)=(-Z1(I)+Z2(I)-Z3(I)+Z4(I)+Z5(I)-Z6(I)+Z7(I)-Z8(I))
      ENDDO

      PG2=PG*PG
C
      DO I=1,NELA
       HX1PG(I) =HX(I,1)*PG
       HX2PG(I) =HX(I,2)*PG
       HX3PG(I) =HX(I,3)*PG
       HX4PG2(I)=HX(I,4)*PG2
       HY1PG(I) =HY(I,1)*PG
       HY2PG(I) =HY(I,2)*PG
       HY3PG(I) =HY(I,3)*PG
       HY4PG2(I)=HY(I,4)*PG2
       HZ1PG(I) =HZ(I,1)*PG
       HZ2PG(I) =HZ(I,2)*PG
       HZ3PG(I) =HZ(I,3)*PG
       HZ4PG2(I)=HZ(I,4)*PG2
      ENDDO
      IP=1
C - - -    
      DO I=1,NELA
       AJP1(I,IP)=AJ1(I)-HX3PG(I)-HX2PG(I)+HX4PG2(I)
       AJP2(I,IP)=AJ2(I)-HY3PG(I)-HY2PG(I)+HY4PG2(I)
       AJP3(I,IP)=AJ3(I)-HZ3PG(I)-HZ2PG(I)+HZ4PG2(I)
C
       AJP4(I,IP)=AJ4(I)-HX1PG(I)-HX3PG(I)+HX4PG2(I)
       AJP5(I,IP)=AJ5(I)-HY1PG(I)-HY3PG(I)+HY4PG2(I)
       AJP6(I,IP)=AJ6(I)-HZ1PG(I)-HZ3PG(I)+HZ4PG2(I)
C
       AJP7(I,IP)=AJ7(I)-HX2PG(I)-HX1PG(I)+HX4PG2(I)
       AJP8(I,IP)=AJ8(I)-HY2PG(I)-HY1PG(I)+HY4PG2(I)
       AJP9(I,IP)=AJ9(I)-HZ2PG(I)-HZ1PG(I)+HZ4PG2(I)
      ENDDO
      IP=2    
C + - -    
      DO I=1,NELA
       AJP1(I,IP)=AJ1(I)-HX3PG(I)-HX2PG(I)+HX4PG2(I)
       AJP2(I,IP)=AJ2(I)-HY3PG(I)-HY2PG(I)+HY4PG2(I)
       AJP3(I,IP)=AJ3(I)-HZ3PG(I)-HZ2PG(I)+HZ4PG2(I)
C
       AJP4(I,IP)=AJ4(I)-HX1PG(I)+HX3PG(I)-HX4PG2(I)
       AJP5(I,IP)=AJ5(I)-HY1PG(I)+HY3PG(I)-HY4PG2(I)
       AJP6(I,IP)=AJ6(I)-HZ1PG(I)+HZ3PG(I)-HZ4PG2(I)
C
       AJP7(I,IP)=AJ7(I)+HX2PG(I)-HX1PG(I)-HX4PG2(I)
       AJP8(I,IP)=AJ8(I)+HY2PG(I)-HY1PG(I)-HY4PG2(I)
       AJP9(I,IP)=AJ9(I)+HZ2PG(I)-HZ1PG(I)-HZ4PG2(I)
      ENDDO
      IP=3    
C - + -    
      DO I=1,NELA
       AJP1(I,IP)=AJ1(I)+HX3PG(I)-HX2PG(I)-HX4PG2(I)
       AJP2(I,IP)=AJ2(I)+HY3PG(I)-HY2PG(I)-HY4PG2(I)
       AJP3(I,IP)=AJ3(I)+HZ3PG(I)-HZ2PG(I)-HZ4PG2(I)
C
       AJP4(I,IP)=AJ4(I)-HX1PG(I)-HX3PG(I)+HX4PG2(I)
       AJP5(I,IP)=AJ5(I)-HY1PG(I)-HY3PG(I)+HY4PG2(I)
       AJP6(I,IP)=AJ6(I)-HZ1PG(I)-HZ3PG(I)+HZ4PG2(I)
C
       AJP7(I,IP)=AJ7(I)-HX2PG(I)+HX1PG(I)-HX4PG2(I)
       AJP8(I,IP)=AJ8(I)-HY2PG(I)+HY1PG(I)-HY4PG2(I)
       AJP9(I,IP)=AJ9(I)-HZ2PG(I)+HZ1PG(I)-HZ4PG2(I)
      ENDDO
      IP=4    
C + + -    
      DO I=1,NELA
       AJP1(I,IP)=AJ1(I)+HX3PG(I)-HX2PG(I)-HX4PG2(I)
       AJP2(I,IP)=AJ2(I)+HY3PG(I)-HY2PG(I)-HY4PG2(I)
       AJP3(I,IP)=AJ3(I)+HZ3PG(I)-HZ2PG(I)-HZ4PG2(I)
C
       AJP4(I,IP)=AJ4(I)-HX1PG(I)+HX3PG(I)-HX4PG2(I)
       AJP5(I,IP)=AJ5(I)-HY1PG(I)+HY3PG(I)-HY4PG2(I)
       AJP6(I,IP)=AJ6(I)-HZ1PG(I)+HZ3PG(I)-HZ4PG2(I)
C
       AJP7(I,IP)=AJ7(I)+HX2PG(I)+HX1PG(I)+HX4PG2(I)
       AJP8(I,IP)=AJ8(I)+HY2PG(I)+HY1PG(I)+HY4PG2(I)
       AJP9(I,IP)=AJ9(I)+HZ2PG(I)+HZ1PG(I)+HZ4PG2(I)
      ENDDO
      IP=5    
C - - +    
      DO I=1,NELA
       AJP1(I,IP)=AJ1(I)-HX3PG(I)+HX2PG(I)-HX4PG2(I)
       AJP2(I,IP)=AJ2(I)-HY3PG(I)+HY2PG(I)-HY4PG2(I)
       AJP3(I,IP)=AJ3(I)-HZ3PG(I)+HZ2PG(I)-HZ4PG2(I)
C
       AJP4(I,IP)=AJ4(I)+HX1PG(I)-HX3PG(I)-HX4PG2(I)
       AJP5(I,IP)=AJ5(I)+HY1PG(I)-HY3PG(I)-HY4PG2(I)
       AJP6(I,IP)=AJ6(I)+HZ1PG(I)-HZ3PG(I)-HZ4PG2(I)
C
       AJP7(I,IP)=AJ7(I)-HX2PG(I)-HX1PG(I)+HX4PG2(I)
       AJP8(I,IP)=AJ8(I)-HY2PG(I)-HY1PG(I)+HY4PG2(I)
       AJP9(I,IP)=AJ9(I)-HZ2PG(I)-HZ1PG(I)+HZ4PG2(I)
      ENDDO
      IP=6    
C + - +    
      DO I=1,NELA
       AJP1(I,IP)=AJ1(I)-HX3PG(I)+HX2PG(I)-HX4PG2(I)
       AJP2(I,IP)=AJ2(I)-HY3PG(I)+HY2PG(I)-HY4PG2(I)
       AJP3(I,IP)=AJ3(I)-HZ3PG(I)+HZ2PG(I)-HZ4PG2(I)
C
       AJP4(I,IP)=AJ4(I)+HX1PG(I)+HX3PG(I)+HX4PG2(I)
       AJP5(I,IP)=AJ5(I)+HY1PG(I)+HY3PG(I)+HY4PG2(I)
       AJP6(I,IP)=AJ6(I)+HZ1PG(I)+HZ3PG(I)+HZ4PG2(I)
C
       AJP7(I,IP)=AJ7(I)+HX2PG(I)-HX1PG(I)-HX4PG2(I)
       AJP8(I,IP)=AJ8(I)+HY2PG(I)-HY1PG(I)-HY4PG2(I)
       AJP9(I,IP)=AJ9(I)+HZ2PG(I)-HZ1PG(I)-HZ4PG2(I)
      ENDDO
      IP=7    
C - + +    
      DO I=1,NELA
       AJP1(I,IP)=AJ1(I)+HX3PG(I)+HX2PG(I)+HX4PG2(I)
       AJP2(I,IP)=AJ2(I)+HY3PG(I)+HY2PG(I)+HY4PG2(I)
       AJP3(I,IP)=AJ3(I)+HZ3PG(I)+HZ2PG(I)+HZ4PG2(I)
C
       AJP4(I,IP)=AJ4(I)+HX1PG(I)-HX3PG(I)-HX4PG2(I)
       AJP5(I,IP)=AJ5(I)+HY1PG(I)-HY3PG(I)-HY4PG2(I)
       AJP6(I,IP)=AJ6(I)+HZ1PG(I)-HZ3PG(I)-HZ4PG2(I)
C
       AJP7(I,IP)=AJ7(I)-HX2PG(I)+HX1PG(I)-HX4PG2(I)
       AJP8(I,IP)=AJ8(I)-HY2PG(I)+HY1PG(I)-HY4PG2(I)
       AJP9(I,IP)=AJ9(I)-HZ2PG(I)+HZ1PG(I)-HZ4PG2(I)
      ENDDO
      IP=8    
C + + +    
      DO I=1,NELA
       AJP1(I,IP)=AJ1(I)+HX3PG(I)+HX2PG(I)+HX4PG2(I)
       AJP2(I,IP)=AJ2(I)+HY3PG(I)+HY2PG(I)+HY4PG2(I)
       AJP3(I,IP)=AJ3(I)+HZ3PG(I)+HZ2PG(I)+HZ4PG2(I)
C
       AJP4(I,IP)=AJ4(I)+HX1PG(I)+HX3PG(I)+HX4PG2(I)
       AJP5(I,IP)=AJ5(I)+HY1PG(I)+HY3PG(I)+HY4PG2(I)
       AJP6(I,IP)=AJ6(I)+HZ1PG(I)+HZ3PG(I)+HZ4PG2(I)
C
       AJP7(I,IP)=AJ7(I)+HX2PG(I)+HX1PG(I)+HX4PG2(I)
       AJP8(I,IP)=AJ8(I)+HY2PG(I)+HY1PG(I)+HY4PG2(I)
       AJP9(I,IP)=AJ9(I)+HZ2PG(I)+HZ1PG(I)+HZ4PG2(I)
      ENDDO
C
C---+----1
      DO IR=1,NPTR
       DO IS=1,NPTS
        DO IT=1,NPTT
           IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
           WI = W_GAUSS(IR,NPTR)*W_GAUSS(IS,NPTS)*W_GAUSS(IT,NPTT)
C
           DO I=1,NELA
              JAC_59_68(I)=AJP5(I,IP)*AJP9(I,IP)-AJP6(I,IP)*AJP8(I,IP)
              JAC_67_49(I)=AJP6(I,IP)*AJP7(I,IP)-AJP4(I,IP)*AJP9(I,IP)
              JAC_48_57(I)=AJP4(I,IP)*AJP8(I,IP)-AJP5(I,IP)*AJP7(I,IP)
           ENDDO
C
           DO I=1,NELA
              VOLP(I,IP)=ONE_OVER_512*WI*(AJP1(I,IP)*JAC_59_68(I)+AJP2(I,IP)*JAC_67_49(I)+AJP3(I,IP)*JAC_48_57(I))
           ENDDO
        ENDDO  ! IT=1,NPTT
       ENDDO   ! IS=1,NPTS
      ENDDO    ! IR=1,NPTR
C
      RETURN
      END

