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
Chd|  RLINK2                        source/constraints/general/rlink/rlink2.F
Chd|-- called by -----------
Chd|        RLINK10                       source/constraints/general/rlink/rlink10.F
Chd|        RLINK11                       source/constraints/general/rlink/rlink10.F
Chd|-- calls ---------------
Chd|        SUM_6_FLOAT                   source/system/parit.F         
Chd|====================================================================
      SUBROUTINE RLINK2(MS,IN,A,AR,V,
     2                  VR,NSN,IC,ICR,NOD,
     3                  SKEW,WEIGHT,FRL6,IFLAG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, IC, ICR, IFLAG
      INTEGER NOD(*),WEIGHT(*)
      my_real
     .   MS(*), IN(*), A(3,*), AR(3,*), V(3,*), VR(3,*), SKEW(*)
      DOUBLE PRECISION FRL6(15,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IC1, ICC, IC2, IC3, I, N, K
      my_real
     .   MASS, INER, AX, AY, AZ, VX, VY, VZ, DAX, DAY, DAZ, AAX, AAY,
     .   AAZ, DVX, DVY, DVZ, VVX, VVY, VVZ,
     .   F1(NSN), F2(NSN), F3(NSN), F4(NSN), F5(NSN), F6(NSN), F7(NSN)
C-----------------------------------------------

      INER = ZERO

      IF(IFLAG == 1)THEN

C Init Parith/ON
       DO K = 1, 6
         FRL6(1,K) = ZERO
         FRL6(2,K) = ZERO
         FRL6(3,K) = ZERO
         FRL6(4,K) = ZERO
         FRL6(5,K) = ZERO
         FRL6(6,K) = ZERO
         FRL6(7,K) = ZERO
         FRL6(8,K) = ZERO
         FRL6(9,K)  = ZERO
         FRL6(10,K) = ZERO
         FRL6(11,K) = ZERO
         FRL6(12,K) = ZERO
         FRL6(13,K) = ZERO
         FRL6(14,K) = ZERO
         FRL6(15,K) = ZERO
       END DO

       IF(IC==0)GOTO 150
C
c      AX  =ZERO
c      AY  =ZERO
c      AZ  =ZERO
c      VX  =ZERO
c      VY  =ZERO
c      VZ  =ZERO
c      MASS=ZERO   
C
       DO I=1,NSN
        N = NOD(I)
        IF(WEIGHT(N)==1) THEN
          F1(I)=MS(N)
          F2(I)=MS(N)*A(1,N)
          F3(I)=MS(N)*A(2,N)
          F4(I)=MS(N)*A(3,N)
          F5(I)=MS(N)*V(1,N)
          F6(I)=MS(N)*V(2,N)
          F7(I)=MS(N)*V(3,N)
        ELSE
          F1(I)=ZERO
          F2(I)=ZERO
          F3(I)=ZERO
          F4(I)=ZERO
          F5(I)=ZERO
          F6(I)=ZERO
          F7(I)=ZERO
        ENDIF
       ENDDO
C
C Traitement Parith/ON avant echange
C
       CALL SUM_6_FLOAT(1  ,NSN  ,F1, FRL6(1,1), 15)
       CALL SUM_6_FLOAT(1  ,NSN  ,F2, FRL6(2,1), 15)
       CALL SUM_6_FLOAT(1  ,NSN  ,F3, FRL6(3,1), 15)
       CALL SUM_6_FLOAT(1  ,NSN  ,F4, FRL6(4,1), 15)
       CALL SUM_6_FLOAT(1  ,NSN  ,F5, FRL6(5,1), 15)
       CALL SUM_6_FLOAT(1  ,NSN  ,F6, FRL6(6,1), 15)
       CALL SUM_6_FLOAT(1  ,NSN  ,F7, FRL6(7,1), 15)
C
C
  150 IF(ICR==0)RETURN
C
c      AX  =ZERO
c      AY  =ZERO
c      AZ  =ZERO
c      VX  =ZERO
c      VY  =ZERO
c      VZ  =ZERO
c      INER=ZERO   
C
      DO I=1,NSN
        N = NOD(I)
        IF(WEIGHT(N)==1) THEN
          F1(I)=IN(N)
          F2(I)=IN(N)*AR(1,N)
          F3(I)=IN(N)*AR(2,N)
          F4(I)=IN(N)*AR(3,N)
          F5(I)=IN(N)*VR(1,N)
          F6(I)=IN(N)*VR(2,N)
          F7(I)=IN(N)*VR(3,N)
        ELSE
          F1(I)=ZERO
          F2(I)=ZERO
          F3(I)=ZERO
          F4(I)=ZERO
          F5(I)=ZERO
          F6(I)=ZERO
          F7(I)=ZERO
        ENDIF
      ENDDO
C
C Traitement Parith/ON avant echange
C
       CALL SUM_6_FLOAT(1  ,NSN  ,F1, FRL6(8,1), 15)
       CALL SUM_6_FLOAT(1  ,NSN  ,F2, FRL6(9,1), 15)
       CALL SUM_6_FLOAT(1  ,NSN  ,F3, FRL6(10,1), 15)
       CALL SUM_6_FLOAT(1  ,NSN  ,F4, FRL6(11,1), 15)
       CALL SUM_6_FLOAT(1  ,NSN  ,F5, FRL6(12,1), 15)
       CALL SUM_6_FLOAT(1  ,NSN  ,F6, FRL6(13,1), 15)
       CALL SUM_6_FLOAT(1  ,NSN  ,F7, FRL6(14,1), 15)
C
      ELSEIF(IFLAG == 2)THEN
C
      IF(IC==0)GOTO 250
       IC1=IC/4
       ICC=IC-4*IC1
       IC2=ICC/2
       IC3=ICC-2*IC2
       MASS = FRL6(1,1)+FRL6(1,2)+FRL6(1,3)+
     +        FRL6(1,4)+FRL6(1,5)+FRL6(1,6)
       AX = FRL6(2,1)+FRL6(2,2)+FRL6(2,3)+
     +      FRL6(2,4)+FRL6(2,5)+FRL6(2,6)
       AY = FRL6(3,1)+FRL6(3,2)+FRL6(3,3)+
     +      FRL6(3,4)+FRL6(3,5)+FRL6(3,6)
       AZ = FRL6(4,1)+FRL6(4,2)+FRL6(4,3)+
     +      FRL6(4,4)+FRL6(4,5)+FRL6(4,6)
       VX = FRL6(5,1)+FRL6(5,2)+FRL6(5,3)+
     +      FRL6(5,4)+FRL6(5,5)+FRL6(5,6)
       VY = FRL6(6,1)+FRL6(6,2)+FRL6(6,3)+
     +      FRL6(6,4)+FRL6(6,5)+FRL6(6,6)
       VZ = FRL6(7,1)+FRL6(7,2)+FRL6(7,3)+
     +      FRL6(7,4)+FRL6(7,5)+FRL6(7,6)
C
       MASS=MAX(EM20,MASS)
       AX= AX/MASS
       AY= AY/MASS
       AZ= AZ/MASS
       VX= VX/MASS
       VY= VY/MASS
       VZ= VZ/MASS
C
       DO I=1,NSN
        N = NOD(I)
        DAX  =A(1,N)-AX
        DAY  =A(2,N)-AY
        DAZ  =A(3,N)-AZ
        AAX  =IC1*(SKEW(1)*DAX+SKEW(2)*DAY+SKEW(3)*DAZ)
        AAY  =IC2*(SKEW(4)*DAX+SKEW(5)*DAY+SKEW(6)*DAZ)
        AAZ  =IC3*(SKEW(7)*DAX+SKEW(8)*DAY+SKEW(9)*DAZ)
        A(1,N) =A(1,N)-AAX*SKEW(1)-AAY*SKEW(4)-AAZ*SKEW(7)
        A(2,N) =A(2,N)-AAX*SKEW(2)-AAY*SKEW(5)-AAZ*SKEW(8)
        A(3,N) =A(3,N)-AAX*SKEW(3)-AAY*SKEW(6)-AAZ*SKEW(9)
C
        DVX  =V(1,N)-VX
        DVY  =V(2,N)-VY
        DVZ  =V(3,N)-VZ
        VVX  =IC1*(SKEW(1)*DVX+SKEW(2)*DVY+SKEW(3)*DVZ)
        VVY  =IC2*(SKEW(4)*DVX+SKEW(5)*DVY+SKEW(6)*DVZ)
        VVZ  =IC3*(SKEW(7)*DVX+SKEW(8)*DVY+SKEW(9)*DVZ)
        V(1,N) =V(1,N)-VVX*SKEW(1)-VVY*SKEW(4)-VVZ*SKEW(7)
        V(2,N) =V(2,N)-VVX*SKEW(2)-VVY*SKEW(5)-VVZ*SKEW(8)
        V(3,N) =V(3,N)-VVX*SKEW(3)-VVY*SKEW(6)-VVZ*SKEW(9)
C
       END DO

  250  IF(ICR==0)RETURN
       IC1=ICR/4
       ICC=ICR-4*IC1
       IC2=ICC/2
       IC3=ICC-2*IC2

       IF(INER==ZERO)RETURN
C
       INER = FRL6(8,1)+FRL6(8,2)+FRL6(8,3)+
     +        FRL6(8,4)+FRL6(8,5)+FRL6(8,6)
       AX = FRL6(9,1)+FRL6(9,2)+FRL6(9,3)+
     +      FRL6(9,4)+FRL6(9,5)+FRL6(9,6)
       AY = FRL6(10,1)+FRL6(10,2)+FRL6(10,3)+
     +      FRL6(10,4)+FRL6(10,5)+FRL6(10,6)
       AZ = FRL6(11,1)+FRL6(11,2)+FRL6(11,3)+
     +      FRL6(11,4)+FRL6(11,5)+FRL6(11,6)
       VX = FRL6(12,1)+FRL6(12,2)+FRL6(12,3)+
     +      FRL6(12,4)+FRL6(12,5)+FRL6(12,6)
       VY = FRL6(13,1)+FRL6(13,2)+FRL6(13,3)+
     +      FRL6(13,4)+FRL6(13,5)+FRL6(13,6)
       VZ = FRL6(14,1)+FRL6(14,2)+FRL6(14,3)+
     +      FRL6(14,4)+FRL6(14,5)+FRL6(14,6)
       AX=AX/INER
       AY=AY/INER
       AZ=AZ/INER
       VX=VX/INER
       VY=VY/INER
       VZ=VZ/INER
C
       DO I=1,NSN
        N = NOD(I)
        DAX  =AR(1,N)-AX
        DAY  =AR(2,N)-AY
        DAZ  =AR(3,N)-AZ
        AAX  =IC1*(SKEW(1)*DAX+SKEW(2)*DAY+SKEW(3)*DAZ)
        AAY  =IC2*(SKEW(4)*DAX+SKEW(5)*DAY+SKEW(6)*DAZ)
        AAZ  =IC3*(SKEW(7)*DAX+SKEW(8)*DAY+SKEW(9)*DAZ)
        AR(1,N) =AR(1,N)-AAX*SKEW(1)-AAY*SKEW(4)-AAZ*SKEW(7)
        AR(2,N) =AR(2,N)-AAX*SKEW(2)-AAY*SKEW(5)-AAZ*SKEW(8)
        AR(3,N) =AR(3,N)-AAX*SKEW(3)-AAY*SKEW(6)-AAZ*SKEW(9)
        DVX  =VR(1,N)-VX
        DVY  =VR(2,N)-VY
        DVZ  =VR(3,N)-VZ
        VVX  =IC1*(SKEW(1)*DVX+SKEW(2)*DVY+SKEW(3)*DVZ)
        VVY  =IC2*(SKEW(4)*DVX+SKEW(5)*DVY+SKEW(6)*DVZ)
        VVZ  =IC3*(SKEW(7)*DVX+SKEW(8)*DVY+SKEW(9)*DVZ)
        VR(1,N) =VR(1,N)-VVX*SKEW(1)-VVY*SKEW(4)-VVZ*SKEW(7)
        VR(2,N) =VR(2,N)-VVX*SKEW(2)-VVY*SKEW(5)-VVZ*SKEW(8)
        VR(3,N) =VR(3,N)-VVX*SKEW(3)-VVY*SKEW(6)-VVZ*SKEW(9)
       ENDDO
      END IF
C
      RETURN
      END
