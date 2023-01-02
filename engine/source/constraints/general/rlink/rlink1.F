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
Chd|  RLINK1                        source/constraints/general/rlink/rlink1.F
Chd|-- called by -----------
Chd|        RLINK10                       source/constraints/general/rlink/rlink10.F
Chd|        RLINK11                       source/constraints/general/rlink/rlink10.F
Chd|-- calls ---------------
Chd|        SUM_6_FLOAT                   source/system/parit.F         
Chd|====================================================================
      SUBROUTINE RLINK1(MS,IN ,A  ,AR    ,NSN  ,
     .                  IC,ICR,NOD,WEIGHT,FRL6 ,
     .                  IFLAG )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, IC, ICR, IFLAG
      INTEGER NOD(*),WEIGHT(*)
C     REAL
      my_real
     .   MS(*), IN(*), A(3,*), AR(3,*)
      DOUBLE PRECISION FRL6(15,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, K
C     REAL
      my_real
     .   MASS, INER, AX, AY, AZ,
     .   F1(NSN), F2(NSN), F3(NSN), F4(NSN)
C-----------------------------------------------
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
       DO I=1,NSN
        N = NOD(I)
        IF(WEIGHT(N)==1) THEN
          F1(I)=MS(N)
          F2(I)=MS(N)*A(1,N)
          F3(I)=MS(N)*A(2,N)
          F4(I)=MS(N)*A(3,N)
        ELSE
          F1(I)=ZERO
          F2(I)=ZERO
          F3(I)=ZERO
          F4(I)=ZERO
        ENDIF
       ENDDO
C
C Traitement Parith/ON avant echange
C
       CALL SUM_6_FLOAT(1  ,NSN  ,F1, FRL6(1,1), 15)
       CALL SUM_6_FLOAT(1  ,NSN  ,F2, FRL6(2,1), 15)
       CALL SUM_6_FLOAT(1  ,NSN  ,F3, FRL6(3,1), 15)
       CALL SUM_6_FLOAT(1  ,NSN  ,F4, FRL6(4,1), 15)

  150  IF(ICR==0.OR.IRODDL==0)RETURN
C
       DO I=1,NSN
        N = NOD(I)
        IF(WEIGHT(N)==1) THEN
          F1(I)=IN(N)
          F2(I)=IN(N)*AR(1,N)
          F3(I)=IN(N)*AR(2,N)
          F4(I)=IN(N)*AR(3,N)
        ELSE
          F1(I)=ZERO
          F2(I)=ZERO
          F3(I)=ZERO
          F4(I)=ZERO
        ENDIF
       ENDDO
C
C Traitement Parith/ON avant echange
C
       CALL SUM_6_FLOAT(1  ,NSN  ,F1, FRL6(5,1), 15)
       CALL SUM_6_FLOAT(1  ,NSN  ,F2, FRL6(6,1), 15)
       CALL SUM_6_FLOAT(1  ,NSN  ,F3, FRL6(7,1), 15)
       CALL SUM_6_FLOAT(1  ,NSN  ,F4, FRL6(8,1), 15)

C
      ELSEIF(IFLAG == 2)THEN
C
       IF(IC==0)GOTO 250
C
       MASS = FRL6(1,1)+FRL6(1,2)+FRL6(1,3)+
     +        FRL6(1,4)+FRL6(1,5)+FRL6(1,6)
       AX = FRL6(2,1)+FRL6(2,2)+FRL6(2,3)+
     +      FRL6(2,4)+FRL6(2,5)+FRL6(2,6)
       AY = FRL6(3,1)+FRL6(3,2)+FRL6(3,3)+
     +      FRL6(3,4)+FRL6(3,5)+FRL6(3,6)
       AZ = FRL6(4,1)+FRL6(4,2)+FRL6(4,3)+
     +      FRL6(4,4)+FRL6(4,5)+FRL6(4,6)
C
       IF(MASS==ZERO)GOTO 250
       AX=AX/MASS
       AY=AY/MASS
       AZ=AZ/MASS
C
       IF(IC==1.OR.IC==3.OR.IC==5.OR.IC==7)THEN
        DO 110 I=1,NSN
         N = NOD(I)
         A(3,N) =AZ
  110   CONTINUE
       ENDIF
       IF(IC==2.OR.IC==3.OR.IC==6.OR.IC==7)THEN
        DO 120 I=1,NSN
         N = NOD(I)
         A(2,N) =AY
  120   CONTINUE
       ENDIF
       IF(IC==4.OR.IC==5.OR.IC==6.OR.IC==7)THEN
        DO 130 I=1,NSN
         N = NOD(I)
         A(1,N) =AX
  130   CONTINUE
       ENDIF
C
  250  IF(ICR==0.OR.IRODDL==0)RETURN
C
       INER = FRL6(5,1)+FRL6(5,2)+FRL6(5,3)+
     +        FRL6(5,4)+FRL6(5,5)+FRL6(5,6)
       AX = FRL6(6,1)+FRL6(6,2)+FRL6(6,3)+
     +      FRL6(6,4)+FRL6(6,5)+FRL6(6,6)
       AY = FRL6(7,1)+FRL6(7,2)+FRL6(7,3)+
     +      FRL6(7,4)+FRL6(7,5)+FRL6(7,6)
       AZ = FRL6(8,1)+FRL6(8,2)+FRL6(8,3)+
     +      FRL6(8,4)+FRL6(8,5)+FRL6(8,6)
C
       IF(INER==ZERO)RETURN
C
       AX=AX/INER
       AY=AY/INER
       AZ=AZ/INER
C
       IF(ICR==1.OR.ICR==3.OR.ICR==5.OR.ICR==7)THEN
        DO 210 I=1,NSN
         N = NOD(I)
         AR(3,N) =AZ
  210   CONTINUE
       ENDIF
       IF(ICR==2.OR.ICR==3.OR.ICR==6.OR.ICR==7)THEN
        DO 220 I=1,NSN
         N = NOD(I)
         AR(2,N) =AY
  220   CONTINUE
       ENDIF
       IF(ICR==4.OR.ICR==5.OR.ICR==6.OR.ICR==7)THEN
        DO 230 I=1,NSN
         N = NOD(I)
         AR(1,N) =AX
  230   CONTINUE
       ENDIF
      END IF
C
      RETURN
      END
