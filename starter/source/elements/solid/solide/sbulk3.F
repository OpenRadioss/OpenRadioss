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
Chd|  SBULK3                        source/elements/solid/solide/sbulk3.F
Chd|-- called by -----------
Chd|        INIVOID                       source/elements/initia/inivoid.F
Chd|        S10INIT3                      source/elements/solid/solide10/s10init3.F
Chd|        S16INIT3                      source/elements/thickshell/solide16/s16init3.F
Chd|        S20INIT3                      source/elements/solid/solide20/s20init3.F
Chd|        S4INIT3                       source/elements/solid/solide4/s4init3.F
Chd|        S6CINIT3                      source/elements/thickshell/solide6c/s6cinit3.F
Chd|        S8CINIT3                      source/elements/thickshell/solide8c/s8cinit3.F
Chd|        S8ZINIT3                      source/elements/solid/solide8z/s8zinit3.F
Chd|        SCINIT3                       source/elements/thickshell/solidec/scinit3.F
Chd|        SINIT3                        source/elements/solid/solide/sinit3.F
Chd|        SUINIT3                       source/elements/elbuf_init/suinit3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SBULK3(
     1          VOLU    ,NC      ,NNC     ,MAT     ,PM      ,
     2          VOLNOD  ,BVOLNOD ,VNS     ,BNS     ,VNSX    ,
     3          BNSX    ,FILL    )
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNC, NC(MVSIZ,NNC), MAT(*)
C     REAL
      my_real
     .   VNS(8,*),BNS(8,*),VNSX(12,*),BNSX(12,*),
     .   VOLU(*), FILL(*),
     .   PM(NPROPM,*), VOLNOD(*), BVOLNOD(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IP, N1, N2, N11, N22, N,
     .        IPERM101(10),IPERM102(10),IPERM103(10),
     .        IPERM161(16),IPERM162(16),
     .        IPERM201(20),IPERM202(20)
C     REAL
      my_real
     .   FAC,AV1,CV1,AV2,CV2
C
      DATA IPERM101/0,0,0,0,1,2,3,1,2,3/
      DATA IPERM102/0,0,0,0,2,3,1,4,4,4/
      DATA IPERM103/1,3,6,5,0,0,0,0,0,0/
      DATA IPERM161/0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8/
      DATA IPERM162/0,0,0,0,0,0,0,0,2,3,4,1,6,7,8,5/
      DATA IPERM201/0,0,0,0,0,0,0,0,1,2,3,4,1,2,3,4,5,6,7,8/
      DATA IPERM202/0,0,0,0,0,0,0,0,2,3,4,1,5,6,7,8,6,7,8,5/
C-----------------------------------------------------------------------
C
      IF(NNC==6) GOTO 60
      IF(NNC==8) GOTO 80
      IF(NNC==10)GOTO 100
      IF(NNC==16)GOTO 160
      IF(NNC==20)GOTO 200
C 
C     4 nodes tetrahedra
      DO I=LFT,LLT
       AV1 =VOLU(I)*FOURTH
       VNS(1,I)=AV1
       VNS(3,I)=AV1
       VNS(6,I)=AV1
       VNS(5,I)=AV1
       CV1=FILL(I)*PM(32,MAT(I))*AV1
       BNS(1,I)=CV1
       BNS(3,I)=CV1
       BNS(6,I)=CV1
       BNS(5,I)=CV1
      ENDDO
      GOTO 999
C 
C     Penta6 :
60    CONTINUE
      DO I=LFT,LLT
       AV1 =VOLU(I)*ONE_OVER_6
       VNS(1,I)=AV1
       VNS(2,I)=AV1
       VNS(3,I)=AV1
       VNS(5,I)=AV1
       VNS(6,I)=AV1
       VNS(7,I)=AV1
       CV1=FILL(I)*PM(32,MAT(I))*AV1
       BNS(1,I)=CV1
       BNS(2,I)=CV1
       BNS(3,I)=CV1
       BNS(5,I)=CV1
       BNS(6,I)=CV1
       BNS(7,I)=CV1
      ENDDO
      GOTO 999
C 
C     8 nodes bricks :
80    CONTINUE
      DO I=LFT,LLT
       AV1 =VOLU(I)*ONE_OVER_8
       VNS(1,I)=AV1
       VNS(2,I)=AV1
       VNS(3,I)=AV1
       VNS(4,I)=AV1
       VNS(5,I)=AV1
       VNS(6,I)=AV1
       VNS(7,I)=AV1
       VNS(8,I)=AV1
       CV1=FILL(I)*PM(32,MAT(I))*AV1
       BNS(1,I)=CV1
       BNS(2,I)=CV1
       BNS(3,I)=CV1
       BNS(4,I)=CV1
       BNS(5,I)=CV1
       BNS(6,I)=CV1
       BNS(7,I)=CV1
       BNS(8,I)=CV1
      ENDDO
      GOTO 999
C 
C     10 nodes tetrahedra :
100   CONTINUE
      DO I=LFT,LLT
       FAC=TRHEE_OVER_14
       AV1 = VOLU(I)*FAC/(FOUR*FAC+SIX)
       AV2 = VOLU(I)*ONE /(FOUR*FAC+SIX)
       VNS(1,I)=AV1
       VNS(3,I)=AV1
       VNS(6,I)=AV1
       VNS(5,I)=AV1
       CV1 = FILL(I)*PM(32,MAT(I))*AV1
       CV2 = FILL(I)*PM(32,MAT(I))*AV2
       BNS(1,I)=CV1
       BNS(3,I)=CV1
       BNS(6,I)=CV1
       BNS(5,I)=CV1
       DO N=5,10         
         IF(NC(I,N)/=0)THEN
           VNSX(N-4,I)=AV2
           BNSX(N-4,I)=CV2
         ELSE
           N11=IPERM101(N)
           N1 =IPERM103(N11)
           N22=IPERM102(N)
           N2 =IPERM103(N22)
           VNS(N1,I)=VNS(N1,I)+HALF*AV2
           VNS(N2,I)=VNS(N2,I)+HALF*AV2
           BNS(N1,I)=BNS(N1,I)+HALF*CV2
           BNS(N2,I)=BNS(N2,I)+HALF*CV2
         ENDIF
       ENDDO
      ENDDO
      GOTO 999
C 
C     16 nodes thick shells :
160   CONTINUE
      DO I=LFT,LLT
       AV1 = VOLU(I)/THIRTY2
       AV2 = VOLU(I)*THREE/THIRTY2
       VNS(1,I)=AV1
       VNS(2,I)=AV1
       VNS(3,I)=AV1
       VNS(4,I)=AV1
       VNS(5,I)=AV1
       VNS(6,I)=AV1
       VNS(7,I)=AV1
       VNS(8,I)=AV1
       CV1 = FILL(I)*PM(32,MAT(I))*AV1
       CV2 = FILL(I)*PM(32,MAT(I))*AV2
       BNS(1,I)=CV1
       BNS(2,I)=CV1
       BNS(3,I)=CV1
       BNS(4,I)=CV1
       BNS(5,I)=CV1
       BNS(6,I)=CV1
       BNS(7,I)=CV1
       BNS(8,I)=CV1
       DO N=9,16        
         IF(NC(I,N)/=0)THEN
           VNSX(N-8,I)=AV2
           BNSX(N-8,I)=CV2
         ELSE
           N1=IPERM161(N)
           N2=IPERM162(N)
           VNS(N1,I)=VNS(N1,I)+HALF*AV2
           VNS(N2,I)=VNS(N2,I)+HALF*AV2
           BNS(N1,I)=BNS(N1,I)+HALF*CV2
           BNS(N2,I)=BNS(N2,I)+HALF*CV2
         ENDIF
       ENDDO
      ENDDO
      GOTO 999
C 
C     20 nodes bricks :
200   CONTINUE
      DO I=LFT,LLT
       FAC=TRHEE_OVER_14
       AV1 = VOLU(I)*FAC/(EIGHT*FAC+TWELVE)
       AV2 = VOLU(I)*ONE /(EIGHT*FAC+TWELVE)
       VNS(1,I)=AV1
       VNS(2,I)=AV1
       VNS(3,I)=AV1
       VNS(4,I)=AV1
       VNS(5,I)=AV1
       VNS(6,I)=AV1
       VNS(7,I)=AV1
       VNS(8,I)=AV1
       CV1 = FILL(I)*PM(32,MAT(I))*AV1
       CV2 = FILL(I)*PM(32,MAT(I))*AV2
       BNS(1,I)=CV1
       BNS(2,I)=CV1
       BNS(3,I)=CV1
       BNS(4,I)=CV1
       BNS(5,I)=CV1
       BNS(6,I)=CV1
       BNS(7,I)=CV1
       BNS(8,I)=CV1
       DO N=9,20        
         IF(NC(I,N)/=0)THEN
           VNSX(N-8,I)=AV2
           BNSX(N-8,I)=CV2
         ELSE
           N1=IPERM201(N)
           N2=IPERM202(N)
           VNS(N1,I)=VNS(N1,I)+HALF*AV2
           VNS(N2,I)=VNS(N2,I)+HALF*AV2
           BNS(N1,I)=BNS(N1,I)+HALF*CV2
           BNS(N2,I)=BNS(N2,I)+HALF*CV2
         ENDIF
       ENDDO
      ENDDO
      GOTO 999
C
 999  CONTINUE
      RETURN
      END
