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
Chd|====================================================================
Chd|  IQEL03                        source/ale/ale3d/iqel03.F     
Chd|-- called by -----------
Chd|        INTAL2                        source/ale/inter/intal2.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IQEL03(X,IRECT,LMSR,MSR,NSV,ILOC,IRTL,
     +                  NSN,NSEG,CRST,NOR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NSN
      INTEGER,INTENT(IN) :: IRECT(4,*), LMSR(*), MSR(*), NSV(*), ILOC(*), IRTL(*),NSEG(*)
      my_real,INTENT(IN) :: X(3,NUMNOD), CRST(2,*)
      my_real,INTENT(INOUT) :: NOR(3,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr08_s_c.inc"
#include      "scr08_a_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II, I, J, K, L, JJ, NN, LK, NUM, NPT, K1, K2, K3, NODE1,
     .   NODE2, KK
      my_real N1, N2, N3, XX112, XX212, XX312, XX114, XX214, XX314, XX123,
     .        XX223, XX323, XX134, XX234, XX334, SS, TT, TP, TM, FS1, FS2,
     .        FS3, SP, SM, FT1, FT2, FT3, SCK, TCK, AJ4, AJ5, AJ6, AJ7, AJ8,
     .       AJ9, V1, V2, V3, XMG, XM1, XM2
C-----------------------------------------------
      DO II=1,NSN
        I=NSV(II)
        J=ILOC(II)
        K=MSR(J)
        L=IRTL(II)
        DO JJ=1,4
          NN = MSR(IRECT(JJ,L))
          IX(JJ) = NN
          XX1(JJ) = X(1,NN)
          XX2(JJ) = X(2,NN)
          XX3(JJ) = X(3,NN)
        ENDDO !next JJ
        XS1=X(1,I)
        YS1=X(2,I)
        ZS1=X(3,I)
        XX112=XX1(1)-XX1(2)
        XX212=XX2(1)-XX2(2)
        XX312=XX3(1)-XX3(2)
        XX114=XX1(1)-XX1(4)
        XX214=XX2(1)-XX2(4)
        XX314=XX3(1)-XX3(4)
        XX123=XX1(2)-XX1(3)
        XX223=XX2(2)-XX2(3)
        XX323=XX3(2)-XX3(3)
        XX134=XX1(3)-XX1(4)
        XX234=XX2(3)-XX2(4)
        XX334=XX3(3)-XX3(4)
        SS=CRST(1,II)
        TT=CRST(2,II)
        TP=FOURTH*(ONE + TT)
        TM=FOURTH*(ONE - TT)
        FS1=TP*XX134-TM*XX112
        FS2=TP*XX234-TM*XX212
        FS3=TP*XX334-TM*XX312
        ! Specific Case
        IF(IX(3) == IX(4) .AND. TM == ZERO) THEN
          FS1=-XX112
          FS2=-XX212
          FS3=-XX312
        ENDIF

        SP=ONE + SS
        SM=ONE - SS
        FT1=-SM*XX114-SP*XX123
        FT2=-SM*XX214-SP*XX223
        FT3=-SM*XX314-SP*XX323

        N1=FS2*FT3-FS3*FT2
        N2=FS3*FT1-FS1*FT3
        N3=FS1*FT2-FS2*FT1

        SCK=ABS(SS) - ONE
        TCK=ABS(TT) - ONE
        
        LK=L
        NUM=NSEG(J+1)-NSEG(J)
        IF(NUM <= 4 .AND. (ABS(SCK) <= FIVEEM2.OR.ABS(TCK) <= FIVEEM2) ) THEN  
          NPT = NSEG(J)-1
          N1  = ZERO
          N2  = ZERO
          N3  = ZERO    
          IF(ABS(SCK) > FIVEEM2 .OR. ABS(TCK) > FIVEEM2) THEN
            !C----------------------------------------------------
            !C     TRAITEMENT DES BORDS
            !C---------------------------------------------------
            IF(K == IX(1)) THEN
              K1=1
              K2=2
              K3=4
            ELSEIF(K == IX(2)) THEN
              K1=2
              K2=3
              K3=1
            ELSEIF(K == IX(3)) THEN
              K1=3
              K2=4
              K3=2
              IF(IX(3) == IX(4)) K2=1
            ELSE
              K1=4
              K2=1
              K3=3
            ENDIF
            AJ4=XX1(K2)-XX1(K1)
            AJ5=XX2(K2)-XX2(K1)
            AJ6=XX3(K2)-XX3(K1)
            AJ7=XX1(K3)-XX1(K1)
            AJ8=XX2(K3)-XX2(K1)
            AJ9=XX3(K3)-XX3(K1)
            N1=N1+AJ5*AJ9-AJ6*AJ8
            N2=N2+AJ6*AJ7-AJ4*AJ9
            N3=N3+AJ4*AJ8-AJ5*AJ7
            V1=XS1-XX1(K1)
            V2=YS1-XX2(K1)
            V3=ZS1-XX3(K1)
            XMG=SQRT(AJ4**2+AJ5**2+AJ6**2)
            XM1=(V1*AJ4+V2*AJ5+V3*AJ6)/XMG
            XMG=SQRT(AJ7**2+AJ8**2+AJ9**2)
            XM2=(V1*AJ7+V2*AJ8+V3*AJ9)/XMG
            NODE1=IX(K1)
            NODE2=IX(K2)
            IF(XM2 > XM1) NODE2=IX(K3)
            DO JJ=1,NUM
              L=LMSR(NPT+JJ)
              IF(L /= LK) THEN
                DO KK=1,4
                  NN=MSR(IRECT(KK,L))
                  IX(KK)=NN
                  XX1(KK)=X(1,NN)
                  XX2(KK)=X(2,NN)
                  XX3(KK)=X(3,NN)
                ENDDO !next KK
                IF(K == IX(1))THEN
                  K1=1
                  K2=2
                  K3=4
                ELSEIF(K == IX(2))THEN
                  K1=2
                  K2=3
                  K3=1
                ELSEIF(K == IX(3))THEN
                  K1=3
                  K2=4
                  K3=2
                  IF(IX(3) == IX(4)) K2=1
                ELSE
                  K1=4
                  K2=1
                  K3=3
                ENDIF
                IF(NODE2 == IX(K2) .OR. NODE2 == IX(K3)) THEN
                  AJ4 = XX1(K2)-XX1(K1)
                  AJ5 = XX2(K2)-XX2(K1)
                  AJ6 = XX3(K2)-XX3(K1)
                  AJ7 = XX1(K3)-XX1(K1)
                  AJ8 = XX2(K3)-XX2(K1)
                  AJ9 = XX3(K3)-XX3(K1)
                  N1  = N1+AJ5*AJ9-AJ6*AJ8
                  N2  = N2+AJ6*AJ7-AJ4*AJ9
                  N3  = N3+AJ4*AJ8-AJ5*AJ7
                ENDIF
              ENDIF!IF(L /= LK)
            ENDDO !next JJ
          ELSE
            !C------------------------------------------------------
            !C     TREATMENT SPECIFIC TO CORNERS
            !C------------------------------------------------------
            DO  JJ=1,NUM 
              L=LMSR(NPT+JJ)
              DO  KK=1,4
                 NN=MSR(IRECT(KK,L))
                 IX(KK)  = NN
                 XX1(KK) = X(1,NN)
                 XX2(KK) = X(2,NN)
                 XX3(KK) = X(3,NN)
              ENDDO !next KK
              IF(K == IX(1)) THEN
                K1=1
                K2=2
                K3=4
              ELSEIF(K == IX(2))THEN
                K1=2
                K2=3
                K3=1
              ELSEIF(K == IX(3))THEN
                K1=3
                K2=4
                K3=2
                IF(IX(3) == IX(4)) K2=1
              ELSE
                K1=4
                K2=1
                K3=3
              ENDIF
              AJ4=XX1(K2)-XX1(K1)
              AJ5=XX2(K2)-XX2(K1)
              AJ6=XX3(K2)-XX3(K1)
              AJ7=XX1(K3)-XX1(K1)
              AJ8=XX2(K3)-XX2(K1)
              AJ9=XX3(K3)-XX3(K1)
              N1=N1+AJ5*AJ9-AJ6*AJ8
              N2=N2+AJ6*AJ7-AJ4*AJ9
              N3=N3+AJ4*AJ8-AJ5*AJ7
            ENDDO !next JJ
          ENDIF
        ENDIF
        XMG       = SQRT(N1*N1+N2*N2+N3*N3)
        N1        = N1/XMG
        N2        = N2/XMG
        N3        = N3/XMG
        NOR(1,II) = N1
        NOR(2,II) = N2
        NOR(3,II) = N3
      ENDDO !next II
C-----------------------------------------------      
      RETURN
      END
