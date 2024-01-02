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
Chd|  FVELINTE                      source/airbag/fvelinte.F      
Chd|-- called by -----------
Chd|        FVMESH0                       source/airbag/fvmesh0.F       
Chd|-- calls ---------------
Chd|        FVBAG_MOD                     share/modules1/fvbag_mod.F    
Chd|====================================================================
      SUBROUTINE FVELINTE(IBUF, ELEM, IXC, IXTG, PM, IPM, ILVOUT, IFV, 
     .                    NNT, NTG, POROSITY, ITAB, NSEG,SURF_ELTYP,
     .                    NTGI,ELTG,NB_NODE,SURF_ELEM)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE FVBAG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*), IXTG(NIXTG,*), IPM(NPROPMI,*), ILVOUT, IFV
      INTEGER IBUF(*), ELEM(3,*), NNT, NTG, ITAB(*),SURF_ELTYP(*),SURF_ELEM(*)
      INTEGER NSEG,NTGI, ELTG(*), NB_NODE
C     REAL
      my_real
     .   PM(NPROPM,*), POROSITY(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, ITAG(NB_NODE), J, JJ, ICMAX, NC, I1, I2, I3, IFOUND
      INTEGER K, KK, ITY, N1, N2, N3, IEL, ILAW, MAT, LEAK
      INTEGER J1, J2, J3, NNT_TMP
      INTEGER ITAGC(NUMELC), ITAGTG(NUMELTG)
      CHARACTER*6 TITL
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: CNS
C
      DO I=1,NB_NODE
         ITAG(I)=0
      ENDDO

C     Number connected surface element to each node
      DO I = 1, NTGI
         N1 = ELEM(1, I)
         N2 = ELEM(2, I)
         N3 = ELEM(3, I) 
         N1 = IBUF(N1)
         N2 = IBUF(N2)
         N3 = IBUF(N3)
         ITAG(N1) = ITAG(N1) + 1
         ITAG(N2) = ITAG(N2) + 1
         ITAG(N3) = ITAG(N3) + 1
      ENDDO
C     Build connectivity
      ICMAX = 0
      DO I = 1, NB_NODE
         ICMAX = MAX(ICMAX, ITAG(I))
      ENDDO
      DO I = 1, NTGI 
         N1 = ELEM(1, I)
         N2 = ELEM(2, I)
         N3 = ELEM(3, I) 
         N1 = IBUF(N1)
         N2 = IBUF(N2)
         N3 = IBUF(N3)
         ITAG(N1) = 0
         ITAG(N2) = 0
         ITAG(N3) = 0
      ENDDO
      ALLOCATE(CNS(NB_NODE,1+ICMAX*2))
      DO I=1,NB_NODE
         CNS(I, 1)=0
      ENDDO
      DO I = 1, NTGI
C     Node 1
         N1 = IBUF(ELEM(1, I))
         NC = CNS(N1, 1)
         NC = NC + 1
         CNS(N1, 1) = NC
         CNS(N1, 1 + NC) = I
C     Node 2
         N2 = IBUF(ELEM(2, I))
         NC = CNS(N2, 1)
         NC = NC + 1
         CNS(N2, 1) = NC
         CNS(N2, 1 + NC) = I
C     Node 3
         N3 = IBUF(ELEM(3, I))
         NC = CNS(N3, 1)
         NC = NC + 1
         CNS(N3, 1) = NC
         CNS(N3, 1 + NC) = I
      ENDDO

      DO I=1,FVDATA(IFV)%NNTR
         IEL=FVDATA(IFV)%IFVTRI(4,I)
         IF (IEL == 0) THEN
C     Triangle interne appuye sur element solide
            IFOUND=0
            I1=FVDATA(IFV)%IFVTRI(1,I)
            I2=FVDATA(IFV)%IFVTRI(2,I)
            I3=FVDATA(IFV)%IFVTRI(3,I)
            N1=FVDATA(IFV)%IFVNOD(1,I1)
            N2=FVDATA(IFV)%IFVNOD(1,I2)
            N3=FVDATA(IFV)%IFVNOD(1,I3)
            IF(N1==2.AND.N2==2.AND.N3==2) THEN
               I1=FVDATA(IFV)%IFVNOD(2,I1)
               I2=FVDATA(IFV)%IFVNOD(2,I2)
               I3=FVDATA(IFV)%IFVNOD(2,I3)
               ITAG(I1) = 1
               ITAG(I2) = 1
               ITAG(I3) = 1
C     Surface elements connected to node I1
               DO J = 1, CNS(I1,1)               
                  N1 = IBUF(ELEM(1, CNS(I1, 1 + J)))
                  N2 = IBUF(ELEM(2, CNS(I1, 1 + J)))
                  N3 = IBUF(ELEM(3, CNS(I1, 1 + J))) 
                  IF (ITAG(N1) * ITAG(N2) * ITAG(N3) /= 0) THEN
                     FVDATA(IFV)%IFVTRI(4,I) = -CNS(I1, 1 + J)
                  ENDIF
               ENDDO
C     reset tags
               ITAG(I1) = 0
               ITAG(I2) = 0
               ITAG(I3) = 0
            ENDIF
         ENDIF
      ENDDO
      
      DEALLOCATE(CNS)

      DO I=1,NUMELC
      ITAGC(I)=0
         DO J=1,4
            JJ=IXC(1+J,I)
            ITAG(JJ)=ITAG(JJ)+1
         ENDDO
      ENDDO
      DO I=1,NUMELTG
      ITAGTG(I)=0
         DO J=1,3
            JJ=IXTG(1+J,I)
            ITAG(JJ)=ITAG(JJ)+1
         ENDDO
      ENDDO
C TAG elements of internal surface
      DO I=1,NSEG
        ITY=SURF_ELTYP(I)
        II =SURF_ELEM(I)
        IF(ITY==3) ITAGC(II)=1
        IF(ITY==7) ITAGTG(II)=1
      ENDDO
C
      ICMAX=0
      DO I=1,NB_NODE
         ICMAX=MAX(ICMAX,ITAG(I))
      ENDDO
C
      ALLOCATE(CNS(NB_NODE,1+ICMAX*2))
      DO I=1,NB_NODE
         CNS(I,1)=0
      ENDDO
      DO I=1,NUMELC
      IF(ITAGC(I)==0) CYCLE
         DO J=1,4
            JJ=IXC(1+J,I)
            NC=CNS(JJ,1)
            NC=NC+1
            CNS(JJ,1)=NC
            CNS(JJ,1+2*(NC-1)+1)=1
            CNS(JJ,1+2*(NC-1)+2)=I
         ENDDO
      ENDDO   
      DO I=1,NUMELTG
      IF(ITAGTG(I)==0) CYCLE
         DO J=1,3
            JJ=IXTG(1+J,I)
            NC=CNS(JJ,1)
            NC=NC+1
            CNS(JJ,1)=NC
            CNS(JJ,1+2*(NC-1)+1)=2
            CNS(JJ,1+2*(NC-1)+2)=I
         ENDDO
      ENDDO   
C
      DO I=1,NB_NODE
         ITAG(I)=ZERO
      ENDDO
C
      NNT_TMP = 0
      DO I=1,FVDATA(IFV)%NNTR
         IEL=FVDATA(IFV)%IFVTRI(4,I)
         IF (IEL < 0) THEN
C     Triangle interne appuye sur element solide
            IFOUND=0
            I1=FVDATA(IFV)%IFVTRI(1,I)
            I2=FVDATA(IFV)%IFVTRI(2,I)
            I3=FVDATA(IFV)%IFVTRI(3,I)
            N1=FVDATA(IFV)%IFVNOD(1,I1)
            N2=FVDATA(IFV)%IFVNOD(1,I2)
            N3=FVDATA(IFV)%IFVNOD(1,I3)
            IF(N1==2.AND.N2==2.AND.N3==2) THEN
               I1=FVDATA(IFV)%IFVNOD(2,I1)
               I2=FVDATA(IFV)%IFVNOD(2,I2)
               I3=FVDATA(IFV)%IFVNOD(2,I3)
               DO J=1,CNS(I1,1)
                  ITY=CNS(I1,1+2*(J-1)+1)
                  JJ=CNS(I1,1+2*(J-1)+2)
                  IF (ITY==1) THEN
                     DO K=1,4
                        KK=IXC(1+K,JJ)
                        ITAG(KK)=1
                     ENDDO
                     IF (ITAG(I1)==1.AND.ITAG(I2)==1.AND.ITAG(I3)==1) THEN
                        IFOUND=JJ
                     ENDIF
                     DO K=1,4
                        KK=IXC(1+K,JJ)
                        ITAG(KK)=0
                     ENDDO
                  ELSEIF (ITY==2) THEN
                     DO K=1,3
                        KK=IXTG(1+K,JJ)
                        ITAG(KK)=1
                     ENDDO
                     IF (ITAG(I1)==1.AND.ITAG(I2)==1.AND.ITAG(I3)==1) THEN
                        IFOUND=NUMELC+JJ
                     ENDIF
                     DO K=1,3
                        KK=IXTG(1+K,JJ)
                        ITAG(KK)=0
                     ENDDO
                  ENDIF
                  IF (IFOUND /= 0) THEN
                     EXIT
                  ENDIF
               ENDDO
               IF(IFOUND/=0) THEN 
                  FVDATA(IFV)%IFVTRI(4,I)=-IFOUND
                  NNT_TMP = NNT_TMP + 1
               ENDIF
            ENDIF            
         ENDIF
      ENDDO
C     
C REDEFINE INTERNAL TRIANGLE FOR COMPATIBILITY WITH BRICKS
C
      DO I=1,NNT
        J=IBUF(I)
        ITAG(J)=I
      ENDDO
      II=0
      DO I=1,FVDATA(IFV)%NNTR
         IFOUND=FVDATA(IFV)%IFVTRI(4,I)
         IF(IFOUND<0) THEN
           I1=FVDATA(IFV)%IFVTRI(1,I)
           I2=FVDATA(IFV)%IFVTRI(2,I)
           I3=FVDATA(IFV)%IFVTRI(3,I)
           J1=FVDATA(IFV)%IFVNOD(1,I1)
           J2=FVDATA(IFV)%IFVNOD(1,I2)
           J3=FVDATA(IFV)%IFVNOD(1,I3)
           IF(J1 == 2 .AND. J2==2 .AND. J3==2) THEN
             N1=FVDATA(IFV)%IFVNOD(2,I1)
             N2=FVDATA(IFV)%IFVNOD(2,I2)
             N3=FVDATA(IFV)%IFVNOD(2,I3)
             II=II+1
             ELEM(1,II)=ITAG(N1)
             ELEM(2,II)=ITAG(N2)
             ELEM(3,II)=ITAG(N3)
             FVDATA(IFV)%IFVTRI(4,I)=-NTG-II
             ELTG(NTG+II)=-IFOUND
           ENDIF
         ENDIF
      ENDDO
C
C DEFINE POROSITY
C
      IF(ILVOUT >= 1) WRITE(IOUT,1000)
      DO I=1,NTGI
           IEL=ELTG(NTG+I)
           IF (IEL<=NUMELC) THEN
             MAT =IXC(1,IEL)
             KK  =IXC(NIXC,IEL)
             TITL='SHELL:'
           ELSEIF (IEL>NUMELC) THEN
             JJ=IEL-NUMELC
             MAT =IXTG(1,JJ)
             KK  =IXTG(NIXTG,JJ)
             TITL='SH3N: '
           ENDIF
C
           ILAW=IPM(2,MAT)
           LEAK=IPM(4,MAT)
           IF (ILAW == 0) THEN
             POROSITY(I)=ONE
           ELSEIF(ILAW == 19 .AND. LEAK == 0) THEN
             POROSITY(I)=PM(56,MAT)
           ELSE
             POROSITY(I)=ZERO             
           ENDIF
           IF(ILVOUT >= 1) THEN
             IF(ILAW == 58 .OR. (ILAW == 19 .AND. LEAK > 0)) THEN
               WRITE(IOUT,1100) I,TITL,KK,LEAK
             ELSE
               WRITE(IOUT,1150) I,TITL,KK,POROSITY(I)
             ENDIF
           ENDIF
      ENDDO
C
      IF(ILVOUT >= 5) THEN
        WRITE(IOUT,1200)
        DO I=1,FVDATA(IFV)%NNTR
          I1=FVDATA(IFV)%IFVTRI(1,I)
          I2=FVDATA(IFV)%IFVTRI(2,I)
          I3=FVDATA(IFV)%IFVTRI(3,I)
          N1=FVDATA(IFV)%IFVTRI(4,I)
          N2=FVDATA(IFV)%IFVTRI(5,I)
          N3=FVDATA(IFV)%IFVTRI(6,I)
          WRITE(IOUT,'(5X,7I8)') I,I1,I2,I3,N1,N2,N3
        ENDDO
C
        WRITE(IOUT,1300)
        DO I=1,FVDATA(IFV)%NNS
          N1=FVDATA(IFV)%IFVNOD(1,I)
          N2=FVDATA(IFV)%IFVNOD(2,I)
          N3=FVDATA(IFV)%IFVNOD(3,I)
          WRITE(IOUT,'(5X,4I8)') I,N1,N2,N3
        ENDDO
      ENDIF
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
 1000 FORMAT(/5X,'FVMBAG  INTERNAL ELEMENTS <-> SHELL or SH3N ',
     .        3X,'POROSITY/ILEAKAGE'
     .       /5X,'----------------------------------------------------',
     .        '-----------')
 1100 FORMAT( 5X,'TRIANGLE:',I8,' <-> ',A,I10,10X,I6)
 1150 FORMAT( 5X,'TRIANGLE:',I8,' <-> ',A,I10,10X,F6.3)
 1200 FORMAT(/5X,'TRIANGLE',9X,'NODES',10X,'ELEMENT',4X,'FINITE VOLUME')
 1300 FORMAT(/5X,'FV POINT',4X,'FLAG',1X,'ELEM/NODE',2X,'NODE')
C
      DEALLOCATE(CNS)
C     
      RETURN
      END
