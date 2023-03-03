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
Chd|  LINEDGE                       source/groups/linedge.F       
Chd|-- called by -----------
Chd|        HM_READ_LINES                 source/groups/hm_read_lines.F 
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|====================================================================
      SUBROUTINE LINEDGE(NSEG0  ,NSEG        ,BUFTMP   ,SLIN_NODES ,KEY,
     .                   FLAG   ,SLIN_ELTYP  ,SLIN_ELEM,LINE_NSEG0)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSEG0,NSEG,FLAG,LINE_NSEG0
      INTEGER BUFTMP(6,*),SLIN_NODES(LINE_NSEG0,*),SLIN_ELTYP(*),
     .        SLIN_ELEM(*)
      CHARACTER KEY*ncharkey
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,J1,J2
      INTEGER IW1(4*NSEG0),IW2(4*NSEG0),IW5(4*NSEG0),IW6(4*NSEG0),
     .        INDEX(8*NSEG0),IWORK(70000), IPERM(4)
      DATA IPERM /2,3,4,1/
C=======================================================================
      K=0
      IW1 = 0
      IW2 = 0
      IW5 = 0
      IW6 = 0
      INDEX = 0
      DO I = 1,NSEG0
        DO J1=1,4
          J2=IPERM(J1)
          IF(BUFTMP(J2,I)/=0.AND.
     .       BUFTMP(J1,I)>BUFTMP(J2,I))THEN
            K=K+1
            IW1(K)=BUFTMP(J2,I)
            IW2(K)=BUFTMP(J1,I)
            IW5(K)=BUFTMP(5,I)
            IW6(K)=BUFTMP(6,I)
          ELSEIF(BUFTMP(J1,I)/=0.AND.
     .           BUFTMP(J1,I)<BUFTMP(J2,I))THEN
            K=K+1
            IW1(K)=BUFTMP(J1,I)
            IW2(K)=BUFTMP(J2,I)
            IW5(K)=BUFTMP(5,I)
            IW6(K)=BUFTMP(6,I)
          ENDIF
        ENDDO
      ENDDO
C-----------------------------------------------
      CALL MY_ORDERS( 0,IWORK,IW1,INDEX,K,1)  
      CALL MY_ORDERS(10,IWORK,IW2,INDEX,K,1)  
C-----------------------------------------------
      IF (KEY(1:4) == 'SURF') THEN
C-----------------------------------------------
C SUPPRESSION DES SEGMENTS DOUBLES
C-----------------------------------------------
        NSEG=1
        IF (FLAG == 0) THEN
          DO I=2,K
            IF(IW1(INDEX(I-1))/=IW1(INDEX(I)).OR.
     .         IW2(INDEX(I-1))/=IW2(INDEX(I))) NSEG=NSEG+1
          ENDDO
        ELSEIF (FLAG == 1) THEN
          SLIN_NODES(1,1) = IW1(INDEX(1))
          SLIN_NODES(1,2) = IW2(INDEX(1))
          SLIN_ELTYP(1)   = IW5(INDEX(1))
          SLIN_ELEM(1)    = IW6(INDEX(1))
          DO I=2,K
            IF(IW1(INDEX(I-1))/=IW1(INDEX(I)).OR.
     .         IW2(INDEX(I-1))/=IW2(INDEX(I)))THEN
              NSEG=NSEG+1
              SLIN_NODES(NSEG,1) = IW1(INDEX(I))
              SLIN_NODES(NSEG,2) = IW2(INDEX(I))
              SLIN_ELTYP(NSEG)   = IW5(INDEX(I))
              SLIN_ELEM(NSEG)    = IW6(INDEX(I))
            ENDIF
          ENDDO
        ENDIF ! IF (FLAG == 0)
      ELSEIF (KEY(1:4) == 'EDGE') THEN
C-----------------------------------------------
C SUPPRESSION DES SEGMENTS INTERNES (SAUF BORDS)
C-----------------------------------------------
        NSEG=0
        IF (FLAG == 0) THEN
          IF(IW1(INDEX(1))/=IW1(INDEX(2)).OR.
     .       IW2(INDEX(1))/=IW2(INDEX(2))) NSEG=1
          DO I=2,K-1
            IF((IW1(INDEX(I-1))/=IW1(INDEX(I)).OR.
     .          IW2(INDEX(I-1))/=IW2(INDEX(I))).AND.
     .         (IW1(INDEX(I+1))/=IW1(INDEX(I)).OR.
     .          IW2(INDEX(I+1))/=IW2(INDEX(I)))) NSEG=NSEG+1
          ENDDO
          IF(IW1(INDEX(K-1))/=IW1(INDEX(K)).OR.
     .       IW2(INDEX(K-1))/=IW2(INDEX(K))) NSEG=NSEG+1                    
C
        ELSEIF (FLAG == 1) THEN
          IF(IW1(INDEX(1))/=IW1(INDEX(2)).OR.
     .       IW2(INDEX(1))/=IW2(INDEX(2)))THEN
             NSEG=1                    
             SLIN_NODES(NSEG,1) = IW1(INDEX(1))
             SLIN_NODES(NSEG,2) = IW2(INDEX(1))
             SLIN_ELTYP(NSEG)   = IW5(INDEX(1))
             SLIN_ELEM(NSEG)    = IW6(INDEX(1))
          ENDIF
          DO I=2,K-1
            IF((IW1(INDEX(I-1))/=IW1(INDEX(I)).OR.
     .          IW2(INDEX(I-1))/=IW2(INDEX(I))).AND.
     .         (IW1(INDEX(I+1))/=IW1(INDEX(I)).OR.
     .          IW2(INDEX(I+1))/=IW2(INDEX(I))))THEN
              NSEG=NSEG+1
              SLIN_NODES(NSEG,1) = IW1(INDEX(I))
              SLIN_NODES(NSEG,2) = IW2(INDEX(I))
              SLIN_ELTYP(NSEG)   = IW5(INDEX(I))
              SLIN_ELEM(NSEG)    = IW6(INDEX(I))
            ENDIF
          ENDDO
          IF(IW1(INDEX(K-1))/=IW1(INDEX(K)).OR.
     .       IW2(INDEX(K-1))/=IW2(INDEX(K)))THEN
            NSEG=NSEG+1                    
            SLIN_NODES(NSEG,1) = IW1(INDEX(K))
            SLIN_NODES(NSEG,2) = IW2(INDEX(K))
            SLIN_ELTYP(NSEG)   = IW5(INDEX(K))
            SLIN_ELEM(NSEG)    = IW6(INDEX(K))
          ENDIF ! IF (FLAG == 0)
        ENDIF ! IF (KEY(1:4) == 'SURF')
C-----------
      ENDIF
C-----------
      RETURN
      END
