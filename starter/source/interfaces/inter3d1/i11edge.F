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
C
Chd|====================================================================
Chd|  I11EDGE                       source/interfaces/inter3d1/i11edge.F
Chd|-- called by -----------
Chd|        LECINS                        source/interfaces/interf1/lecins.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I11EDGE(IPARI,INTBUF_TAB)
C-----------------------------------------------------------------------
C     SUBROUTINE FROM GW
C-----------------------------------------------------------------------
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NRTS, NRTM,NSN,NMN,I,NPTR,OLDPTR
C
      NRTS=IPARI(3)
      NRTM=IPARI(4)
      NSN =IPARI(5)
      NMN =IPARI(6)
C
      !compact IRTS on 2*NRTS
      DO I = 1, NRTS !KD(10)
        INTBUF_TAB%IRECTS(2*(I-1)+1) = INTBUF_TAB%IRECTS(4*(I-1)+1)
        INTBUF_TAB%IRECTS(2*(I-1)+2) = INTBUF_TAB%IRECTS(4*(I-1)+2)
      ENDDO

      INTBUF_TAB%S_IRECTS = 2*NRTS
C
      !compact IRTM on 2*NRTM
      DO I = 1, NRTM !(KD(11)
        INTBUF_TAB%IRECTM(2*(I-1)+1) = INTBUF_TAB%IRECTM(4*(I-1)+1)
        INTBUF_TAB%IRECTM(2*(I-1)+2) = INTBUF_TAB%IRECTM(4*(I-1)+2)
      ENDDO

      INTBUF_TAB%S_IRECTM = 2*NRTM
C
      RETURN
      END
C
Chd|====================================================================
Chd|  I11EDGE1                      source/interfaces/inter3d1/i11edge.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|====================================================================
      SUBROUTINE I11EDGE1(IPARI,IRECTS,IRECTM,IW1   ,
     .                   IW2  ,IW3   ,IW4   ,IW5   ,IW6   ,
     .                   INDEX)
C-----------------------------------------------------------------------
C     SUBROUTINE FROM GW
C-----------------------------------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECTS(4,*), IRECTM(4,*), IPARI(*),INDEX(*) 
      INTEGER IW1(*), IW2(*), IW3(*), IW4(*), IW5(*), IW6(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NRTS, NRTM, K, I,J1,J2,
     .        K1,K2,K3,K4,K5,K6,K7,NRTS2,NRTM2,
     .        IWORK(70000), IPERM(4)
      DATA IPERM /2,3,4,1/
      NRTS=IPARI(3)
      NRTM=IPARI(4)
C
      K=0
      DO I = 1,NRTS
        IF(IRECTS(3,I)==IRECTS(4,I).AND.
     .     IRECTS(3,I)==IRECTS(1,I))THEN
          IF(IRECTS(1,I)>IRECTS(2,I))THEN
            K=K+1
            IW1(K)=IRECTS(2,I)
            IW2(K)=IRECTS(1,I)
          ELSEIF(IRECTS(1,I)<IRECTS(2,I))THEN
            K=K+1
            IW1(K)=IRECTS(1,I)
            IW2(K)=IRECTS(2,I)
          ENDIF
        ELSE
         DO J1=1,4
          J2=IPERM(J1)
          IF(IRECTS(J2,I)/=0.AND.
     .       IRECTS(J1,I)>IRECTS(J2,I))THEN
            K=K+1
            IW1(K)=IRECTS(J2,I)
            IW2(K)=IRECTS(J1,I)
          ELSEIF(IRECTS(J1,I)/=0.AND.
     .           IRECTS(J1,I)<IRECTS(J2,I))THEN
            K=K+1
            IW1(K)=IRECTS(J1,I)
            IW2(K)=IRECTS(J2,I)
          ENDIF
         ENDDO
        ENDIF
      ENDDO
      CALL MY_ORDERS(0,IWORK,IW1,INDEX,K,1)
      CALL MY_ORDERS(10,IWORK,IW2,INDEX,K,1)
      NRTS2=1
      IW3(NRTS2)=IW1(INDEX(1))
      IW4(NRTS2)=IW2(INDEX(1))
      DO I=2,K
        IF(IW1(INDEX(I-1))/=IW1(INDEX(I)).OR.
     .     IW2(INDEX(I-1))/=IW2(INDEX(I)))THEN
            NRTS2=NRTS2+1
            IW3(NRTS2)=IW1(INDEX(I))
            IW4(NRTS2)=IW2(INDEX(I))
        ENDIF
      ENDDO
C
      K=0
      DO I = 1,NRTM
        IF(IRECTM(3,I)==IRECTM(4,I).AND.
     .     IRECTM(3,I)==IRECTM(1,I))THEN
          IF(IRECTM(1,I)>IRECTM(2,I))THEN
            K=K+1
            IW1(K)=IRECTM(2,I)
            IW2(K)=IRECTM(1,I)
          ELSEIF(IRECTM(1,I)<IRECTM(2,I))THEN
            K=K+1
            IW1(K)=IRECTM(1,I)
            IW2(K)=IRECTM(2,I)
          ENDIF
        ELSE
         DO J1=1,4
          J2=IPERM(J1)
          IF(IRECTM(J1,I)/=0.AND.
     .       IRECTM(J1,I)>IRECTM(J2,I))THEN
            K=K+1
            IW1(K)=IRECTM(J2,I)
            IW2(K)=IRECTM(J1,I)
          ELSEIF(IRECTM(J2,I)/=0.AND.
     .       IRECTM(J1,I)<IRECTM(J2,I))THEN
            K=K+1
            IW1(K)=IRECTM(J1,I)
            IW2(K)=IRECTM(J2,I)
          ENDIF
         ENDDO
        ENDIF
      ENDDO
      CALL MY_ORDERS(0,IWORK,IW1,INDEX,K,1)
      CALL MY_ORDERS(10,IWORK,IW2,INDEX,K,1)
      NRTM2=1
      IW5(NRTM2)=IW1(INDEX(1))
      IW6(NRTM2)=IW2(INDEX(1))
      DO I=2,K
        IF(IW1(INDEX(I-1))/=IW1(INDEX(I)).OR.
     .     IW2(INDEX(I-1))/=IW2(INDEX(I)))THEN
            NRTM2=NRTM2+1
            IW5(NRTM2)=IW1(INDEX(I))
            IW6(NRTM2)=IW2(INDEX(I))
        ENDIF
      ENDDO
C
      IPARI(3)=NRTS2
      IPARI(4)=NRTM2
C
      RETURN
      END
C
Chd|====================================================================
Chd|  I11EDGE2                      source/interfaces/inter3d1/i11edge.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I11EDGE2(IPARI,IRECTS,IRECTM,NSV ,MSR,
     .                   IW3   ,IW4   ,IW5   ,IW6 ,NTAG )
C-----------------------------------------------------------------------
C     SUBROUTINE FROM GW
C-----------------------------------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECTS(2,*), IRECTM(2,*), IPARI(*) ,NSV(*),MSR(*)
      INTEGER IW3(*), IW4(*), IW5(*), IW6(*),NTAG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NSN,NMN,NRTS, NRTM, I
C
      NRTS=IPARI(3)
      NRTM=IPARI(4)
C
C
      DO I = 1,NRTS
        IRECTS(1,I) = IW3(I)
        IRECTS(2,I) = IW4(I)
      ENDDO
C
      DO I = 1,NRTM
        IRECTM(1,I) = IW5(I)
        IRECTM(2,I) = IW6(I)
      ENDDO
C----------------------------------
      DO I = 1,NUMNOD
          NTAG(I) = 0
      ENDDO
      DO I = 1,NRTS
        NTAG(IRECTS(1,I)) = 1
        NTAG(IRECTS(2,I)) = 1
      ENDDO
C
      NSN=0
      DO I = 1,NUMNOD
        IF(NTAG(I)==1) THEN
          NSN = NSN+1
          NSV(NSN)=I
          NTAG(I) = 0
        ENDIF
      ENDDO
C
      DO I = 1,NRTM
        NTAG(IRECTM(1,I)) = 1
        NTAG(IRECTM(2,I)) = 1
      ENDDO
C
      NMN=0
      DO I = 1,NUMNOD
        IF(NTAG(I)==1) THEN
          NMN = NMN+1
          MSR(NMN)=I
        ENDIF
      ENDDO
C
      RETURN
      END
