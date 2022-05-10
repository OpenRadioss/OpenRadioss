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
#ifndef DNC
Chd|====================================================================
Chd|  KLDIM                         stub/kldim.F                  
Chd|-- called by -----------
Chd|        IMP_EIGSOL                    com/eig/imp_eigsol.F          
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE KLDIM(EIGIPM, EIGIBUF, NDOF  , LDIAG, LJDIK, 
     .                 LJDIK2, NDDL   , K_DIAG, K_LT , IADK , 
     .                 JDIK  , IKC    , NMS   , INLOC, IDDL )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER EIGIPM(*), EIGIBUF(*),NDOF(*), LDIAG, LJDIK, LJDIK2, 
     .        NDDL, IADK(*), JDIK(*), IKC(*), NMS, INLOC(*), IDDL(*)
      my_real
     .        K_DIAG(*), K_LT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ICT, ICR, NN1, NN2, BDOF(6), I, II, J, JD, ITAG(NDDL),
     .        NBDOF, IND_IB(NDDL), ITAGN(NUMNOD), N, ITAG2(NDDL)
C
      ICT=EIGIPM(3)
      ICR=EIGIPM(4)
      NN1=EIGIPM(10)
      NN2=EIGIPM(11)
      DO I=1,6
         BDOF(I)=0
      ENDDO
      IF (ICT.EQ.1) THEN
         BDOF(3)=1
         NBDOF=1
      ELSEIF (ICT.EQ.2) THEN
         BDOF(2)=1
         NBDOF=1
      ELSEIF (ICT.EQ.3) THEN
         BDOF(2)=1
         BDOF(3)=1
         NBDOF=2
      ELSEIF (ICT.EQ.4) THEN
         BDOF(1)=1
         NBDOF=1
      ELSEIF (ICT.EQ.5) THEN
         BDOF(1)=1
         BDOF(3)=1
         NBDOF=2
      ELSEIF (ICT.EQ.6) THEN
         BDOF(1)=1
         BDOF(2)=1
         NBDOF=2
      ELSEIF (ICT.EQ.7) THEN
         BDOF(1)=1
         BDOF(2)=1
         BDOF(3)=1
         NBDOF=3
      ENDIF    
      IF (ICR.EQ.1) THEN
         BDOF(6)=1
         NBDOF=NBDOF+1
      ELSEIF (ICR.EQ.2) THEN
         BDOF(5)=1
         NBDOF=NBDOF+1
      ELSEIF (ICR.EQ.3) THEN
         BDOF(5)=1
         BDOF(6)=1
         NBDOF=NBDOF+2
      ELSEIF (ICR.EQ.4) THEN
         BDOF(4)=1
         NBDOF=NBDOF+1
      ELSEIF (ICR.EQ.5) THEN
         BDOF(4)=1
         BDOF(6)=1
         NBDOF=NBDOF+2
      ELSEIF (ICR.EQ.6) THEN
         BDOF(4)=1
         BDOF(5)=1
         NBDOF=NBDOF+2
      ELSEIF (ICR.EQ.7) THEN
         BDOF(4)=1
         BDOF(5)=1
         BDOF(6)=1
         NBDOF=NBDOF+3
      ENDIF
C
      DO I=1,NUMNOD
         ITAGN(I)=0
      ENDDO
      DO I=1,NN1
         II=EIGIBUF(I)
         ITAGN(II)=1
      ENDDO
      DO I=1,NN2
         II=EIGIBUF(NN1+I)
         ITAGN(II)=2
      ENDDO
C
      DO I=1,NDDL
         ITAG(I)=0
      ENDDO
      NMS=0
      DO I=1,NUMNOD
         N=INLOC(I)
         II=IDDL(N)
         IF (ITAGN(N).EQ.1) THEN
            DO J=1,NDOF(N)
               ITAG(II+J)=1
            ENDDO
         ELSEIF (ITAGN(N).EQ.2) THEN
            DO J=1,NDOF(N)
               IF (IKC(II+J).LT.1.AND.BDOF(J).EQ.1) NMS=NMS+1
               ITAG(II+J)=BDOF(J)+1
            ENDDO
         ENDIF
         DO J=1,NDOF(N)
            IF (IKC(II+J).GE.1) ITAG(II+J)=-1
         ENDDO
      ENDDO
C Elimination des ddls bloques dans la liste
      DO I=1,NDDL
         ITAG2(I)=ITAG(I)
         ITAG(I)=0
      ENDDO
      II=0
      DO I=1,NDDL
         IF (ITAG2(I).GE.0) THEN
            II=II+1
            ITAG(II)=ITAG2(I)
         ENDIF
      ENDDO
C
      DO I=1,NDDL
         IND_IB(I)=0
      ENDDO
      LDIAG=0
      LJDIK=0
      LJDIK2=0
      DO I=1,NDDL
         IF (ITAG(I).GE.1) THEN
            LDIAG=LDIAG+1
            DO J=IADK(I),IADK(I+1)-1
               JD=JDIK(J)
               IF (ITAG(JD).GE.1) THEN
                  LJDIK=LJDIK+1
                  IF (ITAG(JD).EQ.2.AND.ITAG(I).EQ.1) 
     .                              IND_IB(I)=IND_IB(I)+1
                  IF (ITAG(JD).EQ.1.AND.ITAG(I).EQ.2) 
     .                              IND_IB(JD)=IND_IB(JD)+1
               ENDIF
            ENDDO
         ENDIF
      ENDDO
      DO I=1,NDDL
         LJDIK2=LJDIK2+IND_IB(I)
      ENDDO
C
      RETURN
      END   
#endif
