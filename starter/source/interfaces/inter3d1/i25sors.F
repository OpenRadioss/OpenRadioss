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
Chd|  I25SORS                       source/interfaces/inter3d1/i25sors.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I25SORS(NSN  ,NRTS, ITAB, ILEV  ,IPARI, 
     .                  INTBUF_TAB)
C
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NRTS, ITAB(*), ILEV, IPARI(NPARI)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C     REAL ou REAL*8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J, N, M, K, NISUB, NISUBS
      INTEGER IERROR,WORK(70000)
      INTEGER, DIMENSION(:),ALLOCATABLE :: ITRI, INDEX, LISUBS, INFLG_SUBS, ADDSUBS
      INTEGER, DIMENSION(:,:),ALLOCATABLE :: ISAV
C     REAL ou REAL*8
C-----------------------------------------------
      ALLOCATE (ITRI(NSN),ISAV(3,NSN),INDEX(2*NSN),STAT=IERROR)
      IF (IERROR /= 0) 
     .  CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                     MSGTYPE=MSGERROR,
     .               C1='SORTING SECONDARY NODES IN INTERFACE TYPE25')
C--------------------------------------------------------------
C     Tri nds SECONDARYs vs ID
C--------------------------------------------------------------
C
       DO N=1,NSN
         ITRI(N)  =ITAB(INTBUF_TAB%NSV(N))
         ISAV(1,N)=INTBUF_TAB%NSV(N)
         IF(ILEV==2) ISAV(2,N)=INTBUF_TAB%NBINFLG(N)
       END DO
C
       NISUB = IPARI(36)
       NISUBS =IPARI(37)
       IF(NISUB > 0)THEN
         ALLOCATE (LISUBS(NISUBS),INFLG_SUBS(NISUBS),ADDSUBS(NSN+1),STAT=IERROR)
         IF (IERROR /= 0) 
     .     CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                     MSGTYPE=MSGERROR,
     .                     C1='SORTING SECONDARY NODES IN INTERFACE TYPE25')
         ADDSUBS(1:NSN+1)    =INTBUF_TAB%ADDSUBS(1:NSN+1)
         LISUBS(1:NISUBS)    =INTBUF_TAB%LISUBS(1:NISUBS)
         INFLG_SUBS(1:NISUBS)=INTBUF_TAB%INFLG_SUBS(1:NISUBS)
       END IF
C
       CALL MY_ORDERS(0,WORK,ITRI,INDEX,NSN,1)
C
       DO N=1,NSN
         INTBUF_TAB%NSV(N)    =ISAV(1,INDEX(N))
         IF(ILEV==2) INTBUF_TAB%NBINFLG(N)=ISAV(2,INDEX(N))
       END DO
C
       IF(NISUB > 0)THEN
         INTBUF_TAB%ADDSUBS(1)=1         
         DO N=1,NSN
           M=INDEX(N)
           INTBUF_TAB%ADDSUBS(N+1)= INTBUF_TAB%ADDSUBS(N)
     .                            + ADDSUBS(M+1) - ADDSUBS(M)
           DO K=ADDSUBS(M),ADDSUBS(M+1)-1
             INTBUF_TAB%LISUBS(INTBUF_TAB%ADDSUBS(N)+K-ADDSUBS(M))    =LISUBS(K)
             INTBUF_TAB%INFLG_SUBS(INTBUF_TAB%ADDSUBS(N)+K-ADDSUBS(M))=INFLG_SUBS(K)
           END DO
         END DO
       END IF
C
       DO N=1,NRTS
         DO J=1,4
           M = INTBUF_TAB%IRECTS(4*(N-1)+J)
           IF(M/=0) INTBUF_TAB%IRECTS(4*(N-1)+J)=ISAV(1,INDEX(M))
         END DO
       END DO
C
      DEALLOCATE (ITRI,ISAV,INDEX)
      IF(NISUB > 0)DEALLOCATE (LISUBS,INFLG_SUBS,ADDSUBS)
C--------------------------------------------------------------
      RETURN
      END
