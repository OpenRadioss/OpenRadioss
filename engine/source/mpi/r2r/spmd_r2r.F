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
Chd|  SPMD_R2R_RGET3                source/mpi/r2r/spmd_r2r.F     
Chd|-- called by -----------
Chd|        INIT_LINK_SPMD                source/coupling/rad2rad/r2r_init.F
Chd|        SEND_DATA_SPMD                source/coupling/rad2rad/r2r_exchange.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_R2R_RGET3(
     1    X      ,NNG   ,GRNOD, DD_R2R, WEIGHT, BUFR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNG, GRNOD(*), DD_R2R(*), WEIGHT(*)
      my_real
     .       BUFR(3,*), X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, P, N, L, IERROR, MSGOFF,LOC_PROC, MSGTYP, BUFSIZ,
     .        STATUS(MPI_STATUS_SIZE)
      DATA  MSGOFF/5000/
C-----------------------------------------------
C
      LOC_PROC = ISPMD+1
      L = 0
      DO I = 1, NNG
        N=GRNOD(I)
        IF(WEIGHT(N)==1)THEN
          L = L + 1
          BUFR(1,L) = X(1,N)
          BUFR(2,L) = X(2,N)
          BUFR(3,L) = X(3,N)
        END IF
      END DO
      IF(LOC_PROC==1) THEN
        DO P = 2, NSPMD
          IF(DD_R2R(P)>0)THEN
            BUFSIZ = 3*DD_R2R(P)
            MSGTYP = MSGOFF 
            CALL MPI_RECV(
     S        BUFR(1,L+1),BUFSIZ,REAL,IT_SPMD(P),MSGTYP,
     G        MPI_COMM_WORLD,STATUS,IERROR)
            L = L + DD_R2R(P)
          END IF
        END DO
      ELSEIF(L>0)THEN
        MSGTYP = MSGOFF
        CALL MPI_SEND(
     S     BUFR,L*3,REAL,IT_SPMD(1),MSGTYP,
     G     MPI_COMM_WORLD,IERROR)
      END IF
C
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_R2R_RGET3_DP             source/mpi/r2r/spmd_r2r.F     
Chd|-- called by -----------
Chd|        SEND_DATA_SPMD                source/coupling/rad2rad/r2r_exchange.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_R2R_RGET3_DP(
     1    X      ,NNG   ,GRNOD, DD_R2R, WEIGHT, BUFR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNG, GRNOD(*), DD_R2R(*), WEIGHT(*)
      DOUBLE PRECISION
     .       BUFR(3,*), X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, P, N, L, IERROR, MSGOFF,LOC_PROC, MSGTYP, BUFSIZ,
     .        STATUS(MPI_STATUS_SIZE)
      DATA  MSGOFF/5001/
C-----------------------------------------------
C
      LOC_PROC = ISPMD+1
      L = 0
      DO I = 1, NNG
        N=GRNOD(I)
        IF(WEIGHT(N)==1)THEN
          L = L + 1
          BUFR(1,L) = X(1,N)
          BUFR(2,L) = X(2,N)
          BUFR(3,L) = X(3,N)
        END IF
      END DO
      IF(LOC_PROC==1) THEN
        DO P = 2, NSPMD
          IF(DD_R2R(P)>0)THEN
            BUFSIZ = 3*DD_R2R(P)
            MSGTYP = MSGOFF
            CALL MPI_RECV(
     S        BUFR(1,L+1),BUFSIZ,MPI_DOUBLE_PRECISION,IT_SPMD(P),
     G        MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
            L = L + DD_R2R(P)
          END IF
        END DO
      ELSEIF(L>0)THEN
        MSGTYP = MSGOFF
        CALL MPI_SEND(
     S     BUFR,L*3,MPI_DOUBLE_PRECISION,IT_SPMD(1),MSGTYP,
     G     MPI_COMM_WORLD,IERROR)
      END IF
C
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_R2R_RGET                 source/mpi/r2r/spmd_r2r.F     
Chd|-- called by -----------
Chd|        SEND_DATA_SPMD                source/coupling/rad2rad/r2r_exchange.F
Chd|        SEND_MASS_RBY_SPMD            source/coupling/rad2rad/r2r_init.F
Chd|        SEND_MASS_SPMD                source/coupling/rad2rad/r2r_init.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_R2R_RGET(
     1    M   ,NNG   ,GRNOD, DD_R2R, WEIGHT, BUFR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNG, GRNOD(*), DD_R2R(*), WEIGHT(*)
      my_real
     .       BUFR(*), M(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, P, N, L, IERROR, MSGOFF,LOC_PROC, MSGTYP, BUFSIZ,
     .        STATUS(MPI_STATUS_SIZE)
      DATA  MSGOFF/5002/
C-----------------------------------------------
C
      LOC_PROC = ISPMD+1
      L = 0
      DO I = 1, NNG
        N=GRNOD(I)
        IF(WEIGHT(N)==1)THEN
          L = L + 1
          BUFR(L) = M(N)
        END IF
      END DO
      IF(LOC_PROC==1) THEN
        DO P = 2, NSPMD
          IF(DD_R2R(P)>0)THEN
            BUFSIZ = DD_R2R(P)
            MSGTYP = MSGOFF 
            CALL MPI_RECV(
     S        BUFR(L+1),BUFSIZ,REAL,IT_SPMD(P),MSGTYP,
     G        MPI_COMM_WORLD,STATUS,IERROR)
            L = L + DD_R2R(P)
          END IF
        END DO
      ELSEIF(L>0)THEN
        MSGTYP = MSGOFF 
        CALL MPI_SEND(
     S     BUFR,L,REAL,IT_SPMD(1),MSGTYP,
     G     MPI_COMM_WORLD,IERROR)
      END IF
C
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_R2R_RBY                  source/mpi/r2r/spmd_r2r.F     
Chd|-- called by -----------
Chd|        SEND_DATA_SPMD                source/coupling/rad2rad/r2r_exchange.F
Chd|-- calls ---------------
Chd|        RAD2R_MOD                     share/modules/rad2r.F         
Chd|====================================================================
      SUBROUTINE SPMD_R2R_RBY(
     1    RBY   ,NNG   ,GRNOD, DD_R2R, WEIGHT, IEX, BUFR)
C----6----------------------------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE RAD2R_MOD     
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----C----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNG, GRNOD(*), DD_R2R(*), WEIGHT(*), IEX
      my_real
     .       BUFR(9,*), RBY(NRBY,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, P, N, L, IERROR, MSGOFF,LOC_PROC, MSGTYP, BUFSIZ,
     .        STATUS(MPI_STATUS_SIZE),NOD
      DATA  MSGOFF/5003/
C-----------------------------------------------
C
      LOC_PROC = ISPMD+1
      L = 0
      DO I = 1, NNG
        NOD=GRNOD(I)
        IF(WEIGHT(NOD)==1)THEN
          N=TAG_RBY(ADD_RBY(IEX)+I)	
          L = L + 1
	  DO P = 1, 9
            BUFR(P,L) = RBY(16+P,N)
	  END DO  
        END IF
      END DO
      IF(LOC_PROC==1) THEN
        DO P = 2, NSPMD
          IF(DD_R2R(P)>0)THEN
            BUFSIZ = 9*DD_R2R(P)
            MSGTYP = MSGOFF 
            CALL MPI_RECV(
     S        BUFR(1,L+1),BUFSIZ,REAL,IT_SPMD(P),MSGTYP,
     G        MPI_COMM_WORLD,STATUS,IERROR)
            L = L + DD_R2R(P)
          END IF
        END DO
      ELSEIF(L>0)THEN
        MSGTYP = MSGOFF 
        CALL MPI_SEND(
     S     BUFR,L*9,REAL,IT_SPMD(1),MSGTYP,
     G     MPI_COMM_WORLD,IERROR)
      END IF
C
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_R2R_IDEF                 source/mpi/r2r/spmd_r2r.F     
Chd|-- called by -----------
Chd|        INIT_LINK_SPMD                source/coupling/rad2rad/r2r_init.F
Chd|-- calls ---------------
Chd|        RAD2R_MOD                     share/modules/rad2r.F         
Chd|====================================================================
      SUBROUTINE SPMD_R2R_IDEF(NNG,GRNOD,WEIGHT,IEX,TLEL,TLELN,TCNEL,TCNELDB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE RAD2R_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNG, GRNOD(*), WEIGHT(*),IEX,TLEL,TLELN,TCNEL,TCNELDB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, P, N, L(6), IERROR, MSGOFF,LOC_PROC, MSGTYP, BUFSIZ,
     .        STATUS(MPI_STATUS_SIZE),NB(6),OFFSET1
      DATA  MSGOFF/5004/
C-----------------------------------------------
C
C --- Recolte des infos sur les procs : nb d'elements a envoyer, noeuds doubles, etc ...
C
      LOC_PROC = ISPMD+1
      L(1) = 0
      L(2) = 0
      L(3) = 0
      L(4) = 0
      L(5) = 0
      L(6) = 0
                        
      DO I = 1, NNG
        N=GRNOD(I)
        IF(WEIGHT(N)==0)THEN
          L(1) = L(1) + 1
        END IF
      END DO
          L(2) = TLEL
	  L(3) = NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR+NUMELTG
	  L(4) = TLELN
          L(5) = TCNEL
          L(6) = TCNELDB
	  	  
      IF(LOC_PROC==1) THEN
            DBN(IEX,1)=L(1)	    
	    DBNO(IEX)=L(1)
	    NBEL(IEX,1) = L(2)
	    NBELT_R2R(IEX) = L(2)
	    NBELTN_R2R(IEX) = L(4)
	    OFFSET(1)=0
	    OFFSET1 = L(3)
	    TBCNEL(IEX,1) = L(5)
	    TCNELT(IEX) = L(5)
	    TBCNELDB(IEX,1) = L(6)
	    TCNELTDB(IEX) = L(6)
	    	    
        DO P = 2, NSPMD
            MSGTYP = MSGOFF 
            CALL MPI_RECV(
     S        NB,6,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     G        MPI_COMM_WORLD,STATUS,IERROR)
     
            DBN(IEX,P) = NB(1)
	    NBEL(IEX,P) = NB(2)
	    NBELN(IEX,P) = NB(4)
            DBNO(IEX) = DBNO(IEX) + DBN(IEX,P)
	    NBELT_R2R(IEX) = NBELT_R2R(IEX) + NBEL(IEX,P)
	    NBELTN_R2R(IEX) = NBELTN_R2R(IEX) + NBELN(IEX,P)	    
	    OFFSET(P)= OFFSET1
	    OFFSET1 = OFFSET1 + NB(3)
	    TBCNEL(IEX,P) = NB(5)
	    TCNELT(IEX) = TCNELT(IEX) + NB(5)
	    TBCNELDB(IEX,P) = NB(6)
	    TCNELTDB(IEX) = TCNELTDB(IEX)+NB(6)	    
        END DO
      ELSE
        MSGTYP = MSGOFF 
        CALL MPI_SEND(
     S     L,6,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     G     MPI_COMM_WORLD,IERROR)    
      END IF         
C
#endif
      RETURN
      END      
Chd|====================================================================
Chd|  SPMD_R2R_IGET                 source/mpi/r2r/spmd_r2r.F     
Chd|-- called by -----------
Chd|        GET_MASS_RBY_SPMD             source/coupling/rad2rad/r2r_init.F
Chd|        INIT_LINK_SPMD                source/coupling/rad2rad/r2r_init.F
Chd|        SEND_MASS_RBY_SPMD            source/coupling/rad2rad/r2r_init.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_R2R_IGET(
     1    ITAB   ,NNG   ,GRNOD, DD_R2R, WEIGHT, IBUF,FLAG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNG,GRNOD(*),DD_R2R(*),WEIGHT(*),IBUF(*),ITAB(*),FLAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, P, N, L, IERROR, MSGOFF,LOC_PROC, MSGTYP, BUFSIZ,
     .        STATUS(MPI_STATUS_SIZE)
      DATA  MSGOFF/5005/
C-----------------------------------------------
C
      LOC_PROC = ISPMD+1
      L = 0
      
      DO I = 1, NNG
        N=GRNOD(I)
        IF(WEIGHT(N)==1)THEN
          L = L + 1
	  IF (FLAG==1) THEN
          IBUF(L) = ITAB(N)
	  ELSE
          IBUF(L) = ITAB(I)	  
	  ENDIF
        END IF
      END DO
      
      IF(LOC_PROC==1) THEN
        DO P = 2, NSPMD
          IF(DD_R2R(P)>0)THEN
            BUFSIZ = DD_R2R(P)
            MSGTYP = MSGOFF 
            CALL MPI_RECV(
     S        IBUF(L+1),BUFSIZ,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     G        MPI_COMM_WORLD,STATUS,IERROR)
            L = L + DD_R2R(P)
          END IF
        END DO
      ELSEIF(L>0)THEN
        MSGTYP = MSGOFF 
        CALL MPI_SEND(
     S     IBUF,L,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     G     MPI_COMM_WORLD,IERROR)
      END IF
C
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_R2R_IGET2                source/mpi/r2r/spmd_r2r.F     
Chd|-- called by -----------
Chd|        INIT_LINK_SPMD                source/coupling/rad2rad/r2r_init.F
Chd|-- calls ---------------
Chd|        RAD2R_MOD                     share/modules/rad2r.F         
Chd|====================================================================
      SUBROUTINE SPMD_R2R_IGET2(
     1    ITAB   ,NNG   ,IEX, IBUF, FLAG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE RAD2R_MOD   
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----C-----------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNG, IEX,IBUF(*),ITAB(*),FLAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, P, N, L, IERROR, MSGOFF,LOC_PROC, MSGTYP, BUFSIZ,
     .        STATUS(MPI_STATUS_SIZE)
      DATA  MSGOFF/5006/
C-----------------------------------------------
C
      LOC_PROC = ISPMD+1
      L = 0      

      IF(LOC_PROC==1) THEN     
        DO I = 1, NNG
         IBUF(I) = ITAB(I)
	 L = L+1
        END DO
	       
        DO P = 2, NSPMD
		  
	    IF (FLAG<2) THEN
            BUFSIZ = NBEL(IEX,P)
	    ELSEIF (FLAG==2) THEN
            BUFSIZ = NBELN(IEX,P)
	    ELSEIF (FLAG==3) THEN
	    BUFSIZ = TBCNEL(IEX,P)
	    ELSEIF (FLAG==4) THEN
	    BUFSIZ = TBCNELDB(IEX,P)	    	    
	    ENDIF
	    
	    IF(BUFSIZ>0)THEN
	    
            MSGTYP = MSGOFF 
            CALL MPI_RECV(
     S        IBUF(L+1),BUFSIZ,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     G        MPI_COMM_WORLD,STATUS,IERROR)
  
C--------------OFFSET de la numerotation des elements----     
            IF ((FLAG==1).OR.(FLAG>2)) THEN
	      DO I=1,BUFSIZ
	        IBUF(L+I)=IBUF(L+I)+OFFSET(P)
	      END DO
	    ENDIF
C--------------------------------------------------------	    
	    
	    IF (FLAG<2) THEN     
            L = L + NBEL(IEX,P)
	    ELSEIF (FLAG==2) THEN
            L = L + NBELN(IEX,P)
	    ELSEIF (FLAG==3) THEN
	    L = L + TBCNEL(IEX,P)
	    ELSEIF (FLAG==4) THEN
	    L = L + TBCNELDB(IEX,P)	    	    	    
	    ENDIF
	    	    
          END IF
        END DO
      ELSEIF(NNG>0)THEN
        MSGTYP = MSGOFF
        CALL MPI_SEND(
     S     ITAB,NNG,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     G     MPI_COMM_WORLD,IERROR)
      END IF
C          
C
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_R2R_IGET4                source/mpi/r2r/spmd_r2r.F     
Chd|-- called by -----------
Chd|        INIT_LINK_SPMD                source/coupling/rad2rad/r2r_init.F
Chd|-- calls ---------------
Chd|        RAD2R_MOD                     share/modules/rad2r.F         
Chd|====================================================================
      SUBROUTINE SPMD_R2R_IGET4(
     1    ITAB,NNG,GRNOD,DD_R2R,WEIGHT,IBUF,IEX,DBNBUF,
     2    DDBUF,FLAG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE RAD2R_MOD    
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----C------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNG,GRNOD(*),DD_R2R(*),WEIGHT(*),IBUF(*),ITAB(*),
     .        IEX,FLAG,DBNBUF(*),DDBUF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, P, N, L, IERROR, MSGOFF,LOC_PROC, MSGTYP, BUFSIZ,
     .        STATUS(MPI_STATUS_SIZE)
      DATA  MSGOFF/5007/
C-----------------------------------------------
C
      LOC_PROC = ISPMD+1
      L = 0
      DO I = 1, NNG
        N=GRNOD(I)
        IF(WEIGHT(N)==0)THEN
          L = L + 1
	  IF (FLAG==1) THEN
           IBUF(L) = ITAB(N)	   
	  ELSE
           IBUF(L) = ITAB(I)	  
	  ENDIF
        END IF
      END DO		
      
      IF(LOC_PROC==1) THEN
	  DBNBUF(1)=DBN(IEX,1)
	  DDBUF(1)=DD_R2R(1)       	        
        DO P = 2, NSPMD
	  DBNBUF(P)=DBN(IEX,P)
	  DDBUF(P)=DD_R2R(P)
	    	  
          IF(DBN(IEX,P)>0)THEN
            BUFSIZ = DBN(IEX,P)
            MSGTYP = MSGOFF
            CALL MPI_RECV(
     S        IBUF(L+1),BUFSIZ,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     G        MPI_COMM_WORLD,STATUS,IERROR)
	    
	    L = L + DBN(IEX,P)
	    
          END IF
        END DO
      ELSEIF(L>0)THEN
        MSGTYP = MSGOFF 
        CALL MPI_SEND(
     S     IBUF,L,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     G     MPI_COMM_WORLD,IERROR)
      END IF
C
#endif
      RETURN
      END
      
Chd|====================================================================
Chd|  SPMD_R2R_SYNC                 source/mpi/r2r/spmd_r2r.F     
Chd|-- called by -----------
Chd|        R2R_INIT                      source/coupling/rad2rad/r2r_init.F
Chd|-- calls ---------------
Chd|        GET_NAME_C                    source/coupling/rad2rad/rad2rad_c.c
Chd|====================================================================
      SUBROUTINE SPMD_R2R_SYNC(ADDR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*35 ADDR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, P, N, IERROR, MSGOFF,LOC_PROC, MSGTYP,
     .        STATUS(MPI_STATUS_SIZE),BUFSIZ,BUFA,TOTO
      DATA  MSGOFF/5008/
C-----------------------------------------------
C
      LOC_PROC = ISPMD+1
      BUFSIZ=35
      IF(NSPMD>1) THEN      
       IF(LOC_PROC==1) THEN
        CALL GET_NAME_C(ADDR)
	ADDR=trim(ADDR)
	TOTO=len_trim(ADDR)     
        DO P = 2, NSPMD	  	
            MSGTYP = MSGOFF
            CALL MPI_SEND(
     S        ADDR,BUFSIZ,MPI_CHARACTER,IT_SPMD(P),MSGTYP,
     G        MPI_COMM_WORLD,IERROR)       
        END DO
       ELSE
        MSGTYP = MSGOFF
        CALL MPI_RECV(
     S     ADDR,BUFSIZ,MPI_CHARACTER,IT_SPMD(1),MSGTYP,
     G     MPI_COMM_WORLD,STATUS,IERROR)          
       END IF
      ENDIF
C
C
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_R2R_RSET                 source/mpi/r2r/spmd_r2r.F     
Chd|-- called by -----------
Chd|        GET_STIFF_SPMD                source/coupling/rad2rad/r2r_exchange.F
Chd|-- calls ---------------
Chd|        RAD2R_MOD                     share/modules/rad2r.F         
Chd|====================================================================
      SUBROUTINE SPMD_R2R_RSET(
     1    M      ,NNG     ,GRNOD, DD_R2R, WEIGHT,
     2    BUFR   ,IAD_ELEM,FR_ELEM, LRBUF,IEX )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE RAD2R_MOD    
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----C-----------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNG,LRBUF,IEX,
     .        GRNOD(*),DD_R2R(*),WEIGHT(*),IAD_ELEM(2,*),FR_ELEM(*)
      my_real
     .       BUFR(*), M(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, J, P, N, L, IERROR, MSGOFF, ISHIFT,
     .        LOC_PROC, MSGTYP, BUFSIZ, SIZ, INB, NB_NOD, NOD, LSEND,
     .        REQ_R(NSPMD), REQ_S(NSPMD),IAD_SEND(NSPMD+1),DBL,
     .        IAD_RECV(NSPMD+1), STATUS(MPI_STATUS_SIZE), ITAG(NUMNOD)
      my_real
     .        RBUF(LRBUF)
      DATA  MSGOFF/5009/
C-----------------------------------------------
C
      LOC_PROC = ISPMD+1
      IF(LOC_PROC==1) THEN
        L = DD_R2R(1)+DBN(IEX,1)	
        DO P = 2, NSPMD
          IF((DD_R2R(P)+DBN(IEX,P))>0)THEN
            BUFSIZ = DD_R2R(P)+DBN(IEX,P)
            MSGTYP = MSGOFF
            CALL MPI_SEND(
     S        BUFR(L+1),BUFSIZ,REAL,IT_SPMD(P),MSGTYP,
     G        MPI_COMM_WORLD,IERROR)
            L = L + DD_R2R(P)+DBN(IEX,P)
          END IF
        END DO
      ELSEIF(NNG>0)THEN
        BUFSIZ = NNG
        MSGTYP = MSGOFF
        CALL MPI_RECV(
     S    BUFR,BUFSIZ,REAL,IT_SPMD(1),MSGTYP,
     G    MPI_COMM_WORLD,STATUS,IERROR)
      END IF
      DO I = 1, NUMNOD
        ITAG(I) = 0
      END DO
      L = 0
      DBL = DD_R2R(LOC_PROC)     

      DO I = 1, NNG
        N=GRNOD(I)
        IF(WEIGHT(N)==1)THEN
          L = L + 1
          M(N) = BUFR(L)
	ELSE
          DBL = DBL + 1
          M(N) = BUFR(DBL)	  	 
        ENDIF	  
      END DO
C
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_R2R_RSET4                source/mpi/r2r/spmd_r2r.F     
Chd|-- called by -----------
Chd|        GET_MASS_SPMD                 source/coupling/rad2rad/r2r_init.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_R2R_RSET4(
     1    M      ,NNG     ,GRNOD, DD_R2R, WEIGHT,
     2    BUFR   ,IAD_ELEM,FR_ELEM, LRBUF )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNG,LRBUF,
     .        GRNOD(*),DD_R2R(*),WEIGHT(*),IAD_ELEM(2,*),FR_ELEM(*)
      my_real
     .       BUFR(*), M(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, J, P, N, L, IERROR, MSGOFF, ISHIFT,
     .        LOC_PROC, MSGTYP, BUFSIZ, SIZ, INB, NB_NOD, NOD, LSEND,
     .        REQ_R(NSPMD), REQ_S(NSPMD),IAD_SEND(NSPMD+1),
     .        IAD_RECV(NSPMD+1), STATUS(MPI_STATUS_SIZE), ITAG(NUMNOD)
      my_real
     .        RBUF(LRBUF)
      DATA  MSGOFF/5010/
C-----------------------------------------------
C
      LOC_PROC = ISPMD+1
      IF(LOC_PROC==1) THEN
        L = DD_R2R(1)		
        DO P = 2, NSPMD	
          IF((DD_R2R(P))>0)THEN
            BUFSIZ = DD_R2R(P)
            MSGTYP = MSGOFF 
            CALL MPI_SEND(
     S        BUFR(L+1),BUFSIZ,REAL,IT_SPMD(P),MSGTYP,
     G        MPI_COMM_WORLD,IERROR)
            L = L + DD_R2R(P)
          END IF
        END DO
      ELSEIF(DD_R2R(LOC_PROC)>0)THEN
        BUFSIZ = DD_R2R(LOC_PROC)
        MSGTYP = MSGOFF 
        CALL MPI_RECV(
     S    BUFR,BUFSIZ,REAL,IT_SPMD(1),MSGTYP,
     G    MPI_COMM_WORLD,STATUS,IERROR)
      END IF
      DO I = 1, NUMNOD
        ITAG(I) = 0
      END DO
      L = 0   
      DO I = 1, NNG
        N=GRNOD(I)
        IF(WEIGHT(N)==1)THEN
          L = L + 1
          M(N) = BUFR(L)
          ITAG(N) = 1
        END IF
      END DO

C
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_R2R_RSET3                source/mpi/r2r/spmd_r2r.F     
Chd|-- called by -----------
Chd|        GET_DISPL_SPMD                source/coupling/rad2rad/r2r_getdata.F
Chd|        GET_FORCE_SPMD                source/coupling/rad2rad/r2r_getdata.F
Chd|-- calls ---------------
Chd|        RAD2R_MOD                     share/modules/rad2r.F         
Chd|====================================================================
      SUBROUTINE SPMD_R2R_RSET3(
     1    A      ,NNG     ,GRNOD, DD_R2R, WEIGHT,
     2    BUFR   ,IAD_ELEM,FR_ELEM, LRBUF,IEX )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE RAD2R_MOD    
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----C-----------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNG,LRBUF,IEX,
     .        GRNOD(*),DD_R2R(*),WEIGHT(*),IAD_ELEM(2,*),FR_ELEM(*)
      my_real
     .       BUFR(3,*), A(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, J, P, N, L, IERROR, MSGOFF, ISHIFT,DBL,
     .        LOC_PROC, MSGTYP, BUFSIZ, SIZ, INB, NB_NOD, NOD, LSEND,
     .        REQ_R(NSPMD), REQ_S(NSPMD),IAD_SEND(NSPMD+1),
     .        IAD_RECV(NSPMD+1), STATUS(MPI_STATUS_SIZE), ITAG(NUMNOD)
      my_real
     .        RBUF(LRBUF)
      DATA  MSGOFF/5011/
C-----------------------------------------------
C
      LOC_PROC = ISPMD+1
      IF(LOC_PROC==1) THEN
        L = DD_R2R(1)+DBN(IEX,1)
        DO P = 2, NSPMD
          IF((DD_R2R(P)+DBN(IEX,P))>0)THEN
            BUFSIZ = (DD_R2R(P)+DBN(IEX,P))*3
            MSGTYP = MSGOFF
            CALL MPI_SEND(
     S        BUFR(1,L+1),BUFSIZ,REAL,IT_SPMD(P),MSGTYP,
     G        MPI_COMM_WORLD,IERROR)
            L = L + DD_R2R(P)+DBN(IEX,P)
          END IF
        END DO
      ELSEIF(NNG>0)THEN
        BUFSIZ = NNG*3
        MSGTYP = MSGOFF 
        CALL MPI_RECV(
     S    BUFR,BUFSIZ,REAL,IT_SPMD(1),MSGTYP,
     G    MPI_COMM_WORLD,STATUS,IERROR)    
      END IF
      DO I = 1, NUMNOD
        ITAG(I) = 0
      END DO
      
      L = 0
      DBL = DD_R2R(LOC_PROC) 

      DO I = 1, NNG
        N=GRNOD(I)
        IF(WEIGHT(N)==1)THEN
          L = L + 1 
          A(1,N) = BUFR(1,L)
          A(2,N) = BUFR(2,L)
          A(3,N) = BUFR(3,L)  	  
	ELSE
          DBL = DBL + 1
          A(1,N) = BUFR(1,DBL)
          A(2,N) = BUFR(2,DBL)
          A(3,N) = BUFR(3,DBL)	  	  	 
        ENDIF	  
      END DO
C
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_R2R_RSET3B               source/mpi/r2r/spmd_r2r.F     
Chd|-- called by -----------
Chd|        GET_FORCE_SPMD                source/coupling/rad2rad/r2r_getdata.F
Chd|-- calls ---------------
Chd|        RAD2R_MOD                     share/modules/rad2r.F         
Chd|====================================================================
      SUBROUTINE SPMD_R2R_RSET3B(
     1    A      ,NNG     ,GRNOD, DD_R2R,WEIGHT,
     2    BUFR   ,IAD_ELEM,FR_ELEM, LRBUF ,MS    ,
     3    V      ,WF     ,WF2, IEX     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE RAD2R_MOD    
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----C-----------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NNG,LRBUF,IEX,
     .       GRNOD(*),DD_R2R(*),WEIGHT(*),IAD_ELEM(2,*),FR_ELEM(*)
      my_real
     .       BUFR(3,*), A(3,*), MS(*), V(3,*), WF, WF2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, J, P, N, L, IERROR, MSGOFF, MSGOFF2, ISHIFT,DBL,
     .        LOC_PROC, MSGTYP, BUFSIZ, SIZ, INB, NB_NOD, NOD, LSEND,
     .        REQ_R(NSPMD), REQ_S(NSPMD),IAD_SEND(NSPMD+1),
     .        IAD_RECV(NSPMD+1), STATUS(MPI_STATUS_SIZE), ITAG(NUMNOD)    
      my_real
     .        DF1, DF2, DF3, RBUF(LRBUF), WFB
      DATA  MSGOFF/5012/
      DATA  MSGOFF2/5013/
C-----------------------------------------------
C
      WF=0
      WF2=0

      LOC_PROC = ISPMD+1
      IF(LOC_PROC==1) THEN
        L = DD_R2R(1)+DBN(IEX,1)
        DO P = 2, NSPMD
          IF((DD_R2R(P)+DBN(IEX,P))>0)THEN
            BUFSIZ = (DD_R2R(P)+DBN(IEX,P))*3
            MSGTYP = MSGOFF
            CALL MPI_SEND(
     S        BUFR(1,L+1),BUFSIZ,REAL,IT_SPMD(P),MSGTYP,
     G        MPI_COMM_WORLD,IERROR)
            L = L + DD_R2R(P)+DBN(IEX,P)
          END IF
        END DO
      ELSEIF(NNG>0)THEN
        BUFSIZ = NNG*3
        MSGTYP = MSGOFF 
        CALL MPI_RECV(
     S    BUFR,BUFSIZ,REAL,IT_SPMD(1),MSGTYP,
     G    MPI_COMM_WORLD,STATUS,IERROR)
      END IF
      
      L = 0
      DBL = DD_R2R(LOC_PROC)     

      DO I = 1, NNG
        N=GRNOD(I)
        IF(WEIGHT(N)==1)THEN
          L = L + 1 
          DF1 = MS(N)*BUFR(1,L)-A(1,N)
          DF2 = MS(N)*BUFR(2,L)-A(2,N)
          DF3 = MS(N)*BUFR(3,L)-A(3,N)
          A(1,N) = MS(N)*BUFR(1,L)
          A(2,N) = MS(N)*BUFR(2,L)
          A(3,N) = MS(N)*BUFR(3,L)
C calcul du travail localement
          WF = WF + (DF1*V(1,N)+DF2*V(2,N)+DF3*V(3,N))/TWO
          WF2= WF2+ (DF1*A(1,N)+DF2*A(2,N)+DF3*A(3,N))/(TWO*MS(N))	  	  
	ELSE
          DBL = DBL + 1
          DF1 = MS(N)*BUFR(1,DBL)-A(1,N)
          DF2 = MS(N)*BUFR(2,DBL)-A(2,N)
          DF3 = MS(N)*BUFR(3,DBL)-A(3,N)
          A(1,N) = MS(N)*BUFR(1,DBL)
          A(2,N) = MS(N)*BUFR(2,DBL)
          A(3,N) = MS(N)*BUFR(3,DBL)	  	 
        ENDIF	  
      END DO
      
C Sommation sur les procs de WF
      IF(LOC_PROC==1) THEN
        DO P = 2, NSPMD
            MSGTYP = MSGOFF 
            CALL MPI_RECV(
     S        WFB,1,REAL,IT_SPMD(P),MSGTYP,
     G        MPI_COMM_WORLD,STATUS,IERROR)
            WF = WF+WFB
        END DO
      ELSE
        MSGTYP = MSGOFF 
        CALL MPI_SEND(
     S     WF,1,REAL,IT_SPMD(1),MSGTYP,
     G     MPI_COMM_WORLD,IERROR)     
      END IF
      
C Sommation sur les procs de WF2

      IF(LOC_PROC==1) THEN
        DO P = 2, NSPMD
            MSGTYP = MSGOFF
            CALL MPI_RECV(
     S        WFB,1,REAL,IT_SPMD(P),MSGTYP,
     G        MPI_COMM_WORLD,STATUS,IERROR)
            WF2 = WF2+WFB
        END DO	
      ELSE
        MSGTYP = MSGOFF 
        CALL MPI_SEND(
     S     WF2,1,REAL,IT_SPMD(1),MSGTYP,
     G     MPI_COMM_WORLD,IERROR)
      END IF
           
#endif
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_EXCH_R2R                 source/mpi/r2r/spmd_r2r.F     
Chd|-- called by -----------
Chd|        R2R_EXCHANGE                  source/coupling/rad2rad/r2r_exchange.F
Chd|        R2R_INIT                      source/coupling/rad2rad/r2r_init.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_R2R(
     1   A        ,AR     ,STIFN,STIFR ,MS  ,
     2   IAD_ELEM ,FR_ELEM, SIZE,
     3   LENR     ,DD_R2R,DD_R2R_ELEM,FLAG)
C--------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD_ELEM(2,*),FR_ELEM(*), SIZE, LENR,
     .        DD_R2R(NSPMD+1,SDD_R2R),DD_R2R_ELEM(*),FLAG 
      my_real
     .        A(3,*),AR(3,*),STIFN(*),STIFR(*),MS(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,I,NOD,LOC_PROC,IERROR,
     .        SIZ,J,K,L,NB_NOD,
     .        STATUS(MPI_STATUS_SIZE),
     .        IAD_SEND(NSPMD+1),IAD_RECV(NSPMD+1),
     .        REQ_R(NSPMD),REQ_S(NSPMD),OFFSET,MSGOFF
      my_real
     .        RBUF(SIZE*LENR ),
     .        SBUF(SIZE*LENR )
      DATA MSGOFF/5014/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      L = 1
      IAD_RECV(1) = 1
      
      DO I=1,NSPMD
        SIZ = SIZE*(DD_R2R(I+1,2)-DD_R2R(I,2))
        IF(SIZ/=0)THEN
          MSGTYP = MSGOFF
          CALL MPI_IRECV(
     S      RBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
          L = L + SIZ
        ENDIF
        IAD_RECV(I+1) = L
      END DO
      L = 1
      IAD_SEND(1) = 1
C      
      DO I=1,NSPMD
C preparation envoi  partie fixe (elem) a proc I
        IF(IRODDL/=0) THEN
#include      "vectorize.inc"
            DO J=DD_R2R(I,1),DD_R2R(I+1,1)-1
              NOD = DD_R2R_ELEM(J)
              IF (FLAG==2) THEN
                SBUF(L  ) =  A(1,NOD)
                SBUF(L+1) =  A(2,NOD)
                SBUF(L+2) =  A(3,NOD)
                SBUF(L+3) =  AR(1,NOD)
                SBUF(L+4) =  AR(2,NOD)
                SBUF(L+5) =  AR(3,NOD)
              ELSE                
                SBUF(L  ) =  STIFN(NOD)
                SBUF(L+1) =  STIFR(NOD)
              ENDIF  
              L = L + SIZE
            END DO
          ELSE
#include      "vectorize.inc"
            DO J=DD_R2R(I,1),DD_R2R(I+1,1)-1
              NOD = DD_R2R_ELEM(J)
              IF (FLAG==2) THEN
                SBUF(L  ) =  A(1,NOD)
                SBUF(L+1) =  A(2,NOD)
                SBUF(L+2) =  A(3,NOD)
              ELSE
                SBUF(L  ) =  STIFN(NOD)              
              ENDIF  
              L = L + SIZE
            END DO
        ENDIF
C
        IAD_SEND(I+1) = L
      ENDDO
C
C   echange messages
C
      DO I=1,NSPMD
C--------------------------------------------------------------------
C envoi a N+I mod P
Cel test si msg necessaire a envoyer a completer par test interface
       IF(DD_R2R(I+1,1)-DD_R2R(I,1)>0)THEN
          MSGTYP = MSGOFF
          SIZ = IAD_SEND(I+1)-IAD_SEND(I)
          L = IAD_SEND(I)
          CALL MPI_ISEND(
     S      SBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
       ENDIF
C--------------------------------------------------------------------
      ENDDO
C
C decompactage
C
      OFFSET = DD_R2R(NSPMD+1,1)-1
      DO I = 1, NSPMD
Cel test si msg necessaire a envoyer a completer par test interface
        NB_NOD = DD_R2R(I+1,2)-DD_R2R(I,2)
        IF(NB_NOD>0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
          L = IAD_RECV(I)

          IF(IRODDL/=0) THEN
#include        "vectorize.inc"
            DO J=DD_R2R(I,2),DD_R2R(I+1,2)-1
                NOD = DD_R2R_ELEM(OFFSET+J)
                IF (FLAG==2) THEN
                  A(1,NOD) = RBUF(L)
                  A(2,NOD) = RBUF(L+1)
                  A(3,NOD) = RBUF(L+2)
                  AR(1,NOD)= RBUF(L+3)
                  AR(2,NOD)= RBUF(L+4)
                  AR(3,NOD)= RBUF(L+5)
                ELSE  
                  STIFN(NOD)= RBUF(L)
                  STIFR(NOD)= RBUF(L+1)
                ENDIF                                 
                L = L + SIZE
            END DO
          ELSE
#include        "vectorize.inc"
             DO J=DD_R2R(I,2),DD_R2R(I+1,2)-1
                 NOD = DD_R2R_ELEM(OFFSET+J)
                 IF (FLAG==2) THEN                 
                   A(1,NOD) = RBUF(L)
                   A(2,NOD) = RBUF(L+1)
                   A(3,NOD) = RBUF(L+2)
                 ELSE
                   STIFN(NOD)= RBUF(L)                 
                 ENDIF                  
                 L = L + SIZE
             END DO
          ENDIF
C ---
        ENDIF
C
      END DO
C
Cel wait terminaison isend
C
      DO I = 1, NSPMD
        IF(DD_R2R(I+1,1)-DD_R2R(I,1)>0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF       
      ENDDO
C

#endif
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_EXCH_R2R_2               source/mpi/r2r/spmd_r2r.F     
Chd|-- called by -----------
Chd|        R2R_GETDATA                   source/coupling/rad2rad/r2r_getdata.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_R2R_2(
     1   A ,AR, V, VR     ,MS  , IN,
     2   IAD_ELEM ,FR_ELEM, SIZE, WF, WF2,
     3   LENR     ,DD_R2R,DD_R2R_ELEM,WEIGHT,FLAG)
C--------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD_ELEM(2,*),FR_ELEM(*), SIZE, LENR,
     .        DD_R2R(NSPMD+1,SDD_R2R),DD_R2R_ELEM(*),FLAG,
     .        WEIGHT(*) 
      my_real
     .        A(3,*),AR(3,*), V(3,*),VR(3,*),MS(*),IN(*),
     .        WF,WF2     
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,I,NOD,LOC_PROC,IERROR,
     .        SIZ,J,K,L,NB_NOD,
     .        STATUS(MPI_STATUS_SIZE),
     .        IAD_SEND(NSPMD+1),IAD_RECV(NSPMD+1),
     .        REQ_R(NSPMD),REQ_S(NSPMD),OFFSET,MSGOFF
      my_real
     .        RBUF(SIZE*LENR ),
     .        SBUF(SIZE*LENR ),DF1,DF2,DF3,DF4,DF5,DF6     
      DATA MSGOFF/5015/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      L = 1
      IAD_RECV(1) = 1
      
      DO I=1,NSPMD
        SIZ = SIZE*(DD_R2R(I+1,2)-DD_R2R(I,2))
        IF(SIZ/=0)THEN
          MSGTYP = MSGOFF
          CALL MPI_IRECV(
     S      RBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
          L = L + SIZ
        ENDIF
        IAD_RECV(I+1) = L
      END DO
      L = 1
      IAD_SEND(1) = 1
C      
      DO I=1,NSPMD
C preparation envoi  partie fixe (elem) a proc I
        IF(IRODDL/=0) THEN
#include      "vectorize.inc"
            DO J=DD_R2R(I,1),DD_R2R(I+1,1)-1
              NOD = DD_R2R_ELEM(J)
              SBUF(L  ) =  A(1,NOD)
              SBUF(L+1) =  A(2,NOD)
              SBUF(L+2) =  A(3,NOD)
              SBUF(L+3) =  AR(1,NOD)
              SBUF(L+4) =  AR(2,NOD)
              SBUF(L+5) =  AR(3,NOD)
              IF (FLAG==1) THEN
                SBUF(L+6) =  MS(NOD)
                SBUF(L+7) =  IN(NOD)              
              ENDIF 
              L = L + SIZE
            END DO
          ELSE
#include      "vectorize.inc"
            DO J=DD_R2R(I,1),DD_R2R(I+1,1)-1
              NOD = DD_R2R_ELEM(J)
              SBUF(L  ) =  A(1,NOD)
              SBUF(L+1) =  A(2,NOD)
              SBUF(L+2) =  A(3,NOD)
              IF (FLAG==1) THEN
                SBUF(L+3) =  MS(NOD)              
              ENDIF  
              L = L + SIZE
            END DO
        ENDIF
C
        IAD_SEND(I+1) = L
      ENDDO
C
C   echange messages
C
      DO I=1,NSPMD
C--------------------------------------------------------------------
C envoi a N+I mod P
Cel test si msg necessaire a envoyer a completer par test interface
       IF(DD_R2R(I+1,1)-DD_R2R(I,1)>0)THEN
          MSGTYP = MSGOFF
          SIZ = IAD_SEND(I+1)-IAD_SEND(I)
          L = IAD_SEND(I)
          CALL MPI_ISEND(
     S      SBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
       ENDIF
C--------------------------------------------------------------------
      ENDDO
C
C decompactage
C
      OFFSET = DD_R2R(NSPMD+1,1)-1
      DO I = 1, NSPMD
Cel test si msg necessaire a envoyer a completer par test interface
        NB_NOD = DD_R2R(I+1,2)-DD_R2R(I,2)
        IF(NB_NOD>0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
          L = IAD_RECV(I)

          IF(IRODDL/=0) THEN
#include        "vectorize.inc"
            DO J=DD_R2R(I,2),DD_R2R(I+1,2)-1
                NOD = DD_R2R_ELEM(OFFSET+J)
                IF(WEIGHT(NOD)==1)THEN
                  DF1 = RBUF(L)-A(1,NOD)
                  DF2 = RBUF(L+1)-A(2,NOD)
                  DF3 = RBUF(L+2)-A(3,NOD)
                  DF4 = RBUF(L+3)-AR(1,NOD)
                  DF5 = RBUF(L+4)-AR(2,NOD)
                  DF6 = RBUF(L+5)-AR(3,NOD)                  
                ENDIF                  
                A(1,NOD) = RBUF(L)
                A(2,NOD) = RBUF(L+1)
                A(3,NOD) = RBUF(L+2)
                AR(1,NOD)= RBUF(L+3)
                AR(2,NOD)= RBUF(L+4)
                AR(3,NOD)= RBUF(L+5)
                IF (FLAG==1) THEN
                  MS(NOD)= RBUF(L+6)
                  IN(NOD)= RBUF(L+7)            
                ENDIF                                 
                L = L + SIZE
C calcul du travail localement                
                IF(WEIGHT(NOD)==1)THEN
                  WF = WF + (DF1*V(1,NOD)+DF2*V(2,NOD)+
     .                 DF3*V(3,NOD))/TWO
                  WF2= WF2+ (DF1*A(1,NOD)+DF2*A(2,NOD)+
     .                 DF3*A(3,NOD))/(TWO*MS(NOD))
                  WF = WF + (DF4*VR(1,NOD)+DF5*VR(2,NOD)+
     .                 DF6*VR(3,NOD))/TWO
                  WF2= WF2+ (DF4*AR(1,NOD)+DF5*AR(2,NOD)+
     .                 DF6*AR(3,NOD))/(TWO*IN(NOD))                  
                ENDIF                  
            END DO
          ELSE
#include        "vectorize.inc"
             DO J=DD_R2R(I,2),DD_R2R(I+1,2)-1
                 NOD = DD_R2R_ELEM(OFFSET+J)
                 IF(WEIGHT(NOD)==1)THEN
                   DF1 = RBUF(L)-A(1,NOD)
                   DF2 = RBUF(L+1)-A(2,NOD)
                   DF3 = RBUF(L+2)-A(3,NOD)               
                 ENDIF              
                 A(1,NOD) = RBUF(L)
                 A(2,NOD) = RBUF(L+1)
                 A(3,NOD) = RBUF(L+2)
                 IF (FLAG==1) THEN
                   MS(NOD)= RBUF(L+3)           
                 ENDIF                 
                 L = L + SIZE
C calcul du travail localement                
                 IF(WEIGHT(NOD)==1)THEN
                   WF = WF + (DF1*V(1,NOD)+DF2*V(2,NOD)+
     .                 DF3*V(3,NOD))/TWO
                   WF2= WF2+ (DF1*A(1,NOD)+DF2*A(2,NOD)+
     .                 DF3*A(3,NOD))/(TWO*MS(NOD))                
                 ENDIF  
             END DO
          ENDIF
C ---
        ENDIF
C
      END DO
C
Cel wait terminaison isend
C
      DO I = 1, NSPMD
        IF(DD_R2R(I+1,1)-DD_R2R(I,1)>0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF       
      ENDDO
C

#endif
      RETURN
      END    
Chd|====================================================================
Chd|  SPMD_EXCH_R2R_RBY             source/mpi/r2r/spmd_r2r.F     
Chd|-- called by -----------
Chd|        R2R_INIT                      source/coupling/rad2rad/r2r_init.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_R2R_RBY(
     1   NPBY , RBY  ,IAD_ELEM    ,FR_ELEM, SIZE,
     2   LENR ,DD_R2R,DD_R2R_ELEM ,X)
C--------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "param_c.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD_ELEM(2,*),FR_ELEM(*), SIZE, LENR,
     .        DD_R2R(NSPMD+1,SDD_R2R),DD_R2R_ELEM(*),
     .        NPBY(NNPBY,*) 
      my_real
     .        RBY(NRBY,*)
      DOUBLE PRECISION X(3,*)     
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,I,NOD,LOC_PROC,IERROR,
     .        SIZ,J,K,L,NB_NOD,
     .        STATUS(MPI_STATUS_SIZE),
     .        IAD_SEND(NSPMD+1),IAD_RECV(NSPMD+1),
     .        REQ_R(NSPMD),REQ_S(NSPMD),OFFSET,IDRBY,MSGOFF
      DOUBLE PRECISION
     .        RBUF(SIZE*LENR ),SBUF(SIZE*LENR )
      DATA MSGOFF/5016/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      L = 1
      IAD_RECV(1) = 1
      
      DO I=1,NSPMD
        SIZ = SIZE*(DD_R2R(I+1,2)-DD_R2R(I,2))
        IF(SIZ/=0)THEN
          MSGTYP = MSGOFF
          CALL MPI_IRECV(
     S      RBUF(L),SIZ,MPI_DOUBLE_PRECISION,IT_SPMD(I),
     G      MSGTYP,MPI_COMM_WORLD,REQ_R(I),IERROR)
          L = L + SIZ
        ENDIF
        IAD_RECV(I+1) = L
      END DO
      L = 1
      IAD_SEND(1) = 1
C      
      DO I=1,NSPMD
C preparation envoi  partie fixe (elem) a proc I

#include      "vectorize.inc"
            DO J=DD_R2R(I,1),DD_R2R(I+1,1)-1
              IDRBY = 0
              NOD = DD_R2R_ELEM(J)
              DO K=1,NRBODY
                 IF (NPBY(1,K)==NOD) IDRBY = K
              END DO
              IF (IDRBY>0) THEN
                DO K=1,25
                  SBUF(L+K-1) =  RBY(K,IDRBY)
                END DO
                SBUF(L+26-1) =  X(1,NOD)
                SBUF(L+27-1) =  X(2,NOD) 
                SBUF(L+28-1) =  X(3,NOD)                                                          
              ELSE                
                DO K=1,25
                  SBUF(L+K-1) =  0
                END DO
              ENDIF  
              L = L + SIZE
            END DO
C
        IAD_SEND(I+1) = L
      ENDDO
C
C   echange messages
C
      DO I=1,NSPMD
C--------------------------------------------------------------------
C envoi a N+I mod P
Cel test si msg necessaire a envoyer a completer par test interface
       IF(DD_R2R(I+1,1)-DD_R2R(I,1)>0)THEN
          MSGTYP = MSGOFF
          SIZ = IAD_SEND(I+1)-IAD_SEND(I)
          L = IAD_SEND(I)
          CALL MPI_ISEND(
     S      SBUF(L),SIZ,MPI_DOUBLE_PRECISION,IT_SPMD(I),
     G      MSGTYP,MPI_COMM_WORLD,REQ_S(I),IERROR)
       ENDIF
C--------------------------------------------------------------------
      ENDDO
C
C decompactage
C
      OFFSET = DD_R2R(NSPMD+1,1)-1
      DO I = 1, NSPMD
Cel test si msg necessaire a envoyer a completer par test interface
        NB_NOD = DD_R2R(I+1,2)-DD_R2R(I,2)
        IF(NB_NOD>0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
          L = IAD_RECV(I)

#include        "vectorize.inc"
            DO J=DD_R2R(I,2),DD_R2R(I+1,2)-1
              IDRBY = 0
               NOD = DD_R2R_ELEM(OFFSET+J)
                DO K=1,NRBODY
                   IF (NPBY(1,K)==NOD) IDRBY = K
                END DO                              
                IF (IDRBY>0) THEN
                  DO K=1,25
                    RBY(K,IDRBY) = RBUF(L+K-1)
                  END DO
                  X(1,NOD) = RBUF(L+26-1)  
                  X(2,NOD) = RBUF(L+27-1)   
                  X(3,NOD) = RBUF(L+28-1)                    
                ENDIF                                 
                L = L + SIZE
            END DO
C ---
        ENDIF
C
      END DO
C
Cel wait terminaison isend
C
      DO I = 1, NSPMD
        IF(DD_R2R(I+1,1)-DD_R2R(I,1)>0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF       
      ENDDO
C

#endif
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_EXCH_WORK                source/mpi/r2r/spmd_r2r.F     
Chd|-- called by -----------
Chd|        R2R_GETDATA                   source/coupling/rad2rad/r2r_getdata.F
Chd|-- calls ---------------
Chd|        RAD2R_MOD                     share/modules/rad2r.F         
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_WORK(WF, WF2)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE RAD2R_MOD    
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----C-----------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .       WF, WF2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER P, IERROR, MSGOFF,LOC_PROC, 
     .        MSGTYP,STATUS(MPI_STATUS_SIZE)   
      my_real
     .        WFB
      DATA  MSGOFF/5017/
C-----------------------------------------------
C
      LOC_PROC = ISPMD+1
      
C Sommation sur les procs de WF
      IF(LOC_PROC==1) THEN
        DO P = 2, NSPMD
            MSGTYP = MSGOFF 
            CALL MPI_RECV(
     S        WFB,1,REAL,IT_SPMD(P),MSGTYP,
     G        MPI_COMM_WORLD,STATUS,IERROR)
            WF = WF+WFB
        END DO
      ELSE
        MSGTYP = MSGOFF 
        CALL MPI_SEND(
     S     WF,1,REAL,IT_SPMD(1),MSGTYP,
     G     MPI_COMM_WORLD,IERROR)     
      END IF
      
C Sommation sur les procs de WF2

      IF(LOC_PROC==1) THEN
        DO P = 2, NSPMD
            MSGTYP = MSGOFF 
            CALL MPI_RECV(
     S        WFB,1,REAL,IT_SPMD(P),MSGTYP,
     G        MPI_COMM_WORLD,STATUS,IERROR)
            WF2 = WF2+WFB
        END DO	
      ELSE
        MSGTYP = MSGOFF 
        CALL MPI_SEND(
     S     WF2,1,REAL,IT_SPMD(1),MSGTYP,
     G     MPI_COMM_WORLD,IERROR)
      END IF
           
#endif
      RETURN
      END
C      
Chd|====================================================================
Chd|  SPMD_R2R_TAGEL                source/mpi/r2r/spmd_r2r.F     
Chd|-- called by -----------
Chd|        TAGOFF3N                      source/interfaces/interf/chkstfn3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_R2R_TAGEL(TAGELG,TAGEL,LEN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAGELG(*),TAGEL(*),LEN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, P, N, L, IERROR, MSGOFF,LOC_PROC, MSGTYP, BUFSIZ,
     .        STATUS(MPI_STATUS_SIZE)
      DATA  MSGOFF/5018/
C-----------------------------------------------
C
      LOC_PROC = ISPMD+1
      L = 0
C      
      IF(LOC_PROC==1) THEN
        DO I=1,LEN(LOC_PROC)
          TAGELG(I)=TAGEL(I)
          L = L+1
        ENDDO
        DO P = 2, NSPMD
          IF(LEN(P)>0)THEN
            MSGTYP = MSGOFF
            CALL MPI_RECV(
     S        TAGELG(L+1),LEN(P),MPI_INTEGER,IT_SPMD(P),MSGTYP,
     G        MPI_COMM_WORLD,STATUS,IERROR)
            L = L + LEN(P)
          END IF
        END DO
      ELSEIF(LEN(LOC_PROC)>0)THEN
        MSGTYP = MSGOFF 
        CALL MPI_SEND(
     S     TAGEL,LEN(LOC_PROC),MPI_INTEGER,IT_SPMD(1),MSGTYP,
     G     MPI_COMM_WORLD,IERROR)
      END IF
C
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_EXCH_R2R_ITAG            source/mpi/r2r/spmd_r2r.F     
Chd|-- called by -----------
Chd|        TAGOFF3N                      source/interfaces/interf/chkstfn3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_R2R_ITAG(
     1   ITAG,IAD_ELEM ,FR_ELEM,DD_R2R,DD_R2R_ELEM,LENR)
C--------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD_ELEM(2,*),FR_ELEM(*),
     .        DD_R2R(NSPMD+1,SDD_R2R),DD_R2R_ELEM(*),FLAG,
     .        ITAG(*),LENR 
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,I,NOD,LOC_PROC,IERROR,
     .        SIZ,J,K,L,NB_NOD,
     .        STATUS(MPI_STATUS_SIZE),
     .        IAD_SEND(NSPMD+1),IAD_RECV(NSPMD+1),
     .        REQ_R(NSPMD),REQ_S(NSPMD),OFFSET,
     .        SBUF(2*LENR),RBUF(2*LENR), MSGOFF       
       DATA MSGOFF/5019/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      L = 1
      IAD_RECV(1) = 1
      
      DO I=1,NSPMD
        SIZ = 2*(DD_R2R(I+1,2)-DD_R2R(I,2))
        IF(SIZ/=0)THEN
          MSGTYP = MSGOFF
          CALL MPI_IRECV(
     S      RBUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
          L = L + SIZ
        ENDIF
        IAD_RECV(I+1) = L
      END DO
C
      L = 1
      IAD_SEND(1) = 1
C      
      DO I=1,NSPMD
#include      "vectorize.inc"
        DO J=DD_R2R(I,1),DD_R2R(I+1,1)-1
            NOD = DD_R2R_ELEM(J)
            SBUF(L) =  ITAG(NOD)
            SBUF(L+1) =  ITAG(NUMNOD+NOD)
            L = L + 2
        END DO
        IAD_SEND(I+1) = L
      ENDDO
C
C   echange messages
C
      DO I=1,NSPMD
C--------------------------------------------------------------------
C envoi a N+I mod P
Cel test si msg necessaire a envoyer a completer par test interface
       IF(DD_R2R(I+1,1)-DD_R2R(I,1)>0)THEN
          MSGTYP = MSGOFF
          SIZ = IAD_SEND(I+1)-IAD_SEND(I)
          L = IAD_SEND(I)
          CALL MPI_ISEND(
     S      SBUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
       ENDIF
C--------------------------------------------------------------------
      ENDDO
C
C decompactage
C
      OFFSET = DD_R2R(NSPMD+1,1)-1
      DO I = 1, NSPMD
Cel test si msg necessaire a envoyer a completer par test interface
        NB_NOD = DD_R2R(I+1,2)-DD_R2R(I,2)
        IF(NB_NOD>0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
          L = IAD_RECV(I)
#include        "vectorize.inc"
          DO J=DD_R2R(I,2),DD_R2R(I+1,2)-1
              NOD = DD_R2R_ELEM(OFFSET+J)
              ITAG(NOD) = RBUF(L)
              ITAG(NUMNOD+NOD) = RBUF(L+1)
              L = L + 2 
          END DO
C ---
        ENDIF
C
      END DO
C
Cel wait terminaison isend
C
      DO I = 1, NSPMD
        IF(DD_R2R(I+1,1)-DD_R2R(I,1)>0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF       
      ENDDO
C

#endif
      RETURN
      END      
C
Chd|====================================================================
Chd|  SPMD_EXCH_R2R_SPH             source/mpi/r2r/spmd_r2r.F     
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_R2R_SPH(A,IAD_ELEM,FR_ELEM,DD_R2R,DD_R2R_ELEM,LENR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD_ELEM(2,*),FR_ELEM(*),
     .        DD_R2R(NSPMD+1,SDD_R2R),DD_R2R_ELEM(*),LENR
      my_real
     .        A(3,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,I,NOD,LOC_PROC,IERROR,
     .        SIZ,J,K,L,NB_NOD,
     .        STATUS(MPI_STATUS_SIZE),
     .        IAD_SEND(NSPMD+1),IAD_RECV(NSPMD+1),
     .        REQ_R(NSPMD),REQ_S(NSPMD),OFFSET  
      my_real
     .        RBUF(3*LENR ),SBUF(3*LENR )  
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      L = 1
      IAD_RECV(1) = 1
      LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
      
      DO I=1,NSPMD
        SIZ = 3*(DD_R2R(I+1,1)-DD_R2R(I,1))
        IF(SIZ/=0)THEN
          MSGTYP = 10000 + NSPMD*(I-1) + LOC_PROC
          CALL MPI_IRECV(
     S      RBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
          L = L + SIZ
        ENDIF
        IAD_RECV(I+1) = L
      END DO
      L = 1
      IAD_SEND(1) = 1
C
      OFFSET = DD_R2R(NSPMD+1,1)-1      
      DO I=1,NSPMD
C preparation envoi  partie fixe (elem) a proc I
#include      "vectorize.inc"
        DO J=DD_R2R(I,2),DD_R2R(I+1,2)-1
           NOD = DD_R2R_ELEM(OFFSET+J)
           SBUF(L  ) =  A(1,NOD)
           SBUF(L+1) =  A(2,NOD)
           SBUF(L+2) =  A(3,NOD) 
           L = L + 3
        END DO
C
        IAD_SEND(I+1) = L
      ENDDO
C
C   echange messages
C
      DO I=1,NSPMD
C--------------------------------------------------------------------
C envoi a N+I mod P
       IF(DD_R2R(I+1,2)-DD_R2R(I,2)>0)THEN
          MSGTYP = 10000 + NSPMD*(LOC_PROC-1) + I
          SIZ = IAD_SEND(I+1)-IAD_SEND(I)
          L = IAD_SEND(I)
          CALL MPI_ISEND(
     S      SBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
       ENDIF
C--------------------------------------------------------------------
      ENDDO
C
C decompactage
C
      DO I = 1, NSPMD
        NB_NOD = DD_R2R(I+1,1)-DD_R2R(I,1)
        IF(NB_NOD>0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
          L = IAD_RECV(I)
#include        "vectorize.inc"
          DO J=DD_R2R(I,1),DD_R2R(I+1,1)-1
             NOD = DD_R2R_ELEM(J)             
             A(1,NOD) = RBUF(L)
             A(2,NOD) = RBUF(L+1)
             A(3,NOD) = RBUF(L+2)                
             L = L + 3
          END DO
C ---
        ENDIF
C
      END DO
C
Cel wait terminaison isend
C
      DO I = 1, NSPMD
        IF(DD_R2R(I+1,2)-DD_R2R(I,2)>0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF       
      ENDDO
C

#endif
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_EXCH_R2R_SPHOFF          source/mpi/r2r/spmd_r2r.F     
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_R2R_SPHOFF(OFF_SPH_R2R,IAD_ELEM,
     1   FR_ELEM,DD_R2R,DD_R2R_ELEM,LENR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER OFF_SPH_R2R(*),IAD_ELEM(2,*),
     .        FR_ELEM(*),DD_R2R(NSPMD+1,SDD_R2R),DD_R2R_ELEM(*),LENR
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,I,NOD,LOC_PROC,IERROR,
     .        SIZ,J,K,L,NB_NOD,
     .        STATUS(MPI_STATUS_SIZE),
     .        IAD_SEND(NSPMD+1),IAD_RECV(NSPMD+1),
     .        REQ_R(NSPMD),REQ_S(NSPMD),OFFSET,
     .        RBUF(LENR ),SBUF(LENR ) 
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      L = 1
      IAD_RECV(1) = 1
      
      DO I=1,NSPMD
        SIZ = DD_R2R(I+1,1)-DD_R2R(I,1)
        IF(SIZ/=0)THEN
          MSGTYP = 10000 + NSPMD*(I-1) + LOC_PROC
          CALL MPI_IRECV(
     S      RBUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
          L = L + SIZ
        ENDIF
        IAD_RECV(I+1) = L
      END DO
      L = 1
      IAD_SEND(1) = 1
C
      OFFSET = DD_R2R(NSPMD+1,1)-1      
      DO I=1,NSPMD
C preparation envoi  partie fixe (elem) a proc I
#include      "vectorize.inc"
        DO J=DD_R2R(I,2),DD_R2R(I+1,2)-1
           NOD = DD_R2R_ELEM(OFFSET+J)
           SBUF(L) =  OFF_SPH_R2R(NOD)
           L = L + 1
        END DO
C
        IAD_SEND(I+1) = L
      ENDDO
C
C   echange messages
C
      DO I=1,NSPMD
C--------------------------------------------------------------------
C envoi a N+I mod P
       IF(DD_R2R(I+1,2)-DD_R2R(I,2)>0)THEN
          MSGTYP = 10000 + NSPMD*(LOC_PROC-1) + I
          SIZ = IAD_SEND(I+1)-IAD_SEND(I)
          L = IAD_SEND(I)
          CALL MPI_ISEND(
     S      SBUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
       ENDIF
C--------------------------------------------------------------------
      ENDDO
C
C decompactage
C
      DO I = 1, NSPMD
        NB_NOD = DD_R2R(I+1,1)-DD_R2R(I,1)
        IF(NB_NOD>0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
          L = IAD_RECV(I)
#include        "vectorize.inc"
          DO J=DD_R2R(I,1),DD_R2R(I+1,1)-1
             NOD = DD_R2R_ELEM(J)
             OFF_SPH_R2R(NOD) = RBUF(L)                       
             L = L + 1
          END DO
C ---
        ENDIF
C
      END DO
C
Cel wait terminaison isend
C
      DO I = 1, NSPMD
        IF(DD_R2R(I+1,2)-DD_R2R(I,2)>0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF       
      ENDDO
C

#endif
      RETURN
      END
