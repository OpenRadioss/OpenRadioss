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
Chd|  DSECNOR                       source/output/anim/generate/dsecnor.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        DNWALC                        source/output/anim/generate/dnwalc.F
Chd|        DNWALS                        source/output/anim/generate/dnwals.F
Chd|        WRITE_S_C                     source/output/tools/sortie_c.c
Chd|====================================================================
      SUBROUTINE DSECNOR(X    ,RWBUF,NPRW)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .     X(3,*),RWBUF(NRWLP,*)
      INTEGER NPRW(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,N,K,N2,N3,N4,ITYP,I3000,NXX,NYY,NZZ
      my_real XX,YY,ZZ
      I3000 = 3000
      IF (ISPMD/=0) GOTO 100
      DO N=1,NSECT+NRWALL
         CALL WRITE_S_C(I3000,1)
         CALL WRITE_S_C(I3000,1)
         CALL WRITE_S_C(I3000,1)
      ENDDO
C
      K=1
      DO N=1,NRWALL
         N2=N +NRWALL
         N3=N2+NRWALL
         N4=N3+NRWALL
         ITYP= NPRW(N4)
         IF(IABS(ITYP)==1.OR.ITYP==4)THEN
           NXX = THREE1000*RWBUF(1,N)
           NYY = THREE1000*RWBUF(2,N)
           NZZ = THREE1000*RWBUF(3,N)   
           DO J=1,4
            CALL WRITE_S_C(NXX,1)
            CALL WRITE_S_C(NYY,1)
            CALL WRITE_S_C(NZZ,1)
           ENDDO
         ELSEIF(ITYP==2)THEN
            CALL DNWALC(X,RWBUF(1,N),NPRW(N3))
         ELSEIF(ITYP==3)THEN
            CALL DNWALS(X,RWBUF(1,N),NPRW(N3))
         ENDIF
         K=K+NPRW(N)
         IF(NPRW(N4)==-1)K=K+NINT(RWBUF(8,N))
      ENDDO
C
 100  CONTINUE
      RETURN
      END
