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
Chd|  COLLECT                       source/output/outfile/collect.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|====================================================================
      SUBROUTINE COLLECT(A,ITAB,WEIGHT,NODGLOB)
      USE INOUTFILE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "chara_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*),WEIGHT(*),NODGLOB(*)
      my_real
     .        A(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER I,FILEN,EMPL
       CHARACTER FILNAM*100,CYCLENUM*7
       DOUBLE PRECISION
     .        RECGLOB(4,NUMNOD)
      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=2048) :: TMP_NAME

C-----------------------------------------------

       WRITE(CYCLENUM,'(I7.7)')NCYCLE
       FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_'//CYCLENUM//'.adb'
       LEN_TMP_NAME = OUTFILE_NAME_LEN + LEN_TRIM(FILNAM)
       TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LEN_TRIM(FILNAM))
       OPEN(UNIT=IDBG5,FILE=TMP_NAME(1:LEN_TMP_NAME),ACCESS='SEQUENTIAL',
     .                  FORM='FORMATTED',STATUS='UNKNOWN')

       FILEN = ROOTLEN+17

       DO I = 1, NUMNOD
         EMPL = NODGLOB(I)
         RECGLOB(1,EMPL) = ITAB(I)
         RECGLOB(2,EMPL) =  A(1,I)
         RECGLOB(3,EMPL) =  A(2,I)
         RECGLOB(4,EMPL) =  A(3,I)
       END DO
       DO I = 1, NUMNOD
          WRITE(IDBG5,'(A,I10,I10,Z20,Z20,Z20)' )
     .     '>',NCYCLE,NINT(RECGLOB(1,I)),
     .    RECGLOB(2,I),RECGLOB(3,I),RECGLOB(4,I)
       END DO

       WRITE (IOUT,1300)  FILNAM(1:FILEN)
       WRITE (ISTDO,1300) FILNAM(1:FILEN)
       CLOSE(UNIT=IDBG5)

 1300   FORMAT (4X,' DEBUG ANALYSIS FILE:',1X,A,' WRITTEN')
      RETURN

      END
Chd|====================================================================
Chd|  COLLECT_PIT                   source/output/outfile/collect.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE COLLECT_PIT(A,ITAB,WEIGHT,NODGLOB)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "chara_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*),WEIGHT(*),NODGLOB(*)
      my_real
     .        A(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER I,FILEN,EMPL
       CHARACTER FILNAM*100,CYCLENUM*7
       DOUBLE PRECISION
     .        RECGLOB(4,NUMNOD)

C-----------------------------------------------

       WRITE(CYCLENUM,'(I7.7)')NCYCLE
       FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_'//CYCLENUM//'.pit'

       OPEN(UNIT=IDBG5,FILE=FILNAM,ACCESS='SEQUENTIAL',
     .                  FORM='FORMATTED',STATUS='UNKNOWN')

       FILEN = ROOTLEN+17

       DO I = 1, NUMNOD
         EMPL = NODGLOB(I)
         RECGLOB(1,EMPL) = ITAB(I)
         RECGLOB(2,EMPL) =  A(1,I)
         RECGLOB(3,EMPL) =  A(2,I)
         RECGLOB(4,EMPL) =  A(3,I)
       END DO
       DO I = 1, NUMNOD
          WRITE(IDBG5,'(A,I10,I10,Z20,Z20,Z20)' )
     .     '>',NCYCLE,NINT(RECGLOB(1,I)),
     .    RECGLOB(2,I),RECGLOB(3,I),RECGLOB(4,I)
       END DO

       WRITE (IOUT,1300)  FILNAM(1:FILEN)
       WRITE (ISTDO,1300) FILNAM(1:FILEN)
       CLOSE(UNIT=IDBG5)

 1300   FORMAT (4X,' DEBUG ANALYSIS FILE:',1X,A,' WRITTEN')
      RETURN

      END
Chd|====================================================================
Chd|  COLLECTX                      source/output/outfile/collect.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE COLLECTX(X,ITAB,WEIGHT,NODGLOB)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "chara_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*),WEIGHT(*),NODGLOB(*)
      my_real
     .        X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER I,FILEN,EMPL
       CHARACTER FILNAM*100,CYCLENUM*7
       DOUBLE PRECISION
     .        RECGLOB(4,NUMNOD)

C-----------------------------------------------

       WRITE(CYCLENUM,'(I7.7)')NCYCLE
       FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_'//CYCLENUM//'.xdb'

       OPEN(UNIT=67,FILE=FILNAM,ACCESS='SEQUENTIAL',
     .                  FORM='FORMATTED',STATUS='UNKNOWN')

       FILEN = ROOTLEN+17

       DO I = 1, NUMNOD
         EMPL = NODGLOB(I)
         RECGLOB(1,EMPL) = ITAB(I)
         RECGLOB(2,EMPL) =  X(1,I)
         RECGLOB(3,EMPL) =  X(2,I)
         RECGLOB(4,EMPL) =  X(3,I)
       END DO
       DO I = 1, NUMNOD
          WRITE(67,'(A,I10,I10,Z20,Z20,Z20)' )
     .     '>',NCYCLE,NINT(RECGLOB(1,I)),
     .    RECGLOB(2,I),RECGLOB(3,I),RECGLOB(4,I)
       END DO

       WRITE (IOUT,1300)  FILNAM(1:FILEN)
       WRITE (ISTDO,1300) FILNAM(1:FILEN)
       CLOSE(UNIT=67)

 1300   FORMAT (4X,' DEBUG ANALYSIS FILE:',1X,A,' WRITTEN')
      RETURN

      END
Chd|====================================================================
Chd|  COLLECTV                      source/output/outfile/collect.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE COLLECTV(V,ITAB,WEIGHT,NODGLOB)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "chara_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*),WEIGHT(*),NODGLOB(*)
      my_real
     .        V(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER I,FILEN,EMPL
       CHARACTER FILNAM*100,CYCLENUM*7
       DOUBLE PRECISION
     .        RECGLOB(4,NUMNOD)

C-----------------------------------------------

       WRITE(CYCLENUM,'(I7.7)')NCYCLE
       FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_'//CYCLENUM//'.vdb'

       OPEN(UNIT=67,FILE=FILNAM,ACCESS='SEQUENTIAL',
     .                  FORM='FORMATTED',STATUS='UNKNOWN')

       FILEN = ROOTLEN+17

       DO I = 1, NUMNOD
         EMPL = NODGLOB(I)
         RECGLOB(1,EMPL) = ITAB(I)
         RECGLOB(2,EMPL) =  V(1,I)
         RECGLOB(3,EMPL) =  V(2,I)
         RECGLOB(4,EMPL) =  V(3,I)
       END DO
       DO I = 1, NUMNOD
          WRITE(67,'(A,I10,I10,Z20,Z20,Z20)' )
     .     '>',NCYCLE,NINT(RECGLOB(1,I)),
     .    RECGLOB(2,I),RECGLOB(3,I),RECGLOB(4,I)
       END DO

       WRITE (IOUT,1300)  FILNAM(1:FILEN)
       WRITE (ISTDO,1300) FILNAM(1:FILEN)
       CLOSE(UNIT=67)

 1300   FORMAT (4X,' DEBUG ANALYSIS FILE:',1X,A,' WRITTEN')
      RETURN

      END

Chd|====================================================================
Chd|  COLLECTM                      source/output/outfile/collect.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|====================================================================
      SUBROUTINE COLLECTM(NODNX_SMS,ITAB,WEIGHT,NODGLOB)
      USE INOUTFILE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "chara_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*),WEIGHT(*),NODGLOB(*),NODNX_SMS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER I,FILEN,EMPL,NK,N,M
       CHARACTER FILNAM*100,CYCLENUM*7
       INTEGER RTRI(2,NUMNOD),RECGLOB(2,NUMNOD),
     .         ITRI(NUMNOD),INDEX(2*NUMNOD),WORK(70000)

      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=2048) :: TMP_NAME

C-----------------------------------------------

       WRITE(CYCLENUM,'(I7.7)')NCYCLE
       FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_'//CYCLENUM//'.mdb'
       LEN_TMP_NAME = OUTFILE_NAME_LEN + LEN_TRIM(FILNAM)
       TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LEN_TRIM(FILNAM))
       OPEN(UNIT=67,FILE=TMP_NAME(1:LEN_TMP_NAME),ACCESS='SEQUENTIAL',
     .                  FORM='FORMATTED',STATUS='UNKNOWN')

       FILEN = ROOTLEN+17

       DO I = 1, NUMNOD
         EMPL = NODGLOB(I)
         RTRI(1,EMPL) = ITAB(I)
         RTRI(2,EMPL) = NODNX_SMS(I)
       END DO

       DO I = 1, NUMNOD
         ITRI(I) =RTRI(2,I)
         INDEX(I)=I
       END DO
       CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMNOD,1)
       DO I = 1, NUMNOD
         RECGLOB(1,I) = RTRI(1,INDEX(I))
         RECGLOB(2,I) = RTRI(2,INDEX(I))
       END DO

       WRITE(67,'(A)') '   NODE ID','  NO TERMS'
       DO I = 1, NUMNOD
          WRITE(67,'(I10,I10)' )
     .     RTRI(1,I),RTRI(2,I)
       END DO

       WRITE(67,'(A)')'----- RESUME OF MATRIX STRUCTURE -----'
       I =1
       NK=1
       N=RECGLOB(2,I)
       DO WHILE( I+1 < NUMNOD)
         M=RECGLOB(2,I+1)
         IF(M==N)THEN
           NK=NK+1
         ELSE
           WRITE(67,'(A,I10,A,I10)')
     .        'NO of NODES WITH ',N,' TERMS =',NK
           NK=1
           N =M
         END IF
         I=I+1
       END DO
       WRITE(67,'(A,I10,A,I10)')
     .        'NO of NODES WITH ',N,' TERMS =',NK

       WRITE (IOUT,1300)  FILNAM(1:FILEN)
       WRITE (ISTDO,1300) FILNAM(1:FILEN)
       CLOSE(UNIT=67)

 1300   FORMAT (4X,' DEBUG ANALYSIS FILE:',1X,A,' WRITTEN')
      RETURN

      END


Chd|====================================================================
Chd|  COLLECTT                      source/output/outfile/collect.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|====================================================================
      SUBROUTINE COLLECTT(TEMP,ITAB,WEIGHT,NODGLOB)
      USE INOUTFILE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "chara_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*),WEIGHT(*),NODGLOB(*)
      my_real
     .        TEMP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER I,FILEN,EMPL
       CHARACTER FILNAM*100,CYCLENUM*7
       DOUBLE PRECISION
     .        RECGLOB(2,NUMNOD)
      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=2148) :: TMP_NAME

C-----------------------------------------------

       WRITE(CYCLENUM,'(I7.7)')NCYCLE
       FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_'//CYCLENUM//'.tdb'
       LEN_TMP_NAME = OUTFILE_NAME_LEN + LEN_TRIM(FILNAM)
       TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LEN_TRIM(FILNAM))

       OPEN(UNIT=IDBG8,FILE=FILNAM,ACCESS='SEQUENTIAL',
     .                  FORM='FORMATTED',STATUS='UNKNOWN')

       FILEN = ROOTLEN+17

       DO I = 1, NUMNOD
         EMPL = NODGLOB(I)
         RECGLOB(1,EMPL) = ITAB(I)
         RECGLOB(2,EMPL) =  TEMP(I)
       END DO
       DO I = 1, NUMNOD
          WRITE(IDBG8,'(A,I10,I10,Z20)' )
     .     '>',NCYCLE,NINT(RECGLOB(1,I)),
     .    RECGLOB(2,I)
       END DO

       WRITE (IOUT,1300)  FILNAM(1:FILEN)
       WRITE (ISTDO,1300) FILNAM(1:FILEN)
       CLOSE(UNIT=IDBG8)

 1300   FORMAT (4X,' DEBUG ANALYSIS FILE:',1X,A,' WRITTEN')
      RETURN

      END
