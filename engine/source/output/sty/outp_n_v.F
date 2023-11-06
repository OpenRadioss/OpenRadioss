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
Chd|  OUTP_N_V                      source/output/sty/outp_n_v.F
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F
Chd|-- calls ---------------
Chd|        SPMD_DOUTP_VGATH              source/mpi/interfaces/spmd_outp.F
Chd|====================================================================
      SUBROUTINE OUTP_N_V(KEY,TEXT,LEN,VECT,NUMNOD,ITAB,ITABG,LENG,
     .                    NODGLOB,WEIGHT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "task_c.inc"
#include      "scr16_c.inc"
#include      "spmd_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        CHARACTER*10 KEY
        CHARACTER*40 TEXT
        INTEGER ITAB(*), LEN, NUMNOD,WEIGHT(*),ITABG(*),LENG,
     .          NODGLOB(*)
        my_real
     .     VECT(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,K

        my_real
     .   VGLOB(3,LENG)
C-----------------------------------------------

        IF (NSPMD > 1) THEN

          CALL SPMD_DOUTP_VGATH(VECT,NODGLOB,WEIGHT,VGLOB)
          IF (ISPMD/=0) RETURN
        ENDIF

C
        WRITE(IUGEO,'(2A)')'/NODAL     /VECTOR    /',KEY
        WRITE(IUGEO,'(A)')TEXT(1:LEN)
        IF (OUTYY_FMT==2) THEN
          WRITE(IUGEO,'(A)')'#FORMAT: (I8,1P3E16.9) '
        ELSE
          WRITE(IUGEO,'(A)')'#FORMAT: (I10,1P3E20.13) '
        ENDIF
        WRITE(IUGEO,'(2A)')'# USRNOD               X',
     .             '               Y               Z'

        IF (NSPMD == 1) THEN
          IF (OUTYY_FMT==2) THEN
            WRITE(IUGEO,'(I8,1P3E16.9)')
     .           (ITAB(J),(VECT(I,J),I=1,3),J=1,NUMNOD)
          ELSE
            WRITE(IUGEO,'(I10,1P3E20.13)')
     .           (ITAB(J),(VECT(I,J),I=1,3),J=1,NUMNOD)
          ENDIF
        ELSE
          IF (OUTYY_FMT==2) THEN
            WRITE(IUGEO,'(I8,1P3E16.9)')
     .           (ITABG(J),(VGLOB(I,J),I=1,3),J=1,NUMNODG)
          ELSE
            WRITE(IUGEO,'(I10,1P3E20.13)')
     .           (ITABG(J),(VGLOB(I,J),I=1,3),J=1,NUMNODG)
          ENDIF
        ENDIF
        RETURN
      END
C
Chd|====================================================================
Chd|  OUTP_N_V2                     source/output/sty/outp_n_v.F
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F
Chd|-- calls ---------------
Chd|        SPMD_GLOB_DSUM9               source/mpi/interfaces/spmd_th.F
Chd|====================================================================
      SUBROUTINE OUTP_N_V2(FOPT ,NPBY ,NOM_SECT ,NOM_WALL )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NPBY(NNPBY,*),NOM_SECT(LNOPT1,*),NOM_WALL(LNOPT1,*)
        my_real
     .     FOPT(6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,N
C-----------------------------------------------
        IF(NSPMD > 1)THEN
          CALL SPMD_GLOB_DSUM9(
     .         FOPT,6*(NSECT+NRBODY+NRWALL))
          IF (ISPMD/=0) THEN
            DO N=1,NSECT+NRBODY+NRWALL
              DO I=1,6
                FOPT(I,N) = ZERO
              ENDDO
            ENDDO
            RETURN
          ENDIF
        END IF
C
C----
        WRITE(IUGEO,'(A)')'/RBODY     /VECTOR   /FORCE'
        WRITE(IUGEO,'(A)')
     .    'Rigid bodies : forces and moments'
        IF (OUTYY_FMT==2) THEN
          WRITE(IUGEO,'(A)')'#FORMAT: (I8,1P3E16.9,/,8X,,1P3E16.9) '
          WRITE(IUGEO,'(2A,/,2A)')'#RBODYID',
     .          '              FX              FY              FZ',
     .                            '#       ',
     .          '              MX              MY              MZ'
        ELSE
          WRITE(IUGEO,'(A)')'#FORMAT: (I10,1P3E20.13,/,10X,1P3E20.13) '
          WRITE(IUGEO,'(2A,/,2A)')'# RBODY_ID',
     .     '                  FX                  FY                  FZ',
     .                            '#         ',
     .     '                  MX                  MY                  MZ'
C                                                                     12
        END IF
C
C----
        IF (OUTYY_FMT==2) THEN
          WRITE(IUGEO,'(I8,1P3E16.9,/,8X,1P3E16.9)')
     .       (NPBY(6,N),(FOPT(I,NSECT+N),I=1,3),
     .        (FOPT(I,NSECT+N),I=4,6),N=1,NRBODY)
        ELSE
          WRITE(IUGEO,'(I10,1P3E20.13,/,10X,1P3E20.13)')
     .        (NPBY(6,N),(FOPT(I,NSECT+N),I=1,3),
     .         (FOPT(I,NSECT+N),I=4,6),N=1,NRBODY)
        END IF
C
C----
C
C----
        WRITE(IUGEO,'(A)')'/SECTION   /VECTOR  /FORCE'
        WRITE(IUGEO,'(A)')
     .    'Sections forces and moments'
        IF (OUTYY_FMT==2) THEN
          WRITE(IUGEO,'(A)')'#FORMAT: (I8,1P3E16.9,/,16X,,1P3E16.9) '
          WRITE(IUGEO,'(2A,/,2A)')'#SECT_ID',
     .          '              FX              FY              FZ',
     .                           '#       ',
     .          '              MX              MY              MZ'
        ELSE
          WRITE(IUGEO,'(A)')'#FORMAT: (I10,1P3E20.13,/,10X,1P3E20.13) '
          WRITE(IUGEO,'(2A,/,2A)')'#  SECT_ID',
     .     '                  FX                  FY                  FZ',
     .                           '#         ',
     .     '                  MX                  MY                  MZ'
C                                                                     12
        ENDIF
C
C----
        IF (OUTYY_FMT==2) THEN
          WRITE(IUGEO,'(I8,1P3E16.9,/,8X,1P3E16.9)')
     .    (NOM_SECT(1,N),(FOPT(I,N),I=1,3),
     .             (FOPT(I,N),I=4,6),N=1,NSECT)
        ELSE
          WRITE(IUGEO,'(I10,1P3E20.13,/,10X,1P3E20.13)')
     .    (NOM_SECT(1,N),(FOPT(I,N),I=1,3),
     .             (FOPT(I,N),I=4,6),N=1,NSECT)
        ENDIF
C
        WRITE(IUGEO,'(A)')'/RWALL    /VECTOR  /FORCE'
        WRITE(IUGEO,'(A)')
     .    'Rigid wall forces'
        IF (OUTYY_FMT==2) THEN
          WRITE(IUGEO,'(A)')'#FORMAT: (2I8,1P3E16.9) '
          WRITE(IUGEO,'(2A)')'#RWAL_ID',
     .      '             FX              FY              FZ'
        ELSE
          WRITE(IUGEO,'(A)')'#FORMAT: (2I10,1P3E20.13) '
          WRITE(IUGEO,'(2A)')'#  RWAL_ID',
     .      '                  FX                  FY                  FZ'
C                                                                     12
        ENDIF
C
        IF (OUTYY_FMT==2) THEN
          WRITE(IUGEO,'(I8,1P3E16.9)')
     .    (NOM_WALL(1,N),(FOPT(I,NSECT+NRBODY+N),I=1,3),N=1,NRWALL)
        ELSE
          WRITE(IUGEO,'(I10,1P3E20.13)')
     .    (NOM_WALL(1,N),(FOPT(I,NSECT+NRBODY+N),I=1,3),N=1,NRWALL)
        ENDIF
C

        RETURN
      END

Chd|====================================================================
Chd|  OUTP_N_VC                     source/output/sty/outp_n_v.F
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F
Chd|-- calls ---------------
Chd|        SPMD_GLOB_DSUM9               source/mpi/interfaces/spmd_th.F
Chd|====================================================================
      SUBROUTINE OUTP_N_VC(KEY,TEXT,LEN,VECT,NUMNOD,ITAB,ITABG,LENG,
     .                    NODGLOB,WEIGHT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "task_c.inc"
#include      "scr16_c.inc"
#include      "spmd_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        CHARACTER*10 KEY
        CHARACTER*40 TEXT
        INTEGER ITAB(*), LEN, NUMNOD,WEIGHT(*),ITABG(*),LENG,
     .          NODGLOB(*)

        my_real
     .     VECT(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,K

        my_real
     .   VGLOB(3,NUMNODG)
C-----------------------------------------------

        IF (NSPMD > 1) THEN

          DO I=1,NUMNODG
            VGLOB(1,I)=ZERO
            VGLOB(2,I)=ZERO
            VGLOB(3,I)=ZERO
          ENDDO

          DO K = 1, NUMNOD
            I = NODGLOB(K)
            VGLOB(1,I) = VECT(1,K)
            VGLOB(2,I) = VECT(2,K)
            VGLOB(3,I) = VECT(3,K)
          END DO

          CALL SPMD_GLOB_DSUM9(VGLOB,3*NUMNODG)
          IF (ISPMD/=0) RETURN
        ENDIF

C
        WRITE(IUGEO,'(2A)')'/NODAL     /VECTOR    /',KEY
        WRITE(IUGEO,'(A)')TEXT(1:LEN)

        IF (OUTYY_FMT==2) THEN
          WRITE(IUGEO,'(A)')'#FORMAT: (I8,1P3E16.9) '
        ELSE
          WRITE(IUGEO,'(A)')'#FORMAT: (I10,1P3E20.13) '
        ENDIF
        WRITE(IUGEO,'(2A)')'# USRNOD               X',
     .             '               Y               Z'

        IF (NSPMD == 1) THEN

          IF (OUTYY_FMT==2) THEN
            WRITE(IUGEO,'(I8,1P3E16.9)')
     .           (ITAB(J),(VECT(I,J),I=1,3),J=1,NUMNOD)
          ELSE
            WRITE(IUGEO,'(I10,1P3E20.13)')
     .           (ITAB(J),(VECT(I,J),I=1,3),J=1,NUMNOD)
          ENDIF

        ELSE

          IF (OUTYY_FMT==2) THEN
            WRITE(IUGEO,'(I8,1P3E16.9)')
     .           (ITABG(J),(VGLOB(I,J),I=1,3),J=1,NUMNODG)

          ELSE

            WRITE(IUGEO,'(I10,1P3E20.13)')
     .           (ITABG(J),(VGLOB(I,J),I=1,3),J=1,NUMNODG)

          ENDIF


        ENDIF
        RETURN
      END
