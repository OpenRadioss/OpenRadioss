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
Chd|  PLOT_DISTRIB                  source/general_controls/computation/plot_distrib.F
Chd|-- called by -----------
Chd|        HM_READ_PERTURB_FAIL          source/general_controls/computation/hm_read_perturb_fail.F
Chd|        HM_READ_PERTURB_PART_SHELL    source/general_controls/computation/hm_read_perturb_part_shell.F
Chd|        HM_READ_PERTURB_PART_SOLID    source/general_controls/computation/hm_read_perturb_part_solid.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE PLOT_DISTRIB( ARRAY,S_ARRAY, NB_INTERV,SIZEY,X_MINVALUE,
     . X_MAXVALUE,Y_MAXVALUE,ECRIT)
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NB_INTERV,SIZEY,S_ARRAY
      my_real
     . ARRAY(*),X_MINVALUE, X_MAXVALUE,Y_MAXVALUE
      CHARACTER*1 ECRIT
      CHARACTER(len=60) :: FMTA,FMTB,FMTC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER KK,FLAG_FMT,I,J,K,IGS,ID,FLAG_FMT_TMP,IFIX_TMP,
     .   ISU,NUMA,J10(10),IGAUSS,IFUNC,UID,SUB_ID,MAX_PART,
     .   CPT_PART,NG,ITY,NEL,NFT,II,DISTRIB(NB_INTERV),MAX_DISTRIB
      my_real
     . MEAN,SD,MEAN_INPUT,SD_INPUT,TEMP,INTERV,
     . VALUE,X_MAXVALUE1
      CHARACTER*100 CHAR(100)
      CHARACTER*100 CHAR1(100)
      CHARACTER*100 CHAR2
C=======================================================================
       CHAR=''
       CHAR1=''
       CHAR2=''
       DISTRIB(1:NB_INTERV) = 0
       INTERV = (X_MAXVALUE-X_MINVALUE)/NB_INTERV
       MAX_DISTRIB = 0
c
       DO II = 1, NB_INTERV
         DO J = 1, S_ARRAY
           IF(ARRAY(J) >= X_MINVALUE+INTERV*(II-1) .AND. 
     .        ARRAY(J) < X_MINVALUE+INTERV*II) THEN
             DISTRIB(II) = DISTRIB(II) + 1
           ENDIF
           IF(ARRAY(J) == X_MAXVALUE) DISTRIB(II) = DISTRIB(II) + 1
         ENDDO 
       ENDDO

       X_MAXVALUE1 =ZERO
       DO II = 1, NB_INTERV
         VALUE = DISTRIB(II)
         VALUE = VALUE/S_ARRAY
         VALUE = VALUE * HUNDRED
         CHAR(II)= " "
         X_MAXVALUE1 = MAX(X_MAXVALUE1,VALUE)
         MAX_DISTRIB = MAX(MAX_DISTRIB,DISTRIB(II))
       ENDDO
c
       DO II = 1, NB_INTERV
         VALUE = DISTRIB(II)
         VALUE = VALUE/S_ARRAY
         VALUE = VALUE * HUNDRED
         CHAR(II)= " "
         DO J = 1,(SIZEY+1)
           IF( VALUE > (J-1) * X_MAXVALUE1/(SIZEY+1)) THEN 
             CHAR(II)(J:J)= ECRIT(1:1)
           
           ENDIF
         ENDDO
       ENDDO

       DO II = 1, NB_INTERV
          DO J = 1,(SIZEY+1)
             CHAR1(J)(II:II) = CHAR(II)(J:J)
          ENDDO
       ENDDO

       IF(Y_MAXVALUE /= ZERO) THEN
         WRITE(IOUT,'(10X,1PG20.13)')   Y_MAXVALUE
       ELSEIF(X_MAXVALUE1 /= ZERO) THEN
         WRITE(IOUT,'(15X,I10)')   MAX_DISTRIB
       ENDIF

       DO II = 1,(SIZEY+1)
          IF (X_MAXVALUE1/(X_MAXVALUE1/(SIZEY+1)) > (SIZEY+1)-II+1 )
     .       WRITE(IOUT,'(19X,A,A)') '|',CHAR1((SIZEY+1)-II+1)(1:NB_INTERV)
       ENDDO
c

       DO II = 1,NB_INTERV
          CHAR2(II:II) = '-'
       ENDDO
       CHAR2(NB_INTERV:NB_INTERV+1) = '>'
       WRITE(IOUT,'(20X,A)') CHAR2(1:51)
c
       CHAR2 = ' '
       CHAR2(1:1)=  '|'
       CHAR2(NB_INTERV:NB_INTERV)=  '|'
       WRITE(IOUT,'(20X,A)') CHAR2(1:51)
c       WRITE(IOUT,'(10X,1PG20.13,30X,1PG20.13)') X_MINVALUE,X_MAXVALUE
       IF (NB_INTERV <= 10) THEN 
         FMTA='(10X,1PG20.13,2X,1PG20.13)'
       ELSEIF (NB_INTERV <= 20) THEN 
         FMTA='(10X,1PG20.13,5X,1PG20.13)'
       ELSEIF (NB_INTERV <= 30) THEN 
         FMTA='(10X,1PG20.13,15X,1PG20.13)'
       ELSEIF (NB_INTERV <= 40) THEN 
         FMTA='(10X,1PG20.13,25X,1PG20.13)'
       ELSEIF (NB_INTERV <= 50) THEN 
         FMTA='(10X,1PG20.13,35X,1PG20.13)'
       ELSEIF (NB_INTERV <= 60) THEN 
         FMTA='(10X,1PG20.13,45X,1PG20.13)'
       ELSEIF (NB_INTERV <= 70) THEN 
         FMTA='(10X,1PG20.13,55X,1PG20.13)'
       ELSEIF (NB_INTERV <= 80) THEN 
         FMTA='(10X,1PG20.13,65X,1PG20.13)'
       ELSEIF (NB_INTERV <= 90) THEN 
         FMTA='(10X,1PG20.13,75X,1PG20.13)'
       ELSE
         FMTA='(10X,1PG20.13,85X,1PG20.13)'
       ENDIF
c
       WRITE(IOUT,FMT=FMTA) X_MINVALUE,X_MAXVALUE
c
       WRITE(IOUT,*) ' '
       WRITE(IOUT,*) ' '
C------------------------------ 
      RETURN
      END
