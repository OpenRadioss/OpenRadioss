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
Chd|  THBCS                         source/output/th/thbcs.F      
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        BCS1TH                        source/output/th/bcs1th.F     
Chd|====================================================================
      SUBROUTINE THBCS(NODFT  ,NODLAST,ICODT  ,ICODR ,ISKEW  ,
     2                 SKEW   ,A      ,AR     ,MS    ,IN     ,
     3                 FTHREAC,NODREAC,CPTREAC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "scr14_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODLAST,CPTREAC
      INTEGER NODFT,NODLT,ICODT(*),ICODR(*),ISKEW(*),NODREAC(*)
      my_real
     .    A(3,*), AR(3,*), MS(*),SKEW(LSKEW,*),
     .    FTHREAC(*),IN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, K, ISK, LCOD, NINDX,
     .        INDX(1024),L,J,FLAG_MOM
C-----------------------------------------------
C
      IF (CPTREAC >= 1) THEN
        FLAG_MOM = 0
        DO 420 I = NODFT,NODLAST,1024
          NINDX = 0
          DO 400 N=I,MIN(NODLAST,I+1023)
            IF (ICODT(N)/=0 .AND. NODREAC(N)>0)THEN
              NINDX = NINDX + 1
              INDX(NINDX) = N
            ENDIF
  400     CONTINUE
          CALL BCS1TH(NINDX,INDX,ISKEW,ICODT,
     &                A,SKEW,MS,FTHREAC,
     &                NODREAC,FLAG_MOM,IN)
  420   CONTINUE
C
        IF(IRODDL/=0) THEN
          FLAG_MOM = 1
          DO 440 I = NODFT,NODLAST,1024
            NINDX = 0
            DO 430 N=I,MIN(NODLAST,I+1023)
              IF (ICODR(N)/=0 .AND. NODREAC(N)>0)THEN
                NINDX = NINDX + 1
                INDX(NINDX) = N
              ENDIF
  430       CONTINUE
            CALL BCS1TH(NINDX,INDX,ISKEW,ICODR,
     &                  AR,SKEW,MS,FTHREAC,
     &                  NODREAC,FLAG_MOM,IN)
  440     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
