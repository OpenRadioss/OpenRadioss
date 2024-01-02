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
Chd|  ALESUB1                       source/ale/subcycling/alesub1.F
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|-- calls ---------------
Chd|        BCS3V                         source/ale/inter/bcs3v.F      
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|====================================================================
      SUBROUTINE ALESUB1(
     1  NALE ,V    ,W    ,DSAVE ,ICODT,
     2  ISKEW,SKEW ,D    ,ITASK ,NODFT,
     3  NODLT,FSKY ,FSKYV)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C /ALESUB is an obsolete option
C Multidomain computation (/SUBDOMAIN) is now used instead.
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ALE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NALE(*),ICODT(*),ISKEW(*),ITASK,NODFT,NODLT
      my_real V(3,NUMNOD),W(3,NUMNOD),DSAVE(3,*),SKEW(LSKEW,*),D(3,NUMNOD),FSKY(8,LSKY),FSKYV(LSKY,8)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,LCOD, INDX2(1024),I,NINDX2,KFT1,NISKFT,NISKLT,K
      my_real FAC
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
        FAC=ZERO
        IF(DT1 > ZERO)FAC=ONE/DT1
        IF(ALE%SUB%IFSUB == 0)THEN
           DO N=NODFT,NODLT
              W(1,N)=FAC*(D(1,N)-DSAVE(1,N))
              W(2,N)=FAC*(D(2,N)-DSAVE(2,N))
              W(3,N)=FAC*(D(3,N)-DSAVE(3,N))
              DSAVE(1,N)=V(1,N)
              DSAVE(2,N)=V(2,N)
              DSAVE(3,N)=V(3,N)
             IF(NALE(N) == 0)THEN
              V(1,N)=W(1,N)
              V(2,N)=W(2,N)
              V(3,N)=W(3,N)
             ENDIF
           ENDDO
           DO I=NODFT,NODLT,1024
              NINDX2 = 0
              DO N = I,MIN(NODLT,I+1023)
                LCOD=ICODT(N+NUMNOD+NUMNOD)
                IF(NALE(N)*LCOD /= 0)THEN
                  NINDX2 = NINDX2 + 1
                  INDX2(NINDX2) = N
                ENDIF
              ENDDO
              IF (NINDX2 /= 0)
     .          CALL BCS3V(NINDX2,INDX2,ISKEW,ICODT(2*NUMNOD+1),V,
     .                     W     ,SKEW)
           ENDDO
C
          IF(IPARIT > 0)THEN
           NISKFT =  1+ITASK*LSKY/NTHREAD
           NISKLT = (ITASK+1)*LSKY/NTHREAD
           IF(IVECTOR == 1) THEN
             DO K=1,8
              DO I=NISKFT,NISKLT
                FSKYV(I,K)=ZERO
              ENDDO
             ENDDO
           ELSE
             DO  K=1,8
              DO  I=NISKFT,NISKLT
                FSKY(K,I)=ZERO
              ENDDO
             ENDDO
           ENDIF
          ENDIF
        ENDIF
C
        RETURN
        END
