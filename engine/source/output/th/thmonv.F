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
Chd|  THMONV                        source/output/th/thmonv.F     
Chd|-- called by -----------
Chd|        HIST2                         source/output/th/hist2.F      
Chd|-- calls ---------------
Chd|        FRETITL2                      source/input/freform.F        
Chd|        WRTDES                        source/output/th/wrtdes.F     
Chd|====================================================================
      SUBROUTINE THMONV(J1 ,J2   ,ITHBUF  ,L1   ,L2   ,
     .                  WA ,FSAV ,FSAVVENT,IVOLU,IFORM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
      INTEGER ITHBUF(*),IVOLU(NIMV,*),IFORM
      INTEGER J1,J2,L1,L2,K,IAD2
      my_real
     .        WA(*),FSAV(NTHVKI,*),FSAVVENT(5,*)
C
      INTEGER I,J,L,II,IADVENT,IV,KV,NVENT,ID_VENT,K1
      CHARACTER TITR1*40,ID_TITR1*2
C
Cel gather deja effectue, seul p0 ecrit 
      IF (IMACH/=3.OR.ISPMD==0) THEN
        II = 0
        DO J=J1,J2
          DO L=L1,L2
            II=II+1
            WA(II) = ZERO
          ENDDO
        ENDDO
c
        II = 0
        ID_VENT = 0
        IAD2=J1+3*((J2-J1)+1)
        DO J=J1,J2
              I=ITHBUF(J)
              CALL FRETITL2(TITR1,ITHBUF(IAD2),40)
              ID_TITR1(1:2) = TITR1(21:22)
              READ(ID_TITR1,FMT='(I2)',ERR=100) ID_VENT
100           CONTINUE
c
              DO L=L1,L2
                K=ITHBUF(L) 
                K1 = (K-101)/5+1
                II=II+1
                IF(K<=NTHVKI)THEN
                  WA(II)=FSAV(K,I)
                ELSEIF(K>=100)THEN
C                 variables additionnelles AOUTi,BOUTi,UOUTi,MOUTi,HOUTi
                  KV=MOD(K-101,5)+1
                  IV=ID_VENT
                  IADVENT=IVOLU(16,I)
                  NVENT=IVOLU(11,I)
                  IF (IV<=NVENT) THEN 
                    WA(II) =FSAVVENT(KV,IADVENT+IV)
                  END IF
                END IF
              ENDDO
              IAD2 = IAD2 + 40
        ENDDO
        IF(II>0)CALL WRTDES(WA,WA,II,IFORM,1)
      ENDIF
C
      RETURN
      END
