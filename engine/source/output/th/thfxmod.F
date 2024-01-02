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
Chd|  THFXMOD                       source/output/th/thfxmod.F    
Chd|-- called by -----------
Chd|        HIST2                         source/output/th/hist2.F      
Chd|-- calls ---------------
Chd|        WRTDES                        source/output/th/wrtdes.F     
Chd|====================================================================
      SUBROUTINE THFXMOD(IAD   , NNE   , ITHBUF, IADV  , NVAR, 
     .                   FXBIPM, FXBDEP, FXBVIT, FXBACC,IFORM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "fxbcom.inc"
*
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD,NNE,ITHBUF(*),IADV,NVAR,FXBIPM(NBIPM,*),IFORM
      my_real
     .        FXBDEP(*), FXBVIT(*), FXBACC(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NFXB, IFXBC, I, IFXB, TFXB(NFXBODY), ADR(NFXBODY+1),
     .        N, NN, AVAR, J, JJ, II
      my_real
     .        VV(NVAR,NNE), WA(NVAR*NNE) 
C
      IF (IMACH/=3) THEN
         NFXB=0
         IFXBC=0
         DO I=1,NNE
            IFXB=ITHBUF(IAD+NNE+I-1)
            IF (IFXB/=IFXBC) THEN
               IFXBC=IFXB
               NFXB=NFXB+1
               TFXB(NFXB)=IFXBC
               ADR(NFXB)=I
            ENDIF
         ENDDO
         ADR(NFXB+1)=NNE+1
C
         DO N=1,NVAR
            NN=ITHBUF(IADV+N-1)
            IF (NN==1) THEN
               DO I=1,NFXB
                  IFXB=TFXB(I)
                  AVAR=FXBIPM(13,IFXB)
                  DO J=ADR(I),ADR(I+1)-1
                     JJ=ITHBUF(IAD+J-1)
                     VV(N,J)=FXBDEP(AVAR+JJ-1)
                  ENDDO
               ENDDO
            ENDIF
            IF (NN==2) THEN
               DO I=1,NFXB
                  IFXB=TFXB(I)
                  AVAR=FXBIPM(13,IFXB)
                  DO J=ADR(I),ADR(I+1)-1
                     JJ=ITHBUF(IAD+J-1)
                     VV(N,J)=FXBVIT(AVAR+JJ-1)
                  ENDDO
               ENDDO
            ENDIF
            IF (NN==3) THEN
               DO I=1,NFXB
                  IFXB=TFXB(I)
                  AVAR=FXBIPM(13,IFXB)
                  DO J=ADR(I),ADR(I+1)-1
                     JJ=ITHBUF(IAD+J-1)
                     VV(N,J)=FXBACC(AVAR+JJ-1)
                  ENDDO
               ENDDO
            ENDIF
         ENDDO
C
         II=0
         DO I=1,NNE
            DO J=1,NVAR
               II=II+1
               WA(II)=VV(J,I)
            ENDDO
         ENDDO
         CALL WRTDES(WA, WA, II, IFORM, 1)
      ENDIF   
C
      RETURN
      END
      
