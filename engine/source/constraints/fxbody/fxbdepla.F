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
Chd|  FXBDEPLA                      source/constraints/fxbody/fxbdepla.F
Chd|-- called by -----------
Chd|        FXBODFP1                      source/constraints/fxbody/fxbodfp.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FXBDEPLA(FXBDEP, FXBRPM, X  , D   , DN    , 
     .                    NSN   , FXBNOD, NME, NMOD, FXBMOD,
     .                    ISH   , IFILE , NFX, IRCM)
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
      INTEGER DN, NSN, FXBNOD(*), NME, NMOD, ISH, IFILE, NFX, IRCM
      my_real
     .        FXBDEP(*), FXBRPM(*), X(3,*), D(3,*), FXBMOD(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,IAD,LMOD,N,NDDL,J
      my_real
     .   DT(3,DN), R(9), VMOD(DN*6), VV(6)
C
      DO I=1,9
         R(I)=FXBRPM(I+1)
      ENDDO
      LMOD=(DN+NSN)*6
C
      DO I=1,DN
         DT(1,I)=ZERO
         DT(2,I)=ZERO
         DT(3,I)=ZERO
      ENDDO
      DO I=1,12
         IF (IFILE.EQ.0) THEN
            IAD=(I-1)*LMOD+NSN*6
            DO II=1,DN*6
               VMOD(II)=FXBMOD(IAD+II)
            ENDDO
         ELSEIF (IFILE.EQ.1) THEN
            IAD=0
            DO II=1,DN
               IRCM=IRCM+1
               READ(IFXM,REC=IRCM) (VV(J),J=1,6)
               DO J=1,6
                  VMOD(IAD+J)=VV(J)
               ENDDO
               IAD=IAD+6
            ENDDO
         ENDIF
         IAD=0
         DO II=1,DN
            DT(1,II)=DT(1,II)+FXBDEP(I)*VMOD(IAD+1)
            DT(2,II)=DT(2,II)+FXBDEP(I)*VMOD(IAD+2)
            DT(3,II)=DT(3,II)+FXBDEP(I)*VMOD(IAD+3)
            IAD=IAD+6
         ENDDO
      ENDDO
      IF (ISH.GT.0.AND.IFILE.EQ.1) THEN
         DO I=1,3
            DO II=1,DN
               IRCM=IRCM+1
            ENDDO
         ENDDO
      ENDIF
C     
      IF (NMOD.GT.0) THEN
         DO I=1,NMOD
            IF (IFILE.EQ.0) THEN
               IAD=(NME+I-1)*LMOD+NSN*6
               DO II=1,DN*6
                  VMOD(II)=FXBMOD(IAD+II)
               ENDDO
            ELSEIF (IFILE.EQ.1) THEN
               IAD=0
               DO II=1,DN
                  IRCM=IRCM+1
                  READ(IFXM,REC=IRCM) (VV(J),J=1,6)
                  DO J=1,6
                     VMOD(IAD+J)=VV(J)
                  ENDDO
                  IAD=IAD+6
               ENDDO
            ENDIF
            IAD=0
            DO II=1,DN
               DT(1,II)=DT(1,II)+FXBDEP(NME+I)*
     .              (R(1)*VMOD(IAD+1)+R(2)*VMOD(IAD+2)+
     .               R(3)*VMOD(IAD+3))
               DT(2,II)=DT(2,II)+FXBDEP(NME+I)*
     .              (R(4)*VMOD(IAD+1)+R(5)*VMOD(IAD+2)+
     .               R(6)*VMOD(IAD+3))
               DT(3,II)=DT(3,II)+FXBDEP(NME+I)*
     .              (R(7)*VMOD(IAD+1)+R(8)*VMOD(IAD+2)+
     .               R(9)*VMOD(IAD+3))
               IAD=IAD+6
            ENDDO
         ENDDO
      ENDIF
      DO I=1,DN
         II=NSN+I
         N=FXBNOD(II)
         X(1,N)=X(1,N)-D(1,N)
         X(2,N)=X(2,N)-D(2,N)
         X(3,N)=X(3,N)-D(3,N)
         D(1,N)=DT(1,I)
         D(2,N)=DT(2,I)
         D(3,N)=DT(3,I)
         X(1,N)=X(1,N)+DT(1,I)
         X(2,N)=X(2,N)+DT(2,I)
         X(3,N)=X(3,N)+DT(3,I)
      ENDDO
C
      RETURN
      END
                
