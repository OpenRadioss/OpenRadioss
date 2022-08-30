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
Chd|  FXBGRAV                       source/constraints/fxbody/fxbgrav.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FXBGRAV(IGRV   , IBUF   , NSNI, FXBNOD,
     .                   FXBGRVI, FXBGRVR, NSN , FXBMOD,
     .                   NBML   , NBME   , MS  , GRAV  ,
     .                   SKEW   , IFILE  , NFX , IRCM0 )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGRV(NIGRV,*), IBUF(*), NSNI, FXBNOD(*), FXBGRVI(*), NSN,
     .        NBML, NBME, IFILE, NFX, IRCM0
      my_real
     .        FXBGRVR(*), FXBMOD(*), MS(*), GRAV(LFACGRV,*), SKEW(LSKEW,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ITAG(NUMNOD), I, IADG, NL, NN, IAD, NNG, NLG, LIST(NSN),
     .        IG, ISK, N2, K1, K2, K3, IADM, IIM, II, NNP, IM, IN, IRCM
      my_real
     .        FGRV(3), VMOD(NSN*6), VV(6)
C
      IRCM=IRCM0
      IADG=0
      NLG=0
      DO NL=1,NGRAV
         DO I=1,NUMNOD
            ITAG(I)=0
         ENDDO
         NN=IGRV(1,NL)
         IAD=IGRV(4,NL)
         DO I=1,NN
            ITAG(ABS(IBUF(IAD+I-1)))=1
         ENDDO
         NNG=0
         DO I=1,NSN
            II=FXBNOD(I)
            IF (ITAG(II).GT.0) THEN
               NNG=NNG+1
               FXBGRVI(IADG+2+NNG)=II
            ENDIF
         ENDDO
         IF (NNG.GT.0) THEN
            NLG=NLG+1
            FXBGRVI(IADG+1)=NL
            FXBGRVI(IADG+2)=NNG
            IADG=IADG+2+NNG
         ENDIF
      ENDDO
C
      IAD=0
      IADG=0
      DO I=1,3
         FGRV(I)=ZERO
      ENDDO
      DO IG=1,NLG
         NL=FXBGRVI(IADG+1)
         IADG=IADG+2+FXBGRVI(IADG+2)
         ISK=IGRV(2,NL)/10
         N2=IGRV(2,NL)-10*ISK
         NNP=0
         IF (ISK.LE.1) THEN
            FGRV(N2)=GRAV(1,NL)
         ELSE
            K1=3*N2-2
            K2=3*N2-1
            K3=3*N2
            FGRV(1)=SKEW(K1,ISK)*GRAV(1,NL)
            FGRV(2)=SKEW(K2,ISK)*GRAV(1,NL)
            FGRV(3)=SKEW(K3,ISK)*GRAV(1,NL)
         ENDIF
         DO I=1,NSN
            II=FXBNOD(I)
            IF (ITAG(II).GT.0) THEN
               NNP=NNP+1
               LIST(NNP)=I
            ENDIF
         ENDDO
C---------------------------
C Projection on global modes
C---------------------------
         DO IM=1,NBME
            IF (IFILE.EQ.0) THEN
               IADM=(IM-1)*NSN*6
               DO I=1,NSN*6
                  VMOD(I)=FXBMOD(IADM+I)
               ENDDO
            ELSEIF (IFILE.EQ.1) THEN
               IADM=(IM-1)*NSNI*6
               DO I=1,NSNI*6
                  VMOD(I)=FXBMOD(IADM+I)
               ENDDO
               IADM=NSNI*6
               DO I=1,NSN-NSNI
                  IRCM=IRCM+1
                  READ(IFXM,REC=IRCM) (VV(II),II=1,6)
                  DO II=1,6
                     VMOD(IADM+II)=VV(II)
                  ENDDO
                  IADM=IADM+6
               ENDDO
            ENDIF
            FXBGRVR(IAD+IM)=ZERO
            DO I=1,NNP
               IN=LIST(I)
               IADM=(IN-1)*6
               II=FXBNOD(IN)
               FXBGRVR(IAD+IM)=FXBGRVR(IAD+IM)+
     .                           VMOD(IADM+1)*MS(II)*FGRV(1)+
     .                           VMOD(IADM+2)*MS(II)*FGRV(2)+
     .                           VMOD(IADM+3)*MS(II)*FGRV(3)
            ENDDO
         ENDDO
C--------------------------
C Projection on local modes
C--------------------------
         IIM=0
         DO IM=1,NBML
            IF (IFILE.EQ.0) THEN
               IADM=(NBME+IM-1)*NSN*6
               DO I=1,NSN*6
                  VMOD(I)=FXBMOD(IADM+I)
               ENDDO
            ELSEIF (IFILE.EQ.1) THEN
               IADM=(NBME+IM-1)*NSNI*6
               DO I=1,NSNI*6
                  VMOD(I)=FXBMOD(IADM+I)
               ENDDO
               IADM=NSNI*6
               DO I=1,NSN-NSNI
                  IRCM=IRCM+1
                  READ(IFXM,REC=IRCM) (VV(II),II=1,6)
                  DO II=1,6
                     VMOD(IADM+II)=VV(II)
                  ENDDO
                  IADM=IADM+6
               ENDDO
            ENDIF
            FXBGRVR(IAD+NBME+IIM+1)=ZERO
            FXBGRVR(IAD+NBME+IIM+2)=ZERO
            FXBGRVR(IAD+NBME+IIM+3)=ZERO
            FXBGRVR(IAD+NBME+IIM+4)=ZERO
            FXBGRVR(IAD+NBME+IIM+5)=ZERO
            FXBGRVR(IAD+NBME+IIM+6)=ZERO
            FXBGRVR(IAD+NBME+IIM+7)=ZERO
            FXBGRVR(IAD+NBME+IIM+8)=ZERO
            FXBGRVR(IAD+NBME+IIM+9)=ZERO
            DO I=1,NNP
               IN=LIST(I)
               IADM=(IN-1)*6
               II=FXBNOD(IN)
               FXBGRVR(IAD+NBME+IIM+1)=FXBGRVR(IAD+NBME+IIM+1)+
     .                        VMOD(IADM+1)*MS(II)*FGRV(1)
               FXBGRVR(IAD+NBME+IIM+2)=FXBGRVR(IAD+NBME+IIM+2)+
     .                        VMOD(IADM+2)*MS(II)*FGRV(1)
               FXBGRVR(IAD+NBME+IIM+3)=FXBGRVR(IAD+NBME+IIM+3)+
     .                        VMOD(IADM+3)*MS(II)*FGRV(1)
               FXBGRVR(IAD+NBME+IIM+4)=FXBGRVR(IAD+NBME+IIM+4)+
     .                        VMOD(IADM+1)*MS(II)*FGRV(2)
               FXBGRVR(IAD+NBME+IIM+5)=FXBGRVR(IAD+NBME+IIM+5)+
     .                        VMOD(IADM+2)*MS(II)*FGRV(2)
               FXBGRVR(IAD+NBME+IIM+6)=FXBGRVR(IAD+NBME+IIM+6)+
     .                        VMOD(IADM+3)*MS(II)*FGRV(2)
               FXBGRVR(IAD+NBME+IIM+7)=FXBGRVR(IAD+NBME+IIM+7)+
     .                        VMOD(IADM+1)*MS(II)*FGRV(3)
               FXBGRVR(IAD+NBME+IIM+8)=FXBGRVR(IAD+NBME+IIM+8)+
     .                        VMOD(IADM+2)*MS(II)*FGRV(3)
               FXBGRVR(IAD+NBME+IIM+9)=FXBGRVR(IAD+NBME+IIM+9)+
     .                        VMOD(IADM+3)*MS(II)*FGRV(3)
            ENDDO
            IIM=IIM+9
         ENDDO
         IAD=IAD+NBME+9*NBML
      ENDDO
C
      RETURN
      END SUBROUTINE FXBGRAV
