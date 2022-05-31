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
Chd|  I1TID3                        source/interfaces/inter3d1/i1tid3.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        INIST3                        source/interfaces/inter3d1/inist3.F
Chd|        KINSET                        source/constraints/general/kinset.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I1TID3(X     ,IRECT,CRST ,MSR  ,NSV,
     1                  ILOC  ,IRTL ,NSN  ,ITAB ,IKINE,
     2                  IKINE1,ID   ,TITR ,ILEV ,NTY, CSTS_BIS)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C     INTERFACE TIED CALCUL DE S,T
C-----------------------------------------------
C   M o d u l e s
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
#include      "scr03_c.inc"
#include      "scr07_c.2.inc"
#include      "warn_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN
      INTEGER IRECT(4,*), MSR(*), NSV(*), ILOC(*), IRTL(*),
     .   ITAB(*),IKINE(*),IKINE1(*),ILEV,NTY
      my_real
     .   X(3,*), CRST(2,*), CSTS_BIS(2,*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II, I, J, K, L, JJ, NN, IER
      my_real
     .   N1, N2, N3, SS, TT, ALP
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      ALP = TWOEM2
      DO II=1,NSN
        I=NSV(II)
        IF ((NTY.EQ.1).OR.((NTY.EQ.2).AND.(ILEV /= 25 .and. ILEV /= 26 .and. ILEV /= 27 .and. ILEV /= 28))) THEN
          CALL KINSET(2,ITAB(I),IKINE(I),1,0,IKINE1(I))
          CALL KINSET(2,ITAB(I),IKINE(I),2,0,IKINE1(I))
          CALL KINSET(2,ITAB(I),IKINE(I),3,0,IKINE1(I))
          CALL KINSET(2,ITAB(I),IKINE(I),4,0,IKINE1(I))
          CALL KINSET(2,ITAB(I),IKINE(I),5,0,IKINE1(I))
          CALL KINSET(2,ITAB(I),IKINE(I),6,0,IKINE1(I))
        ENDIF
        J=ILOC(II)
        K=MSR(J)
        L=IRTL(II)
        DO JJ=1,4
          NN=MSR(IRECT(JJ,L))
          XX1(JJ)=X(1,NN)
          XX2(JJ)=X(2,NN)
          XX3(JJ)=X(3,NN)
        ENDDO
        XS1=X(1,I)
        YS1=X(2,I)
        ZS1=X(3,I)
        CALL INIST3(N1,N2,N3,SS,TT,IER,ALP)
        IF(IPRI.GE.1)WRITE(IOUT,FMT=FMW_7I_2F)
     .        ITAB(I),ITAB(K),
     .        L,(ITAB(MSR(IRECT(JJ,L))),JJ=1,4),SS,TT
         IF(IER.EQ.-1)THEN
            CALL ANCMSG(MSGID=85,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=ITAB(I),
     .                  I3=ITAB(K),
     .                  I4=L,
     .                  I5=ITAB(NSV(IRECT(1,L))),
     .                  I6=ITAB(NSV(IRECT(2,L))),
     .                  I7=ITAB(NSV(IRECT(3,L))),
     .                  I8=ITAB(NSV(IRECT(4,L))))
         ELSE IF(IER.EQ.1)THEN
            CALL ANCMSG(MSGID=86,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=ITAB(I),
     .                  I3=ITAB(K),
     .                  I4=L,
     .                  I5=ITAB(MSR(IRECT(1,L))),
     .                  I6=ITAB(MSR(IRECT(2,L))),
     .                  I7=ITAB(MSR(IRECT(3,L))),
     .                  I8=ITAB(MSR(IRECT(4,L))),
     .                  R1=SS,
     .                  R2=TT)
         ENDIF
        CRST(1,II)=SS
        CRST(2,II)=TT
        IF (NTY == 2) THEN
          CSTS_BIS(1,II)=SS
          CSTS_BIS(2,II)=TT
        ENDIF
      ENDDO!next II
C-----------------------------------------------
      RETURN
      END
