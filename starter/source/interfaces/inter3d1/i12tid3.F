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
Chd|  I12TID3                       source/interfaces/inter3d1/i12tid3.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        INIST3                        source/interfaces/inter3d1/inist3.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I12TID3(
     1                   X   , IRECT, CRST, MSR , NSV,
     2                   ILOC, IRTL , NSN , ITAB, ALP,
     3                   ID  , TITR
     .                   )
      USE MESSAGE_MOD
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C-----------------------------------------------
C     TIED/SLIDING INTERFACE TYPE 12
C     (FLUID/FLUID)
C     COMPUTATION OF S,T (secnd node coordinates)
C     check if secnd is facing a main segment
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN)    :: NSN,ID
      INTEGER,INTENT(IN)    :: IRECT(4,*) , MSR(*), NSV(*), ILOC(*), IRTL(*),ITAB(*)
      my_real,INTENT(IN)    :: X(3,NUMNOD), ALP
      my_real,INTENT(INOUT) :: CRST(2,*)
      CHARACTER*nchartitle,INTENT(INOUT) :: TITR
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr07_c.2.inc"
#include      "warn_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: II, I, J, K, L, JJ, NN, IER
      my_real :: N1, N2, N3, SS, TT
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO II=1,NSN
        !secnd node data
        I   = NSV(II)  !itab(i) is user node_id
        J   = ILOC(II)
        K   = MSR(J)
        L   = IRTL(II) !main segment
        XS1 = X(1,I)
        YS1 = X(2,I)
        ZS1 = X(3,I)
        !main segment data
        DO JJ=1,4
          NN      = MSR(IRECT(JJ,L)) !itab( MSR(IRECT(JJ,L)) ) are main segment nodes
          XX1(JJ) = X(1,NN)
          XX2(JJ) = X(2,NN)
          XX3(JJ) = X(3,NN)
	END DO

        CALL INIST3(N1,N2,N3,SS,TT,IER,ALP)
        IF(IPRI>=1)WRITE(IOUT,FMT=FMW_7I_2F) ITAB(I),ITAB(K),L,(ITAB(MSR(IRECT(JJ,L))),JJ=1,4),SS,TT
        IF(IER==-1)THEN
          CALL ANCMSG(MSGID=1247,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANSTOP,
     .                 I1=ITAB(I),I2=ITAB(K),I3=L,
     .                 I4=ITAB(MSR(IRECT(1,L))),
     .                 I5=ITAB(MSR(IRECT(2,L))),
     .                 I6=ITAB(MSR(IRECT(3,L))),
     .                 I7=ITAB(MSR(IRECT(4,L))),
     .                 PRMOD=MSG_CUMU)
        ELSE IF(IER==1)THEN
          CALL ANCMSG(MSGID=1248,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND,
     .                 I1=ITAB(I),I2=L,
     .                 I3=ITAB(MSR(IRECT(1,L))),
     .                 I4=ITAB(MSR(IRECT(2,L))),
     .                 I5=ITAB(MSR(IRECT(3,L))),
     .                 I6=ITAB(MSR(IRECT(4,L))),
     .                 R1=SS,R2=TT,
     .                 PRMOD=MSG_CUMU)
        ENDIF
        CRST(1,II) = SS
        CRST(2,II) = TT
      END DO !II=1,NSN
      CALL ANCMSG(MSGID=1247,
     .  	      MSGTYPE=MSGERROR,
     .  	      ANMODE=ANSTOP,
     .  	      I1=ID,
     .  	      C1=TITR,
     .  	      PRMOD=MSG_PRINT)
      CALL ANCMSG(MSGID=1248,
     .  	      MSGTYPE=MSGWARNING,
     .  	      ANMODE=ANINFO_BLIND,
     .  	      I1=ID,
     .  	      C1=TITR,
     .  	      PRMOD=MSG_PRINT)

      RETURN
      END

