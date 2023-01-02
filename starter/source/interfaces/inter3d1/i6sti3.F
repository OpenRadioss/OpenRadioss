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
C
Chd|====================================================================
Chd|  I6STI3                        source/interfaces/inter3d1/i6sti3.F
Chd|-- called by -----------
Chd|        INIEND                        source/interfaces/inter3d1/iniend.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        LOCAL_INDEX                   source/interfaces/interf1/local_index.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I6STI3(IRECT,STF  ,NRT   ,STFN  ,NSN    ,
     .                  NSV  ,XMAS ,MS    ,NPBY  ,LPBY   ,
     .                  NOINT,ITAB ,ID    ,TITR  )
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "nchar_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRT, NSN, NOINT
C     REAL
      my_real
     .   XMAS
      INTEGER IRECT(4,*), NSV(*), NPBY(NNPBY,*), LPBY(*), ITAB(*)
C     REAL
      my_real
     .   STF(*), STFN(*), MS(*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IFL, KAD, I, J, K, L, M, N, II, NSL, IG, IL
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C
      IFL=0
      KAD=0
      DO N=1,NRBYKIN
        M  =NPBY(1,N)
        NSL=NPBY(2,N)
        DO J=1,NSL
          IF (LPBY(KAD+J) == NSV(1)) THEN
            XMAS=MS(M)
            GOTO 150
          ENDIF
        ENDDO
        KAD =KAD+NSL
      ENDDO
C     Error : no rigid body
      IFL = 1                               
      CALL ANCMSG(MSGID=100,              
     .            MSGTYPE=MSGERROR,       
     .            ANMODE=ANINFO_BLIND_1,  
     .            I1=ID,                  
     .            C1=TITR,                
     .            I2=ITAB(NSV(1)))        
C-------------
  150 CONTINUE
C-------------
      DO I=2,NSN
        II = NSV(I)
        DO J=1,NSL
          IF (LPBY(KAD+J) == II) GOTO 300
        ENDDO
C
        IF (IFL == 0) THEN
          IFL = 1
          KAD = 0
          DO K=1,NRBYKIN
            NSL = NPBY(2,K)
            DO J=1,NSL
              IF (LPBY(KAD+J) == II .and. K /= N) THEN
C                Error : second rigid body
                 CALL ANCMSG(MSGID=1094,
     .                       MSGTYPE=MSGERROR,
     .                       ANMODE=ANINFO_BLIND_1,
     .                       I1=ID,
     .                       C1=TITR)
                GOTO 300
              ENDIF
            ENDDO
            KAD = KAD+NSL
          ENDDO
C         Error : no rigid body
          CALL ANCMSG(MSGID=100,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR,
     .                I2=ITAB(II))
        ENDIF
C
  300 CONTINUE
      ENDDO !I=2,NSN
C--------------------------------------------------------------
C     CALCUL DES RIGIDITES DES SEGMENTS ET DES NOEUDS
C---------------------------------------------------------------
      DO I=1,NRT
        STF(I)=ONE
      ENDDO
C---------------------------------------------
C     CALCUL DES RIGIDITES NODALES
C---------------------------------------------
      DO J=1,NSN
        STFN(J)=ONE
      ENDDO
C
      DO I=1,NRT
        DO J=1,4
          IG=IRECT(J,I)
          CALL LOCAL_INDEX(IL,IG,NSV,NSN)
          IRECT(J,I)=IL
        ENDDO
      ENDDO
C
      RETURN
      END
