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
Chd|  LAG_DIRECT                    source/tools/lagmul/lag_direct.F
Chd|-- called by -----------
Chd|        I2LAGM                        source/tools/lagmul/lag_i2main.F
Chd|        LAG_GJNT                      source/tools/lagmul/lag_gjnt.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE LAG_DIRECT(
     1      IADLL    ,LLL      ,JLL      ,XLL      ,LTSM     ,
     2      V        ,VR       ,A        ,AR       ,MS       ,
     3      IN       ,NC_INI   ,NCL      )
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
#include      "lagmult.inc"
#include      "units_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC_INI, NCL, IADLL(*), LLL(*), JLL(*)
C     REAL
      my_real
     .   LTSM(6,*),XLL(*),MS(*),IN(*),V(3,*),VR(3,*),A(3,*),AR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,IC,JC,IK
      my_real
     .        HLOC(MXDLEN,MXDLEN),RLOC(MXDLEN),S,HIJ
C======================================================================|
      IF (NCL>MXDLEN) THEN
        CALL ANCMSG(MSGID=111,ANMODE=ANINFO,
     .            I1=NCL)
        CALL ARRET(2)
      ENDIF
C---  Local H matrix
      DO K=1,NCL
        RLOC(K) = ZERO
        IC = NC_INI + K
        DO IK=IADLL(IC),IADLL(IC+1)-1
          I = LLL(IK)
          J = JLL(IK)
          IF (J>3) THEN
            LTSM(J,I) = XLL(IK)/IN(I)
          ELSE
            LTSM(J,I) = XLL(IK)/MS(I)
          ENDIF
        ENDDO
        DO L = 1,K
          JC = NC_INI + L
          HIJ = ZERO
          DO IK=IADLL(JC),IADLL(JC+1)-1
            HIJ = HIJ + XLL(IK)*LTSM(JLL(IK),LLL(IK))
          ENDDO
          HLOC(L,K)  = HIJ
        ENDDO
        DO IK=IADLL(IC),IADLL(IC+1)-1
          LTSM(JLL(IK),LLL(IK)) = ZERO
        ENDDO
      ENDDO
      DO K = 2,NCL
        DO L = 1,K
          HLOC(K,L) = HLOC(L,K)
        ENDDO
      ENDDO
C---  second membre
      DO K = 1,NCL
        IC = NC_INI + K
        DO IK=IADLL(IC),IADLL(IC+1)-1
          I = LLL(IK)
          J = JLL(IK)
          IF (J>3) THEN
            J = J-3
            RLOC(K) = RLOC(K) + XLL(IK)*(VR(J,I)/DT12+AR(J,I))
          ELSE
            RLOC(K) = RLOC(K) + XLL(IK)*(V(J,I)/DT12+A(J,I))
          ENDIF
        ENDDO
      ENDDO
C---  Factorise H (Full Cholesky)
      DO J=1,NCL
	    IF (HLOC(J,J)<=ZERO) THEN
          CALL ANCMSG(MSGID=112,ANMODE=ANINFO,
     .            I1=J)
          CALL ARRET(2)
	    ENDIF
        HLOC(J,J) = SQRT(HLOC(J,J))
        DO K=1,J-1
          DO I=J+1,NCL
	        HLOC(I,J) = HLOC(I,J) - HLOC(I,K)*HLOC(J,K)
          ENDDO
        ENDDO
        DO I=J+1,NCL
          HLOC(I,J) = HLOC(I,J)/HLOC(J,J)
          HLOC(I,I) = HLOC(I,I) - HLOC(I,J)*HLOC(I,J)
        ENDDO
      ENDDO
C---  back subst Ly = r,
      DO I=1,NCL
        S = RLOC(I)
        DO J=1,I-1
          S = S - HLOC(I,J)*RLOC(J)
        ENDDO
	    RLOC(I) = S / HLOC(I,I)
      ENDDO
C---  back subst Lz = y
      DO I=NCL,1,-1
        S = RLOC(I)
        DO J=I+1,NCL
          S = S - HLOC(J,I)*RLOC(J)
        ENDDO
	    RLOC(I) = S / HLOC(I,I)
      ENDDO
C---  update accelerations
      DO K=1,NCL
        IC = NC_INI + K
        DO IK=IADLL(IC),IADLL(IC+1)-1
          I = LLL(IK)
          J = JLL(IK)
          IF(J>3) THEN
            J = J-3
            AR(J,I) = AR(J,I) - XLL(IK)*RLOC(K)/IN(I)
          ELSE
            A(J,I)  = A(J,I)  - XLL(IK)*RLOC(K)/MS(I)
          ENDIF
        ENDDO
      ENDDO
C---
      RETURN
      END
