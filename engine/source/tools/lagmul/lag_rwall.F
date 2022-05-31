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
Chd|  LAG_RWALL                     source/tools/lagmul/lag_rwall.F
Chd|-- called by -----------
Chd|        LAG_MULT                      source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE LAG_RWALL(RWL     ,NSW     ,NSN     ,ITIED   ,MSR     ,
     2                     INDEX   ,X       ,V       ,A       ,IADLL   ,
     3                     LLL     ,JLL     ,SLL     ,XLL     ,COMNTAG ,
     4                     N_MUL_MX,NKMAX   ,NC      )
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
#include      "param_c.inc"
#include      "lagmult.inc"
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC, NSN, ITIED, MSR, NDDIM, N_MUL_MX, NKMAX
      INTEGER NSW(*),INDEX(*),LLL(*),JLL(*),SLL(*),IADLL(*),COMNTAG(*)
      my_real
     .   X(*), V(*), A(*), RWL(*), XLL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IK, J, JJ, K, N, N1, N2, N3, M1, M2, M3, NINDEX, 
     .   ICONT
      my_real 
     .   XWL, YWL, ZWL, VXW, VYW, VZW, VNW,
     .   VX, VY, VZ, UX, UY, UZ, XC, YC, ZC, DP0, DP, DV
C-----------------------------------------------
C        NC : nombre de condition cinematique 
C        IC : numero de la condition cinematique (1,NC)
C        IK : 
C        I  : numero global du noeud (1,NUMNOD)
C        J  : direction 1,2,3,4,5,6
C------
C        IADLL(NC)        : IAD = IADLL(IC)
C        IK = IAD,IAD+1,IAD+2,...
C        LLL(LAG_NKF)  : I = LLL(IK)
C        JLL(LAG_NKF)  : J = JLL(IK)
C======================================================================|
      ICONT=0
      NINDEX=0
C
      IF(MSR.EQ.0)THEN
       XWL=RWL(4)
       YWL=RWL(5)
       ZWL=RWL(6)
       VXW=ZERO
       VYW=ZERO
       VZW=ZERO
       VNW=ZERO
      ELSE
       M3=3*MSR
       M2=M3-1
       M1=M2-1
       VXW=V(M1)
       VYW=V(M2)
       VZW=V(M3)
       VNW = VXW*RWL(1)+VYW*RWL(2)+VZW*RWL(3)
       XWL=X(M1)+VXW*DT2
       YWL=X(M2)+VYW*DT2
       ZWL=X(M3)+VZW*DT2
      ENDIF

      DO 20 I=1,NSN
        N =NSW(I)
        N3=3*N
        N2=N3-1
        N1=N2-1
ctmp        VX=V(N1)+A(N1)*DT12
ctmp        VY=V(N2)+A(N2)*DT12
ctmp        VZ=V(N3)+A(N3)*DT12
        VX=V(N1)
        VY=V(N2)
        VZ=V(N3)
        UX=X(N1)+VX*DT2
        UY=X(N2)+VY*DT2
        UZ=X(N3)+VZ*DT2
        XC=UX-XWL
        YC=UY-YWL
        ZC=UZ-ZWL
        DP=XC*RWL(1)+YC*RWL(2)+ZC*RWL(3)
        IF(DP.GT.ZERO) GOTO 20
        ICONT=1
C---    test pour noeuds penetres
        XC=X(N1)-XWL
        YC=X(N2)-YWL
        ZC=X(N3)-ZWL
        DP0=XC*RWL(1)+YC*RWL(2)+ZC*RWL(3)
        IF((VX-VXW)*RWL(1)+(VY-VYW)*RWL(2)+(VZ-VZW)*RWL(3).GT.0.0
     .     .AND.DP0.GT.0.0) GOTO 20
ctmp        IF((VX-VXW)*RWL(1)+(VY-VYW)*RWL(2)+(VZ-VZW)*RWL(3).GT.0.)
ctmp     .    GOTO 20
        NINDEX = NINDEX+1
        INDEX(NINDEX) = I
 20   CONTINUE

      IF(MSR.EQ.0)THEN
C---------------------------
C       Fixed rigid wall
C---------------------------
        IF(ITIED.EQ.0)THEN
          DO J = 1,NINDEX
            I = INDEX(J)
            N =NSW(I)
            NC=NC+1
            IF(NC.GT.N_MUL_MX)THEN
              CALL ANCMSG(MSGID=118,ANMODE=ANINFO,
     .            C1='NC')
              CALL ARRET(2)
            ENDIF
            IADLL(NC+1)=IADLL(NC) + 3
            IF(IADLL(NC+1)-1.GT.NKMAX)THEN
              CALL ANCMSG(MSGID=118,ANMODE=ANINFO,
     .            C1='NK')
              CALL ARRET(2)
            ENDIF
            IK = IADLL(NC)
            LLL(IK) = N
            JLL(IK) = 1
            SLL(IK) = 0
            XLL(IK) = RWL(1)
            IK = IK + 1
            LLL(IK) = N
            JLL(IK) = 2
            SLL(IK) = 0
            XLL(IK) = RWL(2)
            IK = IK + 1
            LLL(IK) = N
            JLL(IK) = 3
            SLL(IK) = 0
            XLL(IK) = RWL(3)
            COMNTAG(N) = COMNTAG(N) + 1
          ENDDO          
C
        ELSEIF(ITIED.EQ.1)THEN
C
          DO J = 1,NINDEX
            I = INDEX(J)
            N = NSW(I)
C---       x
            NC=NC+1
            IF(NC.GT.N_MUL_MX)THEN
              CALL ANCMSG(MSGID=118,ANMODE=ANINFO,
     .            C1='NC')
              CALL ARRET(2)
            ENDIF
            IADLL(NC+1)=IADLL(NC) + 1
            IF(IADLL(NC+1)-1.GT.NKMAX)THEN
              CALL ANCMSG(MSGID=118,ANMODE=ANINFO,
     .            C1='NK')
              CALL ARRET(2)
            ENDIF
            IK = IADLL(NC)
            LLL(IK) = N
            JLL(IK) = 1
            SLL(IK) = 0
            XLL(IK) = ONE
C---       y
            NC=NC+1
            IF(NC.GT.N_MUL_MX)THEN
              CALL ANCMSG(MSGID=118,ANMODE=ANINFO,
     .            C1='NC')
              CALL ARRET(2)
            ENDIF
            IADLL(NC+1)=IADLL(NC) + 1
            IF(IADLL(NC+1)-1.GT.NKMAX)THEN
              CALL ANCMSG(MSGID=118,ANMODE=ANINFO,
     .            C1='NK')
              CALL ARRET(2)
            ENDIF
            IK = IADLL(NC)
            LLL(IK) = N
            JLL(IK) = 2
            SLL(IK) = 0
            XLL(IK) = ONE
C---       z
            NC=NC+1
            IF(NC.GT.N_MUL_MX)THEN
              CALL ANCMSG(MSGID=118,ANMODE=ANINFO,
     .            C1='NC')
              CALL ARRET(2)
            ENDIF
            IADLL(NC+1)=IADLL(NC) + 1
            IF(IADLL(NC+1)-1.GT.NKMAX)THEN
              CALL ANCMSG(MSGID=118,ANMODE=ANINFO,
     .            C1='NK')
              CALL ARRET(2)
            ENDIF
            IK = IADLL(NC)
            LLL(IK) = N
            JLL(IK) = 3
            SLL(IK) = 0
            XLL(IK) = ONE
            COMNTAG(N) = COMNTAG(N) + 1
          ENDDO
        ELSE
c---  add friction
        ENDIF
      ELSE
C---------------------------
C       Moving rigid wall
C---------------------------
        IF(ITIED.EQ.0)THEN
          DO J = 1,NINDEX
            I = INDEX(J)
            N =NSW(I)
            NC=NC+1
            IF(NC.GT.N_MUL_MX)THEN
              CALL ANCMSG(MSGID=118,ANMODE=ANINFO,
     .            C1='NC')
              CALL ARRET(2)
            ENDIF
            IADLL(NC+1)=IADLL(NC) + 6
            IF(IADLL(NC+1)-1.GT.NKMAX)THEN
              CALL ANCMSG(MSGID=118,ANMODE=ANINFO,
     .            C1='NK')
              CALL ARRET(2)
            ENDIF
            IK = IADLL(NC)
            LLL(IK) = N
            JLL(IK) = 1
            SLL(IK) = 0
            XLL(IK) = RWL(1)
            IK = IK + 1
            LLL(IK) = N
            JLL(IK) = 2
            SLL(IK) = 0
            XLL(IK) = RWL(2)
            IK = IK + 1
            LLL(IK) = N
            JLL(IK) = 3
            SLL(IK) = 0
            XLL(IK) = RWL(3)
            IK = IK + 1
            LLL(IK) = MSR
            JLL(IK) = 1
            SLL(IK) = 0
            XLL(IK) =-RWL(1)
            IK = IK + 1
            LLL(IK) = MSR
            JLL(IK) = 2
            SLL(IK) = 0
            XLL(IK) =-RWL(2)
            IK = IK + 1
            LLL(IK) = MSR
            JLL(IK) = 3
            SLL(IK) = 0
            XLL(IK) =-RWL(3)
            COMNTAG(N)   = COMNTAG(N) + 1
            COMNTAG(MSR) = COMNTAG(MSR) + 1
          ENDDO
C
        ELSEIF(ITIED.EQ.1)THEN
          DO J = 1,NINDEX
            I = INDEX(J)
            N = NSW(I)
C---       x
            NC=NC+1
            IF(NC.GT.N_MUL_MX)THEN
              CALL ANCMSG(MSGID=118,ANMODE=ANINFO,
     .            C1='NC')
              CALL ARRET(2)
            ENDIF
            IADLL(NC+1)=IADLL(NC) + 2
            IF(IADLL(NC+1)-1.GT.NKMAX)THEN
              CALL ANCMSG(MSGID=118,ANMODE=ANINFO,
     .            C1='NK')
              CALL ARRET(2)
            ENDIF
            IK = IADLL(NC)
            LLL(IK) = N
            JLL(IK) = 1
            SLL(IK) = 0
            XLL(IK) = ONE
            IK = IK + 1
            LLL(IK) = MSR
            JLL(IK) = 1
            SLL(IK) = 0
            XLL(IK) =-ONE
C---       y
            NC=NC+1
            IF(NC.GT.N_MUL_MX)THEN
              CALL ANCMSG(MSGID=118,ANMODE=ANINFO,
     .            C1='NC')
              CALL ARRET(2)
            ENDIF
            IADLL(NC+1)=IADLL(NC) + 2
            IF(IADLL(NC+1)-1.GT.NKMAX)THEN
              CALL ANCMSG(MSGID=118,ANMODE=ANINFO,
     .            C1='NK')
              CALL ARRET(2)
            ENDIF
            IK = IADLL(NC)
            LLL(IK) = N
            JLL(IK) = 2
            SLL(IK) = 0
            XLL(IK) = ONE
            IK = IK + 1
            LLL(IK) = MSR
            JLL(IK) = 2
            SLL(IK) = 0
            XLL(IK) =-ONE
C---       z
            NC=NC+1
            IF(NC.GT.N_MUL_MX)THEN
              CALL ANCMSG(MSGID=118,ANMODE=ANINFO,
     .            C1='NC')
              CALL ARRET(2)
            ENDIF
            IADLL(NC+1)=IADLL(NC) + 2
            IF(IADLL(NC+1)-1.GT.NKMAX)THEN
              CALL ANCMSG(MSGID=118,ANMODE=ANINFO,
     .            C1='NK')
              CALL ARRET(2)
            ENDIF
            IK = IADLL(NC)
            LLL(IK) = N
            JLL(IK) = 3
            SLL(IK) = 0
            XLL(IK) = ONE
            IK = IK + 1
            LLL(IK) = MSR
            JLL(IK) = 3
            SLL(IK) = 0
            XLL(IK) =-ONE
            COMNTAG(N)   = COMNTAG(N) + 1
            COMNTAG(MSR) = COMNTAG(MSR) + 1
          ENDDO
        ELSE
c---  add friction
        ENDIF
      ENDIF
C---
      RETURN
      END
