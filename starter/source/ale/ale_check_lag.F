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
Chd|  ALE_CHECK_LAG                 source/ale/ale_check_lag.F    
Chd|-- called by -----------
Chd|        ALELEC                        source/ale/alelec.F           
Chd|        R2R_GROUP                     source/coupling/rad2rad/r2r_group.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ALE_CHECK_LAG(NALE,IXS,IXQ,IXC,IXT,IXTG,PM,ITAB,NALE_R2R,FLAG_R2R)
      USE MESSAGE_MOD
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NALE(*), ITAB(*), NALE_R2R(*), FLAG_R2R
      INTEGER IXS(NIXS,*), IXQ(NIXQ,*), IXC(NIXC,*), IXT(NIXT,*)
      INTEGER IXTG(NIXTG,*),IMAT
      my_real
     .   PM(NPROPM,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER M, IAL, I, N, JWARN
      JWARN=0

C--------------------------------------------------------------
C LAGRANGIAN CONDITION FOR NODES ON QUADS
C--------------------------------------------------------------
      IF(NUMELQ.NE.0)THEN
        DO M=1,NUMELQ
          IMAT = IABS(IXQ(1,M))
          IF(IMAT.EQ.0)CYCLE
          IAL  = PM(72,IMAT)
          IF(IAL.NE.0)CYCLE
          DO I=2,5
            N=IXQ(I,M)
            IF(IABS(NALE(N)).EQ.2)THEN
              JWARN=1
              !WARNING NODE CONNECTS LAGRANGIAN QUAD TO EULERIAN QUAD	      
              CALL ANCMSG(MSGID=336,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ITAB(N),
     .                    PRMOD=MSG_CUMU,
     .                    C1='QUAD')
            ENDIF
          NALE(N)=0
        ENDDO !I=2,5
       ENDDO !M=1,NUMELQ
      ENDIF !IF(NUMELQ.NE.0)
      CALL ANCMSG(MSGID=336,
     .            MSGTYPE=MSGWARNING,
     .            ANMODE=ANINFO_BLIND_1,
     .            PRMOD=MSG_PRINT,
     .            C1='QUAD',
     .            C2='QUAD')
      
C--------------------------------------------------------------
C LAGRANGIAN CONDITION FOR NODES ON 3D LAGRANGIANS ELEMS
C--------------------------------------------------------------
      IF(NUMELS.NE.0)THEN
        DO M=1,NUMELS
          IMAT = IABS(IXS(1,M))
          IF(IMAT.EQ.0)CYCLE
          IAL  = PM(72,IMAT)
          IF(IAL.NE.0)CYCLE
          DO I=2,9
            N=IXS(I,M)
            IF(IABS(NALE(N)).EQ.2)THEN
              JWARN=1
              !WARNING NODE CONNECTS LAGRANGIAN SOLID TO EULERIAN SOLID
              CALL ANCMSG(MSGID=336,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ITAB(N),
     .                    PRMOD=MSG_CUMU,
     .                    C1='SOLID')
            ENDIF
          NALE(N)=0
        ENDDO !I=2,9
       ENDDO !M=1,NUMELS
      ENDIF !IF(NUMELS.NE.0)
      CALL ANCMSG(MSGID=336,
     .            MSGTYPE=MSGWARNING,
     .            ANMODE=ANINFO_BLIND_1,
     .            PRMOD=MSG_PRINT,
     .            C1='SOLID',
     .            C2='SOLID')
      
C---------------------------------------------------
C LAGRANGIAN CONDITION FOR NODES ON SHELLS
C---------------------------------------------------
      IF(NUMELC.NE.0)THEN
        DO M=1,NUMELC
          DO I=2,5
            N=IXC(I,M)
            IF(IABS(NALE(N)).EQ.2)THEN
              JWARN=1
	      !WARNING NODE CONNECTS SHELL (LAGRANGIAN) TO EULERIAN SOLID
              CALL ANCMSG(MSGID=336,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ITAB(N),
     .                    PRMOD=MSG_CUMU,
     .                    C1='SHELL')
            ENDIF
          NALE(N)=0
        ENDDO !I=2,5
       ENDDO !M=1,NUMELC
      ENDIF !IF(NUMELC.NE.0)
      CALL ANCMSG(MSGID=336,
     .            MSGTYPE=MSGWARNING,
     .            ANMODE=ANINFO_BLIND_1,
     .            PRMOD=MSG_PRINT,
     .            C1='SHELL',
     .            C2='SOLID')
C
      IF(NUMELTG.NE.0)THEN
       DO M=1,NUMELTG
        DO I=2,4
         N=IXTG(I,M)
         IF(IABS(NALE(N)).EQ.2)THEN
          JWARN=1
          CALL ANCMSG(MSGID=336,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ITAB(N),
     .                PRMOD=MSG_CUMU,
     .                    C1='SH3N')
         ENDIF
         NALE(N)=0
        ENDDO
       ENDDO
      ENDIF
      CALL ANCMSG(MSGID=336,
     .            MSGTYPE=MSGWARNING,
     .            ANMODE=ANINFO_BLIND_1,
     .            PRMOD=MSG_PRINT,
     .            C1='SHELL-3N',
     .            C2='SOLID')
C---------------------------------------------------
C LAGRANGIAN CONDITION FOR NODES ON TRUSSES
C---------------------------------------------------
      IF(NUMELT.NE.0)THEN
        DO M=1,NUMELT
          DO I=2,3
            N=IXT(I,M)
            IF(IABS(NALE(N)).EQ.2)THEN
              JWARN=1
              !WARNING NODE CONNECTS TRUSS (LAGRANGIAN) TO EULERIAN PART
              CALL ANCMSG(MSGID=336,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=ITAB(N),
     .                    PRMOD=MSG_CUMU,
     .                    C1='TRUSS')
            ENDIF
          NALE(N)=0
        ENDDO !I=2,3
       ENDDO !M=1,NUMELT
      ENDIF !IF(NUMELT.NE.0)
      CALL ANCMSG(MSGID=336,
     .            MSGTYPE=MSGWARNING,
     .            ANMODE=ANINFO_BLIND_1,
     .            PRMOD=MSG_PRINT,
     .            C1='TRUSS',
     .            C2='PART')

C---------------------------------------------------
      !WARNING LAGRANGIAN PART CONNECTED TO EULERIAN PART
      IF(JWARN.EQ.1) THEN
         CALL ANCMSG(MSGID=337,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO)
      ENDIF

C---------------------------------------------------
C MULTIDOMAINS : SYNCHRONISATION OF NALE FOR COMMON NODES
C---------------------------------------------------
      IF(IALE .NE. 0)THEN  !this subroutine alebcs is also now called even if IALE=0 and IEULER=1, this block was previously only treated if IALE.NE.0
        !-----------------------------------!
        IF (FLAG_R2R.GT.0) THEN
          DO N=1,NUMNOD
            NALE(N) = NALE_R2R(N)*NALE(N)
          END DO
        ENDIF
        !-----------------------------------!
      ENDIF
      
      RETURN
      END
