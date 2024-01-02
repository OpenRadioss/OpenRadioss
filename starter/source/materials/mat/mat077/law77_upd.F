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
Chd|  LAW77_UPD                     source/materials/mat/mat077/law77_upd.F
Chd|-- called by -----------
Chd|        UPDMAT                        source/materials/updmat.F     
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FUNC_SLOPE                    source/tools/curve/func_slope.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE LAW77_UPD(TITR    ,MAT_ID   ,NUPARAM ,MAT_PARAM ,
     .                     UPARAM  ,NFUNC    ,IFUNC   ,NPC       ,PLD      )
C----------------------------------------------- 
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD      
      USE MATPARAM_DEF_MOD
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  MAT_ID,NFUNC,NUPARAM
      INTEGER STF,SNPC
      INTEGER ,DIMENSION(NFUNC)   :: IFUNC
      INTEGER :: NPC(*)
      my_real PLD(*)
      my_real UPARAM(NUPARAM)
      CHARACTER*nchartitle  :: TITR 
      TYPE(MATPARAM_STRUCT_) :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,K,II,JJ,NDIM,NLOAD,NULOAD,NPT,NEPSP,FUNC_ID,
     .           FUNC_T,FUNC_C,FUNC_S,ICAS,ICONV,
     .           NPT_TRAC,NPT_COMP,NPT_SHEAR,NPTMAX,IFUN_NUP,IFX,IFY,STAT,
     .           LEN,IX0,IY0,IFLAG,IFLAG0,NF,ITENS,ICHK,IC1,IC2,IBID
      my_real :: XINT,YINT,EMAX,E0,EPSMAX,EPS0,EPST1,FAC,DERI,
     .           X0,Y0,X1,Y1,DX,DY,STIFFMIN,STIFFMAX,STIFFINI,STIFFAVG
      INTEGER ,DIMENSION(:)  ,ALLOCATABLE :: SIZE
      my_real ,DIMENSION(:)  ,ALLOCATABLE :: X_COMP,Y_COMP
      my_real ,DIMENSION(NFUNC) :: RATE,YFAC
C=======================================================================
      NLOAD  = INT(UPARAM(7))
      NULOAD = INT(UPARAM(8))
      E0     = UPARAM(2)
      EPSMAX = UPARAM(4)
      EMAX   = UPARAM(2*NFUNC + 12)
      ITENS  = UPARAM(2*NFUNC + 13)
c      
      DO I = 1,NFUNC
        RATE(I) = UPARAM(I + 8)
        YFAC(I) = UPARAM(I + 8 + NFUNC)
      END DO
C=======================================================================
      IBID = MAT_PARAM%ILAW
      IFLAG = 0
c     When Emax=0 we consider the max curve slope.
      IF (EMAX == ZERO) THEN
        DO K=1,NFUNC
          FUNC_ID = IFUNC(K)
          FAC = YFAC(K)
          CALL FUNC_SLOPE(FUNC_ID,FAC,NPC,PLD,STIFFMIN,STIFFMAX,STIFFINI,STIFFAVG)
          UPARAM(2*NFUNC + 12) = STIFFMAX
          UPARAM(3) = (STIFFMAX - E0)/STIFFMAX
          CALL ANCMSG(MSGID=1219,MSGTYPE=MSGINFO,ANMODE=ANINFO_BLIND_1,
     .                I1=MAT_ID,
     .                C1=TITR,
     .                R1=EMAX)
         END DO
c         IF (STIFFMAX < E0) IFLAG0 = 1
       ENDIF ! Emax
c      automatic modification of EPST1 and E0
       EPS0  = ONE
       IFLAG = 0
       IFLAG0= 0
       EPST1 = ONE            
       DO K=1,NLOAD
          FUNC_ID = IFUNC(K)
          ICHK = 0
          IF (FUNC_ID > 0 ) THEN
             FAC = YFAC(K)
             IC1 = NPC(FUNC_ID)
             IC2 = NPC(FUNC_ID+1)
C         loading function
             X0 = PLD(IC1)
             DO II = IC1,IC2-4,2
               JJ = II+2
               DX = PLD(JJ)   - X0
               DY = PLD(JJ+1) - PLD(II+1)
               Y0 =  FAC*PLD(II+1)
               Y1 =  FAC*PLD(JJ+1)
               DERI = FAC * DY / DX
               X1 =  PLD(JJ)
               IF(X1 > ZERO .AND. ICHK == 0 ) THEN
                  ICHK = 1
C                 check of initial rigidity
                  IF(DERI > E0) THEN
                    IFLAG0 = 1
                    E0 = DERI
                    IF(EMAX <E0)EMAX = E0
                  ENDIF
               ENDIF
               IF ( DERI >= EMAX .AND. X0 > ZERO) THEN
                 EPS0 = MIN(EPS0, X0 )
                 IFLAG = 1
                 IF(X0 == EPS0) THEN
                    EPST1 = MIN(EPST1,ABS(EPS0 - Y0/EMAX))
                 ENDIF
               ENDIF
               X0 =  PLD(JJ)
             ENDDO
          ENDIF
      ENDDO ! NLOAD
C      
      IF (IFLAG == 1) THEN
          E0 = MIN(E0, EMAX)
          UPARAM(3) = (EMAX - E0)/EPST1
          UPARAM(4) = EPS0
          CALL ANCMSG(MSGID=864,MSGTYPE=MSGINFO,ANMODE=ANINFO_BLIND_1,
     .                I1=MAT_ID,
     .                C1=TITR,
     .                R1=EPS0)
      ENDIF
      IF (IFLAG0 == 1) THEN
          E0 = MIN(E0, EMAX)
          UPARAM(3) = (EMAX - E0)/EPST1
          UPARAM(2) = E0
          CALL ANCMSG(MSGID=865,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_1,
     .                I1=MAT_ID,
     .                C1=TITR,
     .                R1=E0)
      ENDIF
c--------------------------------------------------------
      RETURN
      END
