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
Chd|  LAW114_UPD                    source/materials/mat/mat114/law114_upd.F
Chd|-- called by -----------
Chd|        UPDMAT                        source/materials/updmat.F     
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE LAW114_UPD(IOUT   ,TITR   ,UPARAM ,NPC    ,PLD    ,  
     .                     NFUNC  ,IFUNC  ,MAT_ID ,FUNC_ID,
     .                     PM     )
      USE MESSAGE_MOD
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*nchartitle  :: TITR
      INTEGER MAT_ID,IOUT,NFUNC
      INTEGER NPC(*), FUNC_ID(*),IFUNC(NFUNC)
      my_real UPARAM(*),PLD(*),PM(NPROPM)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER FUNC,NPT, J,J1,IF3,I7,I11,I13,FLAG_NEGATIVE,FUNC_UL,
     .        NPT_UL,J1_UL,J_UL,NEXT
      my_real
     .        XK,HARD,X1,X2,Y1,Y2,LSCALE,XK_INI,DERI,H,E_OFFSET,
     .        X1_UL,X2_UL,Y1_UL,Y2_UL,DERI_UL,Y,Y_UL,EPS,Y_EPS
      CHARACTER*nchartitle  :: TITR1
C=======================================================================
c     Transform FUNC_ID ->  Function number , leakmat only
C
C     MAT_LAW114 - only Func1 and Func3 can be set on tension
C
      I7  = 40  ! 4 + 6*6
      I11 = 64  ! 4 + 10*6
      I13 = 76  ! 4 + 12*6
      LSCALE = UPARAM(I7 + 1)
      XK   = UPARAM(I11 + 1) 
      HARD = UPARAM(I13 + 1)
      XK_INI = XK
      E_OFFSET = ZERO

c---------------------------------------------------------------
c     traction loading curve
c---------------------------------------------------------------

      FLAG_NEGATIVE = 0
      FUNC = IFUNC(1)
      IF (FUNC > 0 ) THEN     
        NPT=(NPC(FUNC+1)-NPC(FUNC))/2
        DO  J=2,NPT
           J1 =2*(J-2)
           X1 = PLD(NPC(FUNC)  + J1)
           Y1 = PLD(NPC(FUNC)  + J1 + 1)
           X2 = PLD(NPC(FUNC)  + J1 + 2)
           Y2 = PLD(NPC(FUNC)  + J1 + 3)
           XK = MAX(XK,LSCALE*(Y2 - Y1)/(X2 - X1))
           IF ((X1 < 0).AND.(Y1 /= 0)) FLAG_NEGATIVE = 1
           IF ((X2 < 0).AND.(Y2 /= 0)) FLAG_NEGATIVE = 1
           IF ((Y2 > 0).AND.(Y1 < 0)) THEN
             E_OFFSET = X1 - Y1*(X2 - X1)/(Y2 - Y1)
           ENDIF
        ENDDO
        IF(FLAG_NEGATIVE > 0)THEN
          CALL ANCMSG(MSGID=1914, ! 
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=MAT_ID,
     .                  C1=TITR,
     .                  I2=NPC(NFUNCT+FUNC+1))
        ENDIF
        UPARAM(I11 + 1)= XK
C-- compression offset
        UPARAM(118)= E_OFFSET
      ENDIF
c
c---------------------------------------------------------------
c     traction unloading curve
c---------------------------------------------------------------
C
      FLAG_NEGATIVE = 0
      IF3 = 12
      FUNC_UL = IFUNC(IF3+1)
      IF (FUNC_UL  > 0 ) THEN     
        NPT=(NPC(FUNC_UL +1)-NPC(FUNC_UL ))/2
        DO  J=2,NPT
           J1 =2*(J-2)
           X1 = PLD(NPC(FUNC_UL )  + J1)
           Y1 = PLD(NPC(FUNC_UL )  + J1 + 1)
           X2 = PLD(NPC(FUNC_UL )  + J1 + 2)
           Y2 = PLD(NPC(FUNC_UL )  + J1 + 3)
           XK = MAX(XK,LSCALE*(Y2 - Y1)/(X2 - X1))
           IF ((X1 < 0).AND.(Y1 /= 0)) FLAG_NEGATIVE = 1
           IF ((X2 < 0).AND.(Y2 /= 0)) FLAG_NEGATIVE = 1
        ENDDO
        IF(FLAG_NEGATIVE > 0)THEN
          CALL ANCMSG(MSGID=1915, ! 
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=MAT_ID,
     .                  C1=TITR,
     .                  I2=NPC(NFUNCT+FUNC_UL+1))
        ENDIF
        UPARAM(I11 + 1)= MAX(XK,UPARAM(I11 + 1))  
      ENDIF
C
      IF (IFUNC(1) > 0) THEN
        IF ((XK_INI<XK).AND.(XK_INI > ZERO)) THEN
          CALL ANCMSG(MSGID=1640, ! 
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=MAT_ID,
     .                  C1=TITR,
     .                  I2=NPC(NFUNCT+FUNC_UL+1),
!!     .                  C2=TITR1,
     .                  R1=XK_INI,
     .                  R2=XK,
     .                  R3=XK)
        ENDIF
      ENDIF
C
c---------------------------------------------------------------
c     detection of first crossing point between loading/unloading curve
c---------------------------------------------------------------
C
       FUNC = IFUNC(1)
       IF3 = 12
       FUNC_UL = IFUNC(IF3+1)
       Y_EPS = ZERO
C
       IF ((FUNC > 0).AND.(FUNC_UL > 0).AND.(FUNC_UL /=  FUNC)) THEN
C
         NPT=(NPC(FUNC+1)-NPC(FUNC))/2
         NPT_UL=(NPC(FUNC_UL+1)-NPC(FUNC_UL))/2
C
         NEXT = 1
         J_UL = 2
         J = 2
C
         DO WHILE (NEXT > 0)
C
           NEXT = 0
C
           J1 =2*(J-2)
           X1 = PLD(NPC(FUNC)  + J1)
           Y1 = PLD(NPC(FUNC)  + J1 + 1)
           X2 = PLD(NPC(FUNC)  + J1 + 2)
           Y2 = PLD(NPC(FUNC)  + J1 + 3)
           DERI = (Y2 - Y1)/(X2 - X1)
C
           J1_UL =2*(J_UL-2)
           X1_UL = PLD(NPC(FUNC_UL)  + J1_UL)
           Y1_UL = PLD(NPC(FUNC_UL)  + J1_UL + 1)
           X2_UL = PLD(NPC(FUNC_UL)  + J1_UL + 2)
           Y2_UL = PLD(NPC(FUNC_UL)  + J1_UL + 3)
           DERI_UL = (Y2_UL - Y1_UL)/(X2_UL - X1_UL)
C
           IF (X2_UL > X2) THEN
             Y_UL = Y1_UL + DERI_UL*(X2-X1_UL)
             IF (Y_UL < Y2) THEN
               J = J + 1
               NEXT = 1
             ELSEIF (ABS(DERI_UL-DERI) > EM20) THEN
               EPS = (Y1-Y1_UL-DERI*X1+DERI_UL*X1_UL)/(DERI_UL-DERI)
               Y_EPS = Y1 + DERI*(EPS-X1)
             ELSE
               EPS = MAX(Y1,Y1_UL)
               Y_EPS = Y1 + DERI*(EPS-X1)
             ENDIF
           ELSE
             Y = Y1 + DERI*(X2_UL-X1)
             IF (Y > Y2_UL) THEN
               J_UL = J_UL + 1
               NEXT = 1
             ELSEIF (ABS(DERI_UL-DERI) > EM20) THEN
               EPS = (Y1-Y1_UL-DERI*X1+DERI_UL*X1_UL)/(DERI_UL-DERI)
               Y_EPS = Y1 + DERI*(EPS-X1)
             ELSE
               EPS = MAX(Y1,Y1_UL)
               Y_EPS = Y1 + DERI*(EPS-X1)
             ENDIF
           ENDIF
C
         ENDDO
C
       ENDIF
C
       UPARAM(125)= Y_EPS           
C          
c-----------
      RETURN
      END
