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
C|============================================================
C|  M75INIT                                 /mate751/m75init.F
C|------------------------------------------------------------
C|-- appelee par -----------
C|         MATINI                           /mater1/matini.F
C|-- appelle ---------------
C|============================================================
Chd|====================================================================
Chd|  M75INIT                       source/materials/mat/mat075/m75init.F
Chd|-- called by -----------
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE M75INIT(
     1      NEL    , NUPARAM, NUVAR   , NFUNC  , IFUNC  , NPF   ,
     2      TF     , UPARAM , RHO0    , VOLUME , EINT   ,UVAR   ,
     3      PM     , IPM    ,ID       ,TITR) 
      USE MESSAGE_MOD
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C NFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW
C IFUNC   | NFUNC   | I | R | FUNCTION INDEX 
C NPF     |  *      | I | R | FUNCTION ARRAY   
C TF      |  *      | F | R | FUNCTION ARRAY 
C---------+---------+---+---+--------------------------------------------
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C RHO0    | NEL     | F | R | INITIAL DENSITY
C VOLUME  | NEL     | F | R | VOLUME
C EINT    | NEL     | F | R | TOTAL INTERNAL ENERGY
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER       NEL, NUPARAM, NUVAR
      my_real
     .      UPARAM(NUPARAM), RHO0(NEL), VOLUME(NEL), EINT(NEL),
     .      PM(NPROPM,*), IPM(NPROPMI,*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C----------------------------------------------------------------
C  I N P U T  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real
     . UVAR(NEL,NUVAR)
C----------------------------------------------------------------
C  VARIABLES FOR FUNCTION INTERPOLATION 
C----------------------------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real
     .   FINTER,TF(*)
      EXTERNAL FINTER
C----------------------------------------------------------------
C  L O C A L  V A R I A B L E S
C----------------------------------------------------------------
      INTEGER  I, IMAT
C     REAL ou REAL*8
      my_real
     .        CE, PE, PS, EN, RHO0S, ALPHAE, BULKS, C0, BETA, AA,
     .        DALPDPE, ALPHAP, BULK
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      BULK = UPARAM(1)
      PE   = UPARAM(2)
      PS   = UPARAM(3)
      EN   = UPARAM(4)
      IMAT = NINT(UPARAM(6))
      CE   = SQRT(BULK/RHO0(1))
C
      IF(IMAT==0) THEN
        ALPHAE=ZERO
        ALPHAP=ZERO
        AA=ZERO
      ELSE
        RHO0S =PM(89,IMAT)
        ALPHAE=RHO0S/RHO0(1)
        BULKS= PM(32,IMAT)
        C0   = SQRT(BULKS/RHO0S)
        BETA=CE/C0
        AA=(BETA-ONE)/(ALPHAE-ONE)
        DALPDPE=(ONE/BULKS-ONE/BULK)*ALPHAE
        ALPHAP=ALPHAE+PE*DALPDPE
C
        IF (ALPHAE < ONE) THEN
           CALL ANCMSG(MSGID=898,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 I1=ID,
     .                 C1=TITR)
        ENDIF
        IF (BULKS < BULK) THEN
           CALL ANCMSG(MSGID=1003,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 I1=ID,
     .                 C1=TITR)
        ENDIF
      ENDIF
C-----------
C INITIALIZE
C-----------
      UPARAM(11)=ALPHAE
      UPARAM(12)=ALPHAP
      UPARAM(13)=AA
      UPARAM(14)=ONE/EN
      UPARAM(15)=(PS-PE)/(ALPHAP-ONE)**(ONE/EN)
      UPARAM(16)=(ALPHAP-ONE)/(PS-PE)**EN
C
      DO I=1,NEL                    
        UVAR(I,1)=ZERO
        UVAR(I,2)=ZERO
        UVAR(I,3)=ALPHAE
        UVAR(I,4)=ALPHAP
      ENDDO
C         
      RETURN
      END
