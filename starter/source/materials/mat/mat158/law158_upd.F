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
      !||====================================================================
      !||    law158_upd         ../starter/source/materials/mat/mat158/law158_upd.F
      !||--- called by ------------------------------------------------------
      !||    updmat             ../starter/source/materials/updmat.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg             ../starter/source/output/message/message.F
      !||    func_slope         ../starter/source/tools/curve/func_slope.F
      !||    matfun_usr2sys     ../starter/source/materials/tools/matfun_usr2sys.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod        ../starter/share/message_module/message_mod.F
      !||    sensor_mod         ../starter/share/modules1/sensor_mod.F
      !||    table_mod          ../starter/share/modules1/table_mod.F
      !||====================================================================
      SUBROUTINE LAW158_UPD(MAT_PARAM,TITR   ,NPC    ,PLD    ,  
     .                      NFUNC    ,NFUNL  ,IFUNC  ,MAT_ID ,FUNC_ID,
     .                      PM       ,SENSORS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE TABLE_MOD
      USE SENSOR_MOD
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*nchartitle            ,INTENT(IN)    :: TITR
      INTEGER                         ,INTENT(IN)    :: MAT_ID,NFUNC,NFUNL
      INTEGER ,DIMENSION(NFUNC)       ,INTENT(IN)    :: FUNC_ID
      INTEGER ,DIMENSION(NFUNC+NFUNL) ,INTENT(INOUT) :: IFUNC
      INTEGER ,DIMENSION(SNPC)        ,INTENT(IN)    :: NPC
      my_real ,DIMENSION(STF)         ,INTENT(IN)    :: PLD
      my_real ,DIMENSION(NPROPM)      ,INTENT(OUT)   :: PM
      TYPE (SENSORS_)                 ,INTENT(IN)    :: SENSORS
      TYPE(MATPARAM_STRUCT_)          ,INTENT(INOUT) :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ISENS,SENS_ID,FUNC
      my_real KCMAX,KTMAX,GMAX,STIFF,STIFFMIN,STIFFINI,STIFFMAX,STIFFAVG,
     .  YFAC1,YFAC2,YFAC3,KFLEX1,KFLEX2
C=======================================================================
c     Transform FUNC_ID ->  Function number , leakmat only
c     NFUNL = IPM(6)     :  LEAK_MAT functions
c
      KCMAX = ZERO
      IF (NFUNL > 0) THEN
        CALL MATFUN_USR2SYS(TITR,MAT_ID,NFUNL,IFUNC(NFUNC+1),FUNC_ID  )
      ENDIF
c---------------------------------------------------------------
      KFLEX1  = MAT_PARAM%UPARAM(6)
      KFLEX2  = MAT_PARAM%UPARAM(7)
      YFAC1   = MAT_PARAM%UPARAM(12) 
      YFAC2   = MAT_PARAM%UPARAM(13) 
      YFAC3   = MAT_PARAM%UPARAM(14)
     
      SENS_ID = MAT_PARAM%IPARAM(1)
C----------------------------
C     SENSOR NUMBERING CHECK
C----------------------------
      ISENS   = 0
      IF (SENS_ID > 0 ) THEN
        DO I=1,SENSORS%NSENSOR
          IF (SENS_ID == SENSORS%SENSOR_TAB(I)%SENS_ID) THEN
            ISENS = I
            EXIT
          END IF
        ENDDO
        IF (ISENS == 0)  
     .    CALL ANCMSG(MSGID=1240,ANMODE=ANINFO,MSGTYPE=MSGWARNING,
     .          I1=MAT_ID,C1=TITR,I2=ISENS) 
      ENDIF
      MAT_PARAM%IPARAM(1) = ISENS
c---------------------------------------------------------------
c
c     fiber stiffness dir1 (load) 
c
      FUNC = IFUNC(1)                                                            
      IF (FUNC > 0 ) THEN       
                                                  
        CALL FUNC_SLOPE(FUNC,YFAC1,NPC,PLD,STIFFMIN,STIFFMAX,STIFFINI,STIFFAVG)
c
        IF (STIFFMIN <= ZERO) THEN
          CALL ANCMSG(MSGID=1581,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_2,
     .             I1=MAT_ID,
     .             I2=FUNC_ID(IFUNC(1)),
     .             C1=TITR)
        ENDIF
        KCMAX = STIFFMAX
c        
      ENDIF
c
c     fiber stiffness dir2 (load) 
c
      FUNC = IFUNC(2)                                                            
      KTMAX = ZERO
      IF (FUNC > 0 ) THEN                                                         
        CALL FUNC_SLOPE(FUNC,YFAC2,NPC,PLD,STIFFMIN,STIFFMAX,STIFFINI,STIFFAVG)    
c
        IF (STIFFMIN <= ZERO) THEN
          CALL ANCMSG(MSGID=1582 ,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_2,
     .             I1=MAT_ID,
     .             I2=FUNC_ID(IFUNC(2)),
     .             C1=TITR)
        ENDIF
        KTMAX = STIFFMAX
      ENDIF
c
c     shear modulus  (load) 
c
      FUNC = IFUNC(3)                                                            
      GMAX = ZERO
      IF (FUNC > 0 ) THEN                                                         
        CALL FUNC_SLOPE(FUNC,YFAC3,NPC,PLD,STIFFMIN,STIFFMAX,STIFFINI,STIFFAVG)    
c
        IF (STIFFMIN <= ZERO) THEN
          CALL ANCMSG(MSGID=1583 ,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_2,
     .             I1=MAT_ID,
     .             I2=FUNC_ID(IFUNC(3)),
     .             C1=TITR)
        ENDIF
        GMAX  = STIFFMAX
      ENDIF                                       
c
c-------------------------------------------------
c     Flex stiffness dir1
c
      FUNC = IFUNC(4)                                                            
      IF (FUNC > 0 )THEN                                                         
                                                      
        CALL FUNC_SLOPE(FUNC,KFLEX1,NPC,PLD,STIFFMIN,STIFFMAX,STIFFINI,STIFFAVG)    
c
        IF (STIFFMIN <= ZERO) THEN
          CALL ANCMSG(MSGID=1581 , MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_2,
     .             I1=MAT_ID,
     .             I2=FUNC_ID(IFUNC(4)),
     .             C1=TITR)
        ENDIF
      ENDIF                                                                     
c
c     Flex stiffness dir2
c
      FUNC = IFUNC(5)                                                            
      IF (FUNC > 0 )THEN                                                         
        CALL FUNC_SLOPE(FUNC,KFLEX2,NPC,PLD,STIFFMIN,STIFFMAX,STIFFINI,STIFFAVG)    
c
        IF (STIFFMIN <= ZERO) THEN
          CALL ANCMSG(MSGID=1582 , MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_2,
     .             I1=MAT_ID,
     .             I2=FUNC_ID(IFUNC(5)),
     .             C1=TITR)
        ENDIF
      ENDIF                                                                     
c-----------
      STIFF = MAX(KCMAX,KTMAX)
      MAT_PARAM%UPARAM(10) = STIFF
      MAT_PARAM%UPARAM(11) = GMAX
c-----------
      PM(20) = STIFF        ! Stiffness contact
      PM(21) = ZERO         ! NU
      PM(22) = STIFF*HALF   ! GMAX
      PM(23) = EM01*STIFFAVG  ! Hourglass stiffness for QEPH
      PM(24) = STIFF        ! Stiffness for time step computation
c-----------
      RETURN
      END
