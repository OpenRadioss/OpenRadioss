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
Chd|====================================================================
Chd|  LAW58_UPD                     source/materials/mat/mat058/law58_upd.F
Chd|-- called by -----------
Chd|        UPDMAT                        source/materials/updmat.F     
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FUNC_INTERS                   source/tools/curve/func_inters.F
Chd|        FUNC_INTERS_SHEAR             source/tools/curve/func_inters.F
Chd|        FUNC_SLOPE                    source/tools/curve/func_slope.F
Chd|        MATFUN_USR2SYS                source/materials/tools/matfun_usr2sys.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE LAW58_UPD(IOUT   ,TITR   ,UPARAM ,NPC    ,PLD    ,  
     .                     NFUNC  ,NFUNL  ,IFUNC  ,MAT_ID ,FUNC_ID,
     .                     PM     ,NUPARAM,SENSORS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE TABLE_MOD
      USE SENSOR_MOD
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
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*nchartitle  :: TITR
      INTEGER MAT_ID,IOUT,NFUNC,NFUNL,NUPARAM,NPTS
      INTEGER NPC(SNPC), FUNC_ID(NFUNCT),IFUNC(NFUNC+NFUNL)
      my_real UPARAM(NUPARAM),PLD(STF),PM(NPROPM)
      TYPE (SENSORS_) ,INTENT(IN) :: SENSORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,FUNC,FUND,UNLOAD,PN,IOK,ISENS,SENS_ID
      my_real KC,KT,KCMAX,KTMAX,KFC,KFT,GMAX,DERI,STIFF,STIFFMIN,STIFFINI,
     .  STIFFMAX,STIFFAVG,XINT1,YINT1,XINT2,YINT2,FAC,FAC1,FAC2,GFROT,GSH
C=======================================================================
c     Transform FUNC_ID ->  Function number , leakmat only
c     NFUNL = IPM(6)     :  LEAK_MAT functions
c
      IF (NFUNL > 0) THEN
        CALL MATFUN_USR2SYS(TITR,MAT_ID,NFUNL,IFUNC(NFUNC+1),FUNC_ID  )
      ENDIF
c
C----------------------------
C     SENSOR NUMBERING CHECK
C----------------------------
      SENS_ID = UPARAM(25)
      ISENS   = 0
      IF (SENS_ID > 0 ) THEN
        DO I=1,SENSORS%NSENSOR
          IF (SENS_ID == SENSORS%SENSOR_TAB(I)%SENS_ID) THEN
            ISENS = I
            UPARAM(25) = ISENS
            EXIT
          END IF
        ENDDO
        IF (ISENS == 0)  
     .    CALL ANCMSG(MSGID=1240,ANMODE=ANINFO,MSGTYPE=MSGWARNING,
     .          I1=MAT_ID,C1=TITR,I2=ISENS) 
      ENDIF
c---------------------------------------------------------------
      KC   = ZERO
      KT   = ZERO
      KFC  = ZERO
      KFT  = ZERO
      KCMAX= ZERO
      KTMAX= ZERO
      GMAX = ZERO
c
c     fiber stiffness dir1 (load) 
c
      FUNC = IFUNC(1)                                                            
      IF (FUNC > 0 ) THEN       
                                                  
        FAC = UPARAM(28)                                                     
        CALL FUNC_SLOPE(FUNC,FAC,NPC,PLD,STIFFMIN,STIFFMAX,STIFFINI,STIFFAVG)
c
        IF (STIFFMIN <= ZERO) THEN
          CALL ANCMSG(MSGID=1581 ,
     .             MSGTYPE=MSGERROR,
     .             ANMODE=ANINFO_BLIND_2,
     .             I1=MAT_ID,
     .             I2=FUNC_ID(IFUNC(1)),
     .             C1=TITR)
        ENDIF
        KC  = MAX(KC ,STIFFMAX)
        KFC = MAX(KFC,STIFFINI)
        KCMAX = KC
        UPARAM(40) = STIFFINI
c        
      ENDIF
c
c     fiber stiffness dir2 (load) 
c
      FUNC = IFUNC(2)                                                            
      IF (FUNC > 0 ) THEN                                                         
        FAC = UPARAM(29)                                                     
        CALL FUNC_SLOPE(FUNC,FAC,NPC,PLD,STIFFMIN,STIFFMAX,STIFFINI,STIFFAVG)    
c
        IF (STIFFMIN <= ZERO) THEN
          CALL ANCMSG(MSGID=1582 ,
     .             MSGTYPE=MSGERROR,
     .             ANMODE=ANINFO_BLIND_2,
     .             I1=MAT_ID,
     .             I2=FUNC_ID(IFUNC(2)),
     .             C1=TITR)
        ENDIF
        KT  = MAX(KT ,STIFFMAX)
        KFT = MAX(KFT,STIFFINI)
        KTMAX = KT
        UPARAM(41) = STIFFINI
      ENDIF
c
c     shear modulus  (load) 
c
      FUNC = IFUNC(3)                                                            
      IF (FUNC > 0 ) THEN                                                         

        FAC  = UPARAM(30)
        CALL FUNC_SLOPE(FUNC,FAC,NPC,PLD,STIFFMIN,STIFFMAX,STIFFINI,STIFFAVG)    
c
        IF (STIFFMIN <= ZERO) THEN
          CALL ANCMSG(MSGID=1583 ,
     .             MSGTYPE=MSGERROR,
     .             ANMODE=ANINFO_BLIND_2,
     .             I1=MAT_ID,
     .             I2=FUNC_ID(IFUNC(3)),
     .             C1=TITR)
        ENDIF
        GMAX  = MAX(GMAX,STIFFMAX)
        GFROT = STIFFINI
        GSH   = STIFFINI
        IF (UPARAM(21) == ZERO) UPARAM(21) = GFROT
        IF (UPARAM(32) == ZERO) UPARAM(32) = GSH
        
      ENDIF                                       
c
      UNLOAD = NINT(UPARAM(35))
c-------------------------------------------------
      IF (UNLOAD == 1) THEN  ! hystheresis / unload option
c-------------------------------------------------
c
c       Fiber stiffness dir1 (unload)
c
        FUNC = IFUNC(4)                                                            
        IF (FUNC > 0 )THEN                                                         
          FAC  = UPARAM(33)                                                     
          CALL FUNC_SLOPE(FUNC,FAC,NPC,PLD,STIFFMIN,STIFFMAX,STIFFINI,STIFFAVG)    
          KCMAX = MAX(KCMAX,STIFFMAX)
c
          IF (STIFFMIN <= ZERO) THEN
            CALL ANCMSG(MSGID=1581 ,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               I1=MAT_ID,
     .               I2=FUNC_ID(IFUNC(4)),
     .               C1=TITR)
          ENDIF
        ENDIF                                                                     
c
c       Fiber stiffness dir2 (unload)
c
        FUNC = IFUNC(5)                                                            
        IF (FUNC > 0 )THEN                                                         
          FAC  = UPARAM(34)                                                     
          CALL FUNC_SLOPE(FUNC,FAC,NPC,PLD,STIFFMIN,STIFFMAX,STIFFINI,STIFFAVG)    
          KTMAX = MAX(KTMAX,STIFFMAX)                                          
c
          IF (STIFFMIN <= ZERO) THEN
            CALL ANCMSG(MSGID=1582 ,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               I1=MAT_ID,
     .               I2=FUNC_ID(IFUNC(5)),
     .               C1=TITR)
          ENDIF
        ENDIF                                                                     
c
c       shear modulus  (unload) 
c
        FUNC = IFUNC(6)                                                          
        IF (FUNC > 0 )THEN                                                       
          FAC = UPARAM(42)
          CALL FUNC_SLOPE(FUNC,FAC,NPC,PLD,STIFFMIN,STIFFMAX,STIFFINI,STIFFAVG)  
          GMAX = MAX(GMAX,STIFFMAX)
c
          IF (STIFFMIN <= ZERO) THEN
            CALL ANCMSG(MSGID=1583 ,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               I1=MAT_ID,
     .               I2=FUNC_ID(IFUNC(6)),
     .               C1=TITR)
          ENDIF                                       
        ENDIF                                       
c
c       Intersection - direction chaine : load = ifunc(1), unload = ifunc(4)
c
        FUNC = IFUNC(1)
        FUND = IFUNC(4)
c
        IF (FUNC == 0) THEN
          UPARAM(36) = INFINITY    
          UPARAM(37) = INFINITY    
        ELSEIF (FUNC == FUND) THEN
          PN = NPC(FUNC+1)
          UPARAM(36) = PLD(PN - 2)    
          UPARAM(37) = PLD(PN - 1)
        ELSE
          FAC1 = UPARAM(28)
          FAC2 = UPARAM(33)
          CALL FUNC_INTERS(TITR   ,MAT_ID ,FUNC   ,FUND   ,FAC1   ,
     .                     FAC2   ,NPC    ,PLD    ,XINT1  ,YINT1  )
          IF (XINT1 == ZERO .or. YINT1 == ZERO) THEN
            CALL ANCMSG(MSGID=1716 ,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_2,
     .                  I1 = MAT_ID,
     .                  I2 = FUNC_ID(FUNC),                                               
     .                  I3 = FUNC_ID(FUND),                                               
     .                  C1 = TITR  )
          ENDIF 
          UPARAM(36) = XINT1    
          UPARAM(37) = YINT1
        ENDIF    
c
c       Intersection - direction trame : load = ifunc(2), unload = ifunc(5)
c
        FUNC = IFUNC(2)
        FUND = IFUNC(5)
c
        IF (FUNC == 0) THEN
          UPARAM(38) = INFINITY    
          UPARAM(39) = INFINITY
        ELSEIF (FUNC == FUND) THEN
          PN = NPC(FUNC+1)
          UPARAM(38) = PLD(PN - 2)    
          UPARAM(39) = PLD(PN - 1)
        ELSE
          FAC1 = UPARAM(29)
          FAC2 = UPARAM(34)
          CALL FUNC_INTERS(TITR   ,MAT_ID ,FUNC   ,FUND   ,FAC1   ,
     .                     FAC2   ,NPC    ,PLD    ,XINT2  ,YINT2  )
          IF (XINT1 == ZERO .or. YINT1 == ZERO) THEN
            CALL ANCMSG(MSGID=1716 ,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_2,
     .                  I1 = MAT_ID,
     .                  I2 = FUNC_ID(FUNC),                                               
     .                  I3 = FUNC_ID(FUND),                                               
     .                  C1 = TITR  )
          ENDIF 
          UPARAM(38) = XINT2    
          UPARAM(39) = YINT2    
        ENDIF    
c
c       Intersection - Shear : load = ifunc(3), unload = ifunc(6)
c
        FUNC = IFUNC(3)
        FUND = IFUNC(6)
c
        IF (FUNC /= FUND) THEN
          FAC1 = UPARAM(30)
          FAC2 = UPARAM(42)
          CALL FUNC_INTERS_SHEAR(
     .         TITR   ,MAT_ID ,FUNC   ,FUND   ,FAC1   ,FAC2   ,
     .         NPC    ,PLD    ,XINT1  ,YINT1  ,XINT2  ,YINT2  )
c
          IF ((XINT1 == ZERO .or. YINT1 == ZERO .or. 
     .         XINT2 == ZERO .or. YINT2 == ZERO) .or. 
     .         XINT1 * XINT2 > 0) THEN
            CALL ANCMSG(MSGID=1716 ,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_2,
     .                  I1 = MAT_ID,
     .                  I2 = FUNC_ID(FUNC),                                               
     .                  I3 = FUNC_ID(FUND),                                               
     .                  C1 = TITR  )
          ENDIF 
c          
          UPARAM(43) = XINT1
          UPARAM(44) = YINT1
          UPARAM(45) = XINT2
          UPARAM(46) = YINT2
        ELSEIF (FUNC > 0) THEN
          PN = NPC(FUNC)
          UPARAM(43) = PLD(PN)
          UPARAM(44) = PLD(PN+1)
          PN = NPC(FUNC+1)
          UPARAM(45) = PLD(PN - 2)
          UPARAM(46) = PLD(PN - 1)
        ENDIF    
c-----------
      ENDIF  ! UNLOAD
c-----------
      IF (KCMAX > 0 ) KCMAX = KCMAX * TWO
      IF (KTMAX > 0 ) KTMAX = KTMAX * TWO
      IF (GMAX  > 0 ) GMAX  = GMAX  * TWO
      KCMAX = MAX(UPARAM(9) , KCMAX) 
      KTMAX = MAX(UPARAM(10), KTMAX) 
      GMAX  = MAX(UPARAM(14), GMAX)  
      UPARAM(9)  = KCMAX
      UPARAM(10) = KTMAX
      UPARAM(14) = GMAX
c
      STIFF = MAX(KCMAX,KTMAX)
c
      PM(20) = STIFF        ! Stiffness contact
      PM(21) = ZERO         ! NU
      PM(22) = STIFF*HALF   ! GMAX
      PM(24) = STIFF        ! Stiffness for time step computation
C---- for QEPH (hourglass) :
      STIFFAVG = MAX(KC,KT)
      IF ( STIFFAVG > ZERO) PM(23) = EM01*STIFFAVG
c-----------
      RETURN
      END
