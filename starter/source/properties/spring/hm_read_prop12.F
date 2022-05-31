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
Chd|  HM_READ_PROP12                source/properties/spring/hm_read_prop12.F
Chd|-- called by -----------
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_CRYPTED          source/devtools/hm_reader/hm_option_is_crypted.F
Chd|        NL                            source/output/message/message.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROP12(GEO, IGEO, UNITAB, IGTYP, IG,
     .                          PROP_TAG,IDTITL,LSUBMODEL)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE ELBUFTAG_MOD   
      USE SUBMODEL_MOD     
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
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "warn_c.inc"
#include      "tablen_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER  IGEO(NPROPGI),IGTYP,IG
C     REAL ou REAL*8
      my_real GEO(NPROPG)
      CHARACTER*nchartitle,
     .   IDTITL
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER J, IFUNC, IFUNC2, IFUNC3, IECROU, IFV, ISENS,IFL,
     .         ILENG,FTAB_ID,IFRIC
C     REAL ou REAL*8
      my_real
     .  A, B, D, E, F, DN, DX, FRIC, LSCALE, FSCALE, RSCALE, PUN, GF3,
     .  YSCALEF,XSCALEF,FMAX,FMIN
      my_real
     .   A_UNIT,D_UNIT,E_UNIT,F_UNIT,LSCALE_UNIT,GF3_UNIT,RUP_UNIT,XSCALE_UNIT,FMIN_UNIT
      CHARACTER*nchartitle,
     .   SLASH
      LOGICAL IS_AVAILABLE, IS_CRYPTED
C=======================================================================
c
      PUN = EM01
C
      IS_CRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.

C     Double stockage temporaire - supprimer GEO(12,I)=IGTYP apres tests
      IGEO( 1)=IG
      IGEO(11)=IGTYP
      GEO(12) =IGTYP+PUN
C
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_CRYPTED(IS_CRYPTED)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('ISENSOR',ISENS,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('ISFLAG',IFL,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ileng',ILENG,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_A1',IFUNC,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('HFLAG1',IECROU,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_B1',IFV,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('fct_ID31',IFUNC2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_A2',IFUNC3,IS_AVAILABLE,LSUBMODEL)
C--optional-
      CALL HM_GET_INTV('FUNCT_ID',FTAB_ID,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('P12_SPR_PUL_Ifric',IFRIC,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('MASS',GEO(1),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('FRIC',FRIC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('STIFF1',GEO(2),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('DAMP1',GEO(3),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Acoeft1',A,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Bcoeft1',B,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Dcoeft1',D,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MIN_RUP1',DN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAX_RUP1',DX,IS_AVAILABLE,LSUBMODEL,UNITAB) 
      CALL HM_GET_FLOATV('Prop_X_F',F,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Prop_X_E',E,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('scale1',LSCALE,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('h',GF3,IS_AVAILABLE,LSUBMODEL,UNITAB)
C--optional-
      CALL HM_GET_FLOATV('scale2',YSCALEF,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('scale3',XSCALEF,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('P12_SPR_PUL_F_min',FMIN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('P12_SPR_PUL_F_max',FMAX,IS_AVAILABLE,LSUBMODEL,UNITAB)
c            
      CALL HM_GET_FLOATV_DIM('Acoeft1',A_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('Dcoeft1',D_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('h',GF3_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('Prop_X_F',F_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('Prop_X_E',E_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('scale1',LSCALE_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('MIN_RUP1',RUP_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('scale3',XSCALE_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('P12_SPR_PUL_F_min',FMIN_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB) 
C----                             
      IF(GEO(2).EQ.ZERO.AND.GEO(3).EQ.ZERO.AND.IFUNC.EQ.ZERO) THEN 
         CALL ANCMSG(MSGID=602,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      END IF                                                       
      IF(GEO(1).LE.EM15)THEN                                       
         CALL ANCMSG(MSGID=229,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF                                                        
!      IF(IFUNC.NE.0.AND.IECROU.GE.1.AND.GEO(2).EQ.ZERO)THEN        
!         CALL ANCMSG(MSGID=230,
!     .               MSGTYPE=MSGERROR,
!     .               ANMODE=ANINFO_BLIND_1,
!     .               I1=IG,
!     .               C1=TITR)
!      ENDIF                                                        
      IF(IECROU.EQ.4.AND.(IFUNC.EQ.0.OR.IFUNC2.EQ.0))THEN          
         CALL ANCMSG(MSGID=231,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF                                                        
      IF(IECROU.EQ.4.AND.GEO(2).EQ.ZERO)THEN                        
         CALL ANCMSG(MSGID=230,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF                                                        
      IF(IECROU.EQ.5.AND.(IFUNC.EQ.0.OR.IFUNC2.EQ.0))THEN          
         CALL ANCMSG(MSGID=231,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF                                                        
       IF(IECROU.EQ.6.AND.(IFUNC.EQ.0.OR.IFUNC2.EQ.0))THEN
          CALL ANCMSG(MSGID=1057,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=IG,
     .                C1=IDTITL)
       ENDIF
       IF(IECROU.EQ.7.AND.IFUNC==0)THEN
          CALL ANCMSG(MSGID=1058,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=IG,
     .                C1=IDTITL)
       
       ELSEIF(IECROU.EQ.7.AND.IFUNC2.EQ.0)THEN
          CALL ANCMSG(MSGID=1059,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=IG,
     .                C1=IDTITL,
     .                I2=IECROU)
         IECROU = 2
       ENDIF
      IF (IFUNC == 0 .AND. A /= ZERO .AND. A /= ONE) THEN
        CALL ANCMSG(MSGID=663,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF                                        
C----
      IF (A == ZERO) A = ONE * A_UNIT
      IF (D == ZERO) D = ONE * D_UNIT
      IF (E == ZERO) E = ONE * E_UNIT
      IF (F == ZERO) F = ONE * F_UNIT
      IF (LSCALE == ZERO) LSCALE = ONE * LSCALE_UNIT
      IF (GF3 == ZERO) GF3 = ONE * GF3_UNIT
      IF (IFUNC == 0) THEN
        A = ONE
        B = ZERO
        E = ZERO
      ENDIF
      IF (IFL == 1) ISENS=-ISENS
C
      IF (DN == ZERO) DN=-EP30
      IF (DX == ZERO) DX= EP30
      DN = DN * LSCALE / RUP_UNIT
      DX = DX * LSCALE / RUP_UNIT
c
      IF (XSCALEF == ZERO) XSCALEF = ONE * XSCALE_UNIT
      IF (YSCALEF == ZERO) YSCALEF = ONE
      IF (FMIN == ZERO) FMIN = -EP30
      IF (FMAX == ZERO) FMAX =  EP30
C------------------------
      IF(IS_CRYPTED)THEN                                    
        WRITE(IOUT,1000)IG                                     
      ELSE                                                     
       IF (IECROU.NE.5) THEN                                   
        WRITE(IOUT,1400)IG,(GEO(J),J=1,3),IFUNC,LSCALE,IFUNC2,F,IECROU, 
     .                A,B,D,E,IFV,GF3,IFUNC3,DN,DX,ISENS,IFL,ILENG,FRIC,   
     .                FTAB_ID,IFRIC,YSCALEF,XSCALEF,FMIN,FMAX
       ELSE                                                    
        WRITE(IOUT,1500)IG,(GEO(J),J=1,3),IFUNC,LSCALE,IFUNC2,F,IECROU, 
     .                A,B,D,E,IFV,GF3,IFUNC3,DN,DX,ISENS,IFL,ILENG,FRIC,
     .                FTAB_ID,IFRIC,YSCALEF,XSCALEF,FMIN,FMAX
       ENDIF                                                   
      ENDIF                                               
C----
cmi      IF (FTAB_ID > 0) FRIC = YSCALEF
C----
      GEO(2)  = GEO(2) / A
      GEO(7)  = IECROU+PUN                                   
      GEO(8)  = 3                                            
      GEO(9)  = ZERO                                         
      GEO(10) = A                                            
      GEO(11) = B                                            
      GEO(13) = D                                            
      GEO(18) = ONE / F             
      GEO(39) = ONE / LSCALE 
      GEO(40) = E                         
      GEO(132)= GF3                       
      GEO(15) = DN                                           
      GEO(16) = DX                                           
      GEO(17) = FRIC                                         
      GEO(20) = ONE/XSCALEF            
      GEO(80) = IFL                                          
      GEO(93) = ILENG                                        
      GEO(138) = FMIN
      GEO(139) = FMAX
      GEO(140) = YSCALEF
      IGEO(3) = ISENS
C--   If H=6 - addditional internal variables must be stored in UVAR -  - GEO(25) = NUVAR
      IF (IECROU == 6) THEN
        GEO(25) = 1
      ENDIF
C                         
      IGEO(101) = IFUNC                                      
      IGEO(102) = IFV                                        
      IGEO(103) = IFUNC2                                     
      IGEO(201) = FTAB_ID
      IGEO(119) = IFUNC3
      IGEO(202) = IFRIC
C
C-----------------------------
C       PROPERTY BUFFER 
C-----------------------------
C
      PROP_TAG(IGTYP)%G_EINT = 1
      PROP_TAG(IGTYP)%G_FOR  = 1
      PROP_TAG(IGTYP)%G_LENGTH = 1
      PROP_TAG(IGTYP)%G_TOTDEPL = 1
      PROP_TAG(IGTYP)%G_FOREP = 1
      PROP_TAG(IGTYP)%G_DEP_IN_TENS = 1
      PROP_TAG(IGTYP)%G_DEP_IN_COMP = 1
      PROP_TAG(IGTYP)%G_POSX = 5
      PROP_TAG(IGTYP)%G_YIELD = 1
      PROP_TAG(IGTYP)%G_LENGTH_ERR = 1
      PROP_TAG(IGTYP)%G_DFS = 1
      PROP_TAG(IGTYP)%G_INIFRIC = 1
      PROP_TAG(IGTYP)%G_NUVAR = MAX(PROP_TAG(IGTYP)%G_NUVAR,NINT(GEO(25))) ! addtional internal variables for h=6
      PROP_TAG(IGTYP)%G_DEFINI = 1
      PROP_TAG(IGTYP)%G_FORINI = 1
C
C------------------------
 1000 FORMAT(
     &    5X,'SPRING PROPERTY SET'/,
     &    5X,'-------------------'/,
     &    5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     &    5X,'CONFIDENTIAL DATA'//)
 1400 FORMAT(
     & 5X,'SPRING PROPERTY SET (3 NODES PULLEY)'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'SPRING MASS . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SPRING STIFFNESS. . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SPRING DAMPING. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR LOADING ',/,
     & 5X,'FORCE-DISPLACEMENT CURVE. . . . . . . .=',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR UNLOADING ',/,
     & 5X,'FORCE-DISPLACEMENT CURVE (H=4,5,7). . .=',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'HARDENING FLAG H. . . . . . . . . . . .=',I10/,
     & 5X,'0:ELASTIC   1:ISOTROPIC 2:UNCOUPLED',/,
     & 5X,'4:KINEMATIC 5:UNCOUPLED NL (ONE/RE)LOADING',/,
     & 5X,'6:ELASTO PLASTIC WITH HARDENING 7: ELASTIC HYSTERESIS',/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR A. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR B. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR D. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR E. . . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR  ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR GF3. . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR THE ADDITIONAL ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'NEGATIVE FAILURE DISPLACEMENT . . . . .=',1PG20.13/,
     & 5X,'POSITIVE FAILURE DISPLACEMENT . . . . .=',1PG20.13/,
     & 5X,'SENSOR NUMBER (0:NOT USED). . . . . . .=',I10/,
     & 5X,'SENSOR FLAG (0:ACTIV 1:DEACT 2:BOTH). .=',I10/,
     & 5X,'UNIT LENGTH FLAG. . . . . . . . . . . .=',I10/,
     & 5X,'IF=1 UNIT LENGTH MASS,STIFFNESS AND INPUT',/,
     & 5X,'     CURVE ARE STRAIN DEPENDING',/,
     & 5X,'CONSTANT PULLEY FRICTION COEFFICIENT. .=',1PG20.13/
     & 5X,'TABLE ID OF VARIABLE FRICTION FUNCTIONS=',I10/,
     & 5X,'FRICTION FLAG . . . . . . . . . . . . .=',I10/,
     & 5X,'Y SCALE FACTOR IN FRICTION TABLE. . . .=',1PG20.13/
     & 5X,'FORCE ABSCISSA SCALE IN FRICTION TABLE.=',1PG20.13/
     & 5X,'NON REVERSIBLE NEGATIVE LIMIT FORCE . .=',1PG20.13/
     & 5X,'NON REVERSIBLE POSITIVE LIMIT FORCE . .=',1PG20.13/)
 1500 FORMAT(
     & 5X,'SPRING PROPERTY SET (3 NODES PULLEY)'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'SPRING MASS . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SPRING STIFFNESS. . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SPRING DAMPING. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR LOADING ',/,
     & 5X,'FORCE-DISPLACEMENT CURVE. . . . . . . .=',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'PERMANENT DISPL./MAX. DISPL. CURVE(H=5)=',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'HARDENING FLAG H. . . . . . . . . . . .=',I10/,
     & 5X,'0:ELASTIC   1:ISOTROPIC 2:UNCOUPLED',/,
     & 5X,'4:KINEMATIC 5:UNCOUPLED NL (ONE/RE)LOADING',/,
     & 5X,'6:ELASTO PLASTIC WITH HARDENING 7: ELASTIC HYSTERESIS',/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR A. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR B. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR D. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR E. . . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR  ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR GF3. . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR THE ADDITIONAL ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'NEGATIVE FAILURE DISPLACEMENT . . . . .=',1PG20.13/,
     & 5X,'POSITIVE FAILURE DISPLACEMENT . . . . .=',1PG20.13/,
     & 5X,'SENSOR NUMBER (0:NOT USED). . . . . . .=',I10/,
     & 5X,'SENSOR FLAG (0:ACTIV 1:DISACT 2:BOTH) .=',I10/,
     & 5X,'UNIT LENGTH FLAG. . . . . . . . . . . .=',I10/,
     & 5X,'IF=1 UNIT LENGTH MASS,STIFFNESS AND INPUT',/,
     & 5X,'     CURVE ARE STRAIN DEPENDING',/,
     & 5X,'CONSTANT PULLEY FRICTION COEFFICIENT. .=',1PG20.13/
     & 5X,'TABLE ID OF VARIABLE FRICTION FUNCTIONS=',I10/,
     & 5X,'FRICTION FLAG . . . . . . . . . . . . .=',I10/,
     & 5X,'Y SCALE FACTOR IN FRICTION TABLE. . . .=',1PG20.13/
     & 5X,'FORCE ABSCISSA SCALE IN FRICTION TABLE.=',1PG20.13/
     & 5X,'NON REVERSIBLE NEGATIVE LIMIT FORCE . .=',1PG20.13/
     & 5X,'NON REVERSIBLE POSITIVE LIMIT FORCE . .=',1PG20.13/)
c-----------
      RETURN
c-----------
 999  CALL FREERR(1)
c-----------
      RETURN
      END
