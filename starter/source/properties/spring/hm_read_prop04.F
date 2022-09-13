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
Chd|  HM_READ_PROP04                source/properties/spring/hm_read_prop04.F
Chd|-- called by -----------
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_CRYPTED          source/devtools/hm_reader/hm_option_is_crypted.F
Chd|        NL                            source/output/message/message.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROP04(GEO   , IGEO    ,UNITAB   ,IG    ,IGTYP,
     .                          IDTITL, PROP_TAG,LSUBMODEL,IUNIT)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD           
      USE MESSAGE_MOD
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
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER  IGEO(NPROPGI),IGTYP,IUNIT
C     REAL
      my_real
     .   GEO(NPROPG)
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER J, IFUNC, IFUNC2, IFUNC3, IECROU, IFV, IG,ISENS,
     .         IFL, ILENG, IFORM
C     REAL
      my_real
     .   A0, A, B, D, E, F, XM, XIN, XK, XC, DN, DX, PUN, 
     .   LSCALE,GF3,A_UNIT,B_UNIT,D_UNIT,E_UNIT,F_UNIT,LSCALE_UNIT,GF3_UNIT
      CHARACTER*nchartitle,
     .   TITR      
      CHARACTER IDTITL*nchartitle
      LOGICAL IS_AVAILABLE, IS_CRYPTED
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      PUN = EM01
      IS_CRYPTED=.FALSE.
      IS_AVAILABLE = .FALSE.
C=======================================================================
C----------------------
C FROM LECGEO - GENERAL
C----------------------
      GEO(5)=EP06
      IGEO( 1)=IG
      IGEO(11)=IGTYP
      GEO(12) =IGTYP+PUN
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
      CALL HM_GET_INTV('FUN_C1',IFUNC2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_D1',IFUNC3,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('MASS',GEO(1),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('STIFF1',GEO(2),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('DAMP1',GEO(3),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Acoeft1',A,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Bcoeft1',B,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Dcoeft1',D,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MIN_RUP1',DN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAX_RUP1',DX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Prop_FScale',F,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Prop_EScale',E,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('scale1',LSCALE,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('ffac',GF3,IS_AVAILABLE,LSUBMODEL,UNITAB)
      !units for default values
      CALL HM_GET_FLOATV_DIM('Acoeft1',A_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('Bcoeft1',B_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('Dcoeft1',D_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('Prop_FScale',F_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('Prop_EScale',E_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('scale1',LSCALE_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('ffac',GF3_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
C----------------------
C----
      CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1),LTITR)                                        
      IF(GEO(1)<=EM15)THEN                                 
         CALL ANCMSG(MSGID=229,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF                                                  
!      IF(IFUNC/=0.AND.IECROU>=1.AND.GEO(2) == 0.0)THEN   
!         CALL ANCMSG(MSGID=230,
!     .               MSGTYPE=MSGERROR,
!     .               ANMODE=ANINFO_BLIND_1,
!     .               I1=IG,
!     .               C1=TITR)
!      ENDIF                                                  
      IF(IECROU == 4.AND.(IFUNC == 0.OR.IFUNC2 == 0))THEN    
         CALL ANCMSG(MSGID=231,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF                                                  
      IF(IECROU == 4.AND.GEO(2) == ZERO)THEN                  
         CALL ANCMSG(MSGID=230,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF                                                  
      IF(IECROU == 5.AND.(IFUNC == 0.OR.IFUNC2 == 0))THEN    
         CALL ANCMSG(MSGID=231,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF    
       IF(IECROU==6.AND.(IFUNC==0.OR.IFUNC2==0))THEN
          CALL ANCMSG(MSGID=1057,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=IG,
     .                C1=TITR)
       ENDIF
       IF(IECROU==7.AND.IFUNC==0)THEN
          CALL ANCMSG(MSGID=1058,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=IG,
     .                C1=TITR)
       
       ELSEIF(IECROU==7.AND.IFUNC2==0)THEN
          CALL ANCMSG(MSGID=1059,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=IG,
     .                C1=TITR,
     .                I2=IECROU)
         IECROU = 2
       ENDIF

      IF(IECROU == 8.AND. IFUNC == 0)THEN    
         CALL ANCMSG(MSGID=231,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF    
      IF (IFUNC == 0 .AND. A /= ZERO .AND. A /= ONE) THEN
        CALL ANCMSG(MSGID=663,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IG,
     .              C1=TITR)
      ENDIF                                        
C----
      IF (A == ZERO) A = ONE * A_UNIT               
      IF (D == ZERO) D = ONE * D_UNIT         
      IF (E == ZERO) E = ONE * E_UNIT
      IF (F == ZERO) F = ONE * F_UNIT
      IF (GF3 == ZERO) GF3 = ONE * GF3_UNIT
      IF (LSCALE == ZERO) THEN
        IF (ILENG == 0) THEN                                 
          LSCALE = ONE * LSCALE_UNIT
        ELSE
          LSCALE = ONE
        ENDIF  
      ENDIF 
      IF (IFUNC == 0) THEN
        A = ONE
        B = ZERO
        E = ZERO
      ENDIF
      IF (DN == ZERO)DN=-EP30
      IF (DX == ZERO)DX= EP30             
      IF (IFL == 1) ISENS=-ISENS  
C------------------------
      DN = DN * LSCALE                                      
      DX = DX * LSCALE                    
C------------------------
      IF(.NOT. IS_CRYPTED)THEN
       IF(IECROU/=5) THEN
        WRITE(IOUT,1400)IG,(GEO(J),J=1,3),IFUNC,LSCALE,IFUNC2,
     .                 F,IECROU,A,B,D,E,IFV,GF3,IFUNC3,DN,DX,ABS(ISENS),
     .                 IFL,ILENG
       ELSE
        WRITE(IOUT,1500)IG,(GEO(J),J=1,3),IFUNC,LSCALE,IFUNC2,
     .                 F,IECROU,A,B,D,E,IFV,GF3,IFUNC3,DN,DX,ABS(ISENS),
     .                 IFL,ILENG

       ENDIF
      ELSE
        WRITE(IOUT,1000)IG
      ENDIF
C------------------------
      GEO(2)  = GEO(2) / A
      GEO(7)  = IECROU+PUN                  
      GEO(8)  = ONEP1                        
      GEO(9)  = ZERO                        
      GEO(10) = A                           
      GEO(11) = B                           
      GEO(13) = D                           
      GEO(40) = E 
      GEO(132)= GF3                       
      GEO(18) = ONE/F             
      GEO(39) = ONE/LSCALE                        
      GEO(15) = DN                          
      GEO(16) = DX                          
      GEO(80) = IFL                         
      GEO(93) = ILENG
C--   If H=6 - addditional internal variables must be stored in UVAR -  - GEO(25) = NUVAR
      IF (IECROU == 6) THEN
        GEO(25) = 1
      ENDIF
C                       
      IGEO(3)   = ISENS
      IGEO(101) = IFUNC                     
      IGEO(102) = IFV                       
      IGEO(103) = IFUNC2
      IGEO(119) = IFUNC3

C----------------------
C FROM LECGEO - GENERAL
C----------------------
      IF(GEO(39)/=ZERO.AND.IGEO( 9)== 0)IGEO( 9)=NINT(GEO(39))
      IF(GEO(171)/=ZERO.AND.IGEO(10)== 0)IGEO(10)=NINT(GEO(171))
C----------------------
C
      PROP_TAG(IGTYP)%G_EINT = 1
      PROP_TAG(IGTYP)%G_FOR = 1
      PROP_TAG(IGTYP)%G_LENGTH = 1   ! X0 (AL0) - total length
      PROP_TAG(IGTYP)%G_TOTDEPL = 1  ! DX - total deformation (translation)
      PROP_TAG(IGTYP)%G_FOREP = 1    !  FORCE - (ELASTO PLASTIQUE (ISOTROPE))
      PROP_TAG(IGTYP)%G_DEP_IN_TENS = 1  ! DPX  (DPY,DPZ) - max displacement in tension
      PROP_TAG(IGTYP)%G_DEP_IN_COMP = 1  ! DPX2 (DPY2,DPZ2) - max displacement in compression
      PROP_TAG(IGTYP)%G_POSX = 5
      PROP_TAG(IGTYP)%G_YIELD = 1
      PROP_TAG(IGTYP)%G_LENGTH_ERR = 1
      PROP_TAG(IGTYP)%G_NUVAR = MAX(PROP_TAG(IGTYP)%G_NUVAR,NINT(GEO(25))) ! additional internal variables for h=6
      PROP_TAG(IGTYP)%G_DEFINI = 1
      PROP_TAG(IGTYP)%G_FORINI = 1
C
C------------------------
      RETURN
C
 1000   FORMAT(
     &    5X,'SPRING PROPERTY SET'/,
     &    5X,'-------------------'/,
     &    5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     &    5X,'CONFIDENTIAL DATA'//)
 1400 FORMAT(
     & 5X,'SPRING PROPERTY SET'/,
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
     & 5X,'4:KINEMATIC 5:UNCOUPLED NL (UN/RE)LOADING',/,
     & 5X,'6:ELASTO PLASTIC WITH HARDENING 7: ELASTIC HYSTERESIS',/,
     & 5X,'8:ELASTIC, TOTAL LENGTH FUNCTION',/,
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
     & 5X,'     CURVE ARE STRAIN DEPENDING',/)
 1500 FORMAT(
     & 5X,'SPRING PROPERTY SET'/,
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
     & 5X,'4:KINEMATIC 5:UNCOUPLED NL (UN/RE)LOADING',/,
     & 5X,'6:ELASTO PLASTIC WITH HARDENING 7: ELASTIC HYSTERESIS',/,
     & 5X,'8:ELASTIC, TOTAL LENGTH FUNCTION. . . .',/,
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
     & 5X,'     CURVE ARE STRAIN DEPENDING',/)
 999  CALL FREERR(1)
      RETURN
      END
