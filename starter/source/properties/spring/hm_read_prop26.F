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
Chd|  HM_READ_PROP26                source/properties/spring/hm_read_prop26.F
Chd|-- called by -----------
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX_DIM  source/devtools/hm_reader/hm_get_float_array_index_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROP26(GEO     , IGEO  , UNITAB   , IG   , IGTYP,
     .                          PROP_TAG, IDTITL, LSUBMODEL)
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "tablen_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER  IGEO(NPROPGI),IG,IGTYP,FLAG_FMT
C     REAL
      my_real
     .   GEO(NPROPG)
      CHARACTER IDTITL*nchartitle
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER I,J,NFUNC,NFUND,IFUN,IAD,ISENS,IFL,ILENG,IRTYP
C     REAL
      my_real
     .   MASS,KMAX,DMAX,XFAC,YFAC,RATE,ALPHA,DMIN,
     .   PUN,YFAC_DIM,XFAC_DIM
      CHARACTER*nchartitle,
     .   TITR
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED
C=======================================================================
      PUN = EM01
C
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
C
      IGEO( 1)=IG
      IGEO(11)=IGTYP
      GEO(12) =IGTYP+PUN
C
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('ISFLAG',IFL,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('ISENSOR',ISENS,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ileng',ILENG,IS_AVAILABLE,LSUBMODEL)

      CALL HM_GET_INTV('NFUNC',NFUNC,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('NRATEN',NFUND,IS_AVAILABLE,LSUBMODEL)   
      CALL HM_GET_FLOATV('DMIN',DMIN,IS_AVAILABLE,LSUBMODEL,UNITAB)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('m_coeff',MASS,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('SCALE',XFAC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('SCALE',XFAC_DIM,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('STIFF0',KMAX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('DMAX',DMAX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('ALPHA1',ALPHA,IS_AVAILABLE,LSUBMODEL,UNITAB)
C----
      IRTYP = 7                                                       
      CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1),LTITR)
c
      ! Check if at least one loading curve is defined
      IF (NFUNC <= 0) THEN
        CALL ANCMSG(MSGID=2078,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IG,
     .              C1=TITR)
      ENDIF
c
      IF (ALPHA == ZERO) ALPHA = ONE                                   
      IF (XFAC == ZERO)  XFAC  = ONE * XFAC_DIM    
      DMIN = -ABS(DMIN)
      DMAX = ABS(DMAX)
      IF (DMIN == ZERO)  DMIN  = -INFINITY              
      IF (DMAX == ZERO)  DMAX  =  INFINITY
      IF (ILENG == 1)    XFAC  = ONE
c
C---  Loading curves                                                 
      IAD = 100                                                       
      DO I = 1, NFUNC                                              
        CALL HM_GET_INT_ARRAY_INDEX('FUN_LOAD',IFUN,I,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_FLOAT_ARRAY_INDEX('SCALE_LOAD',YFAC,I,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOAT_ARRAY_INDEX('STRAINRATE_LOAD',RATE,I,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('SCALE_LOAD',YFAC_DIM,I,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
        IF (IFUN <= 0) THEN
          CALL ANCMSG(MSGID=862,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=IG,
     .                C1=TITR)
          EXIT
        ENDIF
        IF(I > 1 .AND. RATE < GEO (IAD+100+I-1)) THEN
          CALL ANCMSG(MSGID=861,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=IG,
     .                C1=TITR)
          EXIT
        ENDIF
        IF (YFAC == ZERO) YFAC = ONE * YFAC_DIM
C
        IGEO(IAD+I)     = IFUN
        GEO (IAD+100+I) = RATE                                         
        GEO (IAD+200+I) = YFAC                                     
      ENDDO                                                           
C---  Unloading curves                                              
      IAD = 100+NFUNC
      ! -> defined by user
      IF (NFUND > 0) THEN                                            
        DO I = 1, NFUND                                              
          CALL HM_GET_INT_ARRAY_INDEX('FUN_UNLOAD',IFUN,I,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_FLOAT_ARRAY_INDEX('SCALE_UNLOAD',YFAC,I,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('STRAINRATE_UNLOAD',RATE,I,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX_DIM('SCALE_UNLOAD',YFAC_DIM,I,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
          IF (IFUN <= 0) THEN
            CALL ANCMSG(MSGID=862,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=IG,
     .                  C1=TITR)
            EXIT
          ENDIF
          IF(I > 1 .AND. RATE < GEO (IAD+100+I-1)) THEN
            CALL ANCMSG(MSGID=861,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=IG,
     .                  C1=TITR)
            EXIT
          ENDIF
          IF (YFAC == ZERO) YFAC = ONE * YFAC_DIM
C
          IGEO(IAD+I)     = IFUN                                        
          GEO (IAD+100+I) = RATE                                         
          GEO (IAD+200+I) = YFAC                                       
        ENDDO
      ! -> defined by default 
      ELSE
        CALL ANCMSG(MSGID=2079,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IG,
     .              C1=TITR)
        NFUND = NFUNC
        DO I = 1,NFUND
          IGEO(IAD+I)     = IGEO(100+I)                                      
          GEO (IAD+100+I) = GEO (200+I)                                       
          GEO (IAD+200+I) = GEO (300+I)       
        ENDDO
      ENDIF                                                          
C
      IGEO(20) = NFUNC                                             
      IGEO(21) = NFUND
      GEO(1)   = MASS                                                   
      GEO(2)   = KMAX                                                   
      GEO(4)   = ALPHA   
      GEO(5)   = XFAC                                               
      GEO(8)   = IRTYP + EM20   
      GEO(15)  = DMIN                          
      GEO(16)  = DMAX                                        
C
      IF (MASS < EM15)THEN                                            
         CALL ANCMSG(MSGID=229,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF                                                           
      IF (IFL == 0)THEN                                               
        IGEO(3)=ISENS                                                 
      ELSEIF (IFL == 1)THEN                                           
        IGEO(3)=-ISENS                                                
      ELSEIF (IFL == 2)THEN                                           
        IGEO(3)=ISENS                                                 
      ENDIF                                                           
      GEO(80)=IFL                                                     
      GEO(93)=ILENG                                                   
C
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,1000)IG
      ELSE
        WRITE(IOUT,1500)IG,MASS,KMAX,NFUNC,NFUND,DMIN,DMAX,ALPHA,XFAC,ILENG
        IAD = 100                                                       
        DO I=1,NFUNC
          WRITE(IOUT,1700) IGEO(IAD+I),GEO(IAD+200+I),GEO(IAD+100+I)
        ENDDO
        IAD = 100+NFUNC                                               
        DO I=1,NFUND
          WRITE(IOUT,1800) IGEO(IAD+I),GEO(IAD+200+I),GEO(IAD+100+I)
        ENDDO
      ENDIF
C
      PROP_TAG(IGTYP)%G_EINT = 1
      PROP_TAG(IGTYP)%G_FOR  = 1
      PROP_TAG(IGTYP)%G_LENGTH = 1
      PROP_TAG(IGTYP)%G_TOTDEPL = 1
      PROP_TAG(IGTYP)%G_FOREP = 1
      PROP_TAG(IGTYP)%G_DEP_IN_COMP = 1
      PROP_TAG(IGTYP)%G_POSX = 5  !  just temp - not really used - 
      PROP_TAG(IGTYP)%G_LENGTH_ERR = 1
      PROP_TAG(IGTYP)%G_DV = 1
C
C-----------
      RETURN
C-----------
 1000   FORMAT(
     &    5X,'TABULATED ELASTO-PLASTIC SPRING PROPERTY SET'/,
     &    5X,'-------------------'/,
     &    5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     &    5X,'CONFIDENTIAL DATA'//)
 1500 FORMAT(
     & 5X,'TABULATED ELASTIC SPRING PROPERTY SET'/,
     & 5X,'-------------------------------------'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'SPRING MASS . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'MAXIMUM STIFFNESS . . . . . . . . . . .=',1PG20.13/,
     & 5X,'NUMBER OF LOADING CURVES  . . . . . . .=',I10/,
     & 5X,'NUMBER OF UNLOADING CURVES. . . . . . .=',I10/,
     & 5X,'FAILURE DISPLACEMENT IN COMPRESSION . .=',1PG20.13/,
     & 5X,'FAILURE DISPLACEMENT IN TENSION . . . .=',1PG20.13/,
     & 5X,'STRAIN RATE FILTERING FACTOR  . . . . .=',1PG20.13/,
     & 5X,'ABSCISSA SCALE FACTOR         . . . . .=',1PG20.13/,
     & 5X,'UNIT LENGTH FLAG. . . . . . . . . . . .=',I10/,
     & 5X,'IF=1 UNIT LENGTH MASS,STIFFNESS AND INPUT',/,
     & 5X,'     CURVE ARE STRAIN DEPENDING',/)
 1700 FORMAT(
     & 5X,'YIELD STRESS FUNCTION NUMBER  . . . . . =',I10/
     & 7X,'SCALE FACTOR. . . . . . . . . . . . . . =',1PG20.13/
     & 7X,'STRAIN RATE . . . . . . . . . . . . . . =',1PG20.13)
 1800 FORMAT(
     & 5X,'UNLOADING FUNCTION NUMBER . . . . . . . =',I10/
     & 7X,'SCALE FACTOR. . . . . . . . . . . . . . =',1PG20.13/
     & 7X,'STRAIN RATE . . . . . . . . . . . . . . =',1PG20.13)
c      sensor and sensor flag not used  
C-----------
      RETURN
      END SUBROUTINE HM_READ_PROP26
