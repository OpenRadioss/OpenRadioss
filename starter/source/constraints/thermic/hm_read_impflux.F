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
Chd|  HM_READ_IMPFLUX               source/constraints/thermic/hm_read_impflux.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_IMPFLUX(IB      ,FAC    ,ITAB   ,IXS    ,IGRSURF  ,
     .                      UNITAB ,IGRNOD ,IGRBRIC, LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "sysunit.inc"
#include      "thermal_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IB(NITFLUX,*), ITAB(*), IXS(NIXS,*)
C
      my_real FAC(LFACTHER,*)

      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, I1, I2, IFU, ISENS, NN, ISU, IS,
     .        ID, UID, IFLAGUNIT, ITY
      INTEGER IEL, IGBR, K1, K2
C
      my_real FCX, FCY, FCX_DIM, FCY_DIM
      my_real TEMP, TSTART, TSTOP, TSTOP_DIM
      CHARACTER MESS*40,TITR*nchartitle
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
      DATA MESS/'THERMAL  FLUX  DEFINITION               '/
C=======================================================================
      IS_AVAILABLE = .FALSE.                                       
      K =0    
      K1=0
      K2=0
C--------------------------------------------------
C START BROWSING MODEL THERMAL FLUX
C--------------------------------------------------
      CALL HM_OPTION_START('/IMPFLUX')
C--------------------------------------------------
C BROWSING /RADIATION OPTIONS 1->NIMPFLUX 
C--------------------------------------------------
      DO I=1,NIMPFLUX  
        TITR = ''
C--------------------------------------------------
C EXTRACT DATAS OF /IMPFLUX/... LINE
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       UNIT_ID = UID,
     .                       OPTION_ID = ID,
     .                       OPTION_TITR = TITR)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('entityid',ISU,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('curveid',IFU,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('rad_sensor_id',ISENS,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('grbrick_id',IGBR,IS_AVAILABLE,LSUBMODEL) 
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
        CALL HM_GET_FLOATV('xscale',FCX,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV_DIM('xscale',FCX_DIM,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('magnitude',FCY,IS_AVAILABLE,LSUBMODEL,UNITAB)       
        CALL HM_GET_FLOATV_DIM('magnitude',FCY_DIM,IS_AVAILABLE,LSUBMODEL,UNITAB)         
        CALL HM_GET_FLOATV('rad_tstart',TSTART,IS_AVAILABLE,LSUBMODEL,UNITAB)        
        CALL HM_GET_FLOATV('rad_tstop',TSTOP,IS_AVAILABLE,LSUBMODEL,UNITAB)          
        CALL HM_GET_FLOATV_DIM('rad_tstop',TSTOP_DIM,IS_AVAILABLE,LSUBMODEL,UNITAB)   
C--------------------------------------------------  
        IFLAGUNIT = 0                                        
        DO J=1,NUNITS                           
          IF (UNITAB%UNIT_ID(J) == UID) THEN                     
            IFLAGUNIT = 1                       
            EXIT                                
          ENDIF                                 
        ENDDO                                  
        IF (UID /= 0.AND.IFLAGUNIT == 0) THEN
          CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I2=UID,I1=ID,C1='HEAT FLUX',
     .                C2='HEAT FLUX',C3=TITR)                              
        ENDIF 
        IF(ISU /= 0 .AND. IGBR /=0) THEN
          CALL ANCMSG(MSGID=1229,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1=TITR,I1=ISU,I2=IGBR)                              
        ENDIF
C
        IF (FCX == ZERO) FCX = FCX_DIM
        IF (FCY == ZERO) FCY = FCY_DIM
        IF(TSTOP == ZERO) TSTOP= EP30 * TSTOP_DIM      
C
        IF(ISU > 0) THEN
         IS=0 
         DO J=1,NSURF
          IF (ISU == IGRSURF(J)%ID) IS=J
         ENDDO
         IF(IS > 0)THEN                                          
          NN =IGRSURF(IS)%NSEG
          DO J=1,NN                                              
            K=K+1                                                
            K1=K1+1                                                
            IB(1,K)=IGRSURF(IS)%NODES(J,1)
            IB(2,K)=IGRSURF(IS)%NODES(J,2)
            IB(3,K)=IGRSURF(IS)%NODES(J,3)
            ITY    =IGRSURF(IS)%ELTYP(J)
            IF(ITY==7)THEN                          
C          true triangles (not segments built from 3 nodes).   
             IB(4,K)=0                                           
            ELSE                                                 
             IB(4,K)=IGRSURF(IS)%NODES(J,4)
            ENDIF                                                
            IB(5,K)  = IFU                                           
            IB(6,K)  = ISENS 
            IB(7,K)  = IGRSURF(IS)%ELTYP(IS)
            IB(8,K)  = IGRSURF(IS)%ELEM(IS)
            IF(ITY == 1) THEN
              IB(9,K)  = IXS(11,IGRSURF(IS)%ELEM(J))
            ELSE
              IB(9,K)  = 0
            ENDIF
            IB(10,K) = 0
C                                                  
            FAC(1,K) = FCY                                          
            FAC(2,K) = ONE/FCX 
            FAC(3,K) = ZERO
            FAC(4,K) = TSTART
            FAC(5,K) = TSTOP                                       
            FAC(6,K) = ONE                                      
          ENDDO 
         ELSE
          CALL ANCMSG(MSGID=1230,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1=TITR,I1=ISU)                              
         ENDIF
C		 
        ELSEIF(IGBR > 0) THEN
         IS=0		
         DO J=1,NGRBRIC
           IF (IGBR == IGRBRIC(J)%ID) IS=J
         ENDDO
         IF(IS > 0) THEN
            NN  = IGRBRIC(IS)%NENTITY
            DO J=1,NN
              K=K+1                                                
              K2=K2+1                                                
              IEL = IGRBRIC(IS)%ENTITY(J)
              IB(1,K)  = 0
              IB(2,K)  = 0
              IB(3,K)  = 0
              IB(4,K)  = 0
              IB(5,K)  = IFU                                           
              IB(6,K)  = ISENS 
              IB(8,K)  = IEL
              IB(9,K)  = IXS(11,IEL)
              IB(10,K) = 1
C
              FAC(1,K) = FCY                                          
              FAC(2,K) = ONE/FCX 
              FAC(3,K) = ZERO
              FAC(4,K) = TSTART
              FAC(5,K) = TSTOP                                       
              FAC(6,K) = ONE                                      
            ENDDO
         ELSE
          CALL ANCMSG(MSGID=1231,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1=TITR,I1=IGBR)                              
         ENDIF
        ENDIF                                             
      ENDDO ! I=1,NIMPFLUX
C
      IF(K1 > 0) THEN
       I =0
       I2=0
 100   WRITE (IOUT,2000)
       WRITE (IOUT,2001)
       I1=0
 150   I=I+1
       IF(IB(10,I) == 0) THEN
         I1=I1+1
         I2=I2+1
         WRITE (IOUT,'(5(1X,I10),2(1X,I10),1X,4G20.13)') I,
     .         ITAB(IB(1,I)),ITAB(IB(2,I)),ITAB(IB(3,I)),ITAB(IB(4,I)),
     .         IB(5,I),IB(6,I),FAC(4,I),FAC(5,I),ONE/FAC(2,I),
     .         FAC(1,I)
       ENDIF
       IF(I2 == K1)GO TO 200
       IF(I1 < 50) GO TO 150
       GO TO 100
 200   CONTINUE
      ENDIF
C
      IF(K2 > 0) THEN
       I =0
       I2=0
 300   WRITE (IOUT,3000)
       WRITE (IOUT,3001)
       I1=0
 350   I=I+1
       IF(IB(10,I) == 1) THEN
         I1=I1+1
         I2=I2+1
         WRITE (IOUT,'(2X,I10,2(2X,I10),1X,4G20.13)') IB(9,I),
     .         IB(5,I),IB(6,I),FAC(4,I),FAC(5,I),ONE/FAC(2,I),
     .         FAC(1,I)
       ENDIF
       IF(I2 == K2)GO TO 400
       IF(I1 < 50) GO TO 350
       GO TO 300
 400   CONTINUE
      ENDIF
C
      RETURN

C
 2000 FORMAT(//
     .'     SURFACIC HEAT FLUX DENSITY '/
     .'     -------------------------- ')
 2001 FORMAT(/
     .'    SEGMENT      NODE1      NODE2      NODE3      NODE4 ',
     .'     CURVE     SENSOR          T-START              T-STOP', 8X,
     .'     SCALE-X             SCALE-Y')
 3000 FORMAT(//
     .'     VOLUMIC HEAT FLUX DENSITY  '/
     .'     -------------------------  ')
 3001 FORMAT(/
     .'    BRICK ELEMENT  CURVE      SENSOR          T-START', 9X,
     .'     T-STOP             SCALE-X             SCALE-Y')

C
      RETURN
      END

