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
Chd|  HM_READ_RADIATION             source/loads/thermic/hm_read_radiation.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
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
      SUBROUTINE HM_READ_RADIATION(IB, FAC, ITAB, IXS ,IGRSURF, UNITAB ,LSUBMODEL)
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
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IB(NIRADIA,*), ITAB(*), IXS(NIXS,*)
      my_real FAC(LFACTHER,*)

      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, M, I1, I2, I3, I4, IFU, I, ISENS,NPR0,NN,ISU,IS,
     .        ID,J,UID,IFLAGUNIT,ITY
      my_real
     .   FCX, FCY, FAC_L, FAC_T, FAC_M, EMI, SIGMA, TSTART, TSTOP,
     .   EMISS(NUMRADIA),FCX_DIM,FCY_DIM,TSTOP_DIM
      CHARACTER MESS*40,TITR*nchartitle
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
      DATA MESS/'RADIATIVE FLUX DEFINITION               '/
C=======================================================================
      IS_AVAILABLE = .FALSE.                                    
      K = 0              
C--------------------------------------------------
C START BROWSING MODEL RADIATION
C--------------------------------------------------
      CALL HM_OPTION_START('/RADIATION')
C--------------------------------------------------
C BROWSING /RADIATION OPTIONS 1->NRADIA 
C--------------------------------------------------
      DO I=1,NRADIA
        TITR = ''
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       UNIT_ID = UID,
     .                       OPTION_ID = ID,
     .                       OPTION_TITR = TITR)
        IFLAGUNIT = 0                                           
        DO J=1,NUNITS                           
          IF (UNITAB%UNIT_ID(J) == UID) THEN       
            IFLAGUNIT = 1                       
            EXIT                                
          ENDIF                                 
        ENDDO                                  
        IF (UID /= 0.AND.IFLAGUNIT == 0) THEN
          CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I2=UID,I1=ID,C1='CONVECTION HEAT',
     .                 C2='CONVECTION HEAT',
     .                 C3=TITR)                             
        ENDIF  
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('entityid',ISU,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('curveid',IFU,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('rad_sensor_id',ISENS,IS_AVAILABLE,LSUBMODEL)     
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
        CALL HM_GET_FLOATV('flux',EMI,IS_AVAILABLE,LSUBMODEL,UNITAB)   
C--------------------------------------------------  
        IF (FCX == ZERO) FCX = FCX_DIM
        IF (FCY == ZERO) FCY = FCY_DIM
        IF(TSTOP == ZERO) TSTOP= EP30 * TSTOP_DIM      
C
        FAC_M = FAC_M_WORK
        FAC_T = FAC_T_WORK
        SIGMA=STEFBOLTZ*(FAC_T*FAC_T*FAC_T)/FAC_M
C
        IS=0                                                     
        DO J=1,NSURF
          IF (ISU == IGRSURF(J)%ID) IS=J
        ENDDO                                                    
        IF(IS > 0)THEN                                          
          NN=IGRSURF(IS)%NSEG
          DO J=1,NN                                              
            K=K+1                                                
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
            IB(7,K)  = IGRSURF(IS)%ELTYP(J)
            IB(8,K)  = IGRSURF(IS)%ELEM(J)
            IF(ITY == 1) THEN
              IB(9,K)  = IXS(11,IGRSURF(IS)%ELEM(J))
            ELSE
              IB(9,K)  = 0
            ENDIF
C                                                  
            FAC(1,K) = FCY                                          
            FAC(2,K) = ONE/FCX 
            FAC(3,K) = EMI*SIGMA
            FAC(4,K) = TSTART
            FAC(5,K) = TSTOP                                       
            FAC(6,K) = ONE                                      
C  
C temporary storage for print out.                                                
            EMISS(K)=EMI
          ENDDO                                          
        ENDIF                                                
      ENDDO
C
      I1=1
      I2=MIN0(50,NUMRADIA)
C
   90  WRITE (IOUT,2000)
       WRITE (IOUT,2001)
       DO I=I1,I2
         WRITE (IOUT,'(5(1X,I10),1X,1F10.3,2(1X,I10),1X,4G20.13)') I,
     .         ITAB(IB(1,I)),ITAB(IB(2,I)),ITAB(IB(3,I)),ITAB(IB(4,I)),
     .         EMISS(I),IB(5,I),IB(6,I),FAC(4,I),FAC(5,I),ONE/FAC(2,I),
     .         FAC(1,I)
       ENDDO
       IF(I2 == NUMRADIA)GOTO 200
       I1=I1+50
       I2=MIN0(I2+50,NUMRADIA)
       GOTO 90
 200  RETURN
 300  CALL ANCMSG(MSGID=157,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            I1=K)
C---
 2000 FORMAT(//
     .'     RADIATION   HEAT  '/
     .'     ----------------  ')
 2001 FORMAT(/
     .'    SEGMENT      NODE1      NODE2      NODE3      NODE4  EMISSIVITY',
     .'     CURVE     SENSOR          T-START              T-STOP', 8X,
     .'     SCALE-X             SCALE-Y')
C-----------
      RETURN
      END

