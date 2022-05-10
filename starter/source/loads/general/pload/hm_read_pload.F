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
Chd|  HM_READ_PLOAD                 source/loads/general/pload/hm_read_pload.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PLOAD(IPRES     ,PRES     ,NPREL  ,ITAB    ,ITABM1  ,
     2                  IGRSURF,UNITAB,LSUBMODEL )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE R2R_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE PINCHTYPE_MOD
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
#include      "sysunit.inc"
#include      "r2r_c.inc"
#include      "alefvm.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER NPREL
      INTEGER IPRES(NIBCLD,*), ITAB(*), ITABM1(*)
      my_real
     .   PRES(LFACCLD,*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, M, I1, I2, I3, I4, IFU, I, ISENS,NPR0,NN,ISU,IS,
     .        IAD,ID,J,UID,IFLAGUNIT,IFIX_TMP,
     .        CAPT,H,SUB_INDEX,FLAG_PINCH,KPINCH,IDEL
      INTEGER N1,N2,N3,N4
      my_real
     .   FCX,FCY,FAC_FCX,FAC_FCY
      CHARACTER MESS*40,TITR*nchartitle
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
      DATA MESS/'PRESSURE LOADS DEFINITION               '/
C-----------------------------------------------
C     IBCL(NIBCLD,NUMCLD+NUMPRES), NUMCLD = Total nb of (cloads * nodes)
C                             NUMPRES = Total nb of (ploads * segments)
C     IPRES = IBCL(1:NIBCLD,NUMCCLD+1,NUMCLD+NUMPRES)         
C     1: 1st node number of the segment
C     2: 2nd node number of the segment
C     3: 3rd node number of the segment
C     4: 4th node number of the segment
C     5: Function internal number
C     6: ISENS Sensor User ID
C     7: User ID
C-----------------------------------------------
C     FORC(LFACCLD,NUMCLD+NUMPRES)
C     PRES = FORC(LFACCLD,NUMCLD+1:NUMCLD+NUMPRES)
C     1: Fscale_y  
C     2: 1/Ascale_x
C     3: /=0 <=> Pinching pressure
C=======================================================================
      IS_AVAILABLE = .FALSE.

      NPR0=NPRELD                                                
      NPRELD=0
      K=0
      NPLOADPINCH=0
      KPINCH=0
      PDEL = 0
C--------------------------------------------------
C START BROWSING MODEL /PLOAD
C--------------------------------------------------
      CALL HM_OPTION_START('/PLOAD')

C--------------------------------------------------
C BROWSING MODEL SurPRESe  1->NP0
C--------------------------------------------------
      DO I=1,NPR0     
        TITR = ''                                           
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       UNIT_ID = UID,
     .                       SUBMODEL_INDEX = SUB_INDEX,
     .                       OPTION_TITR = TITR)

C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('entityid',ISU,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('curveid',IFU,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('rad_sensor_id',ISENS,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('ipinch',FLAG_PINCH,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Idel',IDEL,IS_AVAILABLE,LSUBMODEL)

C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
        CALL HM_GET_FLOATV('xscale',FCX,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV_DIM('xscale',FAC_FCX,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('magnitude',FCY,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV_DIM('magnitude',FAC_FCY,IS_AVAILABLE,LSUBMODEL,UNITAB)
C--------------------------------------------------
        IFLAGUNIT = 0 					       
        DO J=1,NUNITS                           
          IF (UNITAB%UNIT_ID(J) == UID) THEN                     
            IFLAGUNIT = 1                       
            EXIT                                
          ENDIF                                 
        ENDDO                                  
        IF (UID.NE.0.AND.IFLAGUNIT.EQ.0) THEN
          CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I2=UID,I1=ID,C1='PRESSURE LOAD',
     .                 C2='PRESSURE LOAD',
     .                 C3=TITR)                              
        ENDIF                                    

        IF (FCX == ZERO) FCX = FAC_FCX
        IF (FCY == ZERO) FCY = FAC_FCY
        IS=0                                                     
        DO J=1,NSURF
          IF (ISU.EQ.IGRSURF(J)%ID) IS=J
        ENDDO        
        IF( IDEL /= 2) THEN
            PDEL = 1    
            IDEL = 1
        ELSE
            PDEL = 0    
            IDEL = 0
        ENDIF
        IF(IS.NE.0)THEN                                          
          NN=IGRSURF(IS)%NSEG
          KPINCH=NN+1
          DO J=1,NN
	    IF (IDDOM.NE.0) THEN	  
C-----------Multidomaines -> on elimine les seg communs, on ne les traite qu'une foi---
	       CAPT=0 
               DO H=1,4
	         IF (TAGNO(NPART+IGRSURF(IS)%NODES(J,H)).EQ.1) CAPT = 1
               END DO	  
	       IF (CAPT.EQ.0) GOTO 150
	    ENDIF   
C
            IF(FLAG_PINCH /= 1) THEN
              K=K+1                                                
              IPRES(1,K)  = IGRSURF(IS)%NODES(J,1)
              IPRES(2,K)  = IGRSURF(IS)%NODES(J,2)				    
              IPRES(3,K)  = IGRSURF(IS)%NODES(J,3)
              IF (IGRSURF(IS)%NODES(J,3)==IGRSURF(IS)%NODES(J,4)) THEN
C               true triangles (not segments built from 3 nodes).   
                IPRES(4,K) = 0                                           
              ELSE                                                 
                IPRES(4,K) = IGRSURF(IS)%NODES(J,4)
              ENDIF     
              IPRES(5,K)     = IFU                                           
              IPRES(6,K)     = ISENS     
              IPRES(7,K) = IDEL          
              IPRES(8,K) = 0                                       
              PRES(1,K) = FCY                                          
              PRES(2,K) = UN/FCX
            ELSE
              NPLOADPINCH = NPLOADPINCH + 1
              KPINCH=KPINCH-1
              IPRES(1,KPINCH)  = IGRSURF(IS)%NODES(J,1)
              IPRES(2,KPINCH)  = IGRSURF(IS)%NODES(J,2)				    
              IPRES(3,KPINCH)  = IGRSURF(IS)%NODES(J,3)
              IF (IGRSURF(IS)%ELTYP(J).EQ.7) THEN
C               true triangles (not segments built from 3 nodes).   
                IPRES(4,KPINCH) = 0                                           
              ELSE                                                 
                IPRES(4,KPINCH) = IGRSURF(IS)%NODES(J,4)
              ENDIF     
              IPRES(5,KPINCH)     = IFU                                           
              IPRES(6,KPINCH)     = ISENS    
              IPRES(7,KPINCH) = IDEL  
              IPRES(8,KPINCH) = 0                                      
              PRES(1,KPINCH) = FCY                                          
              PRES(2,KPINCH) = UN/FCX              
            ENDIF  
C                                  
  150        CONTINUE
          ENDDO
C-----------Multidomaines -> on decompte les seg communs, on ne les compte qu'une foi---	  
	  IF (IDDOM.GT.0) NN = NN-ISURF_R2R(1,IS)                                                  
          NPRELD=NPRELD+NN                                       
        ELSE                                                     
          GOTO 999                                               
        ENDIF						 
      ENDDO
C
      I1=1
      I2=MIN0(50,NPRELD)
C
   90  WRITE (IOUT,2000)

       DO I=I1,I2

         IF(IPRES(4,I) == 0 .AND. IPRES(3,I) == 0)THEN
            ! 2D / Surface made of lines
            WRITE (IOUT,'(3(1X,I10),A,1X,I10,1X,I10,2G20.13)') I,
     .        ITAB(IPRES(1,I)),ITAB(IPRES(2,I)),'                    ',
     .        IPRES(5,I),IPRES(6,I),UN/PRES(2,I),PRES(1,I)

         ELSEIF(IPRES(4,I) == 0 .AND. IPRES(3,I) /= 0)THEN
            ! Surface made of 3 nodes
            WRITE (IOUT,'(4(1X,I10),A,1X,I10,1X,I10,2G20.13)') I,
     .        ITAB(IPRES(1,I)),ITAB(IPRES(2,I)),ITAB(IPRES(3,I)),'          ',
     .        IPRES(5,I),IPRES(6,I),UN/PRES(2,I),PRES(1,I)
         ELSE
            ! Surface made of 4 nodes
            WRITE (IOUT,'(6(1X,I10),1X,I10,2G20.13)') I,
     .        ITAB(IPRES(1,I)),ITAB(IPRES(2,I)),ITAB(IPRES(3,I)),ITAB(IPRES(4,I)),
     .        IPRES(5,I),IPRES(6,I),UN/PRES(2,I),PRES(1,I)
         ENDIF

       ENDDO 

       IF(I2.EQ.NPRELD)GOTO 200
       I1=I1+50
       I2=MIN0(I2+50,NPRELD)
       GOTO 90
 200  RETURN
 300  CALL ANCMSG(MSGID=157,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            I1=K)
C---
 2000 FORMAT(//
     .'     PRESSURE   LOADS  '/
     .'     ----------------  '/
     .'       SEGM      NODE1      NODE2      NODE3      NODE4    CURVE',
     .'     SENSOR         SCALE-X                 SCALE-Y ')
 999  CALL FREERR(3)
C-----------
      RETURN
      END

