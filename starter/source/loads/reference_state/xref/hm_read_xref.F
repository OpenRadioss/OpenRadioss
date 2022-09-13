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
Chd|  HM_READ_XREF                  source/loads/reference_state/xref/hm_read_xref.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        SUBROTPOINT                   source/model/submodel/subrot.F
Chd|        ID                            source/boundary_conditions/ebcs/hm_read_ebcs_inlet.F
Chd|        R2R_SYS                       source/coupling/rad2rad/routines_r2r.F
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_XREF(ITABM1   ,IPART    ,IPARTC   ,IPARTG   ,IPARTS   ,
     .                        UNITAB   ,IXC      ,IXTG     ,IXS      ,X        ,
     .                        XREFC    ,XREFTG   ,XREFS    ,RTRANS   ,LSUBMODEL,
     .                        TAGXREF  ,IDDLEVEL ,ISOLNOD  ,IPM      ,IGEO     ) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
      USE HM_OPTION_READ_MOD       
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "sysunit.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   G l o b a l   V a r i a b l e s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER  ITABM1(*),IPART(LIPART1,*),IPARTC(*),IPARTG(*),IPARTS(*),
     .  IXC(NIXC,*),IXTG(NIXTG,*),IXS(NIXS,*),TAGXREF(NUMNOD)
      INTEGER IDDLEVEL
      my_real
     .  X(3,*),XREFC(4,3,*),XREFTG(3,3,*),XREFS(8,3,*),
     .  RTRANS(NTRANSF,*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C MODIFIED ARGUMENT
      INTEGER,INTENT(IN)::IGEO(NPROPGI,*)
      INTEGER,INTENT(IN)::IPM(NPROPMI,*)  
      INTEGER,INTENT(IN)::ISOLNOD(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  TAGELC(NUMELC),TAGELTG(NUMELTG),TAGELS(NUMELS)
      INTEGER  TAGNOD(NUMNOD),IFLAGUNIT
      INTEGER, DIMENSION(:), ALLOCATABLE :: ID
      INTEGER  I,J,IE,IN,IP,IR,NN,NITER,PARTID,UID,ITYP
      INTEGER  SUB_ID,NNOD,SUB_INDEX,IMID, MAT_ID,MTN,NSOLID,NPT
C     REAL
      my_real
     .  XTMP(3,NUMNOD)
      my_real, DIMENSION(:), ALLOCATABLE ::
     .  XX,YY,ZZ
      CHARACTER MESS*40,TITR*nchartitle,TITR1*nchartitle
      DATA MESS/'XREF'/
      LOGICAL :: IS_AVAILABLE,FOUND      
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS,R2R_SYS
C=======================================================================
      IS_AVAILABLE = .FALSE.
C
      DO IE=1,NUMELC                          
        DO IN = 1,4                           
          NN = IXC(IN+1,IE)
          DO J = 1,3                          
            XREFC(IN,J,IE) = X(J,NN)
          ENDDO                               
        ENDDO                                 
      ENDDO                                   
      DO IE=1,NUMELTG                         
        DO IN = 1,3                           
          NN = IXTG(IN+1,IE)                  
          DO J = 1,3                          
            XREFTG(IN,J,IE) = X(J,NN)      
          ENDDO                               
        ENDDO                                 
      ENDDO                                   
      DO IE=1,NUMELS8                         
        DO IN = 1,8                           
          NN = IXS(IN+1,IE)                   
          DO J = 1,3                          
            XREFS(IN,J,IE) = X(J,NN)       
          ENDDO                               
        ENDDO                                 
      ENDDO        
C
      IF(IDDLEVEL == 0) WRITE(IOUT,1000)
      NITRS = 100
C--------------------------------------------------
C START BROWSING MODEL XREF
C--------------------------------------------------
c
      CALL HM_OPTION_START('/XREF')
c
C--------------------------------------------------
C EXTRACT DATAS
C--------------------------------------------------
C-------------------
C
      !----------------------------------------------------------------------
      ! Loop over XREF
      !----------------------------------------------------------------------
      DO IR = 1, NXREF
C      
        ! Reading the option
        TITR = ''   
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          UNIT_ID = UID,
     .                          SUBMODEL_INDEX = SUB_INDEX,
     .                          SUBMODEL_ID = SUB_ID,
     .                          OPTION_TITR = TITR)
C
        CALL HM_GET_INTV('Comp_Id',PARTID,IS_AVAILABLE,LSUBMODEL)
C
        ! Checking UNIT_ID
        IFLAGUNIT = 0          
        DO J=1,NUNITS                         
          IF (UNITAB%UNIT_ID(J) == UID) THEN               
              IFLAGUNIT = 1                      
              EXIT                              
          ENDIF                               
        ENDDO                                  
        IF (UID/=0.AND.IFLAGUNIT==0) THEN
          CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I2=UID,I1=PARTID,C1='XREF',
     .               C2='XREF',
     .               C3=TITR)                           
        ENDIF       
C
        IP     = 0
        DO I = 1,NPART
          IF (IPART(4,I) == PARTID) THEN
            IP = I
            EXIT
          ENDIF  
        ENDDO
C
        IF (IP /= 0) THEN
          TAGNOD = 0
          TAGELC = 0
          TAGELTG= 0
          TAGELS = 0
          ITYP   = 0
          XTMP = ZERO
          DO IE=1,NUMELC
            IF (IP == IPARTC(IE)) THEN
              TAGELC(IE) = 1
              ITYP = 1
            ENDIF
          ENDDO
          DO IE=1,NUMELTG               
            IF (IP == IPARTG(IE)) THEN  
              TAGELTG(IE) = 1           
              ITYP = 1                  
            ENDIF                       
          ENDDO    
          NSOLID = 0                     
          IF (ITYP == 0) THEN
            DO IE=1,NUMELS8
              IF (IP == IPARTS(IE)) THEN
                TAGELS(IE) = 1
                ITYP = 2
                NSOLID = ISOLNOD(IE)
              ENDIF
            ENDDO
          ENDIF
          IF(ITYP == 2 ) THEN
              IMID =  IPART(1,IP)
              IPID =  IPART(2,IP)
              MAT_ID = IPM(1,IMID)
              MTN = IPM(2, IMID)
              IF(MTN /= 35 .AND.MTN /= 38 .AND. MTN /= 42 .AND.
     .             MTN /= 70 .AND. MTN /= 90)THEN
                   CALL FRETITL2(TITR1,IPM(NPROPMI-LTITR+1,IMID),LTITR)
                   CALL ANCMSG(MSGID=2014,
     .                       MSGTYPE=MSGERROR,
     .                       ANMODE=ANSTOP,
     .                       I1=MAT_ID,
     .                       C1=TITR1,
     .                       I2=MTN )
               END IF
               NPT = IGEO(4,IPID)
               IF( ((NSOLID /= 8 .AND.NSOLID /= 4) .OR.  NPT /= 1 )) THEN
                    CALL FRETITL2(TITR1,IGEO(NPROPGI-LTITR+1,IPID),LTITR)
                    CALL ANCMSG(MSGID=2013,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 I1=IGEO(1,IPID),
     .                 C1=TITR1)   
                ENDIF
          ENDIF
C 
          ! Number of iterations            
          CALL HM_GET_INTV('NITRS',NITER,IS_AVAILABLE,LSUBMODEL)
          NITRS = MAX(NITRS,NITER)
C
          IF(IDDLEVEL == 0) THEN
            WRITE(IOUT,1001) TITR,NITRS,PARTID			
            IF(IPRI >= 5) WRITE(IOUT,'(8X,A7,3(18X,A2))') 'NODE-ID',' X',' Y',' Z'
          ENDIF
C
          ! Read nodes new coordinates
          CALL HM_GET_INTV('refnodesmax',NNOD,IS_AVAILABLE,LSUBMODEL)  
          IF (ALLOCATED(ID)) DEALLOCATE(ID)
          IF (ALLOCATED(XX)) DEALLOCATE(XX)
          IF (ALLOCATED(YY)) DEALLOCATE(YY)
          IF (ALLOCATED(ZZ)) DEALLOCATE(ZZ)
          ALLOCATE(XX(NNOD),YY(NNOD),ZZ(NNOD),ID(NNOD))
C          
          ! Loop over nodes
          DO J = 1,NNOD
C
            CALL HM_GET_INT_ARRAY_INDEX('node_id',ID(J),J,IS_AVAILABLE,LSUBMODEL)
            CALL HM_GET_FLOAT_ARRAY_INDEX('globalx',XX(J),J,IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('globaly',YY(J),J,IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOAT_ARRAY_INDEX('globalz',ZZ(J),J,IS_AVAILABLE, LSUBMODEL, UNITAB)
C
            IF(SUB_ID /= 0)
     .        CALL SUBROTPOINT(XX(J),YY(J),ZZ(J),RTRANS,SUB_ID,LSUBMODEL)
            IF (NSUBDOM==0) NN = USR2SYS(ID(J),ITABM1,MESS,PARTID)
            IF (NSUBDOM>0) THEN
              NN = R2R_SYS(ID(J),ITABM1,MESS)
            ENDIF     
            IF(IDDLEVEL == 0.AND.IPRI >= 5) WRITE(IOUT,'(5X,I10,5X,1P3G20.13)') ID(J),XX(J),YY(J),ZZ(J)
            TAGNOD(NN) = 1
            TAGXREF(NN)= 1
            XTMP(1,NN) = XX(J)
            XTMP(2,NN) = YY(J)
            XTMP(3,NN) = ZZ(J)
          ENDDO 
          IF(IDDLEVEL == 0.AND.IPRI < 5) WRITE(IOUT,1010) NNOD
C
          SELECT CASE (ITYP)
            CASE (1)
              DO IE=1,NUMELC
                IF (TAGELC(IE) == 1) THEN
                  DO IN=1,4
                    NN = IXC(IN+1,IE)
                    IF (TAGNOD(NN) == 1) THEN
                      XREFC(IN,1,IE) = XTMP(1,NN)
                      XREFC(IN,2,IE) = XTMP(2,NN)
                      XREFC(IN,3,IE) = XTMP(3,NN)
                    ENDIF
                  ENDDO
                ENDIF
              ENDDO
              DO IE=1,NUMELTG
                IF (TAGELTG(IE) == 1) THEN
                  DO IN=1,3
                    NN = IXTG(IN+1,IE)
                    IF (TAGNOD(NN) == 1) THEN
                      XREFTG(IN,1,IE) = XTMP(1,NN)
                      XREFTG(IN,2,IE) = XTMP(2,NN)
                      XREFTG(IN,3,IE) = XTMP(3,NN)
                    ENDIF
                  ENDDO
                ENDIF
              ENDDO
            CASE (2)
              DO IE=1,NUMELS8
                IF (TAGELS(IE) == 1) THEN
                  DO IN=1,8
                    NN = IXS(IN+1,IE)
                    IF (TAGNOD(NN) == 1) THEN
                      XREFS(IN,1,IE) = XTMP(1,NN)
                      XREFS(IN,2,IE) = XTMP(2,NN)
                      XREFS(IN,3,IE) = XTMP(3,NN)
                    ENDIF
                  ENDDO
                ENDIF
              ENDDO
          END SELECT
        ENDIF
      END DO
C
      ! Table deallocation
      IF (ALLOCATED(ID)) DEALLOCATE(ID)
      IF (ALLOCATED(XX)) DEALLOCATE(XX)
      IF (ALLOCATED(YY)) DEALLOCATE(YY)
      IF (ALLOCATED(ZZ)) DEALLOCATE(ZZ)
C-----------
      RETURN
 999  CALL FREERR(3)
      RETURN
 1000 FORMAT(//
     & 5X,'    REFERENCE STATE (XREF)  ',/
     & 5X,'    ----------------------  ' )
 1001 FORMAT(/
     & 5X, A  ,/
     & 5X,'NUMBER OF ITERATIONS. . . . . . =',I10/
     & 5X,'PART ID . . . . . . . . . . . . =',I10)
 1010 FORMAT(
     & 5X,'NUMBER OF NODES . . . . . . . . =',I10)
      END SUBROUTINE HM_READ_XREF
