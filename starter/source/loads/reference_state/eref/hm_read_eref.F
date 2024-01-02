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
Chd|====================================================================
Chd|  HM_READ_EREF                  source/loads/reference_state/eref/hm_read_eref.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        NINTRI                        source/system/nintrr.F        
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_EREF(ITABM1  ,IPART ,IPARTC ,IPARTG   ,IPARTS,
     .                        IXC     ,IXTG  ,IXS    ,X        ,XREFC   ,
     .                        XREFTG  ,XREFS ,LSUBMODEL,IDDLEVEL,ITAB   ,
     .                        TAGXREF ,TAGREFSTA )
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   G l o b a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  ITABM1(*),IPART(LIPART1,*),IPARTC(*),IPARTG(*),IPARTS(*)
      INTEGER  IXC(NIXC,*),IXTG(NIXTG,*),IXS(NIXS,*)
      INTEGER  IDDLEVEL,ITAB(NUMNOD),TAGXREF(*),TAGREFSTA(*)
      my_real
     .  X(3,*),XREFC(4,3,*),XREFTG(3,3,*),XREFS(8,3,*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  TAGELC(NUMELC),TAGELTG(NUMELTG),TAGELS(NUMELS8)
      INTEGER  TAGNOD(NUMNOD)
      INTEGER  I,IX(8),IE,J,IN,ID,IP,IR,NN,PARTID,UID,ITYP
      INTEGER  SUB_ID,MM, NEL
      CHARACTER TITLE*ncharline,KEY1*ncharkey,KEY*ncharkey,MESS*40
      LOGICAL :: IS_AVAILABLE
      DATA MESS/'EREF ELEMENT REFERENCE GEOMETRY'/              
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS,R2R_SYS,NINTRI
C=======================================================================
C
      IS_AVAILABLE = .FALSE.
C
      IF(NXREF == 0 .AND. IREFSTA == 0) THEN
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
      ENDIF
C
      IF(IDDLEVEL == 0) WRITE(IOUT,1000)
      NITRS = 100
      TAGNOD(1:NUMNOD) = 0
C
      CALL HM_OPTION_START('/EREF')
C
      DO IR=1,NEREF
C
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       UNIT_ID = UID,
     .                       SUBMODEL_ID = SUB_ID,
     .                       OPTION_TITR = TITLE,
     .                       KEYWORD1 = KEY1,
     .                       KEYWORD2 = KEY)
C
        CALL HM_GET_INTV('component',PARTID,IS_AVAILABLE,LSUBMODEL)
C
        IF (KEY1(1:4) == 'EREF')THEN
          IF(IDDLEVEL == 0) WRITE(IOUT,1001) TITLE,PARTID
          IP = 0
          DO I = 1,NPART
            IF (IPART(4,I) == PARTID) IP = I
          ENDDO
C
          TAGELC(1:NUMELC)  = 0
          TAGELTG(1:NUMELTG)= 0
          TAGELS(1:NUMELS8) = 0
          ITYP   = 0
C
          DO IE=1,NUMELC
            IF (IP == IPARTC(IE).OR.IP==0) THEN
              TAGELC(IE) = 1
              ITYP = 1
            ENDIF
          ENDDO
          DO IE=1,NUMELTG               
            IF (IP == IPARTG(IE).OR.IP==0) THEN  
              TAGELTG(IE) = 1           
              ITYP = 1                  
            ENDIF                       
          ENDDO                         
          IF (ITYP == 0) THEN
            DO IE=1,NUMELS8
              IF (IP == IPARTS(IE).OR.IP==0) THEN
                TAGELS(IE) = 1
                ITYP = 2
              ENDIF
            ENDDO
          ENDIF
C
          SELECT CASE (ITYP)
          CASE (1)
          IF(KEY(1:5)=='SHELL') THEN
            IF(IDDLEVEL == 0.AND.IPRI >= 5) WRITE(IOUT,'(9X,A7,4(9X,A2))') 'ELEM-ID','N1','N2','N3','N4'
            CALL HM_GET_INTV('table_count',NEL,IS_AVAILABLE,LSUBMODEL)
C
            DO I=1,NEL
C
              CALL HM_GET_INT_ARRAY_INDEX('elems_table_elem',ID,I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('elems_table_n1',IX(1),I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('elems_table_n2',IX(2),I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('elems_table_n3',IX(3),I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('elems_table_n4',IX(4),I,IS_AVAILABLE,LSUBMODEL)
C
              IE=NINTRI(ID,IXC,NIXC,NUMELC,NIXC)
              IF(ID > 0 .AND. IE == 0) THEN
                CALL ANCMSG(MSGID=1011,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO,
     .                      C1='SHELL',I1=ID)
              ELSEIF (ID > 0 .AND. TAGELC(IE) == 1) THEN
                DO IN=1,4
                  NN = USR2SYS(IX(IN),ITABM1,MESS,ID)
                  MM = IXC(IN+1,IE)
                  TAGNOD(MM) = 1          
                  XREFC(IN,1,IE) = X(1,NN)
                  XREFC(IN,2,IE) = X(2,NN)
                  XREFC(IN,3,IE) = X(3,NN)
                ENDDO
                IF(IDDLEVEL == 0.AND.IPRI >= 5) WRITE(IOUT,'(5X,5(1X,I10))') ID,(IX(IN),IN=1,4)
              ENDIF
            ENDDO
            IF(IDDLEVEL == 0.AND.IPRI < 5) WRITE(IOUT,1010) NEL
C
          ELSEIF(KEY(1:4)=='SH3N') THEN
            IF(IDDLEVEL == 0.AND.IPRI >= 5) WRITE(IOUT,'(9X,A7,4(9X,A2))') 'ELEM-ID','N1','N2','N3'
            CALL HM_GET_INTV('table_count',NEL,IS_AVAILABLE,LSUBMODEL)
C
            DO I=1,NEL
C
              CALL HM_GET_INT_ARRAY_INDEX('elems_table_elem',ID,I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('elems_table_n1',IX(1),I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('elems_table_n2',IX(2),I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('elems_table_n3',IX(3),I,IS_AVAILABLE,LSUBMODEL)
C
              IE=NINTRI(ID,IXTG,NIXTG,NUMELTG,NIXTG)
              IF(ID > 0 .AND. IE == 0) THEN
                CALL ANCMSG(MSGID=1011,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO,
     .                      C1='TRIANGLE',I1=ID)
              ELSEIF (ID > 0 .AND. TAGELTG(IE) == 1) THEN
                DO IN=1,3
                  NN = USR2SYS(IX(IN),ITABM1,MESS,ID)
                  MM = IXTG(IN+1,IE)
                  TAGNOD(MM) = 1
                  XREFTG(IN,1,IE) = X(1,NN)
                  XREFTG(IN,2,IE) = X(2,NN)
                  XREFTG(IN,3,IE) = X(3,NN)
                ENDDO
                IF(IDDLEVEL == 0.AND.IPRI >= 5) WRITE(IOUT,'(5X,4(1X,I10))') ID,(IX(IN),IN=1,3)
              ENDIF
            ENDDO
            IF(IDDLEVEL == 0.AND.IPRI < 5) WRITE(IOUT,1020) NEL
          ENDIF
C
          CASE (2)
          IF(KEY(1:5)=='BRICK') THEN
C
              IF(IDDLEVEL == 0.AND.IPRI >= 5)WRITE(IOUT,'(9X,A7,8(9X,A2))')'ELEM-ID','N1','N2','N3','N4','N5','N6','N7','N8'
            CALL HM_GET_INTV('table_count',NEL,IS_AVAILABLE,LSUBMODEL)
C
            DO I=1,NEL
C
              CALL HM_GET_INT_ARRAY_INDEX('elems_table_elem',ID,I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('elems_table_n1',IX(1),I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('elems_table_n2',IX(2),I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('elems_table_n3',IX(3),I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('elems_table_n4',IX(4),I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('elems_table_n5',IX(5),I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('elems_table_n6',IX(6),I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('elems_table_n7',IX(7),I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('elems_table_n8',IX(8),I,IS_AVAILABLE,LSUBMODEL)
C
              IE=NINTRI(ID,IXS,NIXS,NUMELS8,NIXS)
              IF(ID > 0 .AND. IE == 0) THEN
                CALL ANCMSG(MSGID=1011,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO,
     .                      C1='SOLID',I1=ID)
              ELSEIF (ID > 0 .AND. TAGELS(IE) == 1) THEN
                DO IN=1,8
                  NN = USR2SYS(IX(IN),ITABM1,MESS,ID)
                  MM = IXS(IN+1,IE)
                  TAGNOD(MM) = 1
                  XREFS(IN,1,IE) = X(1,NN)
                  XREFS(IN,2,IE) = X(2,NN)
                  XREFS(IN,3,IE) = X(3,NN)
                ENDDO
                IF(IDDLEVEL == 0.AND.IPRI >= 5) WRITE(IOUT,'(5X,9(1X,I10))') ID,(IX(IN),IN=1,8)
              ENDIF
            ENDDO
            IF(IDDLEVEL == 0.AND.IPRI < 5) WRITE(IOUT,1030) NEL
          ELSEIF(KEY(1:6)=='TETRA4') THEN
              IF(IDDLEVEL == 0.AND.IPRI >= 5) WRITE(IOUT,'(9X,A7,4(9X,A2))') 'ELEM-ID','N1','N2','N3','N4'
            CALL HM_GET_INTV('table_count',NEL,IS_AVAILABLE,LSUBMODEL)
C
            DO I=1,NEL
C
              CALL HM_GET_INT_ARRAY_INDEX('elems_table_elem',ID,I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('elems_table_n1',IX(1),I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('elems_table_n2',IX(3),I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('elems_table_n3',IX(6),I,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_INT_ARRAY_INDEX('elems_table_n4',IX(5),I,IS_AVAILABLE,LSUBMODEL)
C
              IX(2)=IX(1)
              IX(4)=IX(3)
              IX(8)=IX(5)
              IX(7)=IX(6)
              IE=NINTRI(ID,IXS,NIXS,NUMELS8,NIXS)
C
              IF(ID > 0 .AND. IE == 0) THEN
                CALL ANCMSG(MSGID=1011,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO,
     .                      C1='TETRA4',I1=ID)
              ELSEIF (ID > 0 .AND. TAGELS(IE) == 1) THEN
                DO IN=1,8
                  NN = USR2SYS(IX(IN),ITABM1,MESS,ID)
                  MM = IXS(IN+1,IE)
                  TAGNOD(MM) = 1
                  XREFS(IN,1,IE) = X(1,NN)
                  XREFS(IN,2,IE) = X(2,NN)
                  XREFS(IN,3,IE) = X(3,NN)
                ENDDO
                IF(IDDLEVEL == 0.AND.IPRI >= 5) WRITE(IOUT,'(5X,5(1X,I10))') ID,IX(1),IX(3),IX(6),IX(5)
              ENDIF
            ENDDO
            IF(IDDLEVEL == 0.AND.IPRI < 5) WRITE(IOUT,1040) NEL
          ENDIF
          END SELECT 
C
        ENDIF
      ENDDO
C------------------------------------------
C  CHECK COMPATIBILITY WITH XREF AND REFSTA
C------------------------------------------
      IF(IDDLEVEL == 1) RETURN
      IF( NXREF /= 0) THEN
        DO IN=1,NUMNOD
           IF(TAGNOD(IN) == 0) CYCLE
           IF(TAGXREF(IN) == 1) THEN
             CALL ANCMSG(MSGID=1098,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .                   I1=ITAB(IN))
           ENDIF
        ENDDO
      ENDIF
      IF( IREFSTA /= 0) THEN
        DO IN=1,NUMNOD
           IF(TAGNOD(IN) == 0) CYCLE
           IF(TAGREFSTA(IN) == 1) THEN
             CALL ANCMSG(MSGID=1099,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .                   I1=ITAB(IN))
           ENDIF
        ENDDO
      ENDIF
C
      RETURN
 1000 FORMAT(//
     & 5X,'    REFERENCE STATE (EREF)  ',/
     & 5X,'    ----------------------  ' )
 1001 FORMAT(/
     & 5X, A  ,/
     & 5X,'PART ID . . . . . . . . . . . . =',I10)
 1010 FORMAT(
     & 5X,'NUMBER OF 4-NODES SHELL . . . . =',I10)
 1020 FORMAT(
     & 5X,'NUMBER OF 3-NODES SHELL . . . . =',I10)
 1030 FORMAT(
     & 5X,'NUMBER OF 8-NODES BRICK . . . . =',I10)
 1040 FORMAT(
     & 5X,'NUMBER OF 4-NODES TETRA . . . . =',I10)
      END SUBROUTINE HM_READ_EREF
