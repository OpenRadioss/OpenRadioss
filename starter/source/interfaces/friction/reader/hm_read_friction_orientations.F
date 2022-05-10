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
Chd|  HM_READ_FRICTION_ORIENTATIONS source/interfaces/friction/reader/hm_read_friction_orientations.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        FRICTION_PARTS_SEARCH         source/interfaces/inter3d1/i7sti3.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        SUBROTVECT                    source/model/submodel/subrot.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        INTBUF_FRIC_MOD               share/modules1/intbuf_fric_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_FRICTION_ORIENTATIONS (
     1        INTBUF_FRIC_TAB,NPFRICORTH,IGRPART ,IPART     ,PFRICORTH  ,
     2        IREPFORTH ,ISKN       ,PHIFORTH    ,VFORTH    ,SKEW       ,
     3        IFLAG    ,TAGPRT_FRIC ,RTRANS      ,LSUBMODEL ,UNITAB     )

C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE R2R_MOD
      USE MESSAGE_MOD
      USE INTBUF_FRIC_MOD        
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE UNITAB_MOD
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
#include      "com01_c.inc"
#include      "com02_c.inc"
#include      "com03_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr12_c.inc"
#include      "scr17_c.inc"
#include      "sysunit.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IFLAG ,NPFRICORTH
      INTEGER IPART(LIPART1,*) ,PFRICORTH(*),IREPFORTH(*),TAGPRT_FRIC(*),
     .        ISKN(LISKN,*) !,IDFRICORIENT(*)

      my_real
     .   PHIFORTH(*) ,VFORTH(3,*) ,SKEW(LSKEW,*) ,RTRANS(NTRANSF,*)

c      CHARACTER*ncharline ,  TITFRICORIENT(*)
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRPART) :: IGRPART
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIF ,NIN ,ISK ,IERRR ,IREP ,NOINTFORTH ,IDSK ,
     .      FLAGP ,FLAGGRP ,GRPART ,IDPART ,N ,KK ,IDTGRS ,IPL ,J ,IP ,
     .      IPG ,SUB_ID ,NINPUT ,NL 
      my_real AN ,VX ,VY ,VZ ,PHI 
      CHARACTER*nchartitle,
     .   TITR 
      LOGICAL IS_AVAILABLE        
C
C=======================================================================
C READING /FRIC_ORIENT
C=======================================================================
      IS_AVAILABLE = .FALSE.
C
C--------------------------------------------------
C WRITE TITLE IN OUT FILE
C--------------------------------------------------

      IF(IFLAG==1)    WRITE(IOUT,1000)

      NPFRICORTH = 0 

C--------------------------------------------------
C START BROWSING FRICTION ORIENTATIONS
C--------------------------------------------------
      CALL HM_OPTION_START('/FRIC_ORIENT')
C--------------------------------------------------
C BROWSING FRICTION ORIENTATIONS MODELS 1->NFRIC_ORIENT
C--------------------------------------------------
      DO NIN=1,NFRIC_ORIENT
c
C--------------------------------------------------
C EXTRACT DATAS OF /FRIC_ORIENT
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = NOINTFORTH,
     .                       SUBMODEL_ID = SUB_ID,
     .                       OPTION_TITR = TITR)

c          KFRICORIENT = KFRICORIENT + 1          

C--Output ---

        IF(IFLAG==1) THEN
           WRITE(IOUT,1500) NOINTFORTH, TRIM(TITR)
        ENDIF


C EXTRACT DATAS (INTEGER VALUES) : Number of connected parts as defined by user 
        CALL HM_GET_INTV('n_orient',NINPUT,IS_AVAILABLE,LSUBMODEL)

        DO NL=1,NINPUT

C EXTRACT DATAS (INTEGER VALUES) : 

          CALL HM_GET_INT_ARRAY_INDEX('grpart_ID1',GRPART,NL,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INT_ARRAY_INDEX('part_ID1',IDPART,NL,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INT_ARRAY_INDEX('skew_id',IDSK,NL,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INT_ARRAY_INDEX('iorth',IREP,NL,IS_AVAILABLE,LSUBMODEL)

C EXTRACT DATAS (REAL VALUES) : 

          CALL HM_GET_FLOAT_ARRAY_INDEX('vx',VX,NL,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('vy',VY,NL,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('vz',VZ,NL,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOAT_ARRAY_INDEX('phi',PHI,NL,IS_AVAILABLE,LSUBMODEL,UNITAB)
C--------------------
          IF (SUB_ID .NE. 0)
     .           CALL SUBROTVECT(VX,VY,VZ,RTRANS,SUB_ID,LSUBMODEL)

C
C----CHECK PARTS 
C 
          FLAGP = 0
          FLAGGRP = 0  
          IF(IDPART/=0)THEN
             DO N=1,NPART
                IF(IDPART == IPART(4,N))THEN
                   FLAGP = 1 
                   IP = N
                   EXIT
                ENDIF
             ENDDO

             IF(FLAGP == 0)THEN
                CALL ANCMSG(MSGID=1642,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=NOINTFORTH,
     .                      C1=TITR,
     .                      I2=IDPART)

              ENDIF
          ENDIF
C
C----CHECK PARTS group
C
          IF(GRPART/=0)THEN
             FLAGGRP = 0
             KK=NGRNOD+
     +          NGRBRIC+NGRQUAD+NGRSHEL+NGRSH3N+NGRTRUS+NGRBEAM+NGRSPRI
             DO N=1,NGRPART
                IF (IGRPART(N)%ID == GRPART) THEN
                  IDTGRS=N
                  FLAGGRP = 1
                  EXIT
                END IF
             END DO
             IF(FLAGGRP ==  0) THEN
                CALL ANCMSG(MSGID=1642,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=NOINTFORTH,
     .                      C1=TITR,
     .                      I2=GRPART)
             ENDIF
          ENDIF
C
C----CHECK Values
C

C   Vect orhotrope

          AN=SQRT(VX*VX+VY*VY+VZ*VZ)
          IF(AN .LT. EM10)THEN
             VX=UN
             VY=ZERO
             VZ=ZERO
          ELSE
             VX=VX/AN
             VY=VY/AN
             VZ=VZ/AN
          ENDIF

          ISK = 0
          IF (IDSK.NE.0) THEN
              IERRR = 0
              DO J=0,NUMSKW+NSUBMOD
                 IF(IDSK == ISKN(4,J+1)) THEN
                    ISK=J+1
                    IERRR = 1
                    EXIT
                 ENDIF
              END DO
              IF(IERRR == 0 ) THEN
                 CALL ANCMSG(MSGID=184,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    C1='FRICTION ORIENTATION PART',
     .                    I1=NOINTFORTH,
     .                    C2='FRICTION ORIENTATION PART',
     .                    C3=TITR,
     .                    I2=IDSK) 
              ENDIF
          ENDIF

C
C COUNTING AND STORAGE IN TEMPARY TABLES
C
            
          IF(FLAGP > 0) THEN
             IPG = TAGPRT_FRIC(IP)
             IF(IPG > 0) THEN
                DO NIF =1,NINTERFRIC
                   CALL FRICTION_PARTS_SEARCH (
     .                  IPG,INTBUF_FRIC_TAB(NIF)%S_TABPARTS_FRIC,
     .                  INTBUF_FRIC_TAB(NIF)%TABPARTS_FRIC,IPL ) 
                   IF(IPL >0) THEN
                      NPFRICORTH = NPFRICORTH + 1 
                      IF(IFLAG ==1 ) THEN
                         PFRICORTH(IP) = NPFRICORTH
                         PHIFORTH(NPFRICORTH) = PHI
                         IREPFORTH(NPFRICORTH) = IREP          
                         IF(ISK == 0) THEN
                            VFORTH(1,NPFRICORTH) = VX  
                            VFORTH(2,NPFRICORTH) = VY                   
                            VFORTH(3,NPFRICORTH) = VZ     
                         ELSE    
                            VFORTH(1,NPFRICORTH) = SKEW(1,ISK)  
                            VFORTH(2,NPFRICORTH) = SKEW(2,ISK)                   
                            VFORTH(3,NPFRICORTH) = SKEW(3,ISK)                                                         
                         ENDIF
                      ENDIF
                   ENDIF
                ENDDO
             ENDIF

             IF(IFLAG==1) THEN
                WRITE(IOUT,1501) IDPART
                IF(ISK==0) THEN
                   WRITE(IOUT,1503) IREP,VX,VY,VZ
                ELSE
                  WRITE(IOUT,1504) IREP,IDSK
                ENDIF
             ENDIF
          ENDIF  

          IF(FLAGGRP > 0) THEN
             DO J=1,IGRPART(IDTGRS)%NENTITY
                IP=IGRPART(IDTGRS)%ENTITY(J)
                IPG = TAGPRT_FRIC(IP)
                IF(IPG > 0) THEN
                   DO NIF =1,NINTERFRIC
                      CALL FRICTION_PARTS_SEARCH (
     .                     IPG,INTBUF_FRIC_TAB(NIF)%S_TABPARTS_FRIC,
     .                     INTBUF_FRIC_TAB(NIF)%TABPARTS_FRIC,IPL ) 
                      IF(IPL > 0) THEN
                         NPFRICORTH = NPFRICORTH + 1
                         IF(IFLAG ==1 ) THEN
                             PFRICORTH(IP) = NPFRICORTH
                             PHIFORTH(NPFRICORTH) = PHI  
                             IREPFORTH(NPFRICORTH) = IREP      
                             IF(ISK == 0) THEN
                               VFORTH(1,NPFRICORTH) = VX  
                               VFORTH(2,NPFRICORTH) = VY                   
                               VFORTH(3,NPFRICORTH) = VZ     
                             ELSE    
                               VFORTH(1,NPFRICORTH) = SKEW(1,ISK)  
                               VFORTH(2,NPFRICORTH) = SKEW(2,ISK)                   
                               VFORTH(3,NPFRICORTH) = SKEW(3,ISK)                                                         
                             ENDIF
                         ENDIF
                      ENDIF
                   ENDDO
                ENDIF
             ENDDO
             IF(IFLAG==1) THEN
                WRITE(IOUT,1502) GRPART
                IF(ISK==0) THEN
                   WRITE(IOUT,1503) IREP,VX,VY,VZ
                ELSE
                   WRITE(IOUT,1504) IREP,IDSK
               ENDIF
             ENDIF
          ENDIF 
C

        ENDDO  ! N=1,NLINE
      ENDDO !NIN=1,NFRIC_ORIENT
C
C=======================================================================
      RETURN
 999  CALL FREERR(3)
      RETURN
C-----
 1000 FORMAT(    /1X,'   FRICTION ORIENTATIONS      ' /
     .            1X,' --------------             '// )

 1500 FORMAT(/1X,'     FRICTION ORIENTATIONS CARD NUMBER :',I10,1X,A/
     .            1X,'     -------------------------------             '/)
 1501 FORMAT(/
     .    '          PART . . . . . . . . . . . . . . . . . . ',I10)
 1502 FORMAT(/
     .    '          GR_PART . . . . . . . . . . . . . . . . .',I10)
 1503 FORMAT(
     .    '           LOCAL ORTOTHROPY SYSTEM FLAG. . . . . . =',I10/,
     .    '           X COMPONENT OF DIR 1 OF ORTHOTROPY. . . =',1PG20.13/,
     .    '           Y COMPONENT OF DIR 1 OF ORTHOTROPY. . . =',1PG20.13/,
     .    '           Z COMPONENT OF DIR 1 OF ORTHOTROPY. . . =',1PG20.13/)
 1504 FORMAT(
     .    '           LOCAL ORTOTHROPY SYSTEM FLAG. . . . . . =',I10/,
     .    '           SKEW OF THE FIRST ORTHOTROPY DIRECTION. =',I10/)

      END SUBROUTINE HM_READ_FRICTION_ORIENTATIONS
