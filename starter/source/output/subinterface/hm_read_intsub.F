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
Chd|  HM_READ_INTSUB                source/output/subinterface/hm_read_intsub.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL                       source/starter/freform.F      
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_SZ_R2R                     source/coupling/rad2rad/routines_r2r.F
Chd|        UDOUBLE                       source/system/sysfus.F        
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_INTSUB(IGRNOD,IGRSURF,NOM_OPT,IGRSLIN,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE HM_OPTION_READ_MOD
      USE SUBMODEL_MOD
      USE R2R_MOD
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
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NOM_OPT(LNOPT1,*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (SURF_)   ,TARGET,  DIMENSION(NSURF)   :: IGRSURF
      TYPE (SURF_)   ,TARGET,  DIMENSION(NSLIN)   :: IGRSLIN
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER WORK(70000)
      INTEGER I,NI,NOINT,L,J,K,NISUB,ID,IDINT,IDGRN,IDSURF,IDSURF1,IDSURF2,
     .        TRU,N,J10(10),IA,ITH,INDEX_NOM_OPT
      INTEGER UID,SUB_ID,ID1,ID2,ID3,
     .        INTSUB_TYP(NINTSUB),ISLIN1,ISLIN2,ISU1,ISU2,ISURF1,ISURF2,NFLAG,STAT
      INTEGER, DIMENSION(:), ALLOCATABLE :: ID_INTER,FLAG_INTER
      my_real
     .   BID
      CHARACTER TITR*nchartitle,MESS*40,KEY*ncharline,KEY2*ncharfield,COPT*ncharline
!
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
      INTEGER :: II, NVAR, IDSMAX, NTHINTER,IVAR,NB,ID_TYPE19,OKSURF
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NGR2USR
C-----------------------------------------------
C   D a t a
C-----------------------------------------------
      DATA MESS/'INTERFACES                              '/
C-----------------------------------------------
      WRITE(IOUT,1000)
C
      NI   =0
      INDEX_NOM_OPT = 0
      ALLOCATE (ID_INTER(HM_NINTER),STAT=stat)
      ID_INTER(1:(HM_NINTER)) = 0
      ALLOCATE (FLAG_INTER(HM_NINTER),STAT=stat)
      FLAG_INTER(1:(HM_NINTER)) = 0
C
      CALL HM_OPTION_START('/INTER')
C
      NB = 0
C
      DO I =1,HM_NINTER
C
         NB = NB+1
C----------Multidomaines --> on ignore les interfaces et sous-interfaces non tagees--- 
         IF (NSUBDOM>0) THEN
           IF(TAGINT(NB)==0) CALL HM_SZ_R2R(TAGINT,NB,LSUBMODEL)
         ENDIF
C------------------------------------------------------------------
C
C--------------------------------------------------
C EXTRACT DATAS OF /INTER/... LINE
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = NOINT,
     .                       UNIT_ID = UID,
     .                       OPTION_TITR = TITR,
     .                       KEYWORD2 = KEY)

        IF(KEY(1:LEN_TRIM(KEY))/='SUB') THEN
          NI=NI+1
          INDEX_NOM_OPT=INDEX_NOM_OPT+1
C
C--       read of type19 flag
          ID_TYPE19=0
          CALL HM_GET_INTV('ID_TYPE19',ID_TYPE19,IS_AVAILABLE,LSUBMODEL)        
            
          SELECT CASE(KEY(1:LEN_TRIM(KEY)))

           CASE ('TYPE25')
              FLAG_INTER(NI)    =25
c comment to plug t19 convert
           CASE ('TYPE7','TYPE10','TYPE24')
              FLAG_INTER(NI)    =0
              IF (ID_TYPE19/=0) THEN
C--             type7 interface generated from type19
                FLAG_INTER(NI)    =2
              ENDIF
           CASE ('TYPE11')
              FLAG_INTER(NI)    =1
              IF (ID_TYPE19/=0) THEN
C--             type11 interface generated from type19
                FLAG_INTER(NI)    =2
              ENDIF
           CASE DEFAULT
              FLAG_INTER(NI)    =-1
          END SELECT
C
          ID_INTER(NI) = NOINT
          NOM_OPT(1,INDEX_NOM_OPT)=NOINT
C
          CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,
     .                              NI),LTITR)
        ENDIF
      ENDDO
C
      NB = HM_NINTER
C
      NFLAG = NI
C
C-------------------------------------
C
      CALL HM_OPTION_START('/INTER')
      NISUB=0
      NB = 0
C
      DO I =1,HM_NINTER
C
        NB = NB+1
C----------Multidomaines --> on ignore les interfaces et sous-interfaces non tagees--- 
        IF (NSUBDOM>0) THEN
          IF(TAGINT(NB)==0) CALL HM_SZ_R2R(TAGINT,NB,LSUBMODEL)
        ENDIF
C------------------------------------------------------------------
c
C--------------------------------------------------
C EXTRACT DATAS OF /INTER/... LINE
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       UNIT_ID = UID,
     .                       OPTION_TITR = TITR,
     .                       SUBMODEL_INDEX = SUB_ID,
     .                       KEYWORD2 = KEY)

        IF(KEY(1:LEN_TRIM(KEY))=='SUB') THEN
          NISUB=NISUB+1
          NOM_OPT(1,NINTER+NISUB)=ID
          CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,
     .                              NINTER+NISUB),LTITR)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
          CALL HM_GET_INTV('InterfaceId',IDINT,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('mainentityids',ID1,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('secondaryentityids',ID2,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('Main_ID2',ID3,IS_AVAILABLE,LSUBMODEL)   

          NOM_OPT(2,NINTER+NISUB)=IDINT
          NOM_OPT(3,NINTER+NISUB)=ID1
          NOM_OPT(4,NINTER+NISUB)=ID2
          NOM_OPT(6,NINTER+NISUB)=ID3
        END IF
      END DO
C
C-------------------------------------
C Recherche des ID doubles
C-------------------------------------
      CALL UDOUBLE(NOM_OPT,LNOPT1,NINTER+NINTSUB,MESS,0,BID)
C-------------------------------------
C     verification des ss-interfaces (interfaces concernees) et definition du type
C-------------------------------------
      DO I=1,NINTSUB
        IDINT=NOM_OPT(2,NINTER+I)
        ID=NOM_OPT(1,NINTER+I)
        CALL FRETITL2(TITR,
     .       NOM_OPT(LNOPT1-LTITR+1,NINTER+I),LTITR)
        IF(IDINT /= 0) THEN
          DO NI=1,NFLAG
             IF(ID_INTER(NI) == IDINT)THEN
               INTSUB_TYP(I) = FLAG_INTER(NI)
               GO TO 320
             END IF
          END DO
          CALL ANCMSG(MSGID=578,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID,
     .                C1=TITR,
     .                I2=IDINT)
          INTSUB_TYP(I) = 2
 320      CONTINUE
C
          IF(FLAG_INTER(NI)==-1)THEN
              CALL ANCMSG(MSGID=579,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID,
     .                    C1=TITR)
           END IF
         ELSE
           INTSUB_TYP(I) = 100
         ENDIF
      END DO
C-------------------------------------
C     decodage de INTSUB (nos internes des surfaces, groupes de noeuds).
C-------------------------------------
      DO I=1,NINTSUB
C
        IF (INTSUB_TYP(I)==25 )THEN
C
C--     Interf 25
C
          IDSURF=NOM_OPT(3,NINTER+I)
          ID=NOM_OPT(1,NINTER+I)
          CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,
     .                          NINTER+I),LTITR)
          DO J=1,NSURF
            IF(IGRSURF(J)%ID==IDSURF)THEN
              NOM_OPT(3,NINTER+I)=J
              GO TO 400
            END IF
          END DO
          CALL ANCMSG(MSGID=576,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID,
     .                C1=TITR,
     .                I2=IDSURF)
 400      CONTINUE

          IDGRN=NOM_OPT(4,NINTER+I)
          IF(IDGRN/=0)THEN
            DO J=1,NGRNOD
              IF (IGRNOD(J)%ID==IDGRN) THEN
                NOM_OPT(4,NINTER+I)=J
                GO TO 410
              END IF
            END DO
            CALL ANCMSG(MSGID=577,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=IDGRN)
 410        CONTINUE
          END IF
C
          IDSURF=NOM_OPT(6,NINTER+I)
          IF(IDSURF/=0)THEN
            DO J=1,NSURF
              IF(IGRSURF(J)%ID==IDSURF)THEN
                NOM_OPT(6,NINTER+I)=J
                GO TO 420
              END IF
            END DO
            CALL ANCMSG(MSGID=576,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=IDSURF)
 420        CONTINUE
          END IF
C
          IDSURF1=NOM_OPT(3,NINTER+I)
          IDSURF2=NOM_OPT(6,NINTER+I)
          IDGRN=NOM_OPT(4,NINTER+I)
          IF(IDSURF1/=0.AND.IDSURF2==0.AND.IDGRN==0) THEN
            CALL ANCMSG(MSGID=2055,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO,
     .                I1=ID,
     .                C1=TITR)
          ENDIF
          IF(IDSURF2/=0.AND.IDSURF1==0.AND.IDGRN==0) THEN
            CALL ANCMSG(MSGID=2055,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO,
     .                I1=ID,
     .                C1=TITR)
          ENDIF
          IF(IDGRN/=0.AND.IDSURF1==0.AND.IDSURF2==0) THEN
            CALL ANCMSG(MSGID=2055,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO,
     .                I1=ID,
     .                C1=TITR)
          ENDIF
        ELSEIF (INTSUB_TYP(I)==0 )THEN
C
C--     Interf 7,10,20,24
C
          IDSURF=NOM_OPT(3,NINTER+I)
          ID=NOM_OPT(1,NINTER+I)
          CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,
     .                          NINTER+I),LTITR)
          DO J=1,NSURF
            IF(IGRSURF(J)%ID==IDSURF)THEN
              NOM_OPT(3,NINTER+I)=J
              GO TO 300
            END IF
          END DO
          CALL ANCMSG(MSGID=576,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID,
     .                C1=TITR,
     .                I2=IDSURF)
 300      CONTINUE
          IDGRN=NOM_OPT(4,NINTER+I)
          DO J=1,NGRNOD
            IF (IGRNOD(J)%ID==IDGRN) THEN
              NOM_OPT(4,NINTER+I)=J
              GO TO 310
            END IF
          END DO
          CALL ANCMSG(MSGID=577,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID,
     .                C1=TITR,
     .                I2=IDGRN)
 310      CONTINUE
C
        ELSEIF (INTSUB_TYP(I)==1) THEN
C
C--     Interf 11
C
          ISU1=NOM_OPT(3,NINTER+I)
          ISU2=NOM_OPT(4,NINTER+I)
          ID=NOM_OPT(1,NINTER+I)
          CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,
     .                          NINTER+I),LTITR)
C
          INGR2USR => IGRSLIN(1:NSLIN)%ID
          ISLIN1=NGR2USR(ISU1,INGR2USR,NSLIN)
          ISLIN2=NGR2USR(ISU2,INGR2USR,NSLIN)
          NOM_OPT(3,NINTER+I)=ISLIN1
          NOM_OPT(4,NINTER+I)=ISLIN2
C
          IF (ISLIN1==0) THEN
            CALL ANCMSG(MSGID=1187,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID,
     .                C1=TITR,
     .                I2=ISU1)
          ENDIF
C
          IF (ISLIN2==0) THEN
            CALL ANCMSG(MSGID=1187,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID,
     .                C1=TITR,
     .                I2=ISU2)
          ENDIF
C
        ELSEIF (INTSUB_TYP(I)==2) THEN
C
C--     Interf 19         
C
          ISU1=NOM_OPT(3,NINTER+I)
          ISU2=NOM_OPT(4,NINTER+I)
          ID=NOM_OPT(1,NINTER+I)
          CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,
     .                          NINTER+I),LTITR)
C
C    ------   If one of the 2 surface is empty then type7 is symmetric ------
          IF ((ISU1==0).AND.(ISU2/=0)) THEN
            ISU1 = ISU2
          ELSEIF ((ISU1/=0).AND.(ISU2==0)) THEN
            ISU2 = ISU1                 
          ENDIF 
C
          INGR2USR => IGRSURF(1:NSURF)%ID
          ISURF1=NGR2USR(ISU1,INGR2USR,NSURF)
          ISURF2=NGR2USR(ISU2,INGR2USR,NSURF)
          NOM_OPT(3,NINTER+I)=ISURF1
          NOM_OPT(4,NINTER+I)=ISURF2
C
          IF (ISURF1==0) THEN
            CALL ANCMSG(MSGID=1197,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID,
     .                C1=TITR,
     .                I2=ISU1)
          ENDIF
C
          IF (ISURF2==0) THEN
            CALL ANCMSG(MSGID=1197,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID,
     .                C1=TITR,
     .                I2=ISU2)
          ENDIF
C
        ELSEIF (INTSUB_TYP(I)==100 )THEN
C
C--     Interf 0 : adding all contacts
C

          IDSURF=NOM_OPT(6,NINTER+I)
          ID=NOM_OPT(1,NINTER+I)
          CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,
     .                          NINTER+I),LTITR)
          OKSURF = 0 
          DO J=1,NSURF
            IF(IGRSURF(J)%ID==IDSURF)THEN
              NOM_OPT(6,NINTER+I)=J
              OKSURF = 1
            END IF
          END DO
          IF(OKSURF == 0 ) THEN 
             CALL ANCMSG(MSGID=576,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=IDSURF)
          ENDIF
C
          IDSURF=NOM_OPT(3,NINTER+I)
          IF(IDSURF/=0)THEN
            OKSURF = 0 
            DO J=1,NSURF
              IF(IGRSURF(J)%ID==IDSURF)THEN
                NOM_OPT(3,NINTER+I)=J
                OKSURF = 1
              END IF
            END DO
            IF(OKSURF == 0 ) THEN 
              CALL ANCMSG(MSGID=576,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IDSURF)
            ENDIF
          
          END IF
C
        ENDIF
C
      END DO
C-------------------------------------
C     tri des sous-interfaces par interface
C-------------------------------------
C-------------------------------------
C     recherche si la ss-interface est ecrite sur TH (tag) 
C     (sinon la ss-interface est inutile)
C-------------------------------------

      CALL HM_OPTION_COUNT('/TH/INTER' ,NTHINTER  )
      CALL HM_OPTION_START('/TH/INTER')   
      IF (NTHINTER > 0) THEN 
        DO II=1,NTHINTER
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)  
          CALL HM_GET_INTV('idsmax',IDSMAX,IS_AVAILABLE,LSUBMODEL) 
          DO IVAR=1,IDSMAX
            CALL HM_GET_INT_ARRAY_INDEX('ids',N,IVAR,IS_AVAILABLE,LSUBMODEL) 
            DO K=1,NINTSUB
              IF(N==NOM_OPT(1,NINTER+K))THEN
                NOM_OPT(5,NINTER+K)=1
                EXIT
              END IF
            END DO !next K   
          ENDDO!NEXT IVAR    
        ENDDO !next II
      ENDIF
C-------------------------------------
C     L00
C-------------------------------------
      DO  I=1,NINTSUB
        ID    =NOM_OPT(1,NINTER+I)
        IDINT =NOM_OPT(2,NINTER+I)
        ITH   =NOM_OPT(5,NINTER+I)
        IF (INTSUB_TYP(I)==25) THEN
          IDSURF1=IGRSURF(NOM_OPT(3,NINTER+I))%ID
          IDGRN  =NOM_OPT(4,NINTER+I)
          IF(IDGRN/=0) IDGRN  =IGRNOD(IDGRN)%ID
          IDSURF2=NOM_OPT(6,NINTER+I)
          IF(IDSURF2/=0)IDSURF2=IGRSURF(IDSURF2)%ID
          WRITE(IOUT,1125) ID,IDINT,IDSURF1,IDSURF2,IDGRN,ITH
        ELSEIF (INTSUB_TYP(I)==0) THEN
          IDSURF=IGRSURF(NOM_OPT(3,NINTER+I))%ID
          IDGRN =IGRNOD(NOM_OPT(4,NINTER+I))%ID
          WRITE(IOUT,1100) ID,IDINT,IDSURF,IDGRN,ITH
        ELSEIF (INTSUB_TYP(I)==1) THEN
          ISU1 = IGRSLIN(NOM_OPT(3,NINTER+I))%ID
          ISU2 = IGRSLIN(NOM_OPT(4,NINTER+I))%ID
          WRITE(IOUT,1200) ID,IDINT,ISU1,ISU2,ITH
        ELSEIF (INTSUB_TYP(I)==2) THEN
          ISURF1 = IGRSURF(NOM_OPT(3,NINTER+I))%ID
          ISURF2 = IGRSURF(NOM_OPT(4,NINTER+I))%ID
          WRITE(IOUT,1300) ID,IDINT,ISU1,ISU2,ITH
        ENDIF
      END DO
C-------------------------------------
      DEALLOCATE(ID_INTER,FLAG_INTER)
C-------------------------------------
 1000 FORMAT(    /1X,'   SUB-INTERFACES               ' /
     .            1X,'   --------------               '// )
 1100 FORMAT(//
     .    '    SUB-INTERFACE ID. . . . . . . . . . . . . ',I10/,
     .    '    INTERFACE ID. . . . . . . . . . . . . . . ',I10/,
     .    '    SURFACE ID. . . . . . . . . . . . . . . . ',I10/,
     .    '    NODES GROUP ID. . . . . . . . . . . . . . ',I10/,
     .    '    OUTPUT TO TH (0:NO,1:YES) . . . . . . . . ',I10/)
 1125 FORMAT(//
     .    '    SUB-INTERFACE ID. . . . . . . . . . . . . ',I10/,
     .    '    INTERFACE ID. . . . . . . . . . . . . . . ',I10/,
     .    '    SURFACE ID 1. . . . . . . . . . . . . . . ',I10/,
     .    '    SURFACE ID 2. . . . . . . . . . . . . . . ',I10/,
     .    '    NODES GROUP ID. . . . . . . . . . . . . . ',I10/,
     .    '    OUTPUT TO TH (0:NO,1:YES) . . . . . . . . ',I10/)
 1200 FORMAT(//
     .    '    SUB-INTERFACE ID. . . . . . . . . . . . . ',I10/,
     .    '    INTERFACE ID. . . . . . . . . . . . . . . ',I10/,
     .    '    MAIN LINE ID .  . . . . . . . . . . . . . ',I10/,
     .    '    SECONDARY LINE ID . . . . . . . . . . . . ',I10/,
     .    '    OUTPUT TO TH (0:NO,1:YES) . . . . . . . . ',I10/)
 1300 FORMAT(//
     .    '    SUB-INTERFACE ID. . . . . . . . . . . . . ',I10/,
     .    '    INTERFACE ID. . . . . . . . . . . . . . . ',I10/,
     .    '    MAIN SURFACE ID. . . . . . . . . . . . . .',I10/,
     .    '    SECONDARY SURFACE ID. . . . . . . . . . . ',I10/,
     .    '    OUTPUT TO TH (0:NO,1:YES) . . . . . . . . ',I10/)
      RETURN
      END
