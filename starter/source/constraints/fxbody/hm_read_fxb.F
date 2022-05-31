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
Chd|  HM_READ_FXB1                  source/constraints/fxbody/hm_read_fxb.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL                       source/starter/freform.F      
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        FXRLINE                       source/constraints/fxbody/hm_read_fxb.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        NEWDBL                        source/system/sysfus.F        
Chd|        READ_PCH_FILE                 source/constraints/fxbody/read_pch_file.F
Chd|        UDOUBLE                       source/system/sysfus.F        
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_FXB1(NOM_OPT,FXBNOD,FXBIPM,FXB_MATRIX,FXB_MATRIX_ADD,
     .                        NMANIM,ITAB,ITABM1,FXBFILE_TAB,LSUBMODEL)
      USE MESSAGE_MOD
      USE INOUTFILE_MOD
      USE HM_OPTION_READ_MOD
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
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "fxbcom.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NOM_OPT(LNOPT1,*),FXBIPM(NBIPM,*),FXBNOD(*),FXB_MATRIX_ADD(4,*),NMANIM,ITAB(*),
     .        ITABM1(*)
      my_real
     .        FXB_MATRIX(*)
      CHARACTER, DIMENSION(NFXBODY) :: FXBFILE_TAB*2148
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ID,I,J,L,NFX,NMOD,NMST,NBNO,NTR,NME,IDAMP,
     .        ISHELL,IBLO,IFILE,IDMAST,IANIM,IMIN,IMAX,ADRNOD,NLIG,NRES,ILIG,
     .        NUMNO(10),BID,IFLAGI1,IC,IOLD,IFLAGDBL,IRB,FLAG,IDUM1,IDUM2,IDUM3,
     .        SIZE_MAX,SIZE_MAT,I1,I2,IDOF1,IDOF2,ADR_MAT,ADR_MASS,ADR_MASS0,ADRNOD0,
     .        IL1,IL2,ADR_STIFF,ADR_STIFF0,SIZE_MASS,SIZE_STIFF
      INTEGER IWORK(70000)
      CHARACTER FXBFILE*2148, NWLINE*100, STRERR*29
      CHARACTER TITR*nchartitle,MESS*40,MESS1*40,EXTENSION*3
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: TABSL,ITAG_DOF
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX,ITAG

      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=2148) :: TMP_NAME
      LOGICAL :: IS_AVAILABLE

C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
      DATA MESS/'FLEXIBLE BODY : NODES                   '/
      DATA MESS1/'FLEXIBLE BODY DEFINITION                '/
C=====================================================================
C
      WRITE(IOUT,2000)
C
      SIZE_MAX = 0 
      DO NFX=1,NFXBODY
        SIZE_MAX = MAX(SIZE_MAX,FXBIPM(3,NFX))
      ENDDO
C
      IF (LENNOD > 0) THEN
        ALLOCATE(TABSL(2,LENNOD))
        ALLOCATE(INDEX(3*LENNOD))
        ALLOCATE(ITAG_DOF(6,SIZE_MAX))
        ALLOCATE(ITAG(NUMNOD))
        ITAG(1:NUMNOD) = 0
        ITAG_DOF(1:6,1:SIZE_MAX) = 0
        TABSL = 0
        INDEX = 0
      END IF
C
      ADRNOD  = 1
      ADR_MAT = 1
      IS_AVAILABLE = .FALSE.
C
      CALL HM_OPTION_START('/FXBODY')
C
      ! Loop over FXBODY
      DO NFX = 1,NFXBODY
C
        ! Title and ID
        TITR = ''   
        CALL HM_OPTION_READ_KEY(LSUBMODEL, 
     .                          OPTION_ID   = ID, 
     .                          OPTION_TITR = TITR)   
C
        NOM_OPT(1,NFX) = ID
        CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,NFX),LTITR)
C
        ! Integer data card
        CALL HM_GET_INTV('node_IDm',IDMAST,IS_AVAILABLE,LSUBMODEL)  
        CALL HM_GET_INTV('Ianim'   ,IANIM ,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Imin'    ,IMIN  ,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Imax'    ,IMAX  ,IS_AVAILABLE,LSUBMODEL)
C
        ! File name
        CALL HM_GET_STRING('Filename',FXBFILE   ,2148   ,IS_AVAILABLE)
        FXBFILE_TAB(NFX) = FXBFILE
C
        IF (FXBIPM(41,NFX) == 2) THEN
C
          NBNO = FXBIPM(3,NFX)
          FXBIPM(6,NFX) = ADRNOD
C
          SIZE_STIFF = FXBIPM(42,NFX)
          SIZE_MASS  = FXBIPM(43,NFX)
          ADR_STIFF  = ADR_MAT
          ADR_MASS   = ADR_MAT + SIZE_STIFF
          FXBIPM(44,NFX) = ADR_STIFF
          FXBIPM(45,NFX) = ADR_MASS
          ADR_STIFF0 = ADR_STIFF
          ADR_MASS0  = ADR_MASS
          ADRNOD0    = ADRNOD
C
C--        Pre-reading of pch file for dimensions
          FLAG = 1
          CALL READ_PCH_FILE(FLAG,FXB_MATRIX,ITAG,FXB_MATRIX_ADD,ADR_STIFF,
     .                       ADR_MASS,ITABM1,FXBFILE,ID,TITR)
C
          ADR_MAT = ADR_MAT + SIZE_STIFF + SIZE_MASS
C
C --       Storage of nodes
          DO I=1,NUMNOD
            IF (ITAG(I) > 0) THEN
              FXBNOD(ADRNOD)  = I
              TABSL(1,ADRNOD) = FXBNOD(ADRNOD)
              TABSL(2,ADRNOD) = NFX
              ADRNOD = ADRNOD+1
C--           Local id is stored in ITAG
              ITAG(I) = ADRNOD-ADRNOD0
            ENDIF
          ENDDO          
C
          NMOD   = 0
          ISHELL = 0
C
          DO I=1,SIZE_STIFF
            I1    = FXB_MATRIX_ADD(1,ADR_STIFF0+I-1)
            I2    = FXB_MATRIX_ADD(2,ADR_STIFF0+I-1)
            IDOF1 = FXB_MATRIX_ADD(3,ADR_STIFF0+I-1)
            IDOF2 = FXB_MATRIX_ADD(4,ADR_STIFF0+I-1)
C
C            In FXB_MATRIX_ADD -> local id in FXBNOD us used instead on internal id
            IL1 = ITAG(I1)
            IL2 = ITAG(I2)
	    FXB_MATRIX_ADD(1,ADR_STIFF0+I-1) = IL1
            FXB_MATRIX_ADD(2,ADR_STIFF0+I-1) = IL2
C
C --          Craig-bampton with only boundary nodes - one static mode per dof in connected
C --          oversizing - number of modes can be lower
            IF (ITAG_DOF(IDOF1,IL1)==0) THEN
              NMOD = NMOD + 1
              IF (IDOF1 > 3) ISHELL = 1
               ITAG_DOF(IDOF1,IL1) = 1
            ENDIF
            IF (ITAG_DOF(IDOF2,IL2)==0) THEN
              NMOD = NMOD + 1
              IF (IDOF2 > 3) ISHELL = 1
               ITAG_DOF(IDOF2,IL2) = 1
            ENDIF                  
          ENDDO
C
          DO I=1,SIZE_MASS
            I1 = FXB_MATRIX_ADD(1,ADR_MASS0+I-1)
            I2 = FXB_MATRIX_ADD(2,ADR_MASS0+I-1)
C
C            In FXB_MATRIX_ADD -> local id in FXBNOD us used instead on internal id
            IL1 = ITAG(I1)
            IL2 = ITAG(I2)
	    FXB_MATRIX_ADD(1,ADR_MASS0+I-1) = IL1
            FXB_MATRIX_ADD(2,ADR_MASS0+I-1) = IL2              
          ENDDO

C--  RAZ of ITAG arrays
          ITAG_DOF(1:6,1:NBNO) = 0
          DO I=1,NBNO
            ITAG(FXBNOD(ADRNOD0+I-1)) = 0
          ENDDO
          ITAG(1:NUMNOD) = 0
C
C-- Craig-bampton static modes only
          NMST = NMOD
          IBLO = 0
          IFILE = 0
C
          IF (SIZE_MASS > 0) THEN
            WRITE(IOUT,1200) ID,TRIM(TITR),IDMAST,NBNO
          ELSE
            WRITE(IOUT,1100) ID,TRIM(TITR),IDMAST,NBNO
          ENDIF
C
        ELSE
C
          TMP_NAME = INFILE_NAME(1:INFILE_NAME_LEN)//FXBFILE(1:len_trim(FXBFILE))
          LEN_TMP_NAME = INFILE_NAME_LEN + len_trim(FXBFILE)
          OPEN(UNIT=IFICM,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .       ACCESS='SEQUENTIAL',FORM='FORMATTED',
     .       STATUS='OLD',ERR=1000)
C
C-------------------------------------
C Reading of first lines
C-------------------------------------
C
          CALL FXRLINE(IFICM,NWLINE,ID,TITR)
          READ(NWLINE,FMT='(7I8)',ERR=9999) 
     .       NMOD, NMST, NBNO, ISHELL, IDAMP, IBLO, IFILE
C Gestion des erreurs
          IF (NMOD < 0) THEN
             STRERR='NEGATIVE MODE NUMBER'
             CALL ANCMSG(MSGID=582,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 C1=FXBFILE,
     .                 C2=STRERR)
          ENDIF
          IF (NMST < 0) THEN
             STRERR='NEGATIVE STATIC MODE NUMBER'
             CALL ANCMSG(MSGID=582,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 C1=FXBFILE,
     .                 C2=STRERR)
          ENDIF
          IF (NBNO < 0) THEN
            STRERR='NEGATIVE NODE NUMBER'
            CALL ANCMSG(MSGID=582,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 C1=FXBFILE,
     .                 C2=STRERR)
          ENDIF
          IF (ISHELL /= 0.AND.ISHELL /= 1) THEN
            STRERR='INVALID VALUE FOR FLAG IROT'
            CALL ANCMSG(MSGID=582,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 C1=FXBFILE,
     .                 C2=STRERR)
          ENDIF
          IF (IDAMP /= 0.AND.IDAMP /= 1) THEN
            STRERR='INVALID VALUE FOR FLAG IDAMP'
            CALL ANCMSG(MSGID=582,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 C1=FXBFILE,
     .                 C2=STRERR)
          ENDIF
          IF (IBLO /= 0.AND.IBLO /= 1) THEN
            STRERR='INVALID VALUE FOR FLAG IBLO'
            CALL ANCMSG(MSGID=582,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 C1=FXBFILE,
     .                 C2=STRERR)
          ENDIF
          IF (IFILE /= 0.AND.IFILE /= 1) THEN
            STRERR='INVALID VALUE FOR FLAG IFILE'
            CALL ANCMSG(MSGID=582,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 C1=FXBFILE,
     .                 C2=STRERR)
          ENDIF
C
C-------------------------------------
C Reading of FXB nodes
C-------------------------------------
C
          FXBIPM(6,NFX) = ADRNOD
C
          NLIG = NBNO/10
          NRES = NBNO-NLIG*10
          DO ILIG = 1,NLIG
            CALL FXRLINE(IFICM,NWLINE,ID,TITR)
            READ(NWLINE,'(10I8)',ERR=9999)
     .          (NUMNO(I),I=1,10)
             DO I=1,10
               FXBNOD(ADRNOD)  = USR2SYS(NUMNO(I),ITABM1,MESS,ID)
               TABSL(1,ADRNOD) = FXBNOD(ADRNOD)
               TABSL(2,ADRNOD) = NFX
               ADRNOD = ADRNOD+1
             ENDDO
          ENDDO
          IF (NRES > 0) THEN
            CALL FXRLINE(IFICM,NWLINE,ID,TITR)
            READ(NWLINE,'(10I8)',ERR=9999)
     .          (NUMNO(I),I=1,NRES)
            DO I = 1,NRES
              FXBNOD(ADRNOD)=USR2SYS(NUMNO(I),ITABM1,MESS,ID)
              ADRNOD=ADRNOD+1
            ENDDO
          ENDIF
C
        ENDIF      
C
        NTR = 9
        IF (ISHELL == 0) THEN
          NME = 12
        ELSE
          NME = 15
        ENDIF
C         
        LENGLM = LENGLM+NME*(NME+1)/2
        LENCP  = LENCP +NTR*NMOD*NME
        LENLM  = LENLM +NMOD
        LENFLS = LENFLS+NMST*(2*NMOD-NMST+1)/2
        LENDLS = LENDLS+NMOD-NMST
        LENVAR = LENVAR+NMOD+NME
        LENRPM = LENRPM+NTR+7
        LENMCD = LENMCD+NME*NME
C
        FXBIPM(1,NFX)  = ID
        FXBIPM(2,NFX)  = USR2SYS(IDMAST,ITABM1,MESS,ID)
        FXBIPM(4,NFX)  = NMOD
        FXBIPM(5,NFX)  = NMST
        FXBIPM(16,NFX) = ISHELL
        FXBIPM(17,NFX) = NME
        FXBIPM(28,NFX) = IBLO
        FXBIPM(29,NFX) = IFILE
        FXBIPM(36,NFX) = IANIM
C
        IF (IMAX == 0) IMAX = NMOD
        IMIN = MAX(1,IMIN)
        IMAX = MIN(NMOD,IMAX)
        FXBIPM(37,NFX) = IMIN
        FXBIPM(38,NFX) = IMAX
        IF (IANIM == 1) THEN
          DO I = IMIN,IMAX
            NMANIM = NMANIM+1
          ENDDO
        ENDIF
C
        CLOSE(IFICM)
C
      ENDDO
C
C-------------------------------------
C Recherche des ID doubles
C-------------------------------------
      CALL UDOUBLE(FXBIPM(1,1),NBIPM,NFXBODY,MESS1,0,BID)
C-------------------------------------
C Recherche des MAIN ID doubles
C-------------------------------------
      IC = 567
      I  = 0
c      CALL ANCNTS(IC, I)
      CALL NEWDBL(FXBIPM(2,1),NBIPM,NFXBODY,ITAB,567,ANINFO_BLIND_1,
     .            NOM_OPT)
c      CALL ANCNTG(IC, I, J)
c      CALL ANCHECK(67)
C-------------------------------------
C Bilan SECONDARY nodes doubles
C-------------------------------------
      IF (NFXBODY > 1) THEN
      IWORK=0
      IFLAGDBL=0
      DO I=1,LENNOD
        INDEX(I)=I
      END DO
      CALL MY_ORDERS(0,IWORK,TABSL,INDEX,LENNOD,2)
      IF (LENNOD > 0) THEN
        IOLD=-1
        DO I=1,LENNOD
          IF(INDEX(I) /=0 )THEN
            IF (TABSL(1,INDEX(I))==IOLD) THEN
              IF (IFLAGDBL==0) THEN
                IFLAGI1=I-1
              END IF
              IFLAGDBL=1          
            ELSE
              IF (IFLAGDBL/=0) THEN
                DO J=IFLAGI1,I-1
                  IRB=TABSL(2,INDEX(J))
                  !ID=NOM_OPT(1,IRB)
                  CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,IRB),LTITR)
                  CALL ANCMSG(MSGID=1026,
     .                        MSGTYPE=MSGWARNING,
     .                        ANMODE=ANINFO_BLIND_2,
     .                        I1=ID,
     .                        C1=TITR,
     .                        PRMOD=MSG_CUMU)
                END DO
                CALL ANCMSG(MSGID=1026,
     .                      MSGTYPE=MSGWARNING,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=TABSL(1,INDEX(IFLAGI1)),
     .                      PRMOD=MSG_PRINT)
                IFLAGDBL=0
              END IF
            END IF
            IOLD=TABSL(1,INDEX(I)) 
          ENDIF    
        END DO
      END IF
      END IF
C
C
      DEALLOCATE(TABSL,INDEX,ITAG_DOF,ITAG)
C
      RETURN
1000  CALL ANCMSG(MSGID=565,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            I1=ID,
     .            C1=TITR,
     .            C2=FXBFILE)
9999  CALL ANCMSG(MSGID=566,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            I1=ID,
     .            C1=TITR,
     .            C2=FXBFILE,
     .            C3=NWLINE)
      RETURN
C      
2000  FORMAT(/
     . '      FLEXIBLE BODY DEFINITIONS '/
     . '      ---------------------- '/)
C
1100  FORMAT( /5X,'FLEXIBLE BODY ID ',I10,1X,A
     .       /10X,'MAIN NODE ID                          ',I10
     .       /10X,'NUMBER OF NODES                         ',I10
     .       /10X,'INITIALIZED FROM PCH FILE               ',
     .       /10X,'  --> STIFFNESS MATRIX                  ')
C
1200  FORMAT( /5X,'FLEXIBLE BODY ID ',I10,1X,A
     .       /10X,'MAIN NODE ID                          ',I10
     .       /10X,'NUMBER OF NODES                         ',I10
     .       /10X,'INITIALIZED FROM PCH FILE               ',
     .       /10X,'  --> STIFFNESS MATRIX                  ',
     .       /10X,'  --> MASS MATRIX                       ')
C      
      END    
c=================================================================================
Chd|====================================================================
Chd|  HM_READ_FXB2                  source/constraints/fxbody/hm_read_fxb.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        FXRLINE                       source/constraints/fxbody/hm_read_fxb.F
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_FXB2(FXBIPM, FXBRPM, FXBNOD, FXBGLM,  
     .                   FXBCPM , FXBCPS, FXBLM , FXBFLS, FXBDLS,
     .                   FXBMOD , ITAB   , ITABM1, NOM_OPT,FXB_LAST_ADRESS,
     .                   LSUBMODEL)
      USE MESSAGE_MOD
      USE INOUTFILE_MOD
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
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "fxbcom.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FXBIPM(NBIPM,*), FXBNOD(*),
     .        ITAB(*),ITABM1(*),FXB_LAST_ADRESS(*)
      my_real 
     .        FXBRPM(*), FXBGLM(*), FXBCPM(*), FXBCPS(*), 
     .        FXBLM(*),  FXBFLS(*), FXBDLS(*), FXBMOD(*)
      INTEGER NOM_OPT(LNOPT1,*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NFX,ID,IDMAST,NMOD,NMST,NBNO,NME,NTR,ADRGLM,
     .        ADRCP,ADRLM,ADRFLS,ADRDLS,ADRVAR,ADRRPM,IMOD,INO,I,LEN,
     .        NLIG,NRES,ILIG,ADRCP2,IR,ADRNOD,IDAMP,ISHELL,
     .        ADRMCD,J,INFO,IBLO,IFILE, IANIM, IMIN, IMAX, ADRMOD,IRCM,
     .        NTAG,ADRM1,ADRM2,ADRN1,ADRN2,CNOD(NUMNOD)
      my_real 
     .        FREQ,BETA,OMEGA,DTC1,DTC2,VV(6)
      CHARACTER TITR*nchartitle,NWLINE*100,
     .          FXBFILE*100

      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=2148) :: TMP_NAME
      LOGICAL :: IS_AVAILABLE
C=====================================================================  
C
      ADRMOD = 1
      IRCM   = 0
      ADRGLM = 1
      ADRCP  = 1
      ADRLM  = 1
      ADRFLS = 1
      ADRDLS = 1
      ADRVAR = 1
      ADRRPM = 1
      ADRMCD = 1
      IS_AVAILABLE = .FALSE.
C
      CALL HM_OPTION_START('/FXBODY')
C
      ! Loop over FXBODY
      DO NFX = 1,NFXBODY
C
        !For PCH FXBodies modes are automatically computed in INITIA
        IF (FXBIPM(41,NFX) == 2) CYCLE
C
        ! Title and ID
        TITR = ''   
        CALL HM_OPTION_READ_KEY(LSUBMODEL, 
     .                          OPTION_ID   = ID, 
     .                          OPTION_TITR = TITR)   
C 
        ! Read file name
        CALL HM_GET_STRING('Filename',FXBFILE   ,100   ,IS_AVAILABLE)
        TMP_NAME = INFILE_NAME(1:INFILE_NAME_LEN)//FXBFILE(1:len_trim(FXBFILE))
        LEN_TMP_NAME = INFILE_NAME_LEN + len_trim(FXBFILE)
        OPEN(UNIT=IFICM,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .       ACCESS='SEQUENTIAL',FORM='FORMATTED',
     .       STATUS='OLD',ERR=999)
C
        CALL FXRLINE(IFICM,NWLINE,ID,TITR)
        READ(NWLINE,FMT='(7I8)',ERR=9999) 
     .       NMOD, NMST, NBNO, ISHELL, IDAMP, IBLO, IFILE
C
        FXBIPM(7,NFX)  = ADRMOD
        FXBIPM(8,NFX)  = ADRGLM
        FXBIPM(9,NFX)  = ADRCP
        FXBIPM(10,NFX) = ADRLM
        FXBIPM(11,NFX) = ADRFLS
        FXBIPM(12,NFX) = ADRDLS
        FXBIPM(13,NFX) = ADRVAR
        FXBIPM(14,NFX) = ADRRPM
        FXBIPM(15,NFX) = ADRMCD
        FXBIPM(30,NFX) = IRCM
C
        ADRNOD = FXBIPM(6,NFX)
        NTAG   = FXBIPM(18,NFX)
        NME    = FXBIPM(17,NFX)
C         
        ADRMCD = ADRMCD+NME*NME
C
        NLIG = NBNO/10
        NRES = NBNO-NLIG*10
        DO ILIG = 1,NLIG
          CALL FXRLINE(IFICM,NWLINE,ID,TITR)
        ENDDO
        IF (NRES > 0) THEN
          CALL FXRLINE(IFICM,NWLINE,ID,TITR)
        ENDIF
C
        NTR = 9
C
C-------------------------------------
C Reading of Skew + Freq
C-------------------------------------
C
        CALL FXRLINE(IFICM,NWLINE,ID,TITR)
        READ(NWLINE,'(5F16.0)',ERR=9999)
     .       (FXBRPM(ADRRPM+I-1),I=2,6)
        CALL FXRLINE(IFICM,NWLINE,ID,TITR)
        READ(NWLINE,'(5F16.0)',ERR=9999)
     .       (FXBRPM(ADRRPM+I-1),I=7,10),FREQ
        ADRRPM=ADRRPM+12
C
C-------------------------------------
C Reading of Damping
C-------------------------------------
C
        IF (IDAMP > 0) THEN
          CALL FXRLINE(IFICM,NWLINE,ID,TITR)
          READ(NWLINE,'(2F16.0)',ERR=9999)
     .         (FXBRPM(ADRRPM+I-1),I=1,2)
          BETA=FXBRPM(ADRRPM+1)
          IF (BETA > ZERO) THEN
            OMEGA = TWO*PI*FREQ
            DTC1  = (-BETA*OMEGA+
     .              SQRT(BETA*BETA*OMEGA*OMEGA+FOUR))/OMEGA
            DTC2  = TWO/(BETA*OMEGA*OMEGA)
            FXBRPM(ADRRPM-12)=MIN(DTC1,DTC2)
          ELSE
            OMEGA = TWO*PI*FREQ
            FXBRPM(ADRRPM-12) = TWO/OMEGA
          ENDIF
          ADRRPM = ADRRPM+2
         ELSE
          FXBRPM(ADRRPM)    = ZERO
          FXBRPM(ADRRPM+1)  = ZERO
          FXBRPM(ADRRPM-12) = ONE/(PI*FREQ)
          ADRRPM = ADRRPM+2
         ENDIF
         FXBRPM(ADRRPM)   = ZERO
         FXBRPM(ADRRPM+1) = ZERO
         ADRRPM           = ADRRPM+2
C
C-------------------------------------
C Reading of modes
C-------------------------------------
C
         IF (IFILE == 0) THEN
C On stocke les modes entiers en memoire centrale
            IF (IBLO == 0) THEN
               DO IMOD = 1,NME
                  ADRM1 = ADRMOD
                  ADRM2 = ADRMOD+NTAG*6
                  DO INO = 1,NBNO
                     IF (FXBNOD(ADRNOD+INO-1) < 0) THEN
                        CALL FXRLINE(IFICM,NWLINE,ID,TITR)
                        READ(NWLINE,'(5F16.0)',ERR=9999)
     .                      (FXBMOD(ADRM1+I-1),I=1,5)
                        CALL FXRLINE(IFICM,NWLINE,ID,TITR)
                        READ(NWLINE,'(F16.0)',ERR=9999)
     .                      FXBMOD(ADRM1+5)
                        ADRM1=ADRM1+6
                     ELSEIF (FXBNOD(ADRNOD+INO-1) > 0) THEN
                        CALL FXRLINE(IFICM,NWLINE,ID,TITR)
                        READ(NWLINE,'(5F16.0)',ERR=9999)
     .                      (FXBMOD(ADRM2+I-1),I=1,5)
                        CALL FXRLINE(IFICM,NWLINE,ID,TITR)
                        READ(NWLINE,'(F16.0)',ERR=9999)
     .                      FXBMOD(ADRM2+5)
                        ADRM2=ADRM2+6
                     ENDIF
                  ENDDO
                  ADRMOD = ADRM2
               ENDDO
            ELSEIF (IBLO == 1) THEN
               DO IMOD = 1,NME
                  ADRM1 = ADRMOD
                  ADRM2 = ADRMOD+NTAG*6
                  DO INO = 1,NBNO
                     IF (FXBNOD(ADRNOD+INO-1) < 0) THEN
                        DO I = 1,6
                           FXBMOD(ADRM1+I-1) = ZERO
                        ENDDO
                        ADRM1 = ADRM1+6
                     ELSEIF (FXBNOD(ADRNOD+INO-1) > 0) THEN
                        DO I = 1,6
                           FXBMOD(ADRM2+I-1) = ZERO
                        ENDDO
                        ADRM2 = ADRM2+6
                     ENDIF
                  ENDDO
                  ADRMOD = ADRM2
               ENDDO
            ENDIF   
            DO IMOD = 1,NMOD
               ADRM1 = ADRMOD
               ADRM2 = ADRMOD+NTAG*6
               DO INO = 1,NBNO
                  IF (FXBNOD(ADRNOD+INO-1) < 0) THEN
                     CALL FXRLINE(IFICM,NWLINE,ID,TITR)
                     READ(NWLINE,'(5F16.0)',ERR=9999)
     .                   (FXBMOD(ADRM1+I-1),I=1,5)
                     CALL FXRLINE(IFICM,NWLINE,ID,TITR)
                     READ(NWLINE,'(F16.0)',ERR=9999)
     .                   FXBMOD(ADRM1+5)
                     ADRM1 = ADRM1+6
                  ELSEIF (FXBNOD(ADRNOD+INO-1) > 0) THEN
                     CALL FXRLINE(IFICM,NWLINE,ID,TITR)
                     READ(NWLINE,'(5F16.0)',ERR=9999)
     .                   (FXBMOD(ADRM2+I-1),I=1,5)
                     CALL FXRLINE(IFICM,NWLINE,ID,TITR)
                     READ(NWLINE,'(F16.0)',ERR=9999)
     .                   FXBMOD(ADRM2+5)
                     ADRM2 = ADRM2+6
                  ENDIF
               ENDDO
               ADRMOD = ADRM2
            ENDDO
         ELSEIF (IFILE == 1) THEN
C On stocke seulement les composantes des modes sur les noeuds d'interface
C em memoire centrale
C Les autres composantes sont ecrites sur le disque
            IF (IBLO == 0) THEN
               DO IMOD = 1,NME
                  ADRM1 = ADRMOD
                  DO INO = 1,NBNO
                     IF (FXBNOD(ADRNOD+INO-1) < 0) THEN
                        CALL FXRLINE(IFICM,NWLINE,ID,TITR)
                        READ(NWLINE,'(5F16.0)',ERR=9999)
     .                      (FXBMOD(ADRM1+I-1),I=1,5)
                        CALL FXRLINE(IFICM,NWLINE,ID,TITR)
                        READ(NWLINE,'(F16.0)',ERR=9999)
     .                      FXBMOD(ADRM1+5)
                        ADRM1 = ADRM1+6
                     ELSEIF (FXBNOD(ADRNOD+INO-1) > 0) THEN
                        CALL FXRLINE(IFICM,NWLINE,ID,TITR)
                        READ(NWLINE,'(5F16.0)',ERR=9999) (VV(I),I=1,5)
                        CALL FXRLINE(IFICM,NWLINE,ID,TITR)
                        READ(NWLINE,'(F16.0)',ERR=9999) VV(6)
                        IRCM = IRCM+1
                        WRITE(IFXM,REC=IRCM) (VV(I),I=1,6)
                     ENDIF
                  ENDDO
                  ADRMOD = ADRM1
               ENDDO
            ELSEIF (IBLO == 1) THEN
               DO IMOD = 1,NME
                  ADRM1 = ADRMOD
                  DO INO = 1,NBNO
                     IF (FXBNOD(ADRNOD+INO-1) < 0) THEN
                        DO I = 1,6
                           FXBMOD(ADRM1+I-1) = ZERO
                        ENDDO
                        ADRM1 = ADRM1+6
                     ELSEIF (FXBNOD(ADRNOD+INO-1) > 0) THEN
                        DO I = 1,6
                           VV(I) = ZERO
                        ENDDO
                        IRCM = IRCM+1
                        WRITE(IFXM,REC=IRCM) (VV(I),I=1,6)
                     ENDIF
                  ENDDO
                  ADRMOD = ADRM1
               ENDDO
            ENDIF
            DO IMOD = 1,NMOD
               ADRM1 = ADRMOD
               DO INO = 1,NBNO
                  IF (FXBNOD(ADRNOD+INO-1) < 0) THEN
                     CALL FXRLINE(IFICM,NWLINE,ID,TITR)
                     READ(NWLINE,'(5F16.0)',ERR=9999)
     .                   (FXBMOD(ADRM1+I-1),I=1,5)
                     CALL FXRLINE(IFICM,NWLINE,ID,TITR)
                     READ(NWLINE,'(F16.0)',ERR=9999)
     .                   FXBMOD(ADRM1+5)
                     ADRM1 = ADRM1+6
                  ELSEIF (FXBNOD(ADRNOD+INO-1) > 0) THEN
                     CALL FXRLINE(IFICM,NWLINE,ID,TITR)
                     READ(NWLINE,'(5F16.0)',ERR=9999) (VV(I),I=1,5)
                     CALL FXRLINE(IFICM,NWLINE,ID,TITR)
                     READ(NWLINE,'(F16.0)',ERR=9999) VV(6)
                     IRCM = IRCM+1
                     WRITE(IFXM,REC=IRCM) (VV(I),I=1,6)
                  ENDIF
               ENDDO
               ADRMOD = ADRM1
            ENDDO
         ENDIF
C
         FXBIPM(32,NFX)=IRCM
C
C-------------------------------------
C Reading of Diag Mass Matrix
C-------------------------------------
C
         IF (NMOD > 0) THEN
            LEN  = NMOD
            NLIG = LEN/5
            NRES = LEN-NLIG*5
            DO ILIG = 1,NLIG
               CALL FXRLINE(IFICM,NWLINE,ID,TITR)
               READ(NWLINE,'(5F16.0)',ERR=9999) 
     .              (FXBLM(ADRLM+I-1),I=1,5)
               ADRLM = ADRLM+5
            ENDDO
            IF (NRES > 0) THEN
               CALL FXRLINE(IFICM,NWLINE,ID,TITR)
               READ(NWLINE,'(5F16.0)',ERR=9999)
     .              (FXBLM(ADRLM+I-1),I=1,NRES)
               ADRLM = ADRLM+NRES
            ENDIF
         ENDIF
C
C-------------------------------------
C Reading of Stiff full part matrix
C-------------------------------------
C
         IF (NMST > 0) THEN
            LEN  = NMST*(2*NMOD-NMST+1)/2
            NLIG = LEN/5
            NRES = LEN-NLIG*5
            DO ILIG = 1,NLIG
               CALL FXRLINE(IFICM,NWLINE,ID,TITR)
               READ(NWLINE,'(5F16.0)',ERR=9999)
     .              (FXBFLS(ADRFLS+I-1),I=1,5)
               ADRFLS = ADRFLS+5
            ENDDO
            IF (NRES > 0) THEN
               CALL FXRLINE(IFICM,NWLINE,ID,TITR)
               READ(NWLINE,'(5F16.0)',ERR=9999)
     .              (FXBFLS(ADRFLS+I-1),I=1,NRES)
               ADRFLS = ADRFLS+NRES
            ENDIF
         ENDIF
C
C-------------------------------------
C Reading of Stiff diag part matrix
C-------------------------------------
C                   
         IF ((NMOD-NMST) > 0) THEN
            LEN  = NMOD-NMST
            NLIG = LEN/5
            NRES = LEN-NLIG*5
            DO ILIG = 1,NLIG
               CALL FXRLINE(IFICM,NWLINE,ID,TITR)
               READ(NWLINE,'(5F16.0)',ERR=9999)
     .              (FXBDLS(ADRDLS+I-1),I=1,5)
               ADRDLS = ADRDLS+5
            ENDDO
            IF (NRES > 0) THEN
               CALL FXRLINE(IFICM,NWLINE,ID,TITR)
               READ(NWLINE,'(5F16.0)',ERR=9999)
     .              (FXBDLS(ADRDLS+I-1),I=1,NRES)
               ADRDLS = ADRDLS+NRES
            ENDIF
         ENDIF
C                   
         IF (IBLO == 1) THEN
            LEN = NME*(NME+1)/2
            DO I = 1,LEN
               FXBGLM(ADRGLM) = ZERO
               ADRGLM = ADRGLM+1
            ENDDO
            LEN = NME*NMOD
            DO I = 1,NTR
               DO J = 1,LEN
                  FXBCPM(ADRCP) = ZERO
                  FXBCPS(ADRCP) = ZERO
                  ADRCP = ADRCP+1
               ENDDO
            ENDDO
            GOTO 100
         ENDIF
C
C-------------------------------------
C Reading of Mass Matrix projected on RB modes
C-------------------------------------
C            
         LEN  = NME*(NME+1)/2
         NLIG = LEN/5
         NRES = LEN-NLIG*5
         DO ILIG = 1,NLIG
            CALL FXRLINE(IFICM,NWLINE,ID,TITR)
            READ(NWLINE,'(5F16.0)',ERR=9999)
     .           (FXBGLM(ADRGLM+I-1),I=1,5)
            ADRGLM = ADRGLM+5
         ENDDO
         IF (NRES > 0) THEN
            CALL FXRLINE(IFICM,NWLINE,ID,TITR)
            READ(NWLINE,'(5F16.0)',ERR=9999)
     .           (FXBGLM(ADRGLM+I-1),I=1,NRES)
            ADRGLM = ADRGLM+NRES
         ENDIF
C
C-------------------------------------
C Reading of Coupled Mass Matrix
C-------------------------------------
C                    
         IF (NMOD > 0) THEN
            ADRCP2 = ADRCP
            DO IR = 1,NTR
               LEN  = NME*NMOD
               NLIG = LEN/5
               NRES = LEN-NLIG*5
               DO ILIG = 1,NLIG
                  CALL FXRLINE(IFICM,NWLINE,ID,TITR)
                  READ(NWLINE,'(5F16.0)',ERR=9999)
     .                 (FXBCPM(ADRCP+I-1),I=1,5)
                  ADRCP = ADRCP+5
               ENDDO
               IF (NRES > 0) THEN
                  CALL FXRLINE(IFICM,NWLINE,ID,TITR)
                  READ(NWLINE,'(5F16.0)',ERR=9999)
     .                 (FXBCPM(ADRCP+I-1),I=1,NRES)
                  ADRCP = ADRCP+NRES
               ENDIF
            ENDDO
C
C-------------------------------------
C Reading of Coupled Stiff Matrix
C-------------------------------------
C 
            DO IR = 1,NTR
               LEN  = NME*NMOD
               NLIG = LEN/5
               NRES = LEN-NLIG*5
               DO ILIG = 1,NLIG
                  CALL FXRLINE(IFICM,NWLINE,ID,TITR)
                  READ(NWLINE,'(5F16.0)',ERR=9999)
     .                 (FXBCPS(ADRCP2+I-1),I=1,5)
                  ADRCP2 = ADRCP2+5
               ENDDO
               IF (NRES > 0) THEN
                  CALL FXRLINE(IFICM,NWLINE,ID,TITR)
                  READ(NWLINE,'(5F16.0)',ERR=9999)
     .                 (FXBCPS(ADRCP2+I-1),I=1,NRES)
                  ADRCP2 = ADRCP2+NRES
               ENDIF
            ENDDO
         ENDIF
C
 100     CLOSE(IFICM)
C
C Reorganisation du tableau des noeuds
         DO I = 1,NBNO
            CNOD(I)=FXBNOD(ADRNOD+I-1)
         ENDDO
         ADRN1 = ADRNOD-1
         ADRN2 = ADRNOD-1+NTAG
         DO I = 1,NBNO
            IF (CNOD(I) < 0) THEN
               ADRN1 = ADRN1+1
               FXBNOD(ADRN1) = -CNOD(I)
            ELSEIF (CNOD(I) > 0) THEN
               ADRN2 = ADRN2+1
               FXBNOD(ADRN2)=CNOD(I)
            ENDIF
         ENDDO
C
         ADRVAR=ADRVAR+NMOD+NME
C
         WRITE(IOUT,1100) ID,TRIM(TITR),ITAB(FXBIPM(2,NFX)),NBNO,NME,NMOD,
     .                    NMST,(FXBRPM(FXBIPM(14,NFX)+I),I=1,NTR),
     .                    FXBRPM(FXBIPM(14,NFX))      
      ENDDO
C
C -- Last adresses are stored for storage of otehr fxbodies in initia
C
      FXB_LAST_ADRESS(1) = ADRMOD
      FXB_LAST_ADRESS(2) = ADRGLM
      FXB_LAST_ADRESS(3) = ADRCP
      FXB_LAST_ADRESS(4) = ADRLM
      FXB_LAST_ADRESS(5) = ADRFLS
      FXB_LAST_ADRESS(6) = ADRDLS
      FXB_LAST_ADRESS(7) = ADRVAR
      FXB_LAST_ADRESS(8) = ADRRPM
      FXB_LAST_ADRESS(9) = ADRMCD
C
      RETURN
 999  CALL FREERR(3)
      RETURN
9999  CALL ANCMSG(MSGID=566,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            I1=ID,
     .            C1=TITR,
     .            C2=FXBFILE,
     .            C3=NWLINE)
      RETURN
C         
1100  FORMAT( /5X,'FLEXIBLE BODY ID ',I10,1X,A
     .       /10X,'MAIN NODE ID                          ',I10
     .       /10X,'NUMBER OF NODES                         ',I10
     .       /10X,'NUMBER OF GLOBAL MODES                  ',I10
     .       /10X,'NUMBER OF LOCAL MODES                   ',I10
     .       /10X,'NUMBER OF LOCAL STATIC MODES            ',I10
     .       /10X,'INITIAL ROTATION MATRIX                 ',
     .       /10X,(9(1PE10.3))
     .       /10X,'STABILITY TIME-STEP                     ',1PE10.3)
C
      END    
Chd|====================================================================
Chd|  FXRLINE                       source/constraints/fxbody/hm_read_fxb.F
Chd|-- called by -----------
Chd|        HM_READ_FXB1                  source/constraints/fxbody/hm_read_fxb.F
Chd|        HM_READ_FXB2                  source/constraints/fxbody/hm_read_fxb.F
Chd|        HM_SETFXRBYON                 source/constraints/fxbody/hm_setfxrbyon.F
Chd|        READ_PCH_FILE                 source/constraints/fxbody/read_pch_file.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FXRLINE(IFIC, NWLINE, ID,TITR)                     
      USE MESSAGE_MOD
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFIC, ID
      CHARACTER NWLINE*100
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISTOP     
C
      ISTOP=0
      DO WHILE (ISTOP.EQ.0)
         READ(IFIC,'(A)',END=999) NWLINE
         IF ((NWLINE(1:1).NE.'#').AND.((NWLINE(1:1).NE.'$')).AND.((LEN_TRIM(NWLINE)/=0))) ISTOP=1
      ENDDO
C
      RETURN
  999 CALL ANCMSG(MSGID=569,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            I1=ID,
     .            C1=TITR)
      RETURN
C
      END
