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
Chd|  HM_SETFXRBYON                 source/constraints/fxbody/hm_setfxrbyon.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        FXRLINE                       source/constraints/fxbody/hm_read_fxb.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        READ_PCH_FILE                 source/constraints/fxbody/read_pch_file.F
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_SETFXRBYON(ITABM1,IXS,ISOLOFF,IXC,ISHEOFF,
     .                      IXT,ITRUOFF,IXP,IPOUOFF,IXR,IRESOFF,
     .                      IXTG,ITRIOFF,FXBIPM,LSUBMODEL)
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INOUTFILE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD      
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "fxbcom.inc"
#include      "param_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITABM1(*),FXBIPM(NBIPM,*)
      INTEGER IXS(NIXS,*),ISOLOFF(*),
     *        IXC(NIXC,*),ISHEOFF(*),
     *        IXT(NIXT,*),ITRUOFF(*),
     *        IXP(NIXP,*),IPOUOFF(*),
     *        IXR(NIXR,*),IRESOFF(*),
     *        IXTG(NIXTG,*),ITRIOFF(*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NFX,ID,IDMAST,NMOD,NMST,NBNO,NME,NTR,ADRGLM,
     .        ADRCP,ADRLM,ADRFLS,ADRDLS,ADRVAR,ADRRPM,IMOD,INO,I,LEN,
     .        NLIG,NRES,ILIG,ADRCP2,IR,ADRNOD,NUMNO(10),IDAMP,ISHELL,
     .        ADRMCD,IC,J,INFO,IBLO,IFILE, IANIM, IMIN, IMAX
      INTEGER IDUM1,IDUM3,IDUM4
      INTEGER NOD,DUM_INT,DUM_INT2
      INTEGER II,NALL,LENGTH,FLAG,SIZE_STIFF,SIZE_MASS
      my_real 
     .        FREQ,BETA,OMEGA,DTC1,DTC2,BID
      my_real RDUM1,RDUM2,RDUM3,RDUM4,RDUM5
      CHARACTER TITR*nchartitle,MESS*40,MESS1*40,NWLINE*100,
     .          FXBFILE*100,EXTENSION*3

      INTEGER, DIMENSION(:),ALLOCATABLE :: ITAG

      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=2148) :: TMP_NAME
      LOGICAL :: IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
C=====================================================================  
      ALLOCATE (ITAG(NUMNOD))
C
      LENMAT = 0
      ADRNOD = 1
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
      ! Start ready FXBODY
      CALL HM_OPTION_START('/FXBODY')
C
      ! Loop over FXBODY
      DO NFX = 1,NFXBODY
C
        ITAG(1:NUMNOD) = 0
        SIZE_STIFF     = 0
        SIZE_MASS      = 0
C
        ! Title and ID
        TITR = ''   
        CALL HM_OPTION_READ_KEY(LSUBMODEL, 
     .                          OPTION_ID   = ID, 
     .                          OPTION_TITR = TITR)  
C
        ! Integer data card
        CALL HM_GET_INTV('node_IDm',IDUM1 ,IS_AVAILABLE,LSUBMODEL)  
        CALL HM_GET_INTV('Imin'    ,IMIN  ,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Imax'    ,IMAX  ,IS_AVAILABLE,LSUBMODEL)
C
        ! File name
        CALL HM_GET_STRING('Filename',FXBFILE   ,100   ,IS_AVAILABLE)
C
        LENGTH = LEN_TRIM(FXBFILE)
        IF (LENGTH > 2) EXTENSION = FXBFILE(LENGTH-2:LENGTH)
C
        IF ((EXTENSION == "pch").OR.(EXTENSION == "PCH")) THEN
C--       Pre-reading of pch file for tag of nodes and number of nodes
          FLAG = 0
          CALL READ_PCH_FILE(FLAG,RDUM1,ITAG,IDUM1,SIZE_STIFF,
     .                       SIZE_MASS,ITABM1,FXBFILE,ID,TITR)
C
          NBNO = 0
          DO I=1,NUMNOD
            IF (ITAG(I) > 0) THEN
              NBNO = NBNO + 1
            ENDIF 
          ENDDO
C
          FXBIPM(41,NFX) = 2
          FXBIPM(42,NFX) = SIZE_STIFF
          FXBIPM(43,NFX) = SIZE_MASS
C           
        ELSE
C
C--       Pre-reading of Radioss fxb file for dimensions and tag of nodes
          FXBIPM(41,NFX) = 1
C
          LEN_TMP_NAME = INFILE_NAME_LEN+len_trim(FXBFILE)
          TMP_NAME=INFILE_NAME(1:INFILE_NAME_LEN)//FXBFILE(1:len_trim(FXBFILE))
          OPEN(UNIT=IFICM,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .        ACCESS='SEQUENTIAL',FORM='FORMATTED',
     .        STATUS='OLD',ERR=999)

          CALL FXRLINE(IFICM,NWLINE,ID,TITR)
          READ(NWLINE,FMT='(7I8)',ERR=9999) 
     .        NMOD, NMST, NBNO, ISHELL, IDAMP, IBLO, IFILE
C
          IF (ISHELL == 0) THEN
            NME = 12
          ELSE
            NME = 15
          ENDIF
          IMIN = 0
          IF (IMAX == 0) IMAX = NMOD
          IMIN = MAX(1,IMIN)
          IMAX = MIN(NMOD,IMAX)
C         
          ADRMCD = ADRMCD+NME*NME
C
          NLIG = NBNO/10
          NRES = NBNO-NLIG*10
          DO ILIG = 1,NLIG
            CALL FXRLINE(IFICM,NWLINE,ID,TITR)
            READ(NWLINE,'(10I8)',ERR=9999)
     .           (NUMNO(I),I=1,10)
            DO I=1,10
               NOD = USR2SYS(NUMNO(I),ITABM1,MESS,ID)
               ITAG(NOD)=1
            ENDDO
          ENDDO
          IF (NRES > 0) THEN
            CALL FXRLINE(IFICM,NWLINE,ID,TITR)
            READ(NWLINE,'(10I8)',ERR=9999)
     .           (NUMNO(I),I=1,NRES)
            DO I = 1,NRES
               NOD = USR2SYS(NUMNO(I),ITABM1,MESS,ID)
               ITAG(NOD) = 1
            ENDDO
          ENDIF
C
          NTR = 9
          CALL FXRLINE(IFICM,NWLINE,ID,TITR)
          READ(NWLINE,'(5F16.0)',ERR=9999)
     .        RDUM1,RDUM2,RDUM3,RDUM4,RDUM5 
          CALL FXRLINE(IFICM,NWLINE,ID,TITR)
          READ(NWLINE,'(5F16.0)',ERR=9999)
     .       RDUM1,RDUM2,RDUM3,RDUM4,RDUM5 
C
          IF (IDAMP > 0) THEN
            CALL FXRLINE(IFICM,NWLINE,ID,TITR)
            READ(NWLINE,'(2F16.0)',ERR=9999)
     .           RDUM1,RDUM2
          ELSE

          ENDIF
C
          IF (IBLO == 0) THEN
            DO IMOD = 1,NME
               DO INO = 1,NBNO
                  CALL FXRLINE(IFICM,NWLINE,ID,TITR)
                  CALL FXRLINE(IFICM,NWLINE,ID,TITR)
               ENDDO
            ENDDO
          ENDIF
          DO IMOD = 1,NMOD
            DO INO = 1,NBNO
               CALL FXRLINE(IFICM,NWLINE,ID,TITR)
               CALL FXRLINE(IFICM,NWLINE,ID,TITR)
            ENDDO
          ENDDO
C
          IF (NMOD > 0) THEN
            LEN  = NMOD
            NLIG = LEN/5
            NRES = LEN-NLIG*5
            DO ILIG = 1,NLIG
               CALL FXRLINE(IFICM,NWLINE,ID,TITR)
               READ(NWLINE,'(5F16.0)',ERR=9999) 
     .              RDUM1,RDUM2,RDUM3,RDUM4,RDUM5 
            ENDDO
            IF (NRES > 0) THEN
               CALL FXRLINE(IFICM,NWLINE,ID,TITR)
               READ(NWLINE,'(5F16.0)',ERR=9999)
     .              RDUM1,RDUM2,RDUM3,RDUM4,RDUM5 
            ENDIF
          ENDIF
C
          IF (NMST > 0) THEN
            LEN  = NMST*(2*NMOD-NMST+1)/2
            NLIG = LEN/5
            NRES = LEN-NLIG*5
            DO ILIG = 1,NLIG
              CALL FXRLINE(IFICM,NWLINE,ID,TITR)
              READ(NWLINE,'(5F16.0)',ERR=9999)
     .             RDUM1,RDUM2,RDUM3,RDUM4,RDUM5 
            ENDDO
            IF (NRES > 0) THEN
              CALL FXRLINE(IFICM,NWLINE,ID,TITR)
              READ(NWLINE,'(5F16.0)',ERR=9999)
     .             RDUM1,RDUM2,RDUM3,RDUM4,RDUM5 
            ENDIF
          ENDIF
C                   
          IF ((NMOD-NMST) > 0) THEN
            LEN  = NMOD-NMST
            NLIG = LEN/5
            NRES = LEN-NLIG*5
            DO ILIG = 1,NLIG
               CALL FXRLINE(IFICM,NWLINE,ID,TITR)
               READ(NWLINE,'(5F16.0)',ERR=9999)
     .              RDUM1,RDUM2,RDUM3,RDUM4,RDUM5 
            ENDDO
            IF (NRES > 0) THEN
               CALL FXRLINE(IFICM,NWLINE,ID,TITR)
               READ(NWLINE,'(5F16.0)',ERR=9999)
     .              RDUM1,RDUM2,RDUM3,RDUM4,RDUM5 
            ENDIF
          ENDIF
C                   
          IF (IBLO == 1) THEN
            GOTO 100
          ENDIF
C            
          LEN  = NME*(NME+1)/2
          NLIG = LEN/5
          NRES = LEN-NLIG*5
          DO ILIG = 1,NLIG
            CALL FXRLINE(IFICM,NWLINE,ID,TITR)
            READ(NWLINE,'(5F16.0)',ERR=9999)
     .              RDUM1,RDUM2,RDUM3,RDUM4,RDUM5 
          ENDDO
          IF (NRES > 0) THEN
            CALL FXRLINE(IFICM,NWLINE,ID,TITR)
            READ(NWLINE,'(5F16.0)',ERR=9999)
     .              RDUM1,RDUM2,RDUM3,RDUM4,RDUM5 
          ENDIF
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
     .              RDUM1,RDUM2,RDUM3,RDUM4,RDUM5 
               ENDDO
               IF (NRES > 0) THEN
                  CALL FXRLINE(IFICM,NWLINE,ID,TITR)
                  READ(NWLINE,'(5F16.0)',ERR=9999)
     .              RDUM1,RDUM2,RDUM3,RDUM4,RDUM5 
               ENDIF
            ENDDO
C
            DO IR = 1,NTR
               LEN  = NME*NMOD
               NLIG = LEN/5
               NRES = LEN-NLIG*5
               DO ILIG = 1,NLIG
                  CALL FXRLINE(IFICM,NWLINE,ID,TITR)
                  READ(NWLINE,'(5F16.0)',ERR=9999)
     .              RDUM1,RDUM2,RDUM3,RDUM4,RDUM5 
               ENDDO
               IF (NRES > 0) THEN
                  CALL FXRLINE(IFICM,NWLINE,ID,TITR)
                  READ(NWLINE,'(5F16.0)',ERR=9999)
     .              RDUM1,RDUM2,RDUM3,RDUM4,RDUM5 
               ENDIF
            ENDDO
          ENDIF
C
        ENDIF
C
 100    CLOSE(IFICM)
C
        FXBIPM(3,NFX) = NBNO
        LENNOD = LENNOD + NBNO
        LENMAT = LENMAT + SIZE_STIFF + SIZE_MASS
C
C Elements Solides
        DO II = 1,NUMELS
          NALL = ITAG(IXS(2,II)) * ITAG(IXS(3,II)) *
     +           ITAG(IXS(4,II)) * ITAG(IXS(5,II)) *
     +           ITAG(IXS(6,II)) * ITAG(IXS(7,II)) *
     +           ITAG(IXS(8,II)) * ITAG(IXS(9,II)) 
          IF (NALL /= 0) THEN
            ISOLOFF(II) = 3
          END IF
        ENDDO

C Elements Coques
        DO II=1,NUMELC
          NALL = ITAG(IXC(2,II)) * ITAG(IXC(3,II)) *
     +           ITAG(IXC(4,II)) * ITAG(IXC(5,II)) 
          IF (NALL /= 0) THEN
             ISHEOFF(II) = 3
          END IF
        ENDDO

C Elements TRUSS
        DO II=1,NUMELT
          NALL = ITAG(IXT(2,II)) * ITAG(IXT(3,II))
          IF (NALL /= 0) THEN
             ITRUOFF(II) = 3
          END IF
        ENDDO

C Elements POUTRES
        DO II=1,NUMELP
          NALL = ITAG(IXP(2,II)) * ITAG(IXP(3,II))
          IF (NALL /= 0) THEN
             IPOUOFF(II) = 3
          END IF
        ENDDO

C Elements RESSORTS
        DO II=1,NUMELR
          NALL = ITAG(IXR(2,II)) * ITAG(IXR(3,II))
          IF (NALL /= 0) THEN
             IRESOFF(II) = 3
          END IF
        ENDDO
 
C Elements TRIANGLES
        DO II=1,NUMELTG
          NALL = ITAG(IXTG(2,II)) * ITAG(IXTG(3,II)) * ITAG(IXTG(4,II))
          IF (NALL /= 0) THEN
             ITRIOFF(II) = 3
          END IF
        ENDDO

C fin des traitements sur FXB
      ENDDO
     

      IF (ALLOCATED(ITAG)) DEALLOCATE (ITAG)
C
      RETURN
 999  CALL FREERR(3)
      IF (ALLOCATED(ITAG)) DEALLOCATE (ITAG)
      RETURN
9999  CALL ANCMSG(MSGID=566,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANINFO,
     .            I1=ID,
     .            C1=TITR,
     .            C2=FXBFILE,
     .            C3=NWLINE)
      IF (ALLOCATED(ITAG)) DEALLOCATE (ITAG)
      RETURN
C
      END    


Chd|====================================================================
Chd|  FXSETELOFF                    source/constraints/fxbody/hm_setfxrbyon.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FXSETELOFF(IPARG  ,ELBUF , 
     1                      IXS    ,IXC    ,IXT    ,IXP    ,IXR    ,
     2                      IXTG   ,ISOLOFF,ISHEOFF,ITRUOFF,IPOUOFF,
     3                      IRESOFF,ITRIOFF )                
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
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*)
      INTEGER IXS(NIXS,*), IXC(NIXC,*), IXTG(NIXTG,*), IXT(NIXT,*),
     *        IXP(NIXP,*), IXR(NIXR,*),
     *        ISOLOFF(*),ISHEOFF(*),ITRUOFF(*),IPOUOFF(*),
     *        IRESOFF(*),ITRIOFF(*)
      my_real ELBUF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NG,I,II
      INTEGER MLW,ITY,NEL,NFT,IAD,IOK,IGOF,ISHFT
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C-----------------------------------
C
      DO NG=1,NGROUP
         MLW=IPARG(1,NG)
         ITY=IPARG(5,NG)
         NEL=IPARG(2,NG)
         NFT=IPARG(3,NG)
         IAD=IPARG(4,NG) - 1

C-----------------------
C     1. ELEMENTS SOLIDES
C-----------------------
         IF(ITY.EQ.1.AND.MLW.NE.0)THEN        ! loi0, pas de off
           IOK = 0
           DO I=1,NEL
             II=I+NFT
             IF(ISOLOFF(II)==3)THEN
               ELBUF(IAD+I)= -ABS(ELBUF(IAD+I))
               IF(IPRI.GE.5) WRITE(IOUT,*)' BRICK DEACTIVATION:',
     .         IXS(11,II)
               IOK = 1
             ENDIF
           ENDDO
C----------------------------------------
C       TEST POUR L'ELIMINATION D'ONE GROUPE
C----------------------------------------
           IF(IOK.EQ.1)THEN
             IGOF = 1
             DO I = 1,NEL
               II=I+NFT
               IF(ELBUF(IAD + I).GT.ZERO) IGOF=0
             ENDDO
             IPARG(8,NG) = IGOF
           END IF

C-----------------------
C     3. ELEMENTS COQUES
C-----------------------
         ELSEIF(ITY.EQ.3.AND.MLW.NE.0)THEN       ! loi0, pas de off
           IF(IPARG(23,NG).EQ.21) THEN
             ISHFT=17
           ELSEIF(IPARG(23,NG).EQ.22.OR.IPARG(23,NG).EQ.23) THEN
             ISHFT=23
           ELSE
             ISHFT=16
           ENDIF
           IOK = 0
           DO I=1,NEL
             II=I+NFT
             IF(ISHEOFF(II)==3)THEN
               IF(ELBUF(IAD+I+ISHFT*NEL).GT.ZERO)THEN
                 ELBUF(IAD+I+ISHFT*NEL)= -ELBUF(IAD+I+ISHFT*NEL)
                 IF(IPRI.GE.5) WRITE(IOUT,*)' SHELL DEACTIVATION:',
     .           IXC(7,II)
                 IOK = 1
               ENDIF
             ENDIF
           ENDDO
C----------------------------------------
C       TEST POUR L'ELIMINATION D'ONE GROUPE
C----------------------------------------
           IF(IOK.EQ.1)THEN
             IGOF = 1
             DO I = 1,NEL
               II=I+NFT
               IF(ELBUF(IAD + I + ISHFT*NEL).GT.ZERO) IGOF=0
             ENDDO
             IPARG(8,NG) = IGOF
           END IF
C-----------------------
C     4. ELEMENTS TRUSS
C-----------------------
         ELSEIF(ITY.EQ.4)THEN
           IOK = 0
           DO I=1,NEL
             II=I+NFT
             IF(ITRUOFF(II)==3)THEN
               ELBUF(IAD+I)= ZERO
               IF(IPRI.GE.5) WRITE(IOUT,*)' TRUSS DEACTIVATION:',
     .         IXT(5,II)
               IOK = 1
             ENDIF
           ENDDO
C----------------------------------------
C       TEST POUR L'ELIMINATION D'ONE GROUPE
C----------------------------------------
C Incompatible avec option de gap dans propriete de truss
C        IGOF = 1
C        DO I = 1,NEL
C         IF(ELBUF(IAD + I).NE.ZERO) IGOF=0
C        ENDDO
C        IPARG(8,NG) = IGOF
C-----------------------
C     5. ELEMENTS POUTRES
C-----------------------
         ELSEIF(ITY.EQ.5)THEN
           IOK = 0
           DO I=1,NEL
             II=I+NFT
             IF(IPOUOFF(II)==3)THEN
               ELBUF(IAD+I)= -ABS(ELBUF(IAD+I))
               IF(IPRI.GE.5) WRITE(IOUT,*)' BEAM DEACTIVATION:',
     .         IXP(6,II)
               IOK = 1
             ENDIF
           ENDDO
C----------------------------------------
C       TEST POUR L'ELIMINATION D'ONE GROUPE
C----------------------------------------
           IF(IOK.EQ.1)THEN
             IGOF = 1
             DO I = 1,NEL
               IF(ELBUF(IAD + I).GT.ZERO) IGOF=0
             ENDDO
             IPARG(8,NG) = IGOF
           END IF
C-----------------------
C     6. ELEMENTS RESSORTS
C-----------------------
         ELSEIF(ITY.EQ.6.AND.MLW.NE.3)THEN
           IOK = 0
           DO I=1,NEL
             II=I+NFT
             IF(IRESOFF(II)==3)THEN
               IF(ELBUF(IAD+I).NE.-TEN)
     .         ELBUF(IAD+I)= -ABS(ELBUF(IAD+I))
C             spring is active
               IF(IPRI.GE.5) WRITE(IOUT,*)' SPRING DEACTIVATION:',
     .         IXR(6,II)
               IOK = 1
             ENDIF
           ENDDO
C----------------------------------------
C       TEST POUR L'ELIMINATION D'ONE GROUPE
C----------------------------------------
           IF(IOK.EQ.1)THEN
             IGOF = 1
             DO I = 1,NEL
               IF(ELBUF(IAD + I).NE.ZERO) IGOF=0
             ENDDO
             IPARG(8,NG) = IGOF
           END IF
C-----------------------
C     7. ELEMENTS COQUES 3N
C-----------------------
         ELSEIF(ITY.EQ.7.AND.MLW.NE.0)THEN       ! loi0, pas de off
           ISHFT=16
           IOK = 0
           DO I=1,NEL
             II=I+NFT
             IF(ITRIOFF(II)==3)THEN
               ELBUF(IAD+I+ISHFT*NEL)= -ABS(ELBUF(IAD+I+ISHFT*NEL))
               IF(IPRI.GE.5) WRITE(IOUT,*)' SH_3N DEACTIVATION:',
     .         IXTG(6,II)
               IOK = 1
             ENDIF
           ENDDO
C----------------------------------------
C       TEST POUR L'ELIMINATION D'ONE GROUPE
C----------------------------------------
           IF(IOK.EQ.1)THEN
             IGOF = 1
             DO I = 1,NEL
               II=I+NFT
               IF(ELBUF(IAD + I + ISHFT*NEL).GT.ZERO) IGOF=0
             ENDDO
             IPARG(8,NG) = IGOF
           END IF
C----------------------------------------
         ENDIF
        ENDDO


      END
