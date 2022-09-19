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
Chd|  YCTRL                         source/initial_conditions/inista/yctrl.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREDEC5                       source/starter/freform.F      
Chd|        FREDEC6                       source/starter/freform.F      
Chd|        FREDEC_2KEY_ID_OR_KEY_ID      source/starter/freform.F      
Chd|        FREERR                        source/starter/freform.F      
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE YCTRL(IGRBRIC)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------                     
      USE GROUPDEF_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scry_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER
     .      I,J,NGAUSS,NLAYER ,
     .      NUMS,NIP,NUVAR,JJHBE,J1,NU,IP,N,NPSOLID,
     .      K,IHBE,NPG,ND,NVAR_SHELL,NPT,NE,
     .      NVSHELL0,NUSHELL0,NORTSHEL0,NUSOLID0,NELS,KK,JJ,
     .      ISOLNOD,ISOLID,IFRAM,IORTH,IREP,IGTYP,ISH3N,NDIR,NLAYERS,
     .      UID,SUB_ID,NLAY,NPTR,NPTS,NPTT,IFAIL,IRUPT_TYP,NVAR_RUPT,
     .      ILAY,IMAT,NPT_MAX,NUBEAM0,NVSH_STRA,ISMSTR
      INTEGER IGBR, JGBR, IOK
      CHARACTER MESS*40,KEY2*ncharkey,KEY3*ncharkey
C=======================================================================
      NFILSOL=0
      NUMSOL =0
      NUMQUAD=0
      NUMSHEL=0
      NUMTRUS=0
      NUMBEAM=0
      NUMSPRI=0
      NUMSH3N=0
      NVSHELL0   = 32
      NUSHELL0   = 4
      NORTSHEL0  = 3
      NVAR_SHELL = 0
      NUBEAM0 = 4
      NUBEAM  = 0
      NVBEAM  = 0
      NVSPRI  = 0
      NVTRUSS = 0
      NVSH_STRA =0
!
      IF (ISIGI==3.OR.ISIGI==4.OR.ISIGI==5) THEN
C
C      FICHIER Y000, Block CONTROL
C
       REWIND(IIN5)
C
  300  READ(IIN5,FMT='(A)',END=309,ERR=399)LINE
       IF(LINE(1:8)/='/CONTROL')GOTO 300
       READ(IIN5,FMT='(A)',END=309,ERR=399)LINE
  305  READ(IIN5,FMT='(A)',END=309,ERR=399)LINE
       IF(LINE(1:1)=='#')GOTO 305
C
       IF (IOUTP_FMT==2) THEN
         IF(LINE(1:8)=='        ')GOTO 305
C
       ELSE
         IF(LINE(1:10)=='          ')GOTO 305
       END IF
C
       IF(LINE(1:1)=='/')GOTO 309
  306  READ(IIN5,FMT='(A)',END=309,ERR=399)LINE
       IF(LINE(1:1)=='#')GOTO 306
C
       IF (IOUTP_FMT==2) THEN
         IF(LINE(1:8)=='        ')GOTO 306
C
       ELSE
         IF(LINE(1:10)=='          ')GOTO 306
       END IF
C
       IF(LINE(1:1)=='/')GOTO 309
       IF (IOUTP_FMT==2) THEN
         READ(LINE,'(8I8)')
     .   NUMSOL,NUMQUAD,NUMSHEL,NUMTRUS,NUMBEAM,NUMSPRI,NUMSH3N,
     .   NUMSPHY
C
       ELSE
         READ(LINE,'(8I10)')
     .   NUMSOL,NUMQUAD,NUMSHEL,NUMTRUS,NUMBEAM,NUMSPRI,NUMSH3N,
     .   NUMSPHY
C
       END IF
 309    CONTINUE
C
       IUFACYLD = 0
       IUSHELL = 0
       NUSHELL = 0
       NVSHELL1 = 0
       NVSHELL2 = 0
cc       NGAUSS = 0
cc       NLAYER = 0
cc       NVSHELL = 0
       IUSOLID = 0
       NUSOLID = 0
       NVSOLID1 = 0
       NVSOLID2 = 0
       NVSOLID3 = 0
       NVSOLID4 = 0
       NVSOLID5 = 0
       NVSOLID6 = 0
cc       NPSOLID = 0
cc       NVSOLID = 0
      REWIND(IIN4)
 400  READ(IIN4,FMT='(A)',END=449,ERR=449)LINE
       IF(LINE(1:8)=='/ENDDATA')THEN
         REWIND(IIN4)
         GO TO 409
       ENDIF
       IF(LINE(1:28)/='/SHELL     /SCALAR    /USERS')GOTO 400
       READ(IIN4,FMT='(A)',END=449,ERR=449)LINE
        IUSHELL = 1
C
        I = 0
        J = 0
  405   READ(IIN4,FMT='(A)',END=449,ERR=449)LINE
        IF(LINE(1:1)=='#')GOTO 405
        IF(LINE(1:1)=='/')GOTO 410
         J=J+1
         I=I+1
         IF(J>NUMSHEL+NUMSH3N) GOTO 410
         IF(IOUTP_FMT==2)THEN
           READ(LINE,FMT='(4I8)')IHBE,NIP,NPG,NUVAR
         ELSE
          READ(LINE,FMT='(4I10)')IHBE,NIP,NPG,NUVAR
         ENDIF
cc          NGAUSS = MAX(NPG,NGAUSS)
cc          NLAYER = MAX(NIP,NLAYER)
cc          NVSHELL   = MAX(NUVAR,NVSHELL)
          NUSHELL = MAX(NUSHELL,MAX(1,NPG)*MAX(1,NIP)*NUVAR + NUSHELL0)
          ND = MOD(NUVAR,6)
          NU = (NUVAR - ND)/6
          IF(ND/=0) NU = NU +1
          IF(NUVAR < 6) NU = 1
C --- coque standard.
           DO J1 = 1,NU * MAX(1,NIP)*MAX(1,NPG)
 406       READ(IIN4,FMT='(A)',END=449,ERR=449)LINE
            IF(LINE(1:1)=='#')GOTO 406
            IF(LINE(1:1)=='/')GOTO 410
           ENDDO
          GO TO 405
 449  CONTINUE
C-----------------------------------------------------------------
C      FICHIER Ynnn
C      stress/full
C      NIP + THK + ENER + STRESS + EPS PLASTIC
C-----------------------------------------------------------------
  409  READ(IIN4,FMT='(A)',END=443,ERR=399)LINE
       IF(LINE(1:8)=='/ENDDATA')THEN
         REWIND(IIN4)
         GOTO 425
       ENDIF
  410  IF(LINE(1:33)/='/SHELL     /TENSOR    /STRESS_FUL')GOTO 409
C
       READ(IIN4,FMT='(A)',END=443,ERR=499)LINE
C              
       I = 0
       J = 0
C
  411  READ(IIN4,FMT='(A)',END=443,ERR=499)LINE
       IF(LINE(1:1)=='#')GOTO 411
       IF(LINE(1:1)=='/')GOTO 443
         J=J+1
         I=I+1
C
         IF(J>NUMSHEL+NUMSH3N) GOTO 420
         IF (IOUTP_FMT==2) THEN
           READ(LINE,'(2I8)')NIP,NPG
         ELSE
           READ(LINE,FMT=FMT_2I) NIP,NPG
         ENDIF
C
         READ(IIN4,FMT='(A)',END=443,ERR=499)LINE
          IF(NIP==0)THEN
            NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NPG)*9)
          ELSE
            NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NIP)*MAX(1,NPG)*6)
          ENDIF
C           
          NVSHELL = NVAR_SHELL
         IF (NPG==0.OR.NPG==1)THEN
          IF(NIP==0)THEN
           READ(IIN4,FMT='(A)',END=443,ERR=399)LINE
           READ(IIN4,FMT='(A)',END=443,ERR=399)LINE
         ELSE
           DO K=1,NIP
            READ(IIN4,FMT='(A)',END=443,ERR=399)LINE
           ENDDO
         ENDIF
        ELSEIF(NPG>1) THEN
          IF(NIP==0)THEN
            DO N=1,NPG
            READ(IIN4,FMT='(A)',END=443,ERR=399)LINE
            READ(IIN4,FMT='(A)',END=443,ERR=399)LINE
            ENDDO
          ELSE
c           PT = 60
           DO K=1,NIP
            DO N=1,NPG
             READ(IIN4,FMT='(A)',END=443,ERR=399)LINE
C             PT = PT + 6
            ENDDO
           ENDDO
          ENDIF
         ELSE
        ENDIF
        GO TO 411
 443    CONTINUE
        REWIND(IIN4)
C   /solid/tensor/stress/full 
c -----
C    STRESS FULL + ELstoplastic + energy+ RHO in each point integration
  425  READ(IIN4,FMT='(A)',END=429,ERR=399)LINE
       IF(LINE(1:8) == '/ENDDATA')THEN
         REWIND(IIN4)
         GOTO 429
       ENDIF
  420  IF(LINE(1:30) /= '/SOLID     /TENSOR    /STR_FUL')GOTO 425
       READ(IIN4,FMT='(A)',END=429,ERR=399)LINE
C 
       I = 0
       J = 0
  426  READ(IIN4,FMT='(A)',END=429,ERR=399)LINE
       IF(LINE(1:1) == '#')GOTO 426
       IF(LINE(1:1) == '/')GOTO 429
        I=I+1
        IF(I > NUMSOL+NUMQUAD) GOTO 450

        IF (IOUTP_FMT == 2) THEN
          READ(LINE,'(3I8)')NIP,NUMS,JJHBE
         ELSE
          READ(LINE,'(3I10)')NIP,NUMS, JJHBE
        ENDIF
        NVSOLID1 = MAX(NVSOLID1,NIP*9 + 4)
        IF((NUMS == 8.OR.NUMS == 4).AND.JJHBE == 0)THEN
C
          READ(IIN4,FMT='(A)',END=429,ERR=399)LINE
          IF(NIP == 1)THEN
           READ(IIN4,FMT='(A)',END=429,ERR=399)LINE
           READ(IIN4,FMT='(A)',END=429,ERR=399)LINE
          ELSE
            DO K=1,NIP
             READ(IIN4,FMT='(A)',END=429,ERR=399)LINE
             READ(IIN4,FMT='(A)',END=429,ERR=399)LINE
            ENDDO
          ENDIF
        ELSEIF(NUMS == 10 .OR. NUMS == 16 .OR. NUMS == 20.OR.
     .    (NUMS == 8.AND.JJHBE == 14) .OR .(NUMS == 8.AND.JJHBE == 17).OR.
     .    ((NUMS == 6.OR.NUMS == 8) .AND. (JJHBE==15 .or. JJHBE==12)))THEN
C        
          DO K=1,NIP
           READ(IIN4,FMT='(A)',END=429,ERR=399)LINE
           READ(IIN4,FMT='(A)',END=429,ERR=399)LINE
          ENDDO
        ENDIF
         GO TO 426
C
 429  CONTINUE
      REWIND(IIN4)
C


 230  READ(IIN4,FMT='(A)',END=235,ERR=399)LINE
       IF(LINE(1:8)=='/ENDDATA')THEN
         REWIND(IIN4)
         GOTO 430
       ENDIF
 232  IF(LINE(1:33)/='/SOLID     /TENSOR    /STRESS')GOTO 230
       READ(IIN4,FMT='(A)',END=235,ERR=399)LINE
C         
        NVSOLID1 = MAX(NVSOLID1, 6)
 235  CONTINUE
      REWIND(IIN4)

C
 430  READ(IIN4,FMT='(A)',END=435,ERR=399)LINE
       IF(LINE(1:8)=='/ENDDATA')THEN
         REWIND(IIN4)
         GOTO 444
       ENDIF
 432  IF(LINE(1:33)/='/SOLID     /TENSOR    /STRAIN_FUL')GOTO 430
       READ(IIN4,FMT='(A)',END=435,ERR=399)LINE
C         
       I = 0
       J = 0
 434  READ(IIN4,FMT='(A)',END=435,ERR=399)LINE
       IF(LINE(1:1)=='#')GOTO 434
       IF(LINE(1:1)=='/')GOTO 435
       J=J+1
       I=I+1
       IF(I>NUMSOL+NUMQUAD) GOTO 435
       IF (IOUTP_FMT==2) THEN
          READ(LINE,'(3I8)')NIP,NUMS,NELS
       ELSE
        READ(LINE,'(3I10)')NIP,NUMS, NELS
       ENDIF
        NVSOLID2 = MAX(NVSOLID2, MAX(1,NIP)*6)
       DO KK = 1, NELS
        DO K=1,NIP
        READ(IIN4,FMT='(A)',END=435,ERR=399)LINE
        ENDDO
       ENDDO
      GOTO 434
 435  CONTINUE
      REWIND(IIN4)
C     Brick ( variables users)         
 444  READ(IIN4,FMT='(A)',END=499,ERR=499)LINE
       IF(LINE(1:8)=='/ENDDATA')THEN
         REWIND(IIN4)
         GOTO 498
       ENDIF
 450   IF(LINE(1:28)/='/SOLID     /SCALAR    /USERS')GOTO 444
       READ(IIN4,FMT='(A)',END=498,ERR=399)LINE
        IUSOLID = 1
        I = 0
 455  READ(IIN4,FMT='(A)',END=498,ERR=399)LINE
       IF(LINE(1:1)=='#')GOTO 455
       IF(LINE(1:1)=='/')GOTO 498
        IF(IOUTP_FMT==2)THEN
         READ(LINE,'(4I8)')NUMS,NIP,NUVAR,JJHBE
        ELSE
         READ(LINE,'(4I10)')NUMS,NIP,NUVAR,JJHBE
        ENDIF
        NUSOLID = MAX(NUSOLID,NIP*NUVAR)
       I=I+1
       ND = MOD(NUVAR,6)
       NU = (NUVAR - ND)/6
       IF(ND/=0) NU = NU + 1
       IF(NUVAR < 6) NU = MAX(1,NIP)
       IF(I>NUMSOL+NUMQUAD) GOTO 498
       IF (NUVAR==0) GOTO 455
          DO J1 = 1,NU *  MAX(1,NIP)
  459      READ(IIN4,FMT='(A)',END=498,ERR=399)LINE
           IF(LINE(1:1)=='#')GOTO 459
           IF(LINE(1:1)=='/')GOTO 498
         ENDDO
cc       ENDIF
       GO TO 455
 498   CONTINUE
 499   CONTINUE
       NVSHELL = NVSHELL + NVSHELL0
       REWIND(IIN4)
      ENDIF
C-----------------------------------------
C     CONTRAINTES INITIALES FICHIER D00
C-----------------------------------------
      IF (ISIGI==-3.OR.ISIGI==-4.OR.ISIGI==-5) THEN
C------------------------------------
C INISHELL ---> 
C------------------------------------
        NPT = 0
        KCUR = KINISHE
        IREC=KOPTAD(KCUR)-1
        DO WHILE(IREC<KOPTAD(KCUR)+NLINE(KCUR)+NSLASH(KCUR)-1)
          IREC=IREC+1
          READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
          IF(LINE(1:1)=='/')THEN
            KLINE=LINE
            CALL FREDEC6(KEY2,KEY3)
C-----
          ELSEIF(KEY2(1:6)=='STRS_F')THEN
            NUMSHEL=NUMSHEL+1
            READ(LINE,ERR=999,FMT=FMT_3I)NE,NIP,NPG
            IF(KEY3(1:4)== 'GLOB') THEN
              IF(NIP==0)THEN
                NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NPG)*13)
              ELSE
                NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NIP)*MAX(1,NPG)*8)
              ENDIF
            ELSE
              IF(NIP==0)THEN
                NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NPG)*9)
              ELSE
                NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NIP)*MAX(1,NPG)*6)
              ENDIF
            END IF!(KEY3(1:4)== 'GLOB') THEN
cc            NVSHELL = NVAR_SHELL + NVSHELL0
            IREC=IREC+1
            IF (NPG==0.OR.NPG==1) THEN
             IF(NIP==0)THEN
              IF(KEY3(1:4)=='GLOB')THEN
                IREC=IREC + 2
              ELSE
                IREC=IREC+1
              ENDIF
              IF(KEY3(1:4)=='GLOB')THEN
                IREC=IREC+2
              ELSE
                IREC=IREC+1
              ENDIF
             ELSE
              DO J=1,NIP
                IREC=IREC+2
              ENDDO
             ENDIF
            ELSEIF (NPG>1 )THEN
             IF(NIP==0)THEN
              DO N=1,NPG
                IREC=IREC+2
              ENDDO
              IF(KEY3(1:4)=='GLOB') IREC=IREC +2*NPG
             ELSE
              DO J=1,NIP
               DO N=1,NPG
                IREC=IREC+2
              ENDDO
             ENDDO
            ENDIF
           ENDIF
C-----
          ELSEIF(KEY2(1:6)=='EPSP_F')THEN
            NUMSHEL=NUMSHEL+1
            READ(LINE,ERR=999,FMT=FMT_3I)NE,NIP,NPG
            IF(NIP==0)THEN
             NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NPG)*9)
            ELSE
             NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NIP)*MAX(1,NPG)*6)
            ENDIF
            JJ=MAX(1,NIP)*MAX(1,NPG)
            DO WHILE(JJ > 0)
              IREC=IREC+1
              JJ=JJ-5
            END DO
C-----
          ELSEIF(KEY2(1:5)=='THICK')THEN
            NUMSHEL=NUMSHEL+1
C-----
          ELSEIF(KEY2(1:4)=='EPSP')THEN
            NUMSHEL=NUMSHEL+1
C-----
          ELSEIF(KEY2(1:5)=='ORTHO')THEN
            NUMSHEL=NUMSHEL+1
            READ(LINE,ERR=999,FMT=FMT_3I)NE,NIP,IGTYP
            IF(NIP==0)THEN
              NVAR_SHELL = MAX(NVAR_SHELL, 9)
            ELSE
              NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NIP)*24)
            ENDIF
            IORTSHEL = 1
            NORTSHEL = MAX(NORTSHEL, NORTSHEL0 + MAX(1,NIP)*2)
            NPT = MAX(1,NIP)
            IF (IGTYP == 9) THEN
              IREC=IREC+1
            ELSE
              IREC=IREC+MAX(1,NIP)
            ENDIF
C-----
          ELSEIF(KEY2(1:3)=='AUX')THEN
            IUSHELL = 1
            NUMSHEL=NUMSHEL+1
            READ(LINE,ERR=999,FMT=FMT_4I) NE,NIP,NPG,NUVAR
            IF(NIP==0)THEN
              NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NPG)*9)
            ELSE
              NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NIP)*24)
            ENDIF
            NUSHELL = MAX(NUSHELL,NUSHELL0+MAX(1,NPG)*MAX(1,NIP)*NUVAR)
            JJ=MAX(1,NIP)*MAX(1,NPG)
            KK = NUVAR /5
            IF (MOD(NUVAR,5) > 0) KK = KK + 1
            IREC = IREC + JJ*KK
C-----
          ELSEIF(KEY2(1:6)=='STRA_F')THEN
            NUMSHEL=NUMSHEL+1
            READ(LINE,ERR=999,FMT=FMT_3I)NE,NIP,NPG
            IF(KEY3(1:4)=='GLOB')THEN
             IF (NIP==0) NIP=2
C---------store only up to 2 pts of NIP eij(6)+T, pointer= INISHVAR1             
             NVSH_STRA = MAX(NVSH_STRA,2*MAX(1,NPG)*7)
             DO J=1,NIP
               DO N=1,MAX(1,NPG)
                IREC=IREC+2
               ENDDO
             ENDDO
            ELSE
             NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NPG)*8)
             DO N=1,MAX(1,NPG)
              IREC=IREC+2
             ENDDO
            END IF
C-----
	     ELSEIF(KEY2(1:8)=='ORTH_LOC')THEN
            NUMSHEL=NUMSHEL+1
            READ(LINE,ERR=999,FMT=FMT_4I) NE,NIP,NPG,NDIR
            IF(NIP==0)THEN
              NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NPG)*9)
            ELSE
              NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NIP)*24)
            ENDIF
            IORTSHEL = 2
            NORTSHEL = MAX(NORTSHEL, NORTSHEL0 + MAX(1,NIP)*2)
	       IF (NDIR == 2)
     .        NORTSHEL = MAX(NORTSHEL, NORTSHEL0 + MAX(1,NIP)*4)
            IREC=IREC+NIP
C-----
          ELSEIF(KEY2(1:4)=='FAIL')THEN
            NUMSHEL = NUMSHEL + 1
           READ(LINE,ERR=999,FMT=FMT_9I) NE,NLAY,NPG,
     .                                   NPTT,ILAY,IFAIL,IRUPT_TYP,
     .                                   NVAR_RUPT,IMAT
           NPG = MAX(1,NPG)
           NPTT = MAX(1,NPTT)
           NLAY = MAX(1,NLAY)
           NPT_MAX = MAX(NPTT,NLAY)
           NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NLAY)*24)

            IREC=IREC+NPG*NPTT*((NVAR_RUPT+2)/3)
            NVSHELL1 = MAX(NVSHELL1,NPG*NPT_MAX*5*NVAR_RUPT)
C-----
C-----
          ELSEIF(KEY2(1:9)=='SCALE_YLD')THEN
            IUFACYLD = 1
            NUMSHEL=NUMSHEL+1
            READ(LINE,ERR=999,FMT=FMT_3I)NE,NIP,NPG
            IF(NIP==0)THEN
             NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NPG)*9)
            ELSE
             NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NIP)*MAX(1,NPG)*6)
            ENDIF
            !MAX(1,NIP)*MAX(1,NPG)
            IREC=IREC+MAX(1,NIP)*MAX(1,NPG)
            NVSHELL2 = MAX(NVSHELL2,MAX(1,NPG)*MAX(1,NIP))

C-----
C-----
          ENDIF
        ENDDO
C------------------------------------
C SHELL3N
C------------------------------------
        KCUR = KINISH3
        IREC=KOPTAD(KCUR)-1
        I=NUMSHEL
        DO WHILE(IREC<KOPTAD(KCUR)-1+NLINE(KCUR)+NSLASH(KCUR))
          IREC=IREC+1
          READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
C-----
          IF(LINE(1:1)=='/')THEN
            KLINE=LINE
            CALL FREDEC6(KEY2,KEY3)
C-----
          ELSEIF(KEY2(1:6)=='STRS_F')THEN
           NUMSH3N=NUMSH3N+1
           READ(LINE,ERR=999,FMT=FMT_3I)NE,NIP,NPG
           IF(KEY3(1:4)== 'GLOB') THEN
              IF(NIP==0)THEN
                NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NPG)*13)
              ELSE
                NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NIP)*MAX(1,NPG)*8)
              ENDIF
            ELSE
              IF(NIP==0)THEN
                NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NPG)*9)
              ELSE
                NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NIP)*MAX(1,NPG)*6)
              ENDIF
           END IF!(KEY3(1:4)== 'GLOB') THEN
           IREC=IREC+1
           IF (NPG==0.OR.NPG==1) THEN
            IF(NIP==0)THEN
              IF(KEY3(1:4)=='GLOB')THEN
                IREC=IREC+2
              ELSE
                IREC=IREC+1
              ENDIF
              IF(KEY3(1:4)=='GLOB')THEN
                IREC=IREC + 2
              ELSE
                IREC=IREC+1
              ENDIF
            ELSEIF(NIP/=0)THEN
              DO J=1,NIP
                IREC=IREC+2
              ENDDO
            ENDIF
           ELSEIF (NPG>1 )THEN
C            IF(KEY3(1:4)=='GLOB') THEN
C              CALL ANCMSG(MSGID=523,
C     .                    MSGTYPE=MSGERROR,
C     .                    ANMODE=ANINFO_BLIND_2,I1=NE)
C            END IF
            IF(NIP==0)THEN
             DO N=1,NPG
                IREC=IREC +2
             ENDDO
             IF(KEY3(1:4)=='GLOB') IREC=IREC +2*NPG
            ELSEIF(NIP/=0)THEN
              DO J=1,NIP
               DO N=1,NPG
                IREC=IREC+2
               ENDDO
             ENDDO
            ENDIF
           ENDIF
C-----
          ELSEIF(KEY2(1:6)=='EPSP_F')THEN
            NUMSH3N=NUMSH3N+1
            READ(LINE,ERR=999,FMT=FMT_3I)NE,NIP,NPG
            IF(NIP==0)THEN
             NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NPG)*9)
            ELSE
             NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NIP)*MAX(1,NPG)*6)
            ENDIF
            JJ=MAX(1,NIP)*MAX(1,NPG)
            DO WHILE(JJ > 0)
              IREC=IREC+1
              JJ=JJ-5
            END DO
C-----
          ELSEIF(KEY2(1:5)=='THICK')THEN
            NUMSH3N=NUMSH3N+1
C-----
          ELSEIF(KEY2(1:4)=='EPSP')THEN
            NUMSH3N=NUMSH3N+1
C-----
          ELSEIF(KEY2(1:5)=='ORTHO')THEN
            NUMSH3N=NUMSH3N+1
            READ(LINE,ERR=999,FMT=FMT_3I)NE,NIP,IGTYP
            IF(NIP==0)THEN
              NVAR_SHELL = MAX(NVAR_SHELL, 9)
            ELSE
              NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NIP)*24)
            ENDIF
            IORTSHEL = 1
            NORTSHEL = MAX(NORTSHEL, NORTSHEL0 + MAX(1,NIP)*2)
            NPT = MAX(1,NIP)
            IF (IGTYP == 9) THEN
              IREC=IREC+1
            ELSE
              IREC=IREC+MAX(1,NIP)
            ENDIF
C-----
          ELSEIF(KEY2(1:3)=='AUX')THEN
            IUSHELL = 1
            NUMSH3N=NUMSH3N+1
            READ(LINE,ERR=999,FMT=FMT_4I) NE,NIP,NPG,NUVAR
            IF(NIP==0)THEN
              NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NPG)*9)
            ELSE
              NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NIP)*24)
            ENDIF
            NUSHELL = MAX(NUSHELL,NUSHELL0+MAX(1,NPG)*MAX(1,NIP)*NUVAR)
            JJ=MAX(1,NIP)*MAX(1,NPG)
            KK = NUVAR /5
            IF (MOD(NUVAR,5) > 0) KK = KK + 1
            IREC = IREC + JJ*KK
C-----
          ELSEIF(KEY2(1:6)=='STRA_F')THEN
            NUMSH3N=NUMSH3N+1
            READ(LINE,ERR=999,FMT=FMT_3I)NE,NIP,NPG
            IF(KEY3(1:4)=='GLOB')THEN
             IF (NIP==0) NIP=2
C---------store only up to 2 pts of NIP eij(6)+T, sufficient to compute generalized {e}             
             NVSH_STRA = MAX(NVSH_STRA,2*MAX(1,NPG)*7)
             DO J=1,NIP
               DO N=1,MAX(1,NPG)
                IREC=IREC+2
               ENDDO
             ENDDO
            ELSE
             NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NPG)*8)
             DO N=1,MAX(1,NPG)
              IREC=IREC+2
             ENDDO
            END IF
C-----
          ELSEIF(KEY2(1:8)=='ORTH_LOC')THEN
            NUMSH3N=NUMSH3N+1
            READ(LINE,ERR=999,FMT=FMT_4I) NE,NIP,NPG,NDIR
            IF(NIP==0)THEN
              NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NPG)*9)
            ELSE
              NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NIP)*24)
            ENDIF
            IORTSHEL = 2
            NORTSHEL = MAX(NORTSHEL, NORTSHEL0 + MAX(1,NIP)*2)
            IF (NDIR == 2) NORTSHEL = MAX(NORTSHEL, NORTSHEL0 + MAX(1,NIP)*4)
            IREC=IREC+NIP
C-----
          ELSEIF(KEY2(1:4)=='FAIL')THEN
            NUMSH3N = NUMSH3N + 1
            READ(LINE,ERR=999,FMT=FMT_9I) NE,NLAY,NPG,
     .                                   NPTT,ILAY,IFAIL,IRUPT_TYP,
     .                                   NVAR_RUPT,IMAT
            NPG = MAX(1,NPG)
            NPTT = MAX(1,NPTT)
            NLAY = MAX(1,NLAY)
            NPT_MAX = MAX(NPTT,NLAY)
            NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NLAY)*24)

            IREC=IREC+NPG*NPTT*((NVAR_RUPT+2)/3)
            NVSHELL1 = MAX(NVSHELL1,NPG*NPT_MAX*5*NVAR_RUPT)
c-------------
          ELSEIF(KEY2(1:9)=='SCALE_YLD')THEN
            IUFACYLD = 1
            NUMSH3N = NUMSH3N + 1

            READ(LINE,ERR=999,FMT=FMT_3I)NE,NIP,NPG
            IF(NIP==0)THEN
             NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NPG)*9)
            ELSE
             NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NIP)*MAX(1,NPG)*6)
            ENDIF
            NVSHELL2 = MAX(NVSHELL2,MAX(1,NPG)*MAX(1,NIP))

            IREC=IREC+MAX(1,NIP)*MAX(1,NPG)
C-----
          ENDIF
        ENDDO
        INISHVAR1 = NVAR_SHELL + NVSHELL0 + NPT
        NVSHELL = INISHVAR1 + NVSH_STRA
C------------------------------------
C INIBEAM
C------------------------------------
        KCUR = KINIBEA
        IREC=KOPTAD(KCUR)-1
        DO WHILE(IREC<KOPTAD(KCUR)+NLINE(KCUR)+NSLASH(KCUR)-1)
          IREC=IREC+1
          READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
          IF (LINE(1:1) == '/') THEN
            KLINE=LINE
            CALL FREDEC6(KEY2,KEY3)
C-----
          ELSEIF (KEY2(1:4) == 'FULL') THEN
            NUMBEAM=NUMBEAM+1
            READ(LINE,ERR=999,FMT=FMT_3I) NE,NIP,IGTYP
C------
            IREC = IREC + 4
            NVBEAM = NVBEAM + 11
            IF (IGTYP == 3) THEN
              IREC = IREC + 1 ! EPSP
              NVBEAM = NVBEAM + 1
            ELSEIF (IGTYP == 18) THEN
              IREC = IREC + NIP ! EPSP
              NVBEAM = NVBEAM + 4*NIP
            ENDIF
            IREC = IREC - 1
          ELSEIF (KEY2(1:3) == 'AUX') THEN
            READ(LINE,ERR=999,FMT=FMT_4I) NE,NIP,IGTYP,NUVAR
!  NUVAR
            IF (IGTYP == 18) THEN
              NUMBEAM=NUMBEAM+1
              NUBEAM = MAX(NUBEAM,NUBEAM0 + NIP*NUVAR)
              JJ = MAX(1,NIP)
              KK = NUVAR / 5
              IF (MOD(NUVAR,5) > 0) KK = KK + 1
              IREC = IREC + JJ*KK
            ENDIF ! IF (IGTYP == 18)
C------
          ENDIF ! IF(LINE(1:1)=='/')
        ENDDO ! DO WHILE
C------------------------------------
C INITRUSS
C------------------------------------
        KCUR = KINITRU
        IREC=KOPTAD(KCUR)-1
        DO WHILE(IREC<KOPTAD(KCUR)+NLINE(KCUR)+NSLASH(KCUR)-1)
          IREC=IREC+1
          READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
          IF (LINE(1:1) == '/') THEN
            KLINE=LINE
            CALL FREDEC6(KEY2,KEY3)
C-----
          ELSEIF (KEY2(1:4) == 'FULL') THEN
            NUMTRUS=NUMTRUS+1
            READ(LINE,ERR=999,FMT=FMT_I_X_I) NE,IGTYP
C------
            IREC = IREC + 1
            NVTRUSS = NVTRUSS + 6
C------
          ENDIF ! IF(LINE(1:1)=='/')
        ENDDO ! DO WHILE
      ENDIF
C------------------------------------
C INIBRI ---> 
C------------------------------------
        KCUR = KINIBRI
        IREC=KOPTAD(KCUR)-1
        DO WHILE(IREC<KOPTAD(KCUR)-1+NLINE(KCUR)+NSLASH(KCUR))
          IREC=IREC+1
          READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
C-----
          IF(LINE(1:1)=='/')THEN
            KLINE=LINE
            CALL FREDEC_2KEY_ID_OR_KEY_ID(KEY2,KEY3,UID,SUB_ID)
C-----
          ELSEIF(KEY2(1:4)=='FILL')THEN
            NUMSOL  = NUMSOL + 1
            NFILSOL = 1
C-----
          ELSEIF(KEY2(1:4)=='EPSP')THEN
            NUMSOL = NUMSOL + 1
C-----
          ELSEIF(KEY2(1:4)=='ENER')THEN
            NUMSOL = NUMSOL + 1
C-----
          ELSEIF(KEY2(1:4)=='DENS')THEN
            NUMSOL = NUMSOL + 1
C-----
          ELSEIF(KEY2(1:6)=='STRESS')THEN
            NUMSOL = NUMSOL + 1
            IREC = IREC + 1
            NVSOLID1 = MAX (NVSOLID1,6)
C-----
          ELSEIF(KEY2(1:3)=='AUX')THEN
            NUMSOL = NUMSOL + 1
            READ(LINE,ERR=999,FMT=FMT_7I) NE,NPT,ISOLNOD,ISOLID,IFRAM,IORTH,NUVAR
            IUSOLID = 1
            IREC=IREC+1
            NUSOLID = MAX(NUSOLID,NPT*NUVAR)
            ND = MOD(NUVAR,3)
            NU = (NUVAR - ND )/3
            IF(ND/=0) NU = NU + 1
            DO J1 = 1,NPT * NU
              IREC = IREC + 1
            ENDDO
              IREC = IREC - 1
C-----
          ELSEIF(KEY2(1:6)=='STRS_F')THEN
            READ(LINE,ERR=999,FMT=FMT_9I) NE,NPT,ISOLNOD,ISOLID,NPTR,NPTS,NPTT,NLAY,IGBR
            IF(IGBR > 0) THEN
               IOK  = 0
               JGBR = 0
               DO J=1,NGRBRIC
                 IF (IGBR == IGRBRIC(J)%ID) THEN
                   JGBR = J
                   IOK  = 1
                   EXIT
                 ENDIF
               ENDDO
               IF (IOK == 0) THEN
                 CALL ANCMSG(MSGID=1611,MSGTYPE=MSGERROR,ANMODE=ANINFO,C1='STRS_F',I1=IGBR)
               ENDIF
               NUMSOL = NUMSOL + IGRBRIC(JGBR)%NENTITY
            ELSE
               NUMSOL = NUMSOL + 1
            ENDIF
            NVSOLID1 = MAX (NVSOLID1,NPT*9 + 4)

            IF ((ISOLNOD == 8 .AND. (ISOLID==1.OR.ISOLID==2.OR.ISOLID==12.OR.ISOLID==24))
     .           .OR.(ISOLNOD == 4 .AND. NPT == 1)) THEN
C--------------------First Group of solids : 4 lines to be read-----------------
               IF(ISOLNOD == 8 .AND. ISOLID == 12)THEN
                 DO J=1,NPT
                   IREC = IREC + 4
                 ENDDO
               ELSE
                IREC = IREC + 4
               ENDIF
            ELSE
C--------------------Second Group of solids : 3 lines to be read-----------------
              DO J=1,NPT
                IREC = IREC + 3
              ENDDO
            ENDIF
C-----
          ELSEIF(KEY2(1:6)=='STRA_F')THEN
            NUMSOL = NUMSOL + 1
            READ(LINE,ERR=999,FMT=FMT_5I) NE,NPT,ISOLNOD,ISOLID,IFRAM
            NVSOLID2 = MAX(NVSOLID2, MAX(1,NPT)*6)
            IREC=IREC+1
            IF (NPT == 0 ) THEN
              IREC = IREC + 1
            ELSE
              DO J=1,NPT
                IREC = IREC + 2
              ENDDO
                IREC = IREC - 1
            ENDIF
C-----
          ELSEIF(KEY2(1:5)=='ORTHO')THEN
            NUMSOL = NUMSOL + 1
            READ(LINE,ERR=999,FMT=FMT_5I) NE,NLAYERS,ISOLNOD,IGTYP,JJHBE
            IREC=IREC+1
            NVSOLID3 = MAX(NVSOLID3,NLAYERS * 6)
            IF(KEY2(8:10)=='GLO'.OR.
     .          (IGTYP /= 21 .AND. IGTYP /= 22)) THEN
              IREC=IREC+NLAYERS*2 - 1
            ELSE
              IREC=IREC+NLAYERS - 1
            ENDIF
          ELSEIF(KEY2(1:4)=='FAIL')THEN
            NUMSOL = NUMSOL + 1
           READ(LINE,ERR=999,FMT=FMT_10I) NE,NLAY,
     .            NPTR,NPTS,NPTT,ILAY,IFAIL,IRUPT_TYP,NVAR_RUPT,IMAT
            IREC=IREC+NPTR*NPTS*NPTT*((NVAR_RUPT+2)/3)
            NVSOLID4 = MAX(NVSOLID4,NPTR*NPTS*NPTT*NLAY*5*NVAR_RUPT)
c -    - - -
          ELSEIF(KEY2(1:9)=='SCALE_YLD')THEN
            IUFACYLD = 1
            NUMSOL = NUMSOL + 1
            READ(LINE,ERR=999,FMT=FMT_5I) NE,NPTR,NPTS,NPTT,ILAY
            IREC=IREC+1

            NVSOLID5 = MAX(NVSOLID5,NPTR*NPTS*NPTT*ILAY + 7 )
c
            IREC = IREC + NPTR*NPTS*NPTT*ILAY
            IREC = IREC - 1
c
C-----
          ELSEIF(KEY2(1:4)=='EREF')THEN
            NUMSOL = NUMSOL + 1
            READ(LINE,ERR=999,FMT=FMT_5I) NE,NPT,ISOLNOD,ISOLID,ISMSTR
C------ use NVSOLID5 temporaly, read directly after new reader  or add NVSOLID6    
            NVSOLID5 = MAX(NVSOLID5, ISOLNOD*3)
            IREC=IREC+ISOLNOD
          ENDIF
        ENDDO
C------------------------------------
C INISPRI ---> 
C------------------------------------
        KCUR = KINISPRI
        IREC=KOPTAD(KCUR)-1
        DO WHILE(IREC<KOPTAD(KCUR)+NLINE(KCUR)+NSLASH(KCUR)-1)
          IREC=IREC+1
          READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
          IF (LINE(1:1) == '/') THEN
            KLINE=LINE
            CALL FREDEC6(KEY2,KEY3)
C-----
          ELSEIF(KEY2(1:4) == 'FULL')THEN
            NUMSPRI=NUMSPRI+1
            READ(LINE,ERR=999,FMT=FMT_3I) NE,IGTYP,NUVAR
C------
            IF (IGTYP == 4 . OR. IGTYP == 12) THEN
C------
              IREC=IREC+2
C------
            ELSEIF (IGTYP == 8  .OR. IGTYP == 13 .OR.
     .              IGTYP == 23 .OR. IGTYP == 25) THEN
C------
              IREC=IREC+8
C------
            ELSEIF (IGTYP == 26) THEN
C------
              IREC=IREC+2
C------
C   user springs
            ELSEIF (IGTYP == 29 .OR. IGTYP == 30 .OR. IGTYP == 31 .OR.
     .              IGTYP == 32 .OR. IGTYP == 33 .OR. IGTYP == 35 .OR.
     .              IGTYP == 36 .OR. IGTYP == 44 .OR. IGTYP == 45 .OR.
     .              IGTYP == 46) THEN
C------
              IREC=IREC+7
!  NUVAR
              KK = NUVAR/5
              IF (MOD(NUVAR,5) > 0) KK = KK + 1
              IREC = IREC + KK
            ENDIF ! IF (IGTYP)
C------
          ENDIF ! IF(LINE(1:1)=='/')
        ENDDO ! DO WHILE
C------------------------------------
C QUADS
C------------------------------------
        KCUR = KINIQUA
        IREC=KOPTAD(KCUR)-1
        DO N=1,NLINE(KCUR)+NSLASH(KCUR)
          IREC=IREC+1
          READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
          IF(LINE(1:1) == '/')THEN
            KLINE=LINE
            CALL FREDEC5(KEY2,UID)
          ELSEIF(KEY2(1:4) == 'EPSP')THEN
            NUMQUAD=NUMQUAD+1
          ELSEIF(KEY2(1:4) == 'ENER')THEN
            NUMQUAD=NUMQUAD+1
          ELSEIF(KEY2(1:4) == 'DENS')THEN
            NUMQUAD=NUMQUAD+1
          ELSEIF(KEY2(1:6) == 'STRESS')THEN
            NUMQUAD=NUMQUAD+1
          ENDIF
        ENDDO !DO N=1,NLINE(KCUR)+NSLASH(KCUR)
!-----------
      RETURN
 399  CONTINUE
       CALL ANCMSG(MSGID=557,
     .             MSGTYPE=MSGERROR,
     .             ANMODE=ANINFO_BLIND_1)
 999  CALL FREERR(3)
      RETURN
      END

Chd|====================================================================
Chd|  UEL2SYS                       source/initial_conditions/inista/yctrl.F
Chd|-- called by -----------
Chd|        HM_READ_INISTATE_D00          source/elements/initia/hm_read_inistate_d00.F
Chd|        INIBOLTPREL                   source/loads/bolt/iniboltprel.F
Chd|        LECFILL                       source/elements/initia/lecfill.F
Chd|        LEC_INISTATE_TRI              source/elements/initia/lec_inistate_tri.F
Chd|-- calls ---------------
Chd|====================================================================
      INTEGER FUNCTION UEL2SYS(IU,KSYSUSR,NUMEL)
C     FONCTION DONNE N0 SYSTEME DE L'ELT USER IU (0 si IU nexiste pas)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IU,KSYSUSR(*),NUMEL
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JINF, JSUP, J
      IF (NUMEL == 0) THEN
        UEL2SYS=0
        RETURN
      ENDIF
      JINF=1
      JSUP=NUMEL
      J=MAX(1,NUMEL/2)
   10 IF(JSUP<=JINF.AND.(IU-KSYSUSR(J))/=0) THEN
        UEL2SYS=0
        RETURN
      ENDIF
      IF((IU-KSYSUSR(J))==0)THEN
C     >CAS IU=USR FIN DE LA RECHERCHE
         UEL2SYS=KSYSUSR(J+NUMEL)
         RETURN
      ELSE IF (IU-KSYSUSR(J)<0) THEN
C     >CAS IU<USR
         JSUP=J-1
      ELSE
C     >CAS IU>USR
         JINF=J+1
      ENDIF
      J=MAX(1,(JSUP+JINF)/2)
      GO TO 10
      END
