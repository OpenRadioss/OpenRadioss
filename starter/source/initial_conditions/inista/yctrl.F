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
Chd|  YCTRL                         source/initial_conditions/inista/yctrl.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
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
