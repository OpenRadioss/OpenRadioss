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
Chd|  ANIM_DCOD_KEY_0               source/output/anim/reader/anim_dcod_key_0.F
Chd|-- called by -----------
Chd|        FREANIM                       source/output/anim/reader/freanim.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE ANIM_DCOD_KEY_0(KEY2,KEY3,KEY4,KEY5,KEY6,JERR,IXITKEY)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE STACK_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr14_c.inc"
#include      "scr25_c.inc"
#include      "scrcut_c.inc"
#include      "sms_c.inc"
#include      "chara_c.inc"
#include      "nchara_c.inc"
#include      "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JERR, IXITKEY
      CHARACTER KEY2*ncharkey, KEY3*ncharkey, KEY4*ncharkey, 
     .          KEY5*ncharkey, KEY6*ncharkey
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IDX, N1, N2, N3, ITYP,IADG, J,NTITLE,IUS,IPT
      my_real 
     .        X0,Y0,Z0,VNX,VNY,VNZ,V0
      LOGICAL :: LAW51_PHASE1, LAW51_PHASE2, LAW51_PHASE3, LAW51_PHASE4
C-----------------------------------------------
C     Output parameter for LAW51 multiphase data
C     By default set to FALSE
C     Activated in engine file with 
C     /ANIM/BRIC(ELEM)/LAW51/ALL
C     /ANIM/BRIC(ELEM)/LAW51/1
C     and so on 
      LAW51_PHASE1 = .FALSE. 
      LAW51_PHASE2 = .FALSE. 
      LAW51_PHASE3 = .FALSE.
      LAW51_PHASE4 = .FALSE.
C-----------------------------------------------
      IF(KEY2(1:4)=='COMP')THEN
       IZIP=1
      ELSEIF(KEY2(1:4)=='GZIP')THEN
       IZIP=2
      ELSEIF(KEY2(1:4)=='VERS')THEN
       READ(KEY3,'(I2)')ANIM_VERS
      ELSEIF(KEY2(1:3)=='MAT')THEN
       ANIM_MAT = 1
C
      ELSEIF(KEY2(1:4)=='VECT')THEN
C--------------------------
C       VECTEUR
C--------------------------
        IF    (KEY3(1:3)=='VEL')THEN
          ANIM_V(1) = 1
        ELSEIF(KEY3(1:3)=='DIS')THEN
          ANIM_V(2) = 1
        ELSEIF(KEY3(1:3)=='ACC')THEN
          ANIM_V(3) = 1
        ELSEIF(KEY3(1:4)=='CONT' .AND. KEY3(1:5)/='CONT2'.AND.KEY4(1:3)/='MAX')THEN
          ANIM_V(4) = 1
          ANIMCONT = ANIM_V(4)  
        ELSEIF(KEY3(1:4)=='FINT')THEN
          ANIM_V(5) = 1
        ELSEIF(KEY3(1:4)=='FEXT')THEN
           ANIM_V(6) = 1
        ELSEIF(KEY3(1:4)=='FOPT')THEN
          ANIM_V(7) = 1
          ANIM_V(8) = 1
C         vitesses de rotations
        ELSEIF(KEY3(1:4)=='VROT')THEN
          ANIM_V(9) = 1
C         vitesses fluides pour alebag et /bem/flow
        ELSEIF(KEY3(1:4)=='FVEL'.OR.KEY3(1:4)=='VFLU')THEN
C         vitesses fluides pour alebag et /bem/flow
          ANIM_V(10) = 1
        ELSEIF(KEY3(1:5)=='FRES')THEN
          IF ((ANIM_V(5)/=1.OR.ANIM_V(6)/=1).AND.IDTMINS==0.AND.IDTMINS_INT==0)THEN
            CALL ANCMSG(MSGID=190,MSGTYPE=MSGERROR,ANMODE=ANINFO)
            JERR   =JERR+1
            IXITKEY=IXITKEY+1
          ENDIF
          ANIM_V(11) = 1    
        ELSEIF(KEY3(1:5)=='PCONT'.AND. KEY3(1:6)/='PCONT2')THEN
         ANIM_V(12) = 1
        ELSEIF(KEY3(1:5)=='CONT2')THEN
         ANIM_V(13) = 1
        ELSEIF(KEY3(1:4)=='DROT')THEN
         ANIM_V(14) = 1
        ELSEIF(KEY3(1:5)=='DXANC')THEN
          ANIM_V(15) = 1
        ELSEIF(KEY3(1:4)=='GVEL')THEN
          ANIM_V(16) = 1
        ELSEIF(KEY3(1:5)=='FREAC')THEN
          ANIM_V(17) = 1
        ELSEIF(KEY3(1:5)=='MREAC')THEN
          ANIM_V(18) = 1
        ELSEIF (KEY3(1:5)=='CLUST')THEN
          IF(KEY4(1:5)=='FORCE')THEN
            ANIM_V(19) = 1
          ELSEIF  (KEY4(1:3)=='MOM')THEN
            ANIM_V(20) = 1
          ENDIF
        ELSEIF (KEY3(1:4)=='ZVEL')THEN  !inter22
          ANIM_V(21) = 1    
        ELSEIF (KEY3(1:5)=='ZFVEL')THEN !inter22
          ANIM_V(22) = 1 
        ELSEIF (KEY3(1:4)=='ZMOM')THEN  !inter22
          ANIM_V(23) = 1   
        ELSEIF (KEY3(1:3)=='ZFP')THEN   !inter22
          ANIM_V(24) = 1    
        ELSEIF (KEY3(1:5)=='ZFINT')THEN !inter22
          ANIM_V(25) = 1    
        ELSEIF (KEY3(1:4)=='CONT'.AND.KEY4(1:3)=='MAX')THEN 
          ANIM_V(26) = 1    
          ANIMCONT = ANIM_V(4)  
          ANIM_V(4) = 1     
        ELSEIF(KEY3(1:6)=='PCONT2')THEN
         ANIM_V(27) = 1
        ELSE
          IXITKEY=IXITKEY+1
        ENDIF
C
      ELSEIF(KEY2(1:4)=='TENS')THEN
C--------------------------
C      TENSEUR OLD
C--------------------------
       IF    (KEY3(1:6)=='STRESS')THEN
         IF    (KEY4(1:4)=='MEMB')THEN
           ANIM_T(1) = 1
           ANIM_CT(1) = 1
         ELSEIF(KEY4(1:4)=='BEND')THEN
           ANIM_T(2) = 1
           ANIM_CT(2) = 1
         ELSEIF(KEY4(1:5)=='UPPER')THEN
           ANIM_T(3) = 1
           ANIM_CT(3) = 1
         ELSEIF(KEY4(1:5)=='LOWER')THEN
           ANIM_T(4) = 1
           ANIM_CT(4) = 1
         ELSEIF(KEY4(1:3)=='ALL')THEN
           ISTRESALL =  1
         ELSE
           READ(KEY4(1:5),'(I5)')N1
           IF(N1>0.AND.N1<=100)THEN
             ANIM_T(100+N1) = 1
             ANIM_CT(100+N1) = 1
           ELSE
             IXITKEY=IXITKEY+1
           ENDIF
         ENDIF
       ELSEIF(KEY3(1:5)=='STRAI')THEN
         IF    (KEY4(1:4)=='MEMB')THEN
           ANIM_T(5) = 1
           ANIM_CT(5) = 1
         ELSEIF(KEY4(1:4)=='BEND')THEN
           ANIM_T(6) = 1
           ANIM_CT(6) = 1
         ELSEIF(KEY4(1:5)=='UPPER')THEN
           ANIM_T(7) = 1
           ANIM_CT(7) = 1
         ELSEIF(KEY4(1:5)=='LOWER')THEN
           ANIM_T(8) = 1
           ANIM_CT(8) = 1
         ELSEIF(KEY4(1:3)=='ALL')THEN
           ISTRAIALL = 1
         ELSE
           READ(KEY4(1:5),'(I5)')N1
           IF(N1>0.AND.N1<=100)THEN
             ANIM_T(200+N1) = 1
             ANIM_CT(200+N1) = 1
           ELSE
             IXITKEY=IXITKEY+1
           ENDIF
         ENDIF
       ELSEIF(KEY3(1:5)=='EPSDO')THEN
         IEPSDOT = 1
         IF    (KEY4(1:4)=='MEMB')THEN
           ANIM_T(91) = 1
           ANIM_CT(91) = 1
         ELSEIF(KEY4(1:4)=='BEND')THEN
           ANIM_T(92) = 1
           ANIM_CT(92) = 1
         ELSEIF(KEY4(1:5)=='UPPER')THEN
           ANIM_T(93) = 1
           ANIM_CT(93) = 1
         ELSEIF(KEY4(1:5)=='LOWER')THEN
           ANIM_T(94) = 1
           ANIM_CT(94) = 1
         ELSEIF(KEY4(1:4)=='ALL')THEN
           IEPSDOALL = 1
         ELSE
           READ(KEY4(1:5),'(I5)')N1
           IF(N1>0.AND.N1<=100)THEN
             ANIM_T(300+N1) = 1
             ANIM_CT(300+N1) = 1
           ELSE
             IXITKEY=IXITKEY+1
           ENDIF
         ENDIF
       ELSE
         IXITKEY=IXITKEY+1
       ENDIF
C--------------------------
C    SHELL
C--------------------------
      ELSEIF(KEY2(1:4)=='SHEL')THEN
C
       FMT_ANI = 4
C-----------------------------
C    ply shell for composite with xfem ply formulation
C-------------------------------
      IF(KEY3(1:3)=='PLY') THEN
         ANIM_PLY = 1
          !READ(KEY4,'(I5)')N1
          ! IF(N1>0.AND.N1<=200)THEN
          !    ANIM_PLIES(N1) = 1
          !  ELSE
          !    IXITKEY=IXITKEY+1
          !  ENDIF
C----------------------------------------------------   
C  ooutp for inter ply composites shell
C---------------------------------------
       ELSEIF(KEY3(1:4)=='IPLY') THEN
             IF(KEY4(1:3)=='DAM' .OR. KEY4(1:5)=='MINDA') THEN
                 ANIM_CE(10240)  = 1
             ELSEIF(KEY4(1:5)=='SIGZZ') THEN
                  ANIM_CE(10241) = 1
             ELSEIF(KEY4(1:5)=='SIGYZ') THEN
                  ANIM_CE(10242) = 1
             ELSEIF(KEY4(1:5)=='SIGXZ') THEN
                  ANIM_CE(10243) = 1
             ELSEIF(KEY4(1:5)=='EPSZZ') THEN
                  ANIM_CE(10244) = 1
             ELSEIF(KEY4(1:5)=='EPSYZ') THEN
                  ANIM_CE(10245) = 1
             ELSEIF(KEY4(1:5)=='EPSXZ') THEN
                  ANIM_CE(10246) = 1
             ELSEIF(KEY4(1:4)=='EINT') THEN
                  ANIM_CE(10247)= 1
             ELSEIF(KEY4(1:5)=='MAXDA') THEN
                  ANIM_CE(10669)= 1
             ENDIF           
      !fin        
      ! XFEM for layered shell (real crack path visualisation)
      ELSE IF(KEY3(1:3)=='CRK') THEN
c          ANIM_CRK = 1
C--------------------------
C    SHELL:TENSOR
C--------------------------
      ELSEIF (KEY3(1:4) == 'TENS') THEN
        IF (KEY4(1:5) == 'STRES') THEN
          N1 = 0
          N2 = 0
          IF (KEY5(1:4) == 'MEMB') THEN
            ANIM_CT(1) = 1
          ELSEIF (KEY5(1:4) == 'BEND') THEN
            ANIM_CT(2) = 1
          ELSEIF (KEY5(1:5) == 'UPPER') THEN
            ANIM_CT(3) = 1
          ELSEIF (KEY5(1:5) == 'LOWER') THEN
            ANIM_CT(4) = 1
          ELSEIF (KEY5(1:3) == 'ALL') THEN
            ISTRESALL =  1 ! mean layer value
            IF (KEY6(1:3) == 'ALL') ISTRESFULL =  2 ! all NPTT
          ELSE
            READ(KEY5(1:5),'(I5)') N1
            IF (N1 > 0 .AND. N1 <= 100) THEN
              ! case of PID51  (read NPTT, or, UPPER or LOWER within NPTT)
              IF (KEY6(1:5) == 'UPPER') THEN
                ANIM_CT(400+N1) = 1
              ELSEIF (KEY6(1:5) == 'LOWER') THEN
                ANIM_CT(500+N1) = 1
              ELSEIF (KEY6(1:3) == 'ALL') THEN
                ISTRESFULL =  1
                ANIM_STRESS(N1) = 1
              ELSE
                READ(KEY6(1:5),'(I5)') N2   !  NPTT (max=10) through layer N1 (max=100)
                IF (N2 > 0 .AND. N2 <= 10) THEN
                  ANIM_CT(600 + 10*N1 + N2) = 1 ! NPTT through N1
                ELSE
                  ANIM_CT(100+N1) = 1 ! classic layer out (mean value)
                ENDIF ! IF (N2 > 0 .AND. N2 <= 10)
              ENDIF ! IF (KEY6(1:5) == 'UPPER')
            ELSE
              IXITKEY=IXITKEY+1
            ENDIF
          ENDIF
C------------
        ELSEIF (KEY4(1:5) == 'STRAI') THEN
          N1 = 0
          N2 = 0
          IDX = 1610 + 3*MX_PLY_ANIM
          IF (KEY5(1:4) == 'MEMB') THEN
            ANIM_CT(5) = 1
          ELSEIF (KEY5(1:4) == 'BEND') THEN
            ANIM_CT(6) = 1
          ELSEIF (KEY5(1:5) == 'UPPER') THEN
            ANIM_CT(7) = 1
          ELSEIF (KEY5(1:5) == 'LOWER') THEN
            ANIM_CT(8) = 1
          ELSEIF (KEY5(1:3) == 'ALL') THEN
            ISTRAIALL = 1 ! mean layer value
            IF (KEY6(1:3) == 'ALL') ISTRAINFULL =  2 ! all NPTT
          ELSE
            READ(KEY5(1:5),'(I5)') N1
            IF (N1 > 0 .AND. N1 <= 100) THEN
            !   case of PID_51, 52  (read NPTT, or, UPPER or LOWER, within NPTT)
              IF (KEY6(1:5) == 'UPPER') THEN
                ANIM_CT(IDX + N1) = 1
              ELSEIF (KEY6(1:5) == 'LOWER') THEN
                ANIM_CT(IDX + 100 + N1) = 1
              ELSEIF (KEY6(1:3) == 'ALL') THEN
                ISTRAINFULL =  1
                ANIM_STRAIN(N1) = 1
              ELSE
                READ(KEY6(1:5),'(I5)') N2   !  NPTT (max=10) through layer N1 (max=100)
                IF (N2 > 0 .AND. N2 <= 10) THEN
                  ANIM_CT(IDX + 200 + 10*N1 + N2) = 1 ! NPTT through N1
                ELSE
                  ANIM_CT(200+N1) = 1 ! classic layer out (mean value)
                ENDIF ! IF (N2 > 0 .AND. N2 <= 10)
              ENDIF ! IF (KEY6(1:5) == 'UPPER')
            ELSE
              IXITKEY=IXITKEY+1
            ENDIF ! IF (N1 > 0 .AND. N1 <= 100)
          ENDIF ! IF (KEY5(1:4)
C------------
        ELSEIF(KEY4(1:5)=='EPSDO')THEN
          IEPSDOT = 1
          N1 = 0
          N2 = 0
          IDX = 2820 + 3*MX_PLY_ANIM
          IF (KEY5(1:4) == 'MEMB') THEN
            ANIM_CT(91) = 1
          ELSEIF (KEY5(1:4) == 'BEND') THEN
            ANIM_CT(92) = 1
          ELSEIF (KEY5(1:5) == 'UPPER') THEN
            ANIM_CT(93) = 1
          ELSEIF (KEY5(1:5) == 'LOWER') THEN
            ANIM_CT(94) = 1
          ELSEIF(KEY5(1:5) == 'ALL') THEN
            IEPSDOALL = 1 ! mean layer value
            IF (KEY6(1:3) == 'ALL') IEPSDOFULL =  2 ! all NPTT
          ELSE
            READ(KEY5(1:5),'(I5)') N1
            IF (N1 > 0 .AND. N1 <= 100) THEN
            !   case of PID_51, 52  (read NPTT, or, UPPER or LOWER, within NPTT)
              IF (KEY6(1:5) == 'UPPER') THEN
                ANIM_CT(IDX + N1) = 1
              ELSEIF (KEY6(1:5) == 'LOWER') THEN
                ANIM_CT(IDX + 100 + N1) = 1
              ELSEIF (KEY6(1:3) == 'ALL') THEN
                IEPSDOFULL =  1
                ANIM_EPSDOT(N1) = 1
              ELSE
                READ(KEY6(1:5),'(I5)') N2   !  NPTT (max=10) through layer N1 (max=100)
                IF (N2 > 0 .AND. N2 <= 10) THEN
                  ANIM_CT(IDX + 200 + 10*N1 + N2) = 1 ! NPTT through N1
                ELSE
                  ANIM_CT(300+N1) = 1 ! classic layer out (mean value)
                ENDIF ! IF (N2 > 0 .AND. N2 <= 10)
              ENDIF ! IF (KEY6(1:5) == 'UPPER')
            ELSE
              IXITKEY=IXITKEY+1
            ENDIF ! IF (N1 > 0 .AND. N1 <= 100)
          ENDIF ! IF (KEY5(1:4)
!
        ENDIF
C--------------------------
C    COQUES:SCALAIRES
C--------------------------
       ELSEIF(KEY3(1:4)=='EPSP')THEN
         N1 = 0
         N2 = 0
         IF(KEY4(1:5)=='UPPER')THEN
           ANIM_CE(2040)=1
         ELSEIF(KEY4(1:5)=='LOWER')THEN
           ANIM_CE(2041)=1
         ELSEIF(KEY4(1:3)=='ALL')THEN
           IEPSPALL = 1 ! mean layer value
           IF(KEY5(1:3)=='ALL') IEPSPFULL =  2 ! all NPTT
         ELSE
           READ(KEY4(1:5),'(I5)')N1   !  layer
           IF (N1 > 0 .AND. N1 <= 100) THEN
             ! case of PID51  (read NPTT, or, UPPER or LOWER within NPTT)
             IF(KEY5(1:5)=='UPPER')THEN
               ANIM_CE(10677+N1) = 1
             ELSEIF(KEY5(1:5)=='LOWER')THEN
               ANIM_CE(10777+N1) = 1
             ELSEIF(KEY5(1:3)=='ALL')THEN
               IEPSPFULL = 1
               ANIM_EPSP(N1) = 1
             ELSE
               READ(KEY5(1:5),'(I5)')N2   !  NPTT (max=10) through layer N1 (max=100)
               IF (N2 > 0 .AND. N2 <= 10) THEN
                 ANIM_CE(10877 + 10*N1 + N2) = 1 ! NPTT through N1
               ELSE
                 ANIM_CE(2041+N1) = 1 ! classic layer out (mean value)
               ENDIF
             ENDIF
           ELSE
             ANIM_CE(1) = 1
           ENDIF
         ENDIF
C------------
       ELSEIF(KEY3(1:4)=='DENS')THEN
         ANIM_CE(2) = 1
       ELSEIF(KEY3(1:4)=='ENER')THEN
         ANIM_CE(3) = 1
       ELSEIF(KEY3(1:4)=='TEMP')THEN
         ANIM_CE(4) = 1
       ELSEIF(KEY3(1:4)=='THIC')THEN
         ANIM_CE(5) = 1
       ELSEIF(KEY3(1:1)=='P' .AND.  KEY3(1:3) /= 'PHI' .AND.  KEY3(1:5) /= 'PLA51')THEN
         ANIM_CE(6) = 1
       ELSEIF(KEY3(1:4)=='VONM')THEN
         ANIM_CE(7) = 1
       ELSEIF(KEY3(1:1)=='K')THEN
         ANIM_CE(8) = 1
       ELSEIF(KEY3(1:4)=='TVIS')THEN
         ANIM_CE(9) = 1
       ELSEIF(KEY3(1:5)=='VORTX')THEN
         ANIM_CE(10) = 1
       ELSEIF(KEY3(1:5)=='DAM1')THEN
         ANIM_CE(11) = 1
       ELSEIF(KEY3(1:5)=='DAM2')THEN
         ANIM_CE(12) = 1
       ELSEIF(KEY3(1:5)=='DAM3')THEN
         ANIM_CE(13) = 1
       ELSEIF(KEY3(1:5)=='SIGX ')THEN
         ANIM_CE(14) = 1
       ELSEIF(KEY3(1:5)=='SIGY ')THEN
         ANIM_CE(15) = 1
       ELSEIF(KEY3(1:5)=='SIGZ ')THEN
         ANIM_CE(16) = 1
       ELSEIF(KEY3(1:5)=='SIGXY'.OR.KEY3(1:5)=='SIGYX')THEN
         ANIM_CE(17) = 1
       ELSEIF(KEY3(1:5)=='SIGYZ'.OR.KEY3(1:5)=='SIGZY')THEN
         ANIM_CE(18) = 1
       ELSEIF(KEY3(1:5)=='SIGZX'.OR.KEY3(1:5)=='SIGXZ')THEN
         ANIM_CE(19) = 1
       ELSEIF(KEY3(1:5)=='USR1 '.OR.KEY3(1:5)=='USER1')THEN
         ANIM_CE(20) = 1
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(39+N1)=1
          ANIM_CE(20) = 0
         ENDIF
       ELSEIF(KEY3(1:5)=='USR2 '.OR.KEY3(1:5)=='USER2')THEN
         ANIM_CE(21) = 1
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(139+N1)=1
          ANIM_CE(21) = 0
         ENDIF
       ELSEIF(KEY3(1:5)=='USR3 '.OR.KEY3(1:5)=='USER3')THEN
         ANIM_CE(22) = 1
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(239+N1)=1
          ANIM_CE(22) = 0
         ENDIF
       ELSEIF(KEY3(1:5)=='USR4 '.OR.KEY3(1:5)=='USER4')THEN
         ANIM_CE(23) = 1
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(339+N1)=1
          ANIM_CE(23) = 0
         ENDIF
       ELSEIF(KEY3(1:5)=='USR5 '.OR.KEY3(1:5)=='USER5')THEN
         ANIM_CE(24) = 1
         READ(KEY4,'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(439+N1)=1
          ANIM_CE(24) = 0
         ENDIF
       ELSEIF(KEY3(1:4)=='HOUR')THEN
         ANIM_CE(25) = 1
       ELSEIF    (KEY3(1:4)=='EPSD')THEN
         ANIM_CE(26) = 1
      ELSEIF(KEY3(1:5)=='USR6 '.OR.KEY3(1:5)=='USER6')THEN
         ANIM_CE(27) = 1
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(539+N1)=1
          ANIM_CE(27) = 0
         ENDIF
      ELSEIF(KEY3(1:5)=='USR7 '.OR.KEY3(1:5)=='USER7')THEN
         ANIM_CE(28) = 1
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(639+N1)=1
          ANIM_CE(28) = 0
         ENDIF
      ELSEIF(KEY3(1:5)=='USR8 '.OR.KEY3(1:5)=='USER8')THEN
         ANIM_CE(29) = 1
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(739+N1)=1
          ANIM_CE(29) = 0
         ENDIF
      ELSEIF(KEY3(1:5)=='USR9 '.OR.KEY3(1:5)=='USER9')THEN
         ANIM_CE(30) = 1
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(839+N1)=1
          ANIM_CE(30) = 0
         ENDIF
      ELSEIF(KEY3(1:5)=='USR10')THEN
         ANIM_CE(31) = 1
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(939+N1)=1
          ANIM_CE(31) = 0
         ENDIF
      ELSEIF(KEY3(1:5)=='USR11')THEN
         ANIM_CE(32) = 1
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(1039+N1)=1
          ANIM_CE(32) = 0
         ENDIF
      ELSEIF(KEY3(1:5)=='USR12')THEN
         ANIM_CE(33) = 1
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(1139+N1)=1
          ANIM_CE(33) = 0
         ENDIF
      ELSEIF(KEY3(1:5)=='USR13')THEN
         ANIM_CE(34) = 1
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(1239+N1)=1
          ANIM_CE(34) = 0
         ENDIF
      ELSEIF(KEY3(1:5)=='USR14')THEN
         ANIM_CE(35) = 1
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(1339+N1)=1
          ANIM_CE(35) = 0
         ENDIF
      ELSEIF(KEY3(1:5)=='USR15')THEN
         ANIM_CE(36) = 1
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(1439+N1)=1
          ANIM_CE(36) = 0
         ENDIF
      ELSEIF(KEY3(1:5)=='USR16')THEN
         ANIM_CE(37) = 1
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(1539+N1)=1
          ANIM_CE(37) = 0
         ENDIF
      ELSEIF(KEY3(1:5)=='USR17')THEN
         ANIM_CE(38) = 1
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(1639+N1)=1
          ANIM_CE(38) = 0
         ENDIF
      ELSEIF(KEY3(1:5)=='USR18')THEN
         ANIM_CE(39) = 1
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(1739+N1)=1
          ANIM_CE(39)=0
         ENDIF
      ELSEIF(KEY3(1:5)=='USR19')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(1839+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR20')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(1939+N1)=1
         ENDIF
       ELSEIF(KEY3(1:4)=='FAIL')THEN
         ANIM_CE(2142) = 1
         READ(KEY4(1:5),'(I5)')IFAILA
         IF(IFAILA == 0) IFAILA = 1
       ELSEIF(KEY3(1:5)=='CRMAS')THEN
         ANIM_CE(2143) = 1
       ELSEIF(KEY3(1:5)=='CRVEL')THEN
         ANIM_CE(2144) = 1
       ELSEIF(KEY3(1:4)=='THIN')THEN
         ANIM_CE(2155) = 1
       ELSEIF(KEY3(1:5)=='ERROR')THEN
         IF(KEY4(1:5)=='THICK')THEN
           ANIM_CE(2156) = 1
         ELSE
         ENDIF
      ELSEIF(KEY3(1:5)=='USR21')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(2239+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR22')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(2339+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR23')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(2439+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR24')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(2539+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR25')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(2639+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR26')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(2739+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR27')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(2839+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR28')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(2939+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR29')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(3039+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR30')THEN
         READ(KEY4,'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(3139+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR31')THEN
         READ(KEY4,'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(3239+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR32')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(3339+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR33')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(3439+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR34')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(3539+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR35')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(3639+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR36')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(3739+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR37')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(3839+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR38')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(3939+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR39')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(4039+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR40')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(4139+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR41')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(4239+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR42')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(4339+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR43')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(4439+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR44')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(4539+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR45')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(4639+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR46')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(4739+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR47')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(4839+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR48')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(4939+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR49')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(5039+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR50')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(5139+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR51')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(5239+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR52')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(5339+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR53')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(5439+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR54')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(5539+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR55')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(5639+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR56')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(5739+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR57')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(5839+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR58')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(5939+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR59')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(6039+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR60')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(6139+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR61')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(6239+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR62')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(6339+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR63')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(6439+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR64')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(6539+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR65')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(6639+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR66')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(6739+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR67')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(6839+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR68')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(6939+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR69')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(7039+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR70')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(7139+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR71')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(7239+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR72')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(7339+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR73')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(7439+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR74')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(7539+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR75')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(7639+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR76')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(7739+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR77')THEN
         READ(KEY4,'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(7839+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR78')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(7939+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR79')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(8039+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR80')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(8139+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR81')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(8239+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR82')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(8339+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR83')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(8439+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR84')THEN
         READ(KEY4,'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(8539+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR85')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(8639+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR86')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(8739+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR87')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(8839+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR88')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(8939+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR89')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(9039+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR90')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(9139+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR91')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(9239+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR92')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(9339+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR93')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(9439+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR94')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(9539+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR95')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(9639+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR96')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(9739+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR97')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(9839+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR98')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(9939+N1)=1
         ENDIF
      ELSEIF(KEY3(1:5)=='USR99')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF(N1/=0)THEN
          ANIM_CE(10039+N1)=1
         ENDIF
      ELSEIF(KEY3(1:3)=='PHI')THEN
         IF(KEY4(1:3)=='ALL')THEN
           IPHIALL = 1
         ELSEIF(KEY4(1:4)=='MEMB')THEN
           ANIM_CE(10673) = 1
         ELSEIF(KEY4(1:5)=='UPPER')THEN
           ANIM_CE(10674) = 1
         ELSEIF(KEY4(1:5)=='LOWER')THEN
           ANIM_CE(10675) = 1
         ELSE
           READ(KEY4(1:5),'(I5)')N1
           IF(N1/=0)THEN
            ANIM_CE(10139+N1)=1
           ENDIF
         ENDIF
      ELSEIF(KEY3(1:4)=='DAMA')THEN
        N1 = 0
        N2 = 0
        IDX = 11925 + 4*MX_PLY_ANIM + 6
        ! in addition DAMA for PID_51 and PID_52 - shell composite -
        IF(KEY4(1:5)=='UPPER')THEN
          ANIM_CE(10257)=1
        ELSEIF(KEY4(1:5)=='LOWER')THEN
          ANIM_CE(10258)=1
        ELSEIF(KEY4(1:4)=='MEMB')THEN
          ANIM_CE(10259)=1
        ELSEIF(KEY4(1:3)=='ALL')THEN
          IDAMAALL = 1 ! mean layer value
          IF(KEY5(1:3)=='ALL') IDAMAFULL =  2 ! all NPTT
        ELSE
          READ(KEY4(1:5),'(I5)')N1   !  layer
          IF (N1 > 0 .AND. N1 <= 100) THEN
            !   case of PID_51, 52  (read NPTT, or, UPPER or LOWER, MEMB within NPTT)
            IF (KEY5(1:5) == 'UPPER') THEN
               ANIM_CE(IDX + N1) = 1
            ELSEIF (KEY5(1:5) == 'LOWER') THEN
               ANIM_CE(IDX + 100 + N1) = 1
            ELSEIF (KEY5(1:4) == 'MEMB') THEN
               ANIM_CE(IDX + 200 + N1) = 1
            ELSEIF(KEY5(1:3) == 'ALL')THEN
              IDAMAFULL = 1
              ANIM_DAMA(N1) = 1
            ELSE
              READ(KEY5(1:5),'(I5)')N2   !  NPTT (max=10) through layer N1 (max=100)
              IF (N2 > 0 .AND. N2 <= 10) THEN
                ANIM_CE(IDX + 300 + 10*N1 + N2) = 1 ! NPTT through N1
              ELSE
                ANIM_CE(10259+N1) = 1 ! classic layer out (mean value)
              ENDIF
            ENDIF
          ELSE
            ANIM_CE(10256) = 1
          ENDIF
        ENDIF ! IF(KEY4=='UPPER')THEN
      ELSEIF(KEY3(1:4)=='NXTF')THEN
         IF(KEY4(1:5)=='UPPER')THEN
           ANIM_CE(10360)=1
         ELSEIF(KEY4(1:5)=='LOWER')THEN
           ANIM_CE(10361)=1
         ELSEIF(KEY4(1:4)=='MEMB')THEN
           ANIM_CE(10362)=1
         ELSEIF(KEY4(1:3)=='ALL')THEN
           INXTFALL = 1
         ELSE
           READ(KEY4(1:5),'(I5)')N1
           IF (N1/=0) THEN
             ANIM_CE(10362+N1) = 1
           ENDIF
        ENDIF
      ELSEIF(KEY3(1:5)=='SIG1H')THEN
         IF(KEY4(1:5)=='UPPER')THEN
           ANIM_CE(10463)=1
         ELSEIF(KEY4(1:5)=='LOWER')THEN
           ANIM_CE(10464)=1
         ELSEIF(KEY4(1:4)=='MEMB')THEN
           ANIM_CE(10465)=1
         ELSEIF(KEY4(1:3)=='ALL')THEN
           SIGH1ALL = 1
         ELSE
           READ(KEY4(1:5),'(I5)')N1
           IF (N1/=0) THEN
             ANIM_CE(10465+N1) = 1
           ENDIF
        ENDIF
      ELSEIF(KEY3(1:5)=='SIG2H')THEN
         IF(KEY4(1:5)=='UPPER')THEN
           ANIM_CE(10566)=1
         ELSEIF(KEY4(1:5)=='LOWER')THEN
           ANIM_CE(10567)=1
         ELSEIF(KEY4(1:4)=='MEMB')THEN
           ANIM_CE(10568)=1
         ELSEIF(KEY4(1:3)=='ALL')THEN
           SIGH2ALL = 1
         ELSE
           READ(KEY4(1:5),'(I5)')N1
           IF (N1/=0) THEN
             ANIM_CE(10568+N1) = 1
           ENDIF
        ENDIF
      ELSEIF(KEY3(1:5)=='TDEL')THEN
         ANIM_CE(10670) = 1
      ELSEIF(KEY3(1:3)=='SSP')THEN
         ANIM_CE(10671) = 1     
      ELSEIF(KEY3(1:5)=='SCHLI')THEN !.OR. 'SCHLIEREN'
         ANIM_CE(10672) = 1        
      ELSEIF(KEY3(1:5)=='DOMAI')THEN 
         ANIM_CE(10676) = 1    
      !C   element equivalent stress criteria
      ELSEIF(KEY3(1:5)=='SIGEQ')THEN
         ANIM_CE(10677) = 1
      ELSEIF(KEY3(1:4)=='BULK' .OR. KEY3(1:4)=='QVIS')THEN
         ANIM_CE(11888) = 1   !PID15 uses ANIM_CE(10678:11887)         
      ELSEIF(KEY3(1:4)=='TDET')THEN 
         ANIM_CE(11889) = 1            
      ELSEIF(KEY3(1:5)=='LAW20')THEN
         ANIM_CE(11890) = 1 !dens
         ANIM_CE(11891) = 1
         ANIM_CE(11892) = 0
         ANIM_CE(11893) = 0
         ANIM_CE(11894) = 1 !ener
         ANIM_CE(11895) = 1
         ANIM_CE(11896) = 0
         ANIM_CE(11897) = 0
         ANIM_CE(11898) = 1 !temp
         ANIM_CE(11899) = 1
         ANIM_CE(11900) = 0
         ANIM_CE(11901) = 0
         ANIM_CE(11902) = 1 !pres
         ANIM_CE(11903) = 1
         ANIM_CE(11904) = 0
         ANIM_CE(11905) = 0
         ANIM_CE(11906) = 1 !plas
         ANIM_CE(11907) = 1
         ANIM_CE(11908) = 0
         ANIM_CE(11909) = 0                                                                                                                                                                                                                 
         ANIM_CE(11910) = 1 !ssp           
         ANIM_CE(11911) = 1  
         ANIM_CE(11912) = 0  
         ANIM_CE(11913) = 0  
         ANIM_CE(11914) = 1 !volume 
         ANIM_CE(11915) = 1  
         ANIM_CE(11916) = 0  
         ANIM_CE(11917) = 0  
         ANIM_CE(11918) = 1 !mass 
         ANIM_CE(11919) = 1  
         ANIM_CE(11920) = 0  
         ANIM_CE(11921) = 0  
         ANIM_CE(11922) = 1 !Qvis  
         ANIM_CE(11923) = 1  
         ANIM_CE(11924) = 0 
         ANIM_CE(11925) = 0

      ELSEIF(KEY3(1:5)=='IDPLY')THEN       
        !ANIM_CE(11925) ->  ANIM_CE(11925+3*MX_PLY_ANIM)
        !ANIM_CT(1610) ->  ANIM_CT(1610+2*MX_PLY_ANIM)
         ! === /ANIM/SHELL/IDPLY/STRESS ===
        IF(KEY4(1:5)=='STRES')THEN
          IF(KEY5(1:3) == 'ALL')THEN
            ISTRESALL_PLY = 1
          ELSE
            READ(KEY5(1:5),'(I5)')N1
            READ(KEY6(1:5),'(I5)')IPT
            NBPLY_ANIM_STRESS = NBPLY_ANIM_STRESS + 1
            PLY_ANIM_STRESS(3*(NBPLY_ANIM_STRESS-1)+2) = 2
            PLY_ANIM_STRESS(3*(NBPLY_ANIM_STRESS-1)+1) = N1
            PLY_ANIM_STRESS(3*(NBPLY_ANIM_STRESS-1)+3) = IPT
            ANIM_CT(1610 + NBPLY_ANIM_STRESS) = 1
            IF(NBPLY_ANIM_STRESS > MX_PLY_ANIM) 
     .        CALL ANCMSG(MSGID=268,MSGTYPE=MSGWARNING,I1=MX_PLY_ANIM,ANMODE=ANINFO)
          ENDIF  
        ! /ANIM/SHELL/IDPLY/STRAIN
        ELSEIF(KEY4(1:5)=='STRAI')THEN
          IF(KEY5(1:3) == 'ALL')THEN
            ISTRAINALL_PLY = 1
          ELSE
            READ(KEY5(1:5),'(I5)')N1
            READ(KEY6(1:5),'(I5)')IPT
            NBPLY_ANIM_STRAIN = NBPLY_ANIM_STRAIN + 1
            PLY_ANIM_STRAIN(3*(NBPLY_ANIM_STRAIN-1)+1) = N1
            PLY_ANIM_STRAIN(3*(NBPLY_ANIM_STRAIN-1)+2) = 3
            PLY_ANIM_STRAIN(3*(NBPLY_ANIM_STRAIN-1)+3) = IPT
            ANIM_CT( (1610+ MX_PLY_ANIM) + NBPLY_ANIM_STRAIN) = 1
            IF(NBPLY_ANIM_STRAIN > MX_PLY_ANIM) 
     .        CALL ANCMSG(MSGID=268,MSGTYPE=MSGWARNING,I1=MX_PLY_ANIM,ANMODE=ANINFO)
          ENDIF
        ! /ANIM/SHELL/IDPLY/EPSDOT
        ELSEIF(KEY4(1:5)=='EPSDO')THEN
          IEPSDOT = 1
          IF(KEY5(1:3) == 'ALL')THEN
            IEPSDOTALL_PLY = 1
          ELSE
            READ(KEY5(1:5),'(I5)')N1
            READ(KEY6(1:5),'(I5)')IPT
            NBPLY_ANIM_EPSDOT = NBPLY_ANIM_EPSDOT + 1
            PLY_ANIM_EPSDOT(3*(NBPLY_ANIM_EPSDOT-1)+1) = N1
            PLY_ANIM_EPSDOT(3*(NBPLY_ANIM_EPSDOT-1)+2) = 6
            PLY_ANIM_EPSDOT(3*(NBPLY_ANIM_EPSDOT-1)+3) = IPT
            ANIM_CT( (1610+ 2*MX_PLY_ANIM) + NBPLY_ANIM_EPSDOT) = 1
            IF(NBPLY_ANIM_EPSDOT > MX_PLY_ANIM) 
     .        CALL ANCMSG(MSGID=268,MSGTYPE=MSGWARNING,I1=MX_PLY_ANIM,ANMODE=ANINFO)
          ENDIF
        ELSEIF(KEY4(1:3)=='PHI')THEN
          IF(KEY5(1:3) == 'ALL')THEN
            ! /ANIM/SHELL/IDPLY/PHI/ALL
            IPHIALL_PLY = 1
          ELSE
            ! /ANIM/SHELL/IDPLY/PHI/Id
            READ(KEY5(1:5),'(I5)')N1
            NBPLY_ANIM_PHI = NBPLY_ANIM_PHI + 1
            IF(NBPLY_ANIM_PHI > MX_PLY_ANIM) 
     .        CALL ANCMSG(MSGID=268,MSGTYPE=MSGWARNING,I1=MX_PLY_ANIM,ANMODE=ANINFO)
            PLY_ANIM_PHI(3*(NBPLY_ANIM_PHI-1)+1) = N1
            PLY_ANIM_PHI(3*(NBPLY_ANIM_PHI-1)+2) = 4
            PLY_ANIM_PHI(3*(NBPLY_ANIM_PHI-1)+3) = 0
            ANIM_CE( (11925 + MX_PLY_ANIM) + NBPLY_ANIM_PHI ) = 1  
          ENDIF
        ELSEIF(KEY4(1:4)=='EPSP')THEN
          IF(KEY5(1:3) == 'ALL')THEN
            ! /ANIM/SHELL/IDPLY/EPSP/ALL
            IEPSPALL_PLY = 1
          ELSE
            ! /ANIM/SHELL/IDPLY/EPSP/Id_ply_id_point
            READ(KEY5(1:5),'(I5)')N1
            READ(KEY6(1:5),'(I5)')IPT
            NBPLY_ANIM_EPSP = NBPLY_ANIM_EPSP + 1
            IF(NBPLY_ANIM_EPSP > MX_PLY_ANIM) 
     .        CALL ANCMSG(MSGID=268,MSGTYPE=MSGWARNING,I1=MX_PLY_ANIM,ANMODE=ANINFO)
            PLY_ANIM_EPSP(3*(NBPLY_ANIM_EPSP-1)+1) = N1
            PLY_ANIM_EPSP(3*(NBPLY_ANIM_EPSP-1)+2) = 5
            PLY_ANIM_EPSP(3*(NBPLY_ANIM_EPSP-1)+3) = IPT
            ANIM_CE( (11925 + 2 * MX_PLY_ANIM) + NBPLY_ANIM_EPSP ) = 1  
          ENDIF
        ELSEIF(KEY4(1:4)=='DAMA')THEN
          IF(KEY5(1:3) == 'ALL')THEN
            ! /ANIM/SHELL/IDPLY/DAMA/ALL
            IDAMAALL_PLY = 1
          ELSE
            ! /ANIM/SHELL/IDPLY/DAMA/Id_ply_id_point
            READ(KEY5(1:5),'(I5)')N1
            READ(KEY6(1:5),'(I5)')IPT
            NBPLY_ANIM_DAMA = NBPLY_ANIM_DAMA + 1
            IF(NBPLY_ANIM_DAMA > MX_PLY_ANIM) 
     .        CALL ANCMSG(MSGID=268,MSGTYPE=MSGWARNING,I1=MX_PLY_ANIM,ANMODE=ANINFO)
            PLY_ANIM_DAMA(3*(NBPLY_ANIM_DAMA-1)+1) = N1
            PLY_ANIM_DAMA(3*(NBPLY_ANIM_DAMA-1)+2) = 7
            PLY_ANIM_DAMA(3*(NBPLY_ANIM_DAMA-1)+3) = IPT
            ANIM_CE( (11925 + 3 * MX_PLY_ANIM) + NBPLY_ANIM_DAMA ) = 1  
          ENDIF
        ELSEIF(KEY4(1:3)=='ALL')THEN
          ! /ANIM/SHELL/IDPLY/ALL
          IPLYALL = 1
        ELSE
          ! /ANIM/SHELL/IDPLY/Id
          READ(KEY4(1:5),'(I5)')N1
          NBPLY_ANIM = NBPLY_ANIM + 1
          IF(NBPLY_ANIM > MX_PLY_ANIM) 
     .      CALL ANCMSG(MSGID=268,MSGTYPE=MSGWARNING,I1=MX_PLY_ANIM,ANMODE=ANINFO)
          PLY_ANIM(3*(NBPLY_ANIM-1)+1) = N1
          PLY_ANIM(3*(NBPLY_ANIM-1)+2) = 1
          PLY_ANIM(3*(NBPLY_ANIM-1)+3) = 0
          ANIM_CE(11925 + NBPLY_ANIM) = 1  
        ENDIF  
      ELSEIF(KEY3(1:4) == 'FLDF')THEN
         IDX = 11925+4*MX_PLY_ANIM
         IF (KEY4(1:5) == 'UPPER') THEN
           ANIM_CE(IDX+1)=1
         ELSEIF (KEY4(1:5) == 'LOWER') THEN
           ANIM_CE(IDX+2)=1
         ELSEIF (KEY4(1:4) == 'MEMB') THEN
           ANIM_CE(IDX+3)=1
        ENDIF
      ELSEIF(KEY3(1:4) == 'FLDZ')THEN
         IDX = 11925+4*MX_PLY_ANIM + 3
         IF (KEY4(1:5) == 'UPPER') THEN
           ANIM_CE(IDX+1)=1
         ELSEIF (KEY4(1:5) == 'LOWER') THEN
           ANIM_CE(IDX+2)=1
         ELSEIF (KEY4(1:4) == 'MEMB') THEN
           ANIM_CE(IDX+3)=1
        ENDIF
      ELSEIF(KEY3(1:5) == 'DT   ')THEN
        IDX          = 13242 + 4*MX_PLY_ANIM
        ANIM_CE(IDX) = 1
      ELSEIF(KEY3(1:3) == 'AMS')THEN
        IDX          = 13242 + 4*MX_PLY_ANIM + 1
        ANIM_CE(IDX) = 1    
!NIM/SHELL/WPLA/KEY4/KEY5
      ELSEIF(KEY3(1:4) == 'WPLA')THEN
         N1  = 0
         N2  = 0
         IDX = 13245 + 4*MX_PLY_ANIM 
         IF(KEY4(1:5)=='UPPER')THEN
           ANIM_CE(IDX + 1)=1
         ELSEIF(KEY4(1:5)=='LOWER')THEN
           ANIM_CE(IDX + 2)=1
         ELSEIF(KEY4(1:5)=='ALL')THEN
           IWPLAALL = 1 ! mean layer value
           IF(KEY5(1:3)=='ALL') IWPLAFULL =  2 ! all NPTT
         ELSE
           READ(KEY4(1:5),'(I5)')N1   !  layer
             IF (N1 > 0 .AND. N1 <= 100) THEN
             ! case of PID51  (read NPTT, or, UPPER or LOWER within NPTT)
               IDX = 13347 + 4*MX_PLY_ANIM 
               IF(KEY5(1:5)=='UPPER')THEN
                  ANIM_CE(IDX  + N1) = 1
               ELSEIF(KEY5(1:5)=='LOWER')THEN
                  ANIM_CE(IDX + 100 + N1) = 1
               ELSEIF(KEY5(1:3)=='ALL')THEN
                  IWPLAFULL = 1
                  ANIM_WPLA(N1) = 1
               ELSE
                  IDX = 13547 + 4*MX_PLY_ANIM 
                  READ(KEY5(1:5),'(I5)')N2   !  NPTT (max=10) through layer N1 (max=100)
                  IF (N2 > 0 .AND. N2 <= 10) THEN
                    ANIM_CE(IDX + 10*(N1-1) + N2) = 1 ! NPTT through N1
                  ELSE
                    IDX = 13247 + 4*MX_PLY_ANIM !!! N1 <= 100
                    ANIM_CE(IDX + N1) = 1 ! classic layer out (mean value)
                  ENDIF
               ENDIF
             ELSE
               IDX = 13245 + 4*MX_PLY_ANIM
               ANIM_CE(IDX) = 1 
             ENDIF 
           ENDIF   
!  IDX =   13547 + 4*MX_PLY_ANIM + 10*100 limit
      ELSEIF(KEY3(1:3) == 'OFF')THEN
        IDX = 13547 + 4*MX_PLY_ANIM + 1000 + 1
        ANIM_CE(IDX) = 1
      ELSEIF(KEY3(1:4) == 'MACH')THEN
        IDX = 13547 + 4*MX_PLY_ANIM + 1000 + 2
        ANIM_CE(IDX) = 1
      ELSEIF(KEY3(1:5) == 'COLOR')THEN
        IDX = 13547 + 4*MX_PLY_ANIM + 1000 + 3
        ANIM_CE(IDX) = 1
      ELSEIF(KEY3(1:4) == 'DAMG') THEN 
        IDX = 13547 + 4*MX_PLY_ANIM + 1000 + 4
        N1 = 0
        IF(KEY4(1:4)=='MEAN')THEN
          ANIM_CE(IDX) = 1  
        ELSEIF(KEY4(1:5)=='UPPER')THEN
          ANIM_CE(IDX+1)=1
        ELSEIF(KEY4(1:5)=='LOWER')THEN
          ANIM_CE(IDX+2)=1
        ELSEIF(KEY4(1:4)=='MEMB')THEN
          ANIM_CE(IDX+3)=1
        ELSE
          READ(KEY4(1:5),'(I5)')N1   ! NPTT
          IF (N1 > 0 .AND. N1 <= 11) THEN
            ANIM_CE(IDX+3+N1)=1
          ELSE
            ANIM_CE(IDX) = 1  
          ENDIF
        ENDIF
      ELSEIF(KEY3(1:7)=='NL_EPSP')THEN
        IDX = 4*MX_PLY_ANIM + 14567
        N1  = 0
        IF(KEY4(1:5)=='UPPER')THEN
          ANIM_CE(IDX+1)=1
        ELSEIF(KEY4(1:5)=='LOWER')THEN
          ANIM_CE(IDX+2)=1
        ELSEIF(KEY4(1:3)=='ALL')THEN
          IEPSPNLALL = 1
        ELSE
          READ(KEY4(1:5),'(I5)') N1
          IF (N1 > 0 .AND. N1 <= 11) THEN
            ANIM_CE(IDX+2+N1) = 1 
          ELSE
            ANIM_CE(IDX) = 1
          ENDIF
        ENDIF
      ELSEIF(KEY3(1:7)=='NL_EPSD')THEN
        IDX = 4*MX_PLY_ANIM + 14581
        N1  = 0
        IF(KEY4(1:5)=='UPPER')THEN
          ANIM_CE(IDX+1)=1
        ELSEIF(KEY4(1:5)=='LOWER')THEN
          ANIM_CE(IDX+2)=1
        ELSEIF(KEY4(1:3)=='ALL')THEN
          IEPSDNLALL = 1
        ELSE
          READ(KEY4(1:5),'(I5)') N1
          IF (N1 > 0 .AND. N1 <= 11) THEN
            ANIM_CE(IDX+2+N1) = 1 
          ELSE
            ANIM_CE(IDX) = 1
          ENDIF
        ENDIF
!ANIM/SHELL/TSAIWU/KEY4/KEY5
      ELSEIF (KEY3(1:6) == 'TSAIWU') THEN
         N1  = 0
         N2  = 0
         IDX = 14595 + 4*MX_PLY_ANIM
         IF(KEY4(1:5)=='UPPER')THEN
           ANIM_CE(IDX + 1)=1
         ELSEIF(KEY4(1:5)=='LOWER')THEN
           ANIM_CE(IDX + 2)=1
         ELSEIF(KEY4(1:5)=='ALL')THEN
           ITSAIWUALL = 1 ! mean layer value
           IF(KEY5(1:3)=='ALL') ITSAIWUALL = 2 ! all NPTT
         ELSE
           READ(KEY4(1:5),'(I5)') N1   !  layer
           IF (N1 > 0 .AND. N1 <= 100) THEN
             ! case of PID51  (read NPTT, or, UPPER or LOWER within NPTT)
             IDX = 14697 + 4*MX_PLY_ANIM
             IF (KEY5(1:5)=='UPPER') THEN
               ANIM_CE(IDX + N1) = 1
             ELSEIF(KEY5(1:5)=='LOWER') THEN
               ANIM_CE(IDX + 100 + N1) = 1
             ELSEIF(KEY5(1:3)=='ALL')THEN
               ITSAIWUFULL = N1
             ELSE
               IDX = 14897 + 4*MX_PLY_ANIM
               READ(KEY5(1:5),'(I5)') N2             ! NPTT (max=10) through layer N1 (max=100)
               IF (N2 > 0 .AND. N2 <= 10) THEN
                 ANIM_CE(IDX + 10*(N1-1) + N2) = 1   ! NPTT through N1
               ELSE
                 IDX = 14597 + 4*MX_PLY_ANIM         ! N1 <= 100
                 ANIM_CE(IDX + N1) = 1               ! classic layer out (mean value)
               ENDIF
             ENDIF
           ELSE
             IDX = 14595 + 4*MX_PLY_ANIM 
             ANIM_CE(IDX) = 1 
           ENDIF 
         ENDIF 
      ELSEIF(KEY3(1:9) == 'TILLOTSON')THEN
        IDX = 14596 + 4*MX_PLY_ANIM
        ANIM_CE(IDX) = 1           
      ELSE
        IXITKEY=IXITKEY+1
      ENDIF
C--------------------------
C    BRIQUES
C--------------------------
      ELSEIF(KEY2(1:4)=='BRIC')THEN
C
      FMT_ANI = 4
C--------------------------
C     BRIQUES:TENSEUR
C--------------------------
      IF(KEY3(1:4)=='TENS')THEN !ANIM/BRICK/TENS
        IF(KEY4(1:5)=='STRES')THEN
          IF(KEY5(1:3) == 'ALL')THEN
            IBRICK_STRESSALL = 1
          ELSE
            READ(KEY5(1:5),'(I5)')N1
            IF(N1/=0)THEN
              IF ( MOD(ABS(N1)/10,10) == 0) THEN
                READ(KEY6(1:5),'(I5)')N2
                  ANIM_ST(2010+MOD(ABS(N1),100)+
     .                 N2*10+(ABS(N1)/100)*2010)=1
              ELSE
                ANIM_ST(10+N1)=1
              ENDIF
            ELSE
              ANIM_ST(1) = 1 
            ENDIF
          ENDIF
        ELSEIF(KEY4(1:5)=='STRAI')THEN
          IF(KEY5(1:3) == 'ALL')THEN
            IBRICK_STRAINALL = 1
          ELSE
            READ(KEY5(1:5),'(I5)')N1
            IF(N1/=0)THEN
              IF ( MOD(ABS(N1)/10,10) == 0) THEN
                READ(KEY6(1:5),'(I5)')N2
                ANIM_ST(22110+MOD(ABS(N1),100)+
     .                 N2*10+(ABS(N1)/100)*2010)=1
              ELSE
                ANIM_ST(1010+N1)=1
              ENDIF
            ELSE
              ANIM_ST(2) = 1
            ENDIF
          ENDIF
        ELSEIF(KEY4(1:5)=='EPSDO')THEN
c           IEPSDOT = 1
c           ANIM_ST(3) = 1
        ELSEIF(KEY4(1:4)=='DAMA')THEN
           ANIM_ST(4) = 1
        ELSEIF(KEY4(1:4)=='EPSP')THEN
          IF(KEY5(1:3) == 'ALL')THEN
            IBRICK_EPSPALL = 1
          ELSE
            READ(KEY5(1:5),'(I5)')N1
            IF(N1/=0)THEN
              IF ( MOD(ABS(N1)/10,10) == 0) THEN
                READ(KEY6(1:5),'(I5)')N2
                ANIM_ST(43210+MOD(ABS(N1),100)+
     .                 N2*10+(ABS(N1)/100)*2010)=1
              ELSE
                ANIM_ST(42210+N1)=1
              ENDIF
            ELSE
              ANIM_ST(5) = 1
            ENDIF
          ENDIF
        ELSE
          IXITKEY=IXITKEY+1
        ENDIF
      ELSE IF (KEY3(1:5) == 'LAW51') THEN
        IF (KEY4(1:3) == 'ALL') THEN
           LAW51_PHASE1 = .TRUE.
           LAW51_PHASE2 = .TRUE.
           LAW51_PHASE3 = .TRUE.
           LAW51_PHASE4 = .TRUE.
        ELSE IF (KEY4(1:1) == '1') THEN
           LAW51_PHASE1 = .TRUE.
        ELSE IF (KEY4(1:1) == '2') THEN
           LAW51_PHASE2 = .TRUE.
        ELSE IF (KEY4(1:1) == '3') THEN
           LAW51_PHASE3 = .TRUE.
        ELSE IF (KEY4(1:1) == '4') THEN
           LAW51_PHASE4 = .TRUE.
        ENDIF
C--------------------------
C     BRIQUES:SCALAIRES
C--------------------------
      ELSEIF(KEY3(1:4)=='EPSP')THEN
         ANIM_SE(1) = 1
         IF (LAW51_PHASE1) ANIM_SE(4913) = 1 !EPSP_law51-phase1
         IF (LAW51_PHASE2)   ANIM_SE(4914) = 1  !EPSP_law51-phase2
         IF (LAW51_PHASE3)  ANIM_SE(4915) = 1  !EPSP_law51-phase3
         IF (LAW51_PHASE4)  ANIM_SE(4916) = 1  !EPSP_law51-phase4           
      ELSEIF(KEY3(1:5)=='PLA51')THEN !obsolete      
         ANIM_SE(1) = 1
      ELSEIF(KEY3(1:5)=='DENS3')THEN
           !/ANIM/ELEM/DENS37          
           ANIM_SE(4935) = 1  !law37 -rho_liquid 
           ANIM_SE(4936) = 1  !law37 - rho_gas 
      ELSEIF(KEY3(1:4)=='DENS')THEN
         ANIM_SE(2)    = 1
         IF (LAW51_PHASE1) ANIM_SE(4897) = 1 !DENS_law51-phase1
         IF (LAW51_PHASE2) ANIM_SE(4898) = 1 !DENS_law51-phase2
         IF (LAW51_PHASE3) ANIM_SE(4899) = 1 !DENS_law51-phase3
         IF (LAW51_PHASE4) ANIM_SE(4900) = 1 !DENS_law51-phase4             
           !ANIM_SE(4935) = 1  !law37 -rho_liquid
           !ANIM_SE(4936) = 1  !law37 - rho_gas             
      ELSEIF(KEY3(1:4)=='ENER')THEN
         ANIM_SE(3) = 1
         IF (LAW51_PHASE1) ANIM_SE(4901) = 1 !ENER_law51-phase1
         IF (LAW51_PHASE2) ANIM_SE(4902) = 1  !ENER_law51-phase2
         IF (LAW51_PHASE3) ANIM_SE(4903) = 1  !ENER_law51-phase3
         IF (LAW51_PHASE4) ANIM_SE(4904) = 1  !ENER_law51-phase4
      ELSEIF(KEY3(1:4)=='TEMP')THEN
         ANIM_SE(4) = 1
         IF (LAW51_PHASE1) ANIM_SE(4905) = 1 !TEMP_law51-phase1
         IF (LAW51_PHASE2) ANIM_SE(4906) = 1 !TEMP_law51-phase2
         IF (LAW51_PHASE3) ANIM_SE(4907) = 1 !TEMP_law51-phase3
         IF (LAW51_PHASE4) ANIM_SE(4908) = 1 !TEMP_law51-phase4
      ELSEIF(KEY3(1:5)=='TEM51')THEN
         ANIM_SE(4) = 1
      ELSEIF(KEY3(1:1)=='P'.AND.KEY3(1:5)/='PLA51')THEN
         ANIM_SE(6) = 1
         IF (LAW51_PHASE1) ANIM_SE(4909) = 1 !PRES_law51-phase1
         IF (LAW51_PHASE2) ANIM_SE(4910) = 1 !PRES_law51-phase2
         IF (LAW51_PHASE3) ANIM_SE(4911) = 1 !PRES_law51-phase3
         IF (LAW51_PHASE4) ANIM_SE(4912) = 1 !PRES_law51-phase4           
      ELSEIF(KEY3(1:4)=='VONM')THEN
         ANIM_SE(7) = 1
      ELSEIF(KEY3(1:1)=='K')THEN
         ANIM_SE(8) = 1
      ELSEIF(KEY3(1:4)=='TVIS')THEN
         ANIM_SE(9) = 1
      ELSEIF(KEY3(1:5)=='VORTX')THEN
         ANIM_SE(10) = 1
      ELSEIF(KEY3(1:4)=='DAM1')THEN
         ANIM_SE(11) = 1
      ELSEIF(KEY3(1:4)=='DAM2')THEN
         ANIM_SE(12) = 1
      ELSEIF(KEY3(1:4)=='DAM3')THEN
         ANIM_SE(13) = 1
      ELSEIF(KEY3(1:5)=='SIGX ')THEN
         ANIM_SE(14) = 1
      ELSEIF(KEY3(1:5)=='SIGY ')THEN
         ANIM_SE(15) = 1
      ELSEIF(KEY3(1:5)=='SIGZ ')THEN
         ANIM_SE(16) = 1
      ELSEIF(KEY3(1:5)=='SIGXY'.OR.KEY3(1:5)=='SIGYX')THEN
         ANIM_SE(17) = 1
      ELSEIF(KEY3(1:5)=='SIGYZ'.OR.KEY3(1:5)=='SIGZY')THEN
         ANIM_SE(18) = 1
      ELSEIF(KEY3(1:5)=='SIGZX'.OR.KEY3(1:5)=='SIGXZ')THEN
         ANIM_SE(19) = 1
      ELSEIF(KEY3(1:5)=='USR1 '.OR.KEY3(1:5)=='USER1')THEN
         ANIM_SE(20) = 1
      ELSEIF(KEY3(1:5)=='USR2 '.OR.KEY3(1:5)=='USER2')THEN
         ANIM_SE(21) = 1
      ELSEIF(KEY3(1:5)=='USR3 '.OR.KEY3(1:5)=='USER3')THEN
         ANIM_SE(22) = 1
      ELSEIF(KEY3(1:5)=='USR4 '.OR.KEY3(1:5)=='USER4')THEN
         ANIM_SE(23) = 1
      ELSEIF(KEY3(1:5)=='USR5 '.OR.KEY3(1:5)=='USER5')THEN
         ANIM_SE(24) = 1
      ELSEIF(KEY3(1:4)=='HOUR')THEN
         ANIM_SE(25) = 1
      ELSEIF(KEY3(1:4)=='EPSD')THEN
         ANIM_SE(26) = 1
      ELSEIF(KEY3(1:5)=='USR6 '.OR.KEY3(1:5)=='USER6')THEN
         ANIM_SE(27) = 1
      ELSEIF(KEY3(1:5)=='USR7 '.OR.KEY3(1:5)=='USER7')THEN
         ANIM_SE(28) = 1
      ELSEIF(KEY3(1:5)=='USR8 '.OR.KEY3(1:5)=='USER8')THEN
         ANIM_SE(29) = 1
      ELSEIF(KEY3(1:5)=='USR9 '.OR.KEY3(1:5)=='USER9')THEN
         ANIM_SE(30) = 1
      ELSEIF(KEY3(1:5)=='USR10')THEN
         ANIM_SE(31) = 1
      ELSEIF(KEY3(1:5)=='USR11')THEN
         ANIM_SE(32) = 1
      ELSEIF(KEY3(1:5)=='USR12')THEN
         ANIM_SE(33) = 1
      ELSEIF(KEY3(1:5)=='USR13')THEN
         ANIM_SE(34) = 1
      ELSEIF(KEY3(1:5)=='USR14')THEN
         ANIM_SE(35) = 1
      ELSEIF(KEY3(1:5)=='USR15')THEN
         ANIM_SE(36) = 1
      ELSEIF(KEY3(1:5)=='USR16')THEN
         ANIM_SE(37) = 1
      ELSEIF(KEY3(1:5)=='USR17')THEN
         ANIM_SE(38) = 1
      ELSEIF(KEY3(1:5)=='USR18')THEN
         ANIM_SE(39) = 1
      ELSEIF(KEY3(1:5)=='USR19')THEN
         ANIM_SE(40) = 1
      ELSEIF(KEY3(1:5)=='USR20')THEN
         ANIM_SE(41) = 1
      ELSEIF(KEY3(1:5)=='USR21')THEN
         ANIM_SE(42) = 1
      ELSEIF(KEY3(1:5)=='USR22')THEN
         ANIM_SE(43) = 1
      ELSEIF(KEY3(1:5)=='USR23')THEN
         ANIM_SE(44) = 1
      ELSEIF(KEY3(1:5)=='USR24')THEN
         ANIM_SE(45) = 1
      ELSEIF(KEY3(1:5)=='USR25')THEN
         ANIM_SE(46) = 1
      ELSEIF(KEY3(1:5)=='USR26')THEN
         ANIM_SE(47) = 1
      ELSEIF(KEY3(1:5)=='USR27')THEN
         ANIM_SE(48) = 1
      ELSEIF(KEY3(1:5)=='USR28')THEN
         ANIM_SE(49) = 1
      ELSEIF(KEY3(1:5)=='USR29')THEN
         ANIM_SE(50) = 1
      ELSEIF(KEY3(1:5)=='USR30')THEN
         ANIM_SE(51) = 1
      ELSEIF(KEY3(1:5)=='USR31')THEN
         ANIM_SE(52) = 1
      ELSEIF(KEY3(1:5)=='USR32')THEN
         ANIM_SE(52) = 1
      ELSEIF(KEY3(1:5)=='USR33')THEN
         ANIM_SE(54) = 1
      ELSEIF(KEY3(1:5)=='USR34')THEN
         ANIM_SE(55) = 1
      ELSEIF(KEY3(1:5)=='USR35')THEN
         ANIM_SE(56) = 1
      ELSEIF(KEY3(1:5)=='USR36')THEN
         ANIM_SE(57) = 1
      ELSEIF(KEY3(1:5)=='USR37')THEN
         ANIM_SE(58) = 1
      ELSEIF(KEY3(1:5)=='USR38')THEN
         ANIM_SE(59) = 1
      ELSEIF(KEY3(1:5)=='USR39')THEN
         ANIM_SE(60) = 1
      ELSEIF(KEY3(1:5)=='USR40')THEN
         ANIM_SE(61) = 1
      ELSEIF(KEY3(1:5)=='USR41')THEN
         ANIM_SE(62) = 1
      ELSEIF(KEY3(1:5)=='USR42')THEN
         ANIM_SE(63) = 1
      ELSEIF(KEY3(1:5)=='USR43')THEN
         ANIM_SE(64) = 1
      ELSEIF(KEY3(1:5)=='USR44')THEN
         ANIM_SE(65) = 1
      ELSEIF(KEY3(1:5)=='USR45')THEN
         ANIM_SE(66) = 1
      ELSEIF(KEY3(1:5)=='USR46')THEN
         ANIM_SE(67) = 1
      ELSEIF(KEY3(1:5)=='USR47')THEN
         ANIM_SE(68) = 1
      ELSEIF(KEY3(1:5)=='USR48')THEN
         ANIM_SE(69) = 1
      ELSEIF(KEY3(1:5)=='USR49')THEN
         ANIM_SE(70) = 1
      ELSEIF(KEY3(1:5)=='USR50')THEN
         ANIM_SE(71) = 1
      ELSEIF(KEY3(1:5)=='USR51')THEN
         ANIM_SE(72) = 1
      ELSEIF(KEY3(1:5)=='USR52')THEN
         ANIM_SE(73) = 1
      ELSEIF(KEY3(1:5)=='USR53')THEN
         ANIM_SE(74) = 1
      ELSEIF(KEY3(1:5)=='USR54')THEN
         ANIM_SE(75) = 1
      ELSEIF(KEY3(1:5)=='USR55')THEN
         ANIM_SE(76) = 1
      ELSEIF(KEY3(1:5)=='USR56')THEN
         ANIM_SE(77) = 1
      ELSEIF(KEY3(1:5)=='USR57')THEN
         ANIM_SE(78) = 1
      ELSEIF(KEY3(1:5)=='USR58')THEN
         ANIM_SE(79) = 1
      ELSEIF(KEY3(1:5)=='USR59')THEN
         ANIM_SE(80) = 1
      ELSEIF(KEY3(1:5)=='USR60')THEN
         ANIM_SE(81) = 1                 
      ELSEIF(KEY3(1:4)=='WPLA')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF (N1 /= 0 ) THEN
           ANIM_SE(81+N1) = 1
         ELSE
           ANIM_SE(4964) = 1 
         ENDIF  
      ELSEIF(KEY3(1:4)=='FLAY')THEN
         ANIM_SE(282) = 1
      ELSEIF(KEY3(1:5)=='VFRAC')THEN
        !BIMAT(law20)
        !BIPHAS(law37)   : TRIMAT = -2
        !MULTIMAT(law51) : TRIMAT = 3|4
        ! law51 + law37  : TRIMAT = 3|4
        !consistency with material law checked in radioss2.F since TRIMAT common var not yet read
        ANIM_SE(283) = 1 !vfrac phase1 USR4 (law51 & 37 & 20)
        ANIM_SE(284) = 1 !vfrac phase2 USR4 (law51 & 37 & 20)
        ANIM_SE(285) = 1 !vfrac phase3 USR22 (law51)
        ANIM_SE(286) = 1 !vfrac phase4 USR48 (law51) 
      ELSEIF(KEY3(1:5)=='BFRAC' .OR. KEY3(1:4)=='BF51')THEN
         ANIM_SE(887) = 1
      ELSEIF(KEY3(1:5)=='ORTHD')THEN
         IF(KEY4=='ALL')THEN
           IORTHDALL = 1
         ELSE
           READ(KEY4(1:5),'(I5)')N1
           IF(N1/=0)THEN
            ANIM_SE(286+3*(N1-1)+1)=1
            ANIM_SE(286+3*(N1-1)+2)=1
            ANIM_SE(286+3*(N1-1)+3)=1
           ENDIF
         ENDIF
      ELSEIF(KEY3(1:5)=='VDAM1')THEN 
         READ(KEY4(1:5),'(I5)')N1 !/ijk
         IF(N1 > 0)THEN
            ANIM_SE(890+N1)=1
         ELSE
           ANIM_SE(888) = 1
         ENDIF
      ELSEIF(KEY3(1:5)=='VDAM2')THEN 
         READ(KEY4(1:5),'(I5)')N1 !/ijk
         IF(N1 > 0)THEN
            ANIM_SE(1890+N1)=1
         ELSE
           ANIM_SE(889) = 1
         ENDIF
      ELSEIF(KEY3(1:5)=='VDAM3')THEN 
         READ(KEY4(1:5),'(I5)')N1 !/ijk
         IF(N1 > 0)THEN
            ANIM_SE(2890+N1)=1
         ELSE
           ANIM_SE(890) = 1
         ENDIF
      ELSEIF(KEY3(1:4)=='DAMA')THEN
         READ(KEY4(1:5),'(I5)')N1 !/ijk
         IF(N1 > 0)THEN
           IF ( MOD(ABS(N1)/10,10) == 0) THEN
             READ(KEY5,'(I5)')N2
             ANIM_SE(3890+MOD(ABS(N1),100)+ N2*10+(ABS(N1)/100)*2010)=1
            ELSE
             ANIM_SE(3890+N1)=1
            ENDIF
         ELSE
           ANIM_SE(3890) = 1
         ENDIF
      ELSEIF(KEY3(1:4)=='TDEL')THEN
         ANIM_SE(4890)=1
      ELSEIF(KEY3(1:3)=='SSP')THEN
         ANIM_SE(4891)   = 1   
         IF (LAW51_PHASE1) ANIM_SE(4917) = 1 !SSP_law51-phase1
         IF (LAW51_PHASE2) ANIM_SE(4918) = 1 !SSP_law51-phase2
         IF (LAW51_PHASE3) ANIM_SE(4919) = 1 !SPP_law51-phase3
         IF (LAW51_PHASE4) ANIM_SE(4920) = 1 !SSP_law51-phase4              
      ELSEIF(KEY3(1:5)=='SCHLI')THEN
         ANIM_SE(4892)   = 1          
      ELSEIF(KEY3(1:5)=='DOMAI')THEN
         ANIM_SE(4893)   = 1                   
      ELSEIF(KEY3(1:4)=='FILL')THEN
         ANIM_SE(4894)   = 1
      ELSEIF(KEY3(1:5)=='SIGEQ')THEN  !   element equivalent stress criteria
         ANIM_SE(4895) = 1
      ELSEIF(KEY3(1:4)=='BULK' .OR. KEY3(1:4)=='QVIS')THEN
         ANIM_SE(4896) = 1
         IF (LAW51_PHASE1) ANIM_SE(4931) = 1              
         IF (LAW51_PHASE2) ANIM_SE(4932) = 1
         IF (LAW51_PHASE3) ANIM_SE(4933) = 1
         IF (LAW51_PHASE4) ANIM_SE(4934) = 1                                 

CTTENTION : NUMEROS RESERVES ANIM_SE pour SOUS-MATERIAUX LOI51
C         ANIM_SE(4897) ==> !DENS_law51-phase1
C         ANIM_SE(4898) ==> !           phase2
C         ANIM_SE(4899) ==> !           phase3
C         ANIM_SE(4900) ==> !           phase4
C         ANIM_SE(4901) ==> !ENER_law51-phase1
C         ANIM_SE(4902) ==> !           phase2
C         ANIM_SE(4903) ==> !           phase3
C         ANIM_SE(4904) ==> !           phase4
C         ANIM_SE(4905) ==> !TEMP_law51-phase1
C         ANIM_SE(4906) ==> !           phase2
C         ANIM_SE(4907) ==> !           phase3
C         ANIM_SE(4908) ==> !           phase4
C         ANIM_SE(4909) ==> !PRES_law51-phase1
C         ANIM_SE(4910) ==> !           phase2
C         ANIM_SE(4911) ==> !           phase3
C         ANIM_SE(4912) ==> !           phase4
C         ANIM_SE(4913) ==> !EPSP_law51-phase1
C         ANIM_SE(4914) ==> !           phase2
C         ANIM_SE(4915) ==> !           phase3
C         ANIM_SE(4916) ==> !           phase4
C         ANIM_SE(4917) ==> ! SSP_law51-phase1
C         ANIM_SE(4918) ==> !           phase2
C         ANIM_SE(4919) ==> !           phase3
C         ANIM_SE(4920) ==> !           phase4

      ELSEIF(KEY3(1:3)=='VOL' .OR. KEY3(1:4)=='VOLU')THEN
          ANIM_SE(4921) = 1
          ANIM_SE(4922) = 1
          ANIM_SE(4923) = 1
          ANIM_SE(4924) = 1
          ANIM_SE(4925) = 1                                        

CTTENTION : NUMEROS RESERVES ANIM_SE pour SOUS-MATERIAUX LOI51
C         ANIM_SE(4926) ==> !MASS_law51-phase1
C         ANIM_SE(4927) ==> !           phase2
C         ANIM_SE(4928) ==> !           phase3
C         ANIM_SE(4929) ==> !           phase4

      ELSEIF(KEY3(1:4)=='TDET')THEN
        ANIM_SE(4930) = 1

CTTENTION : NUMEROS RESERVES ANIM_SE pour SOUS-MATERIAUX LOI51
C         ANIM_SE(4931) ==> !QVIS_law51-phase1
C         ANIM_SE(4932) ==> !           phase2
C         ANIM_SE(4933) ==> !           phase3
C         ANIM_SE(4934) ==> !           phase4
C         ANIM_SE(4935) ==> !   law37 - liquid
C         ANIM_SE(4936) ==> !   law37 -    gas

      ELSEIF(KEY3(1:5)=='DT   ')THEN
        ANIM_SE(4937) = 1

      ELSEIF(KEY3(1:3) == 'MOM' .OR. KEY3(2:4) == 'MOM')THEN
        IF(KEY3(1:5) == 'MOM  ')THEN
          ANIM_SE(4938)=1
          ANIM_SE(4939)=1
          ANIM_SE(4940)=1
          ANIM_SE(4941)=1
          ANIM_SE(4942)=1
          ANIM_SE(4943)=1  
          ANIM_SE(4944)=1           
        ELSEIF(KEY3(1:5) == 'MOMX ')THEN
          ANIM_SE(4938)=1
        ELSEIF(KEY3(1:5) == 'MOMY ')THEN
          ANIM_SE(4939)=1
        ELSEIF(KEY3(1:5) == 'MOMZ ')THEN
          ANIM_SE(4940)=1
        ELSEIF(KEY3(1:5) == 'MOMXY')THEN
          ANIM_SE(4941)=1
        ELSEIF(KEY3(1:5) == 'MOMYZ')THEN
          ANIM_SE(4942)=1
        ELSEIF(KEY3(1:5) == 'MOMXZ')THEN
          ANIM_SE(4943)=1
        ELSEIF(KEY3(1:5) == '|MOM|')THEN
          ANIM_SE(4944)=1
        ENDIF
      ELSEIF(KEY3(1:3) == 'VEL' .OR. KEY3(2:4) == 'VEL' )THEN
        IF(KEY3(1:5) == 'VEL  ')THEN
          ANIM_SE(4945)=1
          ANIM_SE(4946)=1
          ANIM_SE(4947)=1
          ANIM_SE(4948)=1
          ANIM_SE(4949)=1
          ANIM_SE(4950)=1
          ANIM_SE(4951)=1
        ELSEIF(KEY3(1:5) == 'VELX ')THEN
          ANIM_SE(4945)=1
        ELSEIF(KEY3(1:5) == 'VELY ')THEN
          ANIM_SE(4946)=1
        ELSEIF(KEY3(1:5) == 'VELZ ')THEN
          ANIM_SE(4947)=1
        ELSEIF(KEY3(1:5) == 'VELXY')THEN
          ANIM_SE(4948)=1
        ELSEIF(KEY3(1:5) == 'VELYZ')THEN
          ANIM_SE(4949)=1
        ELSEIF(KEY3(1:5) == 'VELXZ')THEN
          ANIM_SE(4950)=1
        ELSEIF(KEY3(1:5) == '|VEL|')THEN
          ANIM_SE(4951)=1
        ENDIF 
      ELSEIF(KEY3(1:3) == 'FIN' .OR. KEY3(2:4) == 'FIN')THEN
        IF(KEY3(1:5) == 'FINT ')THEN
          ANIM_SE(4952)=1
          ANIM_SE(4953)=1
          ANIM_SE(4954)=1
          ANIM_SE(4955)=1
          ANIM_SE(4956)=1
          ANIM_SE(4957)=1
          ANIM_SE(4958)=1
        ELSEIF(KEY3(1:5) == 'FINX ')THEN
          ANIM_SE(4952)=1
        ELSEIF(KEY3(1:5) == 'FINY ')THEN
          ANIM_SE(4953)=1
        ELSEIF(KEY3(1:5) == 'FINZ ')THEN
          ANIM_SE(4954)=1
        ELSEIF(KEY3(1:5) == 'FINXY')THEN
          ANIM_SE(4955)=1
        ELSEIF(KEY3(1:5) == 'FINYZ')THEN
          ANIM_SE(4956)=1
        ELSEIF(KEY3(1:5) == 'FINXZ')THEN
          ANIM_SE(4957)=1
        ELSEIF(KEY3(1:5) == '|FIN|')THEN
          ANIM_SE(4958)=1
        ENDIF  
      ELSEIF(KEY3(1:3)=='AMS')THEN
        ANIM_SE(4959) = 1
      ELSEIF(KEY3(1:5)=='VORTY')THEN
        ANIM_E(4960)  = 1
        ANIM_SE(4960) = 1
      ELSEIF(KEY3(1:5)=='VORTZ')THEN
        ANIM_E(4961)  = 1
        ANIM_SE(4961) = 1  
      ELSEIF(KEY3(1:4)=='VORT')THEN
        ANIM_E(4962)  = 1
        ANIM_SE(4962) = 1            
      ELSEIF(KEY3(1:4)=='EINT')THEN
        ANIM_E(4963)  = 1
        ANIM_SE(4963) = 1  
      ELSEIF(KEY3(1:3) == 'OFF')THEN  
        ANIM_SE(4965) = 1         
      ELSEIF(KEY3(1:4)=='MACH')THEN
        ANIM_SE(4966) = 1   
      ELSEIF(KEY3(1:5)=='COLOR')THEN
        ANIM_SE(4967) = 1   
      ELSEIF(KEY3(1:4)=='DAMG')THEN
        ANIM_SE(4968) = 1  
      ELSEIF(KEY3(1:7)=='NL_EPSP')THEN
        ANIM_SE(4969) = 1    
      ELSEIF(KEY3(1:7)=='NL_EPSD')THEN
        ANIM_SE(4970) = 1  
      ELSEIF(KEY3(1:6)=='TSAIWU')THEN
         READ(KEY4(1:5),'(I5)')N1
         IF (N1 /= 0 ) THEN
           ANIM_SE(4971+N1) = 1
         ELSE
           ANIM_SE(4971) = 1 
         ENDIF   
      ELSEIF(KEY3(1:9)=='TILLOTSON')THEN
          ANIM_SE(5172) = 1 

!D YOUR NEW ANIMATION CONTOUR HERE          
      !ELSEIF(...)THEN
      !  ANIM_SE(5173) = 1

      ENDIF
C
C--------------------------
C    FIL
C--------------------------
      ELSEIF(KEY2(1:4)=='TRUS'.OR.KEY2(1:4)=='BEAM'.OR.
     .        KEY2(1:5)=='SPRIN')THEN
C--------------------------
C     FIL:TORSEUR
C--------------------------
      IF(KEY3(1:4)=='FORC')THEN
           ANIM_FT(1) = 1
C--------------------------
C     FILS:SCALAIRES
C--------------------------
!       ELSEIF(KEY3(1:4)=='EPSP')THEN
!          ANIM_FE(1) = 1
      ELSEIF(KEY3(1:4)=='ENER')THEN
         ANIM_FE(3) = 1
      ELSEIF(KEY3(1:4)=='VONM')THEN
         ANIM_FE(7) = 1
      ELSEIF(KEY3(1:5)=='DAM1')THEN
         ANIM_FE(11) = 1
      ELSEIF(KEY3(1:5)=='DAM2')THEN
         ANIM_FE(12) = 1
      ELSEIF(KEY3(1:5)=='DAM3')THEN
         ANIM_FE(13) = 1
      ELSEIF(KEY3(1:5)=='SIGX ')THEN
         ANIM_FE(14) = 1
      ELSEIF(KEY3(1:5)=='SIGY ')THEN
         ANIM_FE(15) = 1
      ELSEIF(KEY3(1:5)=='SIGZ ')THEN
         ANIM_FE(16) = 1
      ELSEIF(KEY3(1:5)=='SIGXY'.OR.KEY3(1:5)=='SIGYX')THEN
         ANIM_FE(17) = 1
      ELSEIF(KEY3(1:5)=='SIGYZ'.OR.KEY3(1:5)=='SIGZY')THEN
         ANIM_FE(18) = 1
      ELSEIF(KEY3(1:5)=='SIGZX'.OR.KEY3(1:5)=='SIGXZ')THEN
         ANIM_FE(19) = 1
      ELSEIF(KEY3(1:5)=='DT   ')THEN
         ANIM_FE(20) = 1
      ELSEIF(KEY3(1:3)=='AMS')THEN
         ANIM_FE(21) = 1        
      ELSEIF(KEY3(1:3)=='OFF')THEN
         ANIM_FE(22) = 1         
      ELSEIF(KEY3(1:4)=='EPSP')THEN
        ANIM_FE(1) = 1 ! global element plastic strain
        IF (KEY2(1:4)=='BEAM') THEN
          N1 = 0
          IF (KEY4(1:3) == 'ALL') THEN
            IBEAM_EPSPALL = 1
          ELSE
            READ(KEY4(1:5),'(I5)')N1 !  ! global element plastic strain
            IF (N1 > 0)  ANIM_FE(22 + N1) = 1
          ENDIF
        ENDIF ! IF (KEY2(1:4)=='BEAM')
      ELSEIF (KEY3(1:5)=='EPSX ') THEN
        IF (KEY2(1:5)=='TRUSS') THEN
          ANIM_FE(123) = 1
        ENDIF
      ELSEIF(KEY3(1:4)=='EPSD')THEN
        ANIM_FE(124) = 1               
      ELSE
        IXITKEY=IXITKEY+1
      ENDIF
C
      ELSEIF(KEY2(1:4)=='ELEM')THEN
        IF (KEY3(1:5) == 'LAW51') THEN
           IF (KEY4(1:3) == 'ALL') THEN
              LAW51_PHASE1 = .TRUE.
              LAW51_PHASE2 = .TRUE.
              LAW51_PHASE3 = .TRUE.
              LAW51_PHASE4 = .TRUE.
           ELSE IF (KEY4(1:1) == '1') THEN
              LAW51_PHASE1 = .TRUE.
           ELSE IF (KEY4(1:1) == '2') THEN
              LAW51_PHASE2 = .TRUE.
           ELSE IF (KEY4(1:1) == '3') THEN
              LAW51_PHASE3 = .TRUE.
           ELSE IF (KEY4(1:1) == '4') THEN
              LAW51_PHASE4 = .TRUE.
           ENDIF
C--------------------------
C   GENERIQUE SCALAIRES
C--------------------------
        ELSE IF    (KEY3(1:4)=='EPSP')THEN
         ANIM_E(1) = 1
         ANIM_SE(1) = 1
         ANIM_CE(1) = 1
         ANIM_FE(1) = 1
         IF (LAW51_PHASE1) ANIM_SE(4913) = 1 !EPSP_law51-phase1
         IF (LAW51_PHASE2) ANIM_SE(4914) = 1 !EPSP_law51-phase2
         IF (LAW51_PHASE3) ANIM_SE(4915) = 1 !EPSP_law51-phase3
         IF (LAW51_PHASE4) ANIM_SE(4916) = 1 !EPSP_law51-phase4             
       ELSEIF(KEY3(1:5)=='DENS3')THEN
           !/ANIM/ELEM/DENS37
           ANIM_SE(4935) = 1  !law37 - rho_liquid
           ANIM_SE(4936) = 1  !law37 - rho_gas
       ELSEIF(KEY3(1:4)=='DENS')THEN
         ANIM_E(2) = 1
         ANIM_SE(2) = 1
         ANIM_CE(2) = 1
         IF (LAW51_PHASE1) THEN 
            ANIM_SE(4897) = 1 !DENS_law51-phase1
            ANIM_CE(11890) = 1
         ENDIF
         IF (LAW51_PHASE2) THEN
            ANIM_SE(4898) = 1 !DENS_law51-phase2
            ANIM_CE(11891) = 1
         ENDIF
         IF (LAW51_PHASE3) THEN
            ANIM_SE(4899) = 1 !DENS_law51-phase3
            ANIM_CE(11892) = 1
         ENDIF
         IF (LAW51_PHASE4) THEN
            ANIM_SE(4900) = 1 !DENS_law51-phase4
            ANIM_CE(11893) = 1
         ENDIF
           !ANIM_SE(4935) = 1  !law37 -rho_liquid
           !ANIM_SE(4936) = 1  !law37 - rho_gas
       ELSEIF(KEY3(1:4)=='ENER')THEN
         ANIM_E(3) = 1
         ANIM_SE(3) = 1
         ANIM_CE(3) = 1
         ANIM_FE(3) = 1
         IF (LAW51_PHASE1) THEN
            ANIM_SE(4901) = 1 !ENER_law51-phase1
            ANIM_CE(11894) = 1
         ENDIF
         IF (LAW51_PHASE2) THEN
            ANIM_SE(4902) = 1 !ENER_law51-phase2
            ANIM_CE(11895) = 1
         ENDIF
         IF (LAW51_PHASE3) THEN
            ANIM_SE(4903) = 1 !ENER_law51-phase3
            ANIM_CE(11896) = 1
         ENDIF
         IF (LAW51_PHASE4) THEN
            ANIM_SE(4904) = 1 !ENER_law51-phase4
            ANIM_CE(11897) = 1
         ENDIF
       ELSEIF(KEY3(1:4)=='TEMP')THEN
         ANIM_E(4) = 1
         ANIM_SE(4) = 1
         ANIM_CE(4) = 1
         IF (LAW51_PHASE1) THEN
            ANIM_SE(4905) = 1 !TEMP_law51-phase1
            ANIM_CE(11898) = 1
         ENDIF
         IF (LAW51_PHASE2) THEN
            ANIM_SE(4906) = 1 !TEMP_law51-phase2
            ANIM_CE(11899) = 1
         ENDIF
         IF (LAW51_PHASE3) THEN
            ANIM_SE(4907) = 1 !TEMP_law51-phase3
            ANIM_CE(11900) = 1
         ENDIF
         IF (LAW51_PHASE4) THEN
            ANIM_SE(4908) = 1 !TEMP_law51-phase4
            ANIM_CE(11901) = 1
         ENDIF
       ELSEIF(KEY3(1:4)=='THIC')THEN
         ANIM_E(5) = 1
         ANIM_CE(5) = 1
       ELSEIF(KEY3(1:1)=='P'.AND.KEY3(1:5)/='PLA51')THEN
         ANIM_E(6) = 1
         ANIM_SE(6) = 1
         ANIM_CE(6) = 1
         IF (LAW51_PHASE1) THEN 
            ANIM_SE(4909) = 1 !PRES_law51-phase1
            ANIM_CE(11902) = 1
         ENDIF
         IF (LAW51_PHASE2) THEN 
            ANIM_SE(4910) = 1 !PRES_law51-phase2
            ANIM_CE(11903) = 1
         ENDIF
         IF (LAW51_PHASE3) THEN 
            ANIM_SE(4911) = 1 !PRES_law51-phase3
            ANIM_CE(11904) = 1
         ENDIF
         IF (LAW51_PHASE4) THEN 
            ANIM_SE(4912) = 1 !PRES_law51-phase4 
            ANIM_CE(11905) = 1
         ENDIF          
       ELSEIF(KEY3(1:4)=='VONM')THEN
         ANIM_E(7) = 1
         ANIM_SE(7) = 1
         ANIM_CE(7) = 1
         ANIM_FE(7) = 1
       ELSEIF(KEY3(1:1)=='K')THEN
         ANIM_E(8) = 1
         ANIM_SE(8) = 1
         ANIM_CE(8) = 1
       ELSEIF(KEY3(1:4)=='TVIS')THEN
         ANIM_E(9) = 1
         ANIM_SE(9) = 1
         ANIM_CE(9) = 1
       ELSEIF(KEY3(1:5)=='VORTX')THEN
         ANIM_E(10) = 1
         ANIM_SE(10) = 1
         ANIM_CE(10) = 1
       ELSEIF(KEY3(1:5)=='DAM1')THEN
         ANIM_E(11) = 1
         ANIM_SE(11) = 1
         ANIM_CE(11) = 1
         ANIM_FE(11) = 1
       ELSEIF(KEY3(1:5)=='DAM2')THEN
         ANIM_E(12) = 1
         ANIM_SE(12) = 1
         ANIM_CE(12) = 1
         ANIM_FE(12) = 1
       ELSEIF(KEY3(1:5)=='DAM3')THEN
         ANIM_E(13) = 1
         ANIM_SE(13) = 1
         ANIM_CE(13) = 1
         ANIM_FE(13) = 1
       ELSEIF(KEY3(1:5)=='SIGX ')THEN
         ANIM_E(14) = 1
         ANIM_SE(14) = 1
         ANIM_CE(14) = 1
         ANIM_FE(14) = 1
       ELSEIF(KEY3(1:5)=='SIGY ')THEN
         ANIM_E(15) = 1
         ANIM_SE(15) = 1
         ANIM_CE(15) = 1
         ANIM_FE(15) = 1
       ELSEIF(KEY3(1:5)=='SIGZ ')THEN
         ANIM_E(16) = 1
         ANIM_SE(16) = 1
         ANIM_CE(16) = 1
         ANIM_FE(16) = 1
       ELSEIF(KEY3(1:5)=='SIGXY'.OR.KEY3(1:5)=='SIGYX')THEN
         ANIM_E(17) = 1
         ANIM_SE(17) = 1
         ANIM_CE(17) = 1
         ANIM_FE(17) = 1
       ELSEIF(KEY3(1:5)=='SIGYZ'.OR.KEY3(1:5)=='SIGZY')THEN
         ANIM_E(18) = 1
         ANIM_SE(18) = 1
         ANIM_CE(18) = 1
         ANIM_FE(18) = 1
       ELSEIF(KEY3(1:5)=='SIGZX'.OR.KEY3(1:5)=='SIGXZ')THEN
         ANIM_E(19) = 1
         ANIM_SE(19) = 1
         ANIM_CE(19) = 1
         ANIM_FE(19) = 1
       ELSEIF(KEY3(1:5)=='USR1 '.OR.KEY3(1:5)=='USER1'.OR.
     .         KEY3(1:5)=='PLA51')THEN
         ANIM_E(20) = 1
         ANIM_SE(20) = 1
         ANIM_CE(20) = 1
       ELSEIF(KEY3(1:5)=='USR2 '.OR.KEY3(1:5)=='USER2'.OR.
     .         KEY3(1:5)=='TEM51')THEN
         ANIM_E(21) = 1
         ANIM_SE(21) = 1
         ANIM_CE(21) = 1
       ELSEIF(KEY3(1:5)=='USR3 '.OR.KEY3(1:5)=='USER3'.OR.
     .         KEY3(1:5)=='BF51 ')THEN
         ANIM_E(22) = 1
         ANIM_SE(22) = 1
         ANIM_CE(22) = 1
       ELSEIF(KEY3(1:5)=='USR4 '.OR.KEY3(1:5)=='USER4')THEN
         ANIM_E(23) = 1
         ANIM_SE(23) = 1
         ANIM_CE(23) = 1
       ELSEIF(KEY3(1:5)=='USR5 '.OR.KEY3(1:5)=='USER5')THEN
         ANIM_E(24) = 1
         ANIM_SE(24) = 1
         ANIM_CE(24) = 1
       ELSEIF(KEY3(1:4)=='HOUR')THEN
         ANIM_E(25) = 1
         ANIM_SE(25) = 1
         ANIM_CE(25) = 1
       ELSEIF(KEY3(1:4)=='EPSD')THEN
         ANIM_CE(26) = 1
         ANIM_SE(26) = 1
      ELSEIF(KEY3(1:5)=='USR6 '.OR.KEY3(1:5)=='USER6')THEN
         ANIM_SE(27) = 1
         ANIM_CE(27) = 1
      ELSEIF(KEY3(1:5)=='USR7 '.OR.KEY3(1:5)=='USER7')THEN
         ANIM_SE(28) = 1
         ANIM_CE(28) = 1
      ELSEIF(KEY3(1:5)=='USR8 '.OR.KEY3(1:5)=='USER8')THEN
         ANIM_SE(29) = 1
         ANIM_CE(29) = 1
      ELSEIF(KEY3(1:5)=='USR9 '.OR.KEY3(1:5)=='USER9')THEN
         ANIM_SE(30) = 1
         ANIM_CE(30) = 1
      ELSEIF(KEY3(1:5)=='USR10')THEN
         ANIM_SE(31) = 1
         ANIM_CE(31) = 1
      ELSEIF(KEY3(1:5)=='USR11')THEN
         ANIM_SE(32) = 1
         ANIM_CE(32) = 1
      ELSEIF(KEY3(1:5)=='USR12')THEN
         ANIM_SE(33) = 1
         ANIM_CE(33) = 1
      ELSEIF(KEY3(1:5)=='USR13')THEN
         ANIM_SE(34) = 1
         ANIM_CE(34) = 1
      ELSEIF(KEY3(1:5)=='USR14')THEN
         ANIM_SE(35) = 1
         ANIM_CE(35) = 1
      ELSEIF(KEY3(1:5)=='USR15')THEN
         ANIM_SE(36) = 1
         ANIM_CE(36) = 1
      ELSEIF(KEY3(1:5)=='USR16')THEN
         ANIM_SE(37) = 1
         ANIM_CE(37) = 1
      ELSEIF(KEY3(1:5)=='USR17')THEN
         ANIM_SE(38) = 1
         ANIM_CE(38) = 1
      ELSEIF(KEY3(1:5)=='USR18')THEN
         ANIM_SE(39) = 1
         ANIM_CE(39) = 1
      ELSEIF(KEY3(1:5)=='USR19')THEN
         ANIM_SE(40) = 1
         ANIM_CE(40) = 1
      ELSEIF(KEY3(1:5)=='USR20')THEN
         ANIM_SE(41) = 1
         ANIM_CE(41) = 1
      ELSEIF(KEY3(1:5)=='USR21')THEN
         ANIM_SE(42) = 1
         ANIM_CE(42) = 1
      ELSEIF(KEY3(1:5)=='USR22')THEN
         ANIM_SE(43) = 1
         ANIM_CE(43) = 1
      ELSEIF(KEY3(1:5)=='USR23')THEN
         ANIM_SE(44) = 1
         ANIM_CE(44) = 1
      ELSEIF(KEY3(1:5)=='USR24')THEN
         ANIM_SE(45) = 1
         ANIM_CE(45) = 1
      ELSEIF(KEY3(1:5)=='USR25')THEN
         ANIM_SE(46) = 1
         ANIM_CE(46) = 1
      ELSEIF(KEY3(1:5)=='USR26')THEN
         ANIM_SE(47) = 1
         ANIM_CE(47) = 1
      ELSEIF(KEY3(1:5)=='USR27')THEN
         ANIM_SE(48) = 1
         ANIM_CE(48) = 1
      ELSEIF(KEY3(1:5)=='USR28')THEN
         ANIM_SE(49) = 1
         ANIM_CE(49) = 1
      ELSEIF(KEY3(1:5)=='USR29')THEN
         ANIM_SE(50) = 1
         ANIM_CE(50) = 1
      ELSEIF(KEY3(1:5)=='USR30')THEN
         ANIM_SE(51) = 1
         ANIM_CE(51) = 1
      ELSEIF(KEY3(1:5)=='USR31')THEN
         ANIM_SE(52) = 1
         ANIM_CE(52) = 1
      ELSEIF(KEY3(1:5)=='USR32')THEN
         ANIM_SE(53) = 1
         ANIM_CE(53) = 1
      ELSEIF(KEY3(1:5)=='USR33')THEN
         ANIM_SE(54) = 1
         ANIM_CE(54) = 1
      ELSEIF(KEY3(1:5)=='USR34')THEN
         ANIM_SE(55) = 1
         ANIM_CE(55) = 1
      ELSEIF(KEY3(1:5)=='USR35')THEN
         ANIM_SE(56) = 1
         ANIM_CE(56) = 1
      ELSEIF(KEY3(1:5)=='USR36')THEN
         ANIM_SE(57) = 1
         ANIM_CE(57) = 1
      ELSEIF(KEY3(1:5)=='USR37')THEN
         ANIM_SE(58) = 1
         ANIM_CE(58) = 1
      ELSEIF(KEY3(1:5)=='USR38')THEN
         ANIM_SE(59) = 1
         ANIM_CE(59) = 1
      ELSEIF(KEY3(1:5)=='USR39')THEN
         ANIM_SE(60) = 1
         ANIM_CE(60) = 1
      ELSEIF(KEY3(1:5)=='USR40')THEN
         ANIM_SE(61) = 1
         ANIM_CE(61) = 1
      ELSEIF(KEY3(1:5)=='USR41')THEN
         ANIM_SE(62) = 1
         ANIM_CE(62) = 1
      ELSEIF(KEY3(1:5)=='USR42')THEN
         ANIM_SE(63) = 1
         ANIM_CE(63) = 1
      ELSEIF(KEY3(1:5)=='USR43')THEN
         ANIM_SE(64) = 1
         ANIM_CE(64) = 1
      ELSEIF(KEY3(1:5)=='USR44')THEN
         ANIM_SE(65) = 1
         ANIM_CE(65) = 1
      ELSEIF(KEY3(1:5)=='USR45')THEN
         ANIM_SE(66) = 1
         ANIM_CE(66) = 1
      ELSEIF(KEY3(1:5)=='USR46')THEN
         ANIM_SE(67) = 1
         ANIM_CE(67) = 1
      ELSEIF(KEY3(1:5)=='USR47')THEN
         ANIM_SE(68) = 1
         ANIM_CE(68) = 1
      ELSEIF(KEY3(1:5)=='USR48')THEN
         ANIM_SE(69) = 1
         ANIM_CE(69) = 1
      ELSEIF(KEY3(1:5)=='USR49')THEN
         ANIM_SE(70) = 1
         ANIM_CE(70) = 1
      ELSEIF(KEY3(1:5)=='USR50')THEN
         ANIM_SE(71) = 1
         ANIM_CE(71) = 1
      ELSEIF(KEY3(1:5)=='VFRAC')THEN
        !BIMAT(law20)
        !BIPHAS(law37)   : TRIMAT = -2
        !MULTIMAT(law51) : TRIMAT = 3|4
        ! law51 & law37  : TRIMAT = 3|4
        !consistency with material law checked in radioss2.F since TRIMAT common var not yet read  
         ANIM_SE(283)   = 1 !vfrac phase1 USR4 (law51 & 37)
         ANIM_SE(284)   = 1 !vfrac phase2 USR4 (law51 & 37)
         ANIM_SE(285)   = 1 !vfrac phase3 USR22 (law51)
         ANIM_SE(286)   = 1 !vfrac phase4 USR48 (law51)
         ANIM_CE(10248) = 1 !vfrac phase1 (law20/51)
         ANIM_CE(10249) = 1 !vfrac phase2 (law20/51)
         ANIM_CE(10250) = 1 !vfrac phase3 (law20/51)
         ANIM_CE(10251) = 1 !vfrac phase4 (law20/51)           
      ELSEIF(KEY3(1:5)=='BFRAC')THEN
         ANIM_SE(887)   = 1                    
      ELSEIF(KEY3(1:3)=='SSP')THEN
         ANIM_SE(4891)   = 1              
         ANIM_CE(10671)  = 1 
         IF (LAW51_PHASE1) THEN 
            ANIM_SE(4917) = 1 ! SSP_law51-phase1
            ANIM_CE(11906) = 1
         ENDIF
         IF (LAW51_PHASE2) THEN 
            ANIM_SE(4918) = 1 !           phase2
            ANIM_CE(11907) = 1
         ENDIF
         IF (LAW51_PHASE3) THEN 
            ANIM_SE(4919) = 1 !           phase3
            ANIM_CE(11908) = 1
         ENDIF
         IF (LAW51_PHASE4) THEN 
            ANIM_SE(4920) = 1 !           phase4
            ANIM_CE(11909) = 1
         ENDIF
       ELSEIF(KEY3(1:5)=='SCHLI')THEN
         ANIM_SE(4892)   = 1              
         ANIM_CE(10672)  = 1    
       ELSEIF(KEY3(1:5)=='DOMAI')THEN
         ANIM_SE(4893)   = 1              
         ANIM_CE(10676)  = 1    
       ELSEIF(KEY3(1:3)=='VOL' .OR. KEY3(1:4)=='VOLU')THEN
         ANIM_SE(4921)   = 1 
         ANIM_CE(4*MX_PLY_ANIM + 14566)  = 1                        
         IF (LAW51_PHASE1) THEN 
            ANIM_SE(4922) = 1 !VOLU_law51-phase1
            ANIM_CE(11914) = 1
         ENDIF
         IF (LAW51_PHASE2) THEN 
            ANIM_SE(4923) = 1 !           phase2
            ANIM_CE(11915) = 1
         ENDIF
         IF (LAW51_PHASE3) THEN 
            ANIM_SE(4924) = 1 !           phase3
            ANIM_CE(11916) = 1
         ENDIF
         IF (LAW51_PHASE4) THEN 
            ANIM_SE(4925) = 1 !           phase4
            ANIM_CE(11917) = 1
         ENDIF
       ELSEIF(KEY3(1:4)=='TDET')THEN
         ANIM_SE(4930)   = 1 
         ANIM_CE(11889)  = 1     
      ELSEIF(KEY3(1:5)=='LAW20')THEN
         ANIM_CE(11890) = 1 !dens
         ANIM_CE(11891) = 1
         ANIM_CE(11892) = 0
         ANIM_CE(11893) = 0
         ANIM_CE(11894) = 1 !ener
         ANIM_CE(11895) = 1
         ANIM_CE(11896) = 0
         ANIM_CE(11897) = 0
         ANIM_CE(11898) = 1 !temp
         ANIM_CE(11899) = 1
         ANIM_CE(11900) = 0
         ANIM_CE(11901) = 0
         ANIM_CE(11902) = 1 !pres
         ANIM_CE(11903) = 1
         ANIM_CE(11904) = 0
         ANIM_CE(11905) = 0
         ANIM_CE(11906) = 1 !plas
         ANIM_CE(11907) = 1
         ANIM_CE(11908) = 0
         ANIM_CE(11909) = 0                                                                                                                                                                                                                 
         ANIM_CE(11910) = 1 !ssp           
         ANIM_CE(11911) = 1  
         ANIM_CE(11912) = 0  
         ANIM_CE(11913) = 0  
         ANIM_CE(11914) = 1 !volume 
         ANIM_CE(11915) = 1  
         ANIM_CE(11916) = 0  
         ANIM_CE(11917) = 0  
         ANIM_CE(11918) = 1 !mass 
         ANIM_CE(11919) = 1  
         ANIM_CE(11920) = 0  
         ANIM_CE(11921) = 0  
         ANIM_CE(11922) = 1 !Qvis  
         ANIM_CE(11923) = 1  
         ANIM_CE(11924) = 0 
         ANIM_CE(11925) = 0                                                              
       ELSEIF(KEY3(1:5)=='SIGEQ')THEN    !   element equivalent stress criteria
         ANIM_SE(4895)  = 1
         ANIM_CE(10677) = 1
       ELSEIF(KEY3(1:4)=='BULK' .OR. KEY3(1:4)=='QVIS')THEN
         ANIM_SE(4896)  = 1
         IF (LAW51_PHASE1) ANIM_SE(4931)  = 1
         IF (LAW51_PHASE2) ANIM_SE(4932)  = 1
         IF (LAW51_PHASE3) ANIM_SE(4933)  = 1
         IF (LAW51_PHASE4) ANIM_SE(4934)  = 1                                            
         ANIM_CE(11888) = 1   !PID15 uses ANIM_CE(10678:11887)  
       ELSEIF(KEY3(1:5)=='DT   ')THEN
         IDX            = 13242 + 4*MX_PLY_ANIM  
         ANIM_SE(4937)  = 1 
         ANIM_CE(IDX)   = 1 
         ANIM_FE(20)    = 1 
       ELSEIF(KEY3(1:3) == 'MOM' .OR. KEY3(2:4) == 'MOM' )THEN
         IF(KEY3(1:5) == 'MOM  ')THEN
           ANIM_SE(4938) =1
           ANIM_SE(4939) =1
           ANIM_SE(4940) =1
           ANIM_SE(4941) =1
           ANIM_SE(4942) =1
           ANIM_SE(4943) =1 
           ANIM_SE(4944) =1            
         ELSEIF(KEY3(1:5) == 'MOMX ')THEN
           ANIM_SE(4938) =1
         ELSEIF(KEY3(1:5) == 'MOMY ')THEN
           ANIM_SE(4939) =1
         ELSEIF(KEY3(1:5) == 'MOMZ ')THEN
           ANIM_SE(4940) =1
         ELSEIF(KEY3(1:5) == 'MOMXY')THEN
           ANIM_SE(4941) =1
         ELSEIF(KEY3(1:5) == 'MOMYZ')THEN
           ANIM_SE(4942) =1
         ELSEIF(KEY3(1:5) == 'MOMXZ')THEN
           ANIM_SE(4943) =1 
         ELSEIF(KEY3(1:5) == '|MOM|')THEN
           ANIM_SE(4944) =1                          
         ENDIF
       ELSEIF(KEY3(1:3) == 'VEL' .OR. KEY3(2:4) == 'VEL' )THEN
         IF(KEY3(1:5) == 'VEL  ')THEN
           ANIM_SE(4945) =1
           ANIM_SE(4946) =1
           ANIM_SE(4947) =1
           ANIM_SE(4948) =1
           ANIM_SE(4949) =1
           ANIM_SE(4950) =1 
           ANIM_SE(4951) =1                         
         ELSEIF(KEY3(1:5) == 'VELX ')THEN
           ANIM_SE(4945) =1
         ELSEIF(KEY3(1:5) == 'VELY ')THEN
           ANIM_SE(4946) =1
         ELSEIF(KEY3(1:5) == 'VELZ ')THEN
           ANIM_SE(4947) =1
         ELSEIF(KEY3(1:5) == 'VELXY')THEN
           ANIM_SE(4948) =1
         ELSEIF(KEY3(1:5) == 'VELYZ')THEN
           ANIM_SE(4949) =1
         ELSEIF(KEY3(1:5) == 'VELXZ')THEN
           ANIM_SE(4950) =1
         ELSEIF(KEY3(1:5) == '|VEL|')THEN
           ANIM_SE(4951) =1             
         ENDIF                                  
       ELSEIF(KEY3(1:3) == 'FIN' .OR. KEY3(2:4) == 'FIN' )THEN
         IF(KEY3(1:5) == 'FINT ')THEN
           ANIM_SE(4952)=1
           ANIM_SE(4953)=1
           ANIM_SE(4954)=1
           ANIM_SE(4955)=1
           ANIM_SE(4956)=1
           ANIM_SE(4957)=1
           ANIM_SE(4958)=1
         ELSEIF(KEY3(1:5) == 'FINX ')THEN
           ANIM_SE(4952)=1
         ELSEIF(KEY3(1:5) == 'FINY ')THEN
           ANIM_SE(4953)=1
         ELSEIF(KEY3(1:5) == 'FINZ ')THEN
           ANIM_SE(4954)=1
         ELSEIF(KEY3(1:5) == 'FINXY')THEN
           ANIM_SE(4955)=1
         ELSEIF(KEY3(1:5) == 'FINYZ')THEN
           ANIM_SE(4956)=1
         ELSEIF(KEY3(1:5) == 'FINXZ')THEN
           ANIM_SE(4957)=1
         ELSEIF(KEY3(1:5) == '|FIN|')THEN
           ANIM_SE(4958)=1
         ENDIF
       ELSEIF(KEY3(1:3)=='AMS')THEN
         IDX           = 13242 + 4*MX_PLY_ANIM + 1  
         ANIM_SE(4959) = 1 
         ANIM_CE(IDX)  = 1 
         ANIM_FE(21)   = 1   
       ELSEIF(KEY3(1:5)=='VORTY')THEN
         ANIM_E(4960)  = 1
         ANIM_SE(4960) = 1
       ELSEIF(KEY3(1:5)=='VORTZ')THEN
         ANIM_E(4961)  = 1
         ANIM_SE(4961) = 1
        ELSEIF(KEY3(1:4)=='VORT')THEN
         ANIM_E(4962)  = 1
         ANIM_SE(4962) = 1
        ELSEIF(KEY3(1:4)=='EINT')THEN
         ANIM_E(4963)  = 1
         ANIM_SE(4963) = 1 
         IDX           = 13242 + 4*MX_PLY_ANIM + 2  
         ANIM_CE(IDX)  = 1   
        ELSEIF(KEY3(1:4)=='WPLA')THEN
         IDX           = 13242 + 4*MX_PLY_ANIM + 3 
         ANIM_CE(IDX)  = 1  
         ANIM_SE(4964) = 1 
        ELSEIF(KEY3(1:3)=='OFF')THEN
         IDX           = 13547 + 4*MX_PLY_ANIM + 1000 + 1
         ANIM_CE(IDX)  = 1  
         ANIM_SE(4965) = 1 
         ANIM_FE(22) = 1     
        ELSEIF(KEY3(1:4)=='MACH')THEN
         IDX           = 13547 + 4*MX_PLY_ANIM + 1000 + 2
         ANIM_CE(IDX)  = 1  
         ANIM_SE(4966) = 1 
        ELSEIF(KEY3(1:5)=='COLOR')THEN
         IDX           = 13547 + 4*MX_PLY_ANIM + 1000 + 3
         ANIM_CE(IDX)  = 1  
         ANIM_SE(4967) = 1  
        ELSEIF(KEY3(1:4) == 'DAMG') THEN 
         IDX           = 14551 + 4*MX_PLY_ANIM
         ANIM_CE(IDX)  = 1  
         ANIM_SE(4968) = 1    
        ELSE IF(KEY3(1:7)=='NL_EPSP')THEN
         IDX           = 14567 + 4*MX_PLY_ANIM
         ANIM_CE(IDX)  = 1
         ANIM_SE(4969) = 1
        ELSE IF(KEY3(1:7)=='NL_EPSD')THEN
         IDX           = 14581 + 4*MX_PLY_ANIM
         ANIM_CE(IDX)  = 1
         ANIM_SE(4970) = 1
        ELSE IF(KEY3(1:6)=='TSAIWU')THEN 
         IDX           = 14595 + 4*MX_PLY_ANIM 
         ANIM_CE(IDX)  = 1 
         ANIM_SE(4971) = 1 
        ELSEIF(KEY3(1:9)=='TILLOTSON')THEN
         IDX = 14596 + 4*MX_PLY_ANIM
         ANIM_CE(IDX)  = 1  
         ANIM_SE(5172) = 1  
         
       ELSE
         IXITKEY=IXITKEY+1
       ENDIF
C--------------------------
C    GPS : GRID POINT STRESS 1,2, use ANIM_N(101-301)
C--------------------------
      ELSEIF(KEY2(1:4)=='GPS1')THEN
      IADG = IAD_GPS
       IF(KEY3(1:1)=='P')THEN
         ANIM_N(IADG+1) = 1
       ELSEIF(KEY3(1:4)=='VONM')THEN
         ANIM_N(IADG+2) = 1
       ELSEIF(KEY3(1:4)=='TENS')THEN
         ANIM_N(IADG+3) = 1
         ANIM_N(IADG+4) = 1
         ANIM_N(IADG+5) = 1
         ANIM_N(IADG+6) = 1
         ANIM_N(IADG+7) = 1
         ANIM_N(IADG+8) = 1
       ELSEIF(KEY3(1:5)=='SIGX ')THEN
         ANIM_N(IADG+3) = 1
       ELSEIF(KEY3(1:5)=='SIGY ')THEN
         ANIM_N(IADG+4) = 1
       ELSEIF(KEY3(1:5)=='SIGZ ')THEN
         ANIM_N(IADG+5) = 1
       ELSEIF(KEY3(1:5)=='SIGXY'.OR.KEY3(1:5)=='SIGYX')THEN
         ANIM_N(IADG+6) = 1
       ELSEIF(KEY3(1:5)=='SIGYZ'.OR.KEY3(1:5)=='SIGZY')THEN
         ANIM_N(IADG+7) = 1
       ELSEIF(KEY3(1:5)=='SIGZX'.OR.KEY3(1:5)=='SIGXZ')THEN
         ANIM_N(IADG+8) = 1
       ELSEIF(KEY3(1:5)=='SHELL')THEN
         IF(KEY4(1:5)=='UPPER')THEN
           ANIM_N(IADG+9) = 1
           ANIM_N(IADG+10) = 1
           ANIM_N(IADG+11) = 1
           ANIM_N(IADG+12) = 1
           ANIM_N(IADG+13) = 1
           ANIM_N(IADG+14) = 1
         ELSEIF(KEY4(1:5)=='LOWER')THEN
           ANIM_N(IADG+15) = 1
           ANIM_N(IADG+16) = 1
           ANIM_N(IADG+17) = 1
           ANIM_N(IADG+18) = 1
           ANIM_N(IADG+19) = 1
           ANIM_N(IADG+20) = 1
         ELSE
           IXITKEY=IXITKEY+1
         ENDIF
       ELSE
         IXITKEY=IXITKEY+1
       ENDIF
      ELSEIF(KEY2(1:4)=='GPS2')THEN
      IADG = IAD_GPS+100
       IF(KEY3(1:1)=='P')THEN
         ANIM_N(IADG+1) = 1
       ELSEIF(KEY3(1:4)=='VONM')THEN
         ANIM_N(IADG+2) = 1
       ELSEIF(KEY3(1:4)=='TENS')THEN
         ANIM_N(IADG+3) = 1
         ANIM_N(IADG+4) = 1
         ANIM_N(IADG+5) = 1
         ANIM_N(IADG+6) = 1
         ANIM_N(IADG+7) = 1
         ANIM_N(IADG+8) = 1
       ELSEIF(KEY3(1:5)=='SIGX ')THEN
         ANIM_N(IADG+3) = 1
       ELSEIF(KEY3(1:5)=='SIGY ')THEN
         ANIM_N(IADG+4) = 1
       ELSEIF(KEY3(1:5)=='SIGZ ')THEN
         ANIM_N(IADG+5) = 1
       ELSEIF(KEY3(1:5)=='SIGXY'.OR.KEY3(1:5)=='SIGYX')THEN
         ANIM_N(IADG+6) = 1
       ELSEIF(KEY3(1:5)=='SIGYZ'.OR.KEY3(1:5)=='SIGZY')THEN
         ANIM_N(IADG+7) = 1
       ELSEIF(KEY3(1:5)=='SIGZX'.OR.KEY3(1:5)=='SIGXZ')THEN
         ANIM_N(IADG+8) = 1
       ELSEIF(KEY3(1:5)=='SHELL')THEN
         IF(KEY4(1:5)=='UPPER')THEN
           ANIM_N(IADG+9) = 1
           ANIM_N(IADG+10) = 1
           ANIM_N(IADG+11) = 1
           ANIM_N(IADG+12) = 1
           ANIM_N(IADG+13) = 1
           ANIM_N(IADG+14) = 1
         ELSEIF(KEY4(1:5)=='LOWER')THEN
           ANIM_N(IADG+15) = 1
           ANIM_N(IADG+16) = 1
           ANIM_N(IADG+17) = 1
           ANIM_N(IADG+18) = 1
           ANIM_N(IADG+19) = 1
           ANIM_N(IADG+20) = 1
         ELSE
          IXITKEY=IXITKEY+1
         ENDIF
       ELSE
         IXITKEY=IXITKEY+1
       ENDIF
      ELSEIF(KEY2(1:3)=='GPS')THEN
      IADG = IAD_GPS+200
       IF(KEY3(1:4)=='TENS')THEN
         ANIM_N(IADG+1) = 1
         ANIM_N(IADG+2) = 1
         ANIM_N(IADG+3) = 1
         ANIM_N(IADG+4) = 1
         ANIM_N(IADG+5) = 1
         ANIM_N(IADG+6) = 1
       ELSEIF(KEY3(1:5)=='SIGX ')THEN
         ANIM_N(IADG+1) = 1
       ELSEIF(KEY3(1:5)=='SIGY ')THEN
         ANIM_N(IADG+2) = 1
       ELSEIF(KEY3(1:5)=='SIGZ ')THEN
         ANIM_N(IADG+3) = 1
       ELSEIF(KEY3(1:5)=='SIGXY'.OR.KEY3(1:5)=='SIGYX')THEN
         ANIM_N(IADG+4) = 1
       ELSEIF(KEY3(1:5)=='SIGYZ'.OR.KEY3(1:5)=='SIGZY')THEN
         ANIM_N(IADG+5) = 1
       ELSEIF(KEY3(1:5)=='SIGZX'.OR.KEY3(1:5)=='SIGXZ')THEN
         ANIM_N(IADG+6) = 1
c
       ELSEIF(KEY3(1:5)=='STRES')THEN
         IADG = IAD_GPS+200
         IF(KEY4(1:4)=='TENS')THEN
           ANIM_N(IADG+1) = 1
           ANIM_N(IADG+2) = 1
           ANIM_N(IADG+3) = 1
           ANIM_N(IADG+4) = 1
           ANIM_N(IADG+5) = 1
           ANIM_N(IADG+6) = 1
         ELSEIF(KEY4(1:5)=='SIGX ')THEN
           ANIM_N(IADG+1) = 1
         ELSEIF(KEY4(1:5)=='SIGY ')THEN
           ANIM_N(IADG+2) = 1
         ELSEIF(KEY4(1:5)=='SIGZ ')THEN
           ANIM_N(IADG+3) = 1
         ELSEIF(KEY4(1:5)=='SIGXY'.OR.KEY3(1:5)=='SIGYX')THEN
           ANIM_N(IADG+4) = 1
         ELSEIF(KEY4(1:5)=='SIGYZ'.OR.KEY3(1:5)=='SIGZY')THEN
           ANIM_N(IADG+5) = 1
         ELSEIF(KEY4(1:5)=='SIGZX'.OR.KEY3(1:5)=='SIGXZ')THEN
           ANIM_N(IADG+6) = 1
         ELSE
           IXITKEY=IXITKEY+1
         ENDIF
c
       ELSEIF(KEY3(1:5)=='STRAI')THEN
         IADG = IAD_GPS+400
         IF(KEY4(1:4)=='TENS')THEN
           ANIM_N(IADG+1) = 1
           ANIM_N(IADG+2) = 1
           ANIM_N(IADG+3) = 1
           ANIM_N(IADG+4) = 1
           ANIM_N(IADG+5) = 1
           ANIM_N(IADG+6) = 1
         ELSEIF(KEY4(1:5)=='SIGX ')THEN
           ANIM_N(IADG+1) = 1
         ELSEIF(KEY4(1:5)=='SIGY ')THEN
           ANIM_N(IADG+2) = 1
         ELSEIF(KEY4(1:5)=='SIGZ ')THEN
           ANIM_N(IADG+3) = 1
         ELSEIF(KEY4(1:5)=='SIGXY'.OR.KEY3(1:5)=='SIGYX')THEN
           ANIM_N(IADG+4) = 1
         ELSEIF(KEY4(1:5)=='SIGYZ'.OR.KEY3(1:5)=='SIGZY')THEN
           ANIM_N(IADG+5) = 1
         ELSEIF(KEY4(1:5)=='SIGZX'.OR.KEY3(1:5)=='SIGXZ')THEN
           ANIM_N(IADG+6) = 1
         ELSE
           IXITKEY=IXITKEY+1
         ENDIF
       ELSE
         IXITKEY=IXITKEY+1
       ENDIF
      ELSEIF(KEY2(1:5)=='ISOST')THEN
      IADG = IAD_GPS+300
       IF(KEY3(1:4)=='TENS')THEN
         ANIM_N(IADG+1) = 1
         ANIM_N(IADG+2) = 1
         ANIM_N(IADG+3) = 1
         ANIM_N(IADG+4) = 1
         ANIM_N(IADG+5) = 1
         ANIM_N(IADG+6) = 1
       ELSEIF(KEY3(1:5)=='SIGX ')THEN
         ANIM_N(IADG+1) = 1
       ELSEIF(KEY3(1:5)=='SIGY ')THEN
         ANIM_N(IADG+2) = 1
       ELSEIF(KEY3(1:5)=='SIGZ ')THEN
         ANIM_N(IADG+3) = 1
       ELSEIF(KEY3(1:5)=='SIGXY'.OR.KEY3(1:5)=='SIGYX')THEN
         ANIM_N(IADG+4) = 1
       ELSEIF(KEY3(1:5)=='SIGYZ'.OR.KEY3(1:5)=='SIGZY')THEN
         ANIM_N(IADG+5) = 1
       ELSEIF(KEY3(1:5)=='SIGZX'.OR.KEY3(1:5)=='SIGXZ')THEN
         ANIM_N(IADG+6) = 1
       ELSE
         IXITKEY=IXITKEY+1
       ENDIF
C-----------------------------
C    NODAL SCALAIRE
C-----------------------------
      ELSEIF(KEY2(1:4)=='NODA')THEN
       IF    (KEY3(1:2)=='DT')THEN
         ANIM_N(1) = 1
       ELSEIF(KEY3(1:4)=='DMAS')THEN
         ANIM_N(2) = 1
       ELSEIF(KEY3(1:1)=='P')THEN
         ANIM_N(3) = 1
       ELSEIF(KEY3(1:4)=='DENS')THEN
         ANIM_N(4) = 1
       ELSEIF(KEY3(1:4)=='ENER')THEN
         ANIM_N(5) = 1
       ELSEIF(KEY3(1:4)=='TEMP')THEN
         ANIM_N(6) = 1
       ELSEIF(KEY3(1:5)=='NVAR1') THEN
         ANIM_N(7) = 1
       ELSEIF(KEY3(1:5)=='NVAR2') THEN
         ANIM_N(8) = 1
       ELSEIF(KEY3(1:5)=='NVAR3') THEN
         ANIM_N(9) = 1
       ELSEIF(KEY3(1:5)=='NVAR4') THEN
         ANIM_N(10) = 1
       ELSEIF(KEY3(1:5)=='NVAR5') THEN
         ANIM_N(11) = 1
       ELSEIF(KEY3(1:5)=='DINER')THEN
         ANIM_N(12) = 1
       ELSEIF(KEY3(1:4)=='FPOT')THEN
         ANIM_N(13) = 1
       ELSEIF(KEY3(1:5)=='NDMAS')THEN
         ANIM_N(14) = 1
       ELSEIF(KEY3(1:5)=='DAMA2')THEN
         ANIM_N(15) = 1
         ANIM_N(16) = 1
       ELSEIF(KEY3(1:5)=='SCHLI')THEN
         ANIM_N(17) = 0 !currently unpluged
       ELSEIF(KEY3(1:5)=='STIFR')THEN
         ANIM_N(18) = 1  
       ELSEIF(KEY3(1:4)=='STIF')THEN
         ANIM_N(19) = 1  
       ELSEIF(KEY3(1:5)=='VFRAC')THEN
         ANIM_N(20) = 1     
         ANIM_N(21) = 1   
         ANIM_N(22) = 1   
         ANIM_N(23) = 1 
       ELSEIF(KEY3(1:5)=='ZVFRA')THEN !inter22
         ANIM_N(24) = 1     
         ANIM_N(25) = 1   
         ANIM_N(26) = 1   
         ANIM_N(27) = 1 
       ELSEIF(KEY3(1:4)=='ZVOL')THEN !inter22
         ANIM_N(28) = 1     
         ANIM_N(29) = 1  
       ELSEIF(KEY3(1:3)=='SSP')THEN
         ANIM_N(30) = 1                                                                            
       ELSE
         IXITKEY=IXITKEY+1
       ENDIF
      ELSEIF(KEY2(1:4)=='MASS')THEN
         ANIM_M = 1
         IF (LAW51_PHASE1) THEN
            ANIM_SE(4926) = 1 !MASS_law51-phase1
            ANIM_CE(11918) = 1
         ENDIF
         IF (LAW51_PHASE2) THEN 
            ANIM_SE(4927) = 1 !MASS_law51-phase2
            ANIM_CE(11919) = 1
         ENDIF
         IF (LAW51_PHASE3) THEN  
            ANIM_SE(4928) = 1 !MASS_law51-phase3
            ANIM_CE(11920) = 1
         ENDIF
         IF (LAW51_PHASE4) THEN 
            ANIM_SE(4929) = 1 !MASS_law51-phase4 
            ANIM_CE(11921) = 1
         ENDIF      
      ELSEIF(KEY2(1:4)=='NUMB')THEN
         ANIM_U = 1
      ELSEIF(KEY2(1:4)=='KEEP')THEN
         ANIM_K = 1
      ELSEIF(KEY2(1:5)=='ALEBA')THEN
       IFVANI=1
       IF(KEY3(1:4)=='MASS')THEN
          ANIM_CE(2145) = 1
       ELSEIF(KEY3(1:4)=='PRES')THEN
          ANIM_CE(2146) = 1
       ELSEIF(KEY3(1:4)=='MOMX')THEN
          ANIM_CE(2147) = 1
       ELSEIF(KEY3(1:4)=='MOMY')THEN
          ANIM_CE(2148) = 1
       ELSEIF(KEY3(1:4)=='MOMZ')THEN
          ANIM_CE(2149) = 1
       ELSEIF(KEY3(1:4)=='DENS')THEN
          ANIM_CE(2150) = 1
       ELSEIF(KEY3(1:4)=='ENER')THEN
          ANIM_CE(2151) = 1
       ELSEIF(KEY3(1:4)=='SSP')THEN
          ANIM_CE(2152) = 1
       ELSEIF(KEY3(1:4)=='GAMA')THEN
          ANIM_CE(2153) = 1
       ELSEIF(KEY3(1:4)=='VISU')THEN
          ANIM_CE(2154) = 1
       ENDIF
      ELSE
        IXITKEY=IXITKEY+1
      ENDIF
C
      RETURN
      END
