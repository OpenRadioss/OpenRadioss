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
Chd|  KINSET                        source/constraints/general/kinset.F
Chd|-- called by -----------
Chd|        CHECKRBY                      source/constraints/general/rbody/checkrby.F
Chd|        HM_READ_BCS                   source/constraints/general/bcs/hm_read_bcs.F
Chd|        HM_READ_CYLJOINT              source/constraints/general/cyl_joint/hm_read_cyljoint.F
Chd|        HM_READ_GJOINT                source/constraints/general/gjoint/hm_read_gjoint.F
Chd|        HM_READ_IMPACC                source/constraints/general/impvel/hm_read_impacc.F
Chd|        HM_READ_LINK                  source/constraints/rigidlink/hm_read_rlink.F
Chd|        HM_READ_MPC                   source/constraints/general/mpc/hm_read_mpc.F
Chd|        HM_READ_NBCS                  source/constraints/general/bcs/hm_read_nbcs.F
Chd|        HM_READ_RBE2                  source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        HM_READ_RBE3                  source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        HM_READ_RBODY_LAGMUL          source/constraints/general/rbody/hm_read_rbody_lagmul.F
Chd|        HM_READ_RWALL_CYL             source/constraints/general/rwall/hm_read_rwall_cyl.F
Chd|        HM_READ_RWALL_LAGMUL          source/constraints/general/rwall/hm_read_rwall_lagmul.F
Chd|        HM_READ_RWALL_PARAL           source/constraints/general/rwall/hm_read_rwall_paral.F
Chd|        HM_READ_RWALL_PLANE           source/constraints/general/rwall/hm_read_rwall_plane.F
Chd|        HM_READ_RWALL_SPHER           source/constraints/general/rwall/hm_read_rwall_spher.F
Chd|        HM_READ_RWALL_THERM           source/constraints/general/rwall/hm_read_rwall_therm.F
Chd|        I1TID3                        source/interfaces/inter3d1/i1tid3.F
Chd|        I2TID3                        source/interfaces/inter3d1/i2tid3.F
Chd|        ITAGSL12                      source/interfaces/inter3d1/inintr2.F
Chd|        ITAGSL2                       source/interfaces/inter3d1/itagsl2.F
Chd|        READ_IMPDISP                  source/constraints/general/impvel/read_impdisp.F
Chd|        READ_IMPVEL                   source/constraints/general/impvel/read_impvel.F
Chd|        READ_IMPVEL_LAGMUL            source/constraints/general/impvel/read_impvel_lagmul.F
Chd|        RIVET0                        source/elements/reader/rivet0.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE KINSET(IK,NODE,IKINE,IDIR,ISK,IKINE1)
      USE MESSAGE_MOD
C
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
#include      "scr03_c.inc"
#include      "scr10_c.inc"
#include      "scr17_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
#include      "kincod_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IK,NODE,IDIR,ISK,IKINE(*),IKINE1(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER*200 TYPE
      CHARACTER*20 DIRECT
      CHARACTER*ncharline ERR_CATEGORY_TMP

      INTEGER JWARN, ISKWT, ISKWR, ITRAN, IROTA, NK, I,
     .        JDIR(7), KDIR(7,3),LTYPE, LTYPEB, LDIRECT
      INTEGER JWARN1, ISKWT1, ISKWR1, ITRAN1, IROTA1
C-----
      DATA JDIR/1,2,4,1,2,4,7/
      DATA KDIR/1,0,1,0,1,0,1,
     .          0,1,1,0,0,1,1,
     .          0,0,0,1,1,1,1/
C-----------------------------------------------------------------
      JWARN = 0
C
      ERR_CATEGORY_TMP=ERR_CATEGORY
      ERR_CATEGORY='KINEMATIC CONDITIONS'
C
      IF(IDIR<=3)THEN
C       TRANSLATION
        ISKWT = IKINE(1+NUMNOD)/10
        ITRAN = IKINE(1+NUMNOD) - 10*ISKWT
        IF(ITRAN==0)THEN
C         LIBRE DANS LES 3 DIRECTIONS
          IKINE(1+NUMNOD) = JDIR(IDIR)+10*ISK
        ELSEIF(ISKWT==ISK.AND.KDIR(ITRAN,IDIR)==0)THEN
C         LIBRE SUR LA DIRECTION DANS LE MEME SKEW
          IKINE(1+NUMNOD) = IKINE(1+NUMNOD)+JDIR(IDIR)
        ELSEIF(KDIR(ITRAN,IDIR)==0)THEN
          JWARN = 1
          IKINE(1+NUMNOD) = IKINE(1+NUMNOD)+JDIR(IDIR)
        ELSEIF(ISKWT==ISK)THEN
C         FIXE SUR LA DIRECTION DANS LE MEME SKEW (ERROR)
          JWARN = 1
        ELSE
          JWARN = 1
        ENDIF
      ELSEIF(IDIR<=6)THEN
C       ROTATION
        ISKWR = IKINE(1+2*NUMNOD)/10
        IROTA = IKINE(1+2*NUMNOD) - 10*ISKWR
        IF(IROTA==0)THEN
C         LIBRE DANS LES 3 DIRECTIONS
          IKINE(1+2*NUMNOD) = JDIR(IDIR)+10*ISK
        ELSEIF(ISKWR==ISK.AND.KDIR(IROTA,IDIR-3)==0)THEN
C         LIBRE SUR LA DIRECTION DANS LE MEME SKEW
          IKINE(1+2*NUMNOD) = IKINE(1+2*NUMNOD)+JDIR(IDIR)
        ELSEIF(KDIR(IROTA,IDIR-3)==0)THEN
          JWARN = 1
          IKINE(1+2*NUMNOD) = IKINE(1+2*NUMNOD)+JDIR(IDIR)
        ELSEIF(ISKWR==ISK)THEN
C         FIXE SUR LA DIRECTION DANS LE MEME SKEW (ERROR)
          JWARN = 1
        ELSE
          JWARN = 1
        ENDIF
      ELSE
c       lagrange multipliers (IDIR = 7)    
        ISKWT = IKINE(1+  NUMNOD)/10
        ISKWR = IKINE(1+2*NUMNOD)/10
        ITRAN = IKINE(1+  NUMNOD) - 10*ISKWT
        IROTA = IKINE(1+2*NUMNOD) - 10*ISKWR
        IF(ITRAN/=0.AND.ITRAN/=7.OR.IROTA/=0.AND.IROTA/=7) THEN
          JWARN = 1
          IKINE(1+  NUMNOD) = IKINE(1+  NUMNOD)+JDIR(IDIR)
          IKINE(1+2*NUMNOD) = IKINE(1+2*NUMNOD)+JDIR(IDIR)
        ELSE
          IKINE(1+  NUMNOD) = IKINE(1+  NUMNOD)+JDIR(IDIR)
          IKINE(1+2*NUMNOD) = IKINE(1+2*NUMNOD)+JDIR(IDIR)
        ENDIF
      ENDIF
C---------------------------------------------------------------
C  CONDITION CINEMATIQUE INCOMPATIBLES ENTRE 2
C  ENTITES DU MEME TYPE
C---------------------------------------------------------------    
      JWARN1 = 0
C
        IF(IDIR<=3)THEN
C         TRANSLATION
          ISKWT1 = IKINE1(1+NUMNOD)/10
          ITRAN1 = IKINE1(1+NUMNOD) - 10*ISKWT1
          IF(ITRAN1==0)THEN
C           LIBRE DANS LES 3 DIRECTIONS
            IKINE1(1+NUMNOD) = JDIR(IDIR)+10*ISK
          ELSEIF(ISKWT1==ISK.AND.KDIR(ITRAN1,IDIR)==0)THEN
C           LIBRE SUR LA DIRECTION DANS LE MEME SKEW
            IKINE1(1+NUMNOD) = IKINE1(1+NUMNOD)+JDIR(IDIR)
          ELSEIF(KDIR(ITRAN1,IDIR)==0)THEN
            JWARN1 = 1
            IKINE1(1+NUMNOD) = IKINE1(1+NUMNOD)+JDIR(IDIR)
          ELSEIF(ISKWT1==ISK)THEN
C           FIXE SUR LA DIRECTION DANS LE MEME SKEW (ERROR)
            JWARN1 = 1
          ELSE
            JWARN1 = 1
          ENDIF
        ELSEIF(IDIR<=6)THEN
C         ROTATION
          ISKWR1 = IKINE1(1+2*NUMNOD)/10
          IROTA1 = IKINE1(1+2*NUMNOD) - 10*ISKWR1
          IF(IROTA1==0)THEN
C           LIBRE DANS LES 3 DIRECTIONS
            IKINE1(1+2*NUMNOD) = JDIR(IDIR)+10*ISK
          ELSEIF(ISKWR1==ISK.AND.KDIR(IROTA1,IDIR-3)==0)THEN
C           LIBRE SUR LA DIRECTION DANS LE MEME SKEW
            IKINE1(1+2*NUMNOD) = IKINE1(1+2*NUMNOD)+JDIR(IDIR)
          ELSEIF(KDIR(IROTA1,IDIR-3)==0)THEN
            JWARN1 = 1
            IKINE1(1+2*NUMNOD) = IKINE1(1+2*NUMNOD)+JDIR(IDIR)
          ELSEIF(ISKWR1==ISK)THEN
C           FIXE SUR LA DIRECTION DANS LE MEME SKEW (ERROR)
            JWARN1 = 1
          ELSE
            JWARN1 = 1
          ENDIF
        ELSE
C         lagrange multipliers (IDIR = 7)     
          ISKWT1 = IKINE1(1+  NUMNOD)/10
          ISKWR1 = IKINE1(1+2*NUMNOD)/10
          ITRAN1 = IKINE1(1+  NUMNOD) - 10*ISKWT1
          IROTA1 = IKINE1(1+2*NUMNOD) - 10*ISKWR1
          IF(ITRAN1/=0.AND.ITRAN1/=7.OR.IROTA1/=0
     .       .AND.IROTA1/=7) THEN
            JWARN1 = 1
            IKINE1(1+  NUMNOD) = IKINE1(1+  NUMNOD)+JDIR(IDIR)
            IKINE1(1+2*NUMNOD) = IKINE1(1+2*NUMNOD)+JDIR(IDIR)
          ELSE
            IKINE1(1+  NUMNOD) = IKINE1(1+  NUMNOD)+JDIR(IDIR)
            IKINE1(1+2*NUMNOD) = IKINE1(1+2*NUMNOD)+JDIR(IDIR)
          ENDIF
        ENDIF
C
C---------------------------------------------------------------
C  CONDITIONS CINEMATIQUES INCOMPATIBLES ENTRE 2
C  ENTITES DE MEME TYPE
C
C   TYPECOND(IKINE(1+3*NUMNOD)) = 1
C   SI AU MOINS 2 CONDITIONS DE TYPE TYPCOND SUR CE NOEUD
C--------------------------------------------------------------- 
        IF (JWARN1 == 1) THEN
          IF(IK==1)THEN
            IF(IBC(IKINE(1))== 1 .AND. IBC(IKINE(1+3*NUMNOD))== 0)
     .              IKINE(1+3*NUMNOD) = IKINE(1+3*NUMNOD) + 1
          ELSEIF(IK==2)THEN     ! interface
            IF(ITF(IKINE(1))== 1 .AND. ITF(IKINE(1+3*NUMNOD))== 0) 
     .              IKINE(1+3*NUMNOD) = IKINE(1+3*NUMNOD) + 2
          ELSEIF(IK==4)THEN
            IF(IWL(IKINE(1))== 1 .AND. IWL(IKINE(1+3*NUMNOD))== 0)
     .              IKINE(1+3*NUMNOD) = IKINE(1+3*NUMNOD) + 4
          ELSEIF(IK==8)THEN
            IF(IRB(IKINE(1))== 1 .AND. IRB(IKINE(1+3*NUMNOD))== 0)
     .              IKINE(1+3*NUMNOD) = IKINE(1+3*NUMNOD) + 8
          ELSEIF(IK==16)THEN
            IF(IVF(IKINE(1))== 1 .AND. IVF(IKINE(1+3*NUMNOD))== 0)
     .              IKINE(1+3*NUMNOD) = IKINE(1+3*NUMNOD) + 16
          ELSEIF(IK==32)THEN
            IF(IRV(IKINE(1))== 1 .AND. IRV(IKINE(1+3*NUMNOD))== 0)
     .              IKINE(1+3*NUMNOD) = IKINE(1+3*NUMNOD) + 32
          ELSEIF(IK==64)THEN
            IF(IJO(IKINE(1))== 1 .AND. IJO(IKINE(1+3*NUMNOD))== 0)
     .              IKINE(1+3*NUMNOD) = IKINE(1+3*NUMNOD) + 64
          ELSEIF(IK==128)THEN
            IF(IRB2(IKINE(1))== 1 .AND. IRB2(IKINE(1+3*NUMNOD))== 0)
     .              IKINE(1+3*NUMNOD) = IKINE(1+3*NUMNOD) + 128
          ELSEIF(IK==256)THEN
            IF(IRBM(IKINE(1))== 1 .AND. IRBM(IKINE(1+3*NUMNOD))== 0)
     .              IKINE(1+3*NUMNOD) = IKINE(1+3*NUMNOD) + 256
          ELSEIF(IK==512)THEN
            IF(ILMULT(IKINE(1))==1.AND.ILMULT(IKINE(1+3*NUMNOD))==0)
     .              IKINE(1+3*NUMNOD) = IKINE(1+3*NUMNOD) + 512
          ELSEIF(IK==1024)THEN
           IF(IRLK(IKINE(1))== 1 .AND. IRLK(IKINE(1+3*NUMNOD))== 0)
     .              IKINE(1+3*NUMNOD) = IKINE(1+3*NUMNOD) + 1024
          ELSEIF(IK==2048)THEN
           IF(IKRBE2(IKINE(1))==1.AND.IKRBE2(IKINE(1+3*NUMNOD))== 0)
     .              IKINE(1+3*NUMNOD) = IKINE(1+3*NUMNOD) + 2048
          ELSEIF(IK==4096)THEN
           IF(IKRBE3(IKINE(1))==1.AND.IKRBE3(IKINE(1+3*NUMNOD))== 0)
     .              IKINE(1+3*NUMNOD) = IKINE(1+3*NUMNOD) + 4096
          ENDIF
        ENDIF
C---------------------------------------------------------------
C  CONDITIONS CINEMATIQUES INCOMPATIBLES ENTRE 2
C  ENTITES DE TYPES DIFFERENTS
C
C SI IKINE(1+4*NUMNOD) = 0 :  PAS DE CONDITIONS INCOMPATIBLES
C DE DIFFERENTS TYPES SUR CE NOEUD
C---------------------------------------------------------------    
        IF(IK==1)THEN
          IF(IBC(IKINE(1))==0 .AND. IBC(IKINE(1+4*NUMNOD))==0
     .       .AND. JWARN==1)
     .        IKINE(1+4*NUMNOD) = IKINE(1+4*NUMNOD) + 1
        ELSEIF(IK==2)THEN
          IF(ITF(IKINE(1))==0 .AND. ITF(IKINE(1+4*NUMNOD))==0
     .       .AND. JWARN==1)
     .        IKINE(1+4*NUMNOD) = IKINE(1+4*NUMNOD) + 2
        ELSEIF(IK==4)THEN
          IF(IWL(IKINE(1))==0 .AND. IWL(IKINE(1+4*NUMNOD))==0
     .       .AND. JWARN==1)
     .        IKINE(1+4*NUMNOD) = IKINE(1+4*NUMNOD) + 4
        ELSEIF(IK==8)THEN
          IF(IRB(IKINE(1))==0 .AND. IRB(IKINE(1+4*NUMNOD))==0
     .       .AND. JWARN==1)
     .        IKINE(1+4*NUMNOD) = IKINE(1+4*NUMNOD) + 8
        ELSEIF(IK==16)THEN
          IF(IVF(IKINE(1))==0 .AND. IVF(IKINE(1+4*NUMNOD))==0
     .       .AND. JWARN==1)
     .        IKINE(1+4*NUMNOD) = IKINE(1+4*NUMNOD) + 16
        ELSEIF(IK==32)THEN
          IF(IRV(IKINE(1))==0 .AND. IRV(IKINE(1+4*NUMNOD))==0
     .       .AND. JWARN==1)
     .        IKINE(1+4*NUMNOD) = IKINE(1+4*NUMNOD) + 32
        ELSEIF(IK==64)THEN
          IF(IJO(IKINE(1))==0 .AND. IJO(IKINE(1+4*NUMNOD))==0
     .       .AND. JWARN==1)
     .        IKINE(1+4*NUMNOD) = IKINE(1+4*NUMNOD) + 64
        ELSEIF(IK==128)THEN
          IF(IRB2(IKINE(1))==0 .AND. IRB2(IKINE(1+4*NUMNOD))==0
     .       .AND. JWARN==1)
     .        IKINE(1+4*NUMNOD) = IKINE(1+4*NUMNOD) + 128
        ELSEIF(IK==256)THEN
          IF(IRBM(IKINE(1))==0 .AND. IRBM(IKINE(1+4*NUMNOD))==0
     .       .AND. JWARN==1)
     .        IKINE(1+4*NUMNOD) = IKINE(1+4*NUMNOD) + 256
        ELSEIF(IK==512)THEN
          IF(ILMULT(IKINE(1))==0 .AND. ILMULT(IKINE(1+4*NUMNOD))==0
     .       .AND. JWARN==1)
     .        IKINE(1+4*NUMNOD)=IKINE(1+4*NUMNOD) + 512
        ELSEIF(IK==1024)THEN
         IF(IRLK(IKINE(1))==0 .AND. IRLK(IKINE(1+4*NUMNOD))==0
     .       .AND. JWARN==1)
     .        IKINE(1+4*NUMNOD)=IKINE(1+4*NUMNOD) + 1024
        ELSEIF(IK==2048)THEN
         IF(IKRBE2(IKINE(1))==0 .AND. IKRBE2(IKINE(1+4*NUMNOD))==0
     .       .AND. JWARN==1)
     .        IKINE(1+4*NUMNOD)=IKINE(1+4*NUMNOD) + 2048
        ELSEIF(IK==4096)THEN
         IF(IKRBE3(IKINE(1))==0 .AND. IKRBE3(IKINE(1+4*NUMNOD))==0
     .       .AND. JWARN==1)
     .        IKINE(1+4*NUMNOD)=IKINE(1+4*NUMNOD) + 4096
        ENDIF
C
        IF(IK==1)THEN
          IF(IBC(IKINE(1))==0)IKINE(1) = IKINE(1) + 1
       ELSEIF(IK==2)THEN
          IF(ITF(IKINE(1))==0)IKINE(1) = IKINE(1) + 2
        ELSEIF(IK==4)THEN
          IF(IWL(IKINE(1))==0)IKINE(1) = IKINE(1) + 4
        ELSEIF(IK==8)THEN
          IF(IRB(IKINE(1))==0)IKINE(1) = IKINE(1) + 8
        ELSEIF(IK==16)THEN
          IF(IVF(IKINE(1))==0)IKINE(1) = IKINE(1) + 16
        ELSEIF(IK==32)THEN
          IF(IRV(IKINE(1))==0)IKINE(1) = IKINE(1) + 32
        ELSEIF(IK==64)THEN
          IF(IJO(IKINE(1))==0)IKINE(1) = IKINE(1) + 64
        ELSEIF(IK==128)THEN
          IF(IRB2(IKINE(1))==0)IKINE(1) = IKINE(1) + 128
        ELSEIF(IK==256)THEN
          IF(IRBM(IKINE(1))==0)IKINE(1) = IKINE(1) + 256
        ELSEIF(IK==512)THEN
          IF(ILMULT(IKINE(1))==0)IKINE(1)=IKINE(1) + 512
        ELSEIF(IK==1024)THEN
         IF(IRLK(IKINE(1))==0)IKINE(1)=IKINE(1) + 1024
        ELSEIF(IK==2048)THEN
         IF(IKRBE2(IKINE(1))==0)IKINE(1)=IKINE(1) + 2048
        ELSEIF(IK==4096)THEN
         IF(IKRBE3(IKINE(1))==0)IKINE(1)=IKINE(1) + 4096
        ENDIF
C
        NK = IBC(IKINE(1))+ITF(IKINE(1))+IWL(IKINE(1))+
     .       IRB(IKINE(1))+IRB2(IKINE(1))+
     .       IVF(IKINE(1))+IRV(IKINE(1))+IJO(IKINE(1))+
     .       IRBM(IKINE(1))+ILMULT(IKINE(1))+IRLK(IKINE(1))+
     .       IKRBE2(IKINE(1))+IKRBE3(IKINE(1))
        IF(NK==1)NK=2
C
      IF (IWL(IKINE(1))/=1 .OR. IRB(IKINE(1))/=1) THEN
        KWARN = KWARN + JWARN
      ENDIF
C
      IF(JWARN==1)THEN
C
         LTYPE = 0
         TYPE = ' '
C
C WARNING WHILE ADDING AN OPTION : TYPE MUST BE LONG ENOUGH
C
         IF(IBC(IKINE(1))==1) THEN
            LTYPEB = 20
            TYPE((LTYPE+1):(LTYPE+1+LTYPEB)) = '-BOUNDARY CONDITION'
            LTYPE = LTYPE + LTYPEB
         ENDIF
         IF(ITF(IKINE(1))==1) THEN
            LTYPEB = 27
            TYPE((LTYPE+1):(LTYPE+1+LTYPEB))=
     .           '-INTERFACE TYPE 1 2 12 OR 9'
            LTYPE = LTYPE + LTYPEB
         ENDIF
         IF(IWL(IKINE(1))==1)  THEN
            LTYPEB = 12
            TYPE((LTYPE+1):(LTYPE+1+LTYPEB)) = '-RIGID WALL'
            LTYPE = LTYPE + LTYPEB
         ENDIF
         IF(IRB(IKINE(1))==1)  THEN
            LTYPEB = 12
            TYPE((LTYPE+1):(LTYPE+1+LTYPEB)) = '-RIGID BODY'
            LTYPE = LTYPE + LTYPEB
         ENDIF
         IF(IRB2(IKINE(1))==1)  THEN
            LTYPEB = 12
            TYPE((LTYPE+1):(LTYPE+1+LTYPEB))='-RIGID BODY'
            LTYPE = LTYPE + LTYPEB
         ENDIF
         IF(IVF(IKINE(1))==1)  THEN
            LTYPEB = 29
            TYPE((LTYPE+1):(LTYPE+1+LTYPEB))=
     .           '-FIXED VELOCITY/DISPLACEMENT'
            LTYPE = LTYPE + LTYPEB
         ENDIF
         IF(IRV(IKINE(1))==1)  THEN
            LTYPEB = 7
            TYPE((LTYPE+1):(LTYPE+1+LTYPEB))='-RIVET'
            LTYPE = LTYPE + LTYPEB
         ENDIF
         IF(IJO(IKINE(1))==1)  THEN
            LTYPEB = 19
            TYPE((LTYPE+1):(LTYPE+1+LTYPEB))='-CYLINDRICAL JOINT'
            LTYPE = LTYPE + LTYPEB
         ENDIF
         IF(IRBM(IKINE(1))==1)  THEN
            LTYPEB = 23
            TYPE((LTYPE+1):(LTYPE+1+LTYPEB))='-IMPOSED BODY VELOCITY'
            LTYPE = LTYPE + LTYPEB
         ENDIF
         IF(ILMULT(IKINE(1))==1)  THEN
            LTYPEB = 22
            TYPE((LTYPE+1):(LTYPE+1+LTYPEB))='-LAGRANGE MULTIPLIERS'
            LTYPE = LTYPE + LTYPEB
         ENDIF
         IF(IRLK(IKINE(1))==1)  THEN
            LTYPEB =12
            TYPE((LTYPE+1):(LTYPE+1+LTYPEB))='-RIGID LINK'
            LTYPE = LTYPE + LTYPEB
         ENDIF
         IF(IKRBE2(IKINE(1))==1)  THEN
            LTYPEB =6
            TYPE((LTYPE+1):(LTYPE+1+LTYPEB))='-RBE2'
            LTYPE = LTYPE + LTYPEB
         ENDIF
         IF(IKRBE3(IKINE(1))==1)  THEN
            LTYPEB =6
            TYPE((LTYPE+1):(LTYPE+1+LTYPEB))='-RBE3'
            LTYPE = LTYPE + LTYPEB
         ENDIF
C
         IF (IDIR == 1) THEN
            DIRECT = 'TRANSLATION X'
            LDIRECT = 13
         ELSE IF (IDIR == 2) THEN
            DIRECT = 'TRANSLATION Y'
            LDIRECT = 13
         ELSE IF (IDIR == 3) THEN
            DIRECT = 'TRANSLATION Z'
            LDIRECT = 13
         ELSE IF (IDIR == 4) THEN
            DIRECT = 'ROTATION X'
            LDIRECT = 10
         ELSE IF (IDIR == 5) THEN
            DIRECT = 'ROTATION Y'
            LDIRECT = 10
         ELSE IF (IDIR == 6) THEN
            DIRECT = 'ROTATION Z'
            LDIRECT = 10
         ELSE
            DIRECT = 'UNKNOWN'
            LDIRECT = 7
         ENDIF
C
         IF ((IWL(IKINE(1))/=1 .OR. IRB(IKINE(1))/=1) .AND. 
     .        IPRI >= 5) THEN
C
C ANINFO_BLIND_2 : Print Nothing on screen, title + description in file
           CALL ANCMSG(MSGID=147,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=NK,
     .                 I2=NODE,
     .                 C1= DIRECT,
     .                 C2= TYPE)
C
          ENDIF
      ENDIF
c
      ERR_CATEGORY=ERR_CATEGORY_TMP
c-----------
      RETURN
      END
