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
Chd|  R2R_GROUP                     source/coupling/rad2rad/r2r_group.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ALE_CHECK_LAG                 source/ale/ale_check_lag.F    
Chd|        ANCMSG                        source/output/message/message.F
Chd|        CHK_FLG_FSI                   source/coupling/rad2rad/routines_r2r.F
Chd|        NEW_LINK                      source/coupling/rad2rad/new_link.F
Chd|        PRINTCENTER                   source/starter/radioss_title.F
Chd|        R2R_INPUT                     source/coupling/rad2rad/r2r_input.F
Chd|        R2R_PRELEC                    source/coupling/rad2rad/r2r_prelec.F
Chd|        TAGNODS_R2R                   source/coupling/rad2rad/tagnod_r2r.F
Chd|        TAGNOD_R2R                    source/coupling/rad2rad/tagnod_r2r.F
Chd|        TAGNOD_R2R_NL                 source/coupling/rad2rad/tagnod_r2r_nl.F
Chd|        TAGNOD_R2R_S                  source/coupling/rad2rad/tagnod_r2r.F
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        GROUP_MOD                     share/modules1/group_mod.F    
Chd|        INIVOL_ARRAY_MOD              share/modules1/inivol_mod.F   
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        RESTMOD                       share/modules1/restart_mod.F  
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE R2R_GROUP(
     1           NGROU,INNOD,FLAG,IPARTS,
     2           IPARTQ,IPARTC,IPARTT,IPARTP,IPARTR,IPARTG,
     3           IPARTSP,IXS10,IXS20,IXS16,KK,BUF_NOD,IXR_KJ,
     4           INOM_OPT,IPART_L,IAD,NALE_R2R,FLG_R2R_ERR ,
     5           PM_STACK ,IWORKSH  ,IGRBRIC2,IGRQUAD2  ,IGRSH4N2,
     6           IGRSH3N2 ,IGRTRUSS2,IGRBEAM2,IGRSPRING2,IGRNOD2 ,
     7           IGRSURF2 ,IGRSLIN2,LSUBMODEL,ALE_EULER,IGEO_,
     8           NLOC_DMG ,DETONATORS,NSENSOR,SEATBELT_SHELL_TO_SPRING,
     9           NB_SEATBELT_SHELLS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE RESTMOD
      USE R2R_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE GROUP_MOD
      USE INOUTFILE_MOD
      USE SUBMODEL_MOD
      USE INIVOL_ARRAY_MOD
      USE NLOCAL_REG_MOD
      USE DETONATORS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
      INTEGER,INTENT(IN) :: IGEO_(NPROPGI,NUMGEO)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
#include      "r2r_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
      INTEGER ,INTENT(IN) :: NSENSOR
      INTEGER NGROU,
     .   BUF_NOD(*),INNOD,FLAG,KK,
     .   IPARTS(*),IXS10(6,*),IXS20(12,*),
     .   IXS16(8,*),IPARTQ(*),IPARTSP(*),
     .   IPARTC(*),IPARTT(*),IPARTP(*),IPARTR(*),
     .   IPARTG(*),IXR_KJ(*),INOM_OPT(*),IPART_L(*),IAD,
     .   NALE_R2R(*),FLG_R2R_ERR,IWORKSH(*),ALE_EULER
      INTEGER ,INTENT(IN) :: NB_SEATBELT_SHELLS
      INTEGER ,INTENT(IN) :: SEATBELT_SHELL_TO_SPRING(NUMELC,2)
      my_real :: PM_STACK(*)
      TYPE (NLOCAL_STR_) ,INTENT(IN)  :: NLOC_DMG
      TYPE(DETONATOR_STRUCT_),TARGET,INTENT(IN) :: DETONATORS
C-----------------------------------------------
!      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD2
      TYPE (GROUP_)  , DIMENSION(NGROU)   :: IGRNOD2
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC2
      TYPE (GROUP_)  , DIMENSION(NGRQUAD) :: IGRQUAD2
      TYPE (GROUP_)  , DIMENSION(NGRSHEL) :: IGRSH4N2
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N2
      TYPE (GROUP_)  , DIMENSION(NGRTRUS) :: IGRTRUSS2
      TYPE (GROUP_)  , DIMENSION(NGRBEAM) :: IGRBEAM2
      TYPE (GROUP_)  , DIMENSION(NGRSPRI) :: IGRSPRING2
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF2
      TYPE (SURF_)   , DIMENSION(NSLIN)   :: IGRSLIN2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER STAT,I,J,IGR,IGRS,N,NUM,K,ADD,COMPT,IGS,IPID_L
      INTEGER ID_TEMP(NB_PART_SUB),NSUBDOM_LOC,P,TMP_PART(NPART)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IGROUP_TEMP2
      INTEGER N_LNK_C,NI,GRM,GRS,MAIN,IGU,NUL,IAD_TMP,COMPT_T2
      INTEGER MODIF,NINTER_PREC,FAC,IO_ERR,NUM_KJ,NSPCONDN,NSPHION,NN
      INTEGER MEMTR(NUMNOD),FLG_SPH,COUNT,NEW_NSLASH_INT,NEW_HM_NINTER,NEW_NINTSUB,NEW_NINIVOL
      CHARACTER TITR*nchartitle,NAME*100
      INTEGER NGRNOD2,NGRBRIC2,NGRQUAD2,NGRSHEL2,NGRSH3N2,NGRTRUS2,NGRBEAM2,NGRSPRI2,LENGRN,ITITLE(LTITR)
      CHARACTER NEW_TITLE(NGROU+10*NSUBDOM)*nchartitle

      INTEGER, DIMENSION(:), ALLOCATABLE :: IGROUP_TEMP2_BUF,TAG_NLOCAL
      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=4096) :: TMP_NAME
C-----------------------------------------------
      N_LNK_C = 0
      TMP_PART(:)= 0
      MODIF = 1
      INNOD = 0
C----- Storage of IGRN in IGRN Temp------------------------------C
      LENGRN = 9
      IF (FLAG == 1) THEN
        ALLOCATE(IGROUP_TEMP2(10,NGROU+10*NSUBDOM))
        IGROUP_TEMP2 = 0
        COUNT = 0
        DO I=1,NGROU
          COUNT = COUNT + IGRNOD2(I)%NENTITY
        ENDDO
        IF (COUNT > 0) THEN
          ALLOCATE(IGROUP_TEMP2_BUF(COUNT))
          IGROUP_TEMP2_BUF(:) = 0
        ENDIF
        IAD_TMP = 1
!
        DO I=1,NGROU
          IGROUP_TEMP2(1,I) = IGRNOD2(I)%ID         ! IGRN(1,*)
          IGROUP_TEMP2(2,I) = IGRNOD2(I)%NENTITY    ! IGRN(2,*)
          IGROUP_TEMP2(3,I) = IGRNOD2(I)%GRTYPE     ! IGRN(4,*)
          IGROUP_TEMP2(4,I) = IGRNOD2(I)%SORTED     ! IGRN(5,*)
          IGROUP_TEMP2(5,I) = IGRNOD2(I)%GRPGRP     ! IGRN(6,*)
          IGROUP_TEMP2(6,I) = IGRNOD2(I)%LEVEL      ! IGRN(7,*)
          NEW_TITLE(I)      = IGRNOD2(I)%TITLE      ! IGRN(11,*)
          IGROUP_TEMP2(8,I) = IGRNOD2(I)%R2R_ALL    ! IGRN(8,*)
          IGROUP_TEMP2(9,I) = IGRNOD2(I)%R2R_SHARE  ! IGRN(9,*)
          IGROUP_TEMP2(7,I) = IAD_TMP
          DO J=1,IGRNOD2(I)%NENTITY
            IGROUP_TEMP2_BUF(IAD_TMP) = IGRNOD2(I)%ENTITY(J)
            IAD_TMP = IAD_TMP + 1
          ENDDO
        ENDDO
      ENDIF
C--------------------------------------------------------------------C
C------Creation of groups of nodes of multidomains interface---------C
C--------------------------------------------------------------------C
!
      NGRNOD2  = NGRNOD
      NGRBRIC2 = NGRBRIC
      NGRQUAD2 = NGRQUAD
      NGRSHEL2 = NGRSHEL
      NGRSH3N2 = NGRSH3N
      NGRTRUS2 = NGRTRUS
      NGRBEAM2 = NGRBEAM
      NGRSPRI2 = NGRSPRI
!
      IF (FLAG == 1) THEN
        NUM = 1
        IGS = NGROU+1
        DO I=1,NGROU
          IF (NUM<=IGRNOD2(I)%ID) NUM=IGRNOD2(I)%ID+1     
        END DO
      ENDIF
C--------------------------------------------------------------------C
      IF (IPID==0) NSUBDOM = 1
      NSUBDOM_LOC = NSUBDOM
      DO P=1,NSUBDOM_LOC
C-----2 pass : pass 1 - contact nodes / passe 2 - connection nodes
        N = P
        IF (IPID==0) N = IDDOM
        COMPT = 0
C--------------------------------------------------------------------C
C---------------FLAG = 0 --> tag of nodes and counting --------------C
C--------------------------------------------------------------------C
        IF (FLAG==0) THEN
C---------------Reset of out file + printout of new heading ---------C
        IPID_L = IPID
        IF (FLG_SWALE==1) THEN
          IF (IPID==0) IPID_L = 1
          IF (IPID/=0) IPID_L = 0          
        ENDIF
        IF (IPID_L==0) THEN
          CLOSE(UNIT=IOUT, STATUS='DELETE',IOSTAT=IO_ERR)

          TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//R2R_FILNAM(1:LEN_TRIM(R2R_FILNAM))    
          LEN_TMP_NAME = OUTFILE_NAME_LEN+LEN_TRIM(R2R_FILNAM)
          OPEN(UNIT=IOUT,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .    ACCESS='SEQUENTIAL',
     .    FORM='FORMATTED',STATUS='UNKNOWN')
          NAME = "SUBDOMAIN "//R2R_FILNAM(1:(LEN_TRIM(R2R_FILNAM)-9))   
          WRITE (IOUT,'(A)')  ''
          CALL PRINTCENTER(" ",0,IOUT,1)
          CALL PRINTCENTER(" ",0,IOUT,2)   
          CALL PRINTCENTER(NAME,LEN_TRIM(NAME),IOUT,2)
          CALL PRINTCENTER(" ",0,IOUT,2)  
          CALL PRINTCENTER(" ",0,IOUT,1)             
        ENDIF
C---------------Allocation of arrays for tag of elements--------------C
        ALLOCATE(TAG_ELC(NUMELC+NPART),TAG_ELS(NUMELS+NPART))
        ALLOCATE(TAG_ELG(NUMELTG+NPART),TAG_ELSP(NUMSPH+NPART))
        ALLOCATE(TAG_ELR(NUMELR+NPART),TAG_ELT(NUMELT+NPART))     
        ALLOCATE(TAG_ELP(NUMELP+NPART),TAG_ELQ(NUMELQ+NPART))
        TAG_ELS(:)=0
        TAG_ELC(:)=0
        TAG_ELG(:)=0      
        TAG_ELT(:)=0
        TAG_ELP(:)=0
        TAG_ELR(:)=0
        TAG_ELQ(:)=0
        TAG_ELSP(:)=0
C---------------Tag of Parts-------------------------------------------C
        DO K=1,NPART
          TAGNO(K)=0
C          ---> TAGNO(K)=-1 => PART K belongs to an already treated subdomain---	       	 
          IF(TMP_PART(K)==-1) TAGNO(K)=-1  
        ENDDO
        ADD = ISUBDOM(3,N)
        DO K=1,NPART
          DO I=1,ISUBDOM(1,N)        
            IF(K == ISUBDOM_PART(I+ADD))THEN
              TAGNO(K)=1
              TMP_PART(K)=-1  
            ENDIF
          ENDDO	
        END DO
C----------------Full domain - inversion of part selection-------------C
        IF (IDDOM == 0) THEN
          DO K=1,NPART
            IF(TAGNO(K)==1) THEN
              TAGNO(K)=0 
            ELSEIF(TAGNO(K)==0) THEN
              TAGNO(K)=1
            ENDIF  
          ENDDO
        ENDIF
C---------------> TAG OF PARTS : ----------------------------------	  	  
C---------------> TAGNO(K) = -1 -> Part of already treated subdmain
C---------------> TAGNO(K) = 0 -> Internal part of subdomain
C---------------> TAGNO(K) = 1 -> External part of subdomain

C---------------> TAG OF NODES : ----------------------------------	  	  
C---------------> TAGNO(K) = -1 -> external node
C---------------> TAGNO(K) = 0 -> free node (not attached to any domain)
C---------------> TAGNO(K) = 1 -> internal node
C---------------> TAGNO(K) = 2 -> coupled node
C---------------> TAGNO(K) = 3 -> coupled node -> main node of RBODY
C---------------> TAGNO(K) = 4 -> coupled node for contacts
C---------------Detag of already treated nodes----------------------C
        CALL TAGNODS_R2R(IXS,IXS10,IXS20,IXS16,IPARTS,TAGNO,-1,N)       
        CALL TAGNOD_R2R(IXQ,NIXQ,2,5,NUMELQ,IPARTQ,TAGNO,NPART,-1,N)
        CALL TAGNOD_R2R(IXC,NIXC,2,5,NUMELC,IPARTC,TAGNO,NPART,-1,N)
        CALL TAGNOD_R2R(IXT,NIXT,2,3,NUMELT,IPARTT,TAGNO,NPART,-1,N)
        CALL TAGNOD_R2R(IXP,NIXP,2,4,NUMELP,IPARTP,TAGNO,NPART,-1,N)
        CALL TAGNOD_R2R(IXR,NIXR,2,3,NUMELR,IPARTR,TAGNO,NPART,-1,N)
        CALL TAGNOD_R2R(IXTG,NIXTG,2,4,NUMELTG,IPARTG,TAGNO,NPART,-1,N)
        CALL TAGNOD_R2R(KXSP,NISP,3,3,NUMSPH,IPARTSP,TAGNO,NPART,-1,N)
C---------------Tag of nodes -> pass 1------------------------------C
        CALL TAGNODS_R2R(IXS,IXS10,IXS20,IXS16,IPARTS,TAGNO,0,N)
        CALL TAGNOD_R2R(IXQ,NIXQ,2,5,NUMELQ,IPARTQ,TAGNO,NPART,0,N)
        CALL TAGNOD_R2R(IXC,NIXC,2,5,NUMELC,IPARTC,TAGNO,NPART,0,N)
        CALL TAGNOD_R2R(IXT,NIXT,2,3,NUMELT,IPARTT,TAGNO,NPART,0,N)
        CALL TAGNOD_R2R(IXP,NIXP,2,3,NUMELP,IPARTP,TAGNO,NPART,0,N)
        CALL TAGNOD_R2R(IXR,NIXR,2,3,NUMELR,IPARTR,TAGNO,NPART,0,N)
        CALL TAGNOD_R2R(IXTG,NIXTG,2,4,NUMELTG,IPARTG,TAGNO,NPART,0,N)
        CALL TAGNOD_R2R(KXSP,NISP,3,3,NUMSPH,IPARTSP,TAGNO,NPART,0,N)
C---------------Tag of nodes -> pass 2-------------------------------C
        CALL TAGNODS_R2R(IXS,IXS10,IXS20,IXS16,IPARTS,TAGNO,1,N)
        CALL TAGNOD_R2R(IXQ,NIXQ,2,5,NUMELQ,IPARTQ,TAGNO,NPART,1,N)
        CALL TAGNOD_R2R(IXC,NIXC,2,5,NUMELC,IPARTC,TAGNO,NPART,1,N)
        CALL TAGNOD_R2R(IXT,NIXT,2,3,NUMELT,IPARTT,TAGNO,NPART,1,N)
        CALL TAGNOD_R2R(IXP,NIXP,2,3,NUMELP,IPARTP,TAGNO,NPART,1,N)
        CALL TAGNOD_R2R(IXR,NIXR,2,3,NUMELR,IPARTR,TAGNO,NPART,1,N)
        CALL TAGNOD_R2R(IXTG,NIXTG,2,4,NUMELTG,IPARTG,TAGNO,NPART,1,N)
        CALL TAGNOD_R2R(KXSP,NISP,3,3,NUMSPH,IPARTSP,TAGNO,NPART,1,N)
C---------------Tag of 3rd nodes of beams/springs -> pass 3----------C
        CALL TAGNOD_R2R(IXP,NIXP,4,4,NUMELP,IPARTP,TAGNO,NPART,3,N)
        CALL TAGNOD_R2R(IXR,NIXR,4,4,NUMELR,IPARTR,TAGNO,NPART,3,N)
C---------------Tag of additional nodes for kjoints-- --------------C        
        CALL TAGNOD_R2R(IXR_KJ,5,1,3,NUMELR,IPARTR,TAGNO,NPART,4,N)        
C---------------Tag of specific nodes -> Skew,Frames-----------------C 
        IF (P==NSUBDOM_LOC) CALL TAGNOD_R2R_S(TAGNO)
C---------------Storage of initial tag of nodes----------------------C 
        DO I=1,NUMNOD
          TAGNO(NPART+NUMNOD+I) = TAGNO(NPART+I)
        END DO     
C--------------------------------------------------------------------C
C////////////////////////////////////////////////////////////////////C
C--------------------------------------------------------------------C
        DO WHILE ((MODIF>0).AND.(COMPT<80))
        MODIF = 0
C---------------Prereading of options for tag of nodes---------------C
        IF (P==NSUBDOM_LOC) THEN
          CALL R2R_PRELEC(IPARTS,
     2           IPARTC,IPARTG,IPARTT,IPARTP,IPARTR,IPARTSP,COMPT_T2,
     3           MODIF,COMPT,INOM_OPT,NSPCONDN,NSPHION,IPART_L,MEMTR,
     4           PM_STACK ,IWORKSH   ,IGRNOD  ,IGRSURF ,IGRSLIN ,
     5           IGRBRIC  ,IGRQUAD   ,IGRSH4N ,IGRSH3N ,IGRTRUSS,
     6           IGRBEAM  ,IGRSPRING ,NEW_NSLASH_INT,LSUBMODEL,NEW_HM_NINTER,
     7           NEW_NINTSUB,NEW_NINIVOL,IXS10,IXS20,IXS16,
     8           DETONATORS,NSENSOR,SEATBELT_SHELL_TO_SPRING,NB_SEATBELT_SHELLS)
        ENDIF
C---------------Tag of nodes of pretaged elements ( for TYPE2)-------C
        IF (COMPT_T2>0) THEN
        CALL TAGNODS_R2R(IXS,IXS10,IXS20,IXS16,TAG_ELS,TAGNO,2,1)
        CALL TAGNOD_R2R(IXC,NIXC,2,5,NUMELC,TAG_ELC,TAGNO,NPART,2,1)
        CALL TAGNOD_R2R(IXTG,NIXTG,2,4,NUMELTG,TAG_ELG,TAGNO,NPART,2,1)
        CALL TAGNOD_R2R(IXT,NIXT,2,3,NUMELT,TAG_ELT,TAGNO,NPART,2,1)
        CALL TAGNOD_R2R(IXP,NIXP,2,4,NUMELP,TAG_ELP,TAGNO,NPART,2,1)
        ENDIF
        COMPT = COMPT+1
        END DO ! DO WHILE
C---------------Tag of nodes ALE/lagrange ---------------------------C
        IF (IALE>0) THEN
          CALL ALE_CHECK_LAG(NALE_R2R,IXS,IXQ,IXC,IXT,IXTG,PM,ITAB,NALE_R2R,0,IGEO_)
        ENDIF
C---------------Check of FLG_FSI ------------------------------------C
        IF (IALE+IEULER>0) THEN
          CALL CHK_FLG_FSI(IXS,PM,IPARTS,ALE_EULER,IGEO_)
        ENDIF
C---------------Error message infinite loop -------------------------C
        IF (COMPT>=80) THEN
          CALL ANCMSG(MSGID=972,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO)
        END IF
C--------------------------------------------------------------------C
C////////////////////////////////////////////////////////////////////C
C--------------------------------------------------------------------C
C---------------Update of number of remaining options----------------C
          NRBODY  = NEW_NRBY
          NRBYKIN = NEW_NRBYKIN
          NJOINT  = NEW_NJOINT
          NINTER_PREC = NINTER
          HM_NINTER   = NEW_HM_NINTER
          NINTER      = NEW_HM_NINTER + NEW_NINTER - NEW_NINTSUB
          NSLASH(KINTER) = NEW_NSLASH_INT
          NINTSUB = NEW_NINTSUB
          NINIVOL = NEW_NINIVOL
C
          NLINK = NEW_NLINK
          NRBE3 = NEW_NRBE3
          NRBE2 = NEW_NRBE2
          NGJOINT = NEW_NGJOINT
          NUMMPC = NEW_NUMMPC
          NSPCOND = NSPCONDN
          NSPHIO = NSPHION
C---------------Determination of flag for coupled sph particles------C
          FLG_SPH = 0
          DO J=1,NUMSPH
            IF (TAGNO(KXSP(NISP*(J-1)+3)+NPART)>1) FLG_SPH = 1
          END DO   
C---------------Counting of remaining nodes--------------------------C
          COMPT = 0
          DO J=1,NUMNOD
            IF (TAGNO(J+NPART)>1) INNOD = INNOD+1	        
          ENDDO
C---------------Warnings for size of interfaces ---------------------C
          IF (INNOD==0) THEN
            CALL ANCMSG(MSGID=839,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                   C1="CONNECTIONS FOUND",
     .                   C2="FOR DOMAIN",
     .                   I1=ISUBDOM(1,P))
          ELSE
C--         For SPH big multidomains interfaces are alloxed -> no error message only a warning
            IF ((FLG_SPH==1).OR.(FLG_FSI==1)) R2R_FLAG_ERR_OFF = 1
C              
            FAC = (100*INNOD) / NUMNOD
            IF (((FAC>20).AND.(FAC<50)).OR.((R2R_FLAG_ERR_OFF==1).AND.(FAC>50))) THEN
              CALL ANCMSG(MSGID=859,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=INNOD,
     .                    I2=FAC)
            ELSEIF (FAC>50) THEN
              FLG_R2R_ERR = 1
              CALL ANCMSG(MSGID=1075,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=INNOD,
     .                    I2=FAC)
            ENDIF
          ENDIF
C---------------Warnings for splitted contact interfaces ------------C
          IF (TAGINT_WARN(1)>0) THEN
            CALL ANCMSG(MSGID=842,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1)
            WRITE(IOUT,1301)
            WRITE(IOUT,1302) (TAGINT_WARN(1+J),J=1,TAGINT_WARN(1))
          ENDIF  
C--------------------------------------------------------------------C
        ELSE
C--------------------------------------------------------------------C			
C---------------FLAG = 1 --> Creation of groups and links------------C
C--------------------------------------------------------------------C
          INNOD = 0
          NN = 4
C
C---------------Tag of nodes with nlocal dof-------------------------C
          IF (NLOC_DMG%IMOD > 0) THEN
            NN = 5
            CALL MY_ALLOC(TAG_NLOCAL,NUMNOD)
            TAG_NLOCAL(1:NUMNOD) = 0
            CALL TAGNOD_R2R_NL(IXC,IXTG,IXS,IXS10,IXS20,
     .                         IXS16,TAG_NLOCAL,IPM)
          ENDIF
C
          DO K=1,NN
            COMPT = 0
            IAD_TMP = IAD
C-----------Storage inside buffer------------------------------------C
            IF (K < 5) THEN
              DO J=1,NUMNOD	     
                IF (TAGNO(J+NPART)==(K+N)) THEN 
                  BUF_NOD(IAD)=J
                  IAD=IAD+1
                  COMPT = COMPT+1	        
                ENDIF
              ENDDO
            ELSE
C-----------Nodes in link type 4 + nlocal dof -> aditional link------C
C-----------Node coupled only if nl material on both sides ----------C 
              DO J=1,NUMNOD
                IF ((TAG_NLOCAL(J)==1).AND.(TAGNO(J+NPART+NUMNOD) == N+1)) THEN
                  BUF_NOD(IAD)=J
                  IAD=IAD+1
                  COMPT = COMPT+1	        
                ENDIF
              ENDDO
            ENDIF
C
            INNOD = INNOD + COMPT   
            IF (COMPT>0) THEN
C---------------Creation of new GRNOD + LINK ------------------------C
              IF (K == 1) THEN
                TITR="MULTIDOMAINS INTERFACE TYPE CONNECTION "
              ELSEIF (K == 2) THEN
                TITR="MULTIDOMAINS INTERFACE TYPE RBODY CONNECTION "
              ELSEIF (K == 4) THEN
                TITR="MULTIDOMAINS INTERFACE TYPE KINEMATIC CONDITION"
              ELSEIF (K == 5) THEN
                TITR="MULTIDOMAINS INTERFACE TYPE NON LOCAL"                 
              ELSE 
                TITR="MULTIDOMAINS INTERFACE TYPE CONTACT "   
              ENDIF
!---
              IGROUP_TEMP2(1,IGS)= NUM
              IGROUP_TEMP2(2,IGS)= COMPT
              IGROUP_TEMP2(3,IGS)= IAD_TMP
              IGROUP_TEMP2(10,IGS)= -1  ! temporary tag new group
              NEW_TITLE(IGS) = TITR
!---
C---------------Creation of new LINK---------------------------------C
               CALL NEW_LINK(NUM,N,K)              
C---------------Incrementation of Variables for future GRNOD---------C
               NUM = NUM+1
               IGS = IGS+1
               IF (COMPT>0) N_LNK_C = N_LNK_C+1   
             ENDIF
           END DO
           IF (INNOD==0) THEN	
             IF (IPID/=0) THEN
             ENDIF  
             CALL ANCMSG(MSGID=839,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO,
     .                   C1="CONNECTIONS FOUND",
     .                   C2="FOR DOMAIN",
     .                   I1=ISUBDOM(1,P))
           ENDIF   
c
           IF (NLOC_DMG%IMOD > 0) THEN
             DEALLOCATE(TAG_NLOCAL)
           ENDIF
        ENDIF
      END DO
C--------------------------------------------------------------------C
C----- Recopy of other groups in IGROUP_TEMP-------------------------C
C--------------------------------------------------------------------C
C----- Transfer of Igroup_temp in IGRN-------------------------------C
C--------------------------------------------------------------------C
      IF (FLAG == 1) THEN
        DO I=1,NGRNOD
          DEALLOCATE(IGRNOD(I)%ENTITY)
        ENDDO
        DEALLOCATE(IGRNOD)
        ALLOCATE(IGRNOD(NGRNOD+N_LNK_C))
        NGRNOD = NGRNOD+N_LNK_C
!
        DO I=1,NGRNOD
          ALLOCATE(IGRNOD(I)%ENTITY(IGROUP_TEMP2(2,I)))
          IGRNOD(I)%ENTITY(1:IGROUP_TEMP2(2,I)) = 0
!
          IGRNOD(I)%ID        = IGROUP_TEMP2(1,I)    ! IGRN(1,*)
          IGRNOD(I)%NENTITY   = IGROUP_TEMP2(2,I)    ! IGRN(2,*)
          IGRNOD(I)%GRTYPE    = IGROUP_TEMP2(3,I)    ! IGRN(4,*)
          IGRNOD(I)%SORTED    = IGROUP_TEMP2(4,I)    ! IGRN(5,*)
          IGRNOD(I)%GRPGRP    = IGROUP_TEMP2(5,I)    ! IGRN(6,*)
          IGRNOD(I)%LEVEL     = IGROUP_TEMP2(6,I)    ! IGRN(7,*)
          IGRNOD(I)%TITLE     = NEW_TITLE(I)         ! IGRN(11,*)
          IGRNOD(I)%R2R_ALL   = IGROUP_TEMP2(8,I)    ! IGRN(8,*)
          IGRNOD(I)%R2R_SHARE = IGROUP_TEMP2(9,I)    ! IGRN(9,*)
!
          IF (IGROUP_TEMP2(10,I) == -1) THEN
            IAD_TMP = IGROUP_TEMP2(3,I)
            DO J=1,IGROUP_TEMP2(2,I)
!   "BUF_NOD" --> temporary array for shared boundary
              IGRNOD(I)%ENTITY(J) = BUF_NOD(IAD_TMP+J-1)
            ENDDO
          ELSE
            IAD_TMP = IGROUP_TEMP2(7,I)
            DO J=1,IGROUP_TEMP2(2,I)
              IGRNOD(I)%ENTITY(J) = IGROUP_TEMP2_BUF(IAD_TMP+J-1)
            ENDDO
          ENDIF
!
        END DO ! DO I=1,NGRNOD
      ENDIF ! IF (FLAG == 1)
C--------------------------------------------------------------------C
C----- Creation of file "_0000.r2r"----------------------------------C
C--------------------------------------------------------------------C
      IF (FLAG == 1) THEN
        IF (IPID/=0) THEN
          WRITE(ISTDO,'(A)')' .. MULTIDOMAINS INPUT FILE GENERATION' 
          CALL R2R_INPUT(IEXLNK)   
        ENDIF      
      ENDIF
C--------------------------------------------------------------------C
C--------------------------------------------------------------------C
C--------------------------------------------------------------------C    
      IF (FLAG == 1) DEALLOCATE(IGROUP_TEMP2)
      IF (FLAG == 1) THEN
        IF (ALLOCATED(IGROUP_TEMP2_BUF))DEALLOCATE(IGROUP_TEMP2_BUF)
      ENDIF
      RETURN
C--------------------------------------------------------------------C
1301  FORMAT( 1X,'LIST OF SPLITTED CONTACT INTERFACES : ')
1302  FORMAT( 1X,10I9)  
      
      END
