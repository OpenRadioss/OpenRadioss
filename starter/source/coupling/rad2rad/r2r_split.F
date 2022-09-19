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
Chd|  R2R_SPLIT                     source/coupling/rad2rad/r2r_split.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        CONSTIT                       source/elements/nodes/constit.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        PRELECSEC                     source/tools/sect/prelecsec.F 
Chd|        TAGELEM_R2R                   source/coupling/rad2rad/tagelem_r2r.F
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        NOD2EL_MOD                    share/modules1/nod2el_mod.F   
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        RESTMOD                       share/modules1/restart_mod.F  
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE R2R_SPLIT(
     1           NB_LINE,
     2           NB_SURF,FLAG,EANI2,BUF_NOD,IXR_KJ,
     3           INOM_OPT,RESERVEP,NALE_R2R,NSPCOND0,
     4           SUBSET ,IGRSURF,IGRNOD  ,IGRBRIC,IGRQUAD,
     5           IGRSH4N,IGRSH3N,IGRTRUSS,IGRBEAM,IGRSPRING,
     6           IGRPART,IGRSLIN,LSUBMODEL,RBY_MSN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE RESTMOD
      USE R2R_MOD
      USE NOD2EL_MOD
      USE FRONT_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com10_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "r2r_c.inc"
#include      "rnur_c.inc"
#include      "tabsiz_c.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "my_allocate.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
      INTEGER BUF_NOD(*),
     .   NB_SURF,FLAG,
     .   EANI2(*),NB_LINE,
     .   IXR_KJ(*),INOM_OPT(*),RESERVEP(*),NALE_R2R(SNALE),
     .   NSPCOND0,RBY_MSN(2,*)
!
      TYPE (SUBSET_) , DIMENSION(NSUBS)   :: SUBSET
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE (SURF_)   , DIMENSION(NSLIN)   :: IGRSLIN
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (GROUP_)  , DIMENSION(NGRQUAD) :: IGRQUAD
      TYPE (GROUP_)  , DIMENSION(NGRSHEL) :: IGRSH4N
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
      TYPE (GROUP_)  , DIMENSION(NGRTRUS) :: IGRTRUSS
      TYPE (GROUP_)  , DIMENSION(NGRBEAM) :: IGRBEAM
      TYPE (GROUP_)  , DIMENSION(NGRSPRI) :: IGRSPRING
      TYPE (GROUP_)  , DIMENSION(NGRPART) :: IGRPART
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,J,ADD,IAD,CUR_ID,TPP,CCPL,NF1,NF2,TYP2
      INTEGER COMPT,NSEG,FSKW,IDOM,NUMNOD_OLD
      INTEGER ISUR,ISURS,NTOT,NB_NOD_SUB,NB_NOD_CPL,NB_NOD
      INTEGER MAXN,MAXANUS,SIXSN,ID_INTER,NUL,TAG,COMPTB
      INTEGER COMPT10,COMPT20,COMPT16,COMPT8,J10,J20,J16,JJ
      INTEGER G1,G2,GRS,GRM,GRS2,LN1,LN2,NI,ID_MON,IAD3,IO_ERR
      INTEGER LNM,LNS,NEW_ID,SIPART0,SIPARTTH,COMPT_IP,COMPT_IP_TMP,L0
      INTEGER ID_PROP,COEFF,NUMSPHA,NSPHRESN,FIRST_CELL,NOD_ID,PART_RES,INOD
      INTEGER JJB,IUN,NRB,NRBODY_OLD
      CHARACTER TITR*40
      INTEGER, DIMENSION(:), POINTER :: PART1,PART2
      INTEGER, DIMENSION(:), ALLOCATABLE :: BUF_TEMP,ITAB_TEMP,IX_TEMP
      INTEGER, DIMENSION(:), ALLOCATABLE :: CORESN,CORESC,CORESTG,COREST
      INTEGER, DIMENSION(:), ALLOCATABLE :: CORESPA,CORESR,CORESP,CORESS,CORESSP
      INTEGER, DIMENSION(:), ALLOCATABLE :: IPART_TEMP,IWA_TEMP
      INTEGER, DIMENSION(:), ALLOCATABLE :: IPM_TEMP,IGEO_TEMP,CORESMA
      INTEGER, DIMENSION(:), ALLOCATABLE :: CORESPRO,TAGNO_TEMP
      INTEGER, DIMENSION(:), ALLOCATABLE :: IX10_TEMP,IX20_TEMP
      INTEGER, DIMENSION(:), ALLOCATABLE :: IX16_TEMP,CORESQ,ITAB_SUP
      INTEGER, DIMENSION(:), ALLOCATABLE :: KXSP_TEMP,RES_TEMP,NALE_R2R_TEMP
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: RBY_MSN_TEMP
      my_real, DIMENSION(:,:), ALLOCATABLE :: X_TEMP
      my_real, DIMENSION(:), ALLOCATABLE :: THK_TMP,PM_TEMP
      my_real, DIMENSION(:), ALLOCATABLE :: EANI_TEMP,GEO_TEMP
      CHARACTER MESS*40
      CHARACTER OPT*ncharkey,KEY*ncharkey
      DATA MESS/'MULTIDOMAIN INITIALIZATION'/
      DATA IUN/1/
C===================================================================================

C----------------------------------------------------------------------------------C
C--------------------------------Initialisation------------------------------------C
C----------------------------------------------------------------------------------C

	N_PART = NPART

C----------------------------------------------------------------------------------C
C-----------------------Premiere passe -> tag et comptage des donnees  ------------C
C----------------------------------------------------------------------------------C

      IF (FLAG==0) THEN

C----------------------------------------------------------------------------------C
C---------------------Allocation et Mize a zero des tableaux de tag----------------C
C----------------------------------------------------------------------------------C

        ALLOCATE(TAG_MAT(NUMMAT),TAG_PROP(NUMGEO),TAG_SUBS(NSUBS))
        ALLOCATE(TAG_SURF(NUMELC+NUMELTG+NUMELS+NPART))

        TAG_SURF(:) = 0
        TAG_SUBS(:) = 0
	TAG_PART(:) = 0

C------------Provisoirement on garde tous les mat et props-------------------------C
        TAG_MAT(:) = 1
        TAG_PROP(:) = 1

C------------Stockage de TAG_EL----------------------------------------------------C        
        ALLOCATE(TAG_ELCF2(NUMELC))
        ALLOCATE(TAG_ELSF2(NUMELS))
        TAG_ELCF2 = 0
        TAG_ELSF2 = 0
        DO I=1,NUMELS
          TAG_ELSF2(I) = TAG_ELS(I+NPART)
        END DO
        DO I=1,NUMELC
          TAG_ELCF2(I) = TAG_ELC(I+NPART)
        END DO

C----------------------------------------------------------------------------------C
C-----------------------------tag des donnees du subdomain-------------------------C
C----------------------------------------------------------------------------------C

       IF (IDDOM/=0) THEN
	  ADD = ISUBDOM(3,IDDOM)
	  DO K=1,NPART
            DO I=1,ISUBDOM(1,IDDOM)
              IF(K == ISUBDOM_PART(I+ADD))THEN
                  TAG_PART(K)=1
              ENDIF
            ENDDO
	  END DO
       ENDIF

C----------------Full domain --> tag des parts de tous les sous domaines-----------C

       IF (IDDOM == 0) THEN

          DO IDOM=1,NSUBDOM
	    ADD = ISUBDOM(3,IDOM)
	    DO K=1,NPART
              DO I=1,ISUBDOM(1,IDOM)
                IF(K == ISUBDOM_PART(I+ADD))THEN
                    TAG_PART(K)=1
                ENDIF
              ENDDO
	    END DO
	  END DO

C-        ----->Full domain --> inversion du tag des parts<---

	  DO K=1,NPART
              IF(TAG_PART(K) == 1) THEN
                  TAG_PART(K)=0
	      ELSE
                  TAG_PART(K)=1
              ENDIF
	  END DO

	ENDIF

C---------------Tag des subsets----------------------------------------------------C
!
Cmi   --- TAG_SUBS  ---> ATTENTION not used
!
!        DO J=1,NSUBS
!	  IAD3 = ISUBS(LISUB1*(J-1)+9)
!          DO K=1,ISUBS(LISUB1*(J-1)+8)
!	    IF (TAG_PART(BUF_NOD(IAD3))>0) TAG_SUBS(J)=1
!	    IAD3 = IAD3+1
!	  END DO
!        END DO
!---
        DO J=1,NSUBS
          DO K=1,SUBSET(J)%NTPART
	    IF (TAG_PART(SUBSET(J)%TPART(K))>0) TAG_SUBS(J)=1
!
	  ENDDO
        ENDDO
!---

C---------------Tag des parts pour tag d'elements----------------------------------C

	DO K=1,NPART
              IF(TAG_PART(K) == 1) THEN
                  TAG_ELS(K)=1
                  TAG_ELQ(K)=1
                  TAG_ELC(K)=1
                  TAG_ELT(K)=1
                  TAG_ELP(K)=1
                  TAG_ELR(K)=1
                  TAG_ELG(K)=1
                  TAG_ELSP(K)=1
		  TAG_SURF(K)=1
C---------------Tag des materiaux et proprietes------------------------------------C
                  TAG_MAT(IPART(LIPART1*(K-1)+1))=1
                  TAG_PROP(IPART(LIPART1*(K-1)+2))=1
              ENDIF
	END DO

C---------------Les noeuds sont deja tages dans TAGNO------------------------------C

C---------------Tag des elements---------------------------------------------------C

        SIPART0 = LIPART1*NPART+LIPART1*NTHPART
	SIPARTTH= 2*9*NPART+2*9*NTHPART
        L0 =SIPARTTH+SIPART0+1

        IF (NUMELS>0) CALL TAGELEM_R2R(NUMELS,IPART(L0),TAG_ELS,NPART)
        L0 = L0+NUMELS

        IF (NUMELQ>0) CALL TAGELEM_R2R(NUMELQ,IPART(L0),TAG_ELQ,NPART)
        L0 = L0+NUMELQ

        IF (NUMELC>0) CALL TAGELEM_R2R(NUMELC,IPART(L0),TAG_ELC,NPART)
        L0 = L0+NUMELC

        IF (NUMELT>0) CALL TAGELEM_R2R(NUMELT,IPART(L0),TAG_ELT,NPART)
        L0 = L0+NUMELT

        IF (NUMELP>0) CALL TAGELEM_R2R(NUMELP,IPART(L0),TAG_ELP,NPART)
        L0 = L0+NUMELP

        IF (NUMELR>0) CALL TAGELEM_R2R(NUMELR,IPART(L0),TAG_ELR,NPART)
        L0 = L0+NUMELR

        IF (NUMELTG>0)CALL TAGELEM_R2R(NUMELTG,IPART(L0),TAG_ELG,NPART)
        L0 = L0+NUMELTG+NUMELUR+NUMELX

        IF (NUMSPH>0) CALL TAGELEM_R2R(NUMSPH,IPART(L0),TAG_ELSP,NPART)

C---------------Tag des injecteurs et de leur prop et mat--------------------------C

	CALL R2R_MONVOL(TAG_MAT,TAG_PROP,IGRSURF,LSUBMODEL)

C----------------------------------------------------------------------------------C
C--------------------------Premiere passe -> comptage -----------------------------C
C----------------------------------------------------------------------------------C

        COMPT = 0
        NNODN = 0
	NODSUPR = 0
        NSPHN = 0
	NELCN = 0
	NELTGN = 0
	NELTN = 0
	NELRN = 0
	NELPN = 0
	NELQN = 0
	NELSN = 0
	NELS10N = 0
	NELS20N = 0
	NELS16N = 0
        NINLETN = 0
	SIZ_IPM_NEW = NPROPMI
	SIZ_PM_NEW = NPROPM
	SIZ_IGEO_NEW = NPROPGI
	SIZ_GEO_NEW = NPROPG

C ---------> comptage des materiaux	-------------------------------------------C
        DO J=1,NUMMAT
          IF (TAG_MAT(J)/=0)THEN
	        SIZ_IPM_NEW = SIZ_IPM_NEW + NPROPMI
	        SIZ_PM_NEW = SIZ_PM_NEW + NPROPM
          ENDIF
        ENDDO
C ---------> comptage des proprits	-------------------------------------------C
        DO J=1,NUMGEO
          IF (TAG_PROP(J)/=0)THEN
	        SIZ_IGEO_NEW = SIZ_IGEO_NEW + NPROPGI
	        SIZ_GEO_NEW = SIZ_GEO_NEW + NPROPG
          ENDIF
        ENDDO
C ---------> comptage des noeuds et des noeuds deletes-----------------------------C
        DO J=1,NUMNOD
          IF (TAGNO(J+NPART)>=0)THEN
	        NNODN = NNODN+1
          ELSE
	  	NODSUPR = NODSUPR+1
          ENDIF
        ENDDO
C ---------> comptage des parts	---------------------------------------------------C
        DO J=1,NPART
          IF (TAG_PART(J)==1) THEN
	        NPARN = NPARN+1
          ENDIF
        ENDDO
C ---------> Noeud poubelle	---------------------------------------------------C
	NNODN = NNODN+1
C ---------> comptage des shell	---------------------------------------------------C
        DO J=1,NUMELC
          IF (TAG_ELC(J+NPART)/=0) THEN
	        NELCN = NELCN+1
          ENDIF
        ENDDO
C ---------> comptage des sh3n	---------------------------------------------------C
        DO J=1,NUMELTG
          IF (TAG_ELG(J+NPART)/=0) THEN
	        NELTGN = NELTGN+1
          ENDIF
        ENDDO
C ---------> comptage des truss	---------------------------------------------------C
        DO J=1,NUMELT
          IF (TAG_ELT(J+NPART)/=0) THEN
	        NELTN = NELTN+1
          ENDIF
        ENDDO
C ---------> comptage des springs--------------------------------------------------C
        DO J=1,NUMELR
          IF (TAG_ELR(J+NPART)/=0) THEN
	        NELRN = NELRN+1
          ENDIF
        ENDDO
C ---------> comptage des beams----------------------------------------------------C
        DO J=1,NUMELP
          IF (TAG_ELP(J+NPART)/=0) THEN
	        NELPN = NELPN+1
          ENDIF
        ENDDO
C ---------> comptage des quads----------------------------------------------------C
        DO J=1,NUMELQ
          IF (TAG_ELQ(J+NPART)/=0) THEN
	        NELQN = NELQN+1
          ENDIF
        ENDDO
C ---------> comptage des solides--------------------------------------------------C
        DO J=1,NUMELS
          IF (TAG_ELS(J+NPART)/=0) THEN
	        NELSN = NELSN+1
		IF (EANI2(J)==10) NELS10N = NELS10N+1
		IF (EANI2(J)==20) NELS20N = NELS20N+1
		IF (EANI2(J)==16) NELS16N = NELS16N+1
          ENDIF
        ENDDO
C ---------> comptage des particules SPH-------------------------------------------C
        DO J=1,NUMSPH
          IF (TAG_ELSP(J+NPART)/=0) THEN
	        NSPHN = NSPHN+1
          ENDIF
        ENDDO
C ---------> comptage des reserves de particules SPH-------------------------------C
        FIRST_CELL = FIRST_SPHRES
        DO J=1,NBPARTINLET
          IF (TAG_ELSP(FIRST_CELL+NPART)/=0) THEN
            NINLETN = NINLETN + 1
          ENDIF
          FIRST_CELL = FIRST_CELL + RESERVEP(J)
        ENDDO

C----------------------------------------------------------------------------------C
      ENDIF ! IF (FLAG==0) THEN

C----------------------------------------------------------------------------------C
C-------------------------Deuxieme passe -> split des tableaux  -------------------C
C----------------------------------------------------------------------------------C

      IF (FLAG==1) THEN

C----------------------------------------------------------------------------------C
C-------------On split les materiaux ----------------------------------------------C
C----------------------------------------------------------------------------------C

          ALLOCATE (IPM_TEMP(NPROPMI*NUMMAT),PM_TEMP(NPROPM*NUMMAT))
          ALLOCATE(CORESMA(NUMMAT))
          DO I=1,NUMMAT
            DO J=1,NPROPMI
              IPM_TEMP(NPROPMI*(I-1)+J)=IPM(NPROPMI*(I-1)+J)
            END DO
          END DO
          DO I=1,NUMMAT
            DO J=1,NPROPM
              PM_TEMP(NPROPM*(I-1)+J)=PM(NPROPM*(I-1)+J)
            END DO
          END DO
          DEALLOCATE(IPM,PM)

C-------------Split----------------------------------------------------------------C

          ALLOCATE(IPM(SIZ_IPM_NEW),PM(SIZ_PM_NEW))
          COMPT = 0
          DO J=1,NUMMAT
            IF ((TAG_MAT(J)/=0).OR.(J==NUMMAT)) THEN
	         COMPT = COMPT+1
		 CORESMA(J)=COMPT
                 DO K=1,NPROPMI
		   IPM(NPROPMI*(COMPT-1)+K)=IPM_TEMP(NPROPMI*(J-1)+K)
		 END DO
                 DO K=1,NPROPM
		   PM(NPROPM*(COMPT-1)+K)=PM_TEMP(NPROPM*(J-1)+K)
		 END DO
            ENDIF
          ENDDO

	  NUMMAT = COMPT
          DEALLOCATE(IPM_TEMP,PM_TEMP)

C----------------------------------------------------------------------------------C
C-------------On split les proprietes ---------------------------------------------C
C----------------------------------------------------------------------------------C

          ALLOCATE (IGEO_TEMP(NPROPGI*NUMGEO),GEO_TEMP(NPROPG*NUMGEO))
          ALLOCATE(CORESPRO(NUMGEO))
          DO I=1,NUMGEO
            DO J=1,NPROPGI
              IGEO_TEMP(NPROPGI*(I-1)+J)=IGEO(NPROPGI*(I-1)+J)
            END DO
          END DO
          DO I=1,NUMGEO
            DO J=1,NPROPG
              GEO_TEMP(NPROPG*(I-1)+J)=GEO(NPROPG*(I-1)+J)
            END DO
          END DO
          DEALLOCATE(IGEO,GEO)

C-------------Split----------------------------------------------------------------C

          ALLOCATE(IGEO(SIZ_IGEO_NEW),GEO(SIZ_GEO_NEW))
          COMPT = 0
	  MAXANUS = 0
          DO J=1,NUMGEO
            IF (TAG_PROP(J)/=0) THEN
	         COMPT = COMPT+1
		 CORESPRO(J)=COMPT
                 DO K=1,NPROPGI
		   MAXANUS = NPROPGI*(COMPT-1)+K
		   IGEO(NPROPGI*(COMPT-1)+K)=IGEO_TEMP(NPROPGI*(J-1)+K)
		 END DO
                 DO K=1,NPROPG
		   GEO(NPROPG*(COMPT-1)+K)=GEO_TEMP(NPROPG*(J-1)+K)
		 END DO
            ENDIF
          ENDDO

	  NUMGEO = COMPT
          DEALLOCATE(IGEO_TEMP,GEO_TEMP)

C----------------------------------------------------------------------------------C
C-------------On split les PARTS---------------------------------------------------C
C----------------------------------------------------------------------------------C

          DO J=1,NPART
		IPART(LIPART1*(J-1)+1)=CORESMA(IPART(LIPART1*(J-1)+1))
		IPART(LIPART1*(J-1)+2)=CORESPRO(IPART(LIPART1*(J-1)+2))
          ENDDO

C----------------------------------------------------------------------------------C
CC-------------On split les noeuds ------------------------------------------------C
C----------------------------------------------------------------------------------C

          ALLOCATE(CORESN(NUMNOD),X_TEMP(3,NUMNOD))
          ALLOCATE(ITAB_TEMP(NUMNOD))
          DO J=1,NUMNOD
	    ITAB_TEMP(J)=ITAB(J)
	    X_TEMP(1,J)=X(3*(J-1)+1)
	    X_TEMP(2,J)=X(3*(J-1)+2)
	    X_TEMP(3,J)=X(3*(J-1)+3)
	  END DO
          DEALLOCATE(ITAB,X)

C-------------Split----------------------------------------------------------------C
          ALLOCATE(ITAB(NNODN),X(3*NNODN),ITAB_SUP(NODSUPR))
	  ALLOCATE(FRONT_R2R(NNODN))
	  ALLOCATE(FLAGKIN_R2R(NNODN))
	  FLAGKIN_R2R(1:NNODN)=0
          FRONT_R2R(1:NNODN)=0
          COMPT = 0
	  COMPTB = 0
	  MAXN=0
          DO J=1,NUMNOD
            IF (TAGNO(J+NPART)>=0)THEN
	         COMPT = COMPT+1
		 ITAB(COMPT)=ITAB_TEMP(J)
		 IF (ITAB(COMPT)>MAXN) MAXN = ITAB(COMPT)
		 CORESN(J)=COMPT
		 X(3*(COMPT-1)+1)=X_TEMP(1,J)
		 X(3*(COMPT-1)+2)=X_TEMP(2,J)
		 X(3*(COMPT-1)+3)=X_TEMP(3,J)
c		 FRONT_R2R(COMPT)=FRONT(J,1)		
		 FRONT_R2R(COMPT) = NLOCAL(J,1)
		 IF(FLAGKIN(J)==1)THEN
		   FLAGKIN_R2R(COMPT)=1
		 ENDIF
c		 ELSE
c		   FRONT_R2R(COMPT) = 0
c		 ENDIF
		 IF (TAGNO(J+NPART)>1) THEN
		    MS(COMPT)=1e-20
		    IF (IRODDL==1) IN(COMPT)=1e-20
		 ENDIF
            ELSE
	         COMPTB = COMPTB+1
		 ITAB_SUP(COMPTB)=ITAB_TEMP(J)
            ENDIF
          ENDDO

C -----------on update les skews---------------------------------------------------C

          DO J=1,NUMSKW
	     DO K=1,3
	        IF (ISKWN(LISKN*J+K)>0)
     .              ISKWN(LISKN*J+K)=CORESN(ISKWN(LISKN*J+K))
	     END DO
          ENDDO

C -----------on update les frames--------------------------------------------------C

	  JJ = SISKWN-SIFRAME
          IF (NSPHN==NUMSPH) THEN
            DO J=1,NUMFRAM
	      DO K=1,3
	        IF (ISKWN(JJ+LISKN*J+K)>0)
     .              ISKWN(JJ+LISKN*J+K)=CORESN(ISKWN(JJ+LISKN*J+K))
	      END DO
            ENDDO
          ELSE
C--         on avance les frames si disparition des particules SPH      
            JJB = SISKWN-SIFRAME-MIN(IUN,NSPCOND0)*(NUMSPH-NSPHN)*LISKN
            DO J=1,NUMFRAM
	      DO K=1,3
                IF (ISKWN(JJ+LISKN*J+K)>0) THEN
	          ISKWN(JJB+LISKN*J+K)=CORESN(ISKWN(JJ+LISKN*J+K))
                ENDIF
	      END DO
	      DO K=4,LISKN
	        ISKWN(JJB+LISKN*J+K)=ISKWN(JJ+LISKN*J+K)
	      END DO
	    END DO
          ENDIF

          NUMNOD_OLD = NUMNOD
	  NUMNOD = COMPT
	  NUMNOD0 = COMPT
          DEALLOCATE(ITAB_TEMP,X_TEMP)

C-------------Reconstitution de ITABM1---------------------------------------------C
          PART1  => ITABM1(1:2*NUMNOD)
          CALL CONSTIT(ITAB,PART1,NUMNOD)

C-------------On cre une deuxime liste de noeuds supprims pour les detection d'erreures
          IF (NODSUPR/=0) THEN
          PART2  => ITABM1(2*NUMNOD+1:2*NUMNOD_OLD)
          CALL CONSTIT(ITAB_SUP,PART2,NODSUPR)
          DEALLOCATE(ITAB_SUP)
          ENDIF

C----------------------------------------------------------------------------------C
C-------------On split NALE_R2R----------------------------------------------------C
C----------------------------------------------------------------------------------C

          IF (SNALE>0) THEN
C
            ALLOCATE(NALE_R2R_TEMP(SNALE))
            DO J=1,NUMNOD_OLD
	      NALE_R2R_TEMP(J)=NALE_R2R(J)
	    END DO

C-------------Split----------------------------------------------------------------C
            NALE_R2R(:) = 0
            COMPT = 0
            DO J=1,NUMNOD_OLD
              IF (TAGNO(J+NPART)>=0) THEN
	         COMPT = COMPT+1
                 NALE_R2R(COMPT) =  NALE_R2R_TEMP(J)
              ENDIF
            END DO
            DEALLOCATE(NALE_R2R_TEMP)

          ENDIF

C----------------------------------------------------------------------------------C
C-------------On split EANI--------------------------------------------------------C
C----------------------------------------------------------------------------------C

	  NTOT = NELSN+NELCN+NELTGN+NELQN
	  ALLOCATE(EANI_TEMP(SEANI))
          COMPT = 0

          DO J=1,SEANI
	     EANI_TEMP(J)=EANI2(J)
	     EANI2(J)=0
	  END DO

	  SEANI = NTOT

          DO J=1,NUMELS
             IF (TAG_ELS(J+NPART)/=0) THEN
	     	COMPT = COMPT + 1
	        EANI2(COMPT)=EANI_TEMP(J)
	     ENDIF
	  END DO

          COMPT = NELSN+NELCN+NELQN
          DO J=1,NUMELTG
             IF (TAG_ELG(J+NPART)/=0) THEN
		COMPT = COMPT + 1
	        EANI2(COMPT)=EANI_TEMP(NUMELS+NUMELQ+NUMELC+J)
	     ENDIF
	  END DO

C----------------------------------------------------------------------------------C
C-------------INITIALIZATION DE IPART----------------------------------------------C
C----------------------------------------------------------------------------------C

          SIPART0 = LIPART1*NPART+LIPART1*NTHPART
	  SIPARTTH= 2*9*NPART+2*9*NTHPART
	  ALLOCATE(IPART_TEMP(SIPART))

	  DO J=1,SIPART
	    IPART_TEMP(J)=IPART(J)
	  END DO

	  DEALLOCATE(IPART)
          SIPART  = SIPART0+SIPARTTH+NELSN+NELQN+NELCN+NELTN+NELPN
     .        + NELRN+NUMELUR+NELTGN+NUMELX+NUMSPH
	  ALLOCATE(IPART(SIPART))

	  DO J=1,SIPART0+SIPARTTH
	    IPART(J)=IPART_TEMP(J)
	  END DO

	  COMPT_IP = SIPART0+SIPARTTH
	  COMPT_IP_TMP = SIPART0+SIPARTTH

C----------------------------------------------------------------------------------C
C-------------On split les elements SOLIDES----------------------------------------C
C----------------------------------------------------------------------------------C

          ALLOCATE(IX_TEMP(SIXS),CORESS(NUMELS))

          DO J=1,NUMELS
	    DO K=1,NIXS
	      IX_TEMP(NIXS*(J-1)+K)=IXS(NIXS*(J-1)+K)
            END DO
	  END DO

C-------------Stockage des termes additionnels IXS10,IXS20,IXS16-------------------C

          DO J=NUMELS+1,SIXS
	    IX_TEMP(J)=IXS(J)
	  END DO

          DEALLOCATE(IXS)

C-------------Split----------------------------------------------------------------C

          SIXSN = NELSN*NIXS+NELS10N*6+NELS20N*12+NELS16N*8
	  ALLOCATE(IXS(SIXSN),TAG_ELSF(NELSN))
          TAG_ELSF = 0
          COMPT = 0
	  COMPT8 = 0
	  COMPT10 = 0
	  COMPT20 = 0
	  COMPT16 = 0
	  J10 = 0
	  J20 = 0
	  J16 = 0

          DO J=1,NUMELS
	     COMPT_IP_TMP=COMPT_IP_TMP+1
	     IF (EANI_TEMP(J)==10) J10 = J10+1
	     IF (EANI_TEMP(J)==20) J20 = J20+1
	     IF (EANI_TEMP(J)==16) J16 = J16+1
             IF (TAG_ELS(J+NPART)/=0) THEN
	     	COMPT_IP=COMPT_IP+1
	        COMPT = COMPT+1
		CORESS(J)=COMPT
		IPART(COMPT_IP)=IPART_TEMP(COMPT_IP_TMP)
	        DO K=1,NIXS
		  IXS(NIXS*(COMPT-1)+K)=IX_TEMP(NIXS*(J-1)+K)
		END DO
		IXS(NIXS*(COMPT-1)+1)=CORESMA(IX_TEMP(NIXS*(J-1)+1))
		IXS(NIXS*(COMPT-1)+10)=CORESPRO(IX_TEMP(NIXS*(J-1)+10))
	        DO K=2,9
		  IXS(NIXS*(COMPT-1)+K)=CORESN(IX_TEMP(NIXS*(J-1)+K))
		END DO
                IF (TAG_ELSF2(J)>1) TAG_ELSF(COMPT) = 1
		IF (EANI_TEMP(J)==10) THEN
		  COMPT10 = COMPT10+1
		  DO K=1,6
		    IXS(NIXS*NELSN+6*(COMPT10-1)+K)=
     .              CORESN(IX_TEMP(NIXS*NUMELS+6*(J10-1)+K))
		  END DO
	        ELSEIF (EANI_TEMP(J)==20) THEN
		  COMPT20 = COMPT20+1
		  DO K=1,12
		    IXS((NIXS*NELSN+6*NELS10N)+12*(COMPT20-1)+K)=
     .                 CORESN(IX_TEMP((NIXS*NUMELS+6*NUMELS10)+
     .                 12*(J20-1)+K))
		  END DO
	        ELSEIF (EANI_TEMP(J)==16) THEN
		  COMPT16 = COMPT16+1
		  DO K=1,8
		    IXS((NIXS*NELSN+6*NELS10N+12*NELS20N)+8*(COMPT16-1)+K)=
     .                 CORESN(IX_TEMP((NIXS*NUMELS+6*NUMELS10+
     .                 12*NUMELS20)+8*(J16-1)+K))
		  END DO
		ELSE
		    COMPT8 = COMPT8+1
		ENDIF

             ENDIF
          ENDDO

	  NUMELS8 = COMPT8
	  NUMELS10 = COMPT10
	  NUMELS20 = COMPT20
	  NUMELS16 = COMPT16
	  NUMELS = COMPT

          DEALLOCATE(IX_TEMP)


C----------------------------------------------------------------------------------C
C-------------On split les elements QUAD-------------------------------------------C
C----------------------------------------------------------------------------------C

          ALLOCATE(IX_TEMP(NUMELQ*NIXQ),CORESQ(NUMELQ))
          DO J=1,NUMELQ
	    DO K=1,NIXQ
	      IX_TEMP(NIXQ*(J-1)+K)=IXQ(NIXQ*(J-1)+K)
            END DO
	  END DO
          DEALLOCATE(IXQ)

C-------------Split----------------------------------------------------------------C

	  ALLOCATE(IXQ(NELQN*NIXQ))
          COMPT = 0
          DO J=1,NUMELQ
	     COMPT_IP_TMP=COMPT_IP_TMP+1
             IF (TAG_ELQ(J+NPART)/=0) THEN
	     	COMPT_IP=COMPT_IP+1
	        COMPT = COMPT+1
		CORESQ(J)=COMPT
		IPART(COMPT_IP)=IPART_TEMP(COMPT_IP_TMP)
	        DO K=1,NIXQ
		  IXQ(NIXQ*(COMPT-1)+K)=IX_TEMP(NIXQ*(J-1)+K)
		END DO
		IXQ(NIXQ*(COMPT-1)+1)=CORESMA(IX_TEMP(NIXQ*(J-1)+1))
		IXQ(NIXQ*(COMPT-1)+6)=CORESPRO(IX_TEMP(NIXQ*(J-1)+6))
	        DO K=2,5
		  IXQ(NIXQ*(COMPT-1)+K)=CORESN(IX_TEMP(NIXQ*(J-1)+K))
		END DO
             ENDIF
          ENDDO

	  NUMELQ = COMPT
          DEALLOCATE(IX_TEMP)

C----------------------------------------------------------------------------------C
C-------------On split les elements SHELL------------------------------------------C
C----------------------------------------------------------------------------------C

	  NUMELC0 = NUMELC
          ALLOCATE(IX_TEMP(NUMELC*NIXC),CORESC(NUMELC))
          DO J=1,NUMELC
	    DO K=1,NIXC
	      IX_TEMP(NIXC*(J-1)+K)=IXC(NIXC*(J-1)+K)
            END DO
	  END DO
          DEALLOCATE(IXC)

C-------------Split----------------------------------------------------------------C
	  ALLOCATE(IXC(NELCN*NIXC),TAG_ELCF(NELCN))
          TAG_ELCF = 0
          COMPT = 0
          DO J=1,NUMELC
	     COMPT_IP_TMP=COMPT_IP_TMP+1
             IF (TAG_ELC(J+NPART)/=0) THEN
	     	COMPT_IP=COMPT_IP+1
	        COMPT = COMPT+1
		CORESC(J)=COMPT
		IPART(COMPT_IP)=IPART_TEMP(COMPT_IP_TMP)
	        DO K=1,NIXC
		  IXC(NIXC*(COMPT-1)+K)=IX_TEMP(NIXC*(J-1)+K)
		END DO
		IXC(NIXC*(COMPT-1)+1)=CORESMA(IX_TEMP(NIXC*(J-1)+1))
		IXC(NIXC*(COMPT-1)+6)=CORESPRO(IX_TEMP(NIXC*(J-1)+6))
	        DO K=2,5
		  IXC(NIXC*(COMPT-1)+K)=CORESN(IX_TEMP(NIXC*(J-1)+K))
		END DO
                IF (TAG_ELCF2(J)>1) TAG_ELCF(COMPT) = 1
             ENDIF
          ENDDO

	  NUMELC = COMPT
          DEALLOCATE(IX_TEMP)

C----------------------------------------------------------------------------------C
C-------------On split les elements TRUSS------------------------------------------C
C----------------------------------------------------------------------------------C

          ALLOCATE(IX_TEMP(NUMELT*NIXT),COREST(NUMELT))
          DO J=1,NUMELT
	    DO K=1,NIXT
	      IX_TEMP(NIXT*(J-1)+K)=IXT(NIXT*(J-1)+K)
            END DO
	  END DO
          DEALLOCATE(IXT)

C-------------Split----------------------------------------------------------------C

	  ALLOCATE(IXT(NELTN*NIXT))
          COMPT = 0
          DO J=1,NUMELT
	     COMPT_IP_TMP=COMPT_IP_TMP+1
             IF (TAG_ELT(J+NPART)/=0) THEN
	     	COMPT_IP=COMPT_IP+1
	        COMPT = COMPT+1
		COREST(J)=COMPT
		IPART(COMPT_IP)=IPART_TEMP(COMPT_IP_TMP)
	        DO K=1,NIXT
		  IXT(NIXT*(COMPT-1)+K)=IX_TEMP(NIXT*(J-1)+K)
		END DO
		IXT(NIXT*(COMPT-1)+1)=CORESMA(IX_TEMP(NIXT*(J-1)+1))
		IXT(NIXT*(COMPT-1)+4)=CORESPRO(IX_TEMP(NIXT*(J-1)+4))
	        DO K=2,3
		  IXT(NIXT*(COMPT-1)+K)=CORESN(IX_TEMP(NIXT*(J-1)+K))
		END DO
             ENDIF
          ENDDO

	  NUMELT = COMPT
          DEALLOCATE(IX_TEMP)


C----------------------------------------------------------------------------------C
C-------------On split les elements BEAM-------------------------------------------C
C----------------------------------------------------------------------------------C

          ALLOCATE(IX_TEMP(NUMELP*NIXP),CORESP(NUMELP))
          DO J=1,NUMELP
	    DO K=1,NIXP
	      IX_TEMP(NIXP*(J-1)+K)=IXP(NIXP*(J-1)+K)
            END DO
	  END DO
          DEALLOCATE(IXP)

C-------------Split----------------------------------------------------------------C

	  ALLOCATE(IXP(NELPN*NIXP))
          COMPT = 0
          DO J=1,NUMELP
	     COMPT_IP_TMP=COMPT_IP_TMP+1
             IF (TAG_ELP(J+NPART)/=0) THEN
	     	COMPT_IP=COMPT_IP+1
	        COMPT = COMPT+1
		CORESP(J)=COMPT
		IPART(COMPT_IP)=IPART_TEMP(COMPT_IP_TMP)
	        DO K=1,NIXP
		  IXP(NIXP*(COMPT-1)+K)=IX_TEMP(NIXP*(J-1)+K)
		END DO
		IXP(NIXP*(COMPT-1)+1)=CORESMA(IX_TEMP(NIXP*(J-1)+1))
		IXP(NIXP*(COMPT-1)+5)=CORESPRO(IX_TEMP(NIXP*(J-1)+5))
	        DO K=2,4
		  IXP(NIXP*(COMPT-1)+K)=CORESN(IX_TEMP(NIXP*(J-1)+K))
		END DO
             ENDIF
          ENDDO

	  NUMELP = COMPT
          DEALLOCATE(IX_TEMP)


C----------------------------------------------------------------------------------C
C-------------On split les elements SPRINGS - KJOINTS -----------------------------C
C----------------------------------------------------------------------------------C

          ALLOCATE(IX_TEMP(NUMELR*5+1))
          DO J=1,NUMELR
	    DO K=1,5
	      IX_TEMP(5*(J-1)+K)=IXR_KJ(5*(J-1)+K)
              IXR_KJ(5*(J-1)+K) = 0
            END DO
	  END DO

C-------------Split----------------------------------------------------------------C

          COMPT = 0
          COMPTB = 0
          DO J=1,NUMELR
          ID_PROP = CORESPRO(IXR(NIXR*(J-1)+1))
	    IF (IGEO(NPROPGI*(ID_PROP-1)+11)==45) THEN
               COMPTB = COMPTB + 1
               IF (TAG_ELR(J+NPART)/=0) THEN
                 COMPT = COMPT + 1
	         DO K=1,3
	           IXR_KJ(5*(COMPT-1)+K)=CORESN(IX_TEMP(5*(COMPTB-1)+K))
	         END DO
                 IXR_KJ(5*(COMPT-1)+4)=IX_TEMP(5*(COMPTB-1)+4)
                 IXR_KJ(5*(COMPT-1)+5)=0                                    	
               ENDIF
            ENDIF
          ENDDO

          IXR_KJ(5*NELRN+1) = COMPT
          DEALLOCATE(IX_TEMP)

C----------------------------------------------------------------------------------C
C-------------On split les elements SPRINGS----------------------------------------C
C----------------------------------------------------------------------------------C

          ALLOCATE(IX_TEMP(NUMELR*NIXR),CORESR(NUMELR))
          DO J=1,NUMELR
	    DO K=1,NIXR
	      IX_TEMP(NIXR*(J-1)+K)=IXR(NIXR*(J-1)+K)
            END DO
	  END DO
          DEALLOCATE(IXR)

C-------------Split----------------------------------------------------------------C

	  ALLOCATE(IXR(NELRN*NIXR))
          COMPT = 0
          DO J=1,NUMELR
	     COMPT_IP_TMP=COMPT_IP_TMP+1
             IF (TAG_ELR(J+NPART)/=0) THEN
	     	COMPT_IP=COMPT_IP+1
	        COMPT = COMPT+1
		CORESR(J)=COMPT
		IPART(COMPT_IP)=IPART_TEMP(COMPT_IP_TMP)
	        DO K=1,NIXR
		  IXR(NIXR*(COMPT-1)+K)=IX_TEMP(NIXR*(J-1)+K)
		END DO
		IXR(NIXR*(COMPT-1)+1)=CORESPRO(IX_TEMP(NIXR*(J-1)+1))
	        DO K=2,3
		  IXR(NIXR*(COMPT-1)+K)=CORESN(IX_TEMP(NIXR*(J-1)+K))
		END DO
		IF (IXR(NIXR*(COMPT-1)+4)/=0) THEN
		  IXR(NIXR*(COMPT-1)+4)=CORESN(IX_TEMP(NIXR*(J-1)+4))
		ENDIF
             ENDIF
          ENDDO

	  NUMELR = COMPT
          DEALLOCATE(IX_TEMP)

C----------------------------------------------------------------------------------C
C-------------On split les elements SH3N-------------------------------------------C
C----------------------------------------------------------------------------------C

	  NUMELTG0 = NUMELTG
          ALLOCATE(IX_TEMP(NUMELTG*NIXTG),CORESTG(NUMELTG))
          DO J=1,NUMELTG
	    DO K=1,NIXTG
	      IX_TEMP(NIXTG*(J-1)+K)=IXTG(NIXTG*(J-1)+K)
            END DO
	  END DO
          DEALLOCATE(IXTG)

C-------------Split----------------------------------------------------------------C

	  ALLOCATE(IXTG(NELTGN*NIXTG))
          COMPT = 0
          DO J=1,NUMELTG
	     COMPT_IP_TMP=COMPT_IP_TMP+1
             IF (TAG_ELG(J+NPART)/=0) THEN
	     	COMPT_IP=COMPT_IP+1
	        COMPT = COMPT+1
		CORESTG(J)=COMPT
		IPART(COMPT_IP)=IPART_TEMP(COMPT_IP_TMP)
	        DO K=1,NIXTG
		  IXTG(NIXTG*(COMPT-1)+K)=IX_TEMP(NIXTG*(J-1)+K)
		END DO
		IXTG(NIXTG*(COMPT-1)+1)=CORESMA(IX_TEMP(NIXTG*(J-1)+1))
		IXTG(NIXTG*(COMPT-1)+5)=CORESPRO(IX_TEMP(NIXTG*(J-1)+5))
	        DO K=2,4
		  IXTG(NIXTG*(COMPT-1)+K)=CORESN(IX_TEMP(NIXTG*(J-1)+K))
		END DO
             ENDIF
          ENDDO

	  NUMELTG = COMPT
          DEALLOCATE(IX_TEMP)

C----------------------------------------------------------------------------------C
C-------------On split les reserves de particules SPH------------------------------C
C----------------------------------------------------------------------------------C

          ALLOCATE(RES_TEMP(NBPARTINLET))
          DO J=1,NBPARTINLET
	    RES_TEMP(J) = RESERVEP(J)
	  END DO

C-------------Split----------------------------------------------------------------C
          COMPT = 0
          PART_RES = 0
          FIRST_CELL = FIRST_SPHRES
          DO J=1,NBPARTINLET
            IF (TAG_ELSP(FIRST_CELL+NPART)/=0) THEN
              COMPT = COMPT + 1
              RESERVEP(COMPT) = RES_TEMP(J)
            ENDIF
            DO K=1,RES_TEMP(J)
              INOD = KXSP(NISP*(FIRST_CELL-1)+3)
              FIRST_CELL = FIRST_CELL+1
            ENDDO
          ENDDO

          NBPARTINLET = COMPT
          DEALLOCATE(RES_TEMP)

C----------------------------------------------------------------------------------C
C-------------On split les particules SPH------------------------------------------C
C----------------------------------------------------------------------------------C

          COEFF = 0
          IF (NSPHN>0) COEFF = 1
          NUMSPHA = NUMSPH - NSPHRES
C
          ALLOCATE(KXSP_TEMP(NISP*NUMSPH),CORESSP(NUMSPH))
          DO J=1,NUMSPH
	    DO K=1,NISP
	      KXSP_TEMP(NISP*(J-1)+K)=KXSP(NISP*(J-1)+K)
            END DO
	  END DO
          DEALLOCATE(IXSP,KXSP,NOD2SP,SPBUF)

C-------------Split----------------------------------------------------------------C
	  ALLOCATE(IXSP(KVOISPH,NSPHN),KXSP(NISP*NSPHN),NOD2SP(COEFF*NUMNOD))
          ALLOCATE(SPBUF(NSPBUF*NSPHN))
          COMPT = 0
          NSPHRESN = 0
          DO J=1,NUMSPH
	     COMPT_IP_TMP=COMPT_IP_TMP+1
             IF (TAG_ELSP(J+NPART)/=0) THEN
	     	COMPT_IP=COMPT_IP+1
	        COMPT = COMPT+1
		CORESSP(J)=COMPT
                IF (J>=FIRST_SPHRES) NSPHRESN=NSPHRESN+1
		IPART(COMPT_IP)=IPART_TEMP(COMPT_IP_TMP)
	        DO K=1,NISP
		  KXSP(NISP*(COMPT-1)+K)=KXSP_TEMP(NISP*(J-1)+K)
		END DO
		KXSP(NISP*(COMPT-1)+3)=CORESN(KXSP_TEMP(NISP*(J-1)+3))
                NOD2SP(CORESN(KXSP_TEMP(NISP*(J-1)+3))) = COMPT
             ENDIF
          ENDDO
          IF ((COMPT/=0).AND.(COMPT/=NUMSPH)) THEN
            CALL ANCMSG(MSGID=1061,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1)
          ENDIF
	  NUMSPH = COMPT
          NUMSPHA = COMPT - NSPHRESN
          NSPHRES =  NSPHRESN
          FIRST_SPHRES = NUMSPHA + 1
          DEALLOCATE(KXSP_TEMP)

C----------------------------------------------------------------------------------C

          DEALLOCATE(IPART_TEMP)

C----------------------------------------------------------------------------------C
C-------------On split THKE pour les elts coques et sh3n --------------------------C
C----------------------------------------------------------------------------------C

	  ALLOCATE(THK_TMP(NUMELTG0+NUMELC0))
          DO J=1,NUMELTG0+NUMELC0
	    THK_TMP(J)=THKE(J)
	  END DO
          DEALLOCATE(THKE)

C-------------Split----------------------------------------------------------------C

	  ALLOCATE(THKE(NUMELTG+NUMELC))
          DO J=1,NUMELC0
             IF (TAG_ELC(J+NPART)/=0) THEN
		THKE(CORESC(J))=THK_TMP(J)
             ENDIF
          ENDDO
          DO J=1,NUMELTG0
             IF (TAG_ELG(J+NPART)/=0) THEN
		THKE(CORESTG(J)+NUMELC)=THK_TMP(J+NUMELC0)
             ENDIF
          ENDDO

          DEALLOCATE(THK_TMP)

C----------------------------------------------------------------------------------C
C-------------On split les groupes-------------------------------------------------C
C----------------------------------------------------------------------------------C

!!          ALLOCATE(BUF_TEMP(SIZE))
!!          DO I=1,SIZE
!!            BUF_TEMP(I)=BUF_NOD(I)
!!          END DO

C ---------> groupes d'elements----------------------------------------------------C
C             --> solides <--
          DO I=1,NGRBRIC
	    COMPT = 0
            IGRBRIC(I)%R2R_SHARE = 0
            DO J=1,IGRBRIC(I)%NENTITY
	      CUR_ID = IGRBRIC(I)%ENTITY(J)
              IF (TAG_ELS(CUR_ID+NPART)/=0)THEN
	        COMPT = COMPT+1
                IGRBRIC(I)%ENTITY(COMPT) = CORESS(CUR_ID)
                IF (TAG_ELS(CUR_ID+NPART)>1)
     .               IGRBRIC(I)%R2R_SHARE = IGRBRIC(I)%R2R_SHARE + 1
              ENDIF
            ENDDO
	    IGRBRIC(I)%R2R_ALL = IGRBRIC(I)%NENTITY
	    IGRBRIC(I)%NENTITY = COMPT
          ENDDO
C             --> quads <--
          DO I=1,NGRQUAD
	    COMPT = 0
            IGRQUAD(I)%R2R_SHARE = 0
            DO J=1,IGRQUAD(I)%NENTITY
	      CUR_ID = IGRQUAD(I)%ENTITY(J)
              IF (TAG_ELQ(CUR_ID+NPART)/=0)THEN
	        COMPT = COMPT+1
                IGRQUAD(I)%ENTITY(COMPT) = CORESQ(CUR_ID)
                IF (TAG_ELQ(CUR_ID+NPART)>1)
     .               IGRQUAD(I)%R2R_SHARE = IGRQUAD(I)%R2R_SHARE + 1
              ENDIF
            ENDDO
	    IGRQUAD(I)%R2R_ALL = IGRQUAD(I)%NENTITY
	    IGRQUAD(I)%NENTITY = COMPT
          ENDDO
C             --> sh4n <--
          DO I=1,NGRSHEL
	    COMPT = 0
            IGRSH4N(I)%R2R_SHARE = 0
            DO J=1,IGRSH4N(I)%NENTITY
	      CUR_ID = IGRSH4N(I)%ENTITY(J)
              IF (TAG_ELC(CUR_ID+NPART)/=0)THEN
	        COMPT = COMPT+1
                IGRSH4N(I)%ENTITY(COMPT) = CORESC(CUR_ID)
                IF (TAG_ELC(CUR_ID+NPART)>1)
     .               IGRSH4N(I)%R2R_SHARE = IGRSH4N(I)%R2R_SHARE + 1
              ENDIF
            ENDDO
	    IGRSH4N(I)%R2R_ALL = IGRSH4N(I)%NENTITY
	    IGRSH4N(I)%NENTITY = COMPT
          ENDDO
C             --> truss <--
          DO I=1,NGRTRUS
	    COMPT = 0
            IGRTRUSS(I)%R2R_SHARE = 0
            DO J=1,IGRTRUSS(I)%NENTITY
	      CUR_ID = IGRTRUSS(I)%ENTITY(J)
              IF (TAG_ELT(CUR_ID+NPART)/=0)THEN
	        COMPT = COMPT+1
                IGRTRUSS(I)%ENTITY(COMPT) = COREST(CUR_ID)
                IF (TAG_ELT(CUR_ID+NPART)>1)
     .               IGRTRUSS(I)%R2R_SHARE = IGRTRUSS(I)%R2R_SHARE + 1
              ENDIF
            ENDDO
	    IGRTRUSS(I)%R2R_ALL = IGRTRUSS(I)%NENTITY
	    IGRTRUSS(I)%NENTITY = COMPT
          ENDDO
C             --> beams <--
          DO I=1,NGRBEAM
	    COMPT = 0
            IGRBEAM(I)%R2R_SHARE = 0
            DO J=1,IGRBEAM(I)%NENTITY
	      CUR_ID = IGRBEAM(I)%ENTITY(J)
              IF (TAG_ELP(CUR_ID+NPART)/=0)THEN
	        COMPT = COMPT+1
                IGRBEAM(I)%ENTITY(COMPT) = CORESP(CUR_ID)
                IF (TAG_ELP(CUR_ID+NPART)>1)
     .               IGRBEAM(I)%R2R_SHARE = IGRBEAM(I)%R2R_SHARE + 1
              ENDIF
            ENDDO
	    IGRBEAM(I)%R2R_ALL = IGRBEAM(I)%NENTITY
	    IGRBEAM(I)%NENTITY = COMPT
          ENDDO
C             --> springs <--
          DO I=1,NGRSPRI
	    COMPT = 0
            IGRSPRING(I)%R2R_SHARE = 0
            DO J=1,IGRSPRING(I)%NENTITY
	      CUR_ID = IGRSPRING(I)%ENTITY(J)
              IF (TAG_ELR(CUR_ID+NPART)/=0)THEN
	        COMPT = COMPT+1
                IGRSPRING(I)%ENTITY(COMPT) = CORESR(CUR_ID)
                IF (TAG_ELR(CUR_ID+NPART)>1)
     .               IGRSPRING(I)%R2R_SHARE = IGRSPRING(I)%R2R_SHARE + 1
              ENDIF
            ENDDO
	    IGRSPRING(I)%R2R_ALL = IGRSPRING(I)%NENTITY
	    IGRSPRING(I)%NENTITY = COMPT
          ENDDO
C             --> sh3n <--
          DO I=1,NGRSH3N
	    COMPT = 0
            IGRSH3N(I)%R2R_SHARE = 0
            DO J=1,IGRSH3N(I)%NENTITY
	      CUR_ID = IGRSH3N(I)%ENTITY(J)
              IF (TAG_ELG(CUR_ID+NPART)/=0)THEN
	        COMPT = COMPT+1
                IGRSH3N(I)%ENTITY(COMPT) = CORESTG(CUR_ID)
                IF (TAG_ELG(CUR_ID+NPART)>1)
     .               IGRSH3N(I)%R2R_SHARE = IGRSH3N(I)%R2R_SHARE + 1
              ENDIF
            ENDDO
	    IGRSH3N(I)%R2R_ALL = IGRSH3N(I)%NENTITY
	    IGRSH3N(I)%NENTITY = COMPT
          ENDDO

C ---------> groupes de parts----------------------------------------------------C

          DO I=1,NGRPART
	    COMPT = 0
            DO J=1,IGRPART(I)%NENTITY
	       CUR_ID = IGRPART(I)%ENTITY(J)
               IF (TAG_PART(CUR_ID)==1)THEN
	         COMPT = COMPT+1
	         IGRPART(I)%ENTITY(COMPT) = CUR_ID
               ENDIF
            ENDDO
	    IGRPART(I)%R2R_ALL = IGRPART(I)%NENTITY
	    IGRPART(I)%NENTITY = COMPT
	  ENDDO
C----------------------------------------------------------------------------------C
C-------------On split les surfaces------------------------------------------------C
C----------------------------------------------------------------------------------C

          DO I=1,NB_SURF
	    NSEG = 0
	    CCPL = 0
            DO J=1,IGRSURF(I)%NSEG
	      NB_NOD_SUB=0
	      NB_NOD_CPL=0
	      TAG = 0
              CUR_ID = IGRSURF(I)%ELEM(J)
              IF (IGRSURF(I)%ELTYP(J) == 1) THEN
C---------------> cas face solide <--
		IF (TAG_ELS(CUR_ID+NPART)/=0) THEN
		   NEW_ID = CORESS(CUR_ID)
		   TAG = 1
	        ENDIF
	      ELSEIF (IGRSURF(I)%ELTYP(J) == 2) THEN
C---------------> cas quad <--
		IF (TAG_ELQ(CUR_ID+NPART)/=0) THEN
		   NEW_ID = CORESQ(CUR_ID)
		   TAG = 1
		ENDIF
              ELSEIF (IGRSURF(I)%ELTYP(J) == 3) THEN
C---------------> cas coque <--
	        IF (TAG_ELC(CUR_ID+NPART)/=0) THEN
	           NEW_ID = CORESC(CUR_ID)
		   TAG = 1
		ENDIF
              ELSEIF (IGRSURF(I)%ELTYP(J) == 7) THEN
C---------------> cas triangle <--
	        IF (TAG_ELG(CUR_ID+NPART)/=0) THEN
	           NEW_ID = CORESTG(CUR_ID)
		   TAG = 1
		ENDIF
              ELSEIF (IGRSURF(I)%ELTYP(J) > 10) THEN
C---------------> cas surface defini par segment avec elements associes <--
                 IF (IGRSURF(I)%ELTYP(J) == 11) THEN
		   IF (TAG_ELS(CUR_ID+NPART)/=0) TAG=1
                 ELSEIF (IGRSURF(I)%ELTYP(J) == 13) THEN
		   IF (TAG_ELC(CUR_ID+NPART)/=0) TAG=1
                 ELSEIF (IGRSURF(I)%ELTYP(J) == 17) THEN
		   IF (TAG_ELG(CUR_ID+NPART)/=0) TAG=1
		 ENDIF
C                  ---> on remet le type de seg a zero pour <--
		 IGRSURF(I)%ELTYP(J) = 0
		 NEW_ID = 0
              ELSEIF (IGRSURF(I)%ELTYP(J) == 0) THEN
C---------------> cas surface defini par segment par noeuds <--
                DO K=1,4
	          NOD_ID = IGRSURF(I)%NODES(J,K)
		  IF (TAGNO(NOD_ID+NPART)/=-1) NB_NOD_CPL=NB_NOD_CPL+1
                END DO
                IF (NB_NOD_CPL==4) THEN
		  TAG = 1
		  NEW_ID = 0
                ENDIF
	      ENDIF
C---------------> TAG = 1, on garde le segment, on update les noeuds <--
              IF (TAG == 1) THEN
	        NSEG = NSEG + 1
                DO K=1,4
	          CUR_ID = IGRSURF(I)%NODES(J,K)
		  IF (TAGNO(CUR_ID+NPART)>1) NB_NOD_CPL=NB_NOD_CPL+1
                  IGRSURF(I)%NODES(NSEG,K) = CORESN(CUR_ID)
                END DO
		IF (NB_NOD_CPL==4) CCPL=CCPL+1
		IGRSURF(I)%ELTYP(NSEG) = IGRSURF(I)%ELTYP(J)
		IGRSURF(I)%ELEM(NSEG) = NEW_ID
              ENDIF
	    END DO
	    ISURF_R2R(2,I)  = IGRSURF(I)%NSEG
	    IGRSURF(I)%NSEG = NSEG
	    ISURF_R2R(1,I)  = CCPL
          END DO

C----------------------------------------------------------------------------------C
C-------------On split les lignes--------------------------------------------------C
C----------------------------------------------------------------------------------C
          DO I=1,NB_LINE
	    NSEG = 0
            DO J=1,IGRSLIN(I)%NSEG
	      NB_NOD_SUB=0
	      TAG = 0
              CUR_ID = IGRSLIN(I)%ELEM(J)
              IF (IGRSLIN(I)%ELTYP(J)==1) THEN
C---------------> cas face solide <--
		IF (TAG_ELS(CUR_ID+NPART)/=0) THEN
		   NEW_ID = CORESS(CUR_ID)
		   TAG = 1
	        ENDIF
	      ELSEIF (IGRSLIN(I)%ELTYP(J)==2) THEN
C---------------> cas quad <--
		IF (TAG_ELQ(CUR_ID+NPART)/=0) THEN
		   NEW_ID = CORESQ(CUR_ID)
		   TAG = 2
		ENDIF
              ELSEIF (IGRSLIN(I)%ELTYP(J)==3) THEN
C---------------> cas coque <--
	        IF (TAG_ELC(CUR_ID+NPART)/=0) THEN
	           NEW_ID = CORESC(CUR_ID)
		   TAG = 3
		ENDIF
              ELSEIF (IGRSLIN(I)%ELTYP(J)==4) THEN
C---------------> cas truss <--
	        IF (TAG_ELT(CUR_ID+NPART)/=0) THEN
	           NEW_ID = COREST(CUR_ID)
		   TAG = 4
		ENDIF
              ELSEIF (IGRSLIN(I)%ELTYP(J)==5) THEN
C---------------> cas poutre <--
	        IF (TAG_ELP(CUR_ID+NPART)/=0) THEN
	           NEW_ID = CORESP(CUR_ID)
		   TAG = 5
		ENDIF
              ELSEIF (IGRSLIN(I)%ELTYP(J)==6) THEN
C---------------> cas spring <--
	        IF (TAG_ELR(CUR_ID+NPART)/=0) THEN
	           NEW_ID = CORESR(CUR_ID)
		   TAG = 6
		ENDIF
              ELSEIF (IGRSLIN(I)%ELTYP(J)==7) THEN
C---------------> cas triangle <--
	        IF (TAG_ELG(CUR_ID+NPART)/=0) THEN
	           NEW_ID = CORESTG(CUR_ID)
		   TAG = 7
		ENDIF
              ELSEIF (IGRSLIN(I)%ELTYP(J)==0) THEN
C---------------> Pas d'element, on compte les noeuds tags <--
                NEW_ID = 0
                DO K=1,2
	          CUR_ID = IGRSLIN(I)%NODES(J,K)
                  IF (TAGNO(CUR_ID+NPART)>=0) NB_NOD_SUB=NB_NOD_SUB+1
                END DO
		IF (NB_NOD_SUB==2) TAG = 8
	      ENDIF
C---------------> TAG = 1, on garde le segment, on update les neouds <--
              IF (TAG > 0) THEN
	        NSEG = NSEG + 1
                DO K=1,2
	          CUR_ID = IGRSLIN(I)%NODES(J,K)
                  IGRSLIN(I)%NODES(NSEG,K) = CORESN(CUR_ID)
                END DO
                IGRSLIN(I)%ELTYP(NSEG) = IGRSLIN(I)%ELTYP(J)
		IGRSLIN(I)%ELEM(NSEG)  = NEW_ID
              ENDIF
	    END DO
            IGRSLIN(I)%NSEG_R2R_ALL = IGRSLIN(I)%NSEG
	    IGRSLIN(I)%NSEG = NSEG
          END DO

C----------------------------------------------------------------------------------C
C-------------On split les groupes de noeuds---------------------------------------C
C----------------------------------------------------------------------------------C

        DO I=1,NGRNOD
! ---------> groupes de noeuds
          COMPT = 0
          CCPL = 0
          DO J=1,IGRNOD(I)%NENTITY
            CUR_ID = IGRNOD(I)%ENTITY(J)
            IF (TAGNO(CUR_ID+NPART) >= 0) THEN
              COMPT = COMPT + 1
              IGRNOD(I)%ENTITY(COMPT) = CORESN(CUR_ID)
            ENDIF
            IF (TAGNO(CUR_ID+NPART)>1) CCPL=CCPL+1
          ENDDO
          IGRNOD(I)%R2R_ALL   = IGRNOD(I)%NENTITY
          IGRNOD(I)%R2R_SHARE = CCPL
          IGRNOD(I)%NENTITY   = COMPT
        ENDDO ! DO I=1,NGRNOD

C----------------------------------------------------------------------------------C
C--------------Mise a jour de TAGNO------------------------------------------------C
C----------------------------------------------------------------------------------C

   	  ALLOCATE(TAGNO_TEMP(2*NUMNOD_OLD+NPART))
          DO J=1,NPART+2*NUMNOD_OLD
	    TAGNO_TEMP(J)=TAGNO(J)
	  END DO

          DEALLOCATE(TAGNO)
	  ALLOCATE(TAGNO(2*NUMNOD+NPART))
          DO J=1,NPART
            TAGNO(J)=TAGNO_TEMP(J)
          END DO
          COMPT=0
          DO J=1,NUMNOD_OLD
            IF (TAGNO_TEMP(J+NPART)>=0)THEN
		COMPT=COMPT+1
		TAGNO(COMPT+NPART)=TAGNO_TEMP(J+NPART)
		TAGNO(COMPT+NPART+NUMNOD)=TAGNO_TEMP(J+NPART+NUMNOD_OLD)
            ENDIF
          ENDDO

C----------------------------------------------------------------------------------C
C------Prlecture et tag des SECTIONS-----------------------------------------------C
C----------------------------------------------------------------------------------C

         CALL PRELECSEC(
     1                  NUL,NUL,ITABM1,2,NOM_OPT(LNOPT1*INOM_OPT(8)+1),
     2                  IGRBRIC ,IGRQUAD   ,IGRSH4N ,IGRSH3N  ,IGRTRUSS,
     3                  IGRBEAM ,IGRSPRING ,IGRNOD, LSUBMODEL )

C----------------------------------------------------------------------------------C
C------On split les GAUGES---------------------------------------------------------C
C----------------------------------------------------------------------------------C

         COMPT = 0
         DO I=1,NBGAUGE
           IF ((TAGGAU(I)>0).AND.(NUMELS>0)) THEN
C---------------> Cas des GAUGES <--
             COMPT = COMPT + 1
           ELSEIF ((TAGGAU(I)<0).AND.(NUMSPH>0)) THEN
C---------------> Cas des GAUGE/SPH <--
             TAGGAU(I) = ABS(TAGGAU(I))
             COMPT = COMPT + 1
           ELSE
             TAGGAU(I) = 0
           ENDIF
         ENDDO

         NBGAUGE = COMPT

C----------------------------------------------------------------------------------C
C------On split RBY_MSN (utilise pour inivel axis) --------------------------------C
C----------------------------------------------------------------------------------C

         NRBODY_OLD = SIZE(TAGRBY)
         ALLOCATE(RBY_MSN_TEMP(2,NRBODY_OLD))
         DO I=1,NRBODY_OLD
	   RBY_MSN_TEMP(1,I) = RBY_MSN(1,I)
           RBY_MSN_TEMP(2,I) = RBY_MSN(2,I)
           RBY_MSN(1,I) = 0
           RBY_MSN(2,I) = 0
         ENDDO

C-------------Split----------------------------------------------------------------C

         NRB =0
         DO I=1,NRBODY_OLD
           IF(TAGRBY(I) > 0) THEN
             NRB = NRB + 1
             RBY_MSN(1,NRB) = RBY_MSN_TEMP(1,I)
             RBY_MSN(2,NRB) = CORESN(RBY_MSN_TEMP(2,I))
           END IF
         ENDDO

C------------------------------------------------------------------------------------C

         FLG_SPLIT = 1
!!         DEALLOCATE(BUF_TEMP)
         DEALLOCATE(CORESC,CORESN,CORESTG,COREST)
         DEALLOCATE(CORESPRO,CORESR,CORESP,CORESS)
         DEALLOCATE(CORESQ,EANI_TEMP,TAGNO_TEMP)

C------------------------------------------------------------------------------------C

         DEALLOCATE(TAG_ELC,TAG_ELS,TAG_ELP,TAG_ELQ,TAG_ELR,TAG_ELT)
         DEALLOCATE(TAG_ELG,TAG_SURF,TAG_PROP,TAG_ELCF2,TAG_ELSF2)

C----------------------------------------------------------------------------------C
C------Recriture de TAGMAT et TAGPROPpour le groupage------------------------------C
C----------------------------------------------------------------------------------C
	DO K=1,NPART
          IF(TAG_PART(K) == 0) THEN
            TAG_MAT(IPART(LIPART1*(K-1)+1))=0
          ENDIF
	END DO

C------------------------------------------------------------------------------------

      ENDIF
      RETURN

C------------------------------------------------------------------------------------C
 999  CALL FREERR(3)
      RETURN

      END
