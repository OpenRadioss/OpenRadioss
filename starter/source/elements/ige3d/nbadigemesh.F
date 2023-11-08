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
Chd|  NBADIGEMESH                   source/elements/ige3d/nbadigemesh.F
Chd|-- called by -----------
Chd|        CONTRL                        source/starter/contrl.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        CONSTIT                       source/elements/nodes/constit.F
Chd|        FREDEC0                       source/starter/freform.F      
Chd|        FREDEC_2KEY_4ID_T             source/starter/freform.F      
Chd|        FREDEC_KEY_3ID_T              source/starter/freform.F      
Chd|        FREERR                        source/starter/freform.F      
Chd|        PRERAFIG3D                    source/elements/ige3d/prerafig3d.F
Chd|        NINTRI                        source/system/nintrr.F        
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESHSURFIG3D_MOD              source/elements/ige3d/meshsurfig3d_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE NBADIGEMESH(LSUBMODEL,NUMNUSR)
C----------------------------------------------------------------------
C   ROUTINE BASEE SUR NBADMESH MAIS SPECIFIQUE AUX ELEMENTS ISOGEOMETRIQUES
C   PERMET LA LECTURE ET LA CONSTRUCTION DE TOUT LES TABLEAUX NECESSAIRES
C   AU RAFFINEMENT DES ELEMENTS IGEO
C   NB : PEUT ETRE ONE PEU REORGANISEE POUR UNE MEILLEURE LECTURE
C----------------------------------------------------------------------
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE MESHSURFIG3D_MOD  
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
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "submod_c.inc"
#include      "tabsiz_c.inc"
#include      "ige3d_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMNUSR
C     REAL
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER, DIMENSION(:), ALLOCATABLE   :: ITAB,ITABM1,SUBID_NODES
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: KXIG3D,IGEO
      INTEGER, DIMENSION(:), ALLOCATABLE   :: IPARTIG3D,IXIG3D,KNOD2ELIG3D,NOD2ELIG3D
      my_real, DIMENSION(:), ALLOCATABLE   :: KNOTLOCPC,KNOTLOCEL,KNOT
      TYPE(TABCONPATCH_IG3D_), DIMENSION(:), ALLOCATABLE, TARGET :: TABCONPATCH
      TYPE(TABCONPATCH_IG3D_), POINTER :: PTABCONPATCH
      INTEGER IPART(4,NPART),TABIDS(NUMELIG3D0),
     .        N,ID,I,J,NLEV,NMUL,J10(10),STAT,NCTRLMAX
      INTEGER USR2SYS,NUMNUSR1,IDS,NI,NJ,NK,NL,K,L,P,Q,QQ,
     .        NN,IAD,IDX1,IDY1,IDZ1,NCTRL,BID,NUM,
     .        NRAFX,NRAFY,NRAFZ,NBLINE,D1,D2,D3,N1,N2,N3
      INTEGER IAD_KNOT,IG,UID,SUB_ID,INTRULE,RAFRULE,
     .        NKNOT1,NKNOT2,NKNOT3,ITGEO,PX,PY,PZ,PID,IPID,MAXNUMGEO,
     .        NBRAFX,NBRAFY,NBRAFZ,NBIG3D_PATCH
      INTEGER OFF_NOD(NSUBMOD), OFF_DEF(NSUBMOD)
C     REAL
      CHARACTER MESS*40,TITR*nchartitle,KEY*ncharkey,IDTITL*nchartitle
      my_real R5(5),RBID
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NINTRI
C-----------------------------------------------
      DATA MESS /'OPTIONS FOR ISOGEOMETRIC MESHING DEFINITION'/
C-----------------------------------------------
      IPART=0
      NCTRLMAX=0
      BID=0
      NRAFMAX=8
      MAXNUMGEO=0
      DEG_MAX=0
      RBID=0
C------
      ALLOCATE(ITAB(NUMNUSR),ITABM1(2*NUMNUSR),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                          MSGTYPE=MSGERROR,
     .                         C1='ITAB') 
      ALLOCATE (SUBID_NODES(NUMNUSR),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='SUBID_NODES') 
      SUBID_NODES(1:NUMNUSR) = 0
C--------------------------------------
C     LECTURE DES PROP IGE
C--------------------------------------
      NUMGEO=NSLASH(KPROP)
      ALLOCATE (IGEO(NPROPGI,NUMGEO),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='IGEO')
      IGEO = 0
C
      KCUR = KPROP
      IREC=KOPTAD(KCUR)-1
      SKNOT = 0
      IAD_KNOT = 0
      DO ITGEO=1,NUMGEO
        KLINE(1:1)=' '
        DO WHILE(KLINE(1:1)/='/')
          IREC=IREC+1
          READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
          KLINE=LINE
        ENDDO
        CALL FREDEC_2KEY_4ID_T(KEY,IG,UID,BID,SUB_ID,IDTITL)
        IF(KEY(1:6)=='TYPE47'.OR. KEY(1:5)=='IGE3D')THEN
          IGEO(1,ITGEO)=IG
          IREC=IREC+1
          READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
          KLINE=LINE
          READ(LINE,ERR=999,FMT=FMT_2I) INTRULE,RAFRULE
          IREC=IREC+1
          READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
          KLINE=LINE
          READ(LINE,ERR=999,FMT=FMT_6I)
     .     D1,D2,D3,N1,N2,N3
          IGEO(40,ITGEO) = IAD_KNOT
          IGEO(41,ITGEO) = D1+1
          IGEO(42,ITGEO) = D2+1
          IGEO(43,ITGEO) = D3+1
          IGEO(44,ITGEO) = N1
          IGEO(45,ITGEO) = N2
          IGEO(46,ITGEO) = N3
          DEG_MAX=MAX(DEG_MAX,D1+2,D2+2,D3+2)
          IREC=IREC+1
          READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
          KLINE=LINE
          DO WHILE(KLINE(1:1)/='/')
            IREC=IREC+1
            SKNOT = SKNOT + 5
            READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
            KLINE=LINE
          ENDDO
          IREC=IREC-1
c
        ENDIF
      ENDDO
c
      ALLOCATE(KNOT(SKNOT),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                          MSGTYPE=MSGERROR,
     .                         C1='KNOT') 
      KNOT=0
C--------------------------------------
C     LECUTRE DES PART
C--------------------------------------
      KCUR = KPART
      IREC=KOPTAD(KCUR)-1
      DO I=1,NPART
        IREC=IREC+1
        READ(IIN,REC=IREC,ERR=999,FMT='(A)')KLINE
        CALL FREDEC_KEY_3ID_T(ID,BID,UID,TITR)
        IREC=IREC+1
        READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
        READ(LINE,ERR=999,FMT=FMT_I)PID
        IPID = NINTRI(PID,IGEO,NPROPGI,NUMGEO,1)   !  ICI REND IPID BIZARRE 
C
        IPART(2,I)=IPID
        IPART(4,I)=ID
      ENDDO


      IADMSTAT=1

C------
      IF(IADMSTAT /= 0) ID_LIMIT_ADMESH=ID_LIMIT
C------
C--------------------------------------
C     DIMENSIONNEMENT DE KNOT
C--------------------------------------
      KCUR = KPROP
      IREC=KOPTAD(KCUR)-1
      IAD_KNOT = 0
      DO ITGEO=1,NUMGEO
        KLINE(1:1)=' '
        DO WHILE(KLINE(1:1)/='/')
          IREC=IREC+1
          READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
          KLINE=LINE
        ENDDO
        CALL FREDEC_2KEY_4ID_T(KEY,IG,UID,BID,SUB_ID,IDTITL)
        IF(KEY(1:6)=='TYPE47'.OR. KEY(1:5)=='IGE3D')THEN
          IREC=IREC+1
          READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
          KLINE=LINE
          READ(LINE,ERR=999,FMT=FMT_2I) INTRULE,RAFRULE
          IREC=IREC+1
          READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
          KLINE=LINE
          READ(LINE,ERR=999,FMT=FMT_6I)
     .     D1,D2,D3,N1,N2,N3
          IGEO(40,ITGEO) = IAD_KNOT
          IGEO(41,ITGEO) = D1+1
          IGEO(42,ITGEO) = D2+1
          IGEO(43,ITGEO) = D3+1
          IGEO(44,ITGEO) = N1
          IGEO(45,ITGEO) = N2
          IGEO(46,ITGEO) = N3
          NKNOT1 = N1+D1+1
          NKNOT2 = N2+D2+1
          NKNOT3 = N3+D3+1
          DO I=1,((N1+D1)/5)+1
            IREC=IREC+1
            READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
            KLINE=LINE
            READ(LINE,ERR=999,FMT=FMT_5F) R5
            DO J=1,5
             IF(IAD_KNOT < NKNOT1+IGEO(40,ITGEO))THEN
               IAD_KNOT = IAD_KNOT + 1
               KNOT(IAD_KNOT) = R5(J)
             ENDIF
            ENDDO
          ENDDO
C
          DO I=1,((N2+D2)/5)+1
            IREC=IREC+1
            READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
            KLINE=LINE
            READ(LINE,ERR=999,FMT=FMT_5F) R5
            DO J=1,5
             IF(IAD_KNOT < NKNOT1+NKNOT2+IGEO(40,ITGEO))THEN
               IAD_KNOT = IAD_KNOT + 1
               KNOT(IAD_KNOT) = R5(J)
             ENDIF
            ENDDO
          ENDDO
C
          DO I=1,((N3+D3)/5)+1
            IREC=IREC+1
            READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
            KLINE=LINE
            READ(LINE,ERR=999,FMT=FMT_5F) R5
            DO J=1,5
             IF(IAD_KNOT < NKNOT1+NKNOT2+NKNOT3+IGEO(40,ITGEO))THEN
               IAD_KNOT = IAD_KNOT + 1
               KNOT(IAD_KNOT) = R5(J)
             ENDIF
            ENDDO
          ENDDO
C
          SKNOT=IAD_KNOT
c
        ENDIF
      ENDDO
C--------------------------------------
C     PRELECTURE DES IGE3D POUR DIMMENSIONNEMENT IXIG3D ET TABCONPATCH
C--------------------------------------
c
      NBIG3D_PATCH = 0
      NBPART_IG3D = 0
c
      NUM  = 0
      NBFILSMAX = 1
      NBMESHSURF = 0
      ADDELIG3D = 0
      KCUR = KIGE3D
      NBPART_IG3D = NBPART_IG3D+1
      IREC = KOPTAD(KCUR)
      IREC=IREC+1
      READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
      
c  IL FAUT DIFFERENCIER LES PART IGE3D DES AUTRES
   
      DO WHILE( LINE(1:1) /= '/' .OR. LINE(1:6) == '/IGE3D')
     
       IF (LINE(1:1) == '/')THEN  !  ON A ONE CHANGEMENT DE PART
         IREC=IREC+1
         READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
       ENDIF
       
       READ(LINE,ERR=999,FMT=FMT_8I)
     . ID,IDX1,IDY1,IDZ1,NCTRL,NBRAFX,NBRAFY,NBRAFZ
       NBIG3D_PATCH=NBIG3D_PATCH+1
       NCTRLMAX = MAX(NCTRLMAX,NCTRL)
       NUM = NUM + NCTRL
       NBFILSMAX = MAX(NBFILSMAX,NBRAFX*NBRAFY*NBRAFZ + 1)
       NBMESHSURF = NBMESHSURF + MAX(NBRAFX-1,0) + MAX(NBRAFY-1,0) + MAX(NBRAFZ-1,0)
       ADDELIG3D = ADDELIG3D + NBRAFX*NBRAFY*NBRAFZ
       IREC = IREC + ((NCTRL-1)/10)+2
       READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
       
       IF (LINE(1:6) == '/IGE3D')THEN  !  ON A ONE CHANGEMENT DE PART
         NBPART_IG3D = NBPART_IG3D+1
         NBIG3D_PATCH=0
         IREC=IREC+1
         READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
       ENDIF
       
      ENDDO
c
      ALLOCATE(IXIG3D(NUM+ADDELIG3D*NCTRLMAX),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                          MSGTYPE=MSGERROR,
     .                         C1='IXIG3D')
      SIXIG3D=NUM 
c
      ALLOCATE(TABCONPATCH(NBPART_IG3D),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                          MSGTYPE=MSGERROR,
     .                         C1='TABCON_PATCH')
C--------------------------------------
C     NOUVELLE PRELECTURE DES IGE3D POUR DES LISTES D'ELEMENTS DE TABCONPATCH
C--------------------------------------
c
      NBIG3D_PATCH = 0
      NBPART_IG3D = 0
c
      KCUR = KIGE3D
      NBPART_IG3D = NBPART_IG3D+1
      PTABCONPATCH => TABCONPATCH(NBPART_IG3D)
      PTABCONPATCH%ID_TABCON=NBPART_IG3D
      IREC = KOPTAD(KCUR)
      IREC=IREC+1
      READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
      DO WHILE( LINE(1:1) /= '/' .OR. LINE(1:6) == '/IGE3D')
      
       IF (LINE(1:1) == '/')THEN
         IREC=IREC+1
         READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
       ENDIF
       
       READ(LINE,ERR=999,FMT=FMT_8I)
     . ID,IDX1,IDY1,IDZ1,NCTRL,NBRAFX,NBRAFY,NBRAFZ
       NBIG3D_PATCH=NBIG3D_PATCH+1
       IREC = IREC + ((NCTRL-1)/10)+2
       READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
       
       IF (LINE(1:6) == '/IGE3D')THEN  !  ON A ONE CHANGEMENT DE PART
         PTABCONPATCH%L_TAB_IG3D=NBIG3D_PATCH
         ALLOCATE(PTABCONPATCH%TAB_IG3D(NBIG3D_PATCH),STAT=stat)
         IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='TABCON_PATCH')
         ALLOCATE(PTABCONPATCH%INITIAL_CUT(3,NBIG3D_PATCH),STAT=stat)
         IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='TABCON_PATCH')
         NBPART_IG3D = NBPART_IG3D+1
         PTABCONPATCH => TABCONPATCH(NBPART_IG3D)
         NBIG3D_PATCH=0
         IREC=IREC+1
         READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
       ENDIF
       
      ENDDO
c
      PTABCONPATCH%L_TAB_IG3D=NBIG3D_PATCH
      ALLOCATE(PTABCONPATCH%TAB_IG3D(NBIG3D_PATCH),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                          MSGTYPE=MSGERROR,
     .                          C1='TABCON_PATCH')
      ALLOCATE(PTABCONPATCH%INITIAL_CUT(3,NBIG3D_PATCH),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                          MSGTYPE=MSGERROR,
     .                          C1='TABCON_PATCH')
C
C--------------------------------------------------
C      HM OFFSETS IDs
C--------------------------------------------------
      DO I=1,NSUBMOD
        OFF_NOD(I) = LSUBMODEL(I)%OFF_NOD
        OFF_DEF(I) = LSUBMODEL(I)%OFF_DEF
      ENDDO
C--------------------------------------------------
C      READING NODES IDs IN HM STRUCTURE
C-------------------------------------------------- 
      CALL CPP_NODE_COUNT(NUMNUSR1)
      CALL CPP_NODE_ID_READ(ITAB,SUBID_NODES) 
C--------------------------------------------------
C      CHECKS NODES IDs
C--------------------------------------------------  
      DO I=1,NUMNUSR1
C--------------------------------------------------
C      SUBMODEL OFFSET
C--------------------------------------------------
        IF(SUBID_NODES(I) /= 0)THEN
          IF(ITAB(I) /= 0) ITAB(I) = ITAB(I) + OFF_NOD(SUBID_NODES(I))
        ENDIF
	IF (ITAB(I) > ID_LIMIT_ADMESH
     .        .AND. (ITAB(I) <  IDFT_NODE_AUTO.OR.
     .               ITAB(I) >= IDLT_NODE_AUTO)) THEN
              CALL ANCMSG(MSGID=1069,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                    I1=ITAB(I),C1=LINE,C2='/NODE') 
	ENDIF 			 
      ENDDO
      IF(ALLOCATED(SUBID_NODES)) DEALLOCATE(SUBID_NODES)
C--------------------------------------------------
C     LECTURE DES CNODES
C--------------------------------------------------
      N = NUMNUSR1
      KCUR = KCNODE                                     
      IREC = KOPTAD(KCUR)-1                              
      DO I=1,NLINE(KCUR)+NSLASH(KCUR)                      
        IREC=IREC+1                                    
        READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
        IF(LINE(1:1)=='/')THEN                       
          KLINE=LINE
        ELSE                                           
          N=N+1 
          READ(LINE,ERR=999,FMT=FMT_I)             
     .           ITAB(N)        
	  IF (ITAB(N) > ID_LIMIT_ADMESH
     .        .AND. (ITAB(N) <  IDFT_NODE_AUTO.OR.
     .               ITAB(N) >= IDLT_NODE_AUTO)) THEN       
              CALL ANCMSG(MSGID=1069,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                    I1=ITAB(N),C1=LINE,C2='/CNODE') 
          ENDIF                          
        ENDIF                             
      ENDDO 
C------
C     CONSTITUTION DU TABLEAU INVERSE DES NOEUDS
C------
C     NUMNUSR=NUMNUSR1+NUMCNOD !
      CALL CONSTIT(ITAB,ITABM1,NUMNUSR)
C--------------------------------------
C     LECTURE DES IGE3D
C--------------------------------------
c
      ALLOCATE(KXIG3D(NIXIG3D,NUMELIG3D0+ADDELIG3D),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                          MSGTYPE=MSGERROR,
     .                         C1='KXIG3D')    
      KXIG3D=0 

      ALLOCATE(IPARTIG3D(NUMELIG3D0+ADDELIG3D),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                          MSGTYPE=MSGERROR,
     .                         C1='IPARTIG3D')    
      IPARTIG3D=0 
C
      NBPART_IG3D = 0
      NBIG3D_PATCH = 0
c
        IAD =1
        KCUR=KIGE3D
        IREC=KOPTAD(KCUR)-1 
        I = 0
        INOD_IGE = FIRSTNOD_ISOGEO
        DO WHILE( I < NUMELIG3D0 )              
         IREC=IREC+1                        
         READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
         IF (LINE(1:1) == '/')THEN  
          NBPART_IG3D = NBPART_IG3D+1
          NBIG3D_PATCH = 0
          PTABCONPATCH => TABCONPATCH(NBPART_IG3D)     
          KLINE=LINE                                     
          CALL FREDEC0(ID)
          IDS=0
          DO J=1,NPART
            IF(IPART(4,J) == ID)IDS=J
          ENDDO
          PTABCONPATCH%PID=IDS
         ELSE
c  ICI ON SAIT DANS QUEL PART IGE ON EST (IDS) NUMGEO MAIS IG3D. ON PEUT DONC POINTER VERS LA BONNE TABCONPATCH_IG3D

          I = I + 1
          KXIG3D(1,I) =IPART(1,IDS)  ! ON SE FICHE DU 1 (PM)
          KXIG3D(2,I) =IPART(2,IDS)
          MAXNUMGEO=MAX(MAXNUMGEO,IPART(2,IDS))
          KXIG3D(4,I) =IAD
          IPARTIG3D(I)=IDS
C
          READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
          READ(LINE,ERR=999,FMT=FMT_8I) ID,IDX1,IDY1,IDZ1,NCTRL,NRAFX,NRAFY,NRAFZ
          NBIG3D_PATCH = NBIG3D_PATCH + 1
          PTABCONPATCH%TAB_IG3D(NBIG3D_PATCH)=I ! ID
          PTABCONPATCH%INITIAL_CUT(1,NBIG3D_PATCH)=NRAFX
          PTABCONPATCH%INITIAL_CUT(2,NBIG3D_PATCH)=NRAFY
          PTABCONPATCH%INITIAL_CUT(3,NBIG3D_PATCH)=NRAFZ
          NCTRLMAX = MAX(NCTRLMAX,NCTRL)
          KXIG3D(3,I)=NCTRL
          KXIG3D(5,I)=ID
          KXIG3D(6,I)=IDX1
          KXIG3D(7,I)=IDY1
          KXIG3D(8,I)=IDZ1
          KXIG3D(12,I)=MAX(NRAFX,1)
          KXIG3D(13,I)=MAX(NRAFY,1)
          KXIG3D(14,I)=MAX(NRAFZ,1)
          KXIG3D(15,I)=INOD_IGE
          INOD_IGE = INOD_IGE + 64
C                  
          NBLINE= ((NCTRL-1)/10)+1

          DO N=1,NBLINE
            IREC=IREC+1
            READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
            READ(LINE,ERR=999,FMT=FMT_10I) J10
            DO J=1,10
             IF(J10(J) /= 0)THEN
               IXIG3D(IAD)=USR2SYS(J10(J),ITABM1,MESS,ID)   ! ITABM1 donne de la merde
               IAD=IAD+1
             ENDIF
            ENDDO
          ENDDO
         ENDIF
        ENDDO
cC-------------------------------------
cC Recherche des ID doubles
cC-------------------------------------
c      DO I=1,NUMELIG3D0
c         TABIDS(I)= KXIG3D(5,I)
c      ENDDO
c      CALL UDOUBLE(TABIDS,1,NUMELIG3D0,MESS,0,BID)
C------
C     inverse connectivity at level 0
C------
      ALLOCATE(KNOD2ELIG3D(NUMNOD+1),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='KNOD2ELIG3D')
      KNOD2ELIG3D=0
      ALLOCATE(NOD2ELIG3D(NCTRLMAX*NUMELIG3D),
     .         STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='NOD2ELIG3D')
      NOD2ELIG3D=0

      DO I=1,NUMELIG3D0
       PX = IGEO(41,KXIG3D(2,I))
       PY = IGEO(42,KXIG3D(2,I))
       PZ = IGEO(43,KXIG3D(2,I))
        DO K=1,PX*PY*PZ
          N = IXIG3D(KXIG3D(4,I)+K-1)
          KNOD2ELIG3D(N) = KNOD2ELIG3D(N) + 1
        END DO
      END DO
C
      DO I=1,NUMNOD
        KNOD2ELIG3D(I+1) = KNOD2ELIG3D(I+1) + KNOD2ELIG3D(I)
      END DO
C
      DO N=NUMNOD,1,-1
        KNOD2ELIG3D(N+1)=KNOD2ELIG3D(N)
      END DO
      KNOD2ELIG3D(1)=0
c
C------
C   LE TRAVAIL DE RAFFINEMENT COMMENCE ICI, AVANT C'EST SEULEMENT DE LA LECTURE
C------

CC ATTENTION AU DIMENSIONNEMENT DES TABLEAUX BASES SUR LES POINTS CAR IL FAUT PRENDRE EN COMPTE LEE NOMBRE DE POINTS EN PLUS
CC QUI NE RESTERA PAS, PAS SEULEMENT LES POINTS FINAUX
c
      SKNOTLOCPC = DEG_MAX*3*(NUMNODIGE0+2*ADDELIG3D*NCTRLMAX)*MAXNUMGEO ! ON PEUT ENCORE AMELIORER
      ALLOCATE (KNOTLOCPC(SKNOTLOCPC)    ,STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                        C1='KNOTLOCPC')
      KNOTLOCPC(:)=0
C
      SKNOTLOCEL = 2*3*(NUMELIG3D0+ADDELIG3D)
      ALLOCATE (KNOTLOCEL(SKNOTLOCEL)    ,STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                        C1='KNOTLOCEL')
      KNOTLOCEL(:)=0
C
      ADDSIXIG3D = 0
      NBNEWX_FINAL = 0
      IF(ADDELIG3D>0) THEN
        CALL PRERAFIG3D(KNOT,KNOTLOCPC,KNOTLOCEL,
     .                  KXIG3D,IXIG3D,IGEO,
     .                  IPARTIG3D,
     .                  RBID,RBID,RBID,RBID,RBID,TABCONPATCH,0)
      ENDIF

      NUMNOD=NUMNOD + NBNEWX_FINAL
      print*,'NBNEWX_FINAL',NBNEWX_FINAL
      print*,'ADDELIG3D',ADDELIG3D
      NUMELIG3D = NUMELIG3D + ADDELIG3D
      IF(NBNEWX_FINAL/=0) THEN
       NADIGEMESH=1
      ENDIF
      FIRSTNOD_ISOGEO=NUMNOD+1
C
C-------------------------------------
      DO I=1,NBPART_IG3D
        IF(TABCONPATCH(I)%L_TAB_IG3D/=0) DEALLOCATE(TABCONPATCH(I)%TAB_IG3D,TABCONPATCH(I)%INITIAL_CUT)
      ENDDO
c
      DEALLOCATE(ITAB,ITABM1,IGEO,KXIG3D,IXIG3D,IPARTIG3D,KNOT,
     .           KNOTLOCPC,KNOTLOCEL,KNOD2ELIG3D,NOD2ELIG3D,TABCONPATCH)
C
      RETURN
C-------------------------------------
  999 CALL FREERR(1)
      RETURN
      END


