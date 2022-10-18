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
Chd|  NBADMESH                      source/model/remesh/nbadmesh.F
Chd|-- called by -----------
Chd|        CONTRL                        source/starter/contrl.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        CONSTIT                       source/elements/nodes/constit.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE NBADMESH(LSUBMODEL,NUMNUSR,UNITAB)
C----------------------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD   
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE UNITAB_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "submod_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMNUSR
C     REAL
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITAB,ITABM1,KNOD2SH,NOD2SH
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IXC,IXTG,TAG
      INTEGER IPART(4,NPART),
     .        N,IP,ID,I,J,NLEV,NMUL,STAT,INDEX_PART,NPART_ADM
      INTEGER USR2SYS,NUMNUSR1,IDS,NI,NJ,NK,NL,K,L,P,Q,QQ,
     .        NN,OFF_DEF(NSUBMOD),OFF_NOD(NSUBMOD),OFF_EL(NSUBMOD),
     .        UID,IP0,ID_IP
C     REAL
      CHARACTER MESS*40,TITR*nchartitle,KEY*ncharkey
      INTEGER , DIMENSION(:), ALLOCATABLE :: IPARTC, SHELL_ID
      INTEGER , DIMENSION(:), ALLOCATABLE :: IPARTTG, SH3N_ID
      REAL*8 , DIMENSION(:), ALLOCATABLE :: SH_ANGLE, SH_THK
      REAL*8 , DIMENSION(:), ALLOCATABLE :: SH3_ANGLE, SH3_THK
      INTEGER, DIMENSION(:), ALLOCATABLE :: SUBID_SHELL,UID_SHELL
      INTEGER, DIMENSION(:), ALLOCATABLE :: SUBID_SH3N,UID_SH3N
      INTEGER, DIMENSION(:), ALLOCATABLE :: SUBID_NODES

      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C-----------------------------------------------
      DATA MESS /'OPTIONS FOR ADAPTIVE MESHING DEFINITION '/
C-----------------------------------------------
      IPART=0
C------
      ALLOCATE(ITAB(NUMNUSR),ITABM1(2*NUMNUSR),
     .         IXC(NIXC,NUMELC0),IXTG(NIXTG,NUMELTG0),
     .        KNOD2SH(0:NUMNUSR),NOD2SH(4*NUMELC0+3*NUMELTG0),
     .        TAG(4,NUMELC0+NUMELTG0),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                          MSGTYPE=MSGERROR,
     .                         C1='ITAB')     
      TAG=0
C------
C--------------------------------------------------
C START BROWSING MODEL PARTS
C--------------------------------------------------
      CALL HM_OPTION_START('PART')
C--------------------------------------------------
C BROWSING MODEL PARTS 1->NPART
C--------------------------------------------------
      DO IP=1,NPART
        TITR = ''
C--------------------------------------------------
C EXTRACT DATAS OF /PART/... LINE
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       UNIT_ID = UID,
     .                       OPTION_TITR = TITR)
        IPART(1,IP)=ID
      ENDDO
C------

C--------------------------------------------------
C    READING /ADMESH/GLOBAL
C--------------------------------------------------

      CALL HM_OPTION_START('/ADMESH/GLOBAL')

      DO N =1,NADMESHG
          TITR = ''

          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_TITR = TITR,
     .                       KEYWORD2 = KEY)

C
          IS_AVAILABLE = .FALSE.
C
C--------* EXTRACT DATAS (INTEGER VALUES) *------
C
          CALL HM_GET_INTV('LEVEL',LEVELMAX,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('Iadmrule',IADMRULE,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('Istatcnd',ISTATCND,IS_AVAILABLE,LSUBMODEL)
C
C--------* EXTRACT DATAS (REAL VALUES) *------
C
          CALL HM_GET_FLOATV('Tdelay',DTADMESH,IS_AVAILABLE,LSUBMODEL,UNITAB)
C

      ENDDO
C------
      IF(NADMESHSTAT > 0) IADMSTAT = 1

      IF(IADMSTAT /= 0) ID_LIMIT_ADMESH=ID_LIMIT
C------

C--------------------------------------------------
C    READING /ADMESH/SET
C--------------------------------------------------

      CALL HM_OPTION_START('/ADMESH/SET')

      DO N =1,NADMESHSET
          TITR = ''

          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       OPTION_TITR = TITR,
     .                       KEYWORD2 = KEY)
C
          IS_AVAILABLE = .FALSE.
C
C--------* EXTRACT DATAS (INTEGER VALUES) *------
C
          CALL HM_GET_INTV('NIP',NPART_ADM,IS_AVAILABLE,LSUBMODEL)
C
          DO I=1,NPART_ADM

             CALL HM_GET_INT_ARRAY_INDEX('PartIds1',ID_IP,I,IS_AVAILABLE,LSUBMODEL)

             IF(ID_IP/=0)THEN
                IP=0
                DO J=1,NPART
                  IF(IPART(1,J)==ID_IP)THEN
                    IP=J
                    GOTO 100
                  END IF
                END DO
 100            CONTINUE
                IF(IP/=0)THEN
                  IPART(4,IP)=LEVELMAX
                ELSE
                  CALL ANCMSG(MSGID=646,
     .                        MSGTYPE=MSGERROR,
     .                        ANMODE=ANINFO,
     .                        I1=ID,
     .                        C1=TITR,
     .                        I2=ID_IP)
                END IF
             END IF

          ENDDO

      ENDDO

C--------------------------------------------------
C      HM OFFSETS IDs
C--------------------------------------------------
      DO I=1,NSUBMOD
        OFF_DEF(I) = LSUBMODEL(I)%OFF_DEF
        OFF_NOD(I) = LSUBMODEL(I)%OFF_NOD
        OFF_EL(I) = LSUBMODEL(I)%OFF_EL
      ENDDO
C--------------------------------------
C     nb shells and 3-node shells + nb nodes estimation (NUMNUSR < ...)
C---------------  
      ALLOCATE (IPARTC(NUMELC)) 
      ALLOCATE (SH_ANGLE(NUMELC)) 
      ALLOCATE (SH_THK(NUMELC))
C--------------------------------------------------
C      ALLOCS & INITS
C--------------------------------------------------
      ALLOCATE (SUBID_SHELL(NUMELC),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='SUBID_SHELL') 
      ALLOCATE (UID_SHELL(NUMELC),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='UID_SHELL') 
      SUBID_SHELL(1:NUMELC) = 0
      UID_SHELL(1:NUMELC) = 0
      INDEX_PART = -1
      UID = -1 
C--------------------------------------------------
C      READING SHELLS INPUTS IN HM STRUCTURE
C-------------------------------------------------- 
      CALL CPP_SHELL_READ(IXC,NIXC,IPARTC,SH_ANGLE,SH_THK,SUBID_SHELL,UID_SHELL)       
C--------------------------------------------------
C      FILL OTHER STRUCTURES + CHECKS
C-------------------------------------------------- 
      IP = 0 
      IP0 = 0 
      DO I=1,NUMELC
C--------------------------------------------------
C      SUBMODEL OFFSET
C-------------------------------------------------
        IF(SUBID_SHELL(I) /= 0)THEN
          IF(IPARTC(I) /= 0) IPARTC(I) = IPARTC(I) + OFF_DEF(SUBID_SHELL(I))
          IF(IXC(NIXC,I) /= 0) IXC(NIXC,I) = IXC(NIXC,I) + OFF_EL(SUBID_SHELL(I))
          DO J=2,5
            IF(IXC(J,I) /= 0) IXC(J,I) = IXC(J,I) + OFF_NOD(SUBID_SHELL(I))
          ENDDO
        ENDIF
C--------------------------------------------------
C      INTERNAL PART ID
C--------------------------------------------------
        IF( IPARTC(I) /= IP0)THEN
          DO J=1,NPART
            IF(IPARTC(I) == IPART(1,J))THEN
              IP = J
              IP0 = IPART(1,J)
            ENDIF
          ENDDO
        ENDIF 

        IF(IP==0)THEN
          CALL ANCMSG(MSGID=735,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID)
        ELSE
	  IXC(1,I)=IP
          IPART(2,IP)=IPART(2,IP)+1 
        END IF
      ENDDO
c
      IF(ALLOCATED(SUBID_SHELL)) DEALLOCATE(SUBID_SHELL)
      IF(ALLOCATED(UID_SHELL)) DEALLOCATE(UID_SHELL)
C------ 
      ALLOCATE (IPARTTG(NUMELTG)) 
      ALLOCATE (SH3_ANGLE(NUMELTG))  
      ALLOCATE (SH3_THK(NUMELTG)) 
C--------------------------------------------------
C      ALLOCS & INITS
C--------------------------------------------------
      ALLOCATE (SUBID_SH3N(NUMELTG),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='SUBID_SH3N') 
      ALLOCATE (UID_SH3N(NUMELTG),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='UID_SH3N') 
      SUBID_SH3N(1:NUMELTG) = 0
      UID_SH3N(1:NUMELTG) = 0
      INDEX_PART = 1
      UID = -1 
C--------------------------------------------------
C      READING SH3N INPUTS IN HM STRUCTURE
C-------------------------------------------------- 
      CALL CPP_SH3N_READ(IXTG,NIXTG,IPARTTG,SH3_ANGLE,SH3_THK,SUBID_SH3N,UID_SH3N)       
C--------------------------------------------------
C      FILL OTHER STRUCTURES + CHECKS
C-------------------------------------------------- 
      IP = 0 
      IP0 = 0  
      DO I=1,NUMELTG
C--------------------------------------------------
C      SUBMODEL OFFSET
C--------------------------------------------------
        IF(SUBID_SH3N(I) /= 0)THEN
          IF(IPARTTG(I) /= 0) IPARTTG(I) = IPARTTG(I) + OFF_DEF(SUBID_SH3N(I))
          IF(IXTG(NIXTG,I) /= 0) IXTG(NIXTG,I) = IXTG(NIXTG,I) + OFF_EL(SUBID_SH3N(I))
          DO J=2,4
            IF(IXTG(J,I) /= 0) IXTG(J,I) = IXTG(J,I) + OFF_NOD(SUBID_SH3N(I))
          ENDDO
        ENDIF
C--------------------------------------------------
C      INTERNAL PART ID
C--------------------------------------------------
        IF( IPARTTG(I) /= IP0)THEN
          DO J=1,NPART
            IF(IPARTTG(I) == IPART(1,J))THEN
              IP = J
              IP0 = IPART(1,J)
            ENDIF
          ENDDO
        ENDIF 
        IF(IP==0)THEN
          CALL ANCMSG(MSGID=735,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=ID)
        ELSE
	  IXTG(1,I)=IP
          IPART(3,IP)=IPART(3,IP)+1 
        END IF
      ENDDO
c
      IF(ALLOCATED(SUBID_SH3N)) DEALLOCATE(SUBID_SH3N)
      IF(ALLOCATED(UID_SH3N)) DEALLOCATE(UID_SH3N)
C--------------------------------------
C     nb shells and 3-node shells + nb nodes exact calculation
C--------------------------------------
      IF(IADMSTAT /= 0)RETURN
C--------------------------------------------------
C      ALLOCS & INITS
C--------------------------------------------------
      ALLOCATE (SUBID_NODES(NUMNUSR),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='SUBID_NODES') 
      SUBID_NODES(1:NUMNUSR) = 0
C--------------------------------------------------
C      READING NODES IDs IN HM STRUCTURE
C-------------------------------------------------- 
      CALL CPP_NODE_COUNT(NUMNUSR1)
      CALL CPP_NODE_ID_READ(ITAB,SUBID_NODES) 
C--------------------------------------------------
C      CHECKS NODES & CNODES IDs
C--------------------------------------------------  
      DO I=1,NUMNUSR
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
C------
C     CONSTITUTION DU TABLEAU INVERSE DES NOEUDS
C------
C     NUMNUSR=NUMNUSR1+NUMCNOD !
      CALL CONSTIT(ITAB,ITABM1,NUMNUSR)
C------
C     4-node shells
C------			  
      DO I=1,NUMELC
	  IF (IXC(NIXC,I)>ID_LIMIT_ADMESH) THEN	     
            CALL ANCMSG(MSGID=1069,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .            I1=IXC(NIXC,I),C1=LINE,C2='/SHELL') 
	  ENDIF
	  DO J=2,5						   
	    IXC(J,I)=USR2SYS(IXC(J,I),ITABM1,MESS,ID)  
	  ENDDO
      ENDDO
      IF(ALLOCATED(IPARTC)) DEALLOCATE(IPARTC)
      IF(ALLOCATED(SH_ANGLE)) DEALLOCATE(SH_ANGLE) 
      IF(ALLOCATED(SH_THK)) DEALLOCATE (SH_THK)
C------			  
      DO I=1,NUMELTG
	  IF (IXTG(NIXTG,I)>ID_LIMIT_ADMESH) THEN	     
            CALL ANCMSG(MSGID=1069,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .            I1=IXTG(NIXTG,I),C1=LINE,C2='/SH3N')
	  ENDIF
	  DO J=2,4						   
	    IXTG(J,I)=USR2SYS(IXTG(J,I),ITABM1,MESS,ID) 
	  ENDDO
      ENDDO
      IF(ALLOCATED(IPARTTG)) DEALLOCATE(IPARTTG)
      IF(ALLOCATED(SH3_ANGLE)) DEALLOCATE (SH3_ANGLE)  
      IF(ALLOCATED(SH3_THK)) DEALLOCATE (SH3_THK) 
C------
C     inverse connectivity at level 0
C------
      KNOD2SH=0
      DO N=1,NUMELC0
	DO I=1,4
	  NI=IXC(I+1,N)
	  KNOD2SH(NI)=KNOD2SH(NI)+1
	END DO
      END DO

      DO N=1,NUMELTG0
	DO I=1,3
	  NI=IXTG(I+1,N)
	  KNOD2SH(NI)=KNOD2SH(NI)+1
	END DO
      END DO

      DO N=2,NUMNUSR
	KNOD2SH(N)=KNOD2SH(N)+KNOD2SH(N-1)
      END DO

      DO N=1,NUMELC0
	DO I=1,4
	  NI=IXC(I+1,N)-1
	  KNOD2SH(NI)=KNOD2SH(NI)+1
	  NOD2SH(KNOD2SH(NI))=N
	END DO
      END DO

      DO N=1,NUMELTG0
	DO I=1,3
	  NI=IXTG(I+1,N)-1
	  KNOD2SH(NI)=KNOD2SH(NI)+1
	  NOD2SH(KNOD2SH(NI))=NUMELC0+N
	END DO
      END DO

      DO N=NUMNUSR,1,-1
	KNOD2SH(N)=KNOD2SH(N-1)
      END DO
      KNOD2SH(0)=0
C------
C
C------
      NUMELC=0
      DO N=1,NUMELC0
	IP  =IXC(1,N)
	NLEV=IPART(4,IP)
	IF(NLEV/=0) THEN
         NUMNOD=NUMNOD+(2**NLEV-1)*(2**NLEV-1)
         DO I=1,4
           IF(TAG(I,N)<NLEV)THEN
             NUMNOD=NUMNOD+(2**NLEV-1)-(2**(TAG(I,N))-1)
             TAG(I,N)=NLEV

             NI=IXC(I+1,N)
             NJ=IXC(MOD(I,4)+2,N)
             DO K=KNOD2SH(NI-1)+1,KNOD2SH(NI)
               P=NOD2SH(K)
               IF(P/=N)THEN
         	DO L=KNOD2SH(NJ-1)+1,KNOD2SH(NJ)
         	  Q=NOD2SH(L)
         	  IF(Q==P)THEN
         	     IF(Q<=NUMELC0)THEN
         	       DO J=1,4
                         NK=IXC(J+1,Q)
                         NL=IXC(MOD(J,4)+2,Q)
                         IF((NK==NI.AND.NL==NJ).OR.
     .                      (NL==NI.AND.NK==NJ))THEN
         		   TAG(J,Q)=NLEV
         		 END IF
         	       END DO
         	     ELSE
         	       QQ=Q-NUMELC0
         	       DO J=1,3
                        NK=IXTG(J+1,QQ)
                        NL=IXTG(MOD(J,3)+2,QQ)
         		IF((NK==NI.AND.NL==NJ).OR.
     .  		   (NL==NI.AND.NK==NJ))THEN
         		   TAG(J,Q)=NLEV
         		 END IF
         	       END DO
         	     END IF
         	  END IF
         	END DO
               END IF
             END DO
           END IF
         END DO
	END IF
	NUMELC =NUMELC +(4**(NLEV+1)-1)/3
      END DO

C
      NUMELTG=0
      DO N=1,NUMELTG0
	IP  =IXTG(1,N)
	NLEV=IPART(4,IP)
	IF(NLEV/=0) THEN
         NUMNOD =NUMNOD+(2**(NLEV-1)+1)*(2**NLEV+1)-3*(2**NLEV)
	 DO I=1,3
	   IF(TAG(I,N+NUMELC0)<NLEV)THEN
	     NUMNOD=NUMNOD+(2**NLEV-1)-(2**(TAG(I,N+NUMELC0))-1)
	     TAG(I,N+NUMELC0)=NLEV

	     NI=IXTG(I+1,N)
	     NJ=IXTG(MOD(I,3)+2,N)
	     DO K=KNOD2SH(NI-1)+1,KNOD2SH(NI)
	       P=NOD2SH(K)
	       IF(P/=N+NUMELC0)THEN
	 	 DO L=KNOD2SH(NJ-1)+1,KNOD2SH(NJ)
	 	   Q=NOD2SH(L)
	 	   IF(Q==P)THEN
	 	     IF(Q<=NUMELC0)THEN
	 	       DO J=1,4
                         NK=IXC(J+1,Q)
                         NL=IXC(MOD(J,4)+2,Q)
                         IF((NK==NI.AND.NL==NJ).OR.
     .                      (NL==NI.AND.NK==NJ))THEN
	 		   TAG(J,Q)=NLEV
	 		 END IF
	 	       END DO
	 	     ELSE
	 	       QQ=Q-NUMELC0
	 	       DO J=1,3
                        NK=IXTG(J+1,QQ)
                        NL=IXTG(MOD(J,3)+2,QQ)
         		IF((NK==NI.AND.NL==NJ).OR.
     .  		   (NL==NI.AND.NK==NJ))THEN
	 		   TAG(J,Q)=NLEV
	 		 END IF
	 	       END DO
	 	     END IF
	 	   END IF
	 	 END DO
	       END IF
	     END DO
	   END IF
	 END DO
	END IF
	NUMELTG =NUMELTG +(4**(NLEV+1)-1)/3
      END DO
C-------------------------------------
      DEALLOCATE(ITAB,ITABM1,IXC,IXTG,KNOD2SH,NOD2SH,TAG)
      RETURN
C-------------------------------------
      END
