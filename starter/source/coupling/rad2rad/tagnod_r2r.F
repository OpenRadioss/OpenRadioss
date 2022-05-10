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
Chd|  TAGNOD_R2R                    source/coupling/rad2rad/tagnod_r2r.F
Chd|-- called by -----------
Chd|        R2R_GROUP                     source/coupling/rad2rad/r2r_group.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE TAGNOD_R2R(IX,NIX,NIX1,NIX2,NUMEL,IPARTE,
     1                  TAGBUF,NPART,FLAG,IDOM)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIX,IX(NIX,*),NIX1,NIX2,NUMEL,IPARTE(*),
     1      TAGBUF(*),NPART,FLAG,IDOM,TAG_EL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,L,NUM_KJ,RES
C=======================================================================

c-----si FLAG = -1 : on detag les noeuds du subdomaine courant------

      IF (FLAG==-1) THEN

        DO J=1,NUMEL
          IF (TAGBUF(IPARTE(J)).EQ.0) THEN
            DO L=NIX1,NIX2
              IF (TAGBUF(IX(L,J)+NPART).EQ.1) THEN
	        TAGBUF(IX(L,J)+NPART)=0
              ENDIF
            ENDDO
	  ENDIF
        ENDDO

c-----si FLAG = 0 : on tag tous les noeuds du domaine------

      ELSEIF (FLAG==0) THEN

        DO J=1,NUMEL
          IF (TAGBUF(IPARTE(J)).EQ.1) THEN
            DO L=NIX1,NIX2
              IF (TAGBUF(IX(L,J)+NPART).LT.2) THEN
	        TAGBUF(IX(L,J)+NPART)=1
              ENDIF
            ENDDO
	  ENDIF
        ENDDO

      ELSEIF (FLAG==1) THEN

c-----si FLAG = 1 : un noeud deja tage sur une part non tagee est un noeud d'intersection------

        DO J=1,NUMEL
          IF (TAGBUF(IPARTE(J)).EQ.0) THEN
            DO L=NIX1,NIX2
              IF (TAGBUF(IX(L,J)+NPART) == 1) THEN
	        TAGBUF(IX(L,J)+NPART)=1+IDOM
	      ELSEIF (TAGBUF(IX(L,J)+NPART)>1) THEN
	        IF (TAGBUF(IX(L,J)+NPART).NE.(1+IDOM)) THEN
C-------------Erreur noeuds communs entre subdomaines----------------
               CALL ANCMSG(MSGID=838,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO,
     .                     I2=TAGBUF(IX(L,J)+NPART)-1,
     .                     I1=IDOM,
     .                     C1="NODES")
		ENDIF
	      ELSE
C-------------Tag des noeuds externes avec l'id de leur domaine------
	        TAGBUF(IX(L,J)+NPART)=-IDOM
              ENDIF
            ENDDO
          ENDIF
        ENDDO

      ELSEIF (FLAG==2) THEN

C-----si FLAG = 2 : tag des noeuds des elements pretages pour les contacts---------

        DO J=1,NUMEL
C----------> on passe le TAG_EL dans IPARTE-------------
          TAG_EL=IPARTE(J+NPART)
          IF (TAG_EL.GT.0) THEN
            DO L=NIX1,NIX2
              IF (TAGBUF(IX(L,J)+NPART).LE.2) THEN
	        TAGBUF(IX(L,J)+NPART)=2*IPARTE(J+NPART)
              ENDIF
            ENDDO
          ELSEIF (TAG_EL.EQ.-1) THEN
            DO L=NIX1,NIX2
              IF (TAGBUF(IX(L,J)+NPART).LT.0) THEN
	        TAGBUF(IX(L,J)+NPART)=0
              ENDIF
            ENDDO
	  ENDIF
        ENDDO

      ELSEIF (FLAG==3) THEN

C-----si FLAG = 3 : tag des noeuds additionnels des springs---------
      
        DO J=1,NUMEL
          IF (TAGBUF(IPARTE(J)).EQ.1) THEN
            DO L=NIX1,NIX2    
              IF (IX(L,J).GT.0) THEN	    
                IF (TAGBUF(IX(L,J)+NPART).LT.1) THEN 
	          TAGBUF(IX(L,J)+NPART)=1
                ENDIF
	      ENDIF	    	   
            ENDDO
	  ENDIF
        ENDDO
 
         DO J=1,NUMEL
          IF (TAGBUF(IPARTE(J)).EQ.0) THEN
            DO L=NIX1,NIX2
              IF (IX(L,J).GT.0) THEN	          	    	
                IF (TAGBUF(IX(L,J)+NPART).EQ.0) THEN
	          TAGBUF(IX(L,J)+NPART)=-1			        				
                ENDIF
	      ENDIF	    	   
            ENDDO
	  ENDIF
        ENDDO

      ELSEIF ((FLAG==4).AND.(NUMEL.GT.0)) THEN
      
C-----si FLAG = 4 : tag des noeuds additionnels des kjoints---------
      
        NUM_KJ = IX(1,NUMEL+1)
        
        DO J=1,NUM_KJ
          RES = IX(5,J)                   
          IF (TAGBUF(IPARTE(RES)).EQ.1) THEN
            DO L=NIX1,NIX2    
              IF (IX(L,J).GT.0) THEN	    
                IF (TAGBUF(IX(L,J)+NPART).LT.1) THEN 
	          TAGBUF(IX(L,J)+NPART)=1
                ENDIF
	      ENDIF	    	   
            ENDDO
	  ENDIF
        ENDDO
 
        DO J=1,NUM_KJ
          RES = IX(5,J)         
          IF (TAGBUF(IPARTE(RES)).EQ.0) THEN
            DO L=NIX1,NIX2
              IF (IX(L,J).GT.0) THEN	          	    	
                IF (TAGBUF(IX(L,J)+NPART).EQ.0) THEN
	          TAGBUF(IX(L,J)+NPART)= -1			        				
                ENDIF
	      ENDIF	    	   
            ENDDO
	  ENDIF
        ENDDO
	
      ENDIF
C-----------
      RETURN
      END

Chd|====================================================================
Chd|  TAGNOD_R2R_S                  source/coupling/rad2rad/tagnod_r2r.F
Chd|-- called by -----------
Chd|        R2R_GROUP                     source/coupling/rad2rad/r2r_group.F
Chd|-- calls ---------------
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        RESTMOD                       share/modules1/restart_mod.F  
Chd|====================================================================
      SUBROUTINE TAGNOD_R2R_S(TAGBUF)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE RESTMOD
      USE R2R_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAGBUF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,OFF
C=======================================================================

c-----on tag les noeuds des skews ------

        DO J=1,NUMSKW
	  DO K=1,3
	     IF (ISKWN(LISKN*J+K).GT.0) THEN
             IF (TAGBUF(ISKWN(LISKN*J+K)+NPART).LE.2) THEN
	        TAGBUF(ISKWN(LISKN*J+K)+NPART) = 2
             ENDIF
	     ENDIF
	  END DO
        ENDDO

c-----on tag les noeuds des frames ------

        OFF = LISKN*(NUMSKW+1)
        DO J=1,NUMFRAM
	  DO K=1,3
	     IF (ISKWN(OFF+LISKN*J+K).GT.0) THEN
             IF (TAGBUF(ISKWN(OFF+LISKN*J+K)+NPART).LE.2)THEN
	        TAGBUF(ISKWN(OFF+LISKN*J+K)+NPART) = 2
             ENDIF
	     ENDIF
	  END DO
        ENDDO

C-----------
      RETURN
      END

Chd|====================================================================
Chd|  TAGNODS_R2R                   source/coupling/rad2rad/tagnod_r2r.F
Chd|-- called by -----------
Chd|        R2R_GROUP                     source/coupling/rad2rad/r2r_group.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE TAGNODS_R2R(IXS,IXS10,IXS20,IXS16,IPARTS,
     .                   TAGBUF,FLAG,IDOM)
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
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IPARTS(*),IXS10(6,*),IXS20(12,*),IXS16(8,*),
     1      TAGBUF(*),FLAG,IDOM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,L,NP
C=======================================================================

      NP = NPART

C-----------------------------------------------------------------------------------------------
c-----si FLAG = -1 : on detag les noeuds du subdomaine courant ---------------------------------
C-----------------------------------------------------------------------------------------------

      IF (FLAG==-1) THEN

      DO J=1,NUMELS8
        IF (TAGBUF(IPARTS(J)) == 0)THEN
          DO L=2,9
	    IF (TAGBUF(IXS(L,J)+NP)<2) TAGBUF(IXS(L,J)+NP)=0
          ENDDO
        ENDIF
      ENDDO
C-----------
      DO I=1,NUMELS10
        J = I + NUMELS8
        IF (TAGBUF(IPARTS(J)) == 0)THEN
          DO L=2,9
	    IF (TAGBUF(IXS(L,J)+NP)<2) TAGBUF(IXS(L,J)+NP)=0
          ENDDO
          DO L=1,6
            IF (IXS10(L,I) /= 0) THEN
	    IF (TAGBUF(IXS10(L,I)+NP)<2) TAGBUF(IXS10(L,I)+NP)=0
	    ENDIF
          ENDDO
        ENDIF
      ENDDO
C-----------
      DO I=1,NUMELS20
        J = I + NUMELS8 + NUMELS10
        IF (TAGBUF(IPARTS(J)) == 0)THEN
          DO L=2,9
	    IF (TAGBUF(IXS(L,J)+NP)<2) TAGBUF(IXS(L,J)+NP)=0
          ENDDO
          DO L=1,12
            IF (IXS20(L,I) /= 0) THEN
	    IF (TAGBUF(IXS20(L,I)+NP)<2) TAGBUF(IXS20(L,I)+NP)=0
	    ENDIF
          ENDDO
        ENDIF
      ENDDO
C-----------
      DO I=1,NUMELS16
        J = I + NUMELS8 + NUMELS10 + NUMELS20
        IF (TAGBUF(IPARTS(J)) == 0) THEN
          DO L=2,9
	    IF (TAGBUF(IXS(L,J)+NP)<2) TAGBUF(IXS(L,J)+NP)=0
          ENDDO
          DO L=1,8
            IF (IXS16(L,I) /= 0) THEN
	    IF (TAGBUF(IXS16(L,I)+NP)<2) TAGBUF(IXS16(L,I)+NP)=0
	    ENDIF
          ENDDO
	ENDIF
      ENDDO

C-----------------------------------------------------------------------------------------------
c-----si FLAG = 0 : on tag tous les noeuds -----------------------------------------------------
C-----------------------------------------------------------------------------------------------

      ELSEIF (FLAG==0) THEN

      DO J=1,NUMELS8
        IF (TAGBUF(IPARTS(J)) == 1)THEN
          DO L=2,9
	    IF (TAGBUF(IXS(L,J)+NP)<2) TAGBUF(IXS(L,J)+NP)=1
          ENDDO
        ENDIF
      ENDDO
C-----------
      DO I=1,NUMELS10
        J = I + NUMELS8
        IF (TAGBUF(IPARTS(J)) == 1)THEN
          DO L=2,9
	    IF (TAGBUF(IXS(L,J)+NP)<2) TAGBUF(IXS(L,J)+NP)=1
          ENDDO
          DO L=1,6
            IF (IXS10(L,I) /= 0) THEN
	    IF (TAGBUF(IXS10(L,I)+NP)<2) TAGBUF(IXS10(L,I)+NP)=1
	    ENDIF
          ENDDO
        ENDIF
      ENDDO
C-----------
      DO I=1,NUMELS20
        J = I + NUMELS8 + NUMELS10
        IF (TAGBUF(IPARTS(J)) == 1)THEN
          DO L=2,9
	    IF (TAGBUF(IXS(L,J)+NP)<2) TAGBUF(IXS(L,J)+NP)=1
          ENDDO
          DO L=1,12
            IF (IXS20(L,I) /= 0) THEN
	    IF (TAGBUF(IXS20(L,I)+NP)<2) TAGBUF(IXS20(L,I)+NP)=1
	    ENDIF
          ENDDO
        ENDIF
      ENDDO
C-----------
      DO I=1,NUMELS16
        J = I + NUMELS8 + NUMELS10 + NUMELS20
        IF (TAGBUF(IPARTS(J)) == 1) THEN
          DO L=2,9
	    IF (TAGBUF(IXS(L,J)+NP)<2) TAGBUF(IXS(L,J)+NP)=1
          ENDDO
          DO L=1,8
            IF (IXS16(L,I) /= 0) THEN
	    IF (TAGBUF(IXS16(L,I)+NP)<2) TAGBUF(IXS16(L,I)+NP)=1
	    ENDIF
          ENDDO
	ENDIF
      ENDDO

C-----------------------------------------------------------------------------------------------
C-----si FLAG = 1 : un noeud deja tage sur une parte non tagee est un noeud d'intersection------
C-----------------------------------------------------------------------------------------------

      ELSEIF (FLAG==1) THEN

      DO J=1,NUMELS8
        IF (TAGBUF(IPARTS(J)) == 0)THEN
          DO L=2,9
	    IF (TAGBUF(IXS(L,J)+NP)==1) TAGBUF(IXS(L,J)+NP)=1+IDOM
	    IF (TAGBUF(IXS(L,J)+NP)>1) THEN
	      IF (TAGBUF(IXS(L,J)+NP).NE.(1+IDOM)) THEN
C-------------Erreur noeuds communs entre subdomaines----------------
               CALL ANCMSG(MSGID=838,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO,
     .                     I2=TAGBUF(IXS(L,J)+NP)-1,
     .                     I1=IDOM,
     .                     C1="NODES")
	      ENDIF
	    ENDIF
	    IF (TAGBUF(IXS(L,J)+NP)<1) TAGBUF(IXS(L,J)+NP)=-IDOM
          ENDDO
        ENDIF
      ENDDO

C--------------------------------------------------------------------------------------
C--------------------------------------------------------------------------------------
      DO I=1,NUMELS10
        J = I + NUMELS8
        IF (TAGBUF(IPARTS(J)) == 0)THEN
          DO L=2,9
	    IF (TAGBUF(IXS(L,J)+NP)==1) TAGBUF(IXS(L,J)+NP)=1+IDOM
	    IF (TAGBUF(IXS(L,J)+NP)>1) THEN
	      IF (TAGBUF(IXS(L,J)+NP).NE.(1+IDOM)) THEN
C-------------Erreur noeuds communs entre subdomaines----------------
               CALL ANCMSG(MSGID=838,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO,
     .                     I2=TAGBUF(IXS(L,J)+NP)-1,
     .                     I1=IDOM,
     .                     C1="NODES")
	      ENDIF
	    ENDIF
	    IF (TAGBUF(IXS(L,J)+NP)<1) TAGBUF(IXS(L,J)+NP)=-IDOM
          ENDDO
          DO L=1,6
            IF (IXS10(L,I) /= 0) THEN
	    IF (TAGBUF(IXS10(L,I)+NP)==1) TAGBUF(IXS10(L,I)+NP)=1+IDOM
	    IF (TAGBUF(IXS10(L,I)+NP)>1) THEN
	      IF (TAGBUF(IXS10(L,I)+NP).NE.(1+IDOM)) THEN
C-------------Erreur noeuds communs entre subdomaines----------------
               CALL ANCMSG(MSGID=838,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO,
     .                     I2=TAGBUF(IXS10(L,I)+NP)-1,
     .                     I1=IDOM,
     .                     C1="NODES")
	      ENDIF
	    ENDIF
	    IF (TAGBUF(IXS10(L,I)+NP)<1) TAGBUF(IXS10(L,I)+NP)=-IDOM
	    ENDIF
          ENDDO
        ENDIF
      ENDDO

C--------------------------------------------------------------------------------------
C--------------------------------------------------------------------------------------
      DO I=1,NUMELS20
        J = I + NUMELS8 + NUMELS10
        IF (TAGBUF(IPARTS(J)) == 0)THEN
          DO L=2,9
	    IF (TAGBUF(IXS(L,J)+NP)==1) TAGBUF(IXS(L,J)+NP)=1+IDOM
	    IF (TAGBUF(IXS(L,J)+NP)>1) THEN
	      IF (TAGBUF(IXS(L,J)+NP).NE.(1+IDOM)) THEN
C-------------Erreur noeuds communs entre subdomaines----------------
               CALL ANCMSG(MSGID=838,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO,
     .                     I2=TAGBUF(IXS(L,J)+NP)-1,
     .                     I1=IDOM,
     .                     C1="NODES")
	      ENDIF
	    ENDIF
	    IF (TAGBUF(IXS(L,J)+NP)<1) TAGBUF(IXS(L,J)+NP)=-IDOM
          ENDDO
          DO L=1,12
            IF (IXS20(L,I) /= 0) THEN
	    IF (TAGBUF(IXS20(L,I)+NP)==1) TAGBUF(IXS20(L,I)+NP)=1+IDOM
	    IF (TAGBUF(IXS20(L,I)+NP)>1) THEN
	      IF (TAGBUF(IXS20(L,I)+NP).NE.(1+IDOM)) THEN
C-------------Erreur noeuds communs entre subdomaines----------------
               CALL ANCMSG(MSGID=838,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO,
     .                     I2=TAGBUF(IXS20(L,I)+NP)-1,
     .                     I1=IDOM,
     .                     C1="NODES")
	      ENDIF
	    ENDIF
	    IF (TAGBUF(IXS20(L,I)+NP)<1) TAGBUF(IXS20(L,I)+NP)=-IDOM
	    ENDIF
          ENDDO
        ENDIF
      ENDDO

C--------------------------------------------------------------------------------------
C--------------------------------------------------------------------------------------
      DO I=1,NUMELS16
        J = I + NUMELS8 + NUMELS10 + NUMELS20
        IF (TAGBUF(IPARTS(J)) == 0) THEN
          DO L=2,9
	    IF (TAGBUF(IXS(L,J)+NP)==1) TAGBUF(IXS(L,J)+NP)=1+IDOM
	    IF (TAGBUF(IXS(L,J)+NP)>1) THEN
	      IF (TAGBUF(IXS(L,J)+NP).NE.(1+IDOM)) THEN
C-------------Erreur noeuds communs entre subdomaines----------------
               CALL ANCMSG(MSGID=838,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO,
     .                     I2=TAGBUF(IXS(L,J)+NP)-1,
     .                     I1=IDOM,
     .                     C1="NODES")
	      ENDIF
	    ENDIF
	    IF (TAGBUF(IXS(L,J)+NP)<1) TAGBUF(IXS(L,J)+NP)=-IDOM
          ENDDO
          DO L=1,8
           IF (IXS16(L,I) /= 0) THEN
	    IF (TAGBUF(IXS16(L,I)+NP)==1) TAGBUF(IXS16(L,I)+NP)=1+IDOM
	    IF (TAGBUF(IXS16(L,I)+NP)>1) THEN
	      IF (TAGBUF(IXS16(L,I)+NP).NE.(1+IDOM)) THEN
C-------------Erreur noeuds communs entre subdomaines----------------
               CALL ANCMSG(MSGID=838,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO,
     .                     I2=TAGBUF(IXS16(L,I)+NP)-1,
     .                     I1=IDOM,
     .                     C1="NODES")
	      ENDIF
	    ENDIF
	    IF (TAGBUF(IXS16(L,I)+NP)<1) TAGBUF(IXS16(L,I)+NP)=-IDOM
	   ENDIF
          ENDDO
	ENDIF
      ENDDO


C-----------------------------------------------------------------------------------------------
c-----si FLAG = 2 : tag des noeuds des elements pretages ( traitement interfaces TYPE2) --------
C-----------------------------------------------------------------------------------------------

      ELSEIF (FLAG==2) THEN

      DO J=1,NUMELS8
        IF (IPARTS(J+NP).NE.0)THEN
          DO L=2,9
	    IF (TAGBUF(IXS(L,J)+NP)<3) TAGBUF(IXS(L,J)+NP)=2*IPARTS(J+NP)
          ENDDO
        ENDIF
      ENDDO
C-----------
      DO I=1,NUMELS10
        J = I + NUMELS8
        IF (IPARTS(J+NP).NE.0)THEN
          DO L=2,9
	    IF (TAGBUF(IXS(L,J)+NP)<3) TAGBUF(IXS(L,J)+NP)=2*IPARTS(J+NP)
          ENDDO
          DO L=1,6
            IF (IXS10(L,I) /= 0) THEN
	IF (TAGBUF(IXS10(L,I)+NP)<3) TAGBUF(IXS10(L,I)+NP)=2*IPARTS(J+NP)
	    ENDIF
          ENDDO
        ENDIF
      ENDDO
C-----------
      DO I=1,NUMELS20
        J = I + NUMELS8 + NUMELS10
        IF (IPARTS(J+NP).NE.0)THEN
          DO L=2,9
	    IF (TAGBUF(IXS(L,J)+NP)<3) TAGBUF(IXS(L,J)+NP)=2*IPARTS(J+NP)
          ENDDO
          DO L=1,12
            IF (IXS20(L,I) /= 0) THEN
	IF (TAGBUF(IXS20(L,I)+NP)<3) TAGBUF(IXS20(L,I)+NP)=2*IPARTS(J+NP)
	    ENDIF
          ENDDO
        ENDIF
      ENDDO
C-----------
      DO I=1,NUMELS16
        J = I + NUMELS8 + NUMELS10 + NUMELS20
        IF (IPARTS(J+NP).NE.0)THEN
          DO L=2,9
	    IF (TAGBUF(IXS(L,J)+NP)<3) TAGBUF(IXS(L,J)+NP)=2*IPARTS(J+NP)
          ENDDO
          DO L=1,8
            IF (IXS16(L,I) /= 0) THEN
	IF (TAGBUF(IXS16(L,I)+NP)<3) TAGBUF(IXS16(L,I)+NP)=2*IPARTS(J+NP)
	    ENDIF
          ENDDO
	ENDIF
      ENDDO

      ENDIF

      RETURN

      END
