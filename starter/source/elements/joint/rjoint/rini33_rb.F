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
Chd|  RINI33_RB                     source/elements/joint/rjoint/rini33_rb.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        PROD_ATB                      source/elements/joint/rjoint/rini33.F
Chd|        FIND_RBY                      source/elements/joint/rjoint/rini33_rb.F
Chd|        GET_U_GEO                     source/user_interface/uaccess.F
Chd|        GET_U_SKEW                    source/user_interface/uaccess.F
Chd|        RESET_U_GEO                   source/user_interface/uaccess.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE RINI33_RB(NEL,NUVAR,IPROP,IXR,NPBY,LPBY,RBY,STIFR,
     1                     UVAR,ITAB,IGEO,IXR_KJ,GMASS)
      USE MESSAGE_MOD
C-------------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      INTEGER NEL,NUVAR,IPROP,IXR(NIXR,*),NPBY(NNPBY,NRBODY),LPBY(*),
     .        ITAB(*),IXR_KJ(5,*),IGEO(NPROPGI)
      my_real
     . RBY(NRBY,NRBODY),STIFR(*),UVAR(NUVAR,*),GMASS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,IEL,J,K,N,L,S,NN,NSL,IERROR,NODES,USR,
     .        IDSK(2),ISK,NSK,ISK2,JTYP,M(2),NOD(2),NODF(3),
     .        RESET_U_GEO,GET_U_SKEW,SRB(6),NO(3),IDSKRB(2),
     .        IDRB(2),ERR_FLG,N1,N2,N3,N4,ID_KJ,NUMEL_KJ,IELUSR,
     .        RB1,RB2,IPID,IDSK2   
C
      my_real
     .   MASS,INER,RM,RI,KNN,KR,L2,U(LSKEW),Q(LSKEW),GET_U_GEO,V(LSKEW),
     .   XSK1,XSK2,LEN
C
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
      INTEGER FIND_RBY
      EXTERNAL GET_U_GEO,RESET_U_GEO,GET_U_SKEW
      DATA NODES/2/
C=======================================================================

      ID=IGEO(1)
      CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1),LTITR)  
C      
      JTYP = NINT(GET_U_GEO(1,IPROP))
      ISK2 = NINT(GET_U_GEO(3,IPROP))
      KNN  = GET_U_GEO(10,IPROP)
      NSK = 2
      IF (ISK2==0) NSK = 1
      ERR_FLG = 0
      IPID=IXR(1,1+NFT)       
C------------------check sur les skews-----------------
      DO I=1,NSK
        IDSK(I) = NINT(GET_U_GEO(1+I,IPROP))
        ISK = GET_U_SKEW(IDSK(I),NO(1),NO(2),NO(3),U)
        IF (ISK==0) THEN
           CALL ANCMSG(MSGID=926,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=IDSK(I))
	   ERR_FLG = 1
	   GOTO 500
	ENDIF
        DO J=1,3
	  SRB(J+3*(I-1)) = FIND_RBY(NO(J),NPBY,LPBY)
        END DO
        IF((SRB(1+3*(I-1))==(SRB(2+3*(I-1)))).AND.
     .     (SRB(1+3*(I-1))==(SRB(3+3*(I-1))))) THEN
	  IDSKRB(I) = SRB(1+3*(I-1))
        ELSE
	  IDSKRB(I) = 0
	  IF (SRB(1+3*(I-1))+SRB(2+3*(I-1))+SRB(3+3*(I-1))==0) THEN
            CALL ANCMSG(MSGID=392,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=IDSK(I))
	  ELSE
            CALL ANCMSG(MSGID=919,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=IDSK(I))
	  ENDIF
        ENDIF
500     CONTINUE
      ENDDO
      IF (ERR_FLG==1) THEN
         GOTO 1000
      ENDIF
      IF (ISK2==0) THEN
	  IDSKRB(2) = IDSKRB(1)
      ENDIF
      
C------------------Boucle sur les neuds-----------------

      IF(NRBODY==0) THEN
        CALL ANCMSG(MSGID=390,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_2,
     .              I1=ID,
     .              C1=TITR)
      ELSE
        DO IEL=1,NEL
          L2 = 0.
          RM = 1.E30
          RI = 1.E30
	  IDRB(1)=0
	  IDRB(2)=0
C-->
          DO I=1,NODES
            M(I) = 0
            K = 0
            NOD(I)=IXR(1+I,NFT+IEL)
            DO N=1,NRBODY
              NSL=NPBY(2,N)
              IF (NPBY(1,N)==NOD(I)) THEN 
C-- Tag for error message - Can't be attached to main node-              
		IDRB(I)=-N
                UVAR(37+I,IEL)= N                
                EXIT
              ENDIF
C
              DO J=1,NSL
                NN = LPBY(J+K)
                IF(NN==NOD(I)) THEN
		  IDRB(I)=N
                  M(I) = NPBY(1,N)
                  MASS = RBY(14,N)
                  INER = (RBY(10,N)+RBY(11,N)+RBY(12,N))/3.0
C                 L2 = INER/MASS
                  UVAR(33+I,IEL)= MASS
                  UVAR(35+I,IEL)= INER
                  UVAR(37+I,IEL)= N
                  IF((I==2).AND.(ISK2==0)) THEN
c---              rigid body principal frame
                    DO II=1,9
                      U(II)= UVAR(3+II,IEL)
                    END DO
                    CALL PROD_ATB(RBY(1,N),U,Q)
                    UVAR(4,IEL) = Q(1)
                    UVAR(5,IEL) = Q(2)
                    UVAR(6,IEL) = Q(3)
                    UVAR(7,IEL) = Q(4)
                    UVAR(8,IEL) = Q(5)
                    UVAR(9,IEL) = Q(6)
                    UVAR(10,IEL)= Q(7)
                    UVAR(11,IEL)= Q(8)
                    UVAR(12,IEL)= Q(9)
                  ENDIF
C-->		  
                  GOTO 100
                ENDIF
              ENDDO
100           K = K+NSL
C
            ENDDO
C
C-->  Stockage d'une masse elementaire (masse hormonique) pour calcul d'energie specifique
            GMASS(IEL) = (UVAR(34,IEL)*UVAR(35,IEL))/MAX(EM20,UVAR(34,IEL)+UVAR(35,IEL))
C-->
            IF (IDRB(I)==0) THEN	    
	      USR = ITAB(NOD(I))
              CALL ANCMSG(MSGID=391,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=USR)
            ELSEIF (IDRB(I) < 0) THEN	    
	      USR = ITAB(NOD(I))
              CALL ANCMSG(MSGID=1768,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXR(NIXR,NFT+IEL),
     .                    I3=USR)
            ELSEIF ((IDRB(I)/=IDSKRB(1)).AND.
     .             (IDRB(I)/=IDSKRB(2))) THEN
	      USR = ITAB(NOD(I))
              CALL ANCMSG(MSGID=920,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=USR,
     .                    C2="OR",
     .                    I3=IDSK(1),
     .                    I4=IDSK(2))
	    ENDIF
          ENDDO

C---> Control consistence Noeud / Skew --------------------
         IF((IDRB(1)==IDSKRB(1)).AND.(IDRB(2)==IDSKRB(2)))GOTO 350
         IF((IDRB(1)==IDSKRB(2)).AND.(IDRB(2)==IDSKRB(1)))THEN
C-->        permutation skews
            CALL ANCMSG(MSGID=921,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=IDSK(2),
     .                  I3=IDSK(1))
            XSK1 = IDSK(2)
            XSK2 = IDSK(1)
	    IERROR = RESET_U_GEO(2,IPROP,XSK1)
	    IERROR = RESET_U_GEO(3,IPROP,XSK2)
	    GOTO 350
	  ENDIF
350       CONTINUE
C-->
        ENDDO
      ENDIF
      	   
C-------------------------------------------------------
 1000 CONTINUE
 
      RETURN

      RETURN
      END
C
Chd|====================================================================
Chd|  FIND_RBY                      source/elements/joint/rjoint/rini33_rb.F
Chd|-- called by -----------
Chd|        RINI33_RB                     source/elements/joint/rjoint/rini33_rb.F
Chd|-- calls ---------------
Chd|====================================================================
      INTEGER FUNCTION FIND_RBY(IDNOD,NPBY,LPBY)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IDNOD,NPBY(NNPBY,*),LPBY(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,K,NSL
C=======================================================================

      FIND_RBY = 0

      K = 0
      DO N=1,NRBYKIN
        NSL=NPBY(2,N)
	DO I=1,NSL
	  IF (NPBY(7,N)/=0) THEN
	    IF (IDNOD==LPBY(K+I)) THEN
	       FIND_RBY = N
	       EXIT
	    ENDIF
	  ENDIF
	END DO
        K=K+NSL
      ENDDO

C-----------------------
      RETURN
      END
