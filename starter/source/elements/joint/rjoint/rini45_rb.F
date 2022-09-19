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
Chd|  RINI45_RB                     source/elements/joint/rjoint/rini45_rb.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        GET_U_GEO                     source/user_interface/uaccess.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE RINI45_RB(NEL,NUVAR,IPROP,IXR,NPBY,
     1                     LPBY,RBY,STIFR,UVAR,ITAB,
     1                     IGEO,IXR_KJ,GMASS,MS,IN)
      USE MESSAGE_MOD
C-------------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      INTEGER NEL,NUVAR,IPROP,IXR(NIXR,*),NPBY(NNPBY,NRBODY),LPBY(*),
     .        ITAB(*),IXR_KJ(5,*),IGEO(NPROPGI)
      my_real
     . RBY(NRBY,NRBODY),STIFR(*),UVAR(NUVAR,*),GMASS(*),MS(*),IN(*)
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
C-->        Search of connected rbody ---
            DO N=1,NRBODY
              NSL=NPBY(2,N)
              IF (NPBY(1,N)==NOD(I)) THEN
C--             Tag for error message - Can't be attached to main node of rbody-              
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
C-->		  
                  GOTO 100
                ENDIF
              ENDDO
100           K = K+NSL
            ENDDO
C
C-->        Stockage d'une masse elementaire (masse hormonique) pour calcul d'energie specifique
            GMASS(IEL) = (UVAR(34,IEL)*UVAR(35,IEL))/MAX(EM20,UVAR(34,IEL)+UVAR(35,IEL))
C-->
            IF (IDRB(I)==0) THEN	
C-->          no rbodies found - kjoint connected to structural node  ---
              UVAR(33+I,IEL)= MS(NOD(I))
              UVAR(35+I,IEL)= IN(NOD(I))
              UVAR(37+I,IEL)= 0
              IF (MS(NOD(I)) <= EM20) THEN
                CALL ANCMSG(MSGID=1773,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_2,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=IXR(NIXR,NFT+IEL),
     .                      I3=ITAB(NOD(I)))
              ELSEIF (IN(NOD(I)) <= EM20) THEN
                CALL ANCMSG(MSGID=1774,
     .                      MSGTYPE=MSGWARNING,
     .                      ANMODE=ANINFO_BLIND_2,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=IXR(NIXR,NFT+IEL),
     .                      I3=ITAB(NOD(I)))
              ENDIF
            ELSEIF (IDRB(I) < 0) THEN
C-->          kjoint connected to main node of rbody - error --	    
              CALL ANCMSG(MSGID=1768,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXR(NIXR,NFT+IEL),
     .                    I3=ITAB(NOD(I)))
	    ENDIF
C            
        ENDDO
C-->
      ENDDO

C---> Print de l'output pour kjoint2--------------------
      DO IEL=1,NEL
        IELUSR = IXR(NIXR,NFT+IEL)
        RB1 = 0
        RB2 = 0
	IF (UVAR(38,IEL) > 0) RB1 = NPBY(6,NINT(UVAR(38,IEL)))
	IF (UVAR(39,IEL) > 0) RB2 = NPBY(6,NINT(UVAR(39,IEL)))
        N1 = ITAB(IXR(2,NFT+IEL))
        N2 = ITAB(IXR(3,NFT+IEL))
	N3 = 0
	N4 = 0
        IF (IXR(4,NFT+IEL)/=0) N3 = ITAB(IXR(4,NFT+IEL)) 	
	LEN=SQRT(UVAR(1,IEL)**2+UVAR(2,IEL)**2+UVAR(3,IEL)**2)
	NUMEL_KJ = IXR_KJ(1,NUMELR+1)
        DO J=1,NUMEL_KJ
          IF (IXR_KJ(4,J)==IELUSR) ID_KJ = J
        END DO
        IF (ID_KJ>0) THEN 	
          IF (IXR_KJ(1,ID_KJ)/=0) N4 = ITAB(IXR_KJ(1,ID_KJ))
        ENDIF
        IDSK2 = NINT(GET_U_GEO(54,IPROP))
        IF (IDSK2==0) THEN
          WRITE(IOUT,2000)      	
          WRITE(IOUT,'(1X,5I10,4X,2I10,2X,F16.7,2X,3F16.7)') IELUSR,N1,
     .    N2,N3,N4,RB1,RB2,LEN,(UVAR(21+K,IEL),K=1,3)
          WRITE(IOUT,'(2(95X,3F16.7/))') (UVAR(21+K,IEL),K=4,9)
        ELSE
          WRITE(IOUT,2100)
          WRITE(IOUT,'(1X,5I10,4X,2I10,2X,F16.7,2X,F16.7,2X,3F16.7)') IELUSR,N1,
     .    N2,N3,N4,RB1,RB2,LEN,UVAR(7,IEL),(UVAR(21+K,IEL),K=1,3)
          WRITE(IOUT,'(2(95X,F16.7,2X,3F16.7))' ) UVAR(8,IEL) ,(UVAR(21+K,IEL),K=4,6)
          WRITE(IOUT,'(2(95X,F16.7,2X,3F16.7/))') UVAR(9,IEL) ,(UVAR(21+K,IEL),K=7,9)
        ENDIF
      ENDDO
      	
C-------------------------------------------------------

      RETURN
 2000 FORMAT(5X,'NUMBER',8X,'N1',8X,'N2',8X,'N3',8X,'N4',
     .       8X,'RBODY1',4X,'RBODY2',12X,'LENGTH',13X,
     .       'LOCAL SKEW (VECTORS)')

 2100 FORMAT(5X,'NUMBER',8X,'N1',8X,'N2',8X,'N3',8X,'N4',
     .       8X,'RBODY1',4X,'RBODY2',12X,'LENGTH',4X,'INITIAL ANGLES (RAD)',13X,
     .       'LOCAL SKEW (VECTORS)')

      RETURN
      END
C
