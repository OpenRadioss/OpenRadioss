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
Chd|  USERIN3                       source/elements/solid/solide/userin3.F
Chd|-- called by -----------
Chd|        S4INIT3                       source/elements/solid/solide4/s4init3.F
Chd|        SINIT3                        source/elements/solid/solide/sinit3.F
Chd|-- calls ---------------
Chd|        SROTA6_M1                     source/output/anim/srota6_M1.F
Chd|====================================================================
      SUBROUTINE USERIN3(
     .   SIGSP   ,SIGI    ,UVAR    ,EPS     ,
     .   IX      ,NIX     ,NSIGI   ,NUVAR   ,NEL     ,
     .   NSIGS   ,IUSER   ,IDEF    ,STRAGLOB,JHBE    ,
     .   IGTYP   ,X       ,BUFGAMA ,PT      ,SIGB    ,
     .   L_SIGB  ,IMAT    ,IPM     ,BUFMAT  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include       "param_c.inc"
#include       "vect01_c.inc"
#include       "com01_c.inc" 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPM(NPROPMI,*)
      INTEGER NIX, N, JPS,IUSER,NSIGS,IDEF,JHBE,IGTYP,L_SIGB,IMAT
      INTEGER IX(NIX,*), NSIGI, NUVAR,NEL,STRAGLOB(*),PT(*)
C     REAL
      my_real
     .   SIGSP(NSIGI,*),SIGI(NSIGS,NEL),UVAR(NEL),
     .   EPS(NEL,6),X(3,*),BUFGAMA(*),SIGB(NEL*L_SIGB),BUFMAT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IIP,JPT, II, JJ, IPT, IPP,IUS,IPSU,IPS,
     .        IFLAGINI,NVAR_TMP,IADB,NRATE
      my_real
     .   GAMA(6),TENS(6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER MA
C-----------------------------------------------
      DO I=LFT,LLT                                                       
        IF (STRAGLOB(I) == 1 )THEN                                       
          IF(JCVT==2)THEN                                   
            GAMA(1)=BUFGAMA(I	     )  				 
            GAMA(2)=BUFGAMA(I +   NEL)  				 
            GAMA(3)=BUFGAMA(I + 2*NEL)  				 
            GAMA(4)=BUFGAMA(I + 3*NEL)  				 
            GAMA(5)=BUFGAMA(I + 4*NEL)  				 
            GAMA(6)=BUFGAMA(I + 5*NEL)  				 
          ELSE                                                           
            GAMA(1)=ONE                                                   
            GAMA(2)=ZERO                                                 
            GAMA(3)=ZERO                                                 
            GAMA(4)=ZERO                                                 
            GAMA(5)=ONE                                                   
            GAMA(6)=ZERO                                                 
          END IF                                                         
        ENDIF                                                            
C    
        II=NFT+I
        JJ=PT(II)
        IFLAGINI = 1
        IF (JJ == 0) IFLAGINI = 0   
c                                                        
        IF (IUSER /= 0 .AND. IFLAGINI == 1) THEN
          IF (MTN == 36 .and. L_SIGB == 6) THEN
            IADB  = IPM(7,IMAT)                  
            NRATE = NINT(BUFMAT(IADB))
            NVAR_TMP = SIGSP(NVSOLID1 + NVSOLID2 + 3, JJ)
            IPSU = NVSOLID1 + NVSOLID2 + 4                     
            IF (NVAR_TMP > 6) THEN
              DO IUS = 1,6                                          
                IPP = I + (IUS - 1)*NEL
                IPS = IPSU + NRATE + 5                               
                SIGB(IPP) = SIGSP(IPS + IUS, JJ)                        
              ENDDO
            ENDIF                                                     
c                        
          ELSE IF (MTN >= 28) THEN     
            NVAR_TMP = SIGSP(NVSOLID1 + NVSOLID2 + 3, JJ)                 
            IPSU = NVSOLID1 + NVSOLID2 + 4                              
            DO IUS = 1, NVAR_TMP                                            
              IPP = I + (IUS -1)*NEL                                     
              UVAR(IPP) = SIGSP(IPSU + IUS, JJ)                          
            ENDDO                                                          
            DO IUS = NVAR_TMP+1, NUVAR                                            
              IPP = I + (IUS -1)*NEL                                     
              UVAR(IPP) = ZERO                          
            ENDDO                                                      
          ENDIF                                                       
        ENDIF                                                              
        IF(IDEF /= 0 .AND. NVSOLID2 /= 0 .AND. IFLAGINI == 1) THEN  		       
            EPS(I,1)=SIGSP(NVSOLID1 + 1 ,JJ)			       
            EPS(I,2)=SIGSP(NVSOLID1 + 2 ,JJ)			       
            EPS(I,3)=SIGSP(NVSOLID1 + 3 ,JJ)			       
            EPS(I,4)=SIGSP(NVSOLID1 + 4 ,JJ)			       
            EPS(I,5)=SIGSP(NVSOLID1 + 5 ,JJ)			       
            EPS(I,6)=SIGSP(NVSOLID1 + 6 ,JJ)			       
            IF (STRAGLOB(I) == 1) THEN				       
              TENS(1)=EPS(I,1)
              TENS(2)=EPS(I,2)
              TENS(3)=EPS(I,3)
              TENS(4)=EPS(I,4)
              TENS(5)=EPS(I,5)
              TENS(6)=EPS(I,6)
              CALL SROTA6_M1(X,IX(1,II),JCVT,TENS,GAMA,JHBE,IGTYP)   
              EPS(I,1)=TENS(1)  		     
              EPS(I,2)=TENS(2)  		     
              EPS(I,3)=TENS(3)  		     
              EPS(I,4)=TENS(4)  		     
              EPS(I,5)=TENS(5)  		     
              EPS(I,6)=TENS(6)
            ENDIF
        ENDIF							       
      ENDDO !  I=LFT,LLT                                                           
c-----------
      RETURN
      END
Chd|====================================================================
Chd|  USTRSIN3                      source/elements/solid/solide/userin3.F
Chd|-- called by -----------
Chd|        S4INIT3                       source/elements/solid/solide4/s4init3.F
Chd|        SINIT3                        source/elements/solid/solide/sinit3.F
Chd|-- calls ---------------
Chd|        SROTA6_M1                     source/output/anim/srota6_M1.F
Chd|====================================================================
      SUBROUTINE USTRSIN3(
     .   SIGI    ,SIG     ,IX      ,NIX     ,NSIGI   ,
     .   NEL     ,STRSGLOB,JHBE    ,IGTYP   ,X       ,
     .   BUFGAMA ,PT      ,VOLDP   ,RHO0    ,RHO     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include       "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIX, JHBE,IGTYP,NSIGI,NEL
      INTEGER IX(NIX,*), STRSGLOB(*),PT(*)
C     REAL
      my_real
     .   SIGI(NSIGI,*),SIG(NEL,6),X(3,*),BUFGAMA(*),RHO0(*),RHO(*)
      DOUBLE PRECISION
     .   VOLDP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IIP,JPT, II, JJ
      my_real
     .   GAMA(6),TENS(6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER MA
C-----------------------------------------------
      DO I=LFT,LLT                                                       
        II=NFT+I
        JJ=PT(II)
        IF ((JCVT+ISORTH)==0.OR.JJ==0 .OR. STRSGLOB(I) /= 1) CYCLE                                       
          IF(JCVT==2)THEN                                   
            GAMA(1)=BUFGAMA(I	     )  				 
            GAMA(2)=BUFGAMA(I +   NEL)  				 
            GAMA(3)=BUFGAMA(I + 2*NEL)  				 
            GAMA(4)=BUFGAMA(I + 3*NEL)  				 
            GAMA(5)=BUFGAMA(I + 4*NEL)  				 
            GAMA(6)=BUFGAMA(I + 5*NEL)  				 
          ELSE                                                           
            GAMA(1)=ONE                                                   
            GAMA(2)=ZERO                                                 
            GAMA(3)=ZERO                                                 
            GAMA(4)=ZERO                                                 
            GAMA(5)=ONE                                                   
            GAMA(6)=ZERO                                                 
          END IF                                                         
C 
          TENS(1)=SIGI(1,JJ)
          TENS(2)=SIGI(2,JJ)
          TENS(3)=SIGI(3,JJ)
          TENS(4)=SIGI(4,JJ)
          TENS(5)=SIGI(5,JJ)
          TENS(6)=SIGI(6,JJ)
          CALL SROTA6_M1(X,IX(1,II),JCVT,TENS,GAMA,JHBE,IGTYP)   
          SIG(I,1)=TENS(1)  		     
          SIG(I,2)=TENS(2)  		     
          SIG(I,3)=TENS(3)  		     
          SIG(I,4)=TENS(4)  		     
          SIG(I,5)=TENS(5)  		     
          SIG(I,6)=TENS(6)
         VOLDP(I) = RHO(I)*VOLDP(I) / RHO0(I)
      ENDDO !  I=LFT,LLT                                                           
c-----------
      RETURN
      END
