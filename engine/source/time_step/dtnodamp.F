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
Chd|  DTNODAMP                      source/time_step/dtnodamp.F   
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE DTNODAMP(ITAB           ,MS       ,IN    ,STIFN ,STIFR ,DT2T,
     1                    WEIGHT         ,IGRNOD   ,DAMPR ,ISTOP ,
     2                    I_DAMP_RDOF_TAB,ICONTACT ,IXC   ,X     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "rad2r_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "scr02_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "units_c.inc"
#include      "com08_c.inc"
#include      "couple_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*),WEIGHT(*),ISTOP,
     .   I_DAMP_RDOF_TAB(*),ICONTACT(*),IXC(NIXC,*)
C     REAL ou REAL*8
      my_real DT2T, DMAST, DINERT, STIFN(*), STIFR(*), 
     .   MS(*) ,IN(*) ,DAMPR(NRDAMP,*),X(3,*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD) :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,I,J,ISK,IGR,ND,K,L,LL
      my_real FACTB,DAMPBR,DAMPAR,D_TSTART,D_TSTOP,DAMPT,
     .   DT2N(NUMNOD),BB,COEFF,STIF_MOD,DT_CIBLE,ETA,BETA,
     .   ALPHA,DAMPA0          
C----------------------------------------------------------

C--------> TAG des elements
        DO ND=1,NDAMP		
          IF (DAMPR(20,ND).GT.0) THEN	
          DO I=1,NUMELC
            DO J=1,4	
              K=IXC(J+1,I)
	      IF (ICONTACT(K).EQ.1) THEN
                DO L=1,4
		  LL = IXC(L+1,I)		
		  IF (LL.NE.K) THEN
		    I_DAMP_RDOF_TAB(LL) = DAMPR(19,ND)
		  ENDIF  
		END DO	    
	      ENDIF
            ENDDO
          ENDDO
	  ENDIF
        ENDDO

        DO ND=1,NDAMP
C-------->	
          IF (NCYCLE.EQ.0) THEN
	    ALPHA = DTMIN1(11)/DTFAC1(11)
	    IF (ALPHA.GT.0) THEN
	      DAMPA0=MAX(DAMPR(9,ND),DAMPR(11,ND),DAMPR(13,ND))      
	      DAMPR(9,ND)=MIN(DAMPR(9,ND),1/ALPHA)
	      DAMPR(11,ND)=MIN(DAMPR(11,ND),1/ALPHA)
	      DAMPR(13,ND)=MIN(DAMPR(13,ND),1/ALPHA)
	      DAMPAR=MAX(DAMPR(9,ND),DAMPR(11,ND),DAMPR(13,ND))
	      IF (DAMPA0.NE.DAMPAR) THEN		      
	        print *,"WARNING ALPHA reduced to",1/ALPHA
	      ENDIF	    	    
	    ENDIF
	  ENDIF	  
C-------->	  	
          IGR   = NINT(DAMPR(2,ND))
          ISK   = NINT(DAMPR(15,ND))
          FACTB = DAMPR(16,ND)
          D_TSTART = DAMPR(17,ND)
          D_TSTOP  = DAMPR(18,ND)	
          IF (TT.GE.D_TSTART .AND. TT.LE.D_TSTOP) THEN
            DAMPAR = MAX(DAMPR(9,ND),DAMPR(11,ND),DAMPR(13,ND))	  	
            DAMPBR = MAX(DAMPR(10,ND),DAMPR(12,ND),DAMPR(14,ND))	    
C-------->	    
            DO I=1,IGRNOD(IGR)%NENTITY
              N=IGRNOD(IGR)%ENTITY(I)
              IF(STIFR(N).LE.ZERO)THEN
                ISTOP=-ITAB(N)
              ELSEIF(IN(N).GT.ZERO)THEN
                DT2N(N)=DTFAC1(11)*SQRT(DEUX*IN(N)/STIFR(N))		
              ENDIF      	       	      	      
            ENDDO	    
C-------->
            DO I=1,IGRNOD(IGR)%NENTITY
              N=IGRNOD(IGR)%ENTITY(I)
              DAMPT  = MIN(DT1,DT2N(N))*FACTB
	      BETA = MIN(DAMPBR,DAMPT)
	      ALPHA = DAMPAR	         	      
	      BB = BETA+UNDEMI*UNDEMI*ALPHA*DT2N(N)*DT2N(N)
	      ETA = SQRT(BB*BB + DT2N(N)*DT2N(N)) - BB	      
	      IF (IDTMIN(11)==3) THEN      
	        DT_CIBLE = ETA+0*MAX(ETA,DTMIN1(11))
              ELSE		
	        DT_CIBLE = ETA
              ENDIF				      
	      COEFF = (UN+(2*BETA/DT_CIBLE))/((1-UNDEMI*ALPHA*DT_CIBLE))
              IF (DAMPR(19,ND).NE.0) THEN	   	        	      	      	              
	        IF ((I_DAMP_RDOF_TAB(N)+ICONTACT(N)).NE.0) THEN	
	          STIFR(N) = STIFR(N)*COEFF		              	       
	        ENDIF
	      ENDIF	 	       	      	      
            ENDDO
C-------->	    
          ENDIF	  
	  	     
	END DO
		
C
      RETURN
      END
