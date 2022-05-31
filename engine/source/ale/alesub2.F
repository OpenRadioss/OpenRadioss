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
Chd|  ALESUB2                       source/ale/alesub2.F          
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        BCS3V                         source/ale/bcs3.F             
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|====================================================================
      SUBROUTINE ALESUB2(
     1   NALE   ,V    ,DSAVE ,ICODT  ,ISKEW  ,
     2   SKEW   ,ASAVE,A     ,D      ,NELTST ,
     3   ITYPTST,ITASK,NODFT ,NODLT  ,DT2SAVE,
     4   STIFN  ,DT2T ,NELTSA,ITYPTSA,NELTS  ,
     5   WEIGHT ,FSKY ,FSKYV )
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com02_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "vale_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NALE(*),ICODT(*),ISKEW(*), WEIGHT(*), NELTST ,ITYPTST,
     .        ITASK,NODFT,NODLT,NELTSA ,ITYPTSA, NELTS
      my_real
     .   V(3,*),DSAVE(3,*),SKEW(LSKEW,*),ASAVE(3,*),A(3,*),D(3,*),
     .   DT2SAVE,STIFN(*),DT2T,FSKY(8,LSKY),FSKYV(LSKY,8)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,LCOD, INDX2(1024), I,IPRI,NINDX2, K, NISKFT, NISKLT
      my_real
     . FAC,ALS
C--------------------------------------------------------
C       ALE SUB-CYCLING SUITE
C--------------------------------------------------------
      IF(ITASK.EQ.0) THEN
          DT2=DT2*DTFSUB
          IF(DT2S.NE.ZERO)DT2=MIN(DT2,ONEP1*DT2S)
          IF(TT.GT.ZERO)THEN
           DT12S=HALF*(DT1+INT(DT2/DT1SAVE)*DT1SAVE)
          ELSE
           DT12S=HALF*DT2
          ENDIF
          DT2S=DT2
          DT1=DT1SAVE
          IPRI=MOD(NCYCLE,IABS(NCPRI))
          IF(IPRI.EQ.0.AND.ISPMD.EQ.0)THEN
            WRITE(IOUT,1000) ' FLUID TIME STEP ',DT2S,' SOLID',NELTS
            IF(NCPRI.LT.0)
     &        WRITE(ISTDO,1000)' FLUID TIME STEP ',DT2S
 1000       FORMAT(A,1PE11.4,A,I10)
          ENDIF
      ENDIF
C
      CALL MY_BARRIER
C
      DT2T=MIN(DT2,DT2SAVE)
      NELTST =NELTSA
      ITYPTST=ITYPTSA
C REMISE A VITESSE DIRECTIONS LAGRANGIENNES
      DO N=NODFT,NODLT
       IF(NALE(N).EQ.0)THEN
        V(1,N)=DSAVE(1,N)
        V(2,N)=DSAVE(2,N)
        V(3,N)=DSAVE(3,N)
       ENDIF
      ENDDO
      DO I=NODFT,NODLT,1024
        NINDX2 = 0
        DO N = I,MIN(NODLT,I+1023)
          LCOD=ICODT(N+NUMNOD+NUMNOD)
          IF(NALE(N)*LCOD.NE.0)THEN
            NINDX2 = NINDX2 + 1
            INDX2(NINDX2) = N
          ENDIF
        ENDDO
        IF (NINDX2.NE.0)
     .  CALL BCS3V(NINDX2,INDX2,ISKEW,ICODT(2*NUMNOD+1),V,
     .  	   DSAVE ,SKEW)
      ENDDO
C
C SAUVEGARDE DES DEPLACEMENTS
C
      DO N=NODFT,NODLT
 	DSAVE(1,N)=D(1,N)
 	DSAVE(2,N)=D(2,N)
 	DSAVE(3,N)=D(3,N)
      ENDDO

      IF(IPARIT.GT.0)THEN
C Les forces venant des solides sont stockees dans fsky
C et reutilises dans les sous cycles
C + meme strategie que P/off, ie utilisation de ASAVE
 	DO N=NODFT,NODLT
 	  ASAVE(1,N)=A(1,N)
 	  ASAVE(2,N)=A(2,N)
 	  ASAVE(3,N)=A(3,N)
 	ENDDO
        NISKFT =  1+ITASK*LSKY/NTHREAD
        NISKLT = (ITASK+1)*LSKY/NTHREAD
 	IF(IVECTOR.EQ.1) THEN
 	  DO K=1,8
 	    DO I=NISKFT,NISKLT
 	      FSKYV(I,K)=ZERO
 	    ENDDO
 	  ENDDO
 	ELSE
 	  DO K=1,8
 	    DO I=NISKFT,NISKLT
 	      FSKY(K,I)=ZERO
 	    ENDDO
 	  ENDDO
 	ENDIF
C Parith/OFF
      ELSE
 	DO N=NODFT,NODLT
 	  A(1,N)=A(1,N)*WEIGHT(N)
 	  A(2,N)=A(2,N)*WEIGHT(N)
 	  A(3,N)=A(3,N)*WEIGHT(N)
C
 	  ASAVE(1,N)=A(1,N)
 	  ASAVE(2,N)=A(2,N)
 	  ASAVE(3,N)=A(3,N)
 	ENDDO
      ENDIF
C
      RETURN
      END
