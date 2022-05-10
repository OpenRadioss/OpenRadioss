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
Chd|  STATIC                        source/general_controls/damping/static.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        NGR2USR                       source/input/freform.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE STATIC(V,VR,A,AR,MS,IN,IGRNOD,WEIGHT_MD)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "scr11_c.inc"
#include      "statr_c.inc"
#include      "stati_c.inc"
#include      "task_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER
     .   WEIGHT_MD(*)
      my_real
     .   V(3,*), VR(3,*), A(3,*), AR(3,*),MS(*),IN(*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD) :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J, I,LAST,N,NGR2USR,ISTAT2,ISTAT3
C     REAL ou REAL*8
      my_real
     .   ENCINO, OMEGA, UOMEGA, DOMEGA, OMEGA2, ENCINT, ENCINN,
     .   ENCGRP,ENCGRPN, ENCGRPR,ENCGRPRN
      EXTERNAL NGR2USR
C-----------------------------------------------
      DATA ENCINO /0.0/
      DATA LAST /0/
      SAVE ENCINO,LAST
C
      IF(ISTATG.LT.0)THEN
        ISTATG=NGR2USR(-ISTATG,IGRNOD,NGRNOD)
      ENDIF
      ISTAT2=0
      ISTAT3=0
      IF (NCYCLE==0) THEN
       IF ((ISTAT==2.OR.ISTAT==3).AND.TST_STOP==ZERO) TST_STOP=TSTOP
      END IF
      IF (ISTAT==2.AND.TT>=TST_START.AND.TT<=TST_STOP) ISTAT2=1
      IF (ISTAT==3.AND.TT>=TST_START.AND.TT<=TST_STOP) ISTAT3=1
      IF((ISTAT==1.OR.ISTAT3==1).AND.ISTATG==0)THEN
C
       OMEGA  = BETATE * DT12
       UOMEGA = UN - OMEGA
       DOMEGA = DEUX*BETATE
       OMEGA2 = (UN-DEUX*OMEGA)**2
       ENCINT = ENCIN
C       ENCINT = ENCIN+ENROT
       ENCINN = ENCINT * OMEGA2
C
       IF(IDTMINS==0) THEN
        DO I = 1, NUMNOD
           A(1,I)  = -DOMEGA*V(1,I)  + UOMEGA*A(1,I)
           A(2,I)  = -DOMEGA*V(2,I)  + UOMEGA*A(2,I)
           A(3,I)  = -DOMEGA*V(3,I)  + UOMEGA*A(3,I)
        ENDDO
        IF (ISPMD.EQ.0) TFEXT = TFEXT - ENCINT + ENCINN
       END IF !(IDTMINS==0) THEN
C	   
        IF(IRODDL.NE.0)THEN
         ENCINT = ENROT
         ENCINN = ENCINT * OMEGA2
          DO I = 1, NUMNOD
           AR(1,I) = -DOMEGA*VR(1,I) + UOMEGA*AR(1,I)
           AR(2,I) = -DOMEGA*VR(2,I) + UOMEGA*AR(2,I)
           AR(3,I) = -DOMEGA*VR(3,I) + UOMEGA*AR(3,I)
          ENDDO
         IF (ISPMD.EQ.0) TFEXT = TFEXT - ENCINT + ENCINN
        ENDIF
C
      ELSEIF(ISTAT2==1.AND.ISTATG==0)THEN
C
       LAST = LAST+1
       ENCINT = ENCIN+ENROT
       IF(ENCINT.LT.ENCINO)THEN
        IF (IMACH.NE.3.OR.(IMACH.EQ.3.AND.ISPMD.EQ.0))
     .   TFEXT = TFEXT - ENCINT 
        ENCINO=ZERO
        LAST=0
C
        DO J=1,3
         DO I = 1, NUMNOD
           V(J,I) = ZERO
         ENDDO
         IF(IRODDL.NE.0)THEN
           DO I = 1, NUMNOD
             VR(J,I) = ZERO
           ENDDO
         ENDIF
        ENDDO
       ELSE
        ENCINO = ENCINT
       ENDIF
      ELSEIF((ISTAT==1.OR.ISTAT3==1).AND.ISTATG/=0)THEN
C
       OMEGA  = BETATE * DT12
       UOMEGA = UN - OMEGA
       DOMEGA = DEUX*BETATE
       OMEGA2 = (UN-DEUX*OMEGA)**2
       ENCINT = ENCIN+ENROT
       ENCINN = ENCINT * OMEGA2
       ENCGRP = ZERO
       ENCGRPR = ZERO
C
       IF(IDTMINS==0) THEN
       DO J=1,3
#include "vectorize.inc"
        DO N=1,IGRNOD(ISTATG)%NENTITY
           I=IGRNOD(ISTATG)%ENTITY(N)
           ENCGRP = ENCGRP + MS(I)*WEIGHT_MD(I)*
     .              V(J,I)*V(J,I)
           A(J,I)  = -DOMEGA*V(J,I)  + UOMEGA*A(J,I)
        ENDDO
       ENDDO
       END IF !(IDTMINS==0) THEN
C	   
        IF(IRODDL.NE.0)THEN
         DO J=1,3
#include "vectorize.inc"
          DO N=1,IGRNOD(ISTATG)%NENTITY
           I=IGRNOD(ISTATG)%ENTITY(N)
           ENCGRPR = ENCGRPR + MS(I)*WEIGHT_MD(I)*
     .              VR(J,I)*VR(J,I)
           AR(J,I) = -DOMEGA*VR(J,I) + UOMEGA*AR(J,I)
          ENDDO
         ENDDO
        ENDIF
C
       ENCGRP = UNDEMI*ENCGRP
       ENCGRPR = UNDEMI*ENCGRPR
       ENCGRPN = ENCGRP * OMEGA2
       ENCGRPRN = ENCGRPR * OMEGA2
       IF (IMACH.NE.3.OR.(IMACH.EQ.3.AND.ISPMD.EQ.0))
     .  TFEXT = TFEXT + ENCGRPN - ENCGRP + ENCGRPRN - ENCGRPR
C
      ELSEIF(ISTAT2==1.AND.ISTATG/=0)THEN
C
       LAST = LAST+1
       ENCINT = ENCIN+ENROT
       IF(ENCINT.LT.ENCINO)THEN
        IF (IMACH.NE.3.OR.(IMACH.EQ.3.AND.ISPMD.EQ.0))
     .   TFEXT = TFEXT - ENCINT 
        ENCINO=ZERO
        LAST=0
C
        DO J=1,3
#include "vectorize.inc"
         DO N=1,IGRNOD(ISTATG)%NENTITY
           I=IGRNOD(ISTATG)%ENTITY(N)
           V(J,I) = ZERO
         ENDDO
         IF(IRODDL.NE.0)THEN
#include "vectorize.inc"
           DO N=1,IGRNOD(ISTATG)%NENTITY
            I=IGRNOD(ISTATG)%ENTITY(N)
            VR(J,I) = ZERO
           ENDDO
         ENDIF
        ENDDO
       ELSE
        ENCINO = ENCINT
       ENDIF
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  E_PERIOD                      source/general_controls/damping/static.F
Chd|-- called by -----------
Chd|        ENER_W0                       source/general_controls/damping/static.F
Chd|-- calls ---------------
Chd|        BUTTERWORTH                   source/tools/univ/butterworth.F
Chd|====================================================================
      SUBROUTINE E_PERIOD(IPI,IPC,F_FIL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPI,IPC
C
      my_real
     .   F_FIL
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "scr11_c.inc"
#include      "statr_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J, I , IDF
C     REAL ou REAL*8
      my_real
     .   ENCINT, EINT,VV1,VV2,FV1,FV2,FV,FF
C-----------------------------------------------
       IF (TT == ZERO) THEN
        FF= ENCIN+ENROT
        FIL_KE(1) = FF                               
        FIL_KE(2) = FF                              
        FIL_KE(3) = FF                              
        FIL_KE(4) = FF
        FF= ENINT
        FIL_IE(1) = FF                               
        FIL_IE(2) = FF                              
        FIL_IE(3) = FF                              
        FIL_IE(4) = FF
       END IF       
       IF (F_FIL>ZERO) THEN
          FF = ENCIN+ENROT
    	     VV1 = FIL_KE(1)                                
    	     VV2 = FIL_KE(2)                               
    	     FV1 = FIL_KE(3)                                
    	     FV2 = FIL_KE(4)                                
    	     CALL BUTTERWORTH(DT2,F_FIL,VV2,VV1,FF,FV2,FV1,FV) 
    	       FIL_KE(1)= FF                               
    	       FIL_KE(2)= VV1                              
    	       FIL_KE(3)= FV                              
    	       FIL_KE(4)= FV1                              
              ENCINT = FV
C         
          FF = ENINT
    	     VV1 = FIL_IE(1)                                
    	     VV2 = FIL_IE(2)                               
    	     FV1 = FIL_IE(3)                                
    	     FV2 = FIL_IE(4)                                
    	     CALL BUTTERWORTH(DT2,F_FIL,VV2,VV1,FF,FV2,FV1,FV) 
    	       FIL_IE(1) = FF                                
    	       FIL_IE(2) = VV1                              
    	       FIL_IE(3) = FV                               
    	       FIL_IE(4) = FV1
              EINT = FV
C 
       ELSE      
        ENCINT = ENCIN+ENROT
        EINT = ENINT
       END IF
       PCIN = PCIN +DT1
       PINT = PINT +DT1
       PIMAX = MAX(PIMAX,PINT)
       PCMAX = MAX(PCMAX,PCIN)
       IF(ENCINT.LT.ENCIN_0.AND.ENCINT>=ZERO)THEN
        ENCIN_0=ZERO
        PCIN = ZERO
        IPC = 1
       ELSE
        IPC = 0
        ENCIN_0 = ENCINT
       ENDIF
       IF(EINT<ZERO) THEN
        EINT_0=ZERO
        PINT = ZERO
        IPI = -2
       ELSEIF(EINT.LT.EINT_0.AND.EINT>=ZERO)THEN
        EINT_0=ZERO
        PINT = ZERO
        IPI = 1
       ELSE
        EINT_0 = EINT
        IPI = 0
       ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  ENER_W0                       source/general_controls/damping/static.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        E_PERIOD                      source/general_controls/damping/static.F
Chd|====================================================================
      SUBROUTINE ENER_W0
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "statr_c.inc"
#include      "scr16_c.inc"
#include      "task_c.inc"
#include      "sms_c.inc"
#include      "warn_c.inc"   
#include      "scr05_c.inc"
#include      "scr07_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J, I,FREQ,IDF,IPI,IPC,NC_ACT,IFIRST
      PARAMETER (NC_ACT = 200)
C     REAL ou REAL*8
      my_real
     .   Q_ES,BETATE_N,FC,FI,BETATE_M
      my_real
     .   F_MAX,F_MIN,F_0,EI_TOL
C     REAL ou REAL*8
      CHARACTER*8 FILNAME
C-----------------------------------------------
      CALL E_PERIOD(IPI,IPC,FREQ_C)
      IF (NCYCLE==0) THEN
       BETATE = BETATE_0
       IF (DEBUG(11)==1.AND.ISPMD==0) THEN
        IDF = 251  
         FILNAME='P_ES.TMP'
         OPEN(UNIT=IDF,FILE=FILNAME,STATUS='UNKNOWN',FORM='FORMATTED')
         write(IDF,*)
     .  '# NCYCLE OMEGA(Retenu)IFIRST           OMEGA_N(new)   P_Eint         P_Kin'
       END IF
C--to avoid division by zero /DT1   
       RETURN
      END IF
C-------      /DEBUG/ADYREL for periods print out
      IF (DEBUG(11)==1.AND.ISPMD==0) THEN
       IDF = 251
      ELSE
       IDF = 0
      END IF 
      IFIRST = NINT(NFIRST)
      EI_TOL=EM12
      IF (IRESP==1) EI_TOL=EM07        
C-----0.5%w_max 5*0.5 for AMS     
      IF(IDTMINS==0) THEN
       F_MAX=EM02/DT1
      ELSE
       F_MAX=ZEP05/DT1
      END IF
      IF(FREQ_C<ZERO) FREQ_C = -FREQ_C*F_MAX
      F_0=EM02*F_MAX
      BETATE_N = F_MAX
      IF (IPI==1) THEN
       FI = UN/PIMAX
       BETATE_N = MIN(F_MAX,FI)
       IF (BETATE==ZERO.AND.EINT_0>EI_TOL) THEN
        IF (NCYCLE>=NC_ACT) THEN
         BETATE = MIN(F_0,BETATE_N)
         IFIRST = 1
        END IF
C------exception for increasing----        
       ELSEIF (IFIRST==1) THEN
        BETATE = BETATE_N
        IFIRST = IFIRST+1
       ELSE
        BETATE = MIN(BETATE,BETATE_N)
       END IF
        IF (IDF>0) write(IDF,1000)-NCYCLE,BETATE,IFIRST,FI,PIMAX ,PCMAX
      END IF
      IF (IPC==1) THEN
       FC = UN/PCMAX
       BETATE_N = MIN(F_MAX,FC)
       IF (BETATE==ZERO.AND.EINT_0>EI_TOL) THEN
        IF (NCYCLE>=NC_ACT) THEN
         BETATE = MIN(F_0,BETATE_N)
         IFIRST = 1
        END IF
       ELSEIF (IFIRST==1) THEN
        BETATE = BETATE_N
        IFIRST = IFIRST+1
       ELSE
C----limiting the using of PCMAX not take it if Ke/Eint is very small       
        IF (ENCIN_0/MAX(EM20,EINT_0)>EM03.AND.BETATE<BETATE_N*TDEMI) THEN
         BETATE_M = UNDEMI*BETATE
         BETATE = MIN(BETATE,BETATE_N)
         BETATE = MAX(BETATE,BETATE_M)
        END IF
       END IF
        IF (IDF>0) write(IDF,1000)NCYCLE,BETATE,IFIRST,FC,PIMAX ,PCMAX
      END IF
C--Case high periods for both Eint and Ke    
      IF (BETATE==ZERO.AND.EINT_0>EI_TOL.AND.NCYCLE>=NC_ACT) THEN
C------initial frequency is 1% of F_MAX otherwise damping too high especially w/ gravity    
       BETATE=F_0
       FI = UN/PIMAX
       IFIRST = 1
       IF (IDF>0) write(IDF,1000)NCYCLE,BETATE,IFIRST,FI,PIMAX ,PCMAX
      END IF
C--Case BETATE too high w/ large periods for both Eint and Ke    
      IF (IFIRST>=1.AND.(IPC+IPI)==0) THEN 
       FI = UN/MAX(PIMAX,PCMAX)
       IF (BETATE>1.1*FI) THEN
        BETATE=FI
        IF (IDF>0) write(IDF,1000)NCYCLE,BETATE,-IFIRST,FI,PIMAX ,PCMAX
       END IF
      END IF      
      IF (IFIRST==1.AND.IRUN>1.AND.MCHECK==0) IFIRST = 2
      NFIRST = IFIRST
C
      RETURN
 1000 FORMAT(I8,5(G14.7)/)
      END
