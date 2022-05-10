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
Chd|  FAIL_TAB_OLD_C                source/materials/fail/tabulated/fail_tab_old_c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|        VINTER                        source/tools/curve/vinter.F   
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FAIL_TAB_OLD_C(
     1           NEL      ,NUPARAM  ,NUVAR    ,UPARAM   ,UVAR     ,
     2           NFUNC    ,IFUNC    ,NPF      ,TF       ,
     3           TIME     ,NGL      ,IPG      ,ILAY     ,IPT      ,
     4           SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     5           DPLA     ,EPSP     ,THK      ,ALDT     ,TEMP     ,
     6           PTHKF    ,OFF      ,FOFF     ,
     7           DFMAX    ,TDEL     ,IGTYP    )
C-----------------------------------------------
C    tabulated failure model
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include "units_c.inc"
#include "comlock.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,NUPARAM,NUVAR,IPG,ILAY,IPT,IGTYP
      INTEGER ,DIMENSION(NEL) ,INTENT(IN) :: NGL
      my_real ,INTENT(IN) :: TIME
      my_real ,DIMENSION(NEL) ,INTENT(IN) :: OFF,THK,ALDT,DPLA,EPSP,
     .   TEMP,SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX
      my_real,DIMENSION(NUPARAM) ,INTENT(IN) :: UPARAM
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER ,DIMENSION(NEL) ,INTENT(INOUT) :: FOFF
      my_real ,INTENT(OUT) :: PTHKF
      my_real ,DIMENSION(NEL) ,INTENT(INOUT) :: DFMAX
      my_real ,DIMENSION(NEL) ,INTENT(OUT)   :: TDEL
      my_real ,DIMENSION(NEL,NUVAR) ,INTENT(INOUT) :: UVAR
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real FINTER ,TF(*)
      EXTERNAL FINTER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  :: I,J,J1,J2,K,IP_THICK,NINDX,NINDXF,IFAIL_SH,NRATE,
     .  IFUN_EL,IFUN_TEMP
      INTEGER, DIMENSION(NEL) :: INDX,INDXF,IPOSV,IADP,ILENP
      INTEGER IPOST(NEL,2)
      INTEGER ,DIMENSION(NFUNC) :: IFUN_STR
C
      my_real  :: P,SVM,DF,FAC,LAMBDA,DCRIT,EL_REF,SC_EL,SC_TEMP,P_THICK,
     .   DP,DD,DN,YY,YY_N,EF1,EF2
      my_real, DIMENSION(NFUNC) :: YFAC,RATE
      my_real, DIMENSION(NEL)   :: EPSF,EPSF_N,SIGM,YYV,DXDYV,LAMBDAV
C-----------------------------------------------
C UVAR(1) = DAMAGE
C UVAR(2) = initial characteristic el. length
C UVAR(3) = IPOS variable for element length scale function interpolation
C=======================================================================
      IF (TIME == ZERO) THEN
        UVAR(1:NEL,2) = ALDT(1:NEL) 
      ENDIF
c---------------------------
      IFAIL_SH  = INT(UPARAM(2))
      PTHKF     = UPARAM(3)
      P_THICK   = UPARAM(3)
      DCRIT     = UPARAM(4)
      DD        = UPARAM(5)
      DN        = UPARAM(6)
      SC_TEMP   = UPARAM(7)
      SC_EL     = UPARAM(8)
      EL_REF    = UPARAM(9)
      NRATE     = NFUNC - 2
      YFAC(1:NRATE) = UPARAM(11+1    :11+NRATE)
      RATE(1:NRATE) = UPARAM(11+NRATE:11+NRATE*2)
c-------------------------------------------------------------------
c---- Failure strain functions
      IFUN_STR(1:NRATE) = IFUNC(1:NRATE) 
c---- Scale functions       
      IFUN_EL   = IFUNC(NRATE+1)
      IFUN_TEMP = IFUNC(NRATE+2)
c---------------------------
      IF (P_THICK > ZERO .and. IFAIL_SH > 1 .and. IGTYP <= 10) THEN
        PTHKF = P_THICK
      ELSEIF (IFAIL_SH == 1) THEN
        PTHKF = EM06
      ELSEIF (IFAIL_SH == 2) THEN
        PTHKF = UN - EM06
      ENDIF
C---------
      NINDXF = 0  
      NINDX  = 0  
      DO I=1,NEL
        IF (OFF(I) == UN .and. FOFF(I) == 1) THEN
          NINDX = NINDX+1                                                 
          INDX(NINDX) = I
        ENDIF
      ENDDO                                                                                   
c-------------------------------------------------------------------
c     Failure strain value - function interpolation
c-------------------------------------------------------------------
      DO J=1,NINDX                           
        I = INDX(J)  
        J1 = 1                                                             
        DO K=2, NRATE-1                                                     
          IF (EPSP(I) > RATE(I)) J1 = K                                   
        ENDDO                                                              
        P    = THIRD*(SIGNXX(I) + SIGNYY(I))     
        SVM  = SQRT(SIGNXX(I)*SIGNXX(I) + SIGNYY(I)*SIGNYY(I)          
     .       - SIGNXX(I)*SIGNYY(I) + TROIS*SIGNXY(I)*SIGNXY(I))        
        SIGM = P / MAX(EM20,SVM)
c----
        IF (NRATE > 1) THEN                                                 
          J2  = J1+1                                                        
          EF1 = YFAC(J1)*FINTER(IFUNC(J1),SIGM,NPF,TF,DF)            
          EF2 = YFAC(J2)*FINTER(IFUNC(J2),SIGM,NPF,TF,DF)            
          FAC = (EPSP(I) - RATE(J1)) / (RATE(J2) - RATE(J1))              
          EPSF(I) = MAX(EF1 + FAC*(EF2 - EF1), EM20)                       
        ELSE                                                               
          EPSF(I) = YFAC(J1)*FINTER(IFUNC(J1),SIGM,NPF,TF,DF)        
        ENDIF  
      ENDDO
c----
      IF (IFUN_EL > 0) THEN
#include "vectorize.inc"
        DO J=1,NINDX                           
          I = INDX(J)  
c----   element length scale function
          LAMBDAV(J) = UVAR(I,2) / EL_REF
          IPOSV(J) = NINT(UVAR(I,3))
          IADP (J) = NPF(IFUN_EL) / 2 + 1
          ILENP(J) = NPF(IFUN_EL + 1) / 2 -IADP (J) - IPOSV(J)
        ENDDO
c
        CALL VINTER(TF,IADP,IPOSV,ILENP,NINDX,LAMBDAV,DXDYV,YYV)
c
#include "vectorize.inc"
        DO J=1,NINDX                           
          I = INDX(J)  
          FAC = SC_EL*YYV(J) 
          EPSF(I)   = EPSF(I)* FAC 
          UVAR(I,3) = IPOSV(J)
        ENDDO                                                            
      ENDIF  
c----  
#include "vectorize.inc"
      DO J=1,NINDX                           
        I = INDX(J)  
        EPSF_N(I) = ZERO
      ENDDO
c----   temperature scale function
      IF (IFUN_TEMP > 0) THEN
        DO J=1,NINDX                           
          I = INDX(J)  
          FAC = SC_TEMP*FINTER(IFUN_TEMP,TEMP(I),NPF,TF,DF) 
          EPSF(I) = EPSF(I)* FAC 
        ENDDO
      ENDIF    
c-----------------------------------------------------------------------------
      DO J=1,NINDX                           
        I = INDX(J)  
          IF (UVAR(I,1) < DCRIT) THEN
            IF (UVAR(I,1) == ZERO) THEN 
              DP = UN
            ELSE
              DP = DN*UVAR(I,1)**(UN-UN/DN)
            ENDIF
            IF (EPSF(I) > ZERO) UVAR(I,1) = UVAR(I,1)+DP*DPLA(I)/EPSF(I)
c-----            
            IF (UVAR(I,1) >= DCRIT) THEN
              NINDXF = NINDXF+1                                                 
              INDXF(NINDXF) = I                                                                                     
              TDEL(I)= TIME  
              IF (IFAIL_SH == 3) THEN
                FOFF(I) = -1
              ELSE
                FOFF(I) = 0
              ENDIF
            ENDIF                                                 
          ENDIF  
      ENDDO ! IEL
c
c--------------------------------------------      
c     Maximum Damage storing for output : 0 < DFMAX < 1
c
      DO J=1,NINDX                           
        I = INDX(J)  
        DFMAX(I)= MIN(UN,MAX(DFMAX(I),UVAR(I,1)/DCRIT))
      ENDDO
c--------------------------------------------      
c     print
c--------------------------------------------      
      IF (NINDXF > 0) THEN  
        DO J=1,NINDXF                           
          I = INDXF(J)  
#include  "lockon.inc"
          WRITE(IOUT, 2000) NGL(I),IPG,ILAY,IPT
          WRITE(ISTDO,2100) NGL(I),IPG,ILAY,IPT,TIME
#include  "lockoff.inc" 
        ENDDO
      ENDIF  
c------------------------
 2000 FORMAT(1X,'FAILURE (FTAB) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3)
 2100 FORMAT(1X,'FAILURE (FTAB) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3,1X,'AT TIME :',1PE12.4)
c------------------------
      RETURN
      END
