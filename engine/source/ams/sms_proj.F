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
Chd|  SMS_INIST                     source/ams/sms_proj.F         
Chd|-- called by -----------
Chd|        SMS_PCG                       source/ams/sms_pcg.F          
Chd|-- calls ---------------
Chd|        MAV_MM                        source/implicit/produt_v.F    
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SMS_MAM_NM                    source/ams/sms_proj.F         
Chd|        SMS_MAV_LT2                   source/ams/sms_pcg.F          
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        SMS_PCG_PROJ                  share/modules/sms_mod.F       
Chd|====================================================================
       SUBROUTINE SMS_INIST( 
     1           IADK  ,JDIK  ,DIAG_K ,LT_K  ,ITASK ,
     2           NODFT1_SMS,NODLT1_SMS,INDX1_SMS,NODNX_SMS,IAD_ELEM ,
     3           FR_ELEM   ,WEIGHT    ,JADI_SMS ,JDII_SMS ,LTI_SMS  ,
     4           ISKYI_SMS ,MSKYI_SMS ,FR_SMS   ,FR_RMS   ,LIST_SMS ,
     5           LIST_RMS  ,MSKYI_FI_SMS ,VFI   ,IMV      ,MV       ,
     6           MV6       ,MW6       ,MS       ,NODFT    ,NODLT    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
         USE SMS_PCG_PROJ
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NODFT, NODLT, ITASK, IADK(*)  ,JDIK(*),
     .         NODFT1_SMS,NODLT1_SMS,INDX1_SMS(*), NODNX_SMS(*),
     .         IAD_ELEM(2,NSPMD+1) ,FR_ELEM(*),WEIGHT(*),
     .         JADI_SMS(*),JDII_SMS(*),
     .         ISKYI_SMS(LSKYI_SMS,*),FR_SMS(NSPMD+1),FR_RMS(NSPMD+1),
     .         LIST_SMS(*), LIST_RMS(*), IMV(*)
C     REAL ou REAL*8
      my_real
     .  DIAG_K(*), LT_K(*)  ,LTI_SMS(*), MSKYI_SMS(*),
     .  MSKYI_FI_SMS(*), VFI(*), MV(*), MS(*)
      DOUBLE PRECISION MV6(6,3,*), MW6(6,3,*)
C-----------------------------------------------
c FUNCTION: initialization of S,T of Projection
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   NUMNOD,NNZ            - dimension of [K]  and number of non zero (complete matrix)
c  I   IADK,JDIK             - indice arrays for compressed row(col.) format of [K]
c  I   DIAG_K(NUMNOD)        - diagonal terms of [K]
c  I   LT_K(NNZ)             - [K]
c  O   Proj_S(NUMNOD,M_VS)   - [S] reduced small Eigenvectors
c  O   Proj_T(NUMNOD,M_VS)   - [T] =[K][S]
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#if !defined(WITHOUT_LINALG)
       CHARACTER          JOBZ, UPLO
       INTEGER I,J,IT,IP,NLIM,ND,IUPD,IPRI,IERROR,NNZI,M,
     .         F_DDLI,L_DDLI,INFO,LW,INORM, M_VS1
       my_real
     .  WORK(3*M_VS_SMS+9),W(M_VS_SMS+3)
C---------------------
      CALL MY_BARRIER
C---------------------
      M_VS1 = NUPDTL_SMS+3
C---------------------
C     T=[K][S]
C---------------------
      DO M = 1,M_VS1
        PROJ_T(NODFT:NODLT,M)=ZERO
      END DO !M = 1,M_VS1
C----------------------
      CALL MY_BARRIER
C---------------------
      IF(IMONM.GT.0.AND.ITASK==0)CALL STARTIME(71,1)
      DO M = 1,M_VS1
        CALL SMS_MAV_LT2(
     1          NODFT   ,NODLT  ,NUMNOD ,IADK  ,JDIK  ,
     2          ITASK   ,DIAG_K ,LT_K   ,PROJ_S(1,M),PROJ_T(1,M),
     3          NODFT1_SMS,NODLT1_SMS,INDX1_SMS,NODNX_SMS,IAD_ELEM ,
     4          FR_ELEM   ,WEIGHT    ,JADI_SMS ,JDII_SMS ,LTI_SMS  ,
     5          ISKYI_SMS ,MSKYI_SMS ,FR_SMS   ,FR_RMS   ,LIST_SMS ,
     6          LIST_RMS  ,MSKYI_FI_SMS ,VFI   ,IMV      ,MV       ,
     7          MV6       ,MW6            )
C----------------------
        CALL MY_BARRIER
C---------------------
      END DO !M = 1,M_VS1
      IF(IMONM.GT.0.AND.ITASK==0)CALL STOPTIME(71,1)
C---------------------- 
C        [k0]=[S]^t[T]
C---------------------
      IF(IMONM.GT.0.AND.ITASK==0)CALL STARTIME(72,1)
      CALL SMS_MAM_NM(NODFT  ,NODLT  ,NUMNOD,M_VS1,PROJ_S ,
     .             PROJ_T ,PROJ_K ,WEIGHT ,ITASK   )
      IF(IMONM.GT.0.AND.ITASK==0)CALL STOPTIME(72,1)
C----------------------
      CALL MY_BARRIER
C---------------------
C        ([k0]-lamda(i)[m0])*phi(i)=0; Rather ([k0]-lamda(i)[I])*phi(i)=0
C---------------------
      IF (ITASK==0) THEN
        JOBZ='V'
        UPLO='U'
        LW=3*M_VS_SMS+9
#ifdef MYREAL8
        CALL DSYEV(JOBZ, UPLO, M_VS1,PROJ_K, M_VS1,
     .             W, WORK, LW, INFO)
#else
        CALL SSYEV(JOBZ, UPLO, M_VS1,PROJ_K, M_VS1,
     .             W, WORK, LW, INFO)
#endif
C----------------------
C        [S]<-[S][phi]
C---------------------
        CALL MAV_MM(NUMNOD ,M_VS1 ,PROJ_S ,PROJ_K  ,ITASK )
C----------------------
C        [T]<-[T][phi]
C---------------------
        CALL MAV_MM(NUMNOD ,M_VS1 ,PROJ_T ,PROJ_K  ,ITASK )
C----------------------
C        [LAMDA]^-1<- 1.0/lamda(i)
C---------------------
        DO I=1,M_VS1
         PROJ_LA_1(I)= ONE/SIGN(MAX(EM20,ABS(W(I))),W(I))
        ENDDO

        if (ncprisms/=0) then
         write(iout,*)' ** INFO ** EIGENVALUES =',
     .       (ONE/PROJ_LA_1(I),I=1,M_VS_SMS)
        endif
C
      END IF !(ITASK==0) THEN
#endif
      RETURN
      END
Chd|====================================================================
Chd|  SMS_INIX                      source/ams/sms_proj.F         
Chd|-- called by -----------
Chd|        SMS_PCG                       source/ams/sms_pcg.F          
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SMS_MAV_MN                    source/ams/sms_proj.F         
Chd|        SMS_MAV_NM                    source/ams/sms_proj.F         
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        SMS_PCG_PROJ                  share/modules/sms_mod.F       
Chd|====================================================================
      SUBROUTINE SMS_INIX(NODFT,NODLT,NUMNOD,X  ,R ,WEIGHT,ITASK ,
     .                    DIAG_SMS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
         USE SMS_PCG_PROJ
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NODFT,NODLT,NUMNOD,ITASK,WEIGHT(*)
C     REAL ou REAL*8
      my_real
     .  X(3,*), R(3,*), DIAG_SMS(*)
C-----------------------------------------------
c FUNCTION: initialization of X0=[S][LAMDA]^-1[S]^t{R}
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   NUMNOD            - equation dimension
c  I   NPV               - projection vector number
c  I   WEIGHT(*)         - itag for each node of subdomains
c  I   NODFT,NODLT,ITASK - id in each ITASK:thread id (//)
c  I   R(3,NUMNOD)       - right-hand vector
c  O   X(3,NUMNOD)       - X0
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER I,J,M,NPV, I3, I2, I1
       my_real
     .         RSAV(3,NUMNOD), UNS
C----------------------
      NPV = NUPDTL_SMS
C----------------------
      DO I=NODFT,NODLT
        RSAV(1,I)=R(1,I)
        RSAV(2,I)=R(2,I)
        RSAV(3,I)=R(3,I)
        IF(DIAG_SMS(I)/=ZERO)THEN
          UNS=ONE/SQRT(DIAG_SMS(I))
          R(1,I)=R(1,I)*UNS
          R(2,I)=R(2,I)*UNS
          R(3,I)=R(3,I)*UNS
        END IF
      END DO
C----------------------
C        {W}=[S]^t{R}
C---------------------
      IF(IMONM.GT.0.AND.ITASK==0)CALL STARTIME(72,1)
      CALL SMS_MAV_NM(NODFT,NODLT,NUMNOD ,NPV   ,PROJ_S ,
     .                R    ,PROJ_W,WEIGHT,ITASK )
      IF(IMONM.GT.0.AND.ITASK==0)CALL STOPTIME(72,1)
C----------------------
      CALL MY_BARRIER
C----------------------
      IF (ITASK == 0) THEN
C---------------------
C        [LAMDA]^-1{W}
C---------------------
       DO I=1,NPV
         I3=3*I
         I2=I3-1
         I1=I3-2
         PROJ_W(I3)=PROJ_W(I3)*PROJ_LA_1(I)
         PROJ_W(I2)=PROJ_W(I2)*PROJ_LA_1(I)
         PROJ_W(I1)=PROJ_W(I1)*PROJ_LA_1(I)
       ENDDO
C---------------------
C       {X0}=[S]{W}
C---------------------
       IF(IMONM.GT.0.AND.ITASK==0)CALL STARTIME(73,1)
       CALL SMS_MAV_MN(NUMNOD ,NPV   ,PROJ_S ,PROJ_W ,X  ,ITASK )
       IF(IMONM.GT.0.AND.ITASK==0)CALL STOPTIME(73,1)
C----------------------
      END IF !(ITASK ==0) THEN
C----------------------
      CALL MY_BARRIER
C----------------------
      DO I=NODFT,NODLT
        R(1,I)=RSAV(1,I)
        R(2,I)=RSAV(2,I)
        R(3,I)=RSAV(3,I)
        IF(DIAG_SMS(I)/=ZERO)THEN
          UNS=ONE/SQRT(DIAG_SMS(I))
          X(1,I)=X(1,I)*UNS
          X(2,I)=X(2,I)*UNS
          X(3,I)=X(3,I)*UNS
        END IF
      END DO
C----------------------
      CALL MY_BARRIER
C----------------------
      RETURN
      END
Chd|====================================================================
Chd|  SMS_PRO_P                     source/ams/sms_proj.F         
Chd|-- called by -----------
Chd|        SMS_PCG                       source/ams/sms_pcg.F          
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SMS_MAV_MN                    source/ams/sms_proj.F         
Chd|        SMS_MAV_NM                    source/ams/sms_proj.F         
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        SMS_PCG_PROJ                  share/modules/sms_mod.F       
Chd|====================================================================
      SUBROUTINE SMS_PRO_P(NODFT ,NODLT ,NUMNOD ,P  ,WEIGHT,ITASK ,
     .                     PJ    ,DIAG_SMS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
         USE SMS_PCG_PROJ
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NODFT ,NODLT ,NUMNOD ,ITASK,WEIGHT(*)
C     REAL ou REAL*8
      my_real
     .  P(3,*) ,PJ(3,*), DIAG_SMS(*)
C-----------------------------------------------
c FUNCTION: Projection of {p}={p}-[S][LAMDA]^-1[T]^t{p}
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   NUMNOD            - equation dimension
c  I   NPV               - projection vector number
c  I   WEIGHT(*)         - itag for each node of subdomains
c  I   NODFT,NODLT,ITASK - id in each ITASK:thread id (//)
c  IO  P(3,NUMNOD)       - PCG p vector
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER I,J,NPV, I3, I2, I1
       my_real
     .        S, UNS
C----------------------
      NPV = NUPDTL_SMS
C----------------------
      DO I=NODFT,NODLT
        S=SQRT(DIAG_SMS(I))
        P(1,I)=P(1,I)*S
        P(2,I)=P(2,I)*S
        P(3,I)=P(3,I)*S
      END DO
C----------------------
      CALL MY_BARRIER
C----------------------
C        {W}=[T]^t{p}
C---------------------
      IF(IMONM.GT.0.AND.ITASK==0)CALL STARTIME(73,1)
      CALL SMS_MAV_NM(NODFT,NODLT,NUMNOD ,NPV  ,PROJ_T ,
     .                P   ,PROJ_W ,WEIGHT,ITASK )
      IF(IMONM.GT.0.AND.ITASK==0)CALL STOPTIME(73,1)
C----------------------
      CALL MY_BARRIER
C---------------------
C        [LAMDA]^-1{W}
C---------------------
      IF (ITASK==0) THEN
        DO I=1,NPV
         I3=3*I
         I2=I3-1
         I1=I3-2
         PROJ_W(I3)=PROJ_W(I3)*PROJ_LA_1(I)
         PROJ_W(I2)=PROJ_W(I2)*PROJ_LA_1(I)
         PROJ_W(I1)=PROJ_W(I1)*PROJ_LA_1(I)
        ENDDO
C----------------------
C       {p}=[S]{W}
C---------------------
        IF(IMONM.GT.0.AND.ITASK==0)CALL STARTIME(73,1)
        CALL SMS_MAV_MN(NUMNOD,NPV   ,PROJ_S ,PROJ_W  ,PJ  ,ITASK )
        IF(IMONM.GT.0.AND.ITASK==0)CALL STOPTIME(73,1)
      END IF
C----------------------
      CALL MY_BARRIER
C----------------------
      DO I=NODFT,NODLT
        P(1,I)=P(1,I)-PJ(1,I)
        P(2,I)=P(2,I)-PJ(2,I)
        P(3,I)=P(3,I)-PJ(3,I)
        IF(DIAG_SMS(I)/=ZERO)THEN
          UNS=ONE/SQRT(DIAG_SMS(I))
          P(1,I)=P(1,I)*UNS
          P(2,I)=P(2,I)*UNS
          P(3,I)=P(3,I)*UNS
        END IF
      ENDDO
C----------------------
      CALL MY_BARRIER
C----------------------
C
      RETURN
      END
Chd|====================================================================
Chd|  SMS_UPDST                     source/ams/sms_proj.F         
Chd|-- called by -----------
Chd|        SMS_PCG                       source/ams/sms_pcg.F          
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SMS_PCG_PROJ                  share/modules/sms_mod.F       
Chd|====================================================================
       SUBROUTINE SMS_UPDST( 
     1           IADK  ,JDIK  ,DIAG_K ,LT_K  ,ITASK ,
     2           NODFT1_SMS,NODLT1_SMS,INDX1_SMS,NODNX_SMS,IAD_ELEM ,
     3           FR_ELEM   ,WEIGHT    ,JADI_SMS ,JDII_SMS ,LTI_SMS  ,
     4           ISKYI_SMS ,MSKYI_SMS ,FR_SMS   ,FR_RMS   ,LIST_SMS ,
     5           LIST_RMS  ,MSKYI_FI_SMS ,VFI   ,IMV      ,MV       ,
     6           MV6       ,MW6       ,MS       ,U        ,P        ,
     7           Y         ,NODFT     ,NODLT    ,KINET    )
C----------------------------------------------- 
C   M o d u l e s
C-----------------------------------------------
         USE SMS_PCG_PROJ
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NODFT, NODLT, ITASK, IADK(*)  ,JDIK(*),
     .         NODFT1_SMS,NODLT1_SMS,INDX1_SMS(*), NODNX_SMS(*),
     .         IAD_ELEM(2,NSPMD+1) ,FR_ELEM(*),WEIGHT(*),
     .         JADI_SMS(*),JDII_SMS(*),
     .         ISKYI_SMS(LSKYI_SMS,*),FR_SMS(NSPMD+1),FR_RMS(NSPMD+1),
     .         LIST_SMS(*), LIST_RMS(*), IMV(*), KINET(*)
C     REAL ou REAL*8
      my_real
     .  DIAG_K(*), LT_K(*)  ,LTI_SMS(*), MSKYI_SMS(*),
     .  MSKYI_FI_SMS(*), VFI(*), MV(*), MS(*), U(3,*), P(3,*), Y(3,*)
      DOUBLE PRECISION MV6(6,*), MW6(6,*)
C-----------------------------------------------
c FUNCTION: update S,T of Projection
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   NUMNOD,NNZ            - dimension of [K]  and number of non zero (complete matrix)
c  I   IADK,JDIK             - indice arrays for compressed row(col.) format of [K]
c  I   DIAG_K(NUMNOD)        - diagonal terms of [K]
c  I   LT_K(NNZ)             - [K]
c  O   Proj_S(NUMNOD,M_VS)   - [S] reduced small Eigenvectors
c  O   Proj_T(NUMNOD,M_VS)   - [T] =[K][S]
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       CHARACTER          JOBZ, UPLO
       INTEGER I,J,N,IT,IP,NLIM,ND,IUPD,IPRI,IERROR,NNZI,M,
     .         INFO,LW,M_VS1,INORM,NP
       my_real
     .  WORK(3*M_VS_SMS+9), W(M_VS_SMS+3), S
C------M_VS input one -NUPDTL_SMS: activated ---------------
       IF (NUPDTL_SMS == 0) RETURN
C----------------------
      CALL MY_BARRIER
C---------------------
       M_VS1 = NUPDTL_SMS + 3
C------add previous solution U ; default : aleatory updated w/ 1 vector x
       DO N=NODFT,NODLT
        IF(KINET(N)==0.AND.NODNX_SMS(N)/=0)THEN
          S=SQRT(DIAG_K(N))
          PROJ_S(N,NUPDTL_SMS+1)=U(1,N)*S
          PROJ_S(N,NUPDTL_SMS+2)=U(2,N)*S
          PROJ_S(N,NUPDTL_SMS+3)=U(3,N)*S
        ELSE
          PROJ_S(N,NUPDTL_SMS+1)=ZERO
          PROJ_S(N,NUPDTL_SMS+2)=ZERO
          PROJ_S(N,NUPDTL_SMS+3)=ZERO
        END IF
       ENDDO
C----------------------
       CALL MY_BARRIER
C---------------------
      RETURN
      END
Chd|====================================================================
Chd|  SMS_INISI                     source/ams/sms_proj.F         
Chd|-- called by -----------
Chd|        SMS_PCG                       source/ams/sms_pcg.F          
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SMS_INIS                      source/ams/sms_proj.F         
Chd|        SMS_MORTHO_GS                 source/ams/sms_proj.F         
Chd|        SPMD_EXCH_SMS                 source/mpi/ams/spmd_exch_sms.F
Chd|        SMS_PCG_PROJ                  share/modules/sms_mod.F       
Chd|====================================================================
      SUBROUTINE SMS_INISI( 
     1           IADK  ,JDIK  ,DIAG_K ,LT_K  ,ITASK ,
     2           NODFT1_SMS,NODLT1_SMS,INDX1_SMS,NODNX_SMS,IAD_ELEM ,
     3           FR_ELEM   ,WEIGHT    ,JADI_SMS ,JDII_SMS ,LTI_SMS  ,
     4           ISKYI_SMS ,MSKYI_SMS ,FR_SMS   ,FR_RMS   ,LIST_SMS ,
     5           LIST_RMS  ,MSKYI_FI_SMS ,VFI   ,IMV      ,MV       ,
     6           MV6       ,MW6       ,MS       ,NODFT    ,NODLT    ,
     7           PREC_SMS  ,KINET     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SMS_PCG_PROJ
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "sms_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NODFT, NODLT, ITASK, IADK(*)  ,JDIK(*),
     .         NODFT1_SMS,NODLT1_SMS,INDX1_SMS(*), NODNX_SMS(*),
     .         IAD_ELEM(2,NSPMD+1) ,FR_ELEM(*),WEIGHT(*),
     .         JADI_SMS(*),JDII_SMS(*),
     .         ISKYI_SMS(LSKYI_SMS,*),FR_SMS(NSPMD+1),FR_RMS(NSPMD+1),
     .         LIST_SMS(*), LIST_RMS(*), IMV(*), KINET(*)
C     REAL ou REAL*8
      my_real
     .  DIAG_K(*), LT_K(*)  ,LTI_SMS(*), MSKYI_SMS(*),
     .  MSKYI_FI_SMS(*), VFI(*), MV(*), MS(*), PREC_SMS(*)
      DOUBLE PRECISION MV6(6,3,*), MW6(6,3,*)
C-----------------------------------------------
c FUNCTION: initialization of S,by M_VS PCG iterations
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   NUMNOD,NNZ            - dimension of [K]  and number of non zero (complete matrix)
c  I   IADK,JDIK             - indice arrays for compressed row(col.) format of [K]
c  I   DIAG_K(NUMNOD)        - diagonal terms of [K]
c  I   LT_K(NNZ)             - [K]
c  O   Proj_S(NUMNOD,M_VS)   - [S] reduced small Eigenvectors
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER I,J,IT,IP,NLIM,ND,IUPD,IPRI,IERROR,NNZI,M,
     .         INFO,LW,INORM,NPV,ITP,SIZE,LENR,N
C---------------------
      NPV=MIN(NUMNOD-3,M_VS_SMS)
      NUPDTL_SMS=NPV
      NPV=NPV+3
C      
      IF (NCG_RUN_SMS == 0) THEN
C---------------------
        IUPD = 0
        IF(ITASK==0)THEN
          CALL SMS_INIS(NUMNOD,1 , NUMNOD,1 ,NPV  ,PROJ_S ,
     .                   NODNX_SMS,KINET )
C projeter PROJ_S sur cond cin. !!!
        END IF
C----------------------
        CALL MY_BARRIER
C---------------------
        IF(NSPMD > 1) THEN
          IF(ITASK == 0)THEN
            SIZE = 1
            LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
            DO I=1,NPV
              CALL SPMD_EXCH_SMS(PROJ_S(1,I),NODNX_SMS,IAD_ELEM,FR_ELEM,
     .                           SIZE,LENR)
            END DO
          END IF
        END IF
C--------matrix GS orthonalization and normalized
        CALL SMS_MORTHO_GS(NODFT  ,NODLT  ,NUMNOD,1      ,NPV,
     .                PROJ_S ,WEIGHT  ,ITASK  )
      ELSE
C
C projeter PROJ_S sur cond cin. !!!
        DO J=1,NPV
        DO N=NODFT,NODLT
         IF(KINET(N)/=0.OR.NODNX_SMS(N)==0)THEN
           PROJ_S(N,J)=ZERO
         END IF
        ENDDO
        ENDDO
C----------------------
       CALL MY_BARRIER
C---------------------
C--------matrix GS orthonalization and normalized
        CALL SMS_MORTHO_GS(NODFT  ,NODLT  ,NUMNOD,1      ,NPV,
     .                PROJ_S ,WEIGHT  ,ITASK  )
      END IF
C--------------------
      CALL MY_BARRIER
C--------------------
      RETURN
      END
Chd|====================================================================
Chd|  SMS_INIS                      source/ams/sms_proj.F         
Chd|-- called by -----------
Chd|        SMS_INISI                     source/ams/sms_proj.F         
Chd|-- calls ---------------
Chd|        ALEAT                         source/system/aleat.F         
Chd|====================================================================
      SUBROUTINE SMS_INIS(NUMNOD,NODFT ,NODLT, NPF,   NPL,   S   ,
     .                    NODNX_SMS,KINET )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NUMNOD, NODFT, NODLT, NPF,   NPL,
     .         NODNX_SMS(*), KINET(*)
C     REAL ou REAL*8
      my_real
     .  S(NUMNOD,*)
C-----------------------------------------------
c FUNCTION: initialization of [S] 
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   NODFT, NODLT      - equation dimension divised by Nthead
c  I   NPF,NPL           - projection vector number (first to last)
c  I   NUMNOD            - equation dimension
c  I   S(NUMNOD,NPV)     - S-Matrix
c  I   NODNX_SMS()       - 
c  I   KINET()           - 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N
      my_real
     .       ALEAT
      EXTERNAL ALEAT
C---------------------
      DO J=NPF,NPL
      DO N=NODFT, NODLT
       IF(KINET(N)==0.AND.NODNX_SMS(N)/=0)THEN
         S(N,J)=ALEAT()
       END IF
      ENDDO
      ENDDO
C
      RETURN
      END
C-----------Hybrid {x}t{y}-.{Weight}--
Chd|====================================================================
Chd|  SMS_PRODUT_H                  source/ams/sms_proj.F         
Chd|-- called by -----------
Chd|        SMS_MAM_NM                    source/ams/sms_proj.F         
Chd|        SMS_MORTHO_GS                 source/ams/sms_proj.F         
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_GLOB_DPSUM9              source/mpi/interfaces/spmd_th.F
Chd|        SPMD_RBCAST                   source/mpi/generic/spmd_rbcast.F
Chd|        SPMD_SUM_S                    source/mpi/implicit/imp_spmd.F
Chd|        SUM_6_FLOAT                   source/system/parit.F         
Chd|====================================================================
      SUBROUTINE SMS_PRODUT_H(NODFT  ,NODLT ,X   ,Y  ,WEIGHT, R ,ITASK )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
#include "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "parit_c.inc"
#include      "scr05_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NODFT  ,NODLT ,WEIGHT(*) ,ITASK
C     REAL ou REAL*8
      my_real
     .  X(*), Y(*)  ,R
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I , N, J, K, L, LLT
      my_real
     .  RTMP(MVSIZ), RL, RBUF
      DOUBLE PRECISION R6T(6), DBUF(6)
C-----------------------------
      IF(IPARIT==0)THEN
        IF (ITASK.EQ.0) R_N2_SMS = ZERO
C----------------------
      CALL MY_BARRIER
C---------------------
        RL = ZERO
        DO N=NODFT,NODLT,MVSIZ
         LLT =MIN(NODLT-N+1,MVSIZ)
C
         DO I=1,LLT
          J=N+I-1
          RTMP(I)=X(J)*Y(J)*WEIGHT(J)
         ENDDO
         DO I=1,LLT
          RL = RL + RTMP(I)
         ENDDO
        END DO 
#include "lockon.inc"
         R_N2_SMS = R_N2_SMS + RL
#include "lockoff.inc"
C----------------------
        CALL MY_BARRIER
C---------------------
        IF (NSPMD > 1 .AND. ITASK == 0) CALL SPMD_SUM_S(R_N2_SMS)
C----------------------
        CALL MY_BARRIER
C---------------------
        R = R_N2_SMS
      ELSE ! IPARIT/=0
C
        DO K=1,6
          R6SMS(K)=ZERO
        ENDDO 
C----------------------
        CALL MY_BARRIER
C---------------------
        DO N=NODFT,NODLT,MVSIZ
          LLT =MIN(NODLT-N+1,MVSIZ)
C
          DO I=1,LLT
           J=N+I-1
           RTMP(I)=X(J)*Y(J)*WEIGHT(J)
          ENDDO
          DO K=1,6
           R6T(K) = ZERO
          ENDDO 
          CALL SUM_6_FLOAT(1,LLT,RTMP,R6T,1)
#include "lockon.inc"
          DO K=1,6
            R6SMS(K)=R6SMS(K)+R6T(K)
          ENDDO 
#include "lockoff.inc"
        END DO
C----------------------
        CALL MY_BARRIER
C---------------------
        IF(NSPMD <= 1)THEN
          IF(ITASK==0)THEN
            R_N2_SMS=R6SMS(1)+R6SMS(2)+R6SMS(3)+
     .               R6SMS(4)+R6SMS(5)+R6SMS(6)
          END IF
        ELSEIF(ITASK==0)THEN
          DO K=1,6
            DBUF(K)  =R6SMS(K)
          END DO
          CALL SPMD_GLOB_DPSUM9(DBUF,6)
          RBUF  = DBUF(1)+DBUF(2)+DBUF(3)+
     .            DBUF(4)+DBUF(5)+DBUF(6)
          CALL SPMD_RBCAST(RBUF,RBUF,1,1,0,2)
          R_N2_SMS=RBUF
        END IF
C----------------------
        CALL MY_BARRIER
C---------------------
        R = R_N2_SMS
      END IF
C----------------------
      CALL MY_BARRIER
C---------------------
      RETURN
      END
C-----------Hybrid {x}t{y}-.{Weight}--
Chd|====================================================================
Chd|  SMS_PRODUT3                   source/ams/sms_proj.F         
Chd|-- called by -----------
Chd|        SMS_MAV_NM                    source/ams/sms_proj.F         
Chd|-- calls ---------------
Chd|        FOAT_TO_6_FLOAT               source/system/parit.F         
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_GLOB_DPSUM9              source/mpi/interfaces/spmd_th.F
Chd|        SPMD_GLOB_DSUM9               source/mpi/interfaces/spmd_th.F
Chd|        SPMD_RBCAST                   source/mpi/generic/spmd_rbcast.F
Chd|====================================================================
      SUBROUTINE SMS_PRODUT3(NODFT  ,NODLT ,X   ,Y  ,WEIGHT, R ,ITASK )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
#include "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "parit_c.inc"
#include      "scr05_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NODFT  ,NODLT ,WEIGHT(*) ,ITASK
C     REAL ou REAL*8
      my_real
     .  X(*), Y(3,*)  ,R(3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I , N, J, K, L, LLT
      my_real
     .  RTMP(3,MVSIZ), RL(3), RBUF(3)
      DOUBLE PRECISION R6(6,3,MVSIZ), R6T(3,6), DBUF(3,6)
C-----------------------------
      IF(IPARIT==0)THEN
        IF (ITASK.EQ.0) THEN
         R_N2_SMS1 = ZERO
         R_N2_SMS2 = ZERO
         R_N2_SMS3 = ZERO
        END IF
C----------------------
        CALL MY_BARRIER
C---------------------
        RL(1:3) = ZERO
        DO N=NODFT,NODLT,MVSIZ
         LLT =MIN(NODLT-N+1,MVSIZ)
C
         DO I=1,LLT
          J=N+I-1
          RTMP(1,I)=X(J)*Y(1,J)*WEIGHT(J)
          RTMP(2,I)=X(J)*Y(2,J)*WEIGHT(J)
          RTMP(3,I)=X(J)*Y(3,J)*WEIGHT(J)
         ENDDO
         DO I=1,LLT
          RL(1) = RL(1) + RTMP(1,I)
          RL(2) = RL(2) + RTMP(2,I)
          RL(3) = RL(3) + RTMP(3,I)
         ENDDO
        END DO 
#include "lockon.inc"
         R_N2_SMS1 = R_N2_SMS1 + RL(1)
         R_N2_SMS2 = R_N2_SMS2 + RL(2)
         R_N2_SMS3 = R_N2_SMS3 + RL(3)
#include "lockoff.inc"
C----------------------
        CALL MY_BARRIER
C---------------------
        IF (NSPMD > 1 .AND. ITASK == 0) THEN
          RBUF(1)=R_N2_SMS1
          RBUF(2)=R_N2_SMS2
          RBUF(3)=R_N2_SMS3
          CALL SPMD_GLOB_DSUM9(RBUF,3)
          R_N2_SMS1=RBUF(1)
          R_N2_SMS2=RBUF(2)
          R_N2_SMS3=RBUF(3)
        END IF
C----------------------
        CALL MY_BARRIER
C---------------------
        R(1) = R_N2_SMS1
        R(2) = R_N2_SMS2
        R(3) = R_N2_SMS3
      ELSE ! IPARIT/=0
C
        DO K=1,6
          X6SMS(1,K)=ZERO
          X6SMS(2,K)=ZERO
          X6SMS(3,K)=ZERO
        ENDDO 
C----------------------
        CALL MY_BARRIER
C---------------------
        DO N=NODFT,NODLT,MVSIZ
          LLT =MIN(NODLT-N+1,MVSIZ)
C
          DO I=1,LLT
           J=N+I-1
           RTMP(1,I)=X(J)*Y(1,J)*WEIGHT(J)
           RTMP(2,I)=X(J)*Y(2,J)*WEIGHT(J)
           RTMP(3,I)=X(J)*Y(3,J)*WEIGHT(J)
          ENDDO
          CALL FOAT_TO_6_FLOAT(1,3*LLT,RTMP,R6)
          DO K=1,6
           R6T(1,K) = ZERO
           R6T(2,K) = ZERO
           R6T(3,K) = ZERO
           DO L=1,LLT
             R6T(1,K) = R6T(1,K) + R6(K,1,L)
             R6T(2,K) = R6T(2,K) + R6(K,2,L)
             R6T(3,K) = R6T(3,K) + R6(K,3,L)
           ENDDO 
          ENDDO 
#include "lockon.inc"
          DO K=1,6
            X6SMS(1,K)=X6SMS(1,K)+R6T(1,K)
            X6SMS(2,K)=X6SMS(2,K)+R6T(2,K)
            X6SMS(3,K)=X6SMS(3,K)+R6T(3,K)
          ENDDO 
#include "lockoff.inc"
        END DO
C----------------------
        CALL MY_BARRIER
C---------------------
        IF(NSPMD <= 1)THEN
          IF(ITASK==0)THEN
            R_N2_SMS1=X6SMS(1,1)+X6SMS(1,2)+X6SMS(1,3)+
     .                X6SMS(1,4)+X6SMS(1,5)+X6SMS(1,6)
            R_N2_SMS2=X6SMS(2,1)+X6SMS(2,2)+X6SMS(2,3)+
     .                X6SMS(2,4)+X6SMS(2,5)+X6SMS(2,6)
            R_N2_SMS3=X6SMS(3,1)+X6SMS(3,2)+X6SMS(3,3)+
     .                X6SMS(3,4)+X6SMS(3,5)+X6SMS(3,6)
          END IF
        ELSEIF(ITASK==0)THEN
          DO K=1,6
            DBUF(1,K)  =X6SMS(1,K)
            DBUF(2,K)  =X6SMS(2,K)
            DBUF(3,K)  =X6SMS(3,K)
          END DO
          CALL SPMD_GLOB_DPSUM9(DBUF,18)
          RBUF(1) = DBUF(1,1)+DBUF(1,2)+DBUF(1,3)+
     .              DBUF(1,4)+DBUF(1,5)+DBUF(1,6)
          RBUF(2) = DBUF(2,1)+DBUF(2,2)+DBUF(2,3)+
     .              DBUF(2,4)+DBUF(2,5)+DBUF(2,6)
          RBUF(3) = DBUF(3,1)+DBUF(3,2)+DBUF(3,3)+
     .              DBUF(3,4)+DBUF(3,5)+DBUF(3,6)
          CALL SPMD_RBCAST(RBUF,RBUF,3,1,0,2)
          R_N2_SMS1=RBUF(1)
          R_N2_SMS2=RBUF(2)
          R_N2_SMS3=RBUF(3)
        END IF
C----------------------
        CALL MY_BARRIER
C---------------------
        R(1) = R_N2_SMS1
        R(2) = R_N2_SMS2
        R(3) = R_N2_SMS3
      END IF
C----------------------
      CALL MY_BARRIER
C---------------------
      RETURN
      END
Chd|====================================================================
Chd|  SMS_MAV_NM                    source/ams/sms_proj.F         
Chd|-- called by -----------
Chd|        SMS_INIX                      source/ams/sms_proj.F         
Chd|        SMS_PRO_P                     source/ams/sms_proj.F         
Chd|-- calls ---------------
Chd|        SMS_PRODUT3                   source/ams/sms_proj.F         
Chd|====================================================================
      SUBROUTINE SMS_MAV_NM(NODFT ,NODLT ,NUMNOD,MD  ,A  ,
     .                      B    ,C    ,WEIGHT,ITASK )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NODFT ,NODLT ,NUMNOD ,MD   ,ITASK ,WEIGHT(*)
C     REAL ou REAL*8
      my_real
     .  A(NUMNOD,*), B(3,*), C(3,*)  
C-----------------------------------------------
c FUNCTION: product {C}=[A]^t{B}
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   NUMNOD,MD                     - Matrix dimension 2D
c  I   WEIGHT(*)                     - itag for each node of subdomains
c  I   NODFT ,NODLT,ITASK            - id in each ITASK:thread id (//)
c  I   A(NUMNOD,MD),B(3,NUMNOD)      - right-hand vector
c  O   C(3,MD)                       - left-hand vector
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
C-----------------------------
      DO I=1,MD
       CALL SMS_PRODUT3(NODFT ,NODLT ,A(1,I) ,B ,WEIGHT ,C(1,I),ITASK)
      ENDDO
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  SMS_MAM_NM                    source/ams/sms_proj.F         
Chd|-- called by -----------
Chd|        SMS_INIST                     source/ams/sms_proj.F         
Chd|-- calls ---------------
Chd|        SMS_PRODUT_H                  source/ams/sms_proj.F         
Chd|====================================================================
      SUBROUTINE SMS_MAM_NM(NODFT ,NODLT ,NUMNOD, MD   ,A   ,
     .                      B   ,C  ,WEIGHT,ITASK)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NODFT ,NODLT ,NUMNOD,MD   ,ITASK,WEIGHT(*)
C     REAL ou REAL*8
      my_real
     .  A(NUMNOD,*), B(NUMNOD,*), C(MD,*)  
C-----------------------------------------------
c FUNCTION: product {C}=[A]^t[B]
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   NUMNOD,MD         - Matrix dimension 2D
c  I   WEIGHT(*)         - itag for each node of subdomains
c  I   NODFT ,NODLT,ITASK- id in each ITASK:thread id (//)
c  I   B(3,NUMNOD,MD)    - right-hand Matrix
c  O   C(NM,MD)          - left-hand vector
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
C-----------------------------
      DO I=1,MD
       DO J=1,MD
        CALL SMS_PRODUT_H( NODFT ,NODLT ,A(1,I)  ,B(1,J) ,WEIGHT,
     .                 C(I,J),ITASK)
       ENDDO
      ENDDO
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  SMS_MORTHO_GS                 source/ams/sms_proj.F         
Chd|-- called by -----------
Chd|        SMS_INISI                     source/ams/sms_proj.F         
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SMS_PRODUT_H                  source/ams/sms_proj.F         
Chd|        VAXPY_H                       source/implicit/produt_v.F    
Chd|        VSCAL_H                       source/implicit/produt_v.F    
Chd|====================================================================
      SUBROUTINE SMS_MORTHO_GS(NODFT  ,NODLT  ,NUMNOD ,MD_F   ,MD_L   ,
     .                     A      ,WEIGHT ,ITASK  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C----------------------------s-------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NUMNOD ,MD_F,MD_L,NODFT ,NODLT    ,WEIGHT(*), ITASK
C     REAL ou REAL*8
      my_real
     .  A(NUMNOD,*)
C-----------------------------------------------
c FUNCTION: stabilized Gram-Schmidt orthonormalization (from MD_F to MD_L)
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   MD_F to MD_L       - vectors to be orthonormalized dim. of A(*,MD) should be MD_L
c  I   WEIGHT(*)          - itag for each node with subdomains
c  IO  A(NUMNOD,MD)       - A(NDDL,MD) orthonormalized for output
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,F_DDL,L_DDL
      my_real
     .  SII,SIJ,S,SJJ
C-----------------------------
       DO J= MD_F ,MD_L 
        DO I=1,J-1
          CALL SMS_PRODUT_H(NODFT ,NODLT ,A(1,I) ,A(1,J) ,WEIGHT, 
     .                      SIJ ,ITASK)
          S = -SIJ
          CALL VAXPY_H(NODFT ,NODLT ,A(1,I) ,A(1,J) ,S  ,ITASK )
C----------------------
          CALL MY_BARRIER
C---------------------
        END DO
        CALL SMS_PRODUT_H(NODFT ,NODLT ,A(1,J) ,A(1,J) ,WEIGHT, 
     .                    SJJ ,ITASK)
        S= ONE/MAX(EM20,SQRT(SJJ))
        CALL VSCAL_H(NODFT ,NODLT ,A(1,J) ,S  ,ITASK )
C----------------------
      CALL MY_BARRIER
C---------------------
       END DO
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  SMS_MAV_MN                    source/ams/sms_proj.F         
Chd|-- called by -----------
Chd|        SMS_INIX                      source/ams/sms_proj.F         
Chd|        SMS_PRO_P                     source/ams/sms_proj.F         
Chd|-- calls ---------------
Chd|        SMS_PRODUT_V_LOC              source/ams/sms_proj.F         
Chd|====================================================================
      SUBROUTINE SMS_MAV_MN(ND   ,MD     ,A     ,B     ,C      ,ITASK )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  ND  ,MD   ,ITASK
C     REAL ou REAL*8
      my_real
     .  A(ND,*), B(3,*), C(3,*)  
C-----------------------------------------------
c FUNCTION: product {C}=[A]{B}
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   ND,MN             - Matrix dimension 2D
c  I   ITASK             - thread id (//)
c  I   B(3,NM)           - right-hand vector
c  O   C(3,ND)           - left-hand vector
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      my_real
     .  W(MD) 
C-----------------------------
      IF (ITASK /= 0) RETURN
C------------may add dynamic smp on ND after--      
      DO I=1,ND
       DO J= 1,MD
        W(J)= A(I,J)
       END DO
       CALL SMS_PRODUT_V_LOC( MD  ,W  ,B  ,C(1,I))
      ENDDO
C--------------------------------------------
      RETURN
      END
C---------------------r={x}^t{y}---
Chd|====================================================================
Chd|  SMS_PRODUT_V_LOC              source/ams/sms_proj.F         
Chd|-- called by -----------
Chd|        SMS_MAV_MN                    source/ams/sms_proj.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SMS_PRODUT_V_LOC( NDDL  ,X   ,Y  ,R)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL  
C     REAL ou REAL*8
      my_real
     .  X(*), Y(3,*)  ,R(3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------
      R(1) = ZERO
      R(2) = ZERO
      R(3) = ZERO
      DO I=1,NDDL
       R(1) = R(1) + X(I)*Y(1,I)
       R(2) = R(2) + X(I)*Y(2,I)
       R(3) = R(3) + X(I)*Y(3,I)
      ENDDO
C--------------------------------------------
      RETURN
      END
