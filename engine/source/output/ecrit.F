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
Chd|  ECRIT                         source/output/ecrit.F         
Chd|-- called by -----------
Chd|        SORTIE_MAIN                   source/output/sortie_main.F   
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ELAPSTIME                     source/system/timer.F         
Chd|        FVSTATS1                      source/airbag/fvstats1.F      
Chd|        MY_FLUSH                      source/system/machine.F       
Chd|        SPMD_EXCH_FVSTATS             source/mpi/airbags/spmd_exch_fvstats.F
Chd|        SPMD_GLOB_DSUM9               source/mpi/interfaces/spmd_th.F
Chd|        SPMD_RBCAST                   source/mpi/generic/spmd_rbcast.F
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        IMP_DYNA                      share/modules/impbufdef_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        STATE_MOD                     ../common_source/modules/state_mod.F
Chd|====================================================================
      SUBROUTINE ECRIT(PARTSAV    ,MS        ,V	       ,IN       ,R          ,
     2                 RBY        ,NPBY      ,DMAS     ,DINER    ,WEIGHT     ,
     3                 A          ,AR        ,FXBIPM   ,FXBRPM   ,MONVOL     ,
     4                 XMOM_SMS   ,SENSOR_TAB,QFRICINT ,IPARI    ,WEIGHT_MD  ,
     5                 TFEXTH     ,IFLAG     ,MS_2D    ,MULTI_FVM,MAS_ND     ,
     6                 KEND       ,H3D_DATA  ,DYNAIN_DATA,USREINT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_DYNA
      USE MESSAGE_MOD
      USE MULTI_FVM_MOD
      USE H3D_MOD
      USE SENSOR_MOD
      USE ANIM_MOD
      USE STATE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr02_c.inc"
#include      "scr06_c.inc"
#include      "scr07_c.inc"
#include      "scr11_c.inc"
#include      "scr16_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "stati_c.inc"
#include      "statr_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "lagmult.inc"
#include      "impl1_c.inc"
#include      "fxbcom.inc"
#include      "timeri_c.inc"
#include      "sms_c.inc"
#include      "rad2r_c.inc"
#include      "inter22.inc"
#include      "itet2_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFLAG
      INTEGER NPBY(NNPBY,*),
     .        WEIGHT(NUMNOD),FXBIPM(NBIPM,*),
     .        IPARI(NPARI,NINTER),WEIGHT_MD(NUMNOD)
      INTEGER MONVOL(*)
      my_real DMAS,DINER,TFEXTH,
     .   PARTSAV(NPSAV,*), MS(NUMNOD), V(3,NUMNOD), A(3,NUMNOD), 
     .   IN(NUMNOD), R(3,NUMNOD), RBY(NRBY,*), AR(3,NUMNOD),FXBRPM(*),
     .   XMOM_SMS(3,*),QFRICINT(*),MS_2D(*),KEND,MAS_ND
      my_real, INTENT(IN) :: USREINT
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
      TYPE(H3D_DATABASE), INTENT(INOUT) :: H3D_DATA
      TYPE (SENSOR_STR_), DIMENSION(NSENSOR) :: SENSOR_TAB
      TYPE (DYNAIN_DATABASE), INTENT(INOUT) :: DYNAIN_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IPRI, INFO, I, J, M, JPRI, ILIGN, K, ITHIS, ADRRPM, ISENS,
     .         NTY,INTHE,IABFIS,SENS_ID,SENS_TYPE
      my_real
     .   ENTOT, ENTOT1, ERR, ERR1, X99, EMASS,MAS,
     .   VX,VY,VZ,DT05,ENTMP(12)  ,RTMP(10),
     .   MVX, MVY, MVZ, TS, MAS2, WEWE2, ENTOT1B,DMASND
      DOUBLE PRECISION ETIME, RETIME, TT0,
     .                 ENCIND, XMOMTD, YMOMTD, ZMOMTD,
     .                 XMASSD, ENROTD, ENINTD, ENCIND2,
     .                 ENROTD2, ENTOTB, EAMSD 

      DATA X99/99.9/
      DATA TT0/-1./
      CHARACTER ELTYP(0:105)*5
C-----------------------------------------------
      DATA ELTYP/'FIXED',
     1           'SOLID','QUAD ','SHELL','TRUSS','BEAM ',
     2           'SPRIN','SH_3N','TRIA ','AIRBA','INTER',
     3           'NODE ','BLAST','     ','     ','     ',
     4           '     ','     ','     ','     ','     ',
     5           '     ','     ','     ','     ','     ',
     6           '     ','     ','     ','     ','     ',
     7           '     ','     ','     ','     ','     ',
     8           '     ','     ','     ','     ','     ',
     9           '     ','     ','     ','     ','     ',
     A           '     ','     ','     ','     ','RENAU',
     B           'SPCEL','FVBAG','     ','     ','     ',
     C           '     ','     ','     ','     ','     ',
     D           '     ','     ','     ','     ','     ',
     E           '     ','     ','     ','     ','     ',
     F           '     ','     ','     ','     ','     ',
     G           '     ','     ','     ','     ','     ',
     H           '     ','     ','     ','     ','     ',
     I           '     ','     ','     ','     ','     ',
     J           '     ','     ','     ','     ','     ',
     K           '     ','     ','     ','     ','XELEM',
     K           'IGE3D','     ','     ','     ','     '/
      DATA ILIGN/55/
C=======================================================================
      IPRI=1
      IFLAG =0
      IF(TT0==-ONE)TT0=TT
      IF(T1S==TT)IPRI=MOD(NCYCLE,IABS(NCPRI))
      INFO=MDESS-MANIM
      ITHIS=0
      IABFIS=0
      IF(TT<THIS)ITHIS=1
      IF(TT<TABFIS(1))IABFIS=1
C--------Multidomains : control of time history for subdomains-----------
      IF ((IRAD2R==1).AND.(R2R_SIU==1).AND.(IDDOM/=0)) THEN
        ITHIS=1
        DO I=1,10
          IF (R2R_TH_MAIN(I)>0) ITHIS=0
        ENDDO
      ENDIF
C get and reset elapsed time
      IF(IMON > 0) CALL ELAPSTIME(ETIME)
      IF(IPRI/=0.AND.ITHIS/=0.AND.
     .   INFO<=0.AND.ISTAT==0
     .   .AND.NTH==0.AND.NANIM==0 .AND.
     .    (IABFIS/=0.OR.ABFILE(1)==0) ) RETURN
C
C     initialization / see corrections rbodies...
      EAMS=ENCIN 
C
C GLOBAL VAR INITIALISES DANS RESOL ET MODIFIE DANS RGBCOR + passage en DOUBLE pour cumul
      ENCIND  = ZERO
      ENROTD  = ZERO
      ENINTD  = ZERO
      XMASSD  = ZERO
      XMOMTD  = ZERO
      YMOMTD  = ZERO
      ZMOMTD  = ZERO
      ENCIND2 = ZERO
      ENROTD2 = ZERO 
      TFEXTH  = ZERO
      EAMSD   = ZERO
C
      DT05=HALF*DT1
C
      IFLAG =1
      IF(N2D == 0 .AND. .NOT. MULTI_FVM%IS_USED) THEN
       IF (IMPL_S==1) THEN
        IF (IDYNA>0) THEN
         DT05=(ONE-DY_G)*DT1
         DO I = 1, NUMNOD
          MAS=MS(I)*WEIGHT_MD(I)
          VX = DY_V(1,I) - DT05*DY_A(1,I)
          VY = DY_V(2,I) - DT05*DY_A(2,I)
          VZ = DY_V(3,I) - DT05*DY_A(3,I)
          ENCIND=ENCIND + ( VX*VX + VY*VY + VZ*VZ)*HALF*MAS
          XMOMTD=XMOMTD+VX*MAS
          YMOMTD=YMOMTD+VY*MAS
          ZMOMTD=ZMOMTD+VZ*MAS
          XMASSD=XMASSD+MAS
          MAS2=MS(I)*(1-WEIGHT_MD(I))*WEIGHT(I)
          ENCIND2=ENCIND2 + ( VX*VX + VY*VY + VZ*VZ)*HALF*MAS2         
         ENDDO
        ELSE
         DO I = 1, NUMNOD
          XMASSD=XMASSD+MS(I)*WEIGHT_MD(I)
         ENDDO
        ENDIF 
       ELSEIF(IDTMINS==0.AND.IDTMINS_INT==0)THEN
C
         DO I = 1, NUMNOD
           MAS=MS(I)*WEIGHT_MD(I)
           VX = V(1,I) + DT05*A(1,I)
           VY = V(2,I) + DT05*A(2,I)
           VZ = V(3,I) + DT05*A(3,I)
           ENCIND=ENCIND + ( VX*VX + VY*VY + VZ*VZ)*HALF*MAS
           XMOMTD=XMOMTD+VX*MAS
           YMOMTD=YMOMTD+VY*MAS
           ZMOMTD=ZMOMTD+VZ*MAS
           XMASSD=XMASSD+MAS
           MAS2=MS(I)*(1-WEIGHT_MD(I))*WEIGHT(I)
           ENCIND2=ENCIND2 + ( VX*VX + VY*VY + VZ*VZ)*HALF*MAS2            
         ENDDO
C
       ELSE
C------  sms
         DO I = 1, NUMNOD
           MAS=MS(I)*WEIGHT_MD(I)
           VX = V(1,I) + DT05*A(1,I)
           VY = V(2,I) + DT05*A(2,I)
           VZ = V(3,I) + DT05*A(3,I)
           MVX=XMOM_SMS(1,I)*WEIGHT_MD(I)
           MVY=XMOM_SMS(2,I)*WEIGHT_MD(I)
           MVZ=XMOM_SMS(3,I)*WEIGHT_MD(I)
           ENCIND=ENCIND + ( VX*MVX + VY*MVY + VZ*MVZ)*HALF
           EAMSD=EAMSD + ( VX*VX + VY*VY + VZ*VZ)*HALF*MAS
           XMOMTD=XMOMTD+MVX
           YMOMTD=YMOMTD+MVY
           ZMOMTD=ZMOMTD+MVZ
           XMASSD=XMASSD+MAS
           MAS2=MS(I)*(1-WEIGHT_MD(I))*WEIGHT(I)
           ENCIND2=ENCIND2 + ( VX*VX + VY*VY + VZ*VZ)*HALF*MAS2            
         ENDDO
       ENDIF 
C
C       ENCIND=0.5*ENCIND

      ELSE IF (MULTI_FVM%IS_USED) THEN
       DO 25 M=1,NPART
       ENCIND = ENCIND + PARTSAV(2,M)
       XMASSD = XMASSD + PARTSAV(6,M)
       XMOMTD = XMOMTD + PARTSAV(3,M)
       YMOMTD = YMOMTD + PARTSAV(4,M)
       ZMOMTD = ZMOMTD + PARTSAV(5,M)
   25  CONTINUE

      ELSE
         DO I = 1, NUMNOD
           MAS=MS_2D(I)*WEIGHT_MD(I)
           VX = V(1,I) + DT05*A(1,I)
           VY = V(2,I) + DT05*A(2,I)
           VZ = V(3,I) + DT05*A(3,I)
           ENCIND=ENCIND + ( VX*VX + VY*VY + VZ*VZ)*HALF*MAS
           XMOMTD=XMOMTD+VX*MAS
           YMOMTD=YMOMTD+VY*MAS
           ZMOMTD=ZMOMTD+VZ*MAS
           XMASSD=XMASSD+MAS
           MAS2=MS_2D(I)*(1-WEIGHT_MD(I))*WEIGHT(I)
           ENCIND2=ENCIND2 + ( VX*VX + VY*VY + VZ*VZ)*HALF*MAS2     
         ENDDO
      ENDIF 

      IF (INT22>0) THEN
       !FVM cells take part in the balance
       DO M=1,NPART
        ENCIND  = ENCIND + PARTSAV(2,M)
        XMASSD  = XMASSD + PARTSAV(6,M)
        XMOMTD  = XMOMTD + PARTSAV(3,M)
        YMOMTD  = YMOMTD + PARTSAV(4,M)
        ZMOMTD  = ZMOMTD + PARTSAV(5,M)
        IF(PARTSAV(6,M)>ZERO)   ENCIND2 = ENCIND2 + HALF/PARTSAV(6,M)*(PARTSAV(3,M)**2+PARTSAV(4,M)**2+PARTSAV(5,M)**2)
       ENDDO
      ENDIF
       
      IF (NS10E>0) THEN
C-------- MS_ND= MAS_ND0    
        ENCIND = ENCIND + KEND
        XMASSD = XMASSD -MAS_ND 
        DMASND = MAX(ZERO,(MAS_ND-MS_ND))
        IF (DMASND>MS_ND*EM10) DMAS  = DMAS -DMASND
C--------DMAS,DMASND are used only at Ncycle=0, update MS_ND for restart
        MS_ND = MAS_ND        
      ENDIF
C
      IF(IRODDL/=0)THEN
       IF (IMPL_S==1) THEN
        IF (IDYNA>0) THEN
         DO I = 1, NUMNOD
          VX = DY_VR(1,I) - DT05*DY_AR(1,I)
          VY = DY_VR(2,I) - DT05*DY_AR(2,I)
          VZ = DY_VR(3,I) - DT05*DY_AR(3,I)
          ENROTD=ENROTD 
     .        + (VX*VX + VY*VY + VZ*VZ)*HALF*IN(I)*WEIGHT_MD(I)
          WEWE2 = (1-WEIGHT_MD(I))*WEIGHT(I)
          ENROTD2=ENROTD2 
     .        + (VX*VX + VY*VY + VZ*VZ)*HALF*IN(I)*WEWE2    
         ENDDO
        ENDIF 
       ELSE
         DO I = 1, NUMNOD
          VX = R(1,I) + DT05*AR(1,I)
          VY = R(2,I) + DT05*AR(2,I)
          VZ = R(3,I) + DT05*AR(3,I)
          ENROTD=ENROTD 
     .         + (VX*VX + VY*VY + VZ*VZ)*HALF*IN(I)*WEIGHT_MD(I)
          WEWE2 = (1-WEIGHT_MD(I))*WEIGHT(I)     
          ENROTD2=ENROTD2 
     .        + (VX*VX + VY*VY + VZ*VZ)*HALF*IN(I)*WEWE2      
         ENDDO
       ENDIF 
C       ENROTD=0.5*ENROTD
      ENDIF
C
      ENINTD = EPOR + USREINT + (DAMPW+EDAMP)*DT05
      DAMPW = EDAMP
      IF (NFXBODY>0) THEN
         DO I=1,NFXBODY
            ADRRPM=FXBIPM(14,I)
            ENINTD=ENINTD+FXBRPM(ADRRPM+10)-FXBRPM(ADRRPM+14)
            ENCIND=ENCIND+FXBRPM(ADRRPM+11)
         ENDDO
      ENDIF
      DO 30 M=1,NPART
      ENROTD= ENROTD + PARTSAV(7,M)
      ENINTD = ENINTD + PARTSAV(1,M) + PARTSAV(24,M)
     .                             + PARTSAV(26,M)
      TFEXT  = TFEXT +PARTSAV(27,M)
      TFEXTH = TFEXTH+PARTSAV(27,M)
   30 CONTINUE
C
C  Add Heat generated by Friction to internal energy
C
      DO I=1,NINTER
         NTY   =IPARI(7,I)
         IF(NTY==7.OR.NTY==21) THEN
            INTHE   =IPARI(47,I)  
            IF (INTHE>0) THEN   
              ENINTD = ENINTD + QFRICINT(I)
            ENDIF
         ENDIF
      ENDDO   
C
C add contribution in DP to my_real var to keep precision
C
      ENCIN=ENCIN+ENCIND    
      ENCIN2=ENCIN2+ENCIND2
      ENROT=ENROT+ENROTD
      ENROT2=ENROT2+ENROTD2
      EAMS  =EAMS+EAMSD  
C             
C      ENCIN = ENCIN + ENCIN2       
C      ENROT = ENROT + ENROT2
C       
      ENINT=ENINTD
      XMOMT=XMOMT+XMOMTD
      YMOMT=YMOMT+YMOMTD
      ZMOMT=ZMOMT+ZMOMTD
      XMASS=XMASS+XMASSD
C
      IF (IMPL_S==1) THEN
       IF (IDYNA==0) THEN
        ENCIN =ZERO
        ENROT =ZERO
        ENCIN2 =ZERO
        ENROT2 =ZERO        
        TFEXT=ENINT
       ELSEIF (IDY_DAMP>0) THEN
C        IF (NSPMD>1) CALL SPMD_SUM_S(DY_EDAMP)
        ENINT = ENINT + DY_EDAMP
       ENDIF
      ENDIF
C
      IF (NSPMD > 1) THEN
C.....envoyer la contribution au proc 0
C.....sommer les contributions puis standard
        ENTMP(1) = ENCIN
        ENTMP(2) = ENROT
        ENTMP(3) = ENINT
        ENTMP(4) = XMOMT
        ENTMP(5) = YMOMT
        ENTMP(6) = ZMOMT
        ENTMP(7) = XMASS
        ENTMP(8) = ECONT
        ENTMP(9) = REINT
        ENTMP(10) = ENCIN2
        ENTMP(11) = ENROT2                
        ENTMP(12) = EAMS    
        CALL SPMD_GLOB_DSUM9(ENTMP,12)
        IF (ISPMD==0) THEN
          ENCIN = ENTMP(1)
          ENROT = ENTMP(2)
          ENINT = ENTMP(3)
          XMOMT = ENTMP(4)
          YMOMT = ENTMP(5)
          ZMOMT = ENTMP(6)
          XMASS = ENTMP(7)
          ECONT = ENTMP(8)
          REINT = ENTMP(9)
          ENCIN2 = ENTMP(10)
          ENROT2 = ENTMP(11) 
          EAMS   = ENTMP(12) 
        ENDIF
C
        IF (ISTAT==2) THEN
C istat=2 => relaxation : broadcast de encin et enrot
          CALL SPMD_RBCAST(ENTMP,ENTMP,1,2,0,2)
          IF (ISPMD/=0) THEN
            ENCIN = ENTMP(1)
            ENROT = ENTMP(2)
            ENCIN2 = ENTMP(10)
            ENROT2 = ENTMP(11)            
          ENDIF
        ELSEIF (ISTAT==3) THEN
C istat=3 => ADYREL : broadcast de encin et enrot ENINT
          CALL SPMD_RBCAST(ENTMP,ENTMP,1,3,0,3)
          IF (ISPMD/=0) THEN
            ENCIN = ENTMP(1)
            ENROT = ENTMP(2)
            ENINT = ENTMP(3)
          ENDIF
        ENDIF
C
        ENTMP(1) = TFEXT
        ENTMP(2) = EHOUR
        ENTMP(3) = ECONTV
        ENTMP(4) = DMAS
        ENTMP(5) = TFEXTH
        ENTMP(6) = ECONTD   
        ENTMP(7) = ECONT_CUMU                             
        CALL SPMD_GLOB_DSUM9(ENTMP,7)
        IF(ISPMD/=0) THEN
          TFEXT = ZERO
          EHOUR = ZERO
          ECONTV = ZERO
          DMAS = ZERO  
          TFEXTH = ZERO
          ECONTD = ZERO
          ECONT_CUMU = ZERO
        ELSE
          TFEXT = ENTMP(1)
          EHOUR = ENTMP(2)
          ECONTV= ENTMP(3)
          DMAS  = ENTMP(4)
          TFEXTH= ENTMP(5)
          ECONTD = ENTMP(6)    
          ECONT_CUMU = ENTMP(7)                
        ENDIF
      ENDIF
C
C     EAMS = [ 1/2 v.Mv - 1/2 m v^2 ]/ 1/2 m v^2   
      IF(ISPMD==0) THEN
        IF(IDTMINS/=0.OR.IDTMINS_INT/=0)THEN
          IF(EAMS > EM20)THEN
            EAMS = (ENCIN-EAMS)/EAMS
          ELSE
            EAMS = ZERO
          END IF
        END IF
      END IF
C
C     ENTOTB =
C     ENCIN + ENINT + ENROT + ENCIN2 + ENROT2
C     ENTOT1B =
C     (NC=0,ENCIN + ENINT + ENROT - TFEXT - TFEXT_MD) 
C     + DEF + TFEXT + DELTAE + TFEXT_MD       

      IF(ISPMD==0) THEN
       ENTOT = ENCIN + ENINT + ENROT
       ENTOTB = ENTOT + ENCIN2 + ENROT2       
       IF(NCYCLE==0) THEN
        ENTOT0=ENTOT - TFEXT - TFEXT_MD
        DELTAE=ENCIN2 + ENROT2                
        MASS0 = XMASS - DMAS
       ENDIF
       MASS0 = MASS0  + DMF
       ENTOT0= ENTOT0 + DEF       
       ENTOT1=ENTOT0 + TFEXT
       ENTOT1B=ENTOT0 + TFEXT + DELTAE + TFEXT_MD       
       IF(ABS(ENTOT1B)>EM20)THEN
         ERR = ENTOTB/ENTOT1B - ONE
         ERR1 = MAX(-X99, MIN(X99,ERR*HUNDRED))     
       ELSE
        ERR = ZERO
        ERR1 =ZERO
       ENDIF
       EMASS = (XMASS - MASS0) / MAX(MASS0,EM20)
C-----------------------------------------------
C       /STATE/LSENSOR
C-----------------------------------------------
        IF (NSTATSENS  > 0) THEN
          MSTATT = 0
          DO I=1,NSTATSENS
             ISENS = LSTATSENS(I)
             TS = SENSOR_TAB(ISENS)%TSTART      
             IF (TT >= TS) THEN
               MSTAT(I) = MSTAT(I)+1    
             ENDIF
             IF (MSTAT(I)==1) MSTATT=1
          ENDDO
        ENDIF
C-----------------------------------------------
C       /OUTP/LSENSOR
C-----------------------------------------------
        IF (NOUTPSENS > 0) THEN
          MOUTPT = 0
          DO I=1,NOUTPSENS
             ISENS = LOUTPSENS(I)
             TS = SENSOR_TAB(ISENS)%TSTART     
             IF (TT >= TS) THEN
                MOUTP(I) = MOUTP(I)+1    
             ENDIF
             IF(MOUTP(I)==1)  MOUTPT=1
          ENDDO
        ENDIF
C-----------------------------------------------
C       STOP OR KILL 
C-----------------------------------------------
C       /STOP/LSENSOR
C-----------------------------------------------
        IF (NSTOPSENS > 0) THEN
          DO I=1,NSTOPSENS
             ISENS = LSTOPSENS(I)
             SENS_TYPE = SENSOR_TAB(ISENS)%TYPE
             TS = EP20
             SENS_ID = SENSOR_TAB(ISENS)%SENS_ID  
             IF (SENSOR_TAB(ISENS)%STATUS == 1) TS = TT
             IF (TT >= TS) THEN
                CALL ANCMSG(MSGID=234,ANMODE=ANINFO,
     .                      I1 = SENS_ID)
                IWARN=IWARN+1
                MSTOP=1
                MREST=1
                IF(NSTH/=0)THEN  !IF THIST FILE
                   THIS= TT
                   IPRI= 0
                ENDIF
                IF(NSABF/=0)THEN  !IF ABF FILE
                   DO J=1,10 
                     IF(ABFILE(J)/=0) THEN
                        TABFIS(J) = TT
                        TABFWR(J) = TT
                     ENDIF
                   ENDDO
                ENDIF
                IF(NSANIM/=0)THEN !IF ANIM FILE 
                   MDESS = 1
                   TANIM = TT
                   IPRI  = 0
                ENDIF 
                IF(NSSTAT/=0)THEN !IF STAT FILE 
                   TSTAT = TT
                   IPRI  = 0
                ENDIF 
                IF(NSOUTP/=0)THEN !IF OUTP FILE 
                  TOUTP = TT
                  IPRI  = 0
               ENDIF 
                IF(NSH3D/=0)THEN !IF H3D FILE 
                   H3D_DATA%TH3D = TT
                   IPRI  = 0
                ENDIF 
                IF(NSDYNANIN/=0)THEN !IF DYNAIN FILE 
                   DYNAIN_DATA%TDYNAIN = TT
                   IPRI  = 0
                ENDIF 
            ENDIF
          ENDDO
       ENDIF
C
       IF((NERR_POSIT==0.AND.ABS(ERR)>DEMXK).OR.
     .    (NERR_POSIT==1.AND.ERR>DEMXK))THEN
         CALL ANCMSG(MSGID=205,ANMODE=ANINFO)
         IERR=IERR+1
         MSTOP=1
         IF(NTH/=0)THEN
          THIS= TT
          IPRI= 0
         ENDIF
         IF(NANIM/=0)THEN
          MDESS = 1
          TANIM = TT
          IPRI  = 0
         ENDIF                 
       ELSEIF((NERR_POSIT==0.AND.ABS(ERR)>DEMXS).OR.
     .        (NERR_POSIT==1.AND.ERR>DEMXS))THEN
         CALL ANCMSG(MSGID=206,ANMODE=ANINFO)
         IWARN=IWARN+1
         MSTOP=1
         MREST=1
         IF(NTH/=0)THEN
          THIS= TT
          IPRI= 0
         ENDIF
         IF(NANIM/=0)THEN
          MDESS = 1
          TANIM = TT
          IPRI  = 0
         ENDIF 
       ENDIF
C
       IF(EMASS>DMTMXK)THEN
         CALL ANCMSG(MSGID=207,ANMODE=ANINFO)
         IERR=IERR+1
         MSTOP=1
         IF(NTH/=0)THEN
          THIS= TT
          IPRI = 0
         ENDIF
         IF(NANIM/=0)THEN
          MDESS = 1
          TANIM = TT
          IPRI  = 0
         ENDIF 
       ELSEIF(EMASS>DMTMXS)THEN
         CALL ANCMSG(MSGID=208,ANMODE=ANINFO)
         IWARN=IWARN+1
         MSTOP=1
         MREST=1
         IF(NTH/=0)THEN
          THIS= TT
          IPRI = 0
         ENDIF
         IF(NANIM/=0)THEN
          MDESS = 1
          TANIM = TT
          IPRI  = 0
         ENDIF 
       ENDIF
      ENDIF
C
C     Communication MSTOP & MREST
C
      IF (NSPMD > 1) THEN
        IF (ISPMD==0) THEN
          RTMP(1)  = MSTOP
          RTMP(2)  = MREST
          RTMP(3)  = MDESS
          RTMP(4)  = TANIM
          RTMP(5)  = THIS
          RTMP(6)  = TSTAT
          RTMP(7)  = TOUTP
          RTMP(8)  = INFO
          RTMP(9)  = H3D_DATA%TH3D
          RTMP(10) = DYNAIN_DATA%TDYNAIN
        ENDIF
C
        CALL SPMD_RBCAST(RTMP,RTMP,10,1,0,2)
        IF (ISPMD/=0) THEN
          MSTOP = NINT(RTMP(1))
          MREST = NINT(RTMP(2))
          MDESS = NINT(RTMP(3))
          TANIM = RTMP(4)
          THIS =  RTMP(5)
          TSTAT = RTMP(6)
          TOUTP = RTMP(7)
          H3D_DATA%TH3D = RTMP(9)
          DYNAIN_DATA%TDYNAIN = RTMP(10)
        ENDIF

        IF(INFO > 0) CALL SPMD_EXCH_FVSTATS(MONVOL)

        IF(ISPMD/=0) RETURN
        ! Only processor 0 will continue

      ENDIF

C-----------------------------------------------
      IF(IPRI==0)THEN
        IF (NLPRI /= 0) ILIGN = NLPRI
        JPRI=MOD(NCYCLE,ILIGN*IABS(NCPRI))
C        IF(ITYPTS==0)ITYPTS=3
        IF(JPRI==0) WRITE(IOUT,1000)
        WRITE(IOUT,1100) NCYCLE,TT,DT2,ELTYP(ITYPTS),NELTS,
     +        ERR1,ENINT,ENCIN,ENROT,TFEXT,EMASS,XMASS,XMASS-MASS0
!             WRITE(IOUT,'(A)')
!      +        '---------------------------------'
!             WRITE(IOUT,'(A,F14.2,A,F14.2)')
!      +        'ENTOTB',ENTOTB,' ENTOT1B',ENTOT1B
!           IF(NCYCLE==0) THEN
!             WRITE(IOUT,'(A)')
!      +        '---------------------------------------------'
!             WRITE(IOUT,'(A,F14.2,A,F14.2,A,F14.2)')
!      +        'ENCIN0',ENCIN,' ENINT0',ENINT,' ENROT0',ENROT
!             WRITE(IOUT,'(A,F14.2,A,F14.2)')
!      +        'ENCIN20',ENCIN2,' ENROT20',ENROT2
!             WRITE(IOUT,'(A,F14.2,A,F14.2)')
!      +        'TFEXT0',TFEXT,' TFEXT_MD0',TFEXT_MD
!             WRITE(IOUT,'(A,F14.2,A,F14.2)')
!      +        'DELTAE0',DELTAE,' DEF0',DEF
!           END IF
!             WRITE(IOUT,'(A)')
!      +        '---------------------------------------------'
!           WRITE(IOUT,'(A,F14.2,A,F14.2,A,F14.2)')
!      +        'ENCIN',ENCIN,' ENINT',ENINT,' ENROT',ENROT
!           WRITE(IOUT,'(A,F14.2,A,F14.2,A,F14.2)')
!      +        'ENCIN2',ENCIN2,' ENROT2',ENROT2
!           WRITE(IOUT,'(A,F14.2,A,F14.2,A,F14.2)')
!      +        'TFEXT',TFEXT,' TFEXT_MD',TFEXT_MD,' DEF',DEF
!             WRITE(IOUT,'(A)')
!      +        '---------------------------------------------'
        CALL MY_FLUSH(IOUT)
        IF(NCPRI<0) THEN
          IF(DEBUG(10)/=0)THEN
            IF(NCYCLE>=DEBUG(10))THEN
              write (*,*) "      ALE ADVECTION SET OFF"
            ENDIF
          ENDIF            
          WRITE(ISTDO,'(A,I8,2(A,1PE11.4),A,0PF5.1,A,1PE11.4)')
     .   ' NC=',NCYCLE,' T=',TT,' DT=',DT2,' ERR=',ERR1,'% DM/M=',EMASS 
          IF(LAG_NC>0) THEN
            WRITE(ISTDO,'(2(A,I8),A,1PE11.4)')
     .      '     LAG_NC=',LAG_NC,',   NITER_GC=',NITER_GC,
     .      ',   LAG_ERSQ2=',LAG_ERSQ2
          ENDIF
          IF(IMON > 0 .AND. TT-TT0 > ZERO) THEN
C calcul temps restant
            RETIME = (ETIME*(TSTOP-TT0)) / (TT-TT0) - ETIME
            WRITE(ISTDO,'(A,F14.2,A,A,F14.2,A)')
     .        ' ELAPSED TIME=',ETIME,' s ',
     .        ' REMAINING TIME=',RETIME,' s'
          END IF 
          CALL MY_FLUSH(ISTDO)
        ENDIF
      ENDIF
C
      IF(INFO>0)THEN
       WRITE (IUSC3,'(//,A)',ERR=990)        ' CURRENT STATE:'
       WRITE (IUSC3,'(A,/)',ERR=990)         ' --------------'
       WRITE (IUSC3,'(A,I10)',ERR=990)       ' CYCLE      =',NCYCLE
       WRITE (IUSC3,'(A,G14.7)',ERR=990)     ' TIME       =',TT
       WRITE (IUSC3,'(A,G14.7,A,I8)',ERR=990)' TIME STEP  =',DT2,ELTYP(ITYPTS),NELTS
       WRITE (IUSC3,'(A,F5.1,A)',ERR=990)    ' ENERGY ERROR      =',ERR1,'%'
       WRITE (IUSC3,'(A,G14.7)',ERR=990)     ' INTERNAL ENERGY   =',ENINT
       WRITE (IUSC3,'(A,G14.7)',ERR=990)     ' KINETIC ENERGY    =',ENCIN
       WRITE (IUSC3,'(A,G14.7)',ERR=990)     ' ROT. KIN. ENERGY  =',ENROT
       WRITE (IUSC3,'(A,G14.7)',ERR=990)     ' EXTERNAL WORK     =',TFEXT
       WRITE (IUSC3,'(A,G14.7)',ERR=990)     ' MASS.ERR (M-M0)/M0=',EMASS

       CALL FVSTATS1(IUSC3,MONVOL,1)
	   
       IF(IMON > 0 .AND. TT > ZERO) THEN
C calcul temps restant
         RETIME = (ETIME*TSTOP) / TT - ETIME
         WRITE(IUSC3,'(A)',ERR=990)        ' '
         WRITE(IUSC3,'(A,F14.2,A)',ERR=990)' CURRENT ELAPSED TIME    =',ETIME,' s '
         WRITE(IUSC3,'(A,F14.2,A)',ERR=990)' REMAINING TIME ESTIMATE =',RETIME,' s'
       END IF 

       CLOSE(IUSC3)
 990   CONTINUE
      ENDIF
C----------------
C     FORMATS
C----------------
 1000 FORMAT('   CYCLE    TIME      TIME-STEP  ELEMENT          ',
     +       'ERROR  I-ENERGY    K-ENERGY T  K-ENERGY R  ',
     +       'EXT-WORK     MAS.ERR     TOTAL MASS  MASS ADDED')
 1050 FORMAT(1H1,'  CYCLE',4X,'TIME',6X,'TIME-STEP',2X,'ELEMENT',5X,
     +       'K-ENERGY R',2X,'K-ENERGY T',4X,'I-ENERGY',4X,'ERROR',3X,
     +       'MASS',7X,'X-MOMENT',4X,'Y-MOMENT',4X,'Z-MOMENT' )
 1100 FORMAT(I8,2(1X,G11.4),1X,A5,1X,I10,1X,F5.1,1H%,7(1X,G11.4))
 1200 FORMAT(' TIME=',G10.4,
     +          'I-ENERGY   ','K-ENERGY   ','MASS       ',
     +          'X-MOMENT   ','Y-MOMENT   ','Z-MOMENT   '  )
C
      RETURN
      END
