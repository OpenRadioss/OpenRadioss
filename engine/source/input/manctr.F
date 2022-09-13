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
Chd|  MANCTR                        source/input/manctr.F         
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        REDKEY2                       source/input/redkey2.F        
Chd|        SPMD_IBCAST                   source/mpi/generic/spmd_ibcast.F
Chd|        SPMD_RBCAST                   source/mpi/generic/spmd_rbcast.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|====================================================================
      SUBROUTINE MANCTR(SENSOR,H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD         
      USE INOUTFILE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "scr06_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "chara_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "scr05_c.inc"
#include      "check.inc"
#include      "rad2r_c.inc"
#include      "nchara_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .     SENSOR(NSENSR,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   P a r a m e t e r s
C-----------------------------------------------
      INTEGER NKEY,I
      PARAMETER (NKEY = 13)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,MANIM0, MREST0, MSTOP0, MFILR, MCYCLE, LK2, IKEY,
     .   WMCHECK0, MSG_R2R, R2R_MSTOP_LOC,ICTLSTOP0, MH3D0
C     REAL
      my_real
     .   TMPMAN(50)
      my_real
     .   TMAN,TMAN_LOC
      CHARACTER FILNAM*100, CARTE*ncharline, KEY2*80, FMT*80, 
     . KEY0(NKEY)*5,REC*1
      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=2148) :: TMP_NAME

C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C     REAL
C-----------------------------------------------
      SAVE MANIM0,MREST0,MSTOP0,MFILR,KEY0,ICTLSTOP0,MH3D0
      DATA MFILR/0/
      DATA MANIM0/0/
      DATA MREST0/0/
      DATA MSTOP0/0/
      DATA MCYCLE/0/
      DATA TMAN/0.0/
      DATA WMCHECK0/0/
      DATA ICTLSTOP0/0/
      DATA MH3D0/0/
      DATA KEY0/'TIME ','CYCLE','GFILE','PATRA','RFILE','STOP ',
     .          'KILL ','INFO ','ANIM ','OUTP ','LFLUS','CHKPT',
     .          'H3D  '/  
C-----------------------------
C     INTERVENTION MANUELLE
C-----------------------------
      MDESS=0
      MREST=0
      MANIM=0
      MSTOP=0
      WMCHECK = 0
      H3D_DATA%MH3D=0
      MH3D0=0

      MSG_R2R = 0
      IF ((IRAD2R==1).AND.(NCYCLE<2)) R2R_MFILR = 0   
C
      IF(IALESUB/=0 .AND. TT+DT2<=T1S+DT2S)RETURN
C SOUS CYCLAGE ALE
C-----------------------------
C     SENSOR 
C-----------------------------
       IF(IANISENS>0)THEN
        IF(TT>SENSOR(2,IANISENS))THEN
C         ACTIVATION
          IANISENS=-IANISENS
          TANIMSENS=TT+DTANISENS
          MANIM=4
          MDESS=MDESS+MANIM
        ENDIF
       ELSEIF(IANISENS<0)THEN
        IF(TT<SENSOR(2,-IANISENS))THEN
C         DEACTIVATION
          IANISENS=-IANISENS
        ELSEIF(TT>TANIMSENS)THEN
C         ACTIF
          TANIMSENS=TANIMSENS+DTANISENS
          MANIM=4
          MDESS=MDESS+MANIM
          RETURN
        ENDIF
       ENDIF
       IF(NLSENS>0)THEN
         DO I=1,NLSENS
           IF(LANISENS(I)>0)THEN
             IF(TT>SENSOR(2,LANISENS(I)))THEN
C           ACTIVATION
               LANISENS(I)=-LANISENS(I)
               MANIM=4
               MDESS=MDESS+MANIM
             ENDIF
           ELSEIF(LANISENS(I)<0)THEN
             IF(TT<SENSOR(2,-LANISENS(I)))THEN
C           DEACTIVATION
               LANISENS(I)=-LANISENS(I)
             ENDIF
           ENDIF
         ENDDO
       ENDIF
       IF(H3D_DATA%N_SENS_H3D>0)THEN
         DO I=1,H3D_DATA%N_SENS_H3D
           IF(H3D_DATA%LSENS_H3D(I)>0)THEN
             IF(TT>SENSOR(2,H3D_DATA%LSENS_H3D(I)))THEN
C           ACTIVATION
               H3D_DATA%LSENS_H3D(I)=-H3D_DATA%LSENS_H3D(I)
               H3D_DATA%MH3D=1
             ENDIF
           ELSEIF(H3D_DATA%LSENS_H3D(I)<0)THEN
             IF(TT<SENSOR(2,-H3D_DATA%LSENS_H3D(I)))THEN
C           DEACTIVATION
               H3D_DATA%LSENS_H3D(I)=-H3D_DATA%LSENS_H3D(I)
             ENDIF
           ENDIF
         ENDDO
       ENDIF
C-----------------------------
C     INTERVENTION MANUELLE
C-----------------------------
C
      IF((NCYCLE/NCINP)*NCINP==NCYCLE)THEN
       IF(ISPMD==0)THEN
        FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'.ctl'
        LEN_TMP_NAME = INFILE_NAME_LEN + LEN_TRIM(FILNAM)
        TMP_NAME=INFILE_NAME(1:INFILE_NAME_LEN)//FILNAM(1:LEN_TRIM(FILNAM))         
        OPEN(UNIT=IUSC3,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .       STATUS='OLD',ERR=999)
        READ(IUSC3,'(A)',ERR=990,END=990)CARTE
        IF(CARTE(1:1)/='/')GOTO 990
         MREST=0
         MANIM=0
         MSTOP=0
         MREST0=0
         MANIM0=0
         MSTOP0=0
         TMAN=0.
         MCYCLE=0
         ICTLSTOP=0
         ICTLSTOP0=0
         MH3D0 = 0
C
 5       CALL REDKEY2(KEY2,LK2,KEY0,NKEY,CARTE,IKEY)
          GOTO (10,20,30,40,50,60,70,80,90,95,96,97,98)IKEY
          GOTO 100
C TIME
 10       IF(LK2>1.AND.LK2<10)THEN
           WRITE(FMT,'(A,I1,A)')'(E',LK2,'.0)'
          ELSEIF(LK2<100)THEN
           WRITE(FMT,'(A,I2,A)')'(E',LK2,'.0)'
          ELSE
           GOTO 100
          ENDIF
          READ(KEY2,FMT,ERR=990,END=990)TMAN
          GOTO 100
C CYCLE
 20       IF(LK2>1.AND.LK2<10)THEN
           WRITE(FMT,'(A,I1,A)')'(I',LK2,')'
          ELSEIF(LK2<100)THEN
           WRITE(FMT,'(A,I2,A)')'(I',LK2,')'
          ELSE
           GOTO 100
          ENDIF  
          READ(KEY2,FMT,ERR=990,END=990)MCYCLE
C MCYCLE DESATIVE EN MULTIDOMAINES	  
	  IF (IRAD2R==1) THEN
	    MSG_R2R = 1
	    MCYCLE = 0
	  ENDIF 	  
          GOTO 100
C PLOT
 30       MANIM0=MANIM0+1
          GOTO 100
C PATRAN
 40       MANIM0=MANIM0+2
          GOTO 100
C RESTART
 50       MREST0=1
          GOTO 100
C STOP
60       MSTOP0=1
         MREST0=1  
         ICTLSTOP0 = 1                    
         GOTO 100
C KILL
 70       MSTOP0=1
          ICTLSTOP0 = 1
          GOTO 100
C INFO
 80       GOTO 100
C ANIM PLOT
 90       MANIM0=MANIM0+4
          GOTO 100
C OUTP PLOT
 95       MANIM0=MANIM0+8
          GOTO 100
C L01 FLUSH
 96       BACKSPACE(IOUT)
          READ(IOUT,'(A)')REC
          GOTO 100
 97         WMCHECK0 = 1
          GOTO 100 
C H3D PLOT
 98       MH3D0=1
          GOTO 100  
 100      CONTINUE
          READ(IUSC3,'(A)',ERR=400,END=400)CARTE
         GOTO 5
C       
 400     MDESS=1
         REWIND IUSC3
C
         WRITE (IUSC3,'(A)',ERR=990)      ' RUN CONTROL FILE'
         WRITE (IUSC3,'(A,/)',ERR=990)    ' ----------------'
         IF(TMAN==0.0.AND.MCYCLE==0)THEN
         IF (MSG_R2R==1) THEN
           WRITE (IUSC3,'(A)',ERR=990)
     .     ' MULTIDOMAINS -> /CYCLE NOT ALLOWED'
         ENDIF        
          WRITE(CARTE,'(A)')' AT CURRENT CYCLE'
         ELSEIF(TMAN==0.0)THEN
          WRITE(CARTE,'(A,I10)')' AT CYCLE',MCYCLE
         ELSEIF(MCYCLE==0)THEN
          WRITE(CARTE,'(A,G14.7)')' AT TIME ',TMAN
         ELSE
          WRITE(CARTE,'(A,G14.7,A,I10)')' IF TIME >=',TMAN,
     .    ' AND IF CYCLE >=',MCYCLE
         ENDIF
         IF(MANIM0==1 .OR.MANIM0== 3.OR.
     .      MANIM0==5 .OR.MANIM0== 7.OR.
     .      MANIM0==9 .OR.MANIM0==11.OR.
     .      MANIM0==13.OR.MANIM0==15)
     .    WRITE(IUSC3,'(/A,A)',ERR=990)' WRITE NEXT PLOT FILE',CARTE
         IF(MANIM0== 2.OR.MANIM0== 3.OR.
     .      MANIM0== 6.OR.MANIM0== 7.OR.
     .      MANIM0==10.OR.MANIM0==11.OR.
     .      MANIM0==14.OR.MANIM0==15)
     .    WRITE(IUSC3,'(A,A)',ERR=990)' WRITE NEXT PATRAN FILES',CARTE
         IF(MANIM0== 4.OR.MANIM0== 5.OR.
     .      MANIM0== 6.OR.MANIM0== 7.OR.
     .      MANIM0==12.OR.MANIM0==13.OR.
     .      MANIM0==14.OR.MANIM0==15)
     .    WRITE(IUSC3,'(A,A)',ERR=990)' WRITE NEXT ANIMATION FILES',
     .                                  CARTE
         IF(MANIM0== 8.OR.MANIM0== 9.OR.
     .      MANIM0==10.OR.MANIM0==11.OR.
     .      MANIM0==12.OR.MANIM0==13.OR.
     .      MANIM0==14.OR.MANIM0==15)
     .    WRITE(IUSC3,'(A,A)',ERR=990)' WRITE NEXT OUTPUT FILES',
     .                                  CARTE
         IF(MREST0==1)
     .    WRITE(IUSC3,'(A,A)',ERR=990)' WRITE NEXT RESTART FILE',CARTE
         IF(MSTOP0==1)
     .    WRITE(IUSC3,'(A,A)',ERR=990)' RUN STOP',CARTE
         IF(MH3D0 == 1)
     .    WRITE(IUSC3,'(A,A)',ERR=990)' UPDATE H3D FILE', CARTE
         MFILR=1
	 IF (IRAD2R==1) R2R_MFILR=1    
         GOTO 999
 990    CONTINUE
        CLOSE (IUSC3)
 999    CONTINUE
       ENDIF             
C       
      ENDIF
C
C---------------------------------
C     TRAITEMENT MULTIDOMAINES
C---------------------------------
       
       IF (IRAD2R==1) THEN
         IF (ISPMD==0) THEN
C *** traitement flag mstop ***       
	   MSTOP = R2R_MSTOP
           R2R_MSTOP = MSTOP0
	   MSTOP0 = MSTOP	 	 	 
       	   IF (R2R_MFILR == 1) THEN      
C *** stockage des flags pour envoi     rad2rad ***
             TMAN_R2R = TMAN	  	       
             R2R_CTR(1) = MREST0
             R2R_CTR(2) = MANIM0
             R2R_CTR(3) = WMCHECK0	   
	   ELSEIF (R2R_MFILR == 2) THEN
C *** on applique les flags recus de rad2rad ***
             MFILR = 1
	     R2R_MFILR = 0
             TMAN = TMAN_R2R
	     MANIM0 = R2R_CTR(2)
	     WMCHECK0= R2R_CTR(3)	     	   	 	 	 	 	 	 
	   ENDIF
	 ENDIF
         IF(NSPMD > 1) CALL SPMD_IBCAST(MFILR,MFILR,1,1,0,2)	   	 	 	 	          
       ENDIF

C---------------------------------
C     ECHANGE MULTIPROCS
C---------------------------------
      IF((IANISENS/=0.OR.NLSENS/=0.OR.H3D_DATA%N_SENS_H3D>0).OR.
     .  ((NCYCLE/NCINP)*NCINP==NCYCLE).OR.
     .  ((IRAD2R==1).AND.(MFILR==1))) THEN    
          IF (ISPMD==0) THEN
             TMPMAN(1) = MDESS
             TMPMAN(2) = MREST0
             TMPMAN(3) = MREST
             TMPMAN(4) = MSTOP0
             TMPMAN(5) = MSTOP
             TMPMAN(6) = MANIM0
             TMPMAN(7) = MANIM
             TMPMAN(8) = MCYCLE
             TMPMAN(9) = MFILR
             TMPMAN(10)= TMAN
             TMPMAN(11)= WMCHECK0
             TMPMAN(12) = ICTLSTOP0
             TMPMAN(13) = MH3D0
             TMPMAN(14) = H3D_DATA%MH3D
           ENDIF
           IF(NSPMD > 1)CALL SPMD_RBCAST(TMPMAN,TMPMAN,1,14,0,2)
           IF (ISPMD/=0)THEN
              MDESS  = NINT(TMPMAN(1))
              MREST0 = NINT(TMPMAN(2))
              MREST  = NINT(TMPMAN(3))
              MSTOP0 = NINT(TMPMAN(4))
              MSTOP  = NINT(TMPMAN(5))
              MANIM0 = NINT(TMPMAN(6))
              MANIM  = NINT(TMPMAN(7))
              MCYCLE = NINT(TMPMAN(8))
              MFILR  = NINT(TMPMAN(9))
              TMAN   = TMPMAN(10)      
              WMCHECK0 = NINT(TMPMAN(11))
              ICTLSTOP0 = NINT(TMPMAN(12))
              MH3D0 =     NINT(TMPMAN(13))
              H3D_DATA%MH3D = NINT(TMPMAN(14))
           ENDIF
        ENDIF
C---------------------------------
    
      IF(MFILR==1)THEN
       IF(TT>=TMAN.AND.NCYCLE>=MCYCLE)THEN
        MFILR=0
        MREST=MREST0
        MANIM=MANIM0
        MSTOP=MSTOP0
        MDESS=MDESS+MANIM
        WMCHECK = WMCHECK0
        ICTLSTOP = ICTLSTOP0
        H3D_DATA%MH3D=MH3D0
       ENDIF
      ENDIF
C
      RETURN
      END
