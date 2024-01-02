Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  SINI43                        source/elements/solid/sconnect/sini43.F
Chd|-- called by -----------
Chd|        SUINIT3                       source/elements/elbuf_init/suinit3.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SINI43(ELBUF_STR,
     1           MLW   ,NEL   ,AREA  ,VOLG  ,RHOG  ,
     2           STIFM ,STIFR ,VISCM ,VISCR ,UPARAM,
     3           MAS1  ,MAS2  ,MAS3  ,MAS4  ,MAS5  ,
     4           MAS6  ,MAS7  ,MAS8  ,INN1  ,INN2  ,
     5           INN3  ,INN4  ,INN5  ,INN6  ,INN7  ,
     6           INN8  ,PM    ,MAT   ,OFFG  ,EINTG ,
     7           PTSOL ,SIGSP ,NSIGI ,NUVAR )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
C-------------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "vect01_c.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s  
C----------------------------------------------------------
      INTEGER NEL,MLW,NSIGI,NUVAR
      INTEGER MAT(*),PTSOL(*)
      my_real
     . AREA(*),VOLG(*),RHOG(*),STIFM(*) ,STIFR(*) , VISCM(*) ,VISCR(*) ,
     . MAS1(*),MAS2(*),MAS3(*),MAS4(*),MAS5(*),MAS6(*),MAS7(*),MAS8(*),
     . INN1(*),INN2(*),INN3(*),INN4(*),INN5(*),INN6(*),INN7(*),INN8(*),
     . UPARAM(*),OFFG(*),EINTG(*),PM(NPROPM,*),SIGSP(NSIGI,NEL)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,JJ,KK(6),IP,IPP,IPSU,IUS,MA,MFLAG,L_PLA,JPS,JPS1,
     .   IFLAGINI,NVAR_TMP,J
      my_real MASS,INER
      my_real GAMA(6)
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(BUF_MAT_) ,POINTER :: MBUF
      my_real, DIMENSION(:)  , POINTER  :: UVAR,PLA,EPS,SIG  
C-----------------------------------------------------------------------
C     SIMPLIFIED MASS AND INERTIA COMPUTATION
C=======================================================================
      IF (MLW == 59) THEN
        MFLAG = NINT(UPARAM(7))
      ELSEIF (MLW == 83) THEN
        MFLAG = NINT(UPARAM(9))
      ELSEIF (MLW == 116) THEN
        MFLAG = NINT(UPARAM(3))
      ELSEIF (MLW == 117) THEN
        MFLAG = NINT(UPARAM(12))
      ELSE
        MFLAG = 0
      ENDIF
!
      DO J=1,6
       KK(J) = (J-1)*NEL
      ENDDO
!
c
      DO I=1,NEL
        MA = MAT(I)
        OFFG(I)  = ONE
        EINTG(I) = PM(23,MA)
        RHOG(I)  = PM(89,MA)
      ENDDO
c
      IF (MFLAG == 1) THEN   ! surface density
        DO I=1,NEL
          MASS = RHOG(I)*AREA(I)*ONE_OVER_8
          MAS1(I) = MASS
          MAS2(I) = MASS
          MAS3(I) = MASS
          MAS4(I) = MASS
          MAS5(I) = MASS
          MAS6(I) = MASS
          MAS7(I) = MASS
          MAS8(I) = MASS
        ENDDO
      ELSE                    ! volume density
        DO I=1,NEL
          MASS = RHOG(I)*VOLG(I)*ONE_OVER_8
          MAS1(I) = MASS
          MAS2(I) = MASS
          MAS3(I) = MASS
          MAS4(I) = MASS
          MAS5(I) = MASS
          MAS6(I) = MASS
          MAS7(I) = MASS
          MAS8(I) = MASS
        ENDDO
      ENDIF
c        
      INER = ZERO
      DO I=1,NEL
        INN1(I) = INER
        INN2(I) = INER
        INN3(I) = INER
        INN4(I) = INER
        INN5(I) = INER
        INN6(I) = INER
        INN7(I) = INER
        INN8(I) = INER
        STIFR(I) = ZERO
        STIFM(I) = ZERO
        VISCM(I) = ZERO
        VISCR(I) = ZERO
      ENDDO
c---------------------------
      IF (ISIGI /= 0) THEN     ! CONTRAINTES INITIALES
        L_PLA = ELBUF_STR%BUFLY(1)%L_PLA
c        GAMA(1)=ONE        
c        GAMA(2)=ZERO      
c        GAMA(3)=ZERO      
c        GAMA(4)=ZERO      
c        GAMA(5)=ONE        
c        GAMA(6)=ZERO      
c       Initialize variables per integration point  
        DO IP=1,4
          LBUF => ELBUF_STR%BUFLY(1)%LBUF(IP,1,1)
          MBUF => ELBUF_STR%BUFLY(1)%MAT(IP,1,1)
          EPS  => ELBUF_STR%BUFLY(1)%LBUF(IP,1,1)%EPE(1:NEL*3)
          SIG  => ELBUF_STR%BUFLY(1)%LBUF(IP,1,1)%SIG(1:NEL*6)
          PLA  => ELBUF_STR%BUFLY(1)%LBUF(IP,1,1)%PLA(1:NEL*L_PLA)
          UVAR => ELBUF_STR%BUFLY(1)%MAT(IP,1,1)%VAR(1:NEL*NUVAR)
          JPS  = 1+ (IP-1)*9
          JPS1 = NVSOLID1 + (IP-1)*6
          DO I=1,NEL
            IFLAGINI = 0
            II = NFT+I
            JJ = PTSOL(II)
            IFLAGINI = 1
            IF (JJ == 0) IFLAGINI = 0
c
            IF (IFLAGINI == 1) THEN   
              IPP = I  
              IF (NVSOLID1 /= 0 ) THEN                 
                SIG(KK(1) + I) = SIGSP(JPS+1,JJ)
                SIG(KK(2) + I) = SIGSP(JPS+2,JJ)
                SIG(KK(3) + I) = SIGSP(JPS+3,JJ)
                SIG(KK(4) + I) = SIGSP(JPS+4,JJ)
                SIG(KK(5) + I) = SIGSP(JPS+5,JJ)
                SIG(KK(6) + I) = SIGSP(JPS+6,JJ)
c                IF (STRSGLOB(I) == 1) 
c     .           CALL SROTA6_M1(X,IX(1,II),JCVT,SIG(1,I),GAMA,JHBE,IGTYP)                    

                 IF (SIGSP(JPS+7,JJ) /= ZERO) LBUF%EINT(I)=SIGSP(JPS+7,JJ)
                 IF (L_PLA > 0 .AND. SIGSP(JPS+8,JJ) /= ZERO) 
     .                   PLA(I) = SIGSP(JPS+8,JJ)       
                 IF (L_PLA == 2 .AND. SIGSP(JPS+9,JJ) /= ZERO) 
     .                   PLA(I+NEL) = SIGSP(JPS+9,JJ)       
               ENDIF
               NVAR_TMP = SIGSP(NVSOLID1 + NVSOLID2 + 3, JJ)  
               IPSU = NVSOLID1 + NVSOLID2 + 4 + (IP - 1)*NVAR_TMP
               DO IUS = 1, NVAR_TMP             
                 IPP = I + (IUS -1)*NEL
                 UVAR(IPP) = SIGSP(IPSU + IUS, JJ)          
               ENDDO 
               DO IUS = NVAR_TMP + 1, NUVAR             
                 IPP = I + (IUS -1)*NEL
                 UVAR(IPP) = ZERO        
               ENDDO
               IF (NVSOLID2 /= 0) THEN      
                 EPS(KK(1) + I) = SIGSP(JPS1 + 3 , JJ)
                 EPS(KK(2) + I) = SIGSP(JPS1 + 5 , JJ)
                 EPS(KK(3) + I) = SIGSP(JPS1 + 6 , JJ)
c                 IF (STRAGLOB(I) == 1)
c     .           CALL SROTA6_M1(X,IX(1,II),JCVT,EPS(1,I),GAMA,JHBE,IGTYP)
               ENDIF  
             ENDIF
        
          ENDDO !  I=1,NEL
        ENDDO   !  IP=1,4
C-----------
      ENDIF  ! ISIGI /= 0
c      
C-----------
      RETURN
      END

