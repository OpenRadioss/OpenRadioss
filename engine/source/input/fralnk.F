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
Chd|  FRALNK                        source/input/fralnk.F         
Chd|-- called by -----------
Chd|        FREFORM                       source/input/freform.F        
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        READ10                        source/input/read10.F         
Chd|        WRIUSC2                       source/input/wriusc2.F        
Chd|        IXYZ                          source/input/ixyz.F           
Chd|        NVAR                          source/input/nvar.F           
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FRALNK(IKAD,KEY0,KVEL,NALELK,IFID)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IKAD(0:*),
     .        KVEL,NALELK,IFID
      CHARACTER KEY0(*)*5
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr07_c.inc"
#include      "units_c.inc"
#include      "nchar_c.inc"
#include      "chara_c.inc"
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER IXYZ, NVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------

      INTEGER I, N, NBC, K, KK, NS, K4,IKEY, M1, M2,IERR
      CHARACTER KEYA*80, CARTE*ncharline, KEY2*5, KEY3*5, KEY4*5
C
      K=0
      IERR=0
      IKEY=KVEL
      
       DO N=1,NALELK
         READ(IUSC1,
     .        REC=IKAD(IKEY)+K,
     .        FMT='(7X,A,1X,A,1X,I5,1X,A,20X,I10)',
     .        ERR=9990)
     .          KEY2,KEY3,K4,KEY4,NBC
         K=K+1
         IF(KEY2=='TRA  '.OR.KEY2=='ROT  ')THEN
           K=K+NBC
           CYCLE
         ENDIF
         CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
         READ(IUSC2,*,ERR=9990,END=9990)M1,M2
         IF(M1<=0.OR.M2<=0)GOTO 9990      !Negative main node null or negative not allowed
         !IF(KEY4(1:5)=='GRNOD')M1=-M1         !Tag on ale link for specific treatment in lectur.F
         
         !---------------------------------!
         !  ALE LINK DEFINED FROM NODES    !
         !---------------------------------!         
         IF(KEY4(1:5)/='GRNOD')THEN
           K=K+1
           KK=K
           NS=0
           !counting number of nodes           
           DO I=1,NBC-1
             READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(A)',ERR=9990)CARTE
             K=K+1
             NS=NS+NVAR(CARTE)
             IF(NS==0)THEN
               IERR=1
             ENDIF
           ENDDO             
           !checking input             
           IF(IERR==1)THEN
             WRITE(ISTDO,*)
     .         ' ** ERROR : NODE ID(S) NOT FOUND IN ALE LINK CARD'
             WRITE(IOUT ,*)
     .         ' ** ERROR : NODE ID(S) NOT FOUND IN ALE LINK CARD'
             CALL ARRET(2)          
           END IF 
           !array size for allocation (LINALE(SLINALE))           
           LLINAL=LLINAL+NS+6                          
         !---------------------------------!
         !  ALE LINK DEFINED FROM GRNOD    !
         !---------------------------------!
         ELSEIF(KEY4(1:5)=='GRNOD')THEN
           K=K+1
           KK=K
           NS=0
           !counting number of nodes
           DO I=1,NBC-1
             READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(A)',ERR=9990)CARTE
             K=K+1
             IF(NVAR(CARTE)>1.AND.M1<0)THEN !one single id per line
               IERR=1
             ENDIF
             IF(NVAR(CARTE)==1.AND.NS>0)THEN !grnod already defined on a previous line
               IERR=1
             ENDIF
             IF(NVAR(CARTE)==1.AND.NS==0)THEN !first single definition
               NS=1
             ENDIF             
           ENDDO
           !checking input
           IF(IERR==1)THEN
             WRITE(ISTDO,*)
     .         ' ** ERROR : SINGLE GRNOD ID NOT FOUND IN ALE LINK CARD'
             WRITE(IOUT ,*)
     .         ' ** ERROR : SINGLE GRNOD ID NOT FOUND IN ALE LINK CARD'
             CALL ARRET(2)          
           END IF
           NS=-NS
           !array size for allocation (LINALE(SLINALE))           
           LLINAL=LLINAL+1+6           
         ENDIF
         
         WRITE(IIN,'(3I10,5X,I3.3,I10)')M1,M2,NS,IXYZ(KEY3),K4
         CALL READ10(IKAD(IKEY)+KK,NBC-1,KEY0(IKEY)) 
        
       ENDDO !next N (ALE LINK)
             
      RETURN
 9990 CONTINUE
      CALL ANCMSG(MSGID=73,ANMODE=ANINFO,
     .            C1=KEY0(IKEY))
      CALL ARRET(0)
      END
C format v12
Chd|====================================================================
Chd|  FRALNK2                       source/input/fralnk.F         
Chd|-- called by -----------
Chd|        FREFORM                       source/input/freform.F        
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        READ10                        source/input/read10.F         
Chd|        WRIUSC2                       source/input/wriusc2.F        
Chd|        IXYZ                          source/input/ixyz.F           
Chd|        NVAR                          source/input/nvar.F           
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FRALNK2(IKAD,KEY0,KALELINK,NALELK,IFID)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IKAD(0:*),
     .        KALELINK,NALELK,IFID
      CHARACTER KEY0(*)*5
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr07_c.inc"
#include      "units_c.inc"
#include      "chara_c.inc"
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER IXYZ, NVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, NBC, K, KK, NS, K4,IKEY, M1, M2, IFORM,IERR
      CHARACTER KEYA*80, CARTE*ncharline, KEY1*5, KEY2*5, KEY3*5,
     .          KEY4*5, IOPT*5
C
      K=0
      IERR=0
      IKEY=KALELINK
      N=0
      DO WHILE(N<NALELK)
        READ(IUSC1,REC=IKAD(IKEY)+K,
     .       FMT='(7X,A,1X,A,1X,I5,1X,A,1X,A,13X,I10)',
     .       ERR=9990) KEY1 , KEY2 , IFORM  , KEY3 , KEY4   ,   NBC
                 !    'LINK','VEL' ,'Iform' ,'XYZ' ,'GRNOD' ,...NBC
        IF(KEY1(1:5)/='LINK ')THEN
          K=K+1
          CYCLE
        ENDIF
        N=N+1
        !---------------------------------!
        !  /ALE/LINK/OFF                  !
        !---------------------------------!
        IF(KEY2(1:5)=='OFF  ')THEN
          NS=0
          K=K+1
          CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
          KK=K
          DO I=0,NBC-1
            READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(A)',ERR=9990)CARTE
            K=K+1
            NS=NS+NVAR(CARTE)
          ENDDO
          WRITE(IIN,'(3I10,5X,I3.3,I10)')-2,-2,NS,0,0
          CALL READ10(IKAD(IKEY)+KK,NBC,KEY0(IKEY))

        !---------------------------------!
        !  /ALE/LINK/ON                   !
        !---------------------------------!
        ELSEIF(KEY2(1:5)=='ON  ')THEN
          NS=0
          K=K+1
          CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
          KK=K
          DO I=0,NBC-1
            READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(A)',ERR=9990)CARTE
            K=K+1
            NS=NS+NVAR(CARTE)
          ENDDO
          WRITE(IIN,'(3I10,5X,I3.3,I10)')-1,-1,NS,0,0
          CALL READ10(IKAD(IKEY)+KK,NBC,KEY0(IKEY))        
                
        !---------------------------------!
        ! '/ALE/LINK/VEL/*'               !
        !---------------------------------!
        ELSEIF(KEY2(1:5)=='VEL  ')THEN
          K=K+1
          CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
          READ(IUSC2,*,ERR=9990,END=9990)M1,M2
          IF(M1<=0.OR.M2<=0)GOTO 9990      !Negative main node null or negative not allowed                 
          K=K+1
          KK=K
          NS=0    
          !---------------------------------!
          !  ALE LINK DEFINED FROM NODES    !
          !---------------------------------!
          IF(KEY4(1:5)=='NODES'.OR.KEY4(1:5)=='     ')THEN
            DO I=1,NBC-1
             READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(A)',ERR=9990)CARTE
             K=K+1
             NS=NS+NVAR(CARTE)
            ENDDO
            LLINAL=LLINAL+NS+6
            !WRITE(IIN,'(3I10,5X,I3.3,I10)')M1,M2,NS,IXYZ(KEY3),IFORM
            !CALL READ10(IKAD(IKEY)+KK,NBC-1,KEY0(IKEY))
          !---------------------------------!
          !  ALE LINK DEFINED FROM GRNOD    !
          !---------------------------------!
          ELSEIF(KEY4(1:5)=='GRNOD')THEN
            DO I=1,NBC-1
              READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(A)',ERR=9990)CARTE
              K=K+1
              IF(NVAR(CARTE)>1.AND.M1<0)THEN !one single id per line
                IERR=1
              ENDIF
              IF(NVAR(CARTE)==1.AND.NS>0)THEN !grnod already defined on a previous line
                IERR=1
              ENDIF
              IF(NVAR(CARTE)==1.AND.NS==0)THEN !first single definition
                NS=1
              ENDIF             
            ENDDO
            !checking input
            IF(IERR==1)THEN
              WRITE(ISTDO,*)
     .          ' ** ERROR : SINGLE GRNOD ID NOT FOUND IN ALE LINK CARD'
              WRITE(IOUT ,*)
     .          ' ** ERROR : SINGLE GRNOD ID NOT FOUND IN ALE LINK CARD'
              CALL ARRET(2)          
            END IF  
            NS=-NS
            LLINAL=LLINAL+1+6             
          ENDIF     
          !---------------------------------!                    
          WRITE(IIN,'(3I10,5X,I3.3,I10)')M1,M2,NS,IXYZ(KEY3),IFORM
          CALL READ10(IKAD(IKEY)+KK,NBC-1,KEY0(IKEY))                  

        ENDIF !(KEY2=='VEL  ')
      ENDDO
C
      RETURN
C
 9990 CONTINUE
      CALL ANCMSG(MSGID=73,ANMODE=ANINFO,
     .            C1=KEY0(IKEY))
      CALL ARRET(0)
      END
