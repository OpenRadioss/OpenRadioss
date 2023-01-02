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
Chd|  I24FOR3E                      source/interfaces/int24/i24for3e.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I24FOR3E() 
      RETURN
      END
C
Chd|====================================================================
Chd|  I24FORC_FIC                   source/interfaces/int24/i24for3e.F
Chd|-- called by -----------
Chd|        I24ASS0                       source/interfaces/int24/i24for3.F
Chd|        I24ASS2                       source/interfaces/int24/i24for3.F
Chd|        I24SMS2                       source/interfaces/int24/i24for3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I24FORC_FIC(NPT     ,IRTSE   ,NSNE    ,IS2SE   ,IS2PT   ,
     +                       NS      ,NFIC    ,FICI    ,FICS    ,IXSS    )
C============================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRTSE(5,*) ,NS,NSNE,IS2SE(2,*),IS2PT(*),NPT,IXSS(4),NFIC
      my_real
     .   FICI(NFIC),FICS(4,NFIC)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
! 4---------------3
! | .           . |
! |   .       .   |
! |     .   .     |
! |       .       |
! |     .   .     |
! |   .       .   |
! | .     o3    . |
! 1---o1------o2--2 NPT=3
C----- NLS : Num. of element with active edge----
      INTEGER I,J,K,NSN0,IP,NS1,NS2,IE,NP0
      INTEGER IK1(4),IK2(4),IE1,IE2,IED
      DATA IK1 /1,2,3,4/
      DATA IK2 /2,3,4,1/
      my_real
     .   FX,FY,FZ,S,STI,FICT(NFIC)
C=======================================================================
C----IRTSE(5,*) -> id of edge
C=======================================================================
       IF (NS<=0) RETURN
       FICS(1:4,1:NFIC) = ZERO
       IP = IS2PT(NS)
       IE1 = IS2SE(1,NS)
       IE2 = IS2SE(2,NS)
       IF (IE1 > 0) THEN
         IE = IE1
         IED=IRTSE(5,IE)
         NS1= IK1(IED)
         NS2= IK2(IED)
       ELSEIF(IE2 > 0) THEN
         IE = IE2
         IED=IRTSE(5,IE)
         NS1= IK2(IED)
         NS2= IK1(IED)
       ELSE
        print *,'probleme EDGES,IE1,IE2=',NS,IE1,IE2
       END IF
       IXSS(1:4) = IRTSE(1:4,IE)
        DO K = 1,NFIC    
         FICT(K) = FICI(K)
c         FICT(K) = FICI(K)/NPT
        END DO    
       IF (IP==NPT) THEN
C---equi_balance to NS1,NS2 and element center   
C-------seg center------- 
        IF (IRTSE(3,IE)/=IRTSE(4,IE)) THEN
         DO J =1,4
          DO K = 1,NFIC    
           FICS(J,K) = FOURTH*FICT(K)
          END DO    
         END DO        
        ELSE
         DO J =1,3
          DO K = 1,NFIC    
           FICS(J,K) = THIRD*FICT(K)
          END DO    
         END DO        
        END IF
C-------NPT should be unpair: 3,5,7....        
       ELSEIF (IP > 0 ) THEN
        NP0 = (NPT-1)/2
        IF (IP > NP0) THEN
C---------right side        
         S = (IP+1)*ONE/(NPT+1)
         DO K = 1,NFIC    
          FICS(NS2,K) = FICS(NS2,K) + S*FICT(K)
         END DO    
         S = (NPT-IP)*ONE/(NPT+1)
         DO K = 1,NFIC    
          FICS(NS1,K) = FICS(NS1,K) + S*FICT(K)
         END DO    
        ELSE
C---------left side        
         S = (NPT-IP+1)*ONE/(NPT+1)
         DO K = 1,NFIC    
          FICS(NS1,K) = FICS(NS1,K) + S*FICT(K)
         END DO    
         S = IP*ONE/(NPT+1)
         DO K = 1,NFIC    
          FICS(NS2,K) = FICS(NS2,K) + S*FICT(K)
         END DO    
        END IF
       END IF
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  I24FOR1_FIC                   source/interfaces/int24/i24for3e.F
Chd|-- called by -----------
Chd|        I24FOR3                       source/interfaces/int24/i24for3.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I24FOR1_FIC(NPT     ,IRTSE   ,NSNE    ,IS2SE   ,IS2PT   ,
     +                       NS      ,FXI     ,FYI     ,FZI     ,FOR1    ,
     +                       INEGA   )
      USE TRI7BOX
C============================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRTSE(5,*) ,NS,NSNE,IS2SE(2,*),IS2PT(*),NPT,INEGA
      my_real
     .   FXI ,FYI ,FZI, FOR1(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
! 4---------------3
! | .           . |
! |   .       .   |
! |     .   .     |
! |       .       |
! |     .   .     |
! |   .       .   |
! | .     o3    . |
! 1---o1------o2--2 NPT=3
C----- NLS : Num. of element with active edge----
      INTEGER I,J,K,NSN0,IP,NS1,NS2,IE,NP0,IX
      INTEGER IK1(4),IK2(4),IE1,IE2,IED
      DATA IK1 /1,2,3,4/
      DATA IK2 /2,3,4,1/
      my_real
     .   FX,FY,FZ,S,FXS(4),FYS(4),FZS(4)
C=======================================================================
C----IRTSE(5,*) -> id of edge, NS1,NS2 local edge nodes
C=======================================================================
       IF (NS<=0) RETURN
       DO J = 1,4
        FXS(J) = ZERO
        FYS(J) = ZERO
        FZS(J) = ZERO
       END DO
       IP = IS2PT(NS)
       IE1 = IS2SE(1,NS)
       IE2 = IS2SE(2,NS)
       IF (IE1 > 0) THEN
         IE = IE1
         IED=IRTSE(5,IE)
         NS1= IK1(IED)
         NS2= IK2(IED)
       ELSEIF(IE2 > 0) THEN
         IE = IE2
         IED=IRTSE(5,IE)
         NS1= IK2(IED)
         NS2= IK1(IED)
       ELSE
        print *,'probleme EDGENS,IE1,IE2=',NS,IE1,IE2
       END IF
       IF (IP==NPT) THEN
C---equi_balance to NS1,NS2 and element center       
         FX = FXI
         FY = FYI
         FZ = FZI         
c         FX = THIRD*FXI
c         FY = THIRD*FYI
c         FZ = THIRD*FZI         
C-------seg center------- 
        IF (IRTSE(3,IE)/=IRTSE(4,IE)) THEN
          FXS(1) = FOURTH*FX
          FYS(1) = FOURTH*FY
          FZS(1) = FOURTH*FZ
         DO J =2,4
          FXS(J) = FXS(1)
          FYS(J) = FYS(1)
          FZS(J) = FZS(1)
         END DO        
        ELSE
          FXS(1) = THIRD*FX
          FYS(1) = THIRD*FY
          FZS(1) = THIRD*FZ
         DO J =2,3
          FXS(J) = FXS(1)
          FYS(J) = FYS(1)
          FZS(J) = FZS(1)
         END DO        
        END IF
C-------NPT should be unpair: 3,5,7....        
       ELSEIF (IP > 0 ) THEN
        NP0 = (NPT-1)/2
        IF (IP > NP0) THEN
C---------right side        
         S = (IP+1)*ONE/(NPT+1)
         FXS(NS2) = FXS(NS2)+S*FXI
         FYS(NS2) = FYS(NS2)+S*FYI
         FZS(NS2) = FZS(NS2)+S*FZI
         S = (NPT-IP)*ONE/(NPT+1)
         FXS(NS1) = FXS(NS1)+S*FXI
         FYS(NS1) = FYS(NS1)+S*FYI
         FZS(NS1) = FZS(NS1)+S*FZI
        ELSE
C---------left side        
         S = (NPT-IP+1)*ONE/(NPT+1)
         FXS(NS1) = FXS(NS1)+S*FXI
         FYS(NS1) = FYS(NS1)+S*FYI
         FZS(NS1) = FZS(NS1)+S*FZI
         S = IP*ONE/(NPT+1)
         FXS(NS2) = FXS(NS2)+S*FXI
         FYS(NS2) = FYS(NS2)+S*FYI
         FZS(NS2) = FZS(NS2)+S*FZI
        END IF
       END IF
       IF (INEGA==-1) THEN
        DO J =1,4
         IX = IRTSE(J,IE)
         FOR1(1,IX) = FOR1(1,IX) - FXS(J)
         FOR1(2,IX) = FOR1(2,IX) - FYS(J)
         FOR1(3,IX) = FOR1(3,IX) - FZS(J)
        END DO
       ELSE
        DO J =1,4
         IX = IRTSE(J,IE)
         FOR1(1,IX) = FOR1(1,IX) + FXS(J)
         FOR1(2,IX) = FOR1(2,IX) + FYS(J)
         FOR1(3,IX) = FOR1(3,IX) + FZS(J)
        END DO
       END IF
C-----------
      RETURN
      END

Chd|====================================================================
Chd|  I24FOR1_FICR                  source/interfaces/int24/i24for3e.F
Chd|-- called by -----------
Chd|        I24FOR3                       source/interfaces/int24/i24for3.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I24FOR1_FICR(NPT     ,IRTSE   ,NSNE    ,IS2SE   ,IS2PT   ,
     +                       NS      ,FXI     ,FYI     ,FZI     ,FOR1    ,
     +                       INEGA  ,NI )
      USE TRI7BOX
C============================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRTSE(5,*) ,NS,NSNE,IS2SE(2,*),IS2PT(*),NPT,INEGA,NI
      my_real
     .   FXI ,FYI ,FZI, FOR1(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
! 4---------------3
! | .           . |
! |   .       .   |
! |     .   .     |
! |       .       |
! |     .   .     |
! |   .       .   |
! | .     o3    . |
! 1---o1------o2--2 NPT=3
C----- NLS : Num. of element with active edge----
      INTEGER I,J,K,NSN0,IP,NS1,NS2,IE,NP0,IX
      INTEGER IK1(4),IK2(4),IE1,IE2,IED
      DATA IK1 /1,2,3,4/
      DATA IK2 /2,3,4,1/
      my_real
     .   FX,FY,FZ,S,FXS(4),FYS(4),FZS(4)
C=======================================================================
C----IRTSE(5,*) -> id of edge, NS1,NS2 local edge nodes
C=======================================================================
       IF (NS<=0) RETURN
       DO J = 1,4
        FXS(J) = ZERO
        FYS(J) = ZERO
        FZS(J) = ZERO
       END DO
       IP = IS2PT(NS)
       IE1 = IS2SE(1,NS)
       IE2 = IS2SE(2,NS)
       IF (IE1 > 0) THEN
         IE = IE1
         IED=IRTSE(5,IE)
         NS1= IK1(IED)
         NS2= IK2(IED)
       ELSEIF(IE2 > 0) THEN
         IE = IE2
         IED=IRTSE(5,IE)
         NS1= IK2(IED)
         NS2= IK1(IED)
       ELSE
        print *,'probleme EDGENS,IE1,IE2=',NS,IE1,IE2
       END IF
       IF (IP==NPT) THEN
C---equi_balance to NS1,NS2 and element center       
         FX = FXI
         FY = FYI
         FZ = FZI         
c         FX = THIRD*FXI
c         FY = THIRD*FYI
c         FZ = THIRD*FZI         
C-------seg center------- 
        IF (IRTSE(3,IE)/=IRTSE(4,IE)) THEN
          FXS(1) = FOURTH*FX
          FYS(1) = FOURTH*FY
          FZS(1) = FOURTH*FZ
         DO J =2,4
          FXS(J) = FXS(1)
          FYS(J) = FYS(1)
          FZS(J) = FZS(1)
         END DO        
        ELSE
          FXS(1) = THIRD*FX
          FYS(1) = THIRD*FY
          FZS(1) = THIRD*FZ
         DO J =2,3
          FXS(J) = FXS(1)
          FYS(J) = FYS(1)
          FZS(J) = FZS(1)
         END DO        
        END IF
C-------NPT should be unpair: 3,5,7....        
       ELSEIF (IP > 0 ) THEN
        NP0 = (NPT-1)/2
        IF (IP > NP0) THEN
C---------right side        
         S = (IP+1)*ONE/(NPT+1)
         FXS(NS2) = FXS(NS2)+S*FXI
         FYS(NS2) = FYS(NS2)+S*FYI
         FZS(NS2) = FZS(NS2)+S*FZI
         S = (NPT-IP)*ONE/(NPT+1)
         FXS(NS1) = FXS(NS1)+S*FXI
         FYS(NS1) = FYS(NS1)+S*FYI
         FZS(NS1) = FZS(NS1)+S*FZI
        ELSE
C---------left side        
         S = (NPT-IP+1)*ONE/(NPT+1)
         FXS(NS1) = FXS(NS1)+S*FXI
         FYS(NS1) = FYS(NS1)+S*FYI
         FZS(NS1) = FZS(NS1)+S*FZI
         S = IP*ONE/(NPT+1)
         FXS(NS2) = FXS(NS2)+S*FXI
         FYS(NS2) = FYS(NS2)+S*FYI
         FZS(NS2) = FZS(NS2)+S*FZI
        END IF
       END IF
       IF (INEGA==-1) THEN
        DO J =1,4
         IX = IRTSE(J,IE)
c         IF(IX >NLSKYFI(NI))print*,ispmd,NI,'ERROR:',IX,NLSKYFI(NI)
         FOR1(1,IX) = FOR1(1,IX) - FXS(J)
         FOR1(2,IX) = FOR1(2,IX) - FYS(J)
         FOR1(3,IX) = FOR1(3,IX) - FZS(J)
        END DO
       ELSE
        DO J =1,4
         IX = IRTSE(J,IE)
         FOR1(1,IX) = FOR1(1,IX) + FXS(J)
         FOR1(2,IX) = FOR1(2,IX) + FYS(J)
         FOR1(3,IX) = FOR1(3,IX) + FZS(J)
        END DO
       END IF
C-----------
      RETURN
      END

Chd|====================================================================
Chd|  I24XVFIC_UPD                  source/interfaces/int24/i24for3e.F
Chd|-- called by -----------
Chd|        I24E2E_FICTIVE_NODES_UPDATE   source/interfaces/int24/i24for3e.F
Chd|-- calls ---------------
Chd|        I24XFIC_INI                   source/interfaces/int24/i24for3e.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I24XVFIC_UPD(IPARI  ,INTBUF_TAB   ,X    ,V ,NPT ,ITAB )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD   
C============================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(*) ,NPT,ITAB(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      my_real
     .   X(3,*),V(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,NRTSE,NSNE,NSN
C=======================================================================
          NRTSE   = IPARI(52)
          NSNE    = IPARI(55)
          NSN     = IPARI(5)
         CALL I24XFIC_INI(NRTSE   ,INTBUF_TAB%IRTSE   ,NSNE    ,INTBUF_TAB%IS2SE ,
     +                   INTBUF_TAB%IS2PT   ,NSN     ,INTBUF_TAB%NSV  ,X        ,
     +                   INTBUF_TAB%XFIC    ,NPT     , ITAB ,1)
         CALL I24XFIC_INI(NRTSE   ,INTBUF_TAB%IRTSE   ,NSNE    ,INTBUF_TAB%IS2SE ,
     +                   INTBUF_TAB%IS2PT   ,NSN     ,INTBUF_TAB%NSV  ,V        ,
     +                   INTBUF_TAB%VFIC    ,NPT     , ITAB ,2 )
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  I24XFIC_INI                   source/interfaces/int24/i24for3e.F
Chd|-- called by -----------
Chd|        I24XVFIC_UPD                  source/interfaces/int24/i24for3e.F
Chd|-- calls ---------------
Chd|        I24FIC_GETN                   source/interfaces/int24/i24for3e.F
Chd|====================================================================
      SUBROUTINE I24XFIC_INI(NRTSE   ,IRTSE   ,NSNE    ,IS2SE   ,IS2PT   ,
     4                       NSN     ,NSV     ,X       ,XFIC ,NPT , ITAB ,FLG)
C============================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"

C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRTSE(5,*) ,NSV(*),NRTSE,NSNE,IS2SE(2,*),IS2PT(*),NSN,NPT,
     .        ITAB(*),FLG
      my_real
     .   X(3,*),XFIC(3,*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
! 4---------------3
! | .           . |
! |   .       .   |
! |     .   .     |
! |       .       |
! |     .   .     |
! |   .       .   |
! | .     o3    . |
! 1---o1------o2--2 NPT=3
C----- NLS : Num. of element with active edge----
      INTEGER I,J,K,NSN0,NS,IP,NS1,NS2,IE,NP0,ipr
      my_real
     .   X0,Y0,Z0,XE0,YE0,ZE0,S
C=======================================================================
C----IRTSE(5,*) -> id of edge
C=======================================================================
      ipr=0
c      if (ncycle==0) ipr=1
       NSN0 = NSN-NSNE
c       if (ipr==1)   write(6,*),'NSN,NSNE=',NSN,NSNE
       DO I=NSN0+1,NSN
       NS=NSV(I)-NUMNOD
C      IF (NS<=0) write(iout,*) '!!!!error, NSV(I),I=',NSV(I),I
       IP = IS2PT(NS)
c       if (ipr==1)   write(6,*),'IP,NS,I=',IP,NS,I
       CALL I24FIC_GETN(NS      ,IRTSE   ,IS2SE   ,IE    ,NS1     ,
     +                  NS2     )
c        if (ipr==1) write(6,*),'IE,IRTSE(j,IE)=',IE,(itab(IRTSE(j,IE)),j=1,5)
       IF (IP==NPT) THEN
C-------seg center-------        
        IF (IRTSE(3,IE)==IRTSE(4,IE)) THEN
         X0=THIRD*(X(1,IRTSE(1,IE))+X(1,IRTSE(2,IE))+X(1,IRTSE(3,IE)))
         Y0=THIRD*(X(2,IRTSE(1,IE))+X(2,IRTSE(2,IE))+X(2,IRTSE(3,IE)))
         Z0=THIRD*(X(3,IRTSE(1,IE))+X(3,IRTSE(2,IE))+X(3,IRTSE(3,IE)))
        ELSE
         X0=FOURTH*(X(1,IRTSE(1,IE))+X(1,IRTSE(2,IE))+X(1,IRTSE(3,IE))+
     +             X(1,IRTSE(4,IE)))
         Y0=FOURTH*(X(2,IRTSE(1,IE))+X(2,IRTSE(2,IE))+X(2,IRTSE(3,IE))+
     +             X(2,IRTSE(4,IE)))
         Z0=FOURTH*(X(3,IRTSE(1,IE))+X(3,IRTSE(2,IE))+X(3,IRTSE(3,IE))+
     +             X(3,IRTSE(4,IE)))
        END IF
C-------edge center-------        
c         XE0=HALF*(X(1,NS1)+X(1,NS2))
c         YE0=HALF*(X(2,NS1)+X(2,NS2))
c         ZE0=HALF*(X(3,NS1)+X(3,NS2))
C         
         XFIC(1,NS) = THIRD*(X0+X(1,NS1)+X(1,NS2))
         XFIC(2,NS) = THIRD*(Y0+X(2,NS1)+X(2,NS2))
         XFIC(3,NS) = THIRD*(Z0+X(3,NS1)+X(3,NS2))

c         if (ipr==1) then
c          write(iout,*),'NS1,NS2,IE,=',itab(NS1),itab(NS2),IE
c          write(iout,*),'NS1,NS2,Xs1,Xs2,xns=',itab(NS1),itab(NS2)
cc          print *,X(3,IRTSE(1,IE)),X(3,IRTSE(2,IE)),X(3,IRTSE(3,IE))
c          write(iout,*)X(1,NS1),X(2,NS1),X(3,NS1)
c          write(iout,*)X(1,NS2),X(2,NS2),X(3,NS2)
c          write(iout,*)XFIC(1,NS),XFIC(2,NS),XFIC(3,NS)
c         end if
C-------NPT should be unpair: 3,5,7         
       ELSEIF (IP > 0 ) THEN
C-------edge center-------        
         XE0=HALF*(X(1,NS1)+X(1,NS2))
         YE0=HALF*(X(2,NS1)+X(2,NS2))
         ZE0=HALF*(X(3,NS1)+X(3,NS2))
         NP0 = (NPT-1)/2
        IF (IP > NP0) THEN
C---------right side        
         S = (IP-NP0)*ONE/(NPT-1)
         XFIC(1,NS) = XE0 +S*(X(1,NS2)-XE0)
         XFIC(2,NS) = YE0 +S*(X(2,NS2)-YE0)
         XFIC(3,NS) = ZE0 +S*(X(3,NS2)-ZE0)         
        ELSE
C---------left side        
         S = IP*ONE/(NPT-1)
         XFIC(1,NS) = X(1,NS1) +S*(XE0 -X(1,NS1))
         XFIC(2,NS) = X(2,NS1) +S*(YE0 -X(2,NS1))
         XFIC(3,NS) = X(3,NS1) +S*(ZE0 -X(3,NS1))         
        END IF
       END IF
      END DO
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  I24FICS_INI                   source/interfaces/int24/i24for3e.F
Chd|-- called by -----------
Chd|        I24E2E_FICTIVE_NODES_UPDATE   source/interfaces/int24/i24for3e.F
Chd|-- calls ---------------
Chd|        I24FIC_GETN                   source/interfaces/int24/i24for3e.F
Chd|====================================================================
      SUBROUTINE I24FICS_INI(IRTSE   ,NSNE    ,IS2SE   ,IS2PT  ,NSN     ,
     4                       NSV     ,S       ,FICS    ,NPT   ,ITAB )
C============================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRTSE(5,*) ,NSV(*),NSNE,IS2SE(2,*),IS2PT(*),NSN,NPT  ,ITAB(*)
      my_real
     .   S(*),FICS(NSNE)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
! 4---------------3
! | .           . |
! |   .       .   |
! |     .   .     |
! |       .       |
! |     .   .     |
! |   .       .   |
! | .     o3    . |
! 1---o1------o2--2 NPT=3
C----- NLS : Num. of element with active edge----
      INTEGER I,J,K,NSN0,NS,IP,NS1,NS2,IE,NP0
      my_real
     .   S0,SE0,FAC
C=======================================================================
C----IRTSE(5,*) -> id of edge
C=======================================================================
      NSN0 = NSN-NSNE
      DO I=NSN0+1,NSN
       NS=NSV(I)-NUMNOD
       IF (NS<=0) print *,'!!!!error, NSV(I),I=',NSV(I),I
       IP = IS2PT(NS)
       CALL I24FIC_GETN(NS      ,IRTSE   ,IS2SE   ,IE    ,NS1     ,
     +                  NS2     )
       IF (IP==NPT) THEN
C-------seg center-------        
        IF (IRTSE(3,IE)==IRTSE(4,IE)) THEN
         S0=THIRD*(S(IRTSE(1,IE))+S(IRTSE(2,IE))+S(IRTSE(3,IE)))
        ELSE
         S0=FOURTH*(S(IRTSE(1,IE))+S(IRTSE(2,IE))+S(IRTSE(3,IE))+
     +             S(IRTSE(4,IE)))
        END IF
C-------edge center-------        
C         
         FICS(NS) = THIRD*(S0+S(NS1)+S(NS2))
C-------NPT should be unpair: 3,5,7         
       ELSEIF (IP > 0 ) THEN
C-------edge center-------        
         SE0=HALF*(S(NS1)+S(NS2))
        NP0 = (NPT-1)/2
        IF (IP > NP0) THEN
C---------right side        
         FAC = (IP-NP0)/(NPT-1)
         FICS(NS) = SE0 +FAC*(S(NS2)-SE0)
        ELSE
C---------left side        
         FAC = IP/(NPT-1)
         FICS(NS) = S(NS1) +FAC*(SE0 -S(NS1))
        END IF
       END IF
      END DO
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  I24FIC_GETN                   source/interfaces/int24/i24for3e.F
Chd|-- called by -----------
Chd|        I24COR3                       source/interfaces/int24/i24cor3.F
Chd|        I24FICS_INI                   source/interfaces/int24/i24for3e.F
Chd|        I24TRIVOX                     source/interfaces/intsort/i24trivox.F
Chd|        I24XFIC_INI                   source/interfaces/int24/i24for3e.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I24FIC_GETN(NS      ,IRTSE   ,IS2SE   ,IE    ,NS1     ,
     4                       NS2     )
C============================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRTSE(5,*) ,NS,IS2SE(2,*),NS1,NS2,IE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C----- get edge NS1,NS2 and--Secnd seg id :IE-
      INTEGER IK1(4),IK2(4),IE1,IE2,IED
      DATA IK1 /1,2,3,4/
      DATA IK2 /2,3,4,1/
C=======================================================================
C----IRTSE(5,*) -> id of edge
C=======================================================================
       IE1 = IS2SE(1,NS)
       IE2 = IS2SE(2,NS)
       IF (IE1 > 0) THEN
         IE = IE1
         IED=IRTSE(5,IE)
         NS1= IRTSE(IK1(IED),IE)
         NS2= IRTSE(IK2(IED),IE)
       ELSEIF(IE2 > 0) THEN
         IE = IE2
         IED=IRTSE(5,IE)
         NS1= IRTSE(IK2(IED),IE)
         NS2= IRTSE(IK1(IED),IE)
       ELSE
        print *,'probleme EDGE NS,IE1,IE2=',NS,IE1,IE2
        call arret(2)
       END IF
C-----------
      RETURN
      END

Chd|====================================================================
Chd|  I24E2E_FICTIVE_NODES_UPDATE   source/interfaces/int24/i24for3e.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        I24FICS_INI                   source/interfaces/int24/i24for3e.F
Chd|        I24XVFIC_UPD                  source/interfaces/int24/i24for3e.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I24E2E_FICTIVE_NODES_UPDATE(INTLIST,NBINTC,IPARI,INTBUF_TAB,
     *            X,V,MS,ITAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"

C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      INTEGER INTLIST(*),NBINTC
      INTEGER IPARI(NPARI,*),ITAB(*)
      my_real
     *    X(3,*),V(3,*),MS(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER KK,N,NPT,NTY,NFIC,NSN,NSNE,IEDGE4
      my_real
     *  TS
C-----------------------------------------------
      DO KK=1,NBINTC
         N   = INTLIST(KK)
         NSN  = IPARI(5,N)
         NTY  = IPARI(7,N)
         NSNE = IPARI(55,N)
         IEDGE4 = IPARI(59,N)
         TS  = INTBUF_TAB(N)%VARIABLES(3)

         IF(NTY == 24.AND.IEDGE4>0.AND.TT>=TS)THEN

C Move XFIC & VFIC to another place for SPMD coherency
            NFIC = 3   
            CALL I24XVFIC_UPD(IPARI(1,N),INTBUF_TAB(N),X   ,V ,NFIC  ,ITAB)

C Move fictive Mass node computation to another place for SPMD coherency

            NPT = 3
            CALL I24FICS_INI(INTBUF_TAB(N)%IRTSE   ,NSNE    ,INTBUF_TAB(N)%IS2SE   ,
     1                       INTBUF_TAB(N)%IS2PT   ,NSN     ,INTBUF_TAB(N)%NSV     ,
     2                       MS                 ,INTBUF_TAB(N)%MSFIC   ,NPT  ,ITAB      )
         ENDIF 
      ENDDO 

      END
