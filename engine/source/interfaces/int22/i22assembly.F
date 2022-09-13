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
Chd|  I22ASS0                       source/interfaces/int22/i22assembly.F
Chd|-- called by -----------
Chd|        I22FOR3                       source/interfaces/int22/i22for3.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I22ASS0(JLT   ,IX1  ,IX2  ,IX3  ,IX4    ,
     2                  NSVG  ,H1   ,H2   ,H3   ,H4     ,STIF ,
     3                  FX1   ,FY1  ,FZ1  ,FX2  ,FY2    ,FZ2  ,
     4                  FX3   ,FY3  ,FZ3  ,FX4  ,FY4    ,FZ4  ,
     5                  FXI   ,FYI  ,FZI  ,A    ,STIFN  ,NIN  ,
     6                  INTTH ,PHI  ,FTHE ,PHI1 , PHI2  ,PHI3 ,
     7                  PHI4  )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C Interface Type22 (/INTER/TYPE22) is an FSI coupling method based on cut cell method. 
C   This experimental cut cell method is not completed, abandonned, and is not an official option.
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
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
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, NIN,INTTH,
     .        IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ),NSVG(MVSIZ)
      my_real
     .    H1(MVSIZ),H2(MVSIZ),H3(MVSIZ),H4(MVSIZ),STIF(MVSIZ),
     .    FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .    FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .    FX3(MVSIZ),FY3(MVSIZ),FZ3(MVSIZ),
     .    FX4(MVSIZ),FY4(MVSIZ),FZ4(MVSIZ),
     .    FXI(MVSIZ),FYI(MVSIZ),FZI(MVSIZ),
     .    A(3,*),  STIFN(*),PHI(*), FTHE(*),
     .    PHI1(*), PHI2(*), PHI3(*), PHI4(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, IG
C
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
          DO I=1,JLT
           J1=IX1(I)
           A(1,J1)=A(1,J1)+FX1(I)
           A(2,J1)=A(2,J1)+FY1(I)
           A(3,J1)=A(3,J1)+FZ1(I)
!           STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H1(I))
C
           J1=IX2(I)
           A(1,J1)=A(1,J1)+FX2(I)
           A(2,J1)=A(2,J1)+FY2(I)
           A(3,J1)=A(3,J1)+FZ2(I)
!           STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H2(I))
C
           J1=IX3(I)
           A(1,J1)=A(1,J1)+FX3(I)
           A(2,J1)=A(2,J1)+FY3(I)
           A(3,J1)=A(3,J1)+FZ3(I)
!           STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H3(I))
C
           J1=IX4(I)
           A(1,J1)=A(1,J1)+FX4(I)
           A(2,J1)=A(2,J1)+FY4(I)
           A(3,J1)=A(3,J1)+FZ4(I)
!           STIFN(J1) = STIFN(J1) + STIF(I)*ABS(H4(I))
         ENDDO

C     
      RETURN
      END


Chd|====================================================================
Chd|  I22ASS2                       source/interfaces/int22/i22assembly.F
Chd|-- called by -----------
Chd|        I22FOR3                       source/interfaces/int22/i22for3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I22ASS2(JLT   ,IX1   ,IX2   ,IX3    ,IX4     ,ITAB   ,
     2                   NSVG  ,H1    ,H2    ,H3     ,H4      ,STIF   ,
     3                   FX1   ,FY1   ,FZ1   ,FX2    ,FY2     ,FZ2    ,
     4                   FX3   ,FY3   ,FZ3   ,FX4    ,FY4     ,FZ4    ,
     5                   FXI   ,FYI   ,FZI   ,FSKYI  ,ISKY    ,NISKYFI,
     6                   NIN   ,NOINT ,INTTH ,PHI    ,FTHESKYI,PHI1   ,
     7                   PHI2  ,PHI3  ,PHI4  ,CB_LOC ,CE_LOC  ,IRECT  ,
     8                   IXS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE I22TRI_MOD
      USE I22BUFBRIC_MOD 
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "warn_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,NISKYFI,NIN,NOINT,INTTH,
     .        ISKY(*),ITAB(*),CB_LOC(MVSIZ),CE_LOC(MVSIZ),
     .        IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ),NSVG(MVSIZ),
     .        IRECT(4,*),IXS(NIXS,*)
      my_real
     .        H1(MVSIZ),H2(MVSIZ),H3(MVSIZ),H4(MVSIZ),STIF(MVSIZ),
     .        FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .        FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .        FX3(MVSIZ),FY3(MVSIZ),FZ3(MVSIZ),
     .        FX4(MVSIZ),FY4(MVSIZ),FZ4(MVSIZ),
     .        FXI(MVSIZ),FYI(MVSIZ),FZI(MVSIZ),
     .        FSKYI(LSKYI,NFSKYI),FTHESKYI(LSKYI),PHI(MVSIZ),
     .        PHI1(*),PHI2(*)  ,PHI3(*) ,PHI4(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, IG, NISKYL1, NISKYL,IGP,IGM,IDR,NISKYFIL, K
C
      NISKYL1 = 0
      NISKYL1 = NISKYL1 + 4*JLT
C-----------------------------------------------
C   P r e - C o n d i t i o n
C-----------------------------------------------
      IF(INT22==0)RETURN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
#include "lockon.inc"
      NISKYL   = NISKY ;
      NISKY    = NISKY + NISKYL1
      NISKYFIL = NISKYFI
      NISKYFI  = NISKYFI
#include "lockoff.inc"
C
      IF (NISKYL+NISKYL1 > LSKYI) THEN
         !CALL ANCMSG(MSGID=26,ANMODE=ANINFO)
         print *, "** ERROR INTER22 : force assembly issue with PARITH/ON option"
         CALL ARRET(2)
      ENDIF
      IF (NISKYFIL > NLSKYFI(NIN)) THEN
        CALL ANCMSG(MSGID=26,ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF
C
      IF(INTTH == 0 ) THEN
        DO I=1,JLT
            NISKYL          = NISKYL + 1
            FSKYI(NISKYL,1) = FX1(I)
            FSKYI(NISKYL,2) = FY1(I)
            FSKYI(NISKYL,3) = FZ1(I)
            FSKYI(NISKYL,4) = ZERO
            ISKY(NISKYL)    = IX1(I)
            if(ibug22_fcontASS==-1)then
              K = ABS(CE_LOC(I))
              print *, "      assembly iparit2 - node_id :   ", itab(IX1(I))
              print *, "       +--BRIQUE :   ", IXS(11,BRICK_LIST(NIN,CB_LOC(I))%ID)
              print *, "       +--FACETTE:   ", ITAB(IRECT(1:4,K))
              print *, "       +--NCYCLE:    ", NCYCLE              
              write(*,FMT='(A,3E30.16)') "      +--Fxyz1:   ", FX1(I),FY1(I),FZ1(I)
            endif
        ENDDO
        DO I=1,JLT
            NISKYL          = NISKYL + 1
            FSKYI(NISKYL,1) = FX2(I)
            FSKYI(NISKYL,2) = FY2(I)
            FSKYI(NISKYL,3) = FZ2(I)
            FSKYI(NISKYL,4) = ZERO
            ISKY(NISKYL)    = IX2(I) 
            if(ibug22_fcontASS==-1)then
              print *, "      assembly iparit2 - node_id :   ", itab(IX2(I))
              write(*,FMT='(A,3E30.16)') "      +--Fxyz2:   ", FX2(I),FY2(I),FZ2(I)
            endif                   
        ENDDO
        DO I=1,JLT
            NISKYL          = NISKYL + 1
            FSKYI(NISKYL,1) = FX3(I)
            FSKYI(NISKYL,2) = FY3(I)
            FSKYI(NISKYL,3) = FZ3(I)
            FSKYI(NISKYL,4) = ZERO
            ISKY(NISKYL)    = IX3(I)
            if(ibug22_fcontASS==-1)then
              print *, "      assembly iparit2 - node_id :   ", itab(IX3(I))
              write(*,FMT='(A,3E30.16)') "      +--Fxyz3:   ", FX3(I),FY3(I),FZ3(I)
            endif            
        ENDDO
        DO I=1,JLT
            NISKYL          = NISKYL + 1
            FSKYI(NISKYL,1) = FX4(I)
            FSKYI(NISKYL,2) = FY4(I)
            FSKYI(NISKYL,3) = FZ4(I)
            FSKYI(NISKYL,4) = ZERO
            ISKY(NISKYL)    = IX4(I)
            if(ibug22_fcontASS==-1)then
              print *, "      assembly iparit2 - node_id :   ", itab(IX4(I))
              write(*,FMT='(A,3E30.16)') "      +--Fxyz4:   ", FX4(I),FY4(I),FZ4(I)
            endif            
        ENDDO

      !### Thermique ###!
      ELSE     
        !not planned yet
      ENDIF!(INTTH == 0 )
C      
      RETURN
      END
C
