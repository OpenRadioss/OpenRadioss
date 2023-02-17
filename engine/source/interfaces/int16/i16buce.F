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
Chd|  I16BUCE                       source/interfaces/int16/i16buce.F
Chd|-- called by -----------
Chd|        I16MAIN                       source/interfaces/int16/i16main.F
Chd|-- calls ---------------
Chd|        I16TRI                        source/interfaces/int16/i16tri.F
Chd|====================================================================
       SUBROUTINE I16BUCE(
     1   NSV        ,IXS      ,IXS16    ,IXS20    ,NELEM    ,
     2   NME      ,MWA        ,NSN      ,CAND_E   ,CAND_N   ,
     3   NOINT    ,I_STOK_GLOB,TZINF    ,MINBOX   ,EMINX    ,
     4   XSAV     ,ITASK      ,X        ,V        ,A        ,
     5   MX_CAND  ,IXS10      ,ESH_T)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NME, NSN, NOINT,IDT,ITASK,MX_CAND,
     .        ESH_T, I_STOK_GLOB
      INTEGER CAND_E(*),CAND_N(*),IXS(NIXS,*),IXS16(8,*),
     .        NSV(*),MWA(*),NELEM(*),IXS20(12,*),IXS10(6,*)
C     REAL
      my_real
     .   TZINF,MINBOX
      my_real
     .   X(3,*),EMINX(6,*),XSAV(3,*),V(3,*) ,A(3,*)  
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C
      INTEGER I, J, K, CONT,NB_N_B,I_STOK   ,NSNF,NSNL,
     .        IP0, IP1, IP2, IP21, IP22, IP31, MAXSIZ,
     .        NE,N8,N10,N20,N16
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF (DEBUG(3)>=1) THEN
#include "lockon.inc"
          WRITE(ISTDO,*)'** NEW SORT FOR INTERFACE NUMBER ',NOINT,
     .                ' AT CYCLE ',NCYCLE
          WRITE(IOUT,*)'** NEW SORT FOR INTERFACE NUMBER ',NOINT,
     .                ' AT CYCLE ',NCYCLE
#include "lockoff.inc"
      ENDIF
C-----------------------------------------------
      NSNF = 1 + ITASK*NSN / NTHREAD
      NSNL = (ITASK+1)*NSN / NTHREAD
      DO I=NSNF,NSNL
        J=NSV(I)
        XSAV(1,J) = X(1,J)+DT2*(V(1,J)+DT12*A(1,J))
        XSAV(2,J) = X(2,J)+DT2*(V(2,J)+DT12*A(2,J))
        XSAV(3,J) = X(3,J)+DT2*(V(3,J)+DT12*A(3,J))
      END DO
      DO K=1,8
          DO I=1+ESH_T,NME+ESH_T
            J=IXS(K+1,NELEM(I))
            XSAV(1,J) = X(1,J)+DT2*(V(1,J)+DT12*A(1,J))
            XSAV(2,J) = X(2,J)+DT2*(V(2,J)+DT12*A(2,J))
            XSAV(3,J) = X(3,J)+DT2*(V(3,J)+DT12*A(3,J))
          ENDDO
      ENDDO
      DO I=1+ESH_T,NME+ESH_T
        NE=NELEM(I)
        N8  = NE
        N10 = N8-NUMELS8
        N20 = N10-NUMELS10
        N16 = N20-NUMELS20
        IF(N16>0)THEN
          DO K=1,8
            J=IXS16(K,N16)
            XSAV(1,J) = X(1,J)+DT2*(V(1,J)+DT12*A(1,J))
            XSAV(2,J) = X(2,J)+DT2*(V(2,J)+DT12*A(2,J))
            XSAV(3,J) = X(3,J)+DT2*(V(3,J)+DT12*A(3,J))
          ENDDO
        ELSEIF(N20>0)THEN
          DO K=1,12
            J=IXS20(K,N20)
            IF(J/=0)THEN	
              XSAV(1,J) = X(1,J)+DT2*(V(1,J)+DT12*A(1,J))
              XSAV(2,J) = X(2,J)+DT2*(V(2,J)+DT12*A(2,J))
              XSAV(3,J) = X(3,J)+DT2*(V(3,J)+DT12*A(3,J))
	    ENDIF	      
          ENDDO
        ELSEIF(N10>0)THEN
          DO K=1,6
            J=IXS10(K,N10)
            XSAV(1,J) = X(1,J)+DT2*(V(1,J)+DT12*A(1,J))
            XSAV(2,J) = X(2,J)+DT2*(V(2,J)+DT12*A(2,J))
            XSAV(3,J) = X(3,J)+DT2*(V(3,J)+DT12*A(3,J))
          ENDDO
        ENDIF
      ENDDO
C-----------------------------------------------
      NB_N_B = 1
C Fin initialisation
C-----------------------------------------------
C
C-----2- TRI PAR BOITES DES ELEMENTS ET DES NOEUDS
C
C-----------------------------------------------
C SI ON A PAS ASSEZ DE MEMOIRE POUR LES PILES ON RECOMMENCE LE TRI
C EN INCREMENTANT LE NB_N_B (NOMBRE DE NOEUDS PAR BOITE FINIE)
C     POINTEUR  NOM                 TAILLE
C     P0........                    NSN + 3
C     P1........Elt Bas Pile        NME
C     P2........Elt PILE            2*NME
C     P21.......BPN                 NSN
C     P22.......PN                  NSN
      MAXSIZ = 3*(NME+100) 
      IP1 = 1
      IP2 = IP1+NME+100
      IP21= IP2+MAXSIZ
      IP22= IP21+NSN
      IP31= IP22+NSN
C-----------------------------------------------
C     nouvelle phase de tri
C-----------------------------------------------
      CONT = 1
C-----------------------------------------------
C     Boucle sur les retris
C-----------------------------------------------
      DO WHILE (CONT/=0)
        CALL I16TRI(
     1   MWA(IP1),MWA(IP2) ,MWA(IP21),MWA(IP22),NSN     ,
     2   TZINF   ,IXS      ,IXS16    ,IXS20    ,NELEM   ,
     3   NSV     ,MAXSIZ   ,CAND_N   ,CAND_E   ,MINBOX  ,
     5   CONT    ,NB_N_B   ,EMINX    ,I_STOK_GLOB,NME   ,
     6   ITASK   ,NOINT    ,X        ,V        ,A       ,
     7   MX_CAND ,IXS10    ,ESH_T    )
      ENDDO
C
      RETURN
      END
