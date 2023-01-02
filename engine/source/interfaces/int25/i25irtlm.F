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
Chd|  I25IRTLM                      source/interfaces/int25/i25irtlm.F
Chd|-- called by -----------
Chd|        I25MAIN_OPT_TRI               source/interfaces/intsort/i25main_opt_tri.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25IRTLM(
     1                   IPARI   ,INTBUF_TAB ,ITAB   ,NIN      )
C=======================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
      USE TRI7BOX
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN
      INTEGER IPARI(NPARI,NINTER), ITAB(*)
C     REAL
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .        I, J, L, H, I_STOK_RTLM,
     .        N, NSN, NSNR, IVIS2
C     REAL
C-----------------------------------------------
C     REAL
C-----------------------------------------------
        NSN   =IPARI(5,NIN)
        NSNR  =IPARI(24,NIN)
        IVIS2 =IPARI(14,NIN)
C
        I_STOK_RTLM = 0
        IF(IVIS2/=-1) THEN

          DO N=1,NSN
c             if(itab(INTBUF_TAB%nsv(n))==10004284)
c     .       print *,'nat',nin,ipari(15,nin),ispmd+1,INTBUF_TAB%IRTLM(4*(N-1)+1),INTBUF_TAB%IRTLM(4*(N-1)+4),
c     .       INTBUF_TAB%PENE_OLD(5*(N-1)+5)
            IF(INTBUF_TAB%IRTLM(4*(N-1)+1) > 0)THEN 
              IF(INTBUF_TAB%STFNS(N)==ZERO)THEN
C
C               Shooting Nodes
                INTBUF_TAB%IRTLM(4*N-3:4*N)=0
              ELSEIF(INTBUF_TAB%IRTLM(4*(N-1)+4) == ISPMD+1)THEN
                L = INTBUF_TAB%IRTLM(4*(N-1)+3)
                IF(INTBUF_TAB%STFM(L)==ZERO)THEN
C
C                 Reset IRTLM when a Secnd node is in contact with a Main surface which was deleted.
                  INTBUF_TAB%IRTLM(4*(N-1)+1)=0
                  INTBUF_TAB%IRTLM(4*(N-1)+2)=0
C
C                 The segment where the node was impacted has been deleted 
C                 at the previous cycle => the node can not impact during THIS cycle, but the next cycle ONLY
                  INTBUF_TAB%IRTLM(4*(N-1)+3)  = -1
                  INTBUF_TAB%IRTLM(4*(N-1)+4)  =  0
                  INTBUF_TAB%TIME_S(2*(N-1)+1) =  EP20
                  INTBUF_TAB%TIME_S(2*(N-1)+2) =  EP20
C
          	  INTBUF_TAB%SECND_FR(6*(N-1)+1:6*N) = ZERO
          	  INTBUF_TAB%STIF_OLD(2*(N-1)+1:2*N)= ZERO
          	  INTBUF_TAB%PENE_OLD(5*(N-1)+1:5*N)= ZERO
C
                ELSE
C
C                 N impacte sur mon domaine SPMD, sur le segment n  IRTLM(3,N)
                  I_STOK_RTLM=I_STOK_RTLM+1
                  INTBUF_TAB%CAND_OPT_N(I_STOK_RTLM)= N
                  INTBUF_TAB%CAND_OPT_E(I_STOK_RTLM)= L
C
C copy old friction forces
                  INTBUF_TAB%SECND_FR(6*(N-1)+4:6*N)=INTBUF_TAB%SECND_FR(6*(N-1)+1:6*(N-1)+3)
C set new friction forces at 0
                  INTBUF_TAB%SECND_FR(6*(N-1)+1:6*(N-1)+3)=ZERO
                  INTBUF_TAB%PENE_OLD(5*(N-1)+2) = INTBUF_TAB%PENE_OLD(5*(N-1)+1)
                  INTBUF_TAB%PENE_OLD(5*(N-1)+1) = ZERO
                  INTBUF_TAB%STIF_OLD(2*(N-1)+2) = INTBUF_TAB%STIF_OLD(2*(N-1)+1) 
                  INTBUF_TAB%STIF_OLD(2*(N-1)+1) = ZERO
C
C                 se prepare a quitter le contact
                  INTBUF_TAB%TIME_S(2*(N-1)+1) = EP20
                  INTBUF_TAB%TIME_S(2*(N-1)+2) = EP20
                END IF
              ELSE ! IF(INTBUF_TAB%IRTLM(4*(N-1)+4) == ISPMD+1)THEN
C
C               Seul le processeur qui a l ancien impact garde les informations correspondantes
                INTBUF_TAB%SECND_FR(6*(N-1)+1:6*N) =ZERO
                INTBUF_TAB%STIF_OLD(2*(N-1)+1:2*N)=ZERO
                INTBUF_TAB%PENE_OLD(5*(N-1)+1)  =ZERO
                INTBUF_TAB%PENE_OLD(5*(N-1)+2)  =ZERO
                INTBUF_TAB%PENE_OLD(5*(N-1)+3)  =ZERO
                INTBUF_TAB%PENE_OLD(5*(N-1)+4)  =ZERO
                INTBUF_TAB%PENE_OLD(5*(N-1)+5)  =ZERO
C
C               se prepare a quitter le contact
                INTBUF_TAB%TIME_S(2*(N-1)+1) = EP20
                INTBUF_TAB%TIME_S(2*(N-1)+2) = EP20
              END IF
            ELSE ! IF(INTBUF_TAB%IRTLM(4*(N-1)+1) > 0)THEN 
C reset all for future impact
c              INTBUF_TAB%IRTLM(4*(N-1)+3)=0
c              INTBUF_TAB%SECND_FR(6*(N-1)+1:6*N) =ZERO
c              INTBUF_TAB%STIF_OLD(2*(N-1)+1:2*N)= ZERO
c              INTBUF_TAB%PENE_OLD(5*(N-1)+1:5*N)= ZERO
               INTBUF_TAB%PENE_OLD(5*(N-1)+3)  =ZERO
               INTBUF_TAB%PENE_OLD(5*(N-1)+4)  =ZERO
               INTBUF_TAB%TIME_S(2*(N-1)+1) = -EP20
               INTBUF_TAB%TIME_S(2*(N-1)+2) =  EP20
            END IF
          END DO
C
          DO N=1,NSNR
c             if(itafi(nin)%p(n)==28552)
c     .       print *,'rem',nin,ipari(15,nin),ispmd+1,IRTLM_FI(NIN)%P(1,N),IRTLM_FI(NIN)%P(4,N),
c     .       PENE_OLDFI(NIN)%P(5,N)
            IF(IRTLM_FI(NIN)%P(1,N) > 0)THEN
              IF(STIFI(NIN)%P(N)==ZERO)THEN
C  
C               Shooting Nodes
                IRTLM_FI(NIN)%P(1:4,N)=0
              ELSEIF(IRTLM_FI(NIN)%P(4,N) == ISPMD+1)THEN
                L = IRTLM_FI(NIN)%P(3,N)
                IF(INTBUF_TAB%STFM(L)==ZERO)THEN
C
C                 Reset IRTLM when a Secnd node is in contact with a Main surface which was deleted.
                  IRTLM_FI(NIN)%P(1,N)=0
                  IRTLM_FI(NIN)%P(2,N)=0
C
C                 The segment where the node was impacted has been deleted 
C                 at the previous cycle => the node can not impact during THIS cycle, but the next cycle ONLY
                  IRTLM_FI(NIN)%P(3,N)  = -1
                  IRTLM_FI(NIN)%P(4,N)  =  0
                  TIME_SFI(NIN)%P(2*(N-1)+1) =  EP20
                  TIME_SFI(NIN)%P(2*(N-1)+2) =  EP20
C
          	  SECND_FRFI(NIN)%P (1:6,N)=ZERO
          	  PENE_OLDFI(NIN)%P(1:5,N)=ZERO
          	  STIF_OLDFI(NIN)%P(1:2,N)=ZERO
C
                ELSE
                  I_STOK_RTLM=I_STOK_RTLM+1
                  INTBUF_TAB%CAND_OPT_N(I_STOK_RTLM)= NSN+N
                  INTBUF_TAB%CAND_OPT_E(I_STOK_RTLM)= L
C
C copy old friction forces
                  SECND_FRFI(NIN)%P(4:6,N)=SECND_FRFI(NIN)%P(1:3,N)
C set new friction forces at 0
                  SECND_FRFI(NIN)%P(1:3,N)=ZERO
                  PENE_OLDFI(NIN)%P(2,N) = PENE_OLDFI(NIN)%P(1,N)
                  PENE_OLDFI(NIN)%P(1,N) = ZERO
                  STIF_OLDFI(NIN)%P(2,N) = STIF_OLDFI(NIN)%P(1,N)
                  STIF_OLDFI(NIN)%P(1,N) = ZERO
C
C                 se prepare a quitter le contact
                  TIME_SFI(NIN)%P(2*(N-1)+1) = EP20
                  TIME_SFI(NIN)%P(2*(N-1)+2) = EP20
                END IF
              ELSE ! IF(IRTLM_FI(NIN)%P(4,N) == ISPMD+1)THEN
C
C               Seul le processeur qui a l ancien impact garde les informations correspondantes
                SECND_FRFI(NIN)%P(1:6,N) =ZERO
                STIF_OLDFI(NIN)%P(1:2,N)=ZERO
                PENE_OLDFI(NIN)%P(1,N)  =ZERO
                PENE_OLDFI(NIN)%P(2,N)  =ZERO
                PENE_OLDFI(NIN)%P(3,N)  =ZERO
                PENE_OLDFI(NIN)%P(4,N)  =ZERO
                PENE_OLDFI(NIN)%P(5,N)  =ZERO
C
C               se prepare a quitter le contact
                TIME_SFI(NIN)%P(2*(N-1)+1) = EP20
                TIME_SFI(NIN)%P(2*(N-1)+2) = EP20
              END IF
            ELSE ! IF(IRTLM_FI(NIN)%P(1,N) > 0)THEN
C reset all for future impact
c              IRTLM_FI(NIN)%P(3,N)=0
c              SECND_FRFI(NIN)%P (1:6,N)=ZERO
c              PENE_OLDFI(NIN)%P(1:5,N)=ZERO
c              STIF_OLDFI(NIN)%P(1:2,N)=ZERO
               PENE_OLDFI(NIN)%P(3,N)  =ZERO
               PENE_OLDFI(NIN)%P(4,N)  =ZERO
               TIME_SFI(NIN)%P(2*(N-1)+1) = -EP20
               TIME_SFI(NIN)%P(2*(N-1)+2) =  EP20
            END IF
          END DO
C
        ELSE ! IVIS2 == -1 : Interface adhesion case
C
          DO N=1,NSN
c             if(itab(INTBUF_TAB%nsv(n))==10004284)
c     .       print *,'nat',nin,ipari(15,nin),ispmd+1,INTBUF_TAB%IRTLM(4*(N-1)+1),INTBUF_TAB%IRTLM(4*(N-1)+4),
c     .       INTBUF_TAB%PENE_OLD(5*(N-1)+5)
            IF(INTBUF_TAB%IRTLM(4*(N-1)+1) > 0)THEN 
              IF(INTBUF_TAB%STFNS(N)==ZERO)THEN
C
C               Shooting Nodes
                INTBUF_TAB%IRTLM(4*N-3:4*N)=0
              ELSEIF(INTBUF_TAB%IRTLM(4*(N-1)+4) == ISPMD+1)THEN
                L = INTBUF_TAB%IRTLM(4*(N-1)+3)
                IF(INTBUF_TAB%STFM(L)==ZERO)THEN
C
C                 Reset IRTLM when a Secnd node is in contact with a Main surface which was deleted.
                  INTBUF_TAB%IRTLM(4*(N-1)+1)=0
                  INTBUF_TAB%IRTLM(4*(N-1)+2)=0
C
C                 The segment where the node was impacted has been deleted 
C                 at the previous cycle => the node can not impact during THIS cycle, but the next cycle ONLY
                  INTBUF_TAB%IRTLM(4*(N-1)+3)  = -1
                  INTBUF_TAB%IRTLM(4*(N-1)+4)  =  0
                  INTBUF_TAB%TIME_S(2*(N-1)+1) =  EP20
                  INTBUF_TAB%TIME_S(2*(N-1)+2) =  EP20
C
          	  INTBUF_TAB%SECND_FR(6*(N-1)+1:6*N) = ZERO
          	  INTBUF_TAB%STIF_OLD(2*(N-1)+1:2*N)= ZERO
          	  INTBUF_TAB%PENE_OLD(5*(N-1)+1:5*N)= ZERO
                  INTBUF_TAB%IF_ADH(N) = 0    
C
                ELSE
C
C                 N impacte sur mon domaine SPMD, sur le segment n  IRTLM(3,N)
                  I_STOK_RTLM=I_STOK_RTLM+1
                  INTBUF_TAB%CAND_OPT_N(I_STOK_RTLM)= N
                  INTBUF_TAB%CAND_OPT_E(I_STOK_RTLM)= L
C
C copy old friction forces
                  INTBUF_TAB%SECND_FR(6*(N-1)+4:6*N)=INTBUF_TAB%SECND_FR(6*(N-1)+1:6*(N-1)+3)
C set new friction forces at 0
                  INTBUF_TAB%SECND_FR(6*(N-1)+1:6*(N-1)+3)=ZERO
                  INTBUF_TAB%PENE_OLD(5*(N-1)+2) = INTBUF_TAB%PENE_OLD(5*(N-1)+1)
                  INTBUF_TAB%PENE_OLD(5*(N-1)+1) = ZERO
                  INTBUF_TAB%STIF_OLD(2*(N-1)+2) = INTBUF_TAB%STIF_OLD(2*(N-1)+1) 
                  INTBUF_TAB%STIF_OLD(2*(N-1)+1) = ZERO
C
C                 se prepare a quitter le contact
                  INTBUF_TAB%TIME_S(2*(N-1)+1) = EP20
                  INTBUF_TAB%TIME_S(2*(N-1)+2) = EP20
                END IF
              ELSE ! IF(INTBUF_TAB%IRTLM(4*(N-1)+4) == ISPMD+1)THEN
C
C               Seul le processeur qui a l ancien impact garde les informations correspondantes
                INTBUF_TAB%SECND_FR(6*(N-1)+1:6*N) =ZERO
                INTBUF_TAB%STIF_OLD(2*(N-1)+1:2*N)=ZERO
                INTBUF_TAB%PENE_OLD(5*(N-1)+1)  =ZERO
                INTBUF_TAB%PENE_OLD(5*(N-1)+2)  =ZERO
                INTBUF_TAB%PENE_OLD(5*(N-1)+3)  =ZERO
                INTBUF_TAB%PENE_OLD(5*(N-1)+4)  =ZERO
                INTBUF_TAB%PENE_OLD(5*(N-1)+5)  =ZERO
                INTBUF_TAB%IF_ADH(N) = 0
C
C               se prepare a quitter le contact
                INTBUF_TAB%TIME_S(2*(N-1)+1) = EP20
                INTBUF_TAB%TIME_S(2*(N-1)+2) = EP20
              END IF
            ELSE ! IF(INTBUF_TAB%IRTLM(4*(N-1)+1) > 0)THEN 
C reset all for future impact
c              INTBUF_TAB%IRTLM(4*(N-1)+3)=0
c              INTBUF_TAB%SECND_FR(6*(N-1)+1:6*N) =ZERO
c              INTBUF_TAB%STIF_OLD(2*(N-1)+1:2*N)= ZERO
c              INTBUF_TAB%PENE_OLD(5*(N-1)+1:5*N)= ZERO
               INTBUF_TAB%TIME_S(2*(N-1)+1) = -EP20
               INTBUF_TAB%TIME_S(2*(N-1)+2) =  EP20
            END IF
          END DO
C
          DO N=1,NSNR
c             if(itafi(nin)%p(n)==28552)
c     .       print *,'rem',nin,ipari(15,nin),ispmd+1,IRTLM_FI(NIN)%P(1,N),IRTLM_FI(NIN)%P(4,N),
c     .       PENE_OLDFI(NIN)%P(5,N)
            IF(IRTLM_FI(NIN)%P(1,N) > 0)THEN
              IF(STIFI(NIN)%P(N)==ZERO)THEN
C  
C               Shooting Nodes
                IRTLM_FI(NIN)%P(1:4,N)=0
              ELSEIF(IRTLM_FI(NIN)%P(4,N) == ISPMD+1)THEN
                L = IRTLM_FI(NIN)%P(3,N)
                IF(INTBUF_TAB%STFM(L)==ZERO)THEN
C
C                 Reset IRTLM when a Secnd node is in contact with a Main surface which was deleted.
                  IRTLM_FI(NIN)%P(1,N)=0
                  IRTLM_FI(NIN)%P(2,N)=0
C
C                 The segment where the node was impacted has been deleted 
C                 at the previous cycle => the node can not impact during THIS cycle, but the next cycle ONLY
                  IRTLM_FI(NIN)%P(3,N)  = -1
                  IRTLM_FI(NIN)%P(4,N)  =  0
                  TIME_SFI(NIN)%P(2*(N-1)+1) =  EP20
                  TIME_SFI(NIN)%P(2*(N-1)+2) =  EP20
C
          	  SECND_FRFI(NIN)%P (1:6,N)=ZERO
          	  PENE_OLDFI(NIN)%P(1:5,N)=ZERO
          	  STIF_OLDFI(NIN)%P(1:2,N)=ZERO
                  IF_ADHFI(NIN)%P(N) = 0
C
                ELSE
                  I_STOK_RTLM=I_STOK_RTLM+1
                  INTBUF_TAB%CAND_OPT_N(I_STOK_RTLM)= NSN+N
                  INTBUF_TAB%CAND_OPT_E(I_STOK_RTLM)= L
C
C copy old friction forces
                  SECND_FRFI(NIN)%P(4:6,N)=SECND_FRFI(NIN)%P(1:3,N)
C set new friction forces at 0
                  SECND_FRFI(NIN)%P(1:3,N)=ZERO
                  PENE_OLDFI(NIN)%P(2,N) = PENE_OLDFI(NIN)%P(1,N)
                  PENE_OLDFI(NIN)%P(1,N) = ZERO
                  STIF_OLDFI(NIN)%P(2,N) = STIF_OLDFI(NIN)%P(1,N)
                  STIF_OLDFI(NIN)%P(1,N) = ZERO
C
C                 se prepare a quitter le contact
                  TIME_SFI(NIN)%P(2*(N-1)+1) = EP20
                  TIME_SFI(NIN)%P(2*(N-1)+2) = EP20
                END IF
              ELSE ! IF(IRTLM_FI(NIN)%P(4,N) == ISPMD+1)THEN
C
C               Seul le processeur qui a l ancien impact garde les informations correspondantes
                SECND_FRFI(NIN)%P(1:6,N) =ZERO
                STIF_OLDFI(NIN)%P(1:2,N)=ZERO
                PENE_OLDFI(NIN)%P(1,N)  =ZERO
                PENE_OLDFI(NIN)%P(2,N)  =ZERO
                PENE_OLDFI(NIN)%P(3,N)  =ZERO
                PENE_OLDFI(NIN)%P(4,N)  =ZERO
                PENE_OLDFI(NIN)%P(5,N)  =ZERO
                IF_ADHFI(NIN)%P(N) = 0
C
C               se prepare a quitter le contact
                TIME_SFI(NIN)%P(2*(N-1)+1) = EP20
                TIME_SFI(NIN)%P(2*(N-1)+2) = EP20
              END IF
            ELSE ! IF(IRTLM_FI(NIN)%P(1,N) > 0)THEN
C reset all for future impact
c              IRTLM_FI(NIN)%P(3,N)=0
c              SECND_FRFI(NIN)%P (1:6,N)=ZERO
c              PENE_OLDFI(NIN)%P(1:5,N)=ZERO
c              STIF_OLDFI(NIN)%P(1:2,N)=ZERO
               TIME_SFI(NIN)%P(2*(N-1)+1) = -EP20
               TIME_SFI(NIN)%P(2*(N-1)+2) =  EP20
            END IF
          END DO
C
        ENDIF ! IVIS2 if       
C
        INTBUF_TAB%I_STOK(3) = I_STOK_RTLM
        INTBUF_TAB%I_STOK(2) = I_STOK_RTLM
C
C-----------------------------------------------------------------------
      RETURN
      END

