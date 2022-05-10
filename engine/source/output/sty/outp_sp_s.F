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
Chd|  OUTP_SP_S                     source/output/sty/outp_sp_s.F 
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        SPMD_RGATHER9_1COMM           source/mpi/interfaces/spmd_outp.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE OUTP_SP_S(NBX  ,KEY   ,TEXT,ELBUF_TAB,IPARG,
     2                     EANI,DD_IAD,KXSP,IPM ,
     3                     SPBUF,SIZLOC,SIZP0,SIZ_WR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD         
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "scr01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "sphcom.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*10 KEY
      CHARACTER*40 TEXT
      INTEGER NBX,SIZLOC,SIZP0
      INTEGER IPARG(NPARG,*),DD_IAD(NSPMD+1,*),
     .        KXSP(NISP,*), IPM(NPROPMI,*),SIZ_WR
      my_real
     .   EANI(*), SPBUF(NSPBUF,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II(6),JJ,N,NN,NG,NEL,MLW,JJ_OLD,NGF,NGL, LEN,WRTLEN,
     .        NUVAR,IUS,RESP0,RES,COMPTEUR,L,K
      INTEGER, DIMENSION(NSPGROUP) :: JJ_LOC
      INTEGER, DIMENSION(NSPGROUP+1,NSPMD) :: ADRESS 
      my_real
     .   WA(SIZLOC),WAP0(SIZ_WR),WAP0_LOC(SIZP0)
      my_real
     .   FUNC(6),S1 ,S2 ,S3,P,VONM2
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
      TYPE(BUF_MAT_) ,POINTER :: MBUF     
C=======================================================================
       IF (ISPMD == 0) THEN
          WRITE(IUGEO,'(2A)')'/SPHCEL    /SCALAR    /',KEY
          WRITE(IUGEO,'(A)')TEXT
          IF (OUTYY_FMT == 2) THEN
           WRITE(IUGEO,'(A)')'#FORMAT: (1P6E12.5) (VAR(I),I=1,NUMSPH)'
          ELSE
           WRITE(IUGEO,'(A)')'#FORMAT: (1P6E20.13) (VAR(I),I=1,NUMSPH)'
          END IF
       ENDIF
C
       JJ_OLD = 1
       NGF = 1
       NGL = 0
       RESP0=0
       JJ = 0
       COMPTEUR = 0
       DO NN=1,NSPGROUP
         NGL = NGL + DD_IAD(ISPMD+1,NN)
         DO NG = NGF, NGL
          ITY  = IPARG(5,NG)
          IF (ITY == 51) THEN
          CALL INITBUF (IPARG    ,NG      ,                    
     2          MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,  
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,  
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,  
     5          NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,  
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,  
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
           LFT=1
           LLT=NEL
           GBUF  => ELBUF_TAB(NG)%GBUF
           MBUF  => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)
           NUVAR =  ELBUF_TAB(NG)%BUFLY(1)%NVAR_MAT
!
           DO I=1,6
             II(I) = NEL*(I-1)
           ENDDO
!
C
           DO I=LFT,LLT
            JJ = JJ + 1
            N  = I + NFT
            WA(JJ) = ZERO
c
            IF (NBX == -2) THEN    ! Von Mises Stress
               P = - (GBUF%SIG(II(1)+I)
     .             +  GBUF%SIG(II(2)+I)
     .             +  GBUF%SIG(II(3)+I)) / TROIS
               S1 = GBUF%SIG(II(1)+I) + P
               S2 = GBUF%SIG(II(2)+I) + P
               S3 = GBUF%SIG(II(3)+I) + P
               VONM2 = TROIS*(GBUF%SIG(II(4)+I)**2 +               
     .                        GBUF%SIG(II(5)+I)**2 +               
     .                        GBUF%SIG(II(6)+I)**2 +               
     .                UNDEMI*(S1*S1+S2*S2+S3*S3))
               WA(JJ)= SQRT(VONM2)
            ELSEIF (NBX == 1) THEN       ! OFF
              WA(JJ) = GBUF%OFF(I)
            ELSEIF (NBX == 2) THEN       ! pressure
              WA(JJ) = - (GBUF%SIG(II(1)+I)
     .                 +  GBUF%SIG(II(2)+I)
     .                 +  GBUF%SIG(II(3)+I)) / TROIS  
            ELSEIF (NBX == 3) THEN       ! energy
              WA(JJ) = GBUF%EINT(I)
            ELSEIF (NBX == 4) THEN       ! density
              WA(JJ) = GBUF%RHO(I)
            ELSEIF (NBX == 5 .and. GBUF%G_TEMP > 0) THEN ! temperature
              WA(JJ) = GBUF%TEMP(I)
            ELSEIF (NBX == 10 .and. GBUF%G_PLA > 0) THEN ! plastic strain
              WA(JJ) = GBUF%PLA(I)
            ELSEIF (NBX == 20 .and. NUVAR >= 1) THEN   ! USER1
               WA(JJ) = MBUF%VAR(I)
            ELSEIF (NBX == 21 .and. NUVAR >= 2) THEN   ! USER2
              IUS = 1
              WA(JJ) = MBUF%VAR(IUS*NEL+I)
            ELSEIF (NBX == 22 .and. NUVAR >= 3) THEN   ! USER3
              IUS = 2
              WA(JJ) = MBUF%VAR(IUS*NEL+I)
            ELSEIF (NBX == 23 .and. NUVAR >= 4) THEN   ! USER4
              IUS = 3
              WA(JJ) = MBUF%VAR(IUS*NEL+I)
            ELSEIF (NBX == 24 .and. NUVAR >= 5) THEN   ! USER5
              IUS = 4
              WA(JJ) = MBUF%VAR(IUS*NEL+I)
            ELSEIF (NBX == 25) THEN
              WA(JJ) = SPBUF(1,NFT + I)
            ELSEIF (NBX == 26) THEN
C  equivalent stress -  (NON VON MISES / VON MISES)
              IF (GBUF%G_SEQ > 0) THEN  !  non VON MISES
                WA(JJ) = GBUF%SEQ(I)
              ELSE                      ! VON MISES
                P = - (GBUF%SIG(II(1)+I)
     .              +  GBUF%SIG(II(2)+I)
     .              +  GBUF%SIG(II(3)+I)) / TROIS
                S1 = GBUF%SIG(II(1)+I) + P
                S2 = GBUF%SIG(II(2)+I) + P
                S3 = GBUF%SIG(II(3)+I) + P
                VONM2 = TROIS*(GBUF%SIG(II(4)+I)**2 +               
     .                         GBUF%SIG(II(5)+I)**2 +               
     .                         GBUF%SIG(II(6)+I)**2 +               
     .                  UNDEMI*(S1*S1+S2*S2+S3*S3))
                WA(JJ)= SQRT(VONM2)
              ENDIF
            ENDIF
           ENDDO ! I=LFT,LLT
          ENDIF
         ENDDO  !  NG = NGF, NGL
c---
         NGF = NGL + 1
        JJ_LOC(NN) = JJ - COMPTEUR            ! size of each group
        COMPTEUR = JJ                         
       ENDDO
!     ++++++++++
       IF( NSPMD>1 ) THEN
        CALL SPMD_RGATHER9_1COMM(WA,JJ,JJ_LOC,WAP0_LOC,SIZP0,ADRESS)
       ELSE
        WAP0_LOC(1:JJ) = WA(1:JJ)
        ADRESS(1,1) = 1
        DO NN = 2,NSPGROUP+1
         ADRESS(NN,1) = JJ_LOC(NN-1) + ADRESS(NN-1,1)
        ENDDO
       ENDIF
!     ++++++++++
       IF(ISPMD.EQ.0) THEN
         RESP0 = 0
         DO NN=1,NSPGROUP
          COMPTEUR = 0
          DO K = 1,NSPMD
           IF((ADRESS(NN+1,K)-1-ADRESS(NN,K))>=0) THEN
            DO L = ADRESS(NN,K),ADRESS(NN+1,K)-1
             COMPTEUR = COMPTEUR + 1
             WAP0(COMPTEUR+RESP0) = WAP0_LOC(L)
            ENDDO  ! l=... , ...
           ENDIF   !if(size_loc>0)
          ENDDO    ! k=1,nspmd

         JJ_OLD = COMPTEUR+RESP0
c---
         IF(JJ_OLD.GT.0) THEN
           RES = MOD(JJ_OLD,6)
           WRTLEN = JJ_OLD-RES
          IF (WRTLEN > 0) THEN
           IF (OUTYY_FMT == 2) THEN
              WRITE(IUGEO,'(1P6E12.5)')(WAP0(J),J=1,WRTLEN)
            ELSE
              WRITE(IUGEO,'(1P6E20.13)')(WAP0(J),J=1,WRTLEN)
            END IF
          END IF
          DO I=1,RES
            WAP0(I)=WAP0(WRTLEN+I)
          ENDDO
          RESP0=RES
         END IF
        ENDDO   ! nn=1,nspgroup

        IF (RESP0 > 0) THEN
          IF (OUTYY_FMT == 2) THEN                           
            WRITE(IUGEO,'(1P6E12.5)')(WAP0(J),J=1,RESP0)   
          ELSE                                               
            WRITE(IUGEO,'(1P6E20.13)')(WAP0(J),J=1,RESP0)  
          ENDIF                                              
        ENDIF
      ENDIF    ! ispmd=0
c-----------
      RETURN
      END
Chd|====================================================================
Chd|  COUNT_ARSZ_SPS                source/output/sty/outp_sp_s.F 
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|        OUTP_ARSZ_SPS                 source/mpi/interfaces/spmd_outp.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE COUNT_ARSZ_SPS(IPARG,DD_IAD,WASZ,SIZ_WRITE_LOC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IPARG(NPARG,*),DD_IAD(NSPMD+1,*),WASZ,
     .          SIZ_WRITE_LOC(NSPGROUP+1)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGF,NGL,NN,ITY,NEL,NG,JJ,
     .        P0ARSZ2,WASZ2
C-----------------------------------------------
      WASZ = 0

      IF (OUTP_SPS( 1) == 1.OR.OUTP_SPS( 2) == 1.OR.
     .    OUTP_SPS( 3) == 1.OR.OUTP_SPS( 4) == 1.OR.
     .    OUTP_SPS( 5) == 1.OR.OUTP_SPS( 6) == 1.OR.
     .    OUTP_SPS( 7) == 1.OR.OUTP_SPS(25) == 1.OR.
     .    OUTP_SPS(20) == 1.OR.OUTP_SPS(21) == 1.OR.
     .    OUTP_SPS(22) == 1.OR.OUTP_SPS(23) == 1.OR.
     .    OUTP_SPS(24) == 1 ) THEN

        NGF = 1
        NGL = 0
        DO NN=1,NSPGROUP
         JJ = 0
         NGL = NGL + DD_IAD(ISPMD+1,NN)
         DO NG = NGF, NGL
          ITY   =IPARG(5,NG)
          IF(ITY == 51) THEN
            NEL = IPARG(2,NG)
            JJ = JJ + NEL
          ENDIF
         ENDDO 
         NGF = NGL + 1
         WASZ = WASZ + JJ
         SIZ_WRITE_LOC(NN) = JJ
        ENDDO
        SIZ_WRITE_LOC(NSPGROUP+1) = WASZ
      ENDIF 
      RETURN
      END
