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
Chd|  ANIOFFC_PLY                   source/output/anim/generate/anioffc_ply.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_IGET_PARTN               source/mpi/anim/spmd_iget_partn.F
Chd|        WRITE_C_C                     source/output/tools/sortie_c.c
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE ANIOFFC_PLY( IPLY,  NEL_PLY, ELBUF_TAB,   IPARG,
     .                        IOFF,  EL2FA,   NBF,     IADD,
     .                        NBF_L, NBPART,  IADG,    NODGLOB,
     .                        IPART, IPARTC,  IPARTTG, NBF_PXFEMG,
     .                        IPM   ,IGEO  ,IXC, STACK  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE PLYXFEM_MOD
      USE STACK_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "scr01_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "scr17_c.inc"
#include      "spmd_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),EL2FA(*),NBF,IOFF(*),
     .        IADD(*),NBF_L,NBPART, IADG(NSPMD,*),NODGLOB(*),
     .        IPART(LIPART1,*), IPARTC(*), IPARTTG(*)
      INTEGER IPLY,NEL_PLY,PLYS,NBF_PXFEMG,IXC(NIXC,*),IPM(NPROPMI,*), 
     .        IGEO(NPROPGI,*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (STACK_PLY) :: STACK
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      INTEGER I, NG, NEL, NFT, IAD, ITY, LFT, NPT,
     .        N, J, LLT, MLW, NBX, IP,  NN, K1, K2,MT,JALE, IMID, 
     .        N1,N2,N3,N4, NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,
     .        N_FA, IHBE, SH_IH, ISTRAIN, IEXPAN,ISHPLYXFEM,NELXFE,IE,
     .        PLYELEMS(NUMELC),IPT,ELC,MPT,NGL(MVSIZ),ISUBSTACK,IPMAT_IPLY,
     .        IPMAT, IPPID
     
      INTEGER IOFFBUF(NBF_L),RBUF,NEL3,NEL5,NEL8,NPTM,NN12,NN13,NN14,
     .         NN15,NN16,NN16A,NPG,PID(MVSIZ),MATLY,NUVARR,NUVARD,NBM_S,
     .         IFAILURE,NPTR,NPTS,IS,IR,OFFIPLY
      REAL R4
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY
      TYPE(L_BUFEL_) ,POINTER :: LBUF
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      NPG = 4
      IOFFBUF = -1
      DO PLYS = 1,NPLYPART
         IPLY = INDX_PLY(PLYS)

C-----------------------------------------------
C     PLY ELEMS INIT
C-----------------------------------------------
        PLYELEMS=0
        DO I=1,PLYSHELL(IPLY)%PLYNUMSHELL
          IPT = PLYSHELL(IPLY)%SHELLIPT(I)
          ELC = PLYSHELL(IPLY)%SHID(I)
          PLYELEMS(ELC)=IPT
        ENDDO
        
C-----------------------------------------------
C
cc      NN1 = EPLYXFE*(IPLY - 1) 
      
C-----------------------------------------------
C
        NELXFE = 0
        IE  = 0
        DO NG=1,NGROUP
          MLW   =IPARG(1,NG)
          NEL   =IPARG(2,NG)
          ITY   =IPARG(5,NG)
          NFT   =IPARG(3,NG)
          IAD   =IPARG(4,NG)
          MPT   = IPARG(6,NG)
          ISHPLYXFEM = IPARG(50,NG)
          ISUBSTACK = IPARG(71,NG)
          LFT=1
          LLT=NEL
          NBX = IAD - 1
C        ccc        NPT    =IPARG(6,NG)
C-----------------------------------------------
C       QUAD
C-----------------------------------------------
C-----------------------------------------------
C       COQUES 4 N
C-----------------------------------------------
          IF(ISHPLYXFEM > 0 ) THEN 
C for batoz shell          
           NPTR = ELBUF_TAB(NG)%NPTR
           NPTS = ELBUF_TAB(NG)%NPTS
C           
           GBUF => ELBUF_TAB(NG)%GBUF
           BUFLY => ELBUF_TAB(NG)%BUFLY(IPT) ! Ipt is no right
           IF (ITY == 3) THEN
             IHBE = IPARG(23,NG)
             NPT    =IPARG(6,NG)
             ISTRAIN=IPARG(44,NG)
             IEXPAN=IPARG(49,NG)
             IFAILURE =  IPARG(43,NG)
             N_FA =NEL_PLY
             NELXFE = NELXFE + NEL
             NBX  = NB6
             
              DO I=1,NEL
                PID(I) = IXC(6,NFT + I)
                NGL(I) = IXC(7,NFT + I)
              ENDDO 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
              IF (IHBE >= 11.AND.IHBE <= 19)THEN
                 NEL3 = NEL*3
                 NEL5 = NEL*5
                 NEL8 = NEL*8
                 NPTM = MAX(1,MPT)
                 NN13=NB12 + NPG*NEL8
                 NN14=NN13 + NPG*NEL8*ISTRAIN
                 NN15=NN14 + NPTM*NPG*NEL
                 IF (MPT >= ONE) THEN
                   NN16=NN15 + NPG*MPT*NEL5
                   NN16A=NN16 + 2*MPT*NEL
                 ENDIF
                 NBM_S = 6*NEL*MPT+NEL
                 NUVARR = ZERO
                 NUVARD = ZERO
C                 
                 IPPID  =  2
                 IPMAT  =  IPPID + NPT ! layer material address  ( NLAY = NPT )
                 IPMAT_IPLY = IPMAT + NPT
                  DO J= 1,MPT-1
                     DO I=1,NEL
                      MATLY  = STACK%IGEO(IPMAT + J,ISUBSTACK)
                      NUVARR = MAX(NUVARR,IPM(221,MATLY))
                      MATLY =   STACK%IGEO(IPMAT_IPLY + J,ISUBSTACK)
                      NUVARD = MAX(NUVARD, IPM(221,MATLY))
                    ENDDO
                 ENDDO
                 DO I=1,NEL
                      MATLY  = STACK%IGEO(IPMAT + J,ISUBSTACK)
                      NUVARR = MAX(NUVARR,IPM(221,MATLY))
                 ENDDO
              ENDIF 
               NBM_S = NBM_S + NEL*MPT*NUVARR
               NBX = NN16A  + NBM_S*MAX(NPG,1) 
               IF(IFAILURE == 1)NBX= NBX + NEL 
C                NBX = NN16A  + NBM_S*MAX(NPG,1) + NEL 
C     .                       + MAX(NPG,1)*NUVARD*NPT*NEL       
C-----------------------------------------------
C       COQUES 3 N
C-----------------------------------------------
           ELSEIF(ITY==7)THEN
c it's not available for T3  
             NBX = NBX+16*NEL
           ENDIF
C-----------------------------------------------
           IF(ITY == 3)THEN
             IF(MLW == 0 .OR. MLW == 13)THEN
C-----------------------------------------------
C           DUMMY ELEMENTS
C-----------------------------------------------
               DO I=LFT,LLT
                 N = I + NFT
                 IF(PLYELEMS(N) > 0) THEN
                   IE = IE + 1
                   IOFF(EL2FA(N_FA+IE)) = 1
                 ENDIF
               ENDDO
             ELSE
C-----------------------------------------------
C         OFF  Is Only for batoz for now. Because Plyxfem is available only with Bato z shell
C-----------------------------------------------
              IF(NADMESH==0.OR.(ITY/=3.AND.ITY/=7))THEN
                DO I=LFT,LLT
                  N = I + NFT
                  IPT = PLYELEMS(N)
                  IF(PLYELEMS(N) > 0) THEN
                    IE = IE +1
                    IOFF(EL2FA(N_FA+IE)) = 1 
                    DO IS = 1,NPTS
                        DO IR = 1,NPTR
                              LBUF => ELBUF_TAB(NG)%BUFLY(IPT)%LBUF(IR,IS,1)
                              OFFIPLY = NINT(MIN(GBUF%OFF(I),LBUF%OFF(I),ONE))
                              IOFF(EL2FA(N_FA+IE)) = MIN(IOFF(EL2FA(N_FA+IE)),OFFIPLY)
                        ENDDO
                    ENDDO   
C!!     .               NINT(MIN(BUFLY%DMG(I),ONE))  ! on traite OFF de layer comme DMG
C!!     .               NINT(MIN(BUFEL(NBX+(IPT- 1)*NEL + I),ONE)) 
                  ENDIF
                ENDDO
             ELSE
              DO I=LFT,LLT
               N = I + NFT
               IPT = PLYELEMS(N)
               IF(PLYELEMS(N) > 0) THEN
                 IP=IPARTC(NFT+I)
                 IE = IE + 1
                  IOFF(EL2FA(N_FA+IE)) = 1 
                  IF(IPART(10,IP)>0)THEN
                    DO IS = 1,NPTS
                        DO IR = 1,NPTR 
                           LBUF => ELBUF_TAB(NG)%BUFLY(IPT)%LBUF(IR,IS,1)
                           OFFIPLY = NINT(MIN(GBUF%OFF(I),LBUF%OFF(I),ONE))
                           IOFF(EL2FA(N_FA+IE))=MIN(IOFF(EL2FA(N_FA+IE)),OFFIPLY)
                        ENDDO
                     ENDDO      
cc     .              NINT(MAX(ZERO,MIN(BUFLY%DMG(I),ONE)))
!!     .              NINT(MAX(ZERO,MIN(BUFLY%LBUF(1,1,1)%OFF(I),ONE)))
                  ELSE
                    DO IS = 1,NPTS
                        DO IR = 1,NPTR 
                             LBUF => ELBUF_TAB(NG)%BUFLY(IPT)%LBUF(IR,IS,1)
                             OFFIPLY = NINT(MIN(GBUF%OFF(I),LBUF%OFF(I),ONE))
                             IOFF(EL2FA(N_FA+IE)) =MIN(IOFF(EL2FA(N_FA+IE)),OFFIPLY) 
                        ENDDO
                     ENDDO   
cc     .               NINT(MIN(BUFLY%DMG(I),ONE))
!!     .               NINT(MIN(BUFLY%LBUF(1,1,1)%OFF(I),ONE))
                  END IF
                ENDIF
              ENDDO
             ENDIF
            ENDIF
           ENDIF
C         
          ENDIF
C-----------------------------------------------
        ENDDO
C----------------------------------------------- 
        IADD(IPLY) =  NEL_PLY + IE
        NEL_PLY = NEL_PLY + PLYSHELL(IPLY)%PLYNUMSHELL
      ENDDO
C
      IF (NSPMD==1)THEN
        CALL WRITE_C_C(IOFF,NBF)
      ELSE

         DO I = 1, NBF_L
           IOFFBUF(I) = IOFF(I)
         ENDDO

        IF (ISPMD==0) THEN
          RBUF = NBF_PXFEMG
          CALL SPMD_IGET_PARTN(1,NBF_L,IOFFBUF,NPLYPART,IADG,RBUF,2)
        ELSE
          RBUF = 1
          CALL SPMD_IGET_PARTN(1,NBF_L,IOFFBUF,NPLYPART,IADG,RBUF,2)
        END IF
      ENDIF
      RETURN
      END
