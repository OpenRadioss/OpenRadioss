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
      !||====================================================================
      !||    write_db                  ../common_source/tools/input_output/write_db.F
      !||--- called by ------------------------------------------------------
      !||    alelag_wrest              ../engine/source/output/restart/wrrest.F
      !||    compress_r_nnz            ../common_source/comm/compress_nnz.F
      !||    convwrest                 ../engine/source/output/restart/wrrest.F
      !||    copy_rval                 ../starter/source/restart/ddsplit/inter_tools.F
      !||    crkxfem_wrestanim         ../engine/source/output/restart/wrrest.F
      !||    ddsplit                   ../starter/source/restart/ddsplit/ddsplit.F
      !||    drape_wrest               ../engine/source/output/restart/wrrest.F
      !||    dswrest                   ../engine/source/output/restart/wrrest.F
      !||    dswrestp                  ../starter/source/restart/ddsplit/wrrest.F
      !||    eigwrest                  ../engine/source/output/restart/wrrest.F
      !||    eigwrestp                 ../starter/source/restart/ddsplit/wrrest.F
      !||    fvwrest                   ../engine/source/output/restart/wrrest.F
      !||    fvwrestp                  ../starter/source/restart/ddsplit/fvwrestp.F
      !||    fxbwrest                  ../engine/source/output/restart/wrrest.F
      !||    fxbwrestp                 ../starter/source/restart/ddsplit/wrrest.F
      !||    fxfluxwrest               ../engine/source/constraints/thermic/fxfluxwrest.F
      !||    fxtempwrest               ../engine/source/output/restart/wrrest.F
      !||    impwrest                  ../engine/source/output/restart/wrrest.F
      !||    intfric_wrestr            ../common_source/modules/interfaces/intbuf_fric_mod.F90
      !||    intstamp_wrestr           ../engine/share/modules/intstamp_mod.F
      !||    nfwrest                   ../engine/source/output/restart/wrrest.F
      !||    nfwrestp                  ../starter/source/restart/ddsplit/wrrest.F
      !||    nitschewrest              ../engine/source/output/restart/wrrest.F
      !||    pblast_write_engine       ../common_source/modules/loads/pblast_mod.F
      !||    pblast_write_starter      ../common_source/modules/loads/pblast_mod.F
      !||    plyxfem_wravuply          ../engine/source/output/restart/wrrest.F
      !||    plyxfem_wrest             ../engine/source/output/restart/wrrest.F
      !||    radiawrest                ../engine/source/output/restart/wrrest.F
      !||    rigmatwrest               ../engine/source/output/restart/wrrest.F
      !||    rthbufwrest               ../engine/source/output/restart/wrrest.F
      !||    split_cand_rval           ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_interfaces          ../starter/source/restart/ddsplit/split_interfaces.F
      !||    split_node_rval           ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_seg_rval_i20        ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_xsav                ../starter/source/restart/ddsplit/inter_tools.F
      !||    spmd_savefi               ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_savesph              ../engine/source/mpi/sph/spmd_sptool.F
      !||    spmd_userwi_rest          ../starter/source/user_interface/user_windows_tools.F
      !||    stack_wrest               ../engine/source/output/restart/wrrest.F
      !||    stat_inimap1d_file_spmd   ../engine/source/output/sta/stat_inimap1d_file_spmd.F
      !||    stat_inimap2d_file_spmd   ../engine/source/output/sta/stat_inimap2d_file_spmd.F
      !||    table_wrestr              ../engine/source/tools/curve/table_tools.F
      !||    thcwrest                  ../engine/source/output/restart/wrrest.F
      !||    userwi_write              ../engine/source/user_interface/userwindow_interface_routines.F
      !||    w_alelag                  ../starter/source/restart/ddsplit/w_alelag.F
      !||    w_anim_crk                ../starter/source/restart/ddsplit/w_anim_crk.F
      !||    w_avuply                  ../starter/source/restart/ddsplit/w_avuply.F
      !||    w_bid                     ../starter/source/restart/ddsplit/w_bid.F
      !||    w_bufbric_22              ../engine/source/interfaces/int22/w_bufbric_22.F
      !||    w_bufel                   ../starter/source/restart/ddsplit/w_bufel.F
      !||    w_cluster                 ../engine/source/output/cluster/w_cluster.F
      !||    w_disp0_vel0_cload        ../starter/source/restart/ddsplit/w_disp0_vel0_cload.F
      !||    w_drape                   ../starter/source/restart/ddsplit/w_drape.F
      !||    w_elbuf_str               ../engine/source/elements/elbuf/w_elbuf_str.F
      !||    w_fac                     ../starter/source/restart/ddsplit/w_fac.F
      !||    w_fbft                    ../starter/source/restart/ddsplit/w_fbft.F
      !||    w_fconv                   ../starter/source/restart/ddsplit/w_fconv.F
      !||    w_fixvel                  ../starter/source/restart/ddsplit/w_fixvel.F
      !||    w_fradia                  ../starter/source/restart/ddsplit/w_fradia.F
      !||    w_frbe3                   ../starter/source/restart/ddsplit/w_frbe3.F
      !||    w_fthflux                 ../starter/source/restart/ddsplit/w_fthflux.F
      !||    w_inivel_str              ../starter/source/restart/ddsplit/w_inivel_str.F90
      !||    w_knotloc                 ../starter/source/restart/ddsplit/w_knotloc.F
      !||    w_outmaxn                 ../starter/source/restart/ddsplit/w_outmaxn.F
      !||    w_pxfem                   ../starter/source/restart/ddsplit/w_pxfem.F
      !||    w_r3nloc                  ../starter/source/restart/ddsplit/w_r3nloc.F
      !||    w_reloc                   ../starter/source/restart/ddsplit/w_reloc.F
      !||    w_reloc2                  ../starter/source/restart/ddsplit/w_reloc2.F
      !||    w_rfilloc                 ../starter/source/restart/ddsplit/w_rfilloc.F
      !||    w_rnloc                   ../starter/source/restart/ddsplit/w_rnloc.F
      !||    w_rnnloc                  ../starter/source/restart/ddsplit/w_rnnloc.F
      !||    w_rsph                    ../starter/source/restart/ddsplit/w_rsph.F
      !||    w_rwar                    ../starter/source/restart/ddsplit/w_rwar.F
      !||    w_seatbelts               ../starter/source/restart/ddsplit/w_seatbelts.F
      !||    w_secbuf                  ../starter/source/restart/ddsplit/w_secbuf.F
      !||    w_skwsph                  ../starter/source/restart/ddsplit/w_skwsph.F
      !||    w_skwsub                  ../starter/source/restart/ddsplit/w_skwsub.F
      !||    w_stack                   ../starter/source/restart/ddsplit/w_stack.F
      !||    w_tabmat_prop             ../starter/source/restart/ddsplit/w_tabmat_prop.F
      !||    write_ale_grid            ../common_source/output/restart/write_ale_grid.F90
      !||    write_bcs_wall            ../common_source/output/restart/write_bcs_wall.F90
      !||    write_common_data         ../common_source/modules/boundary_conditions/ebcs_mod.F
      !||    write_data_fluxout        ../common_source/modules/boundary_conditions/ebcs_mod.F
      !||    write_data_gradp0         ../common_source/modules/boundary_conditions/ebcs_mod.F
      !||    write_data_inip           ../common_source/modules/boundary_conditions/ebcs_mod.F
      !||    write_data_iniv           ../common_source/modules/boundary_conditions/ebcs_mod.F
      !||    write_data_inlet          ../common_source/modules/boundary_conditions/ebcs_mod.F
      !||    write_data_normv          ../common_source/modules/boundary_conditions/ebcs_mod.F
      !||    write_data_nrf            ../common_source/modules/boundary_conditions/ebcs_mod.F
      !||    write_data_pres           ../common_source/modules/boundary_conditions/ebcs_mod.F
      !||    write_data_valvin         ../common_source/modules/boundary_conditions/ebcs_mod.F
      !||    write_data_valvout        ../common_source/modules/boundary_conditions/ebcs_mod.F
      !||    write_data_vel            ../common_source/modules/boundary_conditions/ebcs_mod.F
      !||    write_db_debug            ../starter/source/output/tools/write_debug.F
      !||    write_elgroup_param       ../engine/source/output/restart/write_elgroup_param.F
      !||    write_failparam           ../engine/source/output/restart/write_failparam.F
      !||    write_inivel              ../engine/source/output/restart/write_inivel.F90
      !||    write_intbuf              ../engine/source/output/restart/write_intbuf.F
      !||    write_mat_table           ../engine/source/materials/tools/write_mat_table.F
      !||    write_matparam            ../engine/source/output/restart/write_matparam.F
      !||    write_nloc_struct         ../engine/source/output/restart/write_nloc_struct.F
      !||    write_pcyl                ../engine/source/output/restart/write_pcyl.F
      !||    write_real                ../common_source/comm/write_mod.F
      !||    write_real_1d             ../common_source/comm/write_mod.F
      !||    write_real_2d             ../common_source/comm/write_mod.F
      !||    write_real_3d             ../common_source/comm/write_mod.F
      !||    write_sensors             ../engine/source/output/restart/write_sensors.F
      !||    write_units               ../common_source/comm/write_units.F
      !||    write_viscparam           ../engine/source/output/restart/write_viscparam.F
      !||    wrrestp                   ../engine/source/output/restart/wrrestp.F
      !||--- calls      -----------------------------------------------------
      !||    write_db_c                ../common_source/tools/input_output/write_routtines.c
      !||====================================================================
      SUBROUTINE WRITE_DB(A,N)
C ecriture nombre reel : routine tampon
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N
C     REAL
      my_real
     .   A(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N1,N2
      DOUBLE PRECISION R8(1000)
      REAL R4(1000)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      K=0
      N1=N/1000
      IF(ICRAY==1)THEN
C fortran double precision : 128 bits C double : 64 bits
C fortran real : 64 bits C double : 64 bits
       DO I = 1, N1
        DO J = 1, 1000
          K=K+1
          R4(J) = A(K)
        ENDDO
        CALL WRITE_DB_C(R4,1000)
       ENDDO
       N2=N1*1000
       IF(N2/=N)THEN
        DO J = 1, N-N2
          K=K+1
          R4(J) = A(K)
        ENDDO
        CALL WRITE_DB_C(R4,N-N2)
       ENDIF
      ELSE
C fortran double precision : 64 bits C double : 64 bits
       DO I = 1, N1
        DO J = 1, 1000
          K=K+1
          R8(J) = A(K)
        ENDDO
        CALL WRITE_DB_C(R8,1000)
       ENDDO
       N2=N1*1000
       IF(N2/=N)THEN
        DO J = 1, N-N2
          K=K+1
          R8(J) = A(K)
        ENDDO
        CALL WRITE_DB_C(R8,N-N2)
       ENDIF
      ENDIF
C
      RETURN
      END
      !||====================================================================
      !||    write_db_array     ../common_source/tools/input_output/write_db.F
      !||--- called by ------------------------------------------------------
      !||    intfric_wrestr     ../common_source/modules/interfaces/intbuf_fric_mod.F90
      !||--- calls      -----------------------------------------------------
      !||    write_db_c         ../common_source/tools/input_output/write_routtines.c
      !||====================================================================
      SUBROUTINE WRITE_DB_ARRAY(A,N)
C ecriture nombre reel : routine tampon
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
            INTEGER N
C     REAL
            my_real
     .   A(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
            INTEGER I,J,K,N1,N2
            DOUBLE PRECISION R8(1000)
            REAL R4(1000)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
            K=0
            N1=N/1000
            IF(ICRAY==1)THEN
C fortran double precision : 128 bits C double : 64 bits
C fortran real : 64 bits C double : 64 bits
             DO I = 1, N1
              DO J = 1, 1000
                K=K+1
                R4(J) = A(K)
              ENDDO
              CALL WRITE_DB_ARRAY_C(R4,1000)
             ENDDO
             N2=N1*1000
             IF(N2/=N)THEN
              DO J = 1, N-N2
                K=K+1
                R4(J) = A(K)
              ENDDO
              CALL WRITE_DB_ARRAY_C(R4,N-N2)
             ENDIF
            ELSE
C fortran double precision : 64 bits C double : 64 bits
             DO I = 1, N1
              DO J = 1, 1000
                K=K+1
                R8(J) = A(K)
              ENDDO
              CALL WRITE_DB_C(R8,1000)
             ENDDO
             N2=N1*1000
             IF(N2/=N)THEN
              DO J = 1, N-N2
                K=K+1
                R8(J) = A(K)
              ENDDO
              CALL WRITE_DB_C(R8,N-N2)
             ENDIF
            ENDIF
C
            RETURN
            END
      !||====================================================================
      !||    write_dpdb                 ../common_source/tools/input_output/write_db.F
      !||--- called by ------------------------------------------------------
      !||    add_elapsed_time_mon_off   ../engine/source/system/timer.F
      !||    ddsplit                    ../starter/source/restart/ddsplit/ddsplit.F
      !||    intstamp_wrestr            ../engine/share/modules/intstamp_mod.F
      !||    printime                   ../engine/source/system/timer.F
      !||    w_elbuf_str                ../engine/source/elements/elbuf/w_elbuf_str.F
      !||    write_double               ../common_source/comm/write_mod.F
      !||    write_double_1d            ../common_source/comm/write_mod.F
      !||    write_double_2d            ../common_source/comm/write_mod.F
      !||    write_double_3d            ../common_source/comm/write_mod.F
      !||    write_sensors              ../engine/source/output/restart/write_sensors.F
      !||    wrrestp                    ../engine/source/output/restart/wrrestp.F
      !||--- calls      -----------------------------------------------------
      !||    write_db_c                 ../common_source/tools/input_output/write_routtines.c
      !||====================================================================
      SUBROUTINE WRITE_DPDB(A,N)
C ecriture nombre reel : routine tampon
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N
      DOUBLE PRECISION
     .   A(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N1,N2
      DOUBLE PRECISION R8(1000)
      REAL R4(1000)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      K=0
      N1=N/1000
C fortran double precision : 64 bits C double : 64 bits
       DO I = 1, N1
        DO J = 1, 1000
          K=K+1
          R8(J) = A(K)
        ENDDO
        CALL WRITE_DB_C(R8,1000)
       ENDDO
       N2=N1*1000
       IF(N2/=N)THEN
        DO J = 1, N-N2
          K=K+1
          R8(J) = A(K)
        ENDDO
        CALL WRITE_DB_C(R8,N-N2)
       ENDIF
C
      RETURN
      END
