!>
!! @brief Wrapper Interface for JSBACH that capture and replays the model output data
!!
!! @author
!!  Christian Hovy, Universitaet Hamburg / JSBACH interface by Reiner Schnur, MPI-M Hamburg
!!
!! @par Revision History
!! First version by Christian Hovy (2019-02-22)
!!
MODULE mo_jsb_interface_mock

  USE mo_kind,                ONLY: wp
  USE mo_jsb_interface,       ONLY: jsbach_interface_original => jsbach_interface
  USE m_serialize

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: jsbach_interface, jsbmock_start_capture, jsbmock_start_replay, jsbmock_stop

  INTERFACE jsbach_interface
    MODULE PROCEDURE interface_full
    MODULE PROCEDURE interface_inquire
  END INTERFACE jsbach_interface

  TYPE(t_serializer) :: jsbmock_serializer
  TYPE(t_savepoint) :: jsbmock_savepoint
  LOGICAL :: jsbmock_capture_enabled = .FALSE.
  LOGICAL :: jsbmock_replay_enabled = .FALSE.

CONTAINS

  SUBROUTINE interface_full(model_id, iblk, ics, ice, dtime, steplen, t_air, q_air, rain, snow, wind_air, wind_10m,            &
    & lw_srf_down, swvis_srf_down, swnir_srf_down, swpar_srf_down, fract_par_diffuse, press_srf, drag_srf, t_acoef,            &
    & t_bcoef, q_acoef, q_bcoef, pch, cos_zenith_angle, CO2_air, t_srf, t_eff_srf, qsat_srf, s_srf, fact_q_air, fact_qsat_srf, &
    & evapotrans, latent_hflx, sensible_hflx, grnd_hflx, grnd_hcap, rough_h_srf, rough_m_srf, q_snocpymlt, alb_vis_dir,        &
    & alb_nir_dir, alb_vis_dif, alb_nir_dif, CO2_flux,                                                                         &
    & drag_wtr, drag_ice, t_acoef_wtr, t_bcoef_wtr, q_acoef_wtr, q_bcoef_wtr, t_acoef_ice, t_bcoef_ice, q_acoef_ice,           &
    & q_bcoef_ice, t_lwtr, qsat_lwtr, evapo_wtr, latent_hflx_wtr, sensible_hflx_wtr, albedo_lwtr, t_lice, qsat_lice,           &
    & evapo_ice, latent_hflx_ice, sensible_hflx_ice, albedo_lice, ice_fract_lake, evapopot)

    INTEGER, INTENT(in) :: model_id, iblk, ics, ice
    REAL(wp), INTENT(in) :: dtime, steplen, t_air(:), q_air(:), rain(:), snow(:), wind_air(:), wind_10m(:), lw_srf_down(:),    &
      & swvis_srf_down(:), swnir_srf_down(:), swpar_srf_down(:), fract_par_diffuse(:), press_srf(:), drag_srf(:), t_acoef(:),  &
      & t_bcoef(:), q_acoef(:), q_bcoef(:), pch(:), cos_zenith_angle(:), CO2_air(:)
    REAL(wp), INTENT(in), OPTIONAL :: drag_wtr(:), drag_ice(:), t_acoef_wtr(:), t_bcoef_wtr(:), q_acoef_wtr(:),                &
      & q_bcoef_wtr(:), t_acoef_ice(:), t_bcoef_ice(:), q_acoef_ice(:), q_bcoef_ice(:)
    REAL(wp), INTENT(out) :: t_srf(:), t_eff_srf(:), qsat_srf(:), s_srf(:), fact_q_air(:), fact_qsat_srf(:), evapotrans(:),    &
      & latent_hflx(:), sensible_hflx(:), grnd_hflx(:), grnd_hcap(:), rough_h_srf(:), rough_m_srf(:), q_snocpymlt(:),          &
      & alb_vis_dir(:), alb_nir_dir(:), alb_vis_dif(:), alb_nir_dif(:), CO2_flux(:)
    REAL(wp), INTENT(out), OPTIONAL :: t_lwtr(:), qsat_lwtr(:), evapo_wtr(:), latent_hflx_wtr(:), sensible_hflx_wtr(:),        &
      & albedo_lwtr(:), t_lice(:), qsat_lice(:), evapo_ice(:), latent_hflx_ice(:), sensible_hflx_ice(:), albedo_lice(:),       &
      & ice_fract_lake(:)
    REAL(wp), INTENT(out), OPTIONAL :: evapopot(:)

    CALL jsbach_interface_original(model_id, iblk, ics, ice, dtime, steplen, t_air, q_air, rain, snow, wind_air, wind_10m,     &
    & lw_srf_down, swvis_srf_down, swnir_srf_down, swpar_srf_down, fract_par_diffuse, press_srf, drag_srf, t_acoef,            &
    & t_bcoef, q_acoef, q_bcoef, pch, cos_zenith_angle, CO2_air, t_srf, t_eff_srf, qsat_srf, s_srf, fact_q_air, fact_qsat_srf, &
    & evapotrans, latent_hflx, sensible_hflx, grnd_hflx, grnd_hcap, rough_h_srf, rough_m_srf, q_snocpymlt, alb_vis_dir,        &
    & alb_nir_dir, alb_vis_dif, alb_nir_dif, CO2_flux,                                                                         &
    & drag_wtr, drag_ice, t_acoef_wtr, t_bcoef_wtr, q_acoef_wtr, q_bcoef_wtr, t_acoef_ice, t_bcoef_ice, q_acoef_ice,           &
    & q_bcoef_ice, t_lwtr, qsat_lwtr, evapo_wtr, latent_hflx_wtr, sensible_hflx_wtr, albedo_lwtr, t_lice, qsat_lice,           &
    & evapo_ice, latent_hflx_ice, sensible_hflx_ice, albedo_lice, ice_fract_lake, evapopot)

  END SUBROUTINE interface_full

  ! Not used currently
  SUBROUTINE interface_inquire( model_id, iblk, ics, ice, t_srf, t_eff_srf, qsat_srf, zh_srf, zm_srf, alb_vis, alb_nir)

    INTEGER, INTENT(in) :: model_id, iblk, ics, ice
    REAL(wp), INTENT(inout) :: t_srf(:), t_eff_srf(:), qsat_srf(:), zh_srf(:), zm_srf(:), alb_vis(:), alb_nir(:)

  END SUBROUTINE interface_inquire


  SUBROUTINE jsbmock_start_capture(directory, prefix, savepoint, append)

    CHARACTER(LEN=*), INTENT(IN)  :: directory, prefix, savepoint
    LOGICAL, INTENT(in), OPTIONAL :: append

    CHARACTER                     :: mode

    mode = 'w'
    IF (PRESENT(append)) THEN
      IF (append) THEN
        mode = 'a'
      END IF
    END IF

    CALL fs_create_serializer(directory, prefix, mode, jsbmock_serializer)
    CALL fs_create_savepoint(savepoint, jsbmock_savepoint)

    jsbmock_capture_enabled = .TRUE.
    jsbmock_replay_enabled = .FALSE.

  END SUBROUTINE jsbmock_start_capture

  SUBROUTINE jsbmock_start_replay(directory, prefix, savepoint)

    CHARACTER(LEN=*), INTENT(IN)  :: directory, prefix, savepoint

    CALL fs_create_serializer(directory, prefix, 'r', jsbmock_serializer)
    CALL fs_create_savepoint(savepoint, jsbmock_savepoint)

    jsbmock_capture_enabled = .FALSE.
    jsbmock_replay_enabled = .TRUE.

  END SUBROUTINE jsbmock_start_replay

  SUBROUTINE jsbmock_stop()

    CALL fs_destroy_serializer(jsbmock_serializer)
    CALL fs_destroy_savepoint(jsbmock_savepoint)

    jsbmock_capture_enabled = .FALSE.
    jsbmock_replay_enabled = .FALSE.

  END SUBROUTINE jsbmock_stop

END MODULE mo_jsb_interface_mock
