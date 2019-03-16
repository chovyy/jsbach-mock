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
  USE mo_exception,           ONLY: message
  USE mo_jsb_interface,       ONLY: jsbach_interface_original => jsbach_interface
  USE m_serialize

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: jsbach_interface, jsbach_start_timestep, jsbach_finish_timestep, &
            jsbmock_start_capture, jsbmock_start_replay, jsbmock_stop, jsbmock_capture_active, jsbmock_replay_active

  INTERFACE jsbach_interface
    MODULE PROCEDURE interface_full
    MODULE PROCEDURE interface_inquire
  END INTERFACE jsbach_interface

  TYPE(t_serializer) :: jsbmock_serializer
  LOGICAL :: jsbmock_capture_enabled = .FALSE.
  LOGICAL :: jsbmock_replay_enabled = .FALSE.
  INTEGER :: counter = 0

  CHARACTER(len=*), PARAMETER :: savepoint_prefix = 'jsbach_'
  CHARACTER(len=*), PARAMETER :: modname = 'mo_jsb_interface_mock'

CONTAINS

  SUBROUTINE interface_full(model_id, iblk, ics, ice, &
    & dtime, steplen, t_air, q_air, rain, snow, wind_air, wind_10m, lw_srf_down, swvis_srf_down, swnir_srf_down,               &
    & swpar_srf_down, fract_par_diffuse, press_srf, drag_srf, t_acoef, t_bcoef, q_acoef, q_bcoef, pch, cos_zenith_angle,       &
    & CO2_air, &
    & t_srf, t_eff_srf, qsat_srf, s_srf, fact_q_air, fact_qsat_srf, &
    & evapotrans, latent_hflx, sensible_hflx, grnd_hflx, grnd_hcap, rough_h_srf, rough_m_srf, q_snocpymlt, alb_vis_dir,        &
    & alb_nir_dir, alb_vis_dif, alb_nir_dif, CO2_flux,                                                                         &
    & drag_wtr, drag_ice, t_acoef_wtr, t_bcoef_wtr, q_acoef_wtr, q_bcoef_wtr, t_acoef_ice, t_bcoef_ice, q_acoef_ice,           &
    & q_bcoef_ice, &
    & t_lwtr, qsat_lwtr, evapo_wtr, latent_hflx_wtr, sensible_hflx_wtr, albedo_lwtr, t_lice, qsat_lice, evapo_ice,             &
    & latent_hflx_ice, sensible_hflx_ice, albedo_lice, ice_fract_lake, &
    & evapopot)

    INTEGER, INTENT(in) :: model_id, iblk, ics, ice
    REAL(wp), INTENT(in) :: dtime, steplen, t_air(:), q_air(:), rain(:), snow(:), wind_air(:), wind_10m(:), lw_srf_down(:),    &
      & swvis_srf_down(:), swnir_srf_down(:), swpar_srf_down(:), fract_par_diffuse(:), press_srf(:), drag_srf(:), t_acoef(:),  &
      & t_bcoef(:), q_acoef(:), q_bcoef(:), pch(:), cos_zenith_angle(:), CO2_air(:)
    REAL(wp), INTENT(out) :: t_srf(:), t_eff_srf(:), qsat_srf(:), s_srf(:), fact_q_air(:), fact_qsat_srf(:), evapotrans(:),    &
      & latent_hflx(:), sensible_hflx(:), grnd_hflx(:), grnd_hcap(:), rough_h_srf(:), rough_m_srf(:), q_snocpymlt(:),          &
      & alb_vis_dir(:), alb_nir_dir(:), alb_vis_dif(:), alb_nir_dif(:), CO2_flux(:)
    REAL(wp), INTENT(in), OPTIONAL :: drag_wtr(:), drag_ice(:), t_acoef_wtr(:), t_bcoef_wtr(:), q_acoef_wtr(:),                &
      & q_bcoef_wtr(:), t_acoef_ice(:), t_bcoef_ice(:), q_acoef_ice(:), q_bcoef_ice(:)
    REAL(wp), INTENT(out), OPTIONAL :: t_lwtr(:), qsat_lwtr(:), evapo_wtr(:), latent_hflx_wtr(:), sensible_hflx_wtr(:),        &
      & albedo_lwtr(:), t_lice(:), qsat_lice(:), evapo_ice(:), latent_hflx_ice(:), sensible_hflx_ice(:), albedo_lice(:),       &
      & ice_fract_lake(:)
    REAL(wp), INTENT(out), OPTIONAL :: evapopot(:)

    CHARACTER(len=*), PARAMETER :: routine = modname//':interface_full'

    IF (jsbmock_replay_enabled) THEN
      CALL message(TRIM(routine), 'Replay JSBACH output')
      CALL jsbmock_read(t_srf, t_eff_srf, qsat_srf, s_srf, fact_q_air, fact_qsat_srf, &
        & evapotrans, latent_hflx, sensible_hflx, grnd_hflx, grnd_hcap, rough_h_srf, rough_m_srf, q_snocpymlt, alb_vis_dir,    &
        & alb_nir_dir, alb_vis_dif, alb_nir_dif, CO2_flux,                                                                     &
        & t_lwtr, qsat_lwtr, evapo_wtr, latent_hflx_wtr, sensible_hflx_wtr, albedo_lwtr, t_lice, qsat_lice, evapo_ice,         &
        & latent_hflx_ice, sensible_hflx_ice, albedo_lice, ice_fract_lake, &
        & evapopot)
    ELSE
      CALL jsbach_interface_original(model_id, iblk, ics, ice, dtime, steplen, t_air, q_air, rain, snow, wind_air, wind_10m,   &
        & lw_srf_down, swvis_srf_down, swnir_srf_down, swpar_srf_down, fract_par_diffuse, press_srf, drag_srf, t_acoef,        &
        & t_bcoef, q_acoef, q_bcoef, pch, cos_zenith_angle, CO2_air, t_srf, t_eff_srf, qsat_srf, s_srf, fact_q_air,            &
        & fact_qsat_srf, evapotrans, latent_hflx, sensible_hflx, grnd_hflx, grnd_hcap, rough_h_srf, rough_m_srf, q_snocpymlt,  &
        & alb_vis_dir, alb_nir_dir, alb_vis_dif, alb_nir_dif, CO2_flux,                                                        &
        & drag_wtr, drag_ice, t_acoef_wtr, t_bcoef_wtr, q_acoef_wtr, q_bcoef_wtr, t_acoef_ice, t_bcoef_ice, q_acoef_ice,       &
        & q_bcoef_ice, t_lwtr, qsat_lwtr, evapo_wtr, latent_hflx_wtr, sensible_hflx_wtr, albedo_lwtr, t_lice, qsat_lice,       &
        & evapo_ice, latent_hflx_ice, sensible_hflx_ice, albedo_lice, ice_fract_lake, evapopot)

      IF (jsbmock_capture_enabled) THEN
        CALL message(TRIM(routine), 'Capture JSBACH output')
        CALL jsbmock_write(t_srf, t_eff_srf, qsat_srf, s_srf, fact_q_air, fact_qsat_srf, &
          & evapotrans, latent_hflx, sensible_hflx, grnd_hflx, grnd_hcap, rough_h_srf, rough_m_srf, q_snocpymlt, alb_vis_dir,    &
          & alb_nir_dir, alb_vis_dif, alb_nir_dif, CO2_flux,                                                                     &
          & t_lwtr, qsat_lwtr, evapo_wtr, latent_hflx_wtr, sensible_hflx_wtr, albedo_lwtr, t_lice, qsat_lice, evapo_ice,         &
          & latent_hflx_ice, sensible_hflx_ice, albedo_lice, ice_fract_lake, &
          & evapopot)
      END IF
    END IF

  END SUBROUTINE interface_full

  ! Not used currently
  SUBROUTINE interface_inquire( model_id, iblk, ics, ice, t_srf, t_eff_srf, qsat_srf, zh_srf, zm_srf, alb_vis, alb_nir)

    INTEGER, INTENT(in) :: model_id, iblk, ics, ice
    REAL(wp), INTENT(inout) :: t_srf(:), t_eff_srf(:), qsat_srf(:), zh_srf(:), zm_srf(:), alb_vis(:), alb_nir(:)

  END SUBROUTINE interface_inquire

SUBROUTINE jsbach_start_timestep(model_id)

    INTEGER,  INTENT(in) :: model_id

    CHARACTER(len=*), PARAMETER :: routine = modname//':jsbach_start_timestep'

    CALL message(TRIM(routine), 'Do nothing')

  END SUBROUTINE jsbach_start_timestep

  SUBROUTINE jsbach_finish_timestep(model_id, steplen)

    INTEGER, INTENT(in)  :: model_id
    REAL(wp), INTENT(in) :: steplen

    CHARACTER(len=*), PARAMETER :: routine = modname//':jsbach_finish_timestep'

    CALL message(TRIM(routine), 'Do nothing')

  END SUBROUTINE jsbach_finish_timestep

  SUBROUTINE jsbmock_write(t_srf, t_eff_srf, qsat_srf, s_srf, fact_q_air, fact_qsat_srf, &
    & evapotrans, latent_hflx, sensible_hflx, grnd_hflx, grnd_hcap, rough_h_srf, rough_m_srf, q_snocpymlt, alb_vis_dir,        &
    & alb_nir_dir, alb_vis_dif, alb_nir_dif, CO2_flux,                                                                         &
    & t_lwtr, qsat_lwtr, evapo_wtr, latent_hflx_wtr, sensible_hflx_wtr, albedo_lwtr, t_lice, qsat_lice, evapo_ice,             &
    & latent_hflx_ice, sensible_hflx_ice, albedo_lice, ice_fract_lake, &
    & evapopot)

    REAL(wp), INTENT(in) :: t_srf(:), t_eff_srf(:), qsat_srf(:), s_srf(:), fact_q_air(:), fact_qsat_srf(:), evapotrans(:),    &
      & latent_hflx(:), sensible_hflx(:), grnd_hflx(:), grnd_hcap(:), rough_h_srf(:), rough_m_srf(:), q_snocpymlt(:),          &
      & alb_vis_dir(:), alb_nir_dir(:), alb_vis_dif(:), alb_nir_dif(:), CO2_flux(:)
    REAL(wp), INTENT(in), OPTIONAL :: t_lwtr(:), qsat_lwtr(:), evapo_wtr(:), latent_hflx_wtr(:), sensible_hflx_wtr(:),        &
      & albedo_lwtr(:), t_lice(:), qsat_lice(:), evapo_ice(:), latent_hflx_ice(:), sensible_hflx_ice(:), albedo_lice(:),       &
      & ice_fract_lake(:)
    REAL(wp), INTENT(in), OPTIONAL :: evapopot(:)

    TYPE(t_savepoint) :: savepoint

    savepoint = jsbmock_create_next_savepoint()

    CALL fs_write_field(jsbmock_serializer, savepoint, 't_srf', t_srf)
    CALL fs_write_field(jsbmock_serializer, savepoint, 't_eff_srf', t_eff_srf)
    CALL fs_write_field(jsbmock_serializer, savepoint, 'qsat_srf', qsat_srf)
    CALL fs_write_field(jsbmock_serializer, savepoint, 's_srf', s_srf)
    CALL fs_write_field(jsbmock_serializer, savepoint, 'fact_q_air', fact_q_air)
    CALL fs_write_field(jsbmock_serializer, savepoint, 'fact_qsat_srf', fact_qsat_srf)
    CALL fs_write_field(jsbmock_serializer, savepoint, 'evapotrans', evapotrans)
    CALL fs_write_field(jsbmock_serializer, savepoint, 'latent_hflx', latent_hflx)
    CALL fs_write_field(jsbmock_serializer, savepoint, 'sensible_hflx', sensible_hflx)
    CALL fs_write_field(jsbmock_serializer, savepoint, 'grnd_hflx', grnd_hflx)
    CALL fs_write_field(jsbmock_serializer, savepoint, 'grnd_hcap', grnd_hcap)
    CALL fs_write_field(jsbmock_serializer, savepoint, 'rough_h_srf', rough_h_srf)
    CALL fs_write_field(jsbmock_serializer, savepoint, 'rough_m_srf', rough_m_srf)
    CALL fs_write_field(jsbmock_serializer, savepoint, 'q_snocpymlt', q_snocpymlt)
    CALL fs_write_field(jsbmock_serializer, savepoint, 'alb_vis_dir', alb_vis_dir)
    CALL fs_write_field(jsbmock_serializer, savepoint, 'alb_nir_dir', alb_nir_dir)
    CALL fs_write_field(jsbmock_serializer, savepoint, 'alb_vis_dif', alb_vis_dif)
    CALL fs_write_field(jsbmock_serializer, savepoint, 'alb_nir_dif', alb_nir_dif)
    CALL fs_write_field(jsbmock_serializer, savepoint, 'CO2_flux', CO2_flux)

    IF (PRESENT(t_lwtr)) THEN
      CALL fs_write_field(jsbmock_serializer, savepoint, 't_lwtr', t_lwtr)
    END IF
    IF (PRESENT(qsat_lwtr)) THEN
      CALL fs_write_field(jsbmock_serializer, savepoint, 'qsat_lwtr', qsat_lwtr)
    END IF
    IF (PRESENT(evapo_wtr)) THEN
      CALL fs_write_field(jsbmock_serializer, savepoint, 'evapo_wtr', evapo_wtr)
    END IF
    IF (PRESENT(latent_hflx_wtr)) THEN
      CALL fs_write_field(jsbmock_serializer, savepoint, 'latent_hflx_wtr', latent_hflx_wtr)
    END IF
    IF (PRESENT(sensible_hflx_wtr)) THEN
      CALL fs_write_field(jsbmock_serializer, savepoint, 'sensible_hflx_wtr', sensible_hflx_wtr)
    END IF
    IF (PRESENT(albedo_lwtr)) THEN
      CALL fs_write_field(jsbmock_serializer, savepoint, 'albedo_lwtr', albedo_lwtr)
    END IF
    IF (PRESENT(t_lice)) THEN
      CALL fs_write_field(jsbmock_serializer, savepoint, 't_lice', t_lice)
    END IF
    IF (PRESENT(qsat_lice)) THEN
      CALL fs_write_field(jsbmock_serializer, savepoint, 'qsat_lice', qsat_lice)
    END IF
    IF (PRESENT(evapo_ice)) THEN
      CALL fs_write_field(jsbmock_serializer, savepoint, 'evapo_ice', evapo_ice)
    END IF
    IF (PRESENT(latent_hflx_ice)) THEN
      CALL fs_write_field(jsbmock_serializer, savepoint, 'latent_hflx_ice', latent_hflx_ice)
    END IF
    IF (PRESENT(sensible_hflx_ice)) THEN
      CALL fs_write_field(jsbmock_serializer, savepoint, 'sensible_hflx_ice', sensible_hflx_ice)
    END IF
    IF (PRESENT(albedo_lice)) THEN
      CALL fs_write_field(jsbmock_serializer, savepoint, 'albedo_lice', albedo_lice)
    END IF
    IF (PRESENT(ice_fract_lake)) THEN
      CALL fs_write_field(jsbmock_serializer, savepoint, 'ice_fract_lake', ice_fract_lake)
    END IF

    IF (PRESENT(evapopot)) THEN
      CALL fs_write_field(jsbmock_serializer, savepoint, 'evapopot', evapopot)
    END IF

    CALL fs_destroy_savepoint(savepoint)

  END SUBROUTINE jsbmock_write

  SUBROUTINE jsbmock_read(t_srf, t_eff_srf, qsat_srf, s_srf, fact_q_air, fact_qsat_srf, &
    & evapotrans, latent_hflx, sensible_hflx, grnd_hflx, grnd_hcap, rough_h_srf, rough_m_srf, q_snocpymlt, alb_vis_dir,        &
    & alb_nir_dir, alb_vis_dif, alb_nir_dif, CO2_flux,                                                                         &
    & t_lwtr, qsat_lwtr, evapo_wtr, latent_hflx_wtr, sensible_hflx_wtr, albedo_lwtr, t_lice, qsat_lice, evapo_ice,             &
    & latent_hflx_ice, sensible_hflx_ice, albedo_lice, ice_fract_lake, &
    & evapopot)

    REAL(wp), INTENT(out) :: t_srf(:), t_eff_srf(:), qsat_srf(:), s_srf(:), fact_q_air(:), fact_qsat_srf(:), evapotrans(:),    &
      & latent_hflx(:), sensible_hflx(:), grnd_hflx(:), grnd_hcap(:), rough_h_srf(:), rough_m_srf(:), q_snocpymlt(:),          &
      & alb_vis_dir(:), alb_nir_dir(:), alb_vis_dif(:), alb_nir_dif(:), CO2_flux(:)
    REAL(wp), INTENT(out), OPTIONAL :: t_lwtr(:), qsat_lwtr(:), evapo_wtr(:), latent_hflx_wtr(:), sensible_hflx_wtr(:),        &
      & albedo_lwtr(:), t_lice(:), qsat_lice(:), evapo_ice(:), latent_hflx_ice(:), sensible_hflx_ice(:), albedo_lice(:),       &
      & ice_fract_lake(:)
    REAL(wp), INTENT(out), OPTIONAL :: evapopot(:)

    TYPE(t_savepoint) :: savepoint

    savepoint = jsbmock_create_next_savepoint()

    CALL fs_read_field(jsbmock_serializer, savepoint, 't_srf', t_srf)
    CALL fs_read_field(jsbmock_serializer, savepoint, 't_eff_srf', t_eff_srf)
    CALL fs_read_field(jsbmock_serializer, savepoint, 'qsat_srf', qsat_srf)
    CALL fs_read_field(jsbmock_serializer, savepoint, 's_srf', s_srf)
    CALL fs_read_field(jsbmock_serializer, savepoint, 'fact_q_air', fact_q_air)
    CALL fs_read_field(jsbmock_serializer, savepoint, 'fact_qsat_srf', fact_qsat_srf)
    CALL fs_read_field(jsbmock_serializer, savepoint, 'evapotrans', evapotrans)
    CALL fs_read_field(jsbmock_serializer, savepoint, 'latent_hflx', latent_hflx)
    CALL fs_read_field(jsbmock_serializer, savepoint, 'sensible_hflx', sensible_hflx)
    CALL fs_read_field(jsbmock_serializer, savepoint, 'grnd_hflx', grnd_hflx)
    CALL fs_read_field(jsbmock_serializer, savepoint, 'grnd_hcap', grnd_hcap)
    CALL fs_read_field(jsbmock_serializer, savepoint, 'rough_h_srf', rough_h_srf)
    CALL fs_read_field(jsbmock_serializer, savepoint, 'rough_m_srf', rough_m_srf)
    CALL fs_read_field(jsbmock_serializer, savepoint, 'q_snocpymlt', q_snocpymlt)
    CALL fs_read_field(jsbmock_serializer, savepoint, 'alb_vis_dir', alb_vis_dir)
    CALL fs_read_field(jsbmock_serializer, savepoint, 'alb_nir_dir', alb_nir_dir)
    CALL fs_read_field(jsbmock_serializer, savepoint, 'alb_vis_dif', alb_vis_dif)
    CALL fs_read_field(jsbmock_serializer, savepoint, 'alb_nir_dif', alb_nir_dif)
    CALL fs_read_field(jsbmock_serializer, savepoint, 'CO2_flux', CO2_flux)

    IF (PRESENT(t_lwtr)) THEN
      CALL fs_read_field(jsbmock_serializer, savepoint, 't_lwtr', t_lwtr)
    END IF
    IF (PRESENT(qsat_lwtr)) THEN
      CALL fs_read_field(jsbmock_serializer, savepoint, 'qsat_lwtr', qsat_lwtr)
    END IF
    IF (PRESENT(evapo_wtr)) THEN
      CALL fs_read_field(jsbmock_serializer, savepoint, 'evapo_wtr', evapo_wtr)
    END IF
    IF (PRESENT(latent_hflx_wtr)) THEN
      CALL fs_read_field(jsbmock_serializer, savepoint, 'latent_hflx_wtr', latent_hflx_wtr)
    END IF
    IF (PRESENT(sensible_hflx_wtr)) THEN
      CALL fs_read_field(jsbmock_serializer, savepoint, 'sensible_hflx_wtr', sensible_hflx_wtr)
    END IF
    IF (PRESENT(albedo_lwtr)) THEN
      CALL fs_read_field(jsbmock_serializer, savepoint, 'albedo_lwtr', albedo_lwtr)
    END IF
    IF (PRESENT(t_lice)) THEN
      CALL fs_read_field(jsbmock_serializer, savepoint, 't_lice', t_lice)
    END IF
    IF (PRESENT(qsat_lice)) THEN
      CALL fs_read_field(jsbmock_serializer, savepoint, 'qsat_lice', qsat_lice)
    END IF
    IF (PRESENT(evapo_ice)) THEN
      CALL fs_read_field(jsbmock_serializer, savepoint, 'evapo_ice', evapo_ice)
    END IF
    IF (PRESENT(latent_hflx_ice)) THEN
      CALL fs_read_field(jsbmock_serializer, savepoint, 'latent_hflx_ice', latent_hflx_ice)
    END IF
    IF (PRESENT(sensible_hflx_ice)) THEN
      CALL fs_read_field(jsbmock_serializer, savepoint, 'sensible_hflx_ice', sensible_hflx_ice)
    END IF
    IF (PRESENT(albedo_lice)) THEN
      CALL fs_read_field(jsbmock_serializer, savepoint, 'albedo_lice', albedo_lice)
    END IF
    IF (PRESENT(ice_fract_lake)) THEN
      CALL fs_read_field(jsbmock_serializer, savepoint, 'ice_fract_lake', ice_fract_lake)
    END IF

    IF (PRESENT(evapopot)) THEN
      CALL fs_read_field(jsbmock_serializer, savepoint, 'evapopot', evapopot)
    END IF

    CALL fs_destroy_savepoint(savepoint)

  END SUBROUTINE jsbmock_read

  FUNCTION jsbmock_create_next_savepoint()
    TYPE(t_savepoint) :: jsbmock_create_next_savepoint
    CHARACTER(len=16) :: name

    counter = counter + 1
    WRITE (name, '(A,I0)') savepoint_prefix, counter
    CALL fs_create_savepoint(name, jsbmock_create_next_savepoint)

  END FUNCTION jsbmock_create_next_savepoint

  SUBROUTINE jsbmock_start_capture(directory, prefix, append)

    CHARACTER(LEN=*), INTENT(IN)  :: directory, prefix
    LOGICAL, INTENT(in), OPTIONAL :: append

    CHARACTER                     :: mode

    IF (.NOT. jsbmock_replay_enabled) THEN
      mode = 'w'
      IF (PRESENT(append)) THEN
        IF (append) THEN
          mode = 'a'
        END IF
      END IF

      CALL fs_create_serializer(directory, prefix, mode, jsbmock_serializer)

      jsbmock_capture_enabled = .TRUE.
    END IF

  END SUBROUTINE jsbmock_start_capture

  SUBROUTINE jsbmock_start_replay(directory, prefix)

    CHARACTER(LEN=*), INTENT(IN)  :: directory, prefix

    CALL fs_create_serializer(directory, prefix, 'r', jsbmock_serializer)

    jsbmock_replay_enabled = .TRUE.
    jsbmock_capture_enabled = .FALSE.

  END SUBROUTINE jsbmock_start_replay

  SUBROUTINE jsbmock_stop()

    CALL fs_destroy_serializer(jsbmock_serializer)

    jsbmock_capture_enabled = .FALSE.
    jsbmock_replay_enabled = .FALSE.

  END SUBROUTINE jsbmock_stop

  LOGICAL FUNCTION jsbmock_capture_active()
    jsbmock_capture_active = jsbmock_capture_enabled
  END FUNCTION jsbmock_capture_active

  LOGICAL FUNCTION jsbmock_replay_active()
    jsbmock_replay_active = jsbmock_replay_enabled
  END FUNCTION jsbmock_replay_active

END MODULE mo_jsb_interface_mock
