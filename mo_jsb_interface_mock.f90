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

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: jsbach_interface

  INTERFACE jsbach_interface
    MODULE PROCEDURE interface_full
    MODULE PROCEDURE interface_inquire
  END INTERFACE jsbach_interface

  CHARACTER(len=*), PARAMETER :: modname = 'mo_jsb_interface'

CONTAINS

  SUBROUTINE interface_full( &
    & model_id,         &
    & iblk, ics, ice, dtime, steplen, &
    & t_air,            &
    & q_air,            &
    & rain,             &  
    & snow,             &
    & wind_air,         &
    & wind_10m,         &
    & lw_srf_down,      &
    & swvis_srf_down,   &
    & swnir_srf_down,   &
    & swpar_srf_down,   &
    & fract_par_diffuse, &
    & press_srf,        &
    & drag_srf,         &
    & t_acoef,          &
    & t_bcoef,          &
    & q_acoef,          &
    & q_bcoef,          &
    & pch,              &
    & cos_zenith_angle, &
    & CO2_air,          &
    & t_srf,            &
    & t_eff_srf,        &
    & qsat_srf,         &
    & s_srf,            &
    & fact_q_air,       &
    & fact_qsat_srf,    &
    & evapotrans,       &
    & latent_hflx,      &
    & sensible_hflx,    &
    & grnd_hflx,        &
    & grnd_hcap,        &
    & rough_h_srf,      &
    & rough_m_srf,      &
    & q_snocpymlt,      &
    & alb_vis_dir,      &
    & alb_nir_dir,      &
    & alb_vis_dif,      &
    & alb_nir_dif,      &
    & CO2_flux,         &
    ! For lakes (optional):
    & drag_wtr, drag_ice,                                               & !< in
    & t_acoef_wtr, t_bcoef_wtr, q_acoef_wtr, q_bcoef_wtr,               & !< in
    & t_acoef_ice, t_bcoef_ice, q_acoef_ice, q_bcoef_ice,               & !< in
    & t_lwtr, qsat_lwtr, evapo_wtr, latent_hflx_wtr, sensible_hflx_wtr, & !< out
    & albedo_lwtr,                                                      & !< out
    & t_lice, qsat_lice, evapo_ice, latent_hflx_ice, sensible_hflx_ice, & !< out
    & albedo_lice,                                                      & !< out
    & ice_fract_lake,                                                   & !< out
    ! For standalone jsbach
    & evapopot                                                          & !< out; optional
    & )

    ! Arguments
    INTEGER, INTENT(in) :: &
      & model_id,          &
      & iblk,              &
      & ics,               &
      & ice

    REAL(wp), INTENT(in) :: dtime, steplen
    REAL(wp), INTENT(in) ::    &
      & t_air             (:), &
      & q_air             (:), &
      & rain              (:), &
      & snow              (:), &
      & wind_air          (:), &
      & wind_10m          (:), &
      & lw_srf_down       (:), &
      & swvis_srf_down    (:), &
      & swnir_srf_down    (:), &
      & swpar_srf_down    (:), &
      & fract_par_diffuse  (:), &
      & press_srf         (:), &
      & drag_srf          (:), &
      & t_acoef           (:), &
      & t_bcoef           (:), &
      & q_acoef           (:), &
      & q_bcoef           (:), &
      & pch               (:), &
      & cos_zenith_angle  (:), &
      & CO2_air           (:)
    ! For lakes:
    REAL(wp), INTENT(in), OPTIONAL ::    &
      & drag_wtr          (:), &
      & drag_ice          (:), &
      & t_acoef_wtr       (:), &
      & t_bcoef_wtr       (:), &
      & q_acoef_wtr       (:), &
      & q_bcoef_wtr       (:), &
      & t_acoef_ice       (:), &
      & t_bcoef_ice       (:), &
      & q_acoef_ice       (:), &
      & q_bcoef_ice       (:)
    REAL(wp), INTENT(out) ::   &
      & t_srf             (:), &
      & t_eff_srf         (:), &
      & qsat_srf          (:), &
      & s_srf             (:), &
      & fact_q_air        (:), &
      & fact_qsat_srf     (:), &
      & evapotrans        (:), &
      & latent_hflx       (:), &
      & sensible_hflx     (:), &
      & grnd_hflx         (:), &
      & grnd_hcap         (:), &
      & rough_h_srf       (:), &
      & rough_m_srf       (:), &
      & q_snocpymlt       (:), &
      & alb_vis_dir       (:), &
      & alb_nir_dir       (:), &
      & alb_vis_dif       (:), &
      & alb_nir_dif       (:), &
      & CO2_flux          (:)
    ! For lakes:
    REAL(wp), INTENT(out), OPTIONAL ::   &
      & t_lwtr            (:), &
      & qsat_lwtr         (:), &
      & evapo_wtr         (:), &
      & latent_hflx_wtr   (:), &
      & sensible_hflx_wtr (:), &
      & albedo_lwtr       (:), &
      & t_lice            (:), &
      & qsat_lice         (:), &
      & evapo_ice         (:), &
      & latent_hflx_ice   (:), &
      & sensible_hflx_ice (:), &
      & albedo_lice       (:), &
      & ice_fract_lake    (:)
    ! For standalone jsbach:
    REAL(wp), INTENT(out), OPTIONAL :: evapopot(:)

  END SUBROUTINE interface_full

  !
  ! Not used currently
  SUBROUTINE interface_inquire( model_id, iblk, ics, ice,         &
    & t_srf, t_eff_srf, qsat_srf, zh_srf, zm_srf, alb_vis, alb_nir)

    CHARACTER(len=*), PARAMETER :: routine = modname//':interface_inquire'

    INTEGER, INTENT(in) :: &
      & model_id,          &
      & iblk,              &
      & ics,               &
      & ice

    REAL(wp), INTENT(inout) :: &
      & t_srf    (:), &
      & t_eff_srf(:), &
      & qsat_srf (:), &
      & zh_srf   (:), &
      & zm_srf   (:), &
      & alb_vis  (:), &
      & alb_nir  (:)

  END SUBROUTINE interface_inquire

END MODULE mo_jsb_interface_mock
