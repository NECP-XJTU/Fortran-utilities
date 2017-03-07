character(len=20) :: git_commit_hash='${GIT_COMMIT_HASH}'
character(len=10) :: code_version='${${PROJECT_NAME}_VERSION}'


integer,parameter :: klp = 8
integer,parameter :: knp = 4

#ifdef DBL_INT
  integer,parameter :: kip = klp
#else
  integer,parameter :: kip = knp
#endif

#define intknd integer(kip)


intknd,parameter :: kdp = 8
intknd,parameter :: kfp = 4

#ifdef DBL_REAL
  integer,parameter :: krp = kdp
#else
  integer,parameter :: krp = kfp
#endif

#define realkd real(krp)


realkd,parameter :: epsd = 1.e-13_krp
realkd,parameter :: epss = 1.e-6_krp

#ifdef DBL_REAL
  realkd,parameter :: epsr = epsd
#else
  realkd,parameter :: epsr = epss
#endif

#define logknd logical


intknd,parameter :: max_word_len = 20
intknd,parameter :: max_line_len = 1500

#define charkd character
#define charSt character(len=*)
#define char10 character(len=10)
#define char20 character(len=20)
#define char50 character(len=50)
#define chr100 character(len=100)
#define chr256 character(len=256)
#define charwd character(len=max_word_len)
#define charln character(len=max_line_len)

intknd, parameter::steady_state_para = 1
intknd, parameter::multi_state_para = 2
intknd, parameter::transient_para = 3

intknd,parameter  :: south = 1
intknd,parameter  :: east  = 2
intknd,parameter  :: north = 3
intknd,parameter  :: west  = 4
intknd,parameter  :: top  = 5
intknd,parameter  :: bottom  = 6

intknd,parameter  :: assembly_modular_param = 1
intknd, parameter :: quarter_assembly_modular_param = 2
intknd, parameter :: cell_modular_param = 3

intknd, parameter :: single_para = 1
intknd, parameter :: subsc_para = 2
intknd, parameter :: ctf_para = 3
intknd, parameter :: necpx_para = 4

intknd, parameter :: cram_para = 1
intknd, parameter :: tta_para = 2
intknd, parameter :: qram_para = 3

intknd, parameter::np_para = 1
intknd, parameter::wp_para = 2
intknd, parameter::de_para = 3
intknd, parameter::endf_para = 4

intknd, parameter::npc_para = 1
intknd, parameter::fpc_para = 2
intknd, parameter::spc_para = 3
intknd, parameter::celi_para = 4

realkd,parameter  :: rzero  = 0.0_krp
realkd,parameter  :: rone   = 1.0_krp
realkd,parameter  :: rtwo   = 2.0_krp
realkd,parameter  :: rfour  = 4.0_krp
realkd,parameter  :: rhalf  = 0.5_krp
realkd,parameter  :: rquart = 0.25_krp
realkd,parameter  :: roctant= 0.125_krp

intknd,parameter  :: izero = 0_kip
intknd,parameter  :: ione  = 1_kip
intknd,parameter  :: itwo  = 2_kip

realkd,parameter  :: quartpi     = 0.7853981633974483_krp !16 digits
realkd,parameter  :: halfpi      = 1.570796326794896_krp  !16 digits
realkd,parameter  :: pi          = 3.141592653589793_krp  !16 digits
realkd,parameter  :: twopi       = 6.283185307179586_krp  !16 digits
realkd,parameter  :: sqrtpi      = 1.772453850905516_krp  !16 digits

! parameters for self-shielding calculation

intknd, parameter :: ncm_subgroup_solver = 0
intknd, parameter :: subgroup_solver = 1

intknd,parameter :: sub_scatt_nr = 0
intknd,parameter :: sub_scatt_sir = 1
intknd,parameter :: sub_scatt_ir = 2

realkd,parameter :: black_xs = 1e5_krp     !< xs of black body
realkd,parameter :: inf_mod_d = 10.0_krp   !< width of infinite moderator
realkd,parameter :: dmu_default = 1e-5_krp !< lethargy width of fine group

intknd, parameter :: interfer_bondarenko = 0
intknd, parameter :: interfer_rif = 1
intknd, parameter :: interfer_pmrn = 2

intknd, parameter :: store_dilution = 1
intknd, parameter :: store_shielded_xs = 2

realkd, parameter :: awr_H = 1.00782499_krp
realkd, parameter :: awr_H_endf = 0.9991673_krp

!> Avogadro's Number
realkd, parameter :: Na       = 6.02214129000E+23_krp
realkd, parameter :: cc       = 1.0E-24_krp
realkd, parameter::molm_H = 1.00798_krp
realkd, parameter::molm_O = 15.99940_krp
realkd, parameter::molm_H2O = 18.01536_krp
realkd, parameter::molm_B = 10.81103_krp
realkd, parameter::abun_H1 = 0.99985_krp
realkd, parameter::abun_H2 = 0.00015_krp
realkd, parameter::abun_O16 = 0.99757_krp
realkd, parameter::abun_O17 = 0.00038_krp
realkd, parameter::abun_O18 = 0.00205_krp
realkd, parameter::abun_B10 = 0.199_krp
realkd, parameter::abun_B11 = 0.801_krp

realkd, parameter::ev_to_j = 1.60217662E-19_krp

#define assert_start() CALL procedure_assert_start()

#define assert_component_begin(string) CALL procedure_assert_component_begin(string)

#define assert(bool,string) CALL procedure_assert(bool,__LINE__,string)

#define assert_component_summary(string) CALL procedure_assert_component_summary(string)

#define assert_finish() CALL procedure_assert_finish()
