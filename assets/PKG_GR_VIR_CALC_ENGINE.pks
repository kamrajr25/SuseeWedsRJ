CREATE OR REPLACE PACKAGE GREBATES.PKG_GR_VIR_CALC_ENGINE
AS
   /******************************************************************************
      NAME:       PKG_GR_VIR_CALC_ENGINE
      PURPOSE:    To calculate VIR for direct , indirect and both data sources

      REVISIONS:
      Ver        Date        Author           Description
      ---------  ----------  ---------------  ------------------------------------
      1.0        4/14/2015      totrel       1. Created this package.
      2.0        7/08/2015      totrel       1. Modified for summary report.
   ******************************************************************************/

   -- Constant Values used in the pkg
   g_actual_gen_dt     DATE;
   g_wipnum            NUMBER := NULL;
   g_hdrnum            NUMBER := 0;
   g_calnum            NUMBER;
   g_username          VARCHAR2 (32);
   g_direct            NUMBER;                      -- Value for Direct Source
   g_indirect          NUMBER;                    -- Value for Indirect Source
   g_both              NUMBER;
   g_status_num        NUMBER;
   g_er_status_num     NUMBER;                      -- Status number for Error
   g_ca_status_num     NUMBER;                  -- Status number for Cancelled
   g_inca_status_num   NUMBER;             -- Status number for In-Calculation
   g_reca_status_num   NUMBER;
   g_cl_status_num     NUMBER;
   g_dsrnum            NUMBER;
   g_lnnum             NUMBER := 0;
   g_flattype          NUMBER;
   g_standardtype      NUMBER;
   g_incrementaltype   NUMBER;
   g_isrpt             VARCHAR2 (1);
   g_cleanup_type      grebates.ind_params.param_text_value%TYPE; -- determine if we delete or truncate
   g_stgnum            NUMBER := 0; -- Define stage number when execption raised.
   l_exception         EXCEPTION; -- User defined exception for no rows returned.
   g_indirship         NUMBER;
   g_indirpaid         NUMBER;
   g_indsrnum          NUMBER;
   g_temps_flag        NUMBER := 0;
   l_status_num        grebates.status_grbt.status_grbt_num%TYPE;
   l_wipgdt            DATE;                  -- Value for WIP Generation Date
   l_cr_lccode         grebates.cd_references.ref_typ_val%TYPE;
   l_dr_lccode         grebates.cd_references.ref_typ_val%TYPE;
   g_gross_net         VARCHAR2 (1);
   ln_limit            NUMBER;
   g_termnum           NUMBER;



   PROCEDURE P_VIR_CALC (i_calnum   IN NUMBER,
                         i_dsrnum   IN NUMBER,
                         i_wipnum   IN NUMBER,
                         i_name     IN VARCHAR2,
                         i_status   IN NUMBER,
                         i_isRPT    IN VARCHAR2 DEFAULT 'N');

   PROCEDURE p_vir_sumrpt (i_wipnum         NUMBER,
                           i_hdrnum         NUMBER,
                           i_termnum        NUMBER,
                           i_dsrnum         NUMBER,
                           i_totalsls       NUMBER,
                           i_vir_sales      NUMBER,
                           i_paid_rbtamt    NUMBER,
                           i_tier_per       VARCHAR2);

   PROCEDURE p_dir_stg2dsrc (i_wipnum         NUMBER,
                             i_dsrnum         NUMBER,
                             i_total_sales    NUMBER,
                             i_vir_sales      NUMBER,
                             i_vir_amt        NUMBER);

   PROCEDURE p_elgbl_paid_rbt_lines (i_hdrnum            NUMBER,
                                     i_wipnum            NUMBER,
                                     i_termnum           NUMBER,
                                     i_prdsdt            DATE,
                                     i_prdedt            DATE,
                                     o_paid_rbtamt   OUT NUMBER);


   PROCEDURE p_VIR_updtwip (i_wipnum                  NUMBER,
                            i_rbtamt                  NUMBER DEFAULT 0.0,
                            i_rbtqty                  NUMBER DEFAULT 0.0,
                            i_rbtsls                  NUMBER DEFAULT 0.0,
                            i_wipsts                  NUMBER,
                            i_total_original_sales    NUMBER DEFAULT 0.0,
                            i_total_paid_rebates      NUMBER DEFAULT 0.0,
                            i_vir_rbt_per             VARCHAR2 DEFAULT ' ');

   PROCEDURE p_vir_dirdsrc (i_wipnum         NUMBER,
                            i_hdrnum         NUMBER,
                            i_dsrnum         NUMBER,
                            i_cr_lccode      VARCHAR2,
                            i_dr_lccode      VARCHAR2,
                            i_prdsdt         DATE,
                            i_prdedt         DATE,
                            i_wipgdt         DATE,
                            i_trmnum         NUMBER,
                            i_ern_s_dt_in    rbt_terms.earned_start_dt%TYPE, -- Term Earn Start Date
                            i_ern_e_dt_in    rbt_terms.earned_end_dt%TYPE -- Term Earn End Date
                                                                         );

   PROCEDURE p_vir_datains (i_stgnum    NUMBER,
                            i_wipnum    NUMBER,
                            i_dsrnum    NUMBER);

   PROCEDURE p_rbthdr_snapshot (i_hdrnum        NUMBER,
                                i_status_num    NUMBER,
                                i_wipnum        NUMBER);

   PROCEDURE p_rbttrm_snapshot (i_hdrnum        NUMBER,
                                i_status_num    NUMBER,
                                i_wipnum        NUMBER);

   PROCEDURE p_wiprecalcn (i_wipnum       NUMBER,
                           i_cr_lccode    VARCHAR2,
                           i_dr_lccode    VARCHAR2,
                           i_dsrnum       NUMBER,
                           i_termnum      NUMBER);

   PROCEDURE P_TIER_CALC (i_VIR_sales        NUMBER,
                          i_trmnum           NUMBER,
                          o_VIR_amount   OUT NUMBER,
                          o_tier_per     OUT VARCHAR2);

   PROCEDURE p_flat_tier (i_VIR_sales          NUMBER,
                          i_trmnum             NUMBER,
                          o_flatvirsales   OUT NUMBER,
                          o_tier_per       OUT VARCHAR2);

   PROCEDURE p_incremental_tier (i_VIR_sales                 NUMBER,
                                 i_trmnum                    NUMBER,
                                 o_incrementalvirsales   OUT NUMBER,
                                 o_tier_per              OUT VARCHAR2);

   PROCEDURE P_STANDARD_TIER (i_VIR_sales          IN     NUMBER,
                              i_termnum            IN     NUMBER,
                              O_Final_VIR_Amount      OUT NUMBER,
                              o_tier_per              OUT VARCHAR2);

   FUNCTION fn_wipdtlerr (fn_wipnum NUMBER, fn_errnum NUMBER)
      RETURN NUMBER;

   FUNCTION fn_wiplog (fn_wipnum NUMBER, fn_stage NUMBER)
      RETURN NUMBER;

   FUNCTION fn_total_sales (fn_stgnum NUMBER, fn_wipnum NUMBER)
      RETURN NUMBER;

   FUNCTION fn_linenum (fn_lnnum NUMBER DEFAULT 0)
      RETURN NUMBER;


   PROCEDURE p_errorlog_insertion (i_errnum    NUMBER,
                                   i_errmsg    VARCHAR2,
                                   i_wipnum    NUMBER,
                                   i_calnum    NUMBER,
                                   i_dsrnum    NUMBER,
                                   i_status    NUMBER,
                                   i_isrpt     VARCHAR2 DEFAULT 'N');

   FUNCTION fn_wipexist (fn_wipnum NUMBER)
      RETURN NUMBER;

   PROCEDURE P_WIPRVRSL (i_wipnum       NUMBER,
                         i_status       NUMBER,
                         i_wiptyp       NUMBER,
                         i_pairky       NUMBER,
                         i_dsrnum       NUMBER,
                         i_cr_lccode    VARCHAR2,
                         i_dr_lccode    VARCHAR2,
                         i_name         VARCHAR2);

   PROCEDURE P_SEND_EMAIL (i_wipno        IN NUMBER,
--                           i_hdrno        IN NUMBER,
--                           i_email_to     IN VARCHAR2,
--                           i_email_from   IN VARCHAR2,
                           i_email_msg      IN VARCHAR2
--                           i_EMAIL_CC IN VARCHAR2
                           );

   PROCEDURE P_VIR_INDIR_CALC (i_calnum   IN NUMBER,
                               i_dsrnum   IN NUMBER,
                               i_wipnum   IN NUMBER,
                               i_name     IN VARCHAR2,
                               i_status   IN NUMBER,
                               i_isRPT    IN VARCHAR2);

   PROCEDURE p_vir_indir_rbtcuswhl;

   PROCEDURE p_vir_indir_product;

   PROCEDURE p_vir_indirsales_in;

   PROCEDURE p_vir_indirect;

   FUNCTION fn_total_sales_indirect (fn_stgnum NUMBER, fn_wipnum NUMBER)
      RETURN NUMBER;

   PROCEDURE p_insert_logtable_stage (i_wipnum         NUMBER,
                                      i_dsrnum         NUMBER,
                                      i_total_sales    NUMBER,
                                      i_vir_sales      NUMBER,
                                      i_vir_amt        NUMBER);

   PROCEDURE p_ins_rbt_wip_dtl (i_stgnum    NUMBER,
                                i_wipnum    NUMBER,
                                i_dsrnum    NUMBER);

   PROCEDURE p_insert_to_final_logtable;



   PROCEDURE P_VIR_BOTH_CALC (i_calnum   IN NUMBER,
                              i_dsrnum   IN NUMBER,
                              i_wipnum   IN NUMBER,
                              i_name     IN VARCHAR2,
                              i_status   IN NUMBER,
                              i_isRPT    IN VARCHAR2);
END PKG_GR_VIR_CALC_ENGINE;
/