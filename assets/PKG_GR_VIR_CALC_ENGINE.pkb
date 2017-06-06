CREATE OR REPLACE PACKAGE BODY GREBATES.PKG_GR_VIR_CALC_ENGINE
AS
   /******************************************************************************
       NAME:       PKG_GR_VIR_CALC_ENGINE
       PURPOSE:    To calculate VIR for direct , indirect and both data sources

       REVISIONS:
       Ver        Date        Author           Description
       ---------  ----------  ---------------  ------------------------------------
       1.0        4/14/2015      totrel       1. Created this package.
       2.0        7/08/2015      totrel       1. Modified for summary report.
       3.0        10/28/2015     kansas      1.Modified package to introduce a new column(gross_net_vir_indirect ) in rbt_hdr table
       4.0        04/22/2016    kansas      1.Modified gross defintion for Indirect VIR
    ******************************************************************************/



   PROCEDURE p_log (i_proc VARCHAR2, i_message VARCHAR2)
   AS
   BEGIN
      INSERT INTO IND_LOG (log_num,
                           wipnum,
                           hdrnum,
                           proc,
                           MESSAGE,
                           lastmod)
           VALUES (IND_LOG_sq.NEXTVAL,
                   g_wipnum,
                   g_hdrnum,
                   i_proc,
                   i_message,
                   SYSDATE);

      COMMIT;
   EXCEPTION
      WHEN OTHERS
      THEN
         ROLLBACK;
         RAISE;
   END p_log;


   PROCEDURE P_VIR_CALC (i_calnum   IN NUMBER,
                         i_dsrnum   IN NUMBER,
                         i_wipnum   IN NUMBER,
                         i_name     IN VARCHAR2,
                         i_status   IN NUMBER,
                         i_isRPT    IN VARCHAR2)
   IS
      -- Local variable Declaration


      l_trmnum         NUMBER;
      l_ern_s_dt       rbt_terms.earned_start_dt%TYPE;
      l_ern_e_dt       rbt_terms.earned_end_dt%TYPE;
      l_hdrnum         NUMBER := 0;                        --Value for RBT HDR
      l_prdsdt         DATE;                     --Value for period start date
      l_prdedt         DATE;                  -- Value for wip period end date
      l_wipgdt         DATE;                  -- Value for WIP Generation Date
      l_rbtamt         NUMBER (22, 7) := 0.0;
      l_rbtqty         NUMBER (22, 7) := 0.0;
      l_rbtsls         NUMBER (22, 7) := 0.0;
      l_totalsls       NUMBER (22, 7) := 0.0;
      l_wipstatus      NUMBER;
      l_paid_rbtamt    NUMBER (22, 7) := 0.0;
      l_vir_sales      NUMBER (22, 7) := 0.0;
      l_vir_amt        NUMBER (22, 7) := 0.0;
      l_num_of_terms   NUMBER;
      l_flag           NUMBER;
      l_tier_per       VARCHAR2 (200);
      l_reftyp         VARCHAR2 (50); -- Refence type value for Check/Credit Memo
      l_paynum         NUMBER;
      l_pairke         NUMBER;      -- Pair key is generated based on WIP KEY.
      l_name           VARCHAR2 (256);
      l_wiptyp         NUMBER; -- WIP Type extract from CD Reference based on Original / Reconciliation
   BEGIN

      g_wipnum := i_wipnum;
      g_dsrnum := i_dsrnum;
      g_isrpt := i_isRPT;
      g_actual_gen_dt := SYSDATE;

      SELECT rw.rbt_hdr_num
        INTO g_hdrnum
        FROM rbt_wip rw
       WHERE rw.rbt_wip_num = g_wipnum;

      --Process log enties for PROCEDURE p_grebate_calc
      p_log (i_proc      => ' p_VIR_calc',
             i_message   => '******************  BEGIN i_wipnum:' || g_wipnum);

      g_calnum := i_calnum;

      IF (i_calnum = 4 OR i_calnum = 1)
      THEN
         SELECT username
           INTO g_username
           FROM rbt_wip rw
          WHERE rw.rbt_wip_num = i_wipnum;
      ELSE
         g_username := i_name;
      END IF;

      SELECT cr.ref_num
        INTO g_direct
        FROM cd_references cr
       WHERE cr.ref_typ_val = 'Direct' AND cr.ref_typ = 'VirDataSource';

      SELECT cr.ref_num
        INTO g_indirect
        FROM cd_references cr
       WHERE cr.ref_typ_val = 'Indirect' AND cr.ref_typ = 'VirDataSource';

      SELECT cr.ref_num
        INTO g_both
        FROM cd_references cr
       WHERE cr.ref_typ_val = 'Both' AND cr.ref_typ = 'VirDataSource';

      IF  i_calnum IN (1, 4) AND i_isRPT = 'N' /*AND fn_wipexist (i_wipnum) = 1 */
      THEN
         DELETE FROM rbt_wip_dtl
               WHERE rbt_wip_num = i_wipnum;

         DELETE FROM logtable
               WHERE rbt_wip_num = i_wipnum;
      END IF;

      IF  i_calnum = 1 AND i_isRPT = 'Y'
      THEN

         DELETE FROM logtable
               WHERE rbt_wip_num = i_wipnum;
      END IF;
      -- Get Payment Info Based on Payment type, if RBT_AMT is Positive then Cr Line class Code
      -- else Dr Line class code
      SELECT pi.cr_lc_code, pi.dr_lc_code
        INTO l_cr_lccode, l_dr_lccode
        FROM rbt_hdr rh, paymnt_info pi
       WHERE rh.rbt_hdr_num = g_hdrnum AND pi.paymnt_num = rh.paymnt_num;

      -- Get the status value for an "In Review"
      SELECT sg.status_grbt_num
        INTO g_status_num
        FROM status_grbt sg
       WHERE sg.status_abbr = 'IR';

      -- Get the status value for an "Error"
      SELECT sg.status_grbt_num
        INTO g_er_status_num
        FROM status_grbt sg
       WHERE sg.status_abbr = 'ERR';

      -- Get the status value for an "Cancelled"
      SELECT status_grbt_num
        INTO g_ca_status_num
        FROM status_grbt sg
       WHERE sg.status_abbr = 'CA';

      -- Get the status value for an "Cancelled"
      SELECT status_grbt_num
        INTO g_inca_status_num
        FROM status_grbt sg
       WHERE sg.status_abbr = 'IC';

      -- Get the status value for an "Cancelled"
      SELECT status_grbt_num
        INTO g_reca_status_num
        FROM status_grbt sg
       WHERE sg.status_abbr = 'RC';

      -- Get the status value for an 'Closed'
      SELECT status_grbt_num
        INTO g_cl_status_num
        FROM status_grbt sg
       WHERE sg.status_abbr = 'CLS';

      -- Get the status INCA and update the wip before process the rebate calculation


      IF i_calnum IN (1, 2, 3, 4) AND i_isRPT = 'N'
      THEN
         p_vir_updtwip (i_wipnum,
                        l_rbtamt,
                        l_rbtqty,
                        l_rbtsls,
                        g_inca_status_num);
      END IF;



      CASE
         WHEN i_dsrnum = g_direct
         THEN
            -- Delete temp tables where wipnum exists

            EXECUTE IMMEDIATE 'TRUNCATE TABLE LOGTABLE_TEMP';

            EXECUTE IMMEDIATE 'TRUNCATE TABLE LOGTABLE_TEMP1';

            SELECT rw.rbt_wip_prd_start_dt,
                   rw.rbt_wip_prd_end_dt,
                   rw.wip_gen_dt,
                   rh.rbt_hdr_num,
                   rt.rbt_term_num,
                   rt.earned_start_dt,
                   rt.earned_end_dt
              INTO l_prdsdt,
                   l_prdedt,
                   l_wipgdt,
                   l_hdrnum,
                   l_trmnum,
                   l_ern_s_dt,
                   l_ern_e_dt
              FROM rbt_wip rw,
                   rbt_hdr rh,
                   rbt_terms rt,
                   status_grbt sg
             WHERE     rw.rbt_wip_num = i_wipnum
                   AND rh.rbt_hdr_num = rw.rbt_hdr_num
                   AND rt.rbt_hdr_num = rh.rbt_hdr_num
                   AND sg.status_grbt_num = rt.status_grbt_num
                   AND sg.status_abbr = 'AC'
                   AND ROWNUM = 1;

            -- Conditional Block to process Rebate Calculation (Initial/Re-Calculation/Import/Aprrove)
            CASE
               WHEN    ( (i_calnum = 4 OR i_calnum = 1) AND i_isRPT = 'N')
                    OR (i_calnum = 1 AND i_isRPT = 'Y')
               THEN

                  -- Direct Source rebate calculation for stage - 1

                  p_vir_dirdsrc (i_wipnum -- wip number associated to the header
                                         ,
                                 l_hdrnum                    -- Rebate progarm
                                         ,
                                 i_dsrnum                -- Data Source number
                                         ,
                                 l_cr_lccode -- Credit line Class Code - based on strategy referenced from CD References
                                            ,
                                 l_dr_lccode -- Debit line Class Code - based on strategy referenced from CD References
                                            ,
                                 l_prdsdt          -- Period Start Date of WIP
                                         ,
                                 l_prdedt          -- period end date of a WIP
                                         ,
                                 l_wipgdt               -- Wip genration date.
                                         ,
                                 l_trmnum                       -- Rebate Term
                                         ,
                                 l_ern_s_dt            -- Term Earn Start Date
                                           ,
                                 l_ern_e_dt              -- Term Earn End Date
                                           );

                  -- After Stage1, Data Transformation from LOGTABLE_TEMP to LOGTABLE with condition check as Stage_Num = 1

                  p_vir_datains (1 -- Stage Number 1, Based on stage data insertion into logtable from logtable temp1
                                  , i_wipnum, i_dsrnum);


                  IF fn_wiplog (i_wipnum, 1) = 1
                  THEN
                     l_wipstatus := 1;
                  END IF;




                  l_totalsls := fn_total_sales (1, i_wipnum);


                  p_elgbl_paid_rbt_lines (l_hdrnum,
                                          i_wipnum,
                                          l_trmnum,
                                          l_prdsdt,
                                          l_prdedt,
                                          l_paid_rbtamt);

                  l_vir_sales := l_totalsls - NVL (l_paid_rbtamt, 0);

                   -- Call tier calculation procedure
                  P_TIER_CALC (l_vir_sales,
                               l_trmnum,
                               l_vir_amt,
                               l_tier_per);


                  p_log (
                     i_proc      => 'P_VIR_INDIR_CALC',
                     i_message   =>    'total sales: '
                                    || l_totalsls
                                    || ' total paid rbt amount: '
                                    || l_paid_rbtamt
                                    || ' total vir sales: '
                                    || l_vir_sales
                                    || ' total vir amount :'
                                    || l_vir_amt
                                    || ' Tier %: '
                                    || l_tier_per);


                  p_dir_stg2dsrc (i_wipnum,
                                  i_dsrnum,
                                  l_totalsls,
                                  l_vir_sales,
                                  l_vir_amt);


                  IF i_isRPT = 'N'
                  THEN
                     p_vir_datains (2 -- Stage Number 2, Based on stage data insertion into logtable from logtable temp1
                                     , i_wipnum, i_dsrnum);





                     IF fn_wiplog (i_wipnum, 2) = 1
                     THEN
                        l_wipstatus := 1;
                     END IF;


                     SELECT COUNT (DISTINCT rbt_term_num)
                       INTO l_num_of_terms
                       FROM rbt_wip_dtl
                      WHERE     rbt_wip_num = i_wipnum
                            AND line_status_num = g_status_num;

                     IF l_num_of_terms = 0
                     THEN
                        l_rbtqty := 0;
                        l_rbtsls := 0;
                     ELSE
                        SELECT SUM (qty),
                               SUM (sales_in_dollars),
                               SUM (rbt_amt)
                          INTO l_rbtqty, l_rbtsls, l_rbtamt
                          FROM rbt_wip_dtl
                         WHERE     rbt_wip_num = i_wipnum
                               AND line_status_num = g_status_num;
                     END IF;

                     l_rbtqty := ROUND (l_rbtqty, 3);
                     l_rbtsls := ROUND (l_rbtsls, 5);
                     l_rbtamt :=ROUND (NVL (l_rbtamt, 0.0), 3);


                     -- Update status of RBT_WIP after initial calculation based on WIP Detail lines if error exist then set WIP header status as 'Error'  else 'In-Review''

                     IF    fn_wipdtlerr (i_wipnum, g_er_status_num) = 1
                        OR l_wipstatus = 1
                     THEN
                        p_vir_updtwip (i_wipnum,
                                       l_rbtamt,
                                       l_rbtqty,
                                       l_rbtsls,
                                       g_er_status_num,
                                       l_totalsls,
                                       l_paid_rbtamt,
                                       l_tier_per);
                     ELSE
                        p_vir_updtwip (i_wipnum,
                                       l_rbtamt,
                                       l_rbtqty,
                                       l_rbtsls,
                                       g_status_num,
                                       l_totalsls,
                                       l_paid_rbtamt,
                                       l_tier_per);
                     END IF;


                     -- Added Exception, when no data found then flag set to 0 so snapshot creation will ignore.


                     BEGIN
                        SELECT 1
                          INTO l_flag
                          FROM rbt_wip rw, status_grbt sg
                         WHERE     rw.rbt_wip_num = i_wipnum
                               AND (       sg.status_grbt_num =
                                              rw.status_grbt_num
                                       AND sg.status_abbr = 'IR'
                                    OR     sg.status_grbt_num =
                                              rw.status_grbt_num
                                       AND sg.status_abbr = 'ERR'); -- [SV]: 11-MAY-2012, Defect 5455.
                     EXCEPTION
                        WHEN NO_DATA_FOUND
                        THEN
                           l_flag := 0;
                     END;

                     -- Snapshot Creation for the WIPs status set to In-Review.
                     IF l_flag = 1
                     THEN
                        p_rbthdr_snapshot (l_hdrnum, g_status_num, i_wipnum);


                        p_rbttrm_snapshot (l_hdrnum, g_status_num, i_wipnum);
                     END IF;



                  END IF;


                  p_vir_sumrpt (i_wipnum,
                                l_hdrnum,
                                l_trmnum,
                                i_dsrnum,
                                l_totalsls,
                                l_vir_sales,
                                l_paid_rbtamt,
                                l_tier_per);

               WHEN i_calnum = 2
               THEN
                  g_lnnum := 0;
                  p_log (i_proc      => ' p_vir_calc',
                         i_message   => 'Re-Calculation for Selective WIP');

                  SELECT rt.rbt_term_num
                    INTO l_trmnum
                    FROM rbt_wip rw,
                         rbt_hdr rh,
                         rbt_terms rt,
                         status_grbt sg
                   WHERE     rw.rbt_wip_num = i_wipnum
                         AND rh.rbt_hdr_num = rw.rbt_hdr_num
                         AND rt.rbt_hdr_num = rh.rbt_hdr_num
                         AND sg.status_grbt_num = rt.status_grbt_num
                         AND sg.status_abbr = 'AC'
                         AND ROWNUM = 1;

                  -- If QTY is 0 then wip detail line will drop if the Data Source is External else set status as Cancel.

                  UPDATE rbt_wip_dtl rwd
                     SET rwd.line_status_num = g_ca_status_num
                   WHERE rwd.rbt_wip_num = i_wipnum AND rwd.qty = 0;

                  COMMIT;

                  -- Procedure added for a Re-Calculation across 3 Data Sources - 16-04-2012
                  p_wiprecalcn (i_wipnum,
                                l_cr_lccode,
                                l_dr_lccode,
                                i_dsrnum,
                                l_trmnum);

                  COMMIT;
               WHEN i_calnum = 5
               THEN
                  p_log (i_proc      => ' p_vir_calc',
                         i_message   => 'Wip Reversal for Selective WIP');

                  -- Get the type of payment on Rebate program for a given WIP.
                  SELECT rh.paymnt_num, cr.ref_typ_val
                    INTO l_paynum, l_reftyp
                    FROM rbt_hdr rh,
                         rbt_wip rw,
                         paymnt_info pi,
                         cd_references cr
                   WHERE     rw.rbt_wip_num = i_wipnum
                         AND rh.rbt_hdr_num = rw.rbt_hdr_num
                         AND pi.PAYMNT_NUM = rh.paymnt_num
                         AND cr.ref_num = pi.paymnt_type_num
                         AND cr.ref_typ_val IN ('Check', 'Credit Memo');

                  -- Pass the current wip key to set as Pair Key in Reversal WIP Creation
                  l_pairke := i_wipnum;
                  l_name := g_username;

                  -- Based on payment, Reversal WIP is created.
                  IF l_reftyp = 'Credit Memo'
                  THEN
                     SELECT cr.ref_num
                       INTO l_wiptyp
                       FROM cd_references cr
                      WHERE cr.REF_TYP_VAL = 'Reversal';

                     p_wiprvrsl (i_wipnum,
                                 g_status_num,
                                 l_wiptyp,
                                 l_pairke,
                                 i_dsrnum,
                                 l_cr_lccode,
                                 l_dr_lccode,
                                 l_name);
                  ELSE
                     IF l_reftyp = 'Check'
                     THEN
                        SELECT cr.ref_num
                          INTO l_wiptyp
                          FROM cd_references cr
                         WHERE cr.REF_TYP_VAL = 'Reversal-Check';

                        p_wiprvrsl (i_wipnum,
                                    g_cl_status_num,
                                    l_wiptyp,
                                    l_pairke,
                                    i_dsrnum,
                                    l_cr_lccode,
                                    l_dr_lccode,
                                    l_name);
                     END IF;
                  END IF;

                  COMMIT;
               WHEN i_calnum = 4 AND i_isRPT = 'Y'
               THEN
                  --                  p_log (i_proc      => ' p_vir_indr_calc',
                  --                         i_message   => 'Summary report for wip :'||i_wipnum);

                  SELECT RW.SALES_AMT,
                         RW.TOTAL_ORIGINAL_SALES,
                         RW.TOTAL_PAID_REBATES,
                         RW.VIR_RBT_PER
                    INTO l_vir_sales,
                         l_totalsls,
                         l_paid_rbtamt,
                         l_tier_per
                    FROM RBT_WIP RW
                   WHERE RW.RBT_WIP_NUM = i_wipnum;

                  p_vir_sumrpt (i_wipnum,
                                l_hdrnum,
                                l_trmnum,
                                i_dsrnum,
                                l_totalsls,
                                l_vir_sales,
                                l_paid_rbtamt,
                                l_tier_per);

            END CASE;
         WHEN i_dsrnum = g_indirect
         THEN

            P_VIR_INDIR_CALC (i_calnum,
                              i_dsrnum,
                              i_wipnum,
                              i_name,
                              i_status,
                              i_isRPT);
         WHEN i_dsrnum = g_both
         THEN
            P_VIR_BOTH_CALC (i_calnum,
                             i_dsrnum,
                             i_wipnum,
                             i_name,
                             i_status,
                             i_isRPT);
      END CASE;



      p_log (i_proc      => ' p_vir_calc',
             i_message   => '****************** END i_wipnum:' || i_wipnum);
   EXCEPTION
      WHEN OTHERS
      THEN
               IF i_isRPT = 'Y' THEN
                P_SEND_EMAIL (i_wipnum,'FAIL');
               END IF;
         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               i_wipnum,
                               i_calnum,
                               i_dsrnum,
                               g_er_status_num,
                               i_isRPT);

         ROLLBACK;
         RAISE;
   END P_VIR_CALC;

   PROCEDURE p_vir_sumrpt (i_wipnum         NUMBER,
                           i_hdrnum         NUMBER,
                           i_termnum        NUMBER,
                           i_dsrnum         NUMBER,
                           i_totalsls       NUMBER,
                           i_vir_sales      NUMBER,
                           i_paid_rbtamt    NUMBER,
                           i_tier_per       VARCHAR2)
   AS
      l_count       NUMBER;
      l_count1      NUMBER;
      l_stage_num   NUMBER;
   BEGIN
      p_log (
         i_proc      => ' p_vir_sumrpt',
         i_message   =>    '******************  BEGIN summary report for  i_wipnum:'
                        || i_wipnum);



      -- l_stage_num  := case i_dsrnum when g_direct then 2 when g_indirect then 3 end ;

      SELECT COUNT (*)
        INTO l_count
        FROM VIR_SUMMARY_RPT
       WHERE RBT_wip_num = i_wipnum;

      IF l_count > 0
      THEN
         DELETE FROM VIR_SUMMARY_RPT
               WHERE RBT_wip_num = i_wipnum;

         COMMIT;
      END IF;

      IF i_dsrnum = g_direct OR i_dsrnum = g_both
      THEN
         INSERT INTO VIR_SUMMARY_RPT
            SELECT VIR_SUMMARY_RPT_SQ.NEXTVAL,
                   c.RBT_WIP_NUM,
                   c.RBT_HDR_NUM,
                   c.RBT_TERM_NUM,
                   c.creatdate,
                   c.createby,
                   --c.CUST_WHL_NUM,
                   gbm.bunit_name,
                   c.BASE_PRCLST,
                   CASE c.BASE_PRCLST
                      WHEN '*ALL' THEN '*ALL'
                      WHEN 'WAC' THEN 'WAC'
                      WHEN 'LIST' THEN 'LIST'
                      ELSE gcm.cpgrp_desc
                   END
                      cpgrp_desc,
                  c.sales_dollar,
                   NVL (c.Rebate_payments, 0),
                   c.NET_VIR_SALES_DOLLARS,
                   c.sales_VIR_Units,
                   i_tier_per,
                   c.NET_VIR_EARNED,
                   g_direct
              FROM    (  SELECT b.RBT_WIP_NUM,
                                b.RBT_HDR_NUM,
                                b.RBT_TERM_NUM,
                                b.creatdate,
                                b.createby,
                                b.CUST_WHL_NUM,
                                b.BASE_PRCLST,
                                -- b.CPGRP_DESC,
                                 ROUND (SUM (b.sales_dollar),5) sales_dollar,
                                 ROUND (SUM (b.Rebate_payments),3) Rebate_payments,
                                 ROUND (SUM (b.NET_VIR_SALES_DOLLARS),5)
                                   NET_VIR_SALES_DOLLARS,
                                ROUND ( SUM (b.sales_VIR_Units),3 )sales_VIR_Units,
                                ROUND ( SUM (b.NET_VIR_EARNED),3 )NET_VIR_EARNED
                           FROM (SELECT a.RBT_WIP_NUM,
                                        a.RBT_HDR_NUM,
                                        a.RBT_TERM_NUM,
                                        a.creatdate,
                                        a.createby,
                                        a.BUNIT_NUM,
                                        a.CUST_WHL_NUM,
                                        a.BASE_PRCLST,
                                        -- a.CPGRP_DESC,
                                        a.sales_dollar,
                                          (a.sales_dollar / i_totalsls)
                                        * i_paid_rbtamt
                                           Rebate_payments,
                                        a.NET_SALES_VIR_DOLLARS
                                           NET_VIR_SALES_DOLLARS,
                                        a.QTY sales_VIR_Units,
                                        a.NET_VIR_EARNED
                                           NET_VIR_EARNED
                                   FROM (SELECT L.RBT_WIP_NUM,
                                                L.RBT_HDR_NUM,
                                                L.RBT_TERM_NUM,
                                                SYSDATE creatdate,
                                                L.USERNAME createby,
                                                L.BUNIT_NUM,
                                                m.CUST_WHL_NUM,
                                                CASE
                                                   WHEN L.BASE_CNTRCT_NUM = -3
                                                   THEN
                                                      '*ALL'
                                                   WHEN L.BASE_CNTRCT_NUM = -2
                                                   THEN
                                                      'LIST'
                                                   WHEN L.BASE_CNTRCT_NUM = -1
                                                   THEN
                                                      'WAC'
                                                   ELSE
                                                      L.BASE_PRCLST
                                                END
                                                   BASE_PRCLST,
                                                  (  L.SALES_IN_DOLLARS
                                                   * i_totalsls)
                                                / i_vir_sales
                                                   sales_dollar,
                                                L.SALES_IN_DOLLARS
                                                   NET_SALES_VIR_DOLLARS,
                                                L.RBT_AMT NET_VIR_EARNED,
                                                L.QTY
                                           FROM LOGTABLE l,
                                                --                                             cust_whl_assoc cwa,
                                                --                                             gr_mbr_mv gmm
                                                (SELECT DISTINCT
                                                        CUST_WHL_NUM,
                                                        child_num,
                                                        mbr_dt_start
                                                   FROM (SELECT CWA.CUST_WHL_NUM,
                                                                child_num,
                                                                mbr_dt_start,
                                                                DENSE_RANK ()
                                                                OVER (
                                                                   PARTITION BY child_num
                                                                   ORDER BY
                                                                      mbr_dt_start DESC)
                                                                   RANK
                                                           FROM cust_whl_assoc cwa,
                                                                gr_mbr_mv gmm
                                                          WHERE     CWA.CUST_WHL_NUM =
                                                                       GMM.CTORG_BUNIT_NUM
                                                                AND CWA.CHILD_NUM =
                                                                       GMM.MBR_BUNIT_NUM
                                                                AND rbt_hdr_num =
                                                                       i_hdrnum
                                                                AND CWA.CUST_OR_WHL =
                                                                       'CUST')
                                                  WHERE RANK = 1) m
                                          WHERE --                                        cwa.RBT_HDR_NUM =
                                                    --                                                    l.RBT_HDR_NUM
                                                    --                                             AND
                                                    m.child_num = l.BUNIT_NUM
                                                --                                             AND CWA.CUST_WHL_NUM =
                                                --                                                    GMM.CTORG_BUNIT_NUM
                                                --                                             AND CWA.CHILD_NUM =
                                                --                                                    GMM.MBR_BUNIT_NUM

                                                AND RBT_WIP_NUM = i_wipnum
                                                AND stage_num = 2
                                                AND L.DATA_SOURCE_TYPE_NUM =
                                                       g_direct --                                                    AND CWA.CUST_OR_WHL ='CUST'
                                                               ) a) b
                       GROUP BY b.RBT_WIP_NUM,
                                b.RBT_HDR_NUM,
                                b.RBT_TERM_NUM,
                                b.creatdate,
                                b.createby,
                                b.CUST_WHL_NUM,
                                b.BASE_PRCLST                              --,
                                             -- b.CPGRP_DESC
                      ) c
                   LEFT JOIN
                      gr_cpgrp_mv gcm
                   ON GCM.CPGRP_CONT_ID_ALIAS = c.BASE_PRCLST,
                   gr_bunit_mv gbm
             WHERE gbm.bunit_num = c.CUST_WHL_NUM;
      END IF;

      IF i_dsrnum = g_indirect OR i_dsrnum = g_both
      THEN
         SELECT COUNT (*)
           INTO l_count1
           FROM cust_whl_assoc cwa
          WHERE     rbt_hdr_num = i_hdrnum
                AND CWA.CUST_OR_WHL = 'WHL'
                AND CWA.CUST_WHL_NUM = -3;

         IF l_count1 = 0
         THEN
            INSERT INTO VIR_SUMMARY_RPT
               SELECT VIR_SUMMARY_RPT_SQ.NEXTVAL,
                      c.RBT_WIP_NUM,
                      c.RBT_HDR_NUM,
                      c.RBT_TERM_NUM,
                      c.creatdate,
                      c.createby,
                      --c.CUST_WHL_NUM,
                      gbm.bunit_name,
                      c.BASE_PRCLST,
                      CASE c.BASE_PRCLST
                         WHEN '*ALL' THEN '*ALL'
                         WHEN 'WAC' THEN 'WAC'
                         WHEN 'LIST' THEN 'LIST'
                         ELSE gcm.cpgrp_desc
                      END
                         cpgrp_desc,
                      c.sales_dollar,
                      NVL (c.Rebate_payments, 0),
                      c.NET_VIR_SALES_DOLLARS,
                      c.sales_VIR_Units,
                      i_tier_per,
                      c.NET_VIR_EARNED,
                      g_indirect
                 FROM    (  SELECT b.RBT_WIP_NUM,
                                   b.RBT_HDR_NUM,
                                   b.RBT_TERM_NUM,
                                   b.creatdate,
                                   b.createby,
                                   b.CUST_WHL_NUM,
                                   b.BASE_PRCLST,
                                   ROUND (SUM (b.sales_dollar), 5) sales_dollar,
                                   ROUND (SUM (b.Rebate_payments),
                                              3) Rebate_payments,
                                    ROUND (SUM (b.NET_VIR_SALES_DOLLARS), 5)
                                      NET_VIR_SALES_DOLLARS,
                                    ROUND (SUM (b.sales_VIR_Units),3) sales_VIR_Units,
                                    ROUND (SUM (b.NET_VIR_EARNED),3) NET_VIR_EARNED
                              FROM (SELECT a.RBT_WIP_NUM,
                                           a.RBT_HDR_NUM,
                                           a.RBT_TERM_NUM,
                                           a.creatdate,
                                           a.createby,
                                           a.CUST_WHL_NUM,
                                           a.BASE_PRCLST,
                                           a.sales_dollar,
                                           (a.sales_dollar / i_totalsls)
                                              * i_paid_rbtamt
                                              Rebate_payments,
                                          a.NET_SALES_VIR_DOLLARS
                                              NET_VIR_SALES_DOLLARS,
                                           a.QTY sales_VIR_Units,
                                           a.NET_VIR_EARNED NET_VIR_EARNED
                                      FROM (SELECT L.RBT_WIP_NUM,
                                                   L.RBT_HDR_NUM,
                                                   L.RBT_TERM_NUM,
                                                   SYSDATE creatdate,
                                                   L.USERNAME createby,
                                                   m.CUST_WHL_NUM,
                                                   CASE
                                                      WHEN L.BASE_CNTRCT_NUM =
                                                              -3
                                                      THEN
                                                         '*ALL'
                                                      WHEN L.BASE_CNTRCT_NUM =
                                                              -2
                                                      THEN
                                                         'LIST'
                                                      WHEN L.BASE_CNTRCT_NUM =
                                                              -1
                                                      THEN
                                                         'WAC'
                                                      ELSE
                                                         L.BASE_PRCLST
                                                   END
                                                      BASE_PRCLST,
                                                     (  L.SALES_IN_DOLLARS
                                                      * i_totalsls)
                                                   / i_vir_sales
                                                      sales_dollar,
                                                   L.SALES_IN_DOLLARS
                                                      NET_SALES_VIR_DOLLARS,
                                                   L.RBT_AMT NET_VIR_EARNED,
                                                   L.QTY
                                              FROM LOGTABLE l,
                                                   indirsales_in ind,
                                                   (SELECT DISTINCT
                                                           CUST_WHL_NUM,
                                                           child_num,
                                                           mbr_dt_start
                                                      FROM (SELECT CWA.CUST_WHL_NUM,
                                                                   child_num,
                                                                   mbr_dt_start,
                                                                   DENSE_RANK ()
                                                                   OVER (
                                                                      PARTITION BY child_num
                                                                      ORDER BY
                                                                         mbr_dt_start DESC)
                                                                      RANK
                                                              FROM cust_whl_assoc cwa,
                                                                   gr_mbr_mv gmm
                                                             WHERE     CWA.CUST_WHL_NUM =
                                                                          GMM.CTORG_BUNIT_NUM
                                                                   AND CWA.CHILD_NUM =
                                                                          GMM.MBR_BUNIT_NUM
                                                                   AND rbt_hdr_num =
                                                                          i_hdrnum
                                                                   AND CWA.CUST_OR_WHL =
                                                                          'WHL')
                                                     WHERE RANK = 1) m
                                             WHERE     RBT_WIP_NUM = i_wipnum
                                                   AND stage_num = 2
                                                   AND IND.SUBMITEM_NUM =
                                                          L.SUBMITEM_NUM
                                                   AND L.DATA_SOURCE_TYPE_NUM =
                                                          g_indirect
                                                   AND m.child_num =
                                                          ind.bunit_source) a) b
                          GROUP BY b.RBT_WIP_NUM,
                                   b.RBT_HDR_NUM,
                                   b.RBT_TERM_NUM,
                                   b.creatdate,
                                   b.createby,
                                   b.CUST_WHL_NUM,
                                   b.BASE_PRCLST) c
                      LEFT JOIN
                         gr_cpgrp_mv gcm
                      ON GCM.CPGRP_CONT_ID_ALIAS = c.BASE_PRCLST,
                      gr_bunit_mv gbm
                WHERE gbm.bunit_num = c.CUST_WHL_NUM;
         ELSE
            INSERT INTO VIR_SUMMARY_RPT
               SELECT VIR_SUMMARY_RPT_SQ.NEXTVAL,
                      c.RBT_WIP_NUM,
                      c.RBT_HDR_NUM,
                      c.RBT_TERM_NUM,
                      c.creatdate,
                      c.createby,
                      --c.CUST_WHL_NUM,
                      '*ALL',
                      c.BASE_PRCLST,
                      CASE c.BASE_PRCLST
                         WHEN '*ALL' THEN '*ALL'
                         WHEN 'WAC' THEN 'WAC'
                         WHEN 'LIST' THEN 'LIST'
                         ELSE gcm.cpgrp_desc
                      END
                         cpgrp_desc,
                      c.sales_dollar,
                      NVL (c.Rebate_payments, 0),
                      c.NET_VIR_SALES_DOLLARS,
                      c.sales_VIR_Units,
                      i_tier_per,
                      c.NET_VIR_EARNED,
                      g_indirect
                 FROM    (  SELECT b.RBT_WIP_NUM,
                                   b.RBT_HDR_NUM,
                                   b.RBT_TERM_NUM,
                                   b.creatdate,
                                   b.createby,
                                   -- b.CUST_WHL_NUM,
                                   b.BASE_PRCLST,
                                   ROUND(SUM (b.sales_dollar),5) sales_dollar,
                                   ROUND(SUM (b.Rebate_payments),3) Rebate_payments,
                                   ROUND(SUM (b.NET_VIR_SALES_DOLLARS),5)
                                      NET_VIR_SALES_DOLLARS,
                                   ROUND(SUM (b.sales_VIR_Units),3) sales_VIR_Units,
                                   ROUND (SUM (b.NET_VIR_EARNED), 3)
                                      NET_VIR_EARNED
                              FROM (SELECT a.RBT_WIP_NUM,
                                           a.RBT_HDR_NUM,
                                           a.RBT_TERM_NUM,
                                           a.creatdate,
                                           a.createby,
                                           -- a.CUST_WHL_NUM,
                                           a.BASE_PRCLST,
                                           a.sales_dollar,
                                           (a.sales_dollar / i_totalsls)
                                              * i_paid_rbtamt
                                              Rebate_payments,
                                           a.NET_SALES_VIR_DOLLARS
                                              NET_VIR_SALES_DOLLARS,
                                           a.QTY sales_VIR_Units,
                                           a.NET_VIR_EARNED
                                              NET_VIR_EARNED
                                      FROM (SELECT L.RBT_WIP_NUM,
                                                   L.RBT_HDR_NUM,
                                                   L.RBT_TERM_NUM,
                                                   SYSDATE creatdate,
                                                   L.USERNAME createby,
                                                   -3 CUST_WHL_NUM,
                                                   CASE
                                                      WHEN L.BASE_CNTRCT_NUM =
                                                              -3
                                                      THEN
                                                         '*ALL'
                                                      WHEN L.BASE_CNTRCT_NUM =
                                                              -2
                                                      THEN
                                                         'LIST'
                                                      WHEN L.BASE_CNTRCT_NUM =
                                                              -1
                                                      THEN
                                                         'WAC'
                                                      ELSE
                                                         L.BASE_PRCLST
                                                   END
                                                      BASE_PRCLST,
                                                     (  L.SALES_IN_DOLLARS
                                                      * i_totalsls)
                                                   / i_vir_sales
                                                      sales_dollar,
                                                   L.SALES_IN_DOLLARS
                                                      NET_SALES_VIR_DOLLARS,
                                                   L.RBT_AMT NET_VIR_EARNED,
                                                   L.QTY
                                              FROM LOGTABLE l
                                             WHERE     RBT_WIP_NUM = i_wipnum
                                                   AND stage_num = 2
                                                   AND L.DATA_SOURCE_TYPE_NUM =
                                                          g_indirect) a) b
                          GROUP BY b.RBT_WIP_NUM,
                                   b.RBT_HDR_NUM,
                                   b.RBT_TERM_NUM,
                                   b.creatdate,
                                   b.createby,
                                   -- b.CUST_WHL_NUM,
                                   b.BASE_PRCLST) c
                      LEFT JOIN
                         gr_cpgrp_mv gcm
                      ON GCM.CPGRP_CONT_ID_ALIAS = c.BASE_PRCLST;
         END IF;
      END IF;

      COMMIT;

      IF g_isrpt = 'Y' THEN

      P_SEND_EMAIL (i_wipnum,'SUCCESS');

      END IF;


      p_log (
         i_proc      => ' p_vir_sumrpt',
         i_message   =>    '*************** END summary report for  i_wipnum:'
                        || i_wipnum);
   EXCEPTION
      WHEN OTHERS
      THEN
         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               i_wipnum,
                               g_calnum,
                               i_dsrnum,
                               g_er_status_num,
                               g_isrpt);
         ROLLBACK;
         RAISE;
   END p_vir_sumrpt;

   PROCEDURE p_dir_stg2dsrc (i_wipnum         NUMBER,
                             i_dsrnum         NUMBER,
                             i_total_sales    NUMBER,
                             i_vir_sales      NUMBER,
                             i_vir_amt        NUMBER)
   AS
   BEGIN
      p_log (
         i_proc      => ' p_dir_stg2dsrc',
         i_message   => 'Procedure for an Direct Data Source validation for a Stage - 1 started');
      g_lnnum := 0;

      INSERT INTO logtable (RBT_HDR_NUM,
                            RBT_TERM_NUM,
                            RBT_WIP_NUM,
                            BASE_CNTRCT_NUM,
                            INC_PR_SALES,
                            PROD_ID_PRI,
                            LINE_CLASS_CD,
                            OFFSET_ST_DT,
                            EARNED_YR,
                            EARNED_MONTH,
                            PER_UNIT_RBT,
                            DI_NUM,
                            DI_LINE_NUM,
                            QTY,
                            BASE_CNTRCT_PRICE,
                            STRATEGY_TYPE_NUM,
                            BUS_SEG,
                            PER_START_DT,
                            OFFSET_CNTRCT_NUM,
                            OFFSET_END_DT,
                            OFFSET_CNTRCT_PRICE,
                            SALES_IN_DOLLARS,
                            DATA_SOURCE_TYPE_NUM,
                            DIR_START_DT,
                            DIR_END_DT,
                            BUNIT_NUM,
                            BUNIT_IDENTTYP,
                            UOM_NAME,
                            PROD_DESC,
                            RBT_AMT,
                            LINE_NUM,
                            LINE_STATUS_NUM,
                            WIP_GEN_DT,
                            PAYMNT_NUM,
                            STAGE_NUM,
                            BASE_PRCLST,
                            USERNAME,
                            LASTMOD,
                            OFFSET_PRCLST,
                            COMMENTS,
                            LOGWIPLN_NUM,
                            BUNIT_IDENT,
                            TRANSACTION_TYPE,
                            ORDER_TYPE,
                            SUBMDAT_NUM,
                            SUBMITEM_NUM,
                            ORDER_NUM,
                            ORDER_LINE_NUM,
                            PROD_NUM,
                            BUNIT_NAME,
                            CHECK_DATE,
                            EARNED_START_DT,
                            EARNED_END_DT,
                            PER_UNIT_CNTRCT_NUM,
                            PER_UNIT_CNTRCT_PRICE,
                            BUNITTYP_ID)
         SELECT RBT_HDR_NUM,
                RBT_TERM_NUM,
                RBT_WIP_NUM,
                BASE_CNTRCT_NUM,
                INC_PR_SALES,
                PROD_ID_PRI,
                LINE_CLASS_CD,
                OFFSET_ST_DT,
                EARNED_YR,
                EARNED_MONTH,
                PER_UNIT_RBT,
                DI_NUM,
                DI_LINE_NUM,
                QTY,
                BASE_CNTRCT_PRICE,
                STRATEGY_TYPE_NUM,
                BUS_SEG,
                PER_START_DT,
                OFFSET_CNTRCT_NUM,
                OFFSET_END_DT,
                OFFSET_CNTRCT_PRICE,
                (SALES_IN_DOLLARS / i_total_sales) * i_vir_sales,
                DATA_SOURCE_TYPE_NUM,
                DIR_START_DT,
                DIR_END_DT,
                BUNIT_NUM,
                BUNIT_IDENTTYP,
                UOM_NAME,
                PROD_DESC,
                (SALES_IN_DOLLARS / i_total_sales) * i_vir_amt,
                fn_linenum (g_lnnum),
                LINE_STATUS_NUM,
                WIP_GEN_DT,
                PAYMNT_NUM,
                2,
                BASE_PRCLST,
                USERNAME,
                LASTMOD,
                OFFSET_PRCLST,
                COMMENTS,
                logtable_sq.NEXTVAL,
                BUNIT_IDENT,
                TRANSACTION_TYPE,
                ORDER_TYPE,
                SUBMDAT_NUM,
                SUBMITEM_NUM,
                ORDER_NUM,
                ORDER_LINE_NUM,
                PROD_NUM,
                BUNIT_NAME,
                CHECK_DATE,
                EARNED_START_DT,
                EARNED_END_DT,
                PER_UNIT_CNTRCT_NUM,
                PER_UNIT_CNTRCT_PRICE,
                BUNITTYP_ID
           FROM LOGTABLE
          WHERE RBT_WIP_NUM = i_wipnum AND stage_num = 1;

      COMMIT;
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               i_wipnum,
                               g_calnum,
                               i_dsrnum,
                               g_er_status_num,
                               g_isrpt);
         RAISE;
      WHEN OTHERS
      THEN
         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               i_wipnum,
                               g_calnum,
                               i_dsrnum,
                               g_er_status_num,
                               g_isrpt);
         ROLLBACK;
         RAISE;
   END p_dir_stg2dsrc;

   PROCEDURE p_elgbl_paid_rbt_lines (i_hdrnum            NUMBER,
                                     i_wipnum            NUMBER,
                                     i_termnum           NUMBER,
                                     i_prdsdt            DATE,
                                     i_prdedt            DATE,
                                     o_paid_rbtamt   OUT NUMBER)
   AS
      l_count   NUMBER;
   BEGIN
      o_paid_rbtamt := 0;
      p_log (
         i_proc      => ' p_elgbl_paid_RBT_lines',
         i_message   =>    'Procedure start to filter eligible paid rebate lines for rbt_hdr: '
                        || i_hdrnum);

      SELECT COUNT (*)
        INTO l_count
        FROM VIR_PAID_REBATES
       WHERE vir_hdr_num = i_hdrnum;

      IF l_count > 0
      THEN
         DELETE FROM VIR_PAID_REBATES
               WHERE vir_hdr_num = i_hdrnum;

         COMMIT;
      END IF;

      INSERT INTO VIR_PAID_REBATES
         SELECT VIR_PAID_REBATES_SQ.NEXTVAL,
                i_hdrnum,
                i_termnum,
                i_wipnum,
                RWD.RBT_WIP_DTL_NUM,
                RWD.RBT_HDR_NUM,
                RWD.RBT_TERM_NUM,
                RWD.RBT_WIP_NUM,
                RWD.BASE_CNTRCT_NUM,
                RWD.INC_PR_SALES,
                RWD.PROD_ID_PRI,
                RWD.LINE_CLASS_CD,
                RWD.OFFSET_ST_DT,
                RWD.EARNED_YR,
                RWD.EARNED_MONTH,
                RWD.PER_UNIT_RBT,
                RWD.DI_NUM,
                RWD.DI_LINE_NUM,
                RWD.BASE_CNTRCT_PRICE,
                RWD.STRATEGY_TYPE_NUM,
                RWD.BUS_SEG,
                RWD.PER_START_DT,
                RWD.OFFSET_CNTRCT_NUM,
                RWD.OFFSET_END_DT,
                RWD.OFFSET_CNTRCT_PRICE,
                RWD.SALES_IN_DOLLARS,
                RWD.DATA_SOURCE_TYPE_NUM,
                RWD.DIR_START_DT,
                RWD.DIR_END_DT,
                RWD.BUNIT_NUM,
                RWD.BUNIT_IDENTTYP,
                RWD.UOM_NAME,
                RWD.PROD_DESC,
                RWD.RBT_AMT,
                RWD.LINE_NUM,
                RWD.LINE_STATUS_NUM,
                RWD.WIP_GEN_DT,
                RWD.PAYMNT_NUM,
                RWD.BASE_PRCLST,
                RWD.USERNAME,
                RWD.LASTMOD,
                RWD.OFFSET_PRCLST,
                RWD.COMMENTS,
                RWD.BUNIT_IDENT,
                RWD.TRANSACTION_TYPE,
                RWD.ORDER_TYPE,
                RWD.SUBMDAT_NUM,
                RWD.SUBMITEM_NUM,
                RWD.ORDER_NUM,
                RWD.ORDER_LINE_NUM,
                RWD.QTY,
                RWD.BUNIT_NAME,
                RWD.EARNED_START_DT,
                RWD.EARNED_END_DT,
                RWD.MODIFIED_BY,
                RWD.PER_UNIT_CNTRCT_NUM,
                RWD.PER_UNIT_CNTRCT_PRICE,
                RWD.BUNIT_IDENT_SEC,
                RWD.BUNIT_IDENTTYP_SEC,
                RWD.BUNITTYP_ID
           FROM rbt_wip_dtl rwd
          , rbt_hdr rh
          --, rbt_terms rt
          WHERE                             RWD.RBT_HDR_NUM = RH.RBT_HDR_NUM
--                                    AND RWD.RBT_WIP_NUM = RW.RBT_WIP_NUM
                    --                AND RT.RBT_HDR_NUM = RWD.RBT_HDR_NUM
                    --                AND RT.STATUS_GRBT_NUM = 7
                    --                AND CASE
                    --                       WHEN RW.RBT_WIP_PRD_START_DT >= RT.RBT_TERM_START_DT
                    --                       THEN
                    --                          RW.RBT_WIP_PRD_START_DT
                    --                       WHEN (    rt.rbt_term_start_dt >=
                    --                                    rw.rbt_wip_prd_start_dt
                    --                             AND rt.rbt_term_start_dt <=
                    --                                    rw.rbt_wip_prd_end_dt)
                    --                       THEN
                    --                          rt.rbt_term_start_dt
                    --                    END >= i_prdsdt
                    --                AND CASE
                    --                       WHEN RW.rbt_wip_prd_end_dt >= RT.rbt_term_end_dt
                    --                       THEN
                    --                          Rt.rbt_term_end_dt
                    --                       WHEN (    rw.rbt_wip_prd_end_dt >=
                    --                                    rt.rbt_term_start_dt
                    --                             AND rw.rbt_wip_prd_end_dt <= rt.rbt_term_end_dt)
                    --                       THEN
                    --                          rw.rbt_wip_prd_end_dt
                    --                    END <= i_prdedt
                    --                AND
                   AND RWD.RBT_WIP_NUM IN (SELECT  rbt_wip_num
                                          FROM VIR_REDUCTION_WIP vrw
                                         WHERE vrw.rbt_hdr_num = i_hdrnum)
               AND (EXISTS
                        (SELECT 1
                           FROM vir_prclst_excl price
                          WHERE
                                   PRICE.RBT_HDR_NUM = i_hdrnum
                                   AND ( price.prclst_num IN (-1, -2, -3) AND EXISTS
                                          (SELECT 1
                                             FROM gr_pbasis_mv a,
                                                  gr_pbasiscd_mv c,
                                                  gr_status_mv d,
                                                  gr_uom_mv e,
                                                  gr_prod_mv b
                                            WHERE     B.PROD_ID_PRI =
                                                         RWD.PROD_ID_PRI
                                                  AND A.PROD_NUM = B.PROD_NUM
                                                  AND e.uom_num = a.uom_num
                                                  AND e.uom_name = 'PACKAGES'
                                                  AND a.pbasiscd_num =
                                                         c.pbasiscd_num
                                                  AND (    (       price.prclst_num =
                                                                      -3
                                                               AND c.pbasiscd_ID IN
                                                                      ('WAC',
                                                                       'GR_LIST')
                                                               AND (RWD.BASE_PRCLST IN
                                                                       ('*ALL',
                                                                        'WAC',
                                                                        'LIST'))
                                                            OR (    c.pbasiscd_id =
                                                                       'WAC'
                                                                AND RWD.BASE_PRCLST =
                                                                       'WAC'
                                                                AND price.prclst_num =
                                                                       -1)
                                                            OR (    c.pbasiscd_id =
                                                                       'GR_LIST'
                                                                AND RWD.BASE_PRCLST =
                                                                       'LIST'
                                                                AND price.prclst_num =
                                                                       -2))
                                                       AND d.status_num =
                                                              a.status_num
                                                       AND d.status_abbr IN
                                                              ('ACT',
                                                               'CDP',
                                                               'CP',
                                                               'EP')))
                                OR (EXISTS
                                       (SELECT 1
                                          FROM gr_cpgrp_mv a
                                         WHERE (    (   price.prclst_num = -3
                                                     OR a.cpgrp_num =
                                                           price.prclst_num)
                                                AND (
                                                --Modified for UAT fix
                                                a.cpgrp_cont_id_alias = RWD.BASE_PRCLST
                                                OR  a.cpgrp_cont_id_alias = RWD.OFFSET_PRCLST

--                                                        NVL (
--                                                           RWD.OFFSET_PRCLST,
--                                                           RWD.BASE_PRCLST)

                                                           )))))))

                AND EXISTS
                       (SELECT 1
                          FROM gr_prod_mv p1,
                               gr_prodstat_mv p2,
                               gr_status_mv p3,
                               vir_prod_excl p4
                         WHERE     (   p4.prod_num = -3
                                    OR p4.prod_num = p1.prod_num)
                               AND p4.rbt_hdr_num = i_hdrnum
                               AND p4.incl_flag = 'Y'
                               AND P1.PROD_ID_PRI = RWD.PROD_ID_PRI
                               AND p2.prodstat_num = p1.prodstat_num
                               AND p3.status_num = p2.status_num
                               AND p3.status_abbr IN ('ACT', 'CDP', 'CP'))

                      AND ( ( (g_dsrnum = g_direct OR (g_dsrnum = g_both AND RH.DATA_SOURCE_NUM=72 )) AND EXISTS
                             (SELECT 1
                                FROM (SELECT DISTINCT
                                             cwa.cust_whl_num buntno,
                                             cwa.rbt_hdr_num hdrnum
                                        FROM cust_whl_assoc cwa
                                       WHERE     cwa.rbt_hdr_num = i_hdrnum
                                             AND cwa.cust_or_whl = 'CUST'
                                      UNION ALL
                                      SELECT cwa.child_num buntno,
                                             cwa.rbt_hdr_num hdrnum
                                        FROM cust_whl_assoc cwa
                                       WHERE     cwa.rbt_hdr_num = i_hdrnum
                                             AND cwa.cust_or_whl = 'CUST') cust
                               WHERE     cust.buntno <> 0
                                     AND cust.buntno = RWD.BUNIT_NUM)) or  (g_dsrnum = g_indirect or (g_dsrnum = g_both  AND RH.DATA_SOURCE_NUM<>72) )   );



      --                AND NOT EXISTS
      --                           (SELECT plxl.prclst_num
      --                              FROM vir_prclst_excl plxl
      --                             WHERE     plxl.rbt_hdr_num = i_hdrnum
      --                                   AND plxl.incl_flag = 'N'
      --                                   AND (   (EXISTS
      --                                               (SELECT a.prod_num
      --                                                  FROM gr_pbasis_mv a,
      --                                                       gr_pbasiscd_mv b,
      --                                                       gr_uom_mv c
      --                                                 WHERE     b.pbasiscd_num =
      --                                                              a.pbasiscd_num
      --                                                       AND c.uom_num =
      --                                                              a.uom_num
      --                                                       AND c.uom_name =
      --                                                              'PACKAGES'
      --                                                       AND (   (    plxl.prclst_num =
      --                                                                       -1
      --                                                                AND b.pbasiscd_id =
      --                                                                       'WAC'
      --                                                                AND RWD.BASE_PRCLST =
      --                                                                       'WAC')
      --                                                            OR (    plxl.prclst_num =
      --                                                                       -2
      --                                                                AND b.pbasiscd_id =
      --                                                                       'GR_LIST'
      --                                                                AND RWD.BASE_PRCLST =
      --                                                                       'LIST')
      --                                                            OR (    plxl.prclst_num =
      --                                                                       -3
      --                                                                AND b.pbasiscd_id IN
      --                                                                       ('WAC',
      --                                                                        'GR_LIST')
      --                                                                AND RWD.BASE_PRCLST IN
      --                                                                       ('*ALL',
      --                                                                        'WAC',
      --                                                                        'LIST')))))
      --                                        OR EXISTS
      --                                              (SELECT 1
      --                                                 FROM gr_cpgrp_mv x
      --                                                WHERE     (   x.cpgrp_num =
      --                                                                 plxl.prclst_num
      --                                                           OR plxl.prclst_num =
      --                                                                 -3)
      --                                                      AND x.cpgrp_cont_id_alias =
      --                                                             RWD.BASE_PRCLST)))
      --                AND NOT EXISTS
      --                           (SELECT 1
      --                              FROM gr_prod_mv p1,
      --                                   gr_prodstat_mv p2,
      --                                   gr_status_mv p3,
      --                                   vir_prod_excl p4
      --                             WHERE     p4.prod_num = p1.prod_num
      --                                   AND p4.rbt_hdr_num = i_hdrnum
      --                                   AND p4.incl_flag = 'N'
      --                                   AND P1.PROD_ID_PRI = RWD.PROD_ID_PRI
      --                                   AND p2.prodstat_num = p1.prodstat_num
      --                                   AND p3.status_num = p2.status_num
      --                                   AND p3.status_abbr IN ('ACT', 'CDP', 'CP')
      --                                   );

      COMMIT;

      SELECT SUM (VPR.RBT_AMT)
        INTO O_PAID_RBTAMT
        FROM VIR_PAID_REBATES VPR
       WHERE VPR.VIR_HDR_NUM = i_hdrnum;

      p_log (
         i_proc      => ' p_elgbl_paid_rbt_lines',
         i_message   =>    'Procedure end to filter eligible paid rebate lines for rbt_hdr: '
                        || i_hdrnum);
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         O_PAID_RBTAMT := 0;
         p_log (
            i_proc      => ' p_elgbl_paid_rbt_lines',
            i_message   =>    'No eligible paid rebates lines found for header'
                           || i_hdrnum);
      WHEN OTHERS
      THEN
         O_PAID_RBTAMT := 0;
         p_log (
            i_proc      => ' p_elgbl_paid_rbt_lines',
            i_message   => 'Error in fetching paid rebate lines' || i_hdrnum);
         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               i_wipnum,
                               g_calnum,
                               g_dsrnum,
                               g_er_status_num,
                               g_isrpt);
   END p_elgbl_paid_rbt_lines;



   PROCEDURE p_VIR_updtwip (i_wipnum                  NUMBER,
                            i_rbtamt                  NUMBER DEFAULT 0.0,
                            i_rbtqty                  NUMBER DEFAULT 0.0,
                            i_rbtsls                  NUMBER DEFAULT 0.0,
                            i_wipsts                  NUMBER,
                            i_total_original_sales    NUMBER DEFAULT 0.0,
                            i_total_paid_rebates      NUMBER DEFAULT 0.0,
                            i_vir_rbt_per             VARCHAR2 DEFAULT ' ')
   AS
   BEGIN
      -- Process log enties for Procedure Rebate Wip status update with audit columns

      p_log (
         i_proc      => ' p_VIR_updtwip',
         i_message   => 'Procedure for Rebate Wip status update with audit columns started');

      UPDATE grebates.rbt_wip rw
         SET rw.rbt_amt = NVL (i_rbtamt, 0.0),
             rw.qty = NVL (i_rbtqty, 0.0),
             rw.sales_amt = NVL (i_rbtsls, 0.0),
             rw.status_grbt_num = i_wipsts,
             rw.username = g_username,
             rw.actual_gen_dt = g_actual_gen_dt,
             rw.lastmod = SYSDATE,
             rw.total_original_sales = NVL (i_total_original_sales, 0.0),
             rw.total_paid_rebates = NVL (i_total_paid_rebates, 0.0),
             rw.vir_rbt_per = NVL (i_vir_rbt_per, '0%')
       WHERE rw.rbt_wip_num = i_wipnum;

      COMMIT;
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               i_wipnum,
                               g_calnum,
                               g_dsrnum,
                               g_er_status_num,
                               g_isrpt);
         RAISE;
      WHEN OTHERS
      THEN
         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               i_wipnum,
                               g_calnum,
                               g_dsrnum,
                               g_er_status_num,
                               g_isrpt);
         ROLLBACK;
         RAISE;
   END p_VIR_updtwip;

   -- Procedure for an Direct Data Source validation for a Stage - 1
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
                                                                         )
   AS
      -- Local variable declared in Direct Data Source
      l_dirordr   NUMBER;                       --Value for primary qlfr order
      l_dirinvc   NUMBER;             --Invoice Value for primary qlfr invoice
      l_default   NUMBER;                                --Default for capping
      l_wacprc    NUMBER;
      l_lstprc    NUMBER;
   BEGIN
      -- Process log enties for Procedure for an Direct Data Source validation for a Stage - 1

      p_log (
         i_proc      => ' p_vir_dirdsrc',
         i_message   => 'Procedure for an Direct Data Source validation for a Stage - 1 started');

      -- Referenced values for Primary Oder/Invoice, Secondary Invoice/Order

      SELECT ref_num
        INTO l_dirinvc
        FROM cd_references cr
       WHERE cr.ref_typ = 'VirDirDtQualifier' AND cr.ref_typ_val = 'Invoice';

      SELECT ref_num
        INTO l_dirordr
        FROM cd_references cr
       WHERE cr.ref_typ = 'VirDirDtQualifier' AND cr.ref_typ_val = 'Order';

      SELECT ref_num
        INTO l_default
        FROM cd_references cr
       WHERE cr.ref_typ = 'Default' AND cr.ref_typ_val = 'NullValue';

      SELECT p1.pbasiscd_num
        INTO l_wacprc
        FROM gr_pbasiscd_mv p1
       WHERE p1.pbasiscd_id = 'WAC';

      SELECT p1.pbasiscd_num
        INTO l_lstprc
        FROM gr_pbasiscd_mv p1
       WHERE p1.pbasiscd_id = 'GR_LIST';

      g_lnnum := 0;

      INSERT /*+ append */
            INTO  logtable_temp (logwipln_num,
                                 rbt_hdr_num,
                                 rbt_term_num,
                                 earned_end_dt,
                                 earned_start_dt,
                                 rbt_wip_num,
                                 prod_num,
                                 base_cntrct_num,
                                 prod_id_pri,
                                 line_class_cd,
                                 earned_yr,
                                 earned_month,
                                 di_num,
                                 di_line_num,
                                 order_num,
                                 order_line_num,
                                 qty,
                                 base_cntrct_price,
                                 strategy_type_num,
                                 bus_seg,
                                 per_start_dt,
                                 sales_in_dollars,
                                 data_source_type_num,
                                 dir_start_dt,
                                 dir_end_dt,
                                 bunit_num,
                                 bunit_name,
                                 bunit_ident,
                                 uom_name,
                                 prod_desc,
                                 line_num,
                                 line_status_num,
                                 wip_gen_dt,
                                 paymnt_num,
                                 stage_num,
                                 base_prclst,
                                 username,
                                 lastmod,
                                 order_type,
                                 transaction_type,
                                 bunittyp_id,
                                 bunit_identtyp)
         SELECT logtable_sq.NEXTVAL,
                rbt_hdr_num,
                rbt_term_num,
                i_ern_e_dt_in trmernedt,
                i_ern_s_dt_in trmernsdt,
                i_wipnum,
                prod_num,
                prclst_num,
                ndc11,
                class_cd,
                yr,
                mt,
                invoice_num,
                invoice_line_num,
                order_num,
                order_line_num,
                NVL (qty, 0.0),
                NVL (bcnprc, 0.0),
                strategy_type_num,
                rbt_hdr_desc,
                i_prdsdt,
                NVL (qty * bcnprc, 0.0),
                i_dsrnum,
                Order_start_dt,
                Order_end_dt,
                buntno,
                buntnm,
                buntid,
                'PACKAGES',
                prodds,
                fn_linenum (g_lnnum),
                g_status_num,
                i_wipgdt,
                paymnt_num,
                1,
                CASE
                   WHEN contract_id = 'GRSNON-WHSRETN' AND cntrct_id = 'LIST'
                   THEN
                      'RETURN'
                   WHEN contract_id = 'GRSNON-WHSSTD' AND cntrct_id = 'LIST'
                   THEN
                      'LIST'
                   ELSE
                      cntrct_id
                END
                   cntrct,
                g_username,
                SYSDATE,
                order_type,
                transaction_type,
                bunittyp_id_soldto,
                butype_idtyp_soldto_sec
           FROM (SELECT rh.rbt_hdr_num,
                        rt.rbt_term_num,
                        rt.earned_end_dt,
                        rt.earned_start_dt,
                        rbtdirsls.prod_num,
                        price.prclst_num,
                        rbtdirsls.ndc11,
                        CASE
                           WHEN rbtdirsls.transaction_type = 'SLS'
                           THEN
                              i_cr_lccode
                           WHEN rbtdirsls.transaction_type = 'RTN'
                           THEN
                              i_dr_lccode
                           WHEN rbtdirsls.transaction_type = 'FIN'
                           THEN
                              i_dr_lccode
                        END
                           class_cd,
                        TO_CHAR (rbtdirsls.sales_dt, 'YY') YR,
                        TO_CHAR (rbtdirsls.sales_dt, 'MM') MT,
                        rbtdirsls.invoice_num,
                        rbtdirsls.invoice_line_num,
                        rbtdirsls.order_num,
                        rbtdirsls.order_line_num,
                        ROUND (NVL (rbtdirsls.quantity, 0.0), 3) qty,
                        ROUND (rbtdirsls.amount / rbtdirsls.quantity, 5)
                           bcnprc,
                        rh.strategy_type_num,
                        rh.rbt_hdr_desc,
                        rh.data_source_num,
                        rbtdirsls.sales_dt order_start_dt,
                        rbtdirsls.sales_dt order_end_dt,
                        NVL (rbtdirsls.cust_num_soldto, rbtcuswhl.buntno)
                           buntno,
                        rbtcuswhl.buntnm,
                        DECODE (rbtdirsls.cust_num_soldto,
                                NULL, rbtcuswhl.buntid,
                                rbtdirsls.butype_id_soldto_sec)
                           buntid,
                        'PACKAGES',
                        (SELECT p.PROD_DESC
                           FROM gr_prod_mv p
                          WHERE p.prod_num = rbtdirsls.prod_num)
                           prodds,
                        rh.paymnt_num,
                        CASE
                           WHEN     price.prclst_num = -3
                                AND rbtdirsls.contract_id IS NULL
                           THEN
                              '*ALL'
                           WHEN     price.prclst_num = -1
                                AND rbtdirsls.contract_id IS NULL
                           THEN
                              'WAC'
                           WHEN     price.prclst_num = -2
                                AND rbtdirsls.contract_id IS NULL
                           THEN
                              'LIST'
                           -- [SV]:27-06-2012, Defect 5991 - Contract id with NUlls not displaying when POS and Pricing QLFR is set to NO
                           WHEN     price.prclst_num > 0 --AND rt.PRC_QLFR = 'N' --Nagendra, 13th May 2013, Pricing classifier changes
                                AND rbtdirsls.contract_id IS NULL
                           THEN
                              rbtdirsls.contract_id
                           WHEN     (   price.prclst_num > 0
                                     OR price.prclst_num = -3)
                                AND rbtdirsls.contract_id LIKE 'C%-%'
                           THEN
                              rbtdirsls.contract_id
                           WHEN     (   price.prclst_num = -3
                                     OR price.prclst_num = -2)
                                AND rbtdirsls.contract_id = 'GRSNON-WHSSTD'
                           THEN
                              'LIST'
                           WHEN     (   price.prclst_num = -3
                                     OR price.prclst_num = -2)
                                AND rbtdirsls.contract_id = 'GRSNON-WHSRETN'
                           THEN
                              'RETURN'
                           WHEN     (   price.prclst_num = -3
                                     OR price.prclst_num = -1)
                                AND rbtdirsls.contract_id = 'GRWHSSTD'
                           THEN
                              'WAC'
                           ELSE
                              rbtdirsls.contract_id
                        END
                           cntrct_id,
                        rbtdirsls.order_type,
                        rbtdirsls.transaction_type,
                        rbtdirsls.contract_id,
                        rbtdirsls.bunittyp_id_soldto,
                        rbtdirsls.butype_idtyp_soldto_sec
                   FROM dirsales_in rbtdirsls,
                        status_grbt sg,
                        rbt_hdr rh,
                        (SELECT cust.hdrnum hdrnum,
                                grtpm.bunit_num buntno,
                                grtpm.bunit_name buntnm,
                                grtpidm.buid_identifier buntid,
                                grtpidm.buidtyp_id buntdc
                           FROM gr_bunit_mv grtpm,
                                gr_bunitstat_mv grbstat,
                                gr_status_mv grstat,
                                (SELECT a.bunit_num,
                                        a.buidtyp_id,
                                        a.buid_identifier
                                   FROM gr_buid_mv a --                        , gr_status_mv c
                                                    --                       WHERE     c.status_num = a.status_num
                                                    --                             AND c.status_abbr IN ('ACT', 'CDP', 'CP')
                                ) grtpidm,
                                (  SELECT a.buntno, a.hdrnum
                                     FROM (SELECT cwa.cust_whl_num buntno,
                                                  cwa.rbt_hdr_num hdrnum
                                             FROM cust_whl_assoc cwa
                                            WHERE     cwa.rbt_hdr_num =
                                                         i_hdrnum
                                                  AND cwa.cust_or_whl = 'CUST'
                                           UNION ALL
                                           SELECT cwa.child_num buntno,
                                                  cwa.rbt_hdr_num hdrnum
                                             FROM cust_whl_assoc cwa
                                            WHERE     cwa.rbt_hdr_num =
                                                         i_hdrnum
                                                  AND cwa.cust_or_whl = 'CUST' --                                                  AND (   EXISTS
                                                                              --                                                             (SELECT 1
                                                                              --                                                                FROM grebates.gr_mbr_mv mbr,
                                                                              --                                                                     grebates.gr_status_mv st
                                                                              --                                                               WHERE     mbr.ctorg_bunit_num =
                                                                              --                                                                            cwa.cust_whl_num
                                                                              --                                                                     AND mbr.mbr_bunit_num =
                                                                              --                                                                            cwa.child_num
                                                                              --                                                                     AND mbr.status_num =
                                                                              --                                                                            st.status_num
                                                                              --                                                                     AND status_cd =
                                                                              --                                                                            'mbr' --                                                          AND status_abbr IN
                                                                              --                                                                                 --                                                                 ('ACT',
                                                                              --                                                                                 --                                                                  'CDP',
                                                                              --                                                                                 --                                                                  'EP')
                                                                              --                                                          )
                                                                              --                                                       OR cwa.child_num = 0)
                                          ) a
                                    WHERE a.buntno <> 0
                                 GROUP BY buntno, hdrnum) cust
                          WHERE     cust.buntno = grtpm.bunit_num
                                AND grtpm.bunit_num = grtpidm.bunit_num
                                AND grtpm.bunitstat_num =
                                       grbstat.bunitstat_num
                                AND grbstat.status_num = grstat.status_num
                                -- AND grstat.status_abbr IN ('ACT', 'CDP', 'CP')
                                AND grtpm.bucattyp_cd IN
                                       ('GRS-WHL', 'GRS-RCS', 'GRS-ACT')) rbtcuswhl,
                        vir_prclst_excl price,
                        rbt_terms rt
                  WHERE     rh.rbt_hdr_num = i_hdrnum
                        AND rt.rbt_term_num = i_trmnum
                        AND rh.rbt_hdr_num = rt.rbt_hdr_num
                        AND sg.status_grbt_num = rt.status_grbt_num
                        AND sg.status_abbr = 'AC'
                        AND rh.rbt_hdr_num = rbtcuswhl.hdrnum
                        AND rh.rbt_hdr_num = price.rbt_hdr_num
                        AND price.incl_flag = 'Y'
                        AND price.data_source_num = i_dsrnum
                        AND (   ( (SELECT ref_typ_val
                                     FROM cd_references
                                    WHERE ref_num = RH.PROD_SELECTION_NUM) =
                                    '*ALL')
                             OR EXISTS
                                   (SELECT 1
                                      FROM gr_prod_mv p1,
                                           gr_prodstat_mv p2,
                                           gr_status_mv p3,
                                           vir_prod_excl p4
                                     WHERE     (   p4.prod_num = -3
                                                OR p4.prod_num = p1.prod_num)
                                           AND p4.rbt_hdr_num =
                                                  rh.rbt_hdr_num
                                           AND p4.incl_flag = 'Y'
                                           AND p4.data_source_num = i_dsrnum
                                           AND p1.prod_num =
                                                  rbtdirsls.prod_num
                                           AND p2.prodstat_num =
                                                  p1.prodstat_num
                                           AND p3.status_num = p2.status_num -- AND p3.status_abbr IN ('ACT', 'CDP', 'CP')
                                                                            ))
                        AND rbtdirsls.cust_num_soldto = rbtcuswhl.buntno
                        AND rbtdirsls.butype_id_soldto = rbtcuswhl.buntid
                        AND EXISTS
                               (SELECT 1
                                  FROM grebates.gr_mbr_mv mbr,
                                       grebates.gr_status_mv st,
                                       grebates.cust_whl_assoc cwac
                                 WHERE     mbr.ctorg_bunit_num =
                                              cwac.cust_whl_num
                                       AND mbr.mbr_bunit_num = cwac.child_num
                                       AND mbr.status_num = st.status_num
                                       AND status_cd = 'mbr'
                                       -- AND status_abbr IN ('ACT', 'CDP', 'EP')
                                       AND rbt_hdr_num = i_hdrnum
                                       AND cwac.child_num =
                                              rbtdirsls.cust_num_soldto
                                       AND (       RH.DATE_QUALIFIER_NUM =
                                                      l_dirinvc          -- 97
                                               AND TRUNC (rbtdirsls.sales_dt) BETWEEN mbr.mbr_dt_start
                                                                                  AND NVL (
                                                                                         mbr.mbr_dt_end,
                                                                                         SYSDATE)
                                            OR     RH.DATE_QUALIFIER_NUM =
                                                      l_dirordr          -- 96
                                               AND TRUNC (rbtdirsls.order_dt) BETWEEN mbr.mbr_dt_start
                                                                                  AND NVL (
                                                                                         mbr.mbr_dt_end,
                                                                                         SYSDATE)))
                        AND (   (    price.prclst_num IN (-1, -2, -3)
                                 AND EXISTS
                                        (SELECT 1
                                           FROM gr_pbasis_mv a,
                                                gr_pbasiscd_mv c,
                                                gr_status_mv d,
                                                gr_uom_mv e
                                          WHERE     a.prod_num =
                                                       rbtdirsls.prod_num
                                                AND e.uom_num = a.uom_num
                                                AND e.uom_name = 'PACKAGES'
                                                AND a.pbasiscd_num =
                                                       c.pbasiscd_num
                                                AND (   (    price.prclst_num =
                                                                -3
                                                         AND c.pbasiscd_ID IN
                                                                ('WAC',
                                                                 'GR_LIST')
                                                         AND (   rbtdirsls.contract_id IN
                                                                    ('GRWHSSTD',
                                                                     'GRSNON-WHSSTD',
                                                                     'GRSNON-WHSRETN',
                                                                     'WAC',
                                                                     '-1',
                                                                     'LIST',
                                                                     '-2',
                                                                     '*All',
                                                                     '-3')
                                                              OR rbtdirsls.contract_id
                                                                    IS NULL))
                                                     OR (    c.pbasiscd_id =
                                                                'WAC'
                                                         AND rbtdirsls.contract_id IN
                                                                ('GRWHSSTD',
                                                                 'WAC',
                                                                 '-1')
                                                         AND price.prclst_num =
                                                                -1)
                                                     OR (    c.pbasiscd_id =
                                                                'GR_LIST'
                                                         AND rbtdirsls.contract_id IN
                                                                ('GRSNON-WHSSTD',
                                                                 'GRSNON-WHSRETN',
                                                                 'LIST',
                                                                 '-2')
                                                         AND price.prclst_num =
                                                                -2))
                                                AND d.status_num =
                                                       a.status_num
                                                AND d.status_abbr IN
                                                       ('ACT',
                                                        'CDP',
                                                        'CP',
                                                        'EP')))
                             OR (EXISTS
                                    (SELECT 1
                                       FROM gr_cpgrp_mv a
                                      WHERE (    (   price.prclst_num = -3
                                                  OR a.cpgrp_num =
                                                        price.prclst_num)
                                             AND a.cpgrp_cont_id_alias =
                                                    rbtdirsls.contract_id)))
                             OR (    rbtdirsls.contract_id IS NULL
                                 AND price.prclst_num IN (-3, -1, -2)))
                        AND (   (    rh.gross_net = 'G'
                                 AND (   rbtdirsls.transaction_type IN
                                            ('SLS')
                                      OR (    rbtdirsls.transaction_type IN
                                                 ('FIN')
                                          AND rbtdirsls.order_type IN
                                                 ('R01',
                                                  'R02',
                                                  'R03',
                                                  'R04',
                                                  'R05',
                                                  'R06',
                                                  'R07',
                                                  'R08',
                                                  'R09',
                                                  'R10',
                                                  'R11',
                                                  'R12',
                                                  'R13',
                                                  'R14',
                                                  'R15',
                                                  'R16',
                                                  'R17',
                                                  'R18',
                                                  'R19',
                                                  'R20',
                                                  'R21',
                                                  'R22',
                                                  'R23',
                                                  'R24',
                                                  'R25',
                                                  'R26',
                                                  'R27',
                                                  'R28',
                                                  'R29',
                                                  'R30',
                                                  'R31',
                                                  'R32',
                                                  'R33',
                                                  'R34',
                                                  'R35',
                                                  'R36',
                                                  'R37',
                                                  'R38',
                                                  'R39',
                                                  'R40',
                                                  'R41',
                                                  'R42',
                                                  'R43',
                                                  'R44',
                                                  'R45',
                                                  'R46',
                                                  'R47',
                                                  'R48',
                                                  'R49'))))
                             OR (    rh.gross_net = 'N'
                                 AND (   rbtdirsls.transaction_type IN
                                            ('SLS', 'RTN')
                                      OR (    rbtdirsls.transaction_type IN
                                                 ('FIN')
                                          AND rbtdirsls.order_type NOT IN
                                                 ('SS1',
                                                  'SS2',
                                                  'SS3',
                                                  'SS4',
                                                  'SS5',
                                                  'SS6'))))
                        )
                       AND (   (    RH.DATE_QUALIFIER_NUM = l_dirinvc
                                 AND rbtdirsls.sales_dt BETWEEN i_prdsdt
                                                            AND i_prdedt)
                             OR (    RH.DATE_QUALIFIER_NUM = l_dirordr
                                 AND rbtdirsls.order_dt BETWEEN i_prdsdt
                                                            AND i_prdedt)) --                        AND NOT EXISTS
                                                                          --                                   (SELECT plxl.prclst_num
                                                                          --                                      FROM vir_prclst_excl plxl
                                                                          --                                     WHERE     plxl.rbt_hdr_num =
                                                                          --                                                  rh.rbt_hdr_num
                                                                          --                                           AND plxl.incl_flag = 'N'
                                                                          --                                           AND (   (EXISTS
                                                                          --                                                       (SELECT a.prod_num
                                                                          --                                                          FROM gr_pbasis_mv a,
                                                                          --                                                               gr_pbasiscd_mv b,
                                                                          --                                                               gr_uom_mv c
                                                                          --                                                         WHERE     b.pbasiscd_num =
                                                                          --                                                                      a.pbasiscd_num
                                                                          --                                                               AND c.uom_num =
                                                                          --                                                                      a.uom_num
                                                                          --                                                               AND c.uom_name =
                                                                          --                                                                      'PACKAGES'
                                                                          --                                                               AND (   (    plxl.prclst_num =
                                                                          --                                                                               -1
                                                                          --                                                                        AND b.pbasiscd_id =
                                                                          --                                                                               'WAC'
                                                                          --                                                                        AND rbtdirsls.contract_id =
                                                                          --                                                                               'GRWHSSTD')
                                                                          --                                                                    OR (    plxl.prclst_num =
                                                                          --                                                                               -2
                                                                          --                                                                        AND b.pbasiscd_id =
                                                                          --                                                                               'GR_LIST'
                                                                          --                                                                        AND rbtdirsls.contract_id IN
                                                                          --                                                                               ('GRSNON-WHSSTD',
                                                                          --                                                                                'GRSNON-WHSRETN'))
                                                                          --                                                                    OR (    plxl.prclst_num =
                                                                          --                                                                               -3
                                                                          --                                                                        AND b.pbasiscd_id IN
                                                                          --                                                                               ('WAC',
                                                                          --                                                                                'GR_LIST')
                                                                          --                                                                        AND rbtdirsls.contract_id IN
                                                                          --                                                                               ('GRWHSSTD',
                                                                          --                                                                                'GRSNON-WHSSTD',
                                                                          --                                                                                'GRSNON-WHSRETN')))))
                                                                          --                                                OR EXISTS
                                                                          --                                                      (SELECT 1
                                                                          --                                                         FROM gr_cpgrp_mv x
                                                                          --                                                        WHERE     (   x.cpgrp_num =
                                                                          --                                                                         plxl.prclst_num
                                                                          --                                                                   OR plxl.prclst_num =
                                                                          --                                                                         -3)
                                                                          --                                                              AND x.cpgrp_cont_id_alias =
                                                                          --                                                                     rbtdirsls.contract_id)) --                                           AND (   (    RH.DATE_QUALIFIER_NUM =
                                                                          --                                                                                            --                                                           l_dirinvc
                                                                          --                                                                                            --                                                    AND rbtdirsls.sales_dt BETWEEN plxl.prcl_excl_start_dt
                                                                          --                                                                                            --                                                                               AND plxl.prcl_excl_end_dt)
                                                                          --                                                                                            --                                                OR (    RH.DATE_QUALIFIER_NUM =
                                                                          --                                                                                            --                                                           l_dirordr
                                                                          --                                                                                            --                                                    AND rbtdirsls.order_dt BETWEEN plxl.prcl_excl_start_dt
                                                                          --                                                                                            --                                                                               AND plxl.prcl_excl_end_dt))
                                                                          --                                )
                                                                          --                        AND NOT EXISTS
                                                                          --                                   (SELECT prxl.prod_num
                                                                          --                                      FROM vir_prod_excl prxl
                                                                          --                                     WHERE     prxl.rbt_hdr_num =
                                                                          --                                                  rt.rbt_hdr_num
                                                                          --                                           AND prxl.incl_flag = 'N'
                                                                          --                                           AND prxl.prod_num =
                                                                          --                                                  rbtdirsls.prod_num --                                           AND (   (    prxl.excl_dt_qfr_num =
                                                                          --                                                                    --                                                           l_dirinvc
                                                                          --                                                                    --                                                    AND rbtdirsls.sales_dt BETWEEN prxl.prod_excl_start_dt
                                                                          --                                                                    --                                                                               AND prxl.prod_excl_end_dt)
                                                                          --                                                                    --                                                OR (    prxl.excl_dt_qfr_num =
                                                                          --                                                                    --                                                           l_dirordr
                                                                          --                                                                    --                                                    AND rbtdirsls.order_dt BETWEEN prxl.prod_excl_start_dt
                                                                          --                                                                    --                                                                               AND prxl.prod_excl_end_dt))
                                                                          --                                )
                );

      IF SQL%ROWCOUNT = 0
      THEN
         RAISE NO_DATA_FOUND;
      ELSE
         COMMIT;
      END IF;
   EXCEPTION
      WHEN TOO_MANY_ROWS
      THEN
         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               i_wipnum,
                               g_calnum,
                               i_dsrnum,
                               g_er_status_num);
         RAISE;
      WHEN NO_DATA_FOUND
      THEN
         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               i_wipnum,
                               g_calnum,
                               i_dsrnum,
                               g_er_status_num,
                               g_isrpt);
         RAISE;
      WHEN OTHERS
      THEN
         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               i_wipnum,
                               g_calnum,
                               i_dsrnum,
                               g_er_status_num,
                               g_isrpt);
         ROLLBACK;
         RAISE;
   END p_vir_dirdsrc;

   -- Procedure for Data transaformation from Staging to audit tables and Rbt_Wip_Tables
   PROCEDURE p_vir_datains (i_stgnum    NUMBER,
                            i_wipnum    NUMBER,
                            i_dsrnum    NUMBER)
   AS
      l_strgnm    NUMBER;
      l_asrhstg   NUMBER;
   BEGIN
      -- Process log enties for Procedure for Data transaformation from Staging to audit tables and Rbt_Wip_Tables

      p_log (
         i_proc      => ' p_vir_datains',
         i_message   => 'Procedure for Data transaformation from Staging to audit tables and Rbt_Wip_Tables');

      CASE
         WHEN i_stgnum = 1
         THEN
            INSERT /*+ APPEND */
                  INTO  LOGTABLE
               SELECT *
                 FROM LOGTABLE_TEMP
                WHERE RBT_WIP_NUM = i_wipnum AND stage_num = i_stgnum;
         WHEN i_stgnum = 2
         THEN
            g_lnnum := 0;

            INSERT /*+ append */
                  INTO  rbt_wip_dtl (rbt_wip_dtl_num,
                                     rbt_hdr_num,
                                     rbt_term_num,
                                     earned_end_dt,
                                     earned_start_dt,
                                     rbt_wip_num,
                                     base_cntrct_num,
                                     inc_pr_sales,
                                     prod_id_pri,
                                     line_class_cd,
                                     offset_st_dt,
                                     earned_yr,
                                     earned_month,
                                     per_unit_rbt,
                                     di_num,
                                     di_line_num,
                                     qty,
                                     base_cntrct_price,
                                     strategy_type_num,
                                     bus_seg,
                                     per_start_dt,
                                     offset_cntrct_num,
                                     offset_end_dt,
                                     offset_cntrct_price,
                                     sales_in_dollars,
                                     data_source_type_num,
                                     dir_start_dt,
                                     dir_end_dt,
                                     bunit_num,
                                     bunit_name,
                                     bunit_identtyp_sec,
                                     bunit_ident_sec,
                                     uom_name,
                                     prod_desc,
                                     rbt_amt,
                                     line_num,
                                     line_status_num,
                                     wip_gen_dt,
                                     paymnt_num,
                                     base_prclst,
                                     username,
                                     lastmod,
                                     offset_prclst,
                                     order_type,
                                     transaction_type,
                                     order_num,
                                     order_line_num,
                                     submdat_num,
                                     submitem_num,
                                     per_unit_cntrct_num,
                                     per_unit_cntrct_price,
                                     bunittyp_id)
               SELECT grebates.rbt_wip_dtl_sq.NEXTVAL,
                      rbt_hdr_num,
                      rbt_term_num,
                      earned_end_dt,
                      earned_start_dt,
                      rbt_wip_num,
                      base_cntrct_num,
                      inc_pr_sales,
                      prod_id_pri,
                      line_class_cd,
                      offset_st_dt,
                      earned_yr,
                      earned_month,
                      per_unit_rbt,
                      di_num,
                      di_line_num,
                      qty,
                      base_cntrct_price,
                      strategy_type_num,
                      bus_seg,
                      per_start_dt,
                      offset_cntrct_num,
                      offset_end_dt,
                      offset_cntrct_price,
                      ROUND (sales_in_dollars, 5),
                      data_source_type_num,
                      dir_start_dt,
                      dir_end_dt,
                      bunit_num,
                      bunit_name,
                      bunit_identtyp,
                      bunit_ident,
                      uom_name,
                      prod_desc,
                      ROUND (rbt_amt, 2),
                      fn_linenum (g_lnnum),
                      --                      CASE
                      --                         WHEN (rbt_amt = 0 OR base_cntrct_price = 0)
                      --                         THEN
                      --                            g_ca_status_num
                      --                         ELSE
                      g_status_num --                      END
                                   line_status_num,
                      wip_gen_dt,
                      paymnt_num,
                      base_prclst,
                      username,
                      lastmod,
                      offset_prclst,
                      order_type,
                      transaction_type,
                      order_num,
                      order_line_num,
                      submdat_num,
                      submitem_num,
                      per_unit_cntrct_num,
                      per_unit_cntrct_price,
                      bunittyp_id
                 FROM logtable stg
                WHERE     stg.rbt_wip_num = i_wipnum
                      AND stg.stage_num = i_stgnum
                      AND STG.DATA_SOURCE_TYPE_NUM = i_dsrnum;
      END CASE;

      COMMIT;
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               i_wipnum,
                               g_calnum,
                               i_dsrnum,
                               g_er_status_num,
                               g_isrpt);
         RAISE;
      WHEN OTHERS
      THEN
         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               i_wipnum,
                               g_calnum,
                               i_dsrnum,
                               g_er_status_num,
                               g_isrpt);
         ROLLBACK;
         RAISE;
   END p_vir_datains;



   -----------------------------Procedure for Audit Trail  **RBT HDR** Starts  here ----------------------------------------

   PROCEDURE p_rbthdr_snapshot (i_hdrnum        NUMBER,
                                i_status_num    NUMBER,
                                i_wipnum        NUMBER)
   AS
      l_status_grbt_num_IP    NUMBER;
      l_status_grbt_num_SSD   NUMBER;
      l_version               NUMBER := 0;
      l_paymnt_num            NUMBER;
      l_paymnt_num_max        NUMBER;
      l_rbt_hdr_num_max       NUMBER;
   BEGIN
      --creating snapshot for  payment table ----------------------------------------------

      SELECT paymnt_num
        INTO l_paymnt_num
        FROM rbt_hdr
       WHERE rbt_hdr_num = i_hdrnum;

      ----snpshot  insert in to payment table ---------------------------------------------

      INSERT INTO PAYMNT_INFO (PAYMNT_NUM,
                               PAYMNT_TYPE_NUM,
                               CM_ACCT_NUM,
                               CUST_ADD_NUM,
                               CR_LC_CODE,
                               DR_LC_CODE,
                               PO_DR_NUMBER,
                               CUST_REF_NO,
                               CUST_CONT,
                               BANKADDRESSKEY) --GBL21648293i included the bankAddressKey column
         SELECT PAYMNT_INFO_SQ.NEXTVAL,
                P.PAYMNT_TYPE_NUM,
                P.CM_ACCT_NUM,
                P.CUST_ADD_NUM,
                P.CR_LC_CODE,
                P.DR_LC_CODE,
                P.PO_DR_NUMBER,
                P.CUST_REF_NO,
                P.CUST_CONT,
                BANKADDRESSKEY
           FROM GREBATES.PAYMNT_INFO P
          WHERE P.PAYMNT_NUM = l_paymnt_num;

      COMMIT;

      ----choosing the max payment  number -----------------------------------------------------

      SELECT MAX (paymnt_num) INTO l_paymnt_num_max FROM PAYMNT_INFO;


      --This is for fecthing SUPERSEDED status  from   STATUS_GRBT table --------------


      SELECT STATUS_GRBT_NUM
        INTO l_status_grbt_num_SSD
        FROM STATUS_GRBT
       WHERE STATUS_DESC = 'Superseded' AND STATUS_CD = 'SSD';

      --This is for fecthing IN_PRCRESS status  from   STATUS_GRBT table --------------

      SELECT STATUS_GRBT_NUM
        INTO l_status_grbt_num_IP
        FROM STATUS_GRBT
       WHERE STATUS_DESC = 'In Process' AND STATUS_CD = 'IP';



       INSERT /*+ Append */
            INTO  rbt_hdr (rbt_hdr_num,
                           rbt_hdr_desc,
                           strategy_type_num,
                           rbt_hdr_start_dt,
                           rbt_hdr_end_dt,
                           auto_mng_cust_acct,
                           cust_selection_num,
                           whl_selection_num,
                           data_source_num,
                           post_reconciliation,
                           post_recon_dt,
                           specific_prch_ord,
                           inc_pr_sales,
                           inc_fin_adj,
                           gross_net,
                           paymnt_num,
                           bpl_selection_num,
                           prod_selection_num,
                           disc_type_num,
                           disc_amt,
                           frq_ref_num,
                           rbt_paymnt_window,
                           wip_gen_days,
                           auto_calculate,
                           status_grbt_num,
                           username,
                           lastmod,
                           prior_key,
                           VERSION,
                           date_qualifier_num,
                           prc_qlfr,
                             INDIRECT_BPL_SELECTION_NUM,
                INDIRECT_DATE_QUALIFIER_NUM,
                INDIRECT_PROD_SELECTION_NUM,
                gross_net_vir_indirect)--CR34046, added new column gross_net_vir_indirect
         SELECT rbt_hdr_sq.NEXTVAL,
                r.rbt_hdr_desc,
                r.strategy_type_num,
                r.rbt_hdr_start_dt,
                r.rbt_hdr_end_dt,
                r.auto_mng_cust_acct,
                r.cust_selection_num,
                r.whl_selection_num,
                r.data_source_num,
                r.post_reconciliation,
                r.post_recon_dt,
                r.specific_prch_ord,
                r.inc_pr_sales,
                r.inc_fin_adj,
                r.gross_net,
                l_paymnt_num_max,
                r.bpl_selection_num,
                r.prod_selection_num,
                r.disc_type_num,
                r.disc_amt,
                r.frq_ref_num,
                r.rbt_paymnt_window,
                r.wip_gen_days,
                r.auto_calculate,
                l_status_grbt_num_ssd,
                g_username,
                SYSDATE,
                r.rbt_hdr_num,
                r.VERSION,
                r.date_qualifier_num,
                r.prc_qlfr,
                R.INDIRECT_BPL_SELECTION_NUM,
                R.INDIRECT_DATE_QUALIFIER_NUM,
                R.INDIRECT_PROD_SELECTION_NUM,
                r.gross_net_vir_indirect  --CR34046, added new colmn gross_net_vir_indirect
           FROM rbt_hdr r
          WHERE rbt_hdr_num = i_hdrnum;

      COMMIT;

      --Selecting MAX rbt_hdr_num   from  RBT HDR table ------------------------------------------

      SELECT MAX (rbt_hdr_num) INTO l_rbt_hdr_num_max FROM rbt_hdr;

      ---creating  snapshot  for  RBT HDR MGR -------------------------------------------------------


      INSERT INTO grebates.rbt_mgr_setup (rbt_hdr_num,
                                          username,
                                          rbt_mgr_setup_start_dt,
                                          rbt_mgr_setup_end_dt)
         SELECT l_rbt_hdr_num_max,
                r.username,
                r.rbt_mgr_setup_start_dt,
                r.rbt_mgr_setup_end_dt
           FROM rbt_mgr_setup r
          WHERE     r.rbt_hdr_num = i_hdrnum
                AND r.rbt_mgr_setup_start_dt =
                       (SELECT MAX (rbt_mgr_setup_start_dt)
                          FROM rbt_mgr_setup
                         WHERE rbt_hdr_num = i_hdrnum);

      COMMIT;

      ----Updating the version for the orginal rbt_hdr -------------------------------------------


      SELECT r.VERSION
        INTO l_version
        FROM rbt_hdr r
       WHERE rbt_hdr_num = i_hdrnum;

      UPDATE rbt_hdr
         SET VERSION = (l_version + 1)
       WHERE rbt_hdr_num = i_hdrnum;


      ---added this  sql --update rbt_hdr x set x.version = (select (y.version+1) from rbt_hdr y where x.rbt_hdr_num = y.rbt-hdr_num)

      COMMIT;
   EXCEPTION
      WHEN OTHERS
      THEN
         raise_application_error (
            -20001,
               'While processing  Snapshot for RBT_HDR '
            || '- Error Occured: '
            || SQLERRM);
         ROLLBACK;
         RAISE;
   END p_rbthdr_snapshot;


   ----------------*****-------Procedure  Audit Trail **RBT HDR-** ends  here  -----------******------------------------
   -----------------------Procedure  Audit Trail **RBT TERMS**  BEGINS   here  -----------------------------------------

   PROCEDURE p_rbttrm_snapshot (i_hdrnum        NUMBER,
                                i_status_num    NUMBER,
                                i_wipnum        NUMBER)
   AS
      /******************************************************************************

         Name:       p_rbttrm_snapshot ---RBT TERMS --

      ******************************************************************************/

      l_rbt_wip_num             NUMBER;
      l_status_grbt_num_IP      NUMBER;
      l_status_grbt_num_SSD     NUMBER;
      l_rcon_cnt                NUMBER;
      l_version_recon           NUMBER;
      l_status_grbt_num_recon   NUMBER;
      l_pair_key_recon          NUMBER;
      i_cnt_rbthdr              NUMBER;
      i_version                 NUMBER;
      i_version_rbt_wip         NUMBER;
      i_rbt_wip_num_recon       NUMBER;
   BEGIN
      --This is for fecthing SUPERSEDED status  from   STATUS_GRBT table --------------


      SELECT STATUS_GRBT_NUM
        INTO l_status_grbt_num_SSD
        FROM STATUS_GRBT
       WHERE STATUS_DESC = 'Superseded' AND STATUS_CD = 'SSD';

      --This is for fecthing IN_PRCRESS status  from   STATUS_GRBT table ----------------------------------------------------

      SELECT STATUS_GRBT_NUM
        INTO l_status_grbt_num_IP
        FROM STATUS_GRBT
       WHERE STATUS_DESC = 'In Process' AND STATUS_CD = 'IP';



      INSERT /*+ Append */
            INTO  rbt_terms (rbt_term_num,
                             rbt_term_desc,
                             rbt_hdr_num,
                             rbt_term_start_dt,
                             rbt_term_end_dt,
                             bpl_selection_term_num,
                             prod_selection_term_num,
                             pri_qfr,
                             sec_qfr,
                             sec_qfr_start_dt,
                             sec_qfr_end_dt,
                             disc_amt,
                             bpl_deadnet_mult,
                             base_ovrr_eff_dt,
                             cap_type_num,
                             cap_amt,
                             cap_lvl_type_num,
                             offset_cntrct_num,
                             offset_start_dt,
                             offset_end_dt,
                             offset_deadnet_mult,
                             offset_ovrr_eff_dt,
                             status_grbt_num,
                             username,
                             lastmod,
                             prior_key,
                             VERSION,
                             earned_start_dt,
                             earned_end_dt,
                             prc_qlfr,
                             per_unit_cntrct_num)
         SELECT rbt_terms_sq.NEXTVAL,
                rbt_term_desc,
                rbt_hdr_num,
                rbt_term_start_dt,
                rbt_term_end_dt,
                bpl_selection_term_num,
                prod_selection_term_num,
                pri_qfr,
                sec_qfr,
                sec_qfr_start_dt,
                sec_qfr_end_dt,
                disc_amt,
                bpl_deadnet_mult,
                base_ovrr_eff_dt,
                cap_type_num,
                cap_amt,
                cap_lvl_type_num,
                offset_cntrct_num,
                offset_start_dt,
                offset_end_dt,
                offset_deadnet_mult,
                offset_ovrr_eff_dt,
                l_status_grbt_num_ssd,
                g_username,
                SYSDATE,
                rbt_term_num,
                VERSION,
                earned_start_dt,
                earned_end_dt,
                prc_qlfr,
                per_unit_cntrct_num
           FROM rbt_terms
          WHERE rbt_hdr_num = i_hdrnum AND prior_key = 0;

      COMMIT;



      --Updating the version for the orginal rbt_hdr --------------------------------------------------------------------



      SELECT r.VERSION
        INTO i_version
        FROM rbt_terms r
       WHERE r.rbt_hdr_num = i_hdrnum AND prior_key = 0 AND ROWNUM <= 1;

      UPDATE rbt_terms
         SET VERSION = (i_version + 1)
       WHERE rbt_hdr_num = i_hdrnum AND prior_key = 0;

      COMMIT;


      --***   SQL for  Updating  latest version in the  RBT WIP  table -**** ------
      -- With referencet to defect 5455 - added status_grbt_num of 14 and 5 in where clause

      UPDATE rbt_wip
         SET VERSION = (i_version + 1)
       WHERE     rbt_hdr_num = i_hdrnum
             AND rbt_wip_num IN (SELECT a.rbt_wip_num
                                   FROM (SELECT rbt_wip_num, wip_type_num
                                           FROM rbt_wip
                                          WHERE     rbt_hdr_num = i_hdrnum
                                                AND status_grbt_num IN (1) -- [SV]: 11-May-2012, Defect 5455
                                                AND rbt_wip_num NOT IN
                                                       (SELECT rbt_wip_num
                                                          FROM rbt_wip
                                                         WHERE     pair_key IN
                                                                      (SELECT a.rbt_wip_num
                                                                         FROM rbt_wip a,
                                                                              rbt_wip b
                                                                        WHERE     a.rbt_wip_num =
                                                                                     b.pair_key
                                                                              AND b.rbt_hdr_num =
                                                                                     i_hdrnum
                                                                              AND a.status_grbt_num IN
                                                                                     (13,
                                                                                      5)) -- [SV]: 11-May-2012, Defect 5455
                                                               AND (    wip_type_num =
                                                                           104
                                                                    AND wip_type_num =
                                                                           102))) a
                                  WHERE a.wip_type_num <> 104);

      COMMIT;
   EXCEPTION
      WHEN OTHERS
      THEN
         raise_application_error (
            -20001,
               'While processing  Snapshot for RBT_TERMS '
            || '- Error Occured: '
            || SQLERRM);
         ROLLBACK;
         RAISE;
   END p_rbttrm_snapshot;

   ---------------------Procedure Process  *** end *** for Audit Trail RBT TERMS ----------------------------------------

   -- Procedure added by Satish Vemuri - Recalculation for the given WIP

   PROCEDURE P_TIER_CALC (i_VIR_sales        NUMBER,
                          i_trmnum           NUMBER,
                          o_VIR_amount   OUT NUMBER,
                          o_tier_per     OUT VARCHAR2)
   AS
      i_slabtype   NUMBER;
   BEGIN
      o_VIR_amount := 0;
      p_log (
         i_proc      => 'P_TIER_CALC',
         i_message   => 'Procedure started for Calculating VIR AMOUNT based on tier type');

      SELECT cr.ref_num
        INTO g_flattype
        FROM cd_references cr
       WHERE cr.ref_typ_val = 'Flat' AND cr.ref_typ = 'TierType';

      SELECT cr.ref_num
        INTO g_standardtype
        FROM cd_references cr
       WHERE cr.ref_typ_val = 'Standard' AND cr.ref_typ = 'TierType';

      SELECT cr.ref_num
        INTO g_incrementaltype
        FROM cd_references cr
       WHERE cr.ref_typ_val = 'Incremental' AND cr.ref_typ = 'TierType';

      SELECT vt.vir_slab_type
        INTO i_slabtype
        FROM vir_tiers vt
       WHERE vt.rbt_term_num = i_trmnum AND ROWNUM = 1;


      CASE
         WHEN i_slabtype = g_flattype
         THEN
            --- o_VIR_amount := fn_flat_tier (i_VIR_sales, i_trmnum);
            p_log (i_proc      => 'P_TIER_CALC',
                   i_message   => 'Procedure when slab type -->FLAT');
            p_flat_tier (i_vir_sales,
                         i_trmnum,
                         o_VIR_amount,
                         o_tier_per);


            p_log (
               i_proc      => 'P_TIER_CALC',
               i_message   =>    'Procedure when slab type -->FLAT , VIR_AMOUNT->'
                              || o_VIR_amount);
         WHEN i_slabtype = g_standardtype
         THEN
            -- o_VIR_amount := fn_standard_tier (i_VIR_sales, i_trmnum);
            p_log (i_proc      => 'P_TIER_CALC',
                   i_message   => 'Procedure when slab type --> STANDARD');
            P_STANDARD_TIER (i_vir_sales,
                             i_trmnum,
                             o_VIR_amount,
                             o_tier_per);
            p_log (
               i_proc      => 'P_TIER_CALC',
               i_message   =>    'Procedure when slab type --> STANDARD , VIR_AMOUNT->'
                              || o_VIR_amount);
         WHEN i_slabtype = g_incrementaltype
         THEN
            ---o_VIR_amount := fn_incremental_tier (i_VIR_sales, i_trmnum);

            p_log (i_proc      => 'P_TIER_CALC',
                   i_message   => 'Procedure when slab type --> Incremental');
            p_incremental_tier (i_VIR_sales,
                                i_trmnum,
                                o_VIR_amount,
                                o_tier_per);


            p_log (
               i_proc      => 'P_TIER_CALC',
               i_message   =>    'Procedure when slab type --> Incremental, VIR_AMOUNT->'
                              || o_VIR_amount);
      END CASE;
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         o_VIR_amount := 0;
         p_log (i_proc      => 'P_TIER_CALC',
                i_message   => 'Procedure when NO_DATA_FOUND');
         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               g_wipnum,
                               g_calnum,
                               g_dsrnum,
                               g_er_status_num,
                               g_isrpt);
         RAISE;
      WHEN OTHERS
      THEN
         o_VIR_amount := 0;
         p_log (i_proc => 'P_TIER_CALC', i_message => 'EXCEPTION WHEN_OTHERS');

         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               g_wipnum,
                               g_calnum,
                               g_dsrnum,
                               g_er_status_num,
                               g_isrpt);
         ROLLBACK;
         RAISE;
   END P_TIER_CALC;

   ---************Procedure for calculate flat type vir_amount based on the vir_sales_amount********
   PROCEDURE p_flat_tier (i_VIR_sales          NUMBER,
                          i_trmnum             NUMBER,
                          o_flatvirsales   OUT NUMBER,
                          o_tier_per       OUT VARCHAR2)
   AS
      t_thresholdamt   NUMBER;
      t_rbtper         NUMBER;
      t_startopr       CHAR (2);
   BEGIN
      p_log (
         i_proc      => 'P_FLAT_TIER',
         i_message   =>    'Procedure Started for calculate VIR amount for FLAT tier '
                        || '  trmno-->'
                        || i_trmnum);

      SELECT Threshold_START_AMT, Rebate_percentage, start_operator
        INTO t_thresholdamt, t_rbtper, t_startopr
        FROM VIR_TIERS
       WHERE RBT_TERM_NUM = i_trmnum;

      CASE
         WHEN t_startopr = '>='
         THEN
            IF i_VIR_sales >= t_thresholdamt
            /* p_log (
           i_proc      => 'p_flat_tier',
           i_message   => 'Procedure for calculate flat type vir_amount based on the vir_sales_amount'||t_thresholdamt);*/
            THEN
               o_flatvirsales := ( (i_VIR_sales) * (t_rbtper / 100));
               o_tier_per := t_rbtper || '%';
               p_log (
                  i_proc      => 'p_flat_tier',
                  i_message   =>    'Start_opr is >= and'
                                 || i_VIR_sales
                                 || '>= '
                                 || t_thresholdamt
                                 || ' ,then VIR-->'
                                 || o_flatvirsales
                                 || '  trmno-->'
                                 || i_trmnum
                                 || ' tier %-->'
                                 || o_tier_per);
            ELSE
               o_flatvirsales := 0;
               o_tier_per := '0%';
               p_log (
                  i_proc      => 'p_flat_tier',
                  i_message   =>    'Start_opr is >= and '
                                 || i_VIR_sales
                                 || ' <'
                                 || t_thresholdamt
                                 || ' ,then VIR-->'
                                 || o_flatvirsales
                                 || '  trmno-->'
                                 || i_trmnum
                                 || ' tier %-->'
                                 || o_tier_per);
            END IF;
         WHEN t_startopr = '>'
         THEN
            IF i_VIR_sales > t_thresholdamt
            THEN
               o_flatvirsales := ( (i_VIR_sales) * (t_rbtper / 100));
               o_tier_per := t_rbtper || '%';
               p_log (
                  i_proc      => 'p_flat_tier',
                  i_message   =>    'Start_opr is > and '
                                 || i_VIR_sales
                                 || ' >'
                                 || t_thresholdamt
                                 || ' ,then VIR-->'
                                 || o_flatvirsales
                                 || '   trmno-->'
                                 || i_trmnum
                                 || ' tier %-->'
                                 || o_tier_per);
            ELSE
               o_flatvirsales := 0;
               o_tier_per := '0%';
               p_log (
                  i_proc      => 'p_flat_tier',
                  i_message   =>    'Start_opr is >= and '
                                 || i_VIR_sales
                                 || ' <'
                                 || t_thresholdamt
                                 || ' ,then VIR-->'
                                 || o_flatvirsales
                                 || '   trmno-->'
                                 || i_trmnum
                                 || ' tier %-->'
                                 || o_tier_per);
            END IF;
      END CASE;
   --------RETURN t_flatvirsales;
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         ---------------  RETURN 0;
         o_flatvirsales := 0;
         p_log (
            i_proc      => 'p_flat_tier',
            i_message   =>    'NO_DAT_FOUND EXCEPTION,then VIR-->'
                           || o_flatvirsales
                           || '  trmno-->'
                           || i_trmnum);
   END p_flat_tier;



   PROCEDURE p_incremental_tier (i_VIR_sales                 NUMBER,
                                 i_trmnum                    NUMBER,
                                 o_incrementalvirsales   OUT NUMBER,
                                 o_tier_per              OUT VARCHAR2)
   AS
      t_startthresholdamt     NUMBER;
      t_endthresholdamt       NUMBER;
      t_rbtper                NUMBER;
      t_startopr              CHAR (2);
      t_endopr                CHAR (2);
      t_tiernum               NUMBER;
      t_incrementalvirsales   NUMBER := 0;

      CURSOR c1
      IS
           SELECT threshold_start_amt,
                  threshold_end_amt,
                  start_operator,
                  end_operator,
                  rebate_percentage,
                  tier_num
             FROM grebates.VIR_TIERS
            WHERE RBT_TERM_NUM = i_trmnum
         ORDER BY tier_num;

      TYPE incr_tiers IS TABLE OF c1%ROWTYPE;

      rec_tires               incr_tiers := incr_tiers ();
      l_endval                NUMBER;
      l_startval              NUMBER;
   BEGIN
      o_tier_per := '';
      p_log (
         i_proc      => 'p_incremental_tier',
         i_message   =>    'Procedure Started for INCREMENTAL TIER for Term No-->'
                        || i_trmnum);

      FOR tierRecs IN c1
      LOOP
         rec_tires.EXTEND;

         --Insert data into the varray
         rec_tires (rec_tires.COUNT) := tierRecs;
      --DBMS_OUTPUT.put_line (rec_tires.COUNT);
      END LOOP;

      FOR i IN rec_tires.FIRST .. rec_tires.LAST
      LOOP
         l_endval :=
            CASE rec_tires (i).end_operator
               WHEN '<' THEN -0.01
               WHEN '<=' THEN 0
               ELSE 0
            END;
         l_startval :=
            CASE rec_tires (i).start_operator
               WHEN '>' THEN 0.01
               WHEN '>=' THEN 0
               ELSE 0
            END;

         IF    (    rec_tires (i).start_operator = '>='
                AND i_VIR_sales >= rec_tires (i).threshold_start_amt)
            OR (    rec_tires (i).start_operator = '>'
                AND i_VIR_sales > rec_tires (i).threshold_start_amt)
         THEN
            p_log (
               i_proc      => 'p_incremental_tier',
               i_message   =>    'Inside first IF for calculate vir_amount where tier_no--> '
                              || i
                              || ' for Term No--> '
                              || i_trmnum
                              || ' and i_VIR_sales--> '
                              || i_VIR_sales
                              || ' Threshold start amount %-->'
                              || rec_tires (i).threshold_start_amt);

            IF i <> rec_tires.LAST
            THEN
               p_log (
                  i_proc      => 'p_incremental_tier',
                  i_message   =>    'Inside Second IF for calculate vir_amount where tier_no--> '
                                 || i);

               IF    (    rec_tires (i).end_operator = '<'
                      AND i_VIR_sales < rec_tires (i).threshold_end_amt)
                  OR (    rec_tires (i).end_operator = '<='
                      AND i_VIR_sales <= rec_tires (i).threshold_end_amt)
               THEN
                  p_log (
                     i_proc      => 'p_incremental_tier',
                     i_message   =>    'Inside Third IF for calculate vir_amount where tier_no--> '
                                    || i
                                    || ' for Term No--> '
                                    || i_trmnum
                                    || ' and i_VIR_sales--> '
                                    || i_VIR_sales
                                    || ' Threshold start amount %-->'
                                    || rec_tires (i).threshold_end_amt);



                  IF i = 1
                  THEN
                     t_incrementalvirsales :=
                        i_VIR_sales * rec_tires (i).rebate_percentage / 100;
                     o_tier_per := rec_tires (i).rebate_percentage || '%';
                     p_log (
                        i_proc      => 'p_incremental_tier',
                        i_message   =>    'Inside Fourth IF for calculate vir_amount where tier_no--> '
                                       || i
                                       || ' for Term No--> '
                                       || i_trmnum
                                       || ' and vir_amount--> '
                                       || t_incrementalvirsales
                                       || ' Tier %-->'
                                       || o_tier_per);
                  ELSE
                     t_incrementalvirsales :=
                          t_incrementalvirsales
                        +   (  i_VIR_sales
                             - (  rec_tires (i).threshold_start_amt
                                + l_startval))
                          * rec_tires (i).rebate_percentage
                          / 100;
                     o_tier_per :=
                           o_tier_per
                        || ','
                        || rec_tires (i).rebate_percentage
                        || '%';
                     p_log (
                        i_proc      => 'p_incremental_tier',
                        i_message   =>    'Inside Fourth ELSE for calculate vir_amount where tier_no--> '
                                       || i
                                       || ' for Term No-->'
                                       || i_trmnum
                                       || ' and vir_amount-->'
                                       || t_incrementalvirsales
                                       || ' Tier %-->'
                                       || o_tier_per);
                  END IF;



                  EXIT;
               ELSE
                  IF i = 1
                  THEN
                     t_incrementalvirsales :=
                          (rec_tires (i).threshold_end_amt + l_endval)
                        * rec_tires (i).rebate_percentage
                        / 100;
                     o_tier_per := rec_tires (i).rebate_percentage || '%';
                     p_log (
                        i_proc      => 'p_incremental_tier',
                        i_message   =>    'Inside Fifth IF for calculate vir_amount where tier_no--> '
                                       || i
                                       || ' for Term No-->'
                                       || i_trmnum
                                       || ' and vir_amount-->'
                                       || t_incrementalvirsales
                                       || ' Tier %-->'
                                       || o_tier_per);
                  ELSE
                     t_incrementalvirsales :=
                          t_incrementalvirsales
                        +   (  (rec_tires (i).threshold_end_amt + l_endval)
                             - (  rec_tires (i).threshold_start_amt
                                + l_startval))
                          * rec_tires (i).rebate_percentage
                          / 100;
                     o_tier_per :=
                           o_tier_per
                        || ','
                        || rec_tires (i).rebate_percentage
                        || '%';
                     p_log (
                        i_proc      => 'p_incremental_tier',
                        i_message   =>    'Inside Fifth ELSE for calculate vir_amount where tier_no--> '
                                       || i
                                       || ' for Term No-->'
                                       || i_trmnum
                                       || ' and vir_amount-->'
                                       || t_incrementalvirsales
                                       || ' Tier %-->'
                                       || o_tier_per);
                  END IF;

                  CONTINUE;
               END IF;
            ELSE
               t_incrementalvirsales :=
                    t_incrementalvirsales
                  +   (  i_VIR_sales
                       - (rec_tires (i).threshold_start_amt + l_startval))
                    * rec_tires (i).rebate_percentage
                    / 100;
               o_tier_per :=
                  o_tier_per || ',' || rec_tires (i).rebate_percentage || '%';
               p_log (
                  i_proc      => 'p_incremental_tier',
                  i_message   =>    'Inside Third ELSE for calculate vir_amount where tier_no--> '
                                 || i
                                 || ' for Term No-->'
                                 || i_trmnum
                                 || ' and vir_amount-->'
                                 || t_incrementalvirsales
                                 || ' Tier %-->'
                                 || o_tier_per);
               EXIT;
            END IF;
         ELSE
            IF i = 1
            THEN
               t_incrementalvirsales := 0;
               o_tier_per := '0%';
               p_log (
                  i_proc      => 'p_incremental_tier',
                  i_message   =>    'Inside First ELSE for calculate vir_amount where tier_no--> '
                                 || i
                                 || ' for Term No-->'
                                 || i_trmnum
                                 || ' and vir_amount-->'
                                 || t_incrementalvirsales
                                 || ' Tier %-->'
                                 || o_tier_per);
            ELSE
               p_log (
                  i_proc      => 'p_incremental_tier',
                  i_message   =>    'Inside First ELSE for calculate vir_amount where tier_no--> '
                                 || i
                                 || ' for Term No-->'
                                 || i_trmnum
                                 || ' and vir_amount-->'
                                 || t_incrementalvirsales
                                 || ' Tier %-->'
                                 || o_tier_per);
            END IF;

            EXIT;
         END IF;
      END LOOP;

      o_incrementalvirsales := t_incrementalvirsales;
   ----- RETURN t_incrementalvirsales;
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         o_incrementalvirsales := 0;
         o_tier_per := '0%';
         p_log (
            i_proc      => 'p_incremental_tier',
            i_message   =>    'NO_DATA_FOUND incremental type vir_amount for Term No-->'
                           || i_trmnum
                           || 'and vir_amount-->'
                           || o_incrementalvirsales
                           || 'Tier %'
                           || o_tier_per);
   END p_incremental_tier;

   PROCEDURE P_STANDARD_TIER (i_VIR_sales          IN     NUMBER,
                              i_termnum            IN     NUMBER,
                              O_Final_VIR_Amount      OUT NUMBER,
                              o_tier_per              OUT VARCHAR2)
   AS
      total_tier           NUMBER;
      Final_VIR_amount     NUMBER;

      CURSOR c_tier_cursor
      IS
           SELECT THRESHOLD_START_AMT,
                  Rebate_percentage,
                  TIER_NUM,
                  RBT_TERM_NUM,
                  start_operator
             FROM vir_tiers
            WHERE RBT_TERM_NUM = i_termnum
         ORDER BY TIER_NUM;

      TYPE nestedtable_tier IS TABLE OF c_tier_cursor%ROWTYPE;

      --Creating new instance of nestedtable
      x_nestedtable_tier   nestedtable_tier := nestedtable_tier ();
      j                    NUMBER;
   BEGIN
      o_tier_per := '';
      --Process log enties for PROCEDURE p_grebate_calc

      p_log (
         i_proc      => 'P_STANDARD_TIER',
         i_message   =>    'Procedure Started for calculate VIR amount for STANDARD tier '
                        || '  trmno-->'
                        || i_termnum);

      FOR tierRecs IN c_tier_cursor
      LOOP
         x_nestedtable_tier.EXTEND;
         --Insert data into the nestedtable
         x_nestedtable_tier (x_nestedtable_tier.COUNT) := tierRecs;
      END LOOP;

      --Loop through the nestedtable
      FOR i IN x_nestedtable_tier.FIRST .. x_nestedtable_tier.LAST
      LOOP
         IF    (    x_nestedtable_tier (i).start_operator = '>='
                AND i_VIR_sales >= x_nestedtable_tier (i).THRESHOLD_START_AMT)
            OR (    x_nestedtable_tier (i).start_operator = '>'
                AND i_VIR_sales > x_nestedtable_tier (i).THRESHOLD_START_AMT)
         THEN
            p_log (
               i_proc      => ' P_STANDARD_TIER :',
               i_message   =>    'First IF condition for amount check:i_VIR_sales '
                              || i_VIR_sales
                              || ',TIER NUM:'
                              || x_nestedtable_tier (i).TIER_NUM
                              || 'Threshold amount:'
                              || x_nestedtable_tier (i).THRESHOLD_START_AMT);


            IF i = x_nestedtable_tier.LAST
            THEN
               Final_VIR_amount :=
                    (i_VIR_sales)
                  * ( (x_nestedtable_tier (i).Rebate_percentage) / 100);
               o_tier_per := x_nestedtable_tier (i).Rebate_percentage || '%';

               p_log (
                  i_proc      => ' P_STANDARD_TIER :',
                  i_message   =>    'SECOND IF condition:i_VIR_sales '
                                 || i_VIR_sales
                                 || ',TIER NUM:'
                                 || x_nestedtable_tier (i).TIER_NUM
                                 || ',Tier % :'
                                 || x_nestedtable_tier (i).Rebate_percentage
                                 || ', VIR amount:'
                                 || Final_VIR_amount);
            ELSE
               CONTINUE;
            END IF;
         ELSE
            IF i = 1
            THEN
               Final_VIR_amount := 0;
               o_tier_per := '0%';
               p_log (
                  i_proc      => ' P_STANDARD_TIER :',
                  i_message   =>    'Third IF condition :i_VIR_sales '
                                 || i_VIR_sales
                                 || ',TIER NUM:'
                                 || x_nestedtable_tier (i).TIER_NUM
                                 || ',Tier % :'
                                 || x_nestedtable_tier (i).Rebate_percentage
                                 || ', VIR amount:'
                                 || Final_VIR_amount);
            ELSE
               j := i;
               Final_VIR_amount :=
                    (i_VIR_sales)
                  * ( (x_nestedtable_tier (j - 1).Rebate_percentage) / 100);

               o_tier_per :=
                  x_nestedtable_tier (j - 1).Rebate_percentage || '%';
               p_log (
                  i_proc      => ' P_STANDARD_TIER :',
                  i_message   =>    'Third ELSE condition:i_VIR_sales '
                                 || i_VIR_sales
                                 || ',TIER NUM:'
                                 || x_nestedtable_tier (j - 1).TIER_NUM
                                 || ',Tier % :'
                                 || x_nestedtable_tier (j - 1).Rebate_percentage
                                 || ', VIR amount:'
                                 || Final_VIR_amount);
            END IF;


            EXIT;
         END IF;
      END LOOP;


      O_Final_VIR_Amount := Final_VIR_amount;
   EXCEPTION
      WHEN OTHERS
      THEN

         p_log (i_proc      => ' P_STANDARD_TIER :',
                i_message   => 'Exception  :' || SQLCODE || SQLERRM);
   END P_STANDARD_TIER;

   -- Function defined to identify the wip detail lines for error_status on line, if available then update RBT_WIP Status as error.

   FUNCTION fn_wipdtlerr (fn_wipnum NUMBER, fn_errnum NUMBER)
      RETURN NUMBER
   AS
      l_flag   NUMBER := 0;
   BEGIN
      SELECT 1
        INTO l_flag
        FROM rbt_wip_dtl rwd
       WHERE     rwd.rbt_wip_num = fn_wipnum
             AND rwd.line_status_num = fn_errnum
             AND ROWNUM <= 1;

      -- Setting Error status to WIP based on WIP DETAIL LINE Error
      IF l_flag = 1
      THEN
         RETURN 1;
      END IF;
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         RETURN 0;
   END fn_wipdtlerr;

   -- Function to chhek WIP Lines generated
   FUNCTION fn_wiplog (fn_wipnum NUMBER, fn_stage NUMBER)
      RETURN NUMBER
   AS
      l_flag   NUMBER := 0;
   BEGIN
      SELECT 1
        INTO l_flag
        FROM logtable lg
       WHERE     lg.rbt_wip_num = fn_wipnum
             AND lg.stage_num = fn_stage
             AND ROWNUM <= 1;


      IF l_flag = 1
      THEN
         RETURN 0;
      END IF;
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         RETURN 1;
   END fn_wiplog;

   FUNCTION fn_total_sales (fn_stgnum NUMBER, fn_wipnum NUMBER)
      RETURN NUMBER
   AS
      l_totsls   NUMBER (22, 7);
   BEGIN
      SELECT SUM (SALES_IN_DOLLARS)
        INTO l_totsls
        FROM LOGTABLE_TEMP
       WHERE STAGE_NUM = fn_stgnum AND RBT_WIP_NUM = fn_wipnum;

      RETURN l_totsls;
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               fn_wipnum,
                               g_calnum,
                               G_dsrnum,
                               g_er_status_num,
                               g_isrpt);
         RETURN 0;
      WHEN OTHERS
      THEN
         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               fn_wipnum,
                               g_calnum,
                               G_dsrnum,
                               g_er_status_num,
                               g_isrpt);
         RETURN 0;
   END fn_total_sales;

   -- Function used to get the linenum as incremental value  to set for LINE NUM
   -- in LOGTABLE and as well in RBT_WIP_DTL Table.
   FUNCTION fn_linenum (fn_lnnum NUMBER DEFAULT 0)
      RETURN NUMBER
   AS
   BEGIN
      g_lnnum := g_lnnum + 1;
      RETURN (g_lnnum);
   END fn_linenum;



   PROCEDURE p_errorlog_insertion (i_errnum    NUMBER,
                                   i_errmsg    VARCHAR2,
                                   i_wipnum    NUMBER,
                                   i_calnum    NUMBER,
                                   i_dsrnum    NUMBER,
                                   i_status    NUMBER,
                                   i_isrpt     VARCHAR2 DEFAULT 'N')
   AS
      PRAGMA AUTONOMOUS_TRANSACTION;
      l_srctyp   VARCHAR2 (8);
   --g_hdrnum   NUMBER;
   BEGIN
      --Process log entry for Error Log insertion

      p_log (
         i_proc      => ' p_errorlog_insertion',
         i_message   => 'Error logging. Message:' || SUBSTR (i_errmsg, 1, 256));

      -- Checking for the datasource and accordingly will insert the type of source in Error Log
      IF i_dsrnum = g_both
      THEN
         l_srctyp := 'BOTH';
      ELSE
         IF i_dsrnum = g_direct
         THEN
            l_srctyp := 'DIRECT';
         ELSE
            IF i_dsrnum = g_indirect
            THEN
               l_srctyp := 'INDIRECT';
            ELSE
               l_srctyp := 'INVALID';
            END IF;
         END IF;
      END IF;

      -- Get HDRNUM corresponding to the processed wip and insert into Error Log as Reference.
      --        SELECT rh.rbt_hdr_num INTO g_hdrnum
      --          FROM rbt_hdr rh, rbt_wip rw
      --         WHERE rw.rbt_wip_num=i_wipnum
      --           AND rh.rbt_hdr_num=rw.rbt_hdr_num;

      -- Rollback the status from In Calcualtion to In Review when Exception Throws
      IF i_isrpt = 'N'
      THEN
         p_VIR_updtwip (g_wipnum,
                        NULL,
                        NULL,
                        NULL,
                        i_status);

         IF g_calnum IN (1, 4)
         THEN
            DELETE FROM logtable
                  WHERE rbt_wip_num = i_wipnum;

            DELETE FROM rbt_wip_dtl
                  WHERE rbt_wip_num = i_wipnum;
         END IF;

         COMMIT;
      END IF;

      INSERT INTO error_log (err_log_num,
                             err_org,
                             err_org_typ,
                             err_org_ref_num,
                             err_severity,
                             err_desc,
                             username,
                             lastmod)
           VALUES (
                     error_log_sq.NEXTVAL,
                     i_errnum,
                     l_srctyp,
                     g_hdrnum,
                     'High',
                        'While processing WIP '
                     || i_wipnum
                     || ' - Error Message: '
                     || SUBSTR (i_errmsg, 1, 256),
                     g_username,
                     SYSDATE);

             p_log (i_proc      => ' p_vir_calc',
             i_message   => '****************** END i_wipnum:' || i_wipnum);

      COMMIT;
   EXCEPTION
      WHEN OTHERS
      THEN
         ROLLBACK;
         RAISE;
   END p_errorlog_insertion;

   -- Function used to prevent user from execution for the same WIP.
   FUNCTION fn_wipexist (fn_wipnum NUMBER)
      RETURN NUMBER
   AS
      l_flag   NUMBER := 0;
   BEGIN
      SELECT 1
        INTO l_flag
        FROM rbt_wip rw, rbt_hdr rh
       WHERE     rw.rbt_wip_num = fn_wipnum
             AND rh.rbt_hdr_num = rw.rbt_hdr_num
             AND EXISTS
                    (SELECT NULL
                       FROM logtable lt, rbt_wip_dtl rwd
                      WHERE     lt.rbt_wip_num = rwd.rbt_wip_num
                            AND lt.rbt_hdr_num = rwd.rbt_hdr_num
                            AND lt.rbt_wip_num = rw.rbt_wip_num
                            AND rwd.rbt_wip_num = rw.rbt_wip_num
                            AND lt.rbt_hdr_num = rh.rbt_hdR_num
                            AND rwd.rbt_hdr_num = rh.rbt_hdr_num
                            AND lt.stage_num IN (1, 2, 3, 4))
             AND ROWNUM <= 1;


      IF l_flag = 1
      THEN
         RETURN 1;
      END IF;
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         RETURN 0;
   END fn_wipexist;


   PROCEDURE p_wiprecalcn (i_wipnum       NUMBER,
                           i_cr_lccode    VARCHAR2,
                           i_dr_lccode    VARCHAR2,
                           i_dsrnum       NUMBER,
                           i_termnum      NUMBER)
   AS
      l_vir_amt        NUMBER (22, 7) := 0.0;
      l_tier_per       VARCHAR2 (200);
      l_virsls         NUMBER (22, 7) := 0.0;
      l_newvirsls      NUMBER (22, 7) := 0.0;
      l_paidrbts       NUMBER (22, 7) := 0.0;
      l_orgsls         NUMBER (22, 7) := 0.0;
      l_num_of_terms   NUMBER;
      l_rbtqty         NUMBER (22, 7) := 0.0;
      l_rbtsls         NUMBER (22, 7) := 0.0;
      l_rbtamt         NUMBER (22, 7) := 0.0;
   BEGIN
      -- Process log enties for Procedure for Recalculation for the given WIP.

      p_log (
         i_proc      => ' p_wiprecalcn',
         i_message   => 'Procedure for Recalculation for the given WIP initiated');

      INSERT /*+ Append */
            INTO  LOGTABLE (logwipln_num,
                            rbt_hdr_num,
                            rbt_term_num,
                            earned_end_dt,
                            earned_start_dt,
                            rbt_wip_num,
                            base_cntrct_num,
                            inc_pr_sales,
                            prod_id_pri,
                            line_class_cd,
                            offset_st_dt,
                            earned_yr,
                            earned_month,
                            per_unit_rbt,
                            di_num,
                            di_line_num,
                            qty,
                            base_cntrct_price,
                            strategy_type_num,
                            bus_seg,
                            per_start_dt,
                            offset_cntrct_num,
                            offset_end_dt,
                            offset_cntrct_price,
                            sales_in_dollars,
                            data_source_type_num,
                            bunit_num,
                            bunit_name,
                            bunit_identtyp,
                            bunit_ident,
                            uom_name,
                            prod_desc,
                            rbt_amt,
                            line_num,
                            line_status_num,
                            wip_gen_dt,
                            paymnt_num,
                            stage_num,
                            base_prclst,
                            username,
                            lastmod,
                            offset_prclst,
                            transaction_type,
                            order_type,
                            order_num,
                            order_line_num,
                            submdat_num,
                            submitem_num                  -- Bala, 04 Oct 2014
                                                          -- SoW GP COT Change
                            ,
                            bunittyp_id)
         SELECT logtable_sq.NEXTVAL,
                rbt_hdr_num,
                rbt_term_num,
                earned_end_dt,
                earned_start_dt,
                rbt_wip_num,
                base_cntrct_num,
                inc_pr_sales,
                prod_id_pri,
                line_class_cd,
                offset_st_dt,
                earned_yr,
                earned_month,
                ROUND (NVL (per_unit_rbt, 0.0), 5),
                di_num,
                di_line_num,
                NVL (qty, 0.0),
                base_cntrct_price,
                strategy_type_num,
                bus_seg,
                per_start_dt,
                offset_cntrct_num,
                offset_end_dt,
                offset_cntrct_price,
                ROUND (NVL ( (rwd.qty * rwd.base_cntrct_price), 0.0), 5),
                data_source_type_num,
                bunit_num,
                bunit_name,
                bunit_identtyp,
                bunit_ident,
                uom_name,
                prod_desc,
                RBT_AMT,
                line_num,
                CASE WHEN qty = 0 THEN g_ca_status_num ELSE g_status_num END
                   ln_status,
                wip_gen_dt,
                paymnt_num,
                4,
                base_prclst,
                g_username,
                rwd.lastmod,
                offset_prclst,
                transaction_type,
                order_type,
                order_num,
                order_line_num,
                submdat_num,
                submitem_num                              -- Bala, 04 Oct 2014
                                                          -- SoW GP COT Change
                ,
                bunittyp_id
           FROM rbt_wip_dtl rwd, status_grbt sg
          WHERE     rwd.rbt_wip_num = i_wipnum
                AND rwd.line_status_num = sg.status_grbt_num
                AND sg.status_abbr IN ('CA', 'RC');


      --      SELECT SUM (sales_in_dollars)
      --        INTO l_virsls
      --        FROM rbt_wip_dtl
      --       WHERE     rbt_wip_num = i_wipnum
      --             AND line_status_num IN (g_status_num, g_reca_status_num);

      --      UPDATE grebates.rbt_wip_dtl rwd
      --         SET rwd.sales_in_dollars =
      --                ROUND (NVL ( (rwd.qty * rwd.base_cntrct_price), 0.0), 3)
      --       WHERE     rwd.rbt_wip_num = i_wipnum
      --             AND rwd.line_status_num IN (g_reca_status_num, g_ca_status_num);

      --      SELECT SUM (sales_in_dollars)
      --        INTO l_newvirsls
      --        FROM rbt_wip_dtl
      --       WHERE     rbt_wip_num = i_wipnum
      --             AND line_status_num IN (g_status_num, g_reca_status_num);

      SELECT SUM (ROUND (NVL ( (qty * base_cntrct_price), 0.0), 3))
        INTO l_orgsls
        FROM rbt_wip_dtl
       WHERE     rbt_wip_num = i_wipnum
             AND line_status_num IN (g_status_num, g_reca_status_num);

      SELECT RW.TOTAL_PAID_REBATES
        INTO l_paidrbts
        FROM rbt_wip rw
       WHERE RW.RBT_WIP_NUM = i_wipnum;

      l_newvirsls := l_orgsls - l_paidrbts;


      -- Call tier calculation procedure
      P_TIER_CALC (l_newvirsls,
                   i_termnum,
                   l_vir_amt,
                   l_tier_per);

      UPDATE rbt_wip_dtl
         SET rbt_amt =
                CASE
                   WHEN line_status_num = g_ca_status_num
                   THEN
                      0.0
                   ELSE
                        round(((  ROUND (NVL ( (qty * base_cntrct_price), 0.0), 8)
                         / l_orgsls)
                      * l_vir_amt), 2)
                END,
             sales_in_dollars =
                  (  ROUND (NVL ( (qty * base_cntrct_price), 0.0), 3)
                   / l_orgsls)
                * l_newvirsls,
             line_class_cd =
                CASE
                   WHEN   (  ROUND (NVL ( (qty * base_cntrct_price), 0.0), 3)
                           / l_orgsls)
                        * l_vir_amt >= 0
                   THEN
                      i_cr_lccode
                   ELSE
                      i_dr_lccode
                END,
             line_status_num =
                CASE
                   WHEN line_status_num = g_ca_status_num
                   THEN
                      g_ca_status_num
                   ELSE
                      g_status_num
                END,
             username = g_username,
             lastmod = SYSDATE
       WHERE     rbt_wip_num = i_wipnum
             AND line_status_num IN
                    (g_reca_status_num, g_ca_status_num, g_status_num);


      SELECT COUNT (DISTINCT rbt_term_num)
        INTO l_num_of_terms
        FROM rbt_wip_dtl
       WHERE rbt_wip_num = i_wipnum AND line_status_num = g_status_num;

      IF l_num_of_terms = 0
      THEN
         l_rbtqty := 0;
         l_rbtsls := 0;
      ELSE
         SELECT SUM (qty),
                SUM (sales_in_dollars),
                SUM (ROUND (NVL (rbt_amt, 0.0), 2))
           INTO l_rbtqty, l_rbtsls, l_rbtamt
           FROM rbt_wip_dtl
          WHERE rbt_wip_num = i_wipnum AND line_status_num = g_status_num;
      END IF;

      l_rbtqty := ROUND (l_rbtqty, 3);
      l_rbtsls := ROUND (l_rbtsls, 5);



      -- Update status of RBT_WIP after initial calculation based on WIP Detail lines if error exist then set WIP header status as 'Error'  else 'In-Review''

      IF fn_wipdtlerr (i_wipnum, g_er_status_num) = 1
      THEN
         p_vir_updtwip (i_wipnum,
                        l_rbtamt,
                        l_rbtqty,
                        l_rbtsls,
                        g_er_status_num,
                        l_orgsls,
                        l_paidrbts,
                        l_tier_per);
      ELSE
         p_vir_updtwip (i_wipnum,
                        l_rbtamt,
                        l_rbtqty,
                        l_rbtsls,
                        g_status_num,
                        l_orgsls,
                        l_paidrbts,
                        l_tier_per);
      END IF;

      p_vir_sumrpt (i_wipnum,
                    g_hdrnum,
                    i_termnum,
                    i_dsrnum,
                    l_orgsls,
                    l_rbtsls,
                    l_paidrbts,
                    l_tier_per);


      COMMIT;
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               i_wipnum,
                               g_calnum,
                               i_dsrnum,
                               g_er_status_num);
         RAISE;
      WHEN OTHERS
      THEN
         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               i_wipnum,
                               g_calnum,
                               i_dsrnum,
                               g_er_status_num);
         ROLLBACK;
         RAISE;
   END p_wiprecalcn;

   ----------------*****-------Procedure p_wiprvrsl starts  here  -----------******------------------------

   PROCEDURE P_WIPRVRSL (i_wipnum       NUMBER,
                         i_status       NUMBER,
                         i_wiptyp       NUMBER,
                         i_pairky       NUMBER,
                         i_dsrnum       NUMBER,
                         i_cr_lccode    VARCHAR2,
                         i_dr_lccode    VARCHAR2,
                         i_name         VARCHAR2)
   AS
      l_wipnum        NUMBER;
      l_status        NUMBER;
      l_rbtamt        NUMBER (22, 7) := 0.0;
      l_rbtqty        NUMBER (22, 7) := 0.0;
      l_rbtsls        NUMBER (22, 7) := 0.0;
      l_refval        VARCHAR2 (50);
      l_wiptyp        NUMBER;
      l_revrefnum     NUMBER;
      l_revrcrefnum   NUMBER;
   BEGIN
      -- Process log enties for PROCEDURE WIP Reversal/Reversal-Check

      p_log (
         i_proc      => ' p_wiprvrsl',
         i_message   => 'Start Procedure p_wiprvrsl for wip no-->' || i_wipnum);

      -- Creation of WIP Header for Reversal/Reversal-Check based on Payment Type for a given WIP.
      INSERT INTO RBT_WIP (rbt_wip_num,
                           rbt_hdr_num,
                           rbt_wip_prd_start_dt,
                           rbt_wip_prd_end_dt,
                           wip_gen_dt,
                           actual_gen_dt,
                           status_grbt_num,
                           rbt_amt,
                           wip_type_num,
                           recon_start_dt,
                           pair_key,
                           username,
                           lastmod,
                           rbt_paymnt_window_dt,
                           rbt_paymnt_window,
                           po_dr_number,
                           cust_ref_no,
                           cust_cont,
                           qty,
                           sales_amt,
                           total_original_sales,
                           total_paid_rebates,
                           vir_rbt_per)
         SELECT rbt_wip_sq.NEXTVAL,
                rbt_hdr_num,
                rbt_wip_prd_start_dt,
                rbt_wip_prd_end_dt,
                wip_gen_dt,
                SYSDATE,
                i_status,
                rbt_amt,
                i_wiptyp,
                recon_start_dt,
                i_pairky,
                g_username,
                SYSDATE,
                rbt_paymnt_window_dt,
                rbt_paymnt_window,
                po_dr_number,
                cust_ref_no,
                cust_cont,
                qty,
                sales_amt,
                total_original_sales,
                total_paid_rebates,
                vir_rbt_per
           FROM rbt_wip rw
          WHERE rw.rbt_wip_num = i_wipnum;

      COMMIT;

      SELECT rbt_wip_num
        INTO l_wipnum
        FROM rbt_wip rw
       WHERE rw.pair_key = i_wipnum AND ROWNUM <= 1;

      SELECT cr.ref_num
        INTO l_revrefnum
        FROM cd_references cr
       WHERE cr.ref_typ_val = 'Reversal';

      SELECT cr.ref_num
        INTO l_revrcrefnum
        FROM cd_references cr
       WHERE cr.ref_typ_val = 'Reversal-Check';



      -- WIP Detail Creation for the Reversal/Reversal-Check on Given WIP based up on Rebate Program Payment Type
      INSERT /*+ APPEND */
            INTO  RBT_WIP_DTL (rbt_wip_dtl_num,
                               rbt_hdr_num,
                               rbt_term_num,
                               rbt_wip_num,
                               base_cntrct_num,
                               inc_pr_sales,
                               prod_id_pri,
                               line_class_cd,
                               offset_st_dt,
                               earned_yr,
                               earned_month,
                               per_unit_rbt,
                               di_num,
                               di_line_num,
                               base_cntrct_price,
                               strategy_type_num,
                               bus_seg,
                               per_start_dt,
                               offset_cntrct_num,
                               offset_end_dt,
                               offset_cntrct_price,
                               sales_in_dollars,
                               data_source_type_num,
                               dir_start_dt,
                               dir_end_dt,
                               bunit_num,
                               bunit_identtyp,
                               uom_name,
                               prod_desc,
                               rbt_amt,
                               line_num,
                               line_status_num,
                               wip_gen_dt,
                               paymnt_num,
                               base_prclst,
                               username,
                               lastmod,
                               offset_prclst,
                               comments,
                               bunit_ident,
                               transaction_type,
                               order_type,
                               submdat_num,
                               submitem_num,
                               order_num,
                               order_line_num,
                               qty,
                               bunit_name,
                               earned_start_dt,
                               earned_end_dt)
         SELECT rbt_wip_dtl_sq.NEXTVAL,
                rbt_hdr_num,
                rbt_term_num,
                l_wipnum,
                base_cntrct_num,
                inc_pr_sales,
                prod_id_pri,
                line_class_cd,
                offset_st_dt,
                earned_yr,
                earned_month,
                per_unit_rbt,
                di_num,
                di_line_num,
                base_cntrct_price,
                strategy_type_num,
                bus_seg,
                per_start_dt,
                offset_cntrct_num,
                offset_end_dt,
                offset_cntrct_price,
                sales_in_dollars,
                data_source_type_num,
                dir_start_dt,
                dir_end_dt,
                bunit_num,
                bunit_identtyp,
                uom_name,
                prod_desc,
                rbt_amt,
                line_num,
                CASE
                   WHEN     i_wiptyp = l_revrefnum
                        AND line_status_num = g_cl_status_num
                   THEN
                      g_status_num
                   WHEN     i_wiptyp = l_revrefnum
                        AND line_status_num = g_ca_status_num
                   THEN
                      g_ca_status_num
                   WHEN     i_wiptyp = l_revrcrefnum
                        AND line_status_num = g_cl_status_num
                   THEN
                      g_cl_status_num
                   WHEN     i_wiptyp = l_revrcrefnum
                        AND line_status_num = g_ca_status_num
                   THEN
                      g_ca_status_num
                END
                   ln_status,
                wip_gen_dt,
                paymnt_num,
                base_prclst,
                g_username,
                SYSDATE,
                offset_prclst,
                comments,
                bunit_ident,
                transaction_type,
                order_type,
                submdat_num,
                submitem_num,
                order_num,
                order_line_num,
                qty,
                bunit_name,
                earned_start_dt,
                earned_end_dt
           FROM rbt_wip_dtl rwd
          WHERE rwd.rbt_wip_num = i_wipnum;

      COMMIT;

      -- Reversal/Reversal-Check summation for a given wip.
      UPDATE grebates.rbt_wip_dtl rwd
         SET rwd.sales_in_dollars =
                CASE
                   WHEN rwd.sales_in_dollars > 0
                   THEN
                      rwd.sales_in_dollars * -1
                   WHEN rwd.sales_in_dollars < 0
                   THEN
                      rwd.sales_in_dollars * -1
                END,
             rwd.rbt_amt =
                CASE
                   WHEN rwd.rbt_amt > 0 THEN rwd.rbt_amt * -1
                   WHEN rwd.rbt_amt < 0 THEN rwd.rbt_amt * -1
                END,
             rwd.line_class_cd =
                CASE
                   WHEN (rwd.rbt_amt * -1) > 0 THEN i_cr_lccode
                   WHEN (rwd.rbt_amt * -1) < 0 THEN i_dr_lccode
                END,
             rwd.qty =
                CASE
                   WHEN rwd.qty > 0 THEN rwd.qty * -1
                   WHEN rwd.qty < 0 THEN rwd.qty * -1
                END,
             rwd.username = g_username,
             rwd.lastmod = SYSDATE
       WHERE rwd.rbt_wip_num = l_wipnum;

      SELECT ROUND (SUM (NVL (rwd.rbt_amt, 0.0)), 5),
             ROUND (SUM (NVL (rwd.qty, 0.0)), 3),
             ROUND (SUM (NVL (rwd.sales_in_dollars, 0.0)), 5)
        INTO l_rbtamt, l_rbtqty, l_rbtsls
        FROM grebates.rbt_wip_dtl rwd
       WHERE rwd.rbt_wip_num = l_wipnum;

      SELECT cr.ref_typ_val
        INTO l_refval
        FROM cd_references cr
       WHERE cr.ref_num = i_wiptyp;

      IF l_refval = 'Reversal-Check'
      THEN
         l_status := g_cl_status_num;
      ELSE
         IF l_refval = 'Reversal'
         THEN
            l_status := g_status_num;
         END IF;
      END IF;


      p_vir_updtwip (l_wipnum,
                     l_rbtamt,
                     l_rbtqty,
                     l_rbtsls,
                     l_status);
      COMMIT;
   EXCEPTION
      WHEN TOO_MANY_ROWS
      THEN
         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               l_wipnum,
                               g_calnum,
                               i_dsrnum,
                               g_er_status_num);
         RAISE;
      WHEN NO_DATA_FOUND
      THEN
         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               l_wipnum,
                               g_calnum,
                               i_dsrnum,
                               g_er_status_num);
         RAISE;
      WHEN OTHERS
      THEN
         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               l_wipnum,
                               g_calnum,
                               i_dsrnum,
                               g_er_status_num);
         ROLLBACK;
         RAISE;
   END;

   PROCEDURE P_SEND_EMAIL (i_wipno        IN NUMBER,
--                           i_hdrno        IN NUMBER,
--                           i_email_to     IN VARCHAR2,
--                           i_email_from   IN VARCHAR2,
                           i_email_msg      IN VARCHAR2
--                           i_EMAIL_CC IN VARCHAR2
                           )
   AS

      V_EMAIL_MSG    VARCHAR2 (32767);
      V_EMAIL_SUBJ   VARCHAR2 (255);
      V_DB_NAME     VARCHAR2 (25);
      V_DB_NAME1     VARCHAR2 (25);
      V_EMAIL_TO     VARCHAR2(200);
      V_EMAIL_CC     VARCHAR2(200);
      V_EMAIL_FROM  VARCHAR2(50);
--      V_HDR_DESC     VARCHAR2 (500);
   BEGIN
      p_log (
         i_proc      => ' P_SEND_EMAIL',
         i_message   =>    'Start Procedure P_SEND_EMAIL for wip no-->'
                        || i_wipno
                        );

      V_EMAIL_FROM:='gRebates@pfizer.com';
      SELECT sys_context('USERENV','DB_NAME') INTO V_DB_NAME
      FROM dual;

      V_DB_NAME1 := CASE V_DB_NAME  WHEN 'PACEGRD3' THEN 'DEV:' WHEN 'PACEGRT1' THEN 'SYSTEST:' WHEN 'PACEGRS1' THEN 'STAGE:' WHEN 'PACEGRP1' THEN 'PROD:' ELSE '' END;

--      SELECT RH.RBT_HDR_DESC
--        INTO V_HDR_DESC
--        FROM rbt_hdr rh
--       WHERE rbt_hdr_num = i_hdrno;
--
--      V_HDR_DESC := REPLACE (V_HDR_DESC, ' ', '+');
     IF i_email_msg ='SUCCESS'THEN
     V_EMAIL_SUBJ := V_DB_NAME1 ||' VIR Summary Report - WK#'|| i_wipno||', Generated Successfully ' ;
      V_EMAIL_MSG :=
            '<html>Rebate Manager,</br></br>VIR Summary Report has been successfully generated for the WIP Key '|| i_wipno || '. Please login into gRebates application to access the report on the WIP header screen.</br></br>Thanks</html>' ;


     ELSE
     V_EMAIL_SUBJ := V_DB_NAME1 ||' VIR Summary Report - WK#'|| i_wipno||', ERROR' ;
     V_EMAIL_MSG :=
            '<html>Rebate Manager,</br></br>VIR Summary Report for the WIP Key '|| i_wipno || ' has not been generated due to an error. Please log a ticket with gRebates application support team to analyze the issue further.</br></br>Thanks</html>' ;
     END IF;

     IF V_DB_NAME IN('PACEGRP1','PACEGRS1') THEN  -- PROD and STAGE DB

        SELECT PARAMETER_VALUE INTO V_EMAIL_TO  FROM GREBATES_CONFIG WHERE PARAMETER_NAME ='PROD_TO_EMAIL_ID' AND CATEGORY_TYPE='SUMMARY_RPT_EMAIL';

        IF i_email_msg ='SUCCESS'THEN
        V_EMAIL_CC:='';-- Keep this blank
        ELSE
        SELECT PARAMETER_VALUE INTO V_EMAIL_CC  FROM GREBATES_CONFIG WHERE PARAMETER_NAME ='PROD_CC_EMAIL_ID'  AND CATEGORY_TYPE='SUMMARY_RPT_EMAIL';
        END IF;


     ELSE -- Other environment
        SELECT PARAMETER_VALUE INTO V_EMAIL_TO  FROM GREBATES_CONFIG WHERE PARAMETER_NAME ='TEST_TO_EMAIL_ID'  AND CATEGORY_TYPE='SUMMARY_RPT_EMAIL';
        V_EMAIL_CC:='';-- Keep this blank
     END IF;



      UTL_MAIL.send (sender       => V_EMAIL_FROM,
                     recipients   => V_EMAIL_TO,
                     CC           => V_EMAIL_CC,
                     subject      => V_EMAIL_SUBJ,
                     MESSAGE      => V_EMAIL_MSG,
                     mime_type    => 'text/html; charset=us-ascii');
   EXCEPTION
      WHEN OTHERS
      THEN
         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               i_wipno,
                               g_calnum,
                               g_dsrnum,
                               g_er_status_num,
                               g_isrpt);
   END P_SEND_EMAIL;

   PROCEDURE P_VIR_INDIR_CALC (i_calnum   IN NUMBER,
                               i_dsrnum   IN NUMBER,
                               i_wipnum   IN NUMBER,
                               i_name     IN VARCHAR2,
                               i_status   IN NUMBER,
                               i_isRPT    IN VARCHAR2)
   AS
      l_totalsls       NUMBER (22, 7) := 0.0;
      l_paid_rbtamt    NUMBER (22, 7) := 0.0;
      l_vir_sales      NUMBER (22, 7) := 0.0;
      l_hdrnum         NUMBER := 0;
      l_trmnum         NUMBER;
      l_prdsdt         DATE;                     --Value for period start date
      l_prdedt         DATE;                  -- Value for wip period end date
      l_wipgdt         DATE;                  -- Value for WIP Generation Date
      l_vir_amt        NUMBER (22, 7) := 0.0;
      l_tier_per       VARCHAR2 (200);
      l_rbtqty         NUMBER (22, 7) := 0.0;
      l_rbtsls         NUMBER (22, 7) := 0.0;
      l_rbtamt         NUMBER (22, 7) := 0.0;
      l_wipstatus      NUMBER;
      l_num_of_terms   NUMBER;
      l_flag           NUMBER;
      l_reftyp         VARCHAR2 (50); -- Refence type value for Check/Credit Memo
      l_paynum         NUMBER;
      l_pairke         NUMBER;      -- Pair key is generated based on WIP KEY.
      l_name           VARCHAR2 (256);
      l_wiptyp         NUMBER;
   BEGIN
      g_wipnum := i_wipnum;
      g_indsrnum := i_dsrnum;

      p_log (
         i_proc      => 'P_VIR_INDIR_CALC',
         i_message   =>    'Conditional Block to process Rebate Calculation (Initial/Re-Calculation/Reversal) p_calnum: '
                        || i_calnum
                        || ' p_dsrnum:'
                        || i_dsrnum);





      SELECT ref_num
        INTO g_indirship
        FROM cd_references cr
       WHERE cr.ref_typ = 'VirInDirDtQualifier' AND cr.ref_typ_val = 'Ship';

      SELECT ref_num
        INTO g_indirpaid
        FROM cd_references cr
       WHERE cr.ref_typ = 'VirInDirDtQualifier' AND cr.ref_typ_val = 'Paid';

      BEGIN
         SELECT param_number_value
           INTO ln_limit
           FROM IND_PARAMS
          WHERE param_name = 'LIMIT';
      EXCEPTION
         WHEN OTHERS
         THEN
            ln_limit := 50000;
      END;

      SELECT rw.rbt_wip_prd_start_dt,
             rw.rbt_wip_prd_end_dt,
             rw.wip_gen_dt,
             rh.rbt_hdr_num,
             rt.rbt_term_num
        INTO l_prdsdt,
             l_prdedt,
             l_wipgdt,
             l_hdrnum,
             l_trmnum
        FROM rbt_wip rw,
             rbt_hdr rh,
             rbt_terms rt,
             status_grbt sg
       WHERE     rw.rbt_wip_num = i_wipnum
             AND rh.rbt_hdr_num = rw.rbt_hdr_num
             AND rt.rbt_hdr_num = rh.rbt_hdr_num
             AND sg.status_grbt_num = rt.status_grbt_num
             AND sg.status_abbr = 'AC'
             AND ROWNUM = 1;

      g_termnum := l_trmnum;

      -- Conditional Block to process Rebate Calculation (Initial/Re-Calculation/Import/Aprrovei
      CASE
         WHEN    ( (i_calnum = 4 OR i_calnum = 1) AND i_isRPT = 'N')
              OR (i_calnum = 1 AND i_isRPT = 'Y')
         THEN
            EXECUTE IMMEDIATE 'TRUNCATE TABLE LOGTABLE_MRR';



            EXECUTE IMMEDIATE 'TRUNCATE TABLE LOGTABLE_TEMP';


            EXECUTE IMMEDIATE 'TRUNCATE TABLE IND_RBTCUSWHL1';

            EXECUTE IMMEDIATE 'TRUNCATE TABLE IND_PRODUCT';

            EXECUTE IMMEDIATE 'TRUNCATE TABLE IND_INDIRSALES_IN';


            p_vir_indir_rbtcuswhl;

            p_vir_indir_product;

            p_vir_indirsales_in;

            g_lnnum := 0;
            p_vir_indirect;



            l_totalsls := fn_total_sales_indirect (1, i_wipnum);

            p_elgbl_paid_rbt_lines (l_hdrnum,
                                    i_wipnum,
                                    l_trmnum,
                                    l_prdsdt,
                                    l_prdedt,
                                    l_paid_rbtamt);

            l_vir_sales := l_totalsls - NVL (l_paid_rbtamt, 0);

            -- Call tier calculation procedure
            P_TIER_CALC (l_vir_sales,
                         l_trmnum,
                         l_vir_amt,
                         l_tier_per);


            p_log (
               i_proc      => 'P_VIR_INDIR_CALC',
               i_message   =>    'total sales: '
                              || l_totalsls
                              || ' total paid rbt amount: '
                              || l_paid_rbtamt
                              || ' total vir sales: '
                              || l_vir_sales
                              || ' total vir amount :'
                              || l_vir_amt
                              || ' Tier %: '
                              || l_tier_per);

            p_insert_logtable_stage (i_wipnum,
                                     i_dsrnum,
                                     l_totalsls,
                                     l_vir_sales,
                                     l_vir_amt);

            p_insert_to_final_logtable;

            IF i_isRPT = 'N'
            THEN
               g_lnnum := 0;
               p_ins_rbt_wip_dtl (2, i_wipnum, i_dsrnum);

               SELECT COUNT (DISTINCT rbt_term_num)
                 INTO l_num_of_terms
                 FROM rbt_wip_dtl
                WHERE     rbt_wip_num = i_wipnum
                      AND line_status_num = g_status_num;

               IF l_num_of_terms = 0
               THEN
                  l_rbtqty := 0;
                  l_rbtsls := 0;
               ELSE
                  SELECT SUM (qty),
                         SUM (sales_in_dollars),
                         SUM (NVL (rbt_amt, 0.0))
                    INTO l_rbtqty, l_rbtsls, l_rbtamt
                    FROM rbt_wip_dtl
                   WHERE     rbt_wip_num = i_wipnum
                         AND line_status_num = g_status_num;
               END IF;

               l_rbtqty := ROUND (l_rbtqty, 3);
               l_rbtsls := ROUND (l_rbtsls, 5);



               -- Update status of RBT_WIP after initial calculation based on WIP Detail lines if error exist then set WIP header status as 'Error'  else 'In-Review''

               IF    fn_wipdtlerr (i_wipnum, g_er_status_num) = 1
                  OR l_wipstatus = 1
               THEN
                  p_vir_updtwip (i_wipnum,
                                 l_rbtamt,
                                 l_rbtqty,
                                 l_rbtsls,
                                 g_er_status_num,
                                 l_totalsls,
                                 l_paid_rbtamt,
                                 l_tier_per);
               ELSE
                  p_vir_updtwip (i_wipnum,
                                 l_rbtamt,
                                 l_rbtqty,
                                 l_rbtsls,
                                 g_status_num,
                                 l_totalsls,
                                 l_paid_rbtamt,
                                 l_tier_per);
               END IF;

               BEGIN
                  SELECT 1
                    INTO l_flag
                    FROM rbt_wip rw, status_grbt sg
                   WHERE     rw.rbt_wip_num = i_wipnum
                         AND (       sg.status_grbt_num = rw.status_grbt_num
                                 AND sg.status_abbr = 'IR'
                              OR     sg.status_grbt_num = rw.status_grbt_num
                                 AND sg.status_abbr = 'ERR'); -- [SV]: 11-MAY-2012, Defect 5455.
               EXCEPTION
                  WHEN NO_DATA_FOUND
                  THEN
                     l_flag := 0;
               END;

               -- Snapshot Creation for the WIPs status set to In-Review.
               IF l_flag = 1
               THEN
                  p_rbthdr_snapshot (l_hdrnum, g_status_num, i_wipnum);


                  p_rbttrm_snapshot (l_hdrnum, g_status_num, i_wipnum);
               END IF;
            -- recoiliation not required in VIR
            -- Update status of RBT_WIP for Reconciliation date based on WIP_TYPE
            --  p_update_wip_dates_user;


            END IF;

            p_vir_sumrpt (i_wipnum,
                          l_hdrnum,
                          l_trmnum,
                          i_dsrnum,
                          l_totalsls,
                          l_vir_sales,
                          l_paid_rbtamt,
                          l_tier_per);


         WHEN i_calnum = 2
         THEN
            g_lnnum := 0;
            p_log (i_proc      => ' p_vir_Indir_calc',
                   i_message   => 'Re-Calculation for Selective WIP');

            SELECT rt.rbt_term_num
              INTO l_trmnum
              FROM rbt_wip rw,
                   rbt_hdr rh,
                   rbt_terms rt,
                   status_grbt sg
             WHERE     rw.rbt_wip_num = i_wipnum
                   AND rh.rbt_hdr_num = rw.rbt_hdr_num
                   AND rt.rbt_hdr_num = rh.rbt_hdr_num
                   AND sg.status_grbt_num = rt.status_grbt_num
                   AND sg.status_abbr = 'AC'
                   AND ROWNUM = 1;

            -- If QTY is 0 then wip detail line will drop if the Data Source is External else set status as Cancel.

            UPDATE rbt_wip_dtl rwd
               SET rwd.line_status_num = g_ca_status_num
             WHERE rwd.rbt_wip_num = i_wipnum AND rwd.qty = 0;

            COMMIT;

            -- Procedure added for a Re-Calculation across 3 Data Sources - 16-04-2012
            p_wiprecalcn (i_wipnum,
                          l_cr_lccode,
                          l_dr_lccode,
                          i_dsrnum,
                          l_trmnum);


            COMMIT;
         WHEN i_calnum = 5
         THEN
            p_log (i_proc      => ' p_vir_indir_calc',
                   i_message   => 'Wip Reversal for Selective WIP');

            -- Get the type of payment on Rebate program for a given WIP.
            SELECT rh.paymnt_num, cr.ref_typ_val
              INTO l_paynum, l_reftyp
              FROM rbt_hdr rh,
                   rbt_wip rw,
                   paymnt_info pi,
                   cd_references cr
             WHERE     rw.rbt_wip_num = i_wipnum
                   AND rh.rbt_hdr_num = rw.rbt_hdr_num
                   AND pi.PAYMNT_NUM = rh.paymnt_num
                   AND cr.ref_num = pi.paymnt_type_num
                   AND cr.ref_typ_val IN ('Check', 'Credit Memo');

            -- Pass the current wip key to set as Pair Key in Reversal WIP Creation
            l_pairke := i_wipnum;
            l_name := g_username;

            -- Based on payment, Reversal WIP is created.
            IF l_reftyp = 'Credit Memo'
            THEN
               SELECT cr.ref_num
                 INTO l_wiptyp
                 FROM cd_references cr
                WHERE cr.REF_TYP_VAL = 'Reversal';

               p_wiprvrsl (i_wipnum,
                           g_status_num,
                           l_wiptyp,
                           l_pairke,
                           i_dsrnum,
                           l_cr_lccode,
                           l_dr_lccode,
                           l_name);
            ELSE
               IF l_reftyp = 'Check'
               THEN
                  SELECT cr.ref_num
                    INTO l_wiptyp
                    FROM cd_references cr
                   WHERE cr.REF_TYP_VAL = 'Reversal-Check';

                  p_wiprvrsl (i_wipnum,
                              g_cl_status_num,
                              l_wiptyp,
                              l_pairke,
                              i_dsrnum,
                              l_cr_lccode,
                              l_dr_lccode,
                              l_name);
               END IF;
            END IF;

            COMMIT;
         WHEN i_calnum = 4 AND i_isRPT = 'Y'
         THEN
            --                  p_log (i_proc      => ' p_vir_indr_calc',
            --                         i_message   => 'Summary report for wip :'||i_wipnum);

            SELECT RW.SALES_AMT,
                   RW.TOTAL_ORIGINAL_SALES,
                   RW.TOTAL_PAID_REBATES,
                   RW.VIR_RBT_PER
              INTO l_vir_sales,
                   l_totalsls,
                   l_paid_rbtamt,
                   l_tier_per
              FROM RBT_WIP RW
             WHERE RW.RBT_WIP_NUM = i_wipnum;


            p_vir_sumrpt (i_wipnum,
                          l_hdrnum,
                          l_trmnum,
                          i_dsrnum,
                          l_totalsls,
                          l_vir_sales,
                          l_paid_rbtamt,
                          l_tier_per);

      END CASE;

      p_log (i_proc => 'P_VIR_INDIR_CALC', i_message => 'Procedure End');
   EXCEPTION
      WHEN OTHERS
      THEN
         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               i_wipnum,
                               i_calnum,
                               i_dsrnum,
                               g_er_status_num,
                               i_isRPT);
         ROLLBACK;
         RAISE;
   END P_VIR_INDIR_CALC;


   PROCEDURE p_vir_indir_rbtcuswhl
   AS
      CURSOR cust_cur
      IS
           SELECT a.buntnm, a.hdrnum
             FROM (SELECT cwa.cust_whl_num buntnm, cwa.rbt_hdr_num hdrnum
                     FROM cust_whl_assoc cwa
                    WHERE     cwa.rbt_hdr_num = g_hdrnum
                          AND cwa.cust_or_whl = 'WHL'
                   UNION ALL
                   SELECT cwa.child_num buntnm, cwa.rbt_hdr_num hdrnum
                     FROM cust_whl_assoc cwa
                    WHERE     cwa.rbt_hdr_num = g_hdrnum
                          AND cwa.cust_or_whl = 'WHL'--                          AND (   EXISTS
                                                     --                                     (SELECT 1
                                                     --                                        FROM grebates.gr_mbr_mv mbr,
                                                     --                                             grebates.gr_status_mv st
                                                     --                                       WHERE     mbr.ctorg_bunit_num =
                                                     --                                                    cwa.cust_whl_num
                                                     --                                             AND mbr.mbr_bunit_num =
                                                     --                                                    cwa.child_num
                                                     --                                             AND mbr.status_num = st.status_num
                                                     --                                             AND status_cd = 'mbr'
                                                     --                                             AND status_abbr IN
                                                     --                                                    ('ACT', 'CDP', 'EP') --- ?? should we remove this
                                                     --                                                                        )
                                                     --                               OR cwa.child_num = 0)
                  ) a --Added by Krati 18Nov'13 for corrected mapping for Assoc and members
            WHERE a.buntnm <> 0
         GROUP BY buntnm, hdrnum;
   BEGIN
      -- insert
      p_log (
         i_proc      => 'p_vir_indir_rbtcuswhl',
         i_message   => 'Procedure start: to insert wholesaler in temp table ');


      FOR cust_rec IN cust_cur
      LOOP
         IF cust_rec.buntnm = -3
         THEN
            INSERT /*+ APPEND */
                  INTO  IND_RBTCUSWHL1
               SELECT cust_rec.hdrnum hdrnum,
                      grtpm.bunit_num buntnm,
                      grtpm.bunit_name buntname,
                      grtpidm.buid_identifier buntid,
                      grtpidm.buidtyp_id buntdc,
                      g_wipnum
                 FROM gr_bunit_mv grtpm,
                      gr_bunitstat_mv grbstat,
                      gr_status_mv grstat,
                      (SELECT a.bunit_num, a.buidtyp_id, a.buid_identifier
                         FROM gr_buid_mv a, gr_status_mv c
                        WHERE     c.status_num = a.status_num
                              AND c.status_abbr IN ('ACT', 'CDP', 'CP') -- ?? should we remove this ??
                              AND a.buidtyp_id =
                                     (SELECT MAX (z.buidtyp_id)
                                        FROM gr_buid_mv z
                                       WHERE z.bunit_num = a.bunit_num)) grtpidm
                WHERE     grtpm.bunit_num = grtpidm.bunit_num
                      AND grtpm.bunitstat_num = grbstat.bunitstat_num
                      AND grbstat.status_num = grstat.status_num
                      AND grstat.status_abbr IN ('ACT', 'CDP', 'CP'); -- ?? should we remove this ??
         ELSE
            INSERT /*+ APPEND */
                  INTO  IND_RBTCUSWHL1
               SELECT cust_rec.hdrnum hdrnum,
                      grtpm.bunit_num buntnm,
                      grtpm.bunit_name buntname,
                      grtpidm.buid_identifier buntid,
                      grtpidm.buidtyp_id buntdc,
                      g_wipnum
                 FROM gr_bunit_mv grtpm,
                      gr_bunitstat_mv grbstat,
                      gr_status_mv grstat,
                      (SELECT a.bunit_num, a.buidtyp_id, a.buid_identifier
                         FROM gr_buid_mv a, gr_status_mv c
                        WHERE     c.status_num = a.status_num
                              AND c.status_abbr IN ('ACT', 'CDP', 'CP') -- ?? should we remove this ??
                              AND a.buidtyp_id =
                                     (SELECT MAX (z.buidtyp_id)
                                        FROM gr_buid_mv z
                                       WHERE z.bunit_num = a.bunit_num)) grtpidm
                WHERE     grtpm.bunit_num = cust_rec.buntnm
                      AND grtpm.bunit_num = grtpidm.bunit_num
                      AND grtpm.bunitstat_num = grbstat.bunitstat_num
                      AND grbstat.status_num = grstat.status_num
                      AND grstat.status_abbr IN ('ACT', 'CDP', 'CP'); -- ?? should we remove this ??
         END IF;

         COMMIT;
      END LOOP;

      p_log (
         i_proc      => 'p_vir_indir_rbtcuswhl',
         i_message   => 'Procedure end: to insert wholesaler in temp table ');
   EXCEPTION
      WHEN OTHERS
      THEN
         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               g_wipnum,
                               g_calnum,
                               g_indsrnum,
                               g_er_status_num);
   END p_vir_indir_rbtcuswhl;


   PROCEDURE p_vir_indir_product
   AS
   BEGIN
      -- insert
      p_log (
         i_proc      => 'p_vir_indir_product',
         i_message   => 'Procedure start : to insert product in temp table');


      INSERT /*+ APPEND */
            INTO  IND_PRODUCT
         SELECT rbh.rbt_hdr_num hdrnum,
                g_termnum trmnum,
                prod.prod_num prodnm,
                prod.prodid_identifier prodid,
                prod.prod_desc prodds,
                prod.uom_name uomnam,
                prod.uom_num uom_num,
                g_wipnum
           FROM --                rbt_terms rbt,
                rbt_hdr rbh,
                (SELECT a.prod_num,
                        a.prodid_identifier,
                        b.prod_desc,
                        b.uom_name,
                        b.uom_num
                   FROM (SELECT p.prod_num,
                                p.prod_Desc,
                                u.uom_name,
                                u.uom_num
                           FROM gr_uom_mv u, gr_prod_mv p
                          WHERE u.uom_num = p.uom_num_cbk) b,
                        (SELECT prd.prodid_identifier, prd.prod_num
                           FROM gr_prodid_mv prd, gr_status_mv st
                          WHERE     prd.pident_id = 'NDC-11'
                                AND prd.status_num = st.status_num
                                AND st.status_abbr IN ('ACT', 'CDP', 'CP') -- ?? should we remove this ??
                                                                          ) a
                  WHERE a.prod_num = b.prod_num) prod,
                vir_prod_excl pexln
          WHERE     (    pexln.rbt_hdr_num = rbh.rbt_hdr_num
                     --                     AND rbh.rbt_hdr_num = rbt.rbt_hdr_num
                     AND (   pexln.prod_num = -3
                          OR pexln.prod_num = prod.prod_num)
                     AND pexln.incl_flag = 'Y'
                     AND pexln.data_source_num = g_indsrnum)
                AND rbh.rbt_hdr_num = g_hdrnum;

      -- test count
      --        IF SQL%ROWCOUNT = 0
      --        THEN
      --            g_stgnum := 1;
      --            RAISE l_exception;
      --        ELSE
      --             COMMIT;
      --        END IF;
      p_log (i_proc      => 'p_vir_indir_product',
             i_message   => 'Procedure end : to insert product in temp table');
   EXCEPTION
      WHEN OTHERS
      THEN
         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               g_wipnum,
                               g_calnum,
                               g_indsrnum,
                               g_er_status_num);
   END p_vir_indir_product;


   PROCEDURE p_vir_indirect
   AS
      TYPE ARRAY IS TABLE OF LOGTABLE_MRR%ROWTYPE;

      l_data         ARRAY;
      i              INTEGER;
      l_data_check   NUMBER (22);
      l_prdsdt       DATE;
      l_prdedt       DATE;
      l_hdrnum       NUMBER;
      l_datafound    NUMBER := 0;
      l_hdr_s_dt     rbt_hdr.RBT_HDR_START_DT%TYPE;
      l_hdr_e_dt     rbt_hdr.RBT_HDR_end_DT%TYPE;
      l_cnt          NUMBER := 0;
      l_termnum      NUMBER;


      CURSOR indirsales2 (c_prdsdt      DATE,
                          c_prdedt      DATE,
                          c_hdrnum      NUMBER,
                          c_hdr_s_dt    DATE,
                          c_hdr_e_dt    DATE)
      IS
         SELECT flds.rbt_hdr_num,
                flds.rbt_term_num,
                flds.rbt_wip_num,
                flds.base_cntrct_num,
                flds.inc_pr_sales,
                flds.prod_id_pri,
                flds.line_class_cd,
                NULL OFFSET_ST_DT,
                flds.earned_yr,
                flds.earned_month,
                0.0 PER_UNIT_RBT,
                flds.di_num,
                NULL DI_LINE_NUM,
                flds.qty,
                flds.base_cntrct_price,
                flds.strategy_type_num,
                flds.bus_seg,
                flds.per_start_dt,
                NULL OFFSET_CNTRCT_NUM,
                NULL OFFSET_END_DT,
                NULL OFFSET_CNTRCT_PRICE,
                flds.sales_in_dollars,
                g_indsrnum,
                NULL DIR_START_DT,
                NULL DIR_END_DT,
                flds.bunit_num,
                flds.bunit_identtyp,
                flds.uom_name,
                flds.prod_desc,
                0.0 RBT_AMT,
                fn_linenum (g_lnnum) LINE_NUM,
                flds.line_status_num,
                flds.wip_gen_dt,
                flds.paymnt_num,
                flds.stage_num,
                flds.base_prclst,
                flds.username,
                flds.lastmod,
                NULL OFFSET_PRCLST,
                NULL COMMENTS,
                logtable_sq.NEXTVAL LOGWIPLN_NUM,
                flds.bunit_ident,
                flds.transaction_type,
                NULL ORDER_TYPE,
                flds.submdat_num,
                flds.submitem_num,
                NULL ORDER_NUM,
                NULL ORDER_LINE_NUM,
                flds.prod_num,
                flds.bunit_name,
                flds.check_date,
                flds.hdrearn_start_dt,
                flds.hdrearn_end_dt,
                per_unit_cntrct_num,
                per_unit_cntrct_price,
                bunittyp_id
           FROM (SELECT --DISTINCT -- Chris Biddle 5/18/2012 distinct required for some WIPs
                        rh.rbt_hdr_num,
                        l_termnum rbt_term_num,
                        g_wipnum rbt_wip_num,
                        product.prodnm prod_num,
                        price.prclst_num base_cntrct_num,
                        rbtindsls.puertorico_sale inc_pr_sales,
                        product.prodid prod_id_pri,
                        l_dr_lccode line_class_cd,
                        TO_CHAR (rbtindsls.submitem_dt_cbk_invoice_corr,
                                 'YY')
                           earned_yr,
                        TO_CHAR (rbtindsls.submitem_dt_cbk_invoice_corr,
                                 'MM')
                           earned_month,
                        rbtindsls.submitem_cbk_invoice_orig di_num,
                        rbtindsls.submdat_num,
                        rbtindsls.submitem_num,
                        rbtindsls.submitem_units qty,
                        ROUND (rbtindsls.submitem_contract_price_corr, 5)
                           base_cntrct_price,
                        rh.strategy_type_num,
                        rh.rbt_hdr_desc bus_seg,
                        l_prdsdt per_start_dt,
                        ROUND (
                             rbtindsls.submitem_units
                           * rbtindsls.submitem_contract_price_corr,
                           5)
                           sales_in_dollars,
                        rh.data_source_num data_source_type_num,
                        NVL (rbtindsls.bunit_num, rbtcuswhl.buntnm) bunit_num,
                        rbtcuswhl.buntname bunit_name,
                        DECODE (rbtindsls.bunit_num,
                                NULL, rbtcuswhl.buntid,
                                rbtindsls.cust_bunit_id_sec)
                           bunit_ident,
                        DECODE (rbtindsls.bunit_num,
                                NULL, rbtcuswhl.buntdc,
                                rbtindsls.cust_bunit_idtyp_sec)
                           bunit_identtyp,
                        product.uomnam uom_name,
                        product.prodds prod_desc,
                        l_status_num line_status_num,
                        l_wipgdt wip_gen_dt,
                        rh.paymnt_num,
                        1 stage_num,
                        rbtindsls.cont_id base_prclst,
                        g_username username,
                        SYSDATE lastmod,
                        rbtindsls.line_class transaction_type,
                        rbtindsls.submitem_dt_cbk_invoice_corr check_date,
                        c_hdr_e_dt hdrearn_end_dt,
                        c_hdr_s_dt hdrearn_start_dt,
                        NULL per_unit_cntrct_num,
                        0.0 per_unit_cntrct_price,
                        rbtindsls.cust_bunittyp_id bunittyp_id
                   FROM ind_indirsales_in rbtindsls,
                        --rbt_terms rt,
                        rbt_hdr rh,
                        rbt_wip rw,
                        ind_product product,
                        vir_prclst_excl price,
                        ind_rbtcuswhl1 rbtcuswhl
                  WHERE     rh.rbt_hdr_num = c_hdrnum
                        AND rbtindsls.rbt_wip_num = g_wipnum
                        AND rbtcuswhl.rbt_wip_num = g_wipnum
                        AND product.rbt_wip_num = g_wipnum
                        AND rw.rbt_wip_num = g_wipnum
                        -- AND rt.status_grbt_num IN (7, 8)
                        AND rh.rbt_hdr_num = rbtcuswhl.hdrnum
                        AND rbtindsls.bunit_source = rbtcuswhl.buntnm
                        --AND rh.rbt_hdr_num = c_hdrnum
                        AND price.rbt_hdr_num = rh.rbt_hdr_num
                        AND product.hdrnum = rh.rbt_hdr_num
                        AND price.data_source_num = g_indsrnum
                        ----Added for UAT fix
                         AND (   (    price.prclst_num IN (-1, -2, -3)
                        AND EXISTS
                               (SELECT 1
                                  FROM gr_pbasis_mv A1,
                                       gr_pbasiscd_mv c,
                                       gr_status_mv d,
                                       gr_uom_mv e
                                 WHERE     A1.prod_num = rbtindsls.prod_num
                                       AND e.uom_num = A1.uom_num
                                       AND e.uom_name = 'PACKAGES'
                                       AND A1.pbasiscd_num = c.pbasiscd_num
                                       AND c.pbasiscd_id IN
                                              ('WAC', 'GR_LIST')
                                       AND d.status_num = A1.status_num
                                       AND d.status_abbr IN
                                              ('ACT', 'CDP', 'CP')
                                                                  ))
                    OR (EXISTS
                           (SELECT 1
                              FROM gr_cpgrp_mv A2
                             WHERE        price.prclst_num = -3
                                      AND A2.cpgrp_cont_id_alias =
                                             rbtindsls.cont_id
                                   OR     A2.cpgrp_num = price.prclst_num
                                      AND A2.cpgrp_cont_id_alias =
                                             rbtindsls.cont_id))
                    OR rbtindsls.cont_id IS NULL)
                        AND rbtindsls.prod_num = product.prodnm
                        AND (       rh.inc_pr_sales = 'Y'
                                AND (   rbtindsls.puertorico_sale = 'G'
                                     OR rbtindsls.puertorico_sale = 'R')
                             OR     rh.inc_pr_sales = 'N'
                                AND rbtindsls.puertorico_sale = 'G')
                        AND (   (    rh.GROSS_NET_VIR_INDIRECT = 'G' ---CR#34046, changed gross_net to gross_net_vir_indirect
                                 AND (rbtindsls.line_class = 'CHB'  --CR#34047, Changed the definition of gross for Indirect VIR
                                  OR (rbtindsls.line_class = 'DB'
                                  AND
                                  rbtindsls.subtype_cd='RESUBMIT')))
                             OR (    rh.GROSS_NET_VIR_INDIRECT = 'N'---CR#34046, changed gross_net to gross_net_vir_indirect
                                 AND rbtindsls.line_class IN ('CHB', 'DB')))) flds;
   BEGIN

      p_log (
         i_proc      => 'p_vir_indirect',
         i_message   => 'Procedure start: to apply filter on selected lines');



      SELECT rw.rbt_wip_prd_start_dt,
             rw.rbt_wip_prd_end_dt,
             rh.rbt_hdr_num,
             rh.rbt_hdr_start_dt,
             rh.rbt_hdr_end_dt,
             RH.GROSS_NET_vir_indirect,---CR#34046, changed gross_net to gross_net_vir_indirect
             RT.RBT_TERM_NUM
        INTO l_prdsdt,
             l_prdedt,
             l_hdrnum,
             l_hdr_s_dt,
             l_hdr_e_dt,
             g_gross_net,
             l_termnum
        FROM rbt_wip rw,
             rbt_hdr rh,
             rbt_terms rt,
             status_grbt sg
       WHERE     rw.rbt_wip_num = g_wipnum
             AND rh.rbt_hdr_num = rw.rbt_hdr_num
             AND rt.rbt_hdr_num = rh.rbt_hdr_num
             AND sg.status_grbt_num = rt.status_grbt_num
             AND sg.status_abbr = 'AC' --AND RH.DATA_SOURCE_NUM = g_indsrnum--AND ROWNUM = 1
                                      ;

      OPEN indirsales2 (l_prdsdt,
                        l_prdedt,
                        l_hdrnum,
                        l_hdr_s_dt,
                        l_hdr_e_dt);

      l_cnt := 0;

      LOOP
         FETCH indirsales2
         BULK COLLECT INTO l_data
         LIMIT ln_limit;

         l_cnt := l_cnt + l_data.COUNT;


         IF l_cnt = 0
         THEN
            p_errorlog_insertion (
               SQLCODE,
               'p_vir_indirect - No Data found at Header# ' || l_hdrnum,
               g_wipnum,
               g_calnum,
               g_indsrnum,
               g_er_status_num);
         END IF;

         --         p_log (i_proc => 'p_vir_indirect', i_message => l_data.COUNT);

         FORALL i IN 1 .. l_data.COUNT
            INSERT INTO LOGTABLE_MRR
                 VALUES l_data (i);

         COMMIT;

         EXIT WHEN indirsales2%NOTFOUND;
      END LOOP;

      CLOSE indirsales2;

      COMMIT;



      -- PT Team change 12/07/2012

      BEGIN
         DBMS_STATS.gather_table_stats (
            ownname            => 'grebates',
            tabname            => 'LOGTABLE_MRR',
            partname           => NULL,
            cascade            => TRUE,
            ESTIMATE_PERCENT   => DBMS_STATS.DEFAULT_ESTIMATE_PERCENT,
            method_opt         => 'for all indexed columns size 200',
            degree             => 4);
      END;

      -- PT Team change 12/07/2012




      p_log (
         i_proc      => 'p_vir_indirect',
         i_message   => 'Procedure end: to apply filter on selected lines');
   EXCEPTION
      WHEN OTHERS
      THEN
         p_errorlog_insertion (SQLCODE,
                               'p_vir_indirect',
                               g_wipnum,
                               g_calnum,
                               g_indsrnum,
                               g_er_status_num);
         RAISE;
   END p_vir_indirect;



   FUNCTION fn_total_sales_indirect (fn_stgnum NUMBER, fn_wipnum NUMBER)
      RETURN NUMBER
   AS
      l_totsls   NUMBER (22, 7);
   BEGIN
      SELECT SUM (SALES_IN_DOLLARS)
        INTO l_totsls
        FROM LOGTABLE_MRR
       WHERE STAGE_NUM = fn_stgnum AND RBT_WIP_NUM = fn_wipnum;

      RETURN l_totsls;
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               fn_wipnum,
                               g_calnum,
                               g_indsrnum,
                               g_er_status_num,
                               g_isrpt);
         RETURN 0;
      WHEN OTHERS
      THEN
         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               fn_wipnum,
                               g_calnum,
                               g_indsrnum,
                               g_er_status_num,
                               g_isrpt);
         RETURN 0;
   END fn_total_sales_indirect;

   PROCEDURE p_insert_logtable_stage (i_wipnum         NUMBER,
                                      i_dsrnum         NUMBER,
                                      i_total_sales    NUMBER,
                                      i_vir_sales      NUMBER,
                                      i_vir_amt        NUMBER)
   AS
   BEGIN
      p_log (
         i_proc      => 'p_insert_logtable_stage',
         i_message   => 'Procedure for an InDirect Data Source validation for a Stage - 2 started:Insertion into LOGTABLE_MRR  for STAGE 3');

      -- Insertion into LOGTABLE_MRR P for STAGE 2

      INSERT INTO logtable_mrr (RBT_HDR_NUM,
                                RBT_TERM_NUM,
                                RBT_WIP_NUM,
                                BASE_CNTRCT_NUM,
                                INC_PR_SALES,
                                PROD_ID_PRI,
                                LINE_CLASS_CD,
                                OFFSET_ST_DT,
                                EARNED_YR,
                                EARNED_MONTH,
                                PER_UNIT_RBT,
                                DI_NUM,
                                DI_LINE_NUM,
                                QTY,
                                BASE_CNTRCT_PRICE,
                                STRATEGY_TYPE_NUM,
                                BUS_SEG,
                                PER_START_DT,
                                OFFSET_CNTRCT_NUM,
                                OFFSET_END_DT,
                                OFFSET_CNTRCT_PRICE,
                                SALES_IN_DOLLARS,
                                DATA_SOURCE_TYPE_NUM,
                                DIR_START_DT,
                                DIR_END_DT,
                                BUNIT_NUM,
                                BUNIT_IDENTTYP,
                                UOM_NAME,
                                PROD_DESC,
                                RBT_AMT,
                                LINE_NUM,
                                LINE_STATUS_NUM,
                                WIP_GEN_DT,
                                PAYMNT_NUM,
                                STAGE_NUM,
                                BASE_PRCLST,
                                USERNAME,
                                LASTMOD,
                                OFFSET_PRCLST,
                                COMMENTS,
                                LOGWIPLN_NUM,
                                BUNIT_IDENT,
                                TRANSACTION_TYPE,
                                ORDER_TYPE,
                                SUBMDAT_NUM,
                                SUBMITEM_NUM,
                                ORDER_NUM,
                                ORDER_LINE_NUM,
                                PROD_NUM,
                                BUNIT_NAME,
                                CHECK_DATE,
                                EARNED_START_DT,
                                EARNED_END_DT,
                                PER_UNIT_CNTRCT_NUM,
                                PER_UNIT_CNTRCT_PRICE,
                                BUNITTYP_ID)
                                SELECT RBT_HDR_NUM, -- Added for UAT fix
                RBT_TERM_NUM,
                RBT_WIP_NUM,
                BASE_CNTRCT_NUM,
                INC_PR_SALES,
                PROD_ID_PRI,
                CASE
                   WHEN (RBT_AMT) >= 0
                   THEN
                        l_cr_lccode
                   WHEN (RBT_AMT) < 0
                   THEN
                        l_dr_lccode
                END LINE_CLASS_CD,
                OFFSET_ST_DT,
                EARNED_YR,
                EARNED_MONTH,
                PER_UNIT_RBT,
                DI_NUM,
                DI_LINE_NUM,
                QTY,
                BASE_CNTRCT_PRICE,
                STRATEGY_TYPE_NUM,
                BUS_SEG,
                PER_START_DT,
                OFFSET_CNTRCT_NUM,
                OFFSET_END_DT,
                OFFSET_CNTRCT_PRICE,
                SALES_IN_DOLLARS,
                DATA_SOURCE_TYPE_NUM,
                DIR_START_DT,
                DIR_END_DT,
                BUNIT_NUM,
                BUNIT_IDENTTYP,
                UOM_NAME,
                PROD_DESC,
                RBT_AMT,
                LINE_NUM,
                LINE_STATUS_NUM,
                WIP_GEN_DT,
                PAYMNT_NUM,
                STAGE_NUM,                                                        --3,
                BASE_PRCLST,
                USERNAME,
                LASTMOD,
                OFFSET_PRCLST,
                COMMENTS,
                 logtable_sq.NEXTVAL,
                BUNIT_IDENT,
                TRANSACTION_TYPE,
                ORDER_TYPE,
                SUBMDAT_NUM,
                SUBMITEM_NUM,
                ORDER_NUM,
                ORDER_LINE_NUM,
                PROD_NUM,
                BUNIT_NAME,
                CHECK_DATE,
                EARNED_START_DT,
                EARNED_END_DT,
                PER_UNIT_CNTRCT_NUM,
                PER_UNIT_CNTRCT_PRICE,
                BUNITTYP_ID FROM
         (SELECT RBT_HDR_NUM,
                RBT_TERM_NUM,
                RBT_WIP_NUM,
                BASE_CNTRCT_NUM,
                INC_PR_SALES,
                PROD_ID_PRI,
                LINE_CLASS_CD,
                OFFSET_ST_DT,
                EARNED_YR,
                EARNED_MONTH,
                PER_UNIT_RBT,
                DI_NUM,
                DI_LINE_NUM,
                QTY,
                BASE_CNTRCT_PRICE,
                STRATEGY_TYPE_NUM,
                BUS_SEG,
                PER_START_DT,
                OFFSET_CNTRCT_NUM,
                OFFSET_END_DT,
                OFFSET_CNTRCT_PRICE,
                (SALES_IN_DOLLARS / i_total_sales) * i_vir_sales SALES_IN_DOLLARS,
                DATA_SOURCE_TYPE_NUM,
                DIR_START_DT,
                DIR_END_DT,
                BUNIT_NUM,
                BUNIT_IDENTTYP,
                UOM_NAME,
                PROD_DESC,
                round(round((SALES_IN_DOLLARS / i_total_sales), 8) * round(i_vir_amt, 2), 2) RBT_AMT,
                fn_linenum (g_lnnum) LINE_NUM,
                LINE_STATUS_NUM,
                WIP_GEN_DT,
                PAYMNT_NUM,
                2 STAGE_NUM,                                                        --3,
                BASE_PRCLST,
                USERNAME,
                LASTMOD,
                OFFSET_PRCLST,
                COMMENTS,
              --  logtable_sq.NEXTVAL LOGWIPLN_NUM,
                BUNIT_IDENT,
                TRANSACTION_TYPE,
                ORDER_TYPE,
                SUBMDAT_NUM,
                SUBMITEM_NUM,
                ORDER_NUM,
                ORDER_LINE_NUM,
                PROD_NUM,
                BUNIT_NAME,
                CHECK_DATE,
                EARNED_START_DT,
                EARNED_END_DT,
                PER_UNIT_CNTRCT_NUM,
                PER_UNIT_CNTRCT_PRICE,
                BUNITTYP_ID
           FROM LOGTABLE_MRR
          WHERE RBT_WIP_NUM = i_wipnum AND stage_num = 1);                 --2;

      COMMIT;


      -- PT Team change 12/07/2012

      BEGIN
         DBMS_STATS.gather_table_stats (
            ownname            => 'grebates',
            tabname            => 'LOGTABLE_MRR',
            partname           => NULL,
            cascade            => TRUE,
            ESTIMATE_PERCENT   => DBMS_STATS.DEFAULT_ESTIMATE_PERCENT,
            method_opt         => 'for all indexed columns size 200',
            degree             => 4);
      END;


      -- PT Team change 12/07/2012

      p_log (i_proc => 'p_insert_logtable_stage', i_message => 'Procedure end');
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               i_wipnum,
                               g_calnum,
                               g_indsrnum,
                               g_er_status_num,
                               g_isrpt);
         RAISE;
      WHEN OTHERS
      THEN
         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               i_wipnum,
                               g_calnum,
                               g_indsrnum,
                               g_er_status_num,
                               g_isrpt);
         ROLLBACK;
         RAISE;
   END p_insert_logtable_stage;

   PROCEDURE p_ins_rbt_wip_dtl (i_stgnum    NUMBER,
                                i_wipnum    NUMBER,
                                i_dsrnum    NUMBER)
   AS
      l_strgnm    NUMBER;
      l_asrhstg   NUMBER;
   BEGIN
      -- Process log enties for Procedure for Data transaformation from Staging to audit tables and Rbt_Wip_Tables

      p_log (
         i_proc      => ' p_ins_rbt_wip_dtl',
         i_message   => 'Procedure for Data transaformation from Staging to audit tables and Rbt_Wip_Tables');


      -- g_lnnum := 0;

      INSERT /*+ append */
            INTO  rbt_wip_dtl (rbt_wip_dtl_num,
                               rbt_hdr_num,
                               rbt_term_num,
                               earned_end_dt,
                               earned_start_dt,
                               rbt_wip_num,
                               base_cntrct_num,
                               --                               inc_pr_sales,
                               prod_id_pri,
                               line_class_cd,
                               offset_st_dt,
                               earned_yr,
                               earned_month,
                               per_unit_rbt,
                               --                               di_num,
                               di_line_num,
                               qty,
                               base_cntrct_price,
                               strategy_type_num,
                               bus_seg,
                               per_start_dt,
                               offset_cntrct_num,
                               offset_end_dt,
                               offset_cntrct_price,
                               sales_in_dollars,
                               data_source_type_num,
                               dir_start_dt,
                               dir_end_dt,
                               bunit_num,
                               --                               bunit_name,
                               bunit_identtyp_sec,
                               bunit_ident_sec,
                               uom_name,
                               prod_desc,
                               rbt_amt,
                               line_num,
                               line_status_num,
                               wip_gen_dt,
                               paymnt_num,
                               base_prclst,
                               username,
                               lastmod,
                               offset_prclst,
                               order_type,
                               transaction_type,
                               --                               order_num,
                               --                               order_line_num,
                               --                               submdat_num,
                               --                               submitem_num,
                               per_unit_cntrct_num,
                               per_unit_cntrct_price,
                               bunittyp_id)
         SELECT grebates.rbt_wip_dtl_sq.NEXTVAL,
                rbt_hdr_num,
                rbt_term_num,
                earned_end_dt,
                earned_start_dt,
                rbt_wip_num,
                base_cntrct_num,
                --                inc_pr_sales,
                prod_id_pri,
                line_class_cd,
                offset_st_dt,
                earned_yr,
                earned_month,
                per_unit_rbt,
                --                di_num,
                di_line_num,
                qty,
                base_cntrct_price,
                strategy_type_num,
                bus_seg,
                per_start_dt,
                offset_cntrct_num,
                offset_end_dt,
                offset_cntrct_price,
                sales_in_dollars,
                data_source_type_num,
                dir_start_dt,
                dir_end_dt,
                bunit_num,
                --                bunit_name,
                bunit_identtyp,
                bunit_ident,
                uom_name,
                prod_desc,
                rbt_amt,
                fn_linenum (g_lnnum) line_num,
                --                CASE
                --                   WHEN (rbt_amt = 0 OR base_cntrct_price = 0)
                --                   THEN
                --                      g_ca_status_num
                --                   ELSE
                g_status_num --                END
                             line_status_num,
                wip_gen_dt,
                paymnt_num,
                base_prclst,
                username,
                lastmod,
                offset_prclst,
                order_type,
                transaction_type,
                --                order_num,
                --                order_line_num,
                --                submdat_num,
                --                submitem_num,
                per_unit_cntrct_num,
                per_unit_cntrct_price,
                bunittyp_id
           FROM (  SELECT rbt_hdr_num,
                          rbt_term_num,
                          earned_end_dt,
                          earned_start_dt,
                          rbt_wip_num,
                          base_cntrct_num,
                          --inc_pr_sales,
                          prod_id_pri,
                          line_class_cd,
                          offset_st_dt,
                          earned_yr,
                          earned_month,
                          per_unit_rbt,
                          --di_num,
                          di_line_num,
                          SUM (NVL (qty, 0.0)) qty,
                          ROUND (NVL (base_cntrct_price, 0.0), 5)
                             base_cntrct_price,
                          strategy_type_num,
                          bus_seg,
                          per_start_dt,
                          offset_cntrct_num,
                          offset_end_dt,
                          offset_cntrct_price,
                          SUM (ROUND (NVL (sales_in_dollars, 0.0), 5))
                             sales_in_dollars,
                          data_source_type_num,
                          dir_start_dt,
                          dir_end_dt,
                          bunit_num,
                          --                bunit_name,
                          bunit_identtyp,
                          bunit_ident,
                          uom_name,
                          prod_desc,
                            SUM (rbt_amt) rbt_amt,
                          --                null line_num , -- fn_linenum(g_lnnum)
                          --                CASE
                          --                   WHEN (rbt_amt = 0 OR base_cntrct_price = 0)
                          --                   THEN
                          --                      g_ca_status_num
                          --                   ELSE
                          --                      g_status_num
                          --                END
                          line_status_num,
                          wip_gen_dt,
                          paymnt_num,
                          base_prclst,
                          username,
                          lastmod,
                          offset_prclst,
                          order_type,
                          transaction_type,
                          --                order_num,
                          --                order_line_num,
                          --                submdat_num,
                          --                submitem_num,
                          per_unit_cntrct_num,
                          per_unit_cntrct_price,
                          bunittyp_id
                     FROM logtable_mrr stg
                    WHERE     stg.rbt_wip_num = i_wipnum
                          AND stg.stage_num = i_stgnum
                 GROUP BY rbt_hdr_num,
                          rbt_term_num,
                          rbt_wip_num,
                          base_cntrct_num                 --  ,   inc_pr_sales
                                         ,
                          prod_id_pri,
                          line_class_cd,
                          offset_st_dt,
                          earned_yr,
                          earned_month,
                          per_unit_rbt,
                          di_line_num,
                          ROUND (NVL (base_cntrct_price, 0.0), 5),
                          strategy_type_num,
                          bus_seg,
                          per_start_dt,
                          offset_cntrct_num,
                          offset_end_dt,
                          offset_cntrct_price,
                          data_source_type_num,
                          dir_start_dt,
                          dir_end_dt,
                          bunit_num,
                          bunit_identtyp,
                          bunit_ident,
                          bunittyp_id,
                          uom_name,
                          prod_desc,
                          line_status_num,
                          wip_gen_dt,
                          paymnt_num,
                          base_prclst,
                          username,
                          lastmod,
                          offset_prclst,
                          order_type,
                          transaction_type,
                          earned_end_dt,
                          earned_start_dt,
                          per_unit_cntrct_num,
                          per_unit_cntrct_price) inner_block;


      COMMIT;
      p_log (i_proc => ' p_ins_rbt_wip_dtl', i_message => 'Procedure end');
   EXCEPTION
      WHEN NO_DATA_FOUND
      THEN
         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               i_wipnum,
                               g_calnum,
                               g_indsrnum,
                               g_er_status_num,
                               g_isrpt);
         RAISE;
      WHEN OTHERS
      THEN
         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               i_wipnum,
                               g_calnum,
                               g_indsrnum,
                               g_er_status_num,
                               g_isrpt);
         ROLLBACK;
         RAISE;
   END p_ins_rbt_wip_dtl;

   PROCEDURE p_insert_to_final_logtable
   AS
      CURSOR c_log
      IS
         SELECT *
           FROM LOGTABLE_MRR
          WHERE RBT_WIP_NUM = g_wipnum;

      l_cnt    NUMBER;

      TYPE TEMP_ARRAY IS TABLE OF c_log%ROWTYPE;



      l_data   TEMP_ARRAY;
   BEGIN
      p_log (i_proc      => 'p_insert_to_final_logtable',
             i_message   => 'Insert into final logtable');

      --      BEGIN
      --         IF g_calnum <> 2
      --         THEN
      --            DELETE FROM logtable t
      --                  WHERE t.rbt_wip_num = g_wipnum;
      --         ELSE
      --            DELETE FROM logtable t
      --                  WHERE t.rbt_wip_num = g_wipnum AND t.stage_num = 4;
      --         END IF;
      --
      --         COMMIT;
      --      EXCEPTION
      --         WHEN OTHERS
      --         THEN
      --            NULL;
      --      END;



      OPEN c_log;

      l_cnt := 0;

      LOOP
         FETCH c_log
         BULK COLLECT INTO l_data
         LIMIT ln_limit;

         l_cnt := l_cnt + l_data.COUNT;

         IF l_cnt = 0
         THEN
            p_errorlog_insertion (
               SQLCODE,
                  'p_insert_to_final_logtable - No Data found at Header# '
               || g_hdrnum,
               g_wipnum,
               g_calnum,
               g_indsrnum,
               g_er_status_num);
         END IF;

         FORALL i IN 1 .. l_data.COUNT
            INSERT /*+ APPEND */
                  INTO  LOGTABLE
                 VALUES l_data (i);

         COMMIT;

         EXIT WHEN c_log%NOTFOUND;
      END LOOP;

      CLOSE c_log;

      COMMIT;
      p_log (i_proc      => 'p_insert_to_final_logtable',
             i_message   => 'Procedure end');
   EXCEPTION
      WHEN OTHERS
      THEN
         p_errorlog_insertion (SQLCODE,
                               'p_insert_to_final_logtable',
                               g_wipnum,
                               0,
                               4,
                               'N');
         RAISE;
   END p_insert_to_final_logtable;



   PROCEDURE p_vir_indirsales_in
   AS
      TYPE ARRAY IS TABLE OF ind_indirsales_in%ROWTYPE;

      l_data       ARRAY;
      i            INTEGER;

      l_prdsdt     DATE;
      l_prdedt     DATE;
      l_hdrnum     NUMBER;
      l_hdr_s_dt   DATE;
      l_hdr_e_dt   DATE;



      CURSOR indirsales (
         c_prdsdt    DATE,
         c_prdedt    DATE,
         c_hdrnum    NUMBER)
      IS
         SELECT                                                   -- Suresh TP
                                                                  /*+ PARALLEL ( rbtindsls 8) */
                                                                  -- Suresh TP
               rbtindsls.*, g_wipnum
          FROM INDIRSALES_IN rbtindsls,
               --rbt_terms rt,
               rbt_hdr rh,
               vir_prclst_excl price,
               rbt_wip rw
         WHERE     rh.rbt_hdr_num = c_hdrnum
               AND price.rbt_hdr_num = rh.rbt_hdr_num
               AND price.data_source_num = g_indsrnum
               AND (   (    price.prclst_num IN (-1, -2, -3)
                        AND EXISTS
                               (SELECT 1
                                  FROM gr_pbasis_mv A1,
                                       gr_pbasiscd_mv c,
                                       gr_status_mv d,
                                       gr_uom_mv e
                                 WHERE     A1.prod_num = rbtindsls.prod_num
                                       AND e.uom_num = A1.uom_num
                                       AND e.uom_name = 'PACKAGES'
                                       AND A1.pbasiscd_num = c.pbasiscd_num
                                       AND c.pbasiscd_id IN
                                              ('WAC', 'GR_LIST')
                                       AND d.status_num = A1.status_num
                                       AND d.status_abbr IN
                                              ('ACT', 'CDP', 'CP') -- ?? should we remove this ??
                                                                  ))
                    OR (EXISTS
                           (SELECT 1
                              FROM gr_cpgrp_mv A2
                             WHERE        price.prclst_num = -3
                                      AND A2.cpgrp_cont_id_alias =
                                             rbtindsls.cont_id
                                   OR     A2.cpgrp_num = price.prclst_num
                                      AND A2.cpgrp_cont_id_alias =
                                             rbtindsls.cont_id))
                    OR rbtindsls.cont_id IS NULL)
               AND rh.rbt_hdr_num = rw.rbt_hdr_num
               AND (   (    RH.INDIRECT_DATE_QUALIFIER_NUM = g_indirship -- 197
                        AND TRUNC (rbtindsls.submitem_dt_cbk_invoice_corr) BETWEEN c_prdsdt
                                                                               AND c_prdedt)
                    OR (    RH.INDIRECT_DATE_QUALIFIER_NUM = g_indirpaid -- 196
                        AND TRUNC (rbtindsls.stlstat_dt) BETWEEN c_prdsdt
                                                             AND c_prdedt))
               AND rw.rbt_wip_num = g_wipnum
               AND price.incl_flag = 'Y';
   -- AND rt.status_grbt_num = 7;
   BEGIN
      p_log (
         i_proc      => 'p_vir_indirsales_in',
         i_message   => 'Procedure start:to insert indirect sales line in temp table ');



      SELECT rw.rbt_wip_prd_start_dt,
             rw.rbt_wip_prd_end_dt,
             rh.rbt_hdr_num,
             rh.rbt_hdr_start_dt,
             rh.rbt_hdr_end_dt
        INTO l_prdsdt,
             l_prdedt,
             l_hdrnum,
             l_hdr_s_dt,
             l_hdr_e_dt
        FROM rbt_wip rw,
             rbt_hdr rh,
             rbt_terms rt,
             status_grbt sg
       WHERE     rw.rbt_wip_num = g_wipnum
             AND rh.rbt_hdr_num = rw.rbt_hdr_num
             AND rt.rbt_hdr_num = rh.rbt_hdr_num
             AND sg.status_grbt_num = rt.status_grbt_num
             AND sg.status_abbr = 'AC'
             --AND RH.DATA_SOURCE_NUM = g_indsrnum
             AND ROWNUM = 1;

      OPEN indirsales (l_hdr_s_dt, l_hdr_e_dt, l_hdrnum);

      LOOP
         FETCH indirsales
         BULK COLLECT INTO l_data
         LIMIT ln_limit;

         FORALL i IN 1 .. l_data.COUNT
            INSERT /*+ APPEND */
                  INTO  ind_indirsales_in
                 VALUES l_data (i);

         COMMIT;

         EXIT WHEN indirsales%NOTFOUND;
      END LOOP;

      CLOSE indirsales;

      COMMIT;

      p_log (
         i_proc      => 'p_vir_indirsales_in',
         i_message   => 'Procedure end:to insert indirect sales line in temp table ');
   EXCEPTION
      WHEN OTHERS
      THEN
         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               g_wipnum,
                               g_calnum,
                               g_indsrnum,
                               g_er_status_num);
   END p_vir_indirsales_in;

   PROCEDURE P_VIR_BOTH_CALC (i_calnum   IN NUMBER,
                              i_dsrnum   IN NUMBER,
                              i_wipnum   IN NUMBER,
                              i_name     IN VARCHAR2,
                              i_status   IN NUMBER,
                              i_isRPT    IN VARCHAR2)
   IS
      -- Local variable Declaration

      --      l_cr_lccode        grebates.cd_references.ref_typ_val%TYPE;
      --      l_dr_lccode        grebates.cd_references.ref_typ_val%TYPE;
      l_trmnum           NUMBER;
      l_ern_s_dt         rbt_terms.earned_start_dt%TYPE;
      l_ern_e_dt         rbt_terms.earned_end_dt%TYPE;
      l_hdrnum           NUMBER := 0;                      --Value for RBT HDR
      l_prdsdt           DATE;                   --Value for period start date
      l_prdedt           DATE;                -- Value for wip period end date
      l_wipgdt           DATE;                -- Value for WIP Generation Date
      l_rbtamt           NUMBER (22, 7) := 0.0;
      l_rbtqty           NUMBER (22, 7) := 0.0;
      l_rbtsls           NUMBER (22, 7) := 0.0;
      l_dir_totalsls     NUMBER (22, 7) := 0.0;
      l_indir_totalsls   NUMBER (22, 7) := 0.0;
      l_totalsls         NUMBER (22, 7) := 0.0;
      l_wipstatus        NUMBER;
      l_paid_rbtamt      NUMBER (22, 7) := 0.0;
      l_vir_sales        NUMBER (22, 7) := 0.0;
      l_vir_amt          NUMBER (22, 7) := 0.0;
      l_num_of_terms     NUMBER;
      l_flag             NUMBER;
      l_tier_per         VARCHAR2 (200);
      l_reftyp           VARCHAR2 (50); -- Refence type value for Check/Credit Memo
      l_paynum           NUMBER;
      l_pairke           NUMBER;    -- Pair key is generated based on WIP KEY.
      l_name             VARCHAR2 (256);
      l_wiptyp           NUMBER; -- WIP Type extract from CD Reference based on Original / Reconciliation
   BEGIN

      p_log (
         i_proc      => 'P_VIR_BOTH_CALC',
         i_message   =>    'Conditional Block to process Rebate Calculation (Initial/Re-Calculation/Reversal) p_calnum: '
                        || i_calnum
                        || ' p_dsrnum:'
                        || i_dsrnum);

      SELECT ref_num
        INTO g_indirship
        FROM cd_references cr
       WHERE cr.ref_typ = 'VirInDirDtQualifier' AND cr.ref_typ_val = 'Ship';

      SELECT ref_num
        INTO g_indirpaid
        FROM cd_references cr
       WHERE cr.ref_typ = 'VirInDirDtQualifier' AND cr.ref_typ_val = 'Paid';

      SELECT rw.rbt_wip_prd_start_dt,
             rw.rbt_wip_prd_end_dt,
             rw.wip_gen_dt,
             rh.rbt_hdr_num,
             rt.rbt_term_num,
             rt.earned_start_dt,
             rt.earned_end_dt
        INTO l_prdsdt,
             l_prdedt,
             l_wipgdt,
             l_hdrnum,
             l_trmnum,
             l_ern_s_dt,
             l_ern_e_dt
        FROM rbt_wip rw,
             rbt_hdr rh,
             rbt_terms rt,
             status_grbt sg
       WHERE     rw.rbt_wip_num = i_wipnum
             AND rh.rbt_hdr_num = rw.rbt_hdr_num
             AND rt.rbt_hdr_num = rh.rbt_hdr_num
             AND sg.status_grbt_num = rt.status_grbt_num
             AND sg.status_abbr = 'AC'
             AND ROWNUM = 1;

      g_termnum := l_trmnum;

      BEGIN
         SELECT param_number_value
           INTO ln_limit
           FROM IND_PARAMS
          WHERE param_name = 'LIMIT';
      EXCEPTION
         WHEN OTHERS
         THEN
            ln_limit := 50000;
      END;

      -- Conditional Block to process Rebate Calculation (Initial/Re-Calculation/Import/Aprrove)
      CASE
         WHEN    ( (i_calnum = 4 OR i_calnum = 1) AND i_isRPT = 'N')
              OR (i_calnum = 1 AND i_isRPT = 'Y')
         THEN


            -- Delete temp tables where wipnum exists

            EXECUTE IMMEDIATE 'TRUNCATE TABLE LOGTABLE_TEMP';

            EXECUTE IMMEDIATE 'TRUNCATE TABLE LOGTABLE_TEMP1';

            EXECUTE IMMEDIATE 'TRUNCATE TABLE LOGTABLE_MRR';



            EXECUTE IMMEDIATE 'TRUNCATE TABLE IND_RBTCUSWHL1';

            EXECUTE IMMEDIATE 'TRUNCATE TABLE IND_PRODUCT';

            EXECUTE IMMEDIATE 'TRUNCATE TABLE IND_INDIRSALES_IN';


            -- Direct Source rebate calculation for stage - 1

            p_vir_dirdsrc (i_wipnum     -- wip number associated to the header
                                   ,
                           l_hdrnum                          -- Rebate progarm
                                   ,
                           g_direct                      -- Data Source number
                                   ,
                           l_cr_lccode -- Credit line Class Code - based on strategy referenced from CD References
                                      ,
                           l_dr_lccode -- Debit line Class Code - based on strategy referenced from CD References
                                      ,
                           l_prdsdt                -- Period Start Date of WIP
                                   ,
                           l_prdedt                -- period end date of a WIP
                                   ,
                           l_wipgdt                     -- Wip genration date.
                                   ,
                           l_trmnum                             -- Rebate Term
                                   ,
                           l_ern_s_dt                  -- Term Earn Start Date
                                     ,
                           l_ern_e_dt                    -- Term Earn End Date
                                     );

            -- After Stage1, Data Transformation from LOGTABLE_TEMP to LOGTABLE with condition check as Stage_Num = 1

            p_vir_datains (1 -- Stage Number 1, Based on stage data insertion into logtable from logtable temp1
                            , i_wipnum, g_direct);


            IF fn_wiplog (i_wipnum, 1) = 1
            THEN
               l_wipstatus := 1;
            END IF;


            g_indsrnum := g_indirect;
            p_vir_indir_rbtcuswhl;

            p_vir_indir_product;

            p_vir_indirsales_in;

            g_lnnum := 0;
            p_vir_indirect;


            l_dir_totalsls := fn_total_sales (1, i_wipnum);
            l_indir_totalsls := fn_total_sales_indirect (1, i_wipnum);
            l_totalsls := NVL (l_dir_totalsls, 0) + NVL (l_indir_totalsls, 0);

            p_elgbl_paid_rbt_lines (l_hdrnum,
                                    i_wipnum,
                                    l_trmnum,
                                    l_prdsdt,
                                    l_prdedt,
                                    l_paid_rbtamt);

            l_vir_sales := l_totalsls - NVL (l_paid_rbtamt, 0);


            -- Call tier calculation procedure
            P_TIER_CALC (l_vir_sales,
                         l_trmnum,
                         l_vir_amt,
                         l_tier_per);

            p_log (
               i_proc      => 'P_VIR_BOTH_CALC',
               i_message   =>    'total sales: '
                              || l_totalsls
                              || ' total paid rbt amount: '
                              || l_paid_rbtamt
                              || ' total vir sales: '
                              || l_vir_sales
                              || ' total vir amount :'
                              || l_vir_amt
                              || ' Tier %: '
                              || l_tier_per);
            p_dir_stg2dsrc (i_wipnum,
                            g_direct,
                            l_totalsls,
                            l_vir_sales,
                            l_vir_amt);

            p_insert_logtable_stage (i_wipnum,
                                     g_indirect,
                                     l_totalsls,
                                     l_vir_sales,
                                     l_vir_amt);

            p_insert_to_final_logtable;

            IF i_isRPT = 'N'
            THEN
               p_vir_datains (2 -- Stage Number 2, Based on stage data insertion into logtable from logtable temp1
                               , i_wipnum, g_direct);


               p_ins_rbt_wip_dtl (2, i_wipnum, g_indirect);



               SELECT COUNT (DISTINCT rbt_term_num)
                 INTO l_num_of_terms
                 FROM rbt_wip_dtl
                WHERE     rbt_wip_num = i_wipnum
                      AND line_status_num = g_status_num;

               IF l_num_of_terms = 0
               THEN
                  l_rbtqty := 0;
                  l_rbtsls := 0;
               ELSE
                  SELECT SUM (qty),
                         SUM (sales_in_dollars),
                         SUM (rbt_amt)
                    INTO l_rbtqty, l_rbtsls, l_rbtamt
                    FROM rbt_wip_dtl
                   WHERE     rbt_wip_num = i_wipnum
                         AND line_status_num = g_status_num;
               END IF;

               l_rbtqty := ROUND (l_rbtqty, 3);
               l_rbtsls := ROUND (l_rbtsls, 5);
               l_rbtamt :=ROUND (NVL (l_rbtamt, 0.0), 3);



               -- Update status of RBT_WIP after initial calculation based on WIP Detail lines if error exist then set WIP header status as 'Error'  else 'In-Review''

               IF    fn_wipdtlerr (i_wipnum, g_er_status_num) = 1
                  OR l_wipstatus = 1
               THEN
                  p_vir_updtwip (i_wipnum,
                                 l_rbtamt,
                                 l_rbtqty,
                                 l_rbtsls,
                                 g_er_status_num,
                                 l_totalsls,
                                 l_paid_rbtamt,
                                 l_tier_per);
               ELSE
                  p_vir_updtwip (i_wipnum,
                                 l_rbtamt,
                                 l_rbtqty,
                                 l_rbtsls,
                                 g_status_num,
                                 l_totalsls,
                                 l_paid_rbtamt,
                                 l_tier_per);
               END IF;


               -- Added Exception, when no data found then flag set to 0 so snapshot creation will ignore.


               BEGIN
                  SELECT 1
                    INTO l_flag
                    FROM rbt_wip rw, status_grbt sg
                   WHERE     rw.rbt_wip_num = i_wipnum
                         AND (       sg.status_grbt_num = rw.status_grbt_num
                                 AND sg.status_abbr = 'IR'
                              OR     sg.status_grbt_num = rw.status_grbt_num
                                 AND sg.status_abbr = 'ERR');
               EXCEPTION
                  WHEN NO_DATA_FOUND
                  THEN
                     l_flag := 0;
               END;

               -- Snapshot Creation for the WIPs status set to In-Review.
               IF l_flag = 1
               THEN
                  p_rbthdr_snapshot (l_hdrnum, g_status_num, i_wipnum);


                  p_rbttrm_snapshot (l_hdrnum, g_status_num, i_wipnum);
               END IF;


            END IF;


            p_vir_sumrpt (i_wipnum,
                          l_hdrnum,
                          l_trmnum,
                          i_dsrnum,
                          l_totalsls,
                          l_vir_sales,
                          l_paid_rbtamt,
                          l_tier_per);


         WHEN i_calnum = 2
         THEN
            g_lnnum := 0;
            p_log (i_proc      => ' p_vir_both_calc',
                   i_message   => 'Re-Calculation for Selective WIP');

            SELECT rt.rbt_term_num
              INTO l_trmnum
              FROM rbt_wip rw,
                   rbt_hdr rh,
                   rbt_terms rt,
                   status_grbt sg
             WHERE     rw.rbt_wip_num = i_wipnum
                   AND rh.rbt_hdr_num = rw.rbt_hdr_num
                   AND rt.rbt_hdr_num = rh.rbt_hdr_num
                   AND sg.status_grbt_num = rt.status_grbt_num
                   AND sg.status_abbr = 'AC'
                   AND ROWNUM = 1;

            -- If QTY is 0 then wip detail line will drop if the Data Source is External else set status as Cancel.

            UPDATE rbt_wip_dtl rwd
               SET rwd.line_status_num = g_ca_status_num
             WHERE rwd.rbt_wip_num = i_wipnum AND rwd.qty = 0;

            COMMIT;

            -- Procedure added for a Re-Calculation across 3 Data Sources - 16-04-2012
            p_wiprecalcn (i_wipnum,
                          l_cr_lccode,
                          l_dr_lccode,
                          i_dsrnum,
                          l_trmnum);

            COMMIT;
         WHEN i_calnum = 5
         THEN
            p_log (i_proc      => ' p_vir_both_calc',
                   i_message   => 'Wip Reversal for Selective WIP');

            -- Get the type of payment on Rebate program for a given WIP.
            SELECT rh.paymnt_num, cr.ref_typ_val
              INTO l_paynum, l_reftyp
              FROM rbt_hdr rh,
                   rbt_wip rw,
                   paymnt_info pi,
                   cd_references cr
             WHERE     rw.rbt_wip_num = i_wipnum
                   AND rh.rbt_hdr_num = rw.rbt_hdr_num
                   AND pi.PAYMNT_NUM = rh.paymnt_num
                   AND cr.ref_num = pi.paymnt_type_num
                   AND cr.ref_typ_val IN ('Check', 'Credit Memo');

            -- Pass the current wip key to set as Pair Key in Reversal WIP Creation
            l_pairke := i_wipnum;
            l_name := g_username;

            -- Based on payment, Reversal WIP is created.
            IF l_reftyp = 'Credit Memo'
            THEN
               SELECT cr.ref_num
                 INTO l_wiptyp
                 FROM cd_references cr
                WHERE cr.REF_TYP_VAL = 'Reversal';

               p_wiprvrsl (i_wipnum,
                           g_status_num,
                           l_wiptyp,
                           l_pairke,
                           i_dsrnum,
                           l_cr_lccode,
                           l_dr_lccode,
                           l_name);
            ELSE
               IF l_reftyp = 'Check'
               THEN
                  SELECT cr.ref_num
                    INTO l_wiptyp
                    FROM cd_references cr
                   WHERE cr.REF_TYP_VAL = 'Reversal-Check';

                  p_wiprvrsl (i_wipnum,
                              g_cl_status_num,
                              l_wiptyp,
                              l_pairke,
                              i_dsrnum,
                              l_cr_lccode,
                              l_dr_lccode,
                              l_name);
               END IF;
            END IF;

            COMMIT;
         WHEN i_calnum = 4 AND i_isRPT = 'Y'
         THEN
            --                  p_log (i_proc      => ' p_vir_indr_calc',
            --                         i_message   => 'Summary report for wip :'||i_wipnum);

            SELECT RW.SALES_AMT,
                   RW.TOTAL_ORIGINAL_SALES,
                   RW.TOTAL_PAID_REBATES,
                   RW.VIR_RBT_PER
              INTO l_vir_sales,
                   l_totalsls,
                   l_paid_rbtamt,
                   l_tier_per
              FROM RBT_WIP RW
             WHERE RW.RBT_WIP_NUM = i_wipnum;


            p_vir_sumrpt (i_wipnum,
                          l_hdrnum,
                          l_trmnum,
                          i_dsrnum,
                          l_totalsls,
                          l_vir_sales,
                          l_paid_rbtamt,
                          l_tier_per);

      END CASE;




      p_log (i_proc => 'P_VIR_BOTH_CALC', i_message => 'Procedure end ');
   EXCEPTION
      WHEN OTHERS
      THEN
         p_errorlog_insertion (SQLCODE,
                               SQLERRM,
                               i_wipnum,
                               i_calnum,
                               i_dsrnum,
                               g_er_status_num,
                               i_isRPT);
         ROLLBACK;
         RAISE;
   END P_VIR_BOTH_CALC;
END PKG_GR_VIR_CALC_ENGINE;
/