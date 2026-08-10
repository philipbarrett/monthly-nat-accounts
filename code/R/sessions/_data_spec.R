##########################################################################################
#
# _data_spec.R
#
# Series definitions for the baseline model: which GDP components/indicators, and
# where each one comes from (public FRED/BEA source, plus the original Haver
# mnemonic kept for provenance only). Shared between mly_GDP.R (the estimation
# pipeline) and validate_data_sources.R (the Stage 1 old-vs-new data check), so both
# always agree on the series list -- single source of truth, not duplicated.
#
# Requires l.options already defined (l.quarterly.data$target$name depends on
# l.options$make.gdp.err). See docs/data_sources.md for the full source mapping,
# the historical-splice rationale, and why ~16 series were dropped entirely.
#
##########################################################################################

l.quarterly.data <-
  list( target=list( label='gdp', name=paste0( ' GDP', if(l.options$make.gdp.err) '' else ', sum of components' ),
                     haver='GDPH@USNA', bea_table='T10106', bea_dataset='NIPA', bea_freq='Q',
                     bea_line=1, bea_description='Gross domestic product', scale=0.001 ),
        components=list(
          # cons_s/cons_g/nrfi/rfi: BEA's T10106 sub-component breakdowns (as
          # opposed to top-level aggregates like gdp/invent/x/m/gov) only have
          # chain-dollar LEVEL data from 2007Q1 onward -- confirmed by querying
          # every line's actual date range, not assumed from the table's top-line
          # history. Only growth-rate series go back further on FRED/BEA; no
          # public level series has full history. Spliced with archived Haver
          # data (1950Q1-2006Q4) instead of reconstructing via chain-linked
          # growth rates, since the exact historical levels are already sitting
          # in this repo. See docs/data_sources.md.
          cons_s=list( label='cons_s', name='1. Services consumption', haver='CSH@USNA',
                      bea_table='T10106', bea_dataset='NIPA', bea_freq='Q',
                      bea_line=6, bea_description='Services', scale=0.001, splice=TRUE ),
          cons_g=list( label='cons_g', name='2. Goods consumption', haver='CTGH@USNA',
                      bea_table='T10106', bea_dataset='NIPA', bea_freq='Q',
                      bea_line=3, bea_description='Goods', scale=0.001, splice=TRUE ),
          nrfi=list( label='nrfi', name='3. Non-residential Fixed Investment', haver='FNH@USNA',
                    bea_table='T10106', bea_dataset='NIPA', bea_freq='Q',
                    bea_line=9, bea_description='Nonresidential', scale=0.001, splice=TRUE ),
          rfi=list( label='rfi', name='4. Residential Fixed Investment', haver='FRH@USNA',
                   bea_table='T10106', bea_dataset='NIPA', bea_freq='Q',
                   bea_line=13, bea_description='Residential', scale=0.001, splice=TRUE ),
          invent=list( label='invent', name='8. Inventories', haver='VH@USNA', level.trend=TRUE,
                      bea_table='T10106', bea_dataset='NIPA', bea_freq='Q',
                      bea_line=14, bea_description='Change in private inventories', scale=0.001 ),
          x=list( label='x', name='6. Exports', haver='XH@USNA',
                 bea_table='T10106', bea_dataset='NIPA', bea_freq='Q',
                 bea_line=16, bea_description='Exports', scale=0.001 ),
          m=list( label='m', name='7. Imports', haver='MH@USNA', sign=-1,
                 bea_table='T10106', bea_dataset='NIPA', bea_freq='Q',
                 bea_line=19, bea_description='Imports', scale=0.001 ),
          gov=list( label='gov', name='5. Government', haver='GH@USNA',
                   bea_table='T10106', bea_dataset='NIPA', bea_freq='Q',
                   bea_line=22, bea_description='Government consumption expenditures and gross investment',
                   scale=0.001 )
          ) )

# Monthly baseline series only -- the ~16 series that were pulled but never
# referenced in any l.q/l.x (baseline or otherwise) have been dropped (oil_price,
# advance_exports/imports_goods, polb_inbound/outbound_loaded, ip, capu,
# res_building_employees, pub_construction/fed_constr/fed_emp/fed_receipts/
# fed_outlays/state_emp/local_emp/pub_wages, unemp, inv_retail, inv_xm, inv_trade,
# nondef_cap_goods, constant1, pce_exp, pce_exp_nondur, and the confirmed-dead-code
# cons_housing/ism.servs). Series needed only for the paper's robustness/validation
# appendix (real.retail.sales, pub_wages, pub_construction, fed_outlays,
# nondef_cap_goods, ip, oil_price, res_building_employees) move to the manual
# pathway -- see code/R/functions/legacy_haver/export_robustness_series.R and
# data/manual/robustness_series.csv. See docs/data_sources.md for the full mapping
# and the historical-splice rationale (splice=TRUE below).
l.monthly.data <- list(
  list( label='pce_exp_goods', name='Real PCE', haver='CTGBHM@USECON',
       bea_table='U20406', bea_dataset='NIUnderlyingDetail', bea_freq='M',
       bea_line=2, bea_description='Goods', scale=0.001, splice=TRUE ),
  list( label='pce_exp_dur', name='Real PCE Durables', haver='CDBHM@USECON',
       bea_table='U20406', bea_dataset='NIUnderlyingDetail', bea_freq='M',
       bea_line=3, bea_description='Durable goods', scale=0.001 ),
  list( label='pce_exp_servs', name='Real PCE Services', haver='CSBHM@USECON',
       bea_table='U20406', bea_dataset='NIUnderlyingDetail', bea_freq='M',
       bea_line=150, bea_description='Services', scale=0.001, splice=TRUE ),
  list( label='exports_value', name='Nominal exports', haver='BGSX@USECON', fred_id='BOPTEXP' ),
  list( label='export_prices', name='Export prices', haver='PXEA@USECON', fred_id='IQ' ),
  list( label='imports_value', name='Nominal imports', haver='BGSM@USECON', fred_id='BOPTIMP' ),
  list( label='import_prices', name='Import prices', haver='PMEA@USECON', fred_id='IR' ),
  list( label='house_starts', name='2. Housing starts', haver='HPT@USECON', fred_id='HOUST' ),
  # NB: despite the "1fam" in the label (inherited from the original code, kept
  # unchanged since l.q references this exact string), validation confirmed this is
  # actually MULTIFAMILY (5+ units) starts -- matches HOUST5F almost exactly
  # (344 vs archived 368 for 2024-01), not HOUST1F (single-family, ~1016 for the
  # same month, a ~3x mismatch). The original code's own `name` field already said
  # "total multifamily", contradicting its `label`; the data confirms `name` was
  # right. See docs/data_sources.md.
  list( label='house_starts_1fam', name='3. Housing starts, multifamily (5+ units)', haver='HSTM@USECON', fred_id='HOUST5F' ),
  list( label='inv_chg', name='8. Change in Real Inventories', haver='VNHM@USNA',
       bea_table='U50706BM', bea_dataset='NIUnderlyingDetail', bea_freq='M',
       bea_line=1, bea_description='Nonfarm industries' ),
  list( label='inv_manuf', name='7. Manufacturing inventories', haver='NMI@USECON',
       bea_table='U001B', bea_dataset='NIUnderlyingDetail', bea_freq='M',
       bea_line=2, bea_description='Manufacturing industries', splice=TRUE ),
  list( label='nonres_construction', name='1. Non-residential construction spending',
       haver='CPTNR@USECON', fred_id='TLNRESCONS' )
  )

l.derived.series <- list(
  list( label='inv_manuf_chg', name='9. Change in manufacturing inventories', fn='pc.diff(inv_manuf)' ),
  list( label='export_volume', name='5. Census Exports Volumes', fn='exports_value/export_prices' ),
  list( label='import_volume', name='6. Census Imports Volumes', fn='imports_value/import_prices' )
  )
l.x <- list(
  cons_g=list( label='cons_g', name='Goods Consumption', from='pce_exp_goods' ),
  cons_s=list( label='cons_s', name='Services Consumption', from='pce_exp_servs' ) #,
  # cons=list( label='cons', name='Consumption', from='pce_exp' ),
  # cons_dg=list( label='cons_dg', name='Consumption', from='pce_exp_dur' ),
  # cons_ndg=list( label='cons_ndg', name='Consumption', from='pce_exp_nondur' ),
  )
    # Known data
l.q <- list(
  # cons_g='real.retail.sales', # c('pce_exp_dur'), #  'real.retail.sales', #'constant1', # c('ip','real.retail.sales'), #
  # cons_s=c('cons_housing', 'ism.servs'), #'cons_housing',
  x=c('export_volume'), # 'export_prices'), # 'polb_outbound_loaded'),
  m=c('import_volume'), # 'import_prices'), # 'polb_inbound_loaded'),
  nrfi=c( 'nonres_construction'), # 'nondef_cap_goods', 'ip', 'oil_price'),
  rfi=c( 'house_starts_1fam', 'house_starts'), # 'res_building_employees'),
  # gov=c( 'pub_wages', 'pub_construction', 'fed_outlays'), #, 'fed_constr', 'fed_receipts', 'fed_emp'
  invent=c( 'inv_manuf_chg', 'inv_chg')
  )
    # Indicators
# if(!l.options$make.gdp.err) l.q[['err']] <- NULL
