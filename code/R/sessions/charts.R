##########################################################################################
#
# charts.R
#
# Makes charts for the monthly GDP series
#
# Philip Barrett, Washington DC
# First version: 05feb2025
#
##########################################################################################


#### 0. Preliminaries ####
df.state.names <- as.data.frame(do.call(rbind, l.quarterly.data$components ) ) %>% 
  select( variable=label, name ) %>% 
  mutate_all(unlist) %>%
  rbind( as.data.frame( l.quarterly.data$target ) %>% select( variable=label, name ) ) %>%
  as_tibble()
df.indicator.names <- as.data.frame(do.call(rbind, l.monthly.data ) ) %>%
  select( variable=label, name ) %>%
  mutate_all(unlist) %>%
  rbind( as.data.frame(do.call(rbind, l.derived.series ) ) %>%
               select( variable=label, name ) %>%
               mutate_all(unlist) ) %>%
  as_tibble
plot.yrs <- list( covid=2019:2021, gfc=2006:2010, nice=1997:2004, volkler=1979:1983, 
                  early70s=1969:1976, late50s=1957:1961, all_nber=1950:2025 )
rec.start.dates <- c( '1953-07-01', '1957-08-01', '1960-04-01', '1969-12-01', '1973-11-01', 
                     '1980-01-01', '1981-07-01', '1990-07-01', '2001-03-01', 
                     '2007-12-01', '2020-02-01') %>% as.Date
rec.end.dates <- c( '1954-05-01', '1958-04-01', '1961-02-01', '1970-11-01', '1975-03-01', 
                   '1980-07-01', '1982-11-01', '1991-03-01', '2001-11-01', 
                   '2009-06-01', '2020-04-01' ) %>% as.Date
df.recessions <- data.frame( start=rec.start.dates, end=rec.end.dates )
cons.yr.1 <- 1959

#### 1. Plot the data ####
df.y.plot <- l.df.hats$df.y.hat %>% left_join(df.state.names, by='variable')
ggplot( df.y.plot, aes(x=date) ) + 
  geom_line(aes(y=value)) + 
  geom_line(aes(y=trend), lty=2) + 
  theme_minimal() + 
  facet_wrap(~name, scales = 'free_y') + 
  geom_hline(aes(yintercept=0)) +
  ylab('Billions chained 2017 dollars') +
  xlab('')
ggsave( paste0(l.options$save.dir, 'graphs/nipa_qly.pdf' ), height=4, width=8 )

ggplot( df.y.plot, aes(x=date, y=hat) ) + 
  geom_line() + theme_minimal() + 
  facet_wrap(~name, scales = 'free_y') + 
  geom_hline(aes(yintercept=0)) +
  ylab('Percent departure from trend') +
  xlab('')
ggsave( paste0(l.options$save.dir, 'graphs/nipa_qly_hat.pdf' ), height=8, width=12 )

df.q.plot <-  l.df.hats$df.q.hat %>%
  filter(variable!='constant1') %>%
  left_join( df.indicator.names, by='variable' ) %>%
  mutate( trend.plot=ifelse(!is.na(value), trend, NA) )
ggplot( df.q.plot, aes(x=date, y=value) ) + 
  geom_line() + theme_minimal() + 
  geom_line(aes(y=trend.plot), lty=2) + 
  facet_wrap(~name, scales = 'free_y') + 
  ylab('') + xlab('') +
  geom_hline(aes(yintercept=0))
ggsave( paste0(l.options$save.dir, 'graphs/mly_indicators.pdf' ), height=6, width=12 )

ggplot( df.q.plot, aes(x=date, y=100*hat) ) + 
  geom_line() + theme_minimal() + 
  facet_wrap(~name, scales = 'free_y') + 
  geom_hline(aes(yintercept=0)) +
  ylab('Percent departure from trend') +
  xlab('')
ggsave( paste0(l.options$save.dir, 'graphs/mly_indicators_hat.pdf' ), height=6, width=12 )

if(l.options$obs.resids){
  ggplot( df.q.plot, aes(x=date, y=100*hat.resid) ) + 
    geom_line() + theme_minimal() + 
    facet_wrap(~name, scales = 'free_y') + 
    geom_hline(aes(yintercept=0)) +
    ylab('Percent departure from trend') +
    xlab('')
  ggsave( paste0(l.options$save.dir, 'graphs/mly_indicators_resid.pdf' ), height=6, width=12 )
}


#### 2. Compare to the quarterly NIPA data ####
ggplot( df.out %>% filter(!is.na(nipa)), aes( x=date, y=ks.nipa.err.pc100 ) ) +
  geom_hline(aes(yintercept=0)) + 
  geom_line() +
  theme_minimal() +
  facet_wrap(~var.name, scale='free_y') +
  labs(y='Percent Error on NIPA ') +
  xlab('') # + ylim(c(-10,10))
ggsave( paste0(l.options$save.dir, 'graphs/nipa_err.pdf' ), height=8, width=12 )

ggplot( df.out %>% filter(!is.na(nipa)) %>% 
          select( date, t, variable, 'Quarterly NIPA data'=nipa, 'Quarterly avg. of Monthly estimates'=ks.nipa, 
                  var.name) %>%
          gather( series, value, -date, -t, -variable, -var.name ), 
        aes( x=date, y=value, color=series, lty=series ) ) +
  geom_hline(aes(yintercept=0)) + 
  geom_line() +
  theme_minimal() +
  facet_wrap(~var.name, scale='free_y') +
  theme( legend.position='bottom') +
  scale_color_manual(values=c('blue', 'black')) +
  labs(color='', lty='') +
  ylab('Billions chained 2017 dollars') +
  xlab('')
ggsave( paste0(l.options$save.dir, 'graphs/nipa_compare.pdf' ), height=6, width=12 )

n.yrs <- 3
last.n.yrs <- make_date( df.out$date %>% year %>% max - 1 - n.yrs, 1, 1 )

ggplot( df.out %>% filter(date >= last.n.yrs) %>% 
          group_by(variable) %>%
          mutate( nipa.lin=lead(approx(date, nipa, date, "linear")$y,1),
                  y.ks.ann=3*y.ks) %>%
          select( date, t, variable, 'Quarterly NIPA data'=nipa.lin, 
                  'Monthly estimates at quarterly rate'=y.ks.ann, 
                  var.name) %>%
          gather( series, value, -date, -t, -variable, -var.name ), 
        aes( x=date, y=value ) ) +
  # geom_hline(aes(yintercept=0)) + 
  geom_ribbon(data=df.out %>% filter(date >= last.n.yrs) %>% 
              mutate(upper=3*(y.ks+2*y.ks.se),
                     lower=3*(y.ks-2*y.ks.se)) %>%
              select(date, upper, lower, value=y.ks, var.name ),
              aes( ymax=upper, ymin=lower), fill='blue', alpha=.2) +
  geom_point( data=df.out %>% filter(date >= last.n.yrs) %>% 
                select(date, value=nipa, var.name ) %>% 
                mutate(value=lead(value,1)), size=.8) +
  geom_line(aes(color=series, lty=series), lwd=.5) +
  theme_minimal() +
  facet_wrap(~var.name, scale='free_y') +
  theme( legend.position='bottom') +
  scale_color_manual(values=c('blue', 'black')) +
  labs(color='', lty='') +
  ylab('Billions chained 2017 dollars') +
  xlab('')
ggsave(paste0(l.options$save.dir, 'graphs/website.png' ), 
       width = 1100, height = 600, units = "px", dpi = 144)



#### 3. The output ####
ggplot( df.out, aes( x=date ) ) +
  geom_hline(aes(yintercept=0)) + 
  geom_line(aes(y=y.ks)) +
  geom_line(aes(y=y.ks+1.96*y.ks.se), lty=2) +
  geom_line(aes(y=y.ks-1.96*y.ks.se), lty=2) +
  theme_minimal() +
  facet_wrap(~var.name, scale='free_y') +
  labs( y='Billions chained 2017 dollars', x='',
        caption='Broken lines show 95 percent confidence interval given the estimated DGP.')
ggsave( paste0(l.options$save.dir, 'graphs/mly_ci_all.pdf' ), height=6, width=8 )

ggplot( df.out, aes( x=date ) ) +
  geom_hline(aes(yintercept=0)) + 
  geom_line(aes(y=y.ks)) +
  geom_line(aes(y=y.ks+1.96*y.ks.se.all), lty=2) +
  geom_line(aes(y=y.ks-1.96*y.ks.se.all), lty=2) +
  theme_minimal() +
  facet_wrap(~var.name, scale='free_y') +
  labs( y='Billions chained 2017 dollars', x='',
        caption='Broken lines show 95 percent confidence interval including estimation uncertainty.')
ggsave( paste0(l.options$save.dir, 'graphs/mly_ci_boot_all.pdf' ), height=8, width=12 )

ggplot( df.out %>% filter( variable !='invent', variable !='err' ), aes( x=date ) ) +
  geom_line(aes(y=log(y.ks))) +
  geom_line(aes(y=log(y.ks+1.96*y.ks.se)), lty=2) +
  geom_line(aes(y=log(y.ks-1.96*y.ks.se)), lty=2) +
  theme_minimal() +
  facet_wrap(~var.name, scale='free_y') + 
  labs( y='Log billions chained 2017 dollars', x='',
        caption='Broken lines show 95 percent confidence interval given the estimated DGP.')
ggsave( paste0(l.options$save.dir, 'graphs/mly_ci_all_log.pdf' ), height=8, width=12 )


#### 4. SE decomp ####
gg.se <- ggplot( l.df.filt$df.se.q %>% filter(!is.na(y.ks.se.pc100)), aes( x=date) ) +
  geom_hline(aes(yintercept=0)) + 
  geom_line(aes(y=abs(y.ks.se.pc100), color='Filter uncertainty', lty='Filter uncertainty') ) +
  geom_line(data=df.se.q %>% filter(!is.na(y.ks.se.pc100)), 
            aes(y=abs(y.ks.se.all.pc100), color='Filter + estimation uncertainty', 
                lty='Filter + estimation uncertainty') ) +
  theme_minimal() +
  facet_wrap(~var.name, scale='free_y') +
  labs( y='', x='', color = '', lty='') + 
  scale_color_manual(values = c( 'Filter uncertainty'='black', 
                                 'Filter + estimation uncertainty'='blue') ) +
  scale_linetype_manual(values = c( 'Filter uncertainty'=2, 
                                 'Filter + estimation uncertainty'=1) ) +
  theme(legend.position = 'bottom')
gg.se %>% print
ggsave( paste0(l.options$save.dir, 'graphs/qly_se.pdf' ), gg.se, height=8, width=12 )

ggplot( l.df.filt$df.se.q %>% filter(!is.na(y.ks.se.pc100), year(date)>=2000 ), aes( x=date) ) +
  geom_hline(aes(yintercept=0)) + 
  geom_line(aes(y=abs(y.ks.se.pc100)), lty=2) +
  geom_line(data=df.se.q %>% filter( year(date)>=2000), aes(y=abs(y.ks.se.all.pc100)), col='blue') +
  theme_minimal() +
  facet_wrap(~var.name, scale='free_y') +
  labs( y='', x='' )

ggplot( l.df.filt$df.se.q %>% filter(variable=='gdp'), mapping = aes(x=date) ) +
  geom_hline(aes(yintercept=0)) + 
  geom_bar( data=l.df.filt$df.se.q %>% filter(variable!='gdp'), stat='identity',
            aes( y=y.ks.se.pc100.gdp, group=var.name, fill=var.name ), 
            position = position_stack(reverse = TRUE) ) +
  geom_line( aes( y=y.ks.se.pc100.gdp ), lwd=.9 ) +
  theme_minimal() +
  # scale_fill_brewer(palette = 'Set1') +
  theme(legend.position = 'bottom') +
  labs(x='Date', y='Filter standard deviation, percent', fill='')
ggsave( paste0(l.options$save.dir, 'graphs/qly_se_decomp.pdf' ), height=8, width=12 )

# ggplot( df.se.q %>% filter(variable=='gdp', year(date) >= cons.yr.1), mapping = aes(x=date) ) +
#   geom_hline(aes(yintercept=0)) + 
#   geom_bar( data=df.se.q %>% filter(variable!='gdp', year(date) >= cons.yr.1), stat='identity',
#             aes( y=y.ks.se.pc100.gdp, group=var.name, fill=var.name ), 
#             position = position_stack(reverse = TRUE) ) +
#   geom_line( aes( y=y.ks.se.pc100.gdp ), lwd=.9 ) +
#   theme_minimal() +
#   # scale_fill_brewer(palette = 'Set1') +
#   theme(legend.position = 'bottom') +
#   labs(x='Date', y='Filter standard deviation, percent', fill='')
ggsave( paste0(l.options$save.dir, 'graphs/qly_se_decomp_cons.pdf' ), height=8, width=12 )


#### 4. The episodes of interest ####
for(i.episode in 1:length(plot.yrs)){
  episode.name <- names(plot.yrs)[i.episode]
  # ggplot( df.out %>% filter( year(date) %in% plot.yrs[[i.episode]] ), aes( x=date ) ) +
  #   geom_line(aes(y=y.ks)) +
  #   geom_line(aes(y=y.ks+1.96*y.ks.se), lty=2) +
  #   geom_line(aes(y=y.ks-1.96*y.ks.se), lty=2) +
  #   theme_minimal() +
  #   facet_wrap(~var.name, scale='free_y') +
  #   labs( y='Billions chained 2017 dollars', x='',
  #         caption='Broken lines show 95 percent confidence interval given the estimated DGP.')
  # ggsave( paste0(l.options$save.dir, 'graphs/mly_ci_', episode.name, '_filter.pdf' ), 
  #         height=5, width=8 )
  
  gg.nber <- ggplot( df.out %>% filter( year(date) %in% plot.yrs[[i.episode]] ) ) +
    geom_rect(data=df.recessions %>% filter( year(start) %in% plot.yrs[[i.episode]] ),
              aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf), alpha=0.2 ) +
    geom_line(aes(x=date, y=y.ks)) +
    geom_line(aes(x=date, y=y.ks+1.96*y.ks.se.all), lty=2) +
    geom_line(aes(x=date, y=y.ks-1.96*y.ks.se.all), lty=2) +
    theme_minimal() +
    facet_wrap(~var.name, scale='free_y') +
    labs( y='Billions chained 2017 dollars', x='',
          caption='Broken lines show 95 percent confidence interval including estimation uncertainty.')
  gg.nber %>% print()
  ggsave( paste0(l.options$save.dir, 'graphs/mly_ci_', episode.name, '_boot.pdf' ), 
          gg.nber, height=4, width=8 )
  
  
  gg.none <- ggplot( df.out %>% filter( year(date) %in% plot.yrs[[i.episode]] ) ) +
    geom_rect(data=df.recessions %>% filter( year(start) %in% plot.yrs[[i.episode]] ),
              aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf), alpha=0.2 ) +
    geom_line(aes(x=date, y=y.ks), lwd=.1) +
    geom_hline(aes(yintercept=0)) +
    theme_minimal() +
    facet_wrap(~var.name, scale='free_y') +
    labs( y='Billions chained 2017 dollars', x='')
  gg.none %>% print()
  ggsave( paste0(l.options$save.dir, 'graphs/mly_ci_', episode.name, '_none.pdf' ), 
          gg.none, height=4, width=8 )
  
  # ggplot( df.out %>% filter( year(date) %in% plot.yrs[[i.episode]] ), aes( x=date ) ) +
  #   geom_line(aes(y=y.ks)) +
  #   geom_line(aes(y=y.ks+1.96*y.ks.se), lty=2) +
  #   geom_line(aes(y=y.ks-1.96*y.ks.se), lty=2) +
  #   geom_line(aes(y=y.ks), col='red') +
  #   geom_line(aes(y=y.ks+1.96*y.ks.se.all), lty=2, col='red') +
  #   geom_line(aes(y=y.ks-1.96*y.ks.se.all), lty=2, col='red') +
  #   theme_minimal() +
  #   facet_wrap(~var.name, scale='free_y') +
  #   labs( y='Billions chained 2017 dollars', x='',
  #         caption='Broken lines show 95 percent confidence intervals, black for the Kalman Filter, Red for the bootstrap simulation.')
  # ggsave( paste0(l.options$save.dir, 'graphs/mly_ci_', episode.name, '_both.pdf' ), 
  #         height=5, width=8 )
  
}


#### 5. Statistcial plots ####
m.Q.plot <- l.state.est$Q %>%
  set_rownames(c(v.x, v.y)) %>%
  set_colnames(c(v.x, v.y))
m.cor.plot <- m.Q.plot / ( sqrt(diag(m.Q.plot)) %*% t(sqrt(diag(m.Q.plot))) )
# m.cor.plot[lower.tri(m.cor.plot)] <- NA

df.cor <- m.cor.plot %>% reshape2::melt(na.rm=T)
  # Explicit reshape2::melt (not the bare generic): data.table::melt no longer
  # auto-redirects matrix input to reshape2's method (now a hard error instead
  # of the old deprecation warning, since data.table is loaded and its melt
  # generic wins ahead of reshape2's in the search path).
df.cor.plot <- df.cor %>%
  left_join( df.names %>% select( Var1.name=var.name, Var1=variable) ) %>%
  left_join( df.names %>% select( Var2.name=var.name, Var2=variable) ) %>%
  mutate( Var1.name=as.factor(Var1.name),
          Var2.name=as.factor(Var2.name) ) %>%
  mutate(lvl.1 = as.numeric(Var1.name),
         lvl.2 = as.numeric(Var1.name |> fct_rev()),
         correlation = if_else(lvl.1 < lvl.2, value, as.numeric(NA)) )

gg.pearson <- 
  ggplot(data = df.cor.plot, aes(Var2.name, Var1.name, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", 
                       limit = c(-1.01,1.01), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  labs(x='',y='') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   hjust = 1))+
  coord_fixed()
# gg.pearson %>% print()
ggsave( paste0(l.options$save.dir, 'graphs/Q_correl.pdf' ), 
        gg.pearson, height=4, width=8 )

df.state.est <- l.state.est$check[c(1,3)] %>% 
  rbindlist() %>%
  left_join( df.names %>% rename(var=variable), by='var' ) %>%
  select( -diff.eft, -pc.diff100 ) %>%
  gather( type, value, -h, -var, -var.name ) %>%
  mutate( type.name=case_when(type=='qly.acv'~' Sample',
                              type=='mly.agg.acv.init'~'Unweighted GMM',
                              type=='mly.agg.acv.eft'~'Efficient GMM' ))

ggplot( df.state.est, aes(x=h, y=value, group=type.name, color=type.name, lty=type.name ) ) +
  geom_line(lwd=.9) +
  facet_wrap(~var.name, scale='free_y')+
  theme_minimal() +
  geom_hline(aes(yintercept=0)) +
  theme(legend.position = 'bottom' ) +
  labs(x='Horizon', y='', col='', lty='')
ggsave( paste0(l.options$save.dir, 'graphs/Q_acv.pdf' ), 
        height=8, width=12 )
