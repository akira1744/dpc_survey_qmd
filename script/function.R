pacman::p_load(gt,knitr,DBI,RSQLite,tidyverse,tidylog)

################################################################################

get_mst_latest <- function(con){
  mst_latest<- DBI::dbReadTable(con, 'mst_latest') %>% 
    tibble() %>% 
    select(mstno,病院=hpname,医療圏=iryo,市=city,病床=dpcbed)
  return(mst_latest)
}

################################################################################

get_mst_latest_latlon <- function(con){
  
  mst_latest_latlon<- DBI::dbReadTable(con, 'mst_latest_latlon') %>% tibble() %>% 
  return(mst_latest)
}

################################################################################

get_agg_long_mdc6 <- function(con,set_mdc2cd){
  agg_long_mdc6 <- DBI::dbReadTable(con, 'agg_long_mdc6') %>% 
    tibble() %>% 
    mutate(mdc2cd=substr(mdc6cd,1,2)) %>%
    filter(mdc2cd==set_mdc2cd) %>%
    select(-mdc2cd)
  return(agg_long_mdc6)
}

################################################################################


get_agg_long_mdc10 <- function(con,set_mdc2cd){
  agg_long_mdc10 <- DBI::dbReadTable(con, 'agg_long_mdc10') %>% 
    tibble() %>% 
    mutate(mdc2cd=substr(mdc6cd,1,2)) %>%
    filter(mdc2cd==set_mdc2cd) %>%
    select(-mdc2cd) %>% 
  return(agg_long_mdc10)
}

################################################################################


get_dpcmst_mdc6 <- function(con,set_mdc2cd,set_dpcfy){
  dpcmst_mdc6 <- DBI::dbReadTable(con, 'dpcmst_mdc6') %>% 
    tibble() %>% 
    filter(mdc2cd==set_mdc2cd) %>%
    filter(dpcfy==set_dpcfy) %>% 
    select(-mdc2cd) %>% 
    select(-dpcfy)
  return(dpcmst_mdc6)
}

################################################################################


get_dpcmst_ope <- function(con,set_mdc2cd,set_dpcfy){
  dpcmst_ope <- DBI::dbReadTable(con, 'dpcmst_ope') %>% 
    tibble() %>% 
    mutate(mdc2cd=substr(mdc6cd,1,2)) %>% 
    filter(mdc2cd==set_mdc2cd) %>%
    filter(dpcfy==set_dpcfy) %>% 
    select(-mdc2cd) %>% 
    select(-dpcfy)
  return(dpcmst_ope)
}

################################################################################

create_hp_gt <- function(con,set_hp,table_row){
  
  hp <- DBI::dbReadTable(con, 'agg_hp_all') %>% 
    tibble() 
  
  hp <- hp %>% 
    filter(県内ランク <= table_row |  病院 %in% c('埼玉県合計',set_hp)) 
  
  hp <- hp %>% 
    rename(`2018`=X2018) %>% 
    rename(`2019`=X2019) %>%
    rename(`2020`=X2020) %>%
    rename(`2021`=X2021) %>% 
    select(-mstno) %>% 
    select(県内ランク,everything()) %>%
    print()
  
  
  hp <- hp %>% 
    arrange(県内ランク) %>% 
    mutate(平均日数 = round(平均日数,1)) %>%
    print()
  
  myrow <- hp %>% 
    filter(病院==set_hp) %>% 
    pull(県内ランク)
  
  hp <- hp %>% 
    rename(Rank = 県内ランク) %>% 
    rename(圏内シェア = 医療圏内シェア)

  hp_gt <- hp %>% 
    gt() %>% 
    fmt_percent(columns = c('圏内シェア','救急医療','予定外'),decimals = 1) %>% 
    sub_missing(missing_text = "") %>% 
    tab_style(
      style = list(
        cell_fill(color = "gray90"),
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        rows = myrow
      )
    ) %>% 
    fmt_number(columns=c(`2018`:`2021`),decimals=0) 
  
  return(hp_gt)
}


################################################################################

create_agg_all <- function(con,mst_latest){
  
  # 病院ごと、mdc2ごとの集計を作成する関数
  
  df <- DBI::dbReadTable(con, 'agg_long_mdc2') %>% tibble() %>% 
    arrange(desc(fy),desc(value)) 
    
  dpcmst_mdc2 <- DBI::dbReadTable(con, 'dpcmst_mdc2') %>% tibble()

  # mdc2ごと集計をする関数
  df_wide <- df %>% 
    select(mstno,mdc2cd,fy,value) %>% 
    arrange(fy) %>% 
    pivot_wider(names_from = fy, values_from = value) 
  
  # その他の集計を最終年度の分だけ持ってくる
  df_sub <- df %>% 
    filter(fy==max(fy)) %>% 
    select(mstno,mdc2cd,pref_iryo_share,qqiryo,yoteigai) 
  
  # データを組み合わせて整形
  agg <- df_wide %>% 
    left_join(df_sub,by=c('mstno','mdc2cd')) %>% 
    inner_join(mst_latest,by='mstno') %>%
    select(-mstno) %>% 
    select(医療圏,市,病院,everything()) %>%
    rename(医療圏内シェア = pref_iryo_share) %>% 
    rename(救急医療=qqiryo) %>%
    rename(予定外 = yoteigai)
  
  # ランキングの行を作成
  agg <- agg %>% 
    group_by(mdc2cd) %>%
    mutate(No = row_number(desc(`2021`)),.before=医療圏) %>%
    ungroup()
  
  # 合計の計算
  agg_sum <- agg %>% 
    select(-No,-医療圏,-市,-病院,-医療圏内シェア,-救急医療,-予定外) %>% 
    group_by(mdc2cd) %>% 
    summarise(across(everything(), ~sum(.x,na.rm=T))) %>% 
    mutate(No=NA_integer_, 医療圏=set_pref,市='',病院='合計',
           医療圏内シェア=NA_real_,救急医療=NA_real_,予定外=NA_real_) 
  
  # 合計を結合
  agg_all <- rbind(agg, agg_sum) 
  
  # dpcmst_mdc2を結合
  agg_all <- agg_all %>% 
    left_join(dpcmst_mdc2,by='mdc2cd') %>%
    relocate(mdc2,.after=mdc2cd) 
  
  return(agg_all)
  
}

################################################################################

create_agg_myhp_mdc2_all <- function(con,set_hp){

  agg_mdc2_all <- DBI::dbReadTable(con, 'agg_mdc2_all')
  
  agg_mdc2_all <- agg_mdc2_all %>%
    tibble() %>% 
    rename(`2018`=X2018) %>% 
    rename(`2019`=X2019) %>%
    rename(`2020`=X2020) %>%
    rename(`2021`=X2021) 
  
  df <- agg_mdc2_all %>% 
    filter(病院==set_hp) %>% 
    select(-医療圏,-市,-病院,-病床) %>% 
    arrange(desc(`2021`)) %>% 
    rename(県内ランク=No) %>% 
    relocate(県内ランク,.after=`2021`)
  
  df <- df %>% 
    rename(疾患分類= mdc2) %>% 
    rename(MDC2 = mdc2cd)
  
  df_nrow <- nrow(df)
  
  df_other_nrow =df_nrow-14
  
  df_head <- df %>% head(14)
  
  df_tail <- df %>% tail(df_other_nrow)
  
  df_tail_sum <- df_tail %>% 
    summarise(across(c(`2018`:`2021`), ~sum(.x,na.rm=T))) %>%
    print()
  
  df_tail_sum <- df_tail_sum %>% 
    mutate(
      MDC2='**'
      ,疾患分類='その他疾患分類'
      ,県内ランク=NA_real_
      ,医療圏内シェア=NA_real_
      ,救急医療=NA_real_
      ,予定外=NA_real_
    )
  
  df_tail_sum <- df_tail_sum %>% 
    select("MDC2", "疾患分類", "2018", "2019", "2020", "2021", "県内ランク", 
           "医療圏内シェア", "救急医療", "予定外")

  hp <- DBI::dbReadTable(con, 'agg_hp_all') %>%
    filter(病院 %in% c(set_hp)) %>%
    tibble()

  hp <- hp %>%
    rename(`2018`=X2018) %>%
    rename(`2019`=X2019) %>%
    rename(`2020`=X2020) %>%
    rename(`2021`=X2021) %>%
    select(-mstno,-病床,-平均日数,-医療圏,-市) %>%
    rename(疾患分類 = 病院) %>%
    mutate(MDC2 = '')

  agg <- rbind(df_head,df_tail_sum,hp)
  # 
  return(agg)
  
}

###############################################################################


create_myhp_mdc2_graph <- function(agg_myhp_mdc2_all){
    
    df <- agg_myhp_mdc2_all %>% 
      filter(MDC2!='')
    
    df <- df %>% 
      rename(患者数=`2021`) %>% 
      select(疾患分類,患者数,医療圏内シェア) %>% 
      filter(!is.na(患者数))
    

    # グラフの作成 x軸は医療圏内シェア,y軸は患者数,
    # 点の大きさは患者数 凡例不要
    # 色は医療圏内シェア率, 凡例不要
    # mdc6をtext_repelで表示
    graph <- ggplot(df)+
      aes(x = 医療圏内シェア,y = 患者数)+
      geom_point(aes(size=患者数),color='gray50',alpha=.7)+
      geom_text_repel(aes(label = 疾患分類),size = 3.5,color='gray30')+
      geom_hline(yintercept = 0)+
      geom_vline(xintercept = 0)+
      labs(x = "医療圏内シェア",y = "2021年度患者数")+
      scale_x_continuous(labels = scales::percent,breaks = seq(0,1,.05))+
      scale_y_continuous(breaks = seq(0,10000,200))+
      scale_size_continuous(range = c(7, 14),guide='none')+
      theme_bw()
    
    graph
    
    return(graph)
    
  }

###############################################################################

create_agg_myhp_mdc2_all_gt <- function(agg_myhp_mdc2_all,set_mdc2cd){
  
  myrow <- agg_myhp_mdc2_all %>% 
    rowid_to_column('row') %>% 
    filter(MDC2==set_mdc2cd) %>% 
    pull(row)
  
  agg_myhp_mdc2_all %>% 
    gt() %>% 
    fmt_percent(columns = c('医療圏内シェア','救急医療','予定外'),decimals = 1) %>% 
    sub_missing(missing_text = "") %>% 
    tab_style(
      style = list(
        cell_fill(color = "gray90"),
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        rows = myrow
      )
    ) %>% 
    fmt_number(columns=c(`2018`:`2021`),decimals=0) 
}



################################################################################

get_agg_mdc2_all <-function(set_mdc2cd){
  
  agg_mdc2_all <- DBI::dbReadTable(con, 'agg_mdc2_all')

  # 対象のmdc2で絞り込んで、mdc2の列を消して、ランキングで並び替えする関数
    
  agg_mdc2_all <- agg_mdc2_all %>% 
    tibble() %>% 
    filter(mdc2cd==set_mdc2cd) %>% 
    select(-mdc2cd,-mdc2) %>% 
    arrange(No)
  
  agg_mdc2_all <- agg_mdc2_all %>% 
    rename(`2018`=X2018) %>% 
    rename(`2019`=X2019) %>%
    rename(`2020`=X2020) %>%
    rename(`2021`=X2021) 
  
  return(agg_mdc2_all)
}

################################################################################

create_mdc2_gt <- function(agg_mdc2_all,set_hp,table_row){
  
  # gtを作成する関数
  
  # TOP10と当院分を取得
  agg_mdc2_top <- agg_mdc2_all %>% 
    filter(No<=table_row | 病院 %in% c(set_hp,'合計'))
  
  # 病院列がset_hpの行がが何行目かを取得
  myrow <- agg_mdc2_top %>% 
    rowid_to_column('row') %>%
    filter(病院==set_hp) %>% 
    pull(row) 
  
  agg_mdc2_top <- agg_mdc2_top %>% 
    rename(Rank = No)
  
  # 医療圏内シェアと救急医療と予定外列をパーセントに変更
  agg_mdc2_gt <- agg_mdc2_top %>% 
    gt() %>% 
    fmt_percent(columns = c('医療圏内シェア','救急医療','予定外'),decimals = 1) %>% 
    tab_style(
      style = list(
        cell_fill(color = "gray90"),
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        rows = myrow
      )
    ) %>% 
    sub_missing(missing_text = "") %>% 
    fmt_number(columns=c(`2018`:`2021`),decimals=0) 
  
  return(agg_mdc2_gt)
  
}

################################################################################

create_map_mdc2 <- function(agg_mdc2_all, set_hp, mst_latest, mst_latest_latlon){

  df <- agg_mdc2_all %>% 
    filter(No<=table_row | 病院==set_hp) %>% 
    select(病院,value = `2021`) %>% 
    left_join(select(mst_latest,病院,mstno),by='病院') %>% 
    inner_join(mst_latest_latlon,by='mstno') 
  
  map <- readRDS('data/map_iryo.rds')
  
  base<- ggplot(map)+
    # 塗りなし 線をgrayに
    geom_sf(fill=NA,color='gray50',size=0.1)+
    coord_sf(datum=NA)+ # 軸なし
    geom_text(aes(x=x,y=y,label=iryo),size=2.4,alpha=0.3)+
    theme_void()
  
  map_mdc2 <- base+
    geom_point(data=df,aes(x=long,y=lat,size=value,color=value),alpha=0.4)+
    geom_text_repel(data=df,
                    aes(x=long,y=lat,label=病院),
                    size=2.8,
                    color='gray30',
                    # direction = 'y' 
                    )+
    # scale_size_continuous(guide='none')+
    scale_size_continuous(range = c(3, 15),guide='none')+
    scale_color_viridis_c(option = "viridis")+
    # 凡例を患者数にする
    labs(size='患者数',color='患者数')

  
  return(map_mdc2)
  
}


################################################################################

create_myhp_mdc6_graph <- function(agg_mdc6_all,set_hp){
  
  df <- agg_mdc6_all %>% 
    filter(病院==set_hp) 
  
  df <- df %>% 
    rename(患者数=`2021`) %>% 
    select(mdc6,患者数,医療圏内シェア) %>% 
    filter(!is.na(患者数))
  

  # グラフの作成 x軸は医療圏内シェア,y軸は患者数,
  # 点の大きさは患者数 凡例不要
  # 色は医療圏内シェア率, 凡例不要
  # mdc6をtext_repelで表示
  graph <- ggplot(df)+
    aes(x = 医療圏内シェア,y = 患者数)+
    geom_point(aes(size=患者数),color='gray50',alpha=.7)+
    geom_text_repel(aes(label = mdc6),size = 3.5,color='gray30')+
    geom_hline(yintercept = 0)+
    geom_vline(xintercept = 0)+
    labs(x = "医療圏内シェア",y = "2021年度患者数")+
    scale_x_continuous(labels = scales::percent)+
    scale_size_continuous(range = c(7, 14),guide='none')+
    theme_bw()

  return(graph)
  
}

################################################################################

create_myhp_mdc6_gt <- function(agg_mdc6_all,set_hp,table_row){
  
  # 当院の最新年のmdc6の件数とランキングを一覧で見れるgtを作成する
  df <- agg_mdc6_all %>% 
    filter(病院==set_hp) %>% 
    filter(!is.na(`2021`)) 

  df <- df %>% 
    select(-医療圏,-市,-病院,-病床) %>% 
    head(table_row)

  df <- df %>% 
    rename(県内ランク=No) %>% 
    relocate(県内ランク,.before=医療圏内シェア) 
  
  df <- df %>% 
    mutate(平均日数 = round(平均日数,1))
  
  df <- df %>% 
    rename(疾患名=mdc6) %>% 
    rename(MDC6 = mdc6cd)
  
  # gtの作成
  df_gt <- df %>% 
    gt() %>% 
    fmt_percent(columns = c('医療圏内シェア'),decimals = 1) %>% 
    sub_missing(missing_text = "") %>% 
    fmt_number(columns=c(`2018`:`2021`),decimals=0) 
  
  return(df_gt)
}


################################################################################

create_agg_mdc6 <- function(agg_long_mdc6,table_row){

  # mdc6の集計
  agg_wide_mdc6 <- agg_long_mdc6 %>%
    select(mstno,mdc6cd,fy,value) %>%
    arrange(fy) %>%
    pivot_wider(names_from = fy, values_from = value)

  agg_wide_mdc6

  # その他の集計を最終年度の分だけ持ってくる
  agg_long_mdc6_sub <- agg_long_mdc6 %>%
    filter(fy==max(fy)) %>%
    select(mstno,mdc6cd,pref_iryo_share,stay)

  agg_long_mdc6_sub

  # データを組み合わせて整形
  agg_mdc6 <- agg_wide_mdc6 %>%
    left_join(agg_long_mdc6_sub,by=c('mstno','mdc6cd')) %>%
    inner_join(mst_latest,by='mstno') %>%
    select(-mstno) %>%
    select(mdc6cd,医療圏,市,病院,everything()) %>%
    rename(医療圏内シェア = pref_iryo_share) %>%
    rename(平均日数 = stay)

  agg_mdc6

  # Noの列を作成
  agg_mdc6 <- agg_mdc6 %>%
    group_by(mdc6cd) %>%
    arrange(desc(`2021`)) %>%
    mutate(No = row_number(),.before=mdc6cd) %>%
    ungroup()

  return(agg_mdc6)

}

################################################################################

create_agg_long_mdc6_sum <- function(agg_long_mdc6,max_nend,set_pref){
  
  # mdc6cdごとの実績を集計するための関数
  
  # 埼玉の平均在院を計算するためにsum_stayの列を作る
  df <- agg_long_mdc6 %>% 
    mutate(sum_stay = round(stay * value,0))
  
  # df
  
  # 集計
  agg_tmp <- df %>% 
    group_by(fy,mdc6cd) %>% 
    summarise(
      value = sum(value,na.rm=T),
      sum_stay = sum(sum_stay,.na.rm=T),
      stay = sum_stay / value
    ) %>% 
    ungroup() %>% 
    select(-sum_stay) 
  
  # agg_tmp
  
  # 平均在院日数は直近年度だけ取得
  agg_stay <- agg_tmp %>% 
    filter(fy==as.character(max_nend)) %>% 
    select(-fy,-value)
  
  # agg_stay
  
  # fyでvalueを横に展開
  agg_wide <- agg_tmp %>% 
    select(-stay) %>% 
    arrange(fy) %>% 
    pivot_wider(names_from = fy, values_from = value)
  
  # agg_wide
  
  # 集計を統合
  agg <- agg_wide %>% 
    left_join(agg_stay,by=c('mdc6cd'))
  
  # agg
  
  # 出力用に列名を整形
  out <- agg %>% 
    mutate(No=NA_integer_) %>%
    mutate(医療圏=set_pref) %>% 
    mutate(市='') %>% 
    mutate(病院='合計') %>% 
    mutate(医療圏内シェア=NA_real_) %>%
    rename(平均日数=stay) %>% 
    mutate(病床 = NA_real_) %>%
    select(No,mdc6cd,医療圏,市,病院,`2018`,`2019`,`2020`,`2021`,医療圏内シェア,平均日数,病床) 
  
  # out
  
  return(out)
  
}

################################################################################

create_agg_mdc6_all <- function(agg_mdc6,agg_mdc6_sum,dpcmst_mdc6){
  
  # mdc6の個別集計と合計行を縦に結合して、mdcマスタをjoinする関数
  
  # 合計をrbind
  df <- rbind(agg_mdc6,agg_mdc6_sum)
  
  # マスタの結合
  df <- df %>% 
    left_join(dpcmst_mdc6,by='mdc6cd') %>%
    relocate(mdc6,.after=mdc6cd)

  return (df)
  
}

################################################################################

create_top_mdc6 <- function(agg_mdc6_all,set_hp,table_row){
  
  # 2021年のmdc6ごとの件数を降順で並び替えて取得
  
  df <- agg_mdc6_all %>% 
    filter(病院==set_hp)
  
  # df
  
  top_mdc6 <- df %>% 
    arrange(desc(`2021`)) %>% 
    head(table_row) %>% 
    select(mdc6cd,mdc6) %>% 
    rowid_to_column('No') 
  
  # top_mdc6
  
  return(top_mdc6)
}

################################################################################

create_mdc6_title <- function(top_mdc6,sickno){
  sickcd <- top_mdc6[[sickno,'mdc6cd']]
  sickname <- top_mdc6[[sickno,'mdc6']]
  mdc6_title<- str_c(sickcd,':',sickname)
  return(mdc6_title)
}

################################################################################

create_mdc6_gt <- function(agg_mdc6_all,top_mdc6,sickno){
  
  sickcd <- top_mdc6[[sickno,'mdc6cd']]
  
  # 対象疾患のデータに絞り込み
  agg_mdc6_target <- agg_mdc6_all %>% 
    filter(mdc6cd==sickcd) 
  
  # top10と自院と合計を抽出
  agg_mdc6_target <- agg_mdc6_target %>% 
    filter(No<=table_row | 病院 %in% c(set_hp,'合計')) 
  
  # 疾患の列を削除
  agg_mdc6_target <- agg_mdc6_target %>% 
    select(-mdc6cd,-mdc6) 
  
  # 平均在院日数を小数点第一位まで表示
  agg_mdc6_target <- agg_mdc6_target %>% 
    mutate(平均日数 = round(平均日数,1)) 
  
  agg_mdc6_target <- agg_mdc6_target %>% 
    rename(Rank = No)
  
  # 病院列がset_hpの行がが何行目かを取得
  myrow <- agg_mdc6_target %>% 
    rowid_to_column('row') %>%
    filter(病院==set_hp) %>% 
    pull(row) 
  
  # gtの作成
  agg_mdc6_target_gt <- agg_mdc6_target %>% 
    gt() %>% 
    fmt_percent(columns = c('医療圏内シェア'),decimals = 1) %>% 
    tab_style(
      style = list(
        cell_fill(color = "gray90"),
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        rows = myrow
      )
    ) %>% 
    fmt_number(columns=c(`2018`:`2021`),decimals=0) 
  
  agg_mdc6_target_gt <- agg_mdc6_target_gt %>%
    sub_missing(missing_text = "")

  return(agg_mdc6_target_gt)
  
}

################################################################################

create_agg_mdc10 <- function(agg_long_mdc10,table_row){
  
  # mdc10の集計用関数
  
  # mdc10ごとの件数を集計
  agg_wide <- agg_long_mdc10 %>%
    select(mstno,mdc6cd,opecd,fy,value) %>%
    arrange(fy) %>%
    pivot_wider(names_from = fy, values_from = value)
  
  # agg_wide
  
  # その他の集計を最終年度の分だけ持ってくる
  agg_sub <- agg_long_mdc10 %>%
    filter(fy==max(fy)) %>%
    select(mstno,mdc6cd,opecd,pref_iryo_share,stay)
  
  # agg_sub
  
  # データを組み合わせて整形
  agg <- agg_wide %>%
    left_join(agg_sub,by=c('mstno','mdc6cd','opecd')) %>%
    inner_join(mst_latest,by='mstno') %>%
    select(-mstno) %>%
    select(mdc6cd,opecd,医療圏,市,病院,everything()) %>%
    rename(医療圏内シェア = pref_iryo_share) %>%
    rename(平均日数 = stay)
  
  # agg
  
  # Noの列を作成
  agg <- agg %>%
    group_by(mdc6cd,opecd) %>%
    arrange(desc(`2021`)) %>%
    mutate(No = row_number(),.before=mdc6cd) %>%
    ungroup()
  
  # agg
  
  return(agg)
  
}

################################################################################

create_agg_mdc10_sum <- function(agg_long_mdc10,max_nend,set_pref){
  
  # mdc6cdとopecd事の実績を集計するための関数
  
  # 埼玉の平均在院を計算するためにsum_stayの列を作る
  df <- agg_long_mdc10 %>% 
    mutate(sum_stay = round(stay * value,0))
  
  df
  
  # 集計
  agg_tmp <- df %>% 
    group_by(fy,mdc6cd,opecd) %>% 
    summarise(
      value = sum(value,na.rm=T),
      sum_stay = sum(sum_stay,.na.rm=T),
      stay = sum_stay / value
    ) %>% 
    ungroup() %>% 
    select(-sum_stay) 
  
  agg_tmp
  
  # 平均在院日数は直近年度だけ取得
  agg_stay <- agg_tmp %>% 
    filter(fy==as.character(max_nend)) %>% 
    select(-fy,-value)

  agg_stay
  
  # fyでvalueを横に展開
  agg_wide <- agg_tmp %>% 
    select(-stay) %>% 
    arrange(fy) %>% 
    pivot_wider(names_from = fy, values_from = value)
  
  agg_wide
  
  # 集計を統合
  agg <- agg_wide %>% 
    left_join(agg_stay,by=c('mdc6cd','opecd'))

  # 出力用に列名を整形
  out <- agg %>% 
    mutate(No=NA_integer_) %>%
    mutate(医療圏=set_pref) %>% 
    mutate(市='') %>% 
    mutate(病院='合計') %>% 
    mutate(医療圏内シェア=NA_real_) %>%
    rename(平均日数=stay) %>% 
    mutate(病床 = NA_real_) %>%
    select(No,mdc6cd,opecd,医療圏,市,病院,`2018`,`2019`,`2020`,`2021`,医療圏内シェア,平均日数,病床) 

  return(out)
  
}

################################################################################

create_agg_mdc10_all <- function(agg_mdc10,agg_mdc10_sum,dpcmst_mdc6,dpcmst_ope){
  
  # mdc6の個別集計と合計行を縦に結合して、mdcマスタをjoinする関数
  
  # 合計をrbind
  df <- rbind(agg_mdc10,agg_mdc10_sum)
  
  # mdc6マスタの結合
  df <- df %>% 
    left_join(dpcmst_mdc6,by='mdc6cd') %>%
    relocate(mdc6,.after=mdc6cd)
  
  # opemstの結合
  df <- df %>% 
    left_join(dpcmst_ope,by=c('mdc6cd','opecd')) %>%
    relocate(ope,.after=opecd) 

  return (df)
  
}

################################################################################

create_sickno_mdc10 <- function(agg_mdc10_all,top_mdc6,sickno){
  
  sickno_mdc6cd <- top_mdc6[[sickno,'mdc6cd']]
  
  # sicknoを受け取ったら、それに対応するmdc6とopeを返す関数
  sickno_mdc10 <- agg_mdc10_all %>% 
    filter(mdc6cd==sickno_mdc6cd,病院=='合計') %>% 
    mutate(sickno=sickno) %>% 
    select(sickno,mdc6cd,mdc6,opecd,ope,病院,`2021`) %>% 
    arrange(desc(`2021`)) 

  return(sickno_mdc10)
  
}

################################################################################

create_mdc10_title <- function(sickno_mdc10,set_opecd){
  
  df <- sickno_mdc10 %>% 
    filter(opecd==set_opecd) 
  
  # df
  
  str_mdc6 <- str_c(df$mdc6cd,':',df$mdc6)
  # str_mdc6
  
  str_ope <- str_c(set_opecd,':',df$ope)
  # str_ope
  
  mdc10_title <- str_c(str_mdc6,'<br>',str_ope)
  # mdc10_title
  
  return(mdc10_title)
}

################################################################################

create_mdc10_gt <- function(agg_mdc10_all, sickno_mdc10, set_hp, set_opecd,table_row){
  
  # ヘッダ行が2行になるので、その分1行減らす
  table_row =  table_row -1 
  
  # sicknoとopecdを受け取って、mdc10のgtを返す関数
  
  choice <- sickno_mdc10 %>% 
    filter(opecd==set_opecd) %>%
    select(mdc6cd,opecd) 
  
  choice
  
  df <- agg_mdc10_all %>% 
    inner_join(choice,by=c('mdc6cd','opecd'))

  df <- df %>% 
    filter(No<=table_row | 病院 %in% c(set_hp,'合計'))
    
  # 疾患の列を削除
  df <- df %>% 
    select(-mdc6cd,-mdc6,-opecd, -ope)
  
  # 平均在院日数を小数点第一位まで表示
  df <- df %>%
    mutate(平均日数 = round(平均日数,1))

  # 病院列がset_hpの行がが何行目かを取得
  myrow <- df %>%
    rowid_to_column('row') %>%
    filter(病院==set_hp) %>%
    pull(row)
  
  df <- df %>% 
    rename(Rank = No)

  # gtの作成
  df_gt <- df %>%
    gt() %>%
    fmt_percent(columns = c('医療圏内シェア'),decimals = 1) %>%
    tab_style(
      style = list(
        cell_fill(color = "gray90"),
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        rows = myrow
      )
    ) %>% 
    sub_missing(missing_text = "") %>% 
    fmt_number(columns=c(`2018`:`2021`),decimals=0) 
  
  df_gt
  
  return(df_gt)
  
}
