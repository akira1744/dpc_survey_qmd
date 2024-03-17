################################################################################

# date: 2023-11-28

# author: nuajbec

################################################################################
# read packages
pacman::p_load(
  lubridate, # 日付操作 
  janitor, # 集計表 tabyl(),adorn_totals()
  openxlsx,
  readxl,
  writexl, # Excel出力 write_xlsx()
  ggrepel, # ggplotのテキスト位置自動調整 geom_text_repel() 
  ggridges, # リッジプロット 変数の分布をグループ別に出力 geom_density()
  gghighlight, # ggplotでにhighlight機能を追加 gghighlight()
  patchwork, # ggplotのグラフを結合 
  scales, # ggplotのメモリをパーセントにするのに使用 percent
  export, # パワポ出力 graph2ppt() table2ppt()
  RSQLite, # SQLite操作
  tidyverse, # packageセット(ggplot2 tibble tidyr readr purrr dplyr stringr forcats)
  tidylog # tidyverseにlog出力機能追加
)

# read my function
# source('/home/rstudio/srv/function/index.R')

# clear global variables
# clean_vars()

# shiny用のdbを作成
con_shiny = dbConnect(SQLite(), 'data/shiny.sqlite', synchronous="off")

# table一覧を確認
dbListTables(con_shiny) %>% print()

# con_shinyのtableをすべて読み込みして、同名の変数に格納
dbListTables(con_shiny) %>% 
  map(~dbReadTable(con_shiny, .x)) %>% 
  set_names(dbListTables(con_shiny)) %>% 
  list2env(envir = .GlobalEnv)


# shiny用のdbを作成
con_qmd = dbConnect(SQLite(), 'data/dpc_survey_zenkoku_qmd.sqlite', synchronous="off")

# table一覧を確認
dbListTables(con_qmd) %>% print()

##################################################################################

mst_latest

##################################################################################

# 医療機関マスタ(医療機関ごとの最新年度のデータ)
mst_latest <- tibble(mst_latest) %>% print()

# 対象範囲の病院を抽出
mst_latest <- mst_latest %>% 
  # filter(pref=='埼玉県') %>% 
  select(mstno,hpname,pref,iryo,city,bed,dpcbed) %>% 
  glimpse()

mst_latest %>% 
  count(iryo,sort=T)

# 病院名の整形
rmname <- c(
  '社会医療法人財団石心会　',
  '社会医療法人至仁会　',
  '独立行政法人　国立病院機構　',
  '医療生協さいたま生活協同組合',
  '医療法人社団和風会',
  '医療法人社団清心会',
  '医療法人尚寿会',
  '医療法人一晃会',
  '医療法人靖和会',
  '社会医療法人東明会',
  '医療法人和会',
  '医療法人啓仁会',
  '医療法人社団秀栄会',
  '医療法人積仁会',
  '医療法人慈桜会',
  '医療法人積仁会',
  '医療法人社団東光会',
  '医療法人社団医凰会',
  '一般社団法人　巨樹の会　',
  '医療法人仁栄会',
  '医療法人明晴会',
  '医療法人社団桜友会',
  '医療法人永仁会',
  '医療法人清和会',
  '医療法人社団明雄会',
  '医療法人　豊仁会　',
  '社会医療法人社団尚篤会　',
  '社会医療法人刀仁会　',
  '社会医療法人刀仁会　',
  '公益社団法人東松山医師会',
  '医療法人社団　哺育会　',
  '医療法人直心会',
  '医療法人蒼龍会',
  '医療法人愛和会',
  '医療法人刀圭会',
  '医療法人社団シャローム',
  '医療法人真正会',
  '医療法人瑞穂会',
  '社会福祉法人埼玉医療福祉会',
  '医療法人眞美会',
  '医療法人若葉会　',
  '医療法人若葉会',
  '医療法人聖心会',
  '医療法人財団献心会',
  '医療法人社団　誠弘会',
  '医療法人　慈正会　',
  '医療法人ヘブロン会',
  '医療法人三慶会',
  '医療法人明浩会',
  '医療法人社団　協友会　',
  '医療法人社団双愛会',
  '医療法人社団幸正会',
  '医療法人社団弘象会',
  '医療法人社団松弘会',
  '医療法人聖仁会',
  '医療法人財団新生会',
  '社会医療法人さいたま市民医療センター　',
  '医療法人　三愛会　',
  '医療法人　社団協友会　',
  '医療法人　道心会　',
  '医療法人光仁会',
  '医療法人埼友会',
  '医療法人社団　協友会',
  '医療法人社団　愛友会',
  '医療法人社団全仁会　',
  '医療法人社団全仁会',
  '医療法人社団協友会　',
  '医療法人社団大和会',
  '医療法人社団嬉泉会',
  '医療法人社団庄和会',
  '医療法人社団明日佳',
  '医療法人社団春明会',
  '医療法人純心会',
  '医療法人親和会',
  '医療法人財団　健和会',
  '医療法人財団　明理会',
  '医療法人財団明理会',
  '医療法人財団東京勤労者医療会',
  '医療法人三和会',
  '医療法人　徳洲会　',
  '医療法人双鳳会',
  '医療法人社団彩優会',
  '医療法人社団弘人会',
  '医療法人社団愛友会　',
  '医療法人社団清幸会',
  '医療法人社団生彩会　',
  '医療法人社団白桜会',
  '社会医療法人　壮幸会',
  '社会医療法人社団　埼玉巨樹の会　',
  '医療法人俊仁会',
  '社会医療法人　ジャパンメディカルアライアンス　',
  '医療法人同愛会',
  '医療法人啓清会',
  '医療法人好文会',
  '医療法人柏成会',
  '医療法人社団優慈会',
  '医療法人社団心志会',
  '医療法人社団紘智会',
  '医療法人福島会',
  '医療法人葵',
  '医療法人藤和会',
  '医療法人壽鶴会',
  '医療法人山柳会',
  '医療法人昭仁会',
  '医療法人橘会',
  '医療法人泰一会',
  '医療法人社団　明芳会',
  '医療法人社団　武蔵野会　',
  '医療法人社団武蔵野会',
  '医療法人社団草芳会',
  '医療法人誠壽会',
  '医療法人社団富家会',
  '医療法人社団坪田会',
  '社会医療法人社団　',
  '医療法人　健仁会　',
  '医療法人　新青会　',
  '医療法人あかつき会',
  '医療法人三誠会',
  '医療法人刀水会',
  '医療法人厚和会',
  '医療法人寿康会',
  '医療法人慈公会',
  '医療法人社団　東光会',
  '医療法人社団康幸会',
  '医療法人社団桐和会',
  '医療法人財団啓明会',
  '社会医療法人社団大成会',
  '医療法人社団博翔会',
  '医療法人社団浩蓉会',
  '医療法人藤仁会',
  '医療法人財団ヘリオス会',
  '医療法人財団聖蹟会',
  '医療法人彩清会',
  '医療法人徳洲会　',
  '医療法人花仁会',
  '医療法人桂水会',
  '医療法人社団寿会',
  '医療法人社団聖心会',
  '独立行政法人地域医療機能推進機構　',
  '社会福祉法人　恩賜財団　済生会支部　埼玉県　済生会　',
  '独立行政法人地域医療機能推進機構　',
  '社会福祉法人　恩賜財団　済生会支部埼玉県済生会',
  '学校法人北里研究所　',
  '独立行政法人国立病院機構',
  
  '医療法人社団[　]*',
  '医療法人[　]*'
  )

rmpattern <- str_c(rmname,collapse = '|') %>% print()

# 医療法人名削除
mst_latest <- mst_latest %>% 
  mutate(hpname2 = if_else(pref=='埼玉県',str_replace(hpname,rmpattern,''),hpname),.after=hpname) %>%
  print()



mst_latest %>% 
  filter(hpname==hpname2) %>% 
  select(hpname,hpname2) %>% 
  data.frame()

mst_latest %>% 
  filter(hpname!=hpname2) %>% 
  select(hpname,hpname2) %>% 
  data.frame()

# 全角スペースをreplace
mst_latest <- mst_latest %>% 
  mutate(hpname2 = str_replace_all(hpname2,'　','')) %>%
  print()

# 名前かぶり確認
mst_latest %>% 
  group_by(hpname2) %>% 
  filter(n()>1) %>% 
  select(hpname2,hpname)

# 元の列名に上書き
mst_latest <- mst_latest %>% 
  select(-hpname) %>% 
  rename(hpname = hpname2) %>% 
  print()

rmname <- c(
  '社会福祉法人恩賜財団済生会支部神奈川県済生会',
  '国立研究開発法人',
  '公益財団法人大原記念倉敷中央医療機構',
  '社会医療法人財団石心会',
  '一般財団法人厚生会',
  '医療法人徳洲会',
  '一般財団法人厚生会',
  '医療法人社団誠馨会',
  '公益財団法人榊原記念財団附属',
  '日本赤十字社東京都支部',
  '国家公務員共済組合連合会',
  '三重県厚生農業協同組合連合会',
  '神奈川県厚生農業協同組合連合会',
  '社会福祉法人恩賜財団済生会支部千葉県済生会千葉県済生会',
  '公益社団法人地域医療振興協会',
  '独立行政法人労働者健康安全機構',
  '医療法人社団明芳会',
  '広島県厚生農業協同組合連合会',
  '宗教法人在日本南プレスビテリアンミッション',
  '地域独立行政法人広島市立病院機構',
  '地域独立行政法人福岡市立病院機構',
  '独立行政法人国立病院機構',
  '社会福祉法人聖隷福祉事業団総合病院',
  '社会医療法人渡邊高記念会',
  '医療法人社団のう救会',
  '社会医療法人社団健脳会',
  '学校法人東京女子医科大学',
  '社会医療法人医翔会',
  '医療法人社団晃友会',
  '愛知県厚生農業協同組合連合会',
  '医療法人社団聖仁会',
  '独立行政法人国立病院機構',
  '地方独立行政法人大阪府立病院機構',
  '社会医療法人ジャパンメディカルアライアンス',
  '地方独立行政法人東京都立病院機構',
  '一般財団法人',
  '独立行政法人',
  '公益財団法人',
  '公立大学法人'
) 
rmpattern <- str_c(rmname,collapse = '|') %>% print()
# 
mst_latest <- mst_latest %>% 
  mutate(hpname = str_replace(hpname,rmpattern,''))
  
# qmdに書き出し
dbWriteTable(con_qmd, 'mst_latest', mst_latest, overwrite = T, row.names = F)

unique_mstno <- unique(mst_latest$mstno)

# 緯度経度のデータ
mst_latest_latlon <- tibble(mst_latest_latlon) %>% 
  filter(mstno %in% unique_mstno) %>%
  select(mstno,lat,long) %>% 
  print()

# qmd用DBに書き出し
dbWriteTable(con_qmd, 'mst_latest_latlon', mst_latest_latlon, overwrite = T, row.names = F)

# 施設別集計
agg_long_hp <- agg_long_hp %>% 
  tibble() %>%
  filter(mstno %in% unique_mstno) %>%
  print()

# qmd用DBに書き出し
dbWriteTable(con_qmd, 'agg_long_hp', agg_long_hp, overwrite = T, row.names = F)

# 施設別mdc2別集計
agg_long_mdc2 <- agg_long_mdc2 %>% 
  tibble() %>%
  filter(mstno %in% unique_mstno) %>%
  print()
  
# qmd用DBに書き出し
dbWriteTable(con_qmd, 'agg_long_mdc2', agg_long_mdc2, overwrite = T, row.names = F)

# 施設別mdc6別集計
agg_long_mdc6 <- agg_long_mdc6 %>% 
  tibble() %>%
  filter(mstno %in% unique_mstno) %>%
  print()

# qmd用DBに書き出し
dbWriteTable(con_qmd, 'agg_long_mdc6', agg_long_mdc6, overwrite = T, row.names = F)


# 施設別mdc10別集計
agg_long_mdc10 <- agg_long_mdc10 %>% 
  tibble() %>%
  filter(mstno %in% unique_mstno) %>%
  print()


# qmd用DBに書き出し
dbWriteTable(con_qmd, 'agg_long_mdc10', agg_long_mdc10, overwrite = T, row.names = F)

# mdc2のマスタ
dpcmst_mdc2 <- tibble(dpcmst_mdc2) %>% print()

# qmd用DBに書き出し
dbWriteTable(con_qmd, 'dpcmst_mdc2', dpcmst_mdc2, overwrite = T, row.names = F)

# mdc6のマスタ
dpcmst_mdc6 <- tibble(dpcmst_mdc6) %>% print()

# qmd用DBに書き出し
dbWriteTable(con_qmd, 'dpcmst_mdc6', dpcmst_mdc6, overwrite = T, row.names = F)

# mdc6とicdの対応表
dpc_icd <- tibble(dpc_icd) %>% print()

# qmd用DBに書き出し
dbWriteTable(con_qmd, 'dpc_icd', dpc_icd, overwrite = T, row.names = F)

# opeのマスタ
dpcmst_ope <- tibble(dpcmst_ope) %>% print()

# qmd用DBに書き出し
dbWriteTable(con_qmd, 'dpcmst_ope', dpcmst_ope, overwrite = T, row.names = F)

# knameのマスタ
dpcmst_kname <- tibble(dpcmst_kname) %>% print()

# qmd用DBに書き出し
dbWriteTable(con_qmd, 'dpcmst_kname', dpcmst_kname, overwrite = T, row.names = F)

##############################################################################

create_agg_mdc2_all <- function(agg_long_mdc2,dpcmst_mdc2,mst_latest){
  
  df <- agg_long_mdc2
  
  mst_latest <- mst_latest %>% 
    select(mstno,病院=hpname,県=pref,医療圏=iryo,市=city,病床=dpcbed)
  
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
    select(県,医療圏,市,病院,everything()) %>%
    rename(医療圏内シェア = pref_iryo_share) %>% 
    rename(救急医療=qqiryo) %>%
    rename(予定外 = yoteigai)
  
  # ランキングの行を作成
  agg <- agg %>% 
    group_by(mdc2cd) %>%
    mutate(No = row_number(desc(`2021`)),.before=県) %>%
    ungroup()
  
  # 合計の計算
  agg_sum <- agg %>% 
    select(-No,-県,-医療圏,-市,-病院,-医療圏内シェア,-救急医療,-予定外) %>% 
    group_by(mdc2cd) %>% 
    summarise(across(everything(), ~sum(.x,na.rm=T))) %>% 
    mutate(No=NA_integer_,県=NA_character_, 医療圏=NA_character_,市='',病院='合計',
           医療圏内シェア=NA_real_,救急医療=NA_real_,予定外=NA_real_) 
  
  # 合計を結合
  agg_all <- rbind(agg, agg_sum) 
  
  agg_all
  
  # dpcmst_mdc2を結合
  agg_all <- agg_all %>% 
    left_join(dpcmst_mdc2,by='mdc2cd') %>%
    relocate(mdc2,.after=mdc2cd) 
  
  return(agg_all)
  
}

agg_mdc2_all <- create_agg_mdc2_all(agg_long_mdc2,dpcmst_mdc2,mst_latest) %>% print()

dbWriteTable(con_qmd, 'agg_mdc2_all', agg_mdc2_all, overwrite = T, row.names = F)

################################################################################

create_agg_hp_all <- function(agg_long_hp,mst_latest){
  
  # 全国の実績を集計
  zenkoku_long <- agg_long_hp %>% 
    group_by(fy) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    print()
  
  # 全国の実績をwideにする
  zenkoku_wide <- zenkoku_long %>% 
    arrange(fy) %>% 
    pivot_wider(names_from = fy, values_from = value) %>%
    print()
  
  # 結合用にmstnoを穴埋め
  zenkoku_wide <- zenkoku_wide %>% 
    mutate(mstno='0')
  
  # 各病院の実績をwideにする
  df_wide <- agg_long_hp %>% 
    select(mstno,fy,value) %>%
    arrange(fy) %>% 
    pivot_wider(names_from = fy, values_from = value) %>%
    print()
  
  # 県の実績と各病院の実績を結合
  df_wide <- rbind(df_wide,zenkoku_wide) %>% 
    print()
  
  # サブの実績は2021年の実績を取得
  df_sub <- agg_long_hp %>% 
    filter(fy==max(fy)) %>% 
    select(mstno,pref_iryo_share,stay,qq,yotegai,qqiryo,value) %>% 
    mutate(rank = row_number(desc(value))) %>%
    select(-value) %>% 
    print()
  
  # 結合
  df <- df_wide %>% 
    left_join(df_sub,by='mstno') %>% 
    print()
  
  # 病院マスタを結合
  df <- df %>% 
    left_join(mst_latest,by='mstno') %>% 
    rename(病院=hpname,県=pref,医療圏=iryo,市=city,病床=dpcbed) %>% 
    print()
  
  
  # 合計列について、マスタ列を整理
  df <- df %>% 
    mutate(医療圏 = if_else(mstno=='0','全国',医療圏)) %>% 
    mutate(病院 = if_else(mstno=='0', '全国合計',病院)) %>% 
    mutate(mstno= if_else(mstno=='0',NA_character_,mstno)) %>%
    print()
  
  df <- df %>% 
    arrange(rank) %>% 
    select(mstno,県,医療圏,市,病院,`2018`,`2019`,`2020`,`2021`,
           全国ランク= rank,
           医療圏内シェア=pref_iryo_share,
           平均日数 = stay,
           救急医療 = qqiryo,
           予定外 = yotegai,
           病床
           )
  
  return(df)

}

agg_hp_all <- create_agg_hp_all(agg_long_hp,mst_latest) %>% print()

dbWriteTable(con_qmd, 'agg_hp_all', agg_hp_all, overwrite = T, row.names = F)

