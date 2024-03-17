pacman::p_load(gt,knitr,DBI,RSQLite,ggrepel,tidyverse,tidylog)

map <- readRDS('data/map_iryo.rds')

base<- ggplot(map)+
  # 塗りなし 線をgrayに
  geom_sf(fill=NA,color='gray50',size=0.1)+
  coord_sf(datum=NA)+ # 軸なし
  geom_text(aes(x=x,y=y,label=iryo),size=2.4,alpha=0.3)+
  theme_void()

map <- readRDS('data/saitama_tokyo_map.rds')

map

base<- ggplot(map)+
  # 塗りなし 線をgrayに
  geom_sf(fill=NA,color='gray50',size=0.1)+
  coord_sf(datum=NA)+ # 軸なし
  geom_text(aes(x=x,y=y,label=地区),size=2.4,alpha=0.3)+
  theme_void()

base
