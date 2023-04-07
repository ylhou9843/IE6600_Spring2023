#distribution of vaccination Sites in US
# geo data
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

# calculate the vaccine in each state
vacci_num <- df %>%
  group_by(State) %>%
  summarise(VaccSite_num = n_distinct(Name,latitude,longitude))
df <- left_join(df,vacci_num,by="State")
df$VaccSite_num[is.na(df$VaccSite_num)] <- 0

# calculate the vaccine in each county
tocounty <- read_csv('www/data/ZIP-COUNTY-FIPS_2017-06.csv')
tocounty <- tocounty %>% select(ZIP, STATE, STCOUNTYFP)
tocounty <- tocounty %>% group_by(ZIP) %>% summarise(first(STCOUNTYFP), first(STATE))
tocounty <- rename(tocounty, zip = ZIP, State = `first(STATE)`, County = `first(STCOUNTYFP)`)
df <- merge(df, tocounty %>% select(zip, State, County), by = c("zip", 'State')) 

county_sum <- df %>%
  group_by(County) %>%
  summarise(VaccCt_sum = n_distinct(Name,latitude,longitude))
df <- left_join(df,county_sum,by="County")
df$VaccCt_sum[is.na(df$VaccCt_sum)] <- 0

US_distribution_chart <- function(df,usMap){
  vacci_num <- df %>%
    group_by(State) %>%
    summarise(VaccSite_num= n_distinct(Name,latitude,longitude))
  
  usMap <- left_join(usMap,vacci_num,by="State")
  usMap$VaccSite_num[is.na(usMap$VaccSite_num)] <- 0
  
  p <- ggplot(usMap, aes(x = long, y = lat, group = group,fill=VaccSite_num,
                         text = paste(State, "<br>", "Vaccination Sites number: ", VaccSite_num))) +
    geom_polygon()+
    theme_void()+
    theme(
      panel.background = element_rect(fill = "white", color="grey"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_blank()
    )+
    scale_fill_gradient(low = "#e9f6fb",high = "#0094d4",name="")
  
  ggplotly(p, tooltip = c("text"))
}

state_chart <- function(state, df, usMap){
  df <- df %>% filter(State==state)
  
  fig <- plot_ly(df)
  fig <- fig %>% add_trace(
    type="choroplethmapbox",
    geojson=usMap,
    locations=df$County,
    z=df$VaccCt_sum,
    colorscale="viridis",
    text = 'Vaccine',
    marker=list(line=list(
      width=0),
      opacity=0.5
    )
  )
  fig <- fig %>% layout(
    mapbox=list(
      style="carto-positron",
      zoom = 5,
      center=list(lon=mean(df$longitude), lat=mean(df$latitude)))
  )
}

chart_bar <- function(df,State,k){
  if (State==''){
    vacci_num <- df %>%
      group_by(State) %>%
      summarise(VaccSite_num= n_distinct(Name,latitude,longitude)) %>% arrange(desc(VaccSite_num))
    average_num = mean(vacci_num$VaccSite_num)
    vacci_num <- vacci_num %>% head(n=k) %>% arrange(VaccSite_num) %>% mutate(State_type = ifelse(VaccSite_num>=average_num,"above","below"))
    vacci_num$State <- factor(vacci_num$State,levels = vacci_num$State)
    ggplot(data=vacci_num,aes(x=VaccSite_num,y=State))+
      geom_bar(stat = "identity",aes(fill=State_type))+
      geom_label(aes(label=VaccSite_num),color="black")+
      scale_fill_brewer(palette = 'Set2')+
      geom_vline(xintercept = average_num,color="red")+
      labs(title = paste("Vaccination Sites Number in top",k,"US States"))+
      theme_void()+
      theme(
        legend.position = 'none',
        axis.text.y = element_text(size = 15,face = "bold"),
        plot.title = element_text(hjust = .5,size = 15,family = "American Typewriter",face="bold",color="orange")
      )
  }else{
    vacci_num <- df[df$State==State,] %>% group_by(City) %>% 
      summarise(VaccSite_num=n_distinct(Name,latitude,longitude)) %>%
      arrange(desc(VaccSite_num)) %>% head(n=k)
    
    
    vacci_num$City <- factor(vacci_num$City,levels = vacci_num$City)
    
    ggplot(data=vacci_num,aes(x=City,y=VaccSite_num))+
      geom_bar(stat = "identity", fill = 'lightblue')+
      geom_label(aes(label=VaccSite_num),color="red")+
      labs(title = paste("Vaccination Sites Number in top",k,State,"Cities"))+
      theme_void()+
      theme(
        plot.title = element_text(hjust = .5,size = 15,family = "American Typewriter",face="bold",color="orange"),
        axis.text.x = element_text(angle=45,vjust=.6)
      )
  }
  
}







pie_chart <- function(df,State,City){
  type_num <- df[df$State==State & df$City==City,] %>% group_by(searchable_name,in_stock) %>% summarise(site_num = n_distinct(Name,latitude,longitude),.groups = "keep")
  pie <- type_num %>% group_by(searchable_name) %>% summarise(x=sum(site_num)) %>% arrange(x)
  pie$prop <- pie$x / sum(pie$x)
  pie$label <- paste(pie$searchable_name,"(",scales::percent(pie$prop),")",sep="")
  for (i in seq(nrow(pie), 1)) {
    if (i == nrow(pie)) {
      pie$label_loc[i] = pie$prop[i] / 2
    }else{
      pie$label_loc[i] = sum(pie$prop[(i + 1):nrow(pie)]) + pie$prop[i] / 2
    }
  }
  ggplot(data = pie,aes("a",prop,fill=interaction(prop,label)))+
    geom_bar(stat="identity",width = 1.3)+
    geom_text(aes(1, as.numeric(label_loc), 
                  label = label),
              size =4, color = 'black')+
    scale_fill_brewer(palette = 'Set3')+
    coord_polar(theta = "y")+
    ggtitle(paste("Proportions of vaccination types in",State))+
    theme_void()+
    theme(legend.position = 'none',plot.title = element_text(size=15,hjust = 0.5,face="bold",family = "American Typewriter",color="tomato"))
}

