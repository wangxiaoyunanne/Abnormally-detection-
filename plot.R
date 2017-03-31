
g1= graph.data.frame (page_liking_edge)
plot(g1,edge.arrow.size=.1, vertex.size=5, 
     vertex.label.cex=0.5,vertex.label=NA)


#pages100
page_info_feature <- read.csv("~/Downloads/page_info_feature.csv")

#count_post 100 by 3 orig
count_post <- read.csv("~/Downloads/count_post.txt", header=FALSE)
count_post$ratio = count_post$V2/count_post$V3
count_post[which (is.na(count_post$ratio)),]$ratio = 0.0
#copt the ratio of page_info table
page_info_feature $ratio = 1
  
for( id in 1: dim(page_info_feature)[1]){    
  for (j in 1 :dim(count_post)[1]){
    if(page_info_feature$page_id [id] == count_post$V1[j])
      page_info_feature $ratio[id] = count_post$ratio[j] 
      
    
  }
     
}

# draw graphs

plot(page_info_feature$target, page_info_feature$ratio)
ggplot(page_info_feature, aes(target, ratio) )+ 
  geom_point() +
  geom_smooth(method=lm)

ggplot(page_info_feature, aes(nontarget, ratio) )+ 
  geom_point() 

ggplot(page_info_feature, aes(m_cmt, ratio) )+ 
  geom_point() 

ggplot(page_info_feature, aes(n_cmt, ratio) )+ 
  geom_point() 

ggplot(page_info_feature, aes(indegree, ratio) )+ 
  geom_point() 

ggplot(page_info_feature, aes(outdegree, ratio) )+ 
  geom_point() +
  geom_boxplot()

ggplot(page_info_feature, aes(degree, ratio) )+ 
  geom_point() +
  geom_boxplot()

ggplot(page_info_feature, aes(eccentricity, ratio) )+ 
  geom_point() 

ggplot(page_info_feature, aes(closnesscentrality, ratio) )+ 
  geom_point() 

ggplot(page_info_feature, aes(harmonicclosnesscentrality, ratio) )+ 
  geom_point() 

ggplot(page_info_feature, aes(betweenesscentrality, ratio) )+ 
  geom_point()

ggplot(page_info_feature, aes(eigencentrality, ratio) )+ 
  geom_point()

ggplot(page_info_feature, aes(pageranks, ratio) )+ 
  geom_point()

ggplot(page_info_feature, aes(clustering, ratio) )+ 
  geom_point()

post_feature_12 <- read.csv("~/Downloads/post_feature_12.txt", header=FALSE)
names(post_feature_12) = c('label', 'post_id', 'page_id', 'spanning_time', 
                           'n_comments', 'post_type', 'n_people', 'n_people_post', 
                           'n_people_cmts','T1','T2','T3','T4','T5','T6','T7',
                           'T8','T9','T10','T11','T12','T13','T14','T15','T16',
                           'T17','T18','T19','T20','T21','T22','T23','T24')

post_feature_12 $ratio = 1

for( id in 1: dim(post_feature_12)[1]){    
  for (j in 1 :dim(count_post)[1]){
    if(post_feature_12$page_id [id] == count_post$V1[j])
      post_feature_12 $ratio[id] = count_post$ratio[j] 
    
    
  }
  
}

ggplot(post_feature_12, aes(factor(page_id), spanning_time))+
  geom_boxplot(aes(fill = ratio), outlier.shape= NA)+
  coord_flip()+
  scale_y_log10()

ggplot(post_feature_12, aes(factor(page_id), log10(n_comments+1)))+
  geom_boxplot(aes(fill = ratio))+
  coord_flip()

ggplot(post_feature_12, aes(factor(page_id), log10(n_people+1)))+
  geom_boxplot(aes(fill = ratio))+
  coord_flip()

ggplot(post_feature_12, aes(factor(page_id), log10(n_people_post+1)))+
  geom_boxplot(aes(fill = ratio))+
  coord_flip()

ggplot(post_feature_12, aes(factor(page_id), log10(n_people_cmts+1)))+
  geom_boxplot(aes(fill = ratio))+
  coord_flip()

ggplot(post_feature_12, aes(factor(page_id), T1))+
  geom_boxplot(aes(fill = ratio),outlier.shape= NA)+
  coord_flip()

ggplot(post_feature_12, aes(factor(page_id), T2))+
  geom_boxplot(aes(fill = ratio))+
  coord_flip()

ggplot(post_feature_12, aes(factor(page_id), T3))+
  geom_boxplot(aes(fill = ratio))+
  coord_flip()
ggplot(post_feature_12, aes(factor(page_id), T4))+
  geom_boxplot(aes(fill = ratio))+
  coord_flip()
ggplot(post_feature_12, aes(factor(page_id), T5))+
  geom_boxplot(aes(fill = ratio))+
  coord_flip()
ggplot(post_feature_12, aes(factor(page_id), T10))+
  geom_boxplot(aes(fill = ratio))+
  coord_flip()
ggplot(post_feature_12, aes(factor(page_id), T20))+
  geom_boxplot(aes(fill = ratio))+
  coord_flip()
ggplot(post_feature_12, aes(factor(page_id), T21))+
  geom_boxplot(aes(fill = ratio))+
  coord_flip()
ggplot(post_feature_12, aes(factor(page_id), T22))+
  geom_boxplot(aes(fill = ratio))+
  coord_flip()
ggplot(post_feature_12, aes(factor(page_id), T23))+
  geom_boxplot(aes(fill = ratio))+
  coord_flip()
ggplot(post_feature_12, aes(factor(page_id), T24))+
  geom_boxplot(aes(fill = ratio))+
  coord_flip()
