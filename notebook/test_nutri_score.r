food_data %>%
  select(
    Foodstuffs,
    Gram,
    `Energy kcal`,
    `Protein(g)`,
    `Saturated fatty acids(g)`,
    `Mono+disaccharides(g)`,
    `Fibers(g)`,
    `Sodium(mg)` ) %>%
  na.omit() %>%
  #head(100) %>%
  group_by(Foodstuffs) %>%
  filter(row_number()==1) %>%
  melt(id.vars=c("Foodstuffs","Gram")) %>%
  mutate(value = (value/Gram) * 100) %>%
  select(-Gram) %>%
  dcast(Foodstuffs ~ variable) %>%
  na.omit() -> food_test


food_test = merge(food_test,food_fruit, by="Foodstuffs")

#write.csv2(food_test, file="food_test.csv")

nutriscore(cal=483,
           protein=8,
           fat=22,

           carbohydrate=60,
           fibre=4,
           sodium=590) %>% nutriscore_qual

apply(food_test, 1, function(x) {nutriscore(


  cal=x[2]%>%as.numeric,
  protein=x[3]%>%as.numeric,
  fat=x[4]%>%as.numeric,
  carbohydrate=x[5]%>%as.numeric,
  fibre=x[6]%>%as.numeric,
  sodium=x[7]%>%as.numeric,
  fruit = x[8] %>% as.numeric




)}) %>% nutriscore_qual -> res


food_data %>%
  select(Identity,Foodstuffs, Meal) %>%
  merge(cbind(Foodstuffs=food_test[,1],res), by="Foodstuffs") %>%
  ggplot() + geom_bar(aes(x=Identity, fill=res), position="fill", width=1) +
  facet_wrap(~Meal) +
  scale_fill_brewer(type="div")


food_data %>%
  select(Identity,Foodstuffs, Meal) %>%
  merge(cbind(Foodstuffs=food_test[,1],res), by="Foodstuffs") %>%
  ggplot() + geom_bar(aes(x=Meal, fill=res), position="fill", width=1) +
  #facet_wrap(~Meal) +
  scale_fill_brewer(type="div")
