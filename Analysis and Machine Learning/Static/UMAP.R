
#######################
## Enlarging Dim-Reductions by UMAP
#######################

## read in data
embd_data <- readr::read_csv("all_embeddings_df.csv")
sim <- readr::read_csv("similarity_topics_df.csv")
d <- readr::read_csv("C:/Users/leona/Documents/Data_Science_Project/Shiny_Server/Shiny_App_tries/Backup/20220221/similarity_topics_df_backup.csv")


## clean
embd_data <- embd_data %>%
  select(-X1)

## apply UMAP
umap_model <- umap::umap(d = select(embd_data, -c("combination", "genre")),
                   n_neighbors = 15,
                   min_dist = 0.1,
                   metric = "manhattan",
                   n_epochs = 200)

## collect new dimensions
umap_red <- data.frame(UMAP1 = umap_model$layout[,1],
                       UMAP2 = umap_model$layout[,2],
                       genre = embd_data$genre)

## filter for plausible values
umap_red <- umap_red %>%
  filter(UMAP1 <= 10 & UMAP2 <= 10) 

## check visually
ggplot(umap_red, aes(x = UMAP1, y = UMAP2,
                     col = genre)) + 
  geom_point()


## append to existing DF
d <- d %>%
  select(combination, s1, s2, s3, index_0, index_1, index_2, index_3)

sim_new <- sim %>%
  select(combination, genre, first_appearance, p1, p2, tsne_1, tsne_2, iso_1, iso_2)

sim_new <- dplyr::left_join(sim, d)


## save new file
write.csv(sim_new, "similarity_topics_df.csv", row.names = FALSE)

