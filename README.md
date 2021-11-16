# Data_Science_Project


### To Do:
- In NER einlesen, Fokus: Themen aus Songs extrahieren      -> Leo
- Zuordnung der Songs + Artists zu Genre (wie und woher?)
- Datensätze erweitern (Kaggle: 1999-2019, Nicos Scraping: 2019-2021, Billboard package: 1960-2016)   -> Nico
1 Billboard scrapen, nur song name, artist, week, platzierung
2 Genres zu allen Liedern holen
3 Lyrics zu allen Liedern holen (über Genius package/API)


### Aufbau Shiny-App:

#### Reiter
> Sentiment Analysis, Happy/Sad-Score over time and Genre (Saisonalitäten erkennbar? Große Gesellschaftliche Ereignisse erkennbar?)

#### Reiter   -> Leo
> 3rd Pronouns analysis

#### Reiter   -> Leo 
> Topic Analysis, um welche Themen ging es wann? (siehe To Do Stichpunkt 1)

#### Reiter   -> Nico
> Lieder mit ähnlichen Lyrics/Topics (Stichword Unsupervised Classification)

#### Reiter 
> Länge/Komplexität over time and Genre

#### Reiter (under Discussion)    -> Leo
> Quiz auf Word Cloud Basis, gegeben die Wordcloud eines zufälligen Liedes und vier Antwortmöglichkeiten, welches Lied wird dargestellt?)




### Other

- Automatisierte Aktualisierung der Charts jede Woche (Stichpunkt Sys.sleep(7x24) -> neues Scraping) x
- Nochmal einlesen Shiny-App auf Uni-Server hosten x



### (Possible) Genre Mapping (to be found and possibly modified in unique_genres.xlsx):

-	Funk, boogie, edm, elekto etc. are classified as „dance“
-	Chanson, ESC and soul and adult standards are classified as „pop“
-	Any version of harder rock (punk, grunge etc.) is classified as „rock“
-	Classical music, instumental soundtracks are classified as  „instrumental“
-	Any version of Rap and house is classified as „rnb“
-	Folk music from around the world and children’s music is classified as „folk“

--> now, always 2 DFs are saved (and appended weekly)
1) df_all_billboard_all_weeks_with_genre_lyrics_NOT_CLEANED.csv (with original genres)
2) df_all_billboard_all_weeks_with_reduced_genre_lyrics_NOT_CLEANED.csv (with re-mapped genres)

Whenever the excel file is changed, one should execute the file "Genres_mapping". This will apply the new mapping and create a new 
df_all_billboard_all_weeks_with_reduced_genre_lyrics_NOT_CLEANED.csv file. The weekly updating still works and need not be modified!

