# Data_Science_Project

Welcome to the GitHub Repo of our Data Science Project. 
The latter was developed by Leonard Berger and Nico Schwarzer in the context of the M.Sc. Data Science in Business and Economics at Tübingen University. The goal of this Project is to set up an end-to-end Analytics Workflow. We have scrutenized the development of the Lyrics of popular Songs using NLP techniques. The final result of this Project is a Web Application which can be accessed here: http://193.196.53.66:3838/DS_Project/
For more info, consult the information buttons on the afore-mentioned App.


## Categorization of Files

All files used for the project are to be found in this Repository. To introduce a logical structure and enable a better overview, all files have been added to folders. The following folders thusly exist:

1. Functions to be sourced 
2. Data Acquisition
   * static (Once conducted)
   * dynamic (Used for weekly updating)
3. Analysis and Machine Learning
   * Static_Initial_Analyses (All Analysis steps not used in updating loop)
   * Dynamic_Updating_Analyses (All Analysis steps used in updating loop)
4. Shiny App Files (Server, ui, global)

Also note that when coding the Project, all files and data were saved in one folder. Hence, the folder structure laid out above is not reflected when sourcing in other files.

## Information on Re-Coding

With the help of the files above and the corresponding data, it is principally to re-code almost (all) steps (with the exception of those relying on deprecated methods). The datasets we used are vast and therefore not present here - contact us for a link to a Dropbox!

However, as some many steps need not be repeated for obtaining the final results (such as e.g. the Acquisition of the Historical Data or Model Training) and would take multiple days and sometimes rely on deprecated methods, we highly recommend to simply save the files in the 'Shiny App Files' folder together with the correspnding data and then run the App - this should work easily!

Again, consider that no hierarchical folder structure is needed when using the code on your machine. 

Should you be interested in more information or in the data used (especially that used for the App), feel free to contact us.
*leonard.berger@student.uni-tuebingen.de 
*nico.schwarzer@student.uni-tuebingen.de 




