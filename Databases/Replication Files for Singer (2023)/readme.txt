Dynamics of Gubernatorial Approval: Evidence from a New Database
State Politics and Policy Quarterly
Matthew Singer
University of Connecticut
matthew.m.singer@uconn.edu

STRUCTURE OF THIS README FILE

The analysis in the paper has four parts. The first involves the construction of the quarterly and annual approval measures from the SEAD dataset. The second stage is to generate the descriptive data of the raw and latent data.  The third stage is the count model of where surveys are conducted and how it varies across and within states.  The final stage is the analysis of the quarterly data to look at gubernatorial honeymoons. Underlying it all is the collection of aggregate approval ratings in SEAD v1.0. After listing the software packages used, the data files and code are listed separately for each subset of the project. 


CITATION INFORMATION

Users of the SEAD data should cite not only (1) the version of the SEAD dataset that is being used and (2) this paper but also the original JAR dataset as it draws on Niemi et al.'s data and the SEAD codebook is based on the JAR codebook, using many of the same codes to ensure comparability with the data. Users should also reference Jennifer Jensen's work in maintaining the JAR. Full citation details are available in "Codebook SEAD governors v1.PDF", located in the folder "files and code for SEAD dataset v1". 


COMPUTING ENVIRONMENT:

Analyses were done in stata/SE 17 with a 64Bit machine. The stata files are stored as version 12 to allow for maximum usage. 
The latent approval variables for SEAD v1.0 are generated using R version 4.1.3 and in R Studio
The R code and state routines build on ones developed for the Executive Approval project by Greg Love; I take all responsibility for any errors I have introduced and appreciate his help. 


DOCUMENTATION, DATA FILES, AND CODE, BY PROJECT SECTION

1. Generate the Raw Data and Latent Approval Ratings, SEAD dataset

The following files are used:

Approval data for SEAD v1.xlsx - The datafile containing JAR data and my additions to it. It has two tabs. The first "All data" contains all the survey data that are available. This is what is used to generate the descriptive statistics and the count data. However, there are some states where the only way to extract a latent series is to restrict the analysis to a smaller set of years because the series don't overlap with each other. Specifically, if we only include surveys conducted in 2010 or later from Idaho and Hawaii, from 2009 or later in Louisiana, and 2015 or later of North Dakota we can get an estimate of approval or disapproval for these states. So there is a second tab "data for quarterly analysis" that should be used to generate the latent variables. 

Approval data for SEAD v1.dta - The "All data" tab from "Approval data for SEAD v1.xlsx", this datafile is used for the descriptive data and count data analysis in stata. 

Codebook SEAD governors v1.PDF - codebook for Approval data for SEAD v1.xlsx. This contains the citation information for the data and the descriptions of all the codes used to describe the input series in the SEAD raw data. 

SEAD governor quarterly v1.dta - The datafile of latent estimates. While this set of files contains the necessary commands to generate this dataset, this is the version that I generated, have released as SEAD v.1, and on which I performed the analyses reported here. 

Codebook for SEAD governor quarterly v1.pdf - The codebook for the datafile of latent estimates "SEAD governor quarterly v1.dta." This contains the descriptions of each measure of latent approval.  

generate SEAD.R - The code to generate the latent approval data in R by using the data in Approval data for SEAD v1.xlsx. 

load_functions.R - The code to load the programs for "generate SEAD.R"

Quarters.zip - A series of folders to have the "generate SEAD.R" deposit the data in an organized fashion. Most of these are empty folders that the R programing will look to store the output series and log files from each of the wcalc estimation runs as a state-estimation is a separate text file of output and a separate log file with the number of valid series. So having these folders helps keep everything organized. It creates some redundancies, but there are a lot of files that are generated and so organization is key. The zip file contains a main set of files saved in the folder "Quarters":

--"app_appdis": the estimated series for relative approval (or approval/(approval+disapproval)) estimated with smoothing are stored here as text files.
--"app_appdis_not_smoothed": the estimated series for relative approval (or approval/(approval+disapproval)) estimated without smoothing are stored here as text files.
--"app_series": the estimated series for governor approval estimated with smoothing are stored here as text files.
--"app_series_not_smoothed": the estimated series for governor approval estimated without smoothing are stored here as text files.
--"logs": the log files produced by wcalc are stored here. Each log file takes the name of the state it covers and the specific data series Using the same naming conventions as the folders). The log file records the time periods covered, the number of input series, whether smoothing was used, the iteration history, the eigenvalue for the latent factor, the specific series that were retained, and , for each quarter, the number of valid surveys that were available and used. This last piece of information needs inclusion in the dataset and so I strip it manually from each file as described below in the procedures. 
--"neg_series": the estimated series for governor disapproval estimated with smoothing are stored here as text files.
--"neg_series_not_smoothed": the estimated series for governor disapproval estimated without smoothing are stored here as text files.
--"stata": A series of folders where the "appending output.do" program saves the outputs stored in the other folders as text files as stata files. It contains folders to store the output for the relative approval, approval, and disapproval series (smoothed and unsmoothed) using the same naming conventions as above. It is also where the final merged quarterly file is saved as "SEAD governor quarterly v1.dta"
--"valid survey inputs": A folder where the file "appending log files.do" stores for each state the number of valid surveys used in the estimation of the latent smoothed approval series in each quarter. 

appending output.do - for each of the approval measures, smoothed and not smoothed, this file: (1) converts them from text to stata files (saved in the "stata" subfolder) and names the variables, (2) deletes duplicate observations for each year generated by the wcalc algorithm,  (3) merges them all into one dataset, (4) cleans out observations where the wcalc algorithm could not generate meaningful estimates due to a lack of overlapping series (these estimate values of approval/disapproval that are less than 2 percentage points and often as negative), and (5) merges the files into "SEAD governor quarterly v1.dta", saved in the stata subfolder. 

appending log files.do - To generate the number of valid survey inputs measure, the user must first go through the log files and deleting all the lines before the three columns where the quarters and number of surveys in it are recorded. I suggest saving a backup copy of the "logs" folder and then storing the cleaned versions of the logs here. (note that for simplicity I only use the count for the number of series used for smoothed approval as this is the one that estimates the most series). This do-file then names them for each state and saves them as stata files in the "valid survey inputs folder. It then merges these files together into a single file that is merged with the "SEAD governor quarterly v1.dta" file in the stata folder. 

state names.dta - A file with the names of the 50 states, this is used by R and Stata to know what to name the output files. 

The user should extract all the files and folders into the same director as the data and do files listed below. 

Then the zip file also contains template Quarters - a duplicate of the files in "Quarters" in case the user wants to run the latent variable analysis a second time without saving over the previous run; the old "Quarters" file should be a given a new name to designate that run and then a copy of the "template Quarters" file should be saved as "Quarters"

2. Generate and Graph the Descriptive Data Presented in Table 1 and Figures 1-3

The following files can be used to generate the figures.

Generate Table 1 and Figure 1.do - This do file generates the data that can be cut and pasted into Table 1 and also generates Figure 1
Generate Figure 2 (New York).do - This do file uses the SEAD datasets saved in "files and code for SEAD dataset v1" to generate the three figures that combine to make up Figure 2. 
Generate Figure 3.do - This do file uses the SEAD datasets to generate the 4 example graphs of states with different levels of poling frequency and approval volatility that are in Figure 3. 

3. Model the Count Data as Presented in Table 2

The following files are used to generate Table 2

descriptives count data 1980 to 2020.dta - While the number of surveys conducted in each state-quarter is drawn from the "SEAD governor quarterly v1.dta" dataset, this file contains the variables on state population and election closeness that are used in the analysis. This gets merged with the data to perform the count models. Note that the election competitiveness measures were entered by the researcher by hand as described in the codebook. 

table 2.do - This program file generates the counts of surveys conducted in each quarter in each state, merges this information with the information in "descriptives count data 1980 to 2020.dta", and performs the negative binomial models presented in Table 2. 

Codebook Table 2.pdf - Describes all the variables in the analysis for the count models, including their sources. 

4. Model Governor Approval Dynamics and Honeymoons as Presented in Table 3 and in the Supplemental Appendix

The following files will generate table 3 and the tables in the appendix. 

controls for table 3.dta - This contains the control variables for the analysis in Table 3 that the user will merge with the latent approval measures in "SEAD governor quarterly v1.dta" using the "honeymoon models.do" do file to perform the analysis in Table 3. 

Codebook for analysis in table 3.pdf - Describes all the variables in the dataset "SEAD v1 quarterly v1.dta" and also in the file "controls for table 3.dta." The latent variables are also described in "Codebook for SEAD governor quarterly v1.pdf." The control variables for partisanship, resignations and deaths in office, governor's gender, and honeymoon quarters are coded by the author. The economic data was downloaded from the bureau of labor statistics in May 2021 and so it varies slightly from the current data available from the BLS as it is revised annually. The economic data used in the download are archived in "Copy of ststdnsadata through 2020 (2021 revision).xlsx" but it is much easier to use them as coded here. 

Ststdnsadata through 2020 (2021 revision).xlsx - The 2021 revision of non-seasonably adjusted economic data from the BLS. Data are available at https: //www.bls.gov/lau/rdscnp16.htm, under the link "Employment status of the civilian noninstitutional population, not seasonally adjusted" as a zip file. This is the May 2021 download. To generate the data used in the file "SEAD v1 quarterly with controls for table 3.dta", the user should first remove the sub-state statistical areas from the report and the District of Columbia. Then the relevant information is in column K (the Unemployment Rate). The BLM data are reported monthly; for the quarterly data I take the average for the three months in the quarter. 

honeymoon models.do - This contains the do file to run the models in Table 3 and also Table A1 and A2 in the appendix using the files in this folder. 


INSTRUCTIONS FOR USING EACH SET OF FILES

1. The user should donwload all the files in the replication set to the same directory. That directory needs to be subsituted in for the phrase "[directory]" in the R.studio and stata do files to perform the analysis. 

2. I have designed the files to run each part of the analysis piecemeal because then you can get the table/figure you are most interested in. Each do file is self-contained and does not rely on data manipulations performed in other stages of the analysis. 

3. If the user does not wish to generate the latent measure of approval themselves, they don't have to; the files "SEAD v1 quarterly with controls for table 3.dta" and "SEAD governor quarterly v1.dta" described above are sufficient to perform all the analyses and the file "SEAD v1 quarterly with controls for table 3.dta" includes all the necssary control variables. To ensure comparability with other studies using version 1 of the dataset, this is the strategy I would recommend. 

4. To generate the latent variables, the user should first run the analysis detailed in "generate SEAD.R"after updating the directory for the files and unzipping the file Quarters.zip. This will perform the various wcalc procedures to generate the latent measures. The runes for each state are saved as text files inside the various subfolders of "quarters" described in the section of this file on "files and code for SEAD dataset v1". The estimation procedures for these files proceeds one at a time for each state-indicator pair and is very time intensive. 

The reported files for each state saved in the subfolders of "quarters" will contain many repeatedmeasures of the latent approval rating generated in the iteration process. For states where the wclac estimation could not produce a valid series, these values will be less than 2. The file "appending output.do" should then be run to (1) convert the text files to stata files and save them inside the "stata" subfolder of the "quarters" folder, (2) to remove repeated values and (3) to replace any invalid estimates as missing data. This generates a version of the file "SEAD governor quarterly v1.dta" that is saved in the "stata" subfolder to avoid replacing the author-generated version that is the official version 1 release. If the user wishes to make this the basis of their analysis, they should save it to the main directory instead. 

To generate the data on valid surveys, the user first needs to go into the log files and for each text file delete all the header information. For example, the Alabama log file starts with the following information:

[1] "Estimation report:"
[1] "Period: 1980 4  to 2021 4 165  time points"
[1] "Number of series:  25"
[1] "Number of usable series:  15"
[1] "Exponential smoothing:  TRUE"
[1] "Iteration history: Dimension  1"
[1] ""
  Iteration Converge  Crit Reliability Forward Smoothing Backward Smoothing
1         1   0.0097 0.001       0.943            0.9610             0.6687
2         2    1e-04 0.001       0.943            0.9611             0.6687
[1] ""
[1] "Eigen Estimate  0.76  of possible  0.81"
[1] "  Percent Variance Explained:  94.17"
[1] ""
[1] "Variable information:"
                    Variable Name  N  Mean StdDev Loading
1                 alabamacapstone 30 40.16  11.09    0.98
2                            cces 13 52.27   9.14    1.00
3            davis&penfield&assoc  6 42.33  14.19    0.87
4             masondixon_approval  3 60.00   3.27    0.99
5           masondixon_excellent1  8 50.00   5.05    0.80
6                 morning_consult 16 57.56   9.20    0.99
7                             nbc  2 69.00   6.00    1.00
8               rasmussen_approve  4 58.00   5.74    0.99
9             rasmussen_excellent  3 63.00   1.63    0.97
10        southernmedia_excellent  2 56.00   2.00    1.00
11       southernmedia_excellent2 21 46.46   9.85    0.98
12                      surveyusa 19 58.82   6.82    0.99
13 univsouthernalabama_excellent1  3 40.67   4.03    0.96
14          usapolling_excellent1  2 43.00   2.00    1.00
15          usapolling_excellent2  2 42.50   2.50    1.00
[1] ""
[1] "Number of cases per period:"
    period count
1   1980.4     1
2   1981.1     0
3   1981.2     1

Before continuing to the other time periods. Everything before the line "1   1980.4     1" needs to be deleted and then the log file is saved in the same folder (I recommend creating a copy of the log file folder as a backup and saving it inside the "quarters" folder to have the other information recorded someplace). After this has been done for all 50 states, the user should run the file "appending log files.do" to format those text files as stata files, merge them into a single file, merge that file with the newly generated "SEAD governor quarterly v1.dta" file, and generate the counter for not being imputed for more than three quarters in a row. 

This final user-generated version of "SEAD governor quarterly v1.dta" wiill be saved in the "stata" subfolder of "quarters". The user can save it under the same name in the "files and code for SEAD dataset v1" and run all the analyses except for the ones for Table 3, which will require merging the descriptive data from that folder with it (using the common variables of state and qtr for the basis of the merge).

5. Once the latent variables have either been regenerated or the user has decided to use the author-generated versions of the data provided in "files and code for SEAD dataset v1", they can generate  the decscriptive table 1 and figures 1-3 by running the files "Generate Table 1 and Figure 1.do", "Generate Figure 2 (New York).do" and "Generate Figure 3.do" inside the folder "do files for descriptive figures and tables." The figures will be stroed in that folder; the data for Table 1 must be copied from inside the state output as described in the do file. 

6. The count model in Table 2 can be generated using the file "table 2.do" in the "data and do files for table 2" folder.

7. The model of approval dynamics over time can be generated using the file "honeymoon models.do" in the "data and do files for table 3" folder. 
