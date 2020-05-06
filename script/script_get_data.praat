# Set up output file ----------------------------------------------------------

# Store path to where we want to save data (look at the structure of your proj)
savePath$ = "../data/"

# Choose name for .csv file
outFile$ = "vowel_data.csv"

# Delete current file
filedelete 'savePath$'/'outFile$'

# Create file with headers
fileappend 'savePath$'/'outFile$' id,language,item,vowel_1,vowel_2,'newline$'

# -----------------------------------------------------------------------------





# Set up loop -----------------------------------------------------------------

# Set path to stim files (.wav and .TextGrid)
filePath$ = "../item/"

# Get .wav file and store in list
Create Strings as file list... dirFiles 'filePath$'/*.wav

# Select .wav file and corresponding textgrid
select Strings dirFiles
numberOfStrings = Get number of strings

# -----------------------------------------------------------------------------






# Run loop --------------------------------------------------------------------

for interval from 1 to numberOfStrings
	select Strings dirFiles
	fileName$ = Get string... interval
	prefix$ = fileName$ - ".wav"
	Read from file... 'filePath$'/'prefix$'.wav
	Read from file... 'filePath$'/'prefix$'.TextGrid
    select TextGrid 'prefix$'
  
    #
    # Get id, item, vowel and language labels
    #

	id$ = prefix$
    item$ = Get label of interval: 1, 2
    language$ = Get label of point: 4, 1
    vowel_1$ = Get label of interval: 2, 2
    vowel_2$ = Get label of interval: 3, 2

	# Get vowel durations




    # Print results to window and save to .csv file
    printline 'id$','language$','item$','vowel_1','vowel_2'
    fileappend 'savePath$'/'outFile$' 'id$','language$','item$','vowel_1:2',
    ...'vowel_2:2''newline$'

endfor

# Remove objects from praat menu
#select all
#Remove

# -----------------------------------------------------------------------------
