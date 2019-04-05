# time_intervene
Causal inference in domain of point events with gamma delays


##File rundown (April 2019)

- `code`
  - `js` the pilot experiment (condition selection randomised) and duplicates with the condition selection forced
  - `python` a script for making rudimentary videos from trial data
  - `R` all the analyses and plotting
    	- `analyses_for_mturk.r` reads the processed data from the mTurk pilot and computes various statistics for the cogsci paper
    	- `analyses_for_journal.r` (unfinished) reads the processed data from the Prolific replication and computes various statistics
    	- `bayesian_predictions_*.r` computes likelihoods and posteriors of models under full Bayesian inference algorithm (`calls likelihood_functions.r`; slow, only needs to be done once)
    	- `compare_ppt_mod_*.r` constructs the model comparison table counting consistency between model and participants judgments (slow, prolific version still buggy)
    	- `compare_sims.r` pertains to intervention selections.  Compares the achieved information under humans actions against simulated intervener that are either 'even', 'reactive' or 'random' in their timing of actions
    	- `construct_belief_simulations_*.r` steps through all behavioural data creating the heuristic judgment predictions at every time point and saving them to separate named `../data/*.rdata` files
    	- `create_cogsci_data.r` subsets the full `../data/pilot_data.rdata` (N=60 containing three delay conditions: _between, within_ and _reliable_) to keep within and unreliable (renaming as _unreliable_, _reliable_) to create `../cogsci_data.rdata`
    	- `create_DBN_space_and_funs.r` creates 3-D arrays _DBN3_ (3,3,64) and _DBN4_ (4,4,4096) containing all possible 3 and 4 variable causal structures in the form of adjacency matrices (connection from i to j in graph k if `cell[i,j,k]=1` and stores them along with a function that can match a 2-D array graph input with its index in the 3D array, saving these to `dbn.rdtata`
    	- `get_posterior_entropies_*.r` takes the posteriors and computes Shannon entropy for each time point and adds to the dataframes (only needs to run once)
    	- `intervention_simulation_*.r` simulates the task applying several heuristic intervention rules
    	- `likelihood_functions.r` contains function that computes likelihood some trial data under all causal structures
    	- `notes_plots.r` misc exploratory plotting
    	- `order_only_predictions.r` linking to _The Order of Things_ (2014 cogsci paper) we wondered if a pure event order constraint heuristic would work at all on these data (it doesn't because it doesn't prefer the more recent event over all other prior events so no signal)
    	- `plots_design.r` some plotting of the task itself without behavioural data
    	- plots_*.r` lots of plotting, lots of redundancy, variants for talks, thesis, conference paper etc
    	- 'read_in_squl_data*.r' reads the csv files (exported from the sql database) and cleans up the data into R dataframes.
    	- `simulate_data.r` creates some hand chosen toy data for plotting I think
    	- `tacits` some extra plotting for the _Time and Causality in the Sciences_ book chapter
	- `data` many of these `.rdata` files probably obselete but keeping incase
		- `bonus*` bonus payment stuff
		- `cogsci_data.rdata` the dataset used in the cogsci paper (subset from `pilot_data.rdata` used in thesis)
		- `mf_results.rdata` the model predictions (for cogsci paper)
		- `individual_fits*` likelihoods and posteriors saved separately for each participant
		- `*.csv` original data files from mTurk and prolific
		- `NS_*` "Neuraths ship" referring to the varieties of heuristic compared in the cogsci paper
			- `adapt_between` a subtle hacky addition meaning that for the _between_ condition, the learner would store the inferred delay for a putative causal link and use that to shape their judgements about subsequent candidate causation
			- `boost` variants that overwrite online evolving belief with participants latest online judgment where available (this inflates the correlation with participants but also probably distorts the comparison, we didn't do this in the end)
 - `figures` hundreds of figures arranged by purpose
 - `movies` some demo movies created with quicktime or using the python script for demonstration purposes
  - `write_up` the latex projects for the cogsci paper, the book chapter and some very old notes

