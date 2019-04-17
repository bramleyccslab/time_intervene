# Time -- Intervene

Causal inference in domain of point events with gamma delays

## File rundown (April 2019)

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
  	- `plots_*.r` lots of plotting, lots of redundancy, variants for talks, thesis, conference paper etc
  	- `read_in_squl_data*.r` reads the csv files (exported from the sql database) and cleans up the data into R dataframes.
  	- `simulate_data.r` creates some hand chosen toy data for plotting I think
  	- `tacits` some extra plotting for the _Time and Causality in the Sciences_ book chapter

- `data` many of these `.rdata` files probably obselete but keeping incase

	- `cogsci_data.rdata` the dataset used in the cogsci paper (subset from `pilot_data.rdata` used in thesis)
     - `df.sw` subjectwise (one row per participant) data.frame

     	- `upi` unique personal identifier
      	- `learn_cond` learning condition, all participants were `active`.  This variable is just in case we ever run versions with passive (observation only) participants that might watch another participant play or just watch a device activate by itself
     	- `delay_cond` which delay condition was the participant in? the actual parameters of the delay distributions are in `alpha` and `beta`
     	- `final_score` how many of the 54 edges (6 3-variabl 3-edge and 6 4-variable 6-edge problems) were correctly identified (out of their 4 states: no edge, forward, backward, bidirectional) _at the end_ of each trial
     	- `final_bonus` how many of the 54 edges were identified at the randomly timed bonus points across all trials (recalling we paid based on accuracy at random moments to incentivise efficiency and reporting of struture beliefs as soon as possible).  This determined how much bonus each participant made (5 cents per correct edge).
     	- `*_acc` the scores /54 to get the proportion of correct edge judgments
     	-  `n_ints` how many interventions (left clicks on components) were performed overall by the participant
     	- `n_effects` how many activations of components did they experience (aside from the interventions)
     	- `prop_nodes_tested` proportion of the components did they click on at least once?
     	- `int_space*` how long did they wait between one intervention and the next?
     	- `int_space_event*` how long did they wait on average between the most recent event and their next intervention?
     	- `int_pref_parent` how many times did they intervene on a root (parentless) component per time they intervened on a non-root component (or something like that, might be rescaled by ratio of roots to non-roots)
     	- `*.cy` measures computed for the 6 cyclic problems only
     	- `*.ncy` measures computed for the 6 noncyclic problems only
     	- `m_ent` the average posterior entropy (i.e. uncertainty) achieved by this participant under the ideal Bayesian analysis (lower is better, indicative of more informative intervention selection)
     	- `*3/4` variants restricted to the three variable problems or 4 variable problems (initial uncertainty is very different for these cases because there is a much larger space of 4 variable problems)

    - `df.tw` a trialwise data.frame (one row per trial).  As with `df.sw` conventions except:
    	
	- `trial` what position during the participants run though the experiment did this trial appear
      	- `trial_type` which of the 12 devices (in the order depicted in the paper figure 2) was the participant interacting with on this trial
      	- `n_nodes` how many components were there
      	- `n_ints` how many of the (max 6) interventional clicks did they perform
      	- `score` as with final score
      	- `bonus` as with final bonus
      	- `n_edges` how many edges (potential links) are in the network (3 variable network have 3 and 4 variable network have 6)
      - `practice` there were two practice problems that were repeats of two of the other problems.  I think they've already been subset out of `cogsci_data` so this column is always true but they exist in the raw csv.
      - `graph` the index in DBN3 or DBN4 for the true network
      - `cyclic` does this network contain a loop?
      - `n_ev` how many events occurred in total
      - `n_ef` how many were activations (rather than interventions)
      - `n_*` how many for each component separately
      - `prop_int_root` what proportion were on a parentless node
      - `final_belief` the index of their final judgment
      - `n*_dir` breaking this down into individual edge judgments (0 = no link, 1 = forward link, 2 = backward link, 3 = bidirectional link)
      - `n*_cor` was it right?
      - `n_be_ch` how often did this learner update their model during this trial?
      - `initial_ent` how much uncertainty in bits is there at the beginning of the trial i.e. `-sum(p * logn(p))` for a uniform over the space of graphs.
      - `igain` information gain = final - initial entropy
      - `max_p` how likely was the most likely graph in the set at the end
      - `p_truth` how likely was the true graph at the end (often this is the same graph as the most likely one but not necessarily).

    - `df.be` a beliefwise data.frame (one row per updated participant belief).  As with `df.tw` conventions except:
    	
		- `time` when in ms since beginning of the trial was this belief registered by the interface. A new belief was registered whenever participants hit the "confirm" button in the middle of the device after making some changes to their marked connections.  Total trial length 45,000 ms (the final response which is subject of most other analyses is automatically registered when timer hits zero, so at 45000 + a few 100ms lag)

    - `df.ev` an eventwise data.frame (one row per activation or intervention).  As with other data.frames except:
		- `location` which component was activating
		- `type` was it an intervention "`action`" or an activation "`effect`"?
		- `from` what is the ground truth as to which variable caused this activation if applicable
		- `with_delay` how long was the ground truth actual causal delay from the cause event to this activation event?
    
    - `mf_results.rdata` the model predictions (for cogsci paper)
    - `individual_fits*` likelihoods and posteriors saved separately for each participant
    - `*.csv` original data files from mTurk and prolific
    - `NS_*` "Neuraths ship" referring to the varieties of heuristic compared in the cogsci paper
		- `boost` variants that overwrite online evolving belief with participants latest online judgment where available (this inflates the correlation with participants but also probably distorts the comparison, we didn't do this in the end)
		- `adapt_between` a subtle hacky addition meaning that for the _between_ condition, the learner would store the inferred delay for a putative causal link and use that to shape their judgements about subsequent candidate causation
	- `bonus*` bonus payment stuff
- `figures` hundreds of figures arranged by purpose

- `movies` some demo movies created with quicktime or using the python script for demonstration purposes

- `write_up` the latex projects for the cogsci paper, the book chapter and some very old notes

