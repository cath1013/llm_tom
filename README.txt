From Perceptual Access to Belief Reasoning: Data and Analysis
=============================================================

This repository contains human and large language model (LLM)
responses, coded scores, analysis scripts, and statistical results for
our study. Each observation represents one participant or one
independent model observation.


-- Package contents --

main
|--> README.txt
|--> analysis_glm.R                      runs all analyses
|--> data
|     |--> main.xlsx                      main dataset (see below)
|--> results
|     |--> descriptives.csv               accuracy by condition and source
|     |--> descriptives_by_story.csv      accuracy by vignette
|     |--> results_1a_binomial.csv        Study 1A binomial tests
|     |--> prereq_1b.csv                  Study 1B prerequisite accuracy
|     |--> lrt_1b_omnibus_effects.csv     Study 1B omnibus LRTs
|     |--> lrt_1b_omnibus_margins.csv     Study 1B marginal accuracies
|     |--> lrt_1b_source_by_condition.csv Study 1B human vs. model LRTs
|     |--> fisher_1b_per_model.csv        Study 1B per-model Fisher tests
|     |--> lrt_1b_human_vignette.csv      Study 1B human vignette LRT
|     |--> human_vignette_cells.csv       Study 1B human vignette accuracies
|     |--> fisher_1b_john_vs_other.csv    Study 1B John vs. other vignettes
|     |--> prereq_2.csv                   Study 2 prerequisite accuracy
|     |--> lrt_2_multimodal.csv           Study 2 human vs. Gemini 3 LRT
|     |--> lrt_2_vignette_omnibus.csv     Study 2 vignette LRT



-- Studies and conditions --

Study   Condition                    Source
1A      False belief                 LLM only
1A      Transparency                 LLM only
1A      Spatial-relation             LLM only
1A      Testimony                    LLM only
1B      Clarified transparency       Human and LLM
1B      Clarified spatial-relation   Human and LLM
2       Multimodal transparency      Human and multimodal LLMs



-- Main dataset: data/main.xlsx --

The dataset contains 2,010 observations (450 human participants and
1,560 model sessions). Cells not applicable to that study are recorded as NA.

Identification
  Study, Condition       Study and experimental condition
  Story                  1 = John, 2 = Lucy, 3 = Grace
  Source, Model          Human/model designation and model name
  participant_id         Anonymized human participant identifier
  session_id             Independent model session identifier
  age, gender            Participants' age and gender (human only)

Responses
  study1a_q1_response             Study 1A target response
  study1b_q1_response to
    study1b_q4_response           Study 1B prerequisite and target responses
  study2_q1A_response to
    study2_q3_response           Study 2 prerequisite and target responses
  model_raw_response              Original model response, where applicable

  Note on q1A and q1B (Study 2). The labels A and B do not appear 
  in the original materials as separate questions. Because the question 
  asks about two objects, we split the answer into two parts for coding: 
  q1A is the object named for Picture A, q1B the object named for Picture B. 
  Each part is scored separately and question 1 counts as passed
  only if both are correct (study2_q1_both_correct).

Scoring
  *_correct              Question-level accuracy (1 = correct, 0 = incorrect)
  Target measures: Study 1A Q1; Study 1B Q4; Study 2 Q3.

Exclusions
  included_in_analysis   1 = retained; 0 = excluded
  exclusion_reason       Reason for exclusion, if applicable

