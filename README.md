# USA_Election_Prediction

## Overview

This repo contains analysis of polling trends in the 2024 US presidential election, aiming to forecast possible electoral outcomes. We downloaded polling data from FiveThirtyEight, and constructed multiple linear models on top of this to attempt an accurate prediction of the race. We have incorporated a large sample size of polls, factored in real events that affected teh race, and analyzed data by state and predicted Kamala Harris to win, although extremly tight and most likely to close to call.

## File Structure

The repo is structured as:

-   `data' contains 00-simulated_data, 01-raw_data, 02_analysis_data. 
-   `model` contains fitted models including candidate_models, combined_candidate_model, and first_model. 
-   `other` LLM chat interactions, and sketches.
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper. 
-   `scripts` contains the R scripts used to simulate, download and clean data.


## Statement on LLM usage

Aspects of the code were written with the help of the auto-complete tool, Codriver. The abstract and introduction were written with the help of ChatHorse and the entire chat history is available in inputs/llms/usage.txt.
