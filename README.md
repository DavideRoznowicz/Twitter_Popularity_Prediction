### ML model to predict the popularity of a tweet about food


We developed a **Random-Forest-based model** to predict the popularity of a tweet about food. \
We gathered our dataset through **Twitter API**: after querying our target users’ timelines and filtering out weakly-informative tweets, we ended up with about 15,000 of them, together with over 90 metadata-related variables, to which we added hundreds of text mining-derived features. \
Since downloading and processing more than 1.6 GB of data had proven a computationally intensive task, an initial attempt in python was discarded and taken over by a much faster **OpenMP** version of a **C implementation**.


* If you want to see a nice report, presenting details about the implementation together with the final results, go to [final report](/twitter/report.pdf)


* Instead, if you want to dig into the technical part and see the codes, go to the folder [twitter](/twitter)
