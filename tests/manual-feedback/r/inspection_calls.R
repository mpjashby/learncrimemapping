# These unassigned inspection results should attract style feedback
observations <- data.frame(value = 1:10)
head(observations)
tail(observations)
observations |> head()

# These results are saved or used by a subsequent function
sample_rows <- head(observations)
sample_count <- observations |> head() |> nrow()
