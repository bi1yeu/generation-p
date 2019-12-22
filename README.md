# generation-p: A genetic algorithm pixel art bot

This program uses a genetic algorithm to create 2D images inspired by pixel art. It posts them to Twitter, and posts' likes and retweets serve as input to the algorithm's fitness function. Fitter individuals are those that are more popular. Results [here](https://twitter.com/generationp3).

## usage

Requires [Leiningen](https://leiningen.org/). Provide the following environment variables:

```
export ENV=prod
export CONSUMER_KEY=...
export CONSUMER_SECRET=...
export ACCESS_TOKEN=...
export SECRET_KEY=...
```

```
lein run
```


Resources:

- https://en.wikipedia.org/wiki/Genetic_algorithm
- https://en.wikipedia.org/wiki/Selection_(genetic_algorithm)
- https://en.wikipedia.org/wiki/Interactive_evolutionary_computation
- https://en.wikipedia.org/wiki/Evolutionary_art

