# Removed posts
status 410 {
  /2014-01-07-garbage-collection.html
  /2015-07-18-continuous-integration-with-jenkins-and-stack.html
  /2015-07-26-erlang-gopher-server.html
  /2015-08-09-identity-monads-ahoy.html
  /2015-10-04-secure-communications-over-insecure-channels.html
  /2015-12-15-finite-maps-in-isabelle.html
  /concurrency/2014-12-26-haskell-systematic-concurrency-testing.html
  /concurrency/2015-01-10-pre-emption-bounding.html
  /concurrency/2016-05-18-some-thoughts-on-distributed-systems.html
  /concurrency/2017-02-02-subconcurrency.html
  /etc/2013-05-27-a-gentle-introduction-to-parsec.html
  /etc/2015-08-01-debugging-an-allocation-issue.html
  /etc/2015-09-23-icfp-retrospective.html
  /etc/2016-02-02-cabal-info.html
  /etc/2017-03-15-optimising-haskell.html
  /etc/2017-04-16-representing-generating-comparing-typed-expressions.html
  /relnotes/2016-04-03-dejafu-0.3.0.0-release.html
  /relnotes/2016-09-10-dejafu-0.4.0.0-release.html
  /relnotes/2017-02-21-concurrency-1.1.0.0-dejafu-0.5.0.1-release.html
}

redir 301 {
  # Moved files
  /publications/coco-flops18-prelim.pdf /publications/coco-flops18.pdf
  /publications/coco-flops18-prelim.bib /publications/coco-flops18.bib

  # Renamed posts
  define(`categorised', `/posts/$2 /posts/$1/$2')
  categorised(`etc',         `2013-05-27-a-gentle-introduction-to-parsec.html')
  categorised(`concurrency', `2014-12-26-haskell-systematic-concurrency-testing.html')
  categorised(`concurrency', `2015-01-10-pre-emption-bounding.html')
  categorised(`etc',         `2015-08-01-debugging-an-allocation-issue.html')
  categorised(`concurrency', `2015-08-21-reducing-combinatorial-explosion.html')
  categorised(`relnotes',    `2015-08-27-announce-dejafu.html')
  categorised(`etc',         `2015-09-23-icfp-retrospective.html')
  categorised(`concurrency', `2015-11-29-breaking-the-law-verifying-typeclass-laws-with-quickcheck-and-dejafu.html')
  categorised(`etc',         `2016-01-09-c-is-not-turing-complete.html')
  categorised(`etc',         `2016-02-02-cabal-info.html')
  categorised(`etc',         `2016-02-12-strict-vs-lazy.html')
  categorised(`relnotes',    `2016-04-03-dejafu-0.3.0.0-release.html')
  categorised(`concurrency', `2016-05-13-systematic-concurrency-testing-and-daemon-threads.html')
  categorised(`concurrency', `2016-05-18-some-thoughts-on-distributed-systems.html')
  categorised(`etc',         `2016-08-25-three-months-of-go.html')
  categorised(`relnotes',    `2016-09-10-dejafu-0.4.0.0-release.html')
  categorised(`concurrency', `2017-02-02-subconcurrency.html')
  categorised(`relnotes',    `2017-02-21-concurrency-1.1.0.0-dejafu-0.5.0.1-release.html')

  # Converted to memos
  define(`memod', `/posts/$1 https://memo.barrucadu.co.uk/$2')
  memod(`concurrency/2015-08-21-reducing-combinatorial-explosion.html',  `reducing-combinatorial-explosion.html')
  memod(`concurrency/2015-11-29-breaking-the-law-verifying-typeclass-laws-with-quickcheck-and-dejafu.html', `concurrency-and-typeclass-laws.html')
  memod(`concurrency/2016-05-13-systematic-concurrency-testing-and-daemon-threads.html', `sct-and-daemons.html')
  memod(`concurrency/2017-06-09-property-testing-side-effects.html',     `property-testing-side-effects.html')
  memod(`concurrency/2017-10-14-writing-a-concurrency-testing-library-01.html', `minifu-01.html')
  memod(`concurrency/2017-10-28-writing-a-concurrency-testing-library-02.html', `minifu-02.html')
  memod(`etc/2016-01-09-c-is-not-turing-complete.html',                  `c-is-not-turing-complete.html')
  memod(`etc/2016-02-12-strict-vs-lazy.html',                            `strict-vs-lazy.html')
  memod(`etc/2016-08-25-three-months-of-go.html',                        `three-months-of-go.html')
  memod(`etc/2017-05-18-visualise-your-finances-with-hledger-influxdb-grafana.html', `hledger-influxdb-grafana.html')
  memod(`etc/2017-12-06-the-academic-mindset-and-me.html',               `academic-mindset.html')
  memod(`etc/2017-12-16-i-need-a-budget.html',                           `2018-budget.html')
  memod(`relnotes/2015-08-27-announce-dejafu.html',                      `dejafu-0.1.0.0.html')
  memod(`relnotes/2017-08-16-significant-performance-improvements.html', `throwing-away-traces.html')
  memod(`relnotes/2017-09-22-irc-client-1.0.0.0.html',                   `irc-client-1.0.0.0.html')
}
