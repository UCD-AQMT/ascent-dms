Interactive web page for ASCENT site operators to monitor the health and data transmission status of their instruments.

#### To Deploy

First deploy to test to confirm everything is good before deploying to production
```ascentr::deploy("./services/r/ascentr/inst/apps/ascent-realtime/", title="ascent-realtime-test", account = "ascent")```

```ascentr::deploy("./services/r/ascentr/inst/apps/ascent-realtime/", title="ascent-realtime", account = "ascent")```