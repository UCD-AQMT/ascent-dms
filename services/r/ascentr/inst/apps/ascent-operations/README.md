Interactive web page for ASCENT site operators to monitor the health and data transmission status of their instruments.


#### To Deploy

Deploy to test
```ascentr::deploy("./services/r/ascentr/inst/apps/ascent-operations/", title="ascent-operations-test", account = "ascent")```


Then to production
```ascentr::deploy("./services/r/ascentr/inst/apps/ascent-operations/", title="ascent-operations", account = "ascent")```
