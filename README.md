# jt
Command-line Hadoop RM/History client.

## Running
You need to [install stack](http://docs.haskellstack.org/en/stable/README.html#how-to-install) the
haskell build tool.
```
stack build
stack install
```
This will install `jt` into your `~/.local/bin` which should be in your path after stack is
installed.

You can run without installing using stack:
```
stack exec jt command -- <non-stack options after the -->
```


## Configuration
jt is configured by having a `.hadoop_cluster.conf` file in your tree, tracing back from the CWD, or from your $HOME.
In here you should on each line give a short name to each of your hadoop clusters, then a link to your RM and history end points. e.g.:
tstA http://tstA.example.com:50030 http://tstA.example.com:8080


#Commands::

## Show
````Usage: jt show [--history] [--resource-manager]```
List out what is the default hadoop cluster and other clusters in the system. Optionally just print the URL's for history or RM


## Jobs
````Usage: jt jobs [-u|--user USER] [-c|--cluster CLUSTER] [-l|--limit LIMIT]
               [-o|--history] [-a|--resource-manager] [-s|--state STATE]
               [-t|--tabs]```

List up to `LIMIT` jobs for optionally `USER` or all users from cluster `CLUSTER` or your default cluster. Its also possible to restrict it to the history or RM using the `-o` and `-a` flags respectively. You can filter to a particular state using the state arg.
Pretty printing of output is enabled by default. To use simple tables for consumption in other tooling pass in `-t`.

Example:
```bash
Name                                         User   State    JobId                   StartedTime
MyFirstJob.1450391964.(2015-07-18).(3/3)     myUser FINISHED job_1450230361xxx_xxx 1450401485960
```

## Details
```Usage: jt details [-c|--cluster CLUSTER] [-t|--tabs] JOB```

The details command is to zoom in and get some info on a job. The `-j` arg here controls the job you are searching for. Both the AM and History servers will be searched for the job as necessary.

```
Name             User    State     JobId                   Mappers Reducers    StartedTime         FinishedTime
MyFirstJob      myUser SUCCEEDED job_145023036xxx3_65370   205     2000     2015-12-17 17:02:39 2015-12-17 17:19:17
```
