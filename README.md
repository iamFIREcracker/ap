# ap

THIS IS A WORK IN PROGRESS... USE AT YOUR OWN RISK!

    > cat input.txt
    activity act-01 5
    activity act-02 3
    activity act-03 5
    activity act-04 3
    activity act-05 3
    activity act-06 3
    activity act-07 3
    activity act-08 5
    activity act-09 3
    activity act-10 5
    activity act-11 5
    activity act-12 5 act-03
    activity act-13 10
    person user-01 80 act-01
    person user-02 20 act-02
    person user-03 60 act-03
    person user-04 80 act-04
    person user-05 60 act-05
    person user-06 20 act-06
    person user-07 80

    > bin/ap --today 2020-03-30 < input.txt
    act-04 2020-03-30 2020-04-02 user-04
    act-09 2020-03-30 2020-04-02 user-07
    act-05 2020-03-30 2020-04-06 user-05
    act-01 2020-03-30 2020-04-07 user-01
    act-03 2020-03-30 2020-04-09 user-03
    act-02 2020-03-30 2020-04-20 user-02
    act-06 2020-03-30 2020-04-20 user-06
    act-11 2020-04-02 2020-04-13 user-07
    act-13 2020-04-02 2020-04-21 user-04
    act-08 2020-04-06 2020-04-16 user-05
    act-10 2020-04-07 2020-04-15 user-01
    act-07 2020-04-09 2020-04-16 user-03
    act-12 2020-04-13 2020-04-21 user-07

## Input

### Activities

    activity act-01 10
    activity act-02 5
    activity act-03 5 act-01 act-02

Activities entries are prefixed with the 'activity' keyword, and they specify:

- the activity name
- the effort (in days) required to complete the activity
- an optional list activities that the current one depends on

### People

    person user-01 90
    person user-02 50
    person user-03 50 act-01

Activities entries are prefixed with the 'person' keyword, and they specify:

- the person name
- their allocation to _this_ project, with the idea being that the smaller their
  allocation, the longer it's going to take for them to complete a given
  activity
- an optional list of activities that the user is already working on

### Out of office

    out-of-office user-01 2020-04-25
    out-of-office user-01 2020-12-25

Out of office entries are prefixed with the 'out-of-office' keyword, and they
specify:

- the person this OOO entry refers to
- the date (YYYY-MM-DD) the person won't be up for working

# TODO

- Find a good heuristic
- Fix tests on Windows
- when people start off with multiple activities assigned, `ap` combines them
  together -- which means other activities depending on any of them, might end
  up being claimed later because the completion date was pushed too far ahead
  (i.e. when the two activities got completed)
- Shared, and mutual resources
- Docker support?!
