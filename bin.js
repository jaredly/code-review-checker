#!/usr/bin/env node
const {fetchOne, fetchAll, fetchMine, fetchRevs} = require('./phabrador')
const {diffForCurrentBranch, isCurrentBranchRemoteTracking} = require('./phabrador/git')
const {open: openurl} = require('openurl')
const chalk = require('chalk')
// console.log(process.argv)
let [_, __, arg] = process.argv
arg = arg && arg.toLowerCase()
const options = {}

if (arg && arg[0] === '-') {
  options[arg] = true
  arg = null
}
for (let i=3; i<process.argv.length; i++) {
  options[process.argv[i]] = true
}

const showHelp = () => console.log(`\
Phabrador Retriever: Fetchin your diffs!

Usage:
- 'phab' - if in a diff branch show detail on the current diff. otherwise show all
- 'phab diffs' - show all open revisions that are yours or you are reviewing
- 'phab mine' - show your open revisions
- 'phab rev' - show open revisions that you are reviewing
- 'phab D33032' - show detail on diff D33032
`)

var Spinner = require('cli-spinner').Spinner;
const display = require('./phabrador/display')

const spin = fn => {
  var spinner = new Spinner('contacting phabricator.. %s');
  spinner.setSpinnerString('|/-\\');
  spinner.start();
  return fn(status => spinner.setSpinnerTitle(status + ' %s'), () => spinner.stop(true))
}

const showOne = num => {
  return spin((onStatus, onStop) => fetchOne(num, onStatus).then(({data, users, user}) => {
    onStop()
    console.log(display.showDetail(data, users, user))
    if (options['--open']) {
      openurl(data.diff.uri)
    }
  }, err => {
    console.error('Failed to get stuff', err)
  })
  .catch(err => console.error(err)))
}

const splitByAuthor = (datas, me) => {
  const mine = datas.filter(d => d.diff.authorPHID === me.phid)
  const others = datas.filter(d => d.diff.authorPHID !== me.phid)
  return {mine, others}
}

const howManyToSkip = diffs => {
  const aMonthAgo = Date.now() / 1000 - 60 * 60 * 24 * 30
  return diffs.filter(d => +d.diff.dateModified < aMonthAgo).length
}

const showMany = (fetcher, all) => {
  return spin((onStatus, onStop) => fetcher(onStatus).then(({datas, users, user}) => {
    onStop()
    let {mine, others} = splitByAuthor(datas, user)
    if (others.length) {
      console.log(chalk.bold.bgBlue('### Diffs to review ###\n'))
      if (others.length > 5 && !all) {
        const skipping = howManyToSkip(others)
        if (skipping > 0) {
          console.log(chalk.bold.red(`skipping ${skipping} old diffs that haven't been touched in > 1 month. add --all to see old ones too\n`))
          others = others.slice(skipping)
        }
      }
      console.log(others.map(data => display.showSummary(data, users, user)).join('\n'))
    }
    if (mine.length) {
      console.log(chalk.bold.bgBlue('### Your diffs ###\n'))
      if (mine.length > 5 && !all) {
        const skipping = howManyToSkip(mine)
        if (skipping > 0) {
          console.log(chalk.bold.red(`skipping ${skipping} old diffs that haven't been touched in > 1 month\n`))
          mine = mine.slice(skipping)
        }
      }
      console.log(mine.map(data => display.showSummary(data, users, user)).join('\n'))
    }
    if (!datas.length) {
      console.log(chalk.bold.bgBlue('### No open diffs found ###'))
    }
    if (options['--open']) {
      if (mine.length + others.length > 10) {
        return console.error(`I'm not gonna open ${datas.length} tabs`)
      }
      (mine.concat(others)).forEach(data => {
        openurl(data.diff.uri)
      })
    }
  }, err => {
    console.error('Failed to get stuff', err)
  })
  .catch(err => console.error(err)))
}

if (options['-h'] || options['-help'] || options['--help'] || arg === 'help') {
  showHelp()
} else if (arg === 'diffs') {
  showMany(fetchAll, options['--all']).catch(err => console.error(err))
} else if (arg === 'mine') {
  showMany(fetchMine).catch(err => console.error(err))
} else if (arg === 'rev' || arg === 'revs') {
  showMany(fetchRevs).catch(err => console.error(err))
} else if (!arg || arg === 'diff') {
  isCurrentBranchRemoteTracking().then(isTracking => {
    if (isTracking) return showMany(fetchAll)
    return diffForCurrentBranch().then(diffnum => {
      return showOne(diffnum)
    }, err => showMany(fetchAll))
  }, err => showMany(fetchAll))
  .catch(err => console.error(err))
} else {
  if (+arg == arg) {
    showOne(+arg).catch(err => console.error(err))
  } else {
    const match = arg.match(/D(\d+)$/)
    if (match) {
      showOne(+match[1]).catch(err => console.error(err))
    } else {
      console.log("Unrecognized argument")
      showHelp()
    }
  }
}

/*
run().then(res => {
  console.log(done)
}, err => console.log('fail', err))
*/


