
const createCanduit = require('canduit')

const prom = fn => new Promise((res, rej) => fn((err, val) => err ? rej(err) : res(val)))
const makeExec = canduit => (endpoint, args = {}) => prom(done => canduit.exec(endpoint, args, done))

// const {showDetail, showSummary} = require('./display')

/** TODO don't need yet
const queryPaged = (exec, endpoint, args = {}) => {
  let after = null
  let
  const next = () => {
    return exec(enpoint, after ? {...args, after} : args)
      .then(result => {
      })
  }
}
*/

const fetchInfo = (exec, diffs, me, onStatus) => {
  const PHIDs = diffs.map(diff => diff.phid)
  const revisionIds = diffs.map(diff => diff.id)
  onStatus('fetching comments')
  return Promise.all([
    // TODO account for pages
    exec('feed.query', {filterPHIDs: diffs.map(diff => diff.phid), view: 'text'}),
    exec('differential.getrevisioncomments', {ids: diffs.map(diff => diff.id)})
  ]).then(([feeds, comments]) => {
    const byId = {}
    const byPhid = {}
    const usersToLookup = {}
    diffs.forEach(diff => {
      byId[diff.id] = byPhid[diff.phid] = {diff, feed: []}
      diff.reviewers.forEach(u => usersToLookup[u] = true)
    })
    Object.keys(comments).forEach(id => {
      byId[id].comments = comments[id]
      comments[id].forEach(comment => usersToLookup[comment.authorPHID] = true)
    })
    Object.keys(feeds).forEach(key => {
      feeds[key].id = key
      usersToLookup[feeds[key].authorPHID] = true
      byPhid[feeds[key].objectPHID].feed.push(feeds[key])
    })
    onStatus('fetching user info')
    return exec('user.query', {
      phids: Object.keys(usersToLookup).filter(id => id !== me.phid),
    }).then(userData => {
      const users = {[me.phid]: me}
      userData.forEach(user => users[user.phid] = user)
      Object.keys(byId).forEach(id => {
        const diff = byId[id].diff
        const {statuses, commented} = collectStatus(diff.id, byId[id].feed, diff.reviewers, users)
        byId[id].statuses = statuses
        byId[id].commented = commented
      })
      return {byId, users}
    })
  })
}

const parseAction = (text, id) => text.split(` D${id}:`)[0].replace(/^\w+\s/, '').trim()

const statusActions = {
  'accepted': 'accepted',
  'requested changes to': 'rejected',
  'planned changes to': 'changes-planned',
  'closed': 'closed',
}

const commentActions = ['added inline comments to', 'added a comment to']

const collectStatus = (id, feed, reviewers, users) => {
  const allActions = {}
  const statuses = {}
  const commented = {}
  const stale = {}
  feed.forEach(item => {
    if (statuses[item.authorPHID] &&
        statuses[item.authorPHID] !== 'untouched' &&
        statuses[item.authorPHID] !== 'commented') return
    const action = parseAction(item.text, id)
    if (commentActions.indexOf(action) !== -1) {
      commented[action] = true
      statuses[item.authorPHID] = 'commented'
    } else if (action === 'updated the diff for') {
      reviewers.forEach(id => {
        if (!statuses[id]) stale[id] = true
      })
    }
    allActions[action] = true
    if (statusActions[action]) {
      let status = statusActions[action]
      if (status === 'rejected' && stale[id]) {
        status = 'stale-rejected'
      }
      statuses[item.authorPHID] = status
    }
  })
  // console.log('actions', Object.keys(allActions)) // DEBUG
  return {statuses, commented}
}

const fetchOne = (num, onStatus) => {
  return prom(done => createCanduit(done)).then(makeExec)
  .then(exec => {
    return exec('user.whoami').then(user => {
      onStatus('fetching revision')
      return exec('differential.query', {ids: [num]}).then(result => {
        if (!result.length) {
          throw new UserError(`Diff ${num} not found`)
        }
        return fetchInfo(exec, result, user, onStatus).then(({byId, users}) => {
          return {data: byId[num], users, user}
        })
      }, err => {throw new PhabError("Unable to query phabricator for diffs")})
    }, err => {throw new PhabError("Unable to get user info from phabricator")})
  })
}

const diffSort = (a, b) => +a.diff.dateModified - +b.diff.dateModified

const fetchAll = onStatus => {
  return prom(done => createCanduit(done)).then(makeExec)
  .then(exec => {
    return exec('user.whoami').then(user => {
      return Promise.all([
        exec('differential.query', {reviewers: [user.phid], status: 'status-open'}),
        exec('differential.query', {authors: [user.phid], status: 'status-open'}),
      ]).then(([others, mine]) => {
        const diffs = others.concat(mine)
        return fetchInfo(exec, diffs, user, onStatus).then(({byId, users}) => {
          return {datas: Object.keys(byId).map(id => byId[id]).sort(diffSort), users, user}
        })
      })
    })
  })
}

const fetchMine = onStatus => {
  return prom(done => createCanduit(done)).then(makeExec)
  .then(exec => {
    return exec('user.whoami').then(user => {
      return exec('differential.query', {authors: [user.phid], status: 'status-open'})
      .then(diffs => {
        return fetchInfo(exec, diffs, user, onStatus).then(({byId, users}) => {
          return {datas: Object.keys(byId).map(id => byId[id]).sort(diffSort), users, user}
        })
      })
    })
  })
}

const fetchRevs = onStatus => {
  return prom(done => createCanduit(done)).then(makeExec)
  .then(exec => {
    return exec('user.whoami').then(user => {
      return exec('differential.query', {reviewers: [user.phid], status: 'status-open'})
      .then(diffs => {
        return fetchInfo(exec, diffs, user, onStatus).then(({byId, users}) => {
          return {datas: Object.keys(byId).map(id => byId[id]).sort(diffSort), users, user}
        })
      })
    })
  })
}

module.exports = {fetchAll, fetchOne, fetchMine, fetchRevs}
