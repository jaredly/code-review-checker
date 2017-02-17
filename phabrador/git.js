
const {exec} = require('child_process')

module.exports = {
  diffForCurrentBranch: () => new Promise((res, rej) => {
    exec("git log | grep -m 1 'Differential Revision:'", {shell: true}, (err, out, serr) => {
      if (err) return rej(err)
      const diff = out.trim().match(/\/D(\d+)$/)
      if (!diff) return rej(new Error("couldn't find diff number"))
      res(+diff[1])
    })
  }),
  isCurrentBranchRemoteTracking: () => new Promise((res, rej) => {
    exec("git for-each-ref --format='%(upstream:short)' $(git symbolic-ref -q HEAD)", {shell: true}, (err, out, serr) => {
      if (err) return rej(err)
      const diff = out.trim().match(/^origin\//)
      res(!!diff)
    })
  })
}


