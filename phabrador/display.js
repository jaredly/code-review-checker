
const chalk = require('chalk')

const commentEmoji = '💬'

const statusEmojis = {
  'untouched': '💤',
  'accepted': '✅',
  'rejected': '🚫',
  'stale-rejected': '🚫🕓',
  'closed': '🛳',
  'changes-planned': '✋',
  'commented': commentEmoji,
}

const mergeComments = comments => {
  const res = []
  comments.slice().reverse().forEach(comment => {
    if (!comment.content) return
    const last = res[res.length - 1]
    if (last && comment.authorPHID === last.authorPHID && comment.dateCreated === last.dateCreated) {
      last.content += '\n' + comment.content
    } else {
      res.push(comment)
    }
  })
  return res
}

const formatComments = (comments, users) => {
  return mergeComments(comments)
    .map(comment => `${commentEmoji}  ${users[comment.authorPHID].userName} [${new Date(+comment.dateCreated * 1000).toLocaleString()}]
${comment.content.trim()}`).join('\n\n')
}

const formatStatuses = (statuses, commented, reviewers, users, me) => {
  return reviewers.sort((a, b) => a === me.phid ? -1 : (b === me.phid ? 1 : 0))
    .map(id => `${statusEmojis[statuses[id] || 'untouched']}${commented[id] ? commentEmoji : ''}  ${id === me.phid ? chalk.bold.magenta(users[id].userName) : users[id].userName}`)
    .join('   ')
}

const topLine = (diff, users, me, statusHighlight) => {
  const authorName = users[diff.authorPHID].userName
  const id = `[D${diff.id}]`
  const top = `${statusHighlight(id)} @${authorName}: ${chalk.bold(diff.title)} [${diff.uri}]`
  return top
}

const showDetail = ({diff, feed, comments, statuses, commented}, users, me) => {
  const authorName = users[diff.authorPHID].userName
  const authorStatus = statuses[diff.authorPHID] === 'changes-planned'
    ? `\n${statusEmojis['changes-planned']}  ${authorName} plans changes`
    : (statuses[diff.authorPHID] === 'closed'
        ? `\n${statusEmojis.closed}  ${authorName} closed the revision` : '')
  const statusHighlight = getStatusHighlight(diff.reviewers, statuses, users, me)
  return `\
${topLine(diff, users, me, statusHighlight)}

${chalk.bold('# Summary:')}
${diff.summary}

${chalk.bold('# Test plan:')}
${diff.testPlan}

${chalk.bold('# Comments:')}
${formatComments(comments, users)}

${statusHighlight('Review Status:')}
${formatStatuses(statuses, commented, diff.reviewers, users, me)}
${authorStatus}
`
}

const getStatusHighlight = (reviewers, statuses, users, me) => {
  let nextStep = null
  let allClear = !reviewers.some(id => statuses[id] !== 'accepted')
  if (allClear) return chalk.bold.bgGreen
  let needsChanges = reviewers.some(id => statuses[id] === 'rejected')
  if (needsChanges) return chalk.bold.bgRed
  return chalk.bold
}

const showSummary = ({diff, feed, comments, statuses, commented}, users, me) => {
  const authorName = users[diff.authorPHID].userName
  const authorStatus = statuses[diff.authorPHID] === 'changes-planned'
    ? `\n${statusEmojis['changes-planned']}  ${authorName} plans changes`
    : (statuses[diff.authorPHID] === 'closed'
        ? `\n${statusEmojis.closed}  ${authorName} closed the revision` : '')
  const statusHighlight = getStatusHighlight(diff.reviewers, statuses, users, me)
  const lastComment = comments.length > 0
    ? comments[comments.length - 1]
    : null
  const lastCommentText = lastComment
    ? `\n${commentEmoji}  last comment by ${users[lastComment.authorPHID].userName} - ${new Date(+lastComment.dateCreated * 1000).toLocaleString()}` : ''
  return `\
${topLine(diff, users, me, statusHighlight)} ${lastCommentText}
${formatStatuses(statuses, commented, diff.reviewers, users, me)}
${authorStatus}`
}

module.exports = {showDetail, showSummary}
