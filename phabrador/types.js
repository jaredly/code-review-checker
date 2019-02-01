
/*
NOTE maybe no sections to differentiate "action required" vs "waiting on others"

[newly updated]


[the rest]


type User = {
  photo: string,
  name: string,
}

type UserData = {
  [phid: string]: User,
}

type DiffData = {
  title: string,
  id: number, // D30221
  author: phid,
  status: 'changes-requested' | 'accepted' | 'changes-planned',
  // NOTE so we need to track "last time you looked at this diff"
  feed: Array<{
    userPHID: string,
    type: 'comment' | 'inline-comment' | 'accept' | 'plan changes' | 'request changes' | 'close' | 'abandon' | 'retitle',
    time: number,
    text: string,
  }>,
  userStatuses: {
    [phid: string]: 'untouched' | 'commented' | 'rejected' | 'accepted' | 'stale-rejected' | 'changes-planned',
  }
}
*/

