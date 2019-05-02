<template>
  <div>
    <h2>Invites</h2>
    <input
      v-model="inviteField"
      type="text"
    >
    <button v-stream:click="{ subject: sendInvite$, data: inviteField }">
      Send invite
    </button>
    <span>{{ sendInviteResult }}</span>

    <p v-if="invites == null">
      Loading...
    </p>
    <ul v-else-if="invites.length > 0">
      <li
        v-for="invite in invites"
        :key="invite.id"
      >
        {{ invite.id }}: {{ getOpponentUsername(invite) }}
        <button
          v-if="isMe(invite.player2)"
          v-stream:click="{ subject: acceptInvite$, data: invite }"
        >
          Accept
        </button>
      </li>
    </ul>
    <p v-else>
      No invites to show
    </p>

    <p v-if="acceptInviteResult != null && acceptInviteResult !== ''">Error when accepting invite: {{ acceptInviteResult }}</p>
  </div>
</template>

<script>
import { uniq, assign } from 'lodash';
import * as rxjs from 'rxjs';
import { switchMap, pluck, flatMap, catchError, map, share, scan, mapTo, startWith, ignoreElements } from 'rxjs/operators';

import * as api from '../api';
import authService from '../api/auth';
import { channelPool } from '../api/pusher';

export default {
  data() {
    return {
      inviteField: ''
    };
  },

  domStreams: ['sendInvite$', 'acceptInvite$'],

  methods: {
    isMe(userId) {
      return userId === authService.userId;
    },

    getOpponentUserId(inv) {
      return this.isMe(inv.player1) ? inv.player2 : inv.player1;
    },

    getOpponentUsername(inv) {
      const opponentUid = this.getOpponentUserId(inv);

      if(this.inviteUserInfo == null || this.inviteUserInfo[opponentUid] == null) {
        return 'Loading...';
      }

      return this.inviteUserInfo[opponentUid];
    }
  },

  subscriptions() {
    const invites = authService.isAuthenticated$
      .pipe(
        switchMap(isAuthed =>
          isAuthed ?
            api.getInvitesUpdates(channelPool) :
            rxjs.NEVER
        ),
        share()
      );

    return {
      invites,

      inviteUserInfo: invites
        .pipe(
          map(invList =>
            invList.map(inv => this.getOpponentUserId(inv))
          ),
          map(uniq),
          switchMap(uids =>
            rxjs.merge(...uids.map(uid =>
              api.getUserInfoForUser(uid)
                .pipe(
                  map(uInfo => ({ [uid]: uInfo.username }))
                )
            ))
          ),
          scan((acc, val) => assign({}, acc, val), {})
        ),

      sendInviteResult: this.sendInvite$
        .pipe(
          pluck('data'),
          flatMap(username =>
            api.sendInviteToUsername(username)
              .pipe(
                mapTo('Success!'),
                startWith('Sending...')
              )
          ),
          catchError((err, origObs) => rxjs.concat(rxjs.of(err.body.error), origObs))
        ),

      acceptInviteResult: this.acceptInvite$
        .pipe(
          pluck('data'),
          flatMap(invite => api.acceptInvite(invite.id)),
          ignoreElements(),
          catchError((err, origObs) => rxjs.concat(rxjs.of(err.body.error), origObs))
        )
    };
  }
};
</script>

<style scoped>
</style>
