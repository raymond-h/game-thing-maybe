<template>
  <div>
    <h2>Invites</h2>
    <input type="text" v-model="inviteField">
    <button v-stream:click="{ subject: sendInvite$, data: inviteField }">Send invite</button>
    <ul>
      <li v-for="invite in invites" :key="invite.id">
        {{ invite.id }}: {{ invite.player1 }} -> {{ invite.player2 }}
        <button v-if="isMe(invite.player2)" v-stream:click="{ subject: acceptInvite$, data: invite }">Accept</button>
      </li>
    </ul>
    <p>{{ sendInviteResult }}</p>
    <p>{{ acceptInviteResult }}</p>
  </div>
</template>

<script>
import * as rxjs from 'rxjs';
import { switchMap, pluck, flatMap, catchError } from 'rxjs/operators';

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
    }
  },

  subscriptions() {
    return {
      invites: authService.isAuthenticated$
        .pipe(
          switchMap(isAuthed =>
            isAuthed ?
              api.getInvitesUpdates(channelPool) :
              rxjs.NEVER
          )
        ),

      sendInviteResult: this.sendInvite$
        .pipe(
          pluck('data'),
          flatMap(username => api.sendInviteToUsername(username)),
          catchError((err, origObs) => rxjs.concat(rxjs.of(err.body.error), origObs))
        ),

      acceptInviteResult: this.acceptInvite$
        .pipe(
          pluck('data'),
          flatMap(invite => api.acceptInvite(invite.id))
        )
    };
  }
};
</script>

<style scoped>
</style>
