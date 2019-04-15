<template>
  <div>
    <p>This is the main page!! {{ userInfo }}</p>

    <div v-if="isAuthenticated">
      <input
        v-model="newUsername"
        type="text"
      >
      <button
        v-stream:click="{ subject: changeUsernameBtn$, data: newUsername }"
        :disabled="isUsernameBeingChanged"
      >
        {{ isUsernameBeingChanged ? 'Changing...' : 'Change username' }}
      </button>
    </div>

    <my-invite-list v-if="hasUsernameSet" />
    <my-game-list v-if="hasUsernameSet" />
  </div>
</template>

<script>
import * as rxjs from 'rxjs';
import { switchMap, pluck, startWith, last, mapTo } from 'rxjs/operators';

import * as api from '../api';
import authService from '../api/auth';
import { channelPool } from '../api/pusher';

import InviteList from './InviteList';
import GameList from './GameList';

export default {
  components: {
    'my-invite-list': InviteList,
    'my-game-list': GameList
  },

  data() {
    return {
      newUsername: ''
    };
  },

  computed: {
    hasUsernameSet() {
      if(!this.isAuthenticated || this.userInfo == null) {
        return false;
      }

      return this.userInfo.username != null;
    }
  },

  domStreams: ['changeUsernameBtn$'],

  subscriptions() {
    return {
      isAuthenticated: authService.isAuthenticated$,

      userInfo: authService.isAuthenticated$
        .pipe(
          switchMap(isAuthed =>
            isAuthed ?
              api.getUserInfoUpdates(channelPool, authService.userId) :
              rxjs.NEVER
          )
        ),

      isUsernameBeingChanged: this.changeUsernameBtn$
        .pipe(
          pluck('data'),
          switchMap(username =>
            rxjs.concat(
              rxjs.of(true),
              api.updateUserInfo({ username })
                .pipe(last(), mapTo(false))
            )
          ),
          startWith(false)
        )
    };
  }
};
</script>

<style scoped>
</style>
