<template>
  <div>
    <p>
      This is the main page!! {{ userInfo }}
      <input type="text" v-model="newUsername">
      <button
        v-stream:click="{ subject: changeUsernameBtn$, data: newUsername }"
        :disabled="isUsernameBeingChanged"
      >{{ isUsernameBeingChanged ? 'Changing...' : 'Change username' }}</button>
    </p>
    <InviteList />
  </div>
</template>

<script>
import * as rxjs from 'rxjs';
import { switchMap, pluck, startWith, last, mapTo } from 'rxjs/operators';

import * as api from '../api';
import authService from '../api/auth';
import { channelPool } from '../api/pusher';

import InviteList from './InviteList';

export default {
  components: {
    InviteList
  },

  domStreams: ['changeUsernameBtn$'],

  data() {
    return {
      newUsername: ''
    };
  },

  subscriptions() {
    return {
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
