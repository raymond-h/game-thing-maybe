<template>
  <div>
    <p>This is the main page!! {{ userInfo }}</p>
    <my-invite-list />
  </div>
</template>

<script>
import * as rxjs from 'rxjs';
import { switchMap } from 'rxjs/operators';

import * as api from '../api';
import authService from '../api/auth';
import { channelPool } from '../api/pusher';

import InviteList from './InviteList';

export default {
  components: {
    'my-invite-list': InviteList
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
        )
    };
  }
};
</script>

<style scoped>
</style>
