<template>
  <div>
    <p>This is the main page!! {{ userInfo }}</p>
    <InviteList />
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
    InviteList
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
