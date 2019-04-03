<template>
  <div>
    This is the main page!! {{ userInfo }}
  </div>
</template>

<script>
import * as rxjs from 'rxjs';
import { switchMap } from 'rxjs/operators';

import * as api from '../api';
import authService from '../api/auth';
import { pusher, channelPool } from '../api/pusher';

export default {
  subscriptions() {
    return {
      userInfo: authService.isAuthenticated$
        .pipe(
          switchMap(isAuthed => {
            if(isAuthed) {
              const userId = authService.userId;
              const chanName = `private-${userId.replace(/\|/, ';')}-user-info`;
              return api.getUserInfoUpdates(channelPool, chanName);
            }

            return rxjs.NEVER;
          })
        )
    };
  }
};
</script>

<style scoped>
</style>
