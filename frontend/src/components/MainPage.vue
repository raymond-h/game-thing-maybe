<template>
  <div>
    This is the main page!! {{ userInfo }}
  </div>
</template>

<script>
import { pluck, filter, flatMap } from 'rxjs/operators';

import * as api from '../api';
import authService from '../api/auth';
import pusher from '../api/pusher';

export default {
  mounted() {
    const userId = authService.userId;
    this.userInfoChan = pusher.subscribe(`private-${userId.replace(/\|/, ';')}-user-info`);
  },

  data() {
    return {
      userInfoChan: null
    };
  },

  subscriptions() {
    return {
      userInfo: this.$watchAsObservable('userInfoChan')
        .pipe(
          pluck('newValue'),
          filter(v => v != null),
          flatMap(api.getUserInfoUpdates)
        )
    };
  }
};
</script>

<style scoped>
</style>
